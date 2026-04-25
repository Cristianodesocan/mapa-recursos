suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(readr)
  library(httr2)
  library(jsonlite)
  library(sf)
  library(tibble)
  library(purrr)
  library(digest)
})

stage_script_path <- normalizePath(
  gsub(
    "~\\+~",
    " ",
    sub(
    "^--file=",
    "",
    commandArgs(trailingOnly = FALSE)[startsWith(commandArgs(trailingOnly = FALSE), "--file=")][[1]]
    )
  ),
  winslash = "/",
  mustWork = TRUE
)
source(file.path(
  dirname(dirname(dirname(stage_script_path))),
  "R",
  "utils_pdf.R"
))
source(file.path(
  dirname(dirname(dirname(stage_script_path))),
  "R",
  "canarias_geo.R"
))

CACHE_COLS <- c(
  "query_key", "query", "geocoder", "lat", "lon",
  "display_name_geocoder", "geocode_status",
  "geocode_source", "match_level", "relevance",
  "isla_esperada", "validated_in_island"
)

empty_cache <- function() {
  tibble(
    query_key = character(),
    query = character(),
    geocoder = character(),
    lat = numeric(),
    lon = numeric(),
    display_name_geocoder = character(),
    geocode_status = character(),
    geocode_source = character(),
    match_level = character(),
    relevance = numeric(),
    isla_esperada = character(),
    validated_in_island = logical()
  )
}

load_cache <- function(cache_path) {
  if (!file.exists(cache_path)) return(empty_cache())
  cache <- read_csv(cache_path, show_col_types = FALSE)
  for (col in setdiff(CACHE_COLS, names(cache))) {
    cache[[col]] <- switch(col,
      lat = NA_real_, lon = NA_real_, relevance = NA_real_,
      validated_in_island = NA, NA_character_
    )
  }
  cache <- cache[, CACHE_COLS]
  cache %>% filter(geocode_status %in% c("ok", "not_found"))
}

save_cache <- function(cache, cache_path) {
  safe_write_csv(cache, cache_path)
}

build_query_levels <- function(direccion, codigo_postal, municipio, isla,
                               provincia, comunidad = "Canarias",
                               pais = "Espana") {
  direccion <- coalesce_scalar(direccion)
  codigo_postal <- coalesce_scalar(codigo_postal)
  municipio <- coalesce_scalar(municipio)
  isla <- coalesce_scalar(isla)
  provincia <- coalesce_scalar(provincia)
  comunidad <- coalesce_scalar(comunidad)
  pais <- coalesce_scalar(pais)

  pieces <- function(...) {
    p <- c(...)
    p[!is.na(p) & nzchar(p)]
  }

  full <- pieces(direccion, codigo_postal, municipio, isla, provincia, comunidad, pais)
  no_addr <- pieces(codigo_postal, municipio, isla, provincia, comunidad, pais)
  cp_isla <- pieces(codigo_postal, isla, provincia, comunidad, pais)
  isla_only <- pieces(isla, provincia, comunidad, pais)

  levels <- list()
  if (!is.na(direccion) && length(full) >= 2) {
    levels[["direccion_completa"]] <- paste(full, collapse = ", ")
  }
  if (!is.na(municipio) && length(no_addr) >= 2) {
    levels[["municipio_isla"]] <- paste(no_addr, collapse = ", ")
  }
  if (!is.na(codigo_postal) && length(cp_isla) >= 2) {
    levels[["codigo_postal"]] <- paste(cp_isla, collapse = ", ")
  }
  if (!is.na(isla) && length(isla_only) >= 2) {
    levels[["isla"]] <- paste(isla_only, collapse = ", ")
  }
  levels
}

is_transient_error <- function(status) {
  status %in% c("rate_limited", "server_error", "network_error", "invalid_response")
}

# ---------------- Nominatim ----------------

nominatim_request <- function(query, user_agent, attempts = 3, viewbox = NULL,
                              bounded = FALSE) {
  params <- list(
    q = query,
    format = "jsonv2",
    limit = 1,
    countrycodes = "es",
    addressdetails = 1
  )
  if (!is.null(viewbox)) {
    params$viewbox <- viewbox
    if (isTRUE(bounded)) params$bounded <- 1
  }

  req <- request("https://nominatim.openstreetmap.org/search") %>%
    req_url_query(!!!params) %>%
    req_user_agent(user_agent) %>%
    req_timeout(30) %>%
    req_error(is_error = function(resp) FALSE)

  last_http_status <- NA_integer_
  last_error_message <- NA_character_

  for (attempt in seq_len(attempts)) {
    response <- tryCatch(req_perform(req), error = function(e) e)

    if (inherits(response, "error")) {
      last_error_message <- conditionMessage(response)
      if (attempt < attempts) Sys.sleep(2 ^ attempt)
      next
    }

    status_code <- resp_status(response)
    last_http_status <- status_code

    if (status_code == 429) {
      if (attempt < attempts) Sys.sleep(2 ^ attempt * 2)
      next
    }
    if (status_code >= 500 && status_code < 600) {
      if (attempt < attempts) Sys.sleep(2 ^ attempt)
      next
    }
    if (status_code >= 400) {
      return(list(status = "client_error", http_status = status_code))
    }

    body <- tryCatch(resp_body_json(response, simplifyVector = TRUE), error = function(e) NULL)
    if (is.null(body)) {
      return(list(status = "invalid_response", http_status = status_code))
    }
    if (!length(body)) {
      return(list(status = "not_found", http_status = status_code))
    }
    return(list(
      status = "ok",
      http_status = status_code,
      lat = as.numeric(body$lat[[1]]),
      lon = as.numeric(body$lon[[1]]),
      display_name = body$display_name[[1]] %||% NA_character_,
      relevance = as.numeric(body$importance[[1]] %||% NA_real_)
    ))
  }

  if (!is.na(last_http_status) && last_http_status == 429) {
    return(list(status = "rate_limited", http_status = last_http_status))
  }
  if (!is.na(last_http_status) && last_http_status >= 500) {
    return(list(status = "server_error", http_status = last_http_status))
  }
  list(status = "network_error", http_status = last_http_status, message = last_error_message)
}

# ---------------- Mapbox ----------------

mapbox_request <- function(query, token, attempts = 3, bbox = NULL,
                           proximity = NULL, language = "es") {
  if (is.null(token) || !nzchar(token)) {
    return(list(status = "no_token"))
  }

  query_encoded <- utils::URLencode(query, reserved = TRUE)
  url <- sprintf("https://api.mapbox.com/geocoding/v5/mapbox.places/%s.json",
                 query_encoded)

  params <- list(
    access_token = token,
    country = "es",
    language = language,
    limit = 1,
    types = "address,postcode,place,locality,neighborhood"
  )
  if (!is.null(bbox)) {
    params$bbox <- paste(bbox, collapse = ",")
  }
  if (!is.null(proximity)) {
    params$proximity <- paste(proximity, collapse = ",")
  }

  req <- request(url) %>%
    req_url_query(!!!params) %>%
    req_timeout(30) %>%
    req_error(is_error = function(resp) FALSE)

  last_http_status <- NA_integer_
  last_error_message <- NA_character_

  for (attempt in seq_len(attempts)) {
    response <- tryCatch(req_perform(req), error = function(e) e)

    if (inherits(response, "error")) {
      last_error_message <- conditionMessage(response)
      if (attempt < attempts) Sys.sleep(2 ^ attempt)
      next
    }

    status_code <- resp_status(response)
    last_http_status <- status_code

    if (status_code == 429) {
      if (attempt < attempts) Sys.sleep(2 ^ attempt * 2)
      next
    }
    if (status_code == 401 || status_code == 403) {
      return(list(status = "auth_error", http_status = status_code))
    }
    if (status_code >= 500 && status_code < 600) {
      if (attempt < attempts) Sys.sleep(2 ^ attempt)
      next
    }
    if (status_code >= 400) {
      return(list(status = "client_error", http_status = status_code))
    }

    body <- tryCatch(resp_body_json(response, simplifyVector = FALSE),
                     error = function(e) NULL)
    if (is.null(body)) {
      return(list(status = "invalid_response", http_status = status_code))
    }
    features <- body$features
    if (!length(features)) {
      return(list(status = "not_found", http_status = status_code))
    }

    feature <- features[[1]]
    coords <- feature$center
    if (length(coords) < 2) {
      return(list(status = "invalid_response", http_status = status_code))
    }
    return(list(
      status = "ok",
      http_status = status_code,
      lat = as.numeric(coords[[2]]),
      lon = as.numeric(coords[[1]]),
      display_name = feature$place_name %||% NA_character_,
      relevance = as.numeric(feature$relevance %||% NA_real_)
    ))
  }

  if (!is.na(last_http_status) && last_http_status == 429) {
    return(list(status = "rate_limited", http_status = last_http_status))
  }
  if (!is.na(last_http_status) && last_http_status >= 500) {
    return(list(status = "server_error", http_status = last_http_status))
  }
  list(status = "network_error", http_status = last_http_status,
       message = last_error_message)
}

# ---------------- Cascada con validacion por isla ----------------

bbox_for_mapbox <- function(bbox) {
  if (is.null(bbox)) return(NULL)
  c(bbox$min_lon, bbox$min_lat, bbox$max_lon, bbox$max_lat)
}

bbox_for_nominatim_viewbox <- function(bbox) {
  if (is.null(bbox)) return(NULL)
  paste(bbox$min_lon, bbox$max_lat, bbox$max_lon, bbox$min_lat, sep = ",")
}

call_geocoder <- function(geocoder, query, mapbox_token, user_agent,
                          bbox = NULL, sleep_seconds = 1) {
  if (geocoder == "mapbox") {
    res <- mapbox_request(query, mapbox_token, bbox = bbox_for_mapbox(bbox))
  } else {
    viewbox <- bbox_for_nominatim_viewbox(bbox)
    res <- nominatim_request(query, user_agent, viewbox = viewbox,
                             bounded = !is.null(viewbox))
  }
  Sys.sleep(sleep_seconds)
  res
}

geocode_with_cascade <- function(direccion, codigo_postal, municipio, isla,
                                 provincia, cache, mapbox_token, user_agent,
                                 islas_table, geocoder_pref = "cascade",
                                 sleep_seconds = 1) {
  bbox <- if (!is.na(isla)) isla_to_bbox(isla, islas_table) else NULL

  levels <- build_query_levels(direccion, codigo_postal, municipio, isla,
                               provincia)
  if (!length(levels)) {
    return(list(
      cache = cache,
      transient_error = FALSE,
      result = tibble(
        lat = NA_real_, lon = NA_real_,
        display_name_geocoder = NA_character_,
        geocode_status = "missing_address",
        geocode_source = NA_character_,
        match_level = NA_character_,
        relevance = NA_real_,
        validated_in_island = NA
      )
    ))
  }

  geocoder_chain <- switch(geocoder_pref,
    cascade = c("mapbox", "nominatim"),
    mapbox = "mapbox",
    nominatim = "nominatim",
    c("mapbox", "nominatim")
  )

  if (geocoder_pref == "cascade" &&
      (is.null(mapbox_token) || !nzchar(mapbox_token))) {
    geocoder_chain <- "nominatim"
  }

  last_transient_status <- NA_character_

  for (level_name in names(levels)) {
    query <- levels[[level_name]]

    for (geocoder in geocoder_chain) {
      query_key <- digest(
        paste(geocoder, str_to_lower(query), sep = "::"),
        algo = "xxhash64"
      )

      cached <- cache %>% filter(query_key == !!query_key)
      if (nrow(cached)) {
        hit <- cached[1, ]
        if (hit$geocode_status == "ok" &&
            (is.null(bbox) || isTRUE(hit$validated_in_island))) {
          return(list(
            cache = cache,
            transient_error = FALSE,
            result = tibble(
              lat = hit$lat, lon = hit$lon,
              display_name_geocoder = hit$display_name_geocoder,
              geocode_status = "ok",
              geocode_source = hit$geocoder,
              match_level = level_name,
              relevance = hit$relevance,
              validated_in_island = hit$validated_in_island
            )
          ))
        }
        if (hit$geocode_status == "not_found") next
      }

      log_info("Geocode [{geocoder}|{level_name}] {query}")
      response <- call_geocoder(geocoder, query, mapbox_token, user_agent,
                                bbox = bbox, sleep_seconds = sleep_seconds)

      if (response$status == "ok") {
        validated <- if (is.null(bbox)) NA
                     else coords_inside_bbox(response$lat, response$lon, bbox)
        new_row <- tibble(
          query_key = query_key, query = query, geocoder = geocoder,
          lat = response$lat, lon = response$lon,
          display_name_geocoder = response$display_name,
          geocode_status = if (isTRUE(validated) || is.na(validated)) "ok"
                           else "out_of_island",
          geocode_source = geocoder,
          match_level = level_name,
          relevance = response$relevance %||% NA_real_,
          isla_esperada = isla %||% NA_character_,
          validated_in_island = validated
        )
        cache <- bind_rows(cache, new_row)

        if (isTRUE(validated) || is.na(validated)) {
          return(list(
            cache = cache,
            transient_error = FALSE,
            result = tibble(
              lat = response$lat, lon = response$lon,
              display_name_geocoder = response$display_name,
              geocode_status = "ok",
              geocode_source = geocoder,
              match_level = level_name,
              relevance = response$relevance %||% NA_real_,
              validated_in_island = validated
            )
          ))
        }
        log_warn("Coord fuera de isla esperada ({isla}) en [{geocoder}|{level_name}]; descartando.")
        next
      }

      if (response$status == "not_found") {
        cache <- bind_rows(cache, tibble(
          query_key = query_key, query = query, geocoder = geocoder,
          lat = NA_real_, lon = NA_real_,
          display_name_geocoder = NA_character_,
          geocode_status = "not_found",
          geocode_source = geocoder,
          match_level = level_name,
          relevance = NA_real_,
          isla_esperada = isla %||% NA_character_,
          validated_in_island = NA
        ))
        next
      }

      if (response$status == "no_token" || response$status == "auth_error") {
        log_warn("Mapbox no disponible ({response$status}); cayendo a Nominatim.")
        next
      }

      if (is_transient_error(response$status)) {
        log_warn("Error transitorio en [{geocoder}|{level_name}]: {response$status} (http={response$http_status %||% 'NA'})")
        last_transient_status <- response$status
        next
      }

      log_warn("Respuesta inesperada en [{geocoder}|{level_name}]: {response$status}")
      last_transient_status <- response$status
    }
  }

  if (!is.na(last_transient_status)) {
    return(list(
      cache = cache,
      transient_error = TRUE,
      result = tibble(
        lat = NA_real_, lon = NA_real_,
        display_name_geocoder = NA_character_,
        geocode_status = last_transient_status,
        geocode_source = NA_character_,
        match_level = NA_character_,
        relevance = NA_real_,
        validated_in_island = NA
      )
    ))
  }

  list(
    cache = cache,
    transient_error = FALSE,
    result = tibble(
      lat = NA_real_, lon = NA_real_,
      display_name_geocoder = NA_character_,
      geocode_status = "not_found",
      geocode_source = NA_character_,
      match_level = NA_character_,
      relevance = NA_real_,
      validated_in_island = NA
    )
  )
}

parse_positive_numeric <- function(value, default, name) {
  if (is.null(value) || isTRUE(value)) return(default)
  num <- suppressWarnings(as.numeric(value))
  if (is.na(num) || num < 0) {
    log_abort("Parametro --{name} debe ser numerico >= 0 (recibido: {value})")
  }
  num
}

main <- function() {
  args <- parse_cli_args()
  root <- normalizePath(args$root %||% default_root_from_stage_script(),
                        winslash = "/", mustWork = TRUE)

  user_agent <- args$`user-agent` %||%
    Sys.getenv("MAPA_RECURSOS_UA",
               unset = "MapaRecursosPipeline/2.0 (observatorio@odesocan.org)")

  mapbox_token <- args$`mapbox-token` %||%
    Sys.getenv("MAPBOX_ACCESS_TOKEN", unset = "")
  geocoder_pref <- args$geocoder %||% "cascade"
  if (!geocoder_pref %in% c("cascade", "mapbox", "nominatim")) {
    log_abort("--geocoder debe ser cascade|mapbox|nominatim (recibido: {geocoder_pref})")
  }
  if (geocoder_pref %in% c("cascade", "mapbox") && !nzchar(mapbox_token)) {
    if (geocoder_pref == "mapbox") {
      log_abort("--geocoder mapbox requiere --mapbox-token o env MAPBOX_ACCESS_TOKEN.")
    }
    log_warn("Sin token de Mapbox; cascade degradara a solo Nominatim.")
  }

  sleep_seconds <- parse_positive_numeric(args$sleep, 1, "sleep")
  if (sleep_seconds < 1 && geocoder_pref != "mapbox") {
    log_warn("--sleep={sleep_seconds} es inferior a la politica de 1s/request de Nominatim; forzando a 1s.")
    sleep_seconds <- 1
  }
  max_consecutive_failures <- as.integer(
    parse_positive_numeric(args$`max-failures`, 5, "max-failures")
  )

  input_path <- file.path(root, "salidas", "02_transformacion",
                          "recursos_normalizados.csv")
  output_dir <- file.path(root, "salidas", "03_geocodificacion")
  cache_dir <- file.path(root, "cache")
  cache_path <- file.path(cache_dir, "geocoding_cache.csv")

  ensure_dir(output_dir)
  ensure_dir(cache_dir)

  if (!file.exists(input_path)) {
    log_abort("No existe el dataset normalizado: {input_path}")
  }

  data <- read_csv(input_path, show_col_types = FALSE)
  cache <- load_cache(cache_path)
  islas_table <- load_canarias_islas(root)

  needed_cols <- c("direccion", "codigo_postal", "municipio", "isla", "provincia")
  for (col in needed_cols) {
    if (!col %in% names(data)) data[[col]] <- NA_character_
  }

  unique_targets <- data %>%
    distinct(direccion, codigo_postal, municipio, isla, provincia) %>%
    mutate(target_idx = row_number())

  log_info("Geocodificando {nrow(unique_targets)} combinaciones unicas (cache: {nrow(cache)} entradas, geocoder={geocoder_pref}, sleep={sleep_seconds}s)")

  results <- vector("list", nrow(unique_targets))
  consecutive_failures <- 0L
  aborted_due_to_failures <- FALSE

  for (idx in seq_len(nrow(unique_targets))) {
    row <- unique_targets[idx, ]

    if (aborted_due_to_failures) {
      results[[idx]] <- bind_cols(row, tibble(
        lat = NA_real_, lon = NA_real_,
        display_name_geocoder = NA_character_,
        geocode_status = "skipped_after_failures",
        geocode_source = NA_character_,
        match_level = NA_character_,
        relevance = NA_real_,
        validated_in_island = NA
      ))
      next
    }

    geo <- geocode_with_cascade(
      direccion = row$direccion,
      codigo_postal = row$codigo_postal,
      municipio = row$municipio,
      isla = row$isla,
      provincia = row$provincia,
      cache = cache,
      mapbox_token = mapbox_token,
      user_agent = user_agent,
      islas_table = islas_table,
      geocoder_pref = geocoder_pref,
      sleep_seconds = sleep_seconds
    )
    cache <- geo$cache
    results[[idx]] <- bind_cols(row, geo$result)

    if (isTRUE(geo$transient_error)) {
      consecutive_failures <- consecutive_failures + 1L
      if (consecutive_failures >= max_consecutive_failures) {
        save_cache(cache, cache_path)
        log_warn("{consecutive_failures} fallos transitorios consecutivos; abortando geocodificacion. Restantes marcados como 'skipped_after_failures'.")
        aborted_due_to_failures <- TRUE
      }
    } else {
      consecutive_failures <- 0L
    }

    if (idx %% 25 == 0) save_cache(cache, cache_path)
  }
  save_cache(cache, cache_path)

  geocoded_targets <- bind_rows(results)

  geocoded <- data %>%
    select(-any_of(c("lat", "lon", "geocode_status"))) %>%
    left_join(geocoded_targets,
              by = c("direccion", "codigo_postal", "municipio", "isla", "provincia")) %>%
    select(-any_of("target_idx"))

  csv_path <- file.path(output_dir, "recursos_geocodificados.csv")
  geojson_path <- file.path(output_dir, "recursos_geocodificados.geojson")

  safe_write_csv(geocoded, csv_path)

  geo_rows <- geocoded %>% filter(!is.na(lat), !is.na(lon))
  if (nrow(geo_rows)) {
    sf_data <- st_as_sf(geo_rows, coords = c("lon", "lat"), crs = 4326,
                        remove = FALSE)
    st_write(sf_data, geojson_path, delete_dsn = TRUE, quiet = TRUE)
  } else {
    write_empty_geojson(geojson_path)
  }

  ok_count <- sum(geocoded$geocode_status == "ok", na.rm = TRUE)
  by_source <- geocoded %>%
    filter(geocode_status == "ok") %>%
    count(geocode_source, name = "n")
  by_source_str <- if (nrow(by_source)) {
    paste(by_source$geocode_source, by_source$n, sep = "=", collapse = ", ")
  } else "0"
  transient_count <- sum(geocoded$geocode_status %in%
    c("rate_limited", "server_error", "network_error", "invalid_response", "client_error"),
    na.rm = TRUE)
  out_island_count <- sum(geocoded$geocode_status == "out_of_island", na.rm = TRUE)
  skipped_count <- sum(geocoded$geocode_status == "skipped_after_failures", na.rm = TRUE)

  log_info("Geocodificados {ok_count}/{nrow(geocoded)} ({round(100*ok_count/nrow(geocoded),1)}%) -> {csv_path}")
  log_info("Por fuente: {by_source_str}")
  if (out_island_count > 0) {
    log_warn("Coords descartadas por caer fuera de isla esperada: {out_island_count}")
  }
  if (transient_count > 0) {
    log_warn("Registros con error transitorio: {transient_count}")
  }
  if (skipped_count > 0) {
    log_warn("Registros no procesados tras abort: {skipped_count}")
  }
  if (aborted_due_to_failures) {
    log_abort("Geocodificacion abortada por {max_consecutive_failures} fallos consecutivos. Revisa la conectividad y reintenta (cache conserva los OK).")
  }
}

if (sys.nframe() == 0) {
  main()
}
