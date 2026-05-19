suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
  library(yaml)
  library(dplyr)
  library(stringr)
  library(tibble)
  library(purrr)
  library(glue)
  library(cli)
})

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) return(y)
  if (length(x) == 1 && is.na(x)) return(y)
  x
}

load_scraper_config <- function(config_path) {
  if (!file.exists(config_path)) {
    cli::cli_abort("No existe el archivo de configuracion: {config_path}")
  }
  yaml::read_yaml(config_path)
}

resolve_path <- function(root, path) {
  if (is.null(path) || !nzchar(path)) return(NULL)
  ## Detectar path absoluto sin depender del paquete fs
  ## Cubre: /ruta (Unix), C:\ruta o C:/ruta (Windows), //UNC
  if (grepl("^(/|[A-Za-z]:[/\\\\]|//)", path)) return(path)
  file.path(root, path)
}

ensure_parent_dir <- function(path) {
  if (is.null(path)) return(invisible(NULL))
  parent <- dirname(path)
  if (!dir.exists(parent)) dir.create(parent, recursive = TRUE, showWarnings = FALSE)
}

cache_page_path <- function(cache_dir, page) {
  file.path(cache_dir, sprintf("page_%06d.json", page))
}

read_cached_page <- function(cache_dir, page) {
  path <- cache_page_path(cache_dir, page)
  if (!file.exists(path)) return(NULL)
  tryCatch(
    jsonlite::fromJSON(path, simplifyDataFrame = FALSE),
    error = function(e) NULL
  )
}

write_cached_page <- function(cache_dir, page, payload) {
  ensure_parent_dir(cache_page_path(cache_dir, page))
  path <- cache_page_path(cache_dir, page)
  writeLines(
    jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null", na = "null"),
    path, useBytes = TRUE
  )
  invisible(path)
}

fetch_page <- function(cfg, page) {
  req <- request(cfg$base_url) %>%
    req_url_query(
      page = page,
      showAllElements = tolower(as.character(isTRUE(cfg$show_all_elements))),
      orderBy = cfg$order_by %||% "nci",
      orderSort = cfg$order_sort %||% "asc"
    ) %>%
    req_user_agent(cfg$user_agent %||% "MapaRecursosScraper/1.0") %>%
    req_headers(Accept = "application/json") %>%
    req_timeout(cfg$timeout_seconds %||% 45)

  attempts <- max(1, as.integer(cfg$max_retries %||% 3))
  backoff <- as.numeric(cfg$backoff_base %||% 2)

  for (attempt in seq_len(attempts)) {
    response <- tryCatch(req_perform(req), error = function(e) e)
    if (!inherits(response, "error")) {
      status <- resp_status(response)
      if (status >= 200 && status < 300) {
        body <- tryCatch(resp_body_json(response, simplifyVector = FALSE), error = function(e) NULL)
        if (!is.null(body)) return(body)
      }
      if (status == 429 || status >= 500) {
        Sys.sleep(backoff ^ attempt)
        next
      }
      cli::cli_abort("HTTP {status} en pagina {page}")
    }
    if (attempt < attempts) Sys.sleep(backoff ^ attempt)
  }
  cli::cli_abort("Fallo definitivo al recuperar pagina {page} tras {attempts} intentos")
}

get_page <- function(cfg, page, cache_dir) {
  if (!isTRUE(cfg$overwrite_cache)) {
    cached <- read_cached_page(cache_dir, page)
    if (!is.null(cached)) return(list(payload = cached, from_cache = TRUE))
  }
  payload <- fetch_page(cfg, page)
  write_cached_page(cache_dir, page, payload)
  list(payload = payload, from_cache = FALSE)
}

records_from_payload <- function(payload) {
  records <- payload$listAsociacionResponse %||% list()
  if (!length(records)) return(tibble())
  records %>%
    map_dfr(function(r) {
      tibble(
        id = r$id %||% NA_integer_,
        numero_canario = r$numeroCanario %||% NA_character_,
        cif = r$cif %||% NA_character_,
        denominacion = r$denominacion %||% NA_character_,
        ambito = r$ambito %||% NA_character_,
        seccion = r$secciones %||% NA_character_,
        actividad = r$actividad %||% NA_character_,
        direccion = r$direccion %||% NA_character_,
        provincia = r$provincia %||% NA_character_,
        isla = r$isla %||% NA_character_,
        municipio = r$municipio %||% NA_character_,
        codigo_postal = r$codigoPostal %||% NA_character_,
        web = r$dominioInternet %||% NA_character_,
        email = r$email %||% NA_character_,
        telefono = r$telefono %||% NA_character_,
        fecha_constitucion = r$fechaConstitucion %||% NA_character_,
        vigente = isTRUE(r$vigente),
        mandato_actual = r$mandatoactual %||% NA_integer_,
        fecha_baja = r$fechabaja %||% NA_character_
      )
    })
}

apply_filtros <- function(data, filtros) {
  if (is.null(filtros)) return(data)
  out <- data
  if (isTRUE(filtros$solo_con_web)) {
    out <- out %>%
      filter(!is.na(web), nzchar(web)) %>%
      filter(str_detect(web, "(?i)\\.[a-z]{2,}") | str_detect(web, "(?i)https?://"))
  }
  if (isTRUE(filtros$solo_vigentes)) {
    out <- out %>% filter(isTRUE(vigente) | vigente == TRUE)
  }
  if (length(filtros$islas)) {
    out <- out %>% filter(toupper(isla) %in% toupper(filtros$islas))
  }
  if (length(filtros$actividades)) {
    out <- out %>% filter(toupper(actividad) %in% toupper(filtros$actividades))
  }
  if (length(filtros$ambitos)) {
    out <- out %>% filter(toupper(ambito) %in% toupper(filtros$ambitos))
  }
  out
}

is_probably_url <- function(x) {
  if (is.na(x)) return(FALSE)
  str_detect(x, "(?i)^(https?://|www\\.)") ||
    str_detect(x, "(?i)\\.(es|com|org|net|info|cat|eu|gal)(/|$)")
}

clean_web <- function(web) {
  if (is.na(web) || !nzchar(web)) return(NA_character_)
  cleaned <- str_trim(web)
  if (!is_probably_url(cleaned)) return(NA_character_)
  if (!str_detect(cleaned, "(?i)^https?://")) cleaned <- paste0("https://", cleaned)
  cleaned <- str_replace(cleaned, "/+$", "")
  cleaned
}
