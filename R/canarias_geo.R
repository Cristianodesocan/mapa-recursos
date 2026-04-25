## Funciones de referencia geografica para Canarias.
## Carga las tablas en data/ y ofrece lookups deterministas
## municipio -> isla, codigo_postal -> isla, isla -> bbox y validacion de
## coordenadas dentro del bbox de la isla esperada.

suppressPackageStartupMessages({
  library(readr)
  library(stringr)
  library(stringi)
  library(dplyr)
})

CANARIAS_DATA_DIR <- function(root) {
  file.path(root, "data")
}

normalize_key <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_character_)
  out <- stringi::stri_trans_general(x, "Latin-ASCII")
  out <- str_to_upper(out)
  out <- str_replace_all(out, "[^A-Z0-9 ]", " ")
  out <- str_replace_all(out, "\\s+", " ")
  out <- str_trim(out)
  out[!nzchar(out)] <- NA_character_
  out
}

load_canarias_islas <- function(root) {
  read_csv(file.path(CANARIAS_DATA_DIR(root), "canarias_islas.csv"),
           show_col_types = FALSE) %>%
    mutate(isla_key = normalize_key(isla))
}

load_canarias_municipios <- function(root) {
  read_csv(file.path(CANARIAS_DATA_DIR(root), "canarias_municipios.csv"),
           show_col_types = FALSE) %>%
    mutate(
      municipio_key = normalize_key(municipio),
      isla_key = normalize_key(isla)
    )
}

load_canarias_cp_rangos <- function(root) {
  read_csv(file.path(CANARIAS_DATA_DIR(root), "canarias_cp_rangos.csv"),
           show_col_types = FALSE,
           col_types = cols(
             cp_min = col_integer(), cp_max = col_integer(),
             isla = col_character(), provincia = col_character()
           ))
}

extract_cp <- function(text) {
  if (is.null(text) || length(text) == 0 || is.na(text)) return(NA_character_)
  m <- str_extract(text, "\\b(35|38)\\d{3}\\b")
  if (is.na(m)) return(NA_character_)
  m
}

cp_to_isla <- function(cp, cp_table) {
  if (is.na(cp)) return(list(isla = NA_character_, provincia = NA_character_))
  n <- suppressWarnings(as.integer(cp))
  if (is.na(n)) return(list(isla = NA_character_, provincia = NA_character_))
  hit <- cp_table[cp_table$cp_min <= n & cp_table$cp_max >= n, ]
  if (!nrow(hit)) {
    if (n >= 35000 && n <= 35999) {
      return(list(isla = NA_character_, provincia = "Las Palmas"))
    }
    if (n >= 38000 && n <= 38999) {
      return(list(isla = NA_character_, provincia = "Santa Cruz de Tenerife"))
    }
    return(list(isla = NA_character_, provincia = NA_character_))
  }
  list(isla = hit$isla[[1]], provincia = hit$provincia[[1]])
}

municipio_to_isla <- function(municipio, mun_table) {
  key <- normalize_key(municipio)
  if (is.na(key)) return(list(isla = NA_character_, provincia = NA_character_,
                              municipio_canonico = NA_character_))
  hit <- mun_table[mun_table$municipio_key == key, ]
  if (!nrow(hit)) {
    candidates <- mun_table[str_detect(mun_table$municipio_key,
                                       fixed(key)), ]
    if (!nrow(candidates)) {
      candidates <- mun_table[vapply(mun_table$municipio_key,
                                     function(mk) str_detect(key, fixed(mk)),
                                     logical(1)), ]
    }
    if (nrow(candidates) == 1) hit <- candidates
  }
  if (!nrow(hit)) {
    return(list(isla = NA_character_, provincia = NA_character_,
                municipio_canonico = NA_character_))
  }
  list(isla = hit$isla[[1]], provincia = hit$provincia[[1]],
       municipio_canonico = hit$municipio[[1]])
}

isla_to_bbox <- function(isla, islas_table) {
  key <- normalize_key(isla)
  if (is.na(key)) return(NULL)
  hit <- islas_table[islas_table$isla_key == key, ]
  if (!nrow(hit)) return(NULL)
  list(
    isla = hit$isla[[1]],
    provincia = hit$provincia[[1]],
    min_lat = hit$bbox_min_lat[[1]], max_lat = hit$bbox_max_lat[[1]],
    min_lon = hit$bbox_min_lon[[1]], max_lon = hit$bbox_max_lon[[1]]
  )
}

coords_inside_bbox <- function(lat, lon, bbox) {
  if (is.null(bbox)) return(NA)
  if (is.na(lat) || is.na(lon)) return(NA)
  lat >= bbox$min_lat && lat <= bbox$max_lat &&
    lon >= bbox$min_lon && lon <= bbox$max_lon
}

resolve_canarias_geo <- function(municipio, isla, codigo_postal, direccion,
                                 mun_table, cp_table, islas_table) {
  cp_extracted <- if (!is.null(codigo_postal) && !is.na(codigo_postal) &&
                      nzchar(codigo_postal)) {
    str_extract(codigo_postal, "\\b\\d{5}\\b") %||% extract_cp(direccion)
  } else {
    extract_cp(direccion)
  }

  mun_lookup <- municipio_to_isla(municipio, mun_table)
  cp_lookup <- cp_to_isla(cp_extracted, cp_table)

  isla_clean <- normalize_key(isla)
  if (is.na(isla_clean)) {
    isla_clean <- if (!is.na(mun_lookup$isla)) mun_lookup$isla
                  else if (!is.na(cp_lookup$isla)) cp_lookup$isla
                  else NA_character_
  } else {
    if (!is.na(mun_lookup$isla) && mun_lookup$isla != isla_clean) {
      isla_clean <- mun_lookup$isla
    }
  }

  provincia <- mun_lookup$provincia %||% cp_lookup$provincia
  if (is.na(provincia) && !is.na(isla_clean)) {
    bbox <- isla_to_bbox(isla_clean, islas_table)
    if (!is.null(bbox)) provincia <- bbox$provincia
  }

  list(
    isla = isla_clean,
    provincia = provincia,
    municipio_canonico = mun_lookup$municipio_canonico,
    codigo_postal = cp_extracted
  )
}
