suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(stringi)
  library(readr)
  library(tibble)
  library(tidyr)
  library(purrr)
  library(writexl)
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
source(file.path(dirname(dirname(dirname(stage_script_path))), "R", "canarias_geo.R"))
source(file.path(dirname(stage_script_path), "categoria_tematica.R"))

infer_isla <- function(isla, direccion) {
  isla <- empty_to_na(isla)
  if (!is.na(isla)) {
    return(isla)
  }

  direccion <- str_to_upper(direccion %||% "")
  patterns <- c(
    "LAS PALMAS DE GRAN CANARIA" = "GRAN CANARIA",
    "GRAN CANARIA" = "GRAN CANARIA",
    "SANTA CRUZ DE TENERIFE" = "TENERIFE",
    "TENERIFE" = "TENERIFE",
    "LANZAROTE" = "LANZAROTE",
    "FUERTEVENTURA" = "FUERTEVENTURA",
    "LA PALMA" = "LA PALMA",
    "LA GOMERA" = "LA GOMERA",
    "EL HIERRO" = "EL HIERRO"
  )
  for (pattern in names(patterns)) {
    if (str_detect(direccion, fixed(pattern))) {
      return(patterns[[pattern]])
    }
  }
  NA_character_
}

infer_municipio <- function(municipio, direccion) {
  municipio <- empty_to_na(municipio)
  if (!is.na(municipio)) {
    return(municipio)
  }

  direccion <- normalize_text(direccion %||% "")
  if (!nzchar(direccion)) {
    return(NA_character_)
  }

  candidates <- str_split(direccion, "[,.;]", simplify = FALSE)[[1]] %>%
    normalize_text() %>%
    keep(~ nzchar(.x))
  if (!length(candidates)) {
    return(NA_character_)
  }

  candidates <- rev(candidates)
  for (candidate in candidates) {
    if (
      !str_detect(candidate, regex("^(C/|CALLE|AVDA|AVENIDA|CAMINO|PLAZA|CTRA|CARRETERA|URBANIZACION|URBANIZACIÓN)", ignore_case = TRUE)) &&
      !str_detect(candidate, "^\\d") &&
      nchar(candidate) >= 3 &&
      nchar(candidate) <= 60
    ) {
      return(candidate)
    }
  }
  NA_character_
}

normalize_phone_field <- function(text) {
  collapse_unique(str_split(extract_phones_text(text), "; ", simplify = TRUE))
}

normalize_email_field <- function(text) {
  collapse_unique(str_split(extract_emails_text(text), "; ", simplify = TRUE))
}

clean_direccion <- function(direccion) {
  if (is.na(direccion) || !nzchar(direccion)) return(NA_character_)
  d <- normalize_text(direccion)
  d <- str_replace(d, "^/\\s*", "C/ ")
  d <- str_replace(d, "(?i)^C/(?=[A-Z\u00C0-\u017F])", "C/ ")
  d <- str_replace(d, "(?i)^AVDA\\.?(?=[A-Z\u00C0-\u017F])", "AVDA. ")
  d <- str_replace_all(d, "([A-Z\u00C0-\u017F])(\\d)", "\\1 \\2")
  d <- str_replace_all(d, "\\s+,", ",")
  d <- str_replace_all(d, "\\s+", " ")
  str_trim(d)
}

compute_estado_registro <- function(direccion, telefono, email) {
  has_address <- !is.na(empty_to_na(direccion))
  has_contact <- !is.na(empty_to_na(telefono)) || !is.na(empty_to_na(email))
  if (has_address && has_contact) {
    return("valido")
  }
  if (!has_address) {
    return("sin_direccion")
  }
  if (!has_contact) {
    return("sin_contacto")
  }
  "incompleto"
}

CANONICAL_COLS <- c(
  "fuente_tipo", "fuente_archivo", "fuente_url", "identificador_fuente",
  "pagina", "categoria_principal", "subcategoria", "area",
  "isla", "municipio", "codigo_postal",
  "entidad", "cif", "descripcion", "direccion",
  "telefono_raw", "email_raw", "web", "horario",
  "plazas_raw", "recurso_igualdad", "ambito", "vigente"
)

empty_canonical <- function() {
  tibble(
    fuente_tipo = character(),
    fuente_archivo = character(),
    fuente_url = character(),
    identificador_fuente = character(),
    pagina = integer(),
    categoria_principal = character(),
    subcategoria = character(),
    area = character(),
    isla = character(),
    municipio = character(),
    codigo_postal = character(),
    entidad = character(),
    cif = character(),
    descripcion = character(),
    direccion = character(),
    telefono_raw = character(),
    email_raw = character(),
    web = character(),
    horario = character(),
    plazas_raw = character(),
    recurso_igualdad = character(),
    ambito = character(),
    vigente = logical()
  )
}

load_residencias <- function(path) {
  if (!file.exists(path)) {
    log_warn("Fuente residencias ausente: {path}")
    return(empty_canonical())
  }
  read_csv(path, show_col_types = FALSE) %>%
    transmute(
      fuente_tipo = "pdf_residencias",
      fuente_archivo = fuente_archivo,
      fuente_url = NA_character_,
      identificador_fuente = as.character(pagina),
      pagina = as.integer(pagina),
      categoria_principal = categoria_principal,
      subcategoria = subcategoria,
      area = NA_character_,
      isla = isla,
      municipio = municipio,
      codigo_postal = NA_character_,
      entidad = entidad,
      cif = NA_character_,
      descripcion = NA_character_,
      direccion = direccion,
      telefono_raw = telefono_raw,
      email_raw = email_raw,
      web = NA_character_,
      horario = NA_character_,
      plazas_raw = plazas_raw,
      recurso_igualdad = NA_character_,
      ambito = NA_character_,
      vigente = NA
    )
}

load_vg_discapacidad <- function(path) {
  if (!file.exists(path)) {
    log_warn("Fuente VG/discapacidad ausente: {path}")
    return(empty_canonical())
  }
  read_csv(path, show_col_types = FALSE) %>%
    transmute(
      fuente_tipo = "pdf_vg_discapacidad",
      fuente_archivo = fuente_archivo,
      fuente_url = NA_character_,
      identificador_fuente = as.character(pagina),
      pagina = as.integer(pagina),
      categoria_principal = categoria_principal,
      subcategoria = subcategoria,
      area = area,
      isla = NA_character_,
      municipio = NA_character_,
      codigo_postal = NA_character_,
      entidad = entidad,
      cif = NA_character_,
      descripcion = descripcion,
      direccion = direccion,
      telefono_raw = telefono_raw,
      email_raw = email_raw,
      web = web,
      horario = horario,
      plazas_raw = NA_character_,
      recurso_igualdad = recurso_igualdad,
      ambito = NA_character_,
      vigente = NA
    )
}

normalize_isla_text <- function(x) {
  cleaned <- str_to_upper(stringi::stri_trans_general(x %||% NA_character_, "Latin-ASCII"))
  cleaned <- str_replace_all(cleaned, "\\s+", " ") %>% str_trim()
  empty_to_na(cleaned)
}

build_asociacion_address <- function(direccion, codigo_postal, municipio, isla) {
  parts <- c(empty_to_na(direccion))
  loc_parts <- c(empty_to_na(codigo_postal), empty_to_na(municipio))
  loc_parts <- loc_parts[!is.na(loc_parts)]
  if (length(loc_parts)) {
    parts <- c(parts, paste(loc_parts, collapse = " "))
  }
  isla_clean <- empty_to_na(isla)
  if (!is.na(isla_clean)) parts <- c(parts, str_to_title(isla_clean))
  parts <- parts[!is.na(parts)]
  if (!length(parts)) return(NA_character_)
  normalize_text(paste(parts, collapse = ", "))
}

load_asociaciones <- function(path) {
  if (!file.exists(path)) {
    log_warn("Fuente asociaciones ausente: {path}")
    return(empty_canonical())
  }
  raw <- read_csv(path, show_col_types = FALSE,
                  col_types = cols(.default = col_character(),
                                   vigente = col_logical()))
  if (!nrow(raw)) return(empty_canonical())
  raw %>%
    transmute(
      fuente_tipo = "registro_asociaciones",
      fuente_archivo = fuente %||% "Registro de Asociaciones de Canarias",
      fuente_url = fuente_url,
      identificador_fuente = coalesce(numero_canario, id),
      pagina = NA_integer_,
      categoria_principal = "Asociaciones",
      subcategoria = empty_to_na(actividad),
      area = empty_to_na(seccion),
      isla = normalize_isla_text(isla),
      municipio = empty_to_na(municipio),
      codigo_postal = empty_to_na(codigo_postal),
      entidad = denominacion,
      cif = empty_to_na(cif),
      descripcion = NA_character_,
      direccion = pmap_chr(
        list(direccion, codigo_postal, municipio, isla),
        ~ build_asociacion_address(..1, ..2, ..3, ..4)
      ),
      telefono_raw = empty_to_na(telefono),
      email_raw = empty_to_na(email),
      web = empty_to_na(web),
      horario = NA_character_,
      plazas_raw = NA_character_,
      recurso_igualdad = NA_character_,
      ambito = empty_to_na(ambito),
      vigente = vigente
    )
}

main <- function() {
  args <- parse_cli_args()
  root <- normalizePath(args$root %||% default_root_from_stage_script(), winslash = "/", mustWork = TRUE)

  extraction_dir <- file.path(root, "salidas", "01_extraccion")
  output_dir <- file.path(root, "salidas", "02_transformacion")
  ensure_dir(output_dir)

  sources <- list(
    residencias = load_residencias(file.path(extraction_dir, "residencias_raw.csv")),
    vg_discapacidad = load_vg_discapacidad(file.path(extraction_dir, "vg_discapacidad_raw.csv")),
    asociaciones = load_asociaciones(file.path(extraction_dir, "asociaciones_registro_filtrado.csv"))
  )
  source_counts <- vapply(sources, nrow, integer(1))
  log_info("Fuentes cargadas: {paste(names(source_counts), source_counts, sep='=', collapse=', ')}")
  if (sum(source_counts) == 0) {
    log_abort("No hay registros disponibles en ninguna fuente de extraccion ({extraction_dir})")
  }

  islas_table <- load_canarias_islas(root)
  mun_table <- load_canarias_municipios(root)
  cp_table <- load_canarias_cp_rangos(root)

  resolve_geo_row <- function(municipio, isla, codigo_postal, direccion) {
    resolve_canarias_geo(municipio, isla, codigo_postal, direccion,
                         mun_table, cp_table, islas_table)
  }

  data <- bind_rows(sources) %>%
    mutate(
      across(c(entidad, descripcion, direccion, web, horario, recurso_igualdad), empty_to_na),
      telefono = vapply(telefono_raw, normalize_phone_field, character(1)),
      email = vapply(email_raw, normalize_email_field, character(1)),
      direccion = vapply(direccion, clean_direccion, character(1)),
      direccion = empty_to_na(direccion),
      plazas = suppressWarnings(as.integer(str_extract(replace_na(plazas_raw, ""), "\\d+")))
    )

  geo_resolved <- pmap(
    list(data$municipio, data$isla, data$codigo_postal, data$direccion),
    resolve_geo_row
  )
  data$isla <- vapply(geo_resolved, function(x) x$isla %||% NA_character_, character(1))
  data$municipio_canonico <- vapply(geo_resolved,
                                    function(x) x$municipio_canonico %||% NA_character_,
                                    character(1))
  data$codigo_postal <- vapply(geo_resolved,
                               function(x) x$codigo_postal %||% NA_character_,
                               character(1))
  data$provincia <- vapply(geo_resolved,
                           function(x) x$provincia %||% NA_character_, character(1))
  data$comunidad_autonoma <- ifelse(!is.na(data$provincia), "Canarias", NA_character_)
  data$municipio <- coalesce(data$municipio_canonico, data$municipio)
  data$municipio <- map2_chr(data$municipio, data$direccion, infer_municipio)

  data$tipo_entidad <- vapply(data$fuente_tipo, infer_tipo_entidad, character(1))

  asoc_mask <- data$fuente_tipo == "registro_asociaciones"
  if (any(asoc_mask)) {
    data$categoria_principal[asoc_mask] <- pmap_chr(
      list(
        data$subcategoria[asoc_mask],
        data$area[asoc_mask],
        data$descripcion[asoc_mask],
        data$entidad[asoc_mask],
        data$subcategoria[asoc_mask]
      ),
      ~ infer_categoria_tematica(..1, ..2, ..3, ..4, ..5)
    )
  }

  data <- data %>%
    mutate(
      estado_registro = pmap_chr(
        list(direccion, telefono, email),
        ~ compute_estado_registro(..1, ..2, ..3)
      ),
      id_recurso = pmap_chr(
        list(fuente_tipo, identificador_fuente, entidad, direccion),
        ~ digest(paste(..1, ..2, ..3, ..4, sep = "||"), algo = "xxhash64")
      ),
      lat = NA_real_,
      lon = NA_real_,
      geocode_status = NA_character_
    ) %>%
    select(
      id_recurso,
      fuente_tipo,
      tipo_entidad,
      fuente_archivo,
      fuente_url,
      identificador_fuente,
      pagina,
      categoria_principal,
      subcategoria,
      area,
      ambito,
      comunidad_autonoma,
      provincia,
      isla,
      municipio,
      codigo_postal,
      entidad,
      cif,
      descripcion,
      direccion,
      telefono,
      email,
      web,
      horario,
      plazas,
      recurso_igualdad,
      vigente,
      estado_registro,
      lat,
      lon,
      geocode_status
    )

  csv_path <- file.path(output_dir, "recursos_normalizados.csv")
  xlsx_path <- file.path(output_dir, "recursos_normalizados.xlsx")

  safe_write_csv(data, csv_path)
  writexl::write_xlsx(list(recursos = data), path = xlsx_path)
  log_info("Dataset normalizado: {nrow(data)} registros -> {csv_path}")

  por_fuente <- data %>% count(fuente_tipo, name = "n_registros")
  log_info("Distribucion por fuente_tipo: {paste(por_fuente$fuente_tipo, por_fuente$n_registros, sep='=', collapse=', ')}")

  huerfanos <- data %>% filter(is.na(isla) & is.na(municipio))
  if (nrow(huerfanos)) {
    huerfanos_path <- file.path(output_dir, "registros_sin_ubicacion.csv")
    safe_write_csv(
      huerfanos %>% select(id_recurso, fuente_tipo, fuente_archivo, pagina, entidad, direccion),
      huerfanos_path
    )
    log_warn("{nrow(huerfanos)} registros sin isla ni municipio inferibles -> {huerfanos_path}")
  }

  sin_isla <- sum(is.na(data$isla))
  if (sin_isla) {
    log_warn("{sin_isla} registros sin isla tras inferencia ({round(100*sin_isla/nrow(data),1)}%).")
  }
}

if (sys.nframe() == 0) {
  main()
}
