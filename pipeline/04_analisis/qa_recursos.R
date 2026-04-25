suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(stringi)
  library(jsonlite)
  library(tibble)
  library(tidyr)
  library(writexl)
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

make_dedupe_key <- function(entidad, direccion, municipio) {
  parts <- c(entidad, direccion, municipio)
  parts[is.na(parts)] <- ""
  parts <- str_to_lower(parts)
  parts <- stringi::stri_trans_general(parts, "Latin-ASCII")
  parts <- str_replace_all(parts, "[[:punct:]]+", " ")
  parts <- str_replace_all(parts, "\\s+", " ")
  parts <- str_trim(parts)
  paste(parts, collapse = "|")
}

first_non_na <- function(x) {
  nn <- x[!is.na(x)]
  if (length(nn)) nn[[1]] else x[[1]]
}

build_canonical <- function(data) {
  library(purrr)
  data %>%
    mutate(dedupe_key = pmap_chr(list(entidad, direccion, municipio),
                                 ~ make_dedupe_key(..1, ..2, ..3))) %>%
    group_by(dedupe_key) %>%
    arrange(desc(!is.na(lat)), desc(!is.na(telefono)), desc(!is.na(email)),
            .by_group = TRUE) %>%
    summarise(
      across(everything(), first_non_na),
      n_fuentes = n(),
      .groups = "drop"
    ) %>%
    select(-dedupe_key)
}

main <- function() {
  args <- parse_cli_args()
  root <- normalizePath(args$root %||% default_root_from_stage_script(), winslash = "/", mustWork = TRUE)

  geocoded_path <- file.path(root, "salidas", "03_geocodificacion", "recursos_geocodificados.csv")
  normalized_path <- file.path(root, "salidas", "02_transformacion", "recursos_normalizados.csv")
  input_path <- if (file.exists(geocoded_path)) geocoded_path else normalized_path

  output_dir <- file.path(root, "salidas", "04_analisis")
  ensure_dir(output_dir)

  if (!file.exists(input_path)) {
    log_abort("No existe dataset para QA en 02_transformacion ni en 03_geocodificacion.")
  }

  data <- read_csv(input_path, show_col_types = FALSE)

  summary_by_group <- data %>%
    mutate(
      has_direccion = !is.na(direccion),
      has_contacto = !is.na(telefono) | !is.na(email),
      geocodificado = !is.na(lat) & !is.na(lon)
    ) %>%
    group_by(fuente_archivo, categoria_principal) %>%
    summarise(
      total_registros = n(),
      pct_con_direccion = round(mean(has_direccion) * 100, 2),
      pct_con_contacto = round(mean(has_contacto) * 100, 2),
      pct_geocodificado = round(mean(geocodificado) * 100, 2),
      sin_coordenadas = sum(!geocodificado),
      .groups = "drop"
    )

  match_level_summary <- if ("match_level" %in% names(data)) {
    data %>%
      filter(!is.na(match_level)) %>%
      count(match_level, name = "n") %>%
      arrange(desc(n))
  } else {
    tibble(match_level = character(), n = integer())
  }

  duplicates_groups <- data %>%
    filter(!is.na(entidad), !is.na(direccion)) %>%
    group_by(entidad, direccion) %>%
    summarise(
      duplicados = n(),
      ids = paste(id_recurso, collapse = "; "),
      id_recurso = first(id_recurso),
      .groups = "drop"
    ) %>%
    filter(duplicados > 1) %>%
    mutate(tipo_incidencia = "duplicado_potencial")

  incidencias <- bind_rows(
    data %>%
      filter(is.na(direccion)) %>%
      transmute(id_recurso, entidad, direccion, tipo_incidencia = "sin_direccion",
                ids_grupo = NA_character_),
    data %>%
      filter(is.na(telefono) & is.na(email)) %>%
      transmute(id_recurso, entidad, direccion, tipo_incidencia = "sin_contacto",
                ids_grupo = NA_character_),
    data %>%
      filter(is.na(lat) | is.na(lon)) %>%
      transmute(id_recurso, entidad, direccion, tipo_incidencia = "sin_coordenadas",
                ids_grupo = NA_character_),
    duplicates_groups %>%
      transmute(id_recurso, entidad, direccion, tipo_incidencia,
                ids_grupo = ids)
  ) %>%
    distinct()

  duplicates <- duplicates_groups %>%
    select(entidad, direccion, duplicados, tipo_incidencia)

  por_municipio <- data %>%
    filter(!is.na(municipio)) %>%
    count(isla, municipio, categoria_principal, name = "n_recursos") %>%
    arrange(isla, municipio, categoria_principal)

  por_categoria <- data %>%
    count(categoria_principal, subcategoria, name = "n_recursos") %>%
    arrange(categoria_principal, desc(n_recursos))

  canonical <- build_canonical(data)

  directorio_cols <- c(
    "categoria_principal", "subcategoria", "area", "ambito",
    "entidad", "cif", "municipio", "isla", "codigo_postal", "direccion",
    "telefono", "email", "web", "horario",
    "plazas", "descripcion", "vigente", "estado_registro",
    "lat", "lon", "match_level",
    "fuente_tipo", "fuente_archivo", "id_recurso"
  )
  directorio_cols <- intersect(directorio_cols, names(canonical))
  directorio <- canonical %>% select(all_of(directorio_cols))

  islas <- sort(unique(na.omit(directorio$isla)))
  directorio_sheets <- c(
    list(`Todos los recursos` = directorio %>% arrange(isla, municipio, entidad)),
    setNames(
      lapply(islas, function(i) directorio %>%
               filter(isla == i) %>%
               arrange(municipio, categoria_principal, entidad)),
      substr(islas, 1, 31)
    ),
    list(`Por categoria` = por_categoria, `Por municipio` = por_municipio)
  )

  csv_summary_path <- file.path(output_dir, "resumen_calidad.csv")
  csv_incidencias_path <- file.path(output_dir, "incidencias_registros.csv")
  json_summary_path <- file.path(output_dir, "resumen_calidad.json")
  csv_municipio_path <- file.path(output_dir, "recursos_por_municipio.csv")
  csv_categoria_path <- file.path(output_dir, "recursos_por_categoria.csv")
  csv_canonical_path <- file.path(output_dir, "recursos_canonicos.csv")
  xlsx_directorio_path <- file.path(output_dir, "directorio_recursos.xlsx")

  safe_write_csv(summary_by_group, csv_summary_path)
  safe_write_csv(incidencias, csv_incidencias_path)
  safe_write_csv(por_municipio, csv_municipio_path)
  safe_write_csv(por_categoria, csv_categoria_path)
  safe_write_csv(canonical, csv_canonical_path)
  writexl::write_xlsx(directorio_sheets, path = xlsx_directorio_path)

  payload <- list(
    generado_en = as.character(Sys.time()),
    resumen_por_grupo = summary_by_group,
    match_level = match_level_summary,
    totales = list(
      total_registros = nrow(data),
      total_canonicos = nrow(canonical),
      total_duplicados_potenciales = nrow(duplicates),
      total_descartados_mapa = sum(is.na(data$lat) | is.na(data$lon)),
      total_geocodificados = sum(!is.na(data$lat) & !is.na(data$lon))
    )
  )
  writeLines(
    jsonlite::toJSON(payload, pretty = TRUE, auto_unbox = TRUE, na = "null"),
    json_summary_path, useBytes = TRUE
  )

  log_info("QA + directorio generados en {output_dir}")
}

if (sys.nframe() == 0) {
  main()
}
