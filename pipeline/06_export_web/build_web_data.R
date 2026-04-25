## Genera el JSON estatico que consume web/index.html
## Lee el dataset canonico (04_analisis), filtra coords validas dentro del
## bbox de Canarias y escribe web/data/recursos.json minificado.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(jsonlite)
  library(stringr)
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

CANARIAS_BBOX <- list(min_lat = 27.3, max_lat = 29.6, min_lon = -18.5, max_lon = -13.0)

## Columnas que consume el JS de la web
WEB_COLS <- c(
  "id_recurso", "entidad", "categoria_principal", "subcategoria", "area",
  "tipo_entidad", "isla", "municipio", "codigo_postal", "direccion",
  "telefono", "email", "web", "horario", "plazas", "descripcion",
  "lat", "lon", "geocode_status", "match_level", "validated_in_island"
)

main <- function() {
  args <- parse_cli_args()
  root <- normalizePath(args$root %||% default_root_from_stage_script(),
                        winslash = "/", mustWork = TRUE)

  input_path <- file.path(root, "salidas", "04_analisis", "recursos_canonicos.csv")
  if (!file.exists(input_path)) {
    log_abort("No existe el dataset canonico: {input_path}. Ejecuta antes la fase analisis.")
  }

  data <- read_csv(input_path, show_col_types = FALSE, guess_max = 50000)

  for (col in setdiff(WEB_COLS, names(data))) data[[col]] <- NA

  before <- nrow(data)
  data <- data %>%
    filter(geocode_status == "ok",
           !is.na(lat), !is.na(lon),
           lat >= CANARIAS_BBOX$min_lat, lat <= CANARIAS_BBOX$max_lat,
           lon >= CANARIAS_BBOX$min_lon, lon <= CANARIAS_BBOX$max_lon) %>%
    select(all_of(WEB_COLS))

  output_dir <- file.path(root, "web", "data")
  ensure_dir(output_dir)
  output_path <- file.path(output_dir, "recursos.json")

  ## Convertir NA -> null y serializar minificado
  json <- toJSON(data, na = "null", null = "null", auto_unbox = TRUE,
                 dataframe = "rows", pretty = FALSE)
  writeLines(json, output_path, useBytes = TRUE)

  size_mb <- round(file.info(output_path)$size / (1024 * 1024), 2)
  log_info("Web data exportada: {nrow(data)}/{before} recursos -> {output_path} ({size_mb} MB)")
}

if (sys.nframe() == 0) main()
