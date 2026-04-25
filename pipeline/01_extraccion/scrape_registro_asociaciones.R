#!/usr/bin/env Rscript
## Scraper del Registro de Asociaciones de Canarias
## Lee config YAML, pagina el endpoint publico, cachea por pagina, combina,
## filtra y exporta CSV crudo, CSV filtrado y resumen JSON.
##
## Uso:
##   Rscript pipeline/01_extraccion/scrape_registro_asociaciones.R \
##     --config pipeline/01_extraccion/config/scraper_asociaciones.yml [--max-pages N] [--force]

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(jsonlite)
  library(cli)
  library(purrr)
})

script_path <- normalizePath(sub("^--file=", "",
  commandArgs(trailingOnly = FALSE)[startsWith(commandArgs(trailingOnly = FALSE), "--file=")][[1]]
), winslash = "/")
script_dir <- dirname(script_path)
project_root <- normalizePath(file.path(script_dir, "..", "..", ".."))
source(file.path(project_root, "R", "utils_scraper.R"))

parse_cli <- function(args) {
  out <- list(config = "pipeline/01_extraccion/config/scraper_asociaciones.yml",
              max_pages = NA_integer_, force = FALSE)
  i <- 1
  while (i <= length(args)) {
    a <- args[i]
    if (a == "--config")        { out$config    <- args[i + 1]; i <- i + 2; next }
    if (a == "--max-pages")     { out$max_pages <- as.integer(args[i + 1]); i <- i + 2; next }
    if (a == "--force")         { out$force     <- TRUE; i <- i + 1; next }
    if (a %in% c("-h", "--help")) {
      cat("Uso: Rscript scrape_registro_asociaciones.R [--config PATH] [--max-pages N] [--force]\n")
      quit(status = 0)
    }
    cli::cli_abort("Argumento desconocido: {a}")
  }
  out
}
opt <- parse_cli(commandArgs(trailingOnly = TRUE))

config_path <- resolve_path(project_root, opt$config)
cfg <- load_scraper_config(config_path)

if (isTRUE(opt$force)) cfg$overwrite_cache <- TRUE
if (!is.na(opt$max_pages)) cfg$max_pages <- opt$max_pages

cache_dir <- resolve_path(project_root, cfg$cache_dir %||% "cache/scraper_pages/asociaciones")
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

raw_csv      <- resolve_path(project_root, cfg$output$raw_csv)
filtered_csv <- resolve_path(project_root, cfg$output$filtered_csv)
json_summary <- resolve_path(project_root, cfg$output$json_summary)
walk(list(raw_csv, filtered_csv, json_summary), ensure_parent_dir)

cli::cli_h1("Scraper Registro de Asociaciones")
cli::cli_inform("Endpoint: {cfg$base_url}")
cli::cli_inform("Cache:    {cache_dir} (overwrite={isTRUE(cfg$overwrite_cache)})")

start_page <- as.integer(cfg$start_page %||% 0)
sleep_s    <- as.numeric(cfg$sleep_seconds %||% 1)

## Pagina inicial: descubre el total
first <- get_page(cfg, start_page, cache_dir)
pagination <- first$payload$paginationResponse %||% list()
total_items <- as.integer(pagination$totalItems %||% NA_integer_)
page_size   <- as.integer(pagination$pageSize %||% (cfg$page_size %||% 10))
if (is.na(total_items)) cli::cli_abort("Respuesta sin paginationResponse$totalItems")

total_pages <- ceiling(total_items / page_size)
last_page <- total_pages - 1L
if (!is.null(cfg$max_pages) && !is.na(cfg$max_pages)) {
  last_page <- min(last_page, start_page + as.integer(cfg$max_pages) - 1L)
}

cli::cli_inform("Total items: {total_items} | pageSize: {page_size} | paginas a recuperar: {last_page - start_page + 1L} (de {total_pages})")

all_pages <- vector("list", last_page - start_page + 1L)
all_pages[[1]] <- first$payload

if (last_page > start_page) {
  pages_seq <- seq.int(start_page + 1L, last_page)
  cli::cli_progress_bar("Descargando", total = length(pages_seq))
  for (i in seq_along(pages_seq)) {
    p <- pages_seq[i]
    res <- get_page(cfg, p, cache_dir)
    all_pages[[i + 1L]] <- res$payload
    if (!isTRUE(res$from_cache)) Sys.sleep(sleep_s)
    cli::cli_progress_update()
  }
  cli::cli_progress_done()
}

records <- map_dfr(all_pages, records_from_payload)
cli::cli_inform("Registros recuperados: {nrow(records)}")

records <- records %>%
  mutate(
    web        = vapply(web, clean_web, character(1)),
    email      = ifelse(is.na(email) | !nzchar(email), NA_character_, email),
    telefono   = ifelse(is.na(telefono) | !nzchar(telefono), NA_character_, telefono),
    actividad  = ifelse(is.na(actividad), NA_character_, trimws(actividad)),
    isla       = ifelse(is.na(isla), NA_character_, trimws(isla)),
    municipio  = ifelse(is.na(municipio), NA_character_, trimws(municipio)),
    ambito     = ifelse(is.na(ambito), NA_character_, trimws(ambito)),
    fuente     = "Registro de Asociaciones de Canarias",
    fuente_url = cfg$base_url,
    extraido_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  )

write_csv(records, raw_csv, na = "")
cli::cli_alert_success("Raw CSV: {raw_csv} ({nrow(records)} filas)")

filtered <- apply_filtros(records, cfg$filtros)
write_csv(filtered, filtered_csv, na = "")
cli::cli_alert_success("Filtrado CSV: {filtered_csv} ({nrow(filtered)} filas)")

summary_obj <- list(
  endpoint       = cfg$base_url,
  ejecutado_at   = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
  total_items_api = total_items,
  paginas_recuperadas = length(all_pages),
  registros_raw  = nrow(records),
  registros_filtrados = nrow(filtered),
  con_web         = sum(!is.na(records$web)),
  con_email       = sum(!is.na(records$email)),
  con_telefono    = sum(!is.na(records$telefono)),
  vigentes        = sum(isTRUE(records$vigente) | records$vigente == TRUE, na.rm = TRUE),
  islas           = as.list(table(records$isla, useNA = "ifany")),
  ambitos         = as.list(table(records$ambito, useNA = "ifany")),
  top_actividades = as.list(head(sort(table(records$actividad, useNA = "ifany"), decreasing = TRUE), 20)),
  filtros_aplicados = cfg$filtros
)

writeLines(toJSON(summary_obj, auto_unbox = TRUE, pretty = TRUE, null = "null"), json_summary)
cli::cli_alert_success("Resumen JSON: {json_summary}")
cli::cli_h2("Hecho")
