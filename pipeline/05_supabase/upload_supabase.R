## Sube el dataset canonico a Supabase via PostgREST.
## - Lee variables de entorno: SUPABASE_URL, SUPABASE_SERVICE_ROLE_KEY,
##   SUPABASE_SCHEMA (default "recursos"), SUPABASE_TABLE (default "entidades").
## - Upsert por id_recurso en batches; idempotente (re-correr no duplica).
## - Tolera ausencia de credenciales si --skip-supabase o --dry-run.
##
## Uso:
##   Rscript pipeline/05_supabase/upload_supabase.R --root /path/al/proyecto
##                                                     [--input recursos_canonicos.csv]
##                                                     [--batch-size 500]
##                                                     [--dry-run]

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(httr2)
  library(jsonlite)
  library(stringr)
  library(tibble)
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

UPLOAD_COLS <- c(
  "id_recurso", "fuente_tipo", "tipo_entidad",
  "fuente_archivo", "fuente_url",
  "identificador_fuente", "pagina",
  "categoria_principal", "subcategoria", "area", "ambito",
  "comunidad_autonoma", "provincia", "isla", "municipio", "codigo_postal",
  "entidad", "cif", "descripcion", "direccion",
  "telefono", "email", "web", "horario",
  "plazas", "recurso_igualdad", "vigente",
  "estado_registro",
  "lat", "lon",
  "geocode_status", "geocode_source", "match_level", "validated_in_island"
)

load_dotenv <- function(root) {
  env_path <- file.path(root, ".env")
  if (!file.exists(env_path)) return(invisible())
  lines <- readLines(env_path, warn = FALSE)
  for (line in lines) {
    line <- str_trim(line)
    if (!nzchar(line) || startsWith(line, "#")) next
    parts <- str_split_fixed(line, "=", 2)
    if (nchar(parts[1, 1]) && nchar(parts[1, 2])) {
      key <- str_trim(parts[1, 1])
      value <- str_trim(parts[1, 2])
      value <- str_replace(value, "^['\"](.+)['\"]$", "\\1")
      if (Sys.getenv(key, unset = "") == "") {
        do.call(Sys.setenv, setNames(list(value), key))
      }
    }
  }
}

normalize_for_json <- function(df) {
  for (col in names(df)) {
    if (is.character(df[[col]])) {
      df[[col]] <- ifelse(is.na(df[[col]]) | !nzchar(df[[col]]),
                          NA, df[[col]])
    }
  }
  df
}

upsert_batch <- function(rows, supabase_url, service_key, schema, table,
                         attempts = 4) {
  endpoint <- sprintf("%s/rest/v1/%s",
                      sub("/$", "", supabase_url), table)

  payload <- toJSON(normalize_for_json(rows), na = "null", null = "null",
                    auto_unbox = TRUE, dataframe = "rows")

  req <- request(endpoint) %>%
    req_headers(
      apikey = service_key,
      Authorization = paste("Bearer", service_key),
      `Content-Profile` = schema,
      `Content-Type` = "application/json",
      Prefer = "resolution=merge-duplicates,return=minimal"
    ) %>%
    req_body_raw(payload, type = "application/json") %>%
    req_timeout(60) %>%
    req_error(is_error = function(resp) FALSE)

  for (attempt in seq_len(attempts)) {
    response <- tryCatch(req_perform(req), error = function(e) e)
    if (inherits(response, "error")) {
      log_warn("Error de red en batch (intento {attempt}): {conditionMessage(response)}")
      if (attempt < attempts) Sys.sleep(2 ^ attempt)
      next
    }
    sc <- resp_status(response)
    if (sc >= 200 && sc < 300) {
      return(list(status = "ok", http_status = sc, rows = nrow(rows)))
    }
    if (sc == 429 || (sc >= 500 && sc < 600)) {
      log_warn("Supabase respuesta {sc} en batch (intento {attempt}); reintentando.")
      if (attempt < attempts) Sys.sleep(2 ^ attempt)
      next
    }
    body <- tryCatch(resp_body_string(response), error = function(e) "")
    body_safe <- gsub("\\{", "{{", gsub("\\}", "}}", substr(body, 1, 500)))
    log_warn("Supabase rechazo batch ({sc}): {body_safe}")
    return(list(status = "client_error", http_status = sc, body = body))
  }
  list(status = "exhausted", http_status = NA_integer_)
}

main <- function() {
  args <- parse_cli_args()
  root <- normalizePath(args$root %||% default_root_from_stage_script(),
                        winslash = "/", mustWork = TRUE)

  if (isTRUE(args$`skip-supabase`)) {
    log_info("--skip-supabase activo; omitiendo carga.")
    return(invisible(NULL))
  }

  load_dotenv(root)

  supabase_url <- args$`supabase-url` %||% Sys.getenv("SUPABASE_URL", "")
  service_key  <- args$`service-key`  %||% Sys.getenv("SUPABASE_SERVICE_ROLE_KEY", "")
  schema       <- args$`schema`       %||% Sys.getenv("SUPABASE_SCHEMA",
                                                       unset = "recursos")
  table        <- args$`table`        %||% Sys.getenv("SUPABASE_TABLE",
                                                       unset = "entidades")
  batch_size   <- as.integer(args$`batch-size` %||% 500)
  dry_run      <- isTRUE(args$`dry-run`)

  if (!dry_run) {
    if (!nzchar(supabase_url) || !nzchar(service_key)) {
      log_abort("Faltan SUPABASE_URL y/o SUPABASE_SERVICE_ROLE_KEY (env o flags). Usa --dry-run para validar payload sin subir.")
    }
  }

  input_default <- file.path(root, "salidas", "04_analisis",
                             "recursos_canonicos.csv")
  input_path <- args$input %||% input_default
  if (!file.exists(input_path)) {
    log_abort("No existe el dataset a subir: {input_path}")
  }

  data <- read_csv(input_path, show_col_types = FALSE,
                   guess_max = 50000)

  missing_cols <- setdiff(UPLOAD_COLS, names(data))
  for (col in missing_cols) data[[col]] <- NA

  payload <- data %>% select(all_of(UPLOAD_COLS))

  log_info("Preparado payload: {nrow(payload)} filas, {ncol(payload)} columnas")
  log_info("Destino: {supabase_url} schema={schema} table={table} batch={batch_size}")

  if (dry_run) {
    sample_path <- file.path(root, "salidas", "05_supabase",
                             "supabase_payload_sample.json")
    ensure_dir(dirname(sample_path))
    sample <- payload[seq_len(min(3, nrow(payload))), ]
    writeLines(toJSON(normalize_for_json(sample), na = "null", null = "null",
                      auto_unbox = TRUE, dataframe = "rows", pretty = TRUE),
               sample_path)
    log_info("--dry-run: muestra de payload escrita en {sample_path}")
    return(invisible(NULL))
  }

  total <- nrow(payload)
  if (total == 0) {
    log_warn("Dataset vacio; nada que subir.")
    return(invisible(NULL))
  }

  n_batches <- ceiling(total / batch_size)
  log_info("Subiendo {total} filas en {n_batches} batches...")

  ok_total <- 0L
  failed_batches <- 0L
  for (b in seq_len(n_batches)) {
    from <- (b - 1L) * batch_size + 1L
    to <- min(b * batch_size, total)
    batch <- payload[from:to, ]
    res <- upsert_batch(batch, supabase_url, service_key, schema, table)
    if (res$status == "ok") {
      ok_total <- ok_total + nrow(batch)
      log_info("Batch {b}/{n_batches}: OK ({nrow(batch)} filas)")
    } else {
      failed_batches <- failed_batches + 1L
      log_warn("Batch {b}/{n_batches}: FALLO ({res$status}, http={res$http_status %||% 'NA'})")
    }
  }

  log_info("Carga completada: {ok_total}/{total} filas subidas; {failed_batches} batches fallidos.")
  if (failed_batches > 0) {
    log_abort("{failed_batches} batches fallaron. Revisa logs y reintenta (upsert es idempotente).")
  }
}

if (sys.nframe() == 0) {
  main()
}
