suppressPackageStartupMessages({
  library(pdftools)
  library(dplyr)
  library(stringr)
  library(purrr)
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
source(file.path(dirname(dirname(dirname(stage_script_path))), "R", "utils_pdf.R"))

ISLAS <- c(
  "TENERIFE",
  "LA GOMERA",
  "GRAN CANARIA",
  "LA PALMA",
  "LANZAROTE",
  "FUERTEVENTURA",
  "EL HIERRO"
)

is_island_header <- function(line) {
  normalize_text(line) %in% ISLAS
}

is_table_header <- function(line) {
  line <- normalize_text(line)
  header_patterns <- c(
    "^centros de atencion residencial$",
    "^centros de atención residencial$",
    "^municipio y$",
    "^direccion$",
    "^dirección$",
    "^nombre del centro$",
    "^telefono / email$",
    "^teléfono / email$",
    "^nº plazas/$",
    "^centro de dia$",
    "^centro de día$",
    "^guia de recursos",
    "^las residencias para personas mayores"
  )
  any(str_detect(str_to_lower(line), header_patterns))
}

extract_main_record <- function(line) {
  line <- str_trim(line)
  plazas_match <- str_extract(
    line,
    "\\d+\\s*PLAZAS(?:\\s*/\\s*CENTRO DE D[IÍ]A|/CENTRO DE D[IÍ]A)?$"
  )
  if (is.na(plazas_match)) {
    return(NULL)
  }

  trimmed <- str_trim(substr(line, 1, nchar(line) - nchar(plazas_match)))
  phone_match <- str_extract(trimmed, "(?:\\+34\\s*)?(?:\\d[\\d .\\-/]{6,}\\d)$")
  if (!is.na(phone_match)) {
    trimmed <- str_trim(substr(trimmed, 1, nchar(trimmed) - nchar(phone_match)))
  }

  parts <- split_on_gaps(trimmed)
  if (length(parts) < 2) {
    return(NULL)
  }

  list(
    municipio = parts[[1]],
    entidad = paste(parts[-1], collapse = " "),
    telefono_raw = empty_to_na(phone_match),
    plazas_raw = empty_to_na(plazas_match)
  )
}

parse_residencias_pdf <- function(pdf_path) {
  page_text <- tryCatch(
    pdf_text(pdf_path),
    error = function(e) {
      log_abort("Fallo al leer texto del PDF de residencias {basename(pdf_path)}: {conditionMessage(e)}")
    }
  )
  current_island <- NA_character_
  records <- list()
  record_idx <- 1L

  for (page_number in seq_along(page_text)) {
    raw_lines <- strsplit(page_text[[page_number]], "\n", fixed = TRUE)[[1]]
    lines <- raw_lines %>%
      str_trim() %>%
      keep(~ nzchar(.x))

    if (!length(lines)) {
      next
    }

    normalized_lines <- vapply(lines, normalize_text, character(1))
    only_islands <- normalized_lines[normalized_lines %in% ISLAS]
    if (length(only_islands) == 1 && length(lines) <= 3) {
      current_island <- only_islands[[1]]
      next
    }

    i <- 1L
    while (i <= length(lines)) {
      line <- lines[[i]]

      if (is_island_header(line)) {
        current_island <- line
        i <- i + 1L
        next
      }

      if (is_table_header(line)) {
        i <- i + 1L
        next
      }

      main_record <- extract_main_record(line)
      if (is.null(main_record)) {
        i <- i + 1L
        next
      }

      address_parts <- character()
      email_parts <- character()
      j <- i + 1L

      while (j <= length(lines)) {
        detail_line <- lines[[j]]
        if (is_island_header(detail_line) || is_table_header(detail_line) || !is.null(extract_main_record(detail_line))) {
          break
        }

        detail_parts <- split_on_gaps(detail_line)
        if (length(detail_parts) >= 2 && !is.na(extract_emails_text(detail_parts[[length(detail_parts)]]))) {
          email_parts <- c(email_parts, extract_emails_text(detail_parts[[length(detail_parts)]]))
          address_candidate <- paste(detail_parts[-length(detail_parts)], collapse = " ")
          if (nzchar(address_candidate)) {
            address_parts <- c(address_parts, address_candidate)
          }
        } else if (!is.na(extract_emails_text(detail_line))) {
          email_parts <- c(email_parts, extract_emails_text(detail_line))
        } else {
          address_parts <- c(address_parts, detail_line)
        }

        j <- j + 1L
      }

      records[[record_idx]] <- tibble(
        fuente_archivo = basename(pdf_path),
        pagina = page_number,
        isla = empty_to_na(current_island),
        municipio = empty_to_na(main_record$municipio),
        direccion = empty_to_na(paste(address_parts, collapse = " ")),
        entidad = empty_to_na(main_record$entidad),
        telefono_raw = empty_to_na(main_record$telefono_raw),
        email_raw = collapse_unique(email_parts),
        plazas_raw = empty_to_na(main_record$plazas_raw),
        categoria_principal = "Personas mayores",
        subcategoria = "Centro de atencion residencial"
      )
      record_idx <- record_idx + 1L
      i <- j
    }
  }

  if (!length(records)) {
    return(tibble(
      fuente_archivo = character(),
      pagina = integer(),
      isla = character(),
      municipio = character(),
      direccion = character(),
      entidad = character(),
      telefono_raw = character(),
      email_raw = character(),
      plazas_raw = character(),
      categoria_principal = character(),
      subcategoria = character()
    ))
  }

  bind_rows(records) %>%
    mutate(
      direccion = str_replace_all(direccion, "\\s+,", ","),
      direccion = empty_to_na(direccion)
    )
}

main <- function() {
  args <- parse_cli_args()
  root <- normalizePath(args$root %||% default_root_from_stage_script(), winslash = "/", mustWork = TRUE)
  output_dir <- file.path(root, "salidas", "01_extraccion")
  ensure_dir(output_dir)

  pdf_path <- file.path(root, "fuentes", "TF-2a_GUIA_DE_RECURSOS-Centros de Atención Residencial (1).pdf")
  output_path <- file.path(output_dir, "residencias_raw.csv")

  if (!file.exists(pdf_path)) {
    log_abort("No existe el PDF de residencias: {pdf_path}")
  }

  data <- parse_residencias_pdf(pdf_path)
  safe_write_csv(data, output_path)
  log_info("Residencias extraidas: {nrow(data)} registros -> {output_path}")
}

if (sys.nframe() == 0) {
  main()
}
