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

SECTION_START_PATTERNS <- c(
  "^SERVICIO",
  "^CENTRO",
  "^ASOCIACI",
  "^CONSEJER",
  "^UNIDAD",
  "^PUNTO",
  "^OFICINA",
  "^PROGRAMA"
)

starts_new_entity <- function(line) {
  line <- normalize_text(line)
  any(str_detect(line, regex(paste(SECTION_START_PATTERNS, collapse = "|"))))
}

starts_new_label <- function(line) {
  line <- normalize_text(line)
  any(str_detect(
    line,
    regex(
      "^(Resumen del servicio que ofrece|Datos de contacto|Telefono|Tel[eé]fono|Tlf|M[oó]vil|WhatsApp|Contacto|Correo electr[oó]nico|Email|Direcci[oó]n|Ubicaci[oó]n|Sede|Horario|Web|Recurso de igualdad)",
      ignore_case = TRUE
    )
  ))
}

looks_like_entity_continuation <- function(line) {
  line <- normalize_text(line)
  if (!nzchar(line) || starts_new_label(line) || starts_new_entity(line)) {
    return(FALSE)
  }
  if (str_detect(line, regex("^Provincia de", ignore_case = TRUE))) {
    return(FALSE)
  }
  is_uppercaseish(line) && nchar(line) <= 120
}

is_noise_line <- function(line) {
  line <- normalize_text(line)
  noise_patterns <- c(
    "^Provincia de ",
    "^Datos de contacto:?$",
    "^\\.$",
    "^•",
    "^Gran Canaria$",
    "^Tenerife$",
    "^La Palma$",
    "^La Gomera$",
    "^Lanzarote$",
    "^Fuerteventura$",
    "^El Hierro$"
  )
  any(str_detect(line, regex(paste(noise_patterns, collapse = "|"), ignore_case = TRUE)))
}

consume_section_header <- function(lines, i, area) {
  line <- normalize_text(lines$text[[i]])
  next_line <- if (i < nrow(lines)) normalize_text(lines$text[[i + 1L]]) else ""

  if (area == "Discapacidad" && str_detect(line, regex("^Entidades especializadas en atenci[oó]n a personas", ignore_case = TRUE))) {
    combined <- line
    consumed <- 1L
    if (nzchar(next_line) && str_detect(next_line, regex("^con discapacidad", ignore_case = TRUE))) {
      combined <- normalize_text(paste(line, next_line))
      consumed <- 2L
    }
    return(list(value = combined, consumed = consumed))
  }

  if (area == "Violencia machista" && line %in% c("Servicios Insulares", "Servicios Municipales")) {
    return(list(value = line, consumed = 1L))
  }

  NULL
}

append_field_value <- function(record, field, value, sep = " ") {
  record[[field]] <- append_text(record[[field]], value, sep = sep)
  record
}

parse_lines_to_records <- function(lines, area, pdf_name) {
  current <- NULL
  records <- list()
  current_subcategory <- NA_character_
  record_idx <- 1L
  i <- 1L

  while (i <= nrow(lines)) {
    line <- normalize_text(lines$text[[i]])
    page_number <- lines$pagina[[i]]

    section_header <- consume_section_header(lines, i, area)
    if (!is.null(section_header)) {
      current_subcategory <- section_header$value
      i <- i + section_header$consumed
      next
    }

    if (is_noise_line(line)) {
      i <- i + 1L
      next
    }

    if (starts_new_entity(line)) {
      if (
        !is.null(current) &&
        all(is.na(c(
          current$descripcion,
          current$direccion,
          current$telefono_raw,
          current$email_raw,
          current$web,
          current$horario,
          current$recurso_igualdad
        )))
      ) {
        current$entidad <- normalize_text(paste(current$entidad, line))
        i <- i + 1L
        next
      }

      if (!is.null(current)) {
        records[[record_idx]] <- as_tibble(current)
        record_idx <- record_idx + 1L
      }

      entity <- line
      j <- i + 1L
      while (j <= nrow(lines) && looks_like_entity_continuation(lines$text[[j]])) {
        entity <- normalize_text(paste(entity, lines$text[[j]]))
        j <- j + 1L
      }

      current <- list(
        fuente_archivo = pdf_name,
        pagina = page_number,
        area = area,
        entidad = entity,
        descripcion = NA_character_,
        direccion = NA_character_,
        telefono_raw = NA_character_,
        email_raw = NA_character_,
        web = NA_character_,
        horario = NA_character_,
        recurso_igualdad = NA_character_,
        categoria_principal = area,
        subcategoria = current_subcategory %||% area
      )
      i <- j
      next
    }

    if (is.null(current)) {
      i <- i + 1L
      next
    }

    if (str_detect(line, regex("^Resumen del servicio que ofrece", ignore_case = TRUE))) {
      current <- append_field_value(
        current,
        "descripcion",
        str_remove(line, regex("^Resumen del servicio que ofrece:?\\s*", ignore_case = TRUE))
      )
      i <- i + 1L
      next
    }

    if (str_detect(line, regex("^(Tel[eé]fono|Tlf|M[oó]vil|WhatsApp|Contacto)", ignore_case = TRUE))) {
      current$telefono_raw <- collapse_unique(c(current$telefono_raw, extract_phones_text(line)))
      i <- i + 1L
      next
    }

    if (str_detect(line, regex("^(Correo electr[oó]nico|Email)", ignore_case = TRUE))) {
      current$email_raw <- collapse_unique(c(current$email_raw, extract_emails_text(line)))
      i <- i + 1L
      next
    }

    if (str_detect(line, regex("^(Direcci[oó]n|Ubicaci[oó]n|Sede)", ignore_case = TRUE))) {
      current <- append_field_value(
        current,
        "direccion",
        str_remove(line, regex("^(Direcci[oó]n|Ubicaci[oó]n|Sede):?\\s*", ignore_case = TRUE))
      )
      i <- i + 1L
      next
    }

    if (str_detect(line, regex("^Horario", ignore_case = TRUE))) {
      current <- append_field_value(
        current,
        "horario",
        str_remove(line, regex("^Horario:?\\s*", ignore_case = TRUE))
      )
      i <- i + 1L
      next
    }

    if (str_detect(line, regex("^Web", ignore_case = TRUE))) {
      current <- append_field_value(
        current,
        "web",
        str_remove(line, regex("^Web:?\\s*", ignore_case = TRUE))
      )
      i <- i + 1L
      next
    }

    if (str_detect(line, regex("^Recurso de igualdad", ignore_case = TRUE))) {
      current <- append_field_value(
        current,
        "recurso_igualdad",
        str_remove(line, regex("^Recurso de igualdad:?\\s*", ignore_case = TRUE))
      )
      i <- i + 1L
      next
    }

    if (!starts_new_label(line) && !starts_new_entity(line)) {
      if (!is.na(current$recurso_igualdad) && str_detect(current$subcategoria, regex("discapacidad", ignore_case = TRUE))) {
        current <- append_field_value(current, "recurso_igualdad", line)
      } else if (!is.na(current$direccion) && is.na(current$horario) && !str_detect(line, regex("https?://|@", ignore_case = TRUE))) {
        current <- append_field_value(current, "direccion", line)
      } else if (!is.na(current$descripcion) && is.na(current$direccion)) {
        current <- append_field_value(current, "descripcion", line)
      } else if (!is.na(current$horario)) {
        current <- append_field_value(current, "horario", line)
      } else if (!is.na(current$descripcion)) {
        current <- append_field_value(current, "descripcion", line)
      }
    }

    if (!is.na(extract_phones_text(line))) {
      current$telefono_raw <- collapse_unique(c(current$telefono_raw, extract_phones_text(line)))
    }
    if (!is.na(extract_emails_text(line))) {
      current$email_raw <- collapse_unique(c(current$email_raw, extract_emails_text(line)))
    }
    if (str_detect(line, "https?://")) {
      current <- append_field_value(current, "web", str_extract(line, "https?://\\S+"))
    }

    i <- i + 1L
  }

  if (!is.null(current)) {
    records[[record_idx]] <- as_tibble(current)
  }

  bind_rows(records)
}

extract_area_lines <- function(pdf_tokens, pages) {
  total_pages <- length(pdf_tokens)
  pages <- pages[pages >= 1 & pages <= total_pages]
  if (!length(pages)) {
    log_abort("Rango de paginas vacio o fuera del PDF (paginas totales: {total_pages})")
  }
  map_dfr(pages, function(page_number) {
    ordered_page_lines(pdf_tokens[[page_number]]) %>%
      mutate(pagina = page_number) %>%
      select(pagina, column, y, text)
  })
}

read_pdf_safe <- function(pdf_path, fn, label) {
  result <- tryCatch(fn(pdf_path), error = function(e) e)
  if (inherits(result, "error")) {
    log_abort("Fallo al leer {label} de {basename(pdf_path)}: {conditionMessage(result)}")
  }
  result
}

detect_section_pages <- function(page_text) {
  total_pages <- length(page_text)
  norm <- vapply(page_text, function(t) {
    str_to_lower(normalize_text(t %||% ""))
  }, character(1))

  vg_pattern <- "violencia (machista|de genero|de g[eé]nero)"
  disc_pattern <- "(personas con )?discapacidad"
  bibl_pattern <- "(bibliograf[ií]a|anexos?|gloss?ario|legislaci[oó]n)"

  count_matches <- function(text, pattern) {
    m <- str_extract_all(text, regex(pattern, ignore_case = TRUE))[[1]]
    length(m)
  }

  vg_counts <- vapply(norm, count_matches, integer(1), pattern = vg_pattern)
  disc_counts <- vapply(norm, count_matches, integer(1), pattern = disc_pattern)
  bibl_counts <- vapply(norm, count_matches, integer(1), pattern = bibl_pattern)

  min_content_page <- max(10L, as.integer(ceiling(total_pages * 0.1)))
  density_threshold <- 3L

  vg_dense <- which(vg_counts >= density_threshold & seq_along(vg_counts) >= min_content_page)
  disc_dense <- which(disc_counts >= density_threshold & seq_along(disc_counts) >= min_content_page)

  if (!length(vg_dense) || !length(disc_dense)) {
    return(NULL)
  }

  vg_start <- min(vg_dense)
  disc_candidates <- disc_dense[disc_dense > vg_start]
  if (!length(disc_candidates)) return(NULL)
  disc_start <- min(disc_candidates)

  end_candidates <- which(bibl_counts >= 1L & seq_along(bibl_counts) > disc_start)
  disc_end <- if (length(end_candidates)) min(end_candidates) - 1L else total_pages

  if ((disc_start - vg_start) < 5L) return(NULL)
  if ((disc_end - disc_start) < 5L) return(NULL)

  list(
    violencia = seq.int(vg_start, disc_start - 1L),
    discapacidad = seq.int(disc_start, disc_end)
  )
}

main <- function() {
  args <- parse_cli_args()
  root <- normalizePath(args$root %||% default_root_from_stage_script(), winslash = "/", mustWork = TRUE)
  output_dir <- file.path(root, "salidas", "01_extraccion")
  ensure_dir(output_dir)

  pdf_path <- file.path(root, "fuentes", "RECURSOS VG Y DISCAPACIDAD EN CANARIAS -GUIA-ENTRELANZANDO-REDES-.pdf")
  output_path <- file.path(output_dir, "vg_discapacidad_raw.csv")

  if (!file.exists(pdf_path)) {
    log_abort("No existe el PDF de violencia machista y discapacidad: {pdf_path}")
  }

  pdf_tokens <- read_pdf_safe(pdf_path, pdf_data, "tokens (pdf_data)")
  page_text <- read_pdf_safe(pdf_path, pdf_text, "texto (pdf_text)")

  detected <- detect_section_pages(page_text)
  if (is.null(detected)) {
    log_warn("No se detectaron secciones automaticamente en {basename(pdf_path)}; usando rangos por defecto (16:39 y 41:85).")
    detected <- list(violencia = 16:39, discapacidad = 41:85)
  } else {
    log_info("Paginas detectadas: violencia={min(detected$violencia)}-{max(detected$violencia)}, discapacidad={min(detected$discapacidad)}-{max(detected$discapacidad)}")
  }

  violencia_lines <- extract_area_lines(pdf_tokens, detected$violencia)
  discapacidad_lines <- extract_area_lines(pdf_tokens, detected$discapacidad)

  violencia <- parse_lines_to_records(violencia_lines, "Violencia machista", basename(pdf_path))
  discapacidad <- parse_lines_to_records(discapacidad_lines, "Discapacidad", basename(pdf_path))

  data <- bind_rows(violencia, discapacidad) %>%
    mutate(across(everything(), ~ ifelse(.x == "", NA, .x)))

  safe_write_csv(data, output_path)
  log_info("Recursos de VG y discapacidad extraidos: {nrow(data)} registros -> {output_path}")
}

if (sys.nframe() == 0) {
  main()
}
