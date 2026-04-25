suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(purrr)
  library(tibble)
  library(readr)
  library(glue)
  library(cli)
})

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) {
    return(y)
  }
  if (length(x) == 1 && is.na(x)) {
    return(y)
  }
  x
}

coalesce_scalar <- function(x, default = NA_character_) {
  if (is.null(x) || length(x) == 0) return(default)
  if (length(x) == 1 && is.na(x)) return(default)
  x
}

parse_cli_args <- function(args = commandArgs(trailingOnly = TRUE)) {
  parsed <- list()
  i <- 1
  while (i <= length(args)) {
    current <- args[[i]]
    if (!startsWith(current, "--")) {
      i <- i + 1
      next
    }
    key <- sub("^--", "", current)
    if (i == length(args) || startsWith(args[[i + 1]], "--")) {
      parsed[[key]] <- TRUE
      i <- i + 1
    } else {
      parsed[[key]] <- args[[i + 1]]
      i <- i + 2
    }
  }
  parsed
}

script_path <- function() {
  file_arg <- commandArgs(trailingOnly = FALSE)
  file_arg <- file_arg[startsWith(file_arg, "--file=")]
  if (!length(file_arg)) {
    stop("No se pudo resolver la ruta del script actual.")
  }
  file_path <- sub("^--file=", "", file_arg[[1]])
  file_path <- gsub("~\\+~", " ", file_path)
  normalizePath(file_path, winslash = "/", mustWork = TRUE)
}

default_root_from_stage_script <- function() {
  script <- script_path()
  normalizePath(dirname(dirname(dirname(script))), winslash = "/", mustWork = TRUE)
}

ensure_dir <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  invisible(path)
}

normalize_text <- function(x) {
  x %>%
    str_replace_all("[\r\n\t]", " ") %>%
    str_replace_all("\\s+", " ") %>%
    str_trim()
}

empty_to_na <- function(x) {
  value <- normalize_text(x %||% "")
  value[!nzchar(value)] <- NA_character_
  value
}

append_text <- function(existing, value, sep = " ") {
  value <- empty_to_na(value)
  if (is.na(value)) {
    return(existing)
  }
  existing <- empty_to_na(existing)
  if (is.na(existing)) {
    return(value)
  }
  normalize_text(paste(existing, value, sep = sep))
}

collapse_unique <- function(values, sep = "; ") {
  values <- values[!is.na(values) & nzchar(values)]
  if (!length(values)) {
    return(NA_character_)
  }
  paste(unique(values), collapse = sep)
}

extract_emails_text <- function(text) {
  matches <- str_extract_all(
    text %||% "",
    "[[:alnum:]._%+-]+@[[:alnum:].-]+\\.[[:alpha:]]{2,}"
  )[[1]]
  collapse_unique(matches)
}

extract_phones_text <- function(text) {
  matches <- str_extract_all(
    text %||% "",
    "(?:(?:\\+34\\s*)?(?:\\d[\\d .\\-/]{6,}\\d))"
  )[[1]]
  matches <- normalize_text(matches)
  matches <- matches[nzchar(matches)]
  collapse_unique(matches)
}

split_on_gaps <- function(line) {
  line <- str_trim(line %||% "")
  parts <- str_split(line, "[[:space:]]{2,}", simplify = FALSE)[[1]]
  parts <- normalize_text(parts)
  parts[nzchar(parts)]
}

is_uppercaseish <- function(text) {
  text <- normalize_text(text)
  if (!nzchar(text)) {
    return(FALSE)
  }
  stripped <- str_replace_all(text, "[^A-Za-zÁÉÍÓÚÜÑáéíóúüñ]", "")
  if (!nzchar(stripped)) {
    return(FALSE)
  }
  uppercase_ratio <- mean(strsplit(stripped, "")[[1]] == strsplit(str_to_upper(stripped), "")[[1]])
  uppercase_ratio >= 0.85
}

tokens_to_lines <- function(tokens, y_tolerance = 3) {
  tokens <- tokens %>%
    mutate(text = as.character(text)) %>%
    arrange(y, x)

  if (!nrow(tokens)) {
    return(tibble(y = numeric(), x_min = numeric(), x_max = numeric(), text = character()))
  }

  groups <- integer(nrow(tokens))
  current_group <- 1L
  groups[[1]] <- current_group
  current_y <- tokens$y[[1]]

  if (nrow(tokens) > 1) {
    for (idx in 2:nrow(tokens)) {
      if (abs(tokens$y[[idx]] - current_y) > y_tolerance) {
        current_group <- current_group + 1L
        current_y <- tokens$y[[idx]]
      }
      groups[[idx]] <- current_group
    }
  }

  tokens %>%
    mutate(line_group = groups) %>%
    group_by(line_group) %>%
    summarise(
      y = mean(y),
      x_min = min(x),
      x_max = max(x + width),
      text = {
        row_text <- text
        row_space <- space
        built <- ""
        for (i in seq_along(row_text)) {
          prefix <- if (i == 1 || !isTRUE(row_space[[i]])) "" else " "
          built <- paste0(built, prefix, row_text[[i]])
        }
        normalize_text(built)
      },
      .groups = "drop"
    ) %>%
    filter(nzchar(text))
}

detect_column_boundary <- function(tokens, min_gap = 70, min_share = 0.2) {
  xs <- sort(unique(tokens$x))
  if (length(xs) < 10) {
    return(NA_real_)
  }
  gaps <- diff(xs)
  max_gap_idx <- which.max(gaps)
  if (!length(max_gap_idx) || gaps[[max_gap_idx]] < min_gap) {
    return(NA_real_)
  }
  boundary <- (xs[[max_gap_idx]] + xs[[max_gap_idx + 1]]) / 2
  left_share <- mean(tokens$x < boundary)
  right_share <- mean(tokens$x >= boundary)
  if (left_share < min_share || right_share < min_share) {
    return(NA_real_)
  }
  boundary
}

ordered_page_lines <- function(tokens) {
  boundary <- detect_column_boundary(tokens)
  if (is.na(boundary)) {
    return(tokens_to_lines(tokens) %>% mutate(column = "full", reading_group = 1L))
  }

  left_lines <- tokens %>%
    filter(x < boundary) %>%
    tokens_to_lines() %>%
    mutate(column = "left", reading_group = 1L)

  right_lines <- tokens %>%
    filter(x >= boundary) %>%
    tokens_to_lines() %>%
    mutate(column = "right", reading_group = 2L)

  bind_rows(left_lines, right_lines) %>%
    arrange(reading_group, y, x_min)
}

safe_write_csv <- function(data, path) {
  ensure_dir(dirname(path))
  readr::write_csv(data, path, na = "")
}

write_empty_geojson <- function(path) {
  ensure_dir(dirname(path))
  writeLines('{"type":"FeatureCollection","features":[]}', con = path, useBytes = TRUE)
}

log_info <- function(...) {
  cli::cli_inform(glue(..., .envir = parent.frame()))
}

log_warn <- function(...) {
  cli::cli_warn(glue(..., .envir = parent.frame()))
}

log_abort <- function(...) {
  cli::cli_abort(glue(..., .envir = parent.frame()))
}
