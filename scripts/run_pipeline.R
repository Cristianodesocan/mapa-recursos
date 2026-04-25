suppressPackageStartupMessages({
  library(glue)
  library(cli)
})

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) return(y)
  if (length(x) == 1 && is.na(x)) return(y)
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
  if (!length(file_arg)) stop("No se pudo resolver la ruta del script maestro.")
  file_path <- sub("^--file=", "", file_arg[[1]])
  file_path <- gsub("~\\+~", " ", file_path)
  normalizePath(file_path, winslash = "/", mustWork = TRUE)
}

ensure_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
}

run_stage <- function(root, script_path, extra_args = character(), force = FALSE,
                      pass_root = TRUE, optional = FALSE) {
  args <- shQuote(script_path)
  if (isTRUE(pass_root)) args <- c(args, "--root", shQuote(root))
  if (isTRUE(force)) args <- c(args, "--force")
  if (length(extra_args)) {
    quoted_extra <- vapply(extra_args, function(a) {
      if (startsWith(a, "--")) a else shQuote(a)
    }, character(1))
    args <- c(args, quoted_extra)
  }

  started <- Sys.time()
  status <- system2("Rscript", args = args, stdout = "", stderr = "")
  elapsed <- round(as.numeric(difftime(Sys.time(), started, units = "secs")), 1)

  if (!identical(status, 0L)) {
    if (isTRUE(optional)) {
      cli::cli_alert_warning(glue("{basename(script_path)} fallo (codigo {status}, {elapsed}s) - continuando por ser opcional"))
      return(invisible(elapsed))
    }
    cli::cli_abort(glue("Fallo {basename(script_path)} (codigo {status}, {elapsed}s)"))
  }
  cli::cli_alert_success(glue("{basename(script_path)} OK ({elapsed}s)"))
  invisible(elapsed)
}

install_missing_packages <- function(packages) {
  installed <- rownames(installed.packages())
  missing <- setdiff(packages, installed)
  if (!length(missing)) {
    cli::cli_inform("No faltan paquetes de R.")
    return(invisible(NULL))
  }
  cli::cli_inform(glue("Instalando paquetes faltantes: {paste(missing, collapse = ', ')}"))
  install.packages(missing, repos = "https://cloud.r-project.org")
}

validate_outputs <- function(stage, root) {
  expected <- list(
    extraccion = c(
      file.path(root, "salidas", "01_extraccion", "residencias_raw.csv"),
      file.path(root, "salidas", "01_extraccion", "vg_discapacidad_raw.csv")
    ),
    transformacion = c(
      file.path(root, "salidas", "02_transformacion", "recursos_normalizados.csv")
    ),
    geocodificacion = c(
      file.path(root, "salidas", "03_geocodificacion", "recursos_geocodificados.csv")
    ),
    analisis = c(
      file.path(root, "salidas", "04_analisis", "resumen_calidad.csv"),
      file.path(root, "salidas", "04_analisis", "directorio_recursos.xlsx"),
      file.path(root, "salidas", "04_analisis", "recursos_canonicos.csv")
    ),
    export_web = c(
      file.path(root, "web", "data", "recursos.json")
    ),
    supabase = character()
  )
  missing <- expected[[stage]][!file.exists(expected[[stage]])]
  if (length(missing)) {
    cli::cli_abort(c(
      "Faltan salidas esperadas tras la fase {stage}:",
      set_names(missing, rep("x", length(missing)))
    ))
  }
}

print_run_summary <- function(timings, root) {
  cli::cli_h1("Resumen del pipeline")
  for (nm in names(timings)) {
    cli::cli_inform(glue("- {nm}: {timings[[nm]]}s"))
  }
  outputs <- c(
    file.path(root, "salidas", "02_transformacion", "recursos_normalizados.csv"),
    file.path(root, "salidas", "03_geocodificacion", "recursos_geocodificados.csv"),
    file.path(root, "salidas", "03_geocodificacion", "recursos_geocodificados.geojson"),
    file.path(root, "salidas", "04_analisis", "resumen_calidad.csv"),
    file.path(root, "salidas", "04_analisis", "incidencias_registros.csv"),
    file.path(root, "salidas", "04_analisis", "recursos_por_municipio.csv"),
    file.path(root, "salidas", "04_analisis", "recursos_por_categoria.csv"),
    file.path(root, "salidas", "04_analisis", "recursos_canonicos.csv"),
    file.path(root, "salidas", "04_analisis", "directorio_recursos.xlsx"),
    file.path(root, "web", "data", "recursos.json")
  )
  cli::cli_h2("Salidas disponibles")
  for (path in outputs[file.exists(outputs)]) {
    cli::cli_inform(glue("- {sub(paste0(root,'/'),'',path,fixed=TRUE)}"))
  }
}

main <- function() {
  args <- parse_cli_args()
  root <- normalizePath(dirname(dirname(script_path())), winslash = "/", mustWork = TRUE)

  required_pdfs <- c(
    file.path(root, "fuentes", "TF-2a_GUIA_DE_RECURSOS-Centros de Atención Residencial (1).pdf"),
    file.path(root, "fuentes", "RECURSOS VG Y DISCAPACIDAD EN CANARIAS -GUIA-ENTRELANZANDO-REDES-.pdf")
  )
  for (pdf in required_pdfs) {
    if (!file.exists(pdf)) cli::cli_abort(glue("Falta el PDF requerido: {pdf}"))
  }

  for (sub in c("01_extraccion", "02_transformacion", "03_geocodificacion", "04_analisis", "05_supabase")) {
    ensure_dir(file.path(root, "salidas", sub))
  }
  ensure_dir(file.path(root, "cache"))

  packages <- c(
    "pdftools", "tidyverse", "readr", "writexl", "jsonlite",
    "sf", "httr2", "digest", "glue", "cli",
    "tidyr", "purrr", "yaml"
  )
  if (isTRUE(args$`install-deps`)) install_missing_packages(packages)

  stage_order <- c("extraccion", "transformacion", "geocodificacion", "analisis", "export_web", "supabase")
  scraper_extra <- character()
  if (!is.null(args$`scraper-config`)) {
    scraper_extra <- c(scraper_extra, "--config", args$`scraper-config`)
  }
  if (!is.null(args$`scraper-max-pages`)) {
    scraper_extra <- c(scraper_extra, "--max-pages", args$`scraper-max-pages`)
  }
  stage_scripts <- list(
    extraccion = list(
      list(path = file.path(root, "pipeline", "01_extraccion", "extract_residencias.R")),
      list(path = file.path(root, "pipeline", "01_extraccion", "extract_vg_discapacidad.R")),
      list(
        path = file.path(root, "pipeline", "01_extraccion", "scrape_registro_asociaciones.R"),
        optional = TRUE,
        skip_when = isTRUE(args$`skip-scraper`),
        pass_root = FALSE,
        cwd = root,
        extra = scraper_extra
      )
    ),
    transformacion = list(
      list(path = file.path(root, "pipeline", "02_transformacion", "transform_recursos.R"))
    ),
    geocodificacion = list(
      list(path = file.path(root, "pipeline", "03_geocodificacion", "geocode_recursos.R"))
    ),
    analisis = list(
      list(path = file.path(root, "pipeline", "04_analisis", "qa_recursos.R"))
    ),
    export_web = list(
      list(path = file.path(root, "pipeline", "06_export_web", "build_web_data.R"))
    ),
    supabase = list(
      list(
        path = file.path(root, "pipeline", "05_supabase", "upload_supabase.R"),
        optional = TRUE,
        skip_when = isTRUE(args$`skip-supabase`)
      )
    )
  )

  to_stage_default <- if (isTRUE(args$`skip-supabase`)) "analisis" else "supabase"
  args$to <- args$to %||% to_stage_default

  if (!is.null(args$only)) {
    only <- args$only
    if (!only %in% stage_order) cli::cli_abort("Fase --only desconocida: {only}")
    selected_stages <- only
  } else {
    from_stage <- args$from %||% "extraccion"
    to_stage <- args$to %||% "supabase"
    from_idx <- match(from_stage, stage_order)
    to_idx <- match(to_stage, stage_order)
    if (is.na(from_idx) || is.na(to_idx) || from_idx > to_idx) {
      cli::cli_abort("--from y --to deben ser fases validas en orden.")
    }
    selected_stages <- stage_order[from_idx:to_idx]
  }

  timings <- list()
  total_started <- Sys.time()

  for (stage in selected_stages) {
    if (stage == "geocodificacion" && isTRUE(args$`skip-geocoding`)) {
      geocoded_path <- file.path(root, "salidas", "03_geocodificacion", "recursos_geocodificados.csv")
      if (!file.exists(geocoded_path)) {
        cli::cli_abort("Se solicito --skip-geocoding pero no existe un dataset geocodificado previo.")
      }
      cli::cli_alert_info("Saltando fase de geocodificacion (--skip-geocoding).")
      next
    }

    cli::cli_h1(glue("Fase: {stage}"))
    stage_started <- Sys.time()

    stage_extra <- character()
    if (stage == "geocodificacion") {
      if (!is.null(args$`user-agent`)) {
        stage_extra <- c(stage_extra, "--user-agent", args$`user-agent`)
      }
      if (!is.null(args$sleep)) {
        stage_extra <- c(stage_extra, "--sleep", args$sleep)
      }
      if (!is.null(args$`max-failures`)) {
        stage_extra <- c(stage_extra, "--max-failures", args$`max-failures`)
      }
      if (!is.null(args$`mapbox-token`)) {
        stage_extra <- c(stage_extra, "--mapbox-token", args$`mapbox-token`)
      }
      if (!is.null(args$geocoder)) {
        stage_extra <- c(stage_extra, "--geocoder", args$geocoder)
      }
    }
    if (stage == "supabase") {
      if (!is.null(args$`supabase-url`)) {
        stage_extra <- c(stage_extra, "--supabase-url", args$`supabase-url`)
      }
      if (!is.null(args$`service-key`)) {
        stage_extra <- c(stage_extra, "--service-key", args$`service-key`)
      }
      if (!is.null(args$`supabase-schema`)) {
        stage_extra <- c(stage_extra, "--schema", args$`supabase-schema`)
      }
      if (!is.null(args$`supabase-table`)) {
        stage_extra <- c(stage_extra, "--table", args$`supabase-table`)
      }
      if (isTRUE(args$`supabase-dry-run`)) {
        stage_extra <- c(stage_extra, "--dry-run")
      }
    }

    for (script_spec in stage_scripts[[stage]]) {
      if (isTRUE(script_spec$skip_when)) {
        cli::cli_alert_info(glue("Saltando {basename(script_spec$path)} (flag de saltado activo)."))
        next
      }
      script_extra <- c(stage_extra, script_spec$extra %||% character())
      old_wd <- NULL
      if (!is.null(script_spec$cwd)) {
        old_wd <- setwd(script_spec$cwd)
        on.exit(if (!is.null(old_wd)) setwd(old_wd), add = TRUE)
      }
      run_stage(
        root, script_spec$path,
        extra_args = script_extra,
        force = isTRUE(args$force),
        pass_root = script_spec$pass_root %||% TRUE,
        optional = isTRUE(script_spec$optional)
      )
      if (!is.null(old_wd)) {
        setwd(old_wd)
        old_wd <- NULL
      }
    }
    validate_outputs(stage, root)

    timings[[stage]] <- round(as.numeric(difftime(Sys.time(), stage_started, units = "secs")), 1)
  }

  total_elapsed <- round(as.numeric(difftime(Sys.time(), total_started, units = "secs")), 1)
  timings[["TOTAL"]] <- total_elapsed
  print_run_summary(timings, root)
}

if (sys.nframe() == 0) main()
