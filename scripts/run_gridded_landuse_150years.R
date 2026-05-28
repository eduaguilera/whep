#!/usr/bin/env Rscript
# RAM smoke test for build_gridded_landuse(), including the capacity constraint.
#
# Defaults are deliberately conservative: one year, one worker, no output file.
# Increase YEARS, CHUNK_SIZE, and N_WORKERS gradually while measuring RSS with
# /usr/bin/time -v.
#
# Examples:
#   source("scripts/run_gridded_landuse_150years.R")
#   run_gridded_landuse_smoke(years = 2000)
#   run_gridded_landuse_smoke(years = 1851:2021, chunk_size = 24, n_workers = 24)
#   run_gridded_landuse_smoke(years = "2000:2015", auto_tune = TRUE)
#
# Rscript/env usage is still supported for the guard wrapper:
#   /usr/bin/time -v Rscript scripts/run_gridded_landuse_150years.R

options(cli.progress_show_after = 0)
options(cli.unicode = FALSE)

flush_cat <- function(...) {
  cat(..., "\n")
  if (interactive()) {
    flush.console()
  }
}

env_int <- function(name, default) {
  val <- Sys.getenv(name, unset = NA_character_)
  if (is.na(val) || !nzchar(val)) {
    return(default)
  }
  as.integer(val)
}

env_bool <- function(name, default) {
  val <- Sys.getenv(name, unset = NA_character_)
  if (is.na(val) || !nzchar(val)) {
    return(default)
  }
  tolower(val) %in% c("1", "true", "t", "yes", "y")
}

env_num <- function(name, default) {
  val <- Sys.getenv(name, unset = NA_character_)
  if (is.na(val) || !nzchar(val)) {
    return(default)
  }
  as.numeric(val)
}

mem_available_mb <- function() {
  meminfo <- "/proc/meminfo"
  if (!file.exists(meminfo)) {
    return(NA_real_)
  }
  lines <- readLines(meminfo, warn = FALSE)
  available <- grep("^MemAvailable:", lines, value = TRUE)
  if (length(available) == 0L) {
    return(NA_real_)
  }
  as.numeric(gsub("[^0-9]", "", available[[1L]])) / 1024
}

parse_years <- function(spec, available) {
  if (is.null(spec) || length(spec) == 0L) {
    return(head(available, 1L))
  }
  if (is.numeric(spec) || is.integer(spec)) {
    return(sort(intersect(unique(as.integer(spec)), available)))
  }
  spec <- paste(spec, collapse = ",")
  if (!nzchar(spec)) {
    return(head(available, 1L))
  }
  pieces <- strsplit(spec, ",", fixed = TRUE)[[1L]]
  years <- integer()
  for (piece in pieces) {
    piece <- trimws(piece)
    if (grepl("^[0-9]{4}:[0-9]{4}$", piece)) {
      bounds <- as.integer(strsplit(piece, ":", fixed = TRUE)[[1L]])
      years <- c(years, seq(bounds[[1L]], bounds[[2L]]))
    } else {
      years <- c(years, as.integer(piece))
    }
  }
  sort(intersect(unique(years), available))
}

read_parquet <- function(path) {
  if (requireNamespace("nanoparquet", quietly = TRUE)) {
    return(nanoparquet::read_parquet(path))
  }
  if (requireNamespace("arrow", quietly = TRUE)) {
    return(arrow::read_parquet(path))
  }
  stop("Need nanoparquet or arrow installed to read ", path, call. = FALSE)
}

rss_mb <- function() {
  status_path <- "/proc/self/status"
  if (!file.exists(status_path)) {
    return(NA_real_)
  }
  lines <- readLines(status_path, warn = FALSE)
  vmrss <- grep("^VmRSS:", lines, value = TRUE)
  if (length(vmrss) == 0L) {
    return(NA_real_)
  }
  as.numeric(gsub("[^0-9]", "", vmrss[[1L]])) / 1024
}

gridded_landuse_defaults <- list(
  inputs_dir = file.path("LPJmL_inputs", "whep", "inputs"),
  out_file = file.path("scripts", "gridded_landuse_streamed.csv"),
  years = NULL,
  use_type_cropland = TRUE,
  use_multicropping = TRUE,
  write_output = FALSE,
  overwrite_output = FALSE,
  auto_tune = FALSE,
  chunk_size = 1L,
  n_workers = 1L,
  max_iterations = 1000L,
  target_mem_fraction = 0.70,
  worker_mem_mb = 4500
)

load_whep_for_smoke <- function() {
  if (exists("build_gridded_landuse", mode = "function")) {
    return(invisible(TRUE))
  }

  loaded <- FALSE
  if (requireNamespace("pkgload", quietly = TRUE)) {
    tryCatch(
      {
        pkgload::load_all(".", quiet = TRUE)
        loaded <- TRUE
      },
      error = function(e) NULL
    )
  }
  if (!loaded && requireNamespace("devtools", quietly = TRUE)) {
    tryCatch(
      {
        devtools::load_all(".", quiet = TRUE)
        loaded <- TRUE
      },
      error = function(e) NULL
    )
  }
  if (!loaded) {
    stop("Could not load whep. Run this from the package root.", call. = FALSE)
  }
  invisible(TRUE)
}

run_gridded_landuse_smoke <- function(
  inputs_dir = gridded_landuse_defaults$inputs_dir,
  out_file = gridded_landuse_defaults$out_file,
  years = gridded_landuse_defaults$years,
  use_type_cropland = gridded_landuse_defaults$use_type_cropland,
  use_multicropping = gridded_landuse_defaults$use_multicropping,
  write_output = gridded_landuse_defaults$write_output,
  overwrite_output = gridded_landuse_defaults$overwrite_output,
  auto_tune = gridded_landuse_defaults$auto_tune,
  chunk_size = gridded_landuse_defaults$chunk_size,
  n_workers = gridded_landuse_defaults$n_workers,
  max_iterations = gridded_landuse_defaults$max_iterations,
  target_mem_fraction = gridded_landuse_defaults$target_mem_fraction,
  worker_mem_mb = gridded_landuse_defaults$worker_mem_mb
) {
  chunk_size <- max(1L, as.integer(chunk_size))
  n_workers <- max(1L, as.integer(n_workers))
  max_iterations <- max(1L, as.integer(max_iterations))

  if (write_output && file.exists(out_file) && !overwrite_output) {
    stop(
      "Output file already exists: ", out_file, "\n",
      "Set overwrite_output = TRUE to replace it.",
      call. = FALSE
    )
  }

  flush_cat("Loading inputs from", inputs_dir)
  country_areas <- read_parquet(file.path(inputs_dir, "country_areas.parquet"))
  crop_patterns <- read_parquet(file.path(inputs_dir, "crop_patterns.parquet"))
  gridded_cropland <- read_parquet(file.path(inputs_dir, "gridded_cropland.parquet"))
  country_grid <- read_parquet(file.path(inputs_dir, "country_grid.parquet"))

  type_cropland <- NULL
  if (use_type_cropland) {
    type_path <- file.path(inputs_dir, "type_cropland.parquet")
    if (file.exists(type_path)) {
      type_cropland <- read_parquet(type_path)
    } else {
      flush_cat("type_cropland.parquet not found; using total cropland")
    }
  }

  multicropping <- NULL
  if (use_multicropping) {
    mc_path <- file.path(inputs_dir, "multicropping.parquet")
    if (file.exists(mc_path)) {
      multicropping <- read_parquet(mc_path)
    } else {
      flush_cat("multicropping.parquet not found; using mc=1 default")
    }
  }

  load_whep_for_smoke()

  cft_mapping <- readr::read_csv(
    file.path("inst", "extdata", "cft_mapping.csv"),
    show_col_types = FALSE
  )

  available_years <- sort(unique(as.integer(country_areas[["year"]])))
  years <- parse_years(years, available_years)
  if (length(years) == 0L) {
    stop("No requested years are present in country_areas.", call. = FALSE)
  }

  if (auto_tune) {
    cores <- parallel::detectCores(logical = TRUE)
    mem_mb <- mem_available_mb()
    mem_workers <- if (is.finite(mem_mb)) {
      max(1L, floor((mem_mb * target_mem_fraction) / worker_mem_mb))
    } else {
      1L
    }
    n_workers <- max(1L, min(cores - 1L, mem_workers))
    chunk_size <- max(chunk_size, n_workers)
  }

  flush_cat("country_areas   :", nrow(country_areas), "rows")
  flush_cat("crop_patterns   :", nrow(crop_patterns), "rows")
  flush_cat("gridded_cropland:", nrow(gridded_cropland), "rows")
  flush_cat("country_grid    :", nrow(country_grid), "rows")
  flush_cat("type_cropland   :", if (is.null(type_cropland)) 0L else nrow(type_cropland), "rows")
  flush_cat("multicropping   :", if (is.null(multicropping)) 0L else nrow(multicropping), "rows")
  flush_cat("")
  flush_cat("Years       :", paste(range(years), collapse = "-"), "(", length(years), "years )")
  flush_cat("Chunk size  :", chunk_size)
  flush_cat("Workers     :", n_workers)
  if (auto_tune) {
    flush_cat("Auto tune   : TRUE")
    flush_cat("Mem avail   :", round(mem_available_mb()), "MB")
    flush_cat("Mem fraction:", target_mem_fraction)
    flush_cat("Worker MB   :", worker_mem_mb)
  }
  flush_cat("Max iter    :", max_iterations)
  flush_cat("Write output:", write_output)
  if (write_output) {
    flush_cat("Output file :", out_file)
  }
  flush_cat("")

  if (write_output && overwrite_output && file.exists(out_file)) {
    file.remove(out_file)
  }

  year_chunks <- split(years, ceiling(seq_along(years) / chunk_size))
  write_header <- TRUE
  t_total <- proc.time()[["elapsed"]]
  total_rows <- 0L
  chunk_metrics <- vector("list", length(year_chunks))

  for (chunk_i in seq_along(year_chunks)) {
    yrs <- year_chunks[[chunk_i]]
    t0 <- proc.time()[["elapsed"]]
    gc(reset = TRUE)

    flush_cat(
      sprintf(
        "[chunk %d/%d] years %s | rss %.0f MB",
        chunk_i,
        length(year_chunks),
        paste(yrs, collapse = ","),
        rss_mb()
      )
    )

    result <- build_gridded_landuse(
      country_areas = dplyr::filter(country_areas, year %in% yrs),
      crop_patterns = crop_patterns,
      gridded_cropland = dplyr::filter(gridded_cropland, year %in% yrs),
      country_grid = country_grid,
      config = list(
        years = yrs,
        type_cropland = if (!is.null(type_cropland)) {
          dplyr::filter(type_cropland, year %in% yrs)
        },
        type_mapping = if (!is.null(type_cropland)) cft_mapping,
        multicropping = if (!is.null(multicropping)) {
          dplyr::filter(multicropping, year %in% yrs)
        },
        max_iterations = max_iterations,
        n_workers = min(n_workers, length(yrs))
      )
    )

    total_rows <- total_rows + nrow(result)
    elapsed <- round(proc.time()[["elapsed"]] - t0, 2)
    chunk_rss_mb <- rss_mb()
    flush_cat(
      sprintf(
        "[chunk %d/%d] done in %.2f s | rows %d | rss %.0f MB",
        chunk_i,
        length(year_chunks),
        elapsed,
        nrow(result),
        chunk_rss_mb
      )
    )

    chunk_metrics[[chunk_i]] <- data.frame(
      chunk = chunk_i,
      first_year = min(yrs),
      last_year = max(yrs),
      n_years = length(yrs),
      rows = nrow(result),
      elapsed_s = elapsed,
      rss_mb = chunk_rss_mb
    )

    if (write_output) {
      data.table::fwrite(
        x = data.table::as.data.table(result),
        file = out_file,
        append = !write_header
      )
      write_header <- FALSE
    }

    rm(result)
    gc()
  }

  elapsed_total <- round(proc.time()[["elapsed"]] - t_total, 1)
  final_rss_mb <- round(rss_mb())
  flush_cat("")
  flush_cat("Total rows:", total_rows)
  flush_cat("Elapsed   :", elapsed_total, "s")
  flush_cat("Final RSS :", final_rss_mb, "MB")
  if (write_output) {
    flush_cat("Streamed output:", out_file)
  }

  invisible(list(
    years = years,
    total_rows = total_rows,
    elapsed_s = elapsed_total,
    final_rss_mb = final_rss_mb,
    chunk_metrics = do.call(rbind, chunk_metrics)
  ))
}

run_gridded_landuse_smoke_from_env <- function() {
  run_gridded_landuse_smoke(
    inputs_dir = Sys.getenv(
      "INPUTS_DIR",
      unset = gridded_landuse_defaults$inputs_dir
    ),
    out_file = Sys.getenv("OUT_FILE", unset = gridded_landuse_defaults$out_file),
    years = Sys.getenv("YEARS", unset = ""),
    use_type_cropland = env_bool(
      "USE_TYPE_CROPLAND",
      gridded_landuse_defaults$use_type_cropland
    ),
    use_multicropping = env_bool(
      "USE_MULTICROPPING",
      gridded_landuse_defaults$use_multicropping
    ),
    write_output = env_bool(
      "WRITE_OUTPUT",
      gridded_landuse_defaults$write_output
    ),
    overwrite_output = env_bool(
      "OVERWRITE_OUTPUT",
      gridded_landuse_defaults$overwrite_output
    ),
    auto_tune = env_bool("AUTO", gridded_landuse_defaults$auto_tune),
    chunk_size = env_int("CHUNK_SIZE", gridded_landuse_defaults$chunk_size),
    n_workers = env_int("N_WORKERS", gridded_landuse_defaults$n_workers),
    max_iterations = env_int("MAX_ITER", gridded_landuse_defaults$max_iterations),
    target_mem_fraction = env_num(
      "TARGET_MEM_FRACTION",
      gridded_landuse_defaults$target_mem_fraction
    ),
    worker_mem_mb = env_num("WORKER_MEM_MB", gridded_landuse_defaults$worker_mem_mb)
  )
}

is_rscript_direct_run <- function() {
  file_arg <- commandArgs(FALSE)
  file_arg <- file_arg[grepl("^--file=", file_arg)]
  if (length(file_arg) == 0L) {
    return(FALSE)
  }
  identical(
    basename(sub("^--file=", "", file_arg[[1L]])),
    "run_gridded_landuse_150years.R"
  )
}

if (is_rscript_direct_run()) {
  run_gridded_landuse_smoke_from_env()
}
