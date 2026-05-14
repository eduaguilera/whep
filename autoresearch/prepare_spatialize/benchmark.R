#
# Benchmark harness for the full prepare_spatialize_all.R pipeline.
#
# Profiles every section end-to-end under Rprof and reports:
#   - METRIC total_s=<seconds>         (the optimization score)
#   - per-section wall-clock timings   (to pinpoint where to focus)
#   - PROFILE_START/END                (top 15 self-time entries)
#   - CALLERS_START/END                (top callers of hotspots)
#
# Usage:
#   Rscript autoresearch/prepare_spatialize/benchmark.R 2>&1 | tee /tmp/bench_spatialize.txt
#
# Adjust year_range and l_files_dir to match your local data location.

# ── Config ──────────────────────────────────────────────────────────────────

l_files_dir <- "LPJmL_inputs"

# Source the script to load all section functions without running main().
source("inst/scripts/prepare_spatialize_all.R")

# Override global year_range to keep each run fast (2 years).
year_range <- 1850:2010L
target_res <- 0.5

# ── Helpers ─────────────────────────────────────────────────────────────────

section_times <- list()

time_section <- function(name, expr) {
  t0 <- proc.time()[["elapsed"]]
  force(expr)
  elapsed <- proc.time()[["elapsed"]] - t0
  section_times[[name]] <<- elapsed
  cat(sprintf("  [%s] %.1fs\n", name, elapsed))
  invisible(elapsed)
}

# ── Setup ───────────────────────────────────────────────────────────────────

if (!dir.exists(l_files_dir)) {
  stop("l_files_dir not found: ", l_files_dir)
}
l_files_dir <- normalizePath(l_files_dir, mustWork = FALSE)
output_dir <- file.path(l_files_dir, "whep", "inputs")
run_dir <- file.path(l_files_dir, "whep")

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# ── Run ─────────────────────────────────────────────────────────────────────

gc(reset = TRUE)
prof_file <- tempfile("prof_spatialize_", fileext = ".out")

Rprof(prof_file, interval = 0.02)
t0 <- proc.time()

cat("=== Section timings ===\n")

time_section("prod_cache", {
  prod <- .load_or_cache_production(output_dir, year_range)
})

time_section("s1_country_grid", {
  country_grid <- prepare_country_grid(l_files_dir, target_res)
  .save_parquet(country_grid, output_dir, "country_grid")
})

time_section("s5_mirca_irrigation", {
  prepare_mirca_irrigation(l_files_dir, output_dir, country_grid, target_res)
})

time_section("s4_gridded_cropland", {
  s4_result <- prepare_gridded_cropland(
    l_files_dir,
    year_range,
    target_res,
    output_dir = output_dir,
    country_grid = country_grid
  )
  .save_parquet(s4_result$gridded, output_dir, "gridded_cropland")
})

time_section("s2_country_areas", {
  country_areas <- prepare_country_areas(
    l_files_dir,
    year_range,
    country_grid,
    target_res,
    prod,
    luh2_totals = s4_result$luh2_totals
  )
  .save_parquet(country_areas, output_dir, "country_areas")
})

time_section("s3_crop_patterns", {
  crop_patterns <- prepare_crop_patterns(l_files_dir, target_res)
  .save_parquet(crop_patterns, output_dir, "crop_patterns")
})

time_section("s3b_crop_fert_patterns", {
  crop_fert <- prepare_crop_fert_patterns(l_files_dir, target_res)
  if (!is.null(crop_fert)) {
    .save_parquet(crop_fert, output_dir, "crop_fertilizer_patterns")
  }
})

time_section("s6_yield_inputs", {
  prepare_yield_inputs(
    l_files_dir,
    output_dir,
    target_res,
    year_range = year_range,
    prod = prod
  )
})

time_section("s7_nitrogen_inputs", {
  prepare_nitrogen_inputs(l_files_dir, output_dir, year_range, prod = prod)
})

time_section("s8_livestock_inputs", {
  prepare_livestock_inputs(
    l_files_dir,
    output_dir,
    year_range,
    target_res,
    prod = prod
  )
})

time_section("s9a_hydrology", {
  prepare_hydrology_inputs(l_files_dir, output_dir)
})

time_section("s9b_soil", {
  prepare_soil_inputs(l_files_dir, output_dir, target_res)
})

time_section("s10_crop_spatialize", {
  run_crop_spatialize(run_dir, output_dir, year_range)
})

time_section("s11_livestock_spatialize", {
  run_livestock_spatialize(run_dir, output_dir)
})

elapsed <- (proc.time() - t0)[["elapsed"]]
Rprof(NULL)

# ── Report ──────────────────────────────────────────────────────────────────

cat(sprintf("METRIC total_s=%.2f\n", elapsed))

cat("\n=== Section breakdown ===\n")
for (nm in names(section_times)) {
  pct <- round(100 * section_times[[nm]] / elapsed, 1)
  cat(sprintf("  %5.1f%%  %6.1fs  %s\n", pct, section_times[[nm]], nm))
}

cat("\nPROFILE_START\n")
sp <- summaryRprof(prof_file)$by.self
sp$pct <- round(100 * sp$self.time / sum(sp$self.time), 1)
top <- utils::head(sp, 15)
for (i in seq_len(nrow(top))) {
  cat(sprintf(
    "  %5.1f%% %6.2fs  %s\n",
    top$pct[i],
    top$self.time[i],
    rownames(top)[i]
  ))
}
cat("PROFILE_END\n")

# ── Caller analysis ────────────────────────────────────────────────────────

cat("CALLERS_START\n")

lines <- readLines(prof_file)
lines <- lines[-1]

targets <- c("forderv", "bmerge", "copy", "aggregate", "classify")

for (target in targets) {
  caller_counts <- list()
  for (line in lines) {
    fns <- strsplit(line, " ")[[1]]
    idx <- which(fns == paste0('"', target, '"'))
    if (length(idx) == 0) {
      next
    }
    for (j in idx) {
      if (j < length(fns)) {
        caller <- fns[j + 1]
        if (j + 1 < length(fns)) {
          gp <- fns[j + 2]
          key <- paste0(caller, " <- ", gp)
        } else {
          key <- caller
        }
      } else {
        key <- '"<top>"'
      }
      caller_counts[[key]] <- (caller_counts[[key]] %||% 0L) + 1L
    }
  }
  if (length(caller_counts) == 0) {
    next
  }

  counts <- unlist(caller_counts)
  counts <- sort(counts, decreasing = TRUE)
  total_samples <- sum(counts)
  interval <- 0.02
  cat(sprintf(
    "\n  === %s (%.1fs total) ===\n",
    target,
    total_samples * interval
  ))
  top_callers <- utils::head(counts, 10)
  for (i in seq_along(top_callers)) {
    pct <- round(100 * top_callers[i] / total_samples, 1)
    cat(sprintf(
      "    %5.1f%% %5.1fs  %s\n",
      pct,
      top_callers[i] * interval,
      names(top_callers)[i]
    ))
  }
}

cat("\nCALLERS_END\n")
