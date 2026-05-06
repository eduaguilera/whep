#
# Benchmark harness for run_crop_spatialize (Section 10) only.
#
# Prerequisite: Sections 1-9 outputs must already exist in
#   LPJmL_inputs/whep/inputs/
# Run test_crop_spatialize.R first if outputs are missing.
#
# Two passes:
#   1. METRIC pass  — full run_crop_spatialize() with real parallelism.
#   2. PROFILE pass — calls .run_spatialize_year() directly in a serial
#      lapply loop under Rprof. parallel::mclapply forks children that
#      Rprof cannot follow, so we bypass it and profile the worker directly.
#      Uses profile_years (a short slice) to keep it fast (~15s).
#
# Reports:
#   - METRIC total_s=<seconds>         (the optimization score)
#   - PROFILE_START/END                (top 15 self-time entries)
#   - CALLERS_START/END                (top callers of hotspots)
#
# Usage:
#   Rscript autoresearch/crop_spatialize/benchmark.R 2>&1 | tee /tmp/bench_crop_spatialize.txt

# ── Config ──────────────────────────────────────────────────────────────────

l_files_dir   <- "LPJmL_inputs"

source("inst/scripts/prepare_spatialize_all.R")

year_range    <- 2000:2010L   # METRIC pass: full range
profile_years <- 2000:2002L   # PROFILE pass: short slice, serial

# ── Setup ───────────────────────────────────────────────────────────────────

run_dir   <- file.path(l_files_dir, "whep")
input_dir <- file.path(run_dir, "inputs")

if (!dir.exists(input_dir)) {
  stop(
    "input_dir not found: ", input_dir,
    "\nRun Sections 1-9 first (see test_crop_spatialize.R)"
  )
}

# ── Pass 1: METRIC (real parallelism, full year range) ───────────────────────

cat("=== Pass 1: METRIC (full run) ===\n")
gc(reset = TRUE)
t0 <- proc.time()
run_crop_spatialize(run_dir, input_dir, year_range)
elapsed <- (proc.time() - t0)[["elapsed"]]
cat(sprintf("\nMETRIC total_s=%.2f\n\n", elapsed))

# ── Pass 2: PROFILE (serial, short slice) ────────────────────────────────────
#
# Call .run_spatialize_year() directly — the actual per-year worker — in a
# plain lapply so Rprof captures the full call stack. This bypasses
# run_crop_spatialize() and the mclapply fork boundary entirely.

cat(
  "=== Pass 2: PROFILE (serial .run_spatialize_year, years ",
  min(profile_years), "-", max(profile_years), ") ===\n",
  sep = ""
)

# Load inputs — mirrors shared_chunk construction in run_crop_spatialize()
country_grid <- nanoparquet::read_parquet(
  file.path(input_dir, "country_grid.parquet")
)
country_areas <- nanoparquet::read_parquet(
  file.path(input_dir, "country_areas.parquet")
) |>
  dplyr::filter(year %in% profile_years)
crop_patterns <- nanoparquet::read_parquet(
  file.path(input_dir, "crop_patterns.parquet")
)
gridded_cropland <- nanoparquet::read_parquet(
  file.path(input_dir, "gridded_cropland.parquet")
) |>
  dplyr::filter(year %in% profile_years)

type_cl_path <- file.path(input_dir, "type_cropland.parquet")
type_cropland <- if (file.exists(type_cl_path)) {
  nanoparquet::read_parquet(type_cl_path) |>
    dplyr::filter(year %in% profile_years)
}

cft_mapping  <- .read_cft_mapping()
mapped_items <- cft_mapping$item_prod_code
country_areas <- dplyr::filter(country_areas, item_prod_code %in% mapped_items)
crop_patterns <- dplyr::filter(crop_patterns, item_prod_code %in% mapped_items)

irrig_rf_ratios <- tibble::tribble(
  ~cft_name,              ~yield_ratio, ~n_rate_ratio,
  "temperate_cereals",    1.3,          1.3,
  "rice",                 1.6,          1.4,
  "maize",                1.5,          1.4,
  "tropical_cereals",     1.3,          1.3,
  "pulses",               1.3,          1.2,
  "oil_crops_soybean",    1.3,          1.2,
  "oil_crops_groundnut",  1.3,          1.3,
  "oil_crops_sunflower",  1.3,          1.3,
  "oil_crops_rapeseed",   1.3,          1.3,
  "oil_crops_other",      1.3,          1.3,
  "sugarcane",            1.2,          1.3,
  "temperate_roots",      1.3,          1.3,
  "tropical_roots",       1.2,          1.2,
  "others_annual",        1.3,          1.3,
  "others_perennial",     1.2,          1.2
)
item_ratios <- cft_mapping |>
  dplyr::select(item_prod_code, cft_name) |>
  dplyr::inner_join(irrig_rf_ratios, by = "cft_name")

yields_file    <- file.path(input_dir, "country_yields.parquet")
yield_idx_file <- file.path(input_dir, "spatial_yield_index.parquet")
has_yields     <- file.exists(yields_file)
country_yields <- if (has_yields) nanoparquet::read_parquet(yields_file)
spatial_yield_idx <- if (has_yields && file.exists(yield_idx_file)) {
  nanoparquet::read_parquet(yield_idx_file)
}

n_inputs_file <- file.path(input_dir, "nitrogen_inputs.parquet")
has_nitrogen  <- file.exists(n_inputs_file)
n_rates       <- NULL
spatial_n_idx <- NULL
if (has_nitrogen) {
  n_inputs   <- nanoparquet::read_parquet(n_inputs_file)
  items_prod <- readr::read_csv(
    system.file("extdata", "items_prod.csv", package = "whep"),
    show_col_types = FALSE
  )
  n_rates <- n_inputs |>
    dplyr::filter(!is.na(kg_n_ha), kg_n_ha > 0) |>
    dplyr::summarize(
      kg_n_ha = sum(kg_n_ha, na.rm = TRUE),
      .by = c(year, area_code, crop_name, fert_type)
    ) |>
    dplyr::left_join(
      dplyr::select(items_prod, item_prod_code, item_prod_name),
      by = c("crop_name" = "item_prod_name")
    ) |>
    dplyr::filter(!is.na(item_prod_code))
  spatial_idx_file <- file.path(input_dir, "spatial_n_index.parquet")
  if (file.exists(spatial_idx_file)) {
    spatial_n_idx <- nanoparquet::read_parquet(spatial_idx_file)
  }
}

prof_chunk_dir <- file.path(tempdir(), "prof_crop_chunks")
if (!dir.exists(prof_chunk_dir)) dir.create(prof_chunk_dir, recursive = TRUE)
on.exit(unlink(prof_chunk_dir, recursive = TRUE), add = TRUE)

shared_prof <- list(
  country_areas     = country_areas,
  gridded_cropland  = gridded_cropland,
  type_cropland     = type_cropland,
  country_grid      = country_grid,
  crop_patterns     = crop_patterns,
  cft_mapping       = cft_mapping,
  item_ratios       = item_ratios,
  has_yields        = has_yields,
  country_yields    = country_yields,
  spatial_yield_idx = spatial_yield_idx,
  has_nitrogen      = has_nitrogen,
  n_rates           = n_rates,
  spatial_n_idx     = spatial_n_idx,
  chunk_dir         = prof_chunk_dir
)

gc(reset = TRUE)
prof_file <- tempfile("prof_crop_spatialize_", fileext = ".out")

Rprof(prof_file, interval = 0.02)
lapply(profile_years, \(yr) .run_spatialize_year(yr, shared_prof))
Rprof(NULL)

# ── Profile report ───────────────────────────────────────────────────────────

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

# ── Caller analysis ──────────────────────────────────────────────────────────

cat("CALLERS_START\n")
lines <- readLines(prof_file)
lines <- lines[-1]

targets <- c(
  "forderv", "bmerge", "copy", ".spatialize_year",
  "write_parquet", "bind_rows", ".run_spatialize_year"
)

for (target in targets) {
  caller_counts <- list()
  for (line in lines) {
    fns <- strsplit(line, " ")[[1]]
    idx <- which(fns == paste0('"', target, '"'))
    if (length(idx) == 0) next
    for (j in idx) {
      if (j < length(fns)) {
        caller <- fns[j + 1]
        key <- if (j + 1 < length(fns)) {
          paste0(caller, " <- ", fns[j + 2])
        } else {
          caller
        }
      } else {
        key <- '"<top>"'
      }
      caller_counts[[key]] <- (caller_counts[[key]] %||% 0L) + 1L
    }
  }
  if (length(caller_counts) == 0) next

  counts <- sort(unlist(caller_counts), decreasing = TRUE)
  total_samples <- sum(counts)
  interval <- 0.02
  cat(sprintf(
    "\n  === %s (%.1fs total) ===\n",
    target,
    total_samples * interval
  ))
  top_callers <- utils::head(counts, 10)
  for (i in seq_along(top_callers)) {
    cat(sprintf(
      "    %5.1f%% %5.1fs  %s\n",
      round(100 * top_callers[i] / total_samples, 1),
      top_callers[i] * interval,
      names(top_callers)[i]
    ))
  }
}
cat("\nCALLERS_END\n")
