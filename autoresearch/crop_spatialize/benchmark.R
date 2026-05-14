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

l_files_dir <- "LPJmL_inputs"

source("inst/scripts/prepare_spatialize_all.R")

year_range <- 1850:2020L # METRIC pass: full range
profile_years <- 2000:2002L # PROFILE pass: short slice, serial

# ── Setup ───────────────────────────────────────────────────────────────────

run_dir <- file.path(l_files_dir, "whep")
input_dir <- file.path(run_dir, "inputs")

if (!dir.exists(input_dir)) {
  stop(
    "input_dir not found: ",
    input_dir,
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
# Run run_crop_spatialize() with n_workers = 1 so mclapply falls back to
# purrr::map. No forks → Rprof captures the full call stack across the real
# code path (per-year work + post-parallel CFT/nitrogen/yields helpers).

local({
  cat(
    "=== Pass 2: PROFILE (serial run_crop_spatialize, years ",
    min(profile_years),
    "-",
    max(profile_years),
    ") ===\n",
    sep = ""
  )

  gc(reset = TRUE)
  prof_file <- tempfile("prof_crop_spatialize_", fileext = ".out")

  Rprof(prof_file, interval = 0.02)
  run_crop_spatialize(run_dir, input_dir, profile_years, n_workers = 1L)
  Rprof(NULL)

  # ── Profile report ─────────────────────────────────────────────────────────

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

  targets <- c(
    "forderv",
    "bmerge",
    "copy",
    ".spatialize_year",
    "build_gridded_landuse",
    ".spatialize_nitrogen_chunk",
    ".spatialize_yields_chunk",
    ".write_lu_nc_chunk",
    ".write_nitrogen_nc_chunks",
    ".write_yields_nc_chunk",
    ".pft_nc_write_chunk",
    "ncvar_put"
  )

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
    if (length(caller_counts) == 0) {
      next
    }

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
})
