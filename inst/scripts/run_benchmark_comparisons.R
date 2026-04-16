# -----------------------------------------------------------------------
# run_benchmark_comparisons.R
#
# Multi-comparison spatialize runs at 25-year benchmark years.
#
# Runs three configurations so cell-level maps can be diffed:
#   1. lpjml              — LandInG-faithful (no LUH2 type constraint)
#   2. whep               — full WHEP (LUH2 type-aware allocation)
#   3. lpjml_typeaware    — LPJmL base + type-aware override
#                           (isolates the effect of the constraint alone)
#
# Benchmark years: seq(1850, 2000, 25) intersected with availability.
# Each run writes to its own directory under
# <WHEP_L_FILES_DIR>/whep/spatialize/benchmark/, with a
# run_metadata.yaml alongside.
#
# Requires: WHEP_L_FILES_DIR environment variable pointing to L_files.
#
# Usage:
#   Rscript inst/scripts/run_benchmark_comparisons.R
# -----------------------------------------------------------------------

suppressPackageStartupMessages(library(whep))

l_files <- Sys.getenv("WHEP_L_FILES_DIR", unset = "")
if (!nzchar(l_files) || !dir.exists(l_files)) {
  stop(
    "WHEP_L_FILES_DIR is not set or does not exist. ",
    "Set it before running, e.g. ",
    "Sys.setenv(WHEP_L_FILES_DIR = '/path/to/L_files')."
  )
}

benchmarks <- c(1850L, 1875L, 1900L, 1925L, 1950L, 1975L, 2000L)
base_out <- file.path(l_files, "whep", "spatialize", "benchmark")

cli::cli_h1("Multi-comparison benchmark runs")
cli::cli_alert_info("Years: {.val {benchmarks}}")
cli::cli_alert_info("Output root: {.path {base_out}}")

t0 <- proc.time()

whep::run_spatialize(
  preset = "lpjml",
  years = benchmarks,
  out_dir = file.path(base_out, "lpjml")
)

whep::run_spatialize(
  preset = "whep",
  years = benchmarks,
  out_dir = file.path(base_out, "whep")
)

whep::run_spatialize(
  preset = "lpjml",
  years = benchmarks,
  overrides = list(use_type_constraint = TRUE),
  out_dir = file.path(base_out, "lpjml_typeaware")
)

elapsed <- (proc.time() - t0)[["elapsed"]]
cli::cli_alert_success(
  "All 3 runs complete in {round(elapsed / 60, 1)} min"
)
