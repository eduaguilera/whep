# -----------------------------------------------------------------------
# run_benchmark_comparisons.R
#
# Multi-comparison spatialize runs at 10-year benchmark years.
#
# Runs three configurations so cell-level maps can be diffed:
#   1. lpjml              — LandInG-faithful (no LUH2 type constraint)
#   2. whep               — full WHEP (LUH2 type-aware allocation)
#   3. lpjml_typeaware    — LPJmL base + type-aware override
#                           (isolates the effect of the constraint alone)
#
# Benchmark years: seq(1850, 2020, 10) intersected with availability.
# Each run writes to its own directory under
# <l_files_dir>/whep/spatialize/benchmark/, with a
# run_metadata.yaml alongside.
#
# Usage:
#   Rscript inst/scripts/run_benchmark_comparisons.R
# -----------------------------------------------------------------------

suppressPackageStartupMessages(library(whep))

# Set this to your L_files directory path
l_files <- "LPJmL_inputs" # <-- CHANGE THIS

if (!dir.exists(l_files)) {
  stop("l_files directory does not exist: ", l_files)
}

benchmarks <- as.integer(seq(1850L, 2020L, by = 10L))
base_out <- file.path(l_files, "whep", "spatialize", "benchmark")

cli::cli_h1("Multi-comparison benchmark runs")
cli::cli_alert_info("Years: {.val {benchmarks}}")
cli::cli_alert_info("Output root: {.path {base_out}}")

t0 <- proc.time()

whep::run_spatialize(
  preset = "lpjml",
  years = benchmarks,
  paths = list(l_files_dir = l_files, out_dir = file.path(base_out, "lpjml"))
)

whep::run_spatialize(
  preset = "whep",
  years = benchmarks,
  paths = list(l_files_dir = l_files, out_dir = file.path(base_out, "whep"))
)

whep::run_spatialize(
  preset = "lpjml",
  years = benchmarks,
  overrides = list(use_type_constraint = TRUE),
  paths = list(
    l_files_dir = l_files,
    out_dir = file.path(base_out, "lpjml_typeaware")
  )
)

elapsed <- (proc.time() - t0)[["elapsed"]]
cli::cli_alert_success(
  "All 3 runs complete in {round(elapsed / 60, 1)} min"
)
