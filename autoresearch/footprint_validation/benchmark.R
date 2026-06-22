#
# Deterministic validation benchmark for the footprint/production harness.
#
# Builds the case grid (top-N producers x years x crops x layers), extracts
# WHEP values, and judges them against the pinned ground-truth corpus. Prints:
#   - METRIC n_flag=.. n_pass=.. n_uncovered=.. correctness=..  (score)
#   - FLAGGED_JSON_START/END     cases where WHEP disagrees with ground truth
#   - UNCOVERED_JSON_START/END   cases with no pinned ground truth (need research)
#
# The orchestration (harness.workflow.js) parses these blocks. Lower n_flag is
# better; correctness = n_pass / (n_pass + n_flag) over covered cases.
#
# Usage:
#   Rscript autoresearch/footprint_validation/benchmark.R
# Set WHEP_VALIDATE_REFRESH=1 to rebuild the cached production.

suppressPackageStartupMessages({
  devtools::load_all(".")
  library(dplyr)
  library(readr)
})

here <- "autoresearch/footprint_validation"
source(file.path(here, "validate.R"))
source(file.path(here, "cases.R"))

# ── Config ────────────────────────────────────────────────────────────────
years <- c(1970L, 2000L, 2010L)
crops <- c("Wheat", "Rice")
top_n <- 5L
layers <- c("production", "area")

# ── Build (cached) ──────────────────────────────────────────────────────────
cache_dir <- ".whep_cache"
refresh <- Sys.getenv("WHEP_VALIDATE_REFRESH", "0") == "1"
span <- range(years)

production <- harness_build_or_cache(
  file.path(cache_dir, sprintf("primary_prod_%d_%d.rds", span[1], span[2])),
  function() {
    build_primary_production(start_year = span[1], end_year = span[2]) |>
      dplyr::select(
        "area_code",
        "item_prod_code",
        "item_cbs_code",
        "year",
        "unit",
        "value"
      )
  },
  refresh = refresh
)

# ── Generate + judge ─────────────────────────────────────────────────────────
lookups <- whep_validation_lookups()
cases <- generate_cases(production, lookups, years, crops, top_n, layers)
corpus <- read_ground_truth(file.path(here, "ground_truth.csv"))

verdicts <- run_validation(
  cases,
  list(production = production),
  corpus,
  lookups
)

# ── Report ────────────────────────────────────────────────────────────────
tally <- verdicts |> count(.data$verdict)
get_n <- function(v) {
  out <- tally$n[tally$verdict == v]
  if (length(out) == 0) 0L else out
}
n_flag <- get_n("flag_high") + get_n("flag_low")
n_pass <- get_n("pass")
n_unc <- get_n("uncovered")
covered <- n_pass + n_flag
correctness <- if (covered > 0) round(n_pass / covered, 4) else NA_real_

cat(sprintf(
  "METRIC n_flag=%d n_pass=%d n_uncovered=%d n_total=%d correctness=%s\n",
  n_flag,
  n_pass,
  n_unc,
  nrow(verdicts),
  format(correctness)
))

cat("\nVerdict tally:\n")
print(tally)

flagged <- verdicts |>
  dplyr::filter(.data$verdict %in% c("flag_high", "flag_low")) |>
  dplyr::select(
    "probe_id",
    "layer",
    "area_iso3",
    "item_name",
    "year",
    unit = "whep_unit",
    "whep_value",
    "gt_value",
    "ratio",
    "deviation_pct",
    "source",
    "url",
    "definition"
  )

uncovered <- verdicts |>
  dplyr::filter(.data$verdict == "uncovered") |>
  dplyr::select(
    "probe_id",
    "layer",
    "area_iso3",
    "item_name",
    "year",
    "element",
    unit = "whep_unit"
  )

cat("\nFLAGGED_JSON_START\n")
cat(jsonlite::toJSON(
  flagged,
  dataframe = "rows",
  auto_unbox = TRUE,
  pretty = TRUE
))
cat("\nFLAGGED_JSON_END\n")

cat("\nUNCOVERED_JSON_START\n")
cat(jsonlite::toJSON(
  uncovered,
  dataframe = "rows",
  auto_unbox = TRUE,
  pretty = TRUE
))
cat("\nUNCOVERED_JSON_END\n")
