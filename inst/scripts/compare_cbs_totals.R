# Diff a full-range `get_wide_cbs()` against a committed baseline of COLUMN TOTALS.
#
# Why this exists, and why it is a script rather than a test: two changes in this cycle passed
# 5151 tests while inflating published values by up to 266x. Nothing in the suite compared a
# magnitude against anything, and the pipeline aggregates and joins, so a wrong grouping
# double-counts -- values up, rows flat or DOWN. Row count is the least sensitive detector of
# exactly the defect an aggregation change causes:
#
#   change                                        rows            food
#   summing areas that fold into one bucket        2.16M (-23%)    266x
#   promoting FABIO rest-of-world members          2.81M ( -0%)    1.0x, but feed 13.7x
#
# A full-range build costs several minutes, which is too slow for testthat but trivial next to
# the hours it takes to find one of these afterwards. Run it before committing any change to
# `.aggregate_to_polities()`, `polity_area_code`, a crosswalk, or a `data-raw` script that
# writes one.
#
# The baseline is data, not a target. A change that legitimately adds data moves totals by a
# fraction of a percent, uniformly. What the two withdrawals looked like instead was a handful
# of columns identical (processing, import, stocks) and others 13x -- so read the SHAPE of the
# diff, not just whether it is non-zero.
#
# Usage:
#   Rscript inst/scripts/compare_cbs_totals.R            # compare against the baseline
#   Rscript inst/scripts/compare_cbs_totals.R --write    # record a new baseline
#
# To attribute a diff, isolate code from data by swapping one at a time -- that is what found
# the second withdrawal, and it took one run each:
#   git checkout origin/main -- data/ && Rscript inst/scripts/compare_cbs_totals.R
# "branch code + main data reproduces main's totals" proves no R change moved a number.

suppressMessages({
  library(data.table)
  pkgload::load_all(quiet = TRUE)
})

args <- commandArgs(trailingOnly = TRUE)
write_mode <- "--write" %in% args
baseline_path <- file.path("inst", "scripts", "cbs_totals_baseline.csv")

o <- suppressWarnings(suppressMessages(get_wide_cbs()))
data.table::setDT(o)

value_cols <- names(o)[vapply(o, is.numeric, logical(1))]
value_cols <- sort(setdiff(
  value_cols,
  c("year", "area_code", "item_cbs_code", "polity_area_code")
))

current <- data.table::data.table(
  metric = c("rows", "areas", value_cols),
  value = c(
    nrow(o),
    data.table::uniqueN(o$area_code),
    vapply(value_cols, function(cl) sum(o[[cl]], na.rm = TRUE), numeric(1))
  )
)

if (write_mode) {
  data.table::fwrite(current, baseline_path)
  cli::cli_alert_success(
    "Baseline written to {.path {baseline_path}} ({nrow(current)} metrics)."
  )
  print(current)
  quit(status = 0)
}

if (!file.exists(baseline_path)) {
  cli::cli_abort(c(
    "No baseline at {.path {baseline_path}}.",
    "i" = "Record one with {.code Rscript inst/scripts/compare_cbs_totals.R --write}."
  ))
}

baseline <- data.table::fread(baseline_path)
cmp <- merge(baseline, current, by = "metric", all = TRUE, suffixes = c(
  "_baseline",
  "_current"
))
cmp[, ratio := value_current / value_baseline]
cmp[, pct := 100 * (ratio - 1)]
data.table::setorder(cmp, -ratio)

cat("\n")
print(cmp[, .(
  metric,
  baseline = value_baseline,
  current = value_current,
  ratio = round(ratio, 4),
  pct = round(pct, 3)
)])

# 1% is the band the legitimate changes in this cycle occupied -- label resolutions and
# residue area codes adding data. Anything outside it is a magnitude change and needs an
# explanation before it ships, not after.
off <- cmp[which(is.na(ratio) | abs(ratio - 1) > 0.01)]
if (nrow(off) > 0L) {
  cli::cli_abort(c(
    "{nrow(off)} metric{?s} {?is/are} more than 1% from the baseline.",
    "x" = "{.val {off$metric}}",
    "i" = "A uniform sub-1% shift is added data. A few columns at 13x while others are
       unchanged is a double-count or a fan-out -- read the shape.",
    "i" = "Isolate code from data before diagnosing: {.code git checkout origin/main -- data/}."
  ))
}
cli::cli_alert_success(
  "All {nrow(cmp)} metrics within 1% of the baseline."
)
