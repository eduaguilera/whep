# Diff a full-range `get_wide_cbs()` against a committed baseline of COLUMN TOTALS.
#
# Why this exists, and why it is a script rather than a test: two changes in this cycle passed
# the entire suite while moving published values by up to 266x. Nothing in the suite compared a
# magnitude against anything, and row count is the least sensitive detector of what an
# aggregation change does -- values move while rows stay flat or fall:
#
#   change                                        rows            food
#   relabelling `area` + summing folded areas      2.16M (-23%)    266x
#   promoting FABIO rest-of-world members          2.81M ( -0%)    1.0x, but feed 13.7x
#
# The first row is a BUNDLE, and reading it as one change cost hours. I attributed the 266x to
# the summing and wrote that into seven places as fact. Isolated, the relabelling owns all of it
# -- it created two vocabularies for `area`, a join key, and dropped 702,166 rows -- while the
# summing is harmless, and summing at the cast is what shipped as the fix for whep#425. One
# measurement over two candidate causes attributes nothing; run each half alone.
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
# THE BUILD IS NOT REPRODUCIBLE ACROSS SESSIONS, which is why the band is 1% and not zero. Six
# full-range runs on an unchanged tree gave `rows` of 2,768,578 or 2,764,471 -- a swing of 4,107
# rows (0.148%) -- with values wobbling in the fourth decimal alongside. Tracked in whep#420.
#
# It is LOCALISED but not yet explained, and three tidy stories about it have been wrong. At a
# window exercising the pre-1961 path (1950-1965), eight runs gave 1008989, 1008403, 1002576,
# 1002444, 1002444, 1002444, 1004980, 1002576 -- a ~0.65% spread with repeats. The first four
# looked monotonic, the middle three looked converged, and neither held. Assume no structure in
# small samples of this.
#
# What holds: 1990-1993 is byte-identical over six runs, so the variance needs the pre-1961 path;
# `build_primary_production()` is identical over five sessions, so area resolution is not
# involved; and it is deterministic WITHIN a session. The cached payload files are dated
# 2026-03-25 and unchanged -- only the small `data.txt` index is re-fetched -- so it is not the
# input data changing. 1950-1965 reproduces it in ~3 minutes, which is the loop to use.
#
# This baseline may therefore differ by ~0.15% from a fresh run for reasons that have nothing to
# do with your change. That is what the 1% band is for; do not tighten it without fixing #420.
#
# WITHIN a session it is bit-identical, which narrows the cause and rules out the obvious
# suspects. Two builds in one session with `whep_clear_cache()` between them took 410s and 389s
# -- both genuinely ran -- and agreed to ten significant figures. So nothing here is
# order-dependent or thread-dependent, and the duplicate keys of whep#425 are NOT the cause, though I
# first guessed they were.
#
# I also claimed it was not the pins cache, on the strength of
# `find ~/.cache/pins -newermt "-40 minutes"` returning 0. `bfs` does not accept that relative
# form and the command measured nothing -- 36 entries had in fact been refreshed. Retracted.
#
# If you repeat that experiment, clear the cache. Without it the second build takes 0.0s and
# returns the first one's result, so "identical" measures nothing -- which is how my first
# attempt at it produced a confident wrong answer.
#
# BISECTED BY STAGE: `build_primary_production()` at full range is identical across FIVE separate
# sessions (6,168,623 rows, value 5.781364289e+12), so the divergence is downstream of it, inside
# the commodity-balance build. That also clears the polities work of suspicion -- area resolution,
# the crosswalk and the contract all run in production too, and production does not move.
#
# Two consequences for anyone using this script. A `rows` diff under ~0.2% says nothing at all,
# so do not chase one. And any comparison that hinges on a few thousand rows -- including
# against `main` -- is inside the noise and needs repeat runs before it means anything. The
# defects this script exists to catch are 13x to 266x, orders of magnitude clear of it.
#
# THE BASELINE WAS RE-RECORDED AFTER whep#425 WAS FIXED (2026-07-31). The previous one pinned a
# corrupted build: `dcast()` with no `fun.aggregate` fell back to `length()` and replaced every
# cast value with a row count. Re-recording measured the fix, and it landed on the predicted
# magnitudes to three digits:
#
#   food 259.00x   seed 182.69x   stock_addition 16.90x   stock_withdrawal 15.98x
#   feed 11.89x    other_uses 11.50x   import 7.09x   export 2.63x
#   domestic_supply 1.86x   production 1.77x   processing 1.08x   areas 1.00x
#   rows 0.7216x (2,768,578 -> 1,997,944)
#
# The row DROP is the fix working, not data lost: counts are never zero, so the `value != 0`
# filter had nothing to remove; with quantities restored it removes ~390k genuine zeros.
# `processing` at 1.08x and `areas` at 1.00x are the tell that this was a value defect rather
# than a structural one -- the frame's shape barely moved while its contents changed by 259x.
#
# Even so, this script detects DRIFT. It does not certify values: a baseline is only ever as
# good as the build that produced it, which is what the previous one demonstrated. Read the
# SHAPE of a diff and ask whether the magnitudes are physically plausible.
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

.comparison <- function(baseline, current) {
  cmp <- merge(
    baseline,
    current,
    by = "metric",
    all = TRUE,
    suffixes = c("_baseline", "_current")
  )
  cmp[, ratio := value_current / value_baseline]
  cmp[, pct := 100 * (ratio - 1)]
  data.table::setorder(cmp, -ratio)
  cmp
}

.print_comparison <- function(cmp) {
  cat("\n")
  print(cmp[, .(
    metric,
    baseline = value_baseline,
    current = value_current,
    ratio = round(ratio, 4),
    pct = round(pct, 3)
  )])
}

# `--write` SHOWS WHAT IT IS ABOUT TO OVERWRITE. Re-recording blind is how the previous
# baseline came to certify a corrupted build: whoever ran it saw only "written, 13 metrics"
# and had no prompt to ask whether the numbers were plausible. Printing the diff first makes
# a 259x jump impossible to record without seeing it.
if (write_mode) {
  if (file.exists(baseline_path)) {
    .print_comparison(.comparison(data.table::fread(baseline_path), current))
    cli::cli_alert_info(
      "Above: the OUTGOING baseline against this run. Overwriting it now."
    )
  }
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

cmp <- .comparison(data.table::fread(baseline_path), current)
.print_comparison(cmp)

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
