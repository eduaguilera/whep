# Compare two full-range `get_wide_cbs()` builds, by magnitude AND by
# structure.
#
# WHY THIS EXISTS. Nothing in the test suite compares a CBS column total
# against anything, because a full-range build needs pins and ~9 minutes and
# so cannot live in `R CMD check`. That gap is how whep#425 shipped row counts
# in place of tonnes, and how whep#444 could report feed moving 7% with no way
# to localise it except hand-written throwaway code. This is that code, kept.
#
# WHY IT REPORTS STRUCTURE AS WELL AS TOTALS. whep#480 moved no value at all
# -- mass conserved, `polity_area_code` unchanged for all 266 areas -- and it
# still broke the build, because one bucket started coming out under two
# labels. "No number moved" is not evidence on its own; row counts, key counts
# and label counts are the other half.
#
# USAGE
#   Rscript --vanilla inst/scripts/compare_cbs_totals.R build <root> <out.rds>
#   Rscript --vanilla inst/scripts/compare_cbs_totals.R compare <a.rds> <b.rds>
#
# To isolate a data change from a code change, `git archive` the package into
# a scratch directory, replace the `data/*.rda` under test, and `build` from
# that root. Run each half alone: one measurement over two candidate causes
# attributes nothing (whep#418).
#
# RECORDED BASELINE -- `main` at 6e5af760, full range 1850-2023, 9.1 min:
#   rows 2,005,267   areas 195   feed 9.05014e11   production 3.00743e12
#   other_uses 9.17874e10   domestic_supply 3.14683e12   food 3.14340e11
#   processing 1.76770e12   import 1.11382e11   export 7.91341e10
#   seed 5.03169e10   stock_addition 3.93577e10   stock_withdrawal 1.46513e11

.cbs_totals_build <- function(pkg_root, out) {
  pkgload::load_all(pkg_root, quiet = TRUE)
  started <- Sys.time()
  wide <- get_wide_cbs()
  mins <- as.numeric(difftime(Sys.time(), started, units = "mins"))
  cli::cli_inform("Built in {round(mins, 2)} min.")
  saveRDS(wide, out, compress = FALSE)
  invisible(wide)
}

.cbs_value_cols <- function(wide) {
  setdiff(
    names(wide)[vapply(wide, is.numeric, logical(1))],
    c("year", "area_code", "polity_area_code", "item_cbs_code")
  )
}

# Count buckets emitted under more than one label IN A GIVEN YEAR. Across years a
# bucket legitimately carries several `reporting_polity_name` values, because the
# polity name is periodized -- so the year has to be in the key or this metric
# reports every relabelling as a split.
.cbs_split_labels <- function(x) {
  lab <- unique(x[, c("area_code", "year", "reporting_polity_name")])
  sum(table(lab$area_code, lab$year) > 1L)
}

.cbs_totals_structure <- function(a, b) {
  key <- c("year", "area_code", "item_cbs_code")
  data.frame(
    metric = c(
      "rows",
      "distinct keys",
      "duplicate keys",
      "areas",
      "items",
      "codes with >1 polity label"
    ),
    a = c(
      nrow(a),
      nrow(unique(a[, key])),
      nrow(a) - nrow(unique(a[, key])),
      length(unique(a$area_code)),
      length(unique(a$item_cbs_code)),
      .cbs_split_labels(a)
    ),
    b = c(
      nrow(b),
      nrow(unique(b[, key])),
      nrow(b) - nrow(unique(b[, key])),
      length(unique(b$area_code)),
      length(unique(b$item_cbs_code)),
      .cbs_split_labels(b)
    )
  )
}

.cbs_sum_by <- function(x, group, value) {
  stats::aggregate(
    x[[value]],
    by = list(g = x[[group]]),
    FUN = sum,
    na.rm = TRUE
  )
}

.cbs_totals_by <- function(a, b, group, value) {
  out <- merge(
    .cbs_sum_by(a, group, value),
    .cbs_sum_by(b, group, value),
    by = "g",
    all = TRUE,
    suffixes = c("_a", "_b")
  )
  out$x_a[is.na(out$x_a)] <- 0
  out$x_b[is.na(out$x_b)] <- 0
  out$delta <- out$x_b - out$x_a
  out$ratio <- out$x_b / out$x_a
  names(out)[names(out) == "g"] <- group
  out[order(-abs(out$delta)), ]
}

.cbs_totals_compare <- function(fa, fb) {
  a <- as.data.frame(readRDS(fa))
  b <- as.data.frame(readRDS(fb))

  cat("== structure\n")
  print(.cbs_totals_structure(a, b), row.names = FALSE)

  cat("\n== column totals\n")
  cols <- intersect(.cbs_value_cols(a), .cbs_value_cols(b))
  totals <- data.frame(
    column = cols,
    a = vapply(cols, \(n) sum(a[[n]], na.rm = TRUE), numeric(1)),
    b = vapply(cols, \(n) sum(b[[n]], na.rm = TRUE), numeric(1))
  )
  totals$ratio <- totals$b / totals$a
  print(totals, row.names = FALSE)

  for (value in c("feed", "production")) {
    cat("\n== top 10 areas by |", value, "| delta\n", sep = "")
    print(
      utils::head(.cbs_totals_by(a, b, "area_code", value), 10),
      row.names = FALSE
    )
    cat("\n== top 10 items by |", value, "| delta\n", sep = "")
    print(
      utils::head(.cbs_totals_by(a, b, "item_cbs_code", value), 10),
      row.names = FALSE
    )
  }
  invisible(NULL)
}

.cbs_totals_main <- function(args) {
  if (length(args) < 3L) {
    cli::cli_abort("Usage: compare_cbs_totals.R build|compare <arg1> <arg2>")
  }
  switch(
    args[1],
    build = .cbs_totals_build(args[2], args[3]),
    compare = .cbs_totals_compare(args[2], args[3]),
    cli::cli_abort(
      "Unknown mode {.val {args[1]}}; use {.val build} or {.val compare}."
    )
  )
}

if (sys.nframe() == 0L && !interactive()) {
  .cbs_totals_main(commandArgs(trailingOnly = TRUE))
}
