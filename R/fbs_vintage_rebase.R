# Which Food Balance Sheet vintage sets the level of an item (whep#1495).
#
# `.select_best_source()` treats `faostat-fbs-new` as the reference and scales
# `faostat-fbs-old` up or down to it over the 2010-2013 overlap. For FBS item
# 2551 "Nuts and products" the new vintage is not a coherent mass basis, so
# that direction carries its defect back to 1961.
#
# Measured on the pins, 2010, areas below 5000 with 351 excluded:
#
#   element   faostat-fbs-new   faostat-fbs-old
#   import          13,566 kt          6,450 kt
#   export           7,690 kt          6,508 kt
#   food            19,473 kt         14,876 kt
#
# World production of the ten species is 14,292 kt in shell (`faostat-production`,
# element 5510), so the new vintage eats more nuts than the world grew, and its
# world imports are 1.76x its world exports (1.65x to 1.70x every year to 2022).
# A closed world cannot import what nobody exported. The old vintage closes to
# 0.99x. Protein supply is about the same in both (0.89 vs 0.97 Mt), so the
# extra mass carries no protein: it is a basis, not more nuts.
#
# Where it comes from, regressing each country's FBS 2551 trade on its
# species-level trade in `faostat-trade-totals` (166 importers, R^2 0.97; 151
# exporters, R^2 0.89): in-shell trade passes through at about 1:1 on both
# sides (0.88 import, 1.01 export), but kernels and prepared nuts are scaled up
# far more on the import side than on the export side -- cashew kernels (230)
# 9.8 vs 3.8, almond kernels (231) 4.9 vs 1.6, hazelnut kernels (233) 5.8 vs
# 1.1, prepared nuts (235) 9.7 vs 3.0. The importer's standardisation to
# in-shell equivalent is not the exporter's, so the two sides of one flow
# land on different bases.
#
# `"old_fbs"` therefore takes the level from the old vintage and only the
# change over time from the new one -- the same construction the palm-kernel
# fix applies (`.fix_palm_kernels()`: old-vintage processing, carried past 2013
# by the new vintage's palm-kernel-oil growth). Per key, the factor is
# median(old / new) over the overlap, clamped to the band and minimum overlap
# the forward scaling uses (`.fbs_scale_ratio_bounds()`). The new vintage is
# multiplied by it from 2014 on, the old vintage is kept as reported up to
# 2013, and the old vintage is NOT scaled to the new one for these items. At
# 2010 that gives world imports 6.3 Mt against exports 6.4 Mt (0.98x) and food
# 15.2 Mt; at 2022, 11.8 Mt against 12.1 Mt (0.97x). `"new_fbs"` keeps the
# published new vintage and the forward scaling: the behaviour before whep#1495.
#
# The same import-side excess shows in other items of the new vintage (milk
# excluding butter 1.93x, pineapples 1.75x, olives 2.53x at 2010); they are
# not listed here because each is its own call (see the follow-up issue named
# in the PR for whep#1495).

.nut_basis_choices <- function() {
  c("old_fbs", "new_fbs")
}

# FBS items whose level comes from the old vintage under `"old_fbs"`.
.fbs_rebase_items <- function() {
  2551L
}

# The years both vintages report, from which the factors are taken.
.fbs_overlap_years <- function() {
  2010:2013
}

# One factor per (area_code, item_cbs_code, element): median(old / new) over
# the overlap, clamped. Empty under `"new_fbs"`, and when the build reads no
# new-vintage row of the items, because then there is nothing to rebase.
#
# The overlap is read from the pins when the frames this build loaded do not
# span it, so the factor -- and with it every 2014+ value -- does not depend on
# the years a build asks for.
.fbs_rebase_factors <- function(fbs_new, fbs_old, nut_basis = "old_fbs") {
  nut_basis <- rlang::arg_match(nut_basis, .nut_basis_choices())
  items <- .fbs_rebase_items()
  if (nut_basis == "new_fbs" || !any(fbs_new$item_cbs_code %in% items)) {
    return(.empty_fbs_rebase_factors())
  }
  overlap <- .fbs_overlap_frames(fbs_new, fbs_old)
  keys <- c("area_code", "item_cbs_code", "element", "year")
  new <- .fbs_rebase_rows(overlap$new)
  old <- .fbs_rebase_rows(overlap$old)
  data.table::setnames(new, "value", "new")
  data.table::setnames(old, "value", "old")
  both <- merge(new, old, by = keys)[new != 0 & !is.na(old)]
  if (nrow(both) == 0L) {
    return(.empty_fbs_rebase_factors())
  }
  bounds <- .fbs_scale_ratio_bounds()
  factors <- both[,
    .(n_overlap = .N, ratio = stats::median(old / new)),
    by = c("area_code", "item_cbs_code", "element")
  ][n_overlap >= bounds$min_overlap & is.finite(ratio)]
  factors[, rebase_old_new := pmin(pmax(ratio, bounds$lower), bounds$upper)]
  factors[, c("n_overlap", "ratio") := NULL]
  factors[]
}

.empty_fbs_rebase_factors <- function() {
  data.table::data.table(
    area_code = integer(),
    item_cbs_code = integer(),
    element = character(),
    rebase_old_new = double()
  )
}

# The rebased items' overlap rows of one vintage, summed per key.
.fbs_rebase_rows <- function(fbs) {
  dt <- data.table::as.data.table(fbs)
  dt <- dt[
    item_cbs_code %in% .fbs_rebase_items() &
      year %in% .fbs_overlap_years() &
      !is.na(value)
  ]
  dt <- dt[,
    .(value = sum(value)),
    by = c("area_code", "item_cbs_code", "element", "year")
  ]
  dt[, item_cbs_code := as.integer(item_cbs_code)]
  dt[, area_code := as.integer(area_code)]
  dt[]
}

# Both vintages over the overlap: the frames already loaded when they span it,
# otherwise a read of the overlap years alone.
.fbs_overlap_frames <- function(fbs_new, fbs_old) {
  years <- .fbs_overlap_years()
  if (.spans_years(fbs_new, years) && .spans_years(fbs_old, years)) {
    return(list(new = fbs_new, old = fbs_old))
  }
  list(
    new = .extract_cb("faostat-fbs-new", years = years),
    old = .extract_cb("faostat-fbs-old", years = years)
  )
}

.spans_years <- function(df, years) {
  !is.null(df) && all(years %in% df$year)
}

# Put the rebased items on the old vintage's level inside the source pivot of
# `.select_best_source()`. A key with a factor drops its new-vintage value in
# a year the old vintage reports, and multiplies it by the factor otherwise;
# `rebased` marks the second case so it is labelled as derived.
.apply_fbs_rebase <- function(wide, rebase) {
  wide[, rebased := FALSE]
  if (is.null(rebase) || nrow(rebase) == 0L) {
    return(wide)
  }
  on <- c("area_code", "item_cbs_code", "element")
  wide[rebase, rebase_old_new := i.rebase_old_new, on = on]
  wide[
    !is.na(rebase_old_new),
    `:=`(
      FAOSTAT_FBS_New = data.table::fifelse(
        is.na(FAOSTAT_FBS_Old),
        FAOSTAT_FBS_New * rebase_old_new,
        NA_real_
      ),
      rebased = is.na(FAOSTAT_FBS_Old) & !is.na(FAOSTAT_FBS_New)
    )
  ]
  wide[, rebase_old_new := NULL]
  wide[]
}

# Warn when `"old_fbs"` was asked for and an item it covers reaches the build
# from the new vintage with no factor at all: the old vintage or the overlap
# was absent, and every value of that item stays as FAOSTAT published it.
.warn_fbs_rebase_absent <- function(fbs_new, rebase, nut_basis) {
  if (nut_basis == "new_fbs" || is.null(fbs_new)) {
    return(invisible(NULL))
  }
  present <- unique(as.integer(
    fbs_new$item_cbs_code[fbs_new$item_cbs_code %in% .fbs_rebase_items()]
  ))
  missing <- setdiff(present, as.integer(rebase$item_cbs_code))
  if (length(missing) > 0L) {
    codes <- as.character(missing)
    cli::cli_warn(
      c(
        "{.arg nut_basis} is {.val old_fbs}, but {cli::qty(length(codes))}FBS
         item{?s} {.val {codes}} {?has/have} no old-vintage factor.",
        "i" = "Both vintages must report {min(.fbs_overlap_years())} to
          {max(.fbs_overlap_years())}; without that the item keeps the new
          vintage's published level."
      ),
      class = "whep_fbs_rebase_absent"
    )
  }
  invisible(NULL)
}
