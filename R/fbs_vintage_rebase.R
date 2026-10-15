# FBS item 2551 trade on the old vintage's level (whep#1495).
#
# `.select_best_source()` takes `faostat-fbs-new` as the reference from 2010 on
# and scales `faostat-fbs-old` to it. For FBS item 2551 "Nuts and products"
# the new vintage's trade is not one mass basis. Measured on the pins, 2010,
# areas below 5000 with 351 excluded:
#
#   element   faostat-fbs-new   faostat-fbs-old
#   import          13,566 kt          6,450 kt
#   export           7,690 kt          6,508 kt
#   food            19,473 kt         14,876 kt
#
# World production of the ten species is 14,292 kt in shell (`faostat-production`,
# element 5510), so the new vintage eats more nuts than the world grew, and its
# world imports are 1.76x its world exports (1.62x to 1.70x every year to
# 2022). A closed world cannot import what nobody exported; the old vintage
# closes to 0.99x. Protein supply is about the same in both (0.89 vs 0.97 Mt),
# so the extra mass carries no protein.
#
# Where it comes from, regressing each country's FBS 2551 trade on its
# species-level trade in `faostat-trade-totals` (166 importers, R^2 0.97; 151
# exporters, R^2 0.89): in-shell trade passes through at about 1:1 on both
# sides (0.88 import, 1.01 export), but kernels and prepared nuts are scaled up
# far more on the import side than on the export side -- cashew kernels (230)
# 9.8 vs 3.8, almond kernels (231) 4.9 vs 1.6, hazelnut kernels (233) 5.8 vs
# 1.1, prepared nuts (235) 9.7 vs 3.0. One flow lands on two bases, and the
# importer's excess goes on into its food.
#
# The correction, per (area, year) of the new vintage, 2010 on:
#
# * import and export are multiplied by the area's median old/new ratio over
#   the 2010-2013 overlap -- the old vintage's level, the new vintage's change
#   over time, which is how the palm-kernel fix already joins the two vintages
#   (`.fix_palm_kernels()`). The ratio is clamped to the forward scaling's
#   lower bound and to 1: the defect only inflates, so the correction only
#   removes mass;
# * the net trade it removes (import cut minus export cut) comes out of food
#   and domestic supply, where the excess went. Production, stock variation
#   and the other destinies are not touched, so the row's own supply-use
#   identity holds exactly as published;
# * a row whose net cut is not positive, or exceeds its food, is left as
#   published: there the old vintage is not a credible anchor (Viet Nam's
#   cashew imports are 274 kt in the new vintage and about 20 kt in the old,
#   while its trading partners' exports back the new one).
#
# The old vintage is then scaled to the corrected new one by the forward
# scaling, as for every other item, so 1961-2009 follows.
#
# Measured on the pins at 2010, world imports fall from 13.57 to 6.88 Mt
# against exports of 6.37 Mt (1.08x; 1.01x at 2016) and food from 19.47 to
# 14.11 Mt, under the 14.29 Mt grown in shell. FBS's own protein over that food
# is 62.9 g/kg, against FAO's in-shell basket of 64.7 (`build_food_supply()`,
# whep#1453) and the old vintage's 66.2; over the published food it was 45.6.
#
# The same import-side excess shows in other items of the new vintage (milk
# excluding butter 1.93x, pineapples 1.75x, olives 2.53x at 2010). Each is its
# own call and none is listed here.

.nut_basis_choices <- function() {
  c("rebased", "as_published")
}

# FBS items whose trade is put on the old vintage's level under `"rebased"`.
.fbs_rebase_items <- function() {
  2551L
}

# The years both vintages report, from which the factors are taken.
.fbs_overlap_years <- function() {
  2010:2013
}

# One factor per (area_code, item_cbs_code, element) for import and export:
# median(old / new) over the overlap, clamped to [lower bound, 1]. Empty under
# `"as_published"`, and when the build reads no new-vintage row of the items,
# because then there is nothing to rebase.
#
# The overlap is read from the pins when the frames this build loaded do not
# span it, so the factor -- and with it every 2014+ value -- does not depend on
# the years a build asks for.
.fbs_rebase_factors <- function(fbs_new, fbs_old, nut_basis = "rebased") {
  nut_basis <- rlang::arg_match(nut_basis, .nut_basis_choices())
  if (
    nut_basis == "as_published" ||
      !any(fbs_new$item_cbs_code %in% .fbs_rebase_items())
  ) {
    return(.empty_fbs_rebase_factors())
  }
  overlap <- .fbs_overlap_frames(fbs_new, fbs_old)
  keys <- c("area_code", "item_cbs_code", "element", "year")
  new <- .fbs_rebase_rows(overlap$new)
  old <- .fbs_rebase_rows(overlap$old)
  data.table::setnames(new, "value", "new")
  data.table::setnames(old, "value", "old")
  both <- merge(new, old, by = keys)[new != 0]
  if (nrow(both) == 0L) {
    return(.empty_fbs_rebase_factors())
  }
  bounds <- .fbs_scale_ratio_bounds()
  factors <- both[,
    .(n_overlap = .N, ratio = stats::median(old / new)),
    by = c("area_code", "item_cbs_code", "element")
  ][n_overlap >= bounds$min_overlap & is.finite(ratio)]
  factors[, rebase_old_new := pmin(pmax(ratio, bounds$lower), 1)]
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

# The rebased items' rows of one vintage for some elements, summed per key,
# with integer codes so the two vintages and the factors join on one type.
.fbs_rebase_rows <- function(
  fbs,
  elements = c("import", "export"),
  years = .fbs_overlap_years()
) {
  dt <- data.table::as.data.table(fbs)
  dt <- dt[
    item_cbs_code %in%
      .fbs_rebase_items() &
      element %in% elements &
      year %in% years &
      !is.na(value)
  ]
  dt[,
    .(value = sum(value)),
    by = .(
      area_code = as.integer(area_code),
      item_cbs_code = as.integer(item_cbs_code),
      element,
      year
    )
  ]
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

# Apply the factors to the new vintage. Returns it with the corrected values
# and, as the `rebased_keys` attribute, the (year, area_code, item_cbs_code,
# element) keys whose value moved, so their selected rows can be labelled.
.rebase_fbs_trade <- function(fbs_new, factors) {
  if (is.null(factors) || nrow(factors) == 0L) {
    return(fbs_new)
  }
  # A copy: the frame can be the `.cb_extracts` attribute of the caller's
  # `primary_all`, which a by-reference update would rewrite.
  dt <- data.table::copy(data.table::as.data.table(fbs_new))
  moved <- .fbs_rebase_corrections(dt, factors)
  dt[, `:=`(
    area_int = as.integer(area_code),
    item_int = as.integer(item_cbs_code)
  )]
  dt[
    moved,
    value := i.value,
    on = c(
      year = "year",
      area_int = "area_code",
      item_int = "item_cbs_code",
      element = "element"
    )
  ]
  dt[, c("area_int", "item_int") := NULL]
  data.table::setattr(dt, "rebased_keys", moved[, !"value"])
  dt
}

# The corrected long rows: one per (year, area, item, element) that moves.
.fbs_rebase_corrections <- function(dt, factors) {
  keys <- c("year", "area_code", "item_cbs_code")
  long <- .fbs_rebase_rows(
    dt,
    elements = c("import", "export", "food", "domestic_supply"),
    years = unique(dt$year)
  )
  if (nrow(long) == 0L) {
    return(long)
  }
  wide <- data.table::dcast(
    long,
    year + area_code + item_cbs_code ~ element,
    value.var = "value"
  )
  wide <- .add_rebase_factor(wide, factors, "import")
  wide <- .add_rebase_factor(wide, factors, "export")
  corrected <- data.table::melt(
    .fbs_rebase_balance(wide),
    id.vars = keys,
    variable.name = "element",
    variable.factor = FALSE,
    value.name = "corrected"
  )
  moved <- merge(long, corrected, by = c(keys, "element"))
  moved[value != corrected, c(keys, "element", "corrected"), with = FALSE] |>
    data.table::setnames("corrected", "value")
}

# The area's factor for one element as an `r_<element>` column, 1 where the
# area has none.
.add_rebase_factor <- function(wide, factors, element_name) {
  col <- paste0("r_", element_name)
  f <- factors[element == element_name, !"element"]
  wide[, (col) := 1]
  wide[f, (col) := i.rebase_old_new, on = c("area_code", "item_cbs_code")]
  wide
}

# Cut the trade, take the net cut out of food and domestic supply, and keep a
# row as published where the net cut is not positive or exceeds its food.
.fbs_rebase_balance <- function(wide) {
  wide <- ensure_columns(
    tibble::as_tibble(wide),
    tibble::tibble(
      import = double(),
      export = double(),
      food = double(),
      domestic_supply = double()
    )
  ) |>
    data.table::as.data.table()
  wide[, cut_import := data.table::fcoalesce(import, 0) * (1 - r_import)]
  wide[, cut_export := data.table::fcoalesce(export, 0) * (1 - r_export)]
  wide[, net_cut := cut_import - cut_export]
  wide[, ok := net_cut > 0 & net_cut <= data.table::fcoalesce(food, 0)]
  wide[
    (ok),
    `:=`(
      import = import - cut_import,
      export = export - cut_export,
      food = food - net_cut,
      domestic_supply = domestic_supply - net_cut
    )
  ]
  wide[, .(
    year,
    area_code,
    item_cbs_code,
    import,
    export,
    food,
    domestic_supply
  )]
}

# Label the selected rows whose value is a rebased new-vintage value.
.label_fbs_rebased <- function(wide, rebased) {
  if (is.null(rebased) || nrow(rebased) == 0L) {
    return(wide)
  }
  wide[, `:=`(
    area_int = as.integer(area_code),
    item_int = as.integer(item_cbs_code)
  )]
  wide[
    rebased,
    is_rebased := TRUE,
    on = c(
      year = "year",
      area_int = "area_code",
      item_int = "item_cbs_code",
      element = "element"
    )
  ]
  wide[
    is_rebased %in% TRUE & source == "FAOSTAT_FBS_New",
    source := "FAOSTAT_FBS_New_rebased"
  ]
  wide[, c("area_int", "item_int", "is_rebased") := NULL]
  wide[]
}

# Warn when `"rebased"` was asked for and an item it covers reaches the build
# from the new vintage with no factor at all: the old vintage or the overlap
# was absent, and every value of that item stays as FAOSTAT published it.
.warn_fbs_rebase_absent <- function(fbs_new, factors, nut_basis) {
  if (nut_basis == "as_published" || is.null(fbs_new)) {
    return(invisible(NULL))
  }
  present <- unique(as.integer(
    fbs_new$item_cbs_code[fbs_new$item_cbs_code %in% .fbs_rebase_items()]
  ))
  missing <- setdiff(present, as.integer(factors$item_cbs_code))
  if (length(missing) > 0L) {
    codes <- as.character(missing)
    cli::cli_warn(
      c(
        "{.arg nut_basis} is {.val rebased}, but {cli::qty(length(codes))}FBS
         item{?s} {.val {codes}} {?has/have} no old-vintage factor.",
        "i" = "Both vintages must report {min(.fbs_overlap_years())} to
          {max(.fbs_overlap_years())}; without that the item keeps the new
          vintage's published trade."
      ),
      class = "whep_fbs_rebase_absent"
    )
  }
  invisible(NULL)
}
