# FBS trade on the old vintage's level (whep#1495, whep#1550).
#
# Item 2551 first, where the defect was found:
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
# World production of the ten species is 14,292 kt in shell
# (`faostat-production`, element 5510), so the new vintage eats more nuts than
# the world grew, and its world imports are 1.76x its world exports (1.62x to
# 1.70x every year to 2022). A closed world cannot import what nobody
# exported; the old vintage closes to 0.99x. Protein supply is about the same
# in both (0.89 vs 0.97 Mt), so the extra mass carries no protein.
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
# The same import-side excess shows in other items of the new vintage, and
# `.fbs_rebase_destinies()` extends the rebase to those where the same
# mechanism was verified (whep#1550). See there for the evidence per item.

# `"rebased_no_milk"` leaves milk as published; `"nuts_only"` is the whep#1495
# rebase alone, the behaviour before whep#1550.
.fbs_trade_basis_choices <- function() {
  c("rebased", "rebased_no_milk", "nuts_only", "as_published")
}

# The FBS items whose trade is put on the old vintage's level, and the
# destinies the net trade removed comes out of: the ones the new vintage's
# import excess went into. Measured on the pins, 2010, areas below 5000 with
# 351 excluded, world imports / exports in the new vintage (old vintage):
#
#   2551 Nuts and products               1.76 (0.99)  food        whep#1495
#   2848 Milk - Excluding Butter         1.93 (0.93)  food, feed  whep#1550
#   2618 Pineapples and products         1.75 (0.99)  food        whep#1550
#   2563 Olives (including preserved)    2.53 (1.07)  food        whep#1550
#   2601 Tomatoes and products           1.29 (0.87)  food        whep#1550
#   2612 Lemons, Limes and products      1.25 (0.97)  food        whep#1550
#
# The mechanism is the one found for nuts, checked per item by regressing each
# country's FBS trade on its product-level trade in `faostat-trade-totals`
# (2010, no intercept; R^2 0.93 to 1.00). The primary form passes through at
# about the same factor on both sides, but processed forms are converted to
# primary equivalent with far larger factors on the import side, while the
# old vintage converts both sides alike. Import vs export factor, new vintage
# (old vintage):
#
# * milk: cheese 31.0 vs 7.7 (6.8 vs 9.9), dry whey 30.6 vs 2.3 (-0.7 vs 1.6);
# * pineapples: canned 3.1 vs -0.5 (0.65 vs 0.63), concentrated juice 28.3 vs
#   13.2 (4.2 vs 4.6);
# * olives: preserved 3.15 vs 0.98 (1.00 vs 0.98);
# * tomatoes: peeled 5.9 vs 1.3 (1.1 vs 1.3), paste 5.2 vs 3.8 (3.5 vs 3.8);
# * lemons: concentrated juice 22.2 vs 11.3 (11.6 vs 10.7).
#
# Milk's excess is mostly feed, and its cut is taken from there first (decision
# whep-fbs-new-milk-trade-rebase-default; the trade cut is settled, the
# feed-first order is PROVISIONAL, resting on FAO's own guidelines alone).
#
# * The import side is the inflated one. Product trade balances: FAOSTAT 2010
#   world imports of whole-cow-milk cheese are 4.73 Mt against 4.96 Mt of
#   exports, of dry whey 1.86 against 2.04 Mt (faostat-trade-totals, World),
#   and UN Comtrade's mirror sums agree (HS 0406 0.94x, HS 0404 1.05x). FAO's
#   own cheese extraction rates are 10-15% (Technical Conversion Factors:
#   Netherlands 10, Russia 12, Germany 15), so at most 10 t of milk per t of
#   cheese, as on the export side (7.7), not the import side's 31.0. IDF puts
#   world dairy trade at 51.9 Mt in 2010, about 7% of farm milk (IDF World
#   Dairy Situation 2011, via IDFA), where the new vintage's imports are
#   193.7 Mt against 97.6 Mt of exports (World row; old vintage 96.6 vs
#   104.1 Mt).
# * Whey is cheese's co-product (FAO 2017, Guidelines for the Compilation of
#   Food Balance Sheets, p. 65), and FAO standardises trade "only for the
#   principal derived product" (p. 105) and does not standardise co-product
#   feed (p. 107). The new vintage's import side converts dry whey at 30.6 t
#   of milk per t (export side 2.3), and that mass lands in feed: China
#   imports 265 kt of whey (UN Comtrade HS 0404) and feeds 9.6 Mt of milk in
#   the new vintage against 1.3 Mt in the old. So milk's feed is cut first,
#   down to the area's old-vintage level (the clamped median old/new feed
#   ratio over 2010-2013, as for trade), and only the rest of the cut comes out
#   of food.
#
# Measured on the pins at 2010: world imports 1.93x to 1.13x exports, feed 79
# to 37 Mt, food 499 to 461 Mt. FBS's own milk protein over food, in the 69
# rebased importers whose processing is under a tenth of their food, goes
# from 32.9 to 34.4 g/kg (old vintage 33.2); a pro-rata cut over food and feed
# gave 38.5, too dense, which is why it is not used. Two residuals keep the
# feed-first order provisional: world milk feed ends near 42 Mt, below both
# vintages (80 Mt new, 74 Mt old), because exporters already feed less in the
# new vintage than in the old; and in importers that make much butter the
# food cut is still large (Russia 27.8 to 19.7 Mt at 2010), since part of the
# excess may sit in the new vintage's processing element (205 Mt world, 0.1
# Mt in the old), which no cut touches (whep#1583). The 2017 guidelines
# advise national compilers; they do not say FAOSTAT's 2010- series applies
# them.
# Not listed, though the new vintage's imports exceed its exports there too:
# "Vegetables, other" (2605) converts even fresh vegetables at 2.45 on the
# import side against 1.36 on the export side, which is not this mechanism;
# cassava (2532) and palm kernels (2562) book their excess in rows with no
# food, so the rule would not apply; plantains (2616) and sorghum (2518) have
# no import-side excess the rule would remove (whep#1584).
.fbs_rebase_destinies <- function() {
  tibble::tribble(
    ~item_cbs_code, ~element,
    2551L,          "food",
    2848L,          "food",
    2848L,          "feed",
    2618L,          "food",
    2563L,          "food",
    2601L,          "food",
    2612L,          "food"
  )
}

# FBS items whose trade is put on the old vintage's level under `basis`.
.fbs_rebase_items <- function(basis = "rebased") {
  basis <- rlang::arg_match(basis, .fbs_trade_basis_choices())
  all_items <- unique(.fbs_rebase_destinies()$item_cbs_code)
  switch(
    basis,
    rebased = all_items,
    rebased_no_milk = setdiff(all_items, 2848L),
    nuts_only = 2551L,
    as_published = integer()
  )
}

# The years both vintages report, from which the factors are taken.
.fbs_overlap_years <- function() {
  2010:2013
}

# One factor per (area_code, item_cbs_code, element) for import, export and,
# for milk, feed: median(old / new) over the overlap, clamped to [lower bound,
# 1], for the items `fbs_trade_basis` rebases. Empty under `"as_published"`, and when the
# build reads no new-vintage row of the items, because then there is nothing
# to rebase.
#
# The overlap is read from the pins when the frames this build loaded do not
# span it, so the factor -- and with it every 2014+ value -- does not depend on
# the years a build asks for.
.fbs_rebase_factors <- function(
  fbs_new,
  fbs_old,
  fbs_trade_basis = "rebased"
) {
  items <- .fbs_rebase_items(fbs_trade_basis)
  if (!any(fbs_new$item_cbs_code %in% items)) {
    return(.empty_fbs_rebase_factors())
  }
  overlap <- .fbs_overlap_frames(fbs_new, fbs_old)
  keys <- c("area_code", "item_cbs_code", "element", "year")
  new <- .fbs_factor_rows(overlap$new, items)
  old <- .fbs_factor_rows(overlap$old, items)
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

# The overlap rows a factor is taken from: import and export of every item,
# and feed of the items whose cut comes out of feed first (milk).
.fbs_factor_rows <- function(fbs, items) {
  feed_items <- .fbs_rebase_destinies()$item_cbs_code[
    .fbs_rebase_destinies()$element == "feed"
  ]
  rows <- .fbs_rebase_rows(fbs, items, c("import", "export", "feed"))
  rows[element != "feed" | item_cbs_code %in% feed_items]
}

# Some items' rows of one vintage for some elements, summed per key, with
# integer codes so the two vintages and the factors join on one type.
.fbs_rebase_rows <- function(
  fbs,
  items,
  elements = c("import", "export"),
  years = .fbs_overlap_years()
) {
  dt <- data.table::as.data.table(fbs)
  dt <- dt[
    item_cbs_code %in%
      items &
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
    unique(factors$item_cbs_code),
    elements = c("import", "export", "food", "feed", "domestic_supply"),
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
  wide <- .add_rebase_factor(wide, factors, "feed")
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

# Cut the trade and take the net cut out of domestic supply and the item's
# destinies: first feed, down to the old vintage's level (`r_feed`), where
# feed is one of them, then food. A row whose net cut is not positive, or
# exceeds what those destinies can give, is kept as published.
.fbs_rebase_balance <- function(wide) {
  wide <- ensure_columns(
    tibble::as_tibble(wide),
    tibble::tibble(
      import = double(),
      export = double(),
      food = double(),
      feed = double(),
      domestic_supply = double()
    )
  ) |>
    data.table::as.data.table()
  wide <- .add_rebase_destinies(wide)
  wide[, cut_import := data.table::fcoalesce(import, 0) * (1 - r_import)]
  wide[, cut_export := data.table::fcoalesce(export, 0) * (1 - r_export)]
  wide[, net_cut := cut_import - cut_export]
  wide[, room_feed := in_feed * data.table::fcoalesce(feed, 0) * (1 - r_feed)]
  wide[, cut_feed := pmin(pmax(net_cut, 0), room_feed)]
  # `net_cut - 0` is `net_cut` exactly, so an item that only takes from food
  # moves exactly as under whep#1495.
  wide[, cut_food := net_cut - cut_feed]
  wide[, room_food := in_food * data.table::fcoalesce(food, 0)]
  wide[, ok := net_cut > 0 & cut_food <= room_food]
  wide[
    (ok),
    `:=`(
      import = import - cut_import,
      export = export - cut_export,
      food = food - cut_food,
      feed = feed - cut_feed,
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
    feed,
    domestic_supply
  )]
}

# One 0/1 column per destiny, `in_food` and `in_feed`: whether the item's net
# cut may come out of it (`.fbs_rebase_destinies()`).
.add_rebase_destinies <- function(wide) {
  destinies <- data.table::as.data.table(.fbs_rebase_destinies())
  for (dest in c("food", "feed")) {
    col <- paste0("in_", dest)
    takes <- destinies[element == dest, item_cbs_code]
    wide[, (col) := as.numeric(item_cbs_code %in% takes)]
  }
  wide
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

# Warn when a rebase was asked for and an item it covers reaches the build
# from the new vintage with no factor at all: the old vintage or the overlap
# was absent, and every value of that item stays as FAOSTAT published it.
.warn_fbs_rebase_absent <- function(fbs_new, factors, fbs_trade_basis) {
  items <- .fbs_rebase_items(fbs_trade_basis)
  if (length(items) == 0L || is.null(fbs_new)) {
    return(invisible(NULL))
  }
  present <- unique(as.integer(
    fbs_new$item_cbs_code[fbs_new$item_cbs_code %in% items]
  ))
  missing <- setdiff(present, as.integer(factors$item_cbs_code))
  if (length(missing) > 0L) {
    codes <- as.character(missing)
    cli::cli_warn(
      c(
        "{.arg fbs_trade_basis} is {.val {fbs_trade_basis}}, but
         {cli::qty(length(codes))}FBS item{?s} {.val {codes}} {?has/have} no
         old-vintage factor.",
        "i" = "Both vintages must report {min(.fbs_overlap_years())} to
          {max(.fbs_overlap_years())}; without that the item keeps the new
          vintage's published trade."
      ),
      class = "whep_fbs_rebase_absent"
    )
  }
  invisible(NULL)
}
