# The irrigated:rainfed yield ratio R per cell, crop and year, and the split of
# a cell-crop's production between its rainfed and irrigated hectares that
# keeps that production (issue #1233).
#
#   R_anchor  SPAM2010 v2.0 irrigated yield over all-rainfed yield, per crop
#             and country, each yield the ratio of the country's sums
#             (production / harvested area). Floored at 1.
#   R_level   1 + (R_anchor - 1) * n_t / n_2010, n = national synthetic N per
#             hectare of cropland, n_t through the polity lineage.
#   spatial   LPJmL cell ratio over the country ratio, both 1994-2023.
#   temporal  LPJmL cell-year ratio over the cell's 1994-2023 ratio.
#   R_lt      1 + min(9, (R_level - 1) * spatial): the long-term ratio, the
#             anomaly scaling only the excess gap over 1, capped at 10.
#   R         1 + (R_lt - 1) * temporal; only a bad year passes 10.
#
# split_regime_yield() then gives Y_r = P / (A_r + R A_i), Y_i = R Y_r, so
# A_r Y_r + A_i Y_i = P, and lowers R where Y_i would pass the irrigated-yield
# ceiling or Y_r fall below the rainfed-yield floor.

#' Build the irrigated:rainfed yield ratio per cell, crop and year.
#'
#' @description
#' Gives each cell-crop-year the ratio `R` of its irrigated yield to its
#' rainfed yield, the weight that splits a crop's production, synthetic
#' nitrogen and harvest removals between its two regimes (issue #1233). `R`
#' combines three sources:
#'
#' 1. **Anchor.** The SPAM2010 v2.0 ratio of irrigated to all-rainfed yield
#'    for the item's SPAM crop in the country (see [read_spam_yields()] and
#'    [regime_yield_crop_mapping]). Each yield is the country's production
#'    over its harvested area, summed over its SPAM cells, so micro-stands do
#'    not weigh more than large ones. The anchor is floored at 1.
#' 2. **Level.** The anchor is carried to year `t` by the country's synthetic
#'    nitrogen per hectare of cropland relative to 2010,
#'    `R_level = 1 + (R_anchor - 1) * n_t / n_2010`, so the gap closes towards
#'    1 before synthetic fertiliser and widens with it.
#' 3. **Anomaly**, in two parts. The **spatial** part is the cell's LPJmL
#'    irrigated:rainfed ratio over 1994-2023 divided by the country's, so
#'    drier places get a larger gap. The **temporal** part is the cell's ratio
#'    in year `t` divided by its own 1994-2023 ratio, so worse years get a
#'    larger gap.
#'
#' The anomalies scale only the excess of the ratio over 1, so `R` goes to 1
#' wherever the level does, whatever the anomaly. The long-term ratio is
#' `R_lt = 1 + min(9, (R_level - 1) * spatial)`: it is capped at 10, i.e. its
#' excess over 1 at 9. The ratio of the year is
#' `R = 1 + (R_lt - 1) * temporal`; the temporal part is not capped, so only a
#' bad year takes `R` past 10.
#'
#' This `R` is returned as `ratio_unbounded`, for diagnosis only: a
#' single-year near-zero LPJmL rainfed yield can make it absurdly large. Only
#' the `ratio` of [split_regime_yield()], which applies it to a cell's
#' production and areas under an irrigated-yield ceiling and a rainfed-yield
#' floor, is fit to weight anything.
#'
#' @section Anchor:
#' How `spam_crop` is read is fixed by `spam_basis` in
#' [regime_yield_crop_mapping]:
#' - A single crop, or crops joined by `+` with basis `"direct"` (millet,
#'   coffee): harvested area and production are summed over the crops and the
#'   ratio is taken of the sums.
#' - `"composite_weighted"` (the forage crops): the mean of the member crops'
#'   ratios weighted by each member's SPAM harvested area (irrigated plus
#'   rainfed) in the country. A member with no irrigated or no rainfed yield
#'   there is dropped and the weights of the others renormalised; with no
#'   member left, the global composite is used.
#' - `"product_dominance"` (Linum 772, Hemp 776): `ooil` where the country's
#'   seed production (linseed 333, hempseed 336) exceeds its fibre production
#'   (flax 771 "Flax, raw or retted", true hemp 777), otherwise `ofib`, both
#'   summed over 1961-2023. A country with neither product takes the world's
#'   dominant product and its global ratio. The four products are read from
#'   the raw `faostat-production` pin (`method_dominance`
#'   `"dominance_raw_faostat"`), because WHEP's primary production has no flax
#'   fibre (#1302).
#'
#' A country with no irrigated or no rainfed yield for the crop in SPAM takes
#' the crop's global ratio (the ratio of the world's sums). Countries are
#' matched by ISO3 code onto WHEP's polity buckets, never by name.
#'
#' @section Level:
#' `n_t` is the country's synthetic N over its cropland in year `t`:
#' FAOSTAT's agricultural use of nitrogen (the `faostat-fertilizer-nutrients`
#' pin) from 1961, back-cast to 1913 with the Smil (2001) global series scaled
#' by the country's 1961-1965 share ([smil_2001_synthetic_n_global]), and zero
#' before 1913, when there was no synthetic nitrogen. The share's divisor is
#' the Smil series interpolated over 1961-1965 (15.6 Mt), the same divisor the
#' spatialization scripts' `prepare_nitrogen_inputs()` uses (issue #1303). The
#' cropland is [get_arable_permanent_land()] (FAOSTAT from 1961, LUH2
#' back-cast before).
#'
#' A country that reports no N in year `t` takes the N per hectare of the
#' polity that reported fertiliser for its territory that year, found by
#' walking the `predecessor` edges of [polities] with
#' [resolve_polity_lineage()]: Russia before 1992 takes the USSR's N over the
#' USSR's cropland. A historical polity with no 2010 value of its own (the
#' USSR) takes its successors' combined 2010 N over their combined cropland.
#' Before 1961, a polity with a back-cast N but no back-cast cropland (the
#' USSR and Czechoslovakia, whose codes have no LUH2 country) takes its
#' successors' combined LUH2 cropland, rescaled to its own FAOSTAT cropland
#' of 1961 (`method_ratio_cropland` `"successors_luh2_backcast"`); and a
#' territory for which nothing reported N in 1961-1965 had no synthetic N
#' before 1961, so its level is 1 (`"no_n_reported_pre1961"`).
#' `method_ratio_trend` names the path (`"faostat"`, `"faostat_predecessor"`,
#' ...) and `method_ratio_n_2010` the 2010 basis. A country with no synthetic
#' nitrogen in 2010 keeps `R_level = 1`. What no path reaches gets no ratio
#' (`NA`) rather than a guessed one.
#'
#' @section Anomaly:
#' Each item takes the LPJmL crop functional type of
#' [regime_yield_crop_mapping] (`lpjml_cft`), including the `"others"` stand.
#' The cell ratio is the irrigated over the rainfed per-stand yield of
#' [read_lpjml_regime_yield()]. The cell's and the country's 1994-2023
#' ratios are each a ratio of pooled yields: each regime's yield is the sum of
#' yield times stand area over the sum of stand area, over the cell's (or the
#' country's cells') stands and the window years, the LPJmL counterpart of the
#' SPAM anchor. Both parts are 1 in a cell the LPJmL run does not cover at
#' all (its land mask misses some coastal and island cells), stamped
#' `"no_lpjml_cell"`. The spatial part is 1 where the cell has no 1994-2023
#' ratio; the temporal part is 1 where the cell has no ratio that year (a
#' regime without a stand, or no rainfed harvest) or no 1994-2023 ratio.
#' `ratio_anomaly` is their product, the cell-year ratio over the country's
#' wherever all three ratios exist.
#'
#' @details
#' Four implementation choices:
#' - The 1994-2023 normalisers pool every stand, weighted by stand area
#'   (stand fraction times the cell's geometric area; the land fraction is not
#'   applied), rather than averaging cell ratios.
#' - Linum and Hemp dominance pools each country's 1961-2023 production; a
#'   tie goes to the fibre.
#' - The floor at 1 applies to the finished composite, not to its members.
#' - The yield bounds of [split_regime_yield()] pool every production row of
#'   1961-2023 with positive tonnes and area, whatever its `source`.
#'
#' The USSR's and Czechoslovakia's cropland before 1961 is their successors'
#' combined LUH2 cropland (annual plus perennial crop types), rescaled to the
#' predecessor's own FAOSTAT cropland in 1961 -- the same splicing rule
#' [get_arable_permanent_land()] applies to a single country, which cannot be
#' applied to these two because their codes have no LUH2 country.
#'
#' @param cells A tibble of the cell-crop-years to build, with `lon`, `lat`
#'   (0.5-degree cell centres), `area_code` (a WHEP area code; it is resolved
#'   to its polity bucket for the national inputs), `item_prod_code` and
#'   `year`. Other columns are dropped; duplicated keys are collapsed. The
#'   country's LPJmL 1994-2023 ratio pools the cells of `cells` with that
#'   `area_code` (any crop), so pass every cell of a country, as the gridded
#'   land use holds them, or the spatial part is measured against a partial
#'   country.
#' @param run_dir LPJmL run directory for the anomaly. `NULL` (default) uses
#'   `WHEP_LPJML_RUN_DIR`, as [read_lpjml_regime_yield()] does.
#' @param data Optional named list of inputs used instead of reading them,
#'   chiefly for testing and for reusing work across calls:
#'   - `spam`: [read_spam_yields()] output (SPAM2010).
#'   - `fertilizer`: the `faostat-fertilizer-nutrients` table.
#'   - `cropland`: [get_arable_permanent_land()] output (`area_code`, `year`,
#'     `cropland_ha`).
#'   - `luh2`: the `luh2-areas` table (`ISO3`, `Year`, `Land_Use`,
#'     `Area_Mha`), for the successors' back-cast cropland.
#'   - `faostat_production`: the raw `faostat-production` table
#'     (`Area Code`, `Item Code`, `Element`, `Year`, `Value`), for the Linum
#'     and Hemp dominance.
#'   - `lpjml`: LPJmL crop yields for the years of `cells`, at the crop grain
#'     (`lon`, `lat`, `year`, `lpjml_crop`, `yield_rainfed`, `yield_irrigated`,
#'     `method_regime_yield`).
#'   - `lpjml_window`: the same for the normaliser years, with the stand
#'     fractions `stand_frac_rainfed`, `stand_frac_irrigated`. Only rows in
#'     1994-2023 are used.
#'   - `lpjml_grid`: the cells (`lon`, `lat`) the LPJmL run's output covers.
#' @param example If `TRUE`, return a small fixture instead of building the
#'   ratio. Defaults to `FALSE`.
#' @return A tibble with one row per cell-crop-year of `cells`:
#'   - `lon`, `lat`, `area_code`, `item_prod_code`, `year`: the key.
#'   - `ratio_spam`: the SPAM2010 ratio as computed, before the floor.
#'   - `ratio_anchor`: `max(1, ratio_spam)`.
#'   - `ratio_level`: the anchor carried to `year` by synthetic N.
#'   - `ratio_spatial`: the cell's 1994-2023 LPJmL ratio over the country's.
#'   - `ratio_long_term`: `1 + min(9, (ratio_level - 1) * ratio_spatial)`.
#'   - `ratio_temporal`: the cell-year LPJmL ratio over the cell's 1994-2023
#'     ratio.
#'   - `ratio_anomaly`: `ratio_spatial * ratio_temporal`, for reference.
#'   - `ratio_unbounded`: `1 + (ratio_long_term - 1) * ratio_temporal`; `NA`
#'     where the level is. Not fit to weight anything: pass it to
#'     [split_regime_yield()], whose `ratio` is the bounded one.
#'   - `spam_crop_used`: the SPAM crop code(s) the anchor came from (for Linum
#'     and Hemp, the product chosen for the country).
#'   - `method_ratio_anchor`: `"spam_country"`, `"spam_global"`,
#'     `"spam_composite_country"`, `"spam_composite_global"`,
#'     `"spam_dominance_country"`, `"spam_dominance_global"`,
#'     `"spam_dominance_world"` or `"spam_none"` (no SPAM ratio at all).
#'   - `method_ratio_trend`: where `n_t` came from: `"faostat"` or
#'     `"smil_backcast"` (the country's own), either suffixed
#'     `"_predecessor"`, `"_sibling_interval"`, `"_aggregate"` or
#'     `"_shared_polity"` (from
#'     the polity reporting for it, by the lineage step that found it); or
#'     `"pre_synthetic_n"`, `"no_n_reported_pre1961"`, `"n_2010_zero"`,
#'     `"no_n_t"`, `"no_cropland"` (a reporting polity was found but its
#'     cropland is missing) or `"no_n_2010"`.
#'   - `method_ratio_n_2010`: `"own"`, `"successors"`, `"none"` or
#'     `"not_needed"` (before 1913).
#'   - `method_ratio_cropland`: the cropland `n_t` is divided by: `"own"`,
#'     `"successors_luh2_backcast"`, `"none"` or `"not_needed"`.
#'   - `method_dominance`: `"dominance_raw_faostat"` for Linum and Hemp,
#'     `"not_applicable"` otherwise.
#'   - `method_ratio_spatial`: `"lpjml"`, `"no_lpjml_cell"` or
#'     `"no_cell_normal"`.
#'   - `method_ratio_temporal`: `"lpjml"`, `"lpjml_recycled_climate"` (years
#'     before 1901), `"no_lpjml_cell"`, `"no_cell_ratio"` or
#'     `"no_cell_normal"`.
#'   - `method_regime_yield`: the adjustments applied, joined by `";"`:
#'     `"anchor_floor"` (SPAM ratio below 1), `"level_cap"` (long-term ratio
#'     above 10 before the cap), or `"none"`.
#'
#'   Plus the polity columns below.
#'
#' @inheritSection whep_polity_columns Polity columns
#' @source Yu, Q. et al. (2020). A cultivated planet in 2010 -- Part 2: The
#'   global gridded agricultural-production maps. Earth System Science Data
#'   12, 3545-3572. \doi{10.5194/essd-12-3545-2020}. LPJmL 6.1.1 band
#'   harvests as in [read_lpjml_regime_yield()]. FAOSTAT Fertilizers by
#'   Nutrient, Land Use and Crops and livestock products. Smil, V. (2001)
#'   *Enriching the Earth*, MIT Press.
#' @export
#' @examples
#' build_regime_yield_ratio(example = TRUE)
build_regime_yield_ratio <- function(
  cells,
  run_dir = NULL,
  data = NULL,
  example = FALSE
) {
  if (isTRUE(example)) {
    return(.example_regime_yield_ratio())
  }
  data <- .ryr_check_data(data)
  cells <- .ryr_check_cells(cells)
  keys <- dplyr::left_join(
    cells,
    .ryr_bucket_table(),
    by = "area_code",
    relationship = "many-to-one"
  )
  anchor <- .ryr_anchor(
    dplyr::distinct(keys, .data$bucket, .data$item_prod_code),
    data
  )
  trend <- .ryr_trend(dplyr::distinct(keys, .data$bucket, .data$year), data)
  anomaly <- .ryr_anomaly(cells, run_dir, data)
  keys |>
    dplyr::left_join(
      anchor,
      by = c("bucket", "item_prod_code"),
      relationship = "many-to-one"
    ) |>
    dplyr::left_join(
      trend,
      by = c("bucket", "year"),
      relationship = "many-to-one"
    ) |>
    dplyr::left_join(
      anomaly,
      by = c("lon", "lat", "area_code", "item_prod_code", "year"),
      relationship = "one-to-one"
    ) |>
    .ryr_combine() |>
    .add_reporting_polity_columns()
}

#' Split a cell-crop's production between its rainfed and irrigated regimes.
#'
#' @description
#' Applies the irrigated:rainfed yield ratio `R` of
#' [build_regime_yield_ratio()] to a cell-crop's production `P` and its
#' rainfed and irrigated harvested areas `A_r`, `A_i`, keeping the production:
#' `Y_r = P / (A_r + R * A_i)` and `Y_i = R * Y_r`, so
#' `A_r * Y_r + A_i * Y_i = P`.
#'
#' `R` starts from `ratio_unbounded` and two plausibility bounds can lower it,
#' never below 1. The result is the output's `ratio`, the only ratio fit to
#' weight anything:
#' - **Irrigated-yield ceiling.** Where `Y_i` would exceed the item's `Y_max`,
#'   `R` is lowered until `Y_i = Y_max`, `R = Y_max * A_r / (P - Y_max * A_i)`.
#'   `Y_max` is the 99th percentile of the item's national yields (production
#'   over harvested area) in [get_primary_production()], pooled over
#'   countries and the years 1961-2023.
#' - **Rainfed-yield floor.** Where `Y_r` would fall below the item's
#'   `Y_min`, the 1st percentile of the same pool, `R` is lowered until
#'   `Y_r = Y_min`, `R = (P - Y_min * A_r) / (Y_min * A_i)`.
#'
#' Lowering `R` raises `Y_r` and lowers `Y_i`, so the floor, applied after the
#' ceiling, cannot break it. An item with no FAOSTAT yields of its own takes
#' the pooled yields of the items that directly carry one of its SPAM crops
#' in [regime_yield_crop_mapping] (`spam_basis` `"direct"` or
#' `"direct_aggregate"`), stamped `"bound_via_spam_crop"`.
#'
#' A cell-crop with area in one regime only is a trivial split: all its
#' production is on that regime, no ratio applies (`ratio` is 1, whatever
#' `ratio_unbounded` is) and the bounds are not tested. Only a cell-crop with
#' no area at all, or with both regimes and no `ratio_unbounded`, gets `NA`.
#'
#' @param cells A tibble with `area_code`, `item_prod_code`, `production_t`
#'   (production in the units of [get_primary_production()]'s `"tonnes"`
#'   rows), `rainfed_ha`, `irrigated_ha` and `ratio_unbounded` (as
#'   [build_regime_yield_ratio()] returns it). Other columns are kept.
#' @param bound Which pool of national yields sets `Y_max` and `Y_min`:
#'   `"global"` (default, all countries) or `"region"` (the countries of the
#'   cell's WHEP region, the `region` column of [regions_full]; where the
#'   region has no yields for the item, the world's pool, stamped `_global`).
#' @param production Optional [get_primary_production()] output (`year`,
#'   `area_code`, `item_prod_code`, `unit`, `value`) used instead of building
#'   it.
#' @return `cells` with:
#'   - `ratio`: the irrigated:rainfed yield ratio used, after the bounds (1 on
#'     a trivial split).
#'   - `yield_rainfed`, `yield_irrigated`: production per hectare of each
#'     regime.
#'   - `yield_max`, `yield_min`: the bounds applied; `NA` where the item has
#'     no yields to set them.
#'   - `method_regime_split`: `"yield_ratio"`, `"trivial_rainfed_only"`,
#'     `"trivial_irrigated_only"`, `"no_area"` or `"no_ratio"`.
#'   - `method_regime_bound`: the ceiling: `"not_binding"`, `"clipped"`,
#'     `"clipped_at_one"` (lowered to 1 and still above `Y_max`: the mean
#'     yield itself exceeds it), `"no_bound"`, `"not_applicable_trivial"` or
#'     `"not_applicable"` (no area, or no ratio).
#'   - `method_rainfed_floor`: the floor: `"not_binding"`, `"rainfed_floor"`,
#'     `"rainfed_floor_at_one"` (lowered to 1 and still below `Y_min`: the
#'     mean yield itself is below it), `"no_bound"`,
#'     `"not_applicable_trivial"` or `"not_applicable"`.
#'   - `method_bound_source`: which yields set the bounds: `"own_yields"`,
#'     `"bound_via_spam_crop"`, either suffixed `"_global"` where a regional
#'     pool fell back to the world's, or `"none"`.
#'   - `method_yield_bound`: the `bound` chosen.
#' @export
#' @examples
#' cells <- tibble::tribble(
#'   ~area_code, ~item_prod_code, ~production_t, ~rainfed_ha, ~irrigated_ha,
#'   ~ratio_unbounded,
#'   203L, 15L, 500, 100, 50, 1.8
#' )
#' production <- tibble::tribble(
#'   ~year, ~area_code, ~item_prod_code, ~unit, ~value,
#'   2010L, 203L, 15L, "tonnes", 3000,
#'   2010L, 203L, 15L, "ha", 1000
#' )
#' split_regime_yield(cells, production = production)
split_regime_yield <- function(
  cells,
  bound = c("global", "region"),
  production = NULL
) {
  bound <- rlang::arg_match(bound)
  .ryr_require_cols(
    cells,
    c(
      "area_code",
      "item_prod_code",
      "production_t",
      "rainfed_ha",
      "irrigated_ha",
      "ratio_unbounded"
    ),
    "cells"
  )
  .ryr_check_split_values(cells)
  production <- production %||%
    get_primary_production(
      years = .ryr_bound_years()
    )
  bounds <- .ryr_yield_bounds(
    production,
    bound,
    unique(as.integer(cells$item_prod_code))
  )
  cells |>
    .ryr_attach_bounds(bounds, bound) |>
    .ryr_split()
}

# -- Constants ---------------------------------------------------------------

# The long-term ratio is capped at 10, i.e. 9 on its excess
# `(ratio_level - 1) x ratio_spatial`. An assumption of the method, not a
# sourced value.
.ryr_level_cap <- function() {
  10
}

# SPAM2010 v2.0 is the single anchor year.
.ryr_anchor_year <- function() {
  2010L
}

# The LPJmL normalisers are 30-year means, taken as the 30 most recent years of
# the run (it ends in 2023) and fixed for every target year, so the country
# normaliser is one number per crop and country.
.ryr_normal_window <- function() {
  1994:2023
}

# The first industrial Haber-Bosch output (BASF Oppau, 1913) is where
# smil_2001_synthetic_n_global starts; its documentation fixes earlier years
# at zero synthetic N.
.ryr_first_synthetic_year <- function() {
  1913L
}

# FAOSTAT's fertiliser series starts in 1961; the Smil back-cast scales the
# global series by each country's mean share over 1961-1965, as
# prepare_nitrogen_inputs() does.
.ryr_smil_share_window <- function() {
  1961:1965
}

# The yield bounds pool national yields over 1961-2023; the ceiling is their
# 99th percentile.
.ryr_bound_years <- function() {
  1961:2023
}

.ryr_bound_prob <- function() {
  0.99
}

# The rainfed-yield floor, the 1st percentile of the same pool.
.ryr_floor_prob <- function() {
  0.01
}

# The two products whose harvested area WHEP books jointly on one item
# (inst/extdata/harmonization/primary_double.csv, `Multi_area`), and the SPAM
# aggregate each product belongs to (Yu et al. 2020, Table S3: linseed and
# hempseed in `ooil`, flax and true hemp in `ofib`).
#
# The codes are FAOSTAT QCL item codes, read from the raw
# `faostat-production` pin. WHEP's primary production has no row for flax
# fibre (#1302): FAOSTAT's current QCL books it as 771 "Flax, raw or retted"
# (France 2010: 372,100 t), where primary_double.csv still names 773. Remove
# this raw read, and go back to WHEP's production, when #1302 is fixed.
.ryr_dominance_products <- function() {
  tibble::tribble(
    ~item_prod_code, ~seed_code, ~fibre_code,
    772L,            333L,       771L,
    776L,            336L,       777L
  )
}

.ryr_data_keys <- function() {
  c(
    "spam",
    "fertilizer",
    "cropland",
    "luh2",
    "faostat_production",
    "lpjml",
    "lpjml_window",
    "lpjml_grid"
  )
}

# -- Input checks -------------------------------------------------------------

.ryr_require_cols <- function(data, cols, name) {
  missing <- setdiff(cols, names(data))
  if (length(missing) == 0L) {
    return(invisible(data))
  }
  cli::cli_abort(
    "{.arg {name}} is missing column{?s} {.field {missing}}.",
    class = "whep_regime_yield_columns"
  )
}

.ryr_check_data <- function(data) {
  data <- data %||% list()
  unknown <- setdiff(names(data), .ryr_data_keys())
  if (length(unknown) > 0L) {
    cli::cli_abort(
      c(
        "Unknown {.arg data} element{?s}: {.val {unknown}}.",
        i = "Known elements: {.val {(.ryr_data_keys())}}."
      ),
      class = "whep_regime_yield_data"
    )
  }
  data
}

# The cell key, typed and with coordinates rounded to the 0.5-degree centres
# every join below uses. An item the crop map does not know has no source for
# its ratio, so it is refused rather than given one.
.ryr_check_cells <- function(cells) {
  key <- c("lon", "lat", "area_code", "item_prod_code", "year")
  .ryr_require_cols(cells, key, "cells")
  out <- cells |>
    dplyr::transmute(
      lon = round(as.numeric(.data$lon), 2),
      lat = round(as.numeric(.data$lat), 2),
      area_code = as.integer(.data$area_code),
      item_prod_code = as.integer(.data$item_prod_code),
      year = as.integer(.data$year)
    ) |>
    dplyr::distinct()
  if (anyNA(out)) {
    cli::cli_abort(
      "{.arg cells} has missing values in its key columns.",
      class = "whep_regime_yield_columns"
    )
  }
  unmapped <- setdiff(out$item_prod_code, .ryr_mapping()$item_prod_code)
  if (length(unmapped) > 0L) {
    cli::cli_abort(
      c(
        "{length(unmapped)} item{?s} ha{?s/ve} no row in
         {.code regime_yield_crop_mapping}: {.val {unmapped}}.",
        i = "Every item needs a SPAM crop and an LPJmL CFT for its ratio."
      ),
      class = "whep_regime_yield_items"
    )
  }
  out
}

.ryr_check_split_values <- function(cells) {
  vals <- c(cells$production_t, cells$rainfed_ha, cells$irrigated_ha)
  if (anyNA(vals) || any(vals < 0)) {
    cli::cli_abort(
      "{.field production_t}, {.field rainfed_ha} and {.field irrigated_ha}
       must be present and not negative.",
      class = "whep_regime_yield_columns"
    )
  }
  invisible(cells)
}

.ryr_mapping <- function() {
  mapping <- whep::regime_yield_crop_mapping |>
    dplyr::transmute(
      item_prod_code = as.integer(.data$item_prod_code),
      spam_crop = .data$spam_crop,
      spam_basis = .data$spam_basis,
      lpjml_crop = stringr::str_replace_all(.data$lpjml_cft, "_", " ")
    )
  known <- c(
    "direct",
    "direct_aggregate",
    "group_proxy",
    "proxy",
    "composite_weighted",
    "product_dominance"
  )
  odd <- setdiff(mapping$spam_basis, known)
  if (length(odd) > 0L) {
    cli::cli_abort(
      "Unknown {.field spam_basis} {.val {odd}} in
       {.code regime_yield_crop_mapping}.",
      class = "whep_regime_yield_items"
    )
  }
  mapping
}

# Each area code's polity bucket, the key of every national table this ratio
# reads (FAOSTAT fertiliser, cropland, production, and SPAM through its ISO3).
# `polity_area_code` is functionally determined by `area_code` (asserted), so
# the lookup has no year. A bucket code that is no reporting area of its own
# maps to itself; a code that is neither finds no row, so its bucket is NA and
# it finds no national value.
.ryr_bucket_table <- function() {
  cw <- .polity_crosswalk() |>
    as.data.frame() |>
    tibble::as_tibble() |>
    dplyr::filter(!is.na(.data$area_code), !is.na(.data$polity_area_code)) |>
    dplyr::distinct(
      area_code = as.integer(.data$area_code),
      bucket = as.integer(.data$polity_area_code)
    )
  if (anyDuplicated(cw$area_code) > 0L) {
    cli::cli_abort(
      "An area code resolves to more than one polity bucket.",
      class = "whep_regime_yield_buckets"
    )
  }
  own <- setdiff(cw$bucket, cw$area_code)
  dplyr::bind_rows(cw, tibble::tibble(area_code = own, bucket = own))
}

# -- Anchor: SPAM2010 ratio per bucket and item -------------------------------

# Irrigated over rainfed yield from the regimes' summed harvested area and
# production; NA unless both regimes have area and the rainfed one a harvest.
.ryr_ratio <- function(h_i, p_i, h_r, p_r) {
  defined <- h_i > 0 & h_r > 0 & p_r > 0
  dplyr::if_else(defined, (p_i / h_i) / (p_r / h_r), NA_real_)
}

.ryr_anchor <- function(pairs, data) {
  mapping <- .ryr_mapping()
  pairs <- pairs |>
    dplyr::left_join(
      dplyr::select(mapping, "item_prod_code", "spam_crop", "spam_basis"),
      by = "item_prod_code"
    )
  spam <- .ryr_spam_totals(data$spam %||% read_spam_yields("2010"))
  kind <- dplyr::case_when(
    pairs$spam_basis == "composite_weighted" ~ "composite",
    pairs$spam_basis == "product_dominance" ~ "dominance",
    .default = "pooled"
  )
  parts <- list()
  if (any(kind == "pooled")) {
    parts$pooled <- .ryr_anchor_pooled(pairs[kind == "pooled", ], spam)
  }
  if (any(kind == "composite")) {
    parts$composite <- .ryr_anchor_composite(pairs[kind == "composite", ], spam)
  }
  if (any(kind == "dominance")) {
    # Remove when #1302 is fixed (read WHEP's production instead).
    production <- data$faostat_production %||%
      whep_read_file("faostat-production")
    parts$dominance <- .ryr_anchor_dominance(
      pairs[kind == "dominance", ],
      spam,
      production
    )
  }
  dplyr::bind_rows(parts) |>
    dplyr::mutate(
      method_dominance = dplyr::if_else(
        .data$item_prod_code %in% .ryr_dominance_products()$item_prod_code,
        "dominance_raw_faostat",
        "not_applicable"
      )
    ) |>
    dplyr::select(
      "bucket",
      "item_prod_code",
      "ratio_spam",
      "spam_crop_used",
      "method_ratio_anchor",
      "method_dominance"
    )
}

# SPAM harvested area and production summed per SPAM crop and regime, per
# polity bucket (ISO3 through `.luh2_bridge_iso3c()`, the package's one ISO3
# bridge) and for the world (every pixel, mapped or not).
.ryr_spam_totals <- function(spam) {
  .ryr_require_cols(
    spam,
    c("iso3", "spam_crop", "technology", "harvested_area_ha", "production_t"),
    "spam"
  )
  dt <- data.table::as.data.table(spam)[
    technology %in% c("I", "R"),
    list(
      iso3c = iso3,
      spam_crop,
      technology,
      h = harvested_area_ha,
      p = production_t
    )
  ]
  if (nrow(dt) == 0L || anyNA(dt$h) || anyNA(dt$p)) {
    cli::cli_abort(
      "The SPAM table is empty or has missing harvested area or production
       values.",
      class = "whep_regime_yield_spam"
    )
  }
  by_iso <- dt[,
    list(h = sum(h), p = sum(p)),
    by = c("iso3c", "spam_crop", "technology")
  ]
  country <- .luh2_bridge_iso3c(by_iso)[,
    list(h = sum(h), p = sum(p)),
    by = c("area_code", "spam_crop", "technology")
  ]
  global <- by_iso[,
    list(h = sum(h), p = sum(p)),
    by = c("spam_crop", "technology")
  ]
  list(
    country = .ryr_spread_tech(country) |>
      dplyr::rename(bucket = "area_code") |>
      dplyr::mutate(bucket = as.integer(.data$bucket)),
    global = .ryr_spread_tech(global)
  )
}

.ryr_spread_tech <- function(dt) {
  tibble::as_tibble(dt) |>
    tidyr::pivot_wider(
      names_from = "technology",
      values_from = c("h", "p"),
      values_fill = 0
    ) |>
    ensure_columns(
      tibble::tibble(
        h_I = double(),
        h_R = double(),
        p_I = double(),
        p_R = double()
      ),
      defaults = list(h_I = 0, h_R = 0, p_I = 0, p_R = 0)
    ) |>
    dplyr::rename(h_i = "h_I", h_r = "h_R", p_i = "p_I", p_r = "p_R")
}

# Each distinct `spam_crop` string of the pairs, one row per member crop. The
# members are keyed on that string, not on the item, because a dominance item
# uses a different SPAM crop in different countries.
.ryr_members <- function(pairs, sep) {
  pairs |>
    dplyr::distinct(.data$spam_crop) |>
    dplyr::mutate(member = stringr::str_split(.data$spam_crop, sep)) |>
    tidyr::unnest_longer("member", ptype = character())
}

# Regime totals of each member joined on, for the countries or the world.
.ryr_join_members <- function(members, totals) {
  dplyr::inner_join(
    members,
    dplyr::rename(totals, member = "spam_crop"),
    by = "member",
    relationship = "many-to-many"
  )
}

# The ratio of the regimes summed over the members (pooled).
.ryr_sum_regimes <- function(x, by) {
  x |>
    dplyr::summarise(
      dplyr::across(c("h_i", "p_i", "h_r", "p_r"), sum),
      .by = dplyr::all_of(by)
    ) |>
    dplyr::mutate(
      ratio = .ryr_ratio(.data$h_i, .data$p_i, .data$h_r, .data$p_r)
    ) |>
    dplyr::select(dplyr::all_of(c(by, "ratio")))
}

# The members' own ratios, weighted by their harvested area (irrigated plus
# rainfed); members without a ratio are dropped, which renormalises the rest.
.ryr_weigh_members <- function(x, by) {
  x |>
    dplyr::mutate(
      ratio = .ryr_ratio(.data$h_i, .data$p_i, .data$h_r, .data$p_r),
      weight = .data$h_i + .data$h_r
    ) |>
    dplyr::filter(!is.na(.data$ratio)) |>
    dplyr::summarise(
      ratio = sum(.data$weight * .data$ratio) / sum(.data$weight),
      .by = dplyr::all_of(by)
    )
}

# The country and global ratio of each pair's `spam_crop`, by `combine`
# (`.ryr_sum_regimes` pools, `.ryr_weigh_members` weighs).
.ryr_scope_ratios <- function(pairs, spam, sep, combine) {
  members <- .ryr_members(pairs, sep)
  country <- .ryr_join_members(members, spam$country) |>
    combine(c("bucket", "spam_crop")) |>
    dplyr::rename(ratio_country = "ratio")
  global <- .ryr_join_members(members, spam$global) |>
    combine("spam_crop") |>
    dplyr::rename(ratio_global = "ratio")
  pairs |>
    dplyr::left_join(country, by = c("bucket", "spam_crop")) |>
    dplyr::left_join(global, by = "spam_crop")
}

# One SPAM crop, or several joined by `+` and pooled: the ratio of the summed
# regimes, in the country or, where it has none, in the world.
.ryr_anchor_pooled <- function(pairs, spam) {
  .ryr_scope_ratios(pairs, spam, "\\+", .ryr_sum_regimes) |>
    .ryr_pick_scope("spam") |>
    dplyr::mutate(spam_crop_used = .data$spam_crop)
}

# The country ratio where there is one, else the global one, stamped.
.ryr_pick_scope <- function(x, prefix) {
  x |>
    dplyr::mutate(
      ratio_spam = dplyr::coalesce(.data$ratio_country, .data$ratio_global),
      method_ratio_anchor = dplyr::case_when(
        !is.na(.data$ratio_country) ~ paste0(prefix, "_country"),
        !is.na(.data$ratio_global) ~ paste0(prefix, "_global"),
        .default = "spam_none"
      )
    )
}

# Composites: the member crops' ratios weighted by their harvested area;
# undefined members dropped, the rest renormalised; none left, the global
# composite (global ratios, global weights).
.ryr_anchor_composite <- function(pairs, spam) {
  .ryr_scope_ratios(pairs, spam, "\\+", .ryr_weigh_members) |>
    .ryr_pick_scope("spam_composite") |>
    dplyr::mutate(spam_crop_used = .data$spam_crop)
}

# Linum and Hemp: the SPAM aggregate of the dominant product, per country.
# country produced neither product the world's dominant product is used with
# its global ratio.
.ryr_anchor_dominance <- function(pairs, spam, production) {
  chosen <- pairs |>
    dplyr::select(-"spam_crop") |>
    dplyr::left_join(
      .ryr_dominance(pairs, production),
      by = c("bucket", "item_prod_code")
    ) |>
    dplyr::mutate(spam_crop = .data$spam_crop_used)
  .ryr_scope_ratios(chosen, spam, "\\+", .ryr_sum_regimes) |>
    dplyr::mutate(
      ratio_country = dplyr::if_else(
        .data$dominance_basis == "country",
        .data$ratio_country,
        NA_real_
      )
    ) |>
    .ryr_pick_scope("spam_dominance") |>
    dplyr::mutate(
      method_ratio_anchor = dplyr::if_else(
        .data$dominance_basis == "world" &
          .data$method_ratio_anchor != "spam_none",
        "spam_dominance_world",
        .data$method_ratio_anchor
      )
    )
}

# Seed or fibre per bucket, from FAOSTAT production summed over 1961-2023;
# a bucket that produced neither takes the world's dominant product. A tie
# goes to the fibre (`ooil` only where seed production exceeds fibre).
.ryr_dominance <- function(pairs, production) {
  tonnes <- .ryr_dominance_tonnes(production)
  world <- tonnes |>
    dplyr::summarise(value = sum(.data$value), .by = "item_prod_code")
  pairs |>
    dplyr::distinct(.data$bucket, .data$item_prod_code) |>
    dplyr::left_join(.ryr_dominance_products(), by = "item_prod_code") |>
    .ryr_attach_tonnes(tonnes, "seed_code", "seed_t", "bucket") |>
    .ryr_attach_tonnes(tonnes, "fibre_code", "fibre_t", "bucket") |>
    .ryr_attach_tonnes(world, "seed_code", "world_seed_t") |>
    .ryr_attach_tonnes(world, "fibre_code", "world_fibre_t") |>
    dplyr::mutate(
      dominance_basis = dplyr::if_else(
        .data$seed_t + .data$fibre_t > 0,
        "country",
        "world"
      ),
      seed_wins = dplyr::if_else(
        .data$dominance_basis == "country",
        .data$seed_t > .data$fibre_t,
        .data$world_seed_t > .data$world_fibre_t
      ),
      spam_crop_used = dplyr::if_else(.data$seed_wins, "ooil", "ofib")
    ) |>
    dplyr::select(
      "bucket",
      "item_prod_code",
      "spam_crop_used",
      "dominance_basis"
    )
}

# FAOSTAT tonnes of the seed and fibre products per polity bucket, 1961-2023,
# read from the raw `faostat-production` pin (remove when #1302 is
# fixed). Reporting areas are summed onto their bucket; aggregates with no
# bucket (World, regions, the China 351 aggregate) are dropped.
.ryr_dominance_tonnes <- function(production) {
  .ryr_require_cols(
    production,
    c("Area Code", "Item Code", "Element", "Year", "Value"),
    "faostat_production"
  )
  check_labels_supplied(
    production,
    "Element",
    "Production",
    details = c(i = "Source: the {.val faostat-production} pin.")
  )
  products <- .ryr_dominance_products()
  codes <- c(products$seed_code, products$fibre_code)
  production |>
    dplyr::transmute(
      area_code = as.integer(.data[["Area Code"]]),
      item_prod_code = as.integer(.data[["Item Code"]]),
      year = as.integer(.data$Year),
      element = .data$Element,
      value = as.numeric(.data$Value)
    ) |>
    dplyr::filter(
      .data$element == "Production",
      .data$item_prod_code %in% codes,
      .data$year %in% .ryr_bound_years(),
      !is.na(.data$value)
    ) |>
    dplyr::inner_join(
      .ryr_bucket_table(),
      by = "area_code",
      relationship = "many-to-one"
    ) |>
    dplyr::summarise(
      value = sum(.data$value),
      .by = c("bucket", "item_prod_code")
    ) |>
    dplyr::rename(area_code = "bucket")
}

# Join one product's tonnes on by its code column (and the bucket, when
# given), zero where the area produced none.
.ryr_attach_tonnes <- function(x, tonnes, code_col, value_col, area = NULL) {
  keyed <- dplyr::rename(tonnes, !!code_col := "item_prod_code")
  by <- code_col
  if (!is.null(area)) {
    keyed <- dplyr::rename(keyed, !!area := "area_code")
    by <- c(area, code_col)
  }
  x |>
    dplyr::left_join(dplyr::rename(keyed, !!value_col := "value"), by = by) |>
    dplyr::mutate(!!value_col := dplyr::coalesce(.data[[value_col]], 0))
}

# -- Level: the fertiliser-N scaling ------------------------------------------

.ryr_trend <- function(bucket_years, data) {
  needs_n <- any(bucket_years$year >= .ryr_first_synthetic_year())
  national <- if (needs_n) {
    .ryr_n_intensity(bucket_years, data)
  } else {
    list(n = .ryr_n_proto(), intensity = .ryr_n_intensity_proto())
  }
  n_t <- .ryr_n_t(bucket_years, national)
  n_2010 <- .ryr_n_2010(bucket_years, national$intensity)
  bucket_years |>
    dplyr::left_join(n_t, by = c("bucket", "year")) |>
    dplyr::left_join(n_2010, by = c("bucket", "year")) |>
    dplyr::mutate(
      method_ratio_trend = .ryr_trend_stamp(
        .data$year,
        .data$n_ha,
        .data$n_ha_2010,
        .data$n_path
      ),
      method_ratio_n_2010 = dplyr::if_else(
        .data$year < .ryr_first_synthetic_year(),
        "not_needed",
        dplyr::coalesce(.data$n_2010_path, "none")
      ),
      method_ratio_cropland = dplyr::case_when(
        .data$method_ratio_trend %in% c("pre_synthetic_n", .ryr_no_n_pre()) ~
          "not_needed",
        .default = dplyr::coalesce(.data$cropland_path, "none")
      ),
      n_scale = dplyr::case_when(
        .data$method_ratio_trend %in%
          c("pre_synthetic_n", .ryr_no_n_pre(), "n_2010_zero") ~ 0,
        is.na(.data$n_ha) | is.na(.data$n_ha_2010) ~ NA_real_,
        .default = .data$n_ha / .data$n_ha_2010
      )
    ) |>
    dplyr::select(
      "bucket",
      "year",
      "n_scale",
      "method_ratio_trend",
      "method_ratio_n_2010",
      "method_ratio_cropland"
    )
}

# Before 1961 a territory for which nothing reported N in 1961-1965 (no
# Smil back-cast of its own or of the polity it belonged to) had no synthetic
# N, so its level is 1.
.ryr_no_n_pre <- function() {
  "no_n_reported_pre1961"
}

# The trend stamp. A missing n_t before 1961 with no reporting polity found
# is that zero; a lineage that found a reporter whose cropland is missing
# is `no_cropland` (NA), so a gap in the land series is never read as "no N".
.ryr_trend_stamp <- function(year, n_ha, n_ha_2010, n_path) {
  first_faostat <- min(.ryr_smil_share_window())
  dplyr::case_when(
    year < .ryr_first_synthetic_year() ~ "pre_synthetic_n",
    is.na(n_ha) & is.na(n_path) & year < first_faostat ~ .ryr_no_n_pre(),
    is.na(n_ha_2010) ~ "no_n_2010",
    n_ha_2010 == 0 ~ "n_2010_zero",
    is.na(n_ha) & !is.na(n_path) ~ "no_cropland",
    is.na(n_ha) ~ "no_n_t",
    .default = n_path
  )
}

.ryr_n_proto <- function() {
  tibble::tibble(
    area_code = integer(),
    year = integer(),
    synthetic_n_t = double(),
    n_source = character()
  )
}

.ryr_n_intensity_proto <- function() {
  tibble::tibble(
    area_code = integer(),
    year = integer(),
    synthetic_n_t = double(),
    cropland_ha = double(),
    n_ha = double(),
    n_source = character(),
    cropland_source = character()
  )
}

# n_t of a bucket in year t. Its own FAOSTAT (or Smil back-cast) value
# where it reports one; otherwise the value of the polity that reported
# fertiliser for its territory that year, found by walking the `predecessor`
# edges of [polities] with resolve_polity_lineage() (the USSR's N per hectare
# of USSR cropland for Russia before 1992). Where several reporting buckets
# resolve to that one polity in a year their N and cropland are pooled. The
# walk runs towards every polity that reported N, whether or not its cropland
# is known, so a missing cropland shows as `no_cropland` (`n_path` set, `n_ha`
# NA) rather than as a territory with no N.
.ryr_n_t <- function(bucket_years, national) {
  intensity <- national$intensity
  own <- intensity |>
    dplyr::transmute(
      bucket = .data$area_code,
      .data$year,
      n_ha = .data$n_ha,
      n_path = .data$n_source,
      cropland_path = .data$cropland_source
    )
  rest <- bucket_years |>
    dplyr::filter(
      .data$year >= .ryr_first_synthetic_year(),
      !is.na(.data$bucket)
    ) |>
    dplyr::anti_join(own, by = c("bucket", "year"))
  reporters <- .ryr_reporter_polities(national$n)
  support <- .ryr_as_support(reporters$polity_code, reporters$year)
  inherited <- .ryr_lineage(rest, support) |>
    dplyr::inner_join(
      .ryr_polity_intensity(national$n, intensity, reporters),
      by = c(lineage_polity_code = "polity_code", "year"),
      relationship = "many-to-one"
    ) |>
    dplyr::transmute(
      .data$bucket,
      .data$year,
      .data$n_ha,
      n_path = paste0(
        .data$n_source,
        "_",
        .ryr_lineage_label(.data$method_polity_lineage)
      ),
      .data$cropland_path
    )
  dplyr::bind_rows(own, inherited)
}

# n_2010 of a bucket. Its own 2010 value; for a polity with none (the
# USSR, which did not exist in 2010), its successors' combined 2010 N over
# their combined 2010 cropland. A successor is a bucket reporting in 2010
# whose lineage in year t walks back to the bucket's own polity of year t.
.ryr_n_2010 <- function(bucket_years, intensity) {
  base <- intensity |>
    dplyr::filter(.data$year == .ryr_anchor_year())
  keyed <- bucket_years |>
    dplyr::filter(
      .data$year >= .ryr_first_synthetic_year(),
      !is.na(.data$bucket)
    )
  own <- keyed |>
    dplyr::inner_join(
      dplyr::select(base, bucket = "area_code", n_ha_2010 = "n_ha"),
      by = "bucket"
    ) |>
    dplyr::mutate(n_2010_path = "own")
  historical <- dplyr::anti_join(keyed, own, by = c("bucket", "year"))
  dplyr::bind_rows(own, .ryr_successor_n_2010(historical, base))
}

.ryr_successor_n_2010 <- function(historical, base) {
  proto <- tibble::tibble(
    bucket = integer(),
    year = integer(),
    n_ha_2010 = double(),
    n_2010_path = character()
  )
  if (nrow(historical) == 0L || nrow(base) == 0L) {
    return(proto)
  }
  successors <- .ryr_successors(historical, unique(base$area_code))
  if (nrow(successors) == 0L) {
    return(proto)
  }
  successors |>
    dplyr::inner_join(
      dplyr::select(base, bucket = "area_code", "synthetic_n_t", "cropland_ha"),
      by = "bucket"
    ) |>
    dplyr::summarise(
      n_ha_2010 = sum(.data$synthetic_n_t) / sum(.data$cropland_ha),
      .by = c("historical", "year")
    ) |>
    dplyr::transmute(
      bucket = .data$historical,
      .data$year,
      .data$n_ha_2010,
      n_2010_path = "successors"
    )
}

# The successors of each historical (bucket, year): the `candidates` buckets
# whose lineage in that year walks back to the historical bucket's own polity
# of that year. Returns `historical`, `year`, `bucket` (the successor).
.ryr_successors <- function(historical, candidates) {
  proto <- tibble::tibble(
    historical = integer(),
    year = integer(),
    bucket = integer()
  )
  polity <- historical |>
    dplyr::distinct(.data$bucket, .data$year) |>
    dplyr::rename(area_code = "bucket") |>
    .add_reporting_polity_columns() |>
    dplyr::transmute(
      historical = .data$area_code,
      .data$year,
      polity_code = .data$reporting_polity_code
    ) |>
    dplyr::filter(!is.na(.data$polity_code))
  if (nrow(polity) == 0L) {
    return(proto)
  }
  pairs <- tidyr::expand_grid(
    bucket = as.integer(candidates),
    year = unique(polity$year)
  )
  support <- .ryr_as_support(polity$polity_code, polity$year)
  .ryr_lineage(pairs, support) |>
    dplyr::inner_join(
      polity,
      by = c(lineage_polity_code = "polity_code", "year"),
      relationship = "many-to-many"
    ) |>
    dplyr::filter(.data$historical != .data$bucket) |>
    dplyr::select("historical", "year", "bucket")
}

# The polity each bucket reporting N stands for, in each year it reports.
.ryr_reporter_polities <- function(n_rows) {
  n_rows |>
    dplyr::distinct(.data$area_code, .data$year) |>
    .add_reporting_polity_columns() |>
    dplyr::transmute(
      bucket = .data$area_code,
      .data$year,
      polity_code = .data$reporting_polity_code
    ) |>
    dplyr::filter(!is.na(.data$polity_code))
}

# N per hectare of each reporting polity-year, pooled over the buckets that
# stand for it; `n_ha` is NA where any of them lacks cropland, so a pooled
# value never rests on part of the polity's land.
.ryr_polity_intensity <- function(n_rows, intensity, reporters) {
  n_rows |>
    dplyr::inner_join(
      reporters,
      by = c(area_code = "bucket", "year"),
      relationship = "one-to-one"
    ) |>
    dplyr::left_join(
      dplyr::select(
        intensity,
        "area_code",
        "year",
        "cropland_ha",
        "cropland_source"
      ),
      by = c("area_code", "year"),
      relationship = "one-to-one"
    ) |>
    dplyr::summarise(
      n_ha = sum(.data$synthetic_n_t) / sum(.data$cropland_ha),
      n_source = dplyr::first(.data$n_source),
      cropland_path = dplyr::first(.data$cropland_source),
      .by = c("polity_code", "year")
    )
}

# One-year intervals of the given polities, the support
# resolve_polity_lineage() walks towards.
.ryr_as_support <- function(polity_code, year) {
  tibble::tibble(polity_code = polity_code, start_year = as.integer(year)) |>
    dplyr::distinct() |>
    dplyr::mutate(end_year = .data$start_year + 1L)
}

# resolve_polity_lineage() on the (bucket, year) pairs, against `support`.
# Years the support does not cover cannot be resolved and are left out (their
# n stays NA and is stamped by the caller). The lineage warning is muffled: an
# unresolved pair is stamped by the caller instead.
.ryr_lineage <- function(pairs, support) {
  pairs <- dplyr::filter(pairs, .data$year %in% support$start_year)
  if (nrow(pairs) == 0L) {
    return(tibble::tibble(
      bucket = integer(),
      year = integer(),
      lineage_polity_code = character(),
      method_polity_lineage = character()
    ))
  }
  withCallingHandlers(
    resolve_polity_lineage(
      dplyr::rename(pairs, area_code = "bucket"),
      support
    ),
    whep_lineage_unresolved = function(w) invokeRestart("muffleWarning")
  ) |>
    dplyr::filter(!is.na(.data$lineage_polity_code)) |>
    dplyr::transmute(
      bucket = .data$area_code,
      .data$year,
      .data$lineage_polity_code,
      .data$method_polity_lineage
    )
}

.ryr_lineage_label <- function(method) {
  dplyr::case_when(
    method == "anchor" ~ "shared_polity",
    .default = method
  )
}

# National synthetic N per hectare of cropland, by bucket and year: a list of
# the N rows (`n`, whether or not the bucket has cropland that year) and the
# intensity rows (`intensity`, those with cropland).
.ryr_n_intensity <- function(bucket_years, data) {
  years <- sort(unique(c(bucket_years$year, .ryr_anchor_year())))
  years <- years[years >= .ryr_first_synthetic_year()]
  if (any(years < min(.ryr_smil_share_window()))) {
    years <- sort(unique(c(years, min(.ryr_smil_share_window()))))
  }
  fertilizer <- data$fertilizer %||%
    whep_read_file("faostat-fertilizer-nutrients")
  cropland <- data$cropland %||% get_arable_permanent_land(years = years)
  .ryr_require_cols(cropland, c("area_code", "year", "cropland_ha"), "cropland")
  if (nrow(cropland) == 0L) {
    cli::cli_abort(
      "{.arg cropland} has no rows, so no country has an N intensity.",
      class = "whep_regime_yield_columns"
    )
  }
  land <- cropland |>
    dplyr::transmute(
      area_code = as.integer(.data$area_code),
      year = as.integer(.data$year),
      cropland_ha = .data$cropland_ha,
      cropland_source = "own"
    )
  n_rows <- .ryr_synthetic_n(fertilizer) |>
    dplyr::filter(.data$year %in% years)
  land <- dplyr::bind_rows(land, .ryr_successor_cropland(n_rows, land, data))
  intensity <- n_rows |>
    dplyr::inner_join(land, by = c("area_code", "year")) |>
    dplyr::mutate(
      n_ha = dplyr::if_else(
        .data$cropland_ha > 0,
        .data$synthetic_n_t / .data$cropland_ha,
        NA_real_
      )
    ) |>
    dplyr::filter(!is.na(.data$n_ha)) |>
    dplyr::select(
      "area_code",
      "year",
      "synthetic_n_t",
      "cropland_ha",
      "n_ha",
      "n_source",
      "cropland_source"
    )
  list(n = n_rows, intensity = intensity)
}

# A polity with a Smil back-cast of N before 1961 but no cropland there
# (the USSR 228 and Czechoslovakia 51, whose codes have no LUH2 country and so
# no back-cast in get_arable_permanent_land()) takes its successors' combined
# back-cast cropland. The back-cast follows get_arable_permanent_land()'s own
# rule, applied to the successors together: their combined LUH2 cropland
# (c3ann + c4ann + c3nfx + c3per + c4per) rescaled to the polity's own FAOSTAT
# cropland in 1961, so the series meets FAOSTAT without a step. Successors are
# the buckets whose 1961 lineage walks back to the polity's 1961 polity.
.ryr_successor_cropland <- function(n_rows, land, data) {
  anchor_year <- min(.ryr_smil_share_window())
  proto <- dplyr::slice(land, 0L)
  missing <- n_rows |>
    dplyr::filter(.data$year < anchor_year) |>
    dplyr::anti_join(land, by = c("area_code", "year")) |>
    dplyr::semi_join(
      dplyr::filter(land, .data$year == anchor_year),
      by = "area_code"
    )
  if (nrow(missing) == 0L) {
    return(proto)
  }
  luh2 <- .read_luh2_cft(luh2_data = data$luh2) |>
    tibble::as_tibble() |>
    dplyr::transmute(
      bucket = as.integer(.data$area_code),
      year = as.integer(.data$year),
      .data$luh2_cropland
    )
  successors <- .ryr_successors(
    tibble::tibble(bucket = unique(missing$area_code), year = anchor_year),
    unique(luh2$bucket)
  )
  if (nrow(successors) == 0L) {
    return(proto)
  }
  combined <- successors |>
    dplyr::select("historical", "bucket") |>
    dplyr::inner_join(luh2, by = "bucket", relationship = "many-to-many") |>
    dplyr::summarise(
      luh2_ha = sum(.data$luh2_cropland),
      .by = c("historical", "year")
    )
  at_anchor <- combined |>
    dplyr::filter(.data$year == anchor_year, .data$luh2_ha > 0) |>
    dplyr::select("historical", luh2_anchor = "luh2_ha")
  fao_anchor <- land |>
    dplyr::filter(.data$year == anchor_year) |>
    dplyr::select(historical = "area_code", fao_anchor = "cropland_ha")
  missing |>
    dplyr::distinct(historical = .data$area_code, .data$year) |>
    dplyr::inner_join(combined, by = c("historical", "year")) |>
    dplyr::inner_join(at_anchor, by = "historical") |>
    dplyr::inner_join(fao_anchor, by = "historical") |>
    dplyr::transmute(
      area_code = .data$historical,
      .data$year,
      cropland_ha = .data$fao_anchor * .data$luh2_ha / .data$luh2_anchor,
      cropland_source = "successors_luh2_backcast"
    )
}

# FAOSTAT agricultural use of N per bucket (`.synthetic_n_country()`, the
# reader the synthetic-N chain uses) from 1961, and the Smil back-cast for
# 1913-1960.
.ryr_synthetic_n <- function(fertilizer) {
  faostat <- .synthetic_n_country(fertilizer) |>
    dplyr::mutate(n_source = "faostat")
  dplyr::bind_rows(faostat, .ryr_smil_backcast(faostat))
}

# The pre-1961 back-cast of prepare_nitrogen_inputs()
# (inst/scripts/prepare_spatialize_all.R, `.smil_synth_pre_1961()`): the Smil
# (2001) global series, linearly interpolated between its anchor years, times
# the country's mean 1961-1965 FAOSTAT N over the Smil global mean of the same
# years. The mean is taken from the series interpolated over 1913-1965, so
# the 1960 anchor enters it (15.6 Mt, not the 1965 value of 19.0 Mt); the
# script uses the same divisor since issue #1303.
.ryr_smil_backcast <- function(faostat) {
  window <- .ryr_smil_share_window()
  global <- tibble::tibble(
    year = seq.int(.ryr_first_synthetic_year(), max(window))
  ) |>
    dplyr::left_join(
      whep::smil_2001_synthetic_n_global |>
        dplyr::transmute(
          year = as.integer(.data$year),
          global_t = .data$global_kt_n * 1000
        ),
      by = "year"
    ) |>
    fill_linear(global_t, time_col = year) |>
    dplyr::select("year", "global_t")
  global_mean <- mean(global$global_t[global$year %in% window])
  shares <- faostat |>
    dplyr::filter(.data$year %in% window, .data$synthetic_n_t > 0) |>
    dplyr::summarise(
      share = mean(.data$synthetic_n_t) / global_mean,
      .by = "area_code"
    )
  global |>
    dplyr::filter(.data$year < min(window)) |>
    tidyr::expand_grid(shares) |>
    dplyr::transmute(
      year = .data$year,
      area_code = .data$area_code,
      synthetic_n_t = .data$global_t * .data$share,
      n_source = "smil_backcast"
    )
}

# -- Anomaly: LPJmL cell ratio over its crop x country normaliser -------------

.ryr_anomaly <- function(cells, run_dir, data) {
  items <- dplyr::select(.ryr_mapping(), "item_prod_code", "lpjml_crop")
  lpjml <- data$lpjml %||%
    .lrg_crop_yield(
      years = sort(unique(cells$year)),
      run_dir = run_dir,
      include_others = TRUE
    )
  .ryr_require_cols(
    lpjml,
    c(
      "lon",
      "lat",
      "year",
      "lpjml_crop",
      "yield_rainfed",
      "yield_irrigated",
      "method_regime_yield"
    ),
    "lpjml"
  )
  .ryr_check_supplied(lpjml, "lpjml", "the years of {.arg cells}")
  cell_map <- dplyr::distinct(cells, .data$lon, .data$lat, .data$area_code)
  window <- .ryr_window_sums(
    data$lpjml_window %||% .ryr_read_lpjml_window(run_dir)
  )
  grid <- .ryr_lpjml_grid(data$lpjml_grid %||% .ryr_read_lpjml_grid(run_dir))
  cells |>
    dplyr::left_join(
      grid,
      by = c("lon", "lat"),
      relationship = "many-to-one"
    ) |>
    dplyr::left_join(items, by = "item_prod_code") |>
    dplyr::left_join(
      .ryr_cell_ratio(lpjml),
      by = c("lon", "lat", "year", "lpjml_crop"),
      relationship = "many-to-one"
    ) |>
    dplyr::left_join(
      .ryr_cell_normal(window),
      by = c("lon", "lat", "lpjml_crop"),
      relationship = "many-to-one"
    ) |>
    dplyr::left_join(
      .ryr_lpjml_normal(cell_map, window),
      by = c("area_code", "lpjml_crop"),
      relationship = "many-to-one"
    ) |>
    .ryr_split_anomaly() |>
    dplyr::select(
      "lon",
      "lat",
      "area_code",
      "item_prod_code",
      "year",
      "ratio_spatial",
      "ratio_temporal",
      "method_ratio_spatial",
      "method_ratio_temporal"
    )
}

# The cells the LPJmL run's output covers at all (any band). A WHEP cell
# outside them (coastal and island cells of the run's land mask) has no LPJmL
# ratio for any crop; it keeps an anomaly of 1, stamped `no_lpjml_cell`, apart
# from the cells that are on the grid but lack a stand.
# The cells the LPJmL run covers. Without a run, the cells the pinned layer
# holds at the anchor year stand in: a cell with no crop stand at all is then
# stamped as having no LPJmL cell, with the same anomaly of 1.
.ryr_read_lpjml_grid <- function(run_dir) {
  run_dir <- .lrg_run_dir_or_pin(run_dir)
  if (is.null(run_dir)) {
    return(
      .lrg_read_pin(.ryr_anchor_year(), include_others = TRUE) |>
        dplyr::distinct(.data$lon, .data$lat)
    )
  }
  read_lpjml_npp(
    "harvestc",
    years = .ryr_anchor_year(),
    run_dir = run_dir
  ) |>
    dplyr::distinct(.data$lon, .data$lat)
}

.ryr_lpjml_grid <- function(grid) {
  .ryr_require_cols(grid, c("lon", "lat"), "lpjml_grid")
  .ryr_check_supplied(grid, "lpjml_grid", "the run's grid")
  grid |>
    .ryr_round_cells() |>
    dplyr::distinct(.data$lon, .data$lat) |>
    dplyr::mutate(in_lpjml = TRUE)
}

# The cell-year LPJmL ratio, NA where either regime has no stand or the
# rainfed stand no harvest.
.ryr_cell_ratio <- function(lpjml) {
  lpjml |>
    .ryr_round_cells() |>
    dplyr::transmute(
      .data$lon,
      .data$lat,
      year = as.integer(.data$year),
      .data$lpjml_crop,
      cell_ratio = dplyr::if_else(
        !is.na(.data$yield_irrigated) &
          !is.na(.data$yield_rainfed) &
          .data$yield_rainfed > 0,
        .data$yield_irrigated / .data$yield_rainfed,
        NA_real_
      ),
      lpjml_method = .data$method_regime_yield
    )
}

# The anomaly split in two. spatial = the cell's 1994-2023 ratio over the
# country's; temporal = the cell-year ratio over the cell's 1994-2023 ratio.
# Their product is the single anomaly (cell-year over country) wherever all
# three ratios exist. Each part falls back to 1, stamped, where its ratios do
# not. A cell with a 1994-2023 ratio always has a country one, because the
# country pools that cell's own positive sums, so the spatial part has one
# fallback only; a missing country ratio beside a cell one is an error.
.ryr_split_anomaly <- function(x) {
  orphan <- !is.na(x$cell_normal) & is.na(x$lpjml_normal)
  if (any(orphan)) {
    cli::cli_abort(
      "{sum(orphan)} cell-crop-year{?s} ha{?s/ve} a cell normal but no
       country normal.",
      class = "whep_regime_yield_lpjml"
    )
  }
  x |>
    dplyr::mutate(
      method_ratio_spatial = dplyr::case_when(
        is.na(.data$in_lpjml) ~ "no_lpjml_cell",
        is.na(.data$cell_normal) ~ "no_cell_normal",
        .default = "lpjml"
      ),
      ratio_spatial = dplyr::if_else(
        .data$method_ratio_spatial == "lpjml",
        .data$cell_normal / .data$lpjml_normal,
        1
      ),
      method_ratio_temporal = dplyr::case_when(
        is.na(.data$in_lpjml) ~ "no_lpjml_cell",
        is.na(.data$cell_ratio) ~ "no_cell_ratio",
        is.na(.data$cell_normal) ~ "no_cell_normal",
        .data$lpjml_method == "lpjml_band_harvest_recycled_climate" ~
          "lpjml_recycled_climate",
        .default = "lpjml"
      ),
      ratio_temporal = dplyr::if_else(
        .data$method_ratio_temporal %in% c("lpjml", "lpjml_recycled_climate"),
        .data$cell_ratio / .data$cell_normal,
        1
      )
    )
}

# An empty LPJmL layer would turn every anomaly into the fallback of 1 and
# look like a finished run, so its absence is refused rather than absorbed.
.ryr_check_supplied <- function(x, name, span) {
  if (nrow(x) > 0L) {
    return(invisible(x))
  }
  cli::cli_abort(
    c(
      "{.arg {name}} has no LPJmL rows for {span}.",
      i = "Every anomaly would fall back to 1 without them."
    ),
    class = "whep_regime_yield_lpjml"
  )
}

.ryr_round_cells <- function(x) {
  dplyr::mutate(x, lon = round(.data$lon, 2), lat = round(.data$lat, 2))
}

# The window years, read one at a time (a year of LPJmL bands is millions of
# rows) and kept at the crop grain with the stand fractions.
.ryr_read_lpjml_window <- function(run_dir) {
  purrr::map(.ryr_normal_window(), function(year) {
    .lrg_crop_yield(years = year, run_dir = run_dir, include_others = TRUE)
  }) |>
    dplyr::bind_rows()
}

# The window rows turned into stand areas and yield-weighted sums, the terms
# every 1994-2023 ratio is pooled from: a_* is the stand area (stand fraction
# x the cell's geometric area), s_* = a_* x yield. A stand that is absent (NA
# yield) has no area and adds nothing.
.ryr_window_sums <- function(lpjml_window) {
  .ryr_require_cols(
    lpjml_window,
    c(
      "lon",
      "lat",
      "year",
      "lpjml_crop",
      "yield_rainfed",
      "yield_irrigated",
      "stand_frac_rainfed",
      "stand_frac_irrigated"
    ),
    "lpjml_window"
  )
  window <- dplyr::filter(lpjml_window, .data$year %in% .ryr_normal_window())
  .ryr_check_supplied(window, "lpjml_window", "1994-2023")
  window |>
    .ryr_round_cells() |>
    dplyr::mutate(
      cell_ha = .cell_area_ha_lat(.data$lat),
      a_i = dplyr::if_else(
        is.na(.data$yield_irrigated),
        0,
        .data$stand_frac_irrigated * .data$cell_ha
      ),
      a_r = dplyr::if_else(
        is.na(.data$yield_rainfed),
        0,
        .data$stand_frac_rainfed * .data$cell_ha
      ),
      s_i = .data$a_i * dplyr::coalesce(.data$yield_irrigated, 0),
      s_r = .data$a_r * dplyr::coalesce(.data$yield_rainfed, 0)
    ) |>
    dplyr::select("lon", "lat", "lpjml_crop", "a_i", "a_r", "s_i", "s_r")
}

# The pooled irrigated:rainfed ratio of summed window terms, per `by`; groups
# where either regime has no area or no harvest are dropped (undefined).
.ryr_pooled_ratio <- function(sums, by, name) {
  sums |>
    dplyr::summarise(
      dplyr::across(c("a_i", "a_r", "s_i", "s_r"), sum),
      .by = dplyr::all_of(by)
    ) |>
    dplyr::filter(
      .data$a_i > 0,
      .data$a_r > 0,
      .data$s_i > 0,
      .data$s_r > 0
    ) |>
    dplyr::mutate(
      !!name := (.data$s_i / .data$a_i) / (.data$s_r / .data$a_r)
    ) |>
    dplyr::select(dplyr::all_of(c(by, name)))
}

# Each cell's own 1994-2023 ratio, pooled the same way as the country's.
.ryr_cell_normal <- function(window) {
  .ryr_pooled_ratio(window, c("lon", "lat", "lpjml_crop"), "cell_normal")
}

# LPJmL's irrigated:rainfed ratio per crop and country over the window: each
# regime's yield is the sum of yield x stand area over the sum of
# stand area, pooled over the country's cells and the window years, the
# LPJmL counterpart of the SPAM anchor. A cell of several area codes counts
# in each.
.ryr_lpjml_normal <- function(cell_map, window) {
  window |>
    dplyr::inner_join(
      cell_map,
      by = c("lon", "lat"),
      relationship = "many-to-many"
    ) |>
    .ryr_pooled_ratio(c("area_code", "lpjml_crop"), "lpjml_normal")
}

# -- Combination --------------------------------------------------------------

# The anomalies scale only the excess gap over 1. The long-term excess
# `(ratio_level - 1) x ratio_spatial` is capped at 9, i.e. the long-term R at
# 10; the temporal part scales the capped excess and is not
# capped, so only a bad year takes R past 10. Every factor is non-negative, so
# R >= 1 and R = 1 wherever the level is 1, whatever the anomaly.
.ryr_combine <- function(x) {
  cap <- .ryr_level_cap() - 1
  x |>
    dplyr::mutate(
      ratio_anchor = pmax(1, .data$ratio_spam),
      ratio_level = 1 + (.data$ratio_anchor - 1) * .data$n_scale,
      excess_raw = (.data$ratio_level - 1) * .data$ratio_spatial,
      ratio_long_term = 1 + pmin(.data$excess_raw, cap),
      ratio_anomaly = .data$ratio_spatial * .data$ratio_temporal,
      ratio_unbounded = 1 +
        (.data$ratio_long_term - 1) * .data$ratio_temporal,
      method_regime_yield = .ryr_stamp(
        anchor_floor = .data$ratio_spam < 1,
        level_cap = .data$excess_raw > cap
      )
    ) |>
    dplyr::select(
      "lon",
      "lat",
      "area_code",
      "item_prod_code",
      "year",
      "ratio_spam",
      "ratio_anchor",
      "ratio_level",
      "ratio_spatial",
      "ratio_long_term",
      "ratio_temporal",
      "ratio_anomaly",
      "ratio_unbounded",
      "spam_crop_used",
      "method_ratio_anchor",
      "method_ratio_trend",
      "method_ratio_n_2010",
      "method_ratio_cropland",
      "method_dominance",
      "method_ratio_spatial",
      "method_ratio_temporal",
      "method_regime_yield"
    )
}

# The adjustments that fired, joined by ";", or "none". An NA flag (a ratio
# that could not be built) counts as not fired.
.ryr_stamp <- function(...) {
  tokens <- purrr::imap(list(...), \(fired, name) {
    dplyr::if_else(dplyr::coalesce(fired, FALSE), name, NA_character_)
  })
  joined <- purrr::reduce(tokens, \(a, b) {
    dplyr::case_when(
      is.na(a) ~ b,
      is.na(b) ~ a,
      .default = paste(a, b, sep = ";")
    )
  })
  dplyr::coalesce(joined, "none")
}

# -- Split and bound ----------------------------------------------------------

# Each item's national yields (production over harvested area) over
# 1961-2023, one row per area, item and year, with the area's WHEP region. A
# duplicated national value would weigh twice, so it is refused.
.ryr_national_yields <- function(production) {
  .ryr_require_cols(
    production,
    c("year", "area_code", "item_prod_code", "unit", "value"),
    "production"
  )
  long <- production |>
    dplyr::filter(
      .data$unit %in% c("tonnes", "ha"),
      .data$year %in% .ryr_bound_years()
    ) |>
    dplyr::transmute(
      area_code = as.integer(.data$area_code),
      item_prod_code = as.integer(.data$item_prod_code),
      year = as.integer(.data$year),
      .data$unit,
      .data$value
    )
  if (anyDuplicated(dplyr::select(long, -"value")) > 0L) {
    cli::cli_abort(
      "{.arg production} has more than one value for an area, item, year and
       unit.",
      class = "whep_regime_yield_production"
    )
  }
  long |>
    tidyr::pivot_wider(names_from = "unit", values_from = "value") |>
    ensure_columns(tibble::tibble(tonnes = double(), ha = double())) |>
    dplyr::filter(.data$tonnes > 0, .data$ha > 0) |>
    dplyr::transmute(
      .data$area_code,
      .data$item_prod_code,
      yield = .data$tonnes / .data$ha
    ) |>
    dplyr::left_join(.ryr_region_map(), by = "area_code")
}

# The 99th and 1st percentiles of a pool of yields, per `by`.
.ryr_pool_bounds <- function(yields, by) {
  yields |>
    dplyr::summarise(
      yield_max = stats::quantile(
        .data$yield,
        .ryr_bound_prob(),
        names = FALSE
      ),
      yield_min = stats::quantile(
        .data$yield,
        .ryr_floor_prob(),
        names = FALSE
      ),
      .by = dplyr::all_of(by)
    )
}

# The items that stand in for an item without FAOSTAT yields of its own: the
# items that directly carry one of its SPAM crops in
# [regime_yield_crop_mapping] (`spam_basis` `"direct"` or
# `"direct_aggregate"`, a single SPAM crop). An item's crops are the codes of
# its `spam_crop`, split on `+` and `|`.
.ryr_bound_donors <- function(items) {
  mapping <- .ryr_mapping()
  carriers <- mapping |>
    dplyr::filter(
      .data$spam_basis %in% c("direct", "direct_aggregate"),
      !stringr::str_detect(.data$spam_crop, "[+|]")
    ) |>
    dplyr::select(donor = "item_prod_code", member = "spam_crop")
  mapping |>
    dplyr::filter(.data$item_prod_code %in% items) |>
    dplyr::mutate(member = stringr::str_split(.data$spam_crop, "[+|]")) |>
    tidyr::unnest_longer("member", ptype = character()) |>
    dplyr::inner_join(carriers, by = "member", relationship = "many-to-many") |>
    dplyr::filter(.data$donor != .data$item_prod_code) |>
    dplyr::distinct(.data$item_prod_code, .data$donor)
}

# The yield bounds of each item, in the order they are tried: its own yields,
# then the pooled yields of the items directly carrying its SPAM crop. Under
# `bound = "region"` each is tried in the cell's region first and then in the
# world, so every item with any stand-in gets both bounds; the stamp says
# which pool answered.
.ryr_yield_bounds <- function(production, bound, items) {
  yields <- .ryr_national_yields(production)
  donors <- .ryr_bound_donors(items)
  donor_yields <- donors |>
    dplyr::inner_join(
      dplyr::rename(yields, donor = "item_prod_code"),
      by = "donor",
      relationship = "many-to-many"
    ) |>
    dplyr::select(-"donor")
  scopes <- if (identical(bound, "region")) c("region", "global") else "global"
  purrr::map(scopes, \(scope) {
    .ryr_scope_bounds(yields, donor_yields, scope, bound)
  }) |>
    rlang::set_names(scopes)
}

# One pool's bounds: per item (and region, for the regional pool), from the
# item's own yields, else from its stand-ins'. Under `bound = "region"` the
# world's pool is the second resort and is stamped `_global`.
.ryr_scope_bounds <- function(yields, donor_yields, scope, bound) {
  by <- if (scope == "region") {
    c("item_prod_code", "region")
  } else {
    "item_prod_code"
  }
  suffix <- if (identical(bound, "region") && scope == "global") {
    "_global"
  } else {
    ""
  }
  if (scope == "region") {
    yields <- dplyr::filter(yields, !is.na(.data$region))
    donor_yields <- dplyr::filter(donor_yields, !is.na(.data$region))
  }
  own <- .ryr_pool_bounds(yields, by)
  via <- .ryr_pool_bounds(donor_yields, by) |>
    dplyr::anti_join(own, by = by)
  dplyr::bind_rows(
    dplyr::mutate(own, method_bound_source = paste0("own_yields", suffix)),
    dplyr::mutate(
      via,
      method_bound_source = paste0("bound_via_spam_crop", suffix)
    )
  )
}

# WHEP region of each area code (`regions_full`, `code` -> `region`).
.ryr_region_map <- function() {
  whep::regions_full |>
    dplyr::transmute(
      area_code = as.integer(.data$code),
      region = .data$region
    ) |>
    dplyr::filter(!is.na(.data$area_code)) |>
    dplyr::distinct()
}

# Join the bounds onto the cells: the regional pool first where `bound =
# "region"`, the global pool for what it leaves.
.ryr_attach_bounds <- function(cells, bounds, bound) {
  cells <- dplyr::mutate(cells, .ryr_item = as.integer(.data$item_prod_code))
  global <- dplyr::rename(bounds$global, .ryr_item = "item_prod_code")
  if (identical(bound, "region")) {
    cells <- cells |>
      dplyr::left_join(
        dplyr::rename(.ryr_region_map(), .ryr_region = "region"),
        by = "area_code",
        relationship = "many-to-one"
      ) |>
      dplyr::left_join(
        dplyr::rename(
          bounds$region,
          .ryr_item = "item_prod_code",
          .ryr_region = "region"
        ),
        by = c(".ryr_item", ".ryr_region"),
        relationship = "many-to-one"
      ) |>
      dplyr::left_join(
        global,
        by = ".ryr_item",
        suffix = c("", ".ryr_g"),
        relationship = "many-to-one"
      ) |>
      dplyr::mutate(
        .ryr_use_g = is.na(.data$yield_max),
        yield_max = dplyr::if_else(
          .data$.ryr_use_g,
          .data$yield_max.ryr_g,
          .data$yield_max
        ),
        yield_min = dplyr::if_else(
          .data$.ryr_use_g,
          .data$yield_min.ryr_g,
          .data$yield_min
        ),
        method_bound_source = dplyr::if_else(
          .data$.ryr_use_g,
          .data$method_bound_source.ryr_g,
          .data$method_bound_source
        )
      ) |>
      dplyr::select(-dplyr::ends_with(".ryr_g"))
  } else {
    cells <- dplyr::left_join(
      cells,
      global,
      by = ".ryr_item",
      relationship = "many-to-one"
    )
  }
  dplyr::mutate(
    cells,
    method_bound_source = dplyr::coalesce(.data$method_bound_source, "none"),
    method_yield_bound = bound
  )
}

# The conservation split, with the two plausibility bounds lowering R where
# they bind: first the irrigated-yield ceiling, then the rainfed-yield
# floor. Both only ever lower R, and lowering R raises Y_r and lowers
# Y_i, so the floor cannot break the ceiling. A cell-crop with area in one
# regime only is a trivial split: all its production is on that regime and
# no ratio applies, so `ratio` is set to 1 and the bounds do not apply.
.ryr_split <- function(cells) {
  cells |>
    dplyr::mutate(
      .ryr_p = .data$production_t,
      .ryr_ar = .data$rainfed_ha,
      .ryr_ai = .data$irrigated_ha,
      method_regime_split = dplyr::case_when(
        .data$.ryr_ar + .data$.ryr_ai <= 0 ~ "no_area",
        .data$.ryr_ai <= 0 ~ "trivial_rainfed_only",
        .data$.ryr_ar <= 0 ~ "trivial_irrigated_only",
        is.na(.data$ratio_unbounded) ~ "no_ratio",
        .default = "yield_ratio"
      ),
      ratio = dplyr::case_when(
        .data$method_regime_split == "yield_ratio" ~ .data$ratio_unbounded,
        startsWith(.data$method_regime_split, "trivial") ~ 1,
        .default = NA_real_
      )
    ) |>
    .ryr_apply_ceiling() |>
    .ryr_apply_floor() |>
    dplyr::mutate(
      yield_rainfed = .data$.ryr_p /
        (.data$.ryr_ar + .data$ratio * .data$.ryr_ai),
      yield_irrigated = .data$ratio * .data$yield_rainfed
    ) |>
    dplyr::select(-dplyr::starts_with(".ryr_")) |>
    dplyr::relocate(
      "ratio",
      "yield_rainfed",
      "yield_irrigated",
      "yield_max",
      "yield_min",
      "method_regime_split",
      "method_regime_bound",
      "method_rainfed_floor",
      "method_bound_source",
      "method_yield_bound",
      .after = dplyr::last_col()
    )
}

# Where Y_i = R P / (A_r + R A_i) would pass Y_max, R is solved from
# Y_i = Y_max, never below 1. A clip means R (P - Y_max A_i) > Y_max A_r >= 0,
# so the denominator is positive.
.ryr_apply_ceiling <- function(x) {
  x |>
    dplyr::mutate(
      .ryr_bounded = .data$method_regime_split == "yield_ratio",
      .ryr_yi = .data$ratio *
        .data$.ryr_p /
        (.data$.ryr_ar + .data$ratio * .data$.ryr_ai),
      .ryr_clip = .data$.ryr_bounded &
        !is.na(.data$yield_max) &
        .data$.ryr_yi > .data$yield_max,
      .ryr_r_star = .data$yield_max *
        .data$.ryr_ar /
        (.data$.ryr_p - .data$yield_max * .data$.ryr_ai),
      ratio = dplyr::if_else(
        dplyr::coalesce(.data$.ryr_clip, FALSE),
        pmax(1, .data$.ryr_r_star),
        .data$ratio
      ),
      method_regime_bound = .ryr_bound_stamp(
        .data$method_regime_split,
        .data$yield_max,
        .data$.ryr_clip,
        .data$.ryr_r_star < 1,
        c("clipped", "clipped_at_one")
      )
    )
}

# Where Y_r = P / (A_r + R A_i) would fall below Y_min, R is solved from
# Y_r = Y_min, R = (P - Y_min A_r) / (Y_min A_i), never below 1.
.ryr_apply_floor <- function(x) {
  x |>
    dplyr::mutate(
      .ryr_floored = .data$method_regime_split == "yield_ratio",
      .ryr_yr = .data$.ryr_p /
        (.data$.ryr_ar + .data$ratio * .data$.ryr_ai),
      .ryr_low = .data$.ryr_floored &
        !is.na(.data$yield_min) &
        .data$.ryr_yr < .data$yield_min,
      .ryr_r_floor = (.data$.ryr_p - .data$yield_min * .data$.ryr_ar) /
        (.data$yield_min * .data$.ryr_ai),
      ratio = dplyr::if_else(
        dplyr::coalesce(.data$.ryr_low, FALSE),
        pmax(1, .data$.ryr_r_floor),
        .data$ratio
      ),
      method_rainfed_floor = .ryr_bound_stamp(
        .data$method_regime_split,
        .data$yield_min,
        .data$.ryr_low,
        .data$.ryr_r_floor < 1,
        c("rainfed_floor", "rainfed_floor_at_one")
      )
    )
}

# The stamp of one bound: `names[1]` where it bound, `names[2]` where it was
# lowered to 1 and still does not hold; trivial and ratio-less rows say so.
.ryr_bound_stamp <- function(split, limit, fired, below_one, names) {
  dplyr::case_when(
    startsWith(split, "trivial") ~ "not_applicable_trivial",
    split != "yield_ratio" ~ "not_applicable",
    is.na(limit) ~ "no_bound",
    !fired ~ "not_binding",
    below_one ~ names[2],
    .default = names[1]
  )
}
