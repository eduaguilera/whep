# The irrigated:rainfed yield ratio R per cell, crop and year, and the split of
# a cell-crop's production between its rainfed and irrigated hectares that
# keeps that production (issue #1233; plan decisions D15-D24, user,
# 2026-09-24).
#
#   R_anchor  SPAM2010 v2.0 irrigated yield over all-rainfed yield, per crop
#             and country, each yield the ratio of the country's sums
#             (production / harvested area) (D15, D16). Floored at 1.
#   R_level   1 + (R_anchor - 1) * n_t / n_2010, n = national synthetic N per
#             hectare of cropland (D16), n_t through the polity lineage (D23).
#   spatial   LPJmL cell ratio over the country ratio, both 1994-2023 (D22).
#   long_term min(10, R_level * spatial) (D15 cap, D22).
#   temporal  LPJmL cell-year ratio over the cell's 1994-2023 ratio (D22).
#   R         max(1, long_term * temporal); only a bad year passes 10.
#
# split_regime_yield() then gives Y_r = P / (A_r + R A_i), Y_i = R Y_r, so
# A_r Y_r + A_i Y_i = P, and lowers R where Y_i would pass the D20 bound.

#' Build the irrigated:rainfed yield ratio per cell, crop and year.
#'
#' @description
#' Gives each cell-crop-year the ratio `R` of its irrigated yield to its
#' rainfed yield, the weight that splits a crop's production, synthetic
#' nitrogen and harvest removals between its two regimes (issue #1233). `R`
#' combines three sources, following plan decisions D15-D24:
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
#' 3. **Anomaly**, in two parts (D22). The **spatial** part is the cell's
#'    LPJmL irrigated:rainfed ratio over 1994-2023 divided by the country's,
#'    so drier places get a larger gap. The **temporal** part is the cell's
#'    ratio in year `t` divided by its own 1994-2023 ratio, so worse years
#'    get a larger gap.
#'
#' The long-term component `R_level * spatial` is capped at 10 (D15, D22);
#' the temporal part is not, so only a bad year takes `R` past 10. The ratio
#' is `R = max(1, min(10, R_level * spatial) * temporal)`.
#' [split_regime_yield()] applies it to a cell's production and areas.
#'
#' @section Anchor:
#' How `spam_crop` is read is fixed by `spam_basis` in
#' [regime_yield_crop_mapping]:
#' - A single crop, or crops joined by `+` with basis `"direct"` (millet,
#'   coffee): harvested area and production are summed over the crops and the
#'   ratio is taken of the sums.
#' - `"composite_weighted"` (the forage crops, D18, D19): the mean of the
#'   member crops' ratios weighted by each member's SPAM harvested area
#'   (irrigated plus rainfed) in the country. A member with no irrigated or no
#'   rainfed yield there is dropped and the weights of the others renormalised
#'   (D21); with no member left, the global composite is used.
#' - `"product_dominance"` (Linum 772, Hemp 776, D17): `ooil` where the
#'   country's FAOSTAT seed production (linseed 333, hempseed 336) exceeds its
#'   fibre production (flax 773, true hemp 777), otherwise `ofib`, both summed
#'   over 1961-2023. A country with neither product takes the world's
#'   dominant product and its global ratio.
#'
#' A country with no irrigated or no rainfed yield for the crop in SPAM takes
#' the crop's global ratio (the ratio of the world's sums). Countries are
#' matched by ISO3 code onto WHEP's polity buckets, never by name.
#'
#' @section Level:
#' `n_t` is the country's synthetic N over its cropland in year `t`:
#' FAOSTAT's agricultural use of nitrogen (the `faostat-fertilizer-nutrients`
#' pin) from 1961, back-cast to 1913 with the Smil (2001) global series scaled
#' by the country's 1961-1965 share ([smil_2001_synthetic_n_global], the
#' back-cast of the spatialization scripts' `prepare_nitrogen_inputs()`), and
#' zero before 1913, when there was no synthetic nitrogen. The cropland is
#' [get_arable_permanent_land()] (FAOSTAT from 1961, LUH2 back-cast before).
#'
#' A country that reports no N in year `t` takes the N per hectare of the
#' polity that reported fertiliser for its territory that year (D23), found by
#' walking the `predecessor` edges of [polities] with
#' [resolve_polity_lineage()]: Russia before 1992 takes the USSR's N over the
#' USSR's cropland. A historical polity with no 2010 value of its own (the
#' USSR) takes its successors' combined 2010 N over their combined cropland.
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
#' SPAM anchor. The spatial part is 1 where the cell or the country has no
#' 1994-2023 ratio; the temporal part is 1 where the cell has no ratio that
#' year (a regime without a stand, or no rainfed harvest) or no 1994-2023
#' ratio. `ratio_anomaly` is their product: the cell-year ratio over the
#' country's wherever all three ratios exist, i.e. the single anomaly of D15.
#'
#' @details
#' Four implementation choices the decisions left open, accepted as D24:
#' - The 1994-2023 normalisers pool every stand, weighted by stand area
#'   (stand fraction times the cell's geometric area; the land fraction is not
#'   applied), rather than averaging cell ratios.
#' - Linum and Hemp dominance pools each country's 1961-2023 production; a
#'   tie goes to the fibre.
#' - The floor at 1 applies to the finished composite, not to its members.
#' - The yield bound of [split_regime_yield()] pools every production row of
#'   1961-2023 with positive tonnes and area, whatever its `source`.
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
#'   - `production`: [get_primary_production()] output, for the Linum and
#'     Hemp dominance.
#'   - `lpjml`: LPJmL crop yields for the years of `cells`, at the crop grain
#'     (`lon`, `lat`, `year`, `lpjml_crop`, `yield_rainfed`, `yield_irrigated`,
#'     `method_regime_yield`).
#'   - `lpjml_window`: the same for the normaliser years, with the stand
#'     fractions `stand_frac_rainfed`, `stand_frac_irrigated`. Only rows in
#'     1994-2023 are used.
#' @param example If `TRUE`, return a small fixture instead of building the
#'   ratio. Defaults to `FALSE`.
#' @return A tibble with one row per cell-crop-year of `cells`:
#'   - `lon`, `lat`, `area_code`, `item_prod_code`, `year`: the key.
#'   - `ratio_spam`: the SPAM2010 ratio as computed, before the floor.
#'   - `ratio_anchor`: `max(1, ratio_spam)`.
#'   - `ratio_level`: the anchor carried to `year` by synthetic N.
#'   - `ratio_spatial`: the cell's 1994-2023 LPJmL ratio over the country's.
#'   - `ratio_long_term`: `min(10, ratio_level * ratio_spatial)`.
#'   - `ratio_temporal`: the cell-year LPJmL ratio over the cell's 1994-2023
#'     ratio.
#'   - `ratio_anomaly`: `ratio_spatial * ratio_temporal`, kept for comparison
#'     with the single anomaly of D15.
#'   - `ratio`: `max(1, ratio_long_term * ratio_temporal)`; `NA` where the
#'     level is.
#'   - `spam_crop_used`: the SPAM crop code(s) the anchor came from (for Linum
#'     and Hemp, the product chosen for the country).
#'   - `method_ratio_anchor`: `"spam_country"`, `"spam_global"`,
#'     `"spam_composite_country"`, `"spam_composite_global"`,
#'     `"spam_dominance_country"`, `"spam_dominance_global"`,
#'     `"spam_dominance_world"` or `"spam_none"` (no SPAM ratio at all).
#'   - `method_ratio_trend`: where `n_t` came from: `"faostat"` or
#'     `"smil_backcast"` (the country's own), either suffixed
#'     `"_predecessor"`, `"_sibling_interval"` or `"_shared_polity"` (from
#'     the polity reporting for it, by the lineage step that found it); or
#'     `"pre_synthetic_n"`, `"n_2010_zero"`, `"no_n_t"`, `"no_n_2010"`.
#'   - `method_ratio_n_2010`: `"own"`, `"successors"`, `"none"` or
#'     `"not_needed"` (before 1913).
#'   - `method_ratio_spatial`: `"lpjml"` or `"no_cell_normal"`.
#'   - `method_ratio_temporal`: `"lpjml"`, `"lpjml_recycled_climate"` (years
#'     before 1901), `"no_cell_ratio"` or `"no_cell_normal"`.
#'   - `method_regime_yield`: the adjustments applied, joined by `";"`:
#'     `"anchor_floor"` (SPAM ratio below 1), `"level_cap"` (long-term
#'     component above 10), `"ratio_floor"` (long-term times temporal below
#'     1), or `"none"`.
#'
#'   Plus the polity columns below.
#'
#' @inheritSection whep_polity_columns Polity columns
#' @source Yu, Q. et al. (2020). A cultivated planet in 2010 -- Part 2: The
#'   global gridded agricultural-production maps. Earth System Science Data
#'   12, 3545-3572. \doi{10.5194/essd-12-3545-2020}. LPJmL 6.1.1 band
#'   harvests as in [read_lpjml_regime_yield()]. FAOSTAT Fertilizers by
#'   Nutrient and Land Use. Smil, V. (2001) *Enriching the Earth*, MIT Press.
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
#' `A_r * Y_r + A_i * Y_i = P` (plan decision D15).
#'
#' Where the irrigated yield would exceed the item's plausible maximum
#' `Y_max`, `R` is lowered until `Y_i = Y_max`,
#' `R = Y_max * A_r / (P - Y_max * A_i)`, but never below 1 (D20). `Y_max` is
#' the 99th percentile of the item's national yields (production over
#' harvested area) in [get_primary_production()], pooled over countries and
#' the years 1961-2023.
#'
#' A regime with no area still gets the yield the ratio implies, which
#' multiplies zero area; only a cell-crop with no area at all, or no ratio,
#' gets `NA` yields.
#'
#' @param cells A tibble with `area_code`, `item_prod_code`, `production_t`
#'   (production in the units of [get_primary_production()]'s `"tonnes"`
#'   rows), `rainfed_ha`, `irrigated_ha` and `ratio` (as
#'   [build_regime_yield_ratio()] returns it). Other columns are kept.
#' @param bound Which pool of national yields sets `Y_max`: `"global"`
#'   (default, all countries, D20) or `"region"` (the countries of the cell's
#'   WHEP region, the `region` column of [regions_full]).
#' @param production Optional [get_primary_production()] output (`year`,
#'   `area_code`, `item_prod_code`, `unit`, `value`) used instead of building
#'   it.
#' @return `cells` with:
#'   - `ratio_split`: the ratio used, after the bound.
#'   - `yield_rainfed`, `yield_irrigated`: production per hectare of each
#'     regime.
#'   - `yield_max`: the bound applied; `NA` where the item has no yields to
#'     set one.
#'   - `method_regime_split`: `"yield_ratio"`, `"rainfed_only"`,
#'     `"irrigated_only"`, `"no_area"` or `"no_ratio"`.
#'   - `method_regime_bound`: `"not_binding"`, `"clipped"`,
#'     `"clipped_at_one"` (lowered to 1 and still above `Y_max`: the mean
#'     yield itself exceeds it), `"no_bound"` or `"not_applicable"` (no
#'     irrigated area, no area or no ratio).
#'   - `method_yield_bound`: the `bound` chosen.
#' @export
#' @examples
#' cells <- tibble::tribble(
#'   ~area_code, ~item_prod_code, ~production_t, ~rainfed_ha, ~irrigated_ha,
#'   ~ratio,
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
      "ratio"
    ),
    "cells"
  )
  .ryr_check_split_values(cells)
  production <- production %||%
    get_primary_production(
      years = .ryr_bound_years()
    )
  cells |>
    .ryr_attach_yield_max(.ryr_yield_max(production, bound), bound) |>
    .ryr_split()
}

# -- Constants ---------------------------------------------------------------

# D15 (user, 2026-09-24): the normal-year component is capped at 10; D22
# applies the cap to `ratio_level x ratio_spatial`. The user's assumption, not
# a sourced value.
.ryr_level_cap <- function() {
  10
}

# D16: SPAM2010 v2.0 is the single anchor year.
.ryr_anchor_year <- function() {
  2010L
}

# D15 asks for LPJmL's "own crop x country 30-year mean". Implemented as the
# 30 most recent years of the run (it ends in 2023), fixed for every target
# year so the normaliser is one number per crop and country.
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

# D20: national yields pooled over 1961-2023, 99th percentile.
.ryr_bound_years <- function() {
  1961:2023
}

.ryr_bound_prob <- function() {
  0.99
}

# D17: the two products whose harvested area WHEP books jointly on one item
# (inst/extdata/harmonization/primary_double.csv, `Multi_area`), and the SPAM
# aggregate each product belongs to (Yu et al. 2020, Table S3: linseed and
# hempseed in `ooil`, flax and true hemp in `ofib`).
#
# Measured 2026-09-24: get_primary_production(years = 1961:2023) has no row
# for flax 773 at all (FAOSTAT's current QCL books flax as 771 "Flax, raw or
# retted", e.g. France 2010: 372,100 t), and its Linum 772 tonnes equal
# linseed 333's. So on today's data every country's Linum resolves to `ooil`.
# Hemp is unaffected (hempseed 336 and true hemp 777 both have rows).
.ryr_dominance_products <- function() {
  tibble::tribble(
    ~item_prod_code, ~seed_code, ~fibre_code,
    772L,            333L,       773L,
    776L,            336L,       777L
  )
}

.ryr_data_keys <- function() {
  c(
    "spam",
    "fertilizer",
    "cropland",
    "production",
    "lpjml",
    "lpjml_window"
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
    production <- data$production %||%
      get_primary_production(years = .ryr_bound_years())
    parts$dominance <- .ryr_anchor_dominance(
      pairs[kind == "dominance", ],
      spam,
      production
    )
  }
  dplyr::bind_rows(parts) |>
    dplyr::select(
      "bucket",
      "item_prod_code",
      "ratio_spam",
      "spam_crop_used",
      "method_ratio_anchor"
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

# D18, D19, D21: the member crops' ratios weighted by their harvested area;
# undefined members dropped, the rest renormalised; none left, the global
# composite (global ratios, global weights).
.ryr_anchor_composite <- function(pairs, spam) {
  .ryr_scope_ratios(pairs, spam, "\\+", .ryr_weigh_members) |>
    .ryr_pick_scope("spam_composite") |>
    dplyr::mutate(spam_crop_used = .data$spam_crop)
}

# D17: the SPAM aggregate of the dominant product, per country. Where the
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

# FAOSTAT tonnes of the seed and fibre products per area, 1961-2023.
.ryr_dominance_tonnes <- function(production) {
  .ryr_require_cols(
    production,
    c("year", "area_code", "item_prod_code", "unit", "value"),
    "production"
  )
  products <- .ryr_dominance_products()
  codes <- c(products$seed_code, products$fibre_code)
  production |>
    dplyr::transmute(
      area_code = as.integer(.data$area_code),
      item_prod_code = as.integer(.data$item_prod_code),
      year = as.integer(.data$year),
      .data$unit,
      .data$value
    ) |>
    dplyr::filter(
      .data$unit == "tonnes",
      .data$item_prod_code %in% codes,
      .data$year %in% .ryr_bound_years(),
      !is.na(.data$value)
    ) |>
    dplyr::summarise(
      value = sum(.data$value),
      .by = c("area_code", "item_prod_code")
    )
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

# -- Level: the fertiliser-N scaling of D16 -----------------------------------

.ryr_trend <- function(bucket_years, data) {
  needs_n <- any(bucket_years$year >= .ryr_first_synthetic_year())
  intensity <- if (needs_n) {
    .ryr_n_intensity(bucket_years, data)
  } else {
    .ryr_n_intensity_proto()
  }
  n_t <- .ryr_n_t(bucket_years, intensity)
  n_2010 <- .ryr_n_2010(bucket_years, intensity)
  bucket_years |>
    dplyr::left_join(n_t, by = c("bucket", "year")) |>
    dplyr::left_join(n_2010, by = c("bucket", "year")) |>
    dplyr::mutate(
      method_ratio_trend = dplyr::case_when(
        .data$year < .ryr_first_synthetic_year() ~ "pre_synthetic_n",
        is.na(.data$n_ha_2010) ~ "no_n_2010",
        .data$n_ha_2010 == 0 ~ "n_2010_zero",
        is.na(.data$n_ha) ~ "no_n_t",
        .default = .data$n_path
      ),
      method_ratio_n_2010 = dplyr::if_else(
        .data$year < .ryr_first_synthetic_year(),
        "not_needed",
        dplyr::coalesce(.data$n_2010_path, "none")
      ),
      n_scale = dplyr::case_when(
        .data$method_ratio_trend %in% c("pre_synthetic_n", "n_2010_zero") ~ 0,
        .data$method_ratio_trend %in% c("no_n_2010", "no_n_t") ~ NA_real_,
        .default = .data$n_ha / .data$n_ha_2010
      )
    ) |>
    dplyr::select(
      "bucket",
      "year",
      "n_scale",
      "method_ratio_trend",
      "method_ratio_n_2010"
    )
}

.ryr_n_intensity_proto <- function() {
  tibble::tibble(
    area_code = integer(),
    year = integer(),
    synthetic_n_t = double(),
    cropland_ha = double(),
    n_ha = double(),
    n_source = character()
  )
}

# D23: n_t of a bucket in year t. Its own FAOSTAT (or Smil back-cast) value
# where it reports one; otherwise the value of the polity that reported
# fertiliser for its territory that year, found by walking the `predecessor`
# edges of [polities] with resolve_polity_lineage() (the USSR's N per hectare
# of USSR cropland for Russia before 1992). Where several reporting buckets
# resolve to that one polity in a year their N and cropland are pooled.
.ryr_n_t <- function(bucket_years, intensity) {
  own <- intensity |>
    dplyr::transmute(
      bucket = .data$area_code,
      .data$year,
      n_ha = .data$n_ha,
      n_path = .data$n_source
    )
  rest <- bucket_years |>
    dplyr::filter(
      .data$year >= .ryr_first_synthetic_year(),
      !is.na(.data$bucket)
    ) |>
    dplyr::anti_join(own, by = c("bucket", "year"))
  reporters <- .ryr_reporter_polities(intensity)
  support <- .ryr_as_support(reporters$polity_code, reporters$year)
  inherited <- .ryr_lineage(rest, support) |>
    dplyr::inner_join(
      .ryr_polity_intensity(intensity, reporters),
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
      )
    )
  dplyr::bind_rows(own, inherited)
}

# D23: n_2010 of a bucket. Its own 2010 value; for a polity with none (the
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
  polity <- historical |>
    dplyr::rename(area_code = "bucket") |>
    .add_reporting_polity_columns() |>
    dplyr::transmute(
      bucket = .data$area_code,
      .data$year,
      polity_code = .data$reporting_polity_code
    ) |>
    dplyr::filter(!is.na(.data$polity_code))
  if (nrow(polity) == 0L) {
    return(proto)
  }
  candidates <- tidyr::expand_grid(
    bucket = unique(base$area_code),
    year = unique(polity$year)
  )
  support <- .ryr_as_support(polity$polity_code, polity$year)
  .ryr_lineage(candidates, support) |>
    dplyr::inner_join(
      dplyr::select(base, bucket = "area_code", "synthetic_n_t", "cropland_ha"),
      by = "bucket"
    ) |>
    dplyr::inner_join(
      dplyr::rename(polity, historical = "bucket"),
      by = c(lineage_polity_code = "polity_code", "year"),
      relationship = "many-to-many"
    ) |>
    dplyr::filter(.data$historical != .data$bucket) |>
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

# The polity each reporting bucket stands for in each year it reports N.
.ryr_reporter_polities <- function(intensity) {
  intensity |>
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
# stand for it.
.ryr_polity_intensity <- function(intensity, reporters) {
  intensity |>
    dplyr::inner_join(
      reporters,
      by = c(area_code = "bucket", "year"),
      relationship = "one-to-one"
    ) |>
    dplyr::summarise(
      n_ha = sum(.data$synthetic_n_t) / sum(.data$cropland_ha),
      n_source = dplyr::first(.data$n_source),
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
# unresolved pair is stamped `no_n_t` or `no_n_2010` instead.
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

# National synthetic N per hectare of cropland, by bucket and year.
.ryr_n_intensity <- function(bucket_years, data) {
  years <- sort(unique(c(bucket_years$year, .ryr_anchor_year())))
  years <- years[years >= .ryr_first_synthetic_year()]
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
      cropland_ha = .data$cropland_ha
    )
  .ryr_synthetic_n(fertilizer) |>
    dplyr::filter(.data$year %in% years) |>
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
      "n_source"
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
# years. One difference, deliberate: that script interpolates the 1961-1965
# global mean on a frame holding only those five years, where the only anchor
# is 1965, so its divisor is the 1965 value (19.0 Mt) rather than the mean of
# the interpolated years (15.6 Mt), and its back-cast steps down by that
# factor at 1960/1961. Here the mean is taken from the series interpolated
# over 1913-1965, so the back-cast meets FAOSTAT without the step.
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
  cells |>
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

# D22: the anomaly split in two. spatial = the cell's 1994-2023 ratio over the
# country's; temporal = the cell-year ratio over the cell's 1994-2023 ratio.
# Their product is the D15 anomaly (cell-year over country) wherever all
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
      method_ratio_spatial = dplyr::if_else(
        is.na(.data$cell_normal),
        "no_cell_normal",
        "lpjml"
      ),
      ratio_spatial = dplyr::if_else(
        .data$method_ratio_spatial == "lpjml",
        .data$cell_normal / .data$lpjml_normal,
        1
      ),
      method_ratio_temporal = dplyr::case_when(
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

# D22: each cell's own 1994-2023 ratio, pooled the same way as the country's.
.ryr_cell_normal <- function(window) {
  .ryr_pooled_ratio(window, c("lon", "lat", "lpjml_crop"), "cell_normal")
}

# LPJmL's irrigated:rainfed ratio per crop and country over the window (D15,
# D24): each regime's yield is the sum of yield x stand area over the sum of
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

# D22: the long-term component `ratio_level x ratio_spatial` is capped at 10;
# the temporal part multiplies the capped value and is not capped, so only a
# bad year can take R past 10. R is floored at 1.
.ryr_combine <- function(x) {
  cap <- .ryr_level_cap()
  x |>
    dplyr::mutate(
      ratio_anchor = pmax(1, .data$ratio_spam),
      ratio_level = 1 + (.data$ratio_anchor - 1) * .data$n_scale,
      long_raw = .data$ratio_level * .data$ratio_spatial,
      ratio_long_term = pmin(.data$long_raw, cap),
      ratio_anomaly = .data$ratio_spatial * .data$ratio_temporal,
      product = .data$ratio_long_term * .data$ratio_temporal,
      ratio = pmax(1, .data$product),
      method_regime_yield = .ryr_stamp(
        anchor_floor = .data$ratio_spam < 1,
        level_cap = .data$long_raw > cap,
        ratio_floor = .data$product < 1
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
      "ratio",
      "spam_crop_used",
      "method_ratio_anchor",
      "method_ratio_trend",
      "method_ratio_n_2010",
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

# D20: the 99th percentile of each item's national yields over 1961-2023, for
# the world or per WHEP region. A duplicated national value would weigh twice,
# so it is refused.
.ryr_yield_max <- function(production, bound) {
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
  yields <- long |>
    tidyr::pivot_wider(names_from = "unit", values_from = "value") |>
    ensure_columns(tibble::tibble(tonnes = double(), ha = double())) |>
    dplyr::filter(.data$tonnes > 0, .data$ha > 0) |>
    dplyr::mutate(yield = .data$tonnes / .data$ha)
  by <- "item_prod_code"
  if (identical(bound, "region")) {
    yields <- dplyr::left_join(
      yields,
      .ryr_region_map(),
      by = "area_code"
    ) |>
      dplyr::filter(!is.na(.data$region))
    by <- c(by, "region")
  }
  yields |>
    dplyr::summarise(
      yield_max = stats::quantile(
        .data$yield,
        .ryr_bound_prob(),
        names = FALSE
      ),
      .by = dplyr::all_of(by)
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

.ryr_attach_yield_max <- function(cells, yield_max, bound) {
  cells <- dplyr::mutate(cells, .ryr_item = as.integer(.data$item_prod_code))
  keys <- c(.ryr_item = "item_prod_code")
  if (identical(bound, "region")) {
    cells <- dplyr::left_join(
      cells,
      dplyr::rename(.ryr_region_map(), .ryr_region = "region"),
      by = "area_code",
      relationship = "many-to-one"
    )
    keys <- c(keys, .ryr_region = "region")
  }
  cells |>
    dplyr::left_join(yield_max, by = keys, relationship = "many-to-one") |>
    dplyr::mutate(method_yield_bound = bound)
}

# The conservation split, with the bound lowering R where it binds.
.ryr_split <- function(cells) {
  out <- cells |>
    dplyr::mutate(
      .ryr_p = .data$production_t,
      .ryr_ar = .data$rainfed_ha,
      .ryr_ai = .data$irrigated_ha,
      method_regime_split = dplyr::case_when(
        is.na(.data$ratio) ~ "no_ratio",
        .data$.ryr_ar + .data$.ryr_ai <= 0 ~ "no_area",
        .data$.ryr_ai <= 0 ~ "rainfed_only",
        .data$.ryr_ar <= 0 ~ "irrigated_only",
        .default = "yield_ratio"
      ),
      .ryr_bounded = .data$method_regime_split %in%
        c("yield_ratio", "irrigated_only"),
      .ryr_yi = .data$ratio *
        .data$.ryr_p /
        (.data$.ryr_ar + .data$ratio * .data$.ryr_ai),
      .ryr_clip = .data$.ryr_bounded &
        !is.na(.data$yield_max) &
        .data$.ryr_yi > .data$yield_max,
      # Y_i = R P / (A_r + R A_i) = Y_max solved for R. A clip means
      # R (P - Y_max A_i) > Y_max A_r >= 0, so the denominator is positive.
      .ryr_r_star = .data$yield_max *
        .data$.ryr_ar /
        (.data$.ryr_p - .data$yield_max * .data$.ryr_ai),
      ratio_split = dplyr::if_else(
        dplyr::coalesce(.data$.ryr_clip, FALSE),
        pmax(1, .data$.ryr_r_star),
        .data$ratio
      ),
      method_regime_bound = dplyr::case_when(
        !.data$.ryr_bounded ~ "not_applicable",
        is.na(.data$yield_max) ~ "no_bound",
        !.data$.ryr_clip ~ "not_binding",
        .data$.ryr_r_star < 1 ~ "clipped_at_one",
        .default = "clipped"
      ),
      .ryr_has = .data$method_regime_split %in%
        c("yield_ratio", "rainfed_only", "irrigated_only"),
      yield_rainfed = dplyr::if_else(
        .data$.ryr_has,
        .data$.ryr_p / (.data$.ryr_ar + .data$ratio_split * .data$.ryr_ai),
        NA_real_
      ),
      yield_irrigated = .data$ratio_split * .data$yield_rainfed
    )
  out |>
    dplyr::select(-dplyr::starts_with(".ryr_")) |>
    dplyr::relocate(
      "ratio_split",
      "yield_rainfed",
      "yield_irrigated",
      "yield_max",
      "method_regime_split",
      "method_regime_bound",
      "method_yield_bound",
      .after = dplyr::last_col()
    )
}
