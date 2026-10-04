# Per-land-use-class soil carbon-input assembly (Module B, Task B2c-4). Combines
# the two carbon-input builders into the single c_inputs layer
# build_carbon_balance() consumes, keyed (lon, lat, area_code, year, land_use):
#   - cropland: build_soil_carbon_inputs() emits per CROP within cropland, so it
#     is aggregated to the cropland class by area-weighting the per-hectare
#     carbon densities (weight = the crop's harvested area) and carbon-weighting
#     the humification fraction (weight = each crop's carbon mass = density x
#     area). This is the class-level counterpart of the crop-level input.
#     Under crop groups each crop is first split into a rainfed and an
#     irrigated part (.ci_split_into_groups()): residue and root carbon per
#     hectare re-evaluated at each part's yield from the regime yield ratio
#     the nitrogen chain uses, weed and manure carbon by area.
#   - grassland + natural: build_grass_natural_carbon_inputs() already emits at
#     the class grain, so its rows pass through unchanged.
# All densities are MgC/ha/yr.

#' Assemble the per-land-use-class soil carbon inputs.
#'
#' @description
#' Build the carbon-input layer [build_carbon_balance()] consumes, keyed by
#' `(lon, lat, area_code, year, land_use)`. The cropland class aggregates the
#' per-crop cropland inputs from [build_soil_carbon_inputs()] to the class
#' grain: the class carbon density is the harvested-area-weighted mean of the
#' per-crop densities, and the humification fraction is the carbon-mass-weighted
#' mean of the per-crop fractions (mass = density times area). The grassland and
#' natural classes come from [build_grass_natural_carbon_inputs()] unchanged.
#'
#' @param resolution `"grid"` (default, per cell and class) or `"polity"`
#'   (aggregated to `area_code`, area-weighting the cropland density by the
#'   polity crop area).
#' @param years Optional integer vector of calendar years to keep. `NULL`
#'   (default) keeps every year the inputs cover. Threaded into the default
#'   [build_soil_carbon_inputs()] and [build_grass_natural_carbon_inputs()]
#'   builders so their readers slice to the requested years; ignored for inputs
#'   supplied via `data`.
#' @param data Named list of pre-loaded inputs, each falling back to its builder
#'   when absent: `cropland` (the [build_soil_carbon_inputs()] output, per cell,
#'   crop and year, with `total_c_input_mgc_ha_yr` and `humified_fraction`);
#'   `crop_area` (per cell, crop and year harvested area with columns `lon`,
#'   `lat`, `area_code`, `item_prod_code`, `year`, `crop_area_ha`, used to
#'   area-weight the crop densities); `grass_natural` (the
#'   [build_grass_natural_carbon_inputs()] output at the class grain); and
#'   optional `land_use` (per-cell class `area_ha`, used to area-weight
#'   grassland/natural polity output); and `country_grid`, the polycell support
#'   resolved to one row per cell and `area_code`, from which `crop_area` is
#'   derived when absent. It is the same support [build_soil_carbon_inputs()]
#'   reads, so the weights and the carbon they weight can never come from two
#'   different crosswalks. When
#'   `cropland` or `grass_natural` are absent the respective builder is called
#'   with the remaining members of `data`. The yield split of
#'   `crop_groups$regime` reads three more, each built when absent:
#'   `regime_ratio` ([build_regime_yield_ratio()] output; built on the crop
#'   rows that carry a share and a national yield, so pass the table
#'   [build_nitrogen_balance()] used to make both chains read one ratio);
#'   `regime_production` ([get_primary_production()] output, for each crop's
#'   national yield and the yield bounds; read for `years` and for 1961-2023
#'   when absent); and `residue_humification` (defaults to
#'   [residue_humification]), whose residue and root coefficients re-weight
#'   each part's humified fraction.
#' @param crop_groups How cropland is resolved into land-use classes, a named
#'   list validated element-wise. `method`: `"rotation_groups"` (default)
#'   resolves cropland into crop GROUPS -- herbaceous crops pooled per
#'   irrigation regime (they rotate, so nothing inside the pool is a land-use
#'   change), woody crops per species, rainfed and irrigated separate --
#'   labelled by [soc_crop_group()]; `"none"` keeps the single `cropland`
#'   class the package used before, for comparison and for a caller that
#'   wants one cropland number. The former name of `"rotation_groups"` is
#'   still accepted as a deprecated alias: it resolves to
#'   `"rotation_groups"`, with identical results, and warns once per session.
#'   `irrigation`: where each crop's irrigated share of its cell area comes
#'   from. `"spatialized"` (default) uses [build_gridded_landuse()] on the
#'   pinned spatialization inputs, crop-specific and yearly; `"none"` puts
#'   every crop in its rainfed group. Recorded in `method_c_input`. A
#'   pre-built share layer can be supplied as `data$crop_regime_share` (`lon`,
#'   `lat`, `area_code`, `item_prod_code`, `year`, `irrigated_share`).
#'   `regime`: how a crop's carbon per hectare differs between its rainfed
#'   and irrigated parts. `"yield_split"` (default) re-evaluates each part's
#'   residue and root at the part's own yield. The parts' yields keep the
#'   crop's production, with the irrigated:rainfed yield ratio `R` of the
#'   cell-crop-year that [build_nitrogen_balance()] splits synthetic
#'   nitrogen with (the bounded `ratio` of [split_regime_yield()] on
#'   [build_regime_yield_ratio()]): with irrigated share `s` and mean yield
#'   `Y`, `Y_r = Y / (1 + s (R - 1))` and `Y_i = R Y_r`. Each part carries
#'   the crop's residue and root carbon per hectare times the response of
#'   [calculate_crop_residues()] and [calculate_crop_roots()] from the
#'   crop's mean yield to the part's yield, evaluated in the part's water
#'   regime, so the model's irrigation terms (a lower residue ratio for the
#'   crop groups that respond, a lower root:shoot ratio) apply to the
#'   irrigated part. Residue and root are affine in yield, so a high-yield
#'   hectare carries less than its yield share, and with the irrigation
#'   terms the crop's residue and root carbon fall on irrigated land rather
#'   than being kept. A crop wholly in one regime is re-evaluated at its
#'   mean yield in that regime. Weed and manure carbon keep the crop's
#'   density on both parts. `"area_split"` keeps the crop's densities on
#'   both parts, the rule before the yield split. Recorded per class in
#'   `method_regime`.
#' @param density_basis Which crop area weights the per-crop densities when
#'   they collapse to a class. `"renormalised"` (default) uses the yearly cell
#'   crop area the densities were computed on -- the FAOSTAT-renormalised area
#'   [build_soil_carbon_inputs()] returns as `crop_area_ha` -- so the class
#'   carbon mass equals the sum of the crop masses that were spatialized.
#'   `"static"` uses the time-invariant crop-pattern area split by the
#'   polycell's share of the cell, which the package used before. The two
#'   differ wherever the spatialized cell areas of a polity-crop-year do not
#'   sum to its FAOSTAT harvested area: measured on the 2010 pins over 40,065
#'   cropland cells the per-cell class density ratio renormalised/static has
#'   an area-weighted median of 0.988 (p5-p95 0.922-1.043). Recorded in
#'   `method_area_basis` on cropland rows.
#' @param method_grazing Whose grazing removes carbon from grassland and
#'   returns it as excreta; see [build_grass_natural_carbon_inputs()].
#'   `"whep"` (default) charges the class WHEP's own grass intake and applied
#'   excreta, and so needs `data$livestock_intake` and `data$excreta`;
#'   `"lpjml"` uses the model's livestock module instead and needs neither.
#' @param method_input_cn What a crop of unknown input C:N does to the C:N of
#'   the class it sits in. `"known_crops"` (default) forms the class ratio
#'   from the crops that have one, weighted by the carbon that came with them
#'   -- the rule [build_soil_carbon_inputs()] already applies one level down,
#'   where the input nitrogen totals only the components that carry a nitrogen
#'   and the carbon paired with them. `"require_all"` leaves the class ratio
#'   `NA` as soon as any one crop's is unknown, so the class takes the
#'   land-use default C:N instead of the ratio its other crops measured.
#'   Recorded in `method_input_cn` on cropland rows.
#' @inheritParams build_soil_carbon_inputs
#' @param example If `TRUE`, return a small fixture instead of reading remote
#'   data. Defaults to `FALSE`.
#' @return A tibble keyed by `(lon, lat, area_code, year, land_use)` at `"grid"`
#'   resolution (or `(area_code, year, land_use)` at `"polity"`), with
#'   `c_input_mgc_ha_yr`, `humified_fraction`, `method_c_input`, and
#'   `method_unspatialized`, `method_input_cn` and `method_crop_weights` (all
#'   `NA` on the grassland and natural classes, which are neither spatialized
#'   from polity-crop totals nor collapsed from crops), for `land_use` in
#'   `"cropland"`, `"grassland"` and `"natural"`, plus the polity columns
#'   below. Cropland groups also carry the regime split, `NA` on the single
#'   `"cropland"` class and on grassland and natural:
#'   - `method_regime`: the `crop_groups$regime` rule, `"yield_split"` or
#'     `"area_split"`.
#'   - `method_regime_split`: the split rules of the class's crop parts,
#'     sorted and joined by `"|"`: `"yield_ratio"` (part yields from the
#'     regime yield ratio), `"trivial_rainfed_only"` and
#'     `"trivial_irrigated_only"` (the crop has one regime in the cell),
#'     `"no_ratio"` (no ratio for the cell-crop-year),
#'     `"area_no_production"` (no national yield), `"area_no_components"`
#'     (the per-crop layer has no residue and root densities),
#'     `"area_split"` (asked for) and `"no_regime_share"` (no irrigated
#'     share; booked rainfed). The last five keep the crop's own densities
#'     on both parts.
#'   - `method_residue_regime`: how the parts' residue and root were set,
#'     joined the same way: `"part_yield"` (re-evaluated at a rainfed
#'     part's yield), `"part_yield_irrigation_terms"` (re-evaluated at an
#'     irrigated part's yield, with the model's irrigation terms) or
#'     `"crop_density"` (the crop's own density).
#'   - `yield_split_c_share`: the share of the class's carbon input from crop
#'     parts re-evaluated at the part yields of the regime yield ratio (rule
#'     `"yield_ratio"`).
#'   - `method_regime_cn`: how the parts' input C:N was formed, joined the
#'     same way: `"part_input_cn"` (from the part's own components' carbon
#'     and nitrogen) or `"crop_input_cn"` (the crop's, where the per-crop
#'     layer carries no component nitrogen).
#' @inheritSection whep_polity_columns Polity columns
#' @source Cropland inputs from [build_soil_carbon_inputs()]; grassland and
#'   natural inputs from [build_grass_natural_carbon_inputs()]; assembled per
#'   the WHEP historical carbon-balance design.
#' @export
#' @examples
#' build_carbon_inputs(example = TRUE)
build_carbon_inputs <- function(
  resolution = c("grid", "polity"),
  data = list(),
  years = NULL,
  crop_groups = list(),
  density_basis = c("renormalised", "static"),
  method_grazing = c("whep", "lpjml"),
  method_unspatialized = c("fodder_pattern", "reallocate", "drop"),
  method_input_cn = c("known_crops", "require_all"),
  method_crop_weights = c("spatialized", "static"),
  example = FALSE
) {
  resolution <- rlang::arg_match(resolution)
  density_basis <- rlang::arg_match(density_basis)
  method_grazing <- rlang::arg_match(method_grazing)
  method_unspatialized <- rlang::arg_match(method_unspatialized)
  method_input_cn <- rlang::arg_match(method_input_cn)
  method_crop_weights <- rlang::arg_match(method_crop_weights)
  cfg <- .ci_group_config(crop_groups)
  if (isTRUE(example)) {
    return(.example_carbon_inputs())
  }
  d <- .ci_resolve_inputs(
    data,
    years,
    cfg,
    density_basis,
    method_grazing,
    list(unspatialized = method_unspatialized, weights = method_crop_weights),
    method_input_cn
  )
  dplyr::bind_rows(d$cropland, d$grass_natural) |>
    .ci_finalise(resolution, data$land_use) |>
    .add_reporting_polity_columns()
}

# -- Input resolution ---------------------------------------------------------

.ci_resolve_inputs <- function(
  data,
  years = NULL,
  cfg = .ci_group_config(),
  density_basis = "renormalised",
  method_grazing = "whep",
  spatial = list(unspatialized = "fodder_pattern", weights = "spatialized"),
  method_input_cn = "known_crops"
) {
  # The static weights are only read when they are the basis; the
  # renormalised basis rides on the layer's own yearly area.
  crop_area <- if (density_basis == "static") {
    data$crop_area %||% .ci_crop_area(data)
  }
  list(
    cropland = .ci_cropland_input(
      data,
      years,
      crop_area,
      cfg,
      density_basis,
      spatial,
      method_input_cn
    ),
    crop_area = crop_area,
    grass_natural = data$grass_natural %||%
      build_grass_natural_carbon_inputs(
        data = data,
        years = years,
        method_grazing = method_grazing
      )
  )
}

# -- Cropland crop -> class aggregation ---------------------------------------

# Aggregate the per-crop cropland densities to the cropland class per cell-year.
# The class carbon density is the harvested-area-weighted mean of the per-crop
# densities; the humification fraction is the carbon-mass-weighted mean (mass =
# density x area), so a crop supplying more carbon dominates the class fraction.
# `class_area_ha` (the cell's total cropland area) is carried so the downstream
# polity aggregation can area-weight the per-hectare density and conserve mass.
# The cropland carbon-input class, collapsed to one row per cell-year.
#
# When we build the gridded inputs ourselves the collapse runs per year inside
# the gridding loop, so the pre-collapse table -- ~1.25e6 rows per simulated
# year, of which .ci_cropland_class() keeps about one in forty-two -- never
# accumulates across the span. A caller-supplied `cropland` arrives whole and is
# collapsed in one pass, as before (#624).
#
# The irrigated shares are built for the years each chunk ACTUALLY carries,
# not for the `years` argument: the production chain reads `years` as a
# range (`.build_years()` expands `c(2000, 2010)` to 2000:2010) while
# `build_gridded_landuse()` takes exact years, so shares built up front for
# the argument covered two of eleven years on a real run and booked the
# other nine wholly rainfed, with only the per-year gap count to show it.
.ci_cropland_input <- function(
  data,
  years,
  crop_area,
  cfg,
  basis = "renormalised",
  spatial = list(unspatialized = "fodder_pattern", weights = "spatialized"),
  method_input_cn = "known_crops"
) {
  # Only crop groups are split by regime; the single class needs no context.
  regime <- if (identical(cfg$method, "rotation_groups")) {
    .ci_regime_context(data, years, cfg)
  }
  collapse <- function(cropland) {
    shares <- .ci_regime_shares(data, unique(cropland$year), cfg)
    .ci_cropland_class(
      cropland,
      crop_area,
      shares,
      basis,
      method_input_cn,
      regime
    )
  }
  if (!is.null(data$cropland)) {
    return(collapse(data$cropland))
  }
  .sci_build(
    "grid",
    data,
    years,
    reduce = collapse,
    method = spatial$unspatialized,
    weights = spatial$weights
  )
}

.ci_cropland_class <- function(
  cropland,
  crop_area,
  shares = NULL,
  basis = "renormalised",
  method_input_cn = "known_crops",
  regime = NULL
) {
  joined <- .ci_weighted_cropland(cropland, crop_area, basis)
  if (is.null(shares)) {
    joined <- dplyr::mutate(
      joined,
      land_use = "cropland",
      method_c_input = "humified_weighted"
    )
  } else {
    joined <- .ci_split_into_groups(joined, shares, regime)
  }
  joined |>
    # Same reason as in `.sci_sum_components()`: an input table without an
    # input C:N is missing information, not carrying a zero.
    ensure_columns(tibble::tibble(input_cn = numeric())) |>
    # A hand-supplied per-crop layer need not carry the rule that produced
    # it; NA then says "not recorded", never "dropped". The single cropland
    # class is not split by regime, so its regime stamps are NA too.
    ensure_columns(tibble::tibble(
      method_unspatialized = character(),
      method_crop_weights = character(),
      method_regime = character(),
      method_regime_split = character(),
      method_residue_regime = character(),
      method_regime_cn = character()
    )) |>
    dplyr::mutate(
      c_mass = .data$total_c_input_mgc_ha_yr * .data$crop_area_ha
    ) |>
    dplyr::summarise(
      c_input_mgc_ha_yr = .ci_wmean(
        .data$total_c_input_mgc_ha_yr,
        .data$crop_area_ha
      ),
      humified_fraction = .ci_wmean(.data$humified_fraction, .data$c_mass),
      # Carbon-weighted, because a class's input C:N is the ratio of the
      # carbon and nitrogen it actually receives: weighting by area would let
      # a large, barely-cropped class outvote a small, heavily-amended one.
      # `.env$`, because the per-crop layer a caller supplies may itself
      # carry a `method_input_cn` column and the data mask would find that
      # before the argument.
      input_cn = .ci_class_cn(
        .data$input_cn,
        .data$c_mass,
        .env$method_input_cn
      ),
      class_area_ha = sum(.data$crop_area_ha),
      method_c_input = .data$method_c_input[1],
      method_area_basis = basis,
      method_input_cn = .env$method_input_cn,
      method_unspatialized = .data$method_unspatialized[1],
      method_crop_weights = .data$method_crop_weights[1],
      # The class holds parts of crops split under different rules, so the
      # collapse keeps both which rules occur (`method_regime_split`, the
      # rules of its crop parts, sorted and joined by "|", and likewise
      # `method_residue_regime` and `method_regime_cn`) and how much of the
      # class they cover (`yield_split_c_share`, the carbon-mass share of
      # the class whose residue and root densities were re-evaluated at the
      # part yields of the regime yield ratio). A class of crops none of
      # which could be yield-split reads 0 with the rules that stopped them,
      # never a silent equal split. The share is formed first: once
      # `method_regime_split` is collapsed, the per-part rules are no longer
      # in the data mask.
      yield_split_c_share = .ci_yield_split_share(
        .data$method_regime_split,
        .data$c_mass
      ),
      method_regime = .data$method_regime[1],
      method_regime_split = .nb_method_stamp(.data$method_regime_split),
      method_residue_regime = .nb_method_stamp(.data$method_residue_regime),
      method_regime_cn = .nb_method_stamp(.data$method_regime_cn),
      .by = c("lon", "lat", "area_code", "year", "land_use")
    )
}

# The carbon-mass share of a class whose crop parts were split by the regime
# yield ratio. NA on a class that is not split by regime at all.
.ci_yield_split_share <- function(rule, c_mass) {
  if (all(is.na(rule))) {
    return(NA_real_)
  }
  .ci_wmean(as.numeric(rule %in% "yield_ratio"), c_mass)
}

# The per-crop rows with the area that weights them. `"static"` joins the
# time-invariant crop-pattern weights (dropping any area the layer itself
# carries, so the join cannot suffix two `crop_area_ha` columns);
# `"renormalised"` keeps the layer's own yearly `crop_area_ha`, the basis
# its densities were computed on, and refuses a layer that lacks it.
.ci_weighted_cropland <- function(cropland, crop_area, basis) {
  if (basis == "renormalised") {
    if (!rlang::has_name(cropland, "crop_area_ha")) {
      cli::cli_abort(c(
        "{.arg density_basis} = {.val renormalised} needs {.field crop_area_ha}
         on the per-crop cropland layer.",
        i = "{.fn build_soil_carbon_inputs} returns it; a hand-built
             {.arg data$cropland} must carry it, or use
             {.val static}."
      ))
    }
    return(cropland)
  }
  join_keys <- c("lon", "lat", "area_code", "item_prod_code")
  if (rlang::has_name(crop_area, "year")) {
    join_keys <- c(join_keys, "year")
  }
  cropland |>
    dplyr::select(-dplyr::any_of("crop_area_ha")) |>
    dplyr::inner_join(crop_area, by = join_keys)
}

# Fan each crop row out into its rainfed and irrigated parts and label each
# with its crop group. The area splits by the crop's irrigated share s of
# its cell area. The carbon per hectare follows what each component grows
# with:
# - Residue and root carbon are re-evaluated at the part's yield. An
#   irrigated hectare of the crop yields R times a rainfed one, R the regime
#   yield ratio of the cell-crop-year: the bounded `ratio` split_regime_yield()
#   gives on build_regime_yield_ratio() (the SPAM2010 irrigated:rainfed
#   yield ratio carried over time by synthetic N and over cells and years by
#   LPJmL, issue #1233), through the item split the nitrogen chain splits
#   synthetic N with (`.ryr_item_split()`). The parts keep the crop's
#   production: Y_r = Y / (1 + s (R - 1)) and Y_i = R Y_r. Each part's
#   residue and root carbon per hectare is the crop's times the response of
#   the package's residue and root model from the crop's mean yield to the
#   part's yield and water regime (`.rr_part_factors()`, R/regime_residue.R),
#   so the model's irrigation terms apply to the irrigated part. The residue
#   carbon here is the soil-returned residue, and the destinies are
#   yield-independent fractions, so the part's soil share is the crop's.
# - Weed and manure carbon do not follow the harvest, so both parts keep the
#   crop's own density, as the nitrogen chain splits the matching N inputs
#   by area.
# Each part's total is the sum of its four components and its humified
# fraction the carbon-weighted mean over them. Its input C:N is formed from
# its own components' carbon and nitrogen where the layer carries the
# residue, root and input nitrogen, and is the crop's otherwise
# (`method_regime_cn`). How each crop row was split is stamped in
# `method_regime_split` (see `.ci_regime_items()`), how each part's residue
# and root were set in `method_residue_regime`, and the requested rule in
# `method_regime`. A crop with no share row is wholly rainfed; that is a
# data gap worth seeing, so the count is reported rather than absorbed.
.ci_split_into_groups <- function(joined, shares, regime = NULL) {
  items <- .ci_regime_items(joined, shares, regime)
  dplyr::bind_rows(
    .ci_regime_part(items, FALSE, regime$humification),
    .ci_regime_part(items, TRUE, regime$humification)
  ) |>
    dplyr::filter(.data$crop_area_ha > 0) |>
    dplyr::mutate(
      land_use = soc_crop_group(
        as.integer(.data$item_prod_code),
        .data$irrigated
      ),
      method_c_input = "humified_weighted_rotation_groups",
      method_regime = regime$method %||% "area_split"
    ) |>
    dplyr::select(-dplyr::any_of(.ci_regime_item_cols()))
}

# The per-crop working columns of the split, dropped once the parts exist.
.ci_regime_item_cols <- function() {
  c(
    "irrigated",
    "irrigated_share",
    "has_share",
    "regime_ratio",
    "yield_mean",
    "yield_rainfed",
    "yield_irrigated",
    paste0(
      c("residue_factor_", "root_factor_", "method_residue_regime_"),
      rep(c("rainfed", "irrigated"), each = 3)
    )
  )
}

# Each crop row with its irrigated share, the regime yield ratio, the part
# yields and the factors its two parts scale residue and root carbon by.
# The rule that set them is `method_regime_split`, in the nitrogen chain's
# vocabulary:
# - "yield_ratio": both regimes, each part re-evaluated at its yield from
#   the bounded ratio;
# - "trivial_rainfed_only", "trivial_irrigated_only": one regime only, so
#   one part, re-evaluated at the crop's mean yield in that regime (the
#   irrigation terms apply to a wholly irrigated crop);
# - "no_ratio": both regimes but no ratio for the cell-crop-year;
# - "area_no_production": no national yield (no production, or none on a
#   positive area) to set the yield bounds against;
# - "area_no_components": no residue and root densities to scale (a
#   hand-supplied layer);
# - "area_split": the area split was asked for (`crop_groups$regime`);
# - "no_regime_share": no irrigated share, so wholly rainfed.
# The last five keep the crop's own densities on both parts: with no part
# yield there is nothing to re-evaluate the model at.
.ci_regime_items <- function(joined, shares, regime = NULL) {
  .check_columns(
    shares,
    c("lon", "lat", "area_code", "item_prod_code", "year", "irrigated_share"),
    "data$crop_regime_share"
  )
  shares <- shares |>
    dplyr::mutate(
      lon = round(.data$lon, 2),
      lat = round(.data$lat, 2),
      item_prod_code = as.character(.data$item_prod_code)
    ) |>
    dplyr::select(
      "lon",
      "lat",
      "area_code",
      "item_prod_code",
      "year",
      "irrigated_share"
    )
  with_share <- joined |>
    dplyr::mutate(
      lon = round(.data$lon, 2),
      lat = round(.data$lat, 2),
      item_prod_code = as.character(.data$item_prod_code)
    ) |>
    dplyr::left_join(
      shares,
      by = c("lon", "lat", "area_code", "item_prod_code", "year")
    )
  n_gap <- sum(is.na(with_share$irrigated_share))
  if (n_gap > 0L) {
    cli::cli_inform(c(
      i = "{n_gap} cell-crop-year{?s} carr{?ies/y} no irrigated share and
           {?is/are} booked as rainfed."
    ))
  }
  with_share |>
    dplyr::mutate(
      has_share = !is.na(.data$irrigated_share),
      irrigated_share = pmin(
        pmax(dplyr::coalesce(.data$irrigated_share, 0), 0),
        1
      )
    ) |>
    .ci_regime_factors(regime)
}

# The rule, ratio, mean yield and part factors of every crop row.
.ci_regime_factors <- function(rows, regime) {
  rows$method_regime_split <- .ci_regime_gate(rows, regime)
  rows$regime_ratio <- NA_real_
  rows$yield_mean <- NA_real_
  todo <- is.na(rows$method_regime_split)
  if (any(todo)) {
    bounded <- .ci_bounded_ratio(rows[todo, ], regime)
    rows$regime_ratio[todo] <- bounded$ratio
    rows$yield_mean[todo] <- bounded$yield_mean
    rows$method_regime_split[todo] <- bounded$method_regime_split
  }
  .ci_inform_unsplit(rows)
  .ci_part_factors(rows)
}

# The part yields and the residue and root factors of both parts of every
# crop row. A part is re-evaluated where its yield is known (the rules of
# .rr_reevaluated_rules()) and it has area; every other part keeps factor 1,
# the crop's own densities.
.ci_part_factors <- function(rows) {
  reevaluate <- rows$method_regime_split %in% .rr_reevaluated_rules()
  yields <- .rr_part_yields(
    rows$yield_mean,
    rows$irrigated_share,
    rows$regime_ratio
  )
  rows$yield_rainfed <- dplyr::if_else(reevaluate, yields$rainfed, NA_real_)
  rows$yield_irrigated <- dplyr::if_else(
    reevaluate,
    yields$irrigated,
    NA_real_
  )
  rainfed <- .ci_evaluate_part(
    rows,
    reevaluate & rows$irrigated_share < 1,
    "rainfed"
  )
  irrigated <- .ci_evaluate_part(
    rows,
    reevaluate & rows$irrigated_share > 0,
    "irrigated"
  )
  dplyr::mutate(
    rows,
    residue_factor_rainfed = rainfed$residue_factor,
    root_factor_rainfed = rainfed$root_factor,
    residue_factor_irrigated = irrigated$residue_factor,
    root_factor_irrigated = irrigated$root_factor,
    method_residue_regime_rainfed = .rr_residue_stamp(reevaluate, FALSE),
    method_residue_regime_irrigated = .rr_residue_stamp(reevaluate, TRUE)
  )
}

# One regime's residue and root factors for every crop row: the model's
# response at the part's yield where `evaluate`, 1 elsewhere.
.ci_evaluate_part <- function(rows, evaluate, regime) {
  out <- list(
    residue_factor = rep(1, nrow(rows)),
    root_factor = rep(1, nrow(rows))
  )
  if (!any(evaluate)) {
    return(out)
  }
  factors <- .rr_part_factors(tibble::tibble(
    item_prod_code = rows$item_prod_code[evaluate],
    area_code = rows$area_code[evaluate],
    year = rows$year[evaluate],
    yield_mean = rows$yield_mean[evaluate],
    yield_part = rows[[paste0("yield_", regime)]][evaluate],
    water_regime = regime
  ))
  out$residue_factor[evaluate] <- factors$residue_factor
  out$root_factor[evaluate] <- factors$root_factor
  out
}

# The rows that cannot reach the yield split, with the reason; NA marks the
# rows that go on to it.
.ci_regime_gate <- function(rows, regime) {
  scalable <- if (
    rlang::has_name(rows, "residue_c_mgc_ha_yr") &&
      rlang::has_name(rows, "root_c_mgc_ha_yr")
  ) {
    !is.na(rows$residue_c_mgc_ha_yr) & !is.na(rows$root_c_mgc_ha_yr)
  } else {
    FALSE
  }
  area_split <- is.null(regime) || identical(regime$method, "area_split")
  dplyr::case_when(
    !rows$has_share ~ "no_regime_share",
    area_split ~ "area_split",
    !scalable ~ "area_no_components",
    .default = NA_character_
  )
}

# The bounded regime yield ratio of each crop row, its mean yield and the
# rule that gave them, from the item split the nitrogen chain shares. The
# production is the crop's national yield on its cell area -- the yield of
# the production its carbon densities were computed from; the ratio depends
# on it only through that mean yield and the irrigated share. A row with no
# national yield is "area_no_production" and never reaches the split.
.ci_bounded_ratio <- function(rows, regime) {
  key <- c("lon", "lat", "area_code", "item_prod_code", "year")
  production <- regime$production %||%
    get_primary_production(years = regime$years)
  cells <- rows |>
    dplyr::transmute(
      lon = round(as.numeric(.data$lon), 2),
      lat = round(as.numeric(.data$lat), 2),
      area_code = as.integer(.data$area_code),
      item_prod_code = as.integer(.data$item_prod_code),
      year = as.integer(.data$year),
      crop_area_ha = .data$crop_area_ha,
      irrigated_share = .data$irrigated_share
    ) |>
    dplyr::left_join(
      .ci_national_yield(production, unique(rows$year)),
      by = c("area_code", "item_prod_code", "year"),
      relationship = "many-to-one"
    )
  has_yield <- !is.na(cells$yield_t_ha) & is.finite(cells$crop_area_ha)
  out <- list(
    ratio = rep(NA_real_, nrow(cells)),
    yield_mean = dplyr::if_else(has_yield, cells$yield_t_ha, NA_real_),
    method_regime_split = rep("area_no_production", nrow(cells))
  )
  if (!any(has_yield)) {
    return(out)
  }
  # The regime areas are formed as the nitrogen chain forms them
  # (`.nb_rescale_regime_area()`: irrigated = area x share, rainfed = the
  # rest), so the two chains hand the split the same numbers.
  split <- cells[has_yield, ] |>
    dplyr::mutate(
      irrigated_ha = .data$crop_area_ha * .data$irrigated_share,
      rainfed_ha = .data$crop_area_ha - .data$irrigated_ha
    ) |>
    dplyr::summarise(
      production_t = sum(.data$yield_t_ha * .data$crop_area_ha),
      irrigated_ha = sum(.data$irrigated_ha),
      rainfed_ha = sum(.data$rainfed_ha),
      .by = dplyr::all_of(key)
    ) |>
    .ryr_item_split(regime$ratio, regime$production) |>
    dplyr::select(dplyr::all_of(key), "ratio", "method_regime_split")
  back <- dplyr::left_join(
    cells[has_yield, key],
    split,
    by = key,
    relationship = "many-to-one"
  )
  out$ratio[has_yield] <- back$ratio
  out$method_regime_split[has_yield] <- back$method_regime_split
  out
}

# National yield (t/ha) per polity, crop and year: production over harvested
# area, each summed per unit as .sci_crop_prod_wide() sums them before the
# crop-NPP chain divides a crop's carbon by that area. Only a positive
# production on a positive area gives a yield.
.ci_national_yield <- function(production, years) {
  .check_columns(
    production,
    c("year", "area_code", "item_prod_code", "unit", "value"),
    "data$regime_production"
  )
  production |>
    dplyr::filter(.data$unit %in% c("tonnes", "ha"), .data$year %in% years) |>
    dplyr::summarise(
      value = sum(.data$value, na.rm = TRUE),
      .by = c("area_code", "item_prod_code", "year", "unit")
    ) |>
    tidyr::pivot_wider(names_from = "unit", values_from = "value") |>
    ensure_columns(tibble::tibble(tonnes = double(), ha = double())) |>
    dplyr::filter(.data$tonnes > 0, .data$ha > 0) |>
    dplyr::transmute(
      area_code = as.integer(.data$area_code),
      item_prod_code = as.integer(.data$item_prod_code),
      year = as.integer(.data$year),
      yield_t_ha = .data$tonnes / .data$ha
    )
}

# A crop row with both regimes whose densities could not be split by yield
# is a gap worth seeing, like a crop with no share row: reported, by rule.
.ci_inform_unsplit <- function(rows) {
  gap <- rows$irrigated_share > 0 &
    rows$irrigated_share < 1 &
    rows$crop_area_ha > 0 &
    rows$method_regime_split %in%
      c("no_ratio", "area_no_production", "area_no_components")
  gap <- gap %in% TRUE
  n_gap <- sum(gap)
  if (n_gap == 0L) {
    return(invisible(NULL))
  }
  counts <- table(rows$method_regime_split[gap])
  detail <- paste0(names(counts), ": ", as.integer(counts), collapse = ", ")
  cli::cli_inform(c(
    i = "{n_gap} cell-crop-year{?s} with both regimes kept the crop's carbon
         densities on both parts ({detail})."
  ))
}

# One regime's part of every crop row: its share of the crop's area, and its
# residue and root carbon per hectare scaled by its two factors.
.ci_regime_part <- function(items, irrigated, humification) {
  regime <- if (irrigated) "irrigated" else "rainfed"
  share <- if (irrigated) items$irrigated_share else 1 - items$irrigated_share
  items |>
    dplyr::mutate(
      irrigated = irrigated,
      crop_area_ha = .data$crop_area_ha * share,
      method_residue_regime = items[[paste0("method_residue_regime_", regime)]]
    ) |>
    .ci_scale_part(
      items[[paste0("residue_factor_", regime)]],
      items[[paste0("root_factor_", regime)]],
      humification
    )
}

# Residue and root carbon per hectare times the part's residue and root
# factors; weed and manure carbon unchanged. The total moves by the residue
# and root carbon added or removed, so it stays the sum of the four
# components, and so does the humified carbon, at the residue and root
# coefficients .sci_humified_fraction() weighted them with: the part's
# humified fraction is then the carbon-weighted mean over its own mix. The
# input C:N is re-formed first, from the unscaled carbon. Factors of exactly
# 1 leave the row untouched.
.ci_scale_part <- function(x, residue_factor, root_factor, h) {
  x <- .ci_scale_part_cn(x, residue_factor, root_factor)
  scaled <- (residue_factor != 1 | root_factor != 1) %in% TRUE
  if (!any(scaled)) {
    return(x)
  }
  residue <- x$residue_c_mgc_ha_yr
  root <- x$root_c_mgc_ha_yr
  total <- x$total_c_input_mgc_ha_yr
  d_residue <- (residue_factor - 1) * residue
  d_root <- (root_factor - 1) * root
  new_total <- total + d_residue + d_root
  humified <- x$humified_fraction *
    total +
    d_residue * h$crop_residue +
    d_root * h$root
  dplyr::mutate(
    x,
    residue_c_mgc_ha_yr = dplyr::if_else(
      scaled,
      residue * residue_factor,
      residue
    ),
    root_c_mgc_ha_yr = dplyr::if_else(scaled, root * root_factor, root),
    total_c_input_mgc_ha_yr = dplyr::if_else(scaled, new_total, total),
    humified_fraction = dplyr::if_else(
      scaled & new_total > 0,
      humified / new_total,
      .data$humified_fraction
    )
  )
}

# The part's input C:N, from its own components' carbon and nitrogen. The
# per-crop layer carries the nitrogen per hectare of the components whose
# nitrogen is known (`input_n_mgn_ha_yr`; the carbon those carry is
# `input_cn` times it) and the residue and root nitrogen. Residue and root
# carbon and nitrogen are fixed fractions of the same dry matter, so a part
# moves both by the same factor, and the part's ratio moves by the residue
# and root deltas of whichever of the two carries a known nitrogen. A layer
# without these columns (hand-supplied) leaves each part its crop's ratio,
# stamped "crop_input_cn".
.ci_scale_part_cn <- function(x, residue_factor, root_factor) {
  n_cols <- c(
    "residue_n_mgn_ha_yr",
    "root_n_mgn_ha_yr",
    "input_n_mgn_ha_yr",
    "input_cn"
  )
  if (!all(rlang::has_name(x, n_cols))) {
    return(dplyr::mutate(x, method_regime_cn = "crop_input_cn"))
  }
  scaled <- (residue_factor != 1 | root_factor != 1) %in% TRUE
  residue_n <- x$residue_n_mgn_ha_yr
  root_n <- x$root_n_mgn_ha_yr
  d_c <- dplyr::if_else(
    is.na(residue_n),
    0,
    (residue_factor - 1) * x$residue_c_mgc_ha_yr
  ) +
    dplyr::if_else(is.na(root_n), 0, (root_factor - 1) * x$root_c_mgc_ha_yr)
  d_n <- dplyr::coalesce((residue_factor - 1) * residue_n, 0) +
    dplyr::coalesce((root_factor - 1) * root_n, 0)
  n <- x$input_n_mgn_ha_yr + d_n
  c_with_n <- x$input_cn * x$input_n_mgn_ha_yr + d_c
  dplyr::mutate(
    x,
    input_cn = dplyr::if_else(
      scaled,
      dplyr::if_else(n > 0, c_with_n / n, NA_real_),
      .data$input_cn
    ),
    input_n_mgn_ha_yr = dplyr::if_else(scaled, n, .data$input_n_mgn_ha_yr),
    residue_n_mgn_ha_yr = dplyr::if_else(
      scaled,
      residue_n * residue_factor,
      residue_n
    ),
    root_n_mgn_ha_yr = dplyr::if_else(scaled, root_n * root_factor, root_n),
    method_regime_cn = "part_input_cn"
  )
}

# -- Crop groups: configuration and the irrigated-share layer -----------------

# Validate the crop_groups configuration element-wise. Absent elements take
# their default; unknown names or values abort, so a typo cannot silently
# select the single-class path.
.ci_group_config <- function(crop_groups = list()) {
  known <- c("method", "irrigation", "regime")
  extra <- setdiff(names(crop_groups), known)
  if (length(extra) > 0L) {
    cli::cli_abort(
      "Unknown {.arg crop_groups} element{?s}: {.val {extra}}."
    )
  }
  list(
    method = rlang::arg_match0(
      .ci_group_method_alias(crop_groups$method %||% "rotation_groups"),
      c("none", "rotation_groups"),
      arg_nm = "crop_groups$method"
    ),
    irrigation = rlang::arg_match0(
      crop_groups$irrigation %||% "spatialized",
      c("spatialized", "none"),
      arg_nm = "crop_groups$irrigation"
    ),
    regime = rlang::arg_match0(
      crop_groups$regime %||% "yield_split",
      c("yield_split", "area_split"),
      arg_nm = "crop_groups$regime"
    )
  )
}

# `"spain_hist"` is the former name of `"rotation_groups"`. It still resolves,
# so existing calls keep working and give identical results, but it warns once
# per session and every output records the new name.
.ci_group_method_alias <- function(method) {
  if (!identical(method, "spain_hist")) {
    return(method)
  }
  cli::cli_warn(
    c(
      "{.code crop_groups = list(method = \"spain_hist\")} is deprecated.",
      i = "Use {.code crop_groups = list(method = \"rotation_groups\")}; the
           results are identical and the output records
           {.val rotation_groups}."
    ),
    class = c(
      "whep_crop_groups_method_deprecated",
      "lifecycle_warning_deprecated"
    ),
    .frequency = "once",
    .frequency_id = "whep_crop_groups_method_deprecated"
  )
  "rotation_groups"
}

# The irrigated share of each crop's cell area, per year. NULL means the
# single-class path (or, under `irrigation = "none"`, every crop rainfed, which
# the split treats as a share of zero everywhere).
.ci_regime_shares <- function(data, years, cfg) {
  if (!identical(cfg$method, "rotation_groups")) {
    return(NULL)
  }
  if (!is.null(data$crop_regime_share)) {
    return(.ci_shares_for_years(data$crop_regime_share, years))
  }
  if (identical(cfg$irrigation, "none")) {
    return(tibble::tibble(
      lon = numeric(),
      lat = numeric(),
      area_code = integer(),
      item_prod_code = character(),
      year = integer(),
      irrigated_share = numeric()
    ))
  }
  .ci_spatialized_regime_share(
    years,
    data$country_grid %||% .sci_read_country_grid()
  )
}

# What the regime split needs besides the shares, resolved once per build:
# the rule (`crop_groups$regime`), the ratio table and production a caller
# may supply (`data$regime_ratio`, `data$regime_production`, as in
# build_nitrogen_balance()), the years the national yields are read for
# when they are not -- the `years` the crop-NPP chain read its production
# for, so the read is the same one -- and the residue and root humification
# coefficients the crop's humified fraction was built with.
.ci_regime_context <- function(data, years, cfg) {
  list(
    method = cfg$regime,
    ratio = data$regime_ratio,
    production = data$regime_production,
    years = years,
    humification = .sci_humification_lookup(
      data$residue_humification %||% whep::residue_humification
    )
  )
}

# Trim an injected share layer to the years being collapsed. A layer without
# a `year` column is passed through so the column check downstream names
# what is missing, rather than failing here on the filter.
.ci_shares_for_years <- function(shares, years) {
  if (is.null(years) || !rlang::has_name(shares, "year")) {
    return(shares)
  }
  dplyr::filter(shares, .data$year %in% years)
}

# Crop-specific, yearly irrigated shares from the spatialization chain on its
# pinned inputs: build_gridded_landuse() allocates each crop's national
# irrigated area over cells, so the share is rainfed_ha / irrigated_ha per
# cell x crop x year -- the only source WHEP has that is both crop-specific
# and dynamic (LPJmL's cftfrac folds woody crops into one PFT and cannot
# recover species; MIRCA is a static 2000 snapshot).
#
# The cell-polity support is the carbon path's own (`.sci_read_country_grid()`,
# or the `country_grid` the caller injected), not the spatialize chain's
# `country_grid.parquet`: that file is the centroid crosswalk with no polity
# share, which `build_gridded_landuse()` refuses (S-A5), and using a second
# crosswalk here would key the regime split on different polycells than the
# carbon it splits. The engine call is `.sci_engine_crops()`, the one the
# `"spatialized"` crop weights of build_soil_carbon_inputs() make, so the crop
# geography and its irrigated split come from the same allocation (whep#1002).
.ci_spatialized_regime_share <- function(years, country_grid) {
  .sci_engine_crops(years, country_grid) |>
    dplyr::mutate(
      total = .data$rainfed_ha + .data$irrigated_ha,
      irrigated_share = dplyr::if_else(
        .data$total > 0,
        .data$irrigated_ha / .data$total,
        0
      )
    ) |>
    dplyr::select(
      "lon",
      "lat",
      "area_code",
      "item_prod_code",
      "year",
      "irrigated_share"
    )
}

# The static per-cell crop area that weights each crop's density in its class.
#
# This used to be an inner_join, which threw away every cell-crop the static
# crop-pattern layer has no row for -- re-imposing at the class step the loss
# build_soil_carbon_inputs() had just been taught to avoid, because a crop
# reallocated onto a polity's cropland cells is by definition absent from the
# pattern (whep#599). The layer now carries the area its own densities were
# computed on, so those rows keep theirs. Matched rows still take the static
# area, so nothing that was already gridded moves; whether the static or the
# FAOSTAT-renormalised area should weight the class is a separate question, open
# in whep#1058 as `density_basis`.
.ci_attach_crop_area <- function(cropland, crop_area, join_keys) {
  if (!rlang::has_name(cropland, "crop_area_ha")) {
    return(dplyr::inner_join(cropland, crop_area, by = join_keys))
  }
  cropland |>
    dplyr::rename(layer_crop_area_ha = "crop_area_ha") |>
    dplyr::left_join(crop_area, by = join_keys) |>
    dplyr::mutate(
      crop_area_ha = dplyr::coalesce(
        .data$crop_area_ha,
        .data$layer_crop_area_ha
      )
    ) |>
    dplyr::select(-"layer_crop_area_ha")
}

# -- Finalisation -------------------------------------------------------------

# Grid output keeps per-cell per-class rows; polity output aggregates to
# (area_code, year, land_use). The per-hectare density is area-weighted by the
# cell's class area (`class_area_ha`) so the polity density times the polity
# class area equals the summed grid-level carbon mass; the humification fraction
# is carbon-mass-weighted (mass = density x class area) so humified carbon is
# likewise conserved. Grassland/natural class areas are attached from
# `land_use` when supplied; a class with no available area retains the plain
# mean fallback via .ci_wmean's zero-weight guard.
.ci_finalise <- function(x, resolution, land_use = NULL) {
  # Grassland and natural rows are not spatialized from polity-crop totals and
  # are not collapsed from crops, so they carry neither the allocation rule nor
  # the class-C:N rule nor a regime split; the columns still have to exist
  # for them.
  x <- ensure_columns(
    x,
    tibble::tibble(
      method_unspatialized = character(),
      method_input_cn = character(),
      method_crop_weights = character(),
      method_regime = character(),
      method_regime_split = character(),
      yield_split_c_share = numeric(),
      method_residue_regime = character(),
      method_regime_cn = character()
    )
  )
  drop_cols <- c("class_area_ha")
  if (resolution == "grid") {
    # Grouped cropland keeps its area as `group_area_ha`: the balance splits
    # the cell's LUH2 cropland over the groups in that proportion, since
    # LUH2 knows cropland but not which crops are on it.
    grouped <- .soc_is_cropland(x$land_use) & x$land_use != "cropland"
    if (any(grouped)) {
      x <- dplyr::mutate(
        x,
        group_area_ha = dplyr::if_else(grouped, .data$class_area_ha, NA_real_)
      )
    }
    return(tibble::as_tibble(dplyr::select(x, -dplyr::any_of(drop_cols))))
  }
  if (!is.null(land_use)) {
    x <- .ci_attach_land_use_area(x, land_use)
  }
  x |>
    dplyr::mutate(
      area_weight = dplyr::coalesce(.data$class_area_ha, 0),
      c_mass = .data$c_input_mgc_ha_yr * .data$area_weight
    ) |>
    dplyr::summarise(
      c_input_mgc_ha_yr = .ci_wmean(.data$c_input_mgc_ha_yr, .data$area_weight),
      humified_fraction = .ci_wmean(.data$humified_fraction, .data$c_mass),
      # The yield-split share is a share of carbon, so it is carbon-weighted
      # like the humified fraction; the rules are the union of the cells'.
      yield_split_c_share = .ci_wmean(
        .data$yield_split_c_share,
        .data$c_mass
      ),
      method_regime_split = .ci_rule_union(.data$method_regime_split),
      method_residue_regime = .ci_rule_union(.data$method_residue_regime),
      method_regime_cn = .ci_rule_union(.data$method_regime_cn),
      dplyr::across(
        dplyr::any_of(c(
          "method_c_input",
          "method_area_basis",
          "method_input_cn",
          "method_unspatialized",
          "method_crop_weights",
          "method_regime"
        )),
        \(x) x[1]
      ),
      .by = c("area_code", "year", "land_use")
    ) |>
    tibble::as_tibble()
}

# The distinct rules of a set of "|"-joined rule stamps, joined the same way.
.ci_rule_union <- function(stamps) {
  .nb_method_stamp(unlist(stringr::str_split(stamps, stringr::fixed("|"))))
}

# Supply class-area weights for grassland/natural rows from the same LUH2 layer
# consumed by the carbon balance. Cropland retains its harvested-area weight;
# unmatched classes retain the existing zero-weight/plain-mean fallback.
.ci_attach_land_use_area <- function(x, land_use) {
  areas <- land_use |>
    dplyr::mutate(
      lon = round(.data$lon, 2),
      lat = round(.data$lat, 2),
      land_use = stringr::str_to_lower(.data$land_use)
    ) |>
    dplyr::summarise(
      land_area_ha = sum(.data$area_ha),
      .by = c("lon", "lat", "area_code", "year", "land_use")
    )
  x |>
    dplyr::left_join(
      areas,
      by = c("lon", "lat", "area_code", "year", "land_use")
    ) |>
    dplyr::mutate(
      class_area_ha = dplyr::coalesce(
        .data$class_area_ha,
        .data$land_area_ha
      )
    ) |>
    dplyr::select(-"land_area_ha")
}

.ci_wmean <- function(value, weight) {
  if (sum(weight) == 0) {
    return(mean(value))
  }
  sum(value * weight) / sum(weight)
}

# The class input C:N, from the crops that have one.
#
# `.ci_wmean()` takes no `na.rm`, so a single crop of unknown C:N used to make
# the WHOLE class's ratio NA, and the class then took the land-use default in
# `.soc_marginal_cn()` instead of the ratio its other crops measured (#1123).
#
# `"known_crops"` forms the ratio from the crops whose own C:N is known,
# weighted by the carbon that came with them -- the same rule
# `.sci_sum_components()` applies one level down, where the nitrogen total
# counts only the components that carry a nitrogen and the carbon paired with
# them, "so the ratio is formed from a matched pair rather than dividing all
# the carbon by some of the nitrogen". `"require_all"` keeps the stricter
# reading -- a class containing a component of unknown C:N has an unknown C:N
# -- and is what the package did before. A class where NO crop has a ratio is
# unknown under both.
.ci_class_cn <- function(value, weight, method = "known_crops") {
  if (identical(method, "require_all")) {
    return(.ci_wmean(value, weight))
  }
  keep <- !is.na(value) & !is.na(weight)
  if (!any(keep)) {
    return(NA_real_)
  }
  .ci_wmean(value[keep], weight[keep])
}

# -- Crop-area reader ---------------------------------------------------------

# Per cell-crop harvested area (ha), split between the cell's polycells by their
# share of the cell's land, from the same static support + crop_patterns
# build_soil_carbon_inputs uses (crop_patterns is time-invariant, so no year
# key). Only reached when data$crop_area is absent; country_grid and
# crop_patterns fall back to the same default readers build_soil_carbon_inputs
# uses -- `.sci_read_country_grid()` being the carbon path's shared polycell
# support -- so these weights and the cropland carbon they weight can never be
# built on two different crosswalks.
.ci_crop_area <- function(data) {
  country_grid <- data$country_grid %||% .sci_read_country_grid()
  crop_patterns <- data$crop_patterns %||% .sci_read_crop_patterns()
  cg <- .normalize_carbon_support(country_grid) |>
    dplyr::mutate(lon = round(.data$lon, 2), lat = round(.data$lat, 2))
  crop_patterns |>
    dplyr::mutate(
      lon = round(.data$lon, 2),
      lat = round(.data$lat, 2),
      item_prod_code = as.character(.data$item_prod_code)
    ) |>
    dplyr::inner_join(cg, by = c("lon", "lat")) |>
    dplyr::transmute(
      lon = .data$lon,
      lat = .data$lat,
      area_code = .data$area_code,
      item_prod_code = .data$item_prod_code,
      crop_area_ha = .data$crop_area_ha * .data$cell_area_frac
    )
}
