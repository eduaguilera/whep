# Per-land-use-class soil carbon-input assembly (Module B, Task B2c-4). Combines
# the two carbon-input builders into the single c_inputs layer
# build_carbon_balance() consumes, keyed (lon, lat, area_code, year, land_use):
#   - cropland: build_soil_carbon_inputs() emits per CROP within cropland, so it
#     is aggregated to the cropland class by area-weighting the per-hectare
#     carbon densities (weight = the crop's harvested area) and carbon-weighting
#     the humification fraction (weight = each crop's carbon mass = density x
#     area). This is the class-level counterpart of the crop-level input.
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
#'   with the remaining members of `data`.
#' @param crop_groups How cropland is resolved into land-use classes, a named
#'   list validated element-wise. `method`: `"none"` (default) keeps one
#'   `cropland` class, every number unchanged; `"spain_hist"` marches crop
#'   GROUPS as classes -- herbaceous crops pooled per irrigation regime (they
#'   rotate, so nothing inside the pool is a land-use change), woody crops per
#'   species, rainfed and irrigated separate -- labelled by [soc_crop_group()].
#'   `irrigation`: where each crop's irrigated share of its cell area comes
#'   from. `"spatialized"` (default) uses [build_gridded_landuse()] on the
#'   pinned spatialization inputs, crop-specific and yearly; `"none"` puts
#'   every crop in its rainfed group. Recorded in `method_c_input`. A
#'   pre-built share layer can be supplied as `data$crop_regime_share` (`lon`,
#'   `lat`, `area_code`, `item_prod_code`, `year`, `irrigated_share`).
#' @param example If `TRUE`, return a small fixture instead of reading remote
#'   data. Defaults to `FALSE`.
#' @return A tibble keyed by `(lon, lat, area_code, year, land_use)` at `"grid"`
#'   resolution (or `(area_code, year, land_use)` at `"polity"`), with
#'   `c_input_mgc_ha_yr`, `humified_fraction` and `method_c_input`, for
#'   `land_use` in `"cropland"`, `"grassland"` and `"natural"`, plus the polity
#'   columns below.
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
  example = FALSE
) {
  resolution <- rlang::arg_match(resolution)
  cfg <- .ci_group_config(crop_groups)
  if (isTRUE(example)) {
    return(.example_carbon_inputs())
  }
  d <- .ci_resolve_inputs(data, years, cfg)
  dplyr::bind_rows(d$cropland, d$grass_natural) |>
    .ci_finalise(resolution, data$land_use) |>
    .add_reporting_polity_columns()
}

# -- Input resolution ---------------------------------------------------------

.ci_resolve_inputs <- function(data, years = NULL, cfg = .ci_group_config()) {
  crop_area <- data$crop_area %||% .ci_crop_area(data)
  shares <- .ci_regime_shares(data, years, cfg)
  list(
    cropland = .ci_cropland_input(data, years, crop_area, shares),
    crop_area = crop_area,
    grass_natural = data$grass_natural %||%
      build_grass_natural_carbon_inputs(data = data, years = years)
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
.ci_cropland_input <- function(data, years, crop_area, shares = NULL) {
  if (!is.null(data$cropland)) {
    return(.ci_cropland_class(data$cropland, crop_area, shares))
  }
  .sci_build(
    "grid",
    data,
    years,
    reduce = \(gridded) .ci_cropland_class(gridded, crop_area, shares)
  )
}

.ci_cropland_class <- function(cropland, crop_area, shares = NULL) {
  join_keys <- c("lon", "lat", "area_code", "item_prod_code")
  if (rlang::has_name(crop_area, "year")) {
    join_keys <- c(join_keys, "year")
  }
  joined <- dplyr::inner_join(cropland, crop_area, by = join_keys)
  if (is.null(shares)) {
    joined <- dplyr::mutate(
      joined,
      land_use = "cropland",
      method_c_input = "humified_weighted"
    )
  } else {
    joined <- .ci_split_into_groups(joined, shares)
  }
  joined |>
    dplyr::mutate(
      c_mass = .data$total_c_input_mgc_ha_yr * .data$crop_area_ha
    ) |>
    dplyr::summarise(
      c_input_mgc_ha_yr = .ci_wmean(
        .data$total_c_input_mgc_ha_yr,
        .data$crop_area_ha
      ),
      humified_fraction = .ci_wmean(.data$humified_fraction, .data$c_mass),
      class_area_ha = sum(.data$crop_area_ha),
      method_c_input = .data$method_c_input[1],
      .by = c("lon", "lat", "area_code", "year", "land_use")
    )
}

# Fan each crop row out into its rainfed and irrigated parts and label each
# with its crop group. The area weight splits by the crop's irrigated share
# of its cell area; the per-hectare density and humified fraction are the
# crop's own on both parts (nothing in the input layer distinguishes an
# irrigated hectare's residue from a rainfed one's -- that difference enters
# through the balance's cover and water terms, per group). A crop with no
# share row is wholly rainfed; that is a data gap worth seeing, so the count
# is reported rather than absorbed.
.ci_split_into_groups <- function(joined, shares) {
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
  with_share <- dplyr::mutate(
    with_share,
    irrigated_share = pmin(
      pmax(dplyr::coalesce(.data$irrigated_share, 0), 0),
      1
    )
  )
  parts <- dplyr::bind_rows(
    dplyr::mutate(
      with_share,
      irrigated = FALSE,
      crop_area_ha = .data$crop_area_ha * (1 - .data$irrigated_share)
    ),
    dplyr::mutate(
      with_share,
      irrigated = TRUE,
      crop_area_ha = .data$crop_area_ha * .data$irrigated_share
    )
  ) |>
    dplyr::filter(.data$crop_area_ha > 0)
  parts |>
    dplyr::mutate(
      land_use = soc_crop_group(
        as.integer(.data$item_prod_code),
        .data$irrigated
      ),
      method_c_input = "humified_weighted_spain_hist"
    ) |>
    dplyr::select(-"irrigated", -"irrigated_share")
}

# -- Crop groups: configuration and the irrigated-share layer -----------------

# Validate the crop_groups configuration element-wise. Absent elements take
# their default; unknown names or values abort, so a typo cannot silently
# select the single-class path.
.ci_group_config <- function(crop_groups = list()) {
  known <- c("method", "irrigation")
  extra <- setdiff(names(crop_groups), known)
  if (length(extra) > 0L) {
    cli::cli_abort(
      "Unknown {.arg crop_groups} element{?s}: {.val {extra}}."
    )
  }
  list(
    method = rlang::arg_match0(
      crop_groups$method %||% "none",
      c("none", "spain_hist"),
      arg_nm = "crop_groups$method"
    ),
    irrigation = rlang::arg_match0(
      crop_groups$irrigation %||% "spatialized",
      c("spatialized", "none"),
      arg_nm = "crop_groups$irrigation"
    )
  )
}

# The irrigated share of each crop's cell area, per year. NULL means the
# single-class path (or, under `irrigation = "none"`, every crop rainfed, which
# the split treats as a share of zero everywhere).
.ci_regime_shares <- function(data, years, cfg) {
  if (!identical(cfg$method, "spain_hist")) {
    return(NULL)
  }
  if (!is.null(data$crop_regime_share)) {
    return(data$crop_regime_share)
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
  .ci_spatialized_regime_share(years)
}

# Crop-specific, yearly irrigated shares from the spatialization chain on its
# pinned inputs: build_gridded_landuse() allocates each crop's national
# irrigated area over cells, so the share is rainfed_ha / irrigated_ha per
# cell x crop x year -- the only source WHEP has that is both crop-specific
# and dynamic (LPJmL's cftfrac folds woody crops into one PFT and cannot
# recover species; MIRCA is a static 2000 snapshot).
.ci_spatialized_regime_share <- function(years) {
  aliases <- .spatial_input_aliases()
  read <- function(key, file) {
    .read_spatial_input(NULL, file, aliases[[key]])
  }
  gridded <- build_gridded_landuse(
    country_areas = read("country_areas", "country_areas.parquet"),
    crop_patterns = read("crop_patterns", "crop_patterns.parquet"),
    gridded_cropland = read("gridded_cropland", "gridded_cropland.parquet"),
    country_grid = read("country_grid", "country_grid.parquet"),
    config = list(years = years)
  )
  gridded |>
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
      method_c_input = .data$method_c_input[1],
      .by = c("area_code", "year", "land_use")
    ) |>
    tibble::as_tibble()
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
