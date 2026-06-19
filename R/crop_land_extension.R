#' Build per-crop physical cropland extension.
#'
#' @description
#' Convert gridded crop *harvested* area into per-crop *physical* land area and
#' aggregate it to commodity-balance items, producing a land extension keyed by
#' `(year, area_code, item_cbs_code)` for the FABIO footprint model.
#'
#' The gridded land-use pipeline ([build_gridded_landuse()]) distributes FAOSTAT
#' harvested area across grid cells; per-crop totals therefore conserve to
#' harvested area, which over-counts multi-cropped land and under-counts fallow.
#' This function turns that harvested area into physical occupied land:
#'
#' - `"cropland_apportion"` (default): within each cell, the cell's physical
#'   cropland (`cropland_ha`, from LUH2) is split across crops in proportion to
#'   their share of the cell's harvested area. Per-crop physical area then
#'   conserves to physical cropland rather than to harvested area, capturing both
#'   double-cropping (scaled down) and fallow (resting land charged to the crops
#'   the rotation supports) at the resolution of the grid.
#' - `"intensity_divide"`: each crop's harvested area is divided by the cell
#'   multi-cropping intensity (`mc_rainfed`, `mc_irrigated`). Requires
#'   `multicropping`.
#'
#' Unlike a single country-level cropping-intensity factor applied uniformly to
#' every crop, both methods distribute physical cropland by the actual spatial
#' pattern of each crop.
#'
#' Coverage note: with `"cropland_apportion"` the per-country crop total is
#' bounded by the LUH2 `cropland` layer, which can under-represent perennial or
#' plantation crops (e.g. oil palm, rubber) classified outside cropland; such
#' crops may receive less land than a harvested-area baseline implies.
#'
#' @param gridded_crops Tibble of gridded crop harvested area, the crop-level
#'   output of [build_gridded_landuse()] (built without CFT aggregation). Must
#'   have columns `lon`, `lat`, `year`, `area_code`, `item_prod_code`,
#'   `rainfed_ha`, `irrigated_ha`.
#' @param gridded_cropland Tibble of physical cropland per cell. Must have
#'   columns `lon`, `lat`, `year`, `cropland_ha`.
#' @param items_prod_full Crosswalk from production items to commodity-balance
#'   items. Defaults to [items_prod_full]. Must have columns `item_prod_code`
#'   and `item_cbs_code`.
#' @param method Physical-area conversion method. One of `"cropland_apportion"`
#'   (default) or `"intensity_divide"`.
#' @param multicropping Tibble of per-cell multi-cropping intensity, required for
#'   `method = "intensity_divide"`. Must have columns `lon`, `lat`,
#'   `mc_rainfed`, `mc_irrigated` (and optionally `year`).
#'
#' @return A tibble with columns `year`, `area_code`, `item_cbs_code`,
#'   `impact_u` (physical land area in hectares), and `method_land` (the chosen
#'   method).
#'
#' @export
#'
#' @examples
#' gridded_crops <- tibble::tribble(
#'   ~lon, ~lat, ~year, ~area_code, ~item_prod_code, ~rainfed_ha, ~irrigated_ha,
#'   0.25, 50.25, 2000L, 1L, 15L, 600, 0,
#'   0.25, 50.25, 2000L, 1L, 27L, 200, 0,
#'   0.75, 50.25, 2000L, 1L, 15L, 400, 0
#' )
#' gridded_cropland <- tibble::tribble(
#'   ~lon, ~lat, ~year, ~cropland_ha,
#'   0.25, 50.25, 2000L, 1000,
#'   0.75, 50.25, 2000L, 500
#' )
#' items <- tibble::tribble(
#'   ~item_prod_code, ~item_cbs_code,
#'   15L, 2511L,
#'   27L, 2805L
#' )
#' build_crop_land_extension(gridded_crops, gridded_cropland, items_prod_full = items)
build_crop_land_extension <- function(
  gridded_crops,
  gridded_cropland,
  items_prod_full = whep::items_prod_full,
  method = c("cropland_apportion", "intensity_divide"),
  multicropping = NULL
) {
  method <- match.arg(method)
  .validate_crop_land_inputs(
    gridded_crops,
    gridded_cropland,
    items_prod_full,
    method,
    multicropping
  )

  physical <- switch(
    method,
    cropland_apportion = .crop_physical_apportion(
      gridded_crops,
      gridded_cropland
    ),
    intensity_divide = .crop_physical_intensity(
      gridded_crops,
      multicropping
    )
  )

  .aggregate_crop_land_to_cbs(physical, items_prod_full) |>
    dplyr::mutate(method_land = method)
}

#' Get the per-crop physical cropland extension from spatialization inputs.
#'
#' @description
#' Convenience wrapper that loads the gridded land-use inputs, spatializes crop
#' harvested area with [build_gridded_landuse()] (crop-level, no CFT
#' aggregation), and converts it to a per-crop physical land extension with
#' [build_crop_land_extension()]. The result is keyed by
#' `(year, area_code, item_cbs_code)` and ready to use as `extensions` in
#' [compute_footprint()].
#'
#' @param input_dir Directory holding the spatialization inputs
#'   (`country_areas.parquet`, `crop_patterns.parquet`,
#'   `gridded_cropland.parquet`, `country_grid.parquet`, and optionally
#'   `multicropping.parquet`). Typically `<l_files_dir>/whep/inputs`. Required
#'   unless `example = TRUE`.
#' @param years Numeric vector of years to compute, or `NULL` for all available.
#' @param method Physical-area conversion method passed to
#'   [build_crop_land_extension()].
#' @param use_type_constraint If `TRUE`, restrict each crop to cells of its LUH2
#'   type (requires `type_cropland.parquet`). Defaults to `FALSE`.
#' @param fill_missing_patterns If `TRUE` (default), crops that have harvested
#'   area but no `crop_patterns` rows (e.g. Barley, absent from the Monfreda
#'   layer) are placed with a uniform fallback pattern over each producing
#'   country's cropland, so their land is not silently dropped.
#' @param example If `TRUE`, return a small example output without reading
#'   remote/large data. Defaults to `FALSE`.
#'
#' @return A tibble with columns `year`, `area_code`, `item_cbs_code`,
#'   `impact_u` (physical land in hectares), and `method_land`.
#'
#' @export
#'
#' @examples
#' get_crop_land_extension(example = TRUE)
get_crop_land_extension <- function(
  input_dir = NULL,
  years = NULL,
  method = c("cropland_apportion", "intensity_divide"),
  use_type_constraint = FALSE,
  fill_missing_patterns = TRUE,
  example = FALSE
) {
  method <- match.arg(method)
  if (isTRUE(example)) {
    return(.example_crop_land_extension())
  }
  if (is.null(input_dir) || !dir.exists(input_dir)) {
    cli::cli_abort(
      "{.arg input_dir} must be an existing directory with the
      spatialization inputs."
    )
  }

  inputs <- .load_landuse_inputs(
    input_dir,
    list(use_type_constraint = use_type_constraint)
  )
  if (isTRUE(fill_missing_patterns)) {
    inputs$crop_patterns <- .augment_missing_crop_patterns(
      inputs$crop_patterns,
      inputs$country_areas
    )
  }
  lu_config <- list(
    years = years,
    multicropping = inputs$multicropping,
    type_cropland = if (use_type_constraint) inputs$type_cropland,
    type_mapping = if (use_type_constraint) inputs$type_mapping,
    n_workers = 1L
  )
  gridded_crops <- build_gridded_landuse(
    inputs$country_areas,
    inputs$crop_patterns,
    inputs$gridded_cropland,
    inputs$country_grid,
    config = lu_config
  )
  build_crop_land_extension(
    gridded_crops,
    inputs$gridded_cropland,
    method = method,
    multicropping = inputs$multicropping
  )
}

#' Build a per-crop physical land extension from CROPGRIDS.
#'
#' @description
#' Convert FAOSTAT harvested area into per-crop *physical* cropland using
#' CROPGRIDS, then return a land extension keyed by
#' `(year, area_code, item_cbs_code)` for the FABIO footprint model.
#'
#' CROPGRIDS (Tang et al. 2024) reports, per crop and country, both **harvested**
#' area and **crop (physical) area** for 2020. Their ratio is a genuinely
#' per-crop multi-cropping correction — e.g. rice ~0.81 (heavily double-cropped),
#' most other crops ~0.95-1.0 (single-cropped) — which a single country-level
#' cropping-intensity factor, or a cell-level apportionment by harvested share,
#' cannot reproduce. This function applies that per-(area, item) physical /
#' harvested ratio to WHEP harvested area in each year:
#' \eqn{physical = harvested \times (physical_{cg} / harvested_{cg})}.
#'
#' Note: CROPGRIDS physical area is the land where each crop actually grows; it
#' excludes fallow land (unlike fallow-inclusive cropland-apportionment), so
#' totals are typically a few percent below harvested area.
#'
#' @param harvested Tibble of harvested area with columns `year`, `area_code`,
#'   `item_cbs_code`, `harvested_ha`. If `NULL`, built from
#'   [get_primary_production()] (`unit == "ha"`).
#' @param cropgrids Tibble of national crop areas with columns `area_code`,
#'   `item_cbs_code`, `physical_ha`, `harvested_ha`. If `NULL`, the packaged
#'   table selected by `source` is used.
#' @param source Which packaged table to use when `cropgrids` is `NULL`:
#'   `"cropgrids"` (physical crop area, excludes fallow) or `"cropgrids_fallow"`
#'   (physical area with rotational fallow attributed to crops by
#'   [attribute_fallow_to_crops()]). Also recorded in `method_land`.
#'
#' @return A tibble with columns `year`, `area_code`, `item_cbs_code`,
#'   `impact_u` (physical land area in hectares), and `method_land`.
#'
#' @export
#'
#' @examples
#' harvested <- tibble::tribble(
#'   ~year, ~area_code, ~item_cbs_code, ~harvested_ha,
#'   2000L, 33L, 2511L, 1000,
#'   2000L, 33L, 2807L, 500
#' )
#' cropgrids <- tibble::tribble(
#'   ~area_code, ~item_cbs_code, ~physical_ha, ~harvested_ha,
#'   33L, 2511L, 990, 1000,
#'   33L, 2807L, 400, 500
#' )
#' build_cropgrids_land_extension(harvested, cropgrids)
build_cropgrids_land_extension <- function(
  harvested = NULL,
  cropgrids = NULL,
  source = c("cropgrids", "cropgrids_fallow")
) {
  source <- match.arg(source)
  if (is.null(harvested)) {
    harvested <- .harvested_area_by_cbs(get_primary_production())
  }
  if (is.null(cropgrids)) {
    cropgrids <- .read_cropgrids_land(paste0(source, "_land.csv"))
  }
  .check_required_cols(
    harvested,
    c("year", "area_code", "item_cbs_code", "harvested_ha"),
    "harvested"
  )
  .check_required_cols(
    cropgrids,
    c("area_code", "item_cbs_code", "physical_ha", "harvested_ha"),
    "cropgrids"
  )

  area_ratio <- cropgrids |>
    dplyr::filter(.data$harvested_ha > 0) |>
    dplyr::transmute(
      area_code = as.integer(.data$area_code),
      item_cbs_code = as.integer(.data$item_cbs_code),
      ratio = .data$physical_ha / .data$harvested_ha
    )
  global_ratio <- cropgrids |>
    dplyr::summarise(
      ratio_global = sum(.data$physical_ha, na.rm = TRUE) /
        sum(.data$harvested_ha, na.rm = TRUE),
      .by = item_cbs_code
    ) |>
    dplyr::mutate(item_cbs_code = as.integer(.data$item_cbs_code))

  harvested |>
    dplyr::mutate(
      area_code = as.integer(.data$area_code),
      item_cbs_code = as.integer(.data$item_cbs_code)
    ) |>
    dplyr::left_join(area_ratio, by = c("area_code", "item_cbs_code")) |>
    dplyr::left_join(global_ratio, by = "item_cbs_code") |>
    dplyr::mutate(
      ratio = dplyr::coalesce(.data$ratio, .data$ratio_global, 1),
      impact_u = .data$harvested_ha * .data$ratio,
      method_land = source
    ) |>
    dplyr::filter(.data$impact_u > 0) |>
    dplyr::transmute(
      year = as.integer(.data$year),
      area_code = .data$area_code,
      item_cbs_code = .data$item_cbs_code,
      impact_u = .data$impact_u,
      method_land = .data$method_land
    )
}

#' Attribute reported fallow land to crops.
#'
#' @description
#' Distribute each country's FAOSTAT-reported fallow area among its crops using a
#' precomputed allocation weight, adding the result to each crop's cropped
#' physical area. The weight is typically [gridded_fallow_weights()], which puts
#' fallow on rainfed dryland cereals and rainfed monsoon rice and keeps it off
#' irrigated/continuous systems.
#'
#' The fallow *magnitude* comes from `fallow_total` (FAOSTAT "Land with temporary
#' fallow", item 6640) — reported separately from temporary meadows/pastures, so
#' it isolates real fallow from fodder.
#'
#' @param cropgrids Tibble of national crop areas with columns `area_code`,
#'   `item_cbs_code`, `physical_ha` (cropped physical area), `harvested_ha`.
#' @param fallow_total Tibble of reported fallow area with columns `area_code`
#'   and `fallow_ha`.
#' @param alloc_weight Tibble of `area_code`, `item_cbs_code`, `weight` giving
#'   the within-country allocation weight, e.g. from [gridded_fallow_weights()].
#'
#' @return A tibble with `area_code`, `item_cbs_code`, `physical_ha` (cropped
#'   physical area plus attributed fallow), and `harvested_ha`.
#'
#' @export
#'
#' @examples
#' cropgrids <- tibble::tribble(
#'   ~area_code, ~item_cbs_code, ~physical_ha, ~harvested_ha,
#'   1L, 2511L, 500, 500,
#'   1L, 2807L, 400, 400
#' )
#' fallow_total <- tibble::tribble(~area_code, ~fallow_ha, 1L, 200)
#' # all weight on wheat -> the 200 ha reported fallow goes to wheat
#' alloc_weight <- tibble::tribble(
#'   ~area_code, ~item_cbs_code, ~weight,
#'   1L, 2511L, 1,
#'   1L, 2807L, 0
#' )
#' attribute_fallow_to_crops(cropgrids, fallow_total, alloc_weight)
attribute_fallow_to_crops <- function(cropgrids, fallow_total, alloc_weight) {
  .check_required_cols(
    cropgrids,
    c("area_code", "item_cbs_code", "physical_ha", "harvested_ha"),
    "cropgrids"
  )
  .check_required_cols(
    fallow_total,
    c("area_code", "fallow_ha"),
    "fallow_total"
  )
  .check_required_cols(
    alloc_weight,
    c("area_code", "item_cbs_code", "weight"),
    "alloc_weight"
  )

  fallow <- fallow_total |>
    dplyr::transmute(
      area_code = as.integer(.data$area_code),
      fallow_ha = as.numeric(.data$fallow_ha)
    )
  weights <- alloc_weight |>
    dplyr::transmute(
      area_code = as.integer(.data$area_code),
      item_cbs_code = as.integer(.data$item_cbs_code),
      weight = .data$weight
    )

  cropgrids |>
    dplyr::mutate(
      area_code = as.integer(.data$area_code),
      item_cbs_code = as.integer(.data$item_cbs_code)
    ) |>
    dplyr::left_join(weights, by = c("area_code", "item_cbs_code")) |>
    dplyr::left_join(fallow, by = "area_code") |>
    dplyr::mutate(
      weight = tidyr::replace_na(.data$weight, 0),
      fallow_ha = tidyr::replace_na(.data$fallow_ha, 0)
    ) |>
    dplyr::mutate(
      Wsum = sum(.data$weight, na.rm = TRUE),
      physical_ha = .data$physical_ha +
        dplyr::if_else(
          .data$Wsum > 0,
          .data$fallow_ha * .data$weight / .data$Wsum,
          0
        ),
      .by = area_code
    ) |>
    dplyr::transmute(
      area_code = .data$area_code,
      item_cbs_code = .data$item_cbs_code,
      physical_ha = .data$physical_ha,
      harvested_ha = .data$harvested_ha
    ) |>
    dplyr::arrange(.data$area_code, .data$item_cbs_code)
}

#' Build agro-climatic, rainfed-gated fallow allocation weights.
#'
#' @description
#' Compute a per-(area, item) weight for distributing reported fallow to crops,
#' from gridded crop placement and a crop x agro-climatic-zone propensity. For
#' each grid cell, rainfed crop area is multiplied by the crop's propensity in
#' the cell's agro-climatic zone (derived from GAEZ length of growing period and
#' thermal climate), then summed to country x item. Dryland cereals/pulses score
#' high in arid/semi-arid zones, rainfed rice high in the humid tropics
#' (rice-fallow); perennials and the irrigated share score ~zero.
#'
#' @param gridded_crops Tibble keyed by grid cell and `item_cbs_code` with
#'   columns `lon`, `lat`, `area_code`, `rainfed_ha`.
#' @param grid_aez Tibble of `lon`, `lat`, `lgp` (length of growing period in
#'   days), `thermal` (GAEZ thermal-climate class). If `NULL`, the packaged
#'   `grid_aez.csv` is used.
#' @param propensity Tibble of `item_cbs_code`, `zone`, `fallow_propensity`. If
#'   `NULL`, the packaged `fallow_propensity.csv` is used.
#'
#' @return A tibble with `area_code`, `item_cbs_code`, `weight`.
#'
#' @export
#'
#' @examples
#' gridded_crops <- tibble::tribble(
#'   ~lon, ~lat, ~area_code, ~item_cbs_code, ~rainfed_ha,
#'   0.25, 50.25, 1L, 2511L, 500
#' )
#' grid_aez <- tibble::tribble(~lon, ~lat, ~lgp, ~thermal, 0.25, 50.25, 100, 7L)
#' propensity <- tibble::tribble(
#'   ~item_cbs_code, ~zone, ~fallow_propensity,
#'   2511L, "semiarid", 0.8
#' )
#' gridded_fallow_weights(gridded_crops, grid_aez, propensity)
gridded_fallow_weights <- function(
  gridded_crops,
  grid_aez = NULL,
  propensity = NULL
) {
  if (is.null(grid_aez)) {
    grid_aez <- .read_grid_aez()
  }
  if (is.null(propensity)) {
    propensity <- .read_fallow_propensity()
  }
  .check_required_cols(
    gridded_crops,
    c("lon", "lat", "area_code", "item_cbs_code", "rainfed_ha"),
    "gridded_crops"
  )
  .check_required_cols(grid_aez, c("lon", "lat", "lgp", "thermal"), "grid_aez")
  .check_required_cols(
    propensity,
    c("item_cbs_code", "zone", "fallow_propensity"),
    "propensity"
  )

  aez <- grid_aez |>
    dplyr::transmute(
      lon = .data$lon,
      lat = .data$lat,
      zone = .aez_zone(.data$lgp, .data$thermal)
    )
  prop <- propensity |>
    dplyr::transmute(
      item_cbs_code = as.integer(.data$item_cbs_code),
      zone = .data$zone,
      p = .data$fallow_propensity
    )

  # Weighting area is rainfed crop area (irrigated land is continuously cropped,
  # so it does not contribute to rotational fallow).
  gridded_crops |>
    dplyr::mutate(
      area_code = as.integer(.data$area_code),
      item_cbs_code = as.integer(.data$item_cbs_code)
    ) |>
    dplyr::left_join(aez, by = c("lon", "lat")) |>
    dplyr::mutate(zone = tidyr::replace_na(.data$zone, "subhumid")) |>
    dplyr::left_join(prop, by = c("item_cbs_code", "zone")) |>
    dplyr::mutate(p = tidyr::replace_na(.data$p, 0)) |>
    dplyr::summarise(
      weight = sum(.ext_na0(.data$rainfed_ha) * .data$p, na.rm = TRUE),
      .by = c(area_code, item_cbs_code)
    ) |>
    dplyr::filter(.data$weight > 0)
}

# Agro-climatic zone from GAEZ length of growing period (days) and thermal class
# (classes 1-5 = tropics/subtropics). The humid tropics is the rice-fallow zone.
.aez_zone <- function(lgp, thermal) {
  tropical <- !is.na(thermal) & thermal %in% 1:5
  lgp <- ifelse(is.na(lgp), 200, lgp)
  dplyr::case_when(
    tropical & lgp >= 180 ~ "tropical_humid",
    lgp < 90 ~ "arid",
    lgp < 180 ~ "semiarid",
    lgp < 270 ~ "subhumid",
    TRUE ~ "humid"
  )
}

.read_grid_aez <- function() {
  path <- system.file("extdata", "grid_aez.csv", package = "whep")
  if (!nzchar(path)) {
    cli::cli_abort("{.file grid_aez.csv} not found in installed package.")
  }
  readr::read_csv(path, show_col_types = FALSE)
}

.read_fallow_propensity <- function() {
  path <- system.file("extdata", "fallow_propensity.csv", package = "whep")
  if (!nzchar(path)) {
    cli::cli_abort(
      "{.file fallow_propensity.csv} not found in installed package."
    )
  }
  readr::read_csv(path, show_col_types = FALSE)
}

.harvested_area_by_cbs <- function(primary_prod) {
  primary_prod |>
    dplyr::filter(.data$unit == "ha", !is.na(.data$item_cbs_code)) |>
    dplyr::summarise(
      harvested_ha = sum(.data$value, na.rm = TRUE),
      .by = c(year, area_code, item_cbs_code)
    )
}

.read_cropgrids_land <- function(file = "cropgrids_land.csv") {
  path <- system.file("extdata", file, package = "whep")
  if (!nzchar(path)) {
    cli::cli_abort("{.file {file}} not found in installed package.")
  }
  readr::read_csv(path, show_col_types = FALSE)
}

# --- Fallback patterns for crops absent from crop_patterns --------------------

.augment_missing_crop_patterns <- function(
  crop_patterns,
  country_areas
) {
  missing <- setdiff(
    unique(country_areas$item_prod_code),
    unique(crop_patterns$item_prod_code)
  )
  if (length(missing) == 0L) {
    return(crop_patterns)
  }

  # Place missing-pattern crops where crops actually grow, by weighting each
  # cell by the aggregate cropping intensity of all mapped crops, rather than
  # uniformly over cropland (which would push them onto low-intensity land).
  agg_pattern <- crop_patterns |>
    dplyr::summarise(
      harvest_fraction = sum(.data$harvest_fraction, na.rm = TRUE),
      .by = c("lon", "lat")
    ) |>
    dplyr::filter(.data$harvest_fraction > 0)
  synthetic <- tidyr::expand_grid(
    dplyr::select(agg_pattern, "lon", "lat", "harvest_fraction"),
    item_prod_code = missing
  )

  cli::cli_alert_info(
    "Aggregate-pattern fallback added for {length(missing)} crop{?s}
    missing from {.field crop_patterns}."
  )
  dplyr::bind_rows(crop_patterns, synthetic)
}

# --- Physical-area conversion -------------------------------------------------

.crop_physical_apportion <- function(gridded_crops, gridded_cropland) {
  crops <- data.table::as.data.table(gridded_crops)
  crops[, harvested_ha := .ext_na0(rainfed_ha) + .ext_na0(irrigated_ha)]
  crops <- crops[harvested_ha > 0]
  crops[, cell_harv_total := sum(harvested_ha), by = c("lon", "lat", "year")]

  cropland <- data.table::as.data.table(gridded_cropland)[,
    .(lon, lat, year, cropland_ha)
  ]
  crops <- merge(
    crops,
    cropland,
    by = c("lon", "lat", "year"),
    all.x = TRUE
  )
  # When a cell has crops but no recorded physical cropland, fall back to the
  # cell's harvested total so its area is conserved rather than dropped.
  crops[,
    cropland_ha := data.table::fifelse(
      is.na(cropland_ha) | cropland_ha <= 0,
      cell_harv_total,
      cropland_ha
    )
  ]
  crops[,
    physical_ha := data.table::fifelse(
      cell_harv_total > 0,
      cropland_ha * harvested_ha / cell_harv_total,
      0
    )
  ]
  crops[, .(lon, lat, year, area_code, item_prod_code, physical_ha)]
}

.crop_physical_intensity <- function(gridded_crops, multicropping) {
  crops <- data.table::as.data.table(gridded_crops)
  mc <- data.table::as.data.table(multicropping)
  join_cols <- intersect(c("lon", "lat", "year"), names(mc))
  crops <- merge(crops, mc, by = join_cols, all.x = TRUE)
  crops[is.na(mc_rainfed), mc_rainfed := 1]
  crops[is.na(mc_irrigated), mc_irrigated := 1]
  crops[,
    physical_ha := .ext_na0(rainfed_ha) /
      pmax(mc_rainfed, 1) +
      .ext_na0(irrigated_ha) / pmax(mc_irrigated, 1)
  ]
  crops[, .(lon, lat, year, area_code, item_prod_code, physical_ha)]
}

# --- Aggregation to commodity-balance items -----------------------------------

.aggregate_crop_land_to_cbs <- function(physical, items_prod_full) {
  crosswalk <- items_prod_full |>
    dplyr::transmute(
      item_prod_code = .as_integer_quiet(.data$item_prod_code),
      item_cbs_code = .as_integer_quiet(.data$item_cbs_code)
    ) |>
    dplyr::filter(
      !is.na(.data$item_prod_code),
      !is.na(.data$item_cbs_code)
    ) |>
    dplyr::distinct()

  dt <- data.table::as.data.table(physical)
  dt[, item_prod_code := as.integer(item_prod_code)]
  dt <- data.table::as.data.table(crosswalk)[dt, on = "item_prod_code"]

  .warn_unmapped_crop_land(dt)
  dt <- dt[!is.na(item_cbs_code)]

  out <- dt[,
    .(impact_u = sum(physical_ha, na.rm = TRUE)),
    by = c("year", "area_code", "item_cbs_code")
  ]
  out[,
    `:=`(
      year = as.integer(year),
      area_code = as.integer(area_code),
      item_cbs_code = as.integer(item_cbs_code)
    )
  ]
  out <- out[impact_u > 0]
  data.table::setorder(out, year, area_code, item_cbs_code)
  tibble::as_tibble(out)
}

.warn_unmapped_crop_land <- function(dt) {
  unmapped <- dt[is.na(item_cbs_code), sum(physical_ha, na.rm = TRUE)]
  total <- dt[, sum(physical_ha, na.rm = TRUE)]
  if (unmapped > 0 && total > 0) {
    pct <- round(100 * unmapped / total, 2)
    cli::cli_warn(c(
      "!" = "Dropping {pct}% of physical crop land with no
        {.field item_cbs_code} mapping.",
      "i" = "Add the missing {.field item_prod_code}s to
        {.arg items_prod_full}."
    ))
  }
}

# --- Validation ---------------------------------------------------------------

.validate_crop_land_inputs <- function(
  gridded_crops,
  gridded_cropland,
  items_prod_full,
  method,
  multicropping
) {
  .check_required_cols(
    gridded_crops,
    c(
      "lon",
      "lat",
      "year",
      "area_code",
      "item_prod_code",
      "rainfed_ha",
      "irrigated_ha"
    ),
    "gridded_crops"
  )
  .check_required_cols(
    gridded_cropland,
    c("lon", "lat", "year", "cropland_ha"),
    "gridded_cropland"
  )
  .check_required_cols(
    items_prod_full,
    c("item_prod_code", "item_cbs_code"),
    "items_prod_full"
  )
  if (method == "intensity_divide") {
    if (is.null(multicropping)) {
      cli::cli_abort(
        '{.arg multicropping} is required when
        {.code method = "intensity_divide"}.'
      )
    }
    .check_required_cols(
      multicropping,
      c("lon", "lat", "mc_rainfed", "mc_irrigated"),
      "multicropping"
    )
  }
}

.ext_na0 <- function(x) {
  data.table::fcoalesce(as.numeric(x), 0)
}
