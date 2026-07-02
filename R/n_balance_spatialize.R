# Real cell-polity crosswalk assembly + polity-total-to-crop spatialization
# (Module C, Task C6).
#
# CONFIRMED FACTS (local files inspected; do not re-guess):
# - The cell-polity parquet at Sys.getenv("WHEP_POLITY_FRACTION_PATH") has
#   lon, lat, area_code, polity_frac already (68,527 rows) but no
#   cell_area_ha; that is added here from latitude via the SAME formula
#   .cell_area_ha_lat() in R/feed_lpjml.R (a private package helper, called
#   directly, never redefined).
# - crop_patterns.parquet (Sys.getenv("WHEP_CROP_PATTERNS_PATH")): lon, lat,
#   item_prod_code, harvest_fraction, a STATIC crop-pattern weight (no year
#   dimension), 2,247,239 rows.
# - type_cropland.parquet (Sys.getenv("WHEP_TYPE_CROPLAND_PATH")): lon, lat,
#   year, luh2_type, type_ha, type_irrig_ha, an annual per-cell cropland
#   area by LUH2 class, 27,496,275 rows.
# - Per-cell per-crop hectares = type_ha * harvest_fraction, the exact
#   formula already used by make_lpjml_covariate()'s "crop_pattern" weighting
#   branch (R/lpjml_covariate.R); reused verbatim here rather than
#   reimplemented, and summed across every luh2_type row for a cell-year
#   before multiplying (a cell can carry more than one LUH2 cropland class).
# - Both parquets are too large to read whole into a package-load context
#   (2.2M / 27.5M rows): the reader filters immediately after reading, before
#   any join, to the requested years/item_prod_codes.

#' Assemble WHEP's cell-polity crosswalk with true grid-cell area.
#'
#' @description
#' Reads the cached cell-polity fraction parquet (`lon`, `lat`, `area_code`,
#' `polity_frac`) and adds `cell_area_ha`, computed from latitude with the
#' same 0.5-degree cell-area formula used across the package (see
#' [build_grass_availability_lpjml()]). This assembles the
#' `data$cell_polity` contract that every Module C function (e.g.
#' [build_n_deposition()], [build_urban_n()], [get_soc_climate_drivers()])
#' expects as a required input.
#'
#' @param polity_fraction_path Path to the cell-polity fraction parquet.
#'   Defaults to `Sys.getenv("WHEP_POLITY_FRACTION_PATH")`.
#' @return A tibble with `lon`, `lat`, `area_code`, `polity_frac` and
#'   `cell_area_ha`.
#' @export
#' @examples
#' # Requires WHEP_POLITY_FRACTION_PATH to be set; not run without it.
#' if (nzchar(Sys.getenv("WHEP_POLITY_FRACTION_PATH"))) {
#'   build_cell_polity()
#' }
build_cell_polity <- function(polity_fraction_path = NULL) {
  path <- .resolve_polity_fraction_path(polity_fraction_path)
  raw <- nanoparquet::read_parquet(path) |> tibble::as_tibble()
  .check_columns(
    raw,
    c("lon", "lat", "area_code", "polity_frac"),
    "cell_polity"
  )
  dplyr::mutate(raw, cell_area_ha = .cell_area_ha_lat(.data$lat))
}

#' Spatialize a polity-level nitrogen total to crops and grid cells.
#'
#' @description
#' Promotes a single polity-total nitrogen input (one row per `year`,
#' `area_code`, for one fertiliser type) to the crop level by the same
#' harvested-area-share logic used by
#' [build_crop_soil_n2o_extension()] (`year`/`area_code`-matched, weighted by
#' each crop's share of harvested cropland area), then optionally further to
#' the grid level by distributing each polity-crop total across cells in
#' proportion to the cell's share of that polity-crop's total crop-pattern
#' area (`type_ha * harvest_fraction`, summed over LUH2 cropland classes;
#' the exact formula used by [make_lpjml_covariate()]'s `crop_pattern`
#' weighting).
#'
#' @param country_totals A tibble with `year`, `area_code`, `n_t`: the
#'   polity-level nitrogen total for one fertiliser type.
#' @param crop_shares A tibble with `year`, `area_code`, `item_cbs_code`,
#'   `area_share`: harvested-area-weighted crop shares within each
#'   country-year, e.g. from [build_crop_soil_n2o_extension()]'s internal
#'   crop-area-share helper.
#' @param cell_polity The [build_cell_polity()]-shaped crosswalk (`lon`,
#'   `lat`, `area_code`, `polity_frac`, `cell_area_ha`). Only required when
#'   `resolution` includes `"grid"`.
#' @param resolution Which resolution(s) to return: `"polity_crop"` (default,
#'   `year`/`area_code`/`item_cbs_code` totals only) or `"grid"` (also
#'   distributes to `lon`/`lat` grid cells; requires `crop_patterns` and
#'   `type_cropland` in `data`).
#' @param data Optional named list of pre-loaded grid inputs, used only when
#'   `resolution = "grid"`: `crop_patterns` (`lon`, `lat`, `item_prod_code`,
#'   `harvest_fraction`) and `type_cropland` (`lon`, `lat`, `year`,
#'   `luh2_type`, `type_ha`), each falling back to a lazy parquet read from
#'   `Sys.getenv("WHEP_CROP_PATTERNS_PATH")` /
#'   `Sys.getenv("WHEP_TYPE_CROPLAND_PATH")` when absent. `item_cbs_code` in
#'   `crop_shares`/`country_totals` is matched to the `item_prod_code` column
#'   of `crop_patterns` via [whep::items_prod_full] (the same crosswalk
#'   [build_crop_land_extension()] uses).
#' @return A tibble. For `resolution = "polity_crop"`: `year`, `area_code`,
#'   `item_cbs_code`, `n_t`. For `resolution = "grid"`: `lon`, `lat`,
#'   `area_code`, `year`, `item_cbs_code`, `n_t`.
#' @export
#' @examples
#' spatialize_country_n_to_crops(
#'   country_totals = tibble::tribble(
#'     ~year, ~area_code, ~n_t,
#'     2010L, 10L, 100
#'   ),
#'   crop_shares = tibble::tribble(
#'     ~year, ~area_code, ~item_cbs_code, ~area_share,
#'     2010L, 10L, 2511L, 0.7,
#'     2010L, 10L, 2513L, 0.3
#'   ),
#'   cell_polity = NULL,
#'   resolution = "polity_crop"
#' )
spatialize_country_n_to_crops <- function(
  country_totals,
  crop_shares,
  cell_polity,
  resolution = c("polity_crop", "grid"),
  data = list()
) {
  resolution <- rlang::arg_match(resolution)
  .n_check_totals_shares(country_totals, crop_shares)
  polity_crop <- .n_polity_crop_totals(country_totals, crop_shares)
  if (resolution == "polity_crop") {
    return(polity_crop)
  }
  .check_columns(
    cell_polity,
    c("lon", "lat", "area_code", "polity_frac", "cell_area_ha"),
    "cell_polity"
  )
  .n_grid_totals(polity_crop, cell_polity, data)
}

# ---- Private helpers --------------------------------------------------

# Validate the two required inputs' columns.
.n_check_totals_shares <- function(country_totals, crop_shares) {
  .check_columns(
    country_totals,
    c("year", "area_code", "n_t"),
    "country_totals"
  )
  .check_columns(
    crop_shares,
    c("year", "area_code", "item_cbs_code", "area_share"),
    "crop_shares"
  )
}

# Resolve the cell-polity fraction parquet path from the argument, else the
# env var.
.resolve_polity_fraction_path <- function(polity_fraction_path) {
  resolved <- polity_fraction_path %||%
    Sys.getenv("WHEP_POLITY_FRACTION_PATH")
  if (!.has_path(resolved)) {
    cli::cli_abort(c(
      "No cell-polity fraction parquet available.",
      i = "Pass {.arg polity_fraction_path} or set
           {.envvar WHEP_POLITY_FRACTION_PATH}."
    ))
  }
  resolved
}

# Each crop's share of national harvested cropland area per year (grassland
# excluded). This is the promoted, shared version of
# crop_soil_n2o_extension.R's private .crop_area_shares(), which now
# call-throughs to this function.
.n_crop_area_shares <- function(primary_prod) {
  grass <- c(3000L, 3002L, 3003L)
  primary_prod |>
    dplyr::filter(
      .data$unit == "ha",
      !is.na(.data$item_cbs_code),
      !.data$item_cbs_code %in% grass,
      .data$value > 0
    ) |>
    dplyr::summarise(
      area_ha = sum(.data$value),
      .by = c(year, area_code, item_cbs_code)
    ) |>
    dplyr::mutate(
      area_share = .data$area_ha / sum(.data$area_ha),
      .by = c(year, area_code)
    ) |>
    dplyr::select(year, area_code, item_cbs_code, area_share)
}

# Polity-total N (one fert_type) x crop-area-share -> polity x crop N.
.n_polity_crop_totals <- function(country_totals, crop_shares) {
  crop_shares |>
    dplyr::inner_join(country_totals, by = c("year", "area_code")) |>
    dplyr::transmute(
      year,
      area_code,
      item_cbs_code,
      n_t = .data$n_t * .data$area_share
    )
}

# Distribute each polity-crop N total across grid cells in proportion to the
# cell's share of that polity-crop's crop-pattern hectares.
.n_grid_totals <- function(polity_crop, cell_polity, data) {
  years <- unique(polity_crop$year)
  item_prod_codes <- .n_item_prod_codes(unique(polity_crop$item_cbs_code))
  pattern_ha <- .n_crop_pattern_ha(data, years, item_prod_codes$item_prod_code)
  weights <- .n_cell_weights(pattern_ha, cell_polity, item_prod_codes)
  polity_crop |>
    dplyr::inner_join(
      weights,
      by = c("year", "area_code", "item_cbs_code"),
      relationship = "many-to-many"
    ) |>
    dplyr::transmute(
      lon,
      lat,
      area_code,
      year,
      item_cbs_code,
      n_t = .data$n_t * .data$cell_share
    )
}

# item_cbs_code -> item_prod_code lookup (the same crosswalk used by
# build_crop_land_extension()), restricted to the requested codes.
.n_item_prod_codes <- function(item_cbs_codes) {
  whep::items_prod_full |>
    dplyr::transmute(
      item_prod_code = .as_integer_quiet(.data$item_prod_code),
      item_cbs_code = .as_integer_quiet(.data$item_cbs_code)
    ) |>
    dplyr::filter(
      .data$item_cbs_code %in% item_cbs_codes,
      !is.na(.data$item_prod_code)
    ) |>
    dplyr::distinct(.data$item_cbs_code, .data$item_prod_code)
}

# Per-cell per-crop hectares = type_ha (summed over luh2_type) * harvest_
# fraction, the exact formula used by make_lpjml_covariate()'s
# "crop_pattern" branch (R/lpjml_covariate.R), reused verbatim.
.n_crop_pattern_ha <- function(data, years, item_prod_codes) {
  type_cropland <- .n_read_type_cropland(data$type_cropland, years) |>
    dplyr::summarise(
      type_ha = sum(.data$type_ha, na.rm = TRUE),
      .by = c(lon, lat, year)
    )
  crop_patterns <- .n_read_crop_patterns(data$crop_patterns, item_prod_codes)
  dplyr::inner_join(type_cropland, crop_patterns, by = c("lon", "lat")) |>
    dplyr::mutate(
      crop_pattern_ha = .data$type_ha * .data$harvest_fraction
    ) |>
    dplyr::select(lon, lat, year, item_prod_code, crop_pattern_ha)
}

# Read type_cropland.parquet, filtering to the requested years immediately
# after reading (before any join), so a single-year query stays fast against
# the ~27.5M-row real file.
.n_read_type_cropland <- function(type_cropland, years) {
  raw <- type_cropland %||% .n_read_parquet_env("WHEP_TYPE_CROPLAND_PATH")
  .check_columns(
    raw,
    c("lon", "lat", "year", "luh2_type", "type_ha"),
    "type_cropland"
  )
  dplyr::filter(tibble::as_tibble(raw), .data$year %in% years)
}

# Read crop_patterns.parquet, filtering to the requested item_prod_codes
# immediately after reading (before any join), so a single-crop query stays
# fast against the ~2.2M-row real file.
.n_read_crop_patterns <- function(crop_patterns, item_prod_codes) {
  raw <- crop_patterns %||% .n_read_parquet_env("WHEP_CROP_PATTERNS_PATH")
  .check_columns(
    raw,
    c("lon", "lat", "item_prod_code", "harvest_fraction"),
    "crop_patterns"
  )
  dplyr::filter(
    tibble::as_tibble(raw),
    .data$item_prod_code %in% item_prod_codes
  )
}

# Read a parquet path from an env var, aborting with the env var name if
# unset (never hardcode the real local path).
.n_read_parquet_env <- function(env_var) {
  path <- Sys.getenv(env_var)
  if (!.has_path(path)) {
    cli::cli_abort(c(
      "No {env_var} input available.",
      i = "Pass it via {.arg data}, or set {.envvar {env_var}}."
    ))
  }
  nanoparquet::read_parquet(path) |> tibble::as_tibble()
}

# Each cell's share of its polity-crop's total crop_pattern_ha, joined to
# the cell-polity crosswalk (area_code) and the item_cbs_code<->item_prod_code
# lookup.
.n_cell_weights <- function(pattern_ha, cell_polity, item_prod_codes) {
  pattern_ha |>
    dplyr::inner_join(item_prod_codes, by = "item_prod_code") |>
    dplyr::inner_join(
      dplyr::select(cell_polity, lon, lat, area_code, polity_frac),
      by = c("lon", "lat"),
      relationship = "many-to-many"
    ) |>
    dplyr::mutate(weighted_ha = .data$crop_pattern_ha * .data$polity_frac) |>
    dplyr::mutate(
      group_ha = sum(.data$weighted_ha),
      .by = c(year, area_code, item_cbs_code)
    ) |>
    dplyr::mutate(
      cell_share = dplyr::if_else(
        .data$group_ha > 0,
        .data$weighted_ha / .data$group_ha,
        0
      )
    ) |>
    dplyr::select(lon, lat, area_code, year, item_cbs_code, cell_share)
}
