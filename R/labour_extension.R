#' Build the labour-footprint extension.
#'
#' @description
#' Aggregate Global Labour Database working-hour intensities into a footprint
#' extension keyed by `(year, area_code, item_cbs_code)`, expressed in labour
#' hours. This is the (socio-economic) labour analogue of
#' [build_livestock_ghg_extension()] and [build_crop_soil_n2o_extension()], and
#' feeds [build_footprint()] / [compute_footprint()] the same way.
#'
#' Coefficient source: the Global Labour Database (Juan Infante-Amate and
#' colleagues) provides agricultural working hours per crop and country,
#' disaggregated into a total and several social tiers (extreme-poverty,
#' lower- and upper-middle-income-poverty, not-covered, child and forced
#' labour), alongside the harvested area those hours worked. The per-hectare
#' intensity (hours per harvested hectare) is the ratio of the two.
#'
#' The footprint engine traces one impact column at a time, so the labour tier
#' is selected with `component` (recorded in `method_labour`) rather than
#' returned as several columns:
#' - `"total"` (default): all agricultural working hours.
#' - `"extreme_poverty"`, `"lmic_poverty"`, `"umic_poverty"`: hours worked by
#'   people living below the extreme-poverty line and the lower- and
#'   upper-middle-income-country poverty lines respectively.
#' - `"not_covered"`: hours not covered by the poverty estimates.
#' - `"child"`: hours worked as child labour.
#' - `"forced"`: hours worked as forced labour.
#'
#' Hour intensities (hours per hectare) are joined to whep's own crop harvested
#' area (the `"ha"` rows of [get_primary_production()]) by FAO crop code and
#' country-year, so the footprint tracks whep production rather than the
#' source's, then summed to the commodity-balance item grain. This extension is
#' crop-only: the Global Labour Database covers crop agriculture, so livestock
#' and grazing sectors contribute no labour. Crops, countries or years absent
#' from the coefficient table contribute no hours.
#'
#' @param component Labour tier, one of `"total"` (default),
#'   `"extreme_poverty"`, `"lmic_poverty"`, `"umic_poverty"`, `"not_covered"`,
#'   `"child"` or `"forced"`.
#' @param data Optional named list of pre-loaded inputs to avoid remote reads:
#'   `primary_prod` (the [get_primary_production()] output) and `labour_crop`
#'   (the `gld-labour-crop` pin). Each falls back to its reader when absent.
#' @param example If `TRUE`, return a small fixture instead of reading remote
#'   data. Defaults to `FALSE`.
#'
#' @return A tibble with columns `year`, `area_code`, `item_cbs_code`,
#'   `impact_u` (labour hours) and `method_labour` (the chosen tier, e.g.
#'   `"GLD_total"`).
#'
#' @export
#'
#' @examples
#' build_labour_extension(example = TRUE)
build_labour_extension <- function(
  component = c(
    "total",
    "extreme_poverty",
    "lmic_poverty",
    "umic_poverty",
    "not_covered",
    "child",
    "forced"
  ),
  data = list(),
  example = FALSE
) {
  component <- rlang::arg_match(component)
  if (isTRUE(example)) {
    return(.example_labour_extension())
  }

  primary_prod <- .resolve_labour_input(
    data,
    "primary_prod",
    get_primary_production
  )
  labour_crop <- .resolve_labour_input(
    data,
    "labour_crop",
    function() whep_read_file("gld-labour-crop")
  )
  crop <- .crop_labour_inputs(labour_crop, primary_prod, component)
  .labour_extension(crop, component)
}

# Use an injected input when present, otherwise call its reader thunk.
.resolve_labour_input <- function(data, name, reader) {
  if (is.null(data[[name]])) reader() else data[[name]]
}

# Crop labour (hours): the per-hectare intensity for the chosen tier joined to
# whep crop harvested area by FAO crop and country-year, then summed to the CBS
# item grain.
.crop_labour_inputs <- function(labour_crop, primary_prod, component) {
  intensity <- .crop_labour_intensity(labour_crop, component)
  primary_prod |>
    dplyr::filter(.data$unit == "ha", !is.na(.data$item_cbs_code)) |>
    dplyr::summarise(
      area_ha = sum(.data$value, na.rm = TRUE),
      .by = c(year, area_code, item_prod_code, item_cbs_code)
    ) |>
    dplyr::inner_join(
      intensity,
      by = c("year", "area_code", "item_prod_code")
    ) |>
    dplyr::transmute(
      year,
      area_code,
      item_cbs_code,
      impact_u = .data$area_ha * .data$hours_per_ha
    )
}

# Per-hectare labour intensity (hours per harvested hectare) for the chosen
# tier: tier hours divided by the source harvested area. Keyed by the FAO crop
# code (crop_code) and country code (country_code) of the labour table.
.crop_labour_intensity <- function(labour_crop, component) {
  hours_col <- .labour_hours_column(component)
  labour_crop |>
    dplyr::transmute(
      year = as.integer(.data$year),
      area_code = as.integer(.data$country_code),
      item_prod_code = as.integer(.data$crop_code),
      hours_per_ha = as.numeric(.data[[hours_col]]) /
        as.numeric(.data$area_harvested_ha)
    ) |>
    dplyr::filter(is.finite(.data$hours_per_ha), .data$hours_per_ha >= 0)
}

# Sum crop labour to the IO grain, drop empties and label.
.labour_extension <- function(crop, component) {
  crop |>
    dplyr::summarise(
      impact_u = sum(.data$impact_u, na.rm = TRUE),
      .by = c(year, area_code, item_cbs_code)
    ) |>
    dplyr::filter(.data$impact_u > 0) |>
    dplyr::mutate(
      year = as.integer(.data$year),
      area_code = as.integer(.data$area_code),
      item_cbs_code = as.integer(.data$item_cbs_code),
      method_labour = .labour_method_label(component)
    ) |>
    dplyr::select(year, area_code, item_cbs_code, impact_u, method_labour)
}

# Map a labour tier to its hours column in the gld-labour-crop table.
.labour_hours_column <- function(component) {
  c(
    total = "hours_total",
    extreme_poverty = "hours_extreme_poverty",
    lmic_poverty = "hours_lmic_poverty",
    umic_poverty = "hours_umic_poverty",
    not_covered = "hours_not_covered",
    child = "hours_child",
    forced = "hours_forced"
  )[[component]]
}

.labour_method_label <- function(component) {
  paste0("GLD_", component)
}
