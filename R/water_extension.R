#' Build the water-footprint extension.
#'
#' @description
#' Aggregate Water Footprint Network green/blue water intensities into a
#' footprint extension keyed by `(year, area_code, item_cbs_code)`, expressed in
#' cubic metres of water. This is the water analogue of
#' [build_livestock_ghg_extension()] and [build_crop_soil_n2o_extension()] and
#' feeds [build_footprint()] / [compute_footprint()] the same way. It ports
#' FABIO's water extension to whep's commodity-balance grain.
#'
#' Coefficient sources: crop intensities (cubic metres per tonne) come from
#' Mialyk et al. (2024), the 4TU dataset "Water footprints and crop water use of
#' 175 individual crops for 1990-2019" (doi:10.4121/7b45bcc6-686b-404d-a910-13c87156716a);
#' livestock blue water (drinking plus service water, cubic metres per head) is
#' derived from Chapagain & Hoekstra (2003), Value of Water Research Report
#' Series No. 13, Tables 3.8 and 3.9; grazing green water (cubic metres per
#' hectare) is the country-average green evapotranspiration of managed grassland
#' from the WHEP LPJmL run (2000-2009 mean) over the full grassland area (the
#' occupation basis of [build_grassland_land_extension()]), following the
#' approach of Schyns et al. (2019, doi:10.1073/pnas.1817380116).
#'
#' The footprint engine traces one impact column at a time, so the water type is
#' selected with `component` (recorded in `method_water`), rather than returned
#' as two columns:
#' - `"blue"` (default): irrigation water. **Crops** use the per-tonne blue
#'   intensity (irrigation `wfb_i_m3_t` plus capillary rise `wfb_cr_m3_t`) times
#'   whep crop production; **livestock** use a per-head blue-water coefficient
#'   (drinking, servicing and feed processing) times live-animal head counts.
#'   Blue water is the scarcity-relevant pressure most footprint studies
#'   emphasise, hence the default.
#' - `"green"`: rain water. **Crops** use the per-tonne green intensity
#'   (`wfg_m3_t`) times production; **grazing** uses a per-hectare green-water
#'   coefficient times grazed grassland area (LUH2 pasture and rangeland).
#'
#' Crop intensities (cubic metres per tonne) are joined to whep's own crop
#' production (the `"tonnes"` rows of [get_primary_production()]) by FAO crop
#' code and country-year, so the footprint tracks whep production rather than
#' the source's. Crops, countries or years absent from the coefficient table
#' contribute no water; the WFN crop table currently spans 1990-2019, so years
#' outside that range yield no crop water until extrapolation is added.
#'
#' @param component Water type, `"blue"` (default) or `"green"`.
#' @param data Optional named list of pre-loaded inputs to avoid remote reads:
#'   `primary_prod` (the [get_primary_production()] output), `crop_water` (the
#'   `wfn-water-crop` pin), `livestock_water` (the `water-livestock-blue` pin,
#'   used only for `"blue"`) and `grazing_water` (the `water-grazing-green` pin,
#'   used only for `"green"`). Each falls back to its reader when absent.
#' @param example If `TRUE`, return a small fixture instead of reading remote
#'   data. Defaults to `FALSE`.
#'
#' @return A tibble with columns `year`, `area_code`, `item_cbs_code`,
#'   `impact_u` (water use in cubic metres) and `method_water` (the chosen
#'   component, e.g. `"WFN_blue"`).
#'
#' @export
#'
#' @examples
#' build_water_extension(example = TRUE)
build_water_extension <- function(
  component = c("blue", "green"),
  data = list(),
  example = FALSE
) {
  component <- rlang::arg_match(component)
  if (isTRUE(example)) {
    return(.example_water_extension())
  }

  primary_prod <- .resolve_water_input(
    data,
    "primary_prod",
    get_primary_production
  )
  crop_water <- .resolve_water_input(
    data,
    "crop_water",
    function() whep_read_file("wfn-water-crop")
  )
  crop <- .crop_water_inputs(crop_water, primary_prod, component)
  other <- .non_crop_water(data, primary_prod, component)
  .water_extension(crop, other, component)
}

# Use an injected input when present, otherwise call its reader thunk.
.resolve_water_input <- function(data, name, reader) {
  if (is.null(data[[name]])) reader() else data[[name]]
}

# Blue -> livestock blue water; green -> grazing green water. Only the pin
# relevant to the chosen component is read.
.non_crop_water <- function(data, primary_prod, component) {
  if (component == "blue") {
    livestock_water <- .resolve_water_input(
      data,
      "livestock_water",
      function() whep_read_file("water-livestock-blue")
    )
    .livestock_blue_water(livestock_water, primary_prod)
  } else {
    grazing_water <- .resolve_water_input(
      data,
      "grazing_water",
      function() whep_read_file("water-grazing-green")
    )
    .grazing_green_water(grazing_water, primary_prod)
  }
}

# Crop water (cubic metres): WFN per-tonne intensity for the chosen component
# joined to whep crop production by FAO crop and country-year, then summed to
# the CBS item grain.
.crop_water_inputs <- function(crop_water, primary_prod, component) {
  intensity <- .crop_water_intensity(crop_water, component)
  primary_prod |>
    dplyr::filter(.data$unit == "tonnes", !is.na(.data$item_cbs_code)) |>
    dplyr::summarise(
      production_t = sum(.data$value, na.rm = TRUE),
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
      impact_u = .data$production_t * .data$water_m3_t
    )
}

# Per-tonne water intensity (cubic metres per tonne) for the chosen component.
# Blue sums irrigation and capillary rise; green is rainfed water. Keyed by the
# FAO crop code (crop_code) and country code (country_code) of the WFN table.
.crop_water_intensity <- function(crop_water, component) {
  cols <- if (component == "blue") {
    c("wfb_i_m3_t", "wfb_cr_m3_t")
  } else {
    "wfg_m3_t"
  }
  crop_water |>
    dplyr::transmute(
      year = as.integer(.data$year),
      area_code = as.integer(.data$country_code),
      item_prod_code = as.integer(.data$crop_code),
      water_m3_t = rowSums(
        dplyr::across(dplyr::all_of(cols), as.numeric),
        na.rm = TRUE
      )
    ) |>
    dplyr::filter(!is.na(.data$water_m3_t), .data$water_m3_t >= 0)
}

# Livestock blue water (cubic metres): per-head blue-water coefficient times
# live-animal head counts. Species absent from the coefficient table get none.
.livestock_blue_water <- function(livestock_water, primary_prod) {
  coef <- .livestock_water_coef(livestock_water)
  primary_prod |>
    dplyr::filter(.data$unit == "heads", !is.na(.data$item_cbs_code)) |>
    dplyr::summarise(
      heads = sum(.data$value, na.rm = TRUE),
      .by = c(year, area_code, item_cbs_code)
    ) |>
    dplyr::inner_join(coef, by = "item_cbs_code") |>
    dplyr::transmute(
      year,
      area_code,
      item_cbs_code,
      impact_u = .data$heads * .data$m3_per_head
    )
}

# Grazing green water (cubic metres): per-hectare green-water coefficient times
# grazed grassland area (LUH2 pasture 3000 and rangeland 3002, excluding
# rotational fallow which the crop fallow extension attributes to crops).
.grazing_green_water <- function(grazing_water, primary_prod) {
  coef <- .grazing_water_coef(grazing_water)
  grass <- c(3000L, 3002L)
  primary_prod |>
    dplyr::filter(.data$unit == "ha", .data$item_cbs_code %in% grass) |>
    dplyr::summarise(
      area_ha = sum(.data$value, na.rm = TRUE),
      .by = c(year, area_code, item_cbs_code)
    ) |>
    dplyr::inner_join(coef, by = "area_code") |>
    dplyr::transmute(
      year,
      area_code,
      item_cbs_code,
      impact_u = .data$area_ha * .data$m3_per_ha
    )
}

# Per-head livestock blue-water coefficient (cubic metres per head) keyed by the
# live-animal CBS item code.
.livestock_water_coef <- function(livestock_water) {
  livestock_water |>
    dplyr::transmute(
      item_cbs_code = as.integer(.data$item_cbs_code),
      m3_per_head = as.numeric(.data$m3_per_head)
    ) |>
    dplyr::filter(!is.na(.data$m3_per_head), .data$m3_per_head >= 0)
}

# Per-hectare grazing green-water coefficient (cubic metres per hectare) keyed
# by country.
.grazing_water_coef <- function(grazing_water) {
  grazing_water |>
    dplyr::transmute(
      area_code = as.integer(.data$area_code),
      m3_per_ha = as.numeric(.data$m3_per_ha)
    ) |>
    dplyr::filter(!is.na(.data$m3_per_ha), .data$m3_per_ha >= 0)
}

# Combine crop and non-crop water, sum to the IO grain, drop empties and label.
.water_extension <- function(crop, other, component) {
  dplyr::bind_rows(crop, other) |>
    dplyr::summarise(
      impact_u = sum(.data$impact_u, na.rm = TRUE),
      .by = c(year, area_code, item_cbs_code)
    ) |>
    dplyr::filter(.data$impact_u > 0) |>
    dplyr::mutate(
      year = as.integer(.data$year),
      area_code = as.integer(.data$area_code),
      item_cbs_code = as.integer(.data$item_cbs_code),
      method_water = .water_method_label(component)
    ) |>
    dplyr::select(year, area_code, item_cbs_code, impact_u, method_water)
}

.water_method_label <- function(component) {
  paste0("WFN_", component)
}
