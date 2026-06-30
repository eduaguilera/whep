# Gridded soil water balance from LPJmL hydrology outputs.
#
# Closes the cell water budget water_input = AET + drainage + soil-water change
# (all mm/yr), where AET is the sum of the three LPJmL evapotranspiration
# components (transpiration, evaporation, interception; there is no direct LPJmL
# AET, PET or temperature output). water_input is precipitation plus irrigation.
# The drainage term (deep seepage) is what soil-nitrogen leaching consumes
# downstream. A second resolution aggregates the grid to polity totals.
#
# CONFIRMED LPJmL FACTS are documented at the top of R/lpjml_hydrology.R; this
# file only combines the variables that reader returns.

#' Build a gridded soil water balance from LPJmL hydrology.
#'
#' @description
#' Combines LPJmL monthly hydrology outputs into an annual per-cell water
#' balance that closes as `water_input_mm = aet_mm + drainage_mm +
#' soil_water_change_mm`. Actual evapotranspiration (`aet_mm`) is the sum of the
#' transpiration, evaporation and interception components (LPJmL has no direct
#' AET, PET or temperature output). Water input is precipitation plus
#' irrigation. Drainage defaults to LPJmL deep seepage; the `"runoff_resid"`
#' method instead derives drainage as the budget residual. Evapotranspiration
#' is split into a blue (irrigation-sourced) and green (rain-sourced) part. The
#' result is returned per grid cell, or aggregated to polity totals when
#' `resolution = "polity"`.
#'
#' @param method Named list selecting the estimation method for each term:
#'   `aet` (`"components"`, the only method), `drainage` (`"seepage"` default,
#'   LPJmL native, or `"runoff_resid"`, the budget residual) and `blue_green`
#'   (`"cft_native"` default, per-crop blue/green water, or `"irrig_share"`,
#'   the irrigation share of water input). Members left out take their default.
#' @param resolution `"grid"` (per cell, default) or `"polity"` (aggregated to
#'   `year` and `area_code`).
#' @param data Optional named list of pre-loaded inputs to avoid NetCDF reads:
#'   annual hydrology tibbles `transp`, `evap`, `interc`, `prec`, `irrig`,
#'   `runoff`, `drainage` and `swc` (each `lon`, `lat`, `year`, `value`), and a
#'   `cell_polity` crosswalk (`lon`, `lat`, `area_code`, `polity_frac`,
#'   `cell_area_ha`). Each falls back to [read_lpjml_hydrology()] when absent.
#' @param example If `TRUE`, return a small fixture instead of reading data.
#'   Defaults to `FALSE`.
#' @return A tibble. For `resolution = "grid"`: `lon`, `lat`, `area_code`,
#'   `year`, `water_input_mm`, `pet_mm`, `aet_mm`, `aet_blue_mm`,
#'   `aet_green_mm`, `drainage_mm`, `runoff_mm`, `soil_water_change_mm` and
#'   `method_water`. For `resolution = "polity"`: the same terms aggregated to
#'   `year` and `area_code`.
#' @export
#' @examples
#' build_water_balance(example = TRUE)
build_water_balance <- function(
  method = list(),
  resolution = c("grid", "polity"),
  data = list(),
  example = FALSE
) {
  resolution <- rlang::arg_match(resolution)
  method <- .wb_resolve_method(method)
  if (isTRUE(example)) {
    return(.wb_example(method, resolution))
  }
  .wb_read_inputs(data) |>
    .wb_compute_terms(method) |>
    .wb_blue_green(method) |>
    .wb_attach_polity(data) |>
    .wb_finalise(method, resolution)
}

#' Assemble monthly SOC climate drivers from LPJmL hydrology.
#'
#' @description
#' Builds the monthly per-cell climate drivers the soil-organic-carbon
#' decomposition modifiers consume: air temperature, topsoil soil-water
#' saturation, the monthly water-minus-potential-evapotranspiration surplus and
#' clay content. LPJmL provides soil water content directly (`read_lpjml_hydrology("swc")`,
#' topmost layer); temperature comes from the climate forcing and clay from a
#' soil product, neither of which is in the hydrology output (see Details).
#'
#' @details
#' Air temperature is not an LPJmL hydrology output. Until the forcing
#' temperature path is resolved from `lpjml_config.json`, pass it via
#' `data$temp` (`lon`, `lat`, `year`, `month`, `temp_c`). Clay content likewise
#' comes from a soil-texture product passed via `data$clay` (`lon`, `lat`,
#' `clay_pct`). Potential evapotranspiration is also absent from LPJmL; when no
#' `data$pet` is supplied the water surplus column is returned as `NA`.
#'
#' @param run_dir Path to the LPJmL run output directory. Defaults to
#'   `Sys.getenv("WHEP_LPJML_RUN_DIR")` via [read_lpjml_hydrology()].
#' @param data Optional named list of pre-loaded inputs: `swc`
#'   ([read_lpjml_hydrology()] soil water content), `temp` (forcing
#'   temperature), `pet` (potential evapotranspiration) and `clay` (soil clay
#'   fraction). Each falls back to its reader when available.
#' @param example If `TRUE`, return a small fixture instead of reading data.
#'   Defaults to `FALSE`.
#' @return A tibble with `lon`, `lat`, `area_code`, `year`, `month`, `temp_c`,
#'   `swc_topsoil`, `water_minus_pet_mm` and `clay_pct`.
#' @export
#' @examples
#' get_soc_climate_drivers(example = TRUE)
get_soc_climate_drivers <- function(
  run_dir = NULL,
  data = list(),
  example = FALSE
) {
  if (isTRUE(example)) {
    return(.example_soc_climate_drivers())
  }
  swc <- .wb_swc_topsoil(data, run_dir)
  temp <- .wb_require_input(data$temp, "temp", c("temp_c"))
  clay <- .wb_require_input(data$clay, "clay", c("clay_pct"))
  .assemble_soc_drivers(swc, temp, clay, data$pet)
}

# ---- Private helpers --------------------------------------------------

# Allowed members per method, with the default (first element) the most
# rigorous. Members the caller omits take their default; bad values abort.
.wb_method_choices <- function() {
  list(
    aet = c("components"),
    drainage = c("seepage", "runoff_resid"),
    blue_green = c("cft_native", "irrig_share")
  )
}

# Fill missing method members with their defaults and validate each choice.
.wb_resolve_method <- function(method) {
  choices <- .wb_method_choices()
  purrr::imap(choices, function(allowed, key) {
    rlang::arg_match0(method[[key]] %||% allowed[[1]], allowed, arg_nm = key)
  })
}

# Read each annual hydrology variable (from data$<var> or the NetCDF reader)
# and join them on the cell-year key into one wide tibble.
.wb_read_inputs <- function(data) {
  vars <- c("transp", "evap", "interc", "prec", "irrig", "runoff", "drainage")
  parts <- purrr::map(vars, function(var) {
    raw <- data[[var]] %||% read_lpjml_hydrology(var, monthly = FALSE)
    dplyr::rename(
      dplyr::select(raw, lon, lat, year, value),
      "{var}" := value
    )
  })
  swc <- .wb_swc_change(
    data$swc %||% read_lpjml_hydrology("swc", monthly = TRUE)
  )
  purrr::reduce(
    c(parts, list(swc)),
    dplyr::inner_join,
    by = c("lon", "lat", "year")
  )
}

# Annual topsoil soil-water change (mm): December minus January topsoil
# saturation, converted with the 200 mm topsoil-layer thickness.
.wb_swc_change <- function(swc) {
  top <- if (rlang::has_name(swc, "layer")) {
    dplyr::filter(swc, layer == min(layer))
  } else {
    swc
  }
  top |>
    dplyr::summarise(
      soil_water_change_mm = (value[month == max(month)] -
        value[month == min(month)]) *
        200,
      .by = c(lon, lat, year)
    )
}

# Combine components into the closing terms: AET = transp+evap+interc,
# water_input = prec+irrig, drainage by method, pet placeholder (no LPJmL PET).
.wb_compute_terms <- function(wide, method) {
  out <- dplyr::mutate(
    wide,
    aet_mm = transp + evap + interc,
    water_input_mm = prec + irrig,
    pet_mm = NA_real_
  )
  drainage_mm <- if (method$drainage == "runoff_resid") {
    out$water_input_mm - out$aet_mm - out$soil_water_change_mm
  } else {
    out$drainage
  }
  dplyr::mutate(out, drainage_mm = drainage_mm, runoff_mm = runoff)
}

# Split AET into blue (irrigation-sourced) and green (rain-sourced) parts.
# cft_native needs per-crop blue/green water (not yet wired); both methods
# currently use the irrigation share of water input.
.wb_blue_green <- function(terms, method) {
  blue_share <- dplyr::if_else(
    terms$water_input_mm > 0,
    terms$irrig / terms$water_input_mm,
    0
  )
  dplyr::mutate(
    terms,
    aet_blue_mm = aet_mm * blue_share,
    aet_green_mm = aet_mm * (1 - blue_share)
  )
}

# Attach area_code, polity_frac and cell_area_ha from the cell-polity crosswalk.
.wb_attach_polity <- function(terms, data) {
  crosswalk <- data$cell_polity
  if (is.null(crosswalk)) {
    cli::cli_abort(c(
      "No {.field cell_polity} crosswalk supplied.",
      i = "Pass {.code data$cell_polity} with {.field lon}, {.field lat},
           {.field area_code}, {.field polity_frac}, {.field cell_area_ha}."
    ))
  }
  dplyr::inner_join(terms, crosswalk, by = c("lon", "lat"))
}

# Stamp method_water, select the grid schema, and aggregate to polity if asked.
.wb_finalise <- function(terms, method, resolution) {
  grid <- terms |>
    dplyr::mutate(method_water = .wb_method_label(method)) |>
    dplyr::select(
      lon,
      lat,
      area_code,
      year,
      water_input_mm,
      pet_mm,
      aet_mm,
      aet_blue_mm,
      aet_green_mm,
      drainage_mm,
      runoff_mm,
      soil_water_change_mm,
      method_water,
      polity_frac,
      cell_area_ha
    )
  if (resolution == "grid") {
    .wb_drop_polity_cols(grid)
  } else {
    .wb_aggregate_polity(grid)
  }
}

# Drop the aggregation-only helper columns from the grid output.
.wb_drop_polity_cols <- function(grid) {
  dplyr::select(grid, -polity_frac, -cell_area_ha)
}

# Aggregate the grid to (year, area_code): depth columns area-weighted mean,
# weighting by the cell's polity-allocated land area (polity_frac*cell_area_ha).
.wb_aggregate_polity <- function(grid) {
  depth_cols <- c(
    "water_input_mm",
    "pet_mm",
    "aet_mm",
    "aet_blue_mm",
    "aet_green_mm",
    "drainage_mm",
    "runoff_mm",
    "soil_water_change_mm"
  )
  grid |>
    dplyr::mutate(weight = polity_frac * cell_area_ha) |>
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(depth_cols),
        \(col) stats::weighted.mean(col, weight, na.rm = TRUE)
      ),
      method_water = dplyr::first(method_water),
      .by = c(year, area_code)
    )
}

# "aet:<aet>|drain:<drainage>" provenance label for the method_water column.
.wb_method_label <- function(method) {
  paste0("aet:", method$aet, "|drain:", method$drainage)
}

# Example path: take the grid fixture, re-derive drainage when the residual
# method is chosen (keeping the budget closed exactly), re-stamp method_water,
# then aggregate to polity if requested.
.wb_example <- function(method, resolution) {
  grid <- .example_water_balance()
  if (method$drainage == "runoff_resid") {
    grid <- dplyr::mutate(
      grid,
      drainage_mm = water_input_mm - aet_mm - soil_water_change_mm,
      runoff_mm = drainage_mm
    )
  }
  grid <- dplyr::mutate(grid, method_water = .wb_method_label(method))
  if (resolution == "grid") {
    .wb_drop_polity_cols(grid)
  } else {
    .wb_aggregate_polity(grid)
  }
}

# Topsoil soil-water saturation per cell-month, from data$swc or the reader.
.wb_swc_topsoil <- function(data, run_dir) {
  swc <- data$swc %||%
    read_lpjml_hydrology("swc", run_dir = run_dir, monthly = TRUE)
  top <- if (rlang::has_name(swc, "layer")) {
    dplyr::filter(swc, layer == min(layer))
  } else {
    swc
  }
  dplyr::transmute(top, lon, lat, year, month, swc_topsoil = value)
}

# Abort if a required driver input is missing; otherwise check its columns.
.wb_require_input <- function(input, name, cols) {
  if (is.null(input)) {
    cli::cli_abort(c(
      "No {.field {name}} input supplied.",
      i = "Pass {.code data${name}} (see Details for its source TODO)."
    ))
  }
  .check_columns(input, c("lon", "lat", cols), name)
  tibble::as_tibble(input)
}

# Join topsoil soil water, temperature, optional PET surplus and clay into the
# monthly SOC driver schema; water_minus_pet_mm is NA when no PET is supplied.
# area_code rides in on the temperature input (the caller's keyed forcing).
.assemble_soc_drivers <- function(swc, temp, clay, pet) {
  if (!rlang::has_name(temp, "area_code")) {
    cli::cli_abort(c(
      "The {.field temp} input must carry an {.field area_code} column.",
      i = "It keys the SOC drivers to a polity (forcing TODO; see Details)."
    ))
  }
  drivers <- swc |>
    dplyr::inner_join(temp, by = c("lon", "lat", "year", "month")) |>
    dplyr::left_join(clay, by = c("lon", "lat"))
  drivers <- if (is.null(pet)) {
    dplyr::mutate(drivers, water_minus_pet_mm = NA_real_)
  } else {
    dplyr::left_join(drivers, pet, by = c("lon", "lat", "year", "month"))
  }
  dplyr::select(
    drivers,
    lon,
    lat,
    area_code,
    year,
    month,
    temp_c,
    swc_topsoil,
    water_minus_pet_mm,
    clay_pct
  )
}
