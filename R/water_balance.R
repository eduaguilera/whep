# Gridded soil water balance from LPJmL hydrology outputs.
#
# Closes the cell water budget, in mm/yr:
#   water input equals AET plus runoff plus drainage plus soil-water change,
# where AET is the sum of the three LPJmL evapotranspiration components
# (transpiration, evaporation, interception; there is no direct LPJmL AET, PET
# or temperature output) and water_input is precipitation plus irrigation.
# drainage_mm is LPJmL deep seepage (mseepage.nc): the deep-drainage term that
# soil-nitrogen leaching consumes downstream. runoff_mm is LPJmL total runoff
# (mrunoff.nc, surface + lateral): it leaves the cell laterally and is NOT a
# leaching term, but it must appear in the budget for it to close. A second
# resolution aggregates the grid to polity totals.
#
# CONFIRMED LPJmL FACTS are documented at the top of R/lpjml_hydrology.R; this
# file only combines the variables that reader returns.

#' Build a gridded soil water balance from LPJmL hydrology.
#'
#' @description
#' Combines LPJmL monthly hydrology outputs into an annual per-cell water
#' balance that closes as `water_input_mm = aet_mm + runoff_mm + drainage_mm +
#' soil_water_change_mm`. Actual evapotranspiration (`aet_mm`) is the sum of the
#' transpiration, evaporation and interception components (LPJmL has no direct
#' AET, PET or temperature output). Water input is precipitation plus
#' irrigation. `drainage_mm` is LPJmL deep seepage, the leaching driver
#' downstream; `runoff_mm` is LPJmL total runoff (surface plus lateral), which
#' leaves the cell laterally and is not a leaching term but must appear in the
#' budget. Drainage defaults to native seepage; the `"residual"` method instead
#' reconstructs seepage as `water_input - aet - runoff - soil_water_change`, for
#' use only when the seepage file is absent (it equals seepage when the balance
#' closes). Evapotranspiration is split into a blue (irrigation-sourced) and
#' green (rain-sourced) part. The result is returned per grid cell, or
#' aggregated to polity totals when `resolution = "polity"`.
#'
#' @param method Named list selecting the estimation method for each term:
#'   `aet` (`"components"`, the only method), `drainage` (`"seepage"` default,
#'   LPJmL native seepage, or `"residual"`, a seepage reconstruction from the
#'   budget residual usable only when the seepage file is absent) and
#'   `blue_green` (`"cft_native"` default, per-crop blue/green consumptive
#'   water, or `"irrig_share"`, the irrigation share of water input). Members
#'   left out take their default.
#' @param resolution `"grid"` (per cell, default) or `"polity"` (aggregated to
#'   `year` and `area_code`).
#' @param data Optional named list of pre-loaded inputs to avoid NetCDF reads:
#'   hydrology tibbles `transp`, `evap`, `interc`, `prec`, `irrig`, `runoff`
#'   and `seepage` (each `lon`, `lat`, `year`, `value`; annual-summed
#'   automatically when a `month` column is present), `swc` (`lon`, `lat`,
#'   `year`, `month`, `layer`, `value` fractional saturation), optional
#'   per-crop consumptive water `cft_consump_water_b` and `cft_consump_water_g`
#'   (each `lon`, `lat`, `year`, `value` mm/yr) and a `cell_polity` crosswalk
#'   (`lon`, `lat`, `area_code`, `polity_frac`, `cell_area_ha`). Each falls back
#'   to [read_lpjml_hydrology()] when absent.
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
    drainage = c("seepage", "residual"),
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

# Read each hydrology flux (from data$<var> or the NetCDF reader), annual-sum
# it and join on the cell-year key into one wide tibble. The seepage term uses
# the reader's logical "drainage" var (mseepage.nc) but the data override key is
# `seepage`. The soil-water-change term is appended from the layered swc.
.wb_read_inputs <- function(data) {
  # name -> reader logical var; data overrides use the name (e.g. data$seepage).
  flux_vars <- c(
    transp = "transp",
    evap = "evap",
    interc = "interc",
    prec = "prec",
    irrig = "irrig",
    runoff = "runoff",
    seepage = "drainage"
  )
  parts <- purrr::imap(flux_vars, function(reader_var, name) {
    raw <- data[[name]] %||% read_lpjml_hydrology(reader_var, monthly = FALSE)
    .wb_annual_flux(raw, name)
  })
  swc <- .wb_swc_change(
    data$swc %||% read_lpjml_hydrology("swc", monthly = TRUE)
  )
  wide <- purrr::reduce(
    c(unname(parts), list(swc)),
    dplyr::inner_join,
    by = c("lon", "lat", "year")
  )
  .wb_attach_cft_consump(wide, data)
}

# Attach cell-level blue/green consumptive water (mm/yr) from the per-CFT
# `cft_consump_water_b` / `cft_consump_water_g` inputs, summing the crop-band
# values per cell-year. Columns are NA when the per-CFT inputs are not supplied,
# which makes the cft_native blue/green method fall back (see .wb_blue_green()).
.wb_attach_cft_consump <- function(wide, data) {
  blue <- .wb_cell_consump(data$cft_consump_water_b, "consump_blue_mm")
  green <- .wb_cell_consump(data$cft_consump_water_g, "consump_green_mm")
  wide <- if (is.null(blue)) {
    dplyr::mutate(wide, consump_blue_mm = NA_real_)
  } else {
    dplyr::left_join(wide, blue, by = c("lon", "lat", "year"))
  }
  if (is.null(green)) {
    dplyr::mutate(wide, consump_green_mm = NA_real_)
  } else {
    dplyr::left_join(wide, green, by = c("lon", "lat", "year"))
  }
}

# Sum a per-CFT consumptive-water input over its crop bands to a cell-year
# total named `out_col`. Returns NULL when the input is absent.
.wb_cell_consump <- function(raw, out_col) {
  if (is.null(raw)) {
    return(NULL)
  }
  raw |>
    dplyr::summarise(
      "{out_col}" := sum(value),
      .by = c(lon, lat, year)
    )
}

# Coerce one flux input to annual cell-year totals named `name`. Monthly inputs
# (a `month` column present) are summed over the 12 months; already-annual
# inputs are passed through after renaming `value`.
.wb_annual_flux <- function(raw, name) {
  if (rlang::has_name(raw, "month")) {
    raw <- dplyr::summarise(
      raw,
      value = sum(value),
      .by = c(lon, lat, year)
    )
  }
  dplyr::rename(
    dplyr::select(raw, lon, lat, year, value),
    "{name}" := value
  )
}

# Annual whole-profile soil-water change (mm): for each cell-year, the
# December-minus-prior-December change in column storage, falling back to
# December minus January in the first available year. Column storage sums all
# soil layers as fractional saturation times layer thickness times a porosity
# (water-holding) factor; thickness alone would imply porosity = 1.
.wb_swc_change <- function(swc) {
  state <- .wb_swc_column_state(swc)
  state |>
    dplyr::arrange(year) |>
    dplyr::mutate(
      soil_water_change_mm = storage_dec_mm -
        dplyr::coalesce(dplyr::lag(storage_dec_mm), storage_jan_mm),
      .by = c(lon, lat)
    ) |>
    dplyr::select(lon, lat, year, soil_water_change_mm)
}

# Per cell-year December and January column-storage states (mm), summing all
# layers as saturation * thickness * porosity. Layer thicknesses are the
# differences of the LPJmL layer-depth boundaries (mm).
.wb_swc_column_state <- function(swc) {
  thickness <- .wb_layer_thickness_mm(swc)
  porosity <- .wb_soil_porosity_factor()
  swc |>
    dplyr::mutate(
      depth_mm = value * thickness[layer] * porosity
    ) |>
    dplyr::summarise(
      storage_jan_mm = sum(depth_mm[month == min(month)]),
      storage_dec_mm = sum(depth_mm[month == max(month)]),
      .by = c(lon, lat, year)
    )
}

# LPJmL soil-layer thicknesses (mm) from the cumulative layer-depth boundaries
# 200/500/1000/2000/3000/13000 mm: 200, 300, 500, 1000, 1000, 10000.
.wb_layer_thickness_mm <- function(swc) {
  boundaries <- c(200, 500, 1000, 2000, 3000, 13000)
  n_layer <- max(swc$layer)
  diff(c(0, boundaries))[seq_len(n_layer)]
}

# Representative soil water-holding (porosity) factor converting fractional
# saturation to a water depth. 0.4 matches LPJmL swc_vol-derived layer depths
# for this run (topsoil 0.48 saturation * 200 mm * 0.4 ~ 38 mm vs ~36 mm from
# mswc_vol.nc); using thickness alone would imply porosity = 1.
.wb_soil_porosity_factor <- function() {
  0.4
}

# Combine components into the closing terms: AET sums transp, evap and interc;
# water input sums prec and irrig; runoff carried through; drainage by method;
# pet placeholder (no LPJmL PET). The 4-term identity is water input equals aet
# plus runoff plus drainage plus soil-water change.
.wb_compute_terms <- function(wide, method) {
  out <- dplyr::mutate(
    wide,
    aet_mm = transp + evap + interc,
    water_input_mm = prec + irrig,
    runoff_mm = runoff,
    pet_mm = NA_real_
  )
  drainage_mm <- if (method$drainage == "residual") {
    out$water_input_mm - out$aet_mm - out$runoff_mm - out$soil_water_change_mm
  } else {
    out$seepage
  }
  dplyr::mutate(out, drainage_mm = drainage_mm)
}

# Split AET into blue (irrigation-sourced) and green (rain-sourced) parts and
# record the realized method in `.bg_method`. cft_native uses the per-crop
# consumptive-water split when supplied; with no per-CFT data it warns and
# falls back to the irrigation share (recorded as bg:irrig_share_fallback, not a
# silent degrade). irrig_share always uses the irrigation share of water input.
.wb_blue_green <- function(terms, method) {
  use_cft <- method$blue_green == "cft_native" &&
    .wb_has_cft_consump(terms)
  if (method$blue_green == "cft_native" && !use_cft) {
    cli::cli_warn(c(
      "{.code blue_green = \"cft_native\"} requires per-CFT consumptive water.",
      i = "Supply {.code data$cft_consump_water_b} and
           {.code data$cft_consump_water_g}; falling back to the irrigation
           share for the blue/green split."
    ))
  }
  if (use_cft) {
    .wb_blue_green_cft(terms)
  } else {
    .wb_blue_green_irrig_share(terms, method)
  }
}

# TRUE when both per-CFT consumptive-water columns are present and non-missing
# for every row (so the cft_native split can be computed).
.wb_has_cft_consump <- function(terms) {
  rlang::has_name(terms, "consump_blue_mm") &&
    rlang::has_name(terms, "consump_green_mm") &&
    !anyNA(terms$consump_blue_mm) &&
    !anyNA(terms$consump_green_mm)
}

# Blue/green split from the per-CFT consumptive-water totals: the blue share is
# blue / (blue + green), applied to total AET.
.wb_blue_green_cft <- function(terms) {
  total <- terms$consump_blue_mm + terms$consump_green_mm
  blue_share <- dplyr::if_else(total > 0, terms$consump_blue_mm / total, 0)
  dplyr::mutate(
    terms,
    aet_blue_mm = aet_mm * blue_share,
    aet_green_mm = aet_mm * (1 - blue_share),
    .bg_method = "cft_native"
  )
}

# Blue/green split from the irrigation share of water input. Labelled
# irrig_share when requested, or irrig_share_fallback when cft_native degraded.
.wb_blue_green_irrig_share <- function(terms, method) {
  blue_share <- dplyr::if_else(
    terms$water_input_mm > 0,
    terms$irrig / terms$water_input_mm,
    0
  )
  bg_method <- if (method$blue_green == "cft_native") {
    "irrig_share_fallback"
  } else {
    "irrig_share"
  }
  dplyr::mutate(
    terms,
    aet_blue_mm = aet_mm * blue_share,
    aet_green_mm = aet_mm * (1 - blue_share),
    .bg_method = bg_method
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

# Stamp method_water (including the realized blue_green method), select the grid
# schema, and aggregate to polity if asked.
.wb_finalise <- function(terms, method, resolution) {
  grid <- terms |>
    dplyr::mutate(
      method_water = .wb_method_label(method, dplyr::first(.bg_method))
    ) |>
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
        \(col) .wb_weighted_mean(col, weight)
      ),
      method_water = dplyr::first(method_water),
      .by = c(year, area_code)
    )
}

# Area-weighted mean that returns NA (never NaN) when every value is missing, so
# an all-NA column (e.g. the pet_mm placeholder) aggregates to NA, not NaN.
.wb_weighted_mean <- function(col, weight) {
  if (all(is.na(col))) {
    return(NA_real_)
  }
  stats::weighted.mean(col, weight, na.rm = TRUE)
}

# "aet:<aet>|drain:<drainage>|bg:<blue_green>" provenance label for the
# method_water column. `bg_realized` is the blue_green method actually used
# (cft_native, irrig_share, or irrig_share_fallback when cft_native degraded).
.wb_method_label <- function(method, bg_realized) {
  paste0(
    "aet:",
    method$aet,
    "|drain:",
    method$drainage,
    "|bg:",
    bg_realized
  )
}

# Example path: take the grid fixture, re-derive drainage when the residual
# method is chosen (keeping the 4-term budget closed exactly), re-stamp
# method_water, then aggregate to polity if requested. The fixture carries the
# cft_native blue/green split, so the realized bg method is cft_native.
.wb_example <- function(method, resolution) {
  grid <- .example_water_balance()
  if (method$drainage == "residual") {
    grid <- dplyr::mutate(
      grid,
      drainage_mm = water_input_mm -
        aet_mm -
        runoff_mm -
        soil_water_change_mm
    )
  }
  grid <- dplyr::mutate(
    grid,
    method_water = .wb_method_label(method, "cft_native")
  )
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
