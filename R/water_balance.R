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
#' The output also exposes the footprint-relevant terms folded into the budget:
#' `prec_mm` (precipitation) and `irrig_mm` (applied irrigation, the gross
#' blue-water volume), which satisfy `water_input_mm = prec_mm + irrig_mm`;
#' `blue_consump_mm` and `green_consump_mm`, the LPJmL-native consumptive blue
#' and green water (the per-CFT `cft_consump_water_b` / `cft_consump_water_g`
#' totals when supplied, otherwise the blue and green AET); and `cft_nir_mm`,
#' the net irrigation requirement (LPJmL `cft_nir`), the net blue-water demand,
#' summed to cell level when `data$cft_nir` is supplied and `NA` otherwise.
#' Potential evapotranspiration (`pet_mm`) comes from the CRU climate forcing
#' that drives the LPJmL run and is `NA` until that forcing is wired (see
#' `data$pet`); no PET formula is fabricated here.
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
#' @param polity_validity What to do with a row whose `(area_code, year)`
#'   resolves to a polity that did not exist in that year (the cell-polity
#'   crosswalk has no year dimension, so an early-20th-century cell is labelled
#'   with its present-day territory). `"keep"` (default) keeps every row, which
#'   is the historical behaviour, and warns naming the rows, years and area
#'   codes involved. `"flag"` keeps them and adds the per-row logical
#'   `reporting_polity_out_of_span`, marking exactly which rows are stand-ins.
#'   `"drop"` removes them. All three warn; only `"drop"` changes the numbers.
#'   See [polity_coverage_gaps()], which reports the same rows for an
#'   already-built table.
#' @param data Optional named list of pre-loaded inputs to avoid NetCDF reads:
#'   hydrology tibbles `transp`, `evap`, `interc`, `prec`, `irrig`, `runoff`
#'   and `seepage` (each `lon`, `lat`, `year`, `value`; annual-summed
#'   automatically when a `month` column is present), `swc` (`lon`, `lat`,
#'   `year`, `month`, `layer`, `value` fractional saturation), optional
#'   per-crop consumptive water `cft_consump_water_b` and `cft_consump_water_g`
#'   (each `lon`, `lat`, `year`, `value` mm/yr), an optional `cft_nir`
#'   net-irrigation-requirement input (`lon`, `lat`, `year`, `value` mm/yr,
#'   summed to cell level when supplied; exposed as `cft_nir_mm`, else `NA`)
#'   and a `cell_polity` crosswalk (`lon`, `lat`, `area_code`, `polity_frac`,
#'   `cell_area_ha`). Each falls back to [read_lpjml_hydrology()] when absent,
#'   except `cft_nir` (see Details), `pet` and the consumptive-water inputs.
#' @param example If `TRUE`, return a small fixture instead of reading data.
#'   Defaults to `FALSE`.
#' @return A tibble. For `resolution = "grid"`: `lon`, `lat`, `area_code`,
#'   `year`, `water_input_mm`, `prec_mm`, `irrig_mm`, `pet_mm`, `aet_mm`,
#'   `aet_blue_mm`, `aet_green_mm`, `blue_consump_mm`, `green_consump_mm`,
#'   `cft_nir_mm`, `drainage_mm`, `runoff_mm`, `soil_water_change_mm` and
#'   `method_water`. For `resolution = "polity"`: the same terms aggregated to
#'   `year` and `area_code`. Both resolutions carry the polity columns below,
#'   plus `reporting_polity_out_of_span` when `polity_validity = "flag"`.
#' @inheritSection whep_polity_columns Polity columns
#' @export
#' @examples
#' build_water_balance(example = TRUE)
build_water_balance <- function(
  method = list(),
  resolution = c("grid", "polity"),
  polity_validity = c("keep", "flag", "drop"),
  data = list(),
  example = FALSE
) {
  resolution <- rlang::arg_match(resolution)
  polity_validity <- rlang::arg_match(polity_validity)
  method <- .wb_resolve_method(method)
  if (isTRUE(example)) {
    return(.wb_example(method, resolution, polity_validity))
  }
  .wb_read_inputs(data, method) |>
    .wb_compute_terms(method) |>
    .wb_blue_green(method) |>
    .wb_attach_polity(data) |>
    .wb_finalise(method, resolution) |>
    .wb_resolve_polity_validity(polity_validity)
}

#' Assemble monthly SOC climate drivers from CRU climate and LPJmL hydrology.
#'
#' @description
#' Builds the monthly per-cell climate drivers the soil-organic-carbon
#' decomposition modifiers consume: air temperature, topsoil soil-water
#' saturation, monthly precipitation and potential evapotranspiration (the
#' Century modifier drivers), the monthly water-minus-potential-
#' evapotranspiration surplus (the RothC/HSOC driver), the annual water balance
#' (the AMG driver), the volumetric soil water content with its field-capacity,
#' wilting-point and porosity references (the ICBM moisture drivers) and clay
#' content. Air temperature comes from CRU TS 4.09 ([read_cru_climate()]
#' `"tmp"`, degrees Celsius); potential evapotranspiration from CRU `"pet"`
#' (mm/day), converted to a monthly total by multiplying by the days in the
#' month; the water input (precipitation plus irrigation) from the LPJmL run so
#' it is consistent with the hydrology that produced the soil water content; and
#' soil water content from LPJmL directly ([read_lpjml_hydrology()] `"swc"`,
#' topmost layer). The soil hydraulic references (field capacity, wilting point,
#' porosity) come from the dominant HWSD texture class of each cell via
#' [read_soil_hydraulic()], and the volumetric soil water content is
#' `theta = swc_topsoil * porosity` (the LPJmL fractional saturation scaled by
#' the cell porosity). Clay content is a soil-texture covariate supplied via
#' `data$clay`; the polity key comes from a cell-polity crosswalk
#' (`data$cell_polity`).
#'
#' @details
#' The monthly PET total is `pet_mm = pet_mm_day * days_in_month`; the monthly
#' water surplus is `water_minus_pet_mm = (precip_mm + irrig_mm) - pet_mm`; and
#' the annual water balance `water_balance_mm` is the per-cell-year sum of that
#' surplus, repeated across every month of the cell-year so it can drive the AMG
#' modifier that expects one annual scalar per cell-year. `precip_mm` carries
#' precipitation only (irrigation excluded), as the Century moisture factor
#' expects. The volumetric soil water content `theta = swc_topsoil * porosity`
#' varies by month with the LPJmL saturation, while its `t_field`, `t_wilt` and
#' `porosity` references are static per cell (the dominant HWSD texture class'
#' properties), together driving the ICBM piecewise moisture response. The water
#' input basis is recorded in `method_water_input` (`"lpjml_prec_irrig"`, LPJmL
#' precipitation plus irrigation). Air temperature (CRU) and the soil texture
#' products (clay, hydraulic properties) are not LPJmL outputs, hence the mixed
#' sources.
#'
#' @param run_dir Path to the LPJmL run output directory. `NULL` (default) uses
#'   `WHEP_LPJML_RUN_DIR` when set, and the pinned `lpjml-soc-hydrology`
#'   artifact otherwise, so running LPJmL is not a prerequisite. That artifact
#'   holds only the three LPJmL monthly drivers (topsoil saturation,
#'   precipitation, irrigation); air temperature still comes from CRU and the
#'   texture products from HWSD, both downloadable, so neither is pinned.
#' @param years Optional integer vector of calendar years to keep. `NULL` keeps
#'   every year the inputs cover.
#' @inheritParams build_water_balance
#' @param data Optional named list of pre-loaded inputs, each falling back to
#'   its reader when absent: `temp` (CRU `tmp`, `lon`, `lat`, `year`, `month`,
#'   `value` degrees Celsius), `pet` (CRU `pet`, same schema, mm/day), `prec`
#'   and `irrig` (LPJmL monthly, `lon`, `lat`, `year`, `month`, `value` mm/month),
#'   `swc` ([read_lpjml_hydrology()] soil water content), `clay` (`lon`, `lat`,
#'   `clay_pct`, required), `cell_polity` (`lon`, `lat`, `area_code`, the polity
#'   crosswalk, required) and `soil_hydraulic` (`lon`, `lat`, `t_field`,
#'   `t_wilt`, `porosity`; falls back to [read_soil_hydraulic()], cropped to
#'   `cell_polity` when supplied).
#' @param example If `TRUE`, return a small fixture instead of reading data.
#'   Defaults to `FALSE`.
#' @return A tibble with `lon`, `lat`, `area_code`, `year`, `month`, `temp_c`,
#'   `swc_topsoil`, `precip_mm` and `pet_mm` (monthly, the Century modifier
#'   drivers), `water_minus_pet_mm` (the monthly RothC/HSOC surplus),
#'   `water_balance_mm` (the annual sum of `water_minus_pet_mm`, the AMG
#'   modifier driver, repeated across a cell-year's months), `clay_pct`,
#'   `theta`, `t_field`, `t_wilt` and `porosity` (the ICBM moisture drivers:
#'   the monthly volumetric soil water content and its static field-capacity,
#'   wilting-point and porosity references) and `method_water_input`, plus the
#'   polity columns below, plus `reporting_polity_out_of_span` when
#'   `polity_validity = "flag"`.
#' @inheritSection whep_polity_columns Polity columns
#' @export
#' @examples
#' get_soc_climate_drivers(example = TRUE)
get_soc_climate_drivers <- function(
  run_dir = NULL,
  years = NULL,
  polity_validity = c("keep", "flag", "drop"),
  data = list(),
  example = FALSE
) {
  polity_validity <- rlang::arg_match(polity_validity)
  if (isTRUE(example)) {
    return(.wb_resolve_polity_validity(
      .example_soc_climate_drivers(),
      polity_validity
    ))
  }
  pin <- .socd_pin_hydrology(data, run_dir, years)
  swc <- .wb_swc_topsoil(data, run_dir, years, pin)
  monthly <- .socd_monthly_climate(data, run_dir, years, pin)
  clay <- .wb_require_input(data$clay, "clay", c("clay_pct"))
  polity <- .wb_require_input(data$cell_polity, "cell_polity", c("area_code"))
  hydraulic <- .socd_soil_hydraulic(data)
  .assemble_soc_drivers(swc, monthly, clay, polity, hydraulic) |>
    .wb_resolve_polity_validity(polity_validity)
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
.wb_read_inputs <- function(data, method) {
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
  if (method$drainage == "residual") {
    flux_vars <- flux_vars[names(flux_vars) != "seepage"]
  }
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

# Attach cell-level blue/green consumptive water and net irrigation requirement
# (mm/yr) from the per-CFT `cft_consump_water_b` / `cft_consump_water_g` /
# `cft_nir` inputs, summing the crop-band values per cell-year. Columns are NA
# when the corresponding per-CFT input is not supplied; the all-NA blue/green
# consumptive columns make the cft_native split fall back (see .wb_blue_green()).
.wb_attach_cft_consump <- function(wide, data) {
  band_inputs <- list(
    consump_blue_mm = data$cft_consump_water_b,
    consump_green_mm = data$cft_consump_water_g,
    cft_nir_mm = data$cft_nir
  )
  purrr::reduce2(
    band_inputs,
    names(band_inputs),
    .wb_join_cell_band,
    .init = wide
  )
}

# Join one per-CFT band input summed to cell-year as `out_col`, or add an all-NA
# column when the input is absent.
# TODO(cft_nir): optionally wire read_lpjml_hydrology("cft_nir") here once
# build_water_balance() has a run-directory/year contract. Until then
# cft_nir_mm is NA unless `data$cft_nir` is supplied as a cell-year (or
# per-band) `lon`,`lat`,`year`,`value` tibble.
.wb_join_cell_band <- function(wide, raw, out_col) {
  summed <- .wb_cell_consump(raw, out_col)
  if (is.null(summed)) {
    dplyr::mutate(wide, "{out_col}" := NA_real_)
  } else {
    dplyr::left_join(wide, summed, by = c("lon", "lat", "year"))
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
# water input sums prec and irrig (also exposed as prec_mm and irrig_mm);
# runoff carried through; drainage by method; pet placeholder (no LPJmL PET).
# The 4-term identity is water input equals aet plus runoff plus drainage plus
# soil-water change, and the additive identity water_input == prec + irrig holds.
.wb_compute_terms <- function(wide, method) {
  out <- dplyr::mutate(
    wide,
    aet_mm = transp + evap + interc,
    prec_mm = prec,
    irrig_mm = irrig,
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
# blue / (blue + green), applied to total AET. The native consumptive-water mm
# are exposed directly as blue_consump_mm and green_consump_mm.
.wb_blue_green_cft <- function(terms) {
  total <- terms$consump_blue_mm + terms$consump_green_mm
  blue_share <- dplyr::if_else(total > 0, terms$consump_blue_mm / total, 0)
  dplyr::mutate(
    terms,
    aet_blue_mm = aet_mm * blue_share,
    aet_green_mm = aet_mm * (1 - blue_share),
    blue_consump_mm = consump_blue_mm,
    green_consump_mm = consump_green_mm,
    .bg_method = "cft_native"
  )
}

# Blue/green split from the irrigation share of water input. Labelled
# irrig_share when requested, or irrig_share_fallback when cft_native degraded.
# Without per-CFT consumptive water, blue_consump_mm and green_consump_mm fall
# back to the blue and green AET (the best available consumptive proxy).
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
    blue_consump_mm = aet_mm * blue_share,
    green_consump_mm = aet_mm * (1 - blue_share),
    .bg_method = bg_method
  )
}

# Attach the reporting-polity columns, after reporting -- and optionally
# removing -- the cell-years the crosswalk cannot honestly place in time.
#
# `data$cell_polity` is a present-day rasterization with NO year dimension
# (whep#460, whep#579), while polity validity IS year-scoped. So a cell carrying
# `area_code` 52 (Azerbaijan) is labelled that in 1901 as readily as in 2009,
# and `.add_reporting_polity_columns()` then resolves 1901 to the nearest
# period, `AZE-1991-2025`, a state that did not exist. That substitution is
# already recorded as `mapping_status == "out_of_span"` inside
# `.add_polity_columns_dt()`, but the column is dropped from published outputs,
# so today it is silent. MEASURED on the deployed
# `cell_polity_fraction.parquet` over the 1901-2009 LPJmL run: 1,948 of 19,838
# (area_code, year) pairs, 21 of 182 area codes, 14,761 of 58,791 cells -- the
# post-Soviet and post-Yugoslav successors plus South Sudan.
#
# The rows are not wrong about the water; the cells are real territory and the
# physics is per cell. Only the polity NAME is anachronistic. Dropping them
# therefore deletes valid hydrology, which is why `"keep"` stays the default.
.wb_resolve_polity_validity <- function(table, polity_validity) {
  gaps <- .wb_polity_gaps(table)
  .wb_warn_polity_gaps(table, gaps, polity_validity)
  if (polity_validity == "drop" && nrow(gaps) > 0L) {
    table <- dplyr::anti_join(table, gaps, by = c("area_code", "year"))
  }
  status <- if (polity_validity == "flag") "flag" else NULL
  .add_reporting_polity_columns(table, mapping_status = status)
}

# The (area_code, year) pairs of `table` whose polity is a nearest-period
# stand-in. Resolved on the DISTINCT pairs, not the rows: a gridded water
# balance is millions of rows over at most a few thousand pairs, and
# `polity_coverage_gaps()` resolves whatever it is handed.
.wb_polity_gaps <- function(table) {
  if (!all(c("area_code", "year") %in% names(table))) {
    return(tibble::tibble(area_code = integer(0), year = integer(0)))
  }
  dplyr::distinct(table, area_code, year) |>
    polity_coverage_gaps() |>
    dplyr::select(area_code, year)
}

# Name what the stand-ins are, in the style of .wb_warn_uncovered_cells(). The
# message says whether the rows were kept, flagged or dropped, so a log line is
# self-explanatory about which of the three ran.
.wb_warn_polity_gaps <- function(table, gaps, polity_validity) {
  if (nrow(gaps) == 0L) {
    return(invisible(NULL))
  }
  n_rows <- nrow(dplyr::semi_join(table, gaps, by = c("area_code", "year")))
  codes <- sort(unique(gaps$area_code))
  fate <- c(
    keep = "kept as-is",
    flag = "kept and flagged in reporting_polity_out_of_span",
    drop = "dropped"
  )[[polity_validity]]
  cli::cli_warn(c(
    "!" = "{n_rows} row{?s} over {length(codes)} area code{?s} resolve to a
      polity that did not exist in that row's year (years
      {min(gaps$year)}-{max(gaps$year)}); they are {fate}.",
    i = "The cell-polity crosswalk has no year dimension, so an early cell
      carries its present-day territory. Area codes: {codes}.",
    i = "{.fn polity_coverage_gaps} names the polity each one landed on;
      {.code polity_validity = \"drop\"} removes them."
  ))
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
  .wb_warn_uncovered_cells(terms, crosswalk)
  dplyr::inner_join(terms, crosswalk, by = c("lon", "lat")) |>
    .wb_drop_unsimulated()
}

# Warn when simulated cells have no polity in the crosswalk (they are dropped by
# the join, so their water would silently vanish from the polity totals). Keyed
# on the water terms carrying a real (finite) drainage value.
.wb_warn_uncovered_cells <- function(terms, crosswalk) {
  sim <- terms |>
    dplyr::filter(is.finite(.data$drainage_mm)) |>
    dplyr::distinct(.data$lon, .data$lat)
  missing <- dplyr::anti_join(sim, crosswalk, by = c("lon", "lat"))
  if (nrow(missing) > 0) {
    cli::cli_warn(c(
      "!" = "{nrow(missing)} simulated cell{?s} ha{?s/ve} no polity in the
        cell_polity crosswalk and are dropped from the polity aggregation.",
      i = "Extend the crosswalk to cover the simulated grid to retain them."
    ))
  }
}

# Drop cell-polity rows that carry a polity label but no model data: crosswalk
# cells outside the simulated (LPJmL) grid join in with non-finite drainage, and
# passing that NaN downstream silently poisons leaching (see #381). Warn and
# drop rather than propagate the NaN or silently discard it.
.wb_drop_unsimulated <- function(joined) {
  unsimulated <- !is.finite(joined$drainage_mm)
  if (any(unsimulated)) {
    cells <- joined[unsimulated, ] |>
      dplyr::distinct(.data$lon, .data$lat)
    cli::cli_warn(c(
      "!" = "Dropped {sum(unsimulated)} cell-polity row{?s}
        ({nrow(cells)} cell{?s}) with a polity label but no LPJmL model data
        (crosswalk cells outside the simulated grid).",
      i = "Restrict {.code data$cell_polity} to the simulated grid to avoid
        the non-finite drainage they would otherwise inject."
    ))
    joined <- joined |> dplyr::filter(is.finite(.data$drainage_mm))
  }
  joined
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
      prec_mm,
      irrig_mm,
      pet_mm,
      aet_mm,
      aet_blue_mm,
      aet_green_mm,
      blue_consump_mm,
      green_consump_mm,
      cft_nir_mm,
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
    "prec_mm",
    "irrig_mm",
    "pet_mm",
    "aet_mm",
    "aet_blue_mm",
    "aet_green_mm",
    "blue_consump_mm",
    "green_consump_mm",
    "cft_nir_mm",
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

# Area-weighted mean that returns NA (never NaN) when no valid positive-weight
# entry remains, so an all-NA column (e.g. the pet_mm placeholder) or an
# all-zero-area polity aggregates to NA, not NaN. Invalid weights (e.g. a
# border cell with NA cell_area_ha) are dropped rather than poisoning the whole
# polity total; weighted.mean's na.rm filters only on col, never on weight.
.wb_weighted_mean <- function(col, weight) {
  keep <- is.finite(weight) & weight > 0
  col <- col[keep]
  weight <- weight[keep]
  if (length(col) == 0L || all(is.na(col))) {
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
.wb_example <- function(method, resolution, polity_validity = "keep") {
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
  out <- if (resolution == "grid") {
    .wb_drop_polity_cols(grid)
  } else {
    .wb_aggregate_polity(grid)
  }
  .wb_resolve_polity_validity(out, polity_validity)
}

# Topsoil soil-water saturation per cell-month, from data$swc, the pinned
# artifact or the reader. `years` is forwarded so the reader slices the
# soil-water cube to the requested years instead of materialising the full
# multi-decade 4-D array. The pinned artifact already holds the topsoil layer
# only, so the layer filter below is a no-op on that path.
.wb_swc_topsoil <- function(data, run_dir, years, pin = NULL) {
  swc <- data$swc %||%
    .socd_pin_var(pin, "swc_topsoil") %||%
    read_lpjml_hydrology(
      "swc",
      run_dir = run_dir,
      years = years,
      monthly = TRUE
    )
  swc <- .filter_years_if_present(tibble::as_tibble(swc), years)
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

# Build the monthly climate part of the SOC drivers: CRU air temperature, the
# monthly water surplus water_minus_pet_mm = (LPJmL prec + irrig) - CRU
# pet[mm/day] * days_in_month, and the per-model driver columns the SOC
# decomposition modifiers consume: precip_mm and pet_mm (monthly, for Century)
# and water_balance_mm (the annual sum of water_minus_pet_mm, for AMG). Each
# source falls back to its reader when not injected.
.socd_monthly_climate <- function(data, run_dir, years, pin = NULL) {
  temp <- .socd_read(data$temp, "tmp", years)
  pet <- .socd_read(data$pet, "pet", years)
  prec <- .socd_lpjml(
    data$prec %||% .socd_pin_var(pin, "prec_mm"),
    "prec",
    run_dir,
    years
  )
  irrig <- .socd_lpjml(
    data$irrig %||% .socd_pin_var(pin, "irrig_mm"),
    "irrig",
    run_dir,
    years
  )
  temp |>
    dplyr::rename(temp_c = value) |>
    dplyr::inner_join(
      dplyr::rename(pet, pet_mm_day = value),
      by = c("lon", "lat", "year", "month")
    ) |>
    dplyr::inner_join(
      dplyr::rename(prec, precip_mm = value),
      by = c("lon", "lat", "year", "month")
    ) |>
    dplyr::inner_join(
      dplyr::rename(irrig, irrig_mm = value),
      by = c("lon", "lat", "year", "month")
    ) |>
    dplyr::mutate(
      pet_mm = pet_mm_day * .days_in_month(year, month),
      water_minus_pet_mm = (precip_mm + irrig_mm) - pet_mm,
      method_water_input = "lpjml_prec_irrig"
    ) |>
    .socd_add_water_balance() |>
    dplyr::select(
      lon,
      lat,
      year,
      month,
      temp_c,
      precip_mm,
      pet_mm,
      water_minus_pet_mm,
      water_balance_mm,
      method_water_input
    )
}

# Attach the annual water balance (mm): the per-cell-year sum of the monthly
# water_minus_pet_mm surplus (P + irrig - PET), joined back so every month of a
# cell-year carries that year's single annual scalar (the per-cell-year value
# soc_rate_modifier_amg expects). Keyed on (lon, lat, year): the annual balance
# of a grid cell is independent of which polity later claims it.
.socd_add_water_balance <- function(monthly) {
  annual <- monthly |>
    dplyr::summarise(
      water_balance_mm = sum(water_minus_pet_mm),
      .by = c(lon, lat, year)
    )
  dplyr::inner_join(monthly, annual, by = c("lon", "lat", "year"))
}

# Read a CRU variable (temp or pet) from the injected tibble or read_cru_climate.
.socd_read <- function(input, var, years) {
  raw <- input %||% read_cru_climate(var, years = years)
  .check_columns(raw, c("lon", "lat", "year", "month", "value"), var)
  .filter_years_if_present(tibble::as_tibble(raw), years)
}

# Read an LPJmL monthly hydrology flux from the injected tibble or the reader.
.socd_hydro_alias <- function() {
  "lpjml-soc-hydrology"
}

# Whether the pinned artifact has to be fetched at all: kept separate from the
# fetch so the policy is testable without network access. Note the check is
# "were ALL THREE supplied", not "any": a caller who overrides only `prec` still
# needs a source for the other two.
.socd_needs_pin <- function(data, run_dir) {
  supplied <- !is.null(data$swc) &&
    !is.null(data$prec) &&
    !is.null(data$irrig)
  has_run <- .has_path(run_dir) || .has_path(Sys.getenv("WHEP_LPJML_RUN_DIR"))
  !supplied && !has_run
}

# The pin seam for the three LPJmL monthly drivers get_soc_climate_drivers()
# needs (topsoil saturation, precipitation, irrigation). Read ONCE here and
# handed to both consumers, so a call that falls back for all three fetches the
# artifact once rather than three times. Returns NULL -- meaning "no pin
# needed" -- whenever a run directory is available or every LPJmL var was
# supplied directly, so neither of those paths touches the network.
# Air temperature (CRU) and the soil texture products are NOT in this artifact:
# both come from downloadable third-party sources, so pinning them would freeze
# data a user can fetch themselves.
.socd_pin_hydrology <- function(data, run_dir, years) {
  if (!.socd_needs_pin(data, run_dir)) {
    return(NULL)
  }
  raw <- .read_lpjml_pin(.socd_hydro_alias())
  .check_columns(
    raw,
    c("lon", "lat", "year", "month", "swc_topsoil", "prec_mm", "irrig_mm"),
    .socd_hydro_alias()
  )
  .filter_years_if_present(tibble::as_tibble(raw), years)
}

# Pull one column out of the pinned monthly table as the (lon, lat, year,
# month, value) shape every monthly reader emits, so the pin is
# indistinguishable from a reader downstream.
.socd_pin_var <- function(pin, column) {
  if (is.null(pin)) {
    return(NULL)
  }
  dplyr::transmute(
    pin,
    .data$lon,
    .data$lat,
    .data$year,
    .data$month,
    value = .data[[column]]
  )
}

.socd_lpjml <- function(input, var, run_dir, years) {
  raw <- input %||%
    read_lpjml_hydrology(
      var,
      run_dir = run_dir,
      years = years,
      monthly = TRUE
    )
  .check_columns(raw, c("lon", "lat", "year", "month", "value"), var)
  .filter_years_if_present(tibble::as_tibble(raw), years)
}

# Days in a calendar month, vectorised, honouring leap years.
.days_in_month <- function(year, month) {
  base <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  leap <- (year %% 4 == 0 & year %% 100 != 0) | (year %% 400 == 0)
  base[month] + as.integer(month == 2L & leap)
}

# Join topsoil soil water, the monthly CRU/LPJmL climate, clay, the polity key
# and the soil hydraulic properties into the monthly SOC driver schema.
# area_code comes from the cell-polity crosswalk (lon, lat); a border cell keeps
# every polity it overlaps. theta (volumetric soil water content, the ICBM
# moisture driver) is the LPJmL topsoil fractional saturation times the cell's
# derived porosity: theta = swc_topsoil * porosity.
.assemble_soc_drivers <- function(swc, monthly, clay, polity, hydraulic) {
  swc |>
    dplyr::inner_join(monthly, by = c("lon", "lat", "year", "month")) |>
    dplyr::left_join(clay, by = c("lon", "lat")) |>
    dplyr::inner_join(
      dplyr::select(polity, lon, lat, area_code),
      by = c("lon", "lat")
    ) |>
    dplyr::left_join(hydraulic, by = c("lon", "lat")) |>
    dplyr::mutate(theta = .data$swc_topsoil * .data$porosity) |>
    dplyr::select(
      lon,
      lat,
      area_code,
      year,
      month,
      temp_c,
      swc_topsoil,
      precip_mm,
      pet_mm,
      water_minus_pet_mm,
      water_balance_mm,
      clay_pct,
      theta,
      t_field,
      t_wilt,
      porosity,
      method_water_input
    )
}

# Read (or accept via data$soil_hydraulic) the per-cell soil hydraulic
# properties (t_field, t_wilt, porosity) that feed the ICBM moisture driver,
# via the shared HWSD reader. The cell-polity crosswalk, when supplied, crops
# and gap-fills the HWSD raster to the region of interest.
.socd_soil_hydraulic <- function(data) {
  hydraulic <- data$soil_hydraulic %||%
    read_soil_hydraulic(data = list(cell_polity = data$cell_polity))
  .check_columns(
    hydraulic,
    c("lon", "lat", "t_field", "t_wilt", "porosity"),
    "soil_hydraulic"
  )
  tibble::as_tibble(hydraulic)
}
