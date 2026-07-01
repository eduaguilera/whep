# MANNER process-based ammonia-volatilisation model (Module C, Task C4),
# ported from Spain_Hist MANNER_model.R + nh3.r:81-108.
#
# This module is a PURE, TESTABLE function: it takes driver values as
# arguments and returns an NH3-N loss. It does not read any gridded
# NetCDF/raster itself and does not need population/deposition/HYDE data.
# Wiring real gridded drivers (windspeed, soil pH, application technique by
# era) into this function is out of scope for this task; a later task
# (Module C's N-input assembly) supplies those.

#' Estimate ammonia-N volatilisation with the MANNER process-based model.
#'
#' @description
#' Ports the MANNER (Nicholson et al. 2013) process-based ammonia
#' volatilisation model. Dispatches on `fertiliser` to one of two
#' independent paths: a synthetic-fertiliser path (`"Urea"`, `"AN"`,
#' `"CAN"`, `"AS"`) driven by soil pH, application rate, rainfall and
#' temperature, and an organic-manure path (`"cattle_slurry"`,
#' `"pig_slurry"`, `"FYM"`, `"poultry_manure"`, `"urban"`) driven by
#' rainfall, temperature, wind speed, application technique, system
#' (arable/grassland) and incorporation delay.
#'
#' @details
#' The organic path's `inorganic_n_fraction` lookup ([manure_inorganic_n])
#' maps `manure_type` to a species/manure-stream key: `"cattle_slurry"` and
#' `"pig_slurry"` to the `"Liquid"` stream of Cattle/Pigs respectively,
#' `"FYM"` to Cattle `"Solid"`, and `"poultry_manure"` to Poultry
#' `"Solid"`. This mapping is a documented modelling choice made when
#' porting [manure_inorganic_n] (a reasonable reading of the manure-type
#' naming), not a literal Spain_Hist crosswalk table. `"urban"` bypasses
#' this lookup entirely: it fixes `inorganic_n_fraction = 0.5` regardless
#' of species, matching `nh3.r:102-104`.
#'
#' @param n_applied_t Numeric, nitrogen applied (t).
#' @param fertiliser One of `"Urea"`, `"AN"`, `"CAN"`, `"AS"` (synthetic
#'   path) or `"cattle_slurry"`, `"pig_slurry"`, `"FYM"`,
#'   `"poultry_manure"`, `"urban"` (organic path).
#' @param drivers A named list of driver values. Synthetic path: `soil_ph`
#'   (numeric soil pH), `rate_kg_ha` (numeric N application rate, kg N/ha),
#'   `rainfall_mm` (numeric period precipitation, mm), `irrigated`
#'   (logical), `temp_c` (numeric application-period temperature, deg C),
#'   `temp_c_annual_mean` (numeric annual mean temperature, deg C; only
#'   used for CAN/AS). Organic path: `rainfall_mm`, `irrigated`,
#'   `windspeed_ms` (numeric wind speed, m/s), `technique` (one of the six
#'   [manner_params] `technique` keys), `system` (`"Arable"` or
#'   `"Grassland"`), `temp_c`, `incorporation_delay_h` (numeric hours
#'   between surface application and soil incorporation, or `Inf`/`NA` for
#'   no incorporation), `species` (required unless `fertiliser == "urban"`;
#'   one of the eight [manure_inorganic_n] species, used only to look up
#'   `inorganic_n_fraction`).
#' @param example If `TRUE`, return a small fixture instead of computing
#'   from drivers. Defaults to `FALSE`.
#' @return A tibble with `n_applied_t`, `ef` (realised emission factor),
#'   `nh3_n_t` and `method_manner`.
#' @export
#' @examples
#' calculate_manner_nh3(example = TRUE)
calculate_manner_nh3 <- function(
  n_applied_t = NULL,
  fertiliser = NULL,
  drivers = list(),
  example = FALSE
) {
  if (isTRUE(example)) {
    return(.example_manner_nh3())
  }
  if (fertiliser %in% c("Urea", "AN", "CAN", "AS")) {
    .manner_synthetic(n_applied_t, fertiliser, drivers)
  } else if (
    fertiliser %in%
      c("cattle_slurry", "pig_slurry", "FYM", "poultry_manure", "urban")
  ) {
    .manner_organic(n_applied_t, fertiliser, drivers)
  } else {
    cli::cli_abort(c(
      "Unknown {.arg fertiliser} value {.val {fertiliser}}.",
      i = "Synthetic: {.val Urea}, {.val AN}, {.val CAN}, {.val AS}.",
      i = paste0(
        "Organic: {.val cattle_slurry}, {.val pig_slurry}, {.val FYM}, ",
        "{.val poultry_manure}, {.val urban}."
      )
    ))
  }
}

# ---- Private helpers: synthetic-fertiliser path -----------------------

# Synthetic path: ef = ph * rate * max_nh3 * land_use * rain * temp.
.manner_synthetic <- function(n_applied_t, fertiliser, drivers) {
  ph_class <- .manner_ph_class(drivers$soil_ph)
  ef <- .manner_lookup(whep::manner_params, "ph", fertiliser, ph_class) *
    .manner_synth_rate_factor(fertiliser, ph_class, drivers$rate_kg_ha) *
    .manner_lookup(whep::manner_params, "max_nh3", fertiliser) *
    .manner_lookup(whep::manner_params, "incorporation", "TallerCrop") *
    .manner_synth_rain_factor(fertiliser, ph_class, drivers) *
    .manner_synth_temp_factor(fertiliser, drivers)
  tibble::tibble(
    n_applied_t = n_applied_t,
    ef = ef,
    nh3_n_t = ef * n_applied_t,
    method_manner = paste0("manner_synthetic_", fertiliser)
  )
}

# pH class shared by ph_factor, rate_factor's soil-type axis and
# rain_factor: soil_ph < 7 -> "pH<7" (also "non-calcareous" for the rate
# table), otherwise "other pH" (also "calcareous").
.manner_ph_class <- function(soil_ph) {
  if (soil_ph < 7) "pH<7" else "other pH"
}

# Rate factor: soil_type is the same pH-class axis, re-expressed as
# calcareous/non-calcareous (calcareous == "other pH", MANNER_model.R:325).
.manner_synth_rate_factor <- function(fertiliser, ph_class, rate_kg_ha) {
  soil_type <- if (ph_class == "other pH") "calcareous" else "non-calcareous"
  rate_bin <- .manner_rate_bin(rate_kg_ha)
  whep::manner_rate_factor |>
    dplyr::filter(
      .data$fertiliser == .env$fertiliser,
      .data$soil_type == .env$soil_type,
      .data$rate_bin == .env$rate_bin
    ) |>
    dplyr::pull("factor")
}

# Application-rate bin (kg N/ha), read top-to-bottom against decreasing
# thresholds.
.manner_rate_bin <- function(rate_kg_ha) {
  data.table::fcase(
    rate_kg_ha > 200 , ">200"    ,
    rate_kg_ha > 180 , "180-200" ,
    rate_kg_ha > 150 , "150-180" ,
    rate_kg_ha > 120 , "120-150" ,
    rate_kg_ha > 90  , "90-120"  ,
    rate_kg_ha > 60  , "60-90"   ,
    rate_kg_ha > 30  , "30-60"   ,
    default = "0-30"
  )
}

# Rain factor: same rainfall_class classification as the organic path
# (.manner_rainfall_class), keyed by fertiliser and ph_class.
.manner_synth_rain_factor <- function(fertiliser, ph_class, drivers) {
  rainfall_class <- .manner_rainfall_class(
    drivers$rainfall_mm,
    drivers$irrigated
  )
  whep::manner_rain_factor |>
    dplyr::filter(
      .data$fertiliser == .env$fertiliser,
      .data$ph_class == .env$ph_class,
      .data$rainfall_class == .env$rainfall_class
    ) |>
    dplyr::pull("factor")
}

# Temperature factor: Urea/AN use an absolute reference (8.625 deg C);
# CAN/AS use an anomaly against the cell/region annual mean temperature.
.manner_synth_temp_factor <- function(fertiliser, drivers) {
  if (fertiliser %in% c("Urea", "AN")) {
    exp(0.1386 * (drivers$temp_c - 8.625)) / 3
  } else {
    exp(0.2197225 * (drivers$temp_c - drivers$temp_c_annual_mean)) / 3
  }
}

# ---- Private helpers: organic-manure path ------------------------------

# Organic path: Org_ef = rain_wet * AG * technique * windspeed * system *
# incorporation, with a 0.4 correction for FYM; nh3_n_t also scales by the
# inorganic (ammoniacal) nitrogen fraction of the applied manure.
.manner_organic <- function(n_applied_t, fertiliser, drivers) {
  manure_key <- .manner_manure_key(fertiliser)
  ag <- .manner_ag(manure_key, drivers)
  ef_raw <- .manner_lookup(
    whep::manner_params,
    "rainfall_wet",
    .manner_rainfall_class(drivers$rainfall_mm, drivers$irrigated)
  ) *
    ag *
    .manner_lookup(whep::manner_params, "technique", drivers$technique) *
    .manner_lookup(
      whep::manner_params,
      "windspeed",
      .manner_windspeed_class(drivers$windspeed_ms)
    ) *
    .manner_system_factor(drivers$system) *
    .manner_incorporation_factor(manure_key, drivers$incorporation_delay_h)
  ef <- if (fertiliser == "FYM") ef_raw * 0.4 else ef_raw
  inorganic_n_fraction <- .manner_inorganic_n_fraction(
    fertiliser,
    drivers$species
  )
  tibble::tibble(
    n_applied_t = n_applied_t,
    ef = ef,
    nh3_n_t = ef * n_applied_t * inorganic_n_fraction,
    method_manner = paste0("manner_organic_", fertiliser)
  )
}

# "urban" is not a real manure type in the AG/incorporation sub-tables; it
# borrows cattle_slurry's coefficients since both represent a wet/
# slurry-like organic input (see calculate_manner_nh3 Details for the
# inorganic_n_fraction override that actually distinguishes it).
.manner_manure_key <- function(fertiliser) {
  if (fertiliser == "urban") "cattle_slurry" else fertiliser
}

# AG = climate-and-manure-adjusted ammoniacal availability: AE (clamped
# climate factor) * manure_coef * dm_factor.
.manner_ag <- function(manure_key, drivers) {
  ae <- .manner_climate_factor_ae(drivers$rainfall_mm, drivers$temp_c)
  manure_coef <- c(
    cattle_slurry = 0.324,
    pig_slurry = 0.255,
    FYM = 0.683,
    poultry_manure = 0.523
  )
  dm_factor <- c(
    cattle_slurry = ((8.3 * 6) + 50.2) / 100,
    pig_slurry = ((8.3 * 5) + 50.2) / 100,
    FYM = ((8.3 * 6) + 50.2) / 100,
    poultry_manure = ((8.3 * 6) + 50.2) / 100
  )
  ae * manure_coef[[manure_key]] * dm_factor[[manure_key]]
}

# Climate factor AE, precipitation-banded, clamped to [0.6, 1.5].
.manner_climate_factor_ae <- function(precip_mm_period, temp_c) {
  raw <- data.table::fcase(
    precip_mm_period > 120 , 0.0030 * temp_c + 0.3248 ,
    precip_mm_period > 90  , 0.0043 * temp_c + 0.4641 ,
    precip_mm_period > 60  , 0.0062 * temp_c + 0.6629 ,
    precip_mm_period > 30  , 0.0236 * temp_c + 0.8745 ,
    precip_mm_period > 15  , 0.0319 * temp_c + 1.1806 ,
    default = 0.0431 * temp_c + 1.5936
  )
  pmin(1.5, pmax(0.6, raw))
}

# Arable is drier/faster-drying than grassland (lower NH3 loss).
.manner_system_factor <- function(system) {
  if (system == "Arable") 0.85 else 1.15
}

# Incorporation factor: pick the first (shortest) delay bin whose
# delay_hours is >= the supplied delay, reading manner_incorporation_factor
# top-to-bottom in its monotonically increasing delay_hours order; a
# missing or infinite delay maps to "No incorporation" (factor 1). This
# bin-selection rule is an inference from the monotonic structure of the
# source table (not restated in-line in Spain_Hist), so it should be
# double-checked.
.manner_incorporation_factor <- function(manure_key, incorporation_delay_h) {
  table <- whep::manner_incorporation_factor |>
    dplyr::filter(.data$manure_type == .env$manure_key)
  if (is.na(incorporation_delay_h) || is.infinite(incorporation_delay_h)) {
    return(dplyr::filter(table, .data$delay_bin == "No incorporation")$factor)
  }
  finite_bins <- dplyr::filter(table, !is.na(.data$delay_hours))
  ceiling_bin <- dplyr::filter(
    finite_bins,
    .data$delay_hours >= incorporation_delay_h
  )
  # A delay beyond the largest finite bin (">12 days") clamps to that bin's
  # factor rather than silently returning zero rows.
  if (nrow(ceiling_bin) == 0) {
    return(dplyr::slice_max(finite_bins, .data$delay_hours, n = 1)$factor)
  }
  dplyr::slice_min(ceiling_bin, .data$delay_hours, n = 1)$factor
}

# Inorganic (ammoniacal) nitrogen fraction: "urban" is a fixed 0.5
# override (nh3.r:102-104), independent of species; every other manure
# type looks up manure_inorganic_n via its species/stream mapping (see
# calculate_manner_nh3 Details).
.manner_inorganic_n_fraction <- function(fertiliser, species) {
  if (fertiliser == "urban") {
    return(0.5)
  }
  stream_map <- c(
    cattle_slurry = "Liquid",
    pig_slurry = "Liquid",
    FYM = "Solid",
    poultry_manure = "Solid"
  )
  species_map <- c(
    cattle_slurry = "Cattle",
    pig_slurry = "Pigs",
    FYM = "Cattle",
    poultry_manure = "Poultry"
  )
  whep::manure_inorganic_n |>
    dplyr::filter(
      .data$species == species_map[[fertiliser]],
      .data$manure_stream == stream_map[[fertiliser]]
    ) |>
    dplyr::pull("inorganic_n_fraction")
}

# ---- Private helpers: shared classification / lookup -------------------

# days_rain / wetness / rain_level classification shared by both paths.
.manner_rainfall_class <- function(rainfall_mm, irrigated) {
  days_rain <- if (isTRUE(irrigated)) {
    30
  } else {
    min(rainfall_mm / 100 * 15, 15)
  }
  wetness <- data.table::fcase(
    days_rain > 11 , "wet"      ,
    days_rain > 5  , "moderate" ,
    default = "dry"
  )
  rain_level <- data.table::fcase(
    rainfall_mm > 110 , "heavyrain" ,
    rainfall_mm > 50  , "lightrain" ,
    default = "norain"
  )
  paste0(rain_level, wetness)
}

.manner_windspeed_class <- function(windspeed_ms) {
  data.table::fcase(
    windspeed_ms > 8 , "strongwind" ,
    windspeed_ms > 4 , "moderawind" ,
    default = "nowind"
  )
}

# One-key manner_params lookup (category, key).
.manner_lookup <- function(table, category, key, sub_key = NULL) {
  filtered <- table |>
    dplyr::filter(.data$category == .env$category, .data$key == .env$key)
  if (!is.null(sub_key)) {
    filtered <- dplyr::filter(filtered, .data$sub_key == .env$sub_key)
  }
  dplyr::pull(filtered, "factor")
}

# Toy fixture for a runnable example (one synthetic-fertiliser call).
.example_manner_nh3 <- function() {
  tibble::tribble(
    ~n_applied_t, ~ef, ~nh3_n_t, ~method_manner,
    10, 0.033284475, 0.33284475, "manner_synthetic_Urea"
  )
}
