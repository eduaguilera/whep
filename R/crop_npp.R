#' Estimate potential net primary production.
#'
#' Estimates potential net primary production per unit area, used downstream to
#' scale weed biomass on cropland. Several methods are available; the default
#' `lpjml` reads gross managed-grassland NPP from a finished LPJmL run, while the
#' climate methods compute potential NPP from temperature, water input and
#' actual evapotranspiration.
#'
#' @param x A tibble. The climate methods require `temp_c` (mean annual
#'   temperature, degrees C), `water_input_mm` (precipitation plus irrigation)
#'   and `aet_mm` (actual evapotranspiration). The `lpjml` method instead needs
#'   a spatial key to join the gridded grass NPP (see `lpjml`).
#' @param method Potential-NPP method, one of `"lpjml"` (default), `"miami"`,
#'   `"nceas"` or `"rosenzweig"`.
#' @param lpjml A named list of options for the `lpjml` method (passed to the
#'   LPJmL grass reader). Ignored by the climate methods.
#' @return The input tibble with `npp_potential_dm_t_ha` (potential NPP, t DM per
#'   hectare) and `method_npp_potential` (the method used).
#' @export
#' @examples
#' calculate_potential_npp(
#'   tibble::tibble(temp_c = 15, water_input_mm = 800, aet_mm = 700),
#'   method = "miami"
#' )
calculate_potential_npp <- function(
  x,
  method = c("lpjml", "miami", "nceas", "rosenzweig"),
  lpjml = list()
) {
  method <- rlang::arg_match(method)
  out <- switch(
    method,
    lpjml = .potential_npp_lpjml(x, lpjml),
    miami = .potential_npp_miami(x),
    nceas = .potential_npp_nceas(x),
    rosenzweig = .potential_npp_rosenzweig(x)
  )
  dplyr::mutate(out, method_npp_potential = method)
}

#' Estimate crop above-ground residue biomass.
#'
#' Estimates crop residue dry matter from production and area using an ensemble
#' of an IPCC 2019 linear model and a `bio_coefs` residue:product ratio. The
#' irrigation and modern-variety adjustments activate only when their driver
#' columns are present in `x`.
#'
#' @param x A tibble with `item_prod_code`, `production_t` (fresh matter) and
#'   `area_ha`. Optional `water_regime` enables the irrigation adjustment;
#'   optional `year` plus `region_hanpp` enable the modern-variety adjustment.
#' @param method Residue method: `"ensemble"` (default, weighted IPCC + ratio),
#'   `"ipcc"` (IPCC linear only) or `"ratio"` (`bio_coefs` ratio only).
#' @param weights Named list; `w_ipcc` is the IPCC weight in the ensemble (0-1,
#'   default 0.5), used only when `method = "ensemble"`.
#' @return The input tibble with `product_dm_t`, `yield_dm_t_ha`, `residue_dm_t`
#'   and `method_residue`.
#' @export
#' @examples
#' calculate_crop_residues(
#'   tibble::tibble(item_prod_code = "15", production_t = 100, area_ha = 40)
#' )
calculate_crop_residues <- function(
  x,
  method = c("ensemble", "ipcc", "ratio"),
  weights = list(w_ipcc = 0.5)
) {
  method <- rlang::arg_match(method)
  .crop_npp_validate(
    x,
    c("item_prod_code", "production_t", "area_ha"),
    "calculate_crop_residues"
  )
  w_ipcc <- .residue_weight(weights)
  x |>
    .residue_dm_conversions() |>
    .residue_ipcc() |>
    .residue_ratio() |>
    .residue_irrigation_factor() |>
    .residue_variety_factor() |>
    .residue_combine(method, w_ipcc) |>
    dplyr::mutate(method_residue = method) |>
    .residue_cleanup()
}

.npp_coef <- function(coefs, model, param, component = NULL) {
  mask <- coefs$model == model & coefs$parameter == param
  if (!is.null(component)) {
    mask <- mask & coefs$component == component
  }
  val <- coefs$value[mask]
  if (length(val) != 1L) {
    cli::cli_abort(
      "npp_model_coefs: no unique match for model {.val {model}}, \\
       parameter {.val {param}}."
    )
  }
  val
}

.potential_npp_miami <- function(x) {
  coefs <- whep::whep_coef_table("npp_model_coefs")
  t_max <- .npp_coef(coefs, "Miami", "Max_gCm2yr", "F_MAT")
  t_mid <- .npp_coef(coefs, "Miami", "Midpoint", "F_MAT")
  t_rate <- .npp_coef(coefs, "Miami", "Rate", "F_MAT")
  p_max <- .npp_coef(coefs, "Miami", "Max_gCm2yr", "F_MAP")
  p_rate <- .npp_coef(coefs, "Miami", "Rate", "F_MAP")
  dplyr::mutate(
    x,
    npp_potential_dm_t_ha = pmin(
      (t_max / (1 + exp(t_mid - t_rate * temp_c))) / 100,
      (p_max * (1 - exp(-p_rate * water_input_mm))) / 100
    )
  )
}

.potential_npp_nceas <- function(x) {
  coefs <- whep::whep_coef_table("npp_model_coefs")
  carbon_fraction <- whep::whep_coef_table("weed_coefs")$residue_c_kgdm_weed
  a_max <- .npp_coef(coefs, "NCEAS_nontree_ANPP", "Max_gCm2yr")
  a_rate <- .npp_coef(coefs, "NCEAS_nontree_ANPP", "Rate")
  dplyr::mutate(
    x,
    npp_potential_dm_t_ha = a_max *
      (1 - exp(-a_rate * water_input_mm)) /
      (100 * carbon_fraction)
  )
}

.potential_npp_rosenzweig <- function(x) {
  coefs <- whep::whep_coef_table("npp_model_coefs")
  slope <- .npp_coef(coefs, "Rosenzweig", "Slope")
  intercept <- .npp_coef(coefs, "Rosenzweig", "Intercept")
  dplyr::mutate(
    x,
    npp_potential_dm_t_ha = 10^(slope * log10(aet_mm) + intercept) / 100
  )
}

.potential_npp_lpjml <- function(x, options) {
  cli::cli_abort(c(
    "The {.val lpjml} potential-NPP method is not yet wired.",
    i = "It is implemented in a later task (gross LPJmL grass NPP)."
  ))
}

.crop_npp_validate <- function(x, required, fn) {
  missing <- required[
    !purrr::map_lgl(required, function(col) {
      rlang::has_name(x, col)
    })
  ]
  if (length(missing) > 0) {
    cli::cli_abort("{fn}: missing required column{?s} {.field {missing}}.")
  }
  invisible(x)
}

.residue_weight <- function(weights) {
  w <- weights[["w_ipcc"]]
  if (is.null(w)) {
    w <- 0.5
  }
  if (w < 0 || w > 1) {
    cli::cli_abort("{.arg w_ipcc} must be between 0 and 1, not {w}.")
  }
  w
}

.residue_dm_conversions <- function(x) {
  coefs <- whep::whep_coef_table("bio_coefs") |>
    dplyr::select(
      item_prod_code,
      product_dm_kgfm,
      residue_dm_kgfm,
      residue_kg_product_fm_kg
    )
  x |>
    dplyr::mutate(item_prod_code = as.character(item_prod_code)) |>
    dplyr::left_join(coefs, by = "item_prod_code") |>
    dplyr::mutate(
      production_t = tidyr::replace_na(as.numeric(production_t), 0),
      product_dm_t = production_t * product_dm_kgfm,
      yield_dm_t_ha = dplyr::if_else(area_ha > 0, product_dm_t / area_ha, 0)
    )
}

.residue_ipcc <- function(x) {
  mapping <- whep::whep_coef_table("ipcc_crop_mapping") |>
    dplyr::select(item_prod_code, ipcc_crop, crop_group)
  coefs <- whep::whep_coef_table("ipcc_residue_coefs") |>
    dplyr::select(ipcc_crop, slope_ag, intercept_ag_dm_t_ha)
  x |>
    dplyr::left_join(mapping, by = "item_prod_code") |>
    dplyr::left_join(coefs, by = "ipcc_crop") |>
    dplyr::mutate(
      residue_ipcc_t_ha = dplyr::if_else(
        !is.na(slope_ag),
        slope_ag * yield_dm_t_ha + intercept_ag_dm_t_ha,
        NA_real_
      ),
      residue_ipcc_t = pmax(residue_ipcc_t_ha, 0) * area_ha
    )
}

.residue_ratio <- function(x) {
  dplyr::mutate(
    x,
    residue_ratio_t = production_t * residue_kg_product_fm_kg * residue_dm_kgfm
  )
}

.residue_irrigation_factor <- function(x) {
  if (!rlang::has_name(x, "water_regime")) {
    return(dplyr::mutate(x, residue_ratio_factor = 1))
  }
  irr <- whep::whep_coef_table("irrigation_adj") |>
    dplyr::select(water_regime, residue_ratio_factor)
  sens <- whep::whep_coef_table("irr_residue_crop_adj") |>
    dplyr::select(crop_group, irr_residue_sensitivity)
  x |>
    dplyr::left_join(irr, by = "water_regime") |>
    dplyr::left_join(sens, by = "crop_group") |>
    dplyr::mutate(
      residue_ratio_factor = tidyr::replace_na(residue_ratio_factor, 1),
      irr_residue_sensitivity = tidyr::replace_na(irr_residue_sensitivity, 0),
      residue_ratio_factor = 1 +
        irr_residue_sensitivity * (residue_ratio_factor - 1)
    )
}

.residue_variety_factor <- function(x) {
  if (!all(c("year", "region_hanpp") %in% names(x))) {
    return(dplyr::mutate(x, hi_correction_factor = 1))
  }
  adoption <- whep::whep_coef_table("modern_variety_adoption") |>
    dplyr::select(region_hanpp, crop_group, year, modern_share)
  ranges <- whep::whep_coef_table("hi_crop_ranges") |>
    dplyr::select(crop_group, hi_gap_factor)
  x |>
    dplyr::left_join(adoption, by = c("region_hanpp", "crop_group", "year")) |>
    dplyr::left_join(ranges, by = "crop_group") |>
    dplyr::mutate(
      modern_share = tidyr::replace_na(modern_share, 1),
      hi_gap_factor = tidyr::replace_na(hi_gap_factor, 1),
      hi_correction_factor = 1 + (1 - modern_share) * (hi_gap_factor - 1)
    )
}

.residue_combine <- function(x, method, w_ipcc) {
  dplyr::mutate(
    x,
    residue_ratio_adj_t = residue_ratio_t *
      residue_ratio_factor *
      hi_correction_factor,
    residue_ipcc_adj_t = residue_ipcc_t *
      residue_ratio_factor *
      hi_correction_factor,
    residue_dm_t = .residue_pick(
      method,
      w_ipcc,
      residue_ipcc_adj_t,
      residue_ratio_adj_t
    ),
    residue_dm_t = pmax(tidyr::replace_na(residue_dm_t, 0), 0)
  )
}

.residue_pick <- function(method, w_ipcc, ipcc, ratio) {
  switch(
    method,
    ipcc = ipcc,
    ratio = ratio,
    ensemble = dplyr::if_else(
      !is.na(ipcc),
      w_ipcc * ipcc + (1 - w_ipcc) * ratio,
      ratio
    )
  )
}

.residue_cleanup <- function(x) {
  dplyr::select(
    x,
    -dplyr::any_of(c(
      "product_dm_kgfm",
      "residue_dm_kgfm",
      "residue_kg_product_fm_kg",
      "ipcc_crop",
      "crop_group",
      "slope_ag",
      "intercept_ag_dm_t_ha",
      "residue_ipcc_t_ha",
      "residue_ipcc_t",
      "residue_ipcc_adj_t",
      "residue_ratio_t",
      "residue_ratio_adj_t",
      "residue_ratio_factor",
      "irr_residue_sensitivity",
      "hi_correction_factor",
      "modern_share",
      "hi_gap_factor"
    ))
  )
}
