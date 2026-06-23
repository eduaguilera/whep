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
