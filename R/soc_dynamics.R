# Soil-organic-carbon dynamics selector (Module B, Task B5). A single entry
# point that dispatches to one of the five SOC turnover models in
# R/soc_models.R, computing each model's native annual climate modifier from
# the climate drivers supplied in `data` (via the matching R/soc_climate.R
# function) when those drivers are present; otherwise an already-computed
# `data$climate_modifier` is honoured verbatim (e.g. a caller that derived the
# modifier itself, such as the carbon-balance equilibrium spin-up), falling
# back to a neutral 1 only when neither is present. The chosen model is
# recorded in a method_soc column.

#' Simulate soil organic carbon dynamics with a selectable model.
#'
#' @description
#' Run one of the five soil-organic-carbon turnover models (HSOC, RothC, ICBM,
#' AMG or Century) through a single interface. When climate drivers are present
#' in \code{data} (for example \code{temp_c}), the selected model's native
#' annual climate rate modifier is computed from the matching climate function
#' and passed in; otherwise the model runs with a neutral modifier of 1. The
#' chosen model is stamped into a \code{method_soc} column on the output.
#'
#' @param model SOC model to run: one of \code{"hsoc"}, \code{"rothc"},
#'   \code{"icbm"}, \code{"amg"} or \code{"century"}. Defaults to the most
#'   detailed pool structure available, \code{"hsoc"}.
#' @param data Named list of model arguments. Always carries
#'   \code{initial_soc_mgc_ha}, \code{c_input_mgc_ha_yr} and \code{years};
#'   may also carry \code{clay_pct} and model-specific arguments (for example
#'   \code{c_input_type} and \code{init_mode} for AMG). Climate drivers, when
#'   present, are read from the same list to build the climate modifier
#'   (see \code{\link{soc_rate_modifier_rothc}} and siblings for the per-model
#'   driver names). If the drivers are absent but \code{data$climate_modifier}
#'   already holds a value, that value is used instead of recomputing it;
#'   only when neither is available does the model run with a neutral
#'   modifier of 1.
#' @param example If \code{TRUE}, return a small hardcoded example tibble
#'   instead of running the model.
#' @return A tibble of the selected model's trajectory with an added
#'   \code{method_soc} column naming the model.
#' @source Model bodies and climate functions as cited in
#'   \code{\link{calculate_soc_hsoc}} and \code{\link{soc_rate_modifier_rothc}}.
#' @export
#' @examples
#' calculate_soc_dynamics(
#'   model = "icbm",
#'   data = list(initial_soc_mgc_ha = 50, c_input_mgc_ha_yr = 2, years = 5)
#' )
calculate_soc_dynamics <- function(
  model = c("hsoc", "rothc", "icbm", "amg", "century"),
  data = list(),
  example = FALSE
) {
  if (example) {
    return(.example_soc_dynamics())
  }
  model <- rlang::arg_match(model)
  climate_modifier <- .soc_climate_modifier(model, data)
  .soc_dispatch(model, data, climate_modifier) |>
    dplyr::mutate(method_soc = model)
}

# -- Dispatch -----------------------------------------------------------------

.soc_dispatch <- function(model, data, climate_modifier) {
  data$climate_modifier <- climate_modifier
  fn <- switch(
    model,
    hsoc = calculate_soc_hsoc,
    rothc = calculate_soc_rothc,
    icbm = calculate_soc_icbm,
    amg = calculate_soc_amg,
    century = calculate_soc_century
  )
  args <- data[intersect(names(data), rlang::fn_fmls_names(fn))]
  do.call(fn, args)
}

# -- Climate modifier ---------------------------------------------------------

.soc_climate_modifier <- function(model, data) {
  drivers <- .soc_climate_drivers(model)
  if (!all(purrr::map_lgl(drivers, \(d) rlang::has_name(data, d)))) {
    return(data$climate_modifier %||% 1)
  }
  fn <- switch(
    model,
    hsoc = soc_rate_modifier_rothc,
    rothc = soc_rate_modifier_rothc,
    icbm = soc_rate_modifier_icbm,
    amg = soc_rate_modifier_amg,
    century = soc_rate_modifier_century
  )
  do.call(fn, data[drivers])
}

.soc_climate_drivers <- function(model) {
  switch(
    model,
    hsoc = ,
    rothc = c("temp_c", "water_minus_pet_mm", "clay_pct", "soil_cover"),
    icbm = c("temp_c", "theta", "t_field", "t_wilt", "porosity"),
    amg = c("temp_c", "water_balance_mm"),
    century = c("temp_c", "precip_mm", "pet_mm")
  )
}
