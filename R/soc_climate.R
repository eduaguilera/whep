# Per-model native soil-organic-carbon climate rate modifiers (Module B, Task
# B3). Each soil-carbon turnover model carries its own published temperature
# and moisture response; these are deliberately NOT a shared climate scalar.
# Equations and parameters transcribed verbatim from the verified porting spec
# (plans/ref-module-b-soc-climate-spec.md); two sign/normalization
# reconstructions are flagged inline (AMG bT sign, ICBM 30-degree anchor).

#' Compute the RothC and HSOC annual climate rate modifier.
#'
#' @description
#' Annual a*b*c rate-modifying factor of the RothC / HSOC soil-carbon model:
#' the temperature factor a, the topsoil-moisture-deficit factor b, and the
#' plant-cover factor c, averaged over the supplied monthly series. The same
#' function modifies HSOC decomposition.
#'
#' @param temp_c Numeric monthly air temperature series (degrees Celsius).
#' @param water_minus_pet_mm Numeric monthly water surplus series
#'   (precipitation minus potential evapotranspiration, mm), used to
#'   accumulate the topsoil moisture deficit.
#' @param clay_pct Soil clay content (percent).
#' @param soil_cover Vegetated soil-cover fraction (0 bare, 1 fully covered);
#'   a scalar or monthly series.
#' @param soil_depth_m Topsoil depth over which the moisture deficit is
#'   accumulated (metres). Defaults to 0.3.
#' @return The annual mean of the monthly a*b*c product (a single numeric).
#' @source Coleman, K. & Jenkinson, D. S. (1996). RothC-26.3: a model for the
#'   turnover of carbon in soil. \doi{10.1007/978-3-642-61094-3_17}. Moisture
#'   deficit and cover terms as implemented in the Spain historical SOC
#'   pipeline.
#' @export
#' @examples
#' soc_rate_modifier_rothc(
#'   temp_c = c(1.2, 7.8, 15.0),
#'   water_minus_pet_mm = c(-10, 5, 20),
#'   clay_pct = 20,
#'   soil_cover = 0
#' )
soc_rate_modifier_rothc <- function(
  temp_c,
  water_minus_pet_mm,
  clay_pct,
  soil_cover,
  soil_depth_m = 0.3
) {
  a <- pmax(47.91 / (1 + exp(106.06 / (temp_c + 18.27))), 0)
  max_tsmd <- soil_depth_m *
    100 *
    (-(20 + 1.3 * clay_pct - 0.01 * clay_pct^2)) /
    23
  b <- .rothc_moisture_factor(water_minus_pet_mm, max_tsmd)
  cover_factor <- 0.6 + 0.4 * (1 - soil_cover)
  mean(a * b * cover_factor)
}

#' Compute the ICBM annual climate rate modifier.
#'
#' @description
#' Annual ICBM re_clim rate-modifying factor: a Ratkowsky-type temperature
#' response and a piecewise volumetric-moisture response, multiplied and
#' rescaled to the Swedish Ultuna reference site, averaged over the supplied
#' series.
#'
#' @param temp_c Numeric soil (or air-proxy) temperature series (degrees
#'   Celsius).
#' @param theta Numeric volumetric soil water content series (fraction).
#' @param t_field Field-capacity volumetric water content (fraction).
#' @param t_wilt Wilting-point volumetric water content (fraction).
#' @param porosity Soil porosity (fraction).
#' @return The annual mean of the monthly re_clim values (a single numeric).
#' @source Katterer, T., Reichstein, M., Andren, O. & Lomander, A. (1998).
#'   Temperature dependence of organic matter decomposition.
#'   \doi{10.1007/s003740050430}. Normalization and piecewise moisture form as
#'   implemented in the canonical reclim package.
#' @export
#' @examples
#' soc_rate_modifier_icbm(
#'   temp_c = c(5, 15, 25),
#'   theta = c(0.20, 0.25, 0.30),
#'   t_field = 0.30,
#'   t_wilt = 0.10,
#'   porosity = 0.45
#' )
soc_rate_modifier_icbm <- function(temp_c, theta, t_field, t_wilt, porosity) {
  # Flag: the (T - Tmin)^2 / (30 - Tmin)^2 normalization (=1 at 30 C) is taken
  # from the reclim implementation, not the printed Katterer (1998) equation;
  # the test asserts re_temp == 1 at temp_c == 30.
  re_temp <- ifelse(temp_c < -3.78, 0, (temp_c - (-3.78))^2 / (30 - (-3.78))^2)
  re_wat <- .icbm_moisture_factor(theta, t_field, t_wilt, porosity)
  mean(re_temp * re_wat / 0.1056855)
}

#' Compute the AMG (AMGv2) annual climate rate modifier.
#'
#' @description
#' Annual AMGv2 climate rate-modifying factor: the logistic temperature
#' function f(T) normalized to 1 at the 15 degree reference and the logistic
#' moisture function f(H) of the annual water balance, multiplied and averaged
#' over the supplied series.
#'
#' @param temp_c Numeric mean annual air temperature series (degrees Celsius).
#' @param water_balance_mm Numeric annual water balance series (precipitation
#'   plus irrigation minus potential evapotranspiration, mm).
#' @return The annual mean of the f(T) * f(H) product (a single numeric).
#' @source Clivot, H., Mouny, J.-C., Duparque, A., Dinh, J.-L., Denoroy, P.,
#'   ... Mary, B. (2019). Modeling soil organic carbon evolution in long-term
#'   arable experiments with AMG model. \doi{10.1016/j.envsoft.2019.04.004};
#'   temperature form from Saffih-Hdadi, K. & Mary, B. (2008).
#'   \doi{10.1016/j.soilbio.2007.08.022}.
#' @export
#' @examples
#' soc_rate_modifier_amg(
#'   temp_c = c(8, 15, 22),
#'   water_balance_mm = c(-100, 0, 200)
#' )
soc_rate_modifier_amg <- function(temp_c, water_balance_mm) {
  f_t <- .amg_temperature_factor(temp_c)
  f_h <- .amg_moisture_factor(water_balance_mm)
  mean(f_t * f_h)
}

#' Compute the Century DEFAC annual climate rate modifier.
#'
#' @description
#' Annual Century DEFAC rate-modifying factor: the original monthly-Century
#' Poisson temperature factor (normalized to 1 at the 35 degree optimum) and
#' the precipitation-over-evapotranspiration logistic moisture factor,
#' multiplied and averaged over the supplied series.
#'
#' @param temp_c Numeric monthly air (or soil-surface) temperature series
#'   (degrees Celsius).
#' @param precip_mm Numeric monthly precipitation series (mm).
#' @param pet_mm Numeric monthly potential evapotranspiration series (mm).
#' @return The annual mean of the temperature-by-moisture product (a single
#'   numeric).
#' @source Parton, W. J., Schimel, D. S., Cole, C. V. & Ojima, D. S. (1987).
#'   \doi{10.2136/sssaj1987.03615995005100050015x}; SoilR implementation:
#'   Sierra, C. A., Mueller, M. & Trumbore, S. E. (2012).
#'   \doi{10.5194/gmd-5-1045-2012}.
#' @export
#' @examples
#' soc_rate_modifier_century(
#'   temp_c = c(15, 25, 35),
#'   precip_mm = c(40, 60, 80),
#'   pet_mm = c(50, 70, 90)
#' )
soc_rate_modifier_century <- function(temp_c, precip_mm, pet_mm) {
  t_factor <- .century_temperature_factor(temp_c)
  w_factor <- 1 / (1 + 30 * exp(-8.5 * (precip_mm / pet_mm)))
  mean(t_factor * w_factor)
}

# -- Private helpers ----------------------------------------------------------

.rothc_moisture_factor <- function(water_minus_pet_mm, max_tsmd) {
  tsmd <- .rothc_accumulate_tsmd(water_minus_pet_mm, max_tsmd)
  threshold <- 0.444 * max_tsmd
  b <- ifelse(
    tsmd > threshold,
    1,
    0.2 + 0.8 * (max_tsmd - tsmd) / (max_tsmd - threshold)
  )
  pmax(b, 0.2)
}

.rothc_accumulate_tsmd <- function(water_minus_pet_mm, max_tsmd) {
  # Running monthly topsoil moisture deficit (mm, <= 0): start at min(P-PET, 0)
  # then carry the previous month's deficit forward, floored at max_tsmd (the
  # most negative attainable deficit).
  init <- min(water_minus_pet_mm[1], 0)
  purrr::accumulate(
    water_minus_pet_mm[-1],
    \(prev, balance) max(min(prev + balance, 0), max_tsmd),
    .init = max(init, max_tsmd)
  )
}

.icbm_moisture_factor <- function(theta, t_field, t_wilt, porosity) {
  rs <- 0.5
  topt <- 0.2 + 1.26 * t_field^2.03
  tth <- 0.0965 * log(t_wilt) + 0.3
  lo <- min(topt, t_field)
  hi <- max(topt, t_field)
  re_wat <- dplyr::case_when(
    theta < tth ~ 0,
    theta < lo ~ (theta - tth) / (lo - tth),
    theta <= hi ~ 1,
    TRUE ~ 1 - (1 - rs) * (theta - hi) / (porosity - hi)
  )
  pmax(pmin(re_wat, 1), 0)
}

.amg_temperature_factor <- function(temp_c) {
  # Flag: the AMGv2 SI text-layer dropped the unary minus signs inside exp();
  # the bT exponent sign is reconstructed POSITIVE so that f(T) == 1 at the
  # stated T_Ref = 15 C normalization. The test asserts f_t == 1 at 15 C.
  a_t <- 25
  c_t <- 0.120
  t_ref <- 15
  b_t <- (a_t - 1) * exp(c_t * t_ref)
  ifelse(temp_c < 0, 0, a_t / (1 + b_t * exp(-c_t * temp_c)))
}

.amg_moisture_factor <- function(water_balance_mm) {
  a_h <- 0.03
  b_h <- 5.247
  1 / (1 + a_h * exp(-b_h * water_balance_mm / 1000))
}

.century_temperature_factor <- function(temp_c) {
  t_max <- 45
  t_opt <- 35
  ratio <- (t_max - temp_c) / (t_max - t_opt)
  ratio^0.2 * exp((0.2 / 2.63) * (1 - ratio^2.63))
}
