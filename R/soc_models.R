# Soil-organic-carbon turnover model functions (Module B, Task B4). Five
# per-year SOC simulators sharing a common call contract: HSOC (Spain two-pool
# equilibrium plus dynamic evolution), RothC (five-pool monthly Euler), ICBM
# (analytical two-pool), AMG (analytical active plus stable pool) and Century
# (five-pool ODE). Rate constants and structural fractions are read from
# whep::soc_turnover_params, never inlined. Climate response enters through a
# single annual climate_modifier scalar (the per-model native climate functions
# in R/soc_climate.R produce it). Equations transcribed verbatim from the
# verified porting spec (plans/ref-module-b-soc-models-spec.md).

#' Simulate soil organic carbon with the HSOC two-pool model.
#'
#' @description
#' Annual HSOC trajectory (Spain historical pipeline): a fresh and a humus
#' decomposing pool plus an inert organic matter pool. The inert pool is the
#' Falloon (1998) function of initial carbon. Each year a pool stock loses
#' first-order decomposition and gains its carbon input. Land-use-change carbon
#' transfer is deferred to a later phase (single land use here).
#'
#' @param initial_soc_mgc_ha Initial soil organic carbon stock (Mg C per ha).
#' @param c_input_mgc_ha_yr Annual carbon input (Mg C per ha per year).
#' @param years Number of years to simulate.
#' @param clay_pct Soil clay content (percent); unused, kept for contract.
#' @param climate_modifier Annual climate rate modifier (dimensionless).
#' @param humification_fraction Fraction of carbon input humified into the
#'   humus pool (the remainder feeds the fresh pool).
#' @return A tibble with one row per year and pool: \code{year}, \code{pool}
#'   (fresh, humus, iom), \code{stock_mgc_ha} and \code{rate_mgc_ha}.
#' @source Coleman, K. & Jenkinson, D. S. (1996).
#'   \doi{10.1007/978-3-642-61094-3_17}; inert organic matter:
#'   Falloon, P. et al. (1998). \doi{10.1016/S0038-0717(97)00256-3}.
#' @export
#' @examples
#' calculate_soc_hsoc(
#'   initial_soc_mgc_ha = 50,
#'   c_input_mgc_ha_yr = 2,
#'   years = 5
#' )
calculate_soc_hsoc <- function(
  initial_soc_mgc_ha,
  c_input_mgc_ha_yr,
  years,
  clay_pct = NA,
  climate_modifier = 1,
  humification_fraction = 0.3
) {
  iom <- 0.049 * initial_soc_mgc_ha^1.139
  humus_in <- c_input_mgc_ha_yr * humification_fraction
  fresh_in <- c_input_mgc_ha_yr - humus_in
  inputs <- c(fresh = fresh_in, humus = humus_in)
  rates <- c(
    fresh = .soc_param("hsoc", "fresh", "decomposition_rate"),
    humus = .soc_param("hsoc", "humus", "decomposition_rate")
  )
  .hsoc_evolve(inputs, rates, climate_modifier, years, iom)
}

#' Simulate soil organic carbon with the RothC five-pool model.
#'
#' @description
#' Annual RothC trajectory integrated by monthly Euler steps. Active carbon is
#' partitioned into decomposable plant material, resistant plant material,
#' microbial biomass and humified organic matter, plus a Falloon (1998) inert
#' pool. Decomposed carbon is split between biomass and humus by a clay-texture
#' function.
#'
#' @param initial_soc_mgc_ha Initial soil organic carbon stock (Mg C per ha).
#' @param c_input_mgc_ha_yr Annual carbon input (Mg C per ha per year).
#' @param years Number of years to simulate.
#' @param clay_pct Soil clay content (percent).
#' @param climate_modifier Annual climate rate modifier (dimensionless),
#'   applied to every monthly decomposition step.
#' @return A tibble with one row per year: \code{year}, \code{dpm}, \code{rpm},
#'   \code{bio}, \code{hum}, \code{iom} and \code{soc_total}.
#' @source Coleman, K. & Jenkinson, D. S. (1996).
#'   \doi{10.1007/978-3-642-61094-3_17}; inert organic matter:
#'   Falloon, P. et al. (1998). \doi{10.1016/S0038-0717(97)00256-3}.
#' @export
#' @examples
#' calculate_soc_rothc(
#'   initial_soc_mgc_ha = 50,
#'   c_input_mgc_ha_yr = 2,
#'   years = 5,
#'   clay_pct = 20
#' )
calculate_soc_rothc <- function(
  initial_soc_mgc_ha,
  c_input_mgc_ha_yr,
  years,
  clay_pct = NA,
  climate_modifier = 1
) {
  iom <- 0.049 * initial_soc_mgc_ha^1.139
  pools <- .rothc_init_pools(initial_soc_mgc_ha - iom)
  splits <- .rothc_splits(clay_pct)
  rates <- .soc_rates("rothc", c("dpm", "rpm", "bio", "hum"))
  monthly <- .rothc_run(
    pools,
    rates,
    splits,
    c_input_mgc_ha_yr,
    climate_modifier,
    years
  )
  .rothc_annual(monthly, iom, years)
}

#' Simulate soil organic carbon with the ICBM two-pool model.
#'
#' @description
#' Analytical ICBM trajectory: a young and an old pool with a closed-form
#' solution evaluated at integer years. A degenerate branch handles the case
#' where the two effective rate constants coincide.
#'
#' @param initial_soc_mgc_ha Initial soil organic carbon stock (Mg C per ha).
#' @param c_input_mgc_ha_yr Annual carbon input (Mg C per ha per year).
#' @param years Number of years to simulate.
#' @param clay_pct Soil clay content (percent); unused, kept for contract.
#' @param climate_modifier Annual climate rate modifier (dimensionless),
#'   scaling both pool decomposition rates.
#' @return A tibble with one row per year: \code{year}, \code{y}, \code{o} and
#'   \code{soc_total}.
#' @source Andren, O. & Katterer, T. (1997).
#'   \doi{10.1890/1051-0761(1997)007[1226:ITICBM]2.0.CO;2}.
#' @export
#' @examples
#' calculate_soc_icbm(
#'   initial_soc_mgc_ha = 50,
#'   c_input_mgc_ha_yr = 2,
#'   years = 5
#' )
calculate_soc_icbm <- function(
  initial_soc_mgc_ha,
  c_input_mgc_ha_yr,
  years,
  clay_pct = NA,
  climate_modifier = 1
) {
  k_y <- .soc_param("icbm", "young", "decomposition_rate") * climate_modifier
  k_o <- .soc_param("icbm", "old", "decomposition_rate") * climate_modifier
  h <- .soc_param("icbm", "transfer", "humification_coefficient")
  .icbm_trajectory(initial_soc_mgc_ha, c_input_mgc_ha_yr, years, k_y, k_o, h)
}

#' Simulate soil organic carbon with the AMG model.
#'
#' @description
#' Analytical AMG trajectory: an active decomposing pool and an inert stable
#' pool. The humification coefficient is selected from the carbon input type
#' via \code{whep::amg_h_by_input_type} (default 0.15). The active pool relaxes
#' to its steady state \code{h * input / k}; the stable pool is constant.
#'
#' @param initial_soc_mgc_ha Initial soil organic carbon stock (Mg C per ha).
#' @param c_input_mgc_ha_yr Annual carbon input (Mg C per ha per year).
#' @param years Number of years to simulate.
#' @param clay_pct Soil clay content (percent); unused, kept for contract.
#' @param climate_modifier Annual climate rate modifier (dimensionless),
#'   scaling the active-pool decomposition rate.
#' @param c_input_type Carbon input type label used to look up the
#'   humification coefficient.
#' @param init_mode Initial pool split: \code{"fixed_iom"} (published stable
#'   fraction) or \code{"steady_state"}.
#' @return A tibble with one row per year: \code{year}, \code{ca}, \code{cs}
#'   and \code{soc_total}.
#' @source Saffih-Hdadi, K. & Mary, B. (2008).
#'   \doi{10.1016/j.soilbio.2007.08.022}.
#' @export
#' @examples
#' calculate_soc_amg(
#'   initial_soc_mgc_ha = 50,
#'   c_input_mgc_ha_yr = 2,
#'   years = 5
#' )
calculate_soc_amg <- function(
  initial_soc_mgc_ha,
  c_input_mgc_ha_yr,
  years,
  clay_pct = NA,
  climate_modifier = 1,
  c_input_type = NA,
  init_mode = c("fixed_iom", "steady_state")
) {
  init_mode <- rlang::arg_match(init_mode)
  k <- .soc_param("amg", "active", "decomposition_rate") * climate_modifier
  f_iom <- .soc_param("amg", "stable", "inert_fraction")
  h <- .amg_h(c_input_type)
  .amg_trajectory(
    list(soc0 = initial_soc_mgc_ha, input = c_input_mgc_ha_yr, years = years),
    list(k = k, h = h, f_iom = f_iom, init_mode = init_mode)
  )
}

#' Simulate soil organic carbon with the Century five-pool model.
#'
#' @description
#' Annual Century trajectory integrated with \code{deSolve::lsoda}. Carbon
#' flows through structural and metabolic litter and active, slow and passive
#' soil organic matter pools, with texture-dependent rates and inter-pool
#' transfer fractions.
#'
#' @param initial_soc_mgc_ha Initial soil organic carbon stock (Mg C per ha).
#' @param c_input_mgc_ha_yr Annual carbon input (Mg C per ha per year).
#' @param years Number of years to simulate.
#' @param clay_pct Soil clay content (percent).
#' @param climate_modifier Annual climate rate modifier (dimensionless),
#'   scaling every pool decomposition rate.
#' @return A tibble with one row per year: \code{year}, \code{str},
#'   \code{met}, \code{act}, \code{slw}, \code{pas} and \code{soc_total}.
#' @source Parton, W. J. et al. (1987).
#'   \doi{10.2136/sssaj1987.03615995005100050015x}; SoilR implementation:
#'   Sierra, C. A. et al. (2012). \doi{10.5194/gmd-5-1045-2012}.
#' @export
#' @examples
#' calculate_soc_century(
#'   initial_soc_mgc_ha = 50,
#'   c_input_mgc_ha_yr = 2,
#'   years = 5,
#'   clay_pct = 20
#' )
calculate_soc_century <- function(
  initial_soc_mgc_ha,
  c_input_mgc_ha_yr,
  years,
  clay_pct = NA,
  climate_modifier = 1
) {
  rlang::check_installed("deSolve")
  params <- .century_params(clay_pct, climate_modifier)
  state <- .century_init(initial_soc_mgc_ha)
  .century_solve(state, params, c_input_mgc_ha_yr, years)
}

# -- Shared parameter accessors -----------------------------------------------

.soc_param <- function(model_name, component_name, parameter_name) {
  whep::soc_turnover_params |>
    dplyr::filter(
      model == model_name,
      component == component_name,
      parameter == parameter_name
    ) |>
    dplyr::pull(value)
}

.soc_rates <- function(model_name, components) {
  stats::setNames(
    purrr::map_dbl(
      components,
      \(comp) .soc_param(model_name, comp, "decomposition_rate")
    ),
    components
  )
}

# -- HSOC helpers -------------------------------------------------------------

.hsoc_evolve <- function(inputs, rates, climate_modifier, years, iom) {
  pools <- names(inputs)
  evolved <- purrr::map(
    pools,
    \(p) {
      stock_eq <- inputs[[p]] / (rates[[p]] * climate_modifier)
      .hsoc_pool_series(
        stock_eq,
        inputs[[p]],
        rates[[p]] * climate_modifier,
        years
      )
    }
  )
  iom_series <- tibble::tibble(
    year = 0:years,
    pool = "iom",
    stock_mgc_ha = iom,
    rate_mgc_ha = 0
  )
  dplyr::bind_rows(purrr::set_names(evolved, pools), .id = "pool") |>
    dplyr::bind_rows(iom_series)
}

.hsoc_pool_series <- function(stock_eq, input, decay, years) {
  stocks <- purrr::accumulate(
    seq_len(years),
    \(stock, .) stock - stock * decay + input,
    .init = stock_eq
  )
  tibble::tibble(
    year = 0:years,
    stock_mgc_ha = stocks,
    rate_mgc_ha = input - stocks * decay
  )
}

# -- RothC helpers ------------------------------------------------------------

.rothc_init_pools <- function(active) {
  fracs <- .soc_rates_named("rothc", "init_active_fraction")
  list(
    dpm = active * fracs[["dpm"]],
    rpm = active * fracs[["rpm"]],
    bio = active * fracs[["bio"]],
    hum = active * fracs[["hum"]]
  )
}

.soc_rates_named <- function(model_name, parameter_name) {
  rows <- whep::soc_turnover_params |>
    dplyr::filter(model == model_name, parameter == parameter_name)
  stats::setNames(rows$value, rows$component)
}

.rothc_splits <- function(clay_pct) {
  ratio <- .soc_param("rothc", "input", "dpm_rpm_ratio")
  frac_dpm <- ratio / (1 + ratio)
  x <- 1.67 * (1.85 + 1.60 * exp(-0.0786 * clay_pct))
  list(
    frac_dpm = frac_dpm,
    frac_rpm = 1 - frac_dpm,
    frac_bio = 0.46 / (x + 0.46 + 0.54),
    frac_hum = 0.54 / (x + 0.46 + 0.54)
  )
}

.rothc_run <- function(pools, rates, splits, input, climate_modifier, years) {
  dt <- 1 / 12
  c_in_dpm <- input / 12 * splits$frac_dpm
  c_in_rpm <- input / 12 * splits$frac_rpm
  purrr::accumulate(
    seq_len(12 * years),
    \(state, .) {
      .rothc_step(
        state,
        rates,
        splits,
        c_in_dpm,
        c_in_rpm,
        climate_modifier,
        dt
      )
    },
    .init = pools
  )
}

.rothc_step <- function(state, rates, splits, c_in_dpm, c_in_rpm, abc, dt) {
  dec_dpm <- state$dpm * rates[["dpm"]] * abc * dt
  dec_rpm <- state$rpm * rates[["rpm"]] * abc * dt
  dec_bio <- state$bio * rates[["bio"]] * abc * dt
  dec_hum <- state$hum * rates[["hum"]] * abc * dt
  total_dec <- dec_dpm + dec_rpm + dec_bio + dec_hum
  list(
    dpm = state$dpm - dec_dpm + c_in_dpm,
    rpm = state$rpm - dec_rpm + c_in_rpm,
    bio = state$bio - dec_bio + total_dec * splits$frac_bio,
    hum = state$hum - dec_hum + total_dec * splits$frac_hum
  )
}

.rothc_annual <- function(monthly, iom, years) {
  snapshots <- monthly[seq(1, 12 * years + 1, by = 12)]
  tibble::tibble(
    year = 0:years,
    dpm = purrr::map_dbl(snapshots, "dpm"),
    rpm = purrr::map_dbl(snapshots, "rpm"),
    bio = purrr::map_dbl(snapshots, "bio"),
    hum = purrr::map_dbl(snapshots, "hum"),
    iom = iom
  ) |>
    dplyr::mutate(soc_total = dpm + rpm + bio + hum + iom)
}

# -- ICBM helpers -------------------------------------------------------------

.icbm_trajectory <- function(soc0, input, years, k_y, k_o, h) {
  y_ss <- if (k_y > 0) input / k_y else 0
  o_ss <- if (k_o > 0) h * input / k_o else 0
  y_frac <- if (k_o > 0 && k_y > 0) 1 / (1 + h * k_y / k_o) else 0
  y_0 <- y_frac * soc0
  o_0 <- max(soc0 - y_0, 0)
  t <- seq(0, years)
  y_t <- y_ss + (y_0 - y_ss) * exp(-k_y * t)
  o_t <- .icbm_old_series(t, o_ss, o_0, y_ss, y_0, k_y, k_o, h)
  tibble::tibble(year = t, y = y_t, o = o_t, soc_total = y_t + o_t)
}

.icbm_old_series <- function(t, o_ss, o_0, y_ss, y_0, k_y, k_o, h) {
  if (abs(k_o - k_y) < 1e-8) {
    a <- o_0 - o_ss
    return(
      o_ss + a * exp(-k_o * t) + h * k_y * (y_0 - y_ss) * t * exp(-k_y * t)
    )
  }
  a <- h * k_y * (y_0 - y_ss) / (k_o - k_y)
  o_ss + (o_0 - o_ss + a) * exp(-k_o * t) - a * exp(-k_y * t)
}

# -- AMG helpers --------------------------------------------------------------

.amg_h <- function(c_input_type) {
  if (is.null(c_input_type) || is.na(c_input_type)) {
    return(.amg_default_h())
  }
  label <- stringr::str_to_lower(c_input_type)
  matches <- whep::amg_h_by_input_type |>
    dplyr::filter(!is.na(pattern)) |>
    dplyr::arrange(match_order) |>
    dplyr::filter(stringr::str_detect(label, pattern))
  if (nrow(matches) == 0) .amg_default_h() else matches$h[1]
}

.amg_default_h <- function() {
  whep::amg_h_by_input_type |>
    dplyr::filter(input_type == "default") |>
    dplyr::pull(h)
}

.amg_trajectory <- function(inputs, params) {
  ca_ss <- if (params$k > 0) params$h * inputs$input / params$k else 0
  init <- .amg_init(inputs$soc0, ca_ss, params$f_iom, params$init_mode)
  t <- seq(0, inputs$years)
  ca_t <- ca_ss + (init$ca0 - ca_ss) * exp(-params$k * t)
  tibble::tibble(
    year = t,
    ca = ca_t,
    cs = init$cs0,
    soc_total = ca_t + init$cs0
  )
}

.amg_init <- function(soc0, ca_ss, f_iom, init_mode) {
  if (init_mode == "fixed_iom") {
    return(list(cs0 = f_iom * soc0, ca0 = (1 - f_iom) * soc0))
  }
  ca0 <- min(ca_ss, 0.5 * soc0)
  list(cs0 = max(soc0 - ca0, 0), ca0 = ca0)
}

# -- Century helpers ----------------------------------------------------------

.century_params <- function(clay_pct, xi) {
  ls <- .soc_param("century", "defaults", "lignin_fraction")
  ln <- .soc_param("century", "defaults", "lignin_n_ratio")
  silt_pct <- .soc_param("century", "defaults", "silt_pct")
  texture <- .century_texture(clay_pct, silt_pct, ls, ln)
  c(
    .century_rates(ls, texture$f_txtr, xi),
    texture,
    .century_transfers(ls, texture$es)
  )
}

.century_texture <- function(clay_pct, silt_pct, ls, ln) {
  txtr <- max(min(clay_pct, 100), 0) / 100 + max(min(silt_pct, 100), 0) / 100
  f_txtr <- .soc_param("century", "act", "texture_intercept") -
    .soc_param("century", "act", "texture_slope") * txtr
  es <- .soc_param("century", "act", "respiration_intercept") -
    .soc_param("century", "act", "respiration_texture_slope") * txtr
  fm <- .soc_param("century", "met", "metabolic_intercept") -
    .soc_param("century", "met", "metabolic_ln_slope") * ln
  list(f_txtr = f_txtr, es = es, fm = fm, fs = 1 - fm, ls = ls)
}

.century_rates <- function(ls, f_txtr, xi) {
  weeks <- .soc_param("century", "all", "weeks_per_year")
  base <- .soc_rates_named("century", "base_rate_weekly")
  list(
    k_str = base[["str"]] * exp(-3 * ls) * weeks * xi,
    k_met = base[["met"]] * weeks * xi,
    k_act = base[["act"]] * f_txtr * weeks * xi,
    k_slw = base[["slw"]] * weeks * xi,
    k_pas = base[["pas"]] * weeks * xi
  )
}

.century_transfers <- function(ls, es) {
  list(
    a_str_act = (1 - ls) *
      (1 - .soc_param("century", "str_act", "transfer_fraction_const")),
    a_str_slw = ls *
      (1 - .soc_param("century", "str_slw", "transfer_fraction_const")),
    a_met_act = 1 - .soc_param("century", "met_act", "transfer_fraction_const"),
    a_act_slw = 1 - es - .soc_param("century", "act_pas", "transfer_fraction"),
    a_act_pas = .soc_param("century", "act_pas", "transfer_fraction"),
    a_slw_act = .soc_param("century", "slw_act", "transfer_fraction"),
    a_slw_pas = .soc_param("century", "slw_pas", "transfer_fraction"),
    a_pas_act = 1 - .soc_param("century", "pas_act", "transfer_fraction_const")
  )
}

.century_init <- function(soc0) {
  c(
    str = 0,
    met = 0,
    act = .soc_param("century", "act", "init_fraction") * soc0,
    slw = .soc_param("century", "slw", "init_fraction") * soc0,
    pas = .soc_param("century", "pas", "init_fraction") * soc0
  )
}

.century_rhs <- function(t, state, params) {
  with(as.list(c(state, params)), {
    out_str <- k_str * str
    out_met <- k_met * met
    out_act <- k_act * act
    out_slw <- k_slw * slw
    out_pas <- k_pas * pas
    in_act <- a_str_act *
      out_str +
      a_met_act * out_met +
      a_slw_act * out_slw +
      a_pas_act * out_pas
    in_slw <- a_str_slw * out_str + a_act_slw * out_act
    in_pas <- a_act_pas * out_act + a_slw_pas * out_slw
    list(c(
      fs * input - out_str,
      fm * input - out_met,
      in_act - out_act,
      in_slw - out_slw,
      in_pas - out_pas
    ))
  })
}

.century_solve <- function(state, params, input, years) {
  params$input <- input
  sol <- deSolve::lsoda(
    y = state,
    times = seq(0, years),
    func = .century_rhs,
    parms = params
  )
  out <- tibble::as_tibble(as.data.frame(sol))
  names(out)[1] <- "year"
  out |>
    dplyr::mutate(soc_total = str + met + act + slw + pas)
}
