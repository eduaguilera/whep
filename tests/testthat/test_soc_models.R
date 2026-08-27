# Tests for the five soil-organic-carbon turnover models (Module B, Task B4):
# calculate_soc_hsoc, calculate_soc_rothc, calculate_soc_icbm,
# calculate_soc_amg, calculate_soc_century. The analytical models (ICBM, AMG)
# are checked against their closed-form steady states; the iterative models
# (HSOC, RothC) against convergence, positivity and monotonicity; Century
# against pool positivity and the total identity.

test_that("ICBM old pool converges to its analytical steady state", {
  # Old-pool steady state is h * input / k_O (Ultuna defaults h = 0.13,
  # k_O = 0.00605). Over a long horizon the old pool relaxes onto it.
  out <- whep::calculate_soc_icbm(
    initial_soc_mgc_ha = 50,
    c_input_mgc_ha_yr = 2,
    years = 2000
  )
  target <- 0.13 * 2 / 0.00605
  testthat::expect_equal(utils::tail(out$o, 1), target, tolerance = 0.01)
})

test_that("ICBM steady state scales inversely with the climate modifier", {
  # Doubling the climate modifier doubles both rates, halving the old-pool
  # steady state h * input / (k_O * modifier).
  out <- whep::calculate_soc_icbm(
    initial_soc_mgc_ha = 50,
    c_input_mgc_ha_yr = 2,
    years = 2000,
    climate_modifier = 2
  )
  target <- 0.13 * 2 / (0.00605 * 2)
  testthat::expect_equal(utils::tail(out$o, 1), target, tolerance = 0.01)
})

test_that("ICBM degenerate branch stays finite when rates coincide", {
  # The |k_O - k_Y| < 1e-8 fallback must return finite, positive carbon.
  out <- whep::calculate_soc_icbm(
    initial_soc_mgc_ha = 50,
    c_input_mgc_ha_yr = 2,
    years = 10
  )
  testthat::expect_true(all(is.finite(out$o)))
  testthat::expect_true(all(out$soc_total > 0))
})

test_that("ICBM accumulates inputs when climate stops decomposition", {
  out <- whep::calculate_soc_icbm(
    initial_soc_mgc_ha = 50,
    c_input_mgc_ha_yr = 2,
    years = 5,
    climate_modifier = 0
  )

  testthat::expect_equal(out$soc_total, 50 + 2 * (0:5))
})

test_that("ICBM old-pool solution follows its transfer ODE", {
  # For dO/dt = h*k_y*Y - k_o*O, the derivative at t = 0 must have
  # this sign and magnitude. This catches a reversal of the two transient
  # exponential terms while remaining independent of the steady state.
  k_y <- 0.8
  k_o <- 0.2
  h <- 0.1
  y_ss <- 2
  y_0 <- 5
  o_ss <- h * k_y * y_ss / k_o
  o_0 <- 3
  dt <- 1e-6
  series <- whep:::.icbm_old_series(
    c(0, dt),
    o_ss,
    o_0,
    y_ss,
    y_0,
    k_y,
    k_o,
    h
  )
  observed <- (series[2] - series[1]) / dt
  expected <- h * k_y * y_0 - k_o * o_0
  testthat::expect_equal(observed, expected, tolerance = 1e-5)
})

test_that("AMG active pool converges to its analytical steady state", {
  # Active-pool steady state is h * input / k. Default h = 0.15 (unrecognised
  # input type), k = 0.165.
  out <- whep::calculate_soc_amg(
    initial_soc_mgc_ha = 50,
    c_input_mgc_ha_yr = 2,
    years = 2000
  )
  target <- 0.15 * 2 / 0.165
  testthat::expect_equal(utils::tail(out$ca, 1), target, tolerance = 1e-4)
})

test_that("AMG humification coefficient follows the input type", {
  # A manure input type maps to h = 0.40, raising the active steady state.
  out <- whep::calculate_soc_amg(
    initial_soc_mgc_ha = 50,
    c_input_mgc_ha_yr = 2,
    years = 2000,
    c_input_type = "manure"
  )
  target <- 0.40 * 2 / 0.165
  testthat::expect_equal(utils::tail(out$ca, 1), target, tolerance = 1e-4)
})

test_that("AMG stable pool is constant and the init mode is validated", {
  out <- whep::calculate_soc_amg(
    initial_soc_mgc_ha = 50,
    c_input_mgc_ha_yr = 2,
    years = 20
  )
  testthat::expect_equal(length(unique(out$cs)), 1L)
  # fixed_iom default: stable pool is 0.65 of initial carbon.
  testthat::expect_equal(out$cs[1], 0.65 * 50)
  testthat::expect_error(
    whep::calculate_soc_amg(50, 2, 5, init_mode = "bogus"),
    class = "rlang_error"
  )
})

test_that("AMG accumulates humified inputs when decomposition is zero", {
  out <- whep::calculate_soc_amg(
    initial_soc_mgc_ha = 50,
    c_input_mgc_ha_yr = 2,
    years = 5,
    climate_modifier = 0
  )
  expected <- 50 + 0.15 * 2 * (0:5)

  testthat::expect_equal(out$soc_total, expected)
})

test_that("AMG steady_state equals ca_ss / (1 - f_iom) and ignores the seed", {
  # steady_state mode derives its own equilibrium total analytically from the
  # active steady state and the stable fraction, independent of the supplied
  # initial stock. Default h = 0.15, k = 0.165, f_iom = 0.65.
  target <- (0.15 * 2 / 0.165) / (1 - 0.65)
  finals <- purrr::map_dbl(c(10, 40, 80, 1000), \(s0) {
    out <- whep::calculate_soc_amg(
      initial_soc_mgc_ha = s0,
      c_input_mgc_ha_yr = 2,
      years = 20,
      climate_modifier = 1,
      init_mode = "steady_state"
    )
    utils::tail(out$soc_total, 1)
  })
  # Every seed lands on the identical analytical equilibrium.
  testthat::expect_equal(length(unique(round(finals, 8))), 1L)
  testthat::expect_equal(finals[1], target, tolerance = 1e-8)
  # The trajectory is flat: it starts at equilibrium and stays there.
  out <- whep::calculate_soc_amg(
    initial_soc_mgc_ha = 40,
    c_input_mgc_ha_yr = 2,
    years = 20,
    climate_modifier = 1,
    init_mode = "steady_state"
  )
  testthat::expect_equal(out$ca[1], 0.15 * 2 / 0.165, tolerance = 1e-8)
  testthat::expect_equal(out$cs[1], target - 0.15 * 2 / 0.165, tolerance = 1e-8)
  testthat::expect_equal(length(unique(round(out$soc_total, 8))), 1L)
})

test_that("HSOC returns the three pools and conserves", {
  out <- whep::calculate_soc_hsoc(
    initial_soc_mgc_ha = 50,
    c_input_mgc_ha_yr = 2,
    years = 10
  )
  # Wide, like the four sibling models: one row per year, its own pool columns
  # and a soc_total (#350).
  testthat::expect_named(out, c("year", "fresh", "humus", "iom", "soc_total"))
  testthat::expect_equal(nrow(out), 11L)
  testthat::expect_true(all(out$soc_total > 0))
  testthat::expect_equal(out$soc_total, out$fresh + out$humus + out$iom)
})

test_that("HSOC is stationary when opened exactly at its equilibrium", {
  # The equilibrium is a genuine fixed point: seeded with the stock that solves
  # active + Falloon(total) = total, the trajectory is flat. Asserting this on
  # a stock the caller chose (rather than on any stock the caller passes) is
  # what distinguishes a fixed point from an ignored argument (#348).
  eq_active <- 3.5 / 0.48 + 1.5 / 0.02
  total <- stats::uniroot(
    \(x) x - 0.049 * x^1.139 - eq_active,
    c(eq_active, 10 * eq_active)
  )$root
  out <- whep::calculate_soc_hsoc(
    initial_soc_mgc_ha = total,
    c_input_mgc_ha_yr = 5,
    years = 100
  )
  testthat::expect_true(all(out$soc_total > 0))
  testthat::expect_true(all(abs(diff(out$fresh)) < 1e-8))
  testthat::expect_true(all(abs(diff(out$humus)) < 1e-8))
  # Humified fraction 0.3: fresh input 3.5, humus input 1.5; k = 0.48 / 0.02.
  testthat::expect_equal(out$fresh[1], 3.5 / 0.48, tolerance = 1e-6)
  testthat::expect_equal(out$humus[1], 1.5 / 0.02, tolerance = 1e-6)
})

test_that("RothC stock is positive, converges and is monotone", {
  out <- whep::calculate_soc_rothc(
    initial_soc_mgc_ha = 50,
    c_input_mgc_ha_yr = 2,
    years = 300,
    clay_pct = 20
  )
  testthat::expect_equal(nrow(out), 301L)
  testthat::expect_true(all(out$soc_total > 0))
  steps <- diff(out$soc_total)
  # Constant input and modifier: the total moves one direction throughout.
  testthat::expect_equal(length(unique(sign(round(steps, 8)))), 1L)
  # Late steps are smaller than early steps: it is converging.
  testthat::expect_lt(abs(utils::tail(steps, 1)), abs(steps[1]))
})

test_that("RothC stays bounded under aggressive climate modifiers", {
  # The fast DPM pool (k = 10/yr) makes the monthly explicit-Euler step diverge
  # once k * climate_modifier / 12 exceeds 1; adaptive sub-stepping must keep
  # the trajectory finite and physically plausible instead of exploding.
  cases <- c(2, 3.69, 5, 10)
  for (cm in cases) {
    out <- whep::calculate_soc_rothc(
      initial_soc_mgc_ha = 60,
      c_input_mgc_ha_yr = 3,
      years = 50,
      clay_pct = 20,
      climate_modifier = cm
    )
    ceiling_stock <- 60 + 3 * 50
    testthat::expect_true(all(is.finite(out$soc_total)))
    testthat::expect_true(all(out$soc_total > 0))
    testthat::expect_true(all(out$soc_total < ceiling_stock))
  }
})

test_that("RothC converges to a finite steady state at the reproduction case", {
  # The hot/wet tropical cell that diverged before the sub-stepping guard.
  out <- whep::calculate_soc_rothc(
    initial_soc_mgc_ha = 60,
    c_input_mgc_ha_yr = 3,
    years = 50,
    clay_pct = 20,
    climate_modifier = 3.69
  )
  final <- utils::tail(out$soc_total, 1)
  testthat::expect_true(is.finite(final))
  testthat::expect_gt(final, 0)
  # The stock relaxes monotonically and the annual flux collapses toward zero:
  # the late-year step is a small fraction of the first-year step (converging,
  # not diverging), and a long horizon settles onto a finite asymptote.
  steps <- diff(out$soc_total)
  testthat::expect_true(all(steps < 0))
  testthat::expect_lt(abs(utils::tail(steps, 1)), 0.05 * abs(steps[1]))
  long <- whep::calculate_soc_rothc(
    initial_soc_mgc_ha = 60,
    c_input_mgc_ha_yr = 3,
    years = 300,
    clay_pct = 20,
    climate_modifier = 3.69
  )
  testthat::expect_true(is.finite(utils::tail(long$soc_total, 1)))
  testthat::expect_lt(abs(utils::tail(diff(long$soc_total), 1)), 1e-4)
})

test_that("RothC total equals the sum of its five pools", {
  out <- whep::calculate_soc_rothc(
    initial_soc_mgc_ha = 50,
    c_input_mgc_ha_yr = 2,
    years = 10,
    clay_pct = 20
  )
  testthat::expect_equal(
    out$soc_total,
    out$dpm + out$rpm + out$bio + out$hum + out$iom
  )
})

test_that("Century returns five positive pools summing to the total", {
  testthat::skip_if_not_installed("deSolve")
  out <- whep::calculate_soc_century(
    initial_soc_mgc_ha = 50,
    c_input_mgc_ha_yr = 2,
    years = 50,
    clay_pct = 20
  )
  testthat::expect_equal(nrow(out), 51L)
  pool_sum <- out$str + out$met + out$act + out$slw + out$pas
  testthat::expect_equal(out$soc_total, pool_sum)
  testthat::expect_true(all(out$soc_total > 0))
  testthat::expect_true(all(out$act >= 0))
})

test_that("Century metabolic fraction uses L/N in the tens, not its reciprocal", {
  # Fm = 0.85 - 0.018 * (L/N). With the corrected LN = 40 most litter is
  # structural (Fm ~ 0.13, Fs ~ 0.87); the old reciprocal 0.025 gave Fm ~ 0.85.
  tx <- .century_texture(clay_pct = 20, silt_pct = 45, ls = 0.5, ln = 40)
  testthat::expect_equal(tx$fm, 0.85 - 0.018 * 40)
  testthat::expect_lt(tx$fm, 0.2)
  testthat::expect_equal(tx$fm + tx$fs, 1)
})

test_that("Century silt+clay texture is capped so es / f_txtr stay non-negative", {
  # clay 90% + silt 45% would sum to 1.35 unclamped, driving es and f_txtr
  # negative (respiration < 0 creates carbon); the fraction is capped at 1.
  tx <- .century_texture(clay_pct = 90, silt_pct = 45, ls = 0.5, ln = 40)
  testthat::expect_gte(tx$es, 0)
  testthat::expect_gte(tx$f_txtr, 0)
})

test_that("every model starts its trajectory at initial_soc_mgc_ha", {
  # Cross-model invariant, not a hand-picked expectation: a turnover model is
  # given a measured starting stock and must begin there. HSOC used to discard
  # the argument and start both pools at their own equilibrium input / k, so
  # calculate_soc_hsoc(initial = 200, input = 2) opened at 53 Mg C/ha (#348).
  # That made the default model the only one unable to march from an observed
  # stock, which is what the carbon balance needs to stop initialising cells
  # at a computed equilibrium (#799).
  run <- function(model, s0) {
    traj <- whep::calculate_soc_dynamics(
      model = model,
      data = list(
        initial_soc_mgc_ha = s0,
        c_input_mgc_ha_yr = 2,
        years = 3,
        clay_pct = 20
      )
    )
    unique(traj$soc_total[traj$year == 0])
  }
  models <- c("hsoc", "rothc", "icbm", "amg")
  if (requireNamespace("deSolve", quietly = TRUE)) {
    models <- c(models, "century")
  }
  for (model in models) {
    for (s0 in c(10, 50, 200)) {
      testthat::expect_equal(
        run(model, s0),
        s0,
        tolerance = 1e-8,
        label = paste(model, "year-0 stock at initial", s0)
      )
    }
  }
})

# #348 item 3: `clay_pct` was accepted and never read, so this function and
# `build_carbon_balance(model = "hsoc")` returned different stocks for the same
# soil -- the balance scaled the humification coefficient by the Aguilera
# et al. (2018) Eq. 5-6 texture modifier and the exported function did not.
test_that("HSOC scales humification by soil texture", {
  # d runs 0.72 at 5% clay to 1.13 at 60%: coarse soils stabilise less of the
  # same carbon input, so the humus pool -- which holds most of the stock --
  # is smaller there.
  coarse <- whep::calculate_soc_hsoc(50, 3, years = 500, clay_pct = 5)
  fine <- whep::calculate_soc_hsoc(50, 3, years = 500, clay_pct = 60)

  testthat::expect_false(isTRUE(all.equal(coarse$soc_total, fine$soc_total)))
  testthat::expect_lt(
    dplyr::last(coarse$soc_total),
    dplyr::last(fine$soc_total)
  )
})

test_that("HSOC applies the documented texture modifier, not merely some", {
  # Pinning the modifier itself, so a future change to `.cb_texture_modifier()`
  # cannot pass by staying monotone in clay. 3.51 normalises d to 1 at RothC's
  # Rothamsted reference of 23.4% clay.
  d <- function(clay) 3.51 / (1.67 * (1.85 + 1.60 * exp(-0.0786 * clay)))
  h <- 0.3

  for (clay in c(5, 23.4, 60)) {
    out <- whep::calculate_soc_hsoc(
      initial_soc_mgc_ha = 50,
      c_input_mgc_ha_yr = 2,
      years = 0,
      clay_pct = clay
    )
    hf <- min(h * d(clay), 1)
    # Year 0 splits the active stock in the proportion of the pools' steady
    # states, which is where the effective fraction shows up first.
    testthat::expect_equal(
      out$humus[1] / (out$fresh[1] + out$humus[1]),
      (2 * hf / 0.02) / (2 * (1 - hf) / 0.48 + 2 * hf / 0.02),
      tolerance = 1e-10
    )
  }
})

test_that("HSOC applies no texture adjustment when clay is unknown", {
  # `clay_pct = NA` is the shared call contract's "not supplied", and means no
  # texture adjustment rather than an NA trajectory. It is what every existing
  # direct caller gets, so it must reproduce the tabulated coefficient exactly.
  out <- whep::calculate_soc_hsoc(50, 2, years = 5, clay_pct = NA)

  testthat::expect_false(anyNA(out$soc_total))
  testthat::expect_equal(
    out$humus[1] / (out$fresh[1] + out$humus[1]),
    (2 * 0.3 / 0.02) / (2 * 0.7 / 0.48 + 2 * 0.3 / 0.02),
    tolerance = 1e-10
  )
})

# The point of the fix: one model, one answer. The exported spin-up and the
# closed form `build_carbon_balance()` actually evaluates must agree, and they
# must agree because both scale exactly once -- `.cb_steady_state()` hands over
# the UNSCALED tabulated fraction now that the function scales it itself.
test_that("HSOC agrees with the balance closed form at every clay", {
  input <- 3
  h <- 0.325
  cm <- 1

  for (clay in c(5, 15, 23.4, 40, 60)) {
    seed <- whep:::.cb_seed_stock(
      "hsoc",
      input,
      whep:::.cb_hsoc_hf(h, clay),
      cm
    )
    spin_up <- whep::calculate_soc_hsoc(
      initial_soc_mgc_ha = seed,
      c_input_mgc_ha_yr = input,
      years = 5000L,
      clay_pct = clay,
      climate_modifier = cm,
      humification_fraction = h
    )
    closed <- whep:::.cb_hsoc_equilibrium(input, h, cm, clay)

    testthat::expect_equal(dplyr::last(spin_up$soc_total), closed)
    testthat::expect_equal(
      whep:::.cb_steady_state("hsoc", input, h, cm, clay),
      closed
    )
  }
})

test_that("HSOC carves the inert pool out of the stock, as RothC does", {
  # Falloon (1998) estimates IOM as a component of *measured total* soil organic
  # carbon, so it must be subtracted from the initial stock, not added on top of
  # it. calculate_soc_rothc() already does this
  # (`.rothc_init_pools(initial_soc_mgc_ha - iom)`); HSOC applied the same
  # equation additively, so the two models disagreed on the same coefficient.
  out <- whep::calculate_soc_hsoc(
    initial_soc_mgc_ha = 50,
    c_input_mgc_ha_yr = 2,
    years = 0
  )
  testthat::expect_equal(out$iom[1], 0.049 * 50^1.139, tolerance = 1e-8)
  testthat::expect_equal(out$fresh[1] + out$humus[1], 50 - out$iom[1])
})

test_that("HSOC relaxes from an initial stock toward its equilibrium", {
  # Started above equilibrium the stock must fall toward it and stay above it;
  # started below, rise toward it. This is the transient the carbon balance's
  # forward march depends on, and it is unreachable while the pools are pinned
  # at equilibrium from year 0.
  eq_fresh <- 2 * (1 - 0.3) / 0.48
  eq_humus <- 2 * 0.3 / 0.02
  # The humus pool turns over in 1 / 0.02 = 50 years, so convergence to within
  # 1e-6 of the fixed point needs a horizon many multiples of that.
  above <- whep::calculate_soc_hsoc(200, 2, years = 2000)
  below <- whep::calculate_soc_hsoc(5, 2, years = 2000)
  # Monotone throughout, and strictly moving over the first century, where the
  # transient is still resolvable in double precision (it is exactly flat once
  # converged, so a strict test over the whole horizon would fail on zeros).
  testthat::expect_true(all(diff(above$fresh + above$humus) <= 0))
  testthat::expect_true(all(diff(below$fresh + below$humus) >= 0))
  testthat::expect_true(all(diff(head(above$fresh + above$humus, 100)) < 0))
  testthat::expect_true(all(diff(head(below$fresh + below$humus, 100)) > 0))
  testthat::expect_equal(
    utils::tail(above$fresh + above$humus, 1),
    eq_fresh + eq_humus,
    tolerance = 1e-6
  )
  testthat::expect_equal(
    utils::tail(below$fresh + below$humus, 1),
    eq_fresh + eq_humus,
    tolerance = 1e-6
  )
})

# ---- Century silt is measured, not a signature default -----------------

testthat::test_that(".century_silt falls back EXACTLY to the table", {
  # Any drift here silently moves every existing Century result, so the
  # fallback has to be the tabulated value itself, not a copy of it.
  tabulated <- whep:::.soc_param("century", "defaults", "silt_pct")
  testthat::expect_identical(whep:::.century_silt(NA), tabulated)
  testthat::expect_identical(whep:::.century_silt(NA_real_), tabulated)
  testthat::expect_identical(whep:::.century_silt(28.2), 28.2)
})

testthat::test_that("a Century run with no silt reproduces the old value", {
  # 7.296 yr per unit input at clay 25, cm 1 was the shipped behaviour.
  combos <- tibble::tibble(
    c_input_mgc_ha_yr = 2,
    humified_fraction = 0.13,
    climate_modifier = 1,
    clay_pct = 25
  )
  # No silt_pct column at all: must not warn, must not move.
  testthat::expect_no_warning(
    eq <- whep:::.cb_closed_form_equilibrium("century", combos)
  )
  testthat::expect_equal(eq, 2 * 7.296, tolerance = 1e-4)
})

testthat::test_that("silt is as load-bearing as clay in Century", {
  # Both texture terms are functions of clay PLUS silt, so a silt change
  # must move the equilibrium. HWSD's global mean is 28.2% against the
  # shipped placeholder of 45%.
  ref <- whep:::.cb_century_equilibrium(2, 1, 25, 45)
  real <- whep:::.cb_century_equilibrium(2, 1, 25, 28.2)
  testthat::expect_lt(real, ref)
  testthat::expect_equal(real / ref, 0.891, tolerance = 1e-2)

  # Monotone: more silt, more protection, larger equilibrium.
  silts <- c(5, 20, 35, 50, 70)
  eqs <- vapply(
    silts,
    \(x) whep:::.cb_century_equilibrium(2, 1, 25, x),
    numeric(1)
  )
  testthat::expect_true(all(diff(eqs) > 0))
})

testthat::test_that("calculate_soc_century honours silt_pct", {
  testthat::skip_if_not_installed("deSolve")
  a <- whep::calculate_soc_century(50, 2, 5, clay_pct = 25)
  b <- whep::calculate_soc_century(50, 2, 5, clay_pct = 25, silt_pct = 28.2)
  testthat::expect_false(
    isTRUE(all.equal(dplyr::last(a$soc_total), dplyr::last(b$soc_total)))
  )
  # And the default reproduces the tabulated silt exactly.
  tabulated <- whep:::.soc_param("century", "defaults", "silt_pct")
  c_ <- whep::calculate_soc_century(
    50,
    2,
    5,
    clay_pct = 25,
    silt_pct = tabulated
  )
  testthat::expect_equal(a$soc_total, c_$soc_total)
})
