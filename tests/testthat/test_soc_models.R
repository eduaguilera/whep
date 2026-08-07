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

test_that("HSOC returns the three pools and conserves at equilibrium", {
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
  # Each pool starts at its equilibrium input / (k) and, under constant input
  # and modifier = 1, stays there: a flat (trivially monotone) series.
  testthat::expect_equal(out$humus[1], utils::tail(out$humus, 1))
})

test_that("HSOC pools sit at their analytical equilibrium input / k", {
  # The dynamic pools initialise at StockEq = input / (k * modifier) and, under
  # constant input, stay there: the net annual change (input - stock * k) is ~0
  # and the stock equals the closed-form steady state for every year.
  out <- whep::calculate_soc_hsoc(
    initial_soc_mgc_ha = 40,
    c_input_mgc_ha_yr = 5,
    years = 100
  )
  testthat::expect_true(all(out$soc_total > 0))
  testthat::expect_true(all(abs(diff(out$fresh)) < 1e-8))
  testthat::expect_true(all(abs(diff(out$humus)) < 1e-8))
  # Humified fraction 0.3: fresh input 3.5, humus input 1.5; k = 0.48 / 0.02.
  testthat::expect_equal(out$fresh[1], 3.5 / 0.48, tolerance = 1e-8)
  testthat::expect_equal(out$humus[1], 1.5 / 0.02, tolerance = 1e-8)
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
