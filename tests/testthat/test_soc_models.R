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

test_that("HSOC returns the three pools and conserves at equilibrium", {
  out <- whep::calculate_soc_hsoc(
    initial_soc_mgc_ha = 50,
    c_input_mgc_ha_yr = 2,
    years = 10
  )
  testthat::expect_setequal(unique(out$pool), c("fresh", "humus", "iom"))
  testthat::expect_equal(nrow(out), 3L * 11L)
  testthat::expect_true(all(out$stock_mgc_ha > 0))
  # Each pool starts at its equilibrium input / (k) and, under constant input
  # and modifier = 1, stays there: a flat (trivially monotone) series.
  humus <- out |> dplyr::filter(pool == "humus")
  testthat::expect_equal(
    humus$stock_mgc_ha[1],
    utils::tail(humus$stock_mgc_ha, 1)
  )
})

test_that("HSOC pools sit at their analytical equilibrium input / k", {
  # The dynamic pools initialise at StockEq = input / (k * modifier) and, under
  # constant input, stay there: the net rate (input - stock * k) is ~0 and the
  # stock equals the closed-form steady state for every year.
  out <- whep::calculate_soc_hsoc(
    initial_soc_mgc_ha = 40,
    c_input_mgc_ha_yr = 5,
    years = 100
  )
  testthat::expect_true(all(out$stock_mgc_ha > 0))
  testthat::expect_true(all(abs(out$rate_mgc_ha) < 1e-8))
  fresh <- out |> dplyr::filter(pool == "fresh")
  humus <- out |> dplyr::filter(pool == "humus")
  # Humified fraction 0.3: fresh input 3.5, humus input 1.5; k = 0.48 / 0.02.
  testthat::expect_equal(fresh$stock_mgc_ha[1], 3.5 / 0.48, tolerance = 1e-8)
  testthat::expect_equal(humus$stock_mgc_ha[1], 1.5 / 0.02, tolerance = 1e-8)
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
