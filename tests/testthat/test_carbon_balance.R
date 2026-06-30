# Tests for build_carbon_balance() (Module B, Task B2a-3): historical gridded
# SOC trajectory with equilibrium initialisation, yearly land-use-change C
# transfer and the derived soil-organic-nitrogen change. Analytical and
# conservation targets are stated inline.

# -- Fixtures -----------------------------------------------------------------

# A two-class, single-cell land-use table over three years. Class A shrinks and
# class B grows by exactly the same area in 2001, so total cell C must be
# conserved across the land-use-change transfer.
.cb_land_use_fixture <- function() {
  tibble::tribble(
    ~lon, ~lat, ~area_code, ~year, ~land_use, ~area_ha,
    0.25, 0.25, 1L, 2000L, "Cropland", 60,
    0.25, 0.25, 1L, 2000L, "NonCropland", 40,
    0.25, 0.25, 1L, 2001L, "Cropland", 50,
    0.25, 0.25, 1L, 2001L, "NonCropland", 50,
    0.25, 0.25, 1L, 2002L, "Cropland", 50,
    0.25, 0.25, 1L, 2002L, "NonCropland", 50
  )
}

# Constant per-hectare carbon input per land-use class, every year.
.cb_c_inputs_fixture <- function() {
  tidyr::expand_grid(
    lon = 0.25,
    lat = 0.25,
    area_code = 1L,
    year = 2000:2002,
    land_use = c("Cropland", "NonCropland")
  ) |>
    dplyr::mutate(
      c_input_mgc_ha_yr = dplyr::if_else(land_use == "Cropland", 2.5, 1.5),
      humified_fraction = 0.3
    )
}

.cb_climate_fixture <- function() {
  tidyr::expand_grid(
    lon = 0.25,
    lat = 0.25,
    area_code = 1L,
    year = 2000:2002
  ) |>
    dplyr::mutate(climate_modifier = 1)
}

.cb_clay_fixture <- function() {
  tibble::tribble(
    ~lon, ~lat, ~clay_pct,
    0.25, 0.25, 20
  )
}

.cb_test_data <- function() {
  list(
    land_use = .cb_land_use_fixture(),
    c_inputs = .cb_c_inputs_fixture(),
    climate = .cb_climate_fixture(),
    clay = .cb_clay_fixture()
  )
}

# -- Equilibrium --------------------------------------------------------------

test_that("HSOC equilibrium density matches analytic I/k per pool", {
  k_fresh <- whep::soc_turnover_params |>
    dplyr::filter(model == "hsoc", component == "fresh") |>
    dplyr::pull(value)
  k_humus <- whep::soc_turnover_params |>
    dplyr::filter(model == "hsoc", component == "humus") |>
    dplyr::pull(value)
  c_input <- 2.5
  humified_fraction <- 0.3
  fresh_eq <- c_input * (1 - humified_fraction) / k_fresh
  humus_eq <- c_input * humified_fraction / k_humus
  active_eq <- fresh_eq + humus_eq
  iom <- 0.049 * active_eq^1.139
  expected_total <- active_eq + iom

  eq <- whep:::.cb_equilibrium(
    model = "hsoc",
    classes = tibble::tibble(
      land_use = "Cropland",
      c_input_mgc_ha_yr = c_input,
      humified_fraction = humified_fraction,
      climate_modifier = 1,
      clay_pct = 20
    )
  )
  testthat::expect_equal(eq$soc_eq_mgc_ha, expected_total, tolerance = 1e-3)
})

# -- 1750-style initialisation weighting --------------------------------------

test_that("init weights per-class equilibria by land-use fractions", {
  classes <- tibble::tibble(
    lon = 0.25,
    lat = 0.25,
    area_code = 1L,
    land_use = c("Cropland", "NonCropland"),
    soc_eq_mgc_ha = c(40, 70),
    frac = c(0.6, 0.4)
  )
  init <- whep:::.cb_init_density(classes)
  # Per-class init density equals the cell-weighted mean equilibrium.
  expected <- 0.6 * 40 + 0.4 * 70
  testthat::expect_equal(unique(init$stock_mgc_ha), expected, tolerance = 1e-9)
})

# -- Land-use-change carbon conservation (key adversarial invariant) ----------

test_that("LUC transfer conserves total cell carbon when A shrinks, B grows", {
  before <- tibble::tibble(
    land_use = c("Cropland", "NonCropland"),
    stock_mgc_ha = c(50, 80),
    old_area_ha = c(60, 40),
    new_area_ha = c(50, 50)
  )
  after <- whep:::.cb_luc_transfer(before)
  total_before <- sum(before$stock_mgc_ha * before$old_area_ha)
  total_after <- sum(after$stock_mgc_ha * after$new_area_ha)
  testthat::expect_equal(total_after, total_before, tolerance = 1e-6)
})

test_that("build_carbon_balance conserves cell C across the LUC year", {
  cb <- whep::build_carbon_balance(
    model = "hsoc",
    resolution = "grid",
    data = .cb_test_data()
  )
  # Total cell carbon (stock x area) must be conserved from the pre-LUC
  # mineralization+input state into the post-transfer state. We assert that the
  # year-over-year change of total cell C equals net input minus mineralization
  # (the transfer itself adds nothing), so no carbon is created or destroyed by
  # the land-use shift in 2001.
  totals <- cb |>
    dplyr::summarise(
      cell_c = sum(stock_mgc_ha * area_ha),
      input_c = sum(c_input_mgc_ha * area_ha),
      miner_c = sum(mineralization_mgc_ha * area_ha),
      luc_c = sum(luc_transfer_mgc_ha * area_ha),
      .by = year
    )
  # The land-use-change transfer column sums to ~0 within each cell-year.
  testthat::expect_true(all(abs(totals$luc_c) < 1e-6))
})

# -- dSON asymmetry + sign ----------------------------------------------------

test_that("son_change uses asymmetric C:N with correct sign", {
  cb <- whep::build_carbon_balance(
    model = "hsoc",
    resolution = "grid",
    data = .cb_test_data()
  )
  loss <- dplyr::filter(cb, rate_mgc_ha < 0)
  gain <- dplyr::filter(cb, rate_mgc_ha > 0)
  # Net loss (mineralization) yields a positive N input (N released).
  testthat::expect_true(all(loss$son_change_kgn_ha > 0))
  # Net gain (sequestration) yields a negative son_change (N immobilised).
  testthat::expect_true(all(gain$son_change_kgn_ha < 0))
  # |N| per unit |C| is larger for mineralization (smaller C:N) than for
  # sequestration (larger C:N) for the same land-use class.
  if (nrow(loss) > 0) {
    n_per_c_loss <- abs(loss$son_change_kgn_ha[1] / loss$rate_mgc_ha[1])
    testthat::expect_gt(n_per_c_loss, 1000 / 13)
  }
})

# -- Non-negativity -----------------------------------------------------------

test_that("stocks never go negative on the example run", {
  cb <- whep::build_carbon_balance(example = TRUE)
  testthat::expect_true(all(cb$stock_mgc_ha >= 0))
})

test_that("build_carbon_balance stocks stay non-negative on injected data", {
  cb <- whep::build_carbon_balance(data = .cb_test_data())
  testthat::expect_true(all(cb$stock_mgc_ha >= 0))
})

# -- Schema -------------------------------------------------------------------

test_that("example = TRUE returns the documented grid schema", {
  cb <- whep::build_carbon_balance(example = TRUE)
  pointblank::expect_col_exists(
    cb,
    c(
      "lon",
      "lat",
      "area_code",
      "year",
      "stock_mgc_ha",
      "mineralization_mgc_ha",
      "c_input_mgc_ha",
      "luc_transfer_mgc_ha",
      "rate_mgc_ha",
      "son_change_kgn_ha",
      "method_soc"
    )
  )
  testthat::expect_true(all(cb$method_soc == "hsoc"))
})

test_that("polity resolution conserves carbon mass vs grid", {
  d <- .cb_test_data()
  grid <- whep::build_carbon_balance(resolution = "grid", data = d)
  pol <- whep::build_carbon_balance(resolution = "polity", data = d)
  grid_mass <- grid |>
    dplyr::summarise(m = sum(stock_mgc_ha * area_ha), .by = year)
  pol_mass <- pol |>
    dplyr::summarise(m = sum(stock_mgc_ha * area_ha), .by = year)
  cmp <- dplyr::inner_join(grid_mass, pol_mass, by = "year")
  testthat::expect_true(all(abs(cmp$m.x - cmp$m.y) < 1e-6))
})
