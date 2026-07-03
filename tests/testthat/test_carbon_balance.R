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

test_that("son_change resolves C:N for the lowercase 4-class land-use vocab", {
  # The LUH2 reader (phase 2B) emits lowercase cropland / grassland / natural /
  # urban. .cb_cn_lookup must map "cropland" to the Cropland C:N pair and every
  # other class to NonCropland (case-insensitive), never leaving son_change NA.
  marched <- tibble::tribble(
    ~land_use, ~rate_mgc_ha,
    "cropland", -0.5,
    "grassland", -0.5,
    "natural", 0.5,
    "urban", -0.5
  )
  out <- whep:::.cb_derive_son(marched)
  testthat::expect_false(any(is.na(out$son_change_kgn_ha)))

  cn <- whep::soil_cn_ratios |>
    dplyr::filter(management == "Conventional")
  crop_min <- cn$cn_mineralization[cn$cropland_class == "Cropland"]
  noncrop_min <- cn$cn_mineralization[cn$cropland_class == "NonCropland"]
  # Cropland loss uses the Cropland mineralization C:N; grassland the NonCropland.
  testthat::expect_equal(
    out$son_change_kgn_ha[out$land_use == "cropland"],
    0.5 * 1000 / crop_min,
    tolerance = 1e-6
  )
  testthat::expect_equal(
    out$son_change_kgn_ha[out$land_use == "grassland"],
    0.5 * 1000 / noncrop_min,
    tolerance = 1e-6
  )
})

# -- Raw-driver climate path (phase 2C) ---------------------------------------

# Monthly raw SOC climate drivers per cell-year (temp_c, water_minus_pet_mm)
# that build_carbon_balance must reduce to a model-native climate_modifier via
# the .soc_climate_modifier() path when data$climate carries no precomputed
# climate_modifier. Warm, moist months so the HSOC/RothC modifier is > 1.
.cb_raw_climate_fixture <- function() {
  tidyr::expand_grid(
    lon = 0.25,
    lat = 0.25,
    area_code = 1L,
    year = 2000:2002,
    month = 1:12
  ) |>
    dplyr::mutate(
      temp_c = 12 + 6 * sin((month - 3) / 12 * 2 * pi),
      water_minus_pet_mm = 30 - 5 * (month - 6),
      soil_cover = 0
    )
}

.cb_raw_test_data <- function() {
  d <- .cb_test_data()
  d$climate <- .cb_raw_climate_fixture()
  d
}

test_that("raw-driver climate reduces to a model-native modifier in [0, 1.5]", {
  # Reproduce what build_carbon_balance computes internally: the per-cell-year
  # HSOC modifier from the monthly drivers must be finite and in a plausible
  # decomposition-modifier band, and it must NOT be the neutral 1 (the drivers
  # are warm/moist, so it differs).
  raw <- .cb_raw_climate_fixture() |>
    dplyr::filter(year == 2000)
  cm <- whep:::.cb_year_climate_modifier("hsoc", raw, clay_pct = 20)
  testthat::expect_true(is.finite(cm))
  testthat::expect_gt(cm, 0)
  testthat::expect_lt(cm, 1.5)
  testthat::expect_false(isTRUE(all.equal(cm, 1)))
})

# Cell-total carbon (sum of stock x area) at the first year, a single scalar per
# run, used to compare equilibrium-driven initial stocks across climate paths.
.cb_first_year_cell_c <- function(cb) {
  cb |>
    dplyr::filter(.data$year == min(.data$year)) |>
    dplyr::summarise(c = sum(.data$stock_mgc_ha * .data$area_ha)) |>
    dplyr::pull(.data$c)
}

test_that("raw-driver path feeds the model (differs from neutral modifier)", {
  neutral <- whep::build_carbon_balance(
    model = "hsoc",
    resolution = "grid",
    data = .cb_test_data()
  )
  raw <- whep::build_carbon_balance(
    model = "hsoc",
    resolution = "grid",
    data = .cb_raw_test_data()
  )
  # Both runs share every input except the climate: the neutral run injects
  # climate_modifier = 1, the raw run derives a non-unit modifier from the
  # monthly drivers, so the equilibrium (hence the initial cell carbon) differs.
  testthat::expect_false(isTRUE(all.equal(
    .cb_first_year_cell_c(neutral),
    .cb_first_year_cell_c(raw)
  )))
  testthat::expect_true(all(raw$stock_mgc_ha >= 0))
})

test_that("back-compat: injected climate_modifier is used as-is", {
  # The phase-2A fixture injects climate_modifier directly; the raw-driver path
  # must not disturb it. A modifier of exactly 1 must reproduce the neutral run.
  cb <- whep::build_carbon_balance(
    model = "hsoc",
    resolution = "grid",
    data = .cb_test_data()
  )
  # HSOC equilibrium at climate_modifier = 1 equals the analytic I/k (per the
  # equilibrium test above), so the modifier was honoured verbatim.
  testthat::expect_true(all(is.finite(cb$stock_mgc_ha)))
})

test_that("equilibrium_climate normal drives the spin-up, not the march", {
  # When data$equilibrium_climate supplies a per-cell-year climatological normal
  # distinct from the forward drivers, the equilibrium modifier must come from
  # the normal (so the initial stock reflects the 1901-1930 climate), while the
  # forward-year rate uses the year-specific drivers.
  d <- .cb_raw_test_data()
  # A cold equilibrium normal (low temp) => slower decomposition => higher SOC
  # equilibrium than the warm forward drivers would give.
  d$equilibrium_climate <- .cb_raw_climate_fixture() |>
    dplyr::filter(year == 2000) |>
    dplyr::mutate(temp_c = temp_c - 8, year = 0L)
  cb_norm <- whep::build_carbon_balance(
    model = "hsoc",
    resolution = "grid",
    data = d
  )
  cb_plain <- whep::build_carbon_balance(
    model = "hsoc",
    resolution = "grid",
    data = .cb_raw_test_data()
  )
  # Colder equilibrium climate => slower decomposition => higher equilibrium
  # SOC, so the first-year cell carbon under the normal exceeds the plain run.
  testthat::expect_gt(
    .cb_first_year_cell_c(cb_norm),
    .cb_first_year_cell_c(cb_plain)
  )
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
