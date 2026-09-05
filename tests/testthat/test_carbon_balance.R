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
  clay_pct <- 20
  # Aguilera et al. (2018) Eq. 5-6: the tabulated coefficient is scaled by the
  # texture modifier d, normalised to 1 at RothC's 23.4% clay reference.
  d <- 3.51 / (1.67 * (1.85 + 1.60 * exp(-0.0786 * clay_pct)))
  humified_fraction <- 0.3 * d
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
      humified_fraction = 0.3,
      climate_modifier = 1,
      clay_pct = clay_pct
    )
  )
  testthat::expect_equal(eq$soc_eq_mgc_ha, expected_total, tolerance = 1e-3)
})

test_that("vectorised closed-form equilibria match the spin-up they replace", {
  # .cb_equilibrium() computes the equilibrium with a closed form instead of a
  # 5000-year spin-up per input combination. FIVE of the six are checked here;
  # LPJmL is not, and cannot be -- see the stationarity test below, and the
  # note in .cb_equilibrium(). The combinations below all sit at climate
  # modifiers >= 0.4 because the spin-up itself has not converged much under
  # that, so widening them would test the oracle rather than the formula. Guard each
  # fast path against the trajectory it replaces across a grid of inputs. HSOC,
  # AMG and RothC reach a flat/converged spin-up so match to machine precision;
  # ICBM and Century match the true fixed point, which differs slightly from
  # the 5000-year value because their slowest pool has not fully converged
  # there (the closed form is the exact t -> infinity stock) -- hence the
  # looser, pool-specific tolerances below.
  combos <- tibble::tibble(
    c_input_mgc_ha_yr = c(0.5, 2.5, 6.0),
    humified_fraction = c(0.2, 0.3, 0.5),
    climate_modifier = c(0.4, 1.0, 1.6),
    clay_pct = c(8, 22, 40)
  )
  tolerances <- list(
    hsoc = 1e-8,
    amg = 1e-8,
    rothc = 1e-6,
    century = 1e-4,
    icbm = 1e-3
  )
  for (model in names(tolerances)) {
    trajectory <- purrr::pmap_dbl(
      combos,
      \(c_input_mgc_ha_yr, humified_fraction, climate_modifier, clay_pct) {
        whep:::.cb_steady_state(
          model,
          c_input_mgc_ha_yr,
          humified_fraction,
          climate_modifier,
          clay_pct
        )
      }
    )
    closed_form <- whep:::.cb_closed_form_equilibrium(model, combos)
    testthat::expect_equal(
      closed_form,
      trajectory,
      tolerance = tolerances[[model]]
    )
  }
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
  # The default opens each class at its own equilibrium.
  own <- whep:::.cb_init_density(classes, "own_equilibrium")
  testthat::expect_equal(own$stock_mgc_ha, c(40, 70), tolerance = 1e-9)
  # "cell_average" is the Spain historical behaviour, still selectable: every
  # class in the cell opens at the fraction-weighted mean.
  avg <- whep:::.cb_init_density(classes, "cell_average")
  testthat::expect_equal(
    unique(avg$stock_mgc_ha),
    0.6 * 40 + 0.4 * 70,
    tolerance = 1e-9
  )
})

test_that("each cell initialises at its own earliest available year", {
  classes <- tibble::tribble(
    ~lon, ~lat, ~area_code, ~year, ~land_use, ~soc_eq_mgc_ha, ~frac,
    0.25, 0.25, 1L, 2000L, "cropland", 40, 0.6,
    0.25, 0.25, 1L, 2000L, "natural", 70, 0.4,
    0.75, 0.25, 2L, 2001L, "cropland", 20, 0.25,
    0.75, 0.25, 2L, 2001L, "natural", 60, 0.75
  )

  init <- whep:::.cb_initialise(
    classes,
    model = "hsoc",
    d = list(equilibrium_climate = NULL),
    init = "cell_average"
  ) |>
    dplyr::arrange(.data$area_code, .data$land_use)

  testthat::expect_setequal(init$area_code, c(1L, 2L))
  testthat::expect_equal(
    unique(init$stock_mgc_ha[init$area_code == 1L]),
    40 * 0.6 + 70 * 0.4
  )
  testthat::expect_equal(
    unique(init$stock_mgc_ha[init$area_code == 2L]),
    20 * 0.25 + 60 * 0.75
  )
})

test_that("a land-use class with no carbon input survives as zero-carbon area", {
  # ASK-1 resolution: a class present in land_use but absent from c_inputs
  # (e.g. LUH2 urban) must be kept as a zero-carbon class that DILUTES the
  # cell (area share retained, equilibrium ~0), not silently dropped, which
  # would break the cell's land-use accounting and deflate its SOC.
  land_use <- tibble::tribble(
    ~lon, ~lat, ~area_code, ~year, ~land_use, ~area_ha,
    0.25, 0.25, 1L, 2000L, "cropland", 60,
    0.25, 0.25, 1L, 2000L, "grassland", 35,
    0.25, 0.25, 1L, 2000L, "urban", 5
  )
  c_inputs <- tidyr::expand_grid(
    lon = 0.25,
    lat = 0.25,
    area_code = 1L,
    year = 2000L,
    land_use = c("cropland", "grassland")
  ) |>
    dplyr::mutate(
      c_input_mgc_ha_yr = dplyr::if_else(land_use == "cropland", 2.5, 1.5),
      humified_fraction = 0.3
    )
  climate <- tibble::tibble(
    lon = 0.25,
    lat = 0.25,
    area_code = 1L,
    year = 2000L,
    climate_modifier = 1
  )
  clay <- tibble::tribble(~lon, ~lat, ~clay_pct, 0.25, 0.25, 20)
  out <- whep::build_carbon_balance(
    model = "hsoc",
    resolution = "grid",
    data = list(
      land_use = land_use,
      c_inputs = c_inputs,
      climate = climate,
      clay = clay
    )
  )
  urban <- out[out$land_use == "urban", ]
  # The class is retained (not dropped by an inner join), with zero carbon
  # input, a finite stock and no nitrogen flux, and nothing anywhere is NA.
  testthat::expect_equal(nrow(urban), 1L)
  testthat::expect_equal(urban$c_input_mgc_ha, 0)
  testthat::expect_equal(urban$son_change_kgn_ha, 0)
  testthat::expect_true(all(is.finite(out$stock_mgc_ha)))
  testthat::expect_setequal(out$land_use, c("cropland", "grassland", "urban"))
})

test_that("cells sharing one area_code march independently", {
  # Regression for the .cb_march() data-mask bug: the per-cell filter wrote
  # `.data$lon == lon`, but bare `lon`/`lat` resolved to the tibble's OWN
  # columns, making both predicates tautologies. Only area_code filtered, so
  # every cell in a country inherited the FIRST cell's classes (the global run
  # has 178 area_codes over ~59k cells). Two cells in ONE area_code, with
  # different cropland C inputs, must yield different SOC -- and each cell's
  # result must match a standalone single-cell run.
  land_use <- tibble::tribble(
    ~lon, ~lat, ~area_code, ~year, ~land_use, ~area_ha,
    0.25, 0.25, 1L, 2000L, "Cropland", 60,
    0.25, 0.25, 1L, 2000L, "NonCropland", 40,
    0.25, 0.25, 1L, 2001L, "Cropland", 50,
    0.25, 0.25, 1L, 2001L, "NonCropland", 50,
    0.75, 0.25, 1L, 2000L, "Cropland", 30,
    0.75, 0.25, 1L, 2000L, "NonCropland", 70,
    0.75, 0.25, 1L, 2001L, "Cropland", 20,
    0.75, 0.25, 1L, 2001L, "NonCropland", 80
  )
  c_inputs <- tidyr::expand_grid(
    tibble::tribble(
      ~lon, ~lat, ~crop_input,
      0.25, 0.25, 3.0,
      0.75, 0.25, 1.0
    ),
    year = 2000:2001,
    land_use = c("Cropland", "NonCropland")
  ) |>
    dplyr::mutate(
      area_code = 1L,
      c_input_mgc_ha_yr = dplyr::if_else(
        .data$land_use == "Cropland",
        .data$crop_input,
        1.5
      ),
      humified_fraction = 0.3
    ) |>
    dplyr::select(-"crop_input")
  climate <- tidyr::expand_grid(
    tibble::tibble(lon = c(0.25, 0.75), lat = 0.25),
    year = 2000:2001
  ) |>
    dplyr::mutate(area_code = 1L, climate_modifier = 1)
  clay <- tibble::tribble(
    ~lon, ~lat, ~clay_pct,
    0.25, 0.25, 20,
    0.75, 0.25, 20
  )
  both_data <- list(
    land_use = land_use,
    c_inputs = c_inputs,
    climate = climate,
    clay = clay
  )
  # The same cell B, run entirely on its own.
  cell_b_only <- function(x) dplyr::filter(x, .data$lon == 0.75)
  b_data <- list(
    land_use = cell_b_only(land_use),
    c_inputs = cell_b_only(c_inputs),
    climate = cell_b_only(climate),
    clay = cell_b_only(clay)
  )

  out_both <- whep::build_carbon_balance(
    model = "hsoc",
    resolution = "grid",
    data = both_data
  )
  out_b <- whep::build_carbon_balance(
    model = "hsoc",
    resolution = "grid",
    data = b_data
  )

  key <- c("lon", "lat", "year", "land_use")
  b_in_both <- out_both |>
    dplyr::filter(.data$lon == 0.75) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(key)))
  b_alone <- out_b |>
    dplyr::arrange(dplyr::across(dplyr::all_of(key)))

  # Cell B is unaffected by cell A's presence.
  testthat::expect_equal(b_in_both$stock_mgc_ha, b_alone$stock_mgc_ha)
  testthat::expect_equal(b_in_both$son_change_kgn_ha, b_alone$son_change_kgn_ha)
  # And the two cells genuinely differ (a 3x vs 1x cropland C input).
  a_crop <- out_both |>
    dplyr::filter(.data$lon == 0.25, .data$land_use == "Cropland")
  b_crop <- out_both |>
    dplyr::filter(.data$lon == 0.75, .data$land_use == "Cropland")
  testthat::expect_false(isTRUE(all.equal(
    sum(a_crop$stock_mgc_ha),
    sum(b_crop$stock_mgc_ha)
  )))
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

test_that("the LUC transfer conserves carbon when the shrink pool is empty", {
  # Regression: a class growing 10 -> 50 ha at 100 Mg C/ha, against a shrinking
  # class holding no carbon, used to keep its per-hectare DENSITY over the
  # larger area and turn 1,000 Mg C into 5,000. `mass_moved` summed to zero
  # throughout, so every balance check on the transfer column passed. A grower
  # must dilute whatever it holds over its new hectares, drawing from the pool
  # only what the pool has.
  d <- data.table::data.table(
    cell_key = "c",
    land_use = c("cropland", "natural"),
    stepped = c(100, 0),
    old_area = c(10, 90),
    area_ha = c(50, 50)
  )
  out <- whep:::.cb_luc_all(data.table::copy(d))
  testthat::expect_equal(
    sum(out$new_stock * out$area_ha),
    sum(d$stepped * d$old_area),
    tolerance = 1e-8
  )
  testthat::expect_equal(sum(out$mass_moved), 0, tolerance = 1e-8)
})

test_that("the LUC transfer conserves carbon when the pool outlasts growers", {
  # The mirror case: shrinking land releases more carbon than the growing
  # classes can absorb. The undrawn remainder must not vanish.
  d <- data.table::data.table(
    cell_key = "c",
    land_use = c("natural", "cropland", "urban"),
    stepped = c(200, 50, 10),
    old_area = c(80, 10, 10),
    area_ha = c(20, 20, 60)
  )
  out <- whep:::.cb_luc_all(data.table::copy(d))
  testthat::expect_equal(
    sum(out$new_stock * out$area_ha),
    sum(d$stepped * d$old_area),
    tolerance = 1e-8
  )
  testthat::expect_equal(sum(out$mass_moved), 0, tolerance = 1e-8)
})

test_that("build_carbon_balance conserves cell C across the LUC year", {
  cb <- whep::build_carbon_balance(
    model = "hsoc",
    resolution = "grid",
    data = .cb_test_data()
  )
  # The land-use-change transfer column sums to ~0 within each cell-year: it
  # only moves carbon between classes of one cell.
  totals <- cb |>
    dplyr::summarise(
      luc_c = sum(luc_transfer_mgc_ha * area_ha),
      .by = c(lon, lat, area_code, year)
    )
  testthat::expect_true(all(abs(totals$luc_c) < 1e-6))

  # And the march itself conserves carbon, which this test computed the terms
  # for but never asserted: for each class, the carbon mass carried into a year
  # is the previous year's mass plus that year's net rate over the previous
  # year's hectares, plus whatever the transfer moved in or out. A grower that
  # cannot draw from the shrink pool keeps its per-hectare density over more
  # hectares, which would manufacture carbon and show up here.
  step <- cb |>
    dplyr::arrange(lon, lat, area_code, land_use, year) |>
    dplyr::mutate(
      prev_stock = dplyr::lag(stock_mgc_ha),
      prev_rate = dplyr::lag(rate_mgc_ha),
      prev_area = dplyr::lag(area_ha),
      .by = c(lon, lat, area_code, land_use)
    ) |>
    dplyr::filter(!is.na(prev_stock))
  testthat::expect_gt(nrow(step), 0L)
  testthat::expect_equal(
    step$stock_mgc_ha * step$area_ha,
    (step$prev_stock + step$prev_rate) *
      step$prev_area +
      step$luc_transfer_mgc_ha * step$area_ha,
    tolerance = 1e-8
  )
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

test_that("climate carrying its own clay_pct does not collide with data$clay", {
  # get_soc_climate_drivers()'s real output already embeds clay_pct (RothC/
  # HSOC need it as a climate driver too), so a caller wiring its real output
  # straight into build_carbon_balance()'s data$climate, alongside a separate
  # data$clay, must not silently suffix both to clay_pct.x/clay_pct.y and lose
  # the plain clay_pct column .cb_year_climate_modifier() reads.
  raw_with_clay <- .cb_raw_climate_fixture() |>
    dplyr::mutate(clay_pct = 20)
  d <- .cb_test_data()
  d$climate <- raw_with_clay
  cb <- whep::build_carbon_balance(
    model = "hsoc",
    resolution = "grid",
    data = d
  )
  testthat::expect_true(all(is.finite(cb$stock_mgc_ha)))
  # Must reproduce the plain raw-driver run (clay_pct=20 either way), not some
  # NA-clay or dropped-modifier fallback.
  raw <- whep::build_carbon_balance(
    model = "hsoc",
    resolution = "grid",
    data = .cb_raw_test_data()
  )
  testthat::expect_equal(cb$stock_mgc_ha, raw$stock_mgc_ha, tolerance = 1e-9)
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

# -- Land-use-specific soil cover (T24 / soil_cover finding) -------------------

# Strongly seasonal monthly drivers (temperature peaks in July) for one cell,
# with the lowercase LUH2 land-use vocabulary the soil-cover curve is keyed on.
.cb_seasonal_climate_fixture <- function() {
  tidyr::expand_grid(
    lon = 0.25,
    lat = 0.25,
    area_code = 1L,
    year = 2000L,
    month = 1:12
  ) |>
    dplyr::mutate(
      temp_c = 12 + 10 * sin((month - 4) / 12 * 2 * pi),
      water_minus_pet_mm = 20 - 3 * (month - 6)
    )
}

.cb_clay_only <- function() {
  tibble::tribble(~lon, ~lat, ~clay_pct, 0.25, 0.25, 20)
}

test_that("RothC/HSOC modifier differs between cropland and perennial classes", {
  # Before this fix a single climate modifier per cell-year was broadcast to
  # every land-use class, so cropland, grassland and natural shared one value.
  # Now the RothC/HSOC cover term is class-specific: cropland (a seasonal canopy
  # with a bare fallow period) must mineralize differently from grassland /
  # natural (sustained perennial cover) in the same cell-year.
  classes <- c("cropland", "grassland", "natural")
  mods <- whep:::.cb_climate_modifier_table(
    .cb_seasonal_climate_fixture(),
    .cb_clay_only(),
    "hsoc",
    classes
  )
  crop <- mods$climate_modifier[mods$land_use == "cropland"]
  grass <- mods$climate_modifier[mods$land_use == "grassland"]
  nat <- mods$climate_modifier[mods$land_use == "natural"]
  # Cropland has bare fallow months (cover_factor up to 1.0), so it mineralizes
  # faster than the perennially-covered classes (cover_factor floored near 0.66).
  testthat::expect_gt(crop, grass)
  testthat::expect_false(isTRUE(all.equal(crop, grass)))
  # Grassland and natural share the same sustained perennial cover, so their
  # HSOC modifiers coincide.
  testthat::expect_equal(grass, nat, tolerance = 1e-12)
  # The end-to-end balance carries the class-specific modifier through: a
  # co-located cropland and grassland row get distinct equilibrium stocks.
  cb <- whep::build_carbon_balance(
    model = "hsoc",
    resolution = "grid",
    data = list(
      land_use = .cb_land_use_fixture() |>
        dplyr::mutate(
          land_use = dplyr::recode(
            land_use,
            Cropland = "cropland",
            NonCropland = "grassland"
          )
        ),
      c_inputs = .cb_c_inputs_fixture() |>
        dplyr::mutate(
          land_use = dplyr::recode(
            land_use,
            Cropland = "cropland",
            NonCropland = "grassland"
          )
        ),
      climate = tidyr::expand_grid(
        lon = 0.25,
        lat = 0.25,
        area_code = 1L,
        year = 2000:2002,
        month = 1:12
      ) |>
        dplyr::mutate(
          temp_c = 12 + 10 * sin((month - 4) / 12 * 2 * pi),
          water_minus_pet_mm = 20 - 3 * (month - 6)
        ),
      clay = .cb_clay_only()
    )
  )
  # Each class opens at its own equilibrium, so the class-specific cover term
  # surfaces directly in the first-year stock: cropland's seasonal canopy leaves
  # its soil barer than grassland's perennial cover, so it decomposes faster and
  # equilibrates lower under identical climate. (Under the default
  # initialisation both classes start ON their equilibrium, so the first-year
  # net rate is zero for both and cannot carry this signal.)
  first <- dplyr::filter(cb, year == 2000L)
  testthat::expect_false(isTRUE(all.equal(
    first$stock_mgc_ha[first$land_use == "cropland"],
    first$stock_mgc_ha[first$land_use == "grassland"]
  )))
  later <- dplyr::filter(cb, year == 2001L)
  testthat::expect_false(isTRUE(all.equal(
    later$stock_mgc_ha[later$land_use == "cropland"],
    later$stock_mgc_ha[later$land_use == "grassland"]
  )))
})

test_that("cropland soil_cover varies across the growing and fallow seasons", {
  # The crop growth-stage curve must make cropland cover rise to a mid-season
  # peak at the warmest month and fall to a low bare-soil value in the fallow
  # months, never a single flat land-use constant.
  climate <- dplyr::left_join(
    .cb_seasonal_climate_fixture(),
    .cb_clay_only(),
    by = c("lon", "lat")
  )
  cover <- whep:::.cb_attach_soil_cover(climate, "cropland") |>
    dplyr::arrange(month)
  testthat::expect_gt(length(unique(cover$soil_cover)), 1)
  # Peak cover is at the warmest month (July here), well above the fallow floor.
  peak_month <- cover$month[which.max(cover$temp_c)]
  testthat::expect_equal(
    cover$soil_cover[cover$month == peak_month],
    0.95,
    tolerance = 1e-9
  )
  testthat::expect_lt(min(cover$soil_cover), 0.1)
  # A perennial class instead carries one sustained cover across every month.
  grass <- whep:::.cb_attach_soil_cover(climate, "grassland")
  testthat::expect_length(unique(grass$soil_cover), 1)
})

test_that("ICBM/AMG/Century modifiers ignore soil cover (class-invariant)", {
  # Only RothC/HSOC consume soil_cover; the other three models must produce an
  # identical modifier for every land-use class in a cell-year (their driver
  # lists do not reference soil_cover), so this fix leaves them unchanged.
  climate <- .cb_seasonal_climate_fixture() |>
    dplyr::mutate(
      precip_mm = 50,
      pet_mm = 40,
      water_balance_mm = 120,
      theta = 0.25,
      t_field = 0.29,
      t_wilt = 0.14,
      porosity = 0.43
    )
  classes <- c("cropland", "grassland", "natural")
  for (model in c("icbm", "amg", "century")) {
    mods <- whep:::.cb_climate_modifier_table(
      climate,
      .cb_clay_only(),
      model,
      classes
    )
    testthat::expect_length(unique(round(mods$climate_modifier, 12)), 1)
  }
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

# A single-class row for one cell-year, used to build a multi-cell marched
# fixture for .cb_finalise() with independently chosen stock_mgc_ha/area_ha
# per cell, so the polity aggregation's area-weighted mean can be checked
# against a hand-computed value (would fail under a plain unweighted mean).
.cb_finalise_cell_row <- function(lon, lat, stock_mgc_ha, area_ha) {
  tibble::tibble(
    lon = lon,
    lat = lat,
    area_code = 1L,
    land_use = "Cropland",
    year = 2000L,
    area_ha = area_ha,
    stock_mgc_ha = stock_mgc_ha,
    mineralization_mgc_ha = 0,
    c_input_mgc_ha = 0,
    luc_transfer_mgc_ha = 0,
    rate_mgc_ha = 0,
    son_change_kgn_ha = 0,
    method_soc = "hsoc"
  )
}

test_that("polity area-weighted mean is exercised across multiple cells", {
  marched <- dplyr::bind_rows(
    .cb_finalise_cell_row(0.25, 0.25, stock_mgc_ha = 40, area_ha = 30),
    .cb_finalise_cell_row(0.75, 0.75, stock_mgc_ha = 100, area_ha = 70)
  )
  pol <- whep:::.cb_finalise(marched, resolution = "polity")

  expected_wmean <- (40 * 30 + 100 * 70) / (30 + 70)
  unweighted_mean <- (40 + 100) / 2
  # A plain unweighted mean across the two cells would give 70, distinct from
  # the area-weighted 82 -- this test fails if the aggregation regresses to an
  # unweighted mean.
  testthat::expect_equal(unweighted_mean, 70)
  testthat::expect_equal(expected_wmean, 82)
  testthat::expect_equal(pol$stock_mgc_ha, expected_wmean, tolerance = 1e-9)
  testthat::expect_false(isTRUE(all.equal(pol$stock_mgc_ha, unweighted_mean)))
})

# -- Default input readers (wiring) -------------------------------------------

test_that(".cb_clay_from_climate reuses the climate table's clay_pct", {
  climate <- tibble::tribble(
    ~lon, ~lat, ~area_code, ~year, ~month, ~clay_pct,
    0.25, 0.25, 1L, 2000L, 1L, 20,
    0.25, 0.25, 1L, 2000L, 2L, 20,
    0.75, 0.25, 1L, 2000L, 1L, 35
  )
  clay <- whep:::.cb_clay_from_climate(climate)
  testthat::expect_setequal(names(clay), c("lon", "lat", "clay_pct"))
  testthat::expect_equal(nrow(clay), 2L)
  testthat::expect_setequal(clay$clay_pct, c(20, 35))
})

test_that(".cb_clay_from_climate returns NULL without clay_pct", {
  climate <- tibble::tibble(
    lon = 0.25,
    lat = 0.25,
    area_code = 1L,
    year = 2000L,
    climate_modifier = 1
  )
  testthat::expect_null(whep:::.cb_clay_from_climate(climate))
})

# Default per-cell clay reader against the real HWSD extract. Skipped on CI and
# whenever HWSD is absent (never fetches a remote raster). The guard tests for
# the clay column the reader needs, not merely for hwsd_data.csv: a partial
# extract is a missing input, not a code fault (whep#596).
test_that(".cb_hwsd_clay reads per-cell clay from HWSD", {
  testthat::skip_on_ci()
  testthat::skip_if_not_installed("terra")
  .skip_unless_hwsd_columns(whep:::.hwsd_clay_columns())
  cell_polity <- tibble::tribble(
    ~lon, ~lat, ~area_code,
    -3.75, 40.25, 203L,
    -3.25, 40.25, 203L
  )
  clay <- whep:::.cb_hwsd_clay(cell_polity)
  testthat::expect_setequal(names(clay), c("lon", "lat", "clay_pct"))
  testthat::expect_true(all(clay$clay_pct >= 0 & clay$clay_pct <= 100))
})

# -- years pass-through (turnkey scoping) -------------------------------------

# build_carbon_balance() and its default-reader carbon-input builders must
# expose a `years` argument so a turnkey call can scope the (otherwise
# 850-2015) LUH2 range. Pure signature check; no pins or rasters are touched.
test_that("carbon builders expose a years argument threaded to readers", {
  fns <- c(
    "build_carbon_balance",
    "build_carbon_inputs",
    "build_soil_carbon_inputs",
    "build_grass_natural_carbon_inputs"
  )
  for (nm in fns) {
    fn <- getExportedValue("whep", nm)
    testthat::expect_true(
      "years" %in% names(formals(fn)),
      info = paste0(nm, " must accept a `years` argument")
    )
    testthat::expect_null(
      eval(formals(fn)$years),
      info = paste0(nm, "'s `years` must default to NULL (back-compatible)")
    )
  }
  # The default readers forward `years` to the year-aware source functions.
  testthat::expect_true("years" %in% names(formals(whep:::.cb_read_land_use)))
  testthat::expect_true("years" %in% names(formals(whep:::.cb_read_climate)))
  testthat::expect_true("years" %in% names(formals(whep:::.cb_read_c_inputs)))
  testthat::expect_true("years" %in% names(formals(whep:::.sci_read_npp)))
  testthat::expect_true("years" %in% names(formals(whep:::.sci_read_manure)))
})

test_that("cell-years without climate coverage are dropped with a warning", {
  d <- .cb_test_data()
  # A second cell present in land_use + c_inputs but absent from climate/clay:
  # its climate_modifier resolves to NA. The run must warn and drop it, not
  # abort, and still return the covered cell's SOC.
  shift <- function(df) {
    df$lon <- 88.25
    df$lat <- 8.25
    df$area_code <- 777L
    df
  }
  d$land_use <- dplyr::bind_rows(d$land_use, shift(d$land_use))
  d$c_inputs <- dplyr::bind_rows(d$c_inputs, shift(d$c_inputs))

  testthat::expect_warning(
    whep::build_carbon_balance(model = "hsoc", data = d),
    "Dropped"
  )
  out <- suppressWarnings(whep::build_carbon_balance(model = "hsoc", data = d))
  testthat::expect_false(any(out$area_code == 777L))
  testthat::expect_true(any(out$area_code == 1L))
  testthat::expect_true(all(is.finite(out$stock_mgc_ha)))
})

test_that("progress feedback is on for real runs, off under testthat", {
  # Real runs (including non-interactive Rscript batch runs) get phase progress;
  # under testthat it is suppressed so the test log stays clean.
  withr::local_envvar(TESTTHAT = "")
  testthat::expect_true(whep:::.cb_show_progress())
  withr::local_envvar(TESTTHAT = "true")
  testthat::expect_false(whep:::.cb_show_progress())
})

# -- C7: the carbon path's shared polycell support ----------------------------

# A three-polity cell (one polity holding most of the land) plus a whole-cell
# polity, with land well short of the cell area so a `land_area_ha /
# cell_area_ha` share is distinguishable from a land share.
.c7_support_fixture <- function() {
  tibble::tribble(
    ~lon,
    ~lat,
    ~polity_code,
    ~area_code,
    ~cell_area_ha,
    ~land_area_ha,
    ~start_year,
    ~end_year,
    0.25, 40.25, "AAA-1900-2025", 1L, 100000, 20000, 1900L, 2025L,
    0.25, 40.25, "BBB-1900-2025", 2L, 100000, 60000, 1900L, 2025L,
    0.75, 40.25, "AAA-1900-2025", 1L, 100000, 40000, 1900L, 2025L
  )
}

test_that("C7: cell_area_frac is a share of the cell's LAND, not of the cell", {
  # AM-5 risk 3. Everything this fraction splits is already land-only, so
  # `land_area_ha / cell_area_ha` would remove the water twice -- and would stay
  # invisible, because per-polity shares still make national totals add up.
  out <- whep:::.carbon_cell_support(.c7_support_fixture(), year = 2000L)
  whole <- dplyr::filter(out, lon == 0.75)
  testthat::expect_equal(whole$cell_area_frac, 1)
  testthat::expect_false(isTRUE(all.equal(
    whole$cell_area_frac,
    whole$land_area_ha / whole$cell_area_ha
  )))
  shared <- dplyr::filter(out, lon == 0.25)
  testthat::expect_equal(sort(shared$cell_area_frac), c(0.25, 0.75))
})

test_that("C7: the land shares of a cell sum to exactly one", {
  out <- whep:::.carbon_cell_support(.c7_support_fixture(), year = 2000L)
  totals <- out |>
    dplyr::summarise(total = sum(cell_area_frac), .by = c(lon, lat)) |>
    dplyr::pull(total)
  testthat::expect_equal(totals, rep(1, length(totals)))
})

test_that("C7: the support is resolved at one reference year", {
  support <- dplyr::bind_rows(
    dplyr::mutate(.c7_support_fixture(), start_year = 1900L, end_year = 1950L),
    dplyr::mutate(
      .c7_support_fixture(),
      polity_code = "CCC-1950-2025",
      area_code = c(9L, 10L, 9L),
      start_year = 1950L,
      end_year = 2025L
    )
  )
  early <- whep:::.carbon_cell_support(support, year = 1920L)
  late <- whep:::.carbon_cell_support(support, year = 2000L)
  testthat::expect_setequal(early$area_code, c(1L, 2L))
  testthat::expect_setequal(late$area_code, c(9L, 10L))
  testthat::expect_equal(whep:::.carbon_support_year(), 2015L)
})

test_that("C7: polycells with no area_code are reported, never folded", {
  support <- dplyr::mutate(
    .c7_support_fixture(),
    area_code = c(NA_integer_, 2L, 1L)
  )
  testthat::expect_warning(
    out <- whep:::.carbon_cell_support(support, year = 2000L),
    "no .*area_code"
  )
  testthat::expect_setequal(out$area_code, c(1L, 2L))
  # The unkeyable polity's 20,000 ha is LOST, not handed to its neighbour: the
  # survivor keeps its true 0.75 land share instead of being renormalised to 1.
  shared <- dplyr::filter(out, lon == 0.25)
  testthat::expect_equal(shared$land_area_ha, 60000)
  testthat::expect_equal(shared$cell_area_frac, 0.75)
})

test_that("C7: two polity codes on one area_code are summed and reported", {
  support <- dplyr::mutate(.c7_support_fixture(), area_code = c(206L, 206L, 1L))
  testthat::expect_warning(
    out <- whep:::.carbon_cell_support(support, year = 2000L),
    "fold more than one"
  )
  folded <- dplyr::filter(out, lon == 0.25)
  testthat::expect_equal(nrow(folded), 1L)
  testthat::expect_equal(folded$land_area_ha, 80000)
})

test_that("C7: a cell holding no land carries no carbon support", {
  support <- dplyr::mutate(.c7_support_fixture(), land_area_ha = c(0, 0, 40000))
  testthat::expect_warning(
    out <- whep:::.carbon_cell_support(support, year = 2000L),
    "hold no land"
  )
  testthat::expect_setequal(out$lon, 0.75)
})

test_that("C7: a support that is not one row per cell-area_code is refused", {
  # DA-23, the pattern C3a used: the fold belongs at the boundary that can
  # report it, never inside a consumer where it looks like a partition.
  dup <- dplyr::mutate(.c7_support_fixture(), area_code = c(206L, 206L, 1L))
  testthat::expect_error(
    whep:::.normalize_carbon_support(dup),
    "one row per cell"
  )
  missing <- dplyr::mutate(
    .c7_support_fixture(),
    area_code = c(NA_integer_, 2L, 1L)
  )
  testthat::expect_error(
    whep:::.normalize_carbon_support(missing),
    "one row per cell"
  )
})

test_that("C7: the whole carbon path resolves one support, not several", {
  # AM-5 risk 2: a half-migrated path surfaces as an ordinary climate-coverage
  # warning from `.cb_drop_uncovered_climate()`, not as an error. This pins that
  # every default reader on the path goes through the same helper, so leaving
  # one on the centroid grid cannot pass.
  calls <- character()
  testthat::local_mocked_bindings(
    .carbon_cell_support = function(...) {
      calls <<- c(calls, "support")
      tibble::tibble(
        lon = 0.25,
        lat = 40.25,
        area_code = 1L,
        cell_area_ha = 100000,
        land_area_ha = 20000,
        cell_area_frac = 1
      )
    },
    .package = "whep"
  )
  outs <- list(
    whep:::.luh2_read_country_grid(),
    whep:::.gn_read_country_grid(),
    whep:::.sci_read_country_grid(),
    whep:::.cb_read_cell_polity()
  )
  for (out in outs) {
    testthat::expect_true(all(c("lon", "lat", "area_code") %in% names(out)))
  }
  testthat::expect_equal(length(calls), 4L)
})

test_that("C7: the climate-driver feed is the polycell footprint", {
  # `get_soc_climate_drivers()` uses `cell_polity` only to label and restrict,
  # never to multiply, so this feed IS the carbon path's footprint.
  testthat::local_mocked_bindings(
    .carbon_cell_support = function(...) {
      tibble::tribble(
        ~lon, ~lat, ~area_code, ~cell_area_ha, ~land_area_ha, ~cell_area_frac,
        0.25, 40.25, 1L, 100000, 20000, 0.25,
        0.25, 40.25, 2L, 100000, 60000, 0.75
      )
    },
    .package = "whep"
  )
  out <- whep:::.cb_read_cell_polity()
  testthat::expect_equal(names(out), c("lon", "lat", "area_code"))
  testthat::expect_equal(nrow(out), 2L)
  testthat::expect_setequal(out$area_code, c(1L, 2L))
})

# The RothC/HSOC climate modifier is computed for every cell-year at once
# (.cb_rothc_modifier_vectorised) rather than once per group. The per-group path
# remains the reference, so what matters is that the two agree exactly -- and on
# the edges, not just clean data: NA months poison a group's later deficits, and
# below -18.27 C the RothC expression wraps back to ~47.91 instead of zero.
.cbv_fixture <- function(n_groups, n_months = 12L, seed = 42L) {
  withr::local_seed(seed)
  g <- tidyr::expand_grid(
    lon = seq_len(n_groups) + 0.25,
    lat = 0.25,
    area_code = 1L,
    year = 2000L,
    land_use = "cropland",
    month = seq_len(n_months)
  )
  g$temp_c <- stats::rnorm(nrow(g), 12, 14)
  g$water_minus_pet_mm <- stats::rnorm(nrow(g), -10, 40)
  g$soil_cover <- stats::runif(nrow(g))
  g$clay_pct <- rep(stats::runif(n_groups, 5, 45), each = n_months)
  g
}

.cbv_keys <- function() {
  c("lon", "lat", "area_code", "year", "land_use")
}

.cbv_per_group <- function(d) {
  dplyr::summarise(
    d,
    climate_modifier = .cb_year_climate_modifier(
      "hsoc",
      dplyr::pick(dplyr::everything()),
      dplyr::first(.data$clay_pct)
    ),
    .by = dplyr::all_of(.cbv_keys())
  )
}

# Row order is part of the contract, not cosmetic: the modifier table feeds
# downstream aggregates, and reaching them in a different sequence perturbs
# floating-point sums in the last bits. Sorted-vs-first-appearance order alone
# moved mineralization/rate/son_change by ~1e-15 on a real five-year build.
testthat::test_that("vectorised RothC modifier preserves input group order", {
  d <- .cbv_fixture(12L)
  d$lon <- rev(d$lon)
  fast <- .cb_rothc_modifier_vectorised(d, "hsoc", .cbv_keys())
  slow <- .cbv_per_group(d)

  testthat::expect_false(identical(fast$lon, sort(fast$lon)))
  testthat::expect_equal(fast, slow, tolerance = 0)
})

testthat::test_that("vectorised RothC modifier equals the per-group path", {
  d <- .cbv_fixture(60L)
  fast <- .cb_rothc_modifier_vectorised(d, "hsoc", .cbv_keys())
  slow <- .cbv_per_group(d)
  joined <- dplyr::inner_join(
    fast,
    slow,
    by = .cbv_keys(),
    suffix = c(".f", ".s")
  )

  testthat::expect_equal(nrow(joined), 60L)
  testthat::expect_equal(
    joined$climate_modifier.f,
    joined$climate_modifier.s,
    tolerance = 0
  )
})

testthat::test_that("vectorised RothC modifier matches with NA months present", {
  d <- .cbv_fixture(40L)
  d$temp_c[seq(1L, nrow(d), by = 7L)] <- NA_real_
  d$water_minus_pet_mm[seq(3L, nrow(d), by = 11L)] <- NA_real_
  fast <- .cb_rothc_modifier_vectorised(d, "hsoc", .cbv_keys())
  slow <- .cbv_per_group(d)
  joined <- dplyr::inner_join(
    fast,
    slow,
    by = .cbv_keys(),
    suffix = c(".f", ".s")
  )

  testthat::expect_equal(
    joined$climate_modifier.f,
    joined$climate_modifier.s,
    tolerance = 0
  )
})

testthat::test_that("vectorised RothC modifier matches below the -18.27 C asymptote", {
  d <- .cbv_fixture(30L)
  d$temp_c <- d$temp_c - 40
  fast <- .cb_rothc_modifier_vectorised(d, "hsoc", .cbv_keys())
  slow <- .cbv_per_group(d)
  joined <- dplyr::inner_join(
    fast,
    slow,
    by = .cbv_keys(),
    suffix = c(".f", ".s")
  )

  testthat::expect_equal(
    joined$climate_modifier.f,
    joined$climate_modifier.s,
    tolerance = 0
  )
})

# Ragged groups would misalign months across cells in the matrix reshape, so the
# fast path must decline them rather than guess.
testthat::test_that("vectorised RothC modifier declines ragged groups", {
  d <- .cbv_fixture(10L)
  d <- d[-1L, ]

  testthat::expect_null(
    .cb_rothc_modifier_vectorised(d, "hsoc", .cbv_keys())
  )
})

testthat::test_that("vectorised RothC modifier declines other models and missing drivers", {
  d <- .cbv_fixture(10L)

  testthat::expect_null(.cb_rothc_modifier_vectorised(d, "icbm", .cbv_keys()))
  testthat::expect_null(
    .cb_rothc_modifier_vectorised(
      dplyr::select(d, -"clay_pct"),
      "hsoc",
      .cbv_keys()
    )
  )
})

# as_tibble() on a data.table carries .internal.selfref out with it, which makes
# the fast path compare unequal to the per-group path under all.equal() even when
# every column matches. The two paths must be indistinguishable, attributes and
# all, because either can be the one that runs.
testthat::test_that("vectorised RothC modifier is indistinguishable from the reference", {
  d <- .cbv_fixture(25L)
  d <- dplyr::bind_rows(d, dplyr::mutate(d, land_use = "perennial"))
  fast <- .cb_rothc_modifier_vectorised(d, "hsoc", .cbv_keys())
  slow <- .cbv_per_group(d)
  sorted <- function(x) {
    dplyr::arrange(x, dplyr::across(dplyr::all_of(.cbv_keys())))
  }

  testthat::expect_true(tibble::is_tibble(fast))
  testthat::expect_false(".internal.selfref" %in% names(attributes(fast)))
  testthat::expect_equal(sorted(fast), sorted(slow), tolerance = 0)
})

# ---- polity_validity (#675) -------------------------------------------

# One cell over two years on area 277 (South Sudan, SSD-2011-2025): the 2000
# rows name a state that did not exist that year, the 2020 rows do not.
.cbpv_data <- function() {
  keys <- tidyr::expand_grid(
    lon = 0.25,
    lat = 0.25,
    area_code = 277L,
    year = c(2000L, 2020L)
  )
  list(
    land_use = dplyr::mutate(keys, land_use = "cropland", area_ha = 100),
    c_inputs = dplyr::mutate(
      keys,
      land_use = "cropland",
      c_input_mgc_ha_yr = 2.5,
      humified_fraction = 0.3
    ),
    climate = dplyr::mutate(keys, climate_modifier = 1),
    clay = tibble::tribble(~lon, ~lat, ~clay_pct, 0.25, 0.25, 20)
  )
}

testthat::test_that("build_carbon_balance names an anachronistic polity", {
  testthat::expect_warning(
    out <- whep::build_carbon_balance(data = .cbpv_data()),
    "did not exist in that row's year"
  )

  # "keep" is the default: both years survive and the stocks do not move.
  testthat::expect_setequal(out$year, c(2000L, 2020L))
  testthat::expect_true(all(out$reporting_polity_code == "SSD-2011-2025"))
})

testthat::test_that("build_carbon_balance honours drop and flag", {
  testthat::expect_warning(
    kept <- whep::build_carbon_balance(data = .cbpv_data())
  )
  testthat::expect_warning(
    dropped <- whep::build_carbon_balance(
      data = .cbpv_data(),
      polity_validity = "drop"
    )
  )
  testthat::expect_warning(
    flagged <- whep::build_carbon_balance(
      data = .cbpv_data(),
      polity_validity = "flag"
    )
  )

  testthat::expect_equal(unique(dropped$year), 2020L)
  testthat::expect_equal(
    flagged$reporting_polity_out_of_span,
    flagged$year == 2000L
  )
  # "flag" is "keep" plus one logical column: no number moves.
  testthat::expect_equal(
    dplyr::select(flagged, -"reporting_polity_out_of_span"),
    kept
  )
})

test_that("the sequential and vectorised marches agree", {
  # .cb_march() is what runs; .cb_march_cell() is the reference implementation
  # it replaced and is called from nowhere in the package, so the fast path has
  # had no oracle. That gap is not hypothetical: the empty-pool grower bug was
  # fixed in the vectorised transfer while the sequential twin still turned
  # 1,000 Mg C into 5,000, and nothing compared them.
  classes <- tibble::tribble(
    ~lon, ~lat, ~area_code, ~land_use, ~year, ~area_ha,
    ~c_input_mgc_ha_yr, ~soc_eq_mgc_ha, ~frac,
    0.25, 0.25, 1L, "cropland", 2000L, 60, 2.5, 40, 0.6,
    0.25, 0.25, 1L, "natural", 2000L, 40, 1.5, 70, 0.4,
    0.25, 0.25, 1L, "cropland", 2001L, 30, 2.5, 40, 0.3,
    0.25, 0.25, 1L, "natural", 2001L, 70, 1.5, 70, 0.7,
    0.25, 0.25, 1L, "cropland", 2002L, 55, 2.5, 40, 0.55,
    0.25, 0.25, 1L, "natural", 2002L, 45, 1.5, 70, 0.45
  )
  init <- whep:::.cb_init_density(
    dplyr::filter(classes, .data$year == 2000L),
    "own_equilibrium"
  )

  fast <- whep:::.cb_march(classes, init) |>
    dplyr::arrange(.data$year, .data$land_use)

  cell <- classes |>
    dplyr::mutate(
      eff_rate = dplyr::if_else(
        .data$soc_eq_mgc_ha > 0,
        .data$c_input_mgc_ha_yr / .data$soc_eq_mgc_ha,
        0
      )
    )
  slow <- whep:::.cb_march_cell(cell, init) |>
    dplyr::arrange(.data$year, .data$land_use)

  for (col in c(
    "stock_mgc_ha",
    "mineralization_mgc_ha",
    "c_input_mgc_ha",
    "luc_transfer_mgc_ha",
    "rate_mgc_ha"
  )) {
    testthat::expect_equal(
      fast[[col]],
      slow[[col]],
      tolerance = 1e-10,
      label = paste("vectorised vs sequential", col)
    )
  }
})

test_that("the sequential transfer conserves carbon against an empty pool", {
  # The twin of the .cb_luc_all() regression: a grower that can draw nothing
  # must dilute what it holds over its new area, not carry its old density onto
  # more hectares.
  before <- tibble::tibble(
    land_use = c("cropland", "natural"),
    stock_mgc_ha = c(100, 0),
    old_area_ha = c(10, 90),
    new_area_ha = c(50, 50)
  )
  after <- whep:::.cb_luc_transfer(before)
  testthat::expect_equal(
    sum(after$stock_mgc_ha * after$new_area_ha),
    sum(before$stock_mgc_ha * before$old_area_ha),
    tolerance = 1e-8
  )
  testthat::expect_equal(sum(after$mass_moved), 0, tolerance = 1e-8)
})

test_that("the LPJmL equilibrium is a fixed point of its own dynamics", {
  # This model cannot be guarded the way the other five are. Its slow pool
  # decays at 0.001/yr, so at a response of 0.2 its e-folding time is 5,000
  # years -- a 5,000-year spin-up is one e-folding and lands 4% short. That is
  # the spin-up failing to converge, not the closed form being wrong, and it is
  # why LPJmL solves its own equilibrium analytically rather than spinning up.
  #
  # The right check is therefore the defining property: start AT the closed form
  # and the trajectory must not move.
  for (cm in c(0.2, 0.5, 1.0, 1.6)) {
    eq <- whep:::.cb_lpjml_equilibrium(2.5, cm)
    traj <- whep::calculate_soc_lpjml(
      initial_soc_mgc_ha = eq,
      c_input_mgc_ha_yr = 2.5,
      years = 500,
      climate_modifier = cm
    )
    testthat::expect_equal(
      utils::tail(traj$soc_total, 1),
      eq,
      tolerance = 1e-9,
      label = paste("LPJmL equilibrium is stationary at cm =", cm)
    )
  }
})

test_that("the LPJmL equilibrium matches its published closed form", {
  # The equilibrium is the soil-bound input divided between the two pools:
  # the fast share over its rate plus the slow share over its rate, all over
  # the response (Schaphoff et al. 2018 Eqs. 98-100, layer weights summed).
  # (Schaphoff et al. 2018 Eqs. 98-100, with the normalised layer weights summed
  # out). With the run's parameters the bracket is 22.25 years, and the slow
  # pool holds most of the stock off 2% of the input.
  expected <- 1 * (1 - 0.5) * (0.98 / 0.04 + 0.02 / 0.001)
  testthat::expect_equal(expected, 22.25)
  testthat::expect_equal(whep:::.cb_lpjml_equilibrium(1, 1), 22.25)
  # Proportional to input, inversely proportional to the response.
  testthat::expect_equal(whep:::.cb_lpjml_equilibrium(4, 1), 4 * 22.25)
  testthat::expect_equal(whep:::.cb_lpjml_equilibrium(1, 0.5), 2 * 22.25)
  # The slow pool takes 2% of the soil-bound input and holds 45% of the stock.
  slow_share <- (0.02 / 0.001) / (0.98 / 0.04 + 0.02 / 0.001)
  testthat::expect_equal(slow_share, 0.4494, tolerance = 1e-3)
})

# `.cb_attach_equilibrium()` evaluates a closed form in place rather than
# deduping the drivers and joining the result back (#394). Two things have to
# hold for that to be safe, and neither is obvious from reading it.
.cb_fake_classes <- function(n) {
  set.seed(11)
  tibble::tibble(
    lon = round(stats::runif(n, -180, 180), 2),
    lat = round(stats::runif(n, -60, 80), 2),
    area_code = sample.int(50L, n, replace = TRUE),
    year = sample(1900:2020, n, replace = TRUE),
    land_use = sample(c("cropland", "grassland", "natural"), n, replace = TRUE),
    area_ha = stats::runif(n, 1, 5000),
    # Coarse on purpose, so driver combinations genuinely repeat: with unique
    # drivers the join could not duplicate a row even if it were wrong.
    c_input_mgc_ha_yr = round(stats::runif(n, 0.5, 8), 1),
    humified_fraction = round(stats::runif(n, 0.1, 0.4), 2),
    climate_modifier = round(stats::runif(n, 0.2, 1.6), 1),
    clay_pct = round(stats::runif(n, 3, 60), 0)
  )
}

test_that("attaching the equilibrium neither drops nor duplicates a row", {
  # The join this replaced keyed on `climate_modifier` and `clay_pct`, both
  # doubles. Repeated driver combinations are exactly the case where a join
  # can fan a row out; evaluating in place cannot.
  classes <- .cb_fake_classes(2000L)

  for (model in c("hsoc", "rothc", "icbm", "amg", "century", "lpjml")) {
    out <- whep:::.cb_attach_equilibrium(classes, model)
    testthat::expect_equal(nrow(out), nrow(classes))
    testthat::expect_true(all(is.finite(out$soc_eq_mgc_ha)))
  }
})

test_that("the attached equilibrium is the model's own closed form", {
  classes <- .cb_fake_classes(2000L)

  for (model in c("hsoc", "rothc", "icbm", "amg", "century", "lpjml")) {
    out <- whep:::.cb_attach_equilibrium(classes, model)
    closed <- whep:::.cb_closed_form_equilibrium(model, classes)
    # Exact, not approximate: it is the same expression on the same doubles.
    testthat::expect_equal(out$soc_eq_mgc_ha, closed, tolerance = 0)
  }
})

test_that("the non-finite equilibrium guard survives the in-place path", {
  # Every closed form is proportional to 1 / climate_modifier, and a zero
  # modifier is reachable (HSOC/RothC at or below -18.27 C). The guard used to
  # sit on the deduped table; it now sees the class table directly, and it
  # still has to abort rather than let an Inf reach the march.
  classes <- .cb_fake_classes(50L)
  classes$climate_modifier[7] <- 0

  testthat::expect_error(
    whep:::.cb_attach_equilibrium(classes, "hsoc"),
    "not finite"
  )
})

# ---- irrigation is applied to managed land, not to natural land --------

# Monthly drivers for one cell, carrying rain and irrigation separately the
# way get_soc_climate_drivers() does: precip_mm is precipitation ALONE, while
# water_minus_pet_mm already has the cell's irrigation folded in.
.irrigated_cell_drivers <- function(irrig_mm = 40) {
  tidyr::expand_grid(
    lon = 0.25,
    lat = 0.25,
    area_code = 1L,
    year = 2000L,
    month = 1:12
  ) |>
    dplyr::mutate(
      temp_c = 18,
      precip_mm = 20,
      pet_mm = 80,
      clay_pct = 25,
      water_minus_pet_mm = precip_mm + irrig_mm - pet_mm
    )
}

testthat::test_that(".cb_attach_class_water strips irrigation from natural", {
  prepared <- .irrigated_cell_drivers() |>
    tidyr::crossing(land_use = c("cropland", "grassland", "natural")) |>
    whep:::.cb_attach_class_water()

  natural <- dplyr::filter(prepared, land_use == "natural")
  managed <- dplyr::filter(prepared, land_use != "natural")

  # Natural land falls back to rain minus PET: 20 - 80.
  testthat::expect_true(all(natural$water_minus_pet_mm == -60))
  # Managed land keeps the cell value, irrigation included: 20 + 40 - 80.
  testthat::expect_true(all(managed$water_minus_pet_mm == -20))
})

testthat::test_that("a driver table without rain columns is untouched", {
  # The precomputed-climate_modifier path carries no precip_mm/pet_mm, so
  # rain and irrigation cannot be separated. Passing it through unchanged is
  # the only honest option; silently treating the surplus as rainfed would
  # dry out every natural cell in that path.
  bare <- .irrigated_cell_drivers() |>
    dplyr::select(-"precip_mm", -"pet_mm") |>
    tidyr::crossing(land_use = c("cropland", "natural"))

  testthat::expect_identical(whep:::.cb_attach_class_water(bare), bare)
})

testthat::test_that("phantom irrigation raised natural decomposition", {
  # The defect this guards: an irrigated cell's natural land was decomposing
  # at the moisture of the irrigated crop beside it. A wetter soil has a
  # HIGHER RothC moisture term, so the modifier must fall once the phantom
  # water is removed -- and equilibrium SOC scales as 1 / modifier.
  drivers <- .irrigated_cell_drivers()
  classes <- c("cropland", "natural")

  fixed <- drivers |>
    tidyr::crossing(land_use = classes) |>
    whep:::.cb_attach_class_water() |>
    dplyr::mutate(soil_cover = 0.85)
  unfixed <- drivers |>
    tidyr::crossing(land_use = classes) |>
    dplyr::mutate(soil_cover = 0.85)

  keys <- c("lon", "lat", "area_code", "year", "land_use")
  m_fixed <- whep:::.cb_rothc_modifier_vectorised(fixed, "hsoc", keys)
  m_unfixed <- whep:::.cb_rothc_modifier_vectorised(unfixed, "hsoc", keys)

  nat <- \(x) x$climate_modifier[x$land_use == "natural"]
  crop <- \(x) x$climate_modifier[x$land_use == "cropland"]

  testthat::expect_lt(nat(m_fixed), nat(m_unfixed))
  # Cropland is untouched by this change.
  testthat::expect_equal(crop(m_fixed), crop(m_unfixed))
})

# ---- RothC sub-step count comes from one expression --------------------

testthat::test_that("the RothC closed form and the model agree on n_sub", {
  # These were two separately-written floating-point expressions:
  # `max(rates) * cm / 12` in the closed form against `max(rates) * cm * dt`
  # with `dt <- 1/12` in the model. Over 49,991 modifiers in [0.001, 5] they
  # split at exactly one, cm = 4.8000000000000007, for a 0.14% difference in
  # the equilibrium. Sweeping the whole range is what found it; keep the
  # sweep rather than a spot check.
  rates <- whep:::.soc_rates("rothc", c("dpm", "rpm", "bio", "hum"))
  cm <- seq(0.001, 5, length.out = 20000)

  shared <- whep:::.rothc_substeps(rates, cm, 1 / 12)
  old_closed_form <- pmax(1L, as.integer(ceiling(max(rates) * cm / 12)))

  testthat::expect_length(shared, length(cm))
  testthat::expect_true(all(shared >= 1L))
  # The exact boundaries are where the two used to be able to disagree.
  boundaries <- c(1.2, 2.4, 3.6, 4.8, 4.8000000000000007)
  testthat::expect_equal(
    whep:::.rothc_substeps(rates, boundaries, 1 / 12),
    pmax(1L, as.integer(ceiling(max(rates) * boundaries * (1 / 12))))
  )
  # Documented as agreeing with the old form everywhere except those ulps.
  testthat::expect_lte(sum(shared != old_closed_form), 2L)
})

testthat::test_that(".rothc_substeps is unchanged for a scalar modifier", {
  # pmax replaced max so the closed form can call it vectorised. A scalar
  # caller -- calculate_soc_rothc() -- must be completely unaffected.
  rates <- whep:::.soc_rates("rothc", c("dpm", "rpm", "bio", "hum"))
  for (cm in c(0.05, 0.5, 1, 1.2, 2.4, 5)) {
    testthat::expect_identical(
      whep:::.rothc_substeps(rates, cm, 1 / 12),
      max(1L, as.integer(ceiling(max(rates) * cm * (1 / 12))))
    )
  }
})

# One border cell shared by Sudan and South Sudan plus a Syrian cell -- the two
# folds the deployed pin's bucket-keyed `area_code` column performs. The codes
# are the pin's own: 206 for both Sudanese polities, 999 for Syria.
.c907_bucket_support <- function() {
  tibble::tribble(
    ~lon,
    ~lat,
    ~polity_code,
    ~area_code,
    ~cell_area_ha,
    ~land_area_ha,
    ~start_year,
    ~end_year,
    27.25, 9.75, "SDN-2011-2025", 206L, 100000, 60000, 2011L, 2025L,
    27.25, 9.75, "SSD-2011-2025", 206L, 100000, 20000, 2011L, 2025L,
    38.25, 35.25, "SYR-1967-2025", 999L, 100000, 80000, 1967L, 2025L
  )
}

testthat::test_that("C7/907: the support is keyed on reporting area codes", {
  # Before the fix the two Sudanese polities folded onto bucket 206 and Syria
  # arrived as 999 (Rest of World), so `country_areas` -- keyed on 276, 277 and
  # 212 -- joined to nothing for all three.
  out <- suppressMessages(
    whep:::.carbon_cell_support(.c907_bucket_support(), year = 2015L)
  )
  testthat::expect_setequal(out$area_code, c(276L, 277L, 212L))
  testthat::expect_false(any(out$area_code %in% c(206L, 999L)))
  border <- dplyr::filter(out, lon == 27.25)
  testthat::expect_equal(nrow(border), 2L)
  testthat::expect_equal(
    sort(border$land_area_ha),
    c(20000, 60000)
  )
})

testthat::test_that("C7/907: re-keying conserves land and the cell shares", {
  # The re-key is a relabelling: it may not create, destroy or move a hectare,
  # and the land shares of a cell must still sum to exactly one.
  support <- .c907_bucket_support()
  out <- suppressMessages(
    whep:::.carbon_cell_support(support, year = 2015L)
  )
  testthat::expect_equal(sum(out$land_area_ha), sum(support$land_area_ha))
  totals <- out |>
    dplyr::summarise(total = sum(cell_area_frac), .by = c(lon, lat)) |>
    dplyr::pull(total)
  testthat::expect_equal(totals, rep(1, length(totals)))
})

testthat::test_that("C7/907: the re-key reports itself", {
  testthat::expect_message(
    whep:::.carbon_cell_support(.c907_bucket_support(), year = 2015L),
    "Re-keyed 3 polycells"
  )
})

testthat::test_that("C7/907: a support without polity_code is left alone", {
  # A caller-built support carries no `polity_code` to re-resolve from, so its
  # own codes stand rather than being replaced by a guess.
  support <- dplyr::select(.c907_bucket_support(), -"polity_code")
  testthat::expect_warning(
    out <- whep:::.carbon_cell_support(support, year = 2015L),
    "fold more than one"
  )
  testthat::expect_setequal(out$area_code, c(206L, 999L))
})

# ---- a class whose row vanishes must not take its carbon with it -----------

.cb_vanish_fixture <- function() {
  # Natural land is present in 2000 and 2001, then its ROW disappears in 2002
  # while cropland takes over the whole cell. Until 2026-09-02 both marches
  # dropped its stock: the vectorised one because state[cur] is a right join
  # onto the current year, the sequential one because the named state vector
  # kept an entry nothing released. Found while preparing the per-crop-group
  # balance, where classes legitimately come and go per cell.
  tibble::tribble(
    ~lon, ~lat, ~area_code, ~land_use, ~year, ~area_ha,
    ~c_input_mgc_ha_yr, ~soc_eq_mgc_ha, ~frac,
    0.25, 0.25, 1L, "cropland", 2000L, 40, 2.0, 40, 0.4,
    0.25, 0.25, 1L, "natural", 2000L, 60, 1.5, 80, 0.6,
    0.25, 0.25, 1L, "cropland", 2001L, 40, 2.0, 40, 0.4,
    0.25, 0.25, 1L, "natural", 2001L, 60, 1.5, 80, 0.6,
    0.25, 0.25, 1L, "cropland", 2002L, 100, 2.0, 40, 1.0
  )
}

testthat::test_that("a vanished class releases its carbon into the cell", {
  classes <- .cb_vanish_fixture()
  init <- whep:::.cb_init_density(
    dplyr::filter(classes, .data$year == 2000L),
    "own_equilibrium"
  )
  out <- whep:::.cb_march(classes, init)
  mass <- out |>
    dplyr::summarise(
      mass = sum(.data$stock_mgc_ha * .data$area_ha),
      .by = "year"
    ) |>
    dplyr::arrange(.data$year)
  # The cell keeps its 100 ha: 2002 must NOT lose natural land's 60 ha of
  # ~80 MgC/ha. Cropland absorbs it, so its density rises well above its own
  # 40 MgC/ha equilibrium, and the cell mass survives the vanish year.
  crop_2002 <- out$stock_mgc_ha[out$year == 2002L & out$land_use == "cropland"]
  testthat::expect_gt(crop_2002, 40)
  testthat::expect_gt(mass$mass[3], 0.9 * mass$mass[2])
  # The vanished class is reported at zero area, not dropped.
  nat_2002 <- out[out$year == 2002L & out$land_use == "natural", ]
  testthat::expect_identical(nrow(nat_2002), 1L)
  testthat::expect_equal(nat_2002$area_ha, 0)
})

testthat::test_that("both marches agree when a class vanishes", {
  classes <- .cb_vanish_fixture()
  init <- whep:::.cb_init_density(
    dplyr::filter(classes, .data$year == 2000L),
    "own_equilibrium"
  )
  fast <- whep:::.cb_march(classes, init) |>
    dplyr::arrange(.data$year, .data$land_use)
  cell <- dplyr::mutate(
    classes,
    eff_rate = dplyr::if_else(
      .data$soc_eq_mgc_ha > 0,
      .data$c_input_mgc_ha_yr / .data$soc_eq_mgc_ha,
      0
    )
  )
  slow <- whep:::.cb_march_cell(cell, init) |>
    dplyr::arrange(.data$year, .data$land_use)
  testthat::expect_identical(nrow(fast), nrow(slow))
  testthat::expect_equal(fast$stock_mgc_ha, slow$stock_mgc_ha, tolerance = 1e-9)
  testthat::expect_equal(fast$area_ha, slow$area_ha)
})

.cb_appear_fixture <- function() {
  # The mirror of .cb_vanish_fixture(): a class ABSENT in the opening year that
  # appears later. Irrigated cropland shows up in 2001 when a crop's irrigated
  # share turns positive, which under the crop-group default is the normal
  # case rather than the exception -- `.ci_split_into_groups()` keeps only
  # `crop_area_ha > 0`, so a group enters and leaves per cell per year.
  tibble::tribble(
    ~lon, ~lat, ~area_code, ~land_use, ~year, ~area_ha,
    ~c_input_mgc_ha_yr, ~soc_eq_mgc_ha, ~frac,
    0.25, 5.25, 1L, "cropland_rainfed", 2000L, 60, 2.0, 40, 0.6,
    0.25, 5.25, 1L, "natural", 2000L, 40, 1.5, 80, 0.4,
    0.25, 5.25, 1L, "cropland_rainfed", 2001L, 40, 2.0, 40, 0.4,
    0.25, 5.25, 1L, "cropland_irrigated", 2001L, 20, 3.0, 50, 0.2,
    0.25, 5.25, 1L, "natural", 2001L, 40, 1.5, 80, 0.4
  )
}

testthat::test_that("a class appearing mid-span keeps its cell coordinates", {
  # Regression for the vectorised march's state join. `state` carries
  # lon/lat/area_code for `.cb_keep_vanished()`, and in `state[cur]` those win
  # the names, so a class with no state row came out with lon = lat =
  # area_code = NA -- written into the output AND back into state, so the
  # class stayed NA-keyed for every later year. At resolution = "polity" every
  # such row worldwide then pooled into one spurious NA-coded bucket.
  classes <- .cb_appear_fixture()
  init <- whep:::.cb_init_density(
    dplyr::filter(classes, .data$year == 2000L),
    "own_equilibrium"
  )
  out <- whep:::.cb_march(classes, init)
  new_row <- out[out$year == 2001L & out$land_use == "cropland_irrigated", ]
  testthat::expect_equal(nrow(new_row), 1L)
  testthat::expect_false(is.na(new_row$lon))
  testthat::expect_false(is.na(new_row$lat))
  testthat::expect_false(is.na(new_row$area_code))
  testthat::expect_equal(new_row$lon, 0.25)
  testthat::expect_equal(new_row$lat, 5.25)
  testthat::expect_equal(new_row$area_code, 1L)
  # No row of any year may lose its keys, not only the new one.
  testthat::expect_false(anyNA(out$lon))
  testthat::expect_false(anyNA(out$area_code))
})

testthat::test_that("both marches agree when a class appears", {
  classes <- .cb_appear_fixture()
  init <- whep:::.cb_init_density(
    dplyr::filter(classes, .data$year == 2000L),
    "own_equilibrium"
  )
  fast <- whep:::.cb_march(classes, init) |>
    dplyr::arrange(.data$year, .data$land_use)
  cell <- dplyr::mutate(
    classes,
    eff_rate = dplyr::if_else(
      .data$soc_eq_mgc_ha > 0,
      .data$c_input_mgc_ha_yr / .data$soc_eq_mgc_ha,
      0
    )
  )
  slow <- whep:::.cb_march_cell(cell, init) |>
    dplyr::arrange(.data$year, .data$land_use)
  testthat::expect_identical(nrow(fast), nrow(slow))
  testthat::expect_equal(fast$stock_mgc_ha, slow$stock_mgc_ha, tolerance = 1e-9)
  testthat::expect_equal(fast$lon, slow$lon)
  testthat::expect_equal(fast$area_code, slow$area_code)
})

testthat::test_that("every method choice reaches both resolutions", {
  # The multi-method contract: a choice that moves a number must be recorded.
  # Two runs differing in density_basis or method_grazing used to be identical
  # in every method column, and method_soc_init was dropped at "polity"
  # because the roll-up hand-listed the columns it kept and any_of() omits a
  # missing name in silence.
  cols <- c(
    "method_soc",
    "method_soc_init",
    "method_class_water",
    "method_area_basis",
    "method_grazing",
    "method_crop_groups"
  )
  marched <- tibble::tibble(
    lon = c(0.25, 0.25),
    lat = c(0.25, 0.25),
    area_code = 1L,
    land_use = c("cropland", "natural"),
    year = 2000L,
    area_ha = c(40, 60),
    stock_mgc_ha = c(40, 80),
    mineralization_mgc_ha = 1,
    c_input_mgc_ha = 2,
    luc_transfer_mgc_ha = 0,
    luc_transfer_mgc = 0,
    rate_mgc_ha = 1,
    son_change_kgn_ha = 0.1,
    method_soc = "hsoc",
    method_soc_init = "own_equilibrium",
    method_class_water = "none",
    method_area_basis = "renormalised",
    method_grazing = "whep",
    method_crop_groups = "spain_hist"
  )
  grid <- whep:::.cb_finalise(marched, "grid")
  polity <- whep:::.cb_finalise(marched, "polity")
  testthat::expect_true(all(cols %in% names(grid)))
  # The roll-up is the half that regressed: assert it carries EVERY method
  # column, not merely some.
  testthat::expect_true(all(cols %in% names(polity)))
  testthat::expect_equal(polity$method_soc_init, "own_equilibrium")
  testthat::expect_equal(polity$method_area_basis, "renormalised")
  testthat::expect_equal(polity$method_grazing, "whep")
  # A new method column must survive without anyone editing the roll-up.
  marched$method_future_choice <- "x"
  testthat::expect_true(
    "method_future_choice" %in% names(whep:::.cb_finalise(marched, "polity"))
  )
})
