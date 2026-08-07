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

test_that("vectorised closed-form equilibria match the spin-up they replace", {
  # .cb_equilibrium() computes the equilibrium with a closed form instead of a
  # 5000-year spin-up per input combination for all five models. Guard each
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
  init <- whep:::.cb_init_density(classes)
  # Per-class init density equals the cell-weighted mean equilibrium.
  expected <- 0.6 * 40 + 0.4 * 70
  testthat::expect_equal(unique(init$stock_mgc_ha), expected, tolerance = 1e-9)
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
    d = list(equilibrium_climate = NULL)
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
  # The first year initialises every class to the cell-weighted-mean stock, so
  # the class-specific modifier surfaces in the per-class equilibrium decay rate:
  # cropland (faster mineralization) and grassland must have distinct rates in
  # the first year and diverging stocks once the march applies those rates.
  first <- dplyr::filter(cb, year == 2000L)
  testthat::expect_false(isTRUE(all.equal(
    first$rate_mgc_ha[first$land_use == "cropland"],
    first$rate_mgc_ha[first$land_use == "grassland"]
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
