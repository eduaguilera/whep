# build_cell_moisture_regime(): the wrapper that assembles CRU climate, the
# LPJmL irrigation depth and WHEP's regime areas for the wet/dry classifier.
# Offline throughout: every input is injected through `data`.

# Cell A (9.25, 47.75) is shared by polities 79 and 211; cell B (9.75, 47.75)
# is polity 79's. A is dry on precipitation alone (600 / 1000), B wet
# (1200 / 1000); both are temperate (12 degC, 40 frost days).
.cmw_climate <- function(years = 1981:2010) {
  tidyr::expand_grid(
    tibble::tibble(
      lon = c(9.25, 9.75),
      lat = 47.75,
      annual_precip_mm = c(600, 1200)
    ),
    year = as.integer(years)
  ) |>
    dplyr::mutate(
      mean_annual_temp_c = 12,
      annual_pet_mm = 1000,
      annual_frost_days = 40
    )
}

# LPJmL at the crop grain. In A: irrigated maize 120 mm on a 0.4 stand,
# irrigated "others" 360 mm on a 0.05 stand, and rainfed maize. In B: rainfed
# maize only.
.cmw_crop <- function(years = 1981:2010) {
  stands <- tibble::tribble(
    ~lon, ~lat,  ~lpjml_crop, ~irrig_mm_irrigated, ~stand_frac_irrigated,
    9.25, 47.75, "maize",     120,                 0.4,
    9.25, 47.75, "others",    360,                 0.05,
    9.75, 47.75, "maize",     NA,                  NA
  ) |>
    dplyr::mutate(
      irrig_mm_rainfed = 0,
      stand_frac_rainfed = c(0.3, 0.1, 0.5)
    )
  tidyr::expand_grid(stands, year = as.integer(years)) |>
    dplyr::mutate(
      lpjml_version = "LPJmL C Version 6.1.1",
      lpjml_git_hash = "e6e6c42b88354f4a0df945e55f2ccf3c4a3cb605",
      lpjml_run = "fixture/scenario_1"
    )
}

# WHEP's regime areas. Maize (56 -> 2514) is irrigated in A for polity 79.
# Vegetables, Other (2605) pools green corn (446, on LPJmL maize) and
# cabbages (358, on "others"), both irrigated in A. Polity 211 and cell B grow
# rainfed maize only.
.cmw_areas <- function(years = 1981:2010, extra = NULL) {
  base <- tibble::tribble(
    ~lon, ~lat,  ~area_code, ~item_prod_code, ~rainfed_ha, ~irrigated_ha,
    9.25, 47.75, 79L,        56L,             30,          50,
    9.25, 47.75, 79L,        446L,            0,           10,
    9.25, 47.75, 79L,        358L,            5,           90,
    9.25, 47.75, 211L,       56L,             20,          0,
    9.75, 47.75, 79L,        56L,             40,          0
  )
  tidyr::expand_grid(dplyr::bind_rows(base, extra), year = as.integer(years))
}

.cmw_data <- function(years = 1981:2010, ...) {
  list(
    climate = .cmw_climate(years),
    crop_depth = .cmw_crop(years),
    regime_areas = .cmw_areas(years, ...)
  )
}

.cmw_row <- function(out, lon, area, cbs, regime = "irrigated") {
  out[
    out$lon == lon &
      out$area_code == area &
      out$item_cbs_code == cbs &
      out$water_regime == regime,
  ]
}

testthat::test_that("the output carries the documented columns", {
  testthat::skip_if_not_installed("pointblank")
  out <- whep::build_cell_moisture_regime(
    years = 2010L,
    window = "annual",
    data = .cmw_data(2010L)
  )
  out |>
    pointblank::expect_col_exists(
      c(names(whep:::.cmr_prototype()), "reporting_polity_code")
    )
  out |>
    pointblank::expect_col_vals_in_set("moisture_regime", c("wet", "dry"))
  out |>
    pointblank::expect_col_vals_not_null("method_moisture_regime")
  # One row per cell, polity, balance item and regime with hectares.
  testthat::expect_identical(nrow(out), 6L)
  testthat::expect_identical(
    anyDuplicated(out[c(whep:::.cmr_row_key(), "year")]),
    0L
  )
})

testthat::test_that("CRU's columns reach the classifier under its names", {
  out <- whep::build_cell_moisture_regime(
    years = 2010L,
    window = "annual",
    data = .cmw_data(2010L)
  )
  b <- .cmw_row(out, 9.75, 79L, 2514L, "rainfed")
  testthat::expect_equal(b$mat_c, 12)
  testthat::expect_equal(b$prec_mm, 1200)
  testthat::expect_equal(b$pet_mm, 1000)
  testthat::expect_equal(b$frost_days, 40)
  testthat::expect_identical(b$moisture_regime, "wet")
  d <- .cmw_data(2010L)
  d$climate <- dplyr::rename(d$climate, frs = "annual_frost_days")
  testthat::expect_error(
    whep::build_cell_moisture_regime(2010L, window = "annual", data = d),
    "annual_frost_days"
  )
})

testthat::test_that("rainfed rows take the cell's precipitation alone", {
  out <- whep::build_cell_moisture_regime(
    years = 2010L,
    window = "annual",
    data = .cmw_data(2010L)
  )
  rainfed <- out[out$water_regime == "rainfed", ]
  testthat::expect_true(all(rainfed$irrig_mm == 0))
  testthat::expect_true(all(rainfed$method_irrig_depth == "rainfed_regime"))
  # Both polities of cell A share its climate, and so its class.
  a <- rainfed[rainfed$lon == 9.25, ]
  testthat::expect_setequal(a$area_code, c(79L, 211L))
  testthat::expect_true(all(a$water_mm == 600))
  testthat::expect_true(all(a$moisture_regime == "dry"))
  testthat::expect_true(all(is.na(rainfed$lpjml_run)))
})

testthat::test_that("irrigated rows add their crop's LPJmL depth", {
  out <- whep::build_cell_moisture_regime(
    years = 2010L,
    window = "annual",
    data = .cmw_data(2010L)
  )
  maize <- .cmw_row(out, 9.25, 79L, 2514L)
  testthat::expect_equal(maize$irrig_mm, 120)
  testthat::expect_equal(maize$water_mm, 720)
  testthat::expect_identical(maize$method_irrig_depth, "lpjml_airrig_stand")
  testthat::expect_identical(maize$lpjml_crop, "maize")
  testthat::expect_identical(maize$lpjml_run, "fixture/scenario_1")
  testthat::expect_identical(
    maize$method_moisture_regime,
    "ipcc2019_wet_dry;annual;precip_irrig"
  )
})

testthat::test_that("pooled items weight by WHEP hectares, not LPJmL stands", {
  out <- whep::build_cell_moisture_regime(
    years = 2010L,
    window = "annual",
    data = .cmw_data(2010L)
  )
  veg <- .cmw_row(out, 9.25, 79L, 2605L)
  # Green corn (10 ha irrigated, LPJmL maize 120 mm) and cabbages (90 ha,
  # "others" 360 mm). LPJmL's 0.4 maize stand against its 0.05 "others"
  # stand would give (0.4 * 120 + 0.05 * 360) / 0.45 = 146.7 mm, with the
  # whole maize stand counted for a 10 ha item.
  testthat::expect_equal(veg$irrig_mm, (10 * 120 + 90 * 360) / 100)
  testthat::expect_false(isTRUE(all.equal(veg$irrig_mm, 66 / 0.45)))
  testthat::expect_identical(veg$method_irrig_depth, "lpjml_others_proxy")
  testthat::expect_identical(veg$lpjml_crop, "maize+others")
})

testthat::test_that("pooling without WHEP hectares aborts", {
  items <- tibble::tribble(
    ~lon, ~lat,  ~year, ~area_code, ~item_cbs_code, ~irrig_mm,
    ~method_irrig_depth, ~lpjml_crop,
    9.25, 47.75, 2010L, 1L,         2605L,          120,
    "lpjml_airrig_stand", "maize"
  )
  testthat::expect_error(
    whep:::.lid_cbs_depth(items),
    class = "whep_lpjml_irrig_depth_weights"
  )
})

testthat::test_that("a trailing window averages each year's own inputs", {
  d <- .cmw_data(1981:2010)
  d$crop_depth$irrig_mm_irrigated[
    d$crop_depth$lpjml_crop == "maize" & d$crop_depth$year == 2010L
  ] <- 420
  out <- whep::build_cell_moisture_regime(years = 2010L, data = d)
  maize <- .cmw_row(out, 9.25, 79L, 2514L)
  testthat::expect_equal(maize$irrig_mm, (29 * 120 + 420) / 30)
  testthat::expect_identical(
    maize$method_moisture_regime,
    "ipcc2019_wet_dry;trailing_30;precip_irrig"
  )
})

testthat::test_that("a window year with no WHEP irrigation is left missing", {
  # Maize is irrigated in A only from 2000 on, so 1981-1999 have no
  # irrigated row and no depth; the window mean is not taken over fewer years.
  d <- .cmw_data(1981:2010)
  early <- d$regime_areas$year < 2000L & d$regime_areas$item_prod_code == 56L
  d$regime_areas$irrigated_ha[early & d$regime_areas$area_code == 79L] <- 0
  testthat::expect_message(
    out <- whep::build_cell_moisture_regime(years = 2010L, data = d),
    class = "whep_moisture_regime_unclassified"
  )
  maize <- .cmw_row(out, 9.25, 79L, 2514L)
  testthat::expect_true(is.na(maize$moisture_regime))
  testthat::expect_true(is.na(maize$irrig_mm))
  testthat::expect_match(maize$method_moisture_regime, "missing=irrig_mm$")
  # Rainfed rows are untouched.
  testthat::expect_identical(
    .cmw_row(out, 9.25, 79L, 2514L, "rainfed")$moisture_regime,
    "dry"
  )
})

testthat::test_that("LPJmL's own pre-1901 depths are used, labelled", {
  d <- list(
    climate = .cmw_climate(1901:1930),
    crop_depth = .cmw_crop(1880L) |>
      dplyr::mutate(
        irrig_mm_irrigated = dplyr::if_else(
          .data$lpjml_crop == "maize",
          900,
          .data$irrig_mm_irrigated
        )
      ),
    regime_areas = .cmw_areas(1880L)
  )
  out <- whep::build_cell_moisture_regime(
    years = 1880L,
    window = "annual",
    data = d
  )
  maize <- .cmw_row(out, 9.25, 79L, 2514L)
  # Climate is the held 1901-1930 climatology; irrigation is LPJmL's 1880.
  testthat::expect_equal(maize$irrig_mm, 900)
  testthat::expect_equal(maize$prec_mm, 600)
  testthat::expect_identical(maize$moisture_regime, "wet")
  testthat::expect_identical(
    maize$method_moisture_regime,
    "ipcc2019_wet_dry;annual;precip_irrig;climatology_1901_1930_1of1"
  )
  testthat::expect_identical(
    maize$method_irrig_depth,
    "lpjml_airrig_stand_recycled_climate"
  )
})

testthat::test_that("years outside the LPJmL run take the window's mean", {
  d <- .cmw_data(1981:2010)
  d$crop_depth <- d$crop_depth[d$crop_depth$year <= 2008L, ]
  d$lpjml_years <- 1950:2008
  out <- whep::build_cell_moisture_regime(years = 2010L, data = d)
  maize <- .cmw_row(out, 9.25, 79L, 2514L)
  testthat::expect_equal(maize$irrig_mm, 120)
  testthat::expect_identical(
    maize$method_irrig_depth,
    "lpjml_airrig_stand;outside_run_2of30_series_mean"
  )
  testthat::expect_identical(maize$moisture_regime, "dry")
})

testthat::test_that("an outside-run fill falls back to the country mean", {
  key <- c("lon", "lat", "area_code", "item_cbs_code")
  depth <- tibble::tribble(
    ~lon, ~lat,  ~area_code, ~item_cbs_code, ~year, ~irrig_mm, ~irrigated_ha,
    9.25, 47.75, 79L,        2514L,          2008L, 100,       30,
    9.75, 47.75, 79L,        2514L,          2008L, 200,       10
  )
  outside <- tibble::tribble(
    ~lon, ~lat,  ~area_code, ~item_cbs_code, ~year, ~irrigated_ha,
    10.25, 47.75, 79L,        2514L,          2009L, 5
  )
  fills <- whep:::.cmr_fill_outside_run(
    depth,
    outside,
    span = 2008:2009,
    run_years = 1950:2008
  )
  # The new series has no depth of its own; its country's is weighted by
  # WHEP's irrigated hectares.
  testthat::expect_equal(fills$rows$irrig_mm, (30 * 100 + 10 * 200) / 40)
  testthat::expect_identical(fills$stamps$fill, "country_mean")
  testthat::expect_identical(fills$stamps$n_filled, 1L)
  testthat::expect_error(
    whep:::.cmr_fill_outside_run(
      depth,
      outside,
      span = 2009L,
      run_years = 1950:2008
    ),
    class = "whep_moisture_regime_outside_run"
  )
})

testthat::test_that("an irrigated item with no LPJmL band aborts", {
  # 51 has a commodity balance item but no LPJmL crop.
  extra <- tibble::tibble(
    lon = 9.25,
    lat = 47.75,
    area_code = 79L,
    item_prod_code = 51L,
    rainfed_ha = 0,
    irrigated_ha = 7
  )
  err <- testthat::expect_error(
    whep::build_cell_moisture_regime(
      years = 2010L,
      window = "annual",
      data = .cmw_data(2010L, extra = extra)
    ),
    class = "whep_moisture_regime_stand_mapping"
  )
  summary <- err$diagnosis$summary
  testthat::expect_identical(summary$cause, "mapping")
  testthat::expect_identical(summary$n_rows, 1L)
  testthat::expect_equal(summary$irrigated_ha, 7)
})

testthat::test_that("any other missing stand aborts with its diagnosis", {
  d <- .cmw_data(2010L)
  # Cabbages lose their "others" stand, and a third cell with no LPJmL crop
  # at all and irrigated rice in cell B join them.
  d$crop_depth <- d$crop_depth[d$crop_depth$lpjml_crop != "others", ]
  d$regime_areas <- dplyr::bind_rows(
    d$regime_areas,
    tibble::tribble(
      ~lon, ~lat,  ~area_code, ~item_prod_code, ~rainfed_ha, ~irrigated_ha,
      ~year,
      10.25, 47.75, 79L,        56L,             0,           4,
      2010L,
      9.75, 47.75, 79L,        56L,             0,           3,
      2010L
    )
  )
  d$climate <- dplyr::bind_rows(
    d$climate,
    dplyr::mutate(d$climate[1, ], lon = 10.25)
  )
  err <- testthat::expect_error(
    whep::build_cell_moisture_regime(
      years = 2010L,
      window = "annual",
      data = d
    ),
    class = "whep_moisture_regime_stand_mismatch"
  )
  summary <- err$diagnosis$summary
  testthat::expect_setequal(
    summary$detail,
    c(
      "lpjml_crop_absent_from_cell",
      "lpjml_crop_rainfed_only",
      "no_lpjml_crop_stand_in_cell"
    )
  )
  testthat::expect_equal(sum(summary$irrigated_ha), 90 + 3 + 4)
  testthat::expect_true(all(err$diagnosis$cases$cause == "unexplained"))
})

testthat::test_that("blocks of cells give the unblocked result exactly", {
  d <- .cmw_data(1980:2010)
  whole <- whep::build_cell_moisture_regime(
    years = 2009:2010,
    data = d,
    config = list(max_rows = 1e9)
  )
  blocked <- whep::build_cell_moisture_regime(
    years = 2009:2010,
    data = d,
    config = list(max_rows = 1)
  )
  testthat::expect_identical(blocked, whole)
  testthat::expect_identical(
    length(whep:::.cmr_cell_blocks(
      whep:::.lid_regime_keys(whep:::.lid_check_areas(d$regime_areas)),
      list(plan = list(read_years = 1981:2010), needed = 1981:2010),
      max_rows = 1
    )),
    2L
  )
})

testthat::test_that("precipitation alone gives every row its cell's class", {
  out <- whep::build_cell_moisture_regime(
    years = 2010L,
    window = "annual",
    water = "precip",
    data = .cmw_data(2010L)
  )
  maize <- .cmw_row(out, 9.25, 79L, 2514L)
  testthat::expect_true(is.na(maize$irrig_mm))
  testthat::expect_equal(maize$water_mm, 600)
  testthat::expect_identical(
    maize$method_moisture_regime,
    "ipcc2019_wet_dry;annual;precip"
  )
})

testthat::test_that("years and config are checked", {
  testthat::expect_error(
    whep::build_cell_moisture_regime(data = .cmw_data(2010L)),
    class = "whep_moisture_regime_years"
  )
  testthat::expect_error(
    whep::build_cell_moisture_regime(
      2010L,
      data = .cmw_data(2010L),
      config = list(chunk = 5)
    ),
    class = "whep_moisture_regime_config"
  )
})

testthat::test_that("the example runs and matches the output schema", {
  out <- whep::build_cell_moisture_regime(example = TRUE)
  testthat::expect_gt(nrow(out), 0L)
  # The fixture is the real function's output on the test fixture.
  real <- whep::build_cell_moisture_regime(
    years = 2009:2010,
    data = .cmw_data(1980:2010)
  )
  testthat::expect_identical(names(out), names(real))
  testthat::expect_equal(
    dplyr::semi_join(real, out, by = c(whep:::.cmr_row_key(), "year")),
    out
  )
})
