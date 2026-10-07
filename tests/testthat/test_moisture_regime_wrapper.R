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

testthat::test_that("a cell without CRU climate classifies on its nearest CRU cell", {
  # Cell C (10.25, 47.75) grows rainfed maize but lies off CRU's land mask:
  # its climate is B's (9.75, wet, 0.5 degrees away), not A's (1.0 away).
  extra <- tibble::tribble(
    ~lon,  ~lat,  ~area_code, ~item_prod_code, ~rainfed_ha, ~irrigated_ha,
    10.25, 47.75, 79L,        56L,             40,          0
  )
  testthat::expect_message(
    out <- whep::build_cell_moisture_regime(
      years = 2010L,
      window = "annual",
      data = .cmw_data(2010L, extra = extra)
    ),
    class = "whep_moisture_regime_nearest_climate"
  )
  c_row <- .cmw_row(out, 10.25, 79L, 2514L, "rainfed")
  testthat::expect_identical(c_row$moisture_regime, "wet")
  testthat::expect_equal(c_row$prec_mm, 1200)
  rad <- pi / 180
  km <- 2 * 6371.0088 * asin(cos(47.75 * rad) * sin(0.25 * rad))
  testthat::expect_equal(c_row$climate_fill_km, km, tolerance = 1e-9)
  testthat::expect_identical(
    c_row$method_moisture_regime,
    "ipcc2019_wet_dry;annual;precip_irrig;nearest_cru_cell"
  )
  # The other rows keep their own cell's climate.
  testthat::expect_true(all(is.na(out$climate_fill_km[out$lon != 10.25])))
  testthat::expect_false(anyNA(out$moisture_regime))
})

testthat::test_that("the nearest CRU cell is one with a complete record", {
  # B has a missing PET year, so it cannot lend a climate: C takes A's.
  extra <- tibble::tribble(
    ~lon,  ~lat,  ~area_code, ~item_prod_code, ~rainfed_ha, ~irrigated_ha,
    10.25, 47.75, 79L,        56L,             40,          0
  )
  d <- .cmw_data(2009:2010, extra = extra)
  d$climate$annual_pet_mm[d$climate$lon == 9.75 & d$climate$year == 2009L] <-
    NA
  out <- suppressMessages(whep::build_cell_moisture_regime(
    years = 2010L,
    window = "annual",
    data = d
  ))
  c_row <- .cmw_row(out, 10.25, 79L, 2514L, "rainfed")
  testthat::expect_equal(c_row$prec_mm, 600)
  testthat::expect_identical(c_row$moisture_regime, "dry")
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

testthat::test_that("irrigation averages the years the land was irrigated", {
  # Maize is irrigated in A for polity 79 only in 1999-2010, 12 of the 30
  # window years. Its LPJmL depth is the year less 1900 mm, and A's
  # precipitation rises 10 mm a year from 600 in 1981.
  d <- .cmw_data(1981:2010)
  early <- d$regime_areas$year < 1999L &
    d$regime_areas$item_prod_code == 56L &
    d$regime_areas$area_code == 79L
  d$regime_areas$irrigated_ha[early] <- 0
  maize_depth <- d$crop_depth$lpjml_crop == "maize" &
    d$crop_depth$lon == 9.25
  d$crop_depth$irrig_mm_irrigated[maize_depth] <-
    d$crop_depth$year[maize_depth] - 1900
  in_a <- d$climate$lon == 9.25
  d$climate$annual_precip_mm[in_a] <- 600 + 10 * (d$climate$year[in_a] - 1981)

  out <- whep::build_cell_moisture_regime(years = 2010L, data = d)
  maize <- .cmw_row(out, 9.25, 79L, 2514L)

  # Irrigation: the mean of 1999-2010 alone, not a 30-year mean.
  testthat::expect_equal(maize$irrig_mm, mean(99:110))
  # Climate: still the mean of all 30 years (the 1999-2010 mean is 835).
  testthat::expect_equal(maize$prec_mm, 600 + 10 * mean(0:29))
  testthat::expect_equal(maize$water_mm, 745 + 104.5)
  testthat::expect_identical(maize$moisture_regime, "dry")
  testthat::expect_identical(
    maize$method_moisture_regime,
    "ipcc2019_wet_dry;trailing_30;precip_irrig;irrig_years_12of30"
  )
  testthat::expect_identical(maize$method_irrig_depth, "lpjml_airrig_stand")
  # Vegetables, irrigated every year, carry no count; nothing is unclassified.
  testthat::expect_identical(
    .cmw_row(out, 9.25, 79L, 2605L)$method_moisture_regime,
    "ipcc2019_wet_dry;trailing_30;precip_irrig"
  )
  testthat::expect_false(anyNA(out$moisture_regime))
})

testthat::test_that("a window year the regime areas lack stays missing", {
  # No regime areas at all for 1985: whether the land was irrigated then is
  # unknown, which is not the same as not irrigated.
  d <- .cmw_data(1981:2010)
  d$regime_areas <- d$regime_areas[d$regime_areas$year != 1985L, ]
  testthat::expect_message(
    testthat::expect_message(
      out <- whep::build_cell_moisture_regime(years = 2010L, data = d),
      class = "whep_moisture_regime_area_years"
    ),
    class = "whep_moisture_regime_unclassified"
  )
  irrigated <- out[out$water_regime == "irrigated", ]
  testthat::expect_true(all(is.na(irrigated$moisture_regime)))
  testthat::expect_true(all(
    stringr::str_ends(irrigated$method_moisture_regime, "missing=irrig_mm")
  ))
  testthat::expect_false(anyNA(out$moisture_regime[
    out$water_regime == "rainfed"
  ]))
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

# Stand gaps, 2010. Cell C (10.25) of polity 79 has irrigated maize at 300 mm
# and "others" at 200 mm, where WHEP irrigates 20 ha of maize and 30 of
# cabbages. The gaps: in B (on the grid, rainfed maize only) 3 ha of
# irrigated maize and 2 ha of cabbages; in G (12.25, on the grid, no crop
# stand) 4 ha of maize; in E (11.25, off the grid) 5 ha of maize; in F
# (6.25, polity 68, rainfed maize only) 6 ha of cabbages, a polity with no
# "others" stand anywhere.
.cmw_gap_data <- function(extra = NULL, years = 2010L) {
  d <- .cmw_data(years)
  lpjml <- tibble::tribble(
    ~lon,  ~lat,  ~lpjml_crop, ~irrig_mm_irrigated, ~stand_frac_irrigated,
    ~stand_frac_rainfed,
    10.25, 47.75, "maize",     300,                 0.2,
    0.1,
    10.25, 47.75, "others",    200,                 0.1,
    0,
    6.25,  47.75, "maize",     NA,                  NA,
    0.2
  ) |>
    tidyr::expand_grid(year = as.integer(years)) |>
    dplyr::mutate(
      irrig_mm_rainfed = 0,
      lpjml_version = "LPJmL C Version 6.1.1",
      lpjml_git_hash = "e6e6c42b88354f4a0df945e55f2ccf3c4a3cb605",
      lpjml_run = "fixture/scenario_1"
    )
  d$crop_depth <- dplyr::bind_rows(d$crop_depth, lpjml)
  areas <- tibble::tribble(
    ~lon,  ~lat,  ~area_code, ~item_prod_code, ~rainfed_ha, ~irrigated_ha,
    10.25, 47.75, 79L,        56L,             0,           20,
    10.25, 47.75, 79L,        358L,            0,           30,
    9.75,  47.75, 79L,        56L,             0,           3,
    9.75,  47.75, 79L,        358L,            0,           2,
    12.25, 47.75, 79L,        56L,             0,           4,
    11.25, 47.75, 79L,        56L,             0,           5,
    6.25,  47.75, 68L,        358L,            0,           6
  )
  d$regime_areas <- dplyr::bind_rows(
    d$regime_areas,
    tidyr::expand_grid(
      dplyr::bind_rows(areas, extra),
      year = as.integer(years)
    )
  )
  d$climate <- dplyr::bind_rows(
    d$climate,
    purrr::map(c(10.25, 12.25, 11.25, 6.25), \(x) {
      dplyr::mutate(d$climate[d$climate$lon == 9.25, ], lon = x)
    })
  )
  d$lpjml_grid <- tibble::tibble(
    lon = c(9.25, 9.75, 10.25, 12.25, 6.25),
    lat = 47.75
  )
  d
}

.cmw_gap_run <- function(d = .cmw_gap_data()) {
  whep::build_cell_moisture_regime(2010L, window = "annual", data = d)
}

testthat::test_that("a stand gap on the grid takes the polity's crop mean", {
  testthat::expect_message(
    out <- .cmw_gap_run(),
    class = "whep_moisture_regime_stand_gap"
  )
  # Maize in polity 79: A's 50 + 10 ha at 120 mm and C's 20 ha at 300 mm,
  # weighted by WHEP's hectares (the cell mean would be 210).
  maize_mean <- (60 * 120 + 20 * 300) / 80
  rainfed_only <- .cmw_row(out, 9.75, 79L, 2514L)
  testthat::expect_equal(rainfed_only$irrig_mm, maize_mean)
  testthat::expect_identical(
    rainfed_only$method_irrig_depth,
    "lpjml_landuse_vintage"
  )
  no_stand <- .cmw_row(out, 12.25, 79L, 2514L)
  testthat::expect_equal(no_stand$irrig_mm, maize_mean)
  testthat::expect_identical(
    no_stand$method_irrig_depth,
    "lpjml_landuse_vintage"
  )
  # Cabbages in B: "others" in polity 79 is A's 90 ha at 360 mm and C's 30
  # ha at 200 mm.
  absent <- .cmw_row(out, 9.75, 79L, 2605L)
  testthat::expect_equal(absent$irrig_mm, (90 * 360 + 30 * 200) / 120)
  testthat::expect_identical(absent$method_irrig_depth, "lpjml_landuse_vintage")
  # Every filled row is classified; rows with their own stand are untouched.
  testthat::expect_false(anyNA(out$moisture_regime))
  testthat::expect_equal(.cmw_row(out, 10.25, 79L, 2514L)$irrig_mm, 300)
  testthat::expect_identical(
    .cmw_row(out, 9.25, 79L, 2514L)$method_irrig_depth,
    "lpjml_airrig_stand"
  )
})

testthat::test_that("a stand gap off the LPJmL grid is stamped no_lpjml_cell", {
  out <- suppressMessages(.cmw_gap_run())
  off_grid <- .cmw_row(out, 11.25, 79L, 2514L)
  testthat::expect_equal(off_grid$irrig_mm, (60 * 120 + 20 * 300) / 80)
  testthat::expect_identical(off_grid$method_irrig_depth, "no_lpjml_cell")
  testthat::expect_false(is.na(off_grid$moisture_regime))
})

testthat::test_that("a polity without the stand takes the world's mean", {
  out <- suppressMessages(.cmw_gap_run())
  world <- .cmw_row(out, 6.25, 68L, 2605L)
  # "others" over the world: only polity 79's A and C have the stand.
  testthat::expect_equal(world$irrig_mm, (90 * 360 + 30 * 200) / 120)
  testthat::expect_identical(
    world$method_irrig_depth,
    "lpjml_landuse_vintage_world_mean"
  )
  testthat::expect_false(is.na(world$moisture_regime))
})

testthat::test_that("the stand-gap message counts rows and hectares by cause", {
  msg <- testthat::expect_message(
    .cmw_gap_run(),
    class = "whep_moisture_regime_stand_gap"
  )
  summary <- msg$diagnosis$summary
  by_cause <- stats::setNames(
    summary$irrigated_ha,
    paste(summary$fill, summary$detail)
  )
  testthat::expect_equal(
    by_cause[sort(names(by_cause))],
    c(
      "lpjml_landuse_vintage lpjml_crop_absent_from_cell" = 2,
      "lpjml_landuse_vintage lpjml_crop_rainfed_only" = 3,
      "lpjml_landuse_vintage no_lpjml_crop_stand_in_cell" = 4,
      "lpjml_landuse_vintage_world_mean lpjml_crop_absent_from_cell" = 6,
      "no_lpjml_cell outside_lpjml_grid" = 5
    )
  )
  testthat::expect_identical(sum(summary$n_rows), 5L)
  testthat::expect_identical(nrow(msg$diagnosis$cases), 5L)
})

testthat::test_that("a crop with no irrigated stand anywhere aborts", {
  # Irrigated rice in A: the run has no rice stand in any cell that year.
  extra <- tibble::tibble(
    lon = 9.25,
    lat = 47.75,
    area_code = 79L,
    item_prod_code = 27L,
    rainfed_ha = 0,
    irrigated_ha = 8
  )
  testthat::expect_error(
    .cmw_gap_run(.cmw_gap_data(extra)),
    class = "whep_moisture_regime_stand_no_mean"
  )
})

testthat::test_that("supplied crop depths need their own LPJmL grid", {
  d <- .cmw_gap_data()
  d$lpjml_grid <- NULL
  testthat::expect_error(
    .cmw_gap_run(d),
    class = "whep_moisture_regime_lpjml_grid"
  )
  # Without a stand gap, no grid is needed.
  testthat::expect_no_error(
    whep::build_cell_moisture_regime(
      2010L,
      window = "annual",
      data = .cmw_data(2010L)
    )
  )
})

testthat::test_that("a filled depth before 1901 says its climate is recycled", {
  d <- .cmw_gap_data()
  d$crop_depth$year <- 1880L
  d$regime_areas$year <- 1880L
  d$climate <- tidyr::expand_grid(
    dplyr::distinct(d$climate, lon, lat, annual_precip_mm),
    year = 1901:1930
  ) |>
    dplyr::mutate(
      mean_annual_temp_c = 12,
      annual_pet_mm = 1000,
      annual_frost_days = 40
    )
  out <- suppressMessages(
    whep::build_cell_moisture_regime(1880L, window = "annual", data = d)
  )
  testthat::expect_identical(
    .cmw_row(out, 9.75, 79L, 2514L)$method_irrig_depth,
    "lpjml_landuse_vintage_recycled_climate"
  )
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

testthat::test_that("blocks give the unblocked result with fills and gaps", {
  # Stand gaps in 2010 and maize irrigated in A only from 1999, over two
  # requested years.
  d <- .cmw_gap_data(years = 1980:2010)
  early <- d$regime_areas$year < 1999L &
    d$regime_areas$item_prod_code == 56L &
    d$regime_areas$area_code == 79L &
    d$regime_areas$lon == 9.25
  d$regime_areas$irrigated_ha[early] <- 0
  whole <- suppressMessages(whep::build_cell_moisture_regime(
    years = 2009:2010,
    data = d,
    config = list(max_rows = 1e9)
  ))
  blocked <- suppressMessages(whep::build_cell_moisture_regime(
    years = 2009:2010,
    data = d,
    config = list(max_rows = 1)
  ))
  testthat::expect_identical(blocked, whole)
  # The fixture reaches both new paths.
  testthat::expect_true(any(stringr::str_detect(
    whole$method_moisture_regime,
    "irrig_years_"
  )))
  testthat::expect_true(any(
    whole$method_irrig_depth %in% "lpjml_landuse_vintage"
  ))
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
