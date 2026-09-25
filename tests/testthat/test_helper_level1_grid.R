# T37 -- level-1 fixture contract tests -----------------------------------

testthat::test_that(".level1_country_grid returns the documented tibble shape", {
  grid <- .level1_country_grid()
  testthat::expect_true(tibble::is_tibble(grid))
  testthat::expect_named(
    grid,
    c(
      "lon",
      "lat",
      "area_code",
      "level_polity_code",
      "level",
      "cell_area_frac",
      "polycell_id",
      "start_year",
      "end_year"
    )
  )
  testthat::expect_equal(nrow(grid), 8L)
})

testthat::test_that("assertion (a): cell_area_frac sums to 1 per (lon, lat)", {
  totals <- .level1_country_grid() |>
    dplyr::summarise(total = sum(cell_area_frac), .by = c(lon, lat))
  testthat::expect_equal(totals$total, rep(1, nrow(totals)), tolerance = 1e-8)
})

testthat::test_that("country A never carries a container-keyed (level-0) row", {
  grid <- .level1_country_grid()
  a_rows <- dplyr::filter(grid, area_code == 900L)
  testthat::expect_true(all(a_rows$level >= 1L))
  testthat::expect_true(all(!is.na(a_rows$level_polity_code)))
})

testthat::test_that("assertion (b): A's unit shares sum to its level-0 share per cell", {
  unit_totals <- .level1_country_grid() |>
    dplyr::filter(area_code == 900L) |>
    dplyr::summarise(
      unit_total = sum(cell_area_frac),
      .by = c(lon, lat, area_code)
    )
  level0 <- .level1_level0_shares()
  compare <- dplyr::inner_join(
    unit_totals,
    level0,
    by = c("lon", "lat", "area_code"),
    suffix = c("_unit", "_level0")
  )
  testthat::expect_equal(nrow(compare), nrow(level0))
  testthat::expect_equal(
    compare$unit_total,
    compare$cell_area_frac,
    tolerance = 1e-8
  )
})

testthat::test_that("the straddling cell carries two A rows, both fractional", {
  straddle <- .level1_country_grid() |>
    dplyr::filter(lon == 10.75, lat == 40.25, area_code == 900L)
  testthat::expect_equal(nrow(straddle), 2L)
  testthat::expect_true(all(straddle$cell_area_frac < 1))
  testthat::expect_setequal(
    straddle$level_polity_code,
    c("A-A1-1900-2100", "A-A2-1975-2100")
  )
})

testthat::test_that("the straddling cell is also the A1/A2/B border cell", {
  border <- .level1_country_grid() |>
    dplyr::filter(lon == 10.75, lat == 40.25)
  testthat::expect_equal(nrow(border), 3L)
  testthat::expect_true(901L %in% border$area_code)
  testthat::expect_equal(sum(border$area_code == 900L), 2L)
})

testthat::test_that("the zero-pattern cell has no row for item 44", {
  patterns <- .level1_crop_patterns() |>
    dplyr::filter(lon == 11.25, lat == 40.25)
  testthat::expect_equal(patterns$item_prod_code, 15L)
  testthat::expect_false(44L %in% patterns$item_prod_code)
})

testthat::test_that(".level1_crop_patterns is a tibble with the documented columns", {
  cp <- .level1_crop_patterns()
  testthat::expect_true(tibble::is_tibble(cp))
  testthat::expect_named(
    cp,
    c("lon", "lat", "item_prod_code", "harvest_fraction")
  )
})

testthat::test_that(".level1_gridded_cropland and .level1_gridded_pasture cover 3 years x 6 cells", {
  cropland <- .level1_gridded_cropland()
  pasture <- .level1_gridded_pasture()

  testthat::expect_true(tibble::is_tibble(cropland))
  testthat::expect_named(cropland, c("lon", "lat", "year", "cropland_ha"))
  testthat::expect_equal(nrow(cropland), 18L)
  testthat::expect_setequal(cropland$year, c(1974L, 1975L, 1976L))

  testthat::expect_true(tibble::is_tibble(pasture))
  testthat::expect_named(
    pasture,
    c("lon", "lat", "year", "pasture_ha", "rangeland_ha")
  )
  testthat::expect_equal(nrow(pasture), 18L)
  testthat::expect_setequal(pasture$year, c(1974L, 1975L, 1976L))
})

testthat::test_that(".level1_country_areas is a tibble with the documented columns", {
  areas <- .level1_country_areas()
  testthat::expect_true(tibble::is_tibble(areas))
  testthat::expect_named(
    areas,
    c("year", "area_code", "item_prod_code", "harvested_area_ha")
  )
  testthat::expect_setequal(areas$area_code, c(900L, 901L))
})

testthat::test_that("the admin-shares fixture passes ensure_admin_shares()", {
  shares <- .level1_admin_shares()
  completed <- whep:::ensure_admin_shares(shares)
  testthat::expect_equal(nrow(completed), nrow(shares))
  testthat::expect_true(tibble::is_tibble(completed))
})

testthat::test_that("the coverage change at 1975 is visible", {
  shares <- .level1_admin_shares()
  units_by_year <- shares |>
    dplyr::summarise(
      n_units = dplyr::n_distinct(level_polity_code),
      .by = year
    )
  units_1974 <- dplyr::filter(units_by_year, year == 1974L)$n_units
  units_1975 <- dplyr::filter(units_by_year, year == 1975L)$n_units
  testthat::expect_equal(units_1974, 1L)
  testthat::expect_equal(units_1975, 2L)
  testthat::expect_true(
    "A-A2-1975-2100" %in%
      dplyr::filter(shares, year == 1975L)$level_polity_code
  )
  testthat::expect_false(
    "A-A2-1975-2100" %in%
      dplyr::filter(shares, year == 1974L)$level_polity_code
  )
})

testthat::test_that("shares sum to 1 within each reporting-unit set", {
  shares <- .level1_admin_shares()
  totals <- shares |>
    dplyr::summarise(total = sum(share), .by = c(item_prod_code, year))
  testthat::expect_equal(totals$total, rep(1, nrow(totals)), tolerance = 1e-8)
})

testthat::test_that(".example_level1_country_grid returns the country grid only", {
  example <- .example_level1_country_grid()
  testthat::expect_equal(example, .level1_country_grid())
  testthat::expect_equal(nrow(example), 8L)
})
