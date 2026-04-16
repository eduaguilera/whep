# Helper fixtures ---------------------------------------------------------------

two_country_fixture <- function() {
  country_areas <- tibble::tribble(
    ~year, ~area_code, ~item_prod_code, ~harvested_area_ha,
    2000L,         1L,             15L,               1000,
    2000L,         2L,             15L,                500
  )

  crop_patterns <- tibble::tribble(
    ~lon,  ~lat, ~item_prod_code, ~harvest_fraction,
     0.25, 50.25,             15L,               0.8,
     0.75, 50.25,             15L,               0.2,
     1.25, 50.25,             15L,               0.5,
     1.75, 50.25,             15L,               0.5
  )

  gridded_cropland <- tibble::tribble(
    ~lon,  ~lat,  ~year, ~cropland_ha,
     0.25, 50.25, 2000L,          600,
     0.75, 50.25, 2000L,          400,
     1.25, 50.25, 2000L,          300,
     1.75, 50.25, 2000L,          200
  )

  country_grid <- tibble::tribble(
    ~lon,  ~lat, ~area_code,
     0.25, 50.25,         1L,
     0.75, 50.25,         1L,
     1.25, 50.25,         2L,
     1.75, 50.25,         2L
  )

  list(
    country_areas = country_areas,
    crop_patterns = crop_patterns,
    gridded_cropland = gridded_cropland,
    country_grid = country_grid
  )
}

multi_crop_fixture <- function() {
  country_areas <- tibble::tribble(
    ~year, ~area_code, ~item_prod_code, ~harvested_area_ha,
    2000L,         1L,             15L,               600,
    2000L,         1L,             44L,               400
  )

  crop_patterns <- tibble::tribble(
    ~lon,  ~lat, ~item_prod_code, ~harvest_fraction,
     0.25, 50.25,             15L,               0.6,
     0.75, 50.25,             15L,               0.4,
     0.25, 50.25,             44L,               0.3,
     0.75, 50.25,             44L,               0.7
  )

  gridded_cropland <- tibble::tribble(
    ~lon,  ~lat,  ~year, ~cropland_ha,
     0.25, 50.25, 2000L,          500,
     0.75, 50.25, 2000L,          500
  )

  country_grid <- tibble::tribble(
    ~lon,  ~lat, ~area_code,
     0.25, 50.25,         1L,
     0.75, 50.25,         1L
  )

  list(
    country_areas = country_areas,
    crop_patterns = crop_patterns,
    gridded_cropland = gridded_cropland,
    country_grid = country_grid
  )
}

# build_gridded_landuse ---------------------------------------------------------

testthat::test_that("build_gridded_landuse returns one row per crop-cell with correct columns", {
  fix <- two_country_fixture()
  result <- build_gridded_landuse(
    fix$country_areas,
    fix$crop_patterns,
    fix$gridded_cropland,
    fix$country_grid
  )

  result |>
    pointblank::expect_col_exists(
      c("lon", "lat", "year", "item_prod_code", "rainfed_ha", "irrigated_ha")
    )

  testthat::expect_equal(nrow(result), 4L)
})

testthat::test_that("build_gridded_landuse conserves country totals", {
  fix <- two_country_fixture()
  result <- build_gridded_landuse(
    fix$country_areas,
    fix$crop_patterns,
    fix$gridded_cropland,
    fix$country_grid
  )

  # Country 1 total should be 1000
  country1 <- result |>
    dplyr::filter(lon %in% c(0.25, 0.75)) |>
    dplyr::summarise(total = sum(rainfed_ha + irrigated_ha)) |>
    dplyr::pull(total)
  testthat::expect_equal(country1, 1000, tolerance = 1e-6)

  # Country 2 total should be 500
  country2 <- result |>
    dplyr::filter(lon %in% c(1.25, 1.75)) |>
    dplyr::summarise(total = sum(rainfed_ha + irrigated_ha)) |>
    dplyr::pull(total)
  testthat::expect_equal(country2, 500, tolerance = 1e-6)
})

testthat::test_that("build_gridded_landuse distributes proportionally to pattern", {
  fix <- two_country_fixture()
  result <- build_gridded_landuse(
    fix$country_areas,
    fix$crop_patterns,
    fix$gridded_cropland,
    fix$country_grid
  )

  # Country 1: patterns = 0.8, 0.2; cropland = 600, 400
  # potentials = 0.8*600=480, 0.2*400=80 → fracs = 480/560, 80/560
  cell1 <- result |>
    dplyr::filter(lon == 0.25) |>
    dplyr::pull(rainfed_ha)
  cell2 <- result |>
    dplyr::filter(lon == 0.75) |>
    dplyr::pull(rainfed_ha)

  expected_total <- 1000
  expected_c1 <- 480 / 560 * expected_total
  expected_c2 <- 80 / 560 * expected_total
  testthat::expect_equal(cell1, expected_c1, tolerance = 1e-6)
  testthat::expect_equal(cell2, expected_c2, tolerance = 1e-6)
})

testthat::test_that("build_gridded_landuse handles multiple crops", {
  fix <- multi_crop_fixture()
  result <- build_gridded_landuse(
    fix$country_areas,
    fix$crop_patterns,
    fix$gridded_cropland,
    fix$country_grid
  )

  # Should have 4 rows: 2 cells x 2 crops
  testthat::expect_equal(nrow(result), 4L)

  # Total wheat = 600, total barley = 400 → total = 1000
  total <- result |>
    dplyr::summarise(total = sum(rainfed_ha + irrigated_ha)) |>
    dplyr::pull(total)
  testthat::expect_equal(total, 1000, tolerance = 1e-6)
})

testthat::test_that("build_gridded_landuse handles irrigation split", {
  country_areas <- tibble::tribble(
      ~year, ~area_code, ~item_prod_code,
        ~harvested_area_ha, ~irrigated_area_ha,
      2000L,         1L,             15L,
                      1000,                400
    )

  crop_patterns <- tibble::tribble(
      ~lon,  ~lat, ~item_prod_code, ~harvest_fraction,
       0.25, 50.25,             15L,               0.5,
       0.75, 50.25,             15L,               0.5
    )

  gridded_cropland <- tibble::tribble(
      ~lon,  ~lat,  ~year, ~cropland_ha, ~irrigated_ha,
       0.25, 50.25, 2000L,          500,           200,
       0.75, 50.25, 2000L,          500,           200
    )

  country_grid <- tibble::tribble(
      ~lon,  ~lat, ~area_code,
       0.25, 50.25,         1L,
       0.75, 50.25,         1L
    )

  result <- build_gridded_landuse(
    country_areas,
    crop_patterns,
    gridded_cropland,
    country_grid
  )

  total_irrigated <- result |>
    dplyr::summarise(total = sum(irrigated_ha)) |>
    dplyr::pull(total)
  testthat::expect_equal(total_irrigated, 400, tolerance = 1e-6)

  total_rainfed <- result |>
    dplyr::summarise(total = sum(rainfed_ha)) |>
    dplyr::pull(total)
  testthat::expect_equal(total_rainfed, 600, tolerance = 1e-6)
})

# CFT aggregation ---------------------------------------------------------------

testthat::test_that("build_gridded_landuse aggregates to CFTs when mapping provided", {
  fix <- multi_crop_fixture()
  cft_map <- tibble::tribble(
      ~item_prod_code, ~cft_name,
                  15L, "temperate_cereals",
                  44L, "temperate_cereals"
    )

  result <- build_gridded_landuse(
    fix$country_areas,
    fix$crop_patterns,
    fix$gridded_cropland,
    fix$country_grid,
    cft_mapping = cft_map
  )

  # Should collapse 2 crops into 1 CFT per cell
  testthat::expect_equal(nrow(result), 2L)
  result |>
    pointblank::expect_col_exists("cft_name") |>
    pointblank::expect_col_vals_in_set(
      cft_name,
      "temperate_cereals"
    )

  # Total still conserved
  total <- result |>
    dplyr::summarise(total = sum(rainfed_ha + irrigated_ha)) |>
    dplyr::pull(total)
  testthat::expect_equal(total, 1000, tolerance = 1e-6)
})

# Input validation ---------------------------------------------------------------

testthat::test_that("build_gridded_landuse errors on missing columns", {
  bad_areas <- tibble::tibble(year = 2000L, area_code = 1L)
  good_patterns <- tibble::tribble(
      ~lon, ~lat, ~item_prod_code, ~harvest_fraction,
      0.25, 50.25, 15L, 0.5
    )
  good_cropland <- tibble::tribble(
      ~lon, ~lat, ~year, ~cropland_ha,
      0.25, 50.25, 2000L, 500
    )
  good_grid <- tibble::tribble(
      ~lon, ~lat, ~area_code,
      0.25, 50.25, 1L
    )

  testthat::expect_error(
    build_gridded_landuse(
      bad_areas,
      good_patterns,
      good_cropland,
      good_grid
    ),
    "item_prod_code"
  )
})

testthat::test_that("build_gridded_landuse handles cells with no pattern gracefully", {
  country_areas <- tibble::tribble(
      ~year, ~area_code, ~item_prod_code, ~harvested_area_ha,
      2000L,         1L,             15L,                100
    )

  # Pattern only for one cell, but two cells in the grid
  crop_patterns <- tibble::tribble(
      ~lon,  ~lat, ~item_prod_code, ~harvest_fraction,
       0.25, 50.25,             15L,               1.0
    )

  gridded_cropland <- tibble::tribble(
      ~lon,  ~lat,  ~year, ~cropland_ha,
       0.25, 50.25, 2000L,          500,
       0.75, 50.25, 2000L,          500
    )

  country_grid <- tibble::tribble(
      ~lon,  ~lat, ~area_code,
       0.25, 50.25,         1L,
       0.75, 50.25,         1L
    )

  result <- build_gridded_landuse(
    country_areas,
    crop_patterns,
    gridded_cropland,
    country_grid
  )

  # Area should still be fully allocated
  total <- result |>
    dplyr::summarise(total = sum(rainfed_ha + irrigated_ha)) |>
    dplyr::pull(total)
  testthat::expect_equal(total, 100, tolerance = 1e-6)
})

# Multiple years ----------------------------------------------------------------

testthat::test_that("build_gridded_landuse handles multiple years", {
  country_areas <- tibble::tribble(
      ~year, ~area_code, ~item_prod_code, ~harvested_area_ha,
      2000L,         1L,             15L,               1000,
      2001L,         1L,             15L,               1200
    )

  crop_patterns <- tibble::tribble(
      ~lon,  ~lat, ~item_prod_code, ~harvest_fraction,
       0.25, 50.25,             15L,               0.5,
       0.75, 50.25,             15L,               0.5
    )

  gridded_cropland <- tibble::tribble(
      ~lon,  ~lat,  ~year, ~cropland_ha,
       0.25, 50.25, 2000L,          500,
       0.75, 50.25, 2000L,          500,
       0.25, 50.25, 2001L,          600,
       0.75, 50.25, 2001L,          600
    )

  country_grid <- tibble::tribble(
      ~lon,  ~lat, ~area_code,
       0.25, 50.25,         1L,
       0.75, 50.25,         1L
    )

  result <- build_gridded_landuse(
    country_areas,
    crop_patterns,
    gridded_cropland,
    country_grid
  )

  testthat::expect_equal(nrow(result), 4L)

  yr_2000 <- result |>
    dplyr::filter(year == 2000L) |>
    dplyr::summarise(total = sum(rainfed_ha)) |>
    dplyr::pull(total)
  testthat::expect_equal(yr_2000, 1000, tolerance = 1e-6)

  yr_2001 <- result |>
    dplyr::filter(year == 2001L) |>
    dplyr::summarise(total = sum(rainfed_ha)) |>
    dplyr::pull(total)
  testthat::expect_equal(yr_2001, 1200, tolerance = 1e-6)
})

# Years filter ------------------------------------------------------------------

testthat::test_that("build_gridded_landuse filters year-keyed inputs when years is set", {
  country_areas <- tibble::tribble(
      ~year, ~area_code, ~item_prod_code, ~harvested_area_ha,
      2000L,         1L,             15L,               1000,
      2001L,         1L,             15L,               1200,
      2002L,         1L,             15L,               1400
    )

  crop_patterns <- tibble::tribble(
      ~lon,  ~lat, ~item_prod_code, ~harvest_fraction,
       0.25, 50.25,             15L,               0.5,
       0.75, 50.25,             15L,               0.5
    )

  gridded_cropland <- tibble::tribble(
      ~lon,  ~lat,  ~year, ~cropland_ha,
       0.25, 50.25, 2000L,          500,
       0.75, 50.25, 2000L,          500,
       0.25, 50.25, 2001L,          600,
       0.75, 50.25, 2001L,          600,
       0.25, 50.25, 2002L,          700,
       0.75, 50.25, 2002L,          700
    )

  country_grid <- tibble::tribble(
      ~lon,  ~lat, ~area_code,
       0.25, 50.25,         1L,
       0.75, 50.25,         1L
    )

  result <- whep::build_gridded_landuse(
    country_areas,
    crop_patterns,
    gridded_cropland,
    country_grid,
    years = c(2000L, 2002L)
  )

  testthat::expect_setequal(unique(result$year), c(2000L, 2002L))
  testthat::expect_equal(nrow(result), 4L)
})

testthat::test_that("build_gridded_landuse warns for requested years missing in country_areas", {
  fix <- two_country_fixture()
  testthat::expect_warning(
    whep::build_gridded_landuse(
      fix$country_areas,
      fix$crop_patterns,
      fix$gridded_cropland,
      fix$country_grid,
      years = c(2000L, 1999L)
    ),
    "1999"
  )
})
