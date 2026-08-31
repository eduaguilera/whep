# Helper fixtures ---------------------------------------------------------------

two_country_fixture <- function() {
  country_areas <- tibble::tribble(
    ~year, ~area_code, ~item_prod_code, ~harvested_area_ha,
    2000L,         1L,             15L,               1000,
    2000L,         2L,             15L,                500
  )

  crop_patterns <- tibble::tribble(
    ~lon, ~lat, ~item_prod_code, ~harvest_fraction,
    0.25, 50.25, 15L, 0.8,
    0.75, 50.25, 15L, 0.2,
    1.25, 50.25, 15L, 0.5,
    1.75, 50.25, 15L, 0.5
  )

  gridded_cropland <- tibble::tribble(
    ~lon, ~lat, ~year, ~cropland_ha,
    0.25, 50.25, 2000L, 600,
    0.75, 50.25, 2000L, 400,
    1.25, 50.25, 2000L, 300,
    1.75, 50.25, 2000L, 200
  )

  country_grid <- tibble::tribble(
    ~lon, ~lat, ~area_code, ~cell_area_frac,
    0.25, 50.25, 1L, 1,
    0.75, 50.25, 1L, 1,
    1.25, 50.25, 2L, 1,
    1.75, 50.25, 2L, 1
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
    2000L, 1L, 15L, 600,
    2000L, 1L, 44L, 400
  )

  crop_patterns <- tibble::tribble(
    ~lon, ~lat, ~item_prod_code, ~harvest_fraction,
    0.25, 50.25, 15L, 0.6,
    0.75, 50.25, 15L, 0.4,
    0.25, 50.25, 44L, 0.3,
    0.75, 50.25, 44L, 0.7
  )

  gridded_cropland <- tibble::tribble(
    ~lon, ~lat, ~year, ~cropland_ha,
    0.25, 50.25, 2000L, 500,
    0.75, 50.25, 2000L, 500
  )

  country_grid <- tibble::tribble(
    ~lon, ~lat, ~area_code, ~cell_area_frac,
    0.25, 50.25, 1L, 1,
    0.75, 50.25, 1L, 1
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

testthat::test_that("country-crop with area but no grid cell warns and keeps others", {
  # area_code 3 has a national harvested area but no cell in country_grid,
  # so no grid cell can carry its area. It must not be dropped silently:
  # warn, while still allocating the country that does have cells.
  fix <- two_country_fixture()
  fix$country_areas <- tibble::tribble(
    ~year, ~area_code, ~item_prod_code, ~harvested_area_ha,
    2000L,         1L,             15L,               1000,
    2000L,         3L,             15L,                700
  )

  warnings <- testthat::capture_warnings(
    result <- build_gridded_landuse(
      fix$country_areas,
      fix$crop_patterns,
      fix$gridded_cropland,
      fix$country_grid
    )
  )
  testthat::expect_match(warnings, "no allocatable grid cell", all = FALSE)
  # The grid has no cell for area 3 at all, so the per-call guard fires too.
  testthat::expect_match(warnings, "no cell in", all = FALSE)

  # Country 1 (has cells) is fully allocated; country 3 is not fabricated.
  total <- result |>
    dplyr::summarise(total = sum(rainfed_ha + irrigated_ha)) |>
    dplyr::pull(total)
  testthat::expect_equal(total, 1000, tolerance = 1e-4)
})

testthat::test_that("unallocated-crop warning survives several area codes", {
  # Two unallocatable area codes, not one. `area_code` is integer, and cli's
  # make_quantity() aborts on a numeric quantity of length > 1, so a plural
  # marker reading the code vector turned this warning into a hard error on
  # real data (where many countries are unallocatable) while the single-code
  # fixture above passed.
  fix <- two_country_fixture()
  fix$country_areas <- tibble::tribble(
    ~year, ~area_code, ~item_prod_code, ~harvested_area_ha,
    2000L,         1L,             15L,               1000,
    2000L,         3L,             15L,                700,
    2000L,         4L,             15L,                300
  )

  warnings <- testthat::capture_warnings(
    result <- build_gridded_landuse(
      fix$country_areas,
      fix$crop_patterns,
      fix$gridded_cropland,
      fix$country_grid
    )
  )
  testthat::expect_match(warnings, "no allocatable grid cell", all = FALSE)

  # Both unallocatable codes are named, and the placeable country is intact.
  testthat::expect_match(
    warnings,
    "no allocatable grid cell[\\s\\S]*2 area_codes",
    all = FALSE,
    perl = TRUE
  )
  total <- result |>
    dplyr::summarise(total = sum(rainfed_ha + irrigated_ha)) |>
    dplyr::pull(total)
  testthat::expect_equal(total, 1000, tolerance = 1e-4)
})

testthat::test_that("build_gridded_landuse applies capacity constraint by default", {
  fix <- two_country_fixture()
  result <- build_gridded_landuse(
    fix$country_areas,
    fix$crop_patterns,
    fix$gridded_cropland,
    fix$country_grid
  )

  # Country 1: patterns = 0.8, 0.2; cropland = 600, 400
  # Proportional unconstrained would be 857 / 143.
  # The default capacity constraint (mc = 1) redistributes excess.
  cell1 <- result |>
    dplyr::filter(lon == 0.25) |>
    dplyr::pull(rainfed_ha)
  cell2 <- result |>
    dplyr::filter(lon == 0.75) |>
    dplyr::pull(rainfed_ha)

  # Country total must still be conserved
  testthat::expect_equal(cell1 + cell2, 1000, tolerance = 1e-4)

  # Higher-pattern cell should still get more than lower-pattern cell
  testthat::expect_gt(cell1, cell2)

  # The capacity constraint should have shifted area from the overloaded
  # high-pattern cell to the underloaded low-pattern cell relative to
  # pure proportional allocation (857 / 143).
  testthat::expect_lt(cell1, 857)
  testthat::expect_gt(cell2, 143)
})

testthat::test_that("build_gridded_landuse keeps proportional allocation when multicropping permits it", {
  fix <- two_country_fixture()
  multicropping <- fix$gridded_cropland |>
    dplyr::select(lon, lat, year) |>
    dplyr::mutate(mc_rainfed = 2, mc_irrigated = 2)

  result <- build_gridded_landuse(
    fix$country_areas,
    fix$crop_patterns,
    fix$gridded_cropland,
    fix$country_grid,
    config = list(multicropping = multicropping)
  )

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

testthat::test_that("build_gridded_landuse still constrains cells missing a multicropping row", {
  fix <- two_country_fixture()
  # Only the first cell has a multicropping row; the second cell of
  # country 1 (lon = 0.75) is absent from the layer altogether.
  multicropping <- tibble::tribble(
      ~lon, ~lat, ~mc_rainfed, ~mc_irrigated,
      0.25, 50.25, 1, 1
    )

  result <- build_gridded_landuse(
    fix$country_areas,
    fix$crop_patterns,
    fix$gridded_cropland,
    fix$country_grid,
    config = list(multicropping = multicropping)
  )

  cell1 <- result |>
    dplyr::filter(lon == 0.25) |>
    dplyr::pull(rainfed_ha)
  cell2 <- result |>
    dplyr::filter(lon == 0.75) |>
    dplyr::pull(rainfed_ha)

  # Country total must still be conserved.
  testthat::expect_equal(cell1 + cell2, 1000, tolerance = 1e-4)

  # The cell missing a multicropping row (lon = 0.75) must still be
  # capacity constrained (mc = 1 default), giving the exact same result
  # as supplying no multicropping layer at all for this fixture — not
  # left unconstrained with infinite capacity, which would push its
  # allocation well above the proportional share of 143 ha.
  testthat::expect_equal(cell1, 773.2421083791, tolerance = 1e-6)
  testthat::expect_equal(cell2, 226.7578916209, tolerance = 1e-6)
})

testthat::test_that("build_gridded_landuse distributes proportionally when within capacity", {
  # Single cell with plenty of cropland — no capacity pressure
  country_areas <- tibble::tribble(
    ~year, ~area_code, ~item_prod_code, ~harvested_area_ha,
    2000L, 1L, 15L, 500
  )
  crop_patterns <- tibble::tribble(
    ~lon, ~lat, ~item_prod_code, ~harvest_fraction,
    0.25, 50.25, 15L, 1.0
  )
  gridded_cropland <- tibble::tribble(
    ~lon, ~lat, ~year, ~cropland_ha,
    0.25, 50.25, 2000L, 1000
  )
  country_grid <- tibble::tribble(
    ~lon, ~lat, ~area_code, ~cell_area_frac,
    0.25, 50.25, 1L, 1
  )

  result <- build_gridded_landuse(
    country_areas,
    crop_patterns,
    gridded_cropland,
    country_grid
  )

  testthat::expect_equal(result$rainfed_ha, 500, tolerance = 1e-6)
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
    2000L, 1L, 15L,
    1000, 400
  )

  crop_patterns <- tibble::tribble(
    ~lon, ~lat, ~item_prod_code, ~harvest_fraction,
    0.25, 50.25, 15L, 0.5,
    0.75, 50.25, 15L, 0.5
  )

  gridded_cropland <- tibble::tribble(
    ~lon, ~lat, ~year, ~cropland_ha, ~irrigated_ha,
    0.25, 50.25, 2000L, 500, 200,
    0.75, 50.25, 2000L, 500, 200
  )

  country_grid <- tibble::tribble(
    ~lon, ~lat, ~area_code, ~cell_area_frac,
    0.25, 50.25, 1L, 1,
    0.75, 50.25, 1L, 1
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
    config = list(cft_mapping = cft_map)
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
    ~lon, ~lat, ~area_code, ~cell_area_frac,
    0.25, 50.25, 1L, 1
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
    ~lon, ~lat, ~item_prod_code, ~harvest_fraction,
    0.25, 50.25, 15L, 1.0
  )

  gridded_cropland <- tibble::tribble(
    ~lon, ~lat, ~year, ~cropland_ha,
    0.25, 50.25, 2000L, 500,
    0.75, 50.25, 2000L, 500
  )

  country_grid <- tibble::tribble(
    ~lon, ~lat, ~area_code, ~cell_area_frac,
    0.25, 50.25, 1L, 1,
    0.75, 50.25, 1L, 1
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

# Type-aware allocation ---------------------------------------------------------

testthat::test_that("type-aware allocation excludes cells lacking the crop's LUH2 type", {
  country_areas <- tibble::tribble(
    ~year, ~area_code, ~item_prod_code, ~harvested_area_ha,
    2000L,         1L,             15L,               1000
  )
  # Crop pattern is present in both cells.
  crop_patterns <- tibble::tribble(
    ~lon, ~lat, ~item_prod_code, ~harvest_fraction,
    0.25, 50.25,             15L,               0.5,
    0.75, 50.25,             15L,               0.5
  )
  gridded_cropland <- tibble::tribble(
    ~lon, ~lat, ~year, ~cropland_ha,
    0.25, 50.25, 2000L, 500,
    0.75, 50.25, 2000L, 500
  )
  country_grid <- tibble::tribble(
    ~lon, ~lat, ~area_code, ~cell_area_frac,
    0.25, 50.25, 1L, 1,
    0.75, 50.25, 1L, 1
  )
  # Crop 15 maps to c3ann, present only in the first cell (sparse table:
  # the second cell has no c3ann row).
  type_mapping <- tibble::tribble(
    ~item_prod_code, ~luh2_type,
    15L, "c3ann"
  )
  type_cropland <- tibble::tribble(
    ~lon, ~lat, ~year, ~luh2_type, ~type_ha, ~type_irrig_ha,
    0.25, 50.25, 2000L, "c3ann", 500, 0
  )

  result <- whep::build_gridded_landuse(
    country_areas,
    crop_patterns,
    gridded_cropland,
    country_grid,
    config = list(
      type_cropland = type_cropland,
      type_mapping = type_mapping
    )
  )

  # The cell lacking the crop's LUH2 type must get zero allocation.
  cell_lacking_type <- result |>
    dplyr::filter(lon == 0.75) |>
    dplyr::summarise(total = sum(rainfed_ha + irrigated_ha)) |>
    dplyr::pull(total)
  testthat::expect_equal(cell_lacking_type, 0, tolerance = 1e-6)

  # The full country total lands in the cell that has the type.
  cell_with_type <- result |>
    dplyr::filter(lon == 0.25) |>
    dplyr::summarise(total = sum(rainfed_ha + irrigated_ha)) |>
    dplyr::pull(total)
  testthat::expect_equal(cell_with_type, 1000, tolerance = 1e-6)
})

testthat::test_that("type-aware allocation falls back to total cropland when no cell has the type", {
  country_areas <- tibble::tribble(
    ~year, ~area_code, ~item_prod_code, ~harvested_area_ha,
    2000L,         1L,             15L,               1000
  )
  crop_patterns <- tibble::tribble(
    ~lon, ~lat, ~item_prod_code, ~harvest_fraction,
    0.25, 50.25,             15L,               0.5,
    0.75, 50.25,             15L,               0.5
  )
  gridded_cropland <- tibble::tribble(
    ~lon, ~lat, ~year, ~cropland_ha,
    0.25, 50.25, 2000L, 500,
    0.75, 50.25, 2000L, 500
  )
  country_grid <- tibble::tribble(
    ~lon, ~lat, ~area_code, ~cell_area_frac,
    0.25, 50.25, 1L, 1,
    0.75, 50.25, 1L, 1
  )
  type_mapping <- tibble::tribble(
    ~item_prod_code, ~luh2_type,
    15L, "c3ann"
  )
  # No cell has c3ann for this crop (a different type is present).
  type_cropland <- tibble::tribble(
    ~lon, ~lat, ~year, ~luh2_type, ~type_ha, ~type_irrig_ha,
    0.25, 50.25, 2000L, "c4ann", 500, 0,
    0.75, 50.25, 2000L, "c4ann", 500, 0
  )

  result <- whep::build_gridded_landuse(
    country_areas,
    crop_patterns,
    gridded_cropland,
    country_grid,
    config = list(
      type_cropland = type_cropland,
      type_mapping = type_mapping
    )
  )

  # Whole-group fallback: total is conserved via total cropland.
  total <- result |>
    dplyr::summarise(total = sum(rainfed_ha + irrigated_ha)) |>
    dplyr::pull(total)
  testthat::expect_equal(total, 1000, tolerance = 1e-6)
})

# Multiple years ----------------------------------------------------------------

testthat::test_that("build_gridded_landuse handles multiple years", {
  country_areas <- tibble::tribble(
    ~year, ~area_code, ~item_prod_code, ~harvested_area_ha,
    2000L,         1L,             15L,               1000,
    2001L,         1L,             15L,               1200
  )

  crop_patterns <- tibble::tribble(
    ~lon, ~lat, ~item_prod_code, ~harvest_fraction,
    0.25, 50.25, 15L, 0.5,
    0.75, 50.25, 15L, 0.5
  )

  gridded_cropland <- tibble::tribble(
    ~lon, ~lat, ~year, ~cropland_ha,
    0.25, 50.25, 2000L, 500,
    0.75, 50.25, 2000L, 500,
    0.25, 50.25, 2001L, 600,
    0.75, 50.25, 2001L, 600
  )

  country_grid <- tibble::tribble(
    ~lon, ~lat, ~area_code, ~cell_area_frac,
    0.25, 50.25, 1L, 1,
    0.75, 50.25, 1L, 1
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
    ~lon, ~lat, ~item_prod_code, ~harvest_fraction,
    0.25, 50.25, 15L, 0.5,
    0.75, 50.25, 15L, 0.5
  )

  gridded_cropland <- tibble::tribble(
    ~lon, ~lat, ~year, ~cropland_ha,
    0.25, 50.25, 2000L, 500,
    0.75, 50.25, 2000L, 500,
    0.25, 50.25, 2001L, 600,
    0.75, 50.25, 2001L, 600,
    0.25, 50.25, 2002L, 700,
    0.75, 50.25, 2002L, 700
  )

  country_grid <- tibble::tribble(
    ~lon, ~lat, ~area_code, ~cell_area_frac,
    0.25, 50.25, 1L, 1,
    0.75, 50.25, 1L, 1
  )

  result <- whep::build_gridded_landuse(
    country_areas,
    crop_patterns,
    gridded_cropland,
    country_grid,
    config = list(years = c(2000L, 2002L))
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
      config = list(years = c(2000L, 1999L))
    ),
    "1999"
  )
})

testthat::test_that("shared cells keep independent polity landuse compartments", {
  country_areas <- tibble::tribble(
    ~year, ~area_code, ~item_prod_code, ~harvested_area_ha,
    2000L,         1L,             15L,                100,
    2000L,         2L,             15L,                200
  )
  crop_patterns <- tibble::tribble(
    ~lon,  ~lat, ~item_prod_code, ~harvest_fraction,
    0.25, 50.25,             15L,               1.0
  )
  gridded_cropland <- tibble::tribble(
    ~lon, ~lat, ~year, ~cropland_ha,
    0.25, 50.25, 2000L, 1000
  )
  country_grid <- tibble::tribble(
    ~polycell_id, ~lon, ~lat, ~area_code, ~cell_area_frac,
    "a", 0.25, 50.25, 1L, 0.25,
    "b", 0.25, 50.25, 2L, 0.75
  )

  result <- whep::build_gridded_landuse(
    country_areas,
    crop_patterns,
    gridded_cropland,
    country_grid
  )

  totals <- result |>
    dplyr::summarise(
      total = sum(rainfed_ha + irrigated_ha),
      .by = area_code
    ) |>
    dplyr::arrange(area_code)

  testthat::expect_equal(result$polycell_id, c("a", "b"))
  testthat::expect_equal(totals$total, c(100, 200), tolerance = 1e-6)

  cft_map <- tibble::tibble(
    item_prod_code = 15L,
    cft_name = "temperate_cereals"
  )
  cft_result <- whep::build_gridded_landuse(
    country_areas,
    crop_patterns,
    gridded_cropland,
    country_grid,
    config = list(cft_mapping = cft_map)
  )

  testthat::expect_equal(nrow(cft_result), 2L)
  testthat::expect_setequal(cft_result$area_code, c(1L, 2L))
})

# S-A6 -- every allocation is keyed on the polycell -----------------------------
#
# The border fixture the criterion asks for: two polities sharing one cell with
# deliberately incompatible national totals. It is built to be NON-degenerate,
# because a fixture with one key per cell, equal shares or equal totals cannot
# tell polycell keying apart from cell-then-split: two polycells per shared
# cell, unequal shares (10/90), totals differing 100-fold, and each polity also
# present in a cell it holds alone so its share denominator is testable.
.sa6_border_fixture <- function(area_1 = 1000, area_2 = 10) {
  list(
    country_areas = tibble::tribble(
      ~year, ~area_code, ~item_prod_code, ~harvested_area_ha,
      2000L,         1L,             15L,             area_1,
      2000L,         2L,             15L,             area_2
    ),
    crop_patterns = tibble::tribble(
      ~lon,  ~lat, ~item_prod_code, ~harvest_fraction,
      0.25, 50.25,             15L,               0.5,
      0.75, 50.25,             15L,               0.5,
      1.25, 50.25,             15L,               0.5
    ),
    gridded_cropland = tibble::tribble(
      ~lon,  ~lat,  ~year, ~cropland_ha,
      0.25, 50.25, 2000L,       100000,
      0.75, 50.25, 2000L,       100000,
      1.25, 50.25, 2000L,       100000
    ),
    country_grid = tibble::tribble(
      ~polycell_id,  ~lon,  ~lat, ~area_code, ~cell_area_frac,
      "shared-1",   0.25, 50.25,         1L,             0.1,
      "shared-2",   0.25, 50.25,         2L,             0.9,
      "own-1",      0.75, 50.25,         1L,             1.0,
      "own-2",      1.25, 50.25,         2L,             1.0
    )
  )
}

testthat::test_that("a shared cell delivers only what each polycell carries", {
  fix <- .sa6_border_fixture()
  result <- do.call(whep::build_gridded_landuse, fix)

  # Every output row is polycell-keyed, and the shared cell carries two.
  testthat::expect_setequal(
    result$polycell_id,
    c("shared-1", "shared-2", "own-1", "own-2")
  )

  totals <- result |>
    dplyr::summarise(total = sum(rainfed_ha + irrigated_ha), .by = area_code) |>
    dplyr::arrange(area_code)
  testthat::expect_equal(totals$total, c(1000, 10), tolerance = 1e-9)

  # Each polity's share denominator is over ITS OWN polycells: polity 1 has
  # 0.5 * 100000 * 0.1 in the shared cell against 0.5 * 100000 in its own, so
  # it keeps 1000 * 5000 / 55000 there; polity 2 keeps 10 * 45000 / 95000.
  by_pc <- stats::setNames(
    result$rainfed_ha + result$irrigated_ha,
    result$polycell_id
  )
  testthat::expect_equal(
    unname(by_pc[["shared-1"]]),
    1000 * 5000 / 55000,
    tolerance = 1e-9
  )
  testthat::expect_equal(
    unname(by_pc[["shared-2"]]),
    10 * 45000 / 95000,
    tolerance = 1e-9
  )

  # THE discriminating assertion. Computing the cell's allocation and splitting
  # it afterwards makes the two compartments' shares equal to the AREA shares,
  # 0.1 / 0.9. Keyed on the polycell they are 19.2, because the national totals
  # differ 100-fold. Conservation alone cannot tell these apart.
  ratio <- by_pc[["shared-1"]] / by_pc[["shared-2"]]
  testthat::expect_gt(ratio, 19)
  testthat::expect_false(isTRUE(all.equal(ratio, 0.1 / 0.9)))
})

testthat::test_that("a neighbour's national total cannot move a polycell", {
  # S-A3/S-A6 stated as an independence property rather than a number: change
  # ONLY polity 1's national area and polity 2's grid must be bit-identical.
  small <- do.call(whep::build_gridded_landuse, .sa6_border_fixture(1000, 10))
  # The large run breaches polity 1's own ceiling and says so; that warning is
  # incidental here and is pinned by its own test below.
  large <- suppressWarnings(do.call(
    whep::build_gridded_landuse,
    .sa6_border_fixture(50000000, 10)
  ))
  pick <- function(x) {
    x |>
      dplyr::filter(area_code == 2L) |>
      dplyr::arrange(polycell_id) |>
      dplyr::select(polycell_id, rainfed_ha, irrigated_ha)
  }
  testthat::expect_identical(pick(small), pick(large))
  # ... and polity 1 really did move, so the test is not vacuously satisfied.
  testthat::expect_gt(
    sum(dplyr::filter(large, area_code == 1L)$rainfed_ha),
    sum(dplyr::filter(small, area_code == 1L)$rainfed_ha)
  )
})

testthat::test_that("capacity is clipped per polycell, not per physical cell", {
  # The shared cell holds 1000 ha of cropland split 50/50, so each compartment
  # may carry 500 ha. Polity 1 is forced over its half; polity 2 must not be
  # rescaled by its neighbour's overload.
  fix <- .sa6_border_fixture()
  fix$gridded_cropland <- tibble::tribble(
    ~lon,  ~lat,  ~year, ~cropland_ha,
    0.25, 50.25, 2000L,         1000,
    0.75, 50.25, 2000L,          100,
    1.25, 50.25, 2000L,       100000
  )
  fix$country_grid$cell_area_frac <- c(0.5, 0.5, 1, 1)
  fix$country_areas$harvested_area_ha <- c(2000, 10)

  result <- suppressWarnings(do.call(whep::build_gridded_landuse, fix))
  totals <- result |>
    dplyr::summarise(total = sum(rainfed_ha + irrigated_ha), .by = area_code) |>
    dplyr::arrange(area_code)
  testthat::expect_equal(totals$total, c(2000, 10), tolerance = 1e-6)

  # Polity 2's own two compartments are untouched by polity 1's redistribution.
  reference <- fix
  reference$country_areas$harvested_area_ha <- c(600, 10)
  ref_result <- suppressWarnings(
    do.call(whep::build_gridded_landuse, reference)
  )
  pick <- function(x) {
    x |>
      dplyr::filter(area_code == 2L) |>
      dplyr::arrange(polycell_id) |>
      dplyr::pull(rainfed_ha)
  }
  testthat::expect_equal(pick(result), pick(ref_result), tolerance = 1e-12)
})

testthat::test_that("a breached capacity ceiling is reported, not absorbed", {
  # AM-5 risk 19. `.redistribute_country_dt()` rescales back to the national
  # target after the logit passes, so when a country's cells cannot hold its
  # total the per-cell ceiling gives way silently. Shrinking the land
  # denominator makes this strictly more common, so the breach is measured.
  fix <- list(
    country_areas = tibble::tribble(
      ~year, ~area_code, ~item_prod_code, ~harvested_area_ha,
      2000L,         1L,             15L,                500
    ),
    crop_patterns = tibble::tribble(
      ~lon,  ~lat, ~item_prod_code, ~harvest_fraction,
      0.25, 50.25,             15L,               1.0
    ),
    gridded_cropland = tibble::tribble(
      ~lon,  ~lat,  ~year, ~cropland_ha,
      0.25, 50.25, 2000L,          100
    ),
    country_grid = tibble::tribble(
      ~lon,  ~lat, ~area_code, ~cell_area_frac,
      0.25, 50.25,         1L,               1
    )
  )
  testthat::expect_warning(
    result <- do.call(whep::build_gridded_landuse, fix),
    "more harvested area than their capacity"
  )
  # The magnitude is reported, and it is the real excess: 500 allocated into a
  # 100 ha ceiling.
  testthat::expect_warning(
    do.call(whep::build_gridded_landuse, fix),
    "400 ha over"
  )
  testthat::expect_equal(sum(result$rainfed_ha), 500, tolerance = 1e-6)

  # Halving the compartment's share halves the ceiling and the breach grows --
  # the mechanism the migration makes more common, pinned.
  tighter <- fix
  tighter$country_grid$cell_area_frac <- 0.5
  testthat::expect_warning(
    do.call(whep::build_gridded_landuse, tighter),
    "450 ha over"
  )

  # A country that fits inside its ceiling raises nothing.
  roomy <- fix
  roomy$gridded_cropland$cropland_ha <- 10000
  testthat::expect_no_warning(do.call(whep::build_gridded_landuse, roomy))
})

testthat::test_that("splitting a polycell in two changes no cell's allocation", {
  # The one S-A6 property the border fixtures above cannot reach: two polycells
  # of the SAME `area_code` inside one cell. That is not hypothetical -- DA-23
  # records `polity_area_crosswalk` folding 544 polity codes into 201 buckets,
  # with bucket 206 holding Sudan and South Sudan, so a cell on that border
  # yields exactly this shape.
  #
  # Both potential and capacity are linear in `cell_area_frac`, so splitting one
  # compartment into two whose shares sum to the original is EXACTLY neutral
  # under polycell-keyed redistribution. Grouping `.cell_group` on
  # (lon, lat) instead pools the two and then takes `first(rf_capacity)` --
  # one compartment's ceiling applied to both -- and the identity breaks.
  # Capacity must bind, or the redistribution never runs and the test is
  # vacuous; the warning below is the evidence that it did.
  base_fix <- list(
    country_areas = tibble::tribble(
      ~year, ~area_code, ~item_prod_code, ~harvested_area_ha,
      2000L,         1L,             15L,               1500
    ),
    crop_patterns = tibble::tribble(
      ~lon,  ~lat, ~item_prod_code, ~harvest_fraction,
      0.25, 50.25,             15L,               1.0,
      0.75, 50.25,             15L,               0.1
    ),
    gridded_cropland = tibble::tribble(
      ~lon,  ~lat,  ~year, ~cropland_ha,
      0.25, 50.25, 2000L,         1000,
      0.75, 50.25, 2000L,         1000
    )
  )
  whole <- c(
    base_fix,
    list(
      country_grid = tibble::tribble(
    ~polycell_id,  ~lon,  ~lat, ~area_code, ~cell_area_frac,
    "a",          0.25, 50.25,         1L,             1.0,
    "b",          0.75, 50.25,         1L,             1.0
  )
    )
  )
  split <- c(
    base_fix,
    list(
      country_grid = tibble::tribble(
    ~polycell_id,  ~lon,  ~lat, ~area_code, ~cell_area_frac,
    "a1",         0.25, 50.25,         1L,             0.4,
    "a2",         0.25, 50.25,         1L,             0.6,
    "b",          0.75, 50.25,         1L,             1.0
  )
    )
  )

  testthat::expect_warning(
    whole_out <- do.call(whep::build_gridded_landuse, whole),
    "more harvested area than their capacity"
  )
  split_out <- suppressWarnings(do.call(whep::build_gridded_landuse, split))

  per_cell <- function(x) {
    x |>
      dplyr::summarise(
        total = sum(rainfed_ha + irrigated_ha),
        .by = c(lon, lat)
      ) |>
      dplyr::arrange(lon) |>
      dplyr::pull(total)
  }
  testthat::expect_equal(
    per_cell(split_out),
    per_cell(whole_out),
    tolerance = 1e-9
  )
  # Not vacuous: the two cells really do hold different amounts, and the
  # concentrated cell really was pushed past its ceiling.
  testthat::expect_gt(per_cell(whole_out)[[1L]], per_cell(whole_out)[[2L]])
  testthat::expect_gt(per_cell(whole_out)[[1L]], 1000)
  # The split cell's two compartments keep their own shares of it.
  split_shared <- split_out |>
    dplyr::filter(lon == 0.25) |>
    dplyr::arrange(polycell_id)
  testthat::expect_equal(
    split_shared$rainfed_ha[[2L]] / split_shared$rainfed_ha[[1L]],
    0.6 / 0.4,
    tolerance = 1e-9
  )
})

testthat::test_that("a pattern cell no compartment claims gets no allocation", {
  # `.build_base_grid_cp()` keeps every `crop_patterns` cell, so a cell outside
  # `country_grid` used to arrive with an NA `area_code` and an NA share that
  # was replaced by 1 -- a whole-cell allocation to a polity that does not
  # exist, reachable whenever `country_areas` itself carries an NA code.
  fix <- list(
    country_areas = tibble::tribble(
      ~year, ~area_code, ~item_prod_code, ~harvested_area_ha,
      2000L,         1L,             15L,                100,
      2000L,         NA_integer_,    15L,               9999
    ),
    crop_patterns = tibble::tribble(
      ~lon,  ~lat, ~item_prod_code, ~harvest_fraction,
      0.25, 50.25,             15L,               1.0,
      9.25,  9.25,             15L,               1.0
    ),
    gridded_cropland = tibble::tribble(
      ~lon,  ~lat,  ~year, ~cropland_ha,
      0.25, 50.25, 2000L,         1000,
      9.25,  9.25, 2000L,         1000
    ),
    country_grid = tibble::tribble(
      ~lon,  ~lat, ~area_code, ~cell_area_frac,
      0.25, 50.25,         1L,               1
    )
  )
  testthat::expect_warning(
    result <- do.call(whep::build_gridded_landuse, fix),
    "in no polity compartment"
  )
  testthat::expect_false(any(is.na(result$area_code)))
  testthat::expect_false(any(result$lon == 9.25))
  # The unkeyed national total is not fabricated onto the orphan cell.
  testthat::expect_equal(
    sum(result$rainfed_ha + result$irrigated_ha),
    100,
    tolerance = 1e-9
  )
})

testthat::test_that("time-varying country grids select the valid polity year", {
  country_areas <- tibble::tribble(
    ~year, ~area_code, ~item_prod_code, ~harvested_area_ha,
    1990L,         1L,             15L,                100,
    2000L,         2L,             15L,                200
  )
  crop_patterns <- tibble::tribble(
    ~lon,  ~lat, ~item_prod_code, ~harvest_fraction,
    0.25, 50.25,             15L,               1.0
  )
  gridded_cropland <- tibble::tribble(
    ~lon, ~lat, ~year, ~cropland_ha,
    0.25, 50.25, 1990L, 1000,
    0.25, 50.25, 2000L, 1000
  )
  country_grid <- tibble::tribble(
    ~lon,  ~lat, ~area_code, ~cell_area_frac, ~year,
    0.25, 50.25,         1L,               1, 1990L,
    0.25, 50.25,         2L,               1, 2000L
  )

  result <- whep::build_gridded_landuse(
    country_areas,
    crop_patterns,
    gridded_cropland,
    country_grid
  )

  actual <- result |>
    dplyr::arrange(year) |>
    dplyr::select(year, area_code, rainfed_ha)

  testthat::expect_equal(actual$year, c(1990L, 2000L))
  testthat::expect_equal(actual$area_code, c(1L, 2L))
  testthat::expect_equal(actual$rainfed_ha, c(100, 200), tolerance = 1e-6)
})

# area_key ----------------------------------------------------------------

# 276 Sudan and 277 South Sudan are both reporting areas of bucket 206, so a
# grid keyed on them cannot join a polity-keyed national table; 68 is its own
# bucket and stands in for the rest of the world, which must stay untouched.
off_bucket_fixture <- function() {
  list(
    country_areas = tibble::tribble(
      ~year, ~area_code, ~item_prod_code, ~harvested_area_ha,
      2000L,       276L,             15L,               1000,
      2000L,       277L,             15L,                500,
      2000L,        68L,             15L,                300
    ),
    crop_patterns = tibble::tribble(
      ~lon,  ~lat, ~item_prod_code, ~harvest_fraction,
      0.25, 50.25,             15L,               1.0,
      0.75, 50.25,             15L,               1.0,
      1.25, 50.25,             15L,               1.0
    ),
    gridded_cropland = tibble::tribble(
      ~lon,  ~lat, ~year, ~cropland_ha,
      0.25, 50.25, 2000L,        10000,
      0.75, 50.25, 2000L,        10000,
      1.25, 50.25, 2000L,        10000
    ),
    # The middle cell is shared by two areas of one bucket: that is the fold
    # the re-key has to collapse without picking a winner.
    country_grid = tibble::tribble(
      ~lon,  ~lat, ~area_code, ~cell_area_frac,
      0.25, 50.25,       276L,             1.0,
      0.75, 50.25,       276L,             0.6,
      0.75, 50.25,       277L,             0.4,
      1.25, 50.25,        68L,             1.0
    )
  )
}

testthat::test_that("build_gridded_landuse warns on off-bucket area codes", {
  fix <- off_bucket_fixture()

  testthat::expect_warning(
    result <- whep::build_gridded_landuse(
      fix$country_areas,
      fix$crop_patterns,
      fix$gridded_cropland,
      fix$country_grid
    ),
    "cannot join"
  )
  # The default reproduces the engine's own codes, disagreement and all.
  testthat::expect_setequal(result$area_code, c(276L, 277L, 68L))
  testthat::expect_equal(
    sum(result$area_code != result$polity_area_code),
    3L
  )
  testthat::expect_false(rlang::has_name(result, "grid_area_code"))
})

testthat::test_that("build_gridded_landuse keeps on-bucket grids silent", {
  fix <- off_bucket_fixture()
  on_bucket <- dplyr::filter(fix$country_grid, area_code == 68L)
  on_bucket_patterns <- dplyr::filter(fix$crop_patterns, lon == 1.25)
  on_bucket_cropland <- dplyr::filter(fix$gridded_cropland, lon == 1.25)

  testthat::expect_no_warning(
    whep::build_gridded_landuse(
      dplyr::filter(fix$country_areas, area_code == 68L),
      on_bucket_patterns,
      on_bucket_cropland,
      on_bucket
    )
  )
})

testthat::test_that("area_key = polity_area leaves no disagreeing key", {
  fix <- off_bucket_fixture()

  result <- whep::build_gridded_landuse(
    fix$country_areas,
    fix$crop_patterns,
    fix$gridded_cropland,
    fix$country_grid,
    config = list(area_key = "polity_area")
  )

  # The invariant #582 asks for: no row carries two territorial keys that
  # disagree, and every key is one a polity-keyed national table publishes.
  testthat::expect_equal(
    sum(result$area_code != result$polity_area_code),
    0L
  )
  testthat::expect_equal(whep:::.cell_polity_off_bucket(result), integer(0))
  pointblank::expect_col_vals_in_set(
    result,
    columns = "area_code",
    set = c(68L, 206L)
  )
  # One area_code carries one polity label.
  labels <- dplyr::distinct(
    result,
    area_code,
    reporting_polity_code,
    reporting_polity_name
  )
  testthat::expect_equal(nrow(labels), dplyr::n_distinct(result$area_code))
})

testthat::test_that("area_key = polity_area carries the raw code and mass", {
  fix <- off_bucket_fixture()

  keyed <- whep::build_gridded_landuse(
    fix$country_areas,
    fix$crop_patterns,
    fix$gridded_cropland,
    fix$country_grid,
    config = list(area_key = "polity_area")
  )
  raw <- suppressWarnings(whep::build_gridded_landuse(
    fix$country_areas,
    fix$crop_patterns,
    fix$gridded_cropland,
    fix$country_grid
  ))

  testthat::expect_equal(
    sum(keyed$rainfed_ha + keyed$irrigated_ha),
    sum(raw$rainfed_ha + raw$irrigated_ha),
    tolerance = 1e-9
  )
  # The shared cell folds two reporting areas into one bucket row, and both
  # raw codes survive the fold rather than one being picked.
  shared <- dplyr::filter(keyed, lon == 0.75)
  testthat::expect_equal(nrow(shared), 1L)
  testthat::expect_equal(shared$grid_area_code, "276+277")
  testthat::expect_equal(
    shared$rainfed_ha + shared$irrigated_ha,
    sum(
      dplyr::filter(raw, lon == 0.75) |>
        dplyr::pull(rainfed_ha),
      dplyr::filter(raw, lon == 0.75) |>
        dplyr::pull(irrigated_ha)
    ),
    tolerance = 1e-9
  )
  # An area that is already its own bucket keeps its code unchanged.
  testthat::expect_equal(
    dplyr::filter(keyed, area_code == 68L)$grid_area_code,
    "68"
  )
})

testthat::test_that("build_gridded_landuse rejects an unknown area_key", {
  fix <- off_bucket_fixture()

  testthat::expect_error(
    whep::build_gridded_landuse(
      fix$country_areas,
      fix$crop_patterns,
      fix$gridded_cropland,
      fix$country_grid,
      config = list(area_key = "polity")
    ),
    class = "rlang_error"
  )
})

testthat::test_that("the re-key keeps a code the crosswalk does not carry", {
  # A gap must stay visible as its own code rather than becoming an NA key.
  unknown <- tibble::tibble(
    year = 2000L,
    area_code = 99999L,
    lon = 0.25,
    lat = 50.25,
    rainfed_ha = 5,
    irrigated_ha = 1
  )

  out <- whep:::.spatialize_to_bucket(unknown, c("rainfed_ha", "irrigated_ha"))

  testthat::expect_equal(out$area_code, 99999L)
  testthat::expect_equal(out$grid_area_code, "99999")
  testthat::expect_equal(out$rainfed_ha, 5)
})

# --- Reporting areas the grid cannot represent at all (whep#461) ---------
#
# Substituting one cell-to-polity crosswalk for another is a data-wiring
# change with no engine change, so nothing in the engine notices when the two
# grids disagree about which reporting codes exist. On the deployed pair they
# do: the fractional crosswalk still keys Ethiopia 62 and Sudan 206 where the
# centroid grid uses 238 and 276, and swapping it in deletes both countries'
# whole national total. `.warn_unallocated_crops()` cannot show that -- it
# fires per (country, crop) per year and already names 178 codes on the
# unswapped grid, so two more codes in that list are invisible.
.grid_vintage_fixture <- function() {
  list(
    country_areas = tibble::tribble(
      ~year, ~area_code, ~item_prod_code, ~harvested_area_ha,
      2000L,       238L,             15L,               1000,
      2000L,        68L,             15L,                500
    ),
    crop_patterns = tibble::tribble(
      ~lon,  ~lat, ~item_prod_code, ~harvest_fraction,
      0.25, 50.25,             15L,                 1,
      0.75, 50.25,             15L,                 1
    ),
    gridded_cropland = tibble::tribble(
      ~lon,  ~lat,  ~year, ~cropland_ha,
      0.25, 50.25, 2000L,          5000,
      0.75, 50.25, 2000L,          5000
    ),
    # `cell_area_frac` is explicit because C8/S-A5 forbids the share-less grid
    # this fixture arrived with: `.abort_missing_polity_share()` refuses a
    # crosswalk carrying no share rather than defaulting it to 1 and handing a
    # border cell wholly to one polity. `1` is the honest value here -- each of
    # these cells is owned outright by the single polity keyed on it -- so the
    # missing-reporter behaviour under test is unchanged and the guard is not
    # weakened to accommodate the fixture.
    live_grid = tibble::tribble(
      ~lon,  ~lat, ~area_code, ~cell_area_frac,
      0.25, 50.25,       238L,               1,
      0.75, 50.25,        68L,               1
    ),
    retired_grid = tibble::tribble(
      ~lon,  ~lat, ~area_code, ~cell_area_frac,
      0.25, 50.25,        62L,               1,
      0.75, 50.25,        68L,               1
    )
  )
}

testthat::test_that("a grid holding every reporting code warns about none", {
  fix <- .grid_vintage_fixture()

  testthat::expect_no_warning(
    whep::build_gridded_landuse(
      fix$country_areas,
      fix$crop_patterns,
      fix$gridded_cropland,
      fix$live_grid
    )
  )
})

testthat::test_that("a re-keyed grid names the reporting areas it deletes", {
  fix <- .grid_vintage_fixture()

  warnings <- testthat::capture_warnings(
    result <- whep::build_gridded_landuse(
      fix$country_areas,
      fix$crop_patterns,
      fix$gridded_cropland,
      fix$retired_grid
    )
  )

  testthat::expect_match(
    warnings,
    "no cell in .*country_grid.* at all",
    all = FALSE
  )
  # The quantity at stake is the discriminator: without it the message cannot
  # tell a deleted country from a deleted island.
  testthat::expect_match(warnings, "1000 ha of harvested area", all = FALSE)
  testthat::expect_false(238L %in% result$area_code)
})

testthat::test_that("the missing-reporter warning names every absent code", {
  fn <- whep:::.warn_grid_missing_reporters
  national <- tibble::tribble(
    ~area_code, ~harvested_area_ha,
    238L,                     1000,
    276L,                      500,
    68L,                        10
  )
  grid <- tibble::tibble(area_code = 68L)

  testthat::expect_warning(
    fn(national, grid, "harvested_area_ha", "ha of harvested area"),
    "238"
  )
  testthat::expect_warning(
    fn(national, grid, "harvested_area_ha", "ha of harvested area"),
    "1500 ha"
  )
  testthat::expect_no_warning(
    fn(
      national,
      tibble::tibble(area_code = c(68L, 238L, 276L)),
      "harvested_area_ha",
      "ha of harvested area"
    )
  )
})

testthat::test_that("a national table without the value column still warns", {
  fn <- whep:::.warn_grid_missing_reporters

  testthat::expect_warning(
    fn(
      tibble::tibble(area_code = 238L),
      tibble::tibble(area_code = 68L),
      "harvested_area_ha",
      "ha of harvested area"
    ),
    "238"
  )
})
