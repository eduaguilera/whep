testthat::test_that("make_lpjml_covariate returns custom functions unchanged", {
  custom <- function(centroids_sf, year) rep(year, nrow(centroids_sf))
  testthat::expect_identical(
    whep::make_lpjml_covariate(weighting = custom),
    custom
  )
})

.lpjml_covariate_points <- function() {
  testthat::skip_if_not_installed("sf")
  sf::st_as_sf(
    tibble::tibble(
      lon = c(0.26, 0.74, 9),
      lat = c(50.24, 50.25, 50.25)
    ),
    coords = c("lon", "lat"),
    crs = 4326
  )
}

.write_lpjml_covariate_inputs <- function(dir) {
  gridded_cropland <- tibble::tribble(
    ~lon, ~lat, ~year, ~cropland_ha,
    0.25, 50.25, 2000L,         100,
    0.75, 50.25, 2000L,         200,
    0.25, 50.75, 2000L,          50,
    0.75, 50.75, 2000L,          50
  )
  type_cropland <- tibble::tribble(
    ~lon, ~lat, ~year, ~luh2_type, ~type_ha,
    0.25, 50.25, 2000L, "c3ann",      100,
    0.75, 50.25, 2000L, "c3ann",      100,
    0.25, 50.75, 2000L, "c3ann",       50,
    0.75, 50.75, 2000L, "c3ann",       50,
    0.25, 50.25, 2000L, "c4ann",      500,
    0.75, 50.25, 2000L, "c4ann",      500
  )
  crop_patterns <- tibble::tribble(
    ~lon, ~lat, ~item_prod_code, ~harvest_fraction,
    0.25, 50.25,             15L,              0.25,
    0.75, 50.25,             15L,              0.75,
    0.25, 50.75,             15L,              0.10,
    0.75, 50.75,             15L,              0.10
  )
  nanoparquet::write_parquet(
    gridded_cropland,
    file.path(dir, "gridded_cropland.parquet")
  )
  nanoparquet::write_parquet(
    type_cropland,
    file.path(dir, "type_cropland.parquet")
  )
  nanoparquet::write_parquet(
    crop_patterns,
    file.path(dir, "crop_patterns.parquet")
  )
}

testthat::test_that("total cropland covariate reads local prepared inputs", {
  tmp <- withr::local_tempdir()
  .write_lpjml_covariate_inputs(tmp)

  covariate <- whep::make_lpjml_covariate(
    input_dir = tmp,
    years = 2000L,
    weighting = "total_cropland"
  )
  values <- covariate(.lpjml_covariate_points(), 2000L)

  testthat::expect_length(values, 3L)
  testthat::expect_gt(values[[1]], 0)
  testthat::expect_gt(values[[2]], values[[1]])
  testthat::expect_equal(values[[3]], 0)
})

testthat::test_that("crop-pattern covariate combines LUH2 type and crop pattern", {
  tmp <- withr::local_tempdir()
  .write_lpjml_covariate_inputs(tmp)
  cft_mapping <- tibble::tibble(
    item_prod_code = 15L,
    item_prod_name = "Wheat",
    luh2_type = "c3ann"
  )

  covariate <- whep::make_lpjml_covariate(
    input_dir = tmp,
    years = 2000L,
    weighting = "crop_pattern",
    item_prod_code = 15L,
    cft_mapping = cft_mapping
  )
  values <- covariate(.lpjml_covariate_points(), 2000L)

  testthat::expect_length(values, 3L)
  testthat::expect_gt(values[[1]], 0)
  testthat::expect_gt(values[[2]], values[[1]])
  testthat::expect_equal(values[[3]], 0)
  testthat::expect_match(attr(covariate, "label"), "Wheat")
})

testthat::test_that("crop-pattern covariate requires an item code", {
  testthat::expect_error(
    whep::make_lpjml_covariate(weighting = "crop_pattern"),
    "item_prod_code"
  )
})

testthat::test_that("covariate factory can read pinned spatialization inputs", {
  pins <- list(
    "spatialize-gridded-cropland" = tibble::tribble(
      ~lon, ~lat, ~year, ~cropland_ha,
      0.25, 50.25, 2000L,         100,
      0.75, 50.25, 2000L,         200,
      0.25, 50.75, 2000L,          50,
      0.75, 50.75, 2000L,          50
    )
  )
  testthat::local_mocked_bindings(
    whep_read_file = function(file_alias, ...) pins[[file_alias]],
    .package = "whep"
  )

  covariate <- whep::make_lpjml_covariate(
    years = 2000L,
    weighting = "total_cropland"
  )
  values <- covariate(.lpjml_covariate_points(), 2000L)

  testthat::expect_gt(values[[1]], 0)
  testthat::expect_gt(values[[2]], values[[1]])
})
