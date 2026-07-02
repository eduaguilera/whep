# Fixture mimicking hwsd_data.csv's real columns: one map unit per group of
# rows, each row one USDA texture class' share and pH within that unit.
.hwsd_attr_fixture <- function() {
  tibble::tribble(
    ~mu_global, ~t_usda_tex, ~share, ~t_ph_h2o,
    # Unit 1: texture 7 dominates (60%), so its pH (6.5) is picked, not the
    # higher-share-weighted average with texture 9's pH (5.0).
    1L, 7L, 60, 6.5,
    1L, 9L, 40, 5.0,
    # Unit 2: dominant texture's pH is missing -> defaults to 7.0.
    2L, 12L, 80, NA_real_,
    2L, 1L, 20, 8.2,
    # Unit 3: single texture class, single row.
    3L, 3L, 100, 4.9
  )
}

testthat::test_that(".derive_dominant_soil picks dominant texture's pH", {
  result <- whep:::.derive_dominant_soil(.hwsd_attr_fixture())

  pointblank::expect_col_exists(result, c("mu_global", "t_ph_h2o"))
  testthat::expect_equal(nrow(result), 3L)
  testthat::expect_equal(
    result$t_ph_h2o[result$mu_global == 1L],
    6.5
  )
  testthat::expect_equal(
    result$t_ph_h2o[result$mu_global == 3L],
    4.9
  )
})

testthat::test_that(".derive_dominant_soil defaults missing pH to 7.0", {
  result <- whep:::.derive_dominant_soil(.hwsd_attr_fixture())

  testthat::expect_equal(
    result$t_ph_h2o[result$mu_global == 2L],
    7.0
  )
})

testthat::test_that(".derive_dominant_soil drops rows with NA texture", {
  attr <- tibble::tribble(
    ~mu_global, ~t_usda_tex, ~share, ~t_ph_h2o,
    4L, NA_integer_, 100, 6.0,
    4L, 2L, 50, 5.5
  )
  result <- whep:::.derive_dominant_soil(attr)

  testthat::expect_equal(nrow(result), 1L)
  testthat::expect_equal(result$t_ph_h2o, 5.5)
})

# ---- .gapfill_soil() ---------------------------------------------------

testthat::test_that(".gapfill_soil fills a missing cell from a neighbour", {
  soil_grid <- tibble::tribble(
    ~lon, ~lat, ~soil_ph,
    -0.25, -0.25, 6.0
  )
  country_grid <- tibble::tribble(
    ~lon, ~lat,
    -0.25, -0.25,
    0.25, -0.25
  )
  result <- whep:::.gapfill_soil(soil_grid, country_grid)

  pointblank::expect_col_exists(result, c("lon", "lat", "soil_ph"))
  testthat::expect_equal(nrow(result), 2L)
  filled <- result[result$lon == 0.25 & result$lat == -0.25, ]
  testthat::expect_equal(filled$soil_ph, 6.0)
})

testthat::test_that(".gapfill_soil skips cells already present", {
  soil_grid <- tibble::tribble(
    ~lon, ~lat, ~soil_ph,
    -0.25, -0.25, 6.0
  )
  country_grid <- tibble::tribble(
    ~lon, ~lat,
    -0.25, -0.25
  )
  result <- whep:::.gapfill_soil(soil_grid, country_grid)

  testthat::expect_equal(nrow(result), 1L)
})

# ---- read_soil_ph() -----------------------------------------------------

testthat::test_that("read_soil_ph example fixture is schema-complete", {
  out <- whep::read_soil_ph(example = TRUE)

  pointblank::expect_col_exists(out, c("lon", "lat", "soil_ph"))
  pointblank::expect_col_vals_between(out, "soil_ph", 0, 14)
})

testthat::test_that("read_soil_ph aggregates HWSD raster + attributes", {
  testthat::skip_if_not_installed("terra")
  dir <- withr::local_tempdir()
  readr::write_csv(.hwsd_attr_fixture(), file.path(dir, "hwsd_data.csv"))

  rast <- terra::rast(
    nrows = 12,
    ncols = 12,
    xmin = -1,
    xmax = 1,
    ymin = -1,
    ymax = 1,
    resolution = 1 / 6
  )
  terra::values(rast) <- 1L
  terra::writeRaster(
    rast,
    file.path(dir, "hwsd.bil"),
    filetype = "EHdr",
    overwrite = TRUE
  )

  result <- whep::read_soil_ph(hwsd_dir = dir)

  pointblank::expect_col_exists(result, c("lon", "lat", "soil_ph"))
  testthat::expect_true(all(result$soil_ph == 6.5))
})
