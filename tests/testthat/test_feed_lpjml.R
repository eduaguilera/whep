test_that("grass_access_shares returns the documented defaults", {
  s <- whep::grass_access_shares()
  expect_equal(s$aboveground, 0.46)
  expect_equal(s$grazable, 1)
  expect_equal(s$w_c_dm, 0.45)
})

test_that(".lpjml_grass_to_dm converts gC/m2/yr to grazable t DM/ha/yr", {
  shares <- whep::grass_access_shares(
    aboveground = 0.46,
    grazable = 1,
    w_c_dm = 0.45
  )
  # 1 gC/m2 = 0.01 tC/ha; x aboveground x grazable / w_c_dm -> t DM/ha.
  expect_equal(
    whep:::.lpjml_grass_to_dm(600, shares),
    600 * 0.46 * 1 * 0.01 / 0.45,
    tolerance = 1e-9
  )
})

test_that("a lower grazable share lowers availability proportionally", {
  full <- whep::grass_access_shares(grazable = 1)
  half <- whep::grass_access_shares(grazable = 0.5)
  expect_equal(
    whep:::.lpjml_grass_to_dm(600, half),
    whep:::.lpjml_grass_to_dm(600, full) / 2,
    tolerance = 1e-9
  )
})

test_that("build_grass_availability_lpjml(example = TRUE) returns the tidy schema", {
  av <- whep::build_grass_availability_lpjml(example = TRUE)
  expect_s3_class(av, "tbl_df")
  expect_setequal(
    names(av),
    c(
      "lon",
      "lat",
      "year",
      "grass_npp_gc_m2",
      "grass_avail_dm_t_ha",
      "grass_avail_dm_t"
    )
  )
  expect_true(all(av$grass_avail_dm_t_ha >= 0))
  expect_true(all(av$grass_avail_dm_t >= 0))
})

test_that("build_grass_availability_lpjml defaults to pinned artifacts", {
  testthat::local_mocked_bindings(
    whep_read_file = function(file_alias, ...) {
      expect_equal(file_alias, "lpjml-grass-availability")
      tibble::tibble(
        lon = 0.25,
        lat = 50.25,
        year = c(1999L, 2000L),
        grass_npp_gc_m2 = c(400, 500)
      )
    },
    .package = "whep"
  )
  av <- whep::build_grass_availability_lpjml(
    run_dir = NULL,
    years = 2000L
  )
  expect_equal(av$year, 2000L)
  expect_setequal(
    names(av),
    c(
      "lon",
      "lat",
      "year",
      "grass_npp_gc_m2",
      "grass_avail_dm_t_ha",
      "grass_avail_dm_t"
    )
  )
  expect_true(av$grass_avail_dm_t_ha > 0)
  expect_true(av$grass_avail_dm_t > 0)
})

test_that("build_grass_availability_lpjml accepts custom artifact data", {
  artifact <- tibble::tibble(
    lon = 0.25,
    lat = 50.25,
    year = c(1999L, 2000L),
    grass_npp_gc_m2 = c(400, 500)
  )
  av <- whep::build_grass_availability_lpjml(
    availability = artifact,
    years = 2000L
  )
  expect_equal(av$year, 2000L)
  expect_true(av$grass_avail_dm_t_ha > 0)
  expect_true(av$grass_avail_dm_t > 0)
})

test_that("build_grass_availability_lpjml accepts custom artifact paths", {
  artifact <- tibble::tibble(
    lon = 0.25,
    lat = 50.25,
    year = 2000L,
    grass_avail_dm_t_ha = 2,
    grass_avail_dm_t = 100
  )
  path <- withr::local_tempfile(fileext = ".parquet")
  nanoparquet::write_parquet(artifact, path)
  av <- whep::build_grass_availability_lpjml(availability_path = path)
  expect_equal(av$grass_avail_dm_t_ha, 2)
  expect_equal(av$grass_avail_dm_t, 100)
})

test_that("build_grass_availability_lpjml rejects mixed custom sources", {
  expect_error(
    whep::build_grass_availability_lpjml(
      run_dir = "/tmp/local-run",
      availability = tibble::tibble(lon = 0, lat = 0, year = 2000L)
    ),
    "either a custom availability artifact"
  )
})

test_that("build_grass_availability dispatches lpjml and records the method", {
  av <- whep::build_grass_availability(method = "lpjml", example = TRUE)
  expect_true("method_grass" %in% names(av))
  expect_equal(unique(av$method_grass), "lpjml")
})

test_that("build_grass_availability errors on the unimplemented coefficient method", {
  expect_error(
    whep::build_grass_availability(method = "coefficient"),
    "not yet implemented"
  )
})

test_that("aggregate_grass_to_polity conserves total grass", {
  grass <- whep::build_grass_availability(method = "lpjml", example = TRUE)
  cp <- tibble::tibble(
    lon = grass$lon,
    lat = grass$lat,
    area_code = 1L,
    polity_frac = 1
  )
  agg <- whep::aggregate_grass_to_polity(grass, cp)
  expect_setequal(names(agg), c("area_code", "year", "grass_avail_dm_t"))
  expect_equal(
    sum(agg$grass_avail_dm_t),
    sum(grass$grass_avail_dm_t),
    tolerance = 1e-6
  )
})

test_that("aggregate_grass_to_polity splits a border cell by polity_frac", {
  grass <- tibble::tibble(
    lon = 0.25,
    lat = 0.25,
    year = 2000L,
    grass_avail_dm_t = 100
  )
  cp <- tibble::tibble(
    lon = c(0.25, 0.25),
    lat = c(0.25, 0.25),
    area_code = c(1L, 2L),
    polity_frac = c(0.7, 0.3)
  )
  agg <- whep::aggregate_grass_to_polity(grass, cp)
  expect_equal(agg$grass_avail_dm_t[agg$area_code == 1L], 70)
  expect_equal(agg$grass_avail_dm_t[agg$area_code == 2L], 30)
})

test_that("read_lpjml_grass_productivity(example = TRUE) returns the tidy schema", {
  gp <- whep::read_lpjml_grass_productivity(example = TRUE)
  expect_s3_class(gp, "tbl_df")
  expect_setequal(names(gp), c("lon", "lat", "year", "grass_npp"))
  expect_true(all(gp$grass_npp > 0))
})

test_that("read_lpjml_grass_productivity defaults to pinned artifacts", {
  testthat::local_mocked_bindings(
    whep_read_file = function(file_alias, ...) {
      expect_equal(file_alias, "lpjml-grass-productivity")
      tibble::tibble(
        lon = 0.25,
        lat = 50.25,
        year = c(1999L, 2000L),
        grass_npp = c(0, 700)
      )
    },
    .package = "whep"
  )
  gp <- whep::read_lpjml_grass_productivity(
    run_dir = NULL,
    years = 2000L
  )
  expect_equal(gp$year, 2000L)
  expect_equal(gp$grass_npp, 700)
})

test_that("read_lpjml_grass_productivity accepts custom artifact data", {
  artifact <- tibble::tibble(
    lon = 0.25,
    lat = 50.25,
    year = c(1999L, 2000L),
    grass_npp = c(0, 700)
  )
  gp <- whep::read_lpjml_grass_productivity(
    productivity = artifact,
    years = 2000L
  )
  expect_equal(gp$year, 2000L)
  expect_equal(gp$grass_npp, 700)
})

test_that("read_lpjml_grass_productivity accepts custom artifact paths", {
  artifact <- tibble::tibble(
    lon = 0.25,
    lat = 50.25,
    year = 2000L,
    grass_npp = 700
  )
  path <- withr::local_tempfile(fileext = ".parquet")
  nanoparquet::write_parquet(artifact, path)
  gp <- whep::read_lpjml_grass_productivity(productivity_path = path)
  expect_equal(gp$grass_npp, 700)
})

test_that(".clip_run_years drops out-of-coverage years with a warning", {
  # An LPJmL run covering 1901-2009 (109 time steps) must skip requested years
  # outside it rather than abort the read on an out-of-bounds index.
  expect_warning(
    out <- whep:::.clip_run_years(
      c(1850L, 2000L, 2020L),
      1901L,
      109L,
      "pft_npp.nc"
    ),
    "outside the run's coverage"
  )
  expect_equal(out, 2000L)
  # All in-range years pass through untouched, no warning.
  expect_silent(
    keep <- whep:::.clip_run_years(c(1950L, 2000L), 1901L, 109L, "pft_npp.nc")
  )
  expect_equal(keep, c(1950L, 2000L))
})
