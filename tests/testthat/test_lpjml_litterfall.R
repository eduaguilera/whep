# Litterfall reader. Everything here runs offline: the NetCDF handle is a
# stand-in carrying only the fields the helpers actually touch.

.fake_litfall_nc <- function(vars) {
  list(var = purrr::map(vars, \(n) list(size = n)))
}

testthat::test_that("the data variable is found by elimination, not by name", {
  # litfallc_agr.nc holds ALITFALLC_agr where its three siblings hold their
  # own filename. Keying on the name fails on exactly that one file, so the
  # reader must not key on it.
  agr <- .fake_litfall_nc(list(
    lon_bnds = 2,
    lat_bnds = 2,
    time_bnds = 2,
    ALITFALLC_agr = c(720, 277, 274)
  ))
  testthat::expect_identical(whep:::.litfall_var(agr), "ALITFALLC_agr")

  nv <- .fake_litfall_nc(list(
    lon_bnds = 2,
    lat_bnds = 2,
    time_bnds = 2,
    litfallc_nv = c(720, 277, 274)
  ))
  testthat::expect_identical(whep:::.litfall_var(nv), "litfallc_nv")

  total <- .fake_litfall_nc(list(
    time_bnds = 2,
    lat_bnds = 2,
    lon_bnds = 2,
    litfallc = c(720, 277, 274)
  ))
  testthat::expect_identical(whep:::.litfall_var(total), "litfallc")
})

testthat::test_that("a file with no data variable aborts", {
  bare <- .fake_litfall_nc(list(lon_bnds = 2, lat_bnds = 2, time_bnds = 2))
  testthat::expect_error(whep:::.litfall_var(bare), "no data variable")
})

testthat::test_that("each class maps to its own output file", {
  testthat::expect_identical(whep:::.litfall_file("nv"), "litfallc_nv.nc")
  testthat::expect_identical(whep:::.litfall_file("agr"), "litfallc_agr.nc")
  testthat::expect_identical(
    whep:::.litfall_file("mgrass"),
    "litfallc_mgrass.nc"
  )
  testthat::expect_identical(whep:::.litfall_file("luc"), "litfallc_luc.nc")
  # The total keeps LPJmL's own unsuffixed name.
  testthat::expect_identical(whep:::.litfall_file("total"), "litfallc.nc")
})

testthat::test_that("an unknown class is refused before any file is touched", {
  testthat::expect_error(
    whep::read_lpjml_litterfall(class = "pasture"),
    class = "rlang_error"
  )
})

testthat::test_that("a missing file names the output to add to the run", {
  dir <- withr::local_tempdir()
  testthat::expect_error(
    whep::read_lpjml_litterfall("nv", run_dir = dir),
    "litfallc_nv"
  )
  # And it says why an old run cannot simply be reused for the crop class.
  testthat::expect_error(
    whep::read_lpjml_litterfall("agr", run_dir = dir),
    "prefill"
  )
})

testthat::test_that("the example fixture is a usable natural-land tibble", {
  x <- whep::read_lpjml_litterfall(example = TRUE)
  testthat::expect_s3_class(x, "tbl_df")
  testthat::expect_true(
    all(
      c("lon", "lat", "year", "class", "litterfall_c_mgc_ha_yr") %in% names(x)
    )
  )
  testthat::expect_true(all(x$class == "nv"))
  testthat::expect_true(all(x$litterfall_c_mgc_ha_yr > 0))
})

testthat::test_that("a year outside the file's coverage is refused", {
  # Shared with the cover reader: a year that would index past the time
  # dimension must abort rather than read whatever sits at that offset.
  testthat::expect_error(
    whep:::.fpc_year_index(1750L, first_year = 1901L, steps = 123L),
    "outside the file's coverage"
  )
  testthat::expect_identical(
    whep:::.fpc_year_index(c(1901L, 2023L), first_year = 1901L, steps = 123L),
    c(1L, 123L)
  )
})

# ---- the grid must not expand: a real file, offline, with nlon != nlat -----

# Fixture-based tests cannot catch this. Inside `tibble()` a later expression
# sees the columns already bound, so `each = length(lon)` reads the lon COLUMN
# rather than the axis and the grid expands by a factor of nlon. It only
# appears on a genuine multi-cell read, so the test writes one.
.write_litfall_nc <- function(path, nlon, nlat, nyear, var = "litfallc_nv") {
  lon <- ncdf4::ncdim_def("lon", "degrees_east", seq_len(nlon) - 0.25)
  lat <- ncdf4::ncdim_def("lat", "degrees_north", seq_len(nlat) - 0.25)
  tm <- ncdf4::ncdim_def("time", "years", seq_len(nyear), unlim = TRUE)
  v <- ncdf4::ncvar_def(var, "gC/m2/yr", list(lon, lat, tm), -9999)
  nc <- ncdf4::nc_create(path, list(v))
  on.exit(ncdf4::nc_close(nc))
  ncdf4::ncvar_put(
    nc,
    v,
    array(seq_len(nlon * nlat * nyear) * 1.0, c(nlon, nlat, nyear))
  )
}

testthat::test_that("a real read returns exactly nlon * nlat rows per year", {
  testthat::skip_if_not_installed("ncdf4")
  dir <- withr::local_tempdir()
  # Deliberately unequal, and unequal to the year count, so a wrong recycling
  # cannot coincidentally produce the right length.
  .write_litfall_nc(
    file.path(dir, "litfallc_nv.nc"),
    nlon = 4,
    nlat = 3,
    nyear = 2
  )

  one <- whep::read_lpjml_litterfall(
    "nv",
    run_dir = dir,
    years = 1901L,
    first_year = 1901L
  )
  testthat::expect_identical(nrow(one), 12L)
  testthat::expect_identical(sort(unique(one$lon)), c(0.75, 1.75, 2.75, 3.75))
  testthat::expect_identical(sort(unique(one$lat)), c(0.75, 1.75, 2.75))

  both <- whep::read_lpjml_litterfall("nv", run_dir = dir, first_year = 1901L)
  testthat::expect_identical(nrow(both), 24L)
  testthat::expect_identical(sort(unique(both$year)), c(1901L, 1902L))
})

testthat::test_that("gC/m2/yr is converted to MgC/ha/yr", {
  testthat::skip_if_not_installed("ncdf4")
  dir <- withr::local_tempdir()
  .write_litfall_nc(
    file.path(dir, "litfallc_nv.nc"),
    nlon = 2,
    nlat = 2,
    nyear = 1
  )
  x <- whep::read_lpjml_litterfall("nv", run_dir = dir, first_year = 1901L)
  # Values written were 1..4 gC/m2/yr; 1 gC/m2 = 0.01 MgC/ha.
  testthat::expect_equal(
    sort(x$litterfall_c_mgc_ha_yr),
    c(0.01, 0.02, 0.03, 0.04)
  )
})

testthat::test_that("the crop file's odd variable name reads like the rest", {
  testthat::skip_if_not_installed("ncdf4")
  dir <- withr::local_tempdir()
  # litfallc_agr.nc really does hold ALITFALLC_agr on the 2026-08-27 run.
  .write_litfall_nc(
    file.path(dir, "litfallc_agr.nc"),
    nlon = 3,
    nlat = 2,
    nyear = 1,
    var = "ALITFALLC_agr"
  )
  x <- whep::read_lpjml_litterfall("agr", run_dir = dir, first_year = 1901L)
  testthat::expect_identical(nrow(x), 6L)
  testthat::expect_true(all(x$class == "agr"))
})
