# Write a tiny gridded LPJmL-style monthly NetCDF (2 lon x 2 lat x 12 month)
# holding one positive flux variable, and return where it lives plus the
# expected per-month cell count.
.lpjml_hydro_fixture_cube <- function(
  var_name = "seepage",
  file = "mseepage.nc"
) {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  lon <- c(-179.75, -179.25)
  lat <- c(0.25, 0.75)
  time <- 1:12
  dim_lon <- ncdf4::ncdim_def("lon", "degrees_east", lon)
  dim_lat <- ncdf4::ncdim_def("lat", "degrees_north", lat)
  dim_time <- ncdf4::ncdim_def("time", "months", time)
  var <- ncdf4::ncvar_def(
    var_name,
    "mm",
    list(dim_lon, dim_lat, dim_time),
    missval = -9999
  )
  path <- file.path(dir, file)
  nc <- ncdf4::nc_create(path, list(var))
  vals <- array(
    seq_len(length(lon) * length(lat) * length(time)),
    dim = c(length(lon), length(lat), length(time))
  )
  ncdf4::ncvar_put(nc, var, vals)
  ncdf4::nc_close(nc)
  list(dir = dir, file = file, n_cells = length(lon) * length(lat))
}

testthat::test_that("monthly read returns one tidy row per cell-month", {
  cube <- .lpjml_hydro_fixture_cube()
  result <- whep::read_lpjml_hydrology(
    "drainage",
    run_dir = cube$dir,
    years = 1901L,
    first_year = 1901L,
    monthly = TRUE
  )

  pointblank::expect_col_exists(
    result,
    c("lon", "lat", "year", "month", "value")
  )
  testthat::expect_equal(nrow(result), cube$n_cells * 12)
  pointblank::expect_col_vals_gte(result, "value", 0)
  testthat::expect_setequal(result$month, 1:12)
})

# Write a multi-year gridded LPJmL-style monthly NetCDF (2 lon x 2 lat x
# n_years*12 month), 3-D (lon, lat, time) or 4-D (lon, lat, layer, time) when
# n_layer > 1. Values encode (year_offset, month[, layer]) as
# year_offset*100 + month (+ layer*10000 for the 4-D case), where
# year_offset = year - first_year -- small enough to stay well within
# float32's exact-integer range (NetCDF stores as float by default), unlike
# the raw calendar year, so decoding the value is exact, not just close.
.lpjml_hydro_multiyear_fixture <- function(
  var_name = "seepage",
  file = "mseepage.nc",
  first_year = 1900L,
  n_years = 3L,
  n_layer = 1L
) {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  lon <- c(-179.75, -179.25)
  lat <- c(0.25, 0.75)
  n_time <- n_years * 12L
  time <- seq_len(n_time)
  dim_lon <- ncdf4::ncdim_def("lon", "degrees_east", lon)
  dim_lat <- ncdf4::ncdim_def("lat", "degrees_north", lat)
  dim_time <- ncdf4::ncdim_def("time", "months", time)
  dims <- if (n_layer > 1L) {
    dim_layer <- ncdf4::ncdim_def("layer", "level", seq_len(n_layer))
    list(dim_lon, dim_lat, dim_layer, dim_time)
  } else {
    list(dim_lon, dim_lat, dim_time)
  }
  var <- ncdf4::ncvar_def(var_name, "mm", dims, missval = -9999)
  path <- file.path(dir, file)
  nc <- ncdf4::nc_create(path, list(var))
  year_offset <- (seq_len(n_time) - 1L) %/% 12L
  months <- ((seq_len(n_time) - 1L) %% 12L) + 1L
  code <- year_offset * 100L + months
  n_cell <- length(lon) * length(lat)
  vals <- if (n_layer > 1L) {
    # dim order (lon, lat, layer, time), column-major: layer varies fastest
    # within each time step. Every (lon, lat) cell of a given (layer, time)
    # slice shares the same layer*10000 + year_offset*100 + month code.
    layer_code <- rep(code, each = n_layer) +
      rep(seq_len(n_layer) * 10000L, times = n_time)
    array(
      rep(layer_code, each = n_cell),
      dim = c(length(lon), length(lat), n_layer, n_time)
    )
  } else {
    array(
      rep(code, each = n_cell),
      dim = c(length(lon), length(lat), n_time)
    )
  }
  ncdf4::ncvar_put(nc, var, vals)
  ncdf4::nc_close(nc)
  list(
    dir = dir,
    file = file,
    first_year = first_year,
    n_years = n_years,
    n_layer = n_layer,
    n_cells = length(lon) * length(lat)
  )
}

testthat::test_that("years= reads only the requested year's NetCDF slice", {
  cube <- .lpjml_hydro_multiyear_fixture(first_year = 1900L, n_years = 3L)
  result <- whep::read_lpjml_hydrology(
    "drainage",
    run_dir = cube$dir,
    years = 1901L,
    first_year = cube$first_year,
    monthly = TRUE
  )

  testthat::expect_equal(nrow(result), cube$n_cells * 12)
  testthat::expect_setequal(result$year, 1901L)
  testthat::expect_setequal(result$month, 1:12)
  # Value encodes year_offset*100 + month; decoding it independently must
  # match the year/month columns exactly -- this is the check that would have
  # caught an off-by-one in the adjusted first_year passed into
  # .hydro_slab_to_long() for the sliced (non-NULL years) read.
  decoded_year <- cube$first_year + result$value %/% 100
  decoded_month <- result$value %% 100
  testthat::expect_equal(decoded_year, result$year)
  testthat::expect_equal(decoded_month, result$month)
})

testthat::test_that("years= subsets a 4-D (layered) variable correctly", {
  cube <- .lpjml_hydro_multiyear_fixture(
    var_name = "SWC",
    file = "mswc.nc",
    first_year = 1900L,
    n_years = 3L,
    n_layer = 2L
  )
  result <- whep::read_lpjml_hydrology(
    "swc",
    run_dir = cube$dir,
    years = 1901L,
    first_year = cube$first_year,
    monthly = TRUE
  )

  testthat::expect_equal(nrow(result), cube$n_cells * 12 * cube$n_layer)
  testthat::expect_setequal(result$year, 1901L)
  testthat::expect_setequal(result$layer, seq_len(cube$n_layer))
  decoded_layer <- result$value %/% 10000
  remainder <- result$value %% 10000
  decoded_year <- cube$first_year + remainder %/% 100
  decoded_month <- remainder %% 100
  testthat::expect_equal(decoded_layer, result$layer)
  testthat::expect_equal(decoded_year, result$year)
  testthat::expect_equal(decoded_month, result$month)
})

testthat::test_that("cft_nir exposes and preserves a CFT band, not a soil layer", {
  injected <- tidyr::expand_grid(
    lon = 0.25,
    lat = 0.25,
    year = 2000L,
    month = 1:2,
    layer = 1:2
  ) |>
    dplyr::mutate(value = layer * 10 + month)

  monthly <- whep::read_lpjml_hydrology(
    "cft_nir",
    data = injected,
    monthly = TRUE
  )
  annual <- whep::read_lpjml_hydrology(
    "cft_nir",
    data = injected,
    monthly = FALSE
  )

  testthat::expect_true("band" %in% names(monthly))
  testthat::expect_false("layer" %in% names(monthly))
  testthat::expect_setequal(monthly$band, 1:2)
  testthat::expect_equal(nrow(annual), 2L)
  testthat::expect_setequal(annual$band, 1:2)
  testthat::expect_equal(
    dplyr::arrange(annual, band)$value,
    c(23, 43)
  )
})

testthat::test_that("years=NULL still reads every year (regression guard)", {
  cube <- .lpjml_hydro_multiyear_fixture(first_year = 1900L, n_years = 3L)
  result <- whep::read_lpjml_hydrology(
    "drainage",
    run_dir = cube$dir,
    years = NULL,
    first_year = cube$first_year,
    monthly = TRUE
  )

  testthat::expect_equal(nrow(result), cube$n_cells * 12 * cube$n_years)
  testthat::expect_setequal(result$year, 1900:1902)
})

testthat::test_that("non-contiguous years still returns only those years", {
  cube <- .lpjml_hydro_multiyear_fixture(first_year = 1900L, n_years = 3L)
  result <- whep::read_lpjml_hydrology(
    "drainage",
    run_dir = cube$dir,
    years = c(1900L, 1902L),
    first_year = cube$first_year,
    monthly = TRUE
  )

  # The NetCDF-level slice over-fetches 1900:1902 (the covering contiguous
  # range), but .filter_years_if_present() narrows it back down to exactly
  # the requested non-contiguous years.
  testthat::expect_setequal(result$year, c(1900L, 1902L))
  testthat::expect_equal(nrow(result), cube$n_cells * 12 * 2)
})

testthat::test_that("annual read sums the 12 months per cell", {
  cube <- .lpjml_hydro_fixture_cube()
  result <- whep::read_lpjml_hydrology(
    "drainage",
    run_dir = cube$dir,
    years = 1901L,
    first_year = 1901L,
    monthly = FALSE
  )

  pointblank::expect_col_exists(result, c("lon", "lat", "year", "value"))
  testthat::expect_false("month" %in% names(result))
  testthat::expect_equal(nrow(result), cube$n_cells)
  pointblank::expect_col_vals_gt(result, "value", 0)

  monthly <- whep::read_lpjml_hydrology(
    "drainage",
    run_dir = cube$dir,
    years = 1901L,
    first_year = 1901L,
    monthly = TRUE
  )
  annual_from_monthly <- monthly |>
    dplyr::summarise(value = sum(value), .by = c(lon, lat, year)) |>
    dplyr::arrange(lon, lat)
  result <- dplyr::arrange(result, lon, lat)
  testthat::expect_equal(result$value, annual_from_monthly$value)
})

# LPJmL 6.x renamed mprec.nc's variable from `prec` to its CF short name `pr`.
# Both must read, because both versions' output can sit side by side on one
# machine and a run directory carries no version stamp to branch on.
testthat::test_that("precipitation reads under the 5.x name `prec`", {
  cube <- .lpjml_hydro_fixture_cube(var_name = "prec", file = "mprec.nc")
  result <- whep::read_lpjml_hydrology(
    "prec",
    run_dir = cube$dir,
    years = 1901L,
    first_year = 1901L,
    monthly = TRUE
  )

  testthat::expect_equal(nrow(result), cube$n_cells * 12)
  pointblank::expect_col_vals_gte(result, "value", 0)
})

testthat::test_that("precipitation reads under the 6.x name `pr`", {
  cube <- .lpjml_hydro_fixture_cube(var_name = "pr", file = "mprec.nc")
  result <- whep::read_lpjml_hydrology(
    "prec",
    run_dir = cube$dir,
    years = 1901L,
    first_year = 1901L,
    monthly = TRUE
  )

  testthat::expect_equal(nrow(result), cube$n_cells * 12)
  pointblank::expect_col_vals_gte(result, "value", 0)
})

# The failure this replaces was `argument is of length zero`, which named
# neither the variable nor the file. Listing what the file does contain is what
# makes the next rename cost one run to diagnose.
testthat::test_that("an unresolvable variable name aborts naming the file", {
  cube <- .lpjml_hydro_fixture_cube(var_name = "totally_new", file = "mprec.nc")

  testthat::expect_error(
    whep::read_lpjml_hydrology(
      "prec",
      run_dir = cube$dir,
      years = 1901L,
      first_year = 1901L,
      monthly = TRUE
    ),
    "totally_new"
  )
})

# Write a tiny annual per-CFT NetCDF (2 lon x 2 lat x 3 band x n_years), the
# shape LPJmL writes for cft_consump_water_*: one time step per year, mm/yr,
# with the band names carried in NamePFT the way a real run does.
.lpjml_hydro_cft_fixture <- function(
  var_name = "consump_water_g",
  file = "cft_consump_water_g.nc",
  first_year = 1901L,
  n_years = 3L,
  band_names = c("rainfed maize", "rainfed grassland", "irrigated grassland")
) {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  lon <- c(-179.75, -179.25)
  lat <- c(0.25, 0.75)
  dim_lon <- ncdf4::ncdim_def("lon", "degrees_east", lon)
  dim_lat <- ncdf4::ncdim_def("lat", "degrees_north", lat)
  dim_pft <- ncdf4::ncdim_def("pft", "", seq_along(band_names))
  dim_len <- ncdf4::ncdim_def("len", "", seq_len(max(nchar(band_names))))
  dim_time <- ncdf4::ncdim_def("time", "years", seq_len(n_years))
  var <- ncdf4::ncvar_def(
    var_name,
    "mm/yr",
    list(dim_lon, dim_lat, dim_pft, dim_time),
    missval = -9999
  )
  name_var <- ncdf4::ncvar_def(
    "NamePFT",
    "",
    list(dim_len, dim_pft),
    prec = "char"
  )
  path <- file.path(dir, file)
  nc <- ncdf4::nc_create(path, list(var, name_var))
  # value = band * 100 + year offset, so band and year are both identifiable.
  vals <- array(
    rep(seq_along(band_names) * 100, each = length(lon) * length(lat)) +
      rep(
        seq_len(n_years) - 1L,
        each = length(lon) * length(lat) * length(band_names)
      ),
    dim = c(length(lon), length(lat), length(band_names), n_years)
  )
  ncdf4::ncvar_put(nc, var, vals)
  ncdf4::ncvar_put(nc, name_var, band_names)
  ncdf4::nc_close(nc)
  list(dir = dir, n_cells = length(lon) * length(lat), band_names = band_names)
}

testthat::test_that("annual per-CFT cube decodes years, not months", {
  cube <- .lpjml_hydro_cft_fixture(n_years = 3L)

  result <- whep::read_lpjml_hydrology(
    "cft_consump_water_g",
    run_dir = cube$dir,
    first_year = 1901L
  )

  # One row per cell-band-year, and no month axis invented from an annual file.
  testthat::expect_false("month" %in% names(result))
  testthat::expect_setequal(result$year, 1901:1903)
  testthat::expect_equal(nrow(result), cube$n_cells * 3L * 3L)
})

testthat::test_that("per-CFT bands are named from the file, not by index", {
  cube <- .lpjml_hydro_cft_fixture()

  result <- whep::read_lpjml_hydrology(
    "cft_consump_water_g",
    run_dir = cube$dir,
    first_year = 1901L
  )

  testthat::expect_true(all(c("band", "band_name") %in% names(result)))
  testthat::expect_setequal(result$band_name, cube$band_names)
  # Band 2 is grassland here; its value encodes band 2 (200 + year offset).
  grass <- dplyr::filter(
    result,
    band_name == "rainfed grassland",
    year == 1901L
  )
  testthat::expect_equal(unique(grass$value), 200)
  testthat::expect_equal(unique(grass$band), 2L)
})

testthat::test_that("years= slices an annual cube on the annual time axis", {
  cube <- .lpjml_hydro_cft_fixture(first_year = 1901L, n_years = 5L)

  result <- whep::read_lpjml_hydrology(
    "cft_consump_water_g",
    run_dir = cube$dir,
    first_year = 1901L,
    years = 1903L
  )

  # A monthly time index would have read the wrong slice entirely.
  testthat::expect_setequal(result$year, 1903L)
  grass <- dplyr::filter(result, band_name == "rainfed grassland")
  testthat::expect_equal(unique(grass$value), 202)
})

testthat::test_that("a year past the run's end aborts naming the coverage", {
  # WHEP's LPJmL runs end in 2009, 2018 and 2023 and sit side by side, so
  # whether a year exists depends on which run `run_dir` names. Before #598
  # this reached ncdf4 as an out-of-bounds start=/count= and died with
  # "NetCDF: Start+count exceeds dimension bound", naming neither the run,
  # the coverage, nor the offending years.
  cube <- .lpjml_hydro_multiyear_fixture(first_year = 1900L, n_years = 3L)

  testthat::expect_error(
    whep::read_lpjml_hydrology(
      "drainage",
      run_dir = cube$dir,
      years = 1905L,
      first_year = cube$first_year
    ),
    "1900-1902"
  )
  # A partially covered request is caught too: the covered part must not come
  # back as a silently truncated series.
  testthat::expect_error(
    whep::read_lpjml_hydrology(
      "drainage",
      run_dir = cube$dir,
      years = 1901:1905,
      first_year = cube$first_year
    ),
    "3 requested years outside the run's coverage"
  )
  # Years before the run starts are equally out of coverage.
  testthat::expect_error(
    whep::read_lpjml_hydrology(
      "drainage",
      run_dir = cube$dir,
      years = c(1850L, 1901L),
      first_year = cube$first_year
    ),
    "1 requested year outside the run's coverage"
  )
})

testthat::test_that("coverage is read from the annual axis for annual cubes", {
  # steps_per_year = 1 for the per-CFT consumptive-water cubes: dividing the
  # 5-step time axis by 12 would report a one-year run and reject year 2.
  cube <- .lpjml_hydro_cft_fixture(first_year = 1901L, n_years = 5L)

  testthat::expect_no_error(
    whep::read_lpjml_hydrology(
      "cft_consump_water_g",
      run_dir = cube$dir,
      first_year = 1901L,
      years = 1905L
    )
  )
  testthat::expect_error(
    whep::read_lpjml_hydrology(
      "cft_consump_water_g",
      run_dir = cube$dir,
      first_year = 1901L,
      years = 1906L
    ),
    "1901-1905"
  )
})
