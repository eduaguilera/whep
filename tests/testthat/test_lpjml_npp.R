# Build a tiny LPJmL-style annual pft NetCDF (2 lon x 2 lat x n_pft x n_year)
# holding a per-PFT-stand carbon variable plus the NamePFT character variable,
# matching pft_npp.nc / pft_harvestc.nc structure. Returns the dir and shape.
.lpjml_npp_fixture_cube <- function(
  var_name = "NPP",
  file = "pft_npp.nc",
  pft_names = c(
    "tropical broadleaved evergreen tree",
    "Temperate C3 grass",
    "rainfed grassland",
    "irrigated grassland"
  ),
  first_year = 1901L,
  n_year = 2L
) {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  lon <- c(-179.75, -179.25)
  lat <- c(0.25, 0.75)
  n_pft <- length(pft_names)
  dim_lon <- ncdf4::ncdim_def("lon", "degrees_east", lon)
  dim_lat <- ncdf4::ncdim_def("lat", "degrees_north", lat)
  dim_pft <- ncdf4::ncdim_def("npft", "", seq_len(n_pft))
  dim_time <- ncdf4::ncdim_def(
    "time",
    paste0("years since ", first_year, "-1-1"),
    seq_len(n_year) - 1L
  )
  dim_char <- ncdf4::ncdim_def(
    "len",
    "",
    seq_len(max(nchar(pft_names))),
    create_dimvar = FALSE
  )
  var_name_pft <- ncdf4::ncvar_def(
    "NamePFT",
    "",
    list(dim_char, dim_pft),
    prec = "char"
  )
  var <- ncdf4::ncvar_def(
    var_name,
    "gC/m2/yr",
    list(dim_lon, dim_lat, dim_pft, dim_time),
    missval = -9999
  )
  path <- file.path(dir, file)
  nc <- ncdf4::nc_create(path, list(var_name_pft, var))
  # Distinct value per (pft, year) so band selection is checkable: each cell
  # holds the pft band index times one hundred plus the year offset.
  vals <- array(0, dim = c(length(lon), length(lat), n_pft, n_year))
  for (p in seq_len(n_pft)) {
    for (ty in seq_len(n_year)) {
      vals[,, p, ty] <- p * 100 + (ty - 1L)
    }
  }
  ncdf4::ncvar_put(nc, var, vals)
  ncdf4::ncvar_put(nc, var_name_pft, pft_names)
  ncdf4::nc_close(nc)
  list(
    dir = dir,
    file = file,
    n_cells = length(lon) * length(lat),
    n_pft = n_pft,
    pft_names = pft_names
  )
}

testthat::test_that("read_lpjml_npp returns tidy per cell-pft-year rows", {
  cube <- .lpjml_npp_fixture_cube()
  out <- whep::read_lpjml_npp(
    "npp",
    run_dir = cube$dir,
    years = 1901L
  )
  pointblank::expect_col_exists(
    out,
    c("lon", "lat", "year", "npft", "name_pft", "value")
  )
  # 4 cells x 4 pft x 1 year.
  testthat::expect_equal(nrow(out), cube$n_cells * cube$n_pft)
  testthat::expect_setequal(out$name_pft, cube$pft_names)
  testthat::expect_true(all(out$year == 1901L))
})

testthat::test_that("read_lpjml_npp maps each band to the correct name", {
  cube <- .lpjml_npp_fixture_cube()
  out <- whep::read_lpjml_npp("npp", run_dir = cube$dir, years = 1901L)
  # value = pft_index * 100 + 0 for year 1901.
  grass <- out[out$name_pft == "rainfed grassland", ]
  testthat::expect_true(all(grass$npft == 3L))
  testthat::expect_true(all(grass$value == 300))
})

testthat::test_that("read_lpjml_npp reads harvestc from its own file", {
  cube <- .lpjml_npp_fixture_cube(
    var_name = "harvestc",
    file = "pft_harvestc.nc",
    pft_names = c("rainfed grassland", "irrigated grassland")
  )
  out <- whep::read_lpjml_npp("harvestc", run_dir = cube$dir, years = 1901L)
  testthat::expect_setequal(
    out$name_pft,
    c("rainfed grassland", "irrigated grassland")
  )
  testthat::expect_true(all(out$value >= 0))
})

testthat::test_that("read_lpjml_npp years filter keeps only requested years", {
  cube <- .lpjml_npp_fixture_cube(n_year = 3L)
  out <- whep::read_lpjml_npp("npp", run_dir = cube$dir, years = 1902L)
  testthat::expect_true(all(out$year == 1902L))
  # value = pft * 100 + 1 for year 1902.
  g <- out[out$name_pft == "rainfed grassland", ]
  testthat::expect_true(all(g$value == 301))
})

testthat::test_that("read_lpjml_npp example returns the documented schema", {
  out <- whep::read_lpjml_npp(example = TRUE)
  pointblank::expect_col_exists(
    out,
    c("lon", "lat", "year", "npft", "name_pft", "value")
  )
  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_gt(nrow(out), 0L)
})

testthat::test_that("real pft_npp read carries the expected PFT vocabulary", {
  run_dir <- Sys.getenv("WHEP_LPJML_RUN_DIR")
  testthat::skip_if(
    !nzchar(run_dir) || !dir.exists(run_dir),
    "WHEP_LPJML_RUN_DIR not set or absent"
  )
  testthat::skip_if_not(
    file.exists(file.path(run_dir, "pft_npp.nc")),
    "pft_npp.nc absent"
  )
  out <- whep::read_lpjml_npp("npp", years = 2000L, run_dir = run_dir)
  # 43 PFT bands including the natural grasses and the managed grasslands.
  testthat::expect_true(
    all(
      c(
        "tropical broadleaved evergreen tree",
        "Temperate C3 grass",
        "rainfed grassland",
        "irrigated grassland"
      ) %in%
        out$name_pft
    )
  )
  testthat::expect_equal(length(unique(out$npft)), 43L)
  testthat::expect_true(all(out$year == 2000L))
})
