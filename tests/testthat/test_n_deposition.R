# Write a tiny 5-arcmin HaNi-style NetCDF (one variable, native fine grid)
# covering exactly `n_lon_blocks` x `n_lat_blocks` WHEP 0.5-degree cells (each
# block is 6x6 fine cells, since 0.5 / (5/60) = 6), plus `n_years` annual
# layers, and return where it lives plus the expected 0.5-degree cell count.
.hani_fixture_cube <- function(
  var_name = "ndep_nhx",
  file = "ndep_nhx.nc",
  n_lon_blocks = 1L,
  n_lat_blocks = 1L,
  n_years = 2L,
  fill_value = 1
) {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  fine_step <- 0.5 / 6
  fine_lon <- -0.5 +
    fine_step / 2 +
    (seq_len(6L * n_lon_blocks) - 1) *
      fine_step
  fine_lat <- -0.5 +
    fine_step / 2 +
    (seq_len(6L * n_lat_blocks) - 1) *
      fine_step
  time <- seq_len(n_years) - 1L
  dim_lon <- ncdf4::ncdim_def("lon", "degrees_east", fine_lon)
  dim_lat <- ncdf4::ncdim_def("lat", "degrees_north", fine_lat)
  dim_time <- ncdf4::ncdim_def(
    "time",
    "years since 1850-01-01 00:00:00",
    time,
    calendar = "noleap"
  )
  var <- ncdf4::ncvar_def(
    var_name,
    "g N",
    list(dim_lon, dim_lat, dim_time),
    missval = -9999
  )
  path <- file.path(dir, file)
  nc <- ncdf4::nc_create(path, list(var))
  vals <- array(
    fill_value,
    dim = c(length(fine_lon), length(fine_lat), length(time))
  )
  ncdf4::ncvar_put(nc, var, vals)
  ncdf4::nc_close(nc)
  list(
    dir = dir,
    n_cells = n_lon_blocks * n_lat_blocks,
    fine_cells_per_block = 36L,
    fill_value = fill_value
  )
}

testthat::test_that("read_n_deposition sums fine cells into one 0.5-deg cell", {
  cube <- .hani_fixture_cube(n_lon_blocks = 1L, n_lat_blocks = 1L, n_years = 1L)
  result <- whep::read_n_deposition(
    species = "nhx",
    hani_dir = cube$dir,
    years = 1850L
  )

  pointblank::expect_col_exists(result, c("lon", "lat", "year", "value_g"))
  testthat::expect_equal(nrow(result), 1L)
  testthat::expect_equal(
    result$value_g,
    cube$fine_cells_per_block * cube$fill_value
  )
  testthat::expect_equal(result$lon, -0.25)
  testthat::expect_equal(result$lat, -0.25)
})

testthat::test_that("read_n_deposition keeps adjacent 0.5-deg cells separate", {
  cube <- .hani_fixture_cube(n_lon_blocks = 2L, n_lat_blocks = 1L, n_years = 1L)
  result <- whep::read_n_deposition(
    species = "nhx",
    hani_dir = cube$dir,
    years = 1850L
  )

  testthat::expect_equal(nrow(result), 2L)
  testthat::expect_setequal(result$lon, c(-0.25, 0.25))
  testthat::expect_true(all(
    result$value_g == cube$fine_cells_per_block * cube$fill_value
  ))
})

testthat::test_that("read_n_deposition maps time index to calendar year", {
  cube <- .hani_fixture_cube(n_years = 3L)
  result <- whep::read_n_deposition(
    species = "nhx",
    hani_dir = cube$dir,
    years = c(1850L, 1852L)
  )

  testthat::expect_setequal(result$year, c(1850L, 1852L))
})

# ---- build_n_deposition() ---------------------------------------------

.example_cell_polity <- function() {
  tibble::tribble(
    ~lon, ~lat, ~area_code, ~polity_frac, ~cell_area_ha,
    -0.25, -0.25, 1L, 1, 300000
  )
}

testthat::test_that("build_n_deposition converts mass to a per-hectare rate", {
  nhx <- tibble::tribble(
    ~lon, ~lat, ~year, ~value_g,
    -0.25, -0.25, 2020L, 2000000000
  )
  noy <- tibble::tribble(
    ~lon, ~lat, ~year, ~value_g,
    -0.25, -0.25, 2020L, 1000000000
  )
  out <- whep::build_n_deposition(
    data = list(nhx = nhx, noy = noy, cell_polity = .example_cell_polity())
  )

  pointblank::expect_col_exists(
    out,
    c(
      "lon",
      "lat",
      "area_code",
      "year",
      "deposition_kgn_ha",
      "deposition_n_t",
      "method_deposition"
    )
  )
  pointblank::expect_col_vals_gte(out, "deposition_kgn_ha", 0)
  # (2e9 + 1e9) g / 1000 (g->kg) / 300000 ha = 10 kg N/ha
  testthat::expect_equal(out$deposition_kgn_ha, 10)
  # 10 kg/ha * 300000 ha * 1 (polity_frac) / 1000 (kg->t) = 3000 t
  testthat::expect_equal(out$deposition_n_t, 3000)
})

testthat::test_that("build_n_deposition example fixture is schema-complete", {
  out <- whep::build_n_deposition(example = TRUE)
  pointblank::expect_col_exists(
    out,
    c(
      "lon",
      "lat",
      "area_code",
      "year",
      "deposition_kgn_ha",
      "deposition_n_t",
      "method_deposition"
    )
  )
  pointblank::expect_col_vals_gte(out, "deposition_kgn_ha", 0)
})
