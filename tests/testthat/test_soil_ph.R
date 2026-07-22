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

# ---- .derive_dominant_texture() / read_soil_hydraulic() ----------------

testthat::test_that(".derive_dominant_texture picks the largest-share class", {
  result <- whep:::.derive_dominant_texture(.hwsd_attr_fixture())

  pointblank::expect_col_exists(result, c("mu_global", "t_usda_tex"))
  testthat::expect_equal(result$t_usda_tex[result$mu_global == 1L], 7L)
  testthat::expect_equal(result$t_usda_tex[result$mu_global == 2L], 12L)
  testthat::expect_equal(result$t_usda_tex[result$mu_global == 3L], 3L)
})

testthat::test_that("read_soil_hydraulic example fixture is schema-complete", {
  out <- whep::read_soil_hydraulic(example = TRUE)

  pointblank::expect_col_exists(
    out,
    c("lon", "lat", "t_field", "t_wilt", "porosity")
  )
  testthat::expect_true(all(out$t_field > out$t_wilt))
  testthat::expect_true(all(out$porosity > out$t_field))
})

testthat::test_that("read_soil_hydraulic maps dominant texture to hydraulics", {
  testthat::skip_if_not_installed("terra")
  dir <- withr::local_tempdir()
  # Map unit 1 is dominantly texture 13 (sand): sand hydraulics are porosity
  # 0.43, field capacity 0.08, wilting point 0.03.
  attr <- tibble::tribble(
    ~mu_global, ~t_usda_tex, ~share, ~t_ph_h2o,
    1L, 13L, 100, 6.5
  )
  readr::write_csv(attr, file.path(dir, "hwsd_data.csv"))

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

  result <- whep::read_soil_hydraulic(hwsd_dir = dir)

  pointblank::expect_col_exists(
    result,
    c("lon", "lat", "t_field", "t_wilt", "porosity")
  )
  testthat::expect_true(all(abs(result$porosity - 0.43) < 1e-9))
  testthat::expect_true(all(abs(result$t_field - 0.08) < 1e-9))
  testthat::expect_true(all(abs(result$t_wilt - 0.03) < 1e-9))
  testthat::expect_true(all(
    result$t_field > result$t_wilt & result$porosity > result$t_field
  ))
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

testthat::test_that(".gapfill_soil fills a border coordinate only once", {
  soil_grid <- tibble::tribble(
    ~lon, ~lat, ~soil_ph,
    -0.25, -0.25, 6.0
  )
  # The missing coordinate occurs twice because two polities overlap it.
  country_grid <- tibble::tribble(
    ~lon, ~lat, ~area_code,
    -0.25, -0.25, 1L,
    0.25, -0.25, 1L,
    0.25, -0.25, 2L
  )

  result <- whep:::.gapfill_soil(soil_grid, country_grid)

  testthat::expect_equal(nrow(result), 2L)
  testthat::expect_equal(
    sum(result$lon == 0.25 & result$lat == -0.25),
    1L
  )
})

testthat::test_that(".gapfill_soil uses the caller's fallback, not pH 7.0", {
  soil_grid <- tibble::tribble(
    ~lon, ~lat, ~soil_ph,
    -0.25, -0.25, 0.29
  )
  # A cell with no neighbour within the search window must take the caller's
  # domain-neutral fallback (here the loam field capacity, 0.29), never the
  # pH-domain 7.0.
  country_grid <- tibble::tribble(
    ~lon, ~lat,
    -0.25, -0.25,
    89.75, 89.75
  )
  result <- whep:::.gapfill_soil(
    soil_grid,
    country_grid,
    max_search = 1L,
    fallback = 0.29,
    label = "soil hydraulic"
  )
  far <- result[result$lon == 89.75, ]
  testthat::expect_equal(far$soil_ph, 0.29)
})

testthat::test_that(".gapfill_soil_hydraulic falls back to loam, not pH 7.0", {
  # An isolated target cell far from any aggregated hydraulic data must fall
  # back per-property to the central-texture (loam) reference values, never the
  # pH reader's 7.0 (impossible for a volumetric fraction in (0, 1)).
  loam <- whep::soil_hydraulic_by_texture |>
    dplyr::filter(usda_texture_class == "loam")
  grid <- tibble::tribble(
    ~lon, ~lat, ~t_field, ~t_wilt, ~porosity,
    -0.25, -0.25, 0.21, 0.09, 0.40
  )
  country_grid <- tibble::tribble(
    ~lon, ~lat,
    -0.25, -0.25,
    89.75, 89.75
  )
  result <- whep:::.gapfill_soil_hydraulic(grid, country_grid)
  far <- result[result$lon == 89.75, ]
  testthat::expect_equal(far$t_field, loam$field_capacity)
  testthat::expect_equal(far$t_wilt, loam$wilting_point)
  testthat::expect_equal(far$porosity, loam$porosity)
  # Never the impossible pH-domain fallback.
  testthat::expect_true(all(
    far$t_field < 1 & far$t_wilt < 1 & far$porosity < 1
  ))
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

testthat::test_that(".crop_to_target crops the raster to the target extent", {
  testthat::skip_if_not_installed("terra")
  rast <- terra::rast(
    nrows = 12,
    ncols = 12,
    xmin = -3,
    xmax = 3,
    ymin = -3,
    ymax = 3,
    resolution = 0.5
  )
  terra::values(rast) <- seq_len(terra::ncell(rast))
  target <- tibble::tibble(lon = c(-0.25, 0.25), lat = c(-0.25, 0.25))

  cropped <- whep:::.crop_to_target(rast, target, target_res = 0.5)
  ext <- terra::ext(cropped)

  testthat::expect_lt(terra::ncell(cropped), terra::ncell(rast))
  testthat::expect_gte(ext$xmin, -1)
  testthat::expect_lte(ext$xmax, 1)
  testthat::expect_gte(ext$ymin, -1)
  testthat::expect_lte(ext$ymax, 1)
})

testthat::test_that(".crop_to_target is a no-op without a target grid", {
  testthat::skip_if_not_installed("terra")
  rast <- terra::rast(
    nrows = 4,
    ncols = 4,
    xmin = -1,
    xmax = 1,
    ymin = -1,
    ymax = 1
  )
  terra::values(rast) <- seq_len(terra::ncell(rast))

  unchanged <- whep:::.crop_to_target(rast, NULL, target_res = 0.5)

  testthat::expect_equal(terra::ncell(unchanged), terra::ncell(rast))
})

testthat::test_that("read_soil_ph reads real local HWSD data (smoke)", {
  testthat::skip_if(
    Sys.getenv("WHEP_HWSD_DIR") == "",
    "WHEP_HWSD_DIR not set; skipping real-data smoke test."
  )

  # Crop to a small Iberian target grid: classifying the full-resolution
  # global HWSD raster whole exhausts memory and crashes the R session.
  target <- tidyr::expand_grid(
    lon = seq(-9.75, 3.75, by = 0.5),
    lat = seq(36.25, 43.75, by = 0.5)
  )

  result <- whep::read_soil_ph(data = list(cell_polity = target))

  pointblank::expect_col_exists(result, c("lon", "lat", "soil_ph"))
  testthat::expect_gt(nrow(result), 0L)
  testthat::expect_true(all(result$soil_ph >= 3.5 & result$soil_ph <= 10))
})
