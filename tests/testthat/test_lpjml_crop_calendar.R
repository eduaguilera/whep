# Cropland soil cover from the LPJmL crop calendar. Offline throughout: the
# real-read tests write a small NetCDF rather than touching a run.

testthat::test_that("a month inside a normal season is covered", {
  # Sown day 100, harvested day 250: May (day 137) in, January (day 15) out.
  testthat::expect_true(whep:::.crop_month_inside(100, 250, 5))
  testthat::expect_false(whep:::.crop_month_inside(100, 250, 1))
  testthat::expect_false(whep:::.crop_month_inside(100, 250, 12))
})

testthat::test_that("a season crossing the new year wraps", {
  # 41.7% of cells at 2010 have hdate < sdate. Treating that as an empty
  # interval books every winter cereal as permanently bare.
  sow <- 300
  harvest <- 150
  testthat::expect_true(whep:::.crop_month_inside(sow, harvest, 12))
  testthat::expect_true(whep:::.crop_month_inside(sow, harvest, 1))
  testthat::expect_true(whep:::.crop_month_inside(sow, harvest, 5))
  testthat::expect_false(whep:::.crop_month_inside(sow, harvest, 8))
  # A wrapping season must never be entirely uncovered.
  covered <- vapply(
    1:12,
    \(m) whep:::.crop_month_inside(sow, harvest, m),
    logical(1)
  )
  testthat::expect_gt(sum(covered), 0L)
})

testthat::test_that("it is vectorised over cells", {
  sow <- c(100, 300)
  harvest <- c(250, 150)
  testthat::expect_identical(
    whep:::.crop_month_inside(sow, harvest, 1),
    c(FALSE, TRUE)
  )
})

testthat::test_that("the example fixture is a usable monthly cover", {
  x <- whep::read_lpjml_crop_cover(example = TRUE)
  testthat::expect_s3_class(x, "tbl_df")
  testthat::expect_identical(nrow(x), 12L)
  testthat::expect_true(
    all(c("lon", "lat", "year", "month", "cropland_cover") %in% names(x))
  )
  testthat::expect_true(all(x$cropland_cover >= 0 & x$cropland_cover <= 1))
})

testthat::test_that("a run with no calendar is named, not guessed", {
  dir <- withr::local_tempdir()
  testthat::expect_error(
    whep::read_lpjml_crop_cover(run_dir = dir),
    "sdate"
  )
  # And it says why an older run cannot simply be reused.
  testthat::expect_error(
    whep::read_lpjml_crop_cover(run_dir = dir),
    "2026-08-27"
  )
})

# ---- the cropland override inside the carbon balance ------------------------

.crop_cover_drivers <- function() {
  tidyr::expand_grid(
    lon = 0.25,
    lat = 5.25,
    area_code = 1L,
    year = 2010L,
    month = 1:12
  ) |>
    dplyr::mutate(
      temp_c = 22,
      precip_mm = 90,
      pet_mm = 80,
      clay_pct = 25,
      water_minus_pet_mm = precip_mm - pet_mm
    )
}

testthat::test_that("a NULL crop layer leaves every class on the curve", {
  classes <- c("cropland", "grassland", "natural")
  base <- whep:::.cb_attach_soil_cover(.crop_cover_drivers(), classes)
  with_null <- whep:::.cb_attach_soil_cover(
    .crop_cover_drivers(),
    classes,
    cropland_cover = NULL
  )
  testthat::expect_identical(base, with_null)
})

testthat::test_that("a supplied crop layer replaces cropland cover only", {
  cover <- tibble::tibble(
    lon = 0.25,
    lat = 5.25,
    year = 2010L,
    month = 1:12,
    cropland_cover = c(0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0)
  )
  classes <- c("cropland", "grassland", "natural")
  base <- whep:::.cb_attach_soil_cover(.crop_cover_drivers(), classes)
  out <- whep:::.cb_attach_soil_cover(
    .crop_cover_drivers(),
    classes,
    cropland_cover = cover
  )

  crop <- out[out$land_use == "cropland", ]
  crop <- crop[order(crop$month), ]
  testthat::expect_equal(crop$soil_cover, cover$cropland_cover)

  # Grassland and natural must be untouched: the calendar covers crops only.
  for (cl in c("grassland", "natural")) {
    testthat::expect_equal(
      out$soil_cover[out$land_use == cl],
      base$soil_cover[base$land_use == cl]
    )
  }
})

testthat::test_that("the override is keyed on month, not just year", {
  # The natural layer is one value per cell-year; this one is twelve. Joining
  # on year alone would fan every cropland row out twelvefold.
  cover <- tibble::tibble(
    lon = 0.25,
    lat = 5.25,
    year = 2010L,
    month = 1:12,
    cropland_cover = seq(0, 1, length.out = 12)
  )
  base <- whep:::.cb_attach_soil_cover(.crop_cover_drivers(), "cropland")
  out <- whep:::.cb_attach_soil_cover(
    .crop_cover_drivers(),
    "cropland",
    cropland_cover = cover
  )
  testthat::expect_identical(nrow(out), nrow(base))
  testthat::expect_length(unique(out$soil_cover), 12L)
})

testthat::test_that("a month missing from the layer keeps the curve value", {
  cover <- tibble::tibble(
    lon = 0.25,
    lat = 5.25,
    year = 2010L,
    month = 6L,
    cropland_cover = 0.9
  )
  base <- whep:::.cb_attach_soil_cover(.crop_cover_drivers(), "cropland")
  out <- whep:::.cb_attach_soil_cover(
    .crop_cover_drivers(),
    "cropland",
    cropland_cover = cover
  )
  testthat::expect_equal(out$soil_cover[out$month == 6L], 0.9)
  testthat::expect_equal(
    out$soil_cover[out$month != 6L],
    base$soil_cover[base$month != 6L]
  )
})

testthat::test_that("a crop layer missing its columns is named", {
  testthat::expect_error(
    whep:::.cb_attach_soil_cover(
      .crop_cover_drivers(),
      "cropland",
      cropland_cover = tibble::tibble(lon = 1, lat = 2)
    ),
    "cropland_cover"
  )
})

testthat::test_that("bare cropland decomposes faster than the curve", {
  # The point of the correction: where the calendar says fallow and the curve
  # says canopy, the RothC plant-retainment term rises and equilibrium carbon,
  # which scales as 1 / modifier, falls.
  keys <- c("lon", "lat", "area_code", "year", "land_use")
  bare <- tibble::tibble(
    lon = 0.25,
    lat = 5.25,
    year = 2010L,
    month = 1:12,
    cropland_cover = 0.02
  )
  curve_mod <- whep:::.cb_rothc_modifier_vectorised(
    whep:::.cb_attach_soil_cover(.crop_cover_drivers(), "cropland"),
    "hsoc",
    keys
  )
  bare_mod <- whep:::.cb_rothc_modifier_vectorised(
    whep:::.cb_attach_soil_cover(.crop_cover_drivers(), "cropland", NULL, bare),
    "hsoc",
    keys
  )
  testthat::expect_true(
    all(bare_mod$climate_modifier > curve_mod$climate_modifier)
  )
})

testthat::test_that("the season test works on a grid, not just a vector", {
  # The real read passes 720 x 277 matrices. dplyr::if_else() rejects a matrix
  # condition, so a vector-only test passed while the real path aborted.
  sow <- matrix(c(100, 300, 100, 300), nrow = 2)
  harvest <- matrix(c(250, 150, 250, 150), nrow = 2)
  jan <- whep:::.crop_month_inside(sow, harvest, 1)
  testthat::expect_true(is.matrix(jan))
  testthat::expect_identical(dim(jan), c(2L, 2L))
  testthat::expect_identical(as.vector(jan), c(FALSE, TRUE, FALSE, TRUE))

  may <- whep:::.crop_month_inside(sow, harvest, 5)
  testthat::expect_identical(as.vector(may), c(TRUE, TRUE, TRUE, TRUE))
})
