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

# ---- per-regime cover: rainfed and irrigated bands kept apart ----------------

# Two cells, three calendar bands driven from in-memory arrays: a rainfed
# summer crop (day 120-270) on 30% of cell 1 and 20% of cell 2, an irrigated
# winter crop wrapping the year (day 300-60) on 10% of cell 1 only, and an
# irrigated band with no calendar (sdate 0) that cropped area must still
# count. Grids are 2 x 1 (n = c(2, 1)).
.regime_slab <- function() {
  arrays <- list(
    cftfrac = list(
      matrix(c(0.3, 0.2), nrow = 2),
      matrix(c(0.1, 0.0), nrow = 2),
      matrix(c(0.05, 0.0), nrow = 2)
    ),
    sdate = list(
      matrix(c(120, 120), nrow = 2),
      matrix(c(300, 300), nrow = 2),
      matrix(c(0, 0), nrow = 2)
    ),
    hdate = list(
      matrix(c(270, 270), nrow = 2),
      matrix(c(60, 60), nrow = 2),
      matrix(c(0, 0), nrow = 2)
    )
  )
  function(file, b) arrays[[file]][[b]]
}

testthat::test_that("the accumulation keeps regimes apart when asked", {
  acc <- whep:::.crop_cover_accumulate(
    .regime_slab(),
    band = 1:3,
    group = c("rainfed", "irrigated", "irrigated"),
    n = c(2L, 1L)
  )
  testthat::expect_setequal(names(acc$cropped), c("rainfed", "irrigated"))
  testthat::expect_equal(as.vector(acc$cropped$rainfed), c(0.3, 0.2))
  # The calendar-less band still counts as cropped area under its regime.
  testthat::expect_equal(as.vector(acc$cropped$irrigated), c(0.15, 0.0))
  # June: rainfed crop growing, irrigated winter crop not.
  testthat::expect_equal(as.vector(acc$covered$rainfed[,, 6]), c(0.3, 0.2))
  testthat::expect_equal(as.vector(acc$covered$irrigated[,, 6]), c(0, 0))
  # January: the wrapped winter crop is growing on its 0.1, not on the 0.05.
  testthat::expect_equal(as.vector(acc$covered$rainfed[,, 1]), c(0, 0))
  testthat::expect_equal(as.vector(acc$covered$irrigated[,, 1]), c(0.1, 0))
})

testthat::test_that("per-regime rows carry the regime's share of the cell", {
  acc <- whep:::.crop_cover_accumulate(
    .regime_slab(),
    band = 1:3,
    group = c("rainfed", "irrigated", "irrigated"),
    n = c(2L, 1L)
  )
  rows <- whep:::.crop_cover_rows(
    acc,
    year = 2010L,
    lon = c(0.25, 0.75),
    lat = 5.25,
    by = "regime"
  )
  testthat::expect_named(
    rows,
    c("lon", "lat", "year", "month", "regime", "cropped_frac", "cropland_cover")
  )
  # Cell 2 has no irrigated area, so no irrigated row: 12 + 12 + 12 rows.
  testthat::expect_identical(nrow(rows), 36L)
  jan <- rows[rows$month == 1L & rows$lon == 0.25, ]
  testthat::expect_equal(
    jan$cropland_cover[jan$regime == "irrigated"],
    0.1 / 0.15
  )
  testthat::expect_equal(jan$cropped_frac[jan$regime == "irrigated"], 0.15)
  testthat::expect_equal(jan$cropland_cover[jan$regime == "rainfed"], 0)
})

testthat::test_that("the pooled read is the area-weighted pool of the regimes", {
  slab <- .regime_slab()
  pooled <- whep:::.crop_cover_rows(
    whep:::.crop_cover_accumulate(slab, 1:3, rep("cropland", 3), c(2L, 1L)),
    2010L,
    c(0.25, 0.75),
    5.25,
    by = "cropland"
  )
  testthat::expect_named(
    pooled,
    c("lon", "lat", "year", "month", "cropland_cover")
  )
  per <- whep:::.crop_cover_rows(
    whep:::.crop_cover_accumulate(
      slab,
      1:3,
      c("rainfed", "irrigated", "irrigated"),
      c(2L, 1L)
    ),
    2010L,
    c(0.25, 0.75),
    5.25,
    by = "regime"
  )
  repooled <- per |>
    dplyr::summarise(
      cropland_cover = stats::weighted.mean(cropland_cover, cropped_frac),
      .by = c("lon", "lat", "year", "month")
    ) |>
    dplyr::arrange(lon, month)
  testthat::expect_equal(
    repooled$cropland_cover,
    dplyr::arrange(pooled, lon, month)$cropland_cover
  )
  # Cell 1 in June: 0.3 of 0.45 cropped is growing.
  testthat::expect_equal(
    pooled$cropland_cover[pooled$lon == 0.25 & pooled$month == 6L],
    0.3 / 0.45
  )
})

testthat::test_that("band regimes come from the name, not the index", {
  testthat::expect_identical(
    whep:::.crop_band_regime(c(
      "rainfed rice",
      "irrigated rice",
      "temperate cereals",
      "irrigated temperate cereals"
    )),
    c("rainfed", "irrigated", "rainfed", "irrigated")
  )
})

testthat::test_that("the by argument is validated before any file is touched", {
  testthat::expect_error(
    whep::read_lpjml_crop_cover(run_dir = tempdir(), by = "crop"),
    class = "rlang_error"
  )
})

testthat::test_that("the per-regime fixture pools back to the pooled fixture", {
  pooled <- whep::read_lpjml_crop_cover(example = TRUE)
  per <- whep::read_lpjml_crop_cover(example = TRUE, by = "regime")
  testthat::expect_setequal(unique(per$regime), c("rainfed", "irrigated"))
  testthat::expect_identical(nrow(per), 24L)
  # Summer cover comes from the rainfed 0.3, winter cover from the irrigated
  # 0.1, so the pool is 0.75 in June and 0.25 in January.
  pool <- per |>
    dplyr::summarise(
      cropland_cover = stats::weighted.mean(cropland_cover, cropped_frac),
      .by = "month"
    )
  testthat::expect_equal(pool$cropland_cover[pool$month == 6L], 0.75)
  testthat::expect_equal(pool$cropland_cover[pool$month == 1L], 0.25)
  testthat::expect_identical(
    names(pooled),
    setdiff(names(per), c("regime", "cropped_frac"))
  )
})

# ---- inside the balance: regime profiles ------------------------------------

.regime_cover <- function() {
  per <- whep::read_lpjml_crop_cover(example = TRUE, by = "regime")
  per$lon <- 0.25
  per$lat <- 5.25
  per
}

testthat::test_that("regime profiles ride cropland's curve without a calendar", {
  classes <- c(
    "cropland",
    "cropland_rainfed_herbaceous",
    "cropland_irrigated_herbaceous"
  )
  out <- whep:::.cb_attach_soil_cover(.crop_cover_drivers(), classes)
  wide <- tidyr::pivot_wider(
    out[, c("month", "land_use", "soil_cover")],
    names_from = "land_use",
    values_from = "soil_cover"
  )
  testthat::expect_identical(nrow(wide), 12L)
  testthat::expect_equal(wide$cropland, wide$cropland_rainfed_herbaceous)
  testthat::expect_equal(wide$cropland, wide$cropland_irrigated_herbaceous)
  # And not bare: the curve row was found.
  testthat::expect_true(any(wide$cropland > 0))
})

testthat::test_that("a pooled calendar serves plain cropland and both regimes", {
  cover <- whep::read_lpjml_crop_cover(example = TRUE)
  cover$lat <- 5.25
  classes <- c(
    "cropland",
    "cropland_rainfed_herbaceous",
    "cropland_irrigated_herbaceous",
    "cropland_rainfed_olive",
    "grassland"
  )
  base <- whep:::.cb_attach_soil_cover(.crop_cover_drivers(), classes)
  out <- whep:::.cb_attach_soil_cover(
    .crop_cover_drivers(),
    classes,
    cropland_cover = cover
  )
  for (cl in classes[1:3]) {
    got <- out[out$land_use == cl, ]
    got <- got[order(got$month), ]
    testthat::expect_equal(got$soil_cover, cover$cropland_cover, info = cl)
  }
  # Woody groups and grassland are not annual crops: untouched.
  for (cl in classes[4:5]) {
    testthat::expect_equal(
      out$soil_cover[out$land_use == cl],
      base$soil_cover[base$land_use == cl],
      info = cl
    )
  }
})

testthat::test_that("a per-regime calendar serves each regime its own cover", {
  cover <- .regime_cover()
  classes <- c(
    "cropland",
    "cropland_rainfed_herbaceous",
    "cropland_irrigated_herbaceous"
  )
  out <- whep:::.cb_attach_soil_cover(
    .crop_cover_drivers(),
    classes,
    cropland_cover = cover
  )
  wide <- tidyr::pivot_wider(
    out[, c("month", "land_use", "soil_cover")],
    names_from = "land_use",
    values_from = "soil_cover"
  ) |>
    dplyr::arrange(month)
  rf <- cover$cropland_cover[cover$regime == "rainfed"]
  ir <- cover$cropland_cover[cover$regime == "irrigated"]
  testthat::expect_equal(wide$cropland_rainfed_herbaceous, rf)
  testthat::expect_equal(wide$cropland_irrigated_herbaceous, ir)
  # Plain cropland reads the area-weighted pool: 0.75 in June, 0.25 in Jan.
  testthat::expect_equal(wide$cropland[6], 0.75)
  testthat::expect_equal(wide$cropland[1], 0.25)
})

testthat::test_that("a per-regime layer with a foreign regime is named", {
  cover <- .regime_cover()
  cover$regime[1] <- "flooded"
  testthat::expect_error(
    whep:::.cb_attach_soil_cover(
      .crop_cover_drivers(),
      "cropland",
      cropland_cover = cover
    ),
    "flooded"
  )
  # And one that forgot the area weights cannot pool.
  testthat::expect_error(
    whep:::.cb_attach_soil_cover(
      .crop_cover_drivers(),
      "cropland",
      cropland_cover = dplyr::select(.regime_cover(), -"cropped_frac")
    ),
    "cropped_frac"
  )
})
