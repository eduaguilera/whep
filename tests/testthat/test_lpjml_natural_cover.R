# Natural-land soil cover from LPJmL foliar projective cover.

# Monthly drivers for two cells, warm enough that the RothC temperature term
# is well away from its floor, so a cover change shows up in the modifier.
.fpc_drivers <- function() {
  tidyr::expand_grid(
    lon = c(-0.25, 12.25),
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

testthat::test_that("read_lpjml_natural_cover example is self-contained", {
  out <- whep::read_lpjml_natural_cover(example = TRUE)

  testthat::skip_if_not_installed("pointblank")
  pointblank::expect_col_exists(
    out,
    columns = c("lon", "lat", "year", "natural_stand_frac", "natural_cover")
  )
  pointblank::expect_col_vals_between(
    out,
    columns = "natural_cover",
    left = 0,
    right = 1
  )
})

testthat::test_that(".fpc_year_index refuses a year the file cannot hold", {
  # Reading whatever sits at an out-of-range offset is the failure mode that
  # made a 1750 start silently run on 1851 land use in LPJmL itself.
  testthat::expect_equal(whep:::.fpc_year_index(NULL, 1901L, 3L), 1:3)
  testthat::expect_equal(whep:::.fpc_year_index(1902L, 1901L, 3L), 2L)
  testthat::expect_error(
    whep:::.fpc_year_index(1899L, 1901L, 3L),
    "outside"
  )
  testthat::expect_error(
    whep:::.fpc_year_index(1950L, 1901L, 3L),
    "outside"
  )
})

testthat::test_that("a NULL cover layer leaves every class on the curve", {
  prepared <- whep:::.cb_attach_soil_cover(
    .fpc_drivers(),
    c("cropland", "grassland", "natural")
  )
  with_null <- whep:::.cb_attach_soil_cover(
    .fpc_drivers(),
    c("cropland", "grassland", "natural"),
    natural_cover = NULL
  )
  testthat::expect_identical(prepared, with_null)

  # And the curve really is a constant for natural land, which is the thing
  # being replaced.
  nat <- prepared[prepared$land_use == "natural", ]
  testthat::expect_length(unique(nat$soil_cover), 1L)
})

testthat::test_that("a supplied layer replaces natural cover only", {
  cover <- tibble::tribble(
    ~lon, ~lat, ~year, ~natural_cover,
    -0.25, 5.25, 2010L, 0.99,
    12.25, 5.25, 2010L, 0.04
  )
  classes <- c("cropland", "grassland", "natural")
  base <- whep:::.cb_attach_soil_cover(.fpc_drivers(), classes)
  out <- whep:::.cb_attach_soil_cover(.fpc_drivers(), classes, cover)

  nat <- out[out$land_use == "natural", ]
  testthat::expect_setequal(unique(nat$soil_cover), c(0.99, 0.04))

  # Cropland and managed grassland must be untouched: fpc.nc carries the
  # natural stand only, so there is no measured cover for them.
  for (cl in c("cropland", "grassland")) {
    testthat::expect_equal(
      out$soil_cover[out$land_use == cl],
      base$soil_cover[base$land_use == cl]
    )
  }
})

testthat::test_that("a cell missing from the layer keeps the curve value", {
  # A partial layer is an ordinary state; it must not blank the cells it
  # does not cover.
  cover <- tibble::tribble(
    ~lon, ~lat, ~year, ~natural_cover,
    -0.25, 5.25, 2010L, 0.99
  )
  classes <- c("natural")
  base <- whep:::.cb_attach_soil_cover(.fpc_drivers(), classes)
  out <- whep:::.cb_attach_soil_cover(.fpc_drivers(), classes, cover)

  testthat::expect_true(all(out$soil_cover[out$lon == -0.25] == 0.99))
  testthat::expect_equal(
    out$soil_cover[out$lon == 12.25],
    base$soil_cover[base$lon == 12.25]
  )
})

testthat::test_that("bare natural land decomposes faster than the constant", {
  # The whole point: 0.6 + 0.4 * (1 - cover) sends a near-bare cell from
  # 0.66 to ~1.00, so the modifier RISES and equilibrium carbon - which
  # scales as 1 / modifier - falls.
  keys <- c("lon", "lat", "area_code", "year", "land_use")
  bare <- tibble::tribble(
    ~lon, ~lat, ~year, ~natural_cover,
    -0.25, 5.25, 2010L, 0.02,
    12.25, 5.25, 2010L, 0.02
  )
  curve_mod <- whep:::.cb_rothc_modifier_vectorised(
    whep:::.cb_attach_soil_cover(.fpc_drivers(), "natural"),
    "hsoc",
    keys
  )
  bare_mod <- whep:::.cb_rothc_modifier_vectorised(
    whep:::.cb_attach_soil_cover(.fpc_drivers(), "natural", bare),
    "hsoc",
    keys
  )
  testthat::expect_true(
    all(bare_mod$climate_modifier > curve_mod$climate_modifier)
  )

  # A fully vegetated cell goes the other way.
  full <- dplyr::mutate(bare, natural_cover = 1)
  full_mod <- whep:::.cb_rothc_modifier_vectorised(
    whep:::.cb_attach_soil_cover(.fpc_drivers(), "natural", full),
    "hsoc",
    keys
  )
  testthat::expect_true(
    all(full_mod$climate_modifier < curve_mod$climate_modifier)
  )
})

testthat::test_that("a cover layer missing its columns is named", {
  testthat::expect_error(
    whep:::.cb_attach_soil_cover(
      .fpc_drivers(),
      "natural",
      tibble::tibble(lon = 1, lat = 2)
    ),
    "natural_cover"
  )
})
