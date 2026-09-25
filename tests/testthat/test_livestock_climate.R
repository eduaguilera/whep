# test_livestock_climate.R ----------------------------------------------------

# A four-cell, 35-year stand-in for the CRU record. Two cells sit either side of
# the 18 degC Temperate/Warm cut and one below the 10 degC Cool cut, and the
# series warms with time so a held climatology is distinguishable from any
# single measured year.
.mat_fixture <- function(years = 1901:1935) {
  tidyr::expand_grid(
    lon = c(34.25, 34.75),
    lat = c(-0.25, 0.25),
    year = years
  ) |>
    dplyr::mutate(
      mean_annual_temp_c = 9 +
        (lon - 34.25) * 18 +
        (lat + 0.25) * 4 +
        (year - 1901) * 0.1
    )
}

# build_cell_climate_zone -----------------------------------------------------

testthat::test_that("measured years are labelled as measured", {
  result <- whep::build_cell_climate_zone(
    years = c(1901L, 1935L),
    data = .mat_fixture()
  )

  result |>
    pointblank::expect_col_exists(
      c(
        "lon",
        "lat",
        "year",
        "mean_annual_temp_c",
        "climate_zone",
        "method_climate_zone"
      )
    )
  testthat::expect_setequal(result$year, c(1901L, 1935L))
  testthat::expect_true(all(result$method_climate_zone == "cru_ts_annual"))
})

testthat::test_that("years before the record hold the 30-year climatology", {
  result <- whep::build_cell_climate_zone(
    years = c(1851L, 1900L),
    data = .mat_fixture()
  )

  testthat::expect_true(
    all(result$method_climate_zone == "climatology_1901_1930")
  )
  # Every backcast year carries the same value: the climatology is held, not
  # extrapolated.
  spread <- result |>
    dplyr::summarise(
      n_values = dplyr::n_distinct(mean_annual_temp_c),
      .by = c(lon, lat)
    )
  testthat::expect_true(all(spread$n_values == 1L))
})

testthat::test_that("a backcast label is never a measured label", {
  result <- whep::build_cell_climate_zone(
    years = c(1880L, 1910L),
    data = .mat_fixture()
  )

  labels <- result |>
    dplyr::distinct(year, method_climate_zone)
  testthat::expect_equal(
    labels$method_climate_zone[labels$year == 1880L],
    "climatology_1901_1930"
  )
  testthat::expect_equal(
    labels$method_climate_zone[labels$year == 1910L],
    "cru_ts_annual"
  )
})

testthat::test_that("the held climatology is the window mean, not one year", {
  fixture <- .mat_fixture()
  result <- whep::build_cell_climate_zone(years = 1890L, data = fixture)

  expected <- fixture |>
    dplyr::filter(year %in% 1901:1930) |>
    dplyr::summarise(mat = mean(mean_annual_temp_c), .by = c(lon, lat))
  joined <- dplyr::inner_join(result, expected, by = c("lon", "lat"))

  testthat::expect_equal(joined$mean_annual_temp_c, joined$mat)
})

testthat::test_that("years after the record hold the closing climatology", {
  result <- whep::build_cell_climate_zone(
    years = 2023L,
    data = .mat_fixture()
  )

  testthat::expect_true(
    all(result$method_climate_zone == "climatology_1906_1935")
  )
})

testthat::test_that("an interior hole in the record aborts", {
  gapped <- .mat_fixture(c(1901:1910, 1912:1935))

  testthat::expect_error(
    whep::build_cell_climate_zone(years = 1911L, data = gapped),
    "no data for"
  )
})

testthat::test_that("a missing temperature aborts rather than losing a cell", {
  broken <- .mat_fixture() |>
    dplyr::mutate(
      mean_annual_temp_c = dplyr::if_else(
        lon == 34.25 & year == 1905L,
        NA_real_,
        mean_annual_temp_c
      )
    )

  testthat::expect_error(
    whep::build_cell_climate_zone(years = 1905L, data = broken),
    "missing"
  )
})

testthat::test_that("zones use the IPCC cuts, not the guessed 15/25 ones", {
  # 10 and 18 degC are the Cool/Temperate and Temperate/Warm boundaries
  # (IPCC 2006 Vol.4 Ch.3), both closed on the left of the next class.
  edges <- tibble::tribble(
    ~lon,   ~lat, ~year, ~mean_annual_temp_c,
    0.25,  0.25, 2000L,                 9.9,
    0.75,  0.25, 2000L,                10.0,
    1.25,  0.25, 2000L,                10.1,
    1.75,  0.25, 2000L,                18.0,
    2.25,  0.25, 2000L,                18.1
  )

  result <- whep::build_cell_climate_zone(years = 2000L, data = edges)

  testthat::expect_equal(
    result$climate_zone,
    c("Cool", "Cool", "Temperate", "Temperate", "Warm")
  )
})

testthat::test_that("every zone emitted exists in climate_mcf", {
  result <- whep::build_cell_climate_zone(
    years = c(1890L, 1910L),
    data = .mat_fixture()
  )

  testthat::expect_true(
    all(result$climate_zone %in% whep::climate_mcf$climate_zone)
  )
})

testthat::test_that("the example fixture matches the documented contract", {
  result <- whep::build_cell_climate_zone(example = TRUE)

  result |>
    pointblank::expect_col_exists(
      c(
        "lon",
        "lat",
        "year",
        "mean_annual_temp_c",
        "climate_zone",
        "method_climate_zone"
      )
    )
  result |>
    pointblank::expect_col_vals_in_set(
      "climate_zone",
      c("Cool", "Temperate", "Warm")
    )
  testthat::expect_true(
    any(startsWith(result$method_climate_zone, "climatology_"))
  )
})

# .climate_zone_from_mat ------------------------------------------------------

testthat::test_that("the shared zone helper is reused, not forked", {
  testthat::expect_equal(
    whep:::.climate_zone_from_mat(c(-5, 10, 18, 30, NA)),
    c("Cool", "Cool", "Temperate", "Warm", NA)
  )
})
