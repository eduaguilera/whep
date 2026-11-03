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

# build_cell_climate_zone, reading NetCDF -------------------------------------

# Three land cells, 1901-1904, whose monthly temperatures are a constant plus a
# month offset that sums to zero over the year plus a 0.2 degC/year trend, so
# each cell's annual mean is known without running the reader: cell 2 = 5 degC
# (Cool), cell 3 = 14 degC (Temperate), cell 4 = 22 degC (Warm), each plus
# 0.2 * (year - 1901). Cell 1 is ocean.
.golden_tmp <- function(cell, year, month) {
  c(NA, 5, 14, 22)[cell] + (month - 6.5) + 0.2 * (year - 1901)
}

testthat::test_that("the NetCDF path returns the golden zone table", {
  cube <- .cru_annual_cube("tmp", .golden_tmp)

  result <- whep::build_cell_climate_zone(
    years = c(1899L, 1902L, 2010L),
    cru_dir = cube$dir
  )

  expected <- tibble::tribble(
    ~lon,     ~lat,   ~year, ~mean_annual_temp_c, ~climate_zone, ~method_climate_zone,
    -179.75, -89.25, 1899L,               14.3,  "Temperate", "climatology_1901_1904",
    -179.25, -89.75, 1899L,                5.3,       "Cool", "climatology_1901_1904",
    -179.25, -89.25, 1899L,               22.3,       "Warm", "climatology_1901_1904",
    -179.75, -89.25, 1902L,               14.2,  "Temperate",         "cru_ts_annual",
    -179.25, -89.75, 1902L,                5.2,       "Cool",         "cru_ts_annual",
    -179.25, -89.25, 1902L,               22.2,       "Warm",         "cru_ts_annual",
    -179.75, -89.25, 2010L,               14.3,  "Temperate", "climatology_1901_1904",
    -179.25, -89.75, 2010L,                5.3,       "Cool", "climatology_1901_1904",
    -179.25, -89.25, 2010L,               22.3,       "Warm", "climatology_1901_1904"
  )
  # NetCDF stores single precision, so values are compared to 1e-6.
  testthat::expect_equal(result, expected, tolerance = 1e-6)
  # The reader works in data.table; the result must not carry its pointer out.
  testthat::expect_null(attr(result, ".internal.selfref"))
})

testthat::test_that("a cell short of twelve months is dropped with a warning", {
  holes <- tibble::tibble(cell = 4L, year = 1902L, month = 1L)
  cube <- .cru_annual_cube("tmp", .golden_tmp, holes = holes)

  testthat::expect_warning(
    result <- whep::build_cell_climate_zone(years = 1902L, cru_dir = cube$dir),
    "Dropping 1 cell with fewer than 12 CRU months in 1902"
  )

  testthat::expect_equal(nrow(result), 2L)
  testthat::expect_setequal(result$climate_zone, c("Temperate", "Cool"))
})

# .cru_annual -----------------------------------------------------------------

# Days per month, written out here rather than taken from the package so the
# conversions below are checked against an independent calendar.
.days_common <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
.days_leap <- c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

testthat::test_that("tmp reduces to the mean of the twelve monthly values", {
  cube <- .cru_annual_cube("tmp", .golden_tmp)

  result <- whep:::.cru_annual(cube$dir, c(1901L, 1903L), "tmp") |>
    dplyr::arrange(year, lon, lat)

  testthat::expect_named(
    result,
    c("lon", "lat", "year", "mean_annual_temp_c")
  )
  # Cell order by (year, lon, lat): cell 3, cell 2, cell 4.
  testthat::expect_equal(
    result$mean_annual_temp_c,
    rep(c(14, 5, 22), 2) + rep(c(0, 0.4), each = 3),
    tolerance = 1e-6
  )
})

testthat::test_that("pre is summed over the year, not averaged", {
  # mm/month: 10 * cell + month, plus 1 mm per month per year after 1901.
  cube <- .cru_annual_cube(
    "pre",
    \(cell, year, month) 10 * cell + month + (year - 1901)
  )

  result <- whep:::.cru_annual(cube$dir, 1902L, "pre") |>
    dplyr::arrange(lon, lat)

  testthat::expect_named(result, c("lon", "lat", "year", "annual_precip_mm"))
  # Cell 3, cell 2, cell 4: 12 * 10 * cell + (1 + ... + 12) + 12 * 1.
  testthat::expect_equal(
    result$annual_precip_mm,
    120 * c(3, 2, 4) + 78 + 12
  )
})

testthat::test_that("pet is a daily rate scaled by the days of each month", {
  # mm/day: cell * month, so neither a plain sum nor mean * 365 reproduces it.
  cube <- .cru_annual_cube(
    "pet",
    \(cell, year, month) cell * month,
    years = 1903:1904
  )

  result <- whep:::.cru_annual(cube$dir, c(1903L, 1904L), "pet") |>
    dplyr::filter(lon == -179.25, lat == -89.75)

  testthat::expect_named(result, c("lon", "lat", "year", "annual_pet_mm"))
  # Cell 2 at (lon[2], lat[1]). 1903 is a common year, 1904 a leap year.
  testthat::expect_equal(
    result$annual_pet_mm,
    c(2 * sum(1:12 * .days_common), 2 * sum(1:12 * .days_leap))
  )
})

testthat::test_that("frs is the sum of monthly frost days and keeps zeros", {
  monthly <- c(20, 15, 10, 5, 0, 0, 0, 0, 0, 5, 10, 15)
  cube <- .cru_annual_cube(
    "frs",
    \(cell, year, month) c(NA, 1, 0, 2)[cell] * monthly[month]
  )

  result <- whep:::.cru_annual(cube$dir, 1902L, "frs") |>
    dplyr::arrange(lon, lat)

  testthat::expect_named(result, c("lon", "lat", "year", "annual_frost_days"))
  # Cell 3 has no frost at all: a measured zero, not a dropped or NA cell.
  testthat::expect_equal(result$annual_frost_days, c(0, 80, 160))
  testthat::expect_false(anyNA(result$annual_frost_days))
})

testthat::test_that("an unknown CRU variable aborts before any file is read", {
  cube <- .cru_annual_cube("tmp", .golden_tmp)

  testthat::expect_error(
    whep:::.cru_annual(cube$dir, 1902L, "tmx"),
    "must be one of"
  )
  testthat::expect_error(
    whep:::.cru_annual(cube$dir, 1902L, "wet"),
    class = "rlang_error"
  )
})

testthat::test_that("a variable file without the requested year aborts", {
  dir <- withr::local_tempdir()
  .cru_annual_cube("tmp", .golden_tmp, years = 1901:1904, dir = dir)
  .cru_annual_cube(
    "pre",
    \(cell, year, month) cell,
    years = 1901:1903,
    dir = dir
  )

  testthat::expect_equal(whep:::.cru_covered_years(dir, "pre"), 1901:1903)
  testthat::expect_error(
    whep:::.cru_annual(dir, 1904L, "pre"),
    "no time steps"
  )
})

testthat::test_that("only the requested years' slabs are read", {
  cube <- .cru_annual_cube("pre", \(cell, year, month) cell)
  reads <- new.env()
  reads$n <- 0L
  slice <- whep:::.cru_slice_to_long
  testthat::local_mocked_bindings(
    .cru_slice_to_long = function(...) {
      reads$n <- reads$n + 1L
      slice(...)
    }
  )

  whep:::.cru_annual(cube$dir, 1902L, "pre")

  testthat::expect_equal(reads$n, 12L)
})

# The climatology fill, for the variables other than temperature --------------

testthat::test_that("held climatologies carry every annual variable", {
  dir <- withr::local_tempdir()
  .cru_annual_cube(
    "pre",
    \(cell, year, month) 10 * cell + (year - 1901),
    dir = dir
  )
  .cru_annual_cube("frs", \(cell, year, month) cell * (year - 1900), dir = dir)
  covered <- whep:::.cru_covered_years(dir, "pre")
  plan <- whep:::.climate_year_plan(c(1850L, 1902L), covered)
  annual <- dplyr::inner_join(
    whep:::.cru_annual(dir, plan$read_years, "pre"),
    whep:::.cru_annual(dir, plan$read_years, "frs"),
    by = c("lon", "lat", "year")
  )

  result <- whep:::.assemble_cell_climate(annual, plan) |>
    dplyr::filter(lon == -179.25, lat == -89.75)

  testthat::expect_named(
    result,
    c(
      "lon",
      "lat",
      "year",
      "annual_precip_mm",
      "annual_frost_days",
      "climate_record"
    )
  )
  testthat::expect_equal(result$year, c(1850L, 1902L))
  testthat::expect_equal(
    result$climate_record,
    c("climatology_1901_1904", "cru_ts_annual")
  )
  # Cell 2. Precipitation sums 12 months of 20 + (year - 1901) per year; frost
  # days sum 12 months of 2 * (year - 1900). The held row is the mean of the
  # four annual values, the measured row is 1902 itself.
  testthat::expect_equal(
    result$annual_precip_mm,
    c(mean(12 * (20 + 0:3)), 12 * (20 + 1))
  )
  testthat::expect_equal(
    result$annual_frost_days,
    c(mean(12 * 2 * (1:4)), 12 * 2 * 2)
  )
})

# .climate_zone_from_mat ------------------------------------------------------

testthat::test_that("the shared zone helper is reused, not forked", {
  testthat::expect_equal(
    whep:::.climate_zone_from_mat(c(-5, 10, 18, 30, NA)),
    c("Cool", "Cool", "Temperate", "Warm", NA)
  )
})
