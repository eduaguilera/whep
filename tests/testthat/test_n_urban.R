.example_cell_polity_urban <- function() {
  tibble::tribble(
    ~lon, ~lat, ~area_code,
    -0.25, -0.25, "ESP",
    0.25, -0.25, "ESP"
  )
}

testthat::test_that("build_urban_n converts population to a nitrogen load", {
  urban_population <- tibble::tribble(
    ~lon, ~lat, ~year, ~urban_pop,
    -0.25, -0.25, 2000L, 30898536
  )
  cropland_ha <- tibble::tribble(
    ~lon, ~lat, ~area_code, ~year, ~cropland_ha,
    -0.25, -0.25, "ESP", 2000L, 1000
  )
  out <- whep::build_urban_n(
    data = list(
      urban_population = urban_population,
      cell_polity = .example_cell_polity_urban(),
      cropland_ha = cropland_ha
    )
  )

  pointblank::expect_col_exists(
    out,
    c("lon", "lat", "area_code", "year", "urban_n_t", "method_urban")
  )
  # At year 2000 (an urban_kgn_cap_reference benchmark year), the whole
  # population generates urban_pop * urban_kgn_cap / 1000 t N. This is a
  # single-cell scenario with no same-polity neighbour, so
  # allocate_manure_transport() cannot move anything: the generated load
  # lands entirely on its own cell as residual, regardless of that cell's
  # own room (a cell's own room only bounds what its NEIGHBOURS can send it,
  # not its own locally generated load; see test below for the transport
  # case). 0.9410902 is the real HYDE-derived 2000 rate, weighted by
  # polity_frac (see data-raw/build_urban_kgn_cap.R).
  expected_n_t <- 30898536 * 0.9410902351391244 / 1000
  testthat::expect_equal(out$urban_n_t, expected_n_t, tolerance = 1e-6)
  testthat::expect_equal(out$method_urban, "spain_hist_rate|room_weighted")
})

testthat::test_that("build_urban_n spills surplus to a neighbouring cell with cropland room", {
  # Source cell has urban population but NO cropland: the whole load must be
  # transported to its same-polity neighbour, which has cropland room. This
  # is the explicit test that allocate_manure_transport() is really wired in
  # and really moves N between cells, not a no-op. The population is small
  # enough that the generated N (100 * 0.9410902 / 1000 = 0.0941 t) fits
  # comfortably within the neighbour's room (170 kg/ha * 1000 ha = 170 t),
  # so the whole load is transportable, not partially residual.
  urban_population <- tibble::tribble(
    ~lon, ~lat, ~year, ~urban_pop,
    -0.25, -0.25, 2000L, 100,
    0.25, -0.25, 2000L, 0
  )
  cropland_ha <- tibble::tribble(
    ~lon, ~lat, ~area_code, ~year, ~cropland_ha,
    -0.25, -0.25, "ESP", 2000L, 0,
    0.25, -0.25, "ESP", 2000L, 1000
  )
  out <- whep::build_urban_n(
    data = list(
      urban_population = urban_population,
      cell_polity = .example_cell_polity_urban(),
      cropland_ha = cropland_ha
    )
  )

  source_row <- out[out$lon == -0.25, , drop = FALSE]
  sink_row <- out[out$lon == 0.25, , drop = FALSE]

  # The source cell's own urban N is fully transported away: it should carry
  # zero (or be absent from the result), never the un-transported amount.
  testthat::expect_true(
    nrow(source_row) == 0 || sum(source_row$urban_n_t) < 1e-6
  )
  # The neighbour cell actually receives the transported load.
  expected_n_t <- 100 * 0.9410902351391244 / 1000
  testthat::expect_equal(sink_row$urban_n_t, expected_n_t, tolerance = 1e-6)
  testthat::expect_true(sink_row$urban_n_t > 0)
})

testthat::test_that("build_urban_n splits a border cell by polity_frac", {
  urban_population <- tibble::tribble(
    ~lon, ~lat, ~year, ~urban_pop,
    -0.25, -0.25, 2000L, 1000
  )
  cell_polity <- tibble::tribble(
    ~lon, ~lat, ~area_code, ~polity_frac,
    -0.25, -0.25, "ESP", 0.7,
    -0.25, -0.25, "FRA", 0.3
  )
  cropland_ha <- tibble::tribble(
    ~lon, ~lat, ~area_code, ~year, ~cropland_ha,
    -0.25, -0.25, "ESP", 2000L, 1000,
    -0.25, -0.25, "FRA", 2000L, 1000
  )
  out <- whep::build_urban_n(
    data = list(
      urban_population = urban_population,
      cell_polity = cell_polity,
      cropland_ha = cropland_ha
    )
  )

  generated_n_t <- 1000 * 0.9410902351391244 / 1000
  testthat::expect_equal(sum(out$urban_n_t), generated_n_t, tolerance = 1e-9)
  # The ISO3 the fixture keys cells by is resolved to the numeric WHEP area
  # code on the way out (203 Spain, 68 France), because the output now carries
  # the reporting polity that code resolves to.
  testthat::expect_equal(
    out$urban_n_t[match(c(203L, 68L), out$area_code)],
    generated_n_t * c(0.7, 0.3),
    tolerance = 1e-9
  )
})

testthat::test_that("build_urban_n filters preloaded inputs by years", {
  urban_population <- tibble::tribble(
    ~lon, ~lat, ~year, ~urban_pop,
    -0.25, -0.25, 2000L, 100,
    -0.25, -0.25, 2001L, 100
  )
  cropland_ha <- tibble::tribble(
    ~lon, ~lat, ~area_code, ~year, ~cropland_ha,
    -0.25, -0.25, "ESP", 2000L, 1000,
    -0.25, -0.25, "ESP", 2001L, 1000
  )

  out <- whep::build_urban_n(
    years = 2001L,
    data = list(
      urban_population = urban_population,
      cell_polity = .example_cell_polity_urban(),
      cropland_ha = cropland_ha
    )
  )

  testthat::expect_equal(out$year, 2001L)
  testthat::expect_equal(nrow(out), 1L)
})

testthat::test_that("build_urban_n interpolates the per-capita rate between benchmark years", {
  # 2004 is midway between the real HYDE-derived 2000 (0.9410902) and 2008
  # (1.2422579) benchmark rates in urban_kgn_cap_reference (polity_frac
  # weighted).
  urban_population <- tibble::tribble(
    ~lon, ~lat, ~year, ~urban_pop,
    -0.25, -0.25, 2004L, 1000000
  )
  cropland_ha <- tibble::tribble(
    ~lon, ~lat, ~area_code, ~year, ~cropland_ha,
    -0.25, -0.25, "ESP", 2004L, 1000
  )
  out <- whep::build_urban_n(
    data = list(
      urban_population = urban_population,
      cell_polity = .example_cell_polity_urban(),
      cropland_ha = cropland_ha
    )
  )

  interpolated_rate <- 0.9410902351391244 +
    (1.242257867931792 - 0.9410902351391244) * (2004 - 2000) / (2008 - 2000)
  expected_n_t <- 1000000 * interpolated_rate / 1000
  testthat::expect_equal(out$urban_n_t, expected_n_t, tolerance = 1e-6)
})

testthat::test_that("build_urban_n holds the rate constant outside the benchmark range", {
  urban_population <- tibble::tribble(
    ~lon, ~lat, ~year, ~urban_pop,
    -0.25, -0.25, 1800L, 1000000
  )
  cropland_ha <- tibble::tribble(
    ~lon, ~lat, ~area_code, ~year, ~cropland_ha,
    -0.25, -0.25, "ESP", 1800L, 1000
  )
  out <- whep::build_urban_n(
    data = list(
      urban_population = urban_population,
      cell_polity = .example_cell_polity_urban(),
      cropland_ha = cropland_ha
    )
  )

  # 1800 is before the earliest urban_kgn_cap_reference benchmark (now 1860,
  # the real HYDE-derived rate, polity_frac weighted; see
  # data-raw/build_urban_kgn_cap.R), so the rate is carried backward from
  # 1860.
  expected_n_t <- 1000000 * 1.084416541366471 / 1000
  testthat::expect_equal(out$urban_n_t, expected_n_t, tolerance = 1e-6)
})

testthat::test_that("build_urban_n requires cell_polity and cropland_ha", {
  urban_population <- tibble::tribble(
    ~lon, ~lat, ~year, ~urban_pop,
    -0.25, -0.25, 2000L, 100
  )
  testthat::expect_error(
    whep::build_urban_n(data = list(urban_population = urban_population)),
    "cell_polity"
  )
  testthat::expect_error(
    whep::build_urban_n(
      data = list(
        urban_population = urban_population,
        cell_polity = .example_cell_polity_urban()
      )
    ),
    "cropland_ha"
  )
})

testthat::test_that("build_urban_n example fixture is schema-complete", {
  out <- whep::build_urban_n(example = TRUE)
  pointblank::expect_col_exists(
    out,
    c("lon", "lat", "area_code", "year", "urban_n_t", "method_urban")
  )
  pointblank::expect_col_vals_gte(out, "urban_n_t", 0)
})
