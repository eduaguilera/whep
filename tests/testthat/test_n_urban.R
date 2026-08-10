# These fixtures carry numeric area codes (203 Spain, 68 France) where they
# used to carry ISO3 literals. build_urban_n() routes them through
# .manure_territory_to_area_code(), whose ISO3 form is a deprecated bridge
# (#463); the resolved codes, and so every assertion below, are unchanged.
.example_cell_polity_urban <- function() {
  tibble::tribble(
    ~lon, ~lat, ~area_code,
    -0.25, -0.25, 203L,
    0.25, -0.25, 203L
  )
}

testthat::test_that("build_urban_n converts population to a nitrogen load", {
  urban_population <- tibble::tribble(
    ~lon, ~lat, ~year, ~urban_pop,
    -0.25, -0.25, 2000L, 30898536
  )
  cropland_ha <- tibble::tribble(
    ~lon, ~lat, ~area_code, ~year, ~cropland_ha,
    -0.25, -0.25, 203L, 2000L, 1000
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
    -0.25, -0.25, 203L, 2000L, 0,
    0.25, -0.25, 203L, 2000L, 1000
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
    -0.25, -0.25, 203L, 0.7,
    -0.25, -0.25, 68L, 0.3
  )
  cropland_ha <- tibble::tribble(
    ~lon, ~lat, ~area_code, ~year, ~cropland_ha,
    -0.25, -0.25, 203L, 2000L, 1000,
    -0.25, -0.25, 68L, 2000L, 1000
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
    -0.25, -0.25, 203L, 2000L, 1000,
    -0.25, -0.25, 203L, 2001L, 1000
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
    -0.25, -0.25, 203L, 2004L, 1000
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
    -0.25, -0.25, 203L, 1800L, 1000
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

# ---- C0 characterisation baseline (polycell consumer migration) --------
#
# THESE ARE CHARACTERISATION TESTS, NOT CORRECTNESS ASSERTIONS. They pin
# what build_urban_n() does TODAY, on unmodified pre-migration code, so
# that any value change the polycell consumer migration introduces is
# visible and attributable instead of silent.
#
# The fact that matters most here is a negative one: R/n_urban.R contains
# NO cell_area_ha anywhere. R/n_urban.R:100-105 is
#
#     urban_n_generated_t  is  urban_pop x urban_kgn_cap x polity_frac / 1000
#
# a pure population partition with no area term at all, so this consumer
# does NOT carry the whole-cell area defect and has nothing to re-base.
# Substituting an area denominator for polity_frac here would multiply
# tonnes of N by hectares, a roughly 1e5 inflation. The tests below pin
# both the population identity and the total absence of any area term.

.urban_c0_population <- function() {
  tibble::tribble(
    ~lon, ~lat, ~year, ~urban_pop,
    -0.25, -0.25, 2000L, 1e6,
    0.25, 59.75, 2000L, 2e5
  )
}

# One cell shared by three polities plus a single-polity cell at a very
# different latitude, so a test cannot pass by accident on a crosswalk
# where every cell has exactly one polity with polity_frac = 1 (which is
# what every other fixture in the repo uses).
#
# The codes were opaque letters when C0 was first pinned, chosen so that
# nothing could resolve them and no assertion could pass by looking one
# up. `build_urban_n()` now routes area_code through
# `.manure_territory_to_area_code()` (#463/#512), which aborts on an
# unrecognised value, so the letters are replaced by the numeric codes the
# rest of this file already uses (203 Spain, 68 France, 231 USA). Only the
# LABELS change: every property, value and tolerance pinned below is
# unchanged, and was measured to reproduce exactly under both vocabularies
# before the substitution was made.
.urban_c0_cell_polity <- function() {
  tibble::tribble(
    ~lon, ~lat, ~area_code, ~polity_frac,
    -0.25, -0.25, 203L, 0.5,
    -0.25, -0.25, 68L, 0.3,
    -0.25, -0.25, 231L, 0.2,
    0.25, 59.75, 203L, 1.0
  )
}

# Ample cropland room everywhere, so allocate_manure_transport() has no
# reason to move or withhold anything and the generated load is what the
# output carries.
.urban_c0_cropland <- function() {
  tibble::tribble(
    ~lon, ~lat, ~area_code, ~year, ~cropland_ha,
    -0.25, -0.25, 203L, 2000L, 1000,
    -0.25, -0.25, 68L, 2000L, 1000,
    -0.25, -0.25, 231L, 2000L, 1000,
    0.25, 59.75, 203L, 2000L, 1000
  )
}

.urban_c0_build <- function(
  urban_population = .urban_c0_population(),
  cell_polity = .urban_c0_cell_polity()
) {
  whep::build_urban_n(
    data = list(
      urban_population = urban_population,
      cell_polity = cell_polity,
      cropland_ha = .urban_c0_cropland()
    )
  )
}

# ---- polity_validity (#675) -------------------------------------------

# Area 277 (South Sudan) exists only from 2011: the 2000 row of this cell
# names a state that did not exist that year.
.urban_out_of_span_data <- function() {
  list(
    urban_population = tibble::tribble(
      ~lon, ~lat, ~year, ~urban_pop,
      -0.25, -0.25, 2000L, 100,
      -0.25, -0.25, 2020L, 100
    ),
    cell_polity = tibble::tribble(
      ~lon, ~lat, ~area_code,
      -0.25, -0.25, 277L
    ),
    cropland_ha = tibble::tribble(
      ~lon, ~lat, ~area_code, ~year, ~cropland_ha,
      -0.25, -0.25, 277L, 2000L, 1000,
      -0.25, -0.25, 277L, 2020L, 1000
    )
  )
}

# The 2000 benchmark rate is read from the shipped reference rather than
# hardcoded, so the pin is on the identity pop x rate / 1000 and not on
# the numeric value of the rate (which data-raw may legitimately revise).
.urban_c0_rate_2000 <- function() {
  ref <- whep::urban_kgn_cap_reference
  ref$urban_kgn_cap[ref$year == 2000L]
}

testthat::test_that("C0: urban N is population x rate, conserved to the tonne", {
  out <- .urban_c0_build()
  rate <- .urban_c0_rate_2000()
  expected_t <- sum(.urban_c0_population()$urban_pop) * rate / 1000

  testthat::expect_length(rate, 1L)
  # kg N per capita x people / 1000 = t N. Tolerance is DA-18's locked
  # 1e-9 relative bound; the measured gap on this fixture today is 0.
  testthat::expect_equal(sum(out$urban_n_t), expected_t, tolerance = 1e-9)
  # And the split across the shared cell's three polities is polity_frac
  # exactly, with no area weighting.
  shared <- out[out$lon == -0.25, , drop = FALSE]
  testthat::expect_equal(
    shared$urban_n_t[match(c(203L, 68L, 231L), shared$area_code)],
    1e6 * rate / 1000 * c(0.5, 0.3, 0.2),
    tolerance = 1e-9
  )
})

testthat::test_that("C0: the urban generation step partitions population", {
  # The test above pins the whole pipeline, in which
  # allocate_manure_transport() also conserves mass; this one isolates the
  # generation step at R/n_urban.R:92-106 so a compensating change in the
  # two halves cannot pass unnoticed. `.urban_n_generated` is a private
  # helper, so `:::` is the only access -- the same route
  # test_feed_lpjml.R uses for `.lpjml_grass_to_dm`.
  generated <- whep:::.urban_n_generated(
    .urban_c0_population(),
    .urban_c0_cell_polity()
  )
  rate <- .urban_c0_rate_2000()

  # Population is partitioned, not duplicated and not shed: the joined
  # rows' population-weighted polity_frac recovers the input head count.
  testthat::expect_equal(
    sum(generated$urban_pop * generated$polity_frac),
    sum(.urban_c0_population()$urban_pop),
    tolerance = 1e-9
  )
  testthat::expect_equal(
    sum(generated$urban_n_generated_t),
    sum(.urban_c0_population()$urban_pop) * rate / 1000,
    tolerance = 1e-9
  )
  # Four rows out of two population cells: the shared cell fans out to
  # three polities, so a row count is NOT a conservation check here.
  testthat::expect_equal(nrow(generated), 4L)
})

testthat::test_that("C0: no area column reaches urban N", {
  base <- .urban_c0_build()
  # Hand the crosswalk both area columns the migration will introduce.
  # Today they are ignored completely, because R/n_urban.R never reads an
  # area. THIS IS THE GUARD against wiring an area denominator into a
  # population partition, which would inflate urban N by ~1e5.
  with_areas <- .urban_c0_build(
    cell_polity = dplyr::mutate(
      .urban_c0_cell_polity(),
      cell_area_ha = 308000,
      land_area_ha = 270000
    )
  )

  testthat::expect_identical(with_areas, base)
})

testthat::test_that("C0: population outside the crosswalk is dropped silently", {
  # R/n_urban.R:99 joins the crosswalk with dplyr::inner_join(), so urban
  # population in a cell the crosswalk does not carry contributes nothing
  # and emits no warning. Today the crosswalk misses 1,294 LUH2
  # terrestrial cells, so this path is live. Pinned as current behaviour;
  # it is not asserted to be right.
  extra <- dplyr::bind_rows(
    .urban_c0_population(),
    tibble::tibble(lon = 9.75, lat = 9.75, year = 2000L, urban_pop = 5e5)
  )
  out <- testthat::expect_no_warning(.urban_c0_build(urban_population = extra))

  # Half a million people vanish without trace: the total is unchanged.
  testthat::expect_equal(
    sum(out$urban_n_t),
    sum(.urban_c0_population()$urban_pop) * .urban_c0_rate_2000() / 1000,
    tolerance = 1e-9
  )
  testthat::expect_false(any(out$lon == 9.75))
})

testthat::test_that("build_urban_n names an anachronistic polity", {
  testthat::expect_warning(
    out <- whep::build_urban_n(data = .urban_out_of_span_data()),
    "did not exist in that row's year"
  )

  testthat::expect_equal(nrow(out), 2L)
  testthat::expect_equal(
    out$reporting_polity_code[out$year == 2000L],
    "SSD-2011-2025"
  )
})

testthat::test_that("build_urban_n honours drop and flag", {
  testthat::expect_warning(
    dropped <- whep::build_urban_n(
      data = .urban_out_of_span_data(),
      polity_validity = "drop"
    )
  )
  testthat::expect_warning(
    flagged <- whep::build_urban_n(
      data = .urban_out_of_span_data(),
      polity_validity = "flag"
    )
  )

  testthat::expect_equal(dropped$year, 2020L)
  testthat::expect_equal(
    flagged$reporting_polity_out_of_span,
    flagged$year == 2000L
  )
})
