# These fixtures carry numeric area codes (203 Spain, 68 France) where they
# used to carry ISO3 literals. build_human_n() requires the numeric WHEP area
# code and refuses anything else (#597); when the literals were still bridged
# through .manure_territory_to_area_code() (#463) they resolved to these same
# codes, so every assertion below is unchanged.
.example_cell_polity_human <- function() {
  tibble::tribble(
    ~lon, ~lat, ~area_code,
    -0.25, -0.25, 203L,
    0.25, -0.25, 203L
  )
}

testthat::test_that("build_human_n converts population to a nitrogen load", {
  urban_population <- tibble::tribble(
    ~lon, ~lat, ~year, ~urban_pop,
    -0.25, -0.25, 2000L, 30898536
  )
  cropland_ha <- tibble::tribble(
    ~lon, ~lat, ~area_code, ~year, ~cropland_ha,
    -0.25, -0.25, 203L, 2000L, 1000
  )
  testthat::expect_warning(
    out <- whep::build_human_n(
      population_basis = "urban",
      data = list(
        urban_population = urban_population,
        cell_polity = .example_cell_polity_human(),
        cropland_ha = cropland_ha
      )
    ),
    class = "whep_human_n_undelivered"
  )

  pointblank::expect_col_exists(
    out,
    c("lon", "lat", "area_code", "year", "human_n_t", "method_human")
  )
  # At year 2000 (an human_kgn_cap_reference benchmark year), the whole
  # population generates urban_pop * human_kgn_cap / 1000 t N. This is a
  # single-cell scenario with no same-polity neighbour, so
  # allocate_manure_transport() cannot move anything: the generated load
  # lands entirely on its own cell as residual. Only 170 t of it fits the
  # cell's room; the polity has no other cropland, so the rest stays there,
  # flagged as stranded and warned about (#1336). 0.9410902 is the real
  # HYDE-derived 2000 rate, weighted by polity_frac (see
  # data-raw/build_human_kgn_cap.R).
  expected_n_t <- 30898536 * 0.9410902351391244 / 1000
  testthat::expect_equal(out$human_n_t, expected_n_t, tolerance = 1e-6)
  testthat::expect_equal(
    out$human_n_stranded_t,
    expected_n_t - 170,
    tolerance = 1e-6
  )
  testthat::expect_equal(out$method_human, "calibration_rate|room_weighted")
})

testthat::test_that("build_human_n spills surplus to a neighbouring cell with cropland room", {
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
  out <- whep::build_human_n(
    population_basis = "urban",
    data = list(
      urban_population = urban_population,
      cell_polity = .example_cell_polity_human(),
      cropland_ha = cropland_ha
    )
  )

  source_row <- out[out$lon == -0.25, , drop = FALSE]
  sink_row <- out[out$lon == 0.25, , drop = FALSE]

  # The source cell's own urban N is fully transported away: it should carry
  # zero (or be absent from the result), never the un-transported amount.
  testthat::expect_true(
    nrow(source_row) == 0 || sum(source_row$human_n_t) < 1e-6
  )
  # The neighbour cell actually receives the transported load.
  expected_n_t <- 100 * 0.9410902351391244 / 1000
  testthat::expect_equal(sink_row$human_n_t, expected_n_t, tolerance = 1e-6)
  testthat::expect_true(sink_row$human_n_t > 0)
})

testthat::test_that("build_human_n splits a border cell by polity_frac", {
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
  out <- whep::build_human_n(
    population_basis = "urban",
    data = list(
      urban_population = urban_population,
      cell_polity = cell_polity,
      cropland_ha = cropland_ha
    )
  )

  generated_n_t <- 1000 * 0.9410902351391244 / 1000
  testthat::expect_equal(sum(out$human_n_t), generated_n_t, tolerance = 1e-9)
  # The numeric WHEP area code the fixture keys cells by (203 Spain, 68
  # France) is what the output carries, because the reporting polity is
  # resolved from it.
  testthat::expect_equal(
    out$human_n_t[match(c(203L, 68L), out$area_code)],
    generated_n_t * c(0.7, 0.3),
    tolerance = 1e-9
  )
})

testthat::test_that("the urban basis is bit-identical to main (5421973b)", {
  # Main computed urban_n_generated_t as
  # urban_pop * urban_kgn_cap * polity_frac / 1000, in that exact grouping
  # (R/n_urban.R:197-200 at 5421973b). Floating-point multiplication is not
  # associative, so (urban_pop * polity_frac) * kgn_cap / 1000 -- an
  # intermediate shape this rename briefly took -- differs from main by
  # about 5e-16 relative on this fixture even though it is mathematically
  # the same product. These two values were captured by running
  # build_urban_n() on this exact fixture at 5421973b.
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
  out <- whep::build_human_n(
    population_basis = "urban",
    data = list(
      urban_population = urban_population,
      cell_polity = cell_polity,
      cropland_ha = cropland_ha
    )
  )
  main_n_t <- c(`68` = 0.28232707054173733496, `203` = 0.6587631645973870409)
  testthat::expect_identical(
    out$human_n_t[match(c(68L, 203L), out$area_code)],
    unname(main_n_t)
  )
})

testthat::test_that("build_human_n filters preloaded inputs by years", {
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

  out <- whep::build_human_n(
    population_basis = "urban",
    years = 2001L,
    data = list(
      urban_population = urban_population,
      cell_polity = .example_cell_polity_human(),
      cropland_ha = cropland_ha
    )
  )

  testthat::expect_equal(out$year, 2001L)
  testthat::expect_equal(nrow(out), 1L)
})

testthat::test_that("build_human_n interpolates the per-capita rate between benchmark years", {
  # 2004 is midway between the real HYDE-derived 2000 (0.9410902) and 2008
  # (1.2422579) benchmark rates in human_kgn_cap_reference (polity_frac
  # weighted).
  urban_population <- tibble::tribble(
    ~lon, ~lat, ~year, ~urban_pop,
    -0.25, -0.25, 2004L, 1000000
  )
  cropland_ha <- tibble::tribble(
    ~lon, ~lat, ~area_code, ~year, ~cropland_ha,
    -0.25, -0.25, 203L, 2004L, 1000
  )
  # One cell over its room with nowhere to send the excess; this pins the
  # rate, so the room cap (pinned in the #1336 block) is switched off.
  out <- whep::build_human_n(
    population_basis = "urban",
    method_local_residual = "uncapped",
    data = list(
      urban_population = urban_population,
      cell_polity = .example_cell_polity_human(),
      cropland_ha = cropland_ha
    )
  )

  interpolated_rate <- 0.9410902351391244 +
    (1.242257867931792 - 0.9410902351391244) * (2004 - 2000) / (2008 - 2000)
  expected_n_t <- 1000000 * interpolated_rate / 1000
  testthat::expect_equal(out$human_n_t, expected_n_t, tolerance = 1e-6)
})

testthat::test_that("build_human_n holds the rate constant outside the benchmark range", {
  urban_population <- tibble::tribble(
    ~lon, ~lat, ~year, ~urban_pop,
    -0.25, -0.25, 1800L, 1000000
  )
  cropland_ha <- tibble::tribble(
    ~lon, ~lat, ~area_code, ~year, ~cropland_ha,
    -0.25, -0.25, 203L, 1800L, 1000
  )
  # One cell over its room with nowhere to send the excess; this pins the
  # rate, so the room cap (pinned in the #1336 block) is switched off.
  out <- whep::build_human_n(
    population_basis = "urban",
    method_local_residual = "uncapped",
    data = list(
      urban_population = urban_population,
      cell_polity = .example_cell_polity_human(),
      cropland_ha = cropland_ha
    )
  )

  # 1800 is before the earliest human_kgn_cap_reference benchmark (now 1860,
  # the real HYDE-derived rate, polity_frac weighted; see
  # data-raw/build_human_kgn_cap.R), so the rate is carried backward from
  # 1860.
  expected_n_t <- 1000000 * 1.084416541366471 / 1000
  testthat::expect_equal(out$human_n_t, expected_n_t, tolerance = 1e-6)
})

testthat::test_that("build_human_n requires cell_polity and cropland_ha", {
  urban_population <- tibble::tribble(
    ~lon, ~lat, ~year, ~urban_pop,
    -0.25, -0.25, 2000L, 100
  )
  testthat::expect_error(
    whep::build_human_n(
      population_basis = "urban",
      data = list(urban_population = urban_population)
    ),
    "cell_polity"
  )
  testthat::expect_error(
    whep::build_human_n(
      population_basis = "urban",
      data = list(
        urban_population = urban_population,
        cell_polity = .example_cell_polity_human()
      )
    ),
    "cropland_ha"
  )
})

testthat::test_that("build_human_n example fixture is schema-complete", {
  out <- whep::build_human_n(example = TRUE)
  pointblank::expect_col_exists(
    out,
    c("lon", "lat", "area_code", "year", "human_n_t", "method_human")
  )
  pointblank::expect_col_vals_gte(out, "human_n_t", 0)
})

# ---- C0 characterisation baseline (polycell consumer migration) --------
#
# THESE ARE CHARACTERISATION TESTS, NOT CORRECTNESS ASSERTIONS. They pin
# what build_human_n() does TODAY, on unmodified pre-migration code, so
# that any value change the polycell consumer migration introduces is
# visible and attributable instead of silent.
#
# The fact that matters most here is a negative one: R/n_human.R contains
# NO cell_area_ha anywhere. R/n_human.R:100-105 is
#
#     human_n_generated_t  is  urban_pop x human_kgn_cap x polity_frac / 1000
#
# a pure population partition with no area term at all, so this consumer
# does NOT carry the whole-cell area defect and has nothing to re-base.
# Substituting an area denominator for polity_frac here would multiply
# tonnes of N by hectares, a roughly 1e5 inflation. The tests below pin
# both the population identity and the total absence of any area term.

.human_c0_population <- function() {
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
# up. `build_human_n()` now requires `area_code` to BE the numeric WHEP area
# code (#463/#512, tightened in #597), so the letters are replaced by the
# numeric codes the rest of this file already uses (203 Spain, 68 France,
# 231 USA). Only the LABELS change: every property, value and tolerance
# pinned below is unchanged, and was measured to reproduce exactly under both
# vocabularies before the substitution was made.
.human_c0_cell_polity <- function() {
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
.human_c0_cropland <- function() {
  tibble::tribble(
    ~lon, ~lat, ~area_code, ~year, ~cropland_ha,
    -0.25, -0.25, 203L, 2000L, 1000,
    -0.25, -0.25, 68L, 2000L, 1000,
    -0.25, -0.25, 231L, 2000L, 1000,
    0.25, 59.75, 203L, 2000L, 1000
  )
}

.human_c0_build <- function(
  urban_population = .human_c0_population(),
  cell_polity = .human_c0_cell_polity()
) {
  # Every C0 cell is over its 170 t room with no same-polity neighbour; these
  # tests pin generation and conservation, so the room cap is switched off.
  whep::build_human_n(
    population_basis = "urban",
    method_local_residual = "uncapped",
    data = list(
      urban_population = urban_population,
      cell_polity = cell_polity,
      cropland_ha = .human_c0_cropland()
    )
  )
}

# ---- polity_validity (#675) -------------------------------------------

# Area 277 (South Sudan) exists only from 2011: the 2000 row of this cell
# names a state that did not exist that year.
.human_out_of_span_data <- function() {
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
.human_c0_rate_2000 <- function() {
  ref <- whep::human_kgn_cap_reference
  ref$human_kgn_cap[ref$year == 2000L]
}

testthat::test_that("C0: human N is population x rate, conserved to the tonne", {
  out <- .human_c0_build()
  rate <- .human_c0_rate_2000()
  expected_t <- sum(.human_c0_population()$urban_pop) * rate / 1000

  testthat::expect_length(rate, 1L)
  # kg N per capita x people / 1000 = t N. Tolerance is DA-18's locked
  # 1e-9 relative bound; the measured gap on this fixture today is 0.
  testthat::expect_equal(sum(out$human_n_t), expected_t, tolerance = 1e-9)
  # And the split across the shared cell's three polities is polity_frac
  # exactly, with no area weighting.
  shared <- out[out$lon == -0.25, , drop = FALSE]
  testthat::expect_equal(
    shared$human_n_t[match(c(203L, 68L, 231L), shared$area_code)],
    1e6 * rate / 1000 * c(0.5, 0.3, 0.2),
    tolerance = 1e-9
  )
})

testthat::test_that("C0: the human-N generation step partitions population", {
  # The test above pins the whole pipeline, in which
  # allocate_manure_transport() also conserves mass; this one isolates the
  # generation step at R/n_human.R:92-106 so a compensating change in the
  # two halves cannot pass unnoticed. The step is two private helpers since
  # the population basis became selectable: `.human_polycell_population()`
  # splits each cell's urban count by polity_frac, `.human_n_generated()`
  # applies the rate. `:::` is the only access -- the same route
  # test_feed_lpjml.R uses for `.lpjml_grass_to_dm`.
  generated <- whep:::.human_polycell_population(
    "urban",
    list(urban_population = .human_c0_population()),
    .human_c0_cell_polity(),
    NULL
  ) |>
    whep:::.human_n_generated("urban")
  rate <- .human_c0_rate_2000()

  # Population is partitioned, not duplicated and not shed: the polycells'
  # polity_frac-weighted population recovers the input head count.
  testthat::expect_equal(
    sum(generated$population),
    sum(.human_c0_population()$urban_pop),
    tolerance = 1e-9
  )
  testthat::expect_equal(
    sum(generated$human_n_generated_t),
    sum(.human_c0_population()$urban_pop) * rate / 1000,
    tolerance = 1e-9
  )
  # Four rows out of two population cells: the shared cell fans out to
  # three polities, so a row count is NOT a conservation check here.
  testthat::expect_equal(nrow(generated), 4L)
})

testthat::test_that("C0: no area column reaches human N", {
  base <- .human_c0_build()
  # Hand the crosswalk both area columns the migration will introduce.
  # Today they are ignored completely, because R/n_human.R never reads an
  # area. THIS IS THE GUARD against wiring an area denominator into a
  # population partition, which would inflate urban N by ~1e5.
  with_areas <- .human_c0_build(
    cell_polity = dplyr::mutate(
      .human_c0_cell_polity(),
      cell_area_ha = 308000,
      land_area_ha = 270000
    )
  )

  testthat::expect_identical(with_areas, base)
})

testthat::test_that("C0: population outside the crosswalk is dropped silently", {
  # R/n_human.R:99 joins the crosswalk with dplyr::inner_join(), so urban
  # population in a cell the crosswalk does not carry contributes nothing
  # and emits no warning. Today the crosswalk misses 1,294 LUH2
  # terrestrial cells, so this path is live. Pinned as current behaviour;
  # it is not asserted to be right.
  extra <- dplyr::bind_rows(
    .human_c0_population(),
    tibble::tibble(lon = 9.75, lat = 9.75, year = 2000L, urban_pop = 5e5)
  )
  out <- testthat::expect_no_warning(.human_c0_build(urban_population = extra))

  # Half a million people vanish without trace: the total is unchanged.
  testthat::expect_equal(
    sum(out$human_n_t),
    sum(.human_c0_population()$urban_pop) * .human_c0_rate_2000() / 1000,
    tolerance = 1e-9
  )
  testthat::expect_false(any(out$lon == 9.75))
})

testthat::test_that("build_human_n names an anachronistic polity", {
  testthat::expect_warning(
    out <- whep::build_human_n(
      population_basis = "urban",
      data = .human_out_of_span_data()
    ),
    "did not exist in that row's year"
  )

  testthat::expect_equal(nrow(out), 2L)
  testthat::expect_equal(
    out$reporting_polity_code[out$year == 2000L],
    "SSD-2011-2025"
  )
})

testthat::test_that("build_human_n honours drop and flag", {
  testthat::expect_warning(
    dropped <- whep::build_human_n(
      population_basis = "urban",
      data = .human_out_of_span_data(),
      polity_validity = "drop"
    )
  )
  testthat::expect_warning(
    flagged <- whep::build_human_n(
      population_basis = "urban",
      data = .human_out_of_span_data(),
      polity_validity = "flag"
    )
  )

  testthat::expect_equal(dropped$year, 2020L)
  testthat::expect_equal(
    flagged$reporting_polity_out_of_span,
    flagged$year == 2000L
  )
})

# ---- area_code is required, and checked at the input boundary (#597) ----
#
# `build_human_n()` builds the transport allocator's `territory` key itself,
# from `data$cell_polity$area_code` and `data$cropland_ha$area_code`
# (`.human_source_cells()` / `.human_sink_cells()`), and used to resolve that
# key back to a numeric `area_code` only AFTER transport, in
# `.human_finalise()`, through the manure chain's ISO3 bridge. Two things were
# wrong and neither is visible to a column-set census or an `area_code`
# census, because the output schema and the output codes are identical either
# way and only the CELL the nitrogen lands on moves: the check ran after the
# partition it keys, and an ISO3 was accepted at all.

testthat::test_that("build_human_n refuses a non-numeric area_code, naming the frame", {
  urban_population <- tibble::tribble(
    ~lon, ~lat, ~year, ~urban_pop,
    -0.25, -0.25, 2000L, 100
  )
  cropland_ha <- tibble::tribble(
    ~lon, ~lat, ~area_code, ~year, ~cropland_ha,
    -0.25, -0.25, 203L, 2000L, 1000
  )
  build <- function(cell_polity, cropland) {
    whep::build_human_n(
      population_basis = "urban",
      data = list(
        urban_population = urban_population,
        cell_polity = cell_polity,
        cropland_ha = cropland
      )
    )
  }

  # An ISO3 is refused rather than bridged: it resolves to a
  # polity_area_code aggregation bucket, so "SSD" would silently become 206,
  # Sudan (former). Resolved inside a `dplyr::mutate()` after transport, the
  # old abort reached the caller as a `dplyr` mutate error naming
  # `territory`, a field no caller ever supplies.
  cnd <- testthat::expect_error(
    build(
      tibble::tribble(~lon, ~lat, ~area_code, -0.25, -0.25, "ESP"),
      cropland_ha
    ),
    class = "whep_human_n_area_code_unresolved"
  )
  testthat::expect_match(conditionMessage(cnd), "cell_polity")
  testthat::expect_match(conditionMessage(cnd), "ESP")

  cnd <- testthat::expect_error(
    build(
      .example_cell_polity_human(),
      dplyr::mutate(cropland_ha, area_code = "ESP")
    ),
    class = "whep_human_n_area_code_unresolved"
  )
  testthat::expect_match(conditionMessage(cnd), "cropland_ha")

  # A stringified code is still a string: the column must carry the code, not
  # a spelling of it, or the two frames can disagree about the vocabulary
  # while both look resolvable.
  testthat::expect_error(
    build(
      tibble::tribble(~lon, ~lat, ~area_code, -0.25, -0.25, "203"),
      cropland_ha
    ),
    class = "whep_human_n_area_code_unresolved"
  )
  # And an area name never was resolvable, before or after.
  testthat::expect_error(
    build(
      tibble::tribble(~lon, ~lat, ~area_code, -0.25, -0.25, "Spain"),
      cropland_ha
    ),
    class = "whep_human_n_area_code_unresolved"
  )
})

testthat::test_that("build_human_n refuses a mixed-vocabulary pair instead of stranding its load", {
  # THE ORDERING BUG, kept as a test with its expectation changed from "both
  # resolve and share a territory" to "aborts". Spain is written numerically
  # in `cell_polity` and as an ISO3 in `cropland_ha` -- one polity, two
  # vocabularies. The source cell has urban population and NO cropland room;
  # its same-polity neighbour has ample room, so the whole load must reach
  # the neighbour. Checked only after transport, the two frames' `territory`
  # keys never met, the allocator saw a source with no reachable sink, and
  # the load stranded on the room-less cell (-0.25) while still being
  # relabelled 203 in the output -- silently, with no warning at all, because
  # the ISO3 never reached the resolver. It is now refused up front.
  urban_population <- tibble::tribble(
    ~lon, ~lat, ~year, ~urban_pop,
    -0.25, -0.25, 2000L, 100,
    0.25, -0.25, 2000L, 0
  )
  cropland_iso3 <- tibble::tribble(
    ~lon, ~lat, ~area_code, ~year, ~cropland_ha,
    -0.25, -0.25, "ESP", 2000L, 0,
    0.25, -0.25, "ESP", 2000L, 1000
  )
  testthat::expect_error(
    whep::build_human_n(
      population_basis = "urban",
      data = list(
        urban_population = urban_population,
        cell_polity = .example_cell_polity_human(),
        cropland_ha = cropland_iso3
      )
    ),
    class = "whep_human_n_area_code_unresolved"
  )

  # The same scenario in one vocabulary still places the load by room, on the
  # neighbour, and conserves it: the refusal above is about the key, not
  # about transport.
  out <- whep::build_human_n(
    population_basis = "urban",
    data = list(
      urban_population = urban_population,
      cell_polity = .example_cell_polity_human(),
      cropland_ha = dplyr::mutate(cropland_iso3, area_code = 203L)
    )
  )
  placed <- out[out$human_n_t > 1e-12, , drop = FALSE]
  testthat::expect_equal(placed$lon, 0.25)
  pointblank::expect_col_vals_equal(out, "area_code", 203L)
  testthat::expect_equal(
    sum(out$human_n_t),
    100 * .human_c0_rate_2000() / 1000,
    tolerance = 1e-9
  )
})

testthat::test_that("build_human_n accepts integer and double area_code alike", {
  urban_population <- tibble::tribble(
    ~lon, ~lat, ~year, ~urban_pop,
    -0.25, -0.25, 2000L, 1e6
  )
  build <- function(code) {
    whep::build_human_n(
      population_basis = "urban",
      method_local_residual = "uncapped",
      data = list(
        urban_population = urban_population,
        cell_polity = tibble::tibble(
          lon = -0.25,
          lat = -0.25,
          area_code = code
        ),
        cropland_ha = tibble::tibble(
          lon = -0.25,
          lat = -0.25,
          area_code = code,
          year = 2000L,
          cropland_ha = 1000
        )
      )
    )
  }
  # `build_cell_polity()` is integer-keyed, but a caller assembling the frame
  # by hand or through a join easily ends up with a double. Both must give
  # the same answer, and both must come back as integer.
  as_int <- build(203L)
  as_dbl <- build(203)
  testthat::expect_identical(as_int$area_code, 203L)
  testthat::expect_identical(as_dbl$area_code, 203L)
  testthat::expect_equal(as_int$human_n_t, as_dbl$human_n_t)
})

testthat::test_that("build_human_n refuses a fractional area_code rather than truncating", {
  # as.integer() would turn 203.7 into 203 and name Spain, so a share or a
  # fraction landing in the code column has to fail, not be truncated into
  # some other territory's code.
  cnd <- testthat::expect_error(
    whep::build_human_n(
      population_basis = "urban",
      data = list(
        urban_population = tibble::tribble(
          ~lon, ~lat, ~year, ~urban_pop,
          -0.25, -0.25, 2000L, 100
        ),
        cell_polity = tibble::tibble(
          lon = -0.25,
          lat = -0.25,
          area_code = 203.7
        ),
        cropland_ha = tibble::tibble(
          lon = -0.25,
          lat = -0.25,
          area_code = 203.7,
          year = 2000L,
          cropland_ha = 1000
        )
      )
    ),
    class = "whep_human_n_area_code_unresolved"
  )
  testthat::expect_match(conditionMessage(cnd), "whole number")
})

testthat::test_that("the human-N area_code check is the identity on the numeric vocabulary", {
  # The invariant that makes the published gridded run provably unaffected:
  # `build_cell_polity()` emits integer `area_code`s, and every code the
  # shipped region table knows must survive the boundary check unchanged. A
  # check that folded or truncated a NUMERIC code would move published
  # nitrogen silently, so pin it on the whole vocabulary rather than on a
  # handful of countries.
  codes <- sort(unique(stats::na.omit(as.integer(whep::regions_full$code))))
  testthat::expect_gt(length(codes), 200)
  resolved <- whep:::.human_resolve_area_code(
    tibble::tibble(area_code = codes),
    "cell_polity"
  )
  testthat::expect_identical(resolved$area_code, codes)

  # A zero-row frame (a year filter that keeps nothing) and an NA area_code
  # (a cell the crosswalk resolves to no reporting area, which the package
  # keeps rather than drops) must both survive rather than abort.
  empty <- whep:::.human_resolve_area_code(
    tibble::tibble(area_code = integer(0)),
    "cropland_ha"
  )
  testthat::expect_identical(empty$area_code, integer(0))
  with_na <- whep:::.human_resolve_area_code(
    tibble::tibble(area_code = c(203L, NA_integer_)),
    "cell_polity"
  )
  testthat::expect_identical(with_na$area_code, c(203L, NA_integer_))
})

# ---- population_basis: "urban" vs "total" -----------------------------------

# Two cells of one polity and plenty of cropland room on each, so transport
# moves nothing and the output is exactly the generated load.
.human_basis_polity <- function() {
  tibble::tribble(
    ~lon,  ~lat,  ~area_code,
    -3.75, 40.25, 203L,
    -0.75, 39.25, 203L
  )
}

.human_basis_cropland <- function(year) {
  tibble::tribble(
    ~lon,  ~lat,  ~area_code, ~cropland_ha,
    -3.75, 40.25, 203L,       1e9,
    -0.75, 39.25, 203L,       1e9
  ) |>
    dplyr::mutate(year = year)
}

.human_basis_run <- function(basis, year, population) {
  slot <- if (basis == "urban") "urban_population" else "total_population"
  data <- list(
    cell_polity = .human_basis_polity(),
    cropland_ha = .human_basis_cropland(year)
  )
  data[[slot]] <- population
  whep::build_human_n(population_basis = basis, data = data)
}

testthat::test_that("each basis regenerates its calibration total", {
  # The coefficient rebasing, end to end: in a benchmark year, the
  # calibration population on each basis times the rate on the same basis
  # returns human_n_reference exactly. The urban denominator is recovered from
  # the shipped rate; the total one is the WPP total the shipped table
  # records.
  year <- 2016L
  reference <- whep::human_n_reference
  target_t <- reference$human_n_gg[reference$year == year] * 1000
  urban_ref <- whep::human_kgn_cap_reference
  total_ref <- whep::human_kgn_cap_total_reference
  calibration_urban <- target_t *
    1000 /
    urban_ref$human_kgn_cap[urban_ref$year == year]
  calibration_total <- total_ref$calibration_population[
    total_ref$year == year
  ]
  urban <- .human_basis_run(
    "urban",
    year,
    tibble::tibble(
      lon = c(-3.75, -0.75),
      lat = c(40.25, 39.25),
      year = year,
      urban_pop = calibration_urban * c(0.7, 0.3)
    )
  )
  total <- .human_basis_run(
    "total",
    year,
    tibble::tibble(
      lon = c(-3.75, -0.75),
      lat = c(40.25, 39.25),
      area_code = 203L,
      year = year,
      population = calibration_total * c(0.7, 0.3)
    )
  )
  testthat::expect_equal(sum(urban$human_n_t), target_t, tolerance = 1e-12)
  testthat::expect_equal(sum(total$human_n_t), target_t, tolerance = 1e-12)
  # Not the same population: the total basis carries more people at a lower
  # rate, which is the whole point of rebasing.
  testthat::expect_gt(calibration_total, calibration_urban)
})

testthat::test_that("the total basis applies the per-total-inhabitant rate", {
  # 2012 lies between the 2008 and 2016 benchmarks, so the rate is
  # interpolated; it must be the TOTAL table's interpolation, never the
  # urban one.
  total_ref <- whep::human_kgn_cap_total_reference
  urban_ref <- whep::human_kgn_cap_reference
  rate <- stats::approx(total_ref$year, total_ref$human_kgn_cap, 2012)$y
  urban_rate <- stats::approx(urban_ref$year, urban_ref$human_kgn_cap, 2012)$y
  out <- .human_basis_run(
    "total",
    2012L,
    tibble::tibble(
      lon = -3.75,
      lat = 40.25,
      area_code = 203L,
      year = 2012L,
      population = 1e6
    )
  )
  testthat::expect_equal(sum(out$human_n_t), 1e6 * rate / 1000)
  testthat::expect_false(isTRUE(all.equal(rate, urban_rate)))
})

testthat::test_that("the basis is stamped on every row, both halves together", {
  urban_pop <- tibble::tibble(
    lon = -3.75,
    lat = 40.25,
    year = 2016L,
    urban_pop = 1e6
  )
  urban <- .human_basis_run("urban", 2016L, urban_pop)
  total <- .human_basis_run(
    "total",
    2016L,
    tibble::tibble(
      lon = -3.75,
      lat = 40.25,
      area_code = 203L,
      year = 2016L,
      population = 1e6
    )
  )
  pointblank::expect_col_vals_in_set(
    urban,
    "method_human_population",
    "urban_population"
  )
  pointblank::expect_col_vals_in_set(
    urban,
    "method_human_kgn_cap",
    "kg_n_per_urban_inhabitant"
  )
  pointblank::expect_col_vals_in_set(
    total,
    "method_human_population",
    "total_population"
  )
  pointblank::expect_col_vals_in_set(
    total,
    "method_human_kgn_cap",
    "kg_n_per_total_inhabitant"
  )
})

testthat::test_that("the default basis is the total population", {
  # The term is nitrogen from the whole human population, so an unset
  # `population_basis` reads the total population with the per-inhabitant
  # rate. This replaced the urban default, so a caller that still hands in
  # only an urban population without naming its basis is refused rather than
  # silently rated per inhabitant.
  total_pop <- tibble::tibble(
    lon = -3.75,
    lat = 40.25,
    area_code = 203L,
    year = 2016L,
    population = 1e6
  )
  default <- whep::build_human_n(
    data = list(
      total_population = total_pop,
      cell_polity = .human_basis_polity(),
      cropland_ha = .human_basis_cropland(2016L)
    )
  )
  total <- .human_basis_run("total", 2016L, total_pop)
  testthat::expect_identical(default, total)
  testthat::expect_equal(
    unique(default$method_human_population),
    "total_population"
  )
  testthat::expect_error(
    whep::build_human_n(
      data = list(
        urban_population = tibble::tibble(
          lon = -3.75,
          lat = 40.25,
          year = 2016L,
          urban_pop = 1e6
        ),
        cell_polity = .human_basis_polity(),
        cropland_ha = .human_basis_cropland(2016L)
      )
    ),
    class = "whep_human_n_population_basis_mismatch"
  )
})

testthat::test_that("a population on the other basis is refused, not re-read", {
  urban_population <- tibble::tibble(
    lon = -3.75,
    lat = 40.25,
    year = 2016L,
    urban_pop = 1e6
  )
  total_population <- tibble::tibble(
    lon = -3.75,
    lat = 40.25,
    area_code = 203L,
    year = 2016L,
    population = 1e6
  )
  common <- list(
    cell_polity = .human_basis_polity(),
    cropland_ha = .human_basis_cropland(2016L)
  )
  testthat::expect_error(
    whep::build_human_n(
      years = 2016L,
      population_basis = "total",
      data = c(common, list(urban_population = urban_population))
    ),
    class = "whep_human_n_population_basis_mismatch"
  )
  testthat::expect_error(
    whep::build_human_n(
      years = 2016L,
      population_basis = "urban",
      data = c(common, list(total_population = total_population))
    ),
    class = "whep_human_n_population_basis_mismatch"
  )
})

testthat::test_that("the total basis takes polycells as they are", {
  # A border cell's two polycells were each levelled to their own country's
  # WPP total by build_total_population_grid(); re-splitting their sum by
  # polity_frac would hand each country some of the other's level.
  cell_polity <- tibble::tribble(
    ~lon,  ~lat,  ~area_code, ~polity_frac,
    -0.25, 42.75, 203L,       0.5,
    -0.25, 42.75, 68L,        0.5
  )
  cropland <- tibble::tribble(
    ~lon,  ~lat,  ~area_code, ~year, ~cropland_ha,
    -0.25, 42.75, 203L,       2016L, 1e9,
    -0.25, 42.75, 68L,        2016L, 1e9
  )
  population <- tibble::tribble(
    ~lon,  ~lat,  ~area_code, ~year, ~population,
    -0.25, 42.75, 203L,       2016L, 9e5,
    -0.25, 42.75, 68L,        2016L, 1e5
  )
  out <- whep::build_human_n(
    population_basis = "total",
    data = list(
      total_population = population,
      cell_polity = cell_polity,
      cropland_ha = cropland
    )
  )
  total_ref <- whep::human_kgn_cap_total_reference
  rate <- total_ref$human_kgn_cap[total_ref$year == 2016L]
  testthat::expect_equal(
    out$human_n_t[match(c(203L, 68L), out$area_code)],
    c(9e5, 1e5) * rate / 1000
  )
})

testthat::test_that("a total population keyed off the crosswalk is refused", {
  population <- tibble::tibble(
    lon = -3.75,
    lat = 40.25,
    area_code = 999L,
    year = 2016L,
    population = 1e6
  )
  testthat::expect_error(
    .human_basis_run("total", 2016L, population),
    class = "whep_human_n_area_code_unresolved"
  )
})

testthat::test_that("the total basis builds its population when none is given", {
  # With no data$total_population, the population comes from
  # build_total_population_grid() on the SAME crosswalk, so the two cannot be
  # keyed apart.
  called <- FALSE
  testthat::local_mocked_bindings(
    build_total_population_grid = function(years, data, ...) {
      called <<- TRUE
      testthat::expect_identical(years, 2016L)
      testthat::expect_true(rlang::has_name(data$cell_polity, "area_code"))
      tibble::tibble(
        lon = -3.75,
        lat = 40.25,
        area_code = 203L,
        year = 2016L,
        population = 1e6
      )
    }
  )
  out <- whep::build_human_n(
    years = 2016L,
    population_basis = "total",
    data = list(
      cell_polity = .human_basis_polity(),
      cropland_ha = .human_basis_cropland(2016L)
    )
  )
  testthat::expect_true(called)
  testthat::expect_gt(sum(out$human_n_t), 0)
})

testthat::test_that("the example fixture carries the basis it was asked for", {
  total <- whep::build_human_n(population_basis = "total", example = TRUE)
  testthat::expect_equal(total$method_human_population, "total_population")
  testthat::expect_equal(
    total$method_human_kgn_cap,
    "kg_n_per_total_inhabitant"
  )
})

# ---- the former "urban" names, kept for one release --------------------------

testthat::test_that("build_urban_n() forwards to build_human_n() and warns", {
  # With no population_basis, build_urban_n() defaults to "urban" (its
  # historical basis), not build_human_n()'s own "total" default -- so old
  # calls keep old behaviour. A data list carrying only total_population
  # would abort under that default (whep_human_n_population_basis_mismatch),
  # which is itself evidence the default really changed.
  data <- list(
    urban_population = tibble::tribble(
      ~lon, ~lat, ~year, ~urban_pop,
      -3.75, 40.25, 2016L, 1e6
    ),
    cell_polity = .human_basis_polity(),
    cropland_ha = .human_basis_cropland(2016L)
  )
  cnd <- testthat::expect_warning(
    old <- whep::build_urban_n(data = data),
    class = "whep_build_urban_n_deprecated"
  )
  testthat::expect_s3_class(cnd, "lifecycle_warning_deprecated")
  testthat::expect_identical(
    old,
    whep::build_human_n(population_basis = "urban", data = data)
  )
})

testthat::test_that("an area_code refusal still carries its former class", {
  cnd <- testthat::expect_error(
    whep:::.human_resolve_area_code(
      tibble::tibble(area_code = "ESP"),
      "cell_polity"
    ),
    class = "whep_human_n_area_code_unresolved"
  )
  testthat::expect_s3_class(cnd, "whep_urban_area_code_unresolved")
})

testthat::test_that("the former fert_type keys are read as the renamed term", {
  testthat::expect_identical(
    whep:::.human_legacy_fert_type(c("bnf", "human", "Synthetic")),
    c("bnf", "human", "Synthetic")
  )
  testthat::expect_warning(
    renamed <- whep:::.human_legacy_fert_type(c("urban", "bnf", "Urban")),
    class = "whep_urban_fert_type_deprecated"
  )
  testthat::expect_identical(renamed, c("human", "bnf", "Human"))
})

testthat::test_that("an n_inputs table from before the rename is upgraded", {
  old <- tibble::tibble(
    fert_type = c("urban", "bnf"),
    n_input_t = c(1, 2),
    method_urban_population = c("urban_population", NA),
    method_urban_kgn_cap = c("kg_n_per_urban_inhabitant", NA)
  )
  testthat::expect_warning(
    new <- whep:::.human_upgrade_legacy_inputs(old),
    class = "whep_urban_fert_type_deprecated"
  )
  testthat::expect_identical(new$fert_type, c("human", "bnf"))
  testthat::expect_identical(
    names(new),
    c(
      "fert_type",
      "n_input_t",
      "method_human_population",
      "method_human_kgn_cap"
    )
  )
  testthat::expect_identical(new$n_input_t, old$n_input_t)
  # Both spellings at once cannot be reconciled, so it is refused.
  testthat::expect_error(
    whep:::.human_upgrade_legacy_inputs(
      dplyr::mutate(new, method_urban_population = "urban_population")
    ),
    "former name"
  )
})

testthat::test_that("a main-built n_inputs table gets the stamp columns it never had", {
  # main (5421973b) never recorded a population-basis stamp at all -- it had
  # only one basis -- so its n_inputs table carries no method_urban_* /
  # method_human_* columns whatsoever, unlike the fixture above (which
  # already has method_urban_population/method_urban_kgn_cap columns, just
  # under the old name). Both must end up stamped urban_population /
  # kg_n_per_urban_inhabitant, the only basis main ever produced.
  old <- tibble::tibble(
    fert_type = c("urban", "bnf"),
    n_input_t = c(1, 2)
  )
  testthat::expect_warning(
    new <- whep:::.human_upgrade_legacy_inputs(old),
    class = "whep_urban_fert_type_deprecated"
  )
  testthat::expect_identical(new$fert_type, c("human", "bnf"))
  testthat::expect_identical(
    new$method_human_population,
    c("urban_population", NA_character_)
  )
  testthat::expect_identical(
    new$method_human_kgn_cap,
    c("kg_n_per_urban_inhabitant", NA_character_)
  )
  # A table that already carries a stamp (built after this branch) is left
  # untouched, whatever basis it names.
  stamped <- tibble::tibble(
    fert_type = "human",
    n_input_t = 1,
    method_human_population = "total_population",
    method_human_kgn_cap = "kg_n_per_total_inhabitant"
  )
  testthat::expect_identical(
    whep:::.human_upgrade_legacy_inputs(stamped),
    stamped
  )
})

# ---- human N the transport step cannot deliver (#1171) -----------------
#
# allocate_manure_transport() hands back, at the SOURCE cell, whatever it could
# not send to a ring-1 neighbour. On a source cell with no cropland there is no
# land to put it on, and until #1171 build_human_n() returned it there with
# nothing marking it -- on the 2010 global grid 1,985 cells and 38,425 t N,
# which build_n_inputs() then could not place.
#
# Polity 203: source A at lon 0.25 has people and no cropland; no ring-1
# neighbour has cropland either; cropland sits at ring 2 (lon 1.25, 1000 ha
# and lon -0.75, 3000 ha) and ring 5 (lon 2.75, 4000 ha). Polity 68: source D
# has people and its polity has no cropland anywhere.
.human_undelivered_data <- function() {
  list(
    urban_population = tibble::tribble(
      ~lon,  ~lat,  ~year, ~urban_pop,
      0.25,  -0.25, 2000L, 1000,
      10.25, -0.25, 2000L, 500
    ),
    cell_polity = tibble::tribble(
      ~lon,  ~lat,  ~area_code,
      0.25,  -0.25, 203L,
      1.25,  -0.25, 203L,
      -0.75, -0.25, 203L,
      2.75,  -0.25, 203L,
      10.25, -0.25, 68L
    ),
    cropland_ha = tibble::tribble(
      ~lon,  ~lat,  ~area_code, ~year, ~cropland_ha,
      0.25,  -0.25, 203L,       2000L, 0,
      1.25,  -0.25, 203L,       2000L, 1000,
      -0.75, -0.25, 203L,       2000L, 3000,
      2.75,  -0.25, 203L,       2000L, 4000,
      10.25, -0.25, 68L,        2000L, 0
    )
  )
}

.human_undelivered_load <- function(pop) {
  pop * .human_c0_rate_2000() / 1000
}

.human_n_at <- function(out, lon, col = "human_n_t") {
  sum(out[[col]][out$lon %in% lon])
}

testthat::test_that("undelivered human N is reported, never returned silently", {
  cnd <- testthat::expect_warning(
    out <- whep::build_human_n(
      population_basis = "urban",
      data = .human_undelivered_data()
    ),
    class = "whep_human_n_undelivered"
  )
  testthat::expect_match(conditionMessage(cnd), "2 human-N source cell-years")

  summary <- attr(out, "human_n_undelivered")
  pointblank::expect_col_exists(
    summary,
    c(
      "year",
      "n_cells",
      "undelivered_t",
      "relocated_t",
      "stranded_t",
      "dropped_t",
      "undelivered_share",
      "method_human_residual"
    )
  )
  testthat::expect_equal(summary$n_cells, 2L)
  testthat::expect_equal(
    summary$undelivered_t,
    .human_undelivered_load(1500),
    tolerance = 1e-9
  )
  testthat::expect_equal(summary$undelivered_share, 1, tolerance = 1e-9)
  # The default is "nearest": A's load is relocated, D's cannot be (its polity
  # has no cropland) and stays, flagged, on its own cell.
  testthat::expect_equal(
    summary$relocated_t,
    .human_undelivered_load(1000),
    tolerance = 1e-9
  )
  testthat::expect_equal(
    summary$stranded_t,
    .human_undelivered_load(500),
    tolerance = 1e-9
  )
  testthat::expect_equal(summary$dropped_t, 0)
  pointblank::expect_col_vals_in_set(out, "method_human_residual", "nearest")
})

testthat::test_that("nearest moves undelivered N to the closest ring with cropland", {
  out <- suppressWarnings(
    whep::build_human_n(
      population_basis = "urban",
      data = .human_undelivered_data()
    )
  )
  load_a <- .human_undelivered_load(1000)

  # Nothing left on A; nothing reaches the ring-5 cell; the two ring-2 cells
  # split it by cropland room, 1000 : 3000.
  testthat::expect_equal(.human_n_at(out, 0.25), 0)
  testthat::expect_equal(.human_n_at(out, 2.75), 0)
  testthat::expect_equal(
    .human_n_at(out, 1.25),
    0.25 * load_a,
    tolerance = 1e-9
  )
  testthat::expect_equal(
    .human_n_at(out, -0.75),
    0.75 * load_a,
    tolerance = 1e-9
  )
  testthat::expect_equal(
    .human_n_at(out, -0.75, "human_n_relocated_t"),
    0.75 * load_a,
    tolerance = 1e-9
  )
  # D stays, and says so.
  testthat::expect_equal(
    .human_n_at(out, 10.25, "human_n_stranded_t"),
    .human_undelivered_load(500),
    tolerance = 1e-9
  )
  # Mass is conserved.
  testthat::expect_equal(
    sum(out$human_n_t),
    .human_undelivered_load(1500),
    tolerance = 1e-9
  )
})

testthat::test_that("polity spreads undelivered N over the polity's cropland", {
  out <- suppressWarnings(
    whep::build_human_n(
      population_basis = "urban",
      data = .human_undelivered_data(),
      method_residual = "polity"
    )
  )
  load_a <- .human_undelivered_load(1000)

  # 1000 : 3000 : 4000 ha, distance ignored.
  testthat::expect_equal(
    .human_n_at(out, c(1.25, -0.75, 2.75)),
    load_a,
    tolerance = 1e-9
  )
  testthat::expect_equal(
    .human_n_at(out, 2.75),
    0.5 * load_a,
    tolerance = 1e-9
  )
  testthat::expect_equal(
    .human_n_at(out, 1.25),
    0.125 * load_a,
    tolerance = 1e-9
  )
  testthat::expect_equal(.human_n_at(out, 0.25), 0)
  pointblank::expect_col_vals_in_set(out, "method_human_residual", "polity")
})

testthat::test_that("keep leaves undelivered N in place, flagged", {
  out <- suppressWarnings(
    whep::build_human_n(
      population_basis = "urban",
      data = .human_undelivered_data(),
      method_residual = "keep"
    )
  )
  testthat::expect_equal(
    .human_n_at(out, 0.25, "human_n_stranded_t"),
    .human_undelivered_load(1000),
    tolerance = 1e-9
  )
  testthat::expect_equal(sum(out$human_n_relocated_t), 0)
  testthat::expect_equal(
    sum(out$human_n_stranded_t),
    .human_undelivered_load(1500),
    tolerance = 1e-9
  )
  testthat::expect_equal(
    attr(out, "human_n_undelivered")$stranded_t,
    .human_undelivered_load(1500),
    tolerance = 1e-9
  )
})

testthat::test_that("drop discards undelivered N and records what it cost", {
  testthat::expect_warning(
    out <- whep::build_human_n(
      population_basis = "urban",
      data = .human_undelivered_data(),
      method_residual = "drop"
    ),
    class = "whep_human_n_undelivered"
  )
  testthat::expect_equal(nrow(out), 0L)
  testthat::expect_equal(
    attr(out, "human_n_undelivered")$dropped_t,
    .human_undelivered_load(1500),
    tolerance = 1e-9
  )
})

testthat::test_that("a fully relocated residual is a message, not a warning", {
  data <- .human_undelivered_data()
  data$urban_population <- data$urban_population[1, ]
  testthat::expect_no_warning(
    testthat::expect_message(
      out <- whep::build_human_n(population_basis = "urban", data = data),
      class = "whep_human_n_undelivered"
    )
  )
  testthat::expect_equal(sum(out$human_n_stranded_t), 0)
})

testthat::test_that("residual on a cell with cropland is not undelivered", {
  # One cell, cropland, no neighbour: the whole load is residual but lands on
  # land it can be applied to, so there is nothing to report or relocate.
  data <- list(
    urban_population = tibble::tribble(
      ~lon,  ~lat,  ~year, ~urban_pop,
      -0.25, -0.25, 2000L, 1000
    ),
    cell_polity = .example_cell_polity_human(),
    cropland_ha = tibble::tribble(
      ~lon,  ~lat,  ~area_code, ~year, ~cropland_ha,
      -0.25, -0.25, 203L,       2000L, 1000
    )
  )
  out <- testthat::expect_no_message(whep::build_human_n(
    population_basis = "urban",
    data = data
  ))
  testthat::expect_equal(out$human_n_relocated_t, 0)
  testthat::expect_equal(out$human_n_stranded_t, 0)
  testthat::expect_equal(attr(out, "human_n_undelivered")$n_cells, 0L)
})

testthat::test_that("the example carries the undelivered-N columns", {
  out <- whep::build_human_n(example = TRUE, method_residual = "polity")
  pointblank::expect_col_exists(
    out,
    c("human_n_relocated_t", "human_n_stranded_t", "method_human_residual")
  )
  testthat::expect_equal(out$method_human_residual, "polity")
})

testthat::test_that("nearest fills the nearest ring to its room, then widens", {
  # A's load is sized to overflow the two ring-2 cells: their room is
  # 170 kg N/ha x 4000 ha = 680 t, so the rest goes on to the ring-5 cell,
  # never beyond any cell's room.
  data <- .human_undelivered_data()
  data$urban_population <- data$urban_population[1, ]
  load_a <- 1000
  data$urban_population$urban_pop <- load_a * 1000 / .human_c0_rate_2000()
  testthat::expect_message(
    out <- whep::build_human_n(population_basis = "urban", data = data),
    class = "whep_human_n_undelivered"
  )
  testthat::expect_equal(.human_n_at(out, 1.25), 170, tolerance = 1e-9)
  testthat::expect_equal(.human_n_at(out, -0.75), 510, tolerance = 1e-9)
  testthat::expect_equal(.human_n_at(out, 2.75), 320, tolerance = 1e-9)
  testthat::expect_equal(sum(out$human_n_t), load_a, tolerance = 1e-9)
})

testthat::test_that("nearest strands only what the polity has no room for", {
  # Total room in polity 203 is 170 kg N/ha x 8000 ha = 1360 t.
  data <- .human_undelivered_data()
  data$urban_population <- data$urban_population[1, ]
  data$urban_population$urban_pop <- 2000 * 1000 / .human_c0_rate_2000()
  testthat::expect_warning(
    out <- whep::build_human_n(population_basis = "urban", data = data),
    class = "whep_human_n_undelivered"
  )
  testthat::expect_equal(sum(out$human_n_relocated_t), 1360, tolerance = 1e-9)
  testthat::expect_equal(
    .human_n_at(out, 0.25, "human_n_stranded_t"),
    640,
    tolerance = 1e-9
  )
  testthat::expect_equal(sum(out$human_n_t), 2000, tolerance = 1e-9)
})

testthat::test_that("nearest counts room already taken by the transport step", {
  # B (lon 1.25) is a ring-1 neighbour of source E (lon 1.75, no cropland),
  # whose transported load fills 100 of B's 170 t. A's relocated N then sees
  # only the 70 t B has left, alongside the 510 t at lon -0.75.
  data <- .human_undelivered_data()
  rate <- .human_c0_rate_2000()
  data$urban_population <- tibble::tribble(
    ~lon, ~lat, ~year, ~urban_pop,
    0.25, -0.25, 2000L, 580 * 1000 / rate,
    1.75, -0.25, 2000L, 100 * 1000 / rate
  )
  data$cell_polity <- dplyr::bind_rows(
    data$cell_polity,
    tibble::tibble(lon = 1.75, lat = -0.25, area_code = 203L)
  )
  out <- suppressMessages(whep::build_human_n(
    population_basis = "urban",
    data = data
  ))
  testthat::expect_equal(.human_n_at(out, 1.25), 170, tolerance = 1e-9)
  testthat::expect_equal(
    .human_n_at(out, 1.25, "human_n_relocated_t"),
    70,
    tolerance = 1e-9
  )
  testthat::expect_equal(.human_n_at(out, -0.75), 510, tolerance = 1e-9)
  testthat::expect_equal(.human_n_at(out, 2.75), 0)
})

testthat::test_that("the ring distance wraps at the antimeridian", {
  testthat::expect_equal(
    whep:::.human_ring_distance(179.75, 65.25, -179.75, 65.25),
    1
  )
  testthat::expect_equal(
    whep:::.human_ring_distance(0.25, -0.25, 2.75, 0.75),
    5
  )
})

testthat::test_that("method_residual is validated", {
  testthat::expect_error(
    whep::build_human_n(
      population_basis = "urban",
      data = .human_undelivered_data(),
      method_residual = "farthest"
    ),
    class = "rlang_error"
  )
})

testthat::test_that("the default total basis reports undelivered N too", {
  data <- .human_undelivered_data()
  data$total_population <- data$urban_population |>
    dplyr::inner_join(data$cell_polity, by = c("lon", "lat")) |>
    dplyr::transmute(
      .data$lon,
      .data$lat,
      .data$area_code,
      .data$year,
      population = .data$urban_pop
    )
  data$urban_population <- NULL
  testthat::expect_warning(
    out <- whep::build_human_n(data = data),
    class = "whep_human_n_undelivered"
  )
  summary <- attr(out, "human_n_undelivered")
  testthat::expect_equal(summary$n_cells, 2L)
  testthat::expect_equal(
    summary$relocated_t + summary$stranded_t,
    summary$undelivered_t,
    tolerance = 1e-9
  )
  testthat::expect_equal(sum(out$human_n_t), summary$human_n_t)
})

testthat::test_that("keep is value-identical to the transport step alone", {
  out <- suppressWarnings(
    whep::build_human_n(
      population_basis = "urban",
      data = .human_undelivered_data(),
      method_residual = "keep"
    )
  )
  data <- .human_undelivered_data()
  flows <- whep::allocate_manure_transport(
    whep:::.human_source_cells(
      whep:::.human_n_generated(
        whep:::.human_polycell_population(
          "urban",
          data,
          data$cell_polity,
          NULL
        ),
        "urban"
      )
    ),
    whep:::.human_sink_cells(data$cropland_ha)
  )
  xy <- whep:::.parse_cell_id(flows$sub_territory)
  expected <- flows |>
    dplyr::mutate(lon = xy$lon, lat = xy$lat) |>
    dplyr::summarise(applied_n = sum(.data$applied_n), .by = c("lon", "lat"))
  joined <- dplyr::inner_join(out, expected, by = c("lon", "lat"))
  testthat::expect_equal(nrow(joined), nrow(expected))
  testthat::expect_identical(joined$human_n_t, joined$applied_n)
})

testthat::test_that("build_n_inputs forwards human_n_method_residual", {
  data <- .human_undelivered_data()
  data$human_n_population_basis <- "urban"
  data$human_n_method_residual <- "drop"
  testthat::expect_warning(
    whep:::.n_inputs_human(data),
    class = "whep_human_n_undelivered"
  )
  dropped <- suppressWarnings(whep:::.n_inputs_human(data))
  testthat::expect_equal(nrow(dropped), 0L)
  data$human_n_method_residual <- NULL
  nearest <- suppressWarnings(whep:::.n_inputs_human(data))
  testthat::expect_equal(
    sum(nearest$n_input_t),
    .human_undelivered_load(1500),
    tolerance = 1e-9
  )
})

# ---- residual beyond a cropland cell's own room (#1336) -------------------
#
# A source cell that HAS cropland used to keep its whole residual, with no room
# check, so a city cell with a sliver of cropland carried the city's load on
# that sliver. On the 2010 global grid (total basis) 1,327 such cells held
# 24,346 t N above their 170 kg N/ha room. Under the default
# `method_local_residual = "room_cap"` a cell keeps only what fits in the room
# the transport step left it; the rest is undelivered N for `method_residual`.
#
# Polity 203: source S (lon 0.25) has 1 ha of cropland (0.17 t of room) and no
# ring-1 neighbour with cropland, so its whole load is residual on itself;
# cropland with room sits at ring 2 (lon 1.25, 1000 ha; lon -0.75, 3000 ha).
.human_sliver_data <- function(load_t = 100, sliver_ha = 1) {
  data <- .human_undelivered_data()
  data$urban_population <- tibble::tibble(
    lon = 0.25,
    lat = -0.25,
    year = 2000L,
    urban_pop = load_t * 1000 / .human_c0_rate_2000()
  )
  data$cropland_ha$cropland_ha[data$cropland_ha$lon == 0.25] <- sliver_ha
  data
}

.human_ceiling_t <- function(cropland) {
  cropland |>
    dplyr::transmute(
      .data$lon,
      .data$lat,
      ceiling_t = 0.170 * .data$cropland_ha
    )
}

testthat::test_that("a sliver of cropland keeps only its own room", {
  testthat::expect_message(
    out <- whep::build_human_n(
      population_basis = "urban",
      data = .human_sliver_data()
    ),
    class = "whep_human_n_undelivered"
  )
  testthat::expect_equal(.human_n_at(out, 0.25), 0.17, tolerance = 1e-9)
  testthat::expect_equal(
    .human_n_at(out, 1.25, "human_n_relocated_t"),
    0.25 * 99.83,
    tolerance = 1e-9
  )
  testthat::expect_equal(
    .human_n_at(out, -0.75),
    0.75 * 99.83,
    tolerance = 1e-9
  )
  testthat::expect_equal(sum(out$human_n_t), 100, tolerance = 1e-9)
  pointblank::expect_col_vals_in_set(
    out,
    "method_human_local_residual",
    "room_cap"
  )
  summary <- attr(out, "human_n_undelivered")
  testthat::expect_equal(summary$n_cells, 1L)
  testthat::expect_equal(summary$undelivered_t, 99.83, tolerance = 1e-9)
  testthat::expect_equal(summary$over_room_t, 99.83, tolerance = 1e-9)
  testthat::expect_equal(summary$relocated_t, 99.83, tolerance = 1e-9)
})

testthat::test_that("no cropland cell ends above its room while the polity has room", {
  data <- .human_sliver_data(load_t = 500, sliver_ha = 0.001)
  out <- suppressMessages(
    whep::build_human_n(population_basis = "urban", data = data)
  )
  over <- out |>
    dplyr::inner_join(
      .human_ceiling_t(data$cropland_ha),
      by = c("lon", "lat")
    ) |>
    dplyr::filter(.data$human_n_t > .data$ceiling_t * (1 + 1e-9))
  testthat::expect_equal(nrow(over), 0L)
  testthat::expect_equal(sum(out$human_n_t), 500, tolerance = 1e-9)
})

testthat::test_that("uncapped keeps the whole residual on the sliver, as before", {
  data <- .human_sliver_data()
  out <- testthat::expect_no_message(
    whep::build_human_n(
      population_basis = "urban",
      data = data,
      method_local_residual = "uncapped"
    )
  )
  testthat::expect_equal(.human_n_at(out, 0.25), 100, tolerance = 1e-9)
  testthat::expect_equal(sum(out$human_n_relocated_t), 0)
  testthat::expect_equal(attr(out, "human_n_undelivered")$n_cells, 0L)
  testthat::expect_equal(attr(out, "human_n_undelivered")$over_room_t, 0)
  pointblank::expect_col_vals_in_set(
    out,
    "method_human_local_residual",
    "uncapped"
  )
})

testthat::test_that("the room cap counts N the transport step already landed", {
  # S has 100 ha (17 t of room). Source E, north of S with no cropland of its
  # own and no other cropland in its ring, sends all 10 t of its load to S, so
  # S's own 20 t residual finds 7 t of room: 13 t goes on to ring 2, 1 : 3.
  data <- .human_sliver_data(load_t = 20, sliver_ha = 100)
  rate <- .human_c0_rate_2000()
  data$urban_population <- dplyr::bind_rows(
    data$urban_population,
    tibble::tibble(
      lon = 0.25,
      lat = 0.25,
      year = 2000L,
      urban_pop = 10 * 1000 / rate
    )
  )
  data$cell_polity <- dplyr::bind_rows(
    data$cell_polity,
    tibble::tibble(lon = 0.25, lat = 0.25, area_code = 203L)
  )
  out <- suppressMessages(
    whep::build_human_n(population_basis = "urban", data = data)
  )
  s_n <- sum(out$human_n_t[out$lon == 0.25 & out$lat == -0.25])
  testthat::expect_equal(s_n, 17, tolerance = 1e-9)
  testthat::expect_equal(.human_n_at(out, 1.25), 3.25, tolerance = 1e-9)
  testthat::expect_equal(.human_n_at(out, -0.75), 9.75, tolerance = 1e-9)
  testthat::expect_equal(sum(out$human_n_t), 30, tolerance = 1e-9)
})

testthat::test_that("a polity with no room left strands the excess on its cell", {
  # Polity 68 has one cell, 1 ha of cropland and a 1 t load: 0.17 t fits and
  # 0.83 t has nowhere to go. It stays on the cell, flagged, with a warning.
  data <- list(
    urban_population = tibble::tibble(
      lon = 10.25,
      lat = -0.25,
      year = 2000L,
      urban_pop = 1000 / .human_c0_rate_2000()
    ),
    cell_polity = tibble::tibble(lon = 10.25, lat = -0.25, area_code = 68L),
    cropland_ha = tibble::tibble(
      lon = 10.25,
      lat = -0.25,
      area_code = 68L,
      year = 2000L,
      cropland_ha = 1
    )
  )
  testthat::expect_warning(
    out <- whep::build_human_n(population_basis = "urban", data = data),
    class = "whep_human_n_undelivered"
  )
  testthat::expect_equal(out$human_n_t, 1, tolerance = 1e-9)
  testthat::expect_equal(out$human_n_stranded_t, 0.83, tolerance = 1e-9)
  testthat::expect_equal(
    attr(out, "human_n_undelivered")$stranded_t,
    0.83,
    tolerance = 1e-9
  )
})

testthat::test_that("drop under the room cap discards only the excess", {
  out <- suppressWarnings(
    whep::build_human_n(
      population_basis = "urban",
      data = .human_sliver_data(),
      method_residual = "drop"
    )
  )
  testthat::expect_equal(sum(out$human_n_t), 0.17, tolerance = 1e-9)
  testthat::expect_equal(
    attr(out, "human_n_undelivered")$dropped_t,
    99.83,
    tolerance = 1e-9
  )
})

testthat::test_that("method_local_residual is validated", {
  testthat::expect_error(
    whep::build_human_n(
      population_basis = "urban",
      data = .human_sliver_data(),
      method_local_residual = "threshold"
    ),
    class = "rlang_error"
  )
})

testthat::test_that("build_n_inputs forwards human_n_method_local_residual", {
  data <- .human_sliver_data()
  data$human_n_population_basis <- "urban"
  data$human_n_method_local_residual <- "uncapped"
  uncapped <- whep:::.n_inputs_human(data)
  testthat::expect_equal(
    sum(uncapped$n_input_t[uncapped$lon == 0.25]),
    100,
    tolerance = 1e-9
  )
  data$human_n_method_local_residual <- NULL
  capped <- suppressMessages(whep:::.n_inputs_human(data))
  testthat::expect_equal(
    sum(capped$n_input_t[capped$lon == 0.25]),
    0.17,
    tolerance = 1e-9
  )
})
