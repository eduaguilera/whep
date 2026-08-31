# These fixtures carry numeric area codes (203 Spain, 68 France) where they
# used to carry ISO3 literals. build_urban_n() requires the numeric WHEP area
# code and refuses anything else (#597); when the literals were still bridged
# through .manure_territory_to_area_code() (#463) they resolved to these same
# codes, so every assertion below is unchanged.
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
  # The numeric WHEP area code the fixture keys cells by (203 Spain, 68
  # France) is what the output carries, because the reporting polity is
  # resolved from it.
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
# up. `build_urban_n()` now requires `area_code` to BE the numeric WHEP area
# code (#463/#512, tightened in #597), so the letters are replaced by the
# numeric codes the rest of this file already uses (203 Spain, 68 France,
# 231 USA). Only the LABELS change: every property, value and tolerance
# pinned below is unchanged, and was measured to reproduce exactly under both
# vocabularies before the substitution was made.
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

# ---- area_code is required, and checked at the input boundary (#597) ----
#
# `build_urban_n()` builds the transport allocator's `territory` key itself,
# from `data$cell_polity$area_code` and `data$cropland_ha$area_code`
# (`.urban_source_cells()` / `.urban_sink_cells()`), and used to resolve that
# key back to a numeric `area_code` only AFTER transport, in
# `.urban_finalise()`, through the manure chain's ISO3 bridge. Two things were
# wrong and neither is visible to a column-set census or an `area_code`
# census, because the output schema and the output codes are identical either
# way and only the CELL the nitrogen lands on moves: the check ran after the
# partition it keys, and an ISO3 was accepted at all.

testthat::test_that("build_urban_n refuses a non-numeric area_code, naming the frame", {
  urban_population <- tibble::tribble(
    ~lon, ~lat, ~year, ~urban_pop,
    -0.25, -0.25, 2000L, 100
  )
  cropland_ha <- tibble::tribble(
    ~lon, ~lat, ~area_code, ~year, ~cropland_ha,
    -0.25, -0.25, 203L, 2000L, 1000
  )
  build <- function(cell_polity, cropland) {
    whep::build_urban_n(
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
    class = "whep_urban_area_code_unresolved"
  )
  testthat::expect_match(conditionMessage(cnd), "cell_polity")
  testthat::expect_match(conditionMessage(cnd), "ESP")

  cnd <- testthat::expect_error(
    build(
      .example_cell_polity_urban(),
      dplyr::mutate(cropland_ha, area_code = "ESP")
    ),
    class = "whep_urban_area_code_unresolved"
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
    class = "whep_urban_area_code_unresolved"
  )
  # And an area name never was resolvable, before or after.
  testthat::expect_error(
    build(
      tibble::tribble(~lon, ~lat, ~area_code, -0.25, -0.25, "Spain"),
      cropland_ha
    ),
    class = "whep_urban_area_code_unresolved"
  )
})

testthat::test_that("build_urban_n refuses a mixed-vocabulary pair instead of stranding its load", {
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
    whep::build_urban_n(
      data = list(
        urban_population = urban_population,
        cell_polity = .example_cell_polity_urban(),
        cropland_ha = cropland_iso3
      )
    ),
    class = "whep_urban_area_code_unresolved"
  )

  # The same scenario in one vocabulary still places the load by room, on the
  # neighbour, and conserves it: the refusal above is about the key, not
  # about transport.
  out <- whep::build_urban_n(
    data = list(
      urban_population = urban_population,
      cell_polity = .example_cell_polity_urban(),
      cropland_ha = dplyr::mutate(cropland_iso3, area_code = 203L)
    )
  )
  placed <- out[out$urban_n_t > 1e-12, , drop = FALSE]
  testthat::expect_equal(placed$lon, 0.25)
  pointblank::expect_col_vals_equal(out, "area_code", 203L)
  testthat::expect_equal(
    sum(out$urban_n_t),
    100 * .urban_c0_rate_2000() / 1000,
    tolerance = 1e-9
  )
})

testthat::test_that("build_urban_n accepts integer and double area_code alike", {
  urban_population <- tibble::tribble(
    ~lon, ~lat, ~year, ~urban_pop,
    -0.25, -0.25, 2000L, 1e6
  )
  build <- function(code) {
    whep::build_urban_n(
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
  testthat::expect_equal(as_int$urban_n_t, as_dbl$urban_n_t)
})

testthat::test_that("build_urban_n refuses a fractional area_code rather than truncating", {
  # as.integer() would turn 203.7 into 203 and name Spain, so a share or a
  # fraction landing in the code column has to fail, not be truncated into
  # some other territory's code.
  cnd <- testthat::expect_error(
    whep::build_urban_n(
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
    class = "whep_urban_area_code_unresolved"
  )
  testthat::expect_match(conditionMessage(cnd), "whole number")
})

testthat::test_that("the urban area_code check is the identity on the numeric vocabulary", {
  # The invariant that makes the published gridded run provably unaffected:
  # `build_cell_polity()` emits integer `area_code`s, and every code the
  # shipped region table knows must survive the boundary check unchanged. A
  # check that folded or truncated a NUMERIC code would move published
  # nitrogen silently, so pin it on the whole vocabulary rather than on a
  # handful of countries.
  codes <- sort(unique(stats::na.omit(as.integer(whep::regions_full$code))))
  testthat::expect_gt(length(codes), 200)
  resolved <- whep:::.urban_resolve_area_code(
    tibble::tibble(area_code = codes),
    "cell_polity"
  )
  testthat::expect_identical(resolved$area_code, codes)

  # A zero-row frame (a year filter that keeps nothing) and an NA area_code
  # (a cell the crosswalk resolves to no reporting area, which the package
  # keeps rather than drops) must both survive rather than abort.
  empty <- whep:::.urban_resolve_area_code(
    tibble::tibble(area_code = integer(0)),
    "cropland_ha"
  )
  testthat::expect_identical(empty$area_code, integer(0))
  with_na <- whep:::.urban_resolve_area_code(
    tibble::tibble(area_code = c(203L, NA_integer_)),
    "cell_polity"
  )
  testthat::expect_identical(with_na$area_code, c(203L, NA_integer_))
})
