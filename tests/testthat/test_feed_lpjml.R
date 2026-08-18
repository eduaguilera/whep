test_that("grass_access_shares returns the documented defaults", {
  s <- whep::grass_access_shares()
  expect_equal(s$aboveground, 0.46)
  expect_equal(s$grazable, 1)
  expect_equal(s$w_c_dm, 0.45)
})

test_that(".lpjml_grass_to_dm converts gC/m2/yr to grazable t DM/ha/yr", {
  shares <- whep::grass_access_shares(
    aboveground = 0.46,
    grazable = 1,
    w_c_dm = 0.45
  )
  # 1 gC/m2 = 0.01 tC/ha; x aboveground x grazable / w_c_dm -> t DM/ha.
  expect_equal(
    whep:::.lpjml_grass_to_dm(600, shares),
    600 * 0.46 * 1 * 0.01 / 0.45,
    tolerance = 1e-9
  )
})

test_that("a lower grazable share lowers availability proportionally", {
  full <- whep::grass_access_shares(grazable = 1)
  half <- whep::grass_access_shares(grazable = 0.5)
  expect_equal(
    whep:::.lpjml_grass_to_dm(600, half),
    whep:::.lpjml_grass_to_dm(600, full) / 2,
    tolerance = 1e-9
  )
})

test_that("build_grass_availability_lpjml(example = TRUE) returns the tidy schema", {
  av <- whep::build_grass_availability_lpjml(example = TRUE)
  expect_s3_class(av, "tbl_df")
  expect_setequal(
    names(av),
    c(
      "lon",
      "lat",
      "year",
      "grass_npp_gc_m2",
      "grass_avail_dm_t_ha",
      "grass_avail_dm_t"
    )
  )
  expect_true(all(av$grass_avail_dm_t_ha >= 0))
  expect_true(all(av$grass_avail_dm_t >= 0))
})

test_that("build_grass_availability_lpjml defaults to pinned artifacts", {
  testthat::local_mocked_bindings(
    whep_read_file = function(file_alias, ...) {
      expect_equal(file_alias, "lpjml-grass-availability")
      tibble::tibble(
        lon = 0.25,
        lat = 50.25,
        year = c(1999L, 2000L),
        grass_npp_gc_m2 = c(400, 500)
      )
    },
    .package = "whep"
  )
  av <- whep::build_grass_availability_lpjml(
    run_dir = NULL,
    years = 2000L
  )
  expect_equal(av$year, 2000L)
  expect_setequal(
    names(av),
    c(
      "lon",
      "lat",
      "year",
      "grass_npp_gc_m2",
      "grass_avail_dm_t_ha",
      "grass_avail_dm_t"
    )
  )
  expect_true(av$grass_avail_dm_t_ha > 0)
  expect_true(av$grass_avail_dm_t > 0)
})

test_that("build_grass_availability_lpjml accepts custom artifact data", {
  artifact <- tibble::tibble(
    lon = 0.25,
    lat = 50.25,
    year = c(1999L, 2000L),
    grass_npp_gc_m2 = c(400, 500)
  )
  av <- whep::build_grass_availability_lpjml(
    availability = artifact,
    years = 2000L
  )
  expect_equal(av$year, 2000L)
  expect_true(av$grass_avail_dm_t_ha > 0)
  expect_true(av$grass_avail_dm_t > 0)
})

test_that("build_grass_availability_lpjml accepts custom artifact paths", {
  artifact <- tibble::tibble(
    lon = 0.25,
    lat = 50.25,
    year = 2000L,
    grass_avail_dm_t_ha = 2,
    grass_avail_dm_t = 100
  )
  path <- withr::local_tempfile(fileext = ".parquet")
  nanoparquet::write_parquet(artifact, path)
  av <- whep::build_grass_availability_lpjml(availability_path = path)
  expect_equal(av$grass_avail_dm_t_ha, 2)
  expect_equal(av$grass_avail_dm_t, 100)
})

test_that("build_grass_availability_lpjml rejects mixed custom sources", {
  expect_error(
    whep::build_grass_availability_lpjml(
      run_dir = "/tmp/local-run",
      availability = tibble::tibble(lon = 0, lat = 0, year = 2000L)
    ),
    "either a custom availability artifact"
  )
})

test_that("build_grass_availability dispatches lpjml and records the method", {
  av <- whep::build_grass_availability(method = "lpjml", example = TRUE)
  expect_true("method_grass" %in% names(av))
  expect_equal(unique(av$method_grass), "lpjml")
})

test_that("build_grass_availability errors on the unimplemented coefficient method", {
  expect_error(
    whep::build_grass_availability(method = "coefficient"),
    "not yet implemented"
  )
})

test_that("aggregate_grass_to_polity conserves total grass", {
  grass <- whep::build_grass_availability(method = "lpjml", example = TRUE)
  cp <- tibble::tibble(
    lon = grass$lon,
    lat = grass$lat,
    area_code = 1L,
    polity_frac = 1
  )
  agg <- whep::aggregate_grass_to_polity(grass, cp)
  expect_setequal(names(agg), c("area_code", "year", "grass_avail_dm_t"))
  expect_equal(
    sum(agg$grass_avail_dm_t),
    sum(grass$grass_avail_dm_t),
    tolerance = 1e-6
  )
})

test_that("aggregate_grass_to_polity splits a border cell by polity_frac", {
  grass <- tibble::tibble(
    lon = 0.25,
    lat = 0.25,
    year = 2000L,
    grass_avail_dm_t = 100
  )
  cp <- tibble::tibble(
    lon = c(0.25, 0.25),
    lat = c(0.25, 0.25),
    area_code = c(1L, 2L),
    polity_frac = c(0.7, 0.3)
  )
  agg <- whep::aggregate_grass_to_polity(grass, cp)
  expect_equal(agg$grass_avail_dm_t[agg$area_code == 1L], 70)
  expect_equal(agg$grass_avail_dm_t[agg$area_code == 2L], 30)
})

# ---- C0 characterisation baseline (polycell consumer migration) --------
#
# THESE ARE CHARACTERISATION TESTS, NOT CORRECTNESS ASSERTIONS. They pin
# what aggregate_grass_to_polity() does TODAY, on unmodified pre-migration
# code, so that any value change the polycell consumer migration
# introduces is visible and attributable instead of silent.
#
# aggregate_grass_to_polity() (R/feed_lpjml.R:166-178) multiplies an
# ALREADY-ABSOLUTE per-cell tonnage by polity_frac and sums. There is no
# area term, so grass tonnage is conserved whenever polity_frac partitions
# the cell, and substituting an absolute area for polity_frac here would
# multiply tonnes by hectares. The existing two tests above cover a
# single-polity crosswalk and one two-polity cell in isolation; the
# fixture below shares one cell three ways alongside cells at other
# latitudes, which is the shape a polycell crosswalk actually has.
#
# Note that R/feed_lpjml.R's OTHER cell_area_ha use, at :110, is FROZEN
# by AM-2 and is deliberately not characterised here: it inverts a
# normalisation prepare_spatialize_all.R applied when building the LPJmL
# input, so it must not be migrated.

.grass_c0_grass <- function() {
  tibble::tribble(
    ~lon, ~lat, ~year, ~grass_avail_dm_t,
    -0.25, -0.25, 2000L, 1000,
    0.25, 59.75, 2000L, 250,
    0.75, 0.25, 2000L, 40
  )
}

.grass_c0_cell_polity <- function() {
  tibble::tribble(
    ~lon, ~lat, ~area_code, ~polity_frac,
    -0.25, -0.25, 1L, 0.5,
    -0.25, -0.25, 2L, 0.3,
    -0.25, -0.25, 3L, 0.2,
    0.25, 59.75, 4L, 0.6,
    0.25, 59.75, 5L, 0.4,
    0.75, 0.25, 6L, 1.0
  )
}

test_that("C0: grass is conserved across multi-polity cells", {
  agg <- whep::aggregate_grass_to_polity(
    .grass_c0_grass(),
    .grass_c0_cell_polity()
  )

  # 1000 + 250 + 40 t DM in, the same out. Tolerance is DA-18's locked
  # 1e-9 relative bound; the measured gap on this fixture today is 0.
  expect_equal(
    sum(agg$grass_avail_dm_t),
    sum(.grass_c0_grass()$grass_avail_dm_t),
    tolerance = 1e-9
  )
  expect_equal(sum(agg$grass_avail_dm_t), 1290)
  expect_equal(
    agg$grass_avail_dm_t[match(c(1L, 2L, 3L), agg$area_code)],
    c(500, 300, 200)
  )
})

test_that("C0: no area column reaches grass aggregation", {
  base <- whep::aggregate_grass_to_polity(
    .grass_c0_grass(),
    .grass_c0_cell_polity()
  )
  # Hand the crosswalk both area columns the migration will introduce.
  # Today they are ignored: the tonnage is already absolute, so the only
  # weight is polity_frac. THIS IS THE GUARD against multiplying tonnes
  # of dry matter by hectares.
  with_areas <- whep::aggregate_grass_to_polity(
    .grass_c0_grass(),
    dplyr::mutate(
      .grass_c0_cell_polity(),
      cell_area_ha = 308000,
      land_area_ha = 270000
    )
  )

  expect_identical(with_areas, base)
})

test_that("C0: a grass cell absent from the crosswalk is dropped silently", {
  # R/feed_lpjml.R:169 joins with dplyr::inner_join(), so grass in a cell
  # the crosswalk does not carry disappears with no warning. Pinned as
  # current behaviour; it is not asserted to be right.
  one_cell <- dplyr::filter(.grass_c0_cell_polity(), lon == -0.25)
  agg <- expect_no_warning(
    whep::aggregate_grass_to_polity(.grass_c0_grass(), one_cell)
  )

  # 290 of the fixture's 1290 t DM is lost without trace.
  expect_equal(sum(agg$grass_avail_dm_t), 1000)
  expect_setequal(agg$area_code, c(1L, 2L, 3L))
})

# ---- polity_validity (#675) -------------------------------------------

# Area 277 is South Sudan (SSD-2011-2025); a 2000 grass row on a cell the
# present-day crosswalk labels 277 names a state that did not exist then.
.fl_out_of_span_grass <- function() {
  tibble::tibble(
    lon = c(0.25, 0.25),
    lat = c(0.25, 0.25),
    year = c(2000L, 2020L),
    grass_avail_dm_t = c(100, 200)
  )
}

.fl_out_of_span_cp <- function() {
  tibble::tibble(lon = 0.25, lat = 0.25, area_code = 277L, polity_frac = 1)
}

test_that("aggregate_grass_to_polity names an anachronistic polity", {
  expect_warning(
    agg <- whep::aggregate_grass_to_polity(
      .fl_out_of_span_grass(),
      .fl_out_of_span_cp()
    ),
    "did not exist in that row's year"
  )

  # "keep" is the default: both years survive and the totals do not move.
  expect_equal(nrow(agg), 2L)
  expect_equal(sum(agg$grass_avail_dm_t), 300)
})

test_that("aggregate_grass_to_polity honours drop and flag", {
  expect_warning(
    dropped <- whep::aggregate_grass_to_polity(
      .fl_out_of_span_grass(),
      .fl_out_of_span_cp(),
      polity_validity = "drop"
    )
  )
  expect_warning(
    flagged <- whep::aggregate_grass_to_polity(
      .fl_out_of_span_grass(),
      .fl_out_of_span_cp(),
      polity_validity = "flag"
    )
  )

  expect_equal(dropped$year, 2020L)
  expect_equal(dropped$grass_avail_dm_t, 200)
  # The flag arrives without dragging polity columns onto an output whose
  # published schema has none.
  expect_equal(flagged$reporting_polity_out_of_span, flagged$year == 2000L)
  expect_false("reporting_polity_code" %in% names(flagged))
})

test_that("read_lpjml_grass_productivity(example = TRUE) returns the tidy schema", {
  gp <- whep::read_lpjml_grass_productivity(example = TRUE)
  expect_s3_class(gp, "tbl_df")
  expect_setequal(names(gp), c("lon", "lat", "year", "grass_npp"))
  expect_true(all(gp$grass_npp > 0))
})

test_that("read_lpjml_grass_productivity defaults to pinned artifacts", {
  testthat::local_mocked_bindings(
    whep_read_file = function(file_alias, ...) {
      expect_equal(file_alias, "lpjml-grass-productivity")
      tibble::tibble(
        lon = 0.25,
        lat = 50.25,
        year = c(1999L, 2000L),
        grass_npp = c(0, 700)
      )
    },
    .package = "whep"
  )
  gp <- whep::read_lpjml_grass_productivity(
    run_dir = NULL,
    years = 2000L
  )
  expect_equal(gp$year, 2000L)
  expect_equal(gp$grass_npp, 700)
})

test_that("read_lpjml_grass_productivity accepts custom artifact data", {
  artifact <- tibble::tibble(
    lon = 0.25,
    lat = 50.25,
    year = c(1999L, 2000L),
    grass_npp = c(0, 700)
  )
  gp <- whep::read_lpjml_grass_productivity(
    productivity = artifact,
    years = 2000L
  )
  expect_equal(gp$year, 2000L)
  expect_equal(gp$grass_npp, 700)
})

test_that("read_lpjml_grass_productivity accepts custom artifact paths", {
  artifact <- tibble::tibble(
    lon = 0.25,
    lat = 50.25,
    year = 2000L,
    grass_npp = 700
  )
  path <- withr::local_tempfile(fileext = ".parquet")
  nanoparquet::write_parquet(artifact, path)
  gp <- whep::read_lpjml_grass_productivity(productivity_path = path)
  expect_equal(gp$grass_npp, 700)
})

test_that(".clip_run_years drops out-of-coverage years with a warning", {
  # An LPJmL run covering 1901-2009 (109 time steps) must skip requested years
  # outside it rather than abort the read on an out-of-bounds index.
  expect_warning(
    out <- whep:::.clip_run_years(
      c(1850L, 2000L, 2020L),
      1901L,
      109L,
      "pft_npp.nc"
    ),
    "outside the run's coverage"
  )
  expect_equal(out, 2000L)
  # All in-range years pass through untouched, no warning.
  expect_silent(
    keep <- whep:::.clip_run_years(c(1950L, 2000L), 1901L, 109L, "pft_npp.nc")
  )
  expect_equal(keep, c(1950L, 2000L))
})
