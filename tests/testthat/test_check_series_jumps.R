# Fixtures ------------------------------------------------------------

# A single series with a clean level shift 100 -> 300 at 2003. The ratio
# there is 3 (> the 1.6 default high); every other step is flat (ratio 1).
.step_series <- function() {
  tibble::tibble(
    category = "energy",
    year = 2000:2005,
    value = c(100, 100, 100, 300, 300, 300)
  )
}

# Two series with the same 1.7x jump: an "area" series (should be tight) and
# a "yield" series (legitimately volatile).
.two_series <- function() {
  tibble::tibble(
    category = rep(c("area", "yield"), each = 3),
    year = rep(2000:2002, times = 2),
    value = c(100, 100, 170, 100, 100, 170)
  )
}

# check_series_jumps: detection -------------------------------------------

testthat::test_that("a known step flags at the right year and ratio", {
  flags <- whep::check_series_jumps(
    .step_series(),
    value,
    .by = "category",
    verbose = FALSE
  )

  flags |>
    pointblank::expect_col_exists(
      c("category", "year", "prev_value", "value", "ratio", "allowlisted")
    )
  testthat::expect_equal(nrow(flags), 1L)
  testthat::expect_equal(flags$year, 2003L)
  testthat::expect_equal(flags$ratio, 3)
  testthat::expect_equal(flags$prev_value, 100)
  testthat::expect_equal(flags$value, 300)
  testthat::expect_false(flags$allowlisted)
})

# check_series_jumps: allowlist -------------------------------------------

testthat::test_that("a documented break returns allowlisted = TRUE", {
  allow <- tibble::tibble(category = "energy", year = 2003L)
  flags <- whep::check_series_jumps(
    .step_series(),
    value,
    .by = "category",
    allowlist = allow,
    verbose = FALSE
  )

  testthat::expect_equal(nrow(flags), 1L)
  testthat::expect_true(flags$allowlisted)
  testthat::expect_equal(flags$year, 2003L)
})

# check_series_jumps: near-zero gate --------------------------------------

testthat::test_that("onsets below min_value are not flagged", {
  onset <- tibble::tibble(
    category = "solar",
    year = 2000:2003,
    value = c(0.001, 0.001, 0.05, 0.05)
  )

  flags <- whep::check_series_jumps(
    onset,
    value,
    .by = "category",
    min_value = 0.01,
    verbose = FALSE
  )

  # 0.001 -> 0.05 is a 50x jump but the previous value is below min_value.
  testthat::expect_equal(nrow(flags), 0L)
})

# check_series_jumps: consecutive-only ------------------------------------

testthat::test_that("non-consecutive gaps are skipped unless asked", {
  gapped <- tibble::tibble(
    category = "gas",
    year = c(2000L, 2001L, 2005L),
    value = c(100, 100, 300)
  )

  consecutive <- whep::check_series_jumps(
    gapped,
    value,
    .by = "category",
    verbose = FALSE
  )
  testthat::expect_equal(nrow(consecutive), 0L)

  all_pairs <- whep::check_series_jumps(
    gapped,
    value,
    .by = "category",
    consecutive_only = FALSE,
    verbose = FALSE
  )
  testthat::expect_equal(nrow(all_pairs), 1L)
  testthat::expect_equal(all_pairs$year, 2005L)
  testthat::expect_equal(all_pairs$ratio, 3)
})

# check_series_jumps: per-group bands -------------------------------------

testthat::test_that("per-group bands override the global default", {
  # Default 0.55/1.6 flags both 1.7x jumps.
  default_flags <- whep::check_series_jumps(
    .two_series(),
    value,
    .by = "category",
    verbose = FALSE
  )
  testthat::expect_equal(nrow(default_flags), 2L)

  # Widen only the yield band; area stays on the default and remains flagged.
  bands <- tibble::tribble(
    ~category, ~lo, ~hi,
    "area", 0.9, 1.1,
    "yield", 0.4, 2.5
  )
  banded_flags <- whep::check_series_jumps(
    .two_series(),
    value,
    .by = "category",
    bands = bands,
    verbose = FALSE
  )
  testthat::expect_equal(nrow(banded_flags), 1L)
  testthat::expect_equal(banded_flags$category, "area")
})

# check_series_jumps: empty / typed shape ---------------------------------

testthat::test_that("a flat series returns a typed zero-row tibble", {
  flat <- tibble::tibble(
    category = "hydro",
    year = 2000:2004,
    value = c(50, 50, 50, 50, 50)
  )

  flags <- whep::check_series_jumps(
    flat,
    value,
    .by = "category",
    verbose = FALSE
  )

  testthat::expect_s3_class(flags, "tbl_df")
  testthat::expect_equal(nrow(flags), 0L)
  testthat::expect_named(
    flags,
    c("category", "year", "prev_value", "value", "ratio", "allowlisted")
  )
  testthat::expect_type(flags$ratio, "double")
  testthat::expect_type(flags$prev_value, "double")
  testthat::expect_type(flags$allowlisted, "logical")
})

testthat::test_that("integer input yields a double value column", {
  ints <- tibble::tibble(
    category = "energy",
    year = 2000:2002,
    value = c(100L, 100L, 300L)
  )

  flags <- whep::check_series_jumps(
    ints,
    value,
    .by = "category",
    verbose = FALSE
  )

  testthat::expect_equal(nrow(flags), 1L)
  testthat::expect_type(flags$value, "double")
  testthat::expect_type(flags$prev_value, "double")
  testthat::expect_equal(flags$value, 300)
})

# check_series_jumps: ungrouped + validation ------------------------------

testthat::test_that("works without groups and reports cli counts", {
  one <- tibble::tibble(
    year = 2000:2003,
    value = c(10, 10, 40, 40)
  )

  testthat::expect_message(
    flags <- whep::check_series_jumps(one, value),
    "flagged"
  )
  testthat::expect_equal(nrow(flags), 1L)
  testthat::expect_equal(flags$year, 2002L)
  testthat::expect_equal(flags$ratio, 4)
})

testthat::test_that("input validation aborts on bad columns and bounds", {
  testthat::expect_error(
    whep::check_series_jumps(
      tibble::tibble(year = 2000L),
      value,
      verbose = FALSE
    ),
    "missing column"
  )
  testthat::expect_error(
    whep::check_series_jumps(
      .step_series(),
      value,
      .by = "category",
      ratio_bounds = c(1.6, 0.55),
      verbose = FALSE
    ),
    "ratio_bounds"
  )
})

# check_series_jumps: dropouts (whep#938) ---------------------------------

# Two items in one country: wheat runs 2018-2021, fodder stops after 2019
# (its rows simply end, the shape FAOSTAT fodder has from 2020). A second
# country's fodder falls to an explicit zero instead.
.dropout_panel <- function() {
  tibble::tribble(
    ~area_code, ~item,    ~year, ~value,
    1L,         "wheat",  2018L, 100,
    1L,         "wheat",  2019L, 100,
    1L,         "wheat",  2020L, 110,
    1L,         "wheat",  2021L, 110,
    1L,         "fodder", 2018L, 50,
    1L,         "fodder", 2019L, 50,
    2L,         "fodder", 2018L, 40,
    2L,         "fodder", 2019L, 40,
    2L,         "fodder", 2020L, 0,
    2L,         "fodder", 2021L, 0
  )
}

testthat::test_that("a term that stops is invisible by default", {
  # The published behaviour, and why whep#938 went unannounced: a series
  # that ends has no later pair, and a pair ending in zero is gated out.
  flags <- whep::check_series_jumps(
    .dropout_panel(),
    value,
    .by = c("area_code", "item"),
    verbose = FALSE
  )
  testthat::expect_equal(nrow(flags), 0L)
})

testthat::test_that("dropouts = TRUE flags a series that stops or hits 0", {
  flags <- whep::check_series_jumps(
    .dropout_panel(),
    value,
    .by = c("area_code", "item"),
    dropouts = TRUE,
    verbose = FALSE
  )

  # One flag per series, at the first year without it: 2020 -> 2021 is zero
  # to zero and is not a second step.
  testthat::expect_equal(nrow(flags), 2L)
  testthat::expect_equal(flags$area_code, c(1L, 2L))
  testthat::expect_equal(flags$item, c("fodder", "fodder"))
  testthat::expect_identical(flags$year, c(2020L, 2020L))
  testthat::expect_equal(flags$prev_value, c(50, 40))
  testthat::expect_equal(flags$value, c(0, 0))
  testthat::expect_equal(flags$ratio, c(0, 0))
})

testthat::test_that("a series running to the last year is not a dropout", {
  flags <- whep::check_series_jumps(
    dplyr::filter(.dropout_panel(), item == "wheat"),
    value,
    .by = c("area_code", "item"),
    dropouts = TRUE,
    verbose = FALSE
  )
  testthat::expect_equal(nrow(flags), 0L)
})

testthat::test_that("a mid-series gap is a dropout; its return is not", {
  gapped <- tibble::tibble(
    category = "fodder",
    year = c(2010L, 2011L, 2014L, 2015L),
    value = c(80, 80, 80, 80)
  )
  # The time grid is the years present anywhere in `data`, so give the
  # missing years a row in another series.
  panel <- dplyr::bind_rows(
    gapped,
    tibble::tibble(category = "wheat", year = 2010:2015, value = 100)
  )
  flags <- whep::check_series_jumps(
    panel,
    value,
    .by = "category",
    dropouts = TRUE,
    verbose = FALSE
  )
  # 2011 -> 2012 falls to nothing; 2013 -> 2014 is an onset from zero, which
  # the min_value gate keeps quiet exactly as it does without dropouts.
  testthat::expect_identical(flags$year, 2012L)
  testthat::expect_equal(flags$ratio, 0)
})

testthat::test_that("a documented dropout can be allowlisted", {
  allow <- tibble::tibble(area_code = 1L, item = "fodder", year = 2020L)
  flags <- whep::check_series_jumps(
    .dropout_panel(),
    value,
    .by = c("area_code", "item"),
    dropouts = TRUE,
    allowlist = allow,
    verbose = FALSE
  )
  testthat::expect_equal(flags$allowlisted, c(TRUE, FALSE))
})

testthat::test_that("a fall to a value below min_value is still gated", {
  # dropouts adds zero, and only zero: a fall to 0.001 under min_value = 0.01
  # stays a near-zero value the gate is there to ignore.
  tiny <- tibble::tibble(
    category = "fodder",
    year = 2000:2001,
    value = c(50, 0.001)
  )
  flags <- whep::check_series_jumps(
    tiny,
    value,
    .by = "category",
    min_value = 0.01,
    dropouts = TRUE,
    verbose = FALSE
  )
  testthat::expect_equal(nrow(flags), 0L)
})

testthat::test_that("an ungrouped series that stops early is flagged", {
  # With no `.by` the whole table is one series, so it can only stop when a
  # later year is present with no value, i.e. an explicit NA is not a stop.
  one <- tibble::tibble(year = 2000:2002, value = c(10, 10, 0))
  flags <- whep::check_series_jumps(
    one,
    value,
    dropouts = TRUE,
    verbose = FALSE
  )
  testthat::expect_identical(flags$year, 2002L)
})

testthat::test_that("a character dropouts scopes the panel to that group", {
  # Country 2's fodder runs to 2019 and the country's own panel ends there:
  # with the whole-table panel it stops at 2020; scoped to area_code it does
  # not stop at all. Country 1's fodder stops while country 1 goes on.
  panel <- dplyr::filter(.dropout_panel(), !(area_code == 2L & year >= 2020L))
  whole <- whep::check_series_jumps(
    panel,
    value,
    .by = c("area_code", "item"),
    dropouts = TRUE,
    verbose = FALSE
  )
  testthat::expect_equal(whole$area_code, c(1L, 2L))

  scoped <- whep::check_series_jumps(
    panel,
    value,
    .by = c("area_code", "item"),
    dropouts = "area_code",
    verbose = FALSE
  )
  testthat::expect_equal(scoped$area_code, 1L)
  testthat::expect_identical(scoped$year, 2020L)
})

testthat::test_that("a dropouts column outside .by is refused", {
  testthat::expect_error(
    whep::check_series_jumps(
      .dropout_panel(),
      value,
      .by = "item",
      dropouts = "area_code",
      verbose = FALSE
    ),
    "not in"
  )
})

testthat::test_that("dropouts must be TRUE, FALSE or column names", {
  testthat::expect_error(
    whep::check_series_jumps(
      .step_series(),
      value,
      .by = "category",
      dropouts = NA,
      verbose = FALSE
    ),
    "dropouts"
  )
})
