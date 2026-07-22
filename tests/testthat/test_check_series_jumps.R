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
