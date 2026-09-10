# The subject of this file is that an absent ROW is invisible to every check
# the package already has, and visible to this one.

testthat::test_that("a missing month is invisible to every na.rm choice", {
  short <- .lattice_month_fixture(n_cells = 1L, drop = 12L)

  # Nothing is NA, so no na.rm policy differs; the row count is a count of the
  # rows that came, which is exactly what it says; and the annual sum is a
  # perfectly ordinary number.
  testthat::expect_false(anyNA(short))
  testthat::expect_equal(
    sum(short$value, na.rm = TRUE),
    sum(short$value, na.rm = FALSE)
  )
  annual <- dplyr::summarise(short, total = sum(value), .by = c(lon, lat, year))
  testthat::expect_equal(nrow(annual), 1L)
  testthat::expect_true(is.finite(annual$total))

  # And the lattice check sees it.
  gaps <- whep::key_lattice_gaps(
    short,
    list(month = 1:12),
    .by = c("lon", "lat", "year")
  )
  testthat::expect_equal(nrow(gaps), 1L)
  testthat::expect_equal(gaps$month, 12L)
})

testthat::test_that("a complete lattice yields zero gaps of the right shape", {
  full <- .lattice_month_fixture(n_cells = 3L)
  gaps <- whep::key_lattice_gaps(
    full,
    list(month = 1:12),
    .by = c("lon", "lat", "year")
  )
  testthat::expect_equal(nrow(gaps), 0L)
  testthat::expect_named(gaps, c("lon", "lat", "year", "month"))
  testthat::expect_type(gaps$month, "integer")
})

testthat::test_that("only the incomplete groups are reported", {
  short <- .lattice_month_fixture(n_cells = 3L, drop = c(4L, 7L))
  gaps <- whep::key_lattice_gaps(
    short,
    list(month = 1:12),
    .by = c("lon", "lat", "year")
  )
  testthat::expect_equal(nrow(gaps), 2L)
  testthat::expect_setequal(gaps$month, c(4L, 7L))
  testthat::expect_equal(length(unique(gaps$lon)), 1L)
})

testthat::test_that("duplicate rows do not disguise a missing key", {
  short <- .lattice_month_fixture(n_cells = 1L, drop = 12L)
  doubled <- dplyr::bind_rows(short, short)
  # The row count is now 22, more than a complete year's twelve, so any
  # count-of-rows test would pass.
  testthat::expect_gt(nrow(doubled), 12L)
  gaps <- whep::key_lattice_gaps(
    doubled,
    list(month = 1:12),
    .by = c("lon", "lat", "year")
  )
  testthat::expect_equal(gaps$month, 12L)
})

testthat::test_that("a surplus key does not let the count fast path pass", {
  # Eleven expected months plus a thirteenth: twelve distinct keys, so the
  # scalar count identity would say "complete" if it were applied blindly.
  short <- .lattice_month_fixture(n_cells = 1L, drop = 7L) |>
    dplyr::bind_rows(tibble::tibble(
      lon = 0.25,
      lat = 0.25,
      year = 2000L,
      month = 13L,
      value = 13
    ))
  testthat::expect_equal(dplyr::n_distinct(short$month), 12L)
  gaps <- whep::key_lattice_gaps(
    short,
    list(month = 1:12),
    .by = c("lon", "lat", "year")
  )
  testthat::expect_equal(gaps$month, 7L)
})

testthat::test_that("an ungrouped check treats the table as one group", {
  gaps <- whep::key_lattice_gaps(
    tibble::tibble(month = c(1:6, 8:12)),
    list(month = 1:12)
  )
  testthat::expect_equal(gaps$month, 7L)
})

testthat::test_that("a data frame expected set carries a per-year lattice", {
  # The expected key set varies along one of its own keys: 2000 admits two
  # areas and 2001 admits one. Expressed as a frame and not grouped on, which
  # a cross product cannot say.
  expected <- tibble::tribble(
    ~year, ~area_code,
    2000L, 11L,
    2000L, 21L,
    2001L, 11L
  )
  data <- tibble::tribble(
    ~year, ~area_code,
    2000L, 11L,
    2001L, 11L
  )
  gaps <- whep::key_lattice_gaps(data, expected)
  testthat::expect_equal(nrow(gaps), 1L)
  testthat::expect_equal(gaps$year, 2000L)
  testthat::expect_equal(gaps$area_code, 21L)
})

testthat::test_that("a rule may read the data before naming the lattice", {
  short <- .lattice_month_fixture(n_cells = 1L, drop = 12L)
  rule <- function(data) list(month = seq_len(max(data$month) + 1L))
  gaps <- whep::key_lattice_gaps(
    short,
    rule,
    .by = c("lon", "lat", "year")
  )
  testthat::expect_equal(gaps$month, 12L)
})

testthat::test_that("a non-contiguous expected set is honoured exactly", {
  # Only the two boundary months are required, which is what the soil-water
  # change term actually reads. A hole at month 7 is therefore not a gap.
  hole <- .lattice_month_fixture(n_cells = 1L, drop = 7L)
  testthat::expect_equal(
    nrow(whep::key_lattice_gaps(
      hole,
      list(month = c(1L, 12L)),
      .by = c("lon", "lat", "year")
    )),
    0L
  )
  no_december <- .lattice_month_fixture(n_cells = 1L, drop = 12L)
  testthat::expect_equal(
    whep::key_lattice_gaps(
      no_december,
      list(month = c(1L, 12L)),
      .by = c("lon", "lat", "year")
    )$month,
    12L
  )
})

testthat::test_that("max_gaps refuses to enumerate an absent input", {
  testthat::expect_error(
    whep::key_lattice_gaps(
      tibble::tibble(year = 2000L, month = 1L),
      list(month = 1:100),
      .by = "year",
      max_gaps = 10
    ),
    "more than"
  )
})

testthat::test_that("a bad expected set or column is refused by name", {
  short <- .lattice_month_fixture(n_cells = 1L, drop = 12L)
  testthat::expect_error(
    whep::key_lattice_gaps(short, list(1:12), .by = "year"),
    "named list"
  )
  testthat::expect_error(
    whep::key_lattice_gaps(short, list(quarter = 1:4), .by = "year"),
    "quarter"
  )
  testthat::expect_error(
    whep::key_lattice_gaps(short, list(month = 1:12), .by = 42),
    "character"
  )
  testthat::expect_error(
    whep::key_lattice_gaps(
      short,
      list(month = 1:12),
      .by = "year",
      max_gaps = c(1, 2)
    ),
    "single non-negative"
  )
  testthat::expect_error(
    whep::key_lattice_gaps(short, tibble::tibble(month = integer())),
    "empty key set"
  )
})

testthat::test_that("check_keys_complete aborts naming the missing keys", {
  short <- .lattice_month_fixture(n_cells = 1L, drop = 12L)
  expect_lattice_guard(
    # The annual sum is finite, non-negative and NA-free over eleven months.
    well_formed = {
      annual <- dplyr::summarise(short, total = sum(value), .by = year)
      nrow(annual) == 1L && is.finite(annual$total) && !anyNA(annual)
    },
    guard = whep::check_keys_complete(
      short,
      list(month = 1:12),
      .by = c("lon", "lat", "year")
    )
  )
  cnd <- rlang::catch_cnd(
    whep::check_keys_complete(
      short,
      list(month = 1:12),
      .by = c("lon", "lat", "year")
    ),
    classes = "error"
  )
  testthat::expect_s3_class(cnd, "whep_absent_input")
  testthat::expect_equal(cnd$missing$month, 12L)
  testthat::expect_match(conditionMessage(cnd), "month=12")
})

testthat::test_that("check_keys_complete warns and returns data unchanged", {
  short <- .lattice_month_fixture(n_cells = 1L, drop = 12L)
  expect_lattice_guard(
    well_formed = sum(short$value) == sum(1:11),
    guard = whep::check_keys_complete(
      short,
      list(month = 1:12),
      .by = c("lon", "lat", "year"),
      action = "warn"
    ),
    condition = "warning"
  )
  returned <- suppressWarnings(
    whep::check_keys_complete(
      short,
      list(month = 1:12),
      .by = c("lon", "lat", "year"),
      action = "warn"
    )
  )
  testthat::expect_equal(returned, short)
})

testthat::test_that("a complete lattice is passed silently and unchanged", {
  full <- .lattice_month_fixture(n_cells = 2L)
  testthat::expect_silent(
    returned <- whep::check_keys_complete(
      full,
      list(month = 1:12),
      .by = c("lon", "lat", "year")
    )
  )
  testthat::expect_equal(returned, full)
})

testthat::test_that("an unknown action is rejected", {
  full <- .lattice_month_fixture(n_cells = 1L)
  testthat::expect_error(
    whep::check_keys_complete(
      full,
      list(month = 1:12),
      .by = "year",
      action = "drop"
    ),
    class = "rlang_error"
  )
})

testthat::test_that("caller details are interpolated in the caller's frame", {
  # A `details` bullet naming a caller local must resolve to that local. Before
  # this was fixed, `{var}` reached cli in the wrong environment, found
  # stats::var and died pasting a closure -- which would have turned every
  # guard at every wired site into a crash with an unrelated message.
  short <- .lattice_month_fixture(n_cells = 1L, drop = 12L)
  caller_local <- "mseepage"
  cnd <- rlang::catch_cnd(
    whep::check_keys_complete(
      short,
      list(month = 1:12),
      .by = c("lon", "lat", "year"),
      details = c(i = "The source was {.field {caller_local}}.")
    ),
    classes = "error"
  )
  testthat::expect_s3_class(cnd, "whep_incomplete_lattice")
  testthat::expect_match(conditionMessage(cnd), "mseepage")
})
