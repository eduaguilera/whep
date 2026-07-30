# The builders that DOCUMENT polity columns must also emit them populated.
#
# `check_like_ci.R` verifies that all eight exported builders document the polity columns they
# attach. That is a documentation check: it says nothing about whether the columns arrive with
# values in them. Asking the second question found two that do not.
#
# Measured on the five builders that take `example = TRUE`, which is the path the documented
# examples in `?build_supply_use` and friends actually run:
#
#   build_detailed_trade    10 rows, 0 with NA polity columns
#   get_primary_residues    10 rows, 0
#   get_wide_cbs            10 rows, 0
#   build_supply_use        10 rows, 1     <- whep#417
#   get_feed_intake         11 rows, 2     <- whep#417
#
# Both causes are the same and neither is a polities problem: those rows carry
# `area_code = NA` — no code at all, rather than an unmapped one — so
# `.add_reporting_polity_columns()` has nothing to resolve. build_supply_use's row has
# `value = 3.33e-14`, floating-point residue rather than an observation; get_feed_intake's two
# carry real item codes but no area, which looks like unmatched rows from a join.
#
# PINNED AT THE CURRENT COUNTS rather than asserted to zero. Asserting zero would fail the
# suite over a defect this branch does not own — the fix belongs to whoever owns those
# fixtures, and filtering the rows away in the polity step would hide the join behaviour
# instead of fixing it. Pinning means the three clean builders cannot start emitting NA
# quietly, and fixing whep#417 forces this list to be updated, which is the signal that it
# was fixed.
#
# The real builds do resolve everything: a full-range `get_wide_cbs()` gives 0 NA across
# 2,160,968 rows. This is an example-fixture problem, not a coverage gap.

.polity_na_counts <- function(builder) {
  out <- tryCatch(
    suppressWarnings(suppressMessages(do.call(builder, list(example = TRUE)))),
    error = function(e) NULL
  )
  if (is.null(out)) {
    return(NA_integer_)
  }
  frame <- as.data.frame(out)
  cols <- grep("polity", names(frame), value = TRUE)
  if (length(cols) == 0L) {
    return(NA_integer_)
  }
  sum(is.na(frame[[cols[1]]]))
}

test_that("example builders emit the polity columns they document", {
  expected <- c(
    build_detailed_trade = 0L,
    get_primary_residues = 0L,
    get_wide_cbs = 0L,
    # whep#417: rows with no area_code at all reach the output.
    build_supply_use = 1L,
    get_feed_intake = 2L
  )

  got <- vapply(names(expected), .polity_na_counts, integer(1))
  # Non-vacuous: every builder must have produced a frame with a polity column, or the
  # comparison below is about NA_integer_ values rather than about coverage.
  expect_false(any(is.na(got)))
  expect_equal(got, expected)
})

test_that("every builder that documents polity columns actually has them", {
  # The complement of the documentation gate: it checks the .Rd mentions the columns, this
  # checks the object carries them. A builder could document four and emit none.
  for (builder in c(
    "build_detailed_trade",
    "get_primary_residues",
    "get_wide_cbs",
    "build_supply_use",
    "get_feed_intake"
  )) {
    out <- tryCatch(
      suppressWarnings(suppressMessages(do.call(
        builder,
        list(example = TRUE)
      ))),
      error = function(e) NULL
    )
    testthat::skip_if(is.null(out), paste(builder, "example unavailable"))
    cols <- grep("polity", names(as.data.frame(out)), value = TRUE)
    expect_gte(length(cols), 4L)
    expect_true("reporting_polity_code" %in% cols)
  }
})
