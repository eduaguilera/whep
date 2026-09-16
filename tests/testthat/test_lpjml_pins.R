# The LPJmL-derived-pin tripwire, `validation/lpjml_pins.R` (#559).
#
# What is exercised here is the COMPARISON, not the pins. `check_one()` and the
# four detectors it composes are pure functions over two lists, so they can be
# driven from hand-built fixtures. Reading a pin is what
# `Rscript validation/lpjml_pins.R` does, and it needs the pins board -- which
# is the one thing a test may not touch (#490).
#
# `validation/` is in `.Rbuildignore`, so this file is absent from the built
# tarball and these tests skip under `R CMD check`. They are not thereby
# unrun: `offline-tests.yaml` checks the repository out and calls
# `devtools::test()` against the source tree, where `validation/` is present.

.lpjml_pins_env <- function() {
  path <- testthat::test_path("..", "..", "validation", "lpjml_pins.R")
  testthat::skip_if_not(
    file.exists(path),
    "validation/ is .Rbuildignore'd, so it is absent from a built package"
  )
  env <- new.env(parent = globalenv())
  # Sourcing runs nothing. Both the package load at the head of the script and
  # the `main()` call at its foot are guarded on `sys.nframe() == 0L`, which is
  # true only under `Rscript` and false inside `source()`.
  source(path, local = env)
  env
}

# The recorded lpjml-grass-productivity row of `gt_lpjml_pins.json`, so the
# fixtures below deviate from a real baseline rather than a made-up one.
.recorded_mean <- 183.282239534719

.pin_observed <- function(...) {
  utils::modifyList(
    list(
      alias = "lpjml-grass-productivity",
      fatal = NULL,
      n_rows = 6809325,
      first_year = 1901,
      last_year = 2023,
      n_window = 611000,
      mean = .recorded_mean,
      median = 102.713092803955,
      violations = tibble::tibble(
        column = "grass_npp",
        below = 0L,
        above = 0L,
        n_missing = 0L
      )
    ),
    list(...)
  )
}

.pin_expected <- function(...) {
  utils::modifyList(
    list(
      n_rows = 6809325,
      first_year = 1901,
      last_year = 2023,
      mean = .recorded_mean,
      median = 102.713092803955
    ),
    list(...)
  )
}

test_that("a pin matching its recorded baseline passes", {
  env <- .lpjml_pins_env()

  row <- env$check_one(.pin_observed(), .pin_expected())

  expect_equal(row$verdict, "ok")
  expect_equal(row$pin, "lpjml-grass-productivity")
})

# The headline guarantee of #559: a pin swap must not pass quietly. 1e-4 is two
# orders below the 1.7% hydrology shift that slipped through at the 2% tolerance
# the script was originally written with, and one order above MAGNITUDE_TOL.
test_that("a recorded mean off by 1e-4 relative is flagged", {
  env <- .lpjml_pins_env()

  row <- env$check_one(
    .pin_observed(mean = .recorded_mean * (1 + 1e-4)),
    .pin_expected()
  )

  expect_equal(row$verdict, "DEVIATES")
  expect_match(row$detail, "vs recorded")
})

test_that("the flagging is two-sided", {
  env <- .lpjml_pins_env()

  row <- env$check_one(
    .pin_observed(mean = .recorded_mean * (1 - 1e-4)),
    .pin_expected()
  )

  expect_equal(row$verdict, "DEVIATES")
})

# Pins the tolerance itself. Without this the test above would still pass if
# MAGNITUDE_TOL were tightened to 0, which would make every run flag on
# floating-point summation order alone.
test_that("drift within MAGNITUDE_TOL is not flagged", {
  env <- .lpjml_pins_env()

  expect_equal(env$MAGNITUDE_TOL, 1e-5)
  row <- env$check_one(
    .pin_observed(mean = .recorded_mean * (1 + 1e-6)),
    .pin_expected()
  )

  expect_equal(row$verdict, "ok")
})

test_that("a changed row count is flagged", {
  env <- .lpjml_pins_env()

  row <- env$check_one(.pin_observed(n_rows = 6809324), .pin_expected())

  expect_equal(row$verdict, "DEVIATES")
  expect_match(row$detail, "rows 6809324 vs recorded 6809325")
})

test_that("a changed year span is flagged", {
  env <- .lpjml_pins_env()

  row <- env$check_one(.pin_observed(last_year = 2019), .pin_expected())

  expect_equal(row$verdict, "DEVIATES")
  expect_match(row$detail, "span 1901-2019 vs recorded 1901-2023")
})

# An impossibility violation says something different from baseline drift --
# a broken layer rather than a changed model -- so it must survive into the
# detail even when the magnitudes are untouched.
test_that("an impossible value is flagged as such, not as drift", {
  env <- .lpjml_pins_env()

  row <- env$check_one(
    .pin_observed(
      violations = tibble::tibble(
        column = "grass_npp",
        below = 3L,
        above = 0L,
        n_missing = 0L
      )
    ),
    .pin_expected()
  )

  expect_equal(row$verdict, "DEVIATES")
  expect_match(row$detail, "IMPOSSIBLE VALUES: grass_npp 3 below/0 above")
})

test_that("a pin missing a required column is a schema failure", {
  env <- .lpjml_pins_env()

  row <- env$check_one(
    list(
      alias = "lpjml-grass-productivity",
      fatal = "missing columns: grass_npp",
      n_rows = 6809325
    ),
    .pin_expected()
  )

  expect_equal(row$verdict, "SCHEMA")
  expect_match(row$detail, "missing columns: grass_npp")
})

test_that("a pin absent from the baseline is reported, not skipped", {
  env <- .lpjml_pins_env()

  row <- env$check_one(.pin_observed(), NULL)

  expect_equal(row$verdict, "NEW")
  expect_match(row$detail, "record it")
})

# The contract between the two files: `validate_all.R` folds this check into
# its scorecard by parsing one METRIC line, exactly as it does for
# `stability.R` and `nourishment_axis.R`. If either side of that line moves,
# the sweep silently scores the tripwire as unavailable -- the failure mode
# #559 exists to remove.
test_that("the METRIC line is the one validate_all.R parses", {
  env <- .lpjml_pins_env()
  table <- tibble::tibble(
    pin = c("a", "b", "c", "d", "e"),
    n_rows = c(1, 2, 3, 4, 5),
    verdict = c("ok", "ok", "DEVIATES", "SCHEMA", "NEW"),
    detail = c(
      "mean 1",
      "mean 2",
      "IMPOSSIBLE VALUES: swc_topsoil 0 below/7 above",
      "missing columns: year",
      "not in the baseline; record it"
    )
  )

  metric <- grep(
    "^METRIC",
    capture.output(env$emit_metric(table)),
    value = TRUE
  )
  # The parser `validate_all.R` applies, character for character.
  pin_num <- function(key) {
    as.numeric(sub(paste0(".*", key, "=([0-9]+).*"), "\\1", metric))
  }

  expect_length(metric, 1L)
  expect_equal(pin_num("pins_checked"), 5)
  expect_equal(pin_num("pins_ok"), 2)
  expect_equal(pin_num("deviating"), 1)
  expect_equal(pin_num("schema"), 2)
  expect_equal(pin_num("impossible"), 1)
})
