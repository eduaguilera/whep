# The upstream-coefficient tripwire, `validation/upstream_coefs.R` (#524).
#
# What is exercised is the COMPARISON, driven by hand-built frames. Reading the
# workbook is what `Rscript validation/upstream_coefs.R` does, and it lives in
# another repository, which is precisely what the test suite may not touch
# (#490).
#
# `validation/` is in `.Rbuildignore`, so these skip under `R CMD check` and run
# for real in `offline-tests.yaml`, which checks the repository out and calls
# `devtools::test()` against the source tree.

.upstream_coefs_env <- function() {
  path <- testthat::test_path("..", "..", "validation", "upstream_coefs.R")
  testthat::skip_if_not(
    file.exists(path),
    "validation/ is .Rbuildignore'd, so it is absent from a built package"
  )
  env <- new.env(parent = globalenv())
  # Sourcing runs nothing: the package load at the head and the `main()` call
  # at the foot are both guarded on `sys.nframe() == 0L`.
  source(path, local = env)
  env
}

test_that("identical columns report no differences", {
  env <- .upstream_coefs_env()

  expect_equal(env$cell_differences(c(1, 2, 3), c(1, 2, 3)), 0L)
  expect_equal(env$cell_differences(c("a", "b"), c("a", "b")), 0L)
  expect_equal(env$cell_differences(c(1, NA), c(1, NA)), 0L)
})

test_that("a changed coefficient is counted", {
  env <- .upstream_coefs_env()

  expect_equal(env$cell_differences(c(1, 2, 3), c(1, 2.5, 3)), 1L)
  expect_equal(env$cell_differences(c("a", "b"), c("a", "c")), 1L)
  # One side missing where the other is not is a difference, not a match.
  expect_equal(env$cell_differences(c(1, NA), c(1, 2)), 1L)
})

# The headline reason this compares numerically. #524's own caveat: a string
# comparison reports ~190 phantom differences on this workbook, which drowns
# the five real columns in noise.
test_that("a double round-trip is not mistaken for an edit", {
  env <- .upstream_coefs_env()

  expect_equal(
    env$cell_differences("0.56000000000000005", "0.56"),
    0L
  )
  expect_equal(env$cell_differences(0.1 + 0.2, 0.3), 0L)
})

# `Equiv` is a crop name in most rows and a bare number in two of them. A
# column-level "is this numeric" test falls through to text for the whole
# column, and then the two numeric rows read as differences forever.
test_that("a mixed text-and-number column compares per cell", {
  env <- .upstream_coefs_env()
  upstream <- c("Wheat", "Maize", "0.56000000000000005", NA)
  packaged <- c("Wheat", "Maize", "0.56", NA)

  expect_equal(env$cell_differences(upstream, packaged), 0L)
  # ... while a real edit in the same column still counts.
  expect_equal(
    env$cell_differences(upstream, c("Wheat", "Barley", "0.56", NA)),
    1L
  )
})

test_that("the tolerance is what it says it is", {
  env <- .upstream_coefs_env()

  expect_equal(env$CELL_TOL, 1e-6)
  expect_equal(env$cell_differences(1, 1 + 1e-7), 0L)
  expect_equal(env$cell_differences(1, 1 + 1e-5), 1L)
})

test_that("changed_columns names only the columns that moved", {
  env <- .upstream_coefs_env()
  upstream <- tibble::tibble(
    key = c("a", "b", "c"),
    stable = c(1, 2, 3),
    moved = c(1, 2, 3),
    upstream_only = c(9, 9, 9)
  )
  packaged <- tibble::tibble(
    key = c("a", "b", "c"),
    stable = c(1, 2, 3),
    moved = c(1, 2, 4)
  )

  changed <- env$changed_columns(upstream, packaged)

  expect_named(changed, "moved")
  expect_equal(unname(changed[["moved"]]), 1L)
})

test_that("a workbook that cannot be found is reported, not passed", {
  env <- .upstream_coefs_env()
  spec <- list(
    name = "bogus",
    workbook = "inst/extdata/DoesNotExist.xlsx",
    sheet = "Coefs",
    skip = 1,
    packaged = function() whep::biomass_coefs,
    rows = 421L
  )

  observed <- env$measure_source(spec)
  row <- env$check_one(observed, list(md5 = "irrelevant"))

  # The failure mode this whole script exists to remove is a quiet pass.
  expect_equal(row$verdict, "UNAVAILABLE")
  expect_match(row$detail, "DoesNotExist")
})

.upstream_observed <- function(...) {
  utils::modifyList(
    list(
      name = "biomass_coefs",
      unavailable = NULL,
      path = "somewhere/Biomass_coefs.xlsx",
      md5 = "3f0c4f2270ad103608fc7429c7cab741",
      upstream_rows = 421L,
      packaged_rows = 421L,
      expected_rows = 421L,
      only_upstream = character(),
      only_packaged = character(),
      changed = integer()
    ),
    list(...)
  )
}

.upstream_expected <- function(...) {
  utils::modifyList(
    list(
      md5 = "3f0c4f2270ad103608fc7429c7cab741",
      only_upstream = list(),
      only_packaged = list(),
      changed = list()
    ),
    list(...)
  )
}

test_that("a workbook matching its baseline passes", {
  env <- .upstream_coefs_env()

  row <- env$check_one(.upstream_observed(), .upstream_expected())

  expect_equal(row$verdict, "ok")
})

# The earliest available signal: the workbook's own checksum moves on any edit,
# including one that touches no shared column.
test_that("an edited workbook is flagged by its checksum alone", {
  env <- .upstream_coefs_env()

  row <- env$check_one(
    .upstream_observed(md5 = strrep("0", 32)),
    .upstream_expected()
  )

  expect_equal(row$verdict, "DRIFTED")
  expect_match(row$detail, "workbook md5")
})

test_that("a newly differing column is named", {
  env <- .upstream_coefs_env()

  row <- env$check_one(
    .upstream_observed(changed = c(Root_kgC_kgDM = 23L)),
    .upstream_expected()
  )

  expect_equal(row$verdict, "DRIFTED")
  expect_match(row$detail, "Root_kgC_kgDM \\(23 cells\\)")
})

# Drift back into agreement is still drift: it means someone changed something
# without re-recording, and the baseline no longer describes the repository.
test_that("a column that agrees again is flagged too", {
  env <- .upstream_coefs_env()

  row <- env$check_one(
    .upstream_observed(),
    .upstream_expected(changed = list(Root_kgC_kgDM = 23L))
  )

  expect_equal(row$verdict, "DRIFTED")
  expect_match(row$detail, "agree again")
})

# A changed row count breaks the row-aligned comparison, so it has to be said
# on its own rather than surfacing as a pile of meaningless column diffs.
test_that("a changed row count is reported as invalidating, not as diffs", {
  env <- .upstream_coefs_env()

  row <- env$check_one(
    .upstream_observed(upstream_rows = 430L),
    .upstream_expected()
  )

  expect_equal(row$verdict, "DRIFTED")
  expect_match(row$detail, "ROW COUNT")
})

test_that("a column appearing or vanishing upstream is flagged", {
  env <- .upstream_coefs_env()

  row <- env$check_one(
    .upstream_observed(only_upstream = "New_Column"),
    .upstream_expected()
  )

  expect_equal(row$verdict, "DRIFTED")
  expect_match(row$detail, "upstream-only columns: New_Column")
})

# The contract with validate_all.R, which folds this in by parsing one line.
test_that("the METRIC line is the one validate_all.R parses", {
  env <- .upstream_coefs_env()
  table <- tibble::tibble(
    source = c("a", "b", "c", "d"),
    verdict = c("ok", "DRIFTED", "UNAVAILABLE", "UNAVAILABLE"),
    detail = c("", "", "", "")
  )

  metric <- grep(
    "^METRIC",
    capture.output(env$emit_metric(table)),
    value = TRUE
  )
  coef_num <- function(key) {
    as.numeric(sub(paste0(".*", key, "=([0-9]+).*"), "\\1", metric))
  }

  expect_length(metric, 1L)
  expect_equal(coef_num("sources_checked"), 4)
  expect_equal(coef_num("sources_ok"), 1)
  expect_equal(coef_num("drifted"), 1)
  expect_equal(coef_num("unavailable"), 2)
})
