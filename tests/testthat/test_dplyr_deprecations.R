# Package-wide guard, not a per-script test file. dplyr 1.2.0 soft-deprecated
# `case_match()` in favour of `recode_values()`. `lifecycle` escalates a
# soft-deprecation to warn on EVERY call while testthat is running, so it is
# nearly silent in a plain build and floods the test log instead: measured on
# dplyr 1.2.1, the five `case_match()` sites this guard was written for
# produced 97 of the 140 warnings raised by test_build_production.R,
# test_n_balance*.R and test_energy_co2_extension.R alone (whep#850). That
# noise is not cosmetic -- in whep#843 it made an unrelated
# `expect_no_warning()` assertion fail.
#
# The check scans the namespace rather than the dplyr version, so it holds on
# any installed dplyr: a reintroduced call fails here on 1.1.4 too, years
# before the removal that would otherwise surface it as a hard error.

# dplyr functions that warn when called, as of dplyr 1.2.1. `recode()` is
# deliberately absent: it is superseded, not deprecated, and does not warn.
.deprecated_dplyr_calls <- function() {
  c(
    "case_match",
    "cur_data",
    "cur_data_all",
    "all_equal",
    "progress_estimated"
  )
}

testthat::test_that("no function calls a deprecated dplyr verb", {
  testthat::expect_equal(
    .whep_callers_of(.deprecated_dplyr_calls()),
    character()
  )
})

testthat::test_that("the deprecation scanner is not vacuous", {
  # Guards the guard: an empty result above must mean "no offenders", not
  # "scanner broken". The probe mimics the real call shape, namespaced.
  probe <- function(x) {
    dplyr::case_match(x, "a" ~ 1)
  }

  testthat::expect_equal(
    intersect(.fun_call_names(probe), .deprecated_dplyr_calls()),
    "case_match"
  )
})
