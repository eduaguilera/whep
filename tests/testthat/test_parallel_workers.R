testthat::test_that(".core_limit_in_force reads the check variable", {
  withr::local_envvar(c("_R_CHECK_LIMIT_CORES_" = ""))
  testthat::expect_false(.core_limit_in_force())

  withr::local_envvar(c("_R_CHECK_LIMIT_CORES_" = "false"))
  testthat::expect_false(.core_limit_in_force())

  withr::local_envvar(c("_R_CHECK_LIMIT_CORES_" = "FALSE"))
  testthat::expect_false(.core_limit_in_force())

  purrr::walk(c("TRUE", "true", "warn", "1", "yes"), function(value) {
    withr::local_envvar(c("_R_CHECK_LIMIT_CORES_" = value))
    testthat::expect_true(.core_limit_in_force())
  })
})

testthat::test_that(".parallel_workers clamps to 2 under the check limit", {
  testthat::skip_on_os("windows")
  withr::local_envvar(c("_R_CHECK_LIMIT_CORES_" = "TRUE"))

  # The clamp is what keeps `parallel:::.check_ncores()` from aborting, so
  # it must hold for the derived default as well as for any explicit ask.
  testthat::expect_lte(.parallel_workers(), 2L)
  purrr::walk(c(1L, 2L, 3L, 8L, 64L), function(requested) {
    testthat::expect_lte(.parallel_workers(requested), 2L)
  })
  testthat::expect_equal(.parallel_workers(1L), 1L)
})

testthat::test_that(".parallel_workers honours the ask when unlimited", {
  testthat::skip_on_os("windows")
  withr::local_envvar(c("_R_CHECK_LIMIT_CORES_" = "false"))

  testthat::expect_equal(.parallel_workers(1L), 1L)
  testthat::expect_equal(.parallel_workers(7L), 7L)

  detected <- parallel::detectCores()
  testthat::skip_if(is.na(detected), "detectCores() cannot tell")
  testthat::expect_equal(.parallel_workers(), max(1L, detected %/% 2L))
})

testthat::test_that(".parallel_workers always returns one usable count", {
  withr::local_envvar(c("_R_CHECK_LIMIT_CORES_" = "false"))

  purrr::walk(
    list(0L, -3L, NA_integer_, NA, integer(0), c(4L, 5L)),
    function(requested) {
      workers <- .parallel_workers(requested)
      testthat::expect_length(workers, 1L)
      testthat::expect_true(is.integer(workers))
      testthat::expect_gte(workers, 1L)
    }
  )
})

testthat::test_that(".parallel_workers runs serially on Windows", {
  testthat::local_mocked_bindings(.is_windows = function() TRUE)

  testthat::expect_equal(.parallel_workers(), 1L)
  testthat::expect_equal(.parallel_workers(16L), 1L)
})
