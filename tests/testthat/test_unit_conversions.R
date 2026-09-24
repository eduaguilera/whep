test_that("conversion factors are the exact identities they name", {
  expect_identical(whep:::.kg_per_tonne(), 1000)
  expect_identical(whep:::.kg_per_teragram(), 1e9)
  expect_identical(whep:::.persons_per_thousand(), 1000)
  # Tg -> t -> kg must compose to Tg -> kg exactly.
  expect_identical(1e6 * whep:::.kg_per_tonne(), whep:::.kg_per_teragram())
})

test_that("bound violations are counted by kind without touching x", {
  x <- c(-1, 0, 0.5, 1, 2, NA, NaN, Inf, -Inf)
  x_before <- x
  diagnostic <- whep:::.bound_violations(x, lower = 0, upper = 1)
  expect_identical(x, x_before)
  expect_identical(diagnostic$n_values, 9L)
  expect_identical(diagnostic$n_missing, 2L)
  expect_identical(diagnostic$n_non_finite, 2L)
  expect_identical(diagnostic$n_below, 1L)
  expect_identical(diagnostic$n_above, 1L)
})

test_that("boundary values themselves are in range", {
  diagnostic <- whep:::.bound_violations(c(0, 1), lower = 0, upper = 1)
  expect_identical(diagnostic$n_below + diagnostic$n_above, 0L)
})

test_that("empty input yields zero counts", {
  diagnostic <- whep:::.bound_violations(numeric(0), lower = 0)
  expect_identical(diagnostic$n_values, 0L)
  expect_identical(diagnostic$n_below, 0L)
})

test_that("an out-of-range value warns and returns the diagnostic", {
  expect_warning(
    diagnostic <- whep:::.warn_out_of_bounds(c(-2, 3), "q", lower = 0),
    class = "whep_out_of_bounds_warning"
  )
  expect_identical(diagnostic$n_below, 1L)
})

test_that("an infinite value warns; a missing value does not", {
  expect_warning(
    whep:::.warn_out_of_bounds(c(1, Inf), "q", lower = 0),
    class = "whep_out_of_bounds_warning"
  )
  expect_no_warning(whep:::.warn_out_of_bounds(c(1, NA), "q", lower = 0))
})

test_that("a malformed bound rule aborts rather than guessing", {
  expect_error(
    whep:::.bound_violations("1", lower = 0),
    class = "whep_bound_rule_error"
  )
  expect_error(
    whep:::.bound_violations(1, lower = 2, upper = 1),
    class = "whep_bound_rule_error"
  )
  expect_error(
    whep:::.bound_violations(1, lower = c(0, 1)),
    class = "whep_bound_rule_error"
  )
  expect_error(
    whep:::.bound_violations(1, lower = NA_real_),
    class = "whep_bound_rule_error"
  )
})

test_that("a negative per-capita N total warns and is kept unchanged", {
  anthropogenic <- tibble::tribble(
    ~year, ~area_code, ~anthropogenic_n_t,
    2000L,        10L,               -5,
    2000L,        20L,               10
  )
  population <- tibble::tribble(
    ~year, ~area_code, ~population,
    2000L,        10L,        1000,
    2000L,        20L,        1000
  )
  expect_warning(
    out <- whep:::.n_percapita_per_capita(anthropogenic, population),
    class = "whep_out_of_bounds_warning"
  )
  expect_identical(out$n_percapita_kg, c(-5, 10))
})

test_that("a negative pathway pressure warns and is kept unchanged", {
  x <- tibble::tibble(
    actual_air_kgn_ha = c(-1, 5),
    critical_air_kgn_ha = c(2, 2),
    area_ha = c(10, 10)
  )
  expect_warning(
    out <- whep:::.npb_decompose_medium(x, "air"),
    class = "whep_out_of_bounds_warning"
  )
  expect_identical(out$actual_air_n_t, c(-1, 5) * 10 / 1000)
})
