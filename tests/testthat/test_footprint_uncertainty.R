# run_fn that maps an extension vector to a single output cell
# whose value is the total extension.
.linear_run_fn <- function() {
  function(ext) {
    tibble::tibble(target_area = 1L, target_item = 10L, value = sum(ext))
  }
}

.rng_state <- function() {
  if (!exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
    return(NULL)
  }
  get(".Random.seed", envir = globalenv(), inherits = FALSE)
}

# Runs `code` as a session that has never drawn a random number. Putting the
# suite's own RNG state back is `with_preserve_seed()`'s job here; only the
# assertion inside `code` is under test.
.with_no_rng_state <- function(code) {
  withr::with_preserve_seed({
    suppressWarnings(rm(list = ".Random.seed", envir = globalenv()))
    force(code)
  })
}

.spread_run <- function(seed = NULL) {
  whep::propagate_fp_uncertainty(
    .linear_run_fn(),
    extensions = c(60, 40),
    cov = 0.2,
    options = list(n = 20, seed = seed)
  )
}

testthat::test_that("zero CoV gives a degenerate (zero-spread) interval", {
  res <- whep::propagate_fp_uncertainty(
    .linear_run_fn(),
    extensions = c(60, 40),
    cov = 0,
    options = list(n = 50, seed = 1)
  )

  res |>
    pointblank::expect_col_exists(
      c("mean", "sd", "cv", "q_low", "q_med", "q_high")
    )
  testthat::expect_equal(res$mean, 100)
  testthat::expect_equal(res$sd, 0)
  testthat::expect_equal(res$q_med, 100)
})

testthat::test_that("positive CoV produces a spread around the base", {
  res <- whep::propagate_fp_uncertainty(
    .linear_run_fn(),
    extensions = c(60, 40),
    cov = 0.2,
    options = list(n = 500, seed = 42)
  )

  testthat::expect_true(res$sd > 0)
  testthat::expect_true(res$q_low < res$q_high)
  # Lognormal factors are mean-one, so the MC mean tracks the base.
  testthat::expect_equal(res$mean, 100, tolerance = 0.05)
})

testthat::test_that("propagation is reproducible under a fixed seed", {
  run <- function() {
    whep::propagate_fp_uncertainty(
      .linear_run_fn(),
      extensions = c(60, 40),
      cov = 0.2,
      options = list(n = 100, seed = 7)
    )
  }
  testthat::expect_equal(run()$sd, run()$sd)
})

testthat::test_that("combine_cov adds components in quadrature", {
  testthat::expect_equal(whep::combine_cov(0.3, 0.4), 0.5)
  testthat::expect_equal(
    whep::combine_cov(c(0.3, 0.0), c(0.4, 0.5)),
    c(0.5, 0.5)
  )
  testthat::expect_error(whep::combine_cov(-0.1, 0.2), "non-negative")
})

testthat::test_that("sensitivity elasticity equals the contribution share", {
  sens <- whep::footprint_sensitivity(
    .linear_run_fn(),
    extensions = c(60, 40)
  )

  sens |> pointblank::expect_col_exists(c("sector", "elasticity"))
  by_sector <- sens |> dplyr::arrange(sector)
  # For a linear total, elasticity to sector i is its share.
  testthat::expect_equal(by_sector$elasticity, c(0.6, 0.4), tolerance = 1e-6)
})

testthat::test_that("a seeded run leaves the caller's RNG state untouched", {
  set.seed(999)
  before <- .rng_state()

  invisible(.spread_run(seed = 1))

  testthat::expect_identical(.rng_state(), before)
})

testthat::test_that("a seeded run does not create .Random.seed from nothing", {
  .with_no_rng_state({
    invisible(.spread_run(seed = 3))
    testthat::expect_null(.rng_state())
  })
})

testthat::test_that("a seeded run ignores the surrounding RNG state", {
  set.seed(1)
  first <- .spread_run(seed = 7)
  set.seed(2)
  second <- .spread_run(seed = 7)

  testthat::expect_equal(first, second)
})

testthat::test_that("an unseeded run keeps consuming the caller's stream", {
  # The seed is restored only when one was asked for. Restoring unconditionally
  # would make consecutive unseeded runs repeat one draw instead of taking
  # independent ones.
  set.seed(11)
  first <- .spread_run()
  second <- .spread_run()

  testthat::expect_false(isTRUE(all.equal(first$sd, second$sd)))
})

testthat::test_that("uncertainty rejects unknown options and non-functions", {
  testthat::expect_error(
    whep::propagate_fp_uncertainty(
      .linear_run_fn(),
      c(1, 2),
      options = list(bogus = 1)
    ),
    "Unknown option"
  )
  testthat::expect_error(
    whep::propagate_fp_uncertainty("not a function", c(1, 2)),
    "must be a function"
  )
})

.probs_run <- function(probs) {
  whep::propagate_fp_uncertainty(
    .linear_run_fn(),
    extensions = c(60, 40),
    cov = 0.2,
    options = list(n = 5, seed = 1, probs = probs)
  )
}

testthat::test_that("malformed probs abort before any draw is computed", {
  # A wrong length used to surface only as an NA q_high, after all n runs.
  testthat::expect_error(.probs_run(c(0.05, 0.95)), "three numbers")
  testthat::expect_error(.probs_run(c(0.025, NA, 0.975)), "three numbers")
  testthat::expect_error(.probs_run("a"), "three numbers")
  testthat::expect_error(.probs_run(c(-0.1, 0.5, 0.975)), "within")
  testthat::expect_error(.probs_run(c(0.025, 0.5, 1.5)), "within")
  # Descending probs used to yield q_low > q_high silently.
  testthat::expect_error(.probs_run(c(0.975, 0.5, 0.025)), "ascending")
})

testthat::test_that("valid custom probs are honoured", {
  res <- .probs_run(c(0.05, 0.5, 0.95))

  testthat::expect_false(is.na(res$q_high))
  testthat::expect_true(res$q_low <= res$q_med)
  testthat::expect_true(res$q_med <= res$q_high)
})
