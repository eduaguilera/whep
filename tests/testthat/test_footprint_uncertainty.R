# run_fn that maps an extension vector to a single output cell
# whose value is the total extension.
.linear_run_fn <- function() {
  function(ext) {
    tibble::tibble(target_area = 1L, target_item = 10L, value = sum(ext))
  }
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
