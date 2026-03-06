# compute_leontief_inverse -------------------------------------------

testthat::test_that(
  "compute_leontief_inverse works for 2-sector model",
  {
    z <- matrix(
      c(10, 20, 15, 5), nrow = 2, byrow = TRUE
    )
    x <- c(100, 200)
    l_inv <- compute_leontief_inverse(z, x)

    testthat::expect_true(is.matrix(l_inv))
    testthat::expect_equal(dim(l_inv), c(2, 2))
    testthat::expect_true(all(l_inv >= 0))
  }
)

testthat::test_that("Leontief inverse satisfies L(I-A) = I", {
  z <- matrix(
    c(10, 20, 15, 5), nrow = 2, byrow = TRUE
  )
  x <- c(100, 200)

  l_inv <- compute_leontief_inverse(z, x)
  x_inv <- ifelse(x == 0, 0, 1 / x)
  a <- t(t(z) * x_inv)

  testthat::expect_equal(
    l_inv %*% (diag(2) - a), diag(2),
    tolerance = 1e-10
  )
})

testthat::test_that(
  "Leontief handles zero output sectors",
  {
    z <- matrix(c(0, 0, 5, 0), nrow = 2)
    x <- c(0, 100)
    l_inv <- compute_leontief_inverse(z, x)

    testthat::expect_true(is.matrix(l_inv))
    testthat::expect_true(all(l_inv >= 0))
    testthat::expect_equal(l_inv[1, 1], 1)
  }
)

testthat::test_that("Leontief validates inputs", {
  testthat::expect_error(
    compute_leontief_inverse("not a matrix", c(1, 2)),
    "must be a matrix"
  )
  testthat::expect_error(
    compute_leontief_inverse(
      matrix(1, nrow = 2, ncol = 3), c(1, 2)
    ),
    "must be a square matrix"
  )
  testthat::expect_error(
    compute_leontief_inverse(
      matrix(0, 2, 2), c(1, 2, 3)
    ),
    "must match"
  )
})

testthat::test_that(
  "3-sector model y can be recovered from L",
  {
    z <- matrix(
      c(5, 10, 0, 3, 2, 8, 1, 4, 6),
      nrow = 3, byrow = TRUE
    )
    y <- c(85, 87, 89)
    x <- rowSums(z) + y

    l_inv <- compute_leontief_inverse(z, x)
    x_recovered <- l_inv %*% y

    testthat::expect_equal(
      as.vector(x_recovered), x,
      tolerance = 1e-8
    )
  }
)
