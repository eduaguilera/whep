# compute_leontief_inverse -------------------------------------------

testthat::test_that("compute_leontief_inverse works for 2-sector model", {
  z <- matrix(
    c(10, 20, 15, 5),
    nrow = 2,
    byrow = TRUE
  )
  x <- c(100, 200)
  l_inv <- compute_leontief_inverse(z, x)

  testthat::expect_true(is.matrix(l_inv))
  testthat::expect_equal(dim(l_inv), c(2, 2))
  testthat::expect_true(all(l_inv >= 0))
})

testthat::test_that("Leontief inverse satisfies L(I-A) = I", {
  z <- matrix(
    c(10, 20, 15, 5),
    nrow = 2,
    byrow = TRUE
  )
  x <- c(100, 200)

  l_inv <- compute_leontief_inverse(z, x)
  x_inv <- ifelse(x == 0, 0, 1 / x)
  a <- t(t(z) * x_inv)

  testthat::expect_equal(
    l_inv %*% (diag(2) - a),
    diag(2),
    tolerance = 1e-10
  )
})

testthat::test_that("Leontief handles zero output sectors", {
  z <- matrix(c(0, 0, 5, 0), nrow = 2)
  x <- c(0, 100)
  l_inv <- compute_leontief_inverse(z, x)

  testthat::expect_true(is.matrix(l_inv))
  testthat::expect_true(all(l_inv >= 0))
  testthat::expect_equal(l_inv[1, 1], 1)
})

testthat::test_that("Leontief validates inputs", {
  testthat::expect_error(
    compute_leontief_inverse("not a matrix", c(1, 2)),
    "must be a matrix"
  )
  testthat::expect_error(
    compute_leontief_inverse(
      matrix(1, nrow = 2, ncol = 3),
      c(1, 2)
    ),
    "must be a square matrix"
  )
  testthat::expect_error(
    compute_leontief_inverse(
      matrix(0, 2, 2),
      c(1, 2, 3)
    ),
    "must match"
  )
})

testthat::test_that("compute_leontief_inverse aborts for large n", {
  z_big <- Matrix::sparseMatrix(
    i = 1:10,
    j = 1:10,
    x = rep(1, 10),
    dims = c(10, 10)
  )
  x_big <- rep(100, 10)
  testthat::expect_error(
    compute_leontief_inverse(z_big, x_big, max_n = 5),
    "System too large"
  )
})

testthat::test_that("A column capping prevents singularity when inputs exceed output", {
  # z col 2 sums to 14 against x = 10, so A col 2 > 1
  z <- matrix(c(5, 6, 3, 8), nrow = 2, byrow = TRUE)
  x <- c(10, 10)
  l_inv <- NULL
  testthat::expect_warning(
    {
      l_inv <- compute_leontief_inverse(z, x)
    },
    "Capping"
  )

  testthat::expect_true(is.matrix(l_inv))
  testthat::expect_true(all(l_inv >= 0))
})

testthat::test_that("A column capping leaves explicit value-added leakage", {
  z <- matrix(c(6, 0, 4, 0), nrow = 2, byrow = TRUE)
  x <- c(10, 10)
  value_added_floor <- 1e-3

  testthat::expect_warning(
    {
      a <- .technical_coefficients(
        z,
        x,
        value_added_floor = value_added_floor
      )
    },
    "Capping"
  )

  testthat::expect_equal(
    Matrix::colSums(a)[1],
    1 - value_added_floor,
    tolerance = 1e-12
  )
  testthat::expect_lt(Matrix::colSums(a)[1], 1)
})

testthat::test_that("3-sector model y can be recovered from L", {
  z <- matrix(
    c(5, 10, 0, 3, 2, 8, 1, 4, 6),
    nrow = 3,
    byrow = TRUE
  )
  y <- c(85, 87, 89)
  x <- rowSums(z) + y

  l_inv <- compute_leontief_inverse(z, x)
  x_recovered <- l_inv %*% y

  testthat::expect_equal(
    as.vector(x_recovered),
    x,
    tolerance = 1e-8
  )
})

testthat::test_that("Leontief validates value_added_floor", {
  testthat::expect_error(
    compute_leontief_inverse(
      matrix(0, 2, 2),
      c(1, 2),
      value_added_floor = 1
    ),
    "value_added_floor"
  )
})

testthat::test_that("min_output zeroes A columns of residue outputs (#1110)", {
  # The 2x2 case from issue whep#1110: sector 2's output 1e-12 is residue.
  z <- matrix(c(0, 0, 5, 0), nrow = 2)
  x <- c(100, 1e-12)

  testthat::expect_warning(
    divided <- .technical_coefficients(z, x, max_column_sum = 100),
    "Capping"
  )
  testthat::expect_equal(unname(Matrix::colSums(divided)), c(0, 100))

  traced <- testthat::expect_no_warning(
    .technical_coefficients(z, x, max_column_sum = 100, min_output = 1e-8)
  )
  testthat::expect_equal(unname(Matrix::colSums(traced)), c(0, 0))
  testthat::expect_equal(
    as.matrix(Matrix::solve(Matrix::Diagonal(2) - traced)),
    diag(2)
  )

  # Outputs above the threshold are divided exactly as before.
  x_ok <- c(100, 10)
  testthat::expect_equal(
    .technical_coefficients(z, x_ok, max_column_sum = 100, min_output = 1e-8),
    .technical_coefficients(z, x_ok, max_column_sum = 100)
  )
})

testthat::test_that("residue-output warning counts only columns with inputs", {
  z <- matrix(c(0, 0, 5, 0), nrow = 2)

  testthat::expect_equal(
    .warn_residue_output_inputs(z, c(100, 1e-3), 1e-8, "traceable"),
    0L
  )
  testthat::expect_equal(
    .warn_residue_output_inputs(z * 0, c(100, 1e-12), 1e-8, "traceable"),
    0L
  )
  testthat::expect_warning(
    n <- .warn_residue_output_inputs(z, c(100, 1e-12), 1e-8, "traceable"),
    "not traced",
    class = "whep_residue_output_inputs"
  )
  testthat::expect_equal(n, 1L)
  testthat::expect_warning(
    .warn_residue_output_inputs(z, c(100, 1e-12), 1e-8, "nonzero"),
    "over-trace",
    class = "whep_residue_output_inputs"
  )
})
