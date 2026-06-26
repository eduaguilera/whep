testthat::test_that("balance_ras hits the target margins", {
  m <- matrix(c(1, 2, 3, 4), nrow = 2)
  b <- whep::balance_ras(m, target_rows = c(10, 20), target_cols = c(12, 18))

  testthat::expect_equal(rowSums(b), c(10, 20), tolerance = 1e-6)
  testthat::expect_equal(colSums(b), c(12, 18), tolerance = 1e-6)
  # zero pattern preserved (no structural fill)
  testthat::expect_true(all((b == 0) == (m == 0)))
})

testthat::test_that("balance_ras validates inputs", {
  testthat::expect_error(
    whep::balance_ras(matrix(c(-1, 2, 3, 4), 2), c(1, 1), c(1, 1)),
    "non-negative"
  )
  testthat::expect_error(
    whep::balance_ras(matrix(1:4, 2), c(1, 1, 1), c(1, 1)),
    "dimensions"
  )
  testthat::expect_error(
    whep::balance_ras(matrix(1:4, 2), c(10, 20), c(1, 1)),
    "must equal"
  )
})

testthat::test_that("balance_ras warns when it cannot converge", {
  # a structural zero row with a positive target can never be filled
  m <- matrix(c(0, 0, 1, 1), nrow = 2)
  testthat::expect_warning(
    whep::balance_ras(m, c(5, 5), c(5, 5), max_iter = 50L),
    "did not converge"
  )
})

testthat::test_that("balance_io_flows yields a conserving system", {
  # connected (fully positive) flows so the margins are feasible
  z_mat <- matrix(c(1, 4, 2, 3), nrow = 2)
  x_vec <- c(10, 10)
  y_mat <- c(3, 3)
  zb <- whep::balance_io_flows(z_mat, x_vec, y_mat)

  # row balance: intermediate use equals output minus final demand
  testthat::expect_equal(rowSums(zb), x_vec - y_mat, tolerance = 1e-6)
  # conservation identity: (I - A) x == y, so the footprint conserves
  a_mat <- zb %*% diag(1 / x_vec)
  testthat::expect_equal(
    as.numeric((diag(2) - a_mat) %*% x_vec),
    y_mat,
    tolerance = 1e-6
  )
})

testthat::test_that("balance_io_flows accepts a final-demand matrix", {
  z_mat <- matrix(c(1, 4, 2, 3), nrow = 2)
  x_vec <- c(10, 10)
  y_mat <- matrix(c(1, 2, 2, 1), nrow = 2) # row sums c(3, 3)
  zb <- whep::balance_io_flows(z_mat, x_vec, y_mat)

  testthat::expect_equal(rowSums(zb), x_vec - c(3, 3), tolerance = 1e-6)
})
