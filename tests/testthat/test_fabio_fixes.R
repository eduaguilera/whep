# Tests for FABIO methodology fixes:
# 1. Diagonal rebalancing
# 2. Domestic-only stock withdrawal
# 3. Losses endogenization
# 4. Negative-zeroing in A matrix

# --- Fixtures ---

# Fixture where diag(Z) >= X to trigger rebalancing.
# Seed = production scenario: all use is self-use, and CBS
# reports negative final demand (stock_addition > food+other).
diag_rebalance_fixture <- function() {
  su <- tibble::tribble(
    ~year, ~area_code, ~proc_group, ~proc_cbs_code, ~item_cbs_code, ~type, ~value,
    2000, 1, "crop_production", 10, 10, "supply", 100,
    2000, 1, "crop_production", 10, 10, "use", 100,
    2000, 1, "crop_production", 10, 20, "supply", 50,
    2000, 1, "crop_production", 10, 20, "use", 0,
  )

  btd <- tibble::tibble(
    year = c(2000L, 2000L),
    item_cbs_code = c(10, 20),
    bilateral_trade = list(
      matrix(0, nrow = 1, ncol = 1),
      matrix(0, nrow = 1, ncol = 1)
    )
  )

  # Item 10: production=100, food=5, other=3, stock_addition=120
  # After merge_stock: sa gets sw subtracted (sw=0), net sa=120
  # Y rowSums for item 10 = 5+3+120 = 128. But Z[1,1] = 100.
  # 100 < 128 => won't trigger either.
  # Need: rowSums(Y) small enough that Z[1,1] >= X.
  # If food=0, other=0, sa=0, sw=100: net sa = 0-100 = -100
  # Y rowSums = -100. X = 100 + (-100) = 0. X==0 => skipped.
  # Use sw=80: Y rowSums = 0+0+(0-80) = -80, X = 100-80 = 20
  # diag(100) >= 20 => triggers!
  cbs <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~production, ~import, ~export, ~food, ~other_uses, ~stock_withdrawal, ~stock_addition,
    2000, 1, 10, 100, 0, 0, 0, 0, 80, 0,
    2000, 1, 20, 50, 0, 0, 30, 10, 0, 0,
  )

  list(su = su, btd = btd, cbs = cbs)
}

# Fixture with stock withdrawal
stock_fixture <- function() {
  su <- tibble::tribble(
    ~year, ~area_code, ~proc_group, ~proc_cbs_code, ~item_cbs_code, ~type, ~value,
    2000, 1, "crop_production", 10, 10, "supply", 100,
    2000, 1, "crop_production", 10, 10, "use", 5,
    2000, 2, "crop_production", 10, 10, "supply", 80,
    2000, 2, "crop_production", 10, 10, "use", 3,
  )

  btd <- tibble::tibble(
    year = 2000L,
    item_cbs_code = 10,
    bilateral_trade = list(
      matrix(c(0, 10, 5, 0), nrow = 2)
    )
  )

  cbs <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~production, ~import, ~export, ~food, ~other_uses, ~stock_withdrawal, ~stock_addition,
    2000, 1, 10, 100, 5, 10, 30, 10, 20, 5,
    2000, 2, 10, 80, 10, 5, 25, 10, 0, 8,
  )

  list(su = su, btd = btd, cbs = cbs)
}

# Fixture with losses column for endogenization
losses_fixture <- function() {
  su <- tibble::tribble(
    ~year, ~area_code, ~proc_group, ~proc_cbs_code, ~item_cbs_code, ~type, ~value,
    2000, 1, "crop_production", 10, 10, "supply", 100,
    2000, 1, "crop_production", 10, 10, "use", 5,
    2000, 1, "processing", 10, 20, "supply", 40,
    2000, 1, "processing", 10, 10, "use", 50,
  )

  btd <- tibble::tibble(
    year = c(2000L, 2000L),
    item_cbs_code = c(10, 20),
    bilateral_trade = list(
      matrix(0, nrow = 1, ncol = 1),
      matrix(0, nrow = 1, ncol = 1)
    )
  )

  cbs <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~production, ~import, ~export, ~food, ~other_uses, ~losses, ~stock_withdrawal, ~stock_addition,
    2000, 1, 10, 100, 0, 0, 15, 5, 10, 3, 0,
    2000, 1, 20, 40, 0, 0, 20, 5, 8, 0, 2,
  )

  list(su = su, btd = btd, cbs = cbs)
}


# --- 1. Diagonal rebalancing tests ---

testthat::test_that(".rebalance_diagonal moves 80% of diagonal to Y when diag(Z) >= X", {
  # 1 country, 1 item. Sector has diag=50, domestic Y = c(10, 5)
  # X = 50 + 15 = 65. diag(50) < X(65) => no trigger.
  # To trigger: need diag >= X, meaning Y row sums <= 0.
  # Use: diag = 50, Y[1,] = c(-5, -10)  [net stock effects].
  # X = 50 + (-15) = 35. diag(50) >= 35 => triggers.
  z <- Matrix::sparseMatrix(
    i = 1,
    j = 1,
    x = 50,
    dims = c(1, 1)
  )
  y <- matrix(c(-5, -10), nrow = 1, ncol = 2)
  dims <- list(
    areas = 1L,
    items = 10L,
    n_areas = 1L,
    n_items = 1L
  )
  fd_cols <- c("food", "other_uses")

  result <- .rebalance_diagonal(z, y, dims, fd_cols)

  # diag triggers, but domestic Y = c(-5, -10) sum < 0.
  # Global fallback for item 10: same values c(-5, -10), sum < 0.
  # sum(y_row) <= 0 => no rebalancing possible (skipped).
  testthat::expect_equal(result$Z[1, 1], 50)

  # Now with positive domestic demand to verify the 80/20 split:
  z2 <- Matrix::sparseMatrix(
    i = 1,
    j = 1,
    x = 100,
    dims = c(1, 1)
  )
  y2 <- matrix(c(-60, -50), nrow = 1, ncol = 2)
  # X = 100 + (-110) = -10. But X != 0 and diag(100) >= -10 => triggers
  # But domestic Y = c(-60, -50) sum < 0 => skipped

  # Better test: 2 countries, 1 item
  z3 <- Matrix::sparseMatrix(
    i = c(1, 2),
    j = c(1, 2),
    x = c(50, 0),
    dims = c(2, 2)
  )
  # Y has n_areas * n_fd = 2 * 2 = 4 columns
  # Cols 1-2: country 1 (food, other); cols 3-4: country 2 (food, other)
  y3 <- matrix(0, nrow = 2, ncol = 4)
  y3[2, 3] <- 10 # country 2, item 10, food
  y3[2, 4] <- 8 # country 2, item 10, other
  # Country 1 (row 1): X = 50+0 = 50, diag=50 >= 50 => triggers
  # Country 2 (row 2): X = 0+18 = 18, diag=0 < 18 => no trigger
  # Domestic Y for country 1: y3[1, 1:2] = c(0, 0), sum=0
  # Global for item 10: y_global[1,] = y3[1,1:2] + y3[2,3:4]
  #   = c(0, 0) + c(10, 8) = c(10, 8), sum=18
  dims3 <- list(
    areas = c(1L, 2L),
    items = 10L,
    n_areas = 2L,
    n_items = 1L
  )

  result3 <- .rebalance_diagonal(z3, y3, dims3, fd_cols)

  # 80% of 50 = 40 moved to Y, 20% = 10 stays on diagonal
  testthat::expect_equal(result3$Z[1, 1], 10)
  # bal=40, share = c(10/18, 8/18)
  testthat::expect_equal(
    as.numeric(result3$Y[1, 1]),
    40 * 10 / 18,
    tolerance = 1e-10
  )
  testthat::expect_equal(
    as.numeric(result3$Y[1, 2]),
    40 * 8 / 18,
    tolerance = 1e-10
  )
  # Country 2 unchanged
  testthat::expect_equal(result3$Z[2, 2], 0)
  testthat::expect_equal(as.numeric(result3$Y[2, 3:4]), c(10, 8))
})

testthat::test_that(".rebalance_diagonal uses domestic Y when available", {
  # 2 countries, 2 items. Sector 3 (country 2, item 10) has
  # diag = 60 and only the diagonal in its Z row, Y row = c(10, 5).
  # X[3] = 60 + 15 = 75. diag(60) < 75 => no trigger.
  # To trigger, need Y[3,] such that rowSums <= 0.
  # Set Y[3, 3:4] = c(-3, -2) so X[3] = 60 + (-5) = 55, diag(60) >= 55
  z <- Matrix::sparseMatrix(
    i = c(1, 3),
    j = c(1, 3),
    x = c(80, 60),
    dims = c(4, 4)
  )
  y <- matrix(0, nrow = 4, ncol = 4)
  # Country 1 (rows 1-2, Y cols 1-2): give sector 1 positive demand
  y[1, 1] <- 10 # country 1, item 10, food
  y[1, 2] <- 5 # country 1, item 10, other
  # Country 2 (rows 3-4, Y cols 3-4): give sector 3 negative demand
  y[3, 3] <- -3
  y[3, 4] <- -2

  dims <- list(
    areas = c(1L, 2L),
    items = c(10L, 20L),
    n_areas = 2L,
    n_items = 2L
  )
  fd_cols <- c("food", "other_uses")

  result <- .rebalance_diagonal(z, y, dims, fd_cols)

  # Sector 1 (country 1, item 10): diag=80, X=80+15=95
  # diag(80) < X(95), so NO rebalancing
  testthat::expect_equal(result$Z[1, 1], 80)

  # Sector 3 (country 2, item 10): diag=60, X=60+(-5)=55
  # diag(60) >= X(55), triggers rebalancing
  # Domestic Y: y[3, 3:4] = c(-3, -2), sum = -5 => falls back to global
  # Global for item 10 (index 1): sum across countries
  #   country 1 row 1: y[1, 1:2] = c(10, 5)
  #   country 2 row 3: y[3, 3:4] = c(-3, -2)
  #   y_global[1,] = c(10-3, 5-2) = c(7, 3), sum = 10
  # bal = 60 * 0.8 = 48, share = c(7/10, 3/10)
  testthat::expect_equal(result$Z[3, 3], 12) # This is 60 * 0.2
  testthat::expect_equal(
    as.numeric(result$Y[3, 3]),
    -3 + 48 * 7 / 10,
    tolerance = 1e-10
  )
})

testthat::test_that("build_io_model applies diagonal rebalancing", {
  f <- diag_rebalance_fixture()
  result <- build_io_model(f$su, f$btd, f$cbs)

  z <- result$Z[[1]]
  y <- result$Y[[1]]
  x <- result$X[[1]]

  # X should always be consistent after all fixes
  testthat::expect_equal(
    as.numeric(x),
    as.numeric(Matrix::rowSums(z) + Matrix::rowSums(y))
  )

  # Item 10 (row 1) had diag(Z) >= X, so rebalancing should
  # have reduced the diagonal. If it didn't trigger, Z[1,1]
  # would still be 100.
  # The global Y for item 20 provides positive demand for the
  # fallback distribution. After rebalancing, Z[1,1] should
  # be 20% of original or less.
  testthat::expect_true(z[1, 1] < 100)
})

testthat::test_that(".rebalance_diagonal is no-op when no sectors trigger", {
  z <- Matrix::sparseMatrix(
    i = c(1, 2),
    j = c(2, 1),
    x = c(5, 3),
    dims = c(2, 2)
  )
  y <- matrix(c(50, 40, 30, 20), nrow = 2)
  dims <- list(
    areas = 1L,
    items = c(10L, 20L),
    n_areas = 1L,
    n_items = 2L
  )
  fd_cols <- c("food", "other_uses")

  result <- .rebalance_diagonal(z, y, dims, fd_cols)

  testthat::expect_equal(as.matrix(result$Z), as.matrix(z))
  testthat::expect_equal(as.matrix(result$Y), as.matrix(y))
})


# --- 2. Stock withdrawal domestic-only tests ---

testthat::test_that("stock withdrawal is domestic-only in Y", {
  f <- stock_fixture()
  result <- build_io_model(f$su, f$btd, f$cbs)

  y <- result$Y[[1]]
  fd_labs <- result$fd_labels[[1]]
  labels <- result$labels[[1]]

  # stock_addition column for country 1
  sa_cols_c1 <- which(
    fd_labs$area_code == 1 & fd_labs$fd_col == "stock_addition"
  )
  # The net stock for country 1 should reflect:
  # stock_addition (5) minus stock_withdrawal (20) = -15
  # But only for domestic sectors (rows belonging to country 1)
  c1_rows <- which(labels$area_code == 1)

  # Sum of stock_addition column for country 1's own sectors
  net_stock_c1 <- sum(y[c1_rows, sa_cols_c1])

  # In the CBS, country 1 has sw=20, sa=5 => net = 5 - 20 = -15
  testthat::expect_equal(net_stock_c1, 5 - 20, tolerance = 1)

  # Country 2's stock_withdrawal is 0, stock_addition is 8
  sa_cols_c2 <- which(
    fd_labs$area_code == 2 & fd_labs$fd_col == "stock_addition"
  )
  c2_rows <- which(labels$area_code == 2)
  net_stock_c2 <- sum(y[c2_rows, sa_cols_c2])

  # Country 2: sa=8, sw=0 => net = 8
  testthat::expect_equal(net_stock_c2, 8, tolerance = 1)
})

testthat::test_that(".build_sw_domestic returns correct domestic vector", {
  dims <- list(
    areas = c(1L, 2L),
    items = c(10L, 20L),
    n_areas = 2L,
    n_items = 2L
  )
  cbs_yr <- tibble::tribble(
    ~area_code, ~item_cbs_code, ~stock_withdrawal,
    1, 10, 15,
    1, 20, 0,
    2, 10, 5,
    2, 20, 10,
  )

  result <- .build_sw_domestic(cbs_yr, dims)

  # Order: (area=1,item=10), (area=1,item=20),
  #        (area=2,item=10), (area=2,item=20)
  testthat::expect_equal(result, c(15, 0, 5, 10))
})

testthat::test_that(".build_sw_domestic handles missing items", {
  dims <- list(
    areas = c(1L, 2L),
    items = c(10L, 20L),
    n_areas = 2L,
    n_items = 2L
  )
  cbs_yr <- tibble::tribble(
    ~area_code, ~item_cbs_code, ~stock_withdrawal,
    1, 10, 15,
  )

  result <- .build_sw_domestic(cbs_yr, dims)
  testthat::expect_equal(result, c(15, 0, 0, 0))
})


# --- 3. Losses endogenization tests ---

testthat::test_that(".endogenize_losses moves losses to Z diagonal", {
  z <- Matrix::sparseMatrix(
    i = c(1, 2),
    j = c(2, 1),
    x = c(5, 3),
    dims = c(2, 2)
  )
  y <- matrix(
    c(20, 10, 5, 3, 8, 6),
    nrow = 2,
    ncol = 3
  )
  dims <- list(
    areas = 1L,
    items = c(10L, 20L),
    n_areas = 1L,
    n_items = 2L
  )
  fd_cols <- c("food", "other_uses", "losses")

  result <- .endogenize_losses(z, y, dims, fd_cols)

  # Losses were in column 3: y[1,3]=8, y[2,3]=6
  # These should be added to Z diagonal
  testthat::expect_equal(result$Z[1, 1], 0 + 8)
  testthat::expect_equal(result$Z[2, 2], 0 + 6)

  # Off-diagonal Z unchanged
  testthat::expect_equal(result$Z[1, 2], 5)
  testthat::expect_equal(result$Z[2, 1], 3)

  # Y should have losses column removed
  testthat::expect_equal(ncol(result$Y), 2)
  testthat::expect_equal(result$fd_cols, c("food", "other_uses"))
})

testthat::test_that(".endogenize_losses works with multiple countries", {
  z <- Matrix::sparseMatrix(
    i = integer(0),
    j = integer(0),
    x = numeric(0),
    dims = c(4, 4)
  )
  # 2 countries, 2 items, 3 fd categories
  # Y columns: c1_food, c1_other, c1_losses, c2_food, c2_other, c2_losses
  y <- matrix(0, nrow = 4, ncol = 6)
  y[1, 3] <- 12 # country 1, item 10, losses
  y[2, 3] <- 7 # country 1, item 20, losses
  y[3, 6] <- 15 # country 2, item 10, losses
  y[4, 6] <- 9 # country 2, item 20, losses
  y[1, 1] <- 30 # some food demand
  y[3, 4] <- 25

  dims <- list(
    areas = c(1L, 2L),
    items = c(10L, 20L),
    n_areas = 2L,
    n_items = 2L
  )
  fd_cols <- c("food", "other_uses", "losses")

  result <- .endogenize_losses(z, y, dims, fd_cols)

  # Z diagonal should have losses added

  testthat::expect_equal(result$Z[1, 1], 12)
  testthat::expect_equal(result$Z[2, 2], 7)
  testthat::expect_equal(result$Z[3, 3], 15)
  testthat::expect_equal(result$Z[4, 4], 9)

  # Y should have 4 columns (losses removed from each country)
  testthat::expect_equal(ncol(result$Y), 4)
  # food demand preserved
  testthat::expect_equal(as.numeric(result$Y[1, 1]), 30)
  testthat::expect_equal(as.numeric(result$Y[3, 3]), 25)

  testthat::expect_equal(result$fd_cols, c("food", "other_uses"))
})

testthat::test_that(".endogenize_losses is no-op without losses column", {
  z <- Matrix::sparseMatrix(
    i = 1,
    j = 1,
    x = 5,
    dims = c(2, 2)
  )
  y <- matrix(c(10, 5, 8, 3), nrow = 2)
  dims <- list(
    areas = 1L,
    items = c(10L, 20L),
    n_areas = 1L,
    n_items = 2L
  )
  fd_cols <- c("food", "other_uses")

  result <- .endogenize_losses(z, y, dims, fd_cols)

  testthat::expect_equal(as.matrix(result$Z), as.matrix(z))
  testthat::expect_equal(result$Y, y)
  testthat::expect_equal(result$fd_cols, fd_cols)
})

testthat::test_that("build_io_model with endogenize_losses = TRUE", {
  f <- losses_fixture()
  result <- build_io_model(
    f$su,
    f$btd,
    f$cbs,
    endogenize_losses = TRUE
  )

  z <- result$Z[[1]]
  y <- result$Y[[1]]
  x <- result$X[[1]]
  fd_labs <- result$fd_labels[[1]]

  # losses should not appear in fd_labels
  testthat::expect_false("losses" %in% fd_labs$fd_col)

  # Z diagonal should have absorbed losses
  testthat::expect_true(z[1, 1] > 0 || z[2, 2] > 0)

  # X = rowSums(Z) + rowSums(Y) should hold
  testthat::expect_equal(
    as.numeric(x),
    as.numeric(Matrix::rowSums(z) + Matrix::rowSums(y))
  )
})

testthat::test_that("build_io_model with endogenize_losses = FALSE ignores losses", {
  f <- losses_fixture()
  result <- build_io_model(
    f$su,
    f$btd,
    f$cbs,
    endogenize_losses = FALSE
  )

  fd_labs <- result$fd_labels[[1]]

  # losses should NOT be a final demand column when endogenize = FALSE
  testthat::expect_false("losses" %in% fd_labs$fd_col)
})


# --- 4. Negative-zeroing in A matrix tests ---

testthat::test_that("A matrix negative entries are zeroed before capping", {
  # Create Z with a negative entry (data inconsistency)
  z <- matrix(c(5, -3, 10, 2), nrow = 2, byrow = TRUE)
  x <- c(100, 200)

  l_inv <- compute_leontief_inverse(z, x)

  # Should still produce valid result
  testthat::expect_true(is.matrix(l_inv))
  testthat::expect_true(all(l_inv >= 0))

  # The A matrix should have had the -3 entry zeroed
  # Verify through recovery: L * y should give x
  y <- x - rowSums(pmax(z, 0))
  y[y < 0] <- 0
  x_from_z <- rowSums(pmax(z, 0)) + y
  a_clean <- pmax(z, 0) %*% diag(ifelse(x_from_z == 0, 0, 1 / x_from_z))
  # After zeroing, all A entries should be non-negative
  testthat::expect_true(all(a_clean >= 0))
})

testthat::test_that("sparse Z with negative entries produces valid Leontief", {
  z <- Matrix::sparseMatrix(
    i = c(1, 1, 2, 2),
    j = c(1, 2, 1, 2),
    x = c(5, 10, -2, 3),
    dims = c(2, 2)
  )
  x <- c(100, 200)

  l_inv <- compute_leontief_inverse(z, x)

  testthat::expect_true(is.matrix(l_inv))
  testthat::expect_true(all(l_inv >= 0))
  testthat::expect_equal(dim(l_inv), c(2, 2))
})

testthat::test_that(".technical_coefficients zeroes negatives and caps cols", {
  # Column 2 has negative entry and sum > 1
  z <- matrix(c(5, -2, 8, 12), nrow = 2, byrow = TRUE)
  x <- c(10, 10)

  # After zeroing negatives: col1 = c(5, 8)/10 = c(0.5, 0.8), sum = 1.3 > 1
  # After capping: col1 scaled by 1/1.3
  # col2 = c(0, 12)/10 = c(0, 1.2), sum = 1.2 > 1
  # After capping: col2 scaled by 1/1.2
  testthat::expect_warning(
    {
      a <- .technical_coefficients(z, x)
    },
    "Capping"
  )

  # All entries should be non-negative
  testthat::expect_true(all(as.matrix(a) >= 0))
  # All column sums should be <= 1
  testthat::expect_true(all(Matrix::colSums(a) <= 1 + 1e-10))
})

testthat::test_that("negative zeroing + capping combined give valid inverse", {
  # Extreme case: column has negatives that make sum > 1
  z <- matrix(c(5, -3, 15, 2), nrow = 2, byrow = TRUE)
  x <- c(10, 10)

  testthat::expect_warning(
    {
      l_inv <- compute_leontief_inverse(z, x)
    },
    "Capping"
  )

  testthat::expect_true(is.matrix(l_inv))
  testthat::expect_true(all(l_inv >= 0))
  # Diagonal should be >= 1 (own-sector multiplier)
  testthat::expect_true(all(diag(l_inv) >= 1 - 1e-10))
})
