# Helper fixtures -------------------------------------------------------

io_two_country_fixture <- function() {
  su <- tibble::tribble(
    ~year, ~area_code, ~proc_group, ~proc_cbs_code, ~item_cbs_code, ~type, ~value,
    2000, 1, "crop_production", 10, 10, "supply", 100,
    2000, 1, "crop_production", 10, 10, "use", 5,
    2000, 1, "processing", 10, 20, "supply", 40,
    2000, 1, "processing", 10, 10, "use", 50,
    2000, 2, "crop_production", 10, 10, "supply", 80,
    2000, 2, "crop_production", 10, 10, "use", 3,
    2000, 2, "processing", 10, 20, "supply", 30,
    2000, 2, "processing", 10, 10, "use", 40,
  )

  btd <- tibble::tibble(
    year = c(2000L, 2000L),
    item_cbs_code = c(10, 20),
    bilateral_trade = list(
      matrix(c(0, 10, 5, 0), nrow = 2),
      matrix(c(0, 2, 3, 0), nrow = 2)
    )
  )

  cbs <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~production, ~import, ~export, ~food, ~other_uses, ~stock_withdrawal, ~stock_addition,
    2000, 1, 10, 100, 5, 10, 20, 5, 3, 0,
    2000, 1, 20, 40, 3, 2, 30, 5, 0, 2,
    2000, 2, 10, 80, 10, 5, 15, 10, 0, 4,
    2000, 2, 20, 30, 2, 3, 20, 4, 1, 0,
  )

  list(su = su, btd = btd, cbs = cbs)
}

io_single_country_fixture <- function() {
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
    ~year, ~area_code, ~item_cbs_code, ~production, ~import, ~export, ~food, ~other_uses, ~stock_withdrawal, ~stock_addition,
    2000, 1, 10, 100, 0, 0, 30, 10, 5, 0,
    2000, 1, 20, 40, 0, 0, 25, 10, 0, 3,
  )

  list(su = su, btd = btd, cbs = cbs)
}

# build_io_model --------------------------------------------------------

testthat::test_that("build_io_model handles 2-country 2-item example", {
  f <- io_two_country_fixture()
  result <- build_io_model(f$su, f$btd, f$cbs)

  testthat::expect_s3_class(result, "tbl_df")
  testthat::expect_equal(nrow(result), 1)
  pointblank::expect_col_exists(
    result,
    c("year", "Z", "Y", "X", "labels")
  )

  z <- result$Z[[1]]
  y <- result$Y[[1]]
  x <- result$X[[1]]
  n <- 2 * 2

  testthat::expect_equal(dim(z), c(n, n))
  testthat::expect_equal(nrow(y), n)
  testthat::expect_equal(length(x), n)
})

testthat::test_that("build_io_model works with explicit inputs", {
  f <- io_two_country_fixture()

  result <- whep::build_io_model(f$su, f$btd, f$cbs, years = 2000)

  testthat::expect_s3_class(result, "tbl_df")
  testthat::expect_equal(nrow(result), 1)
  testthat::expect_equal(result$year[[1]], 2000)
})

testthat::test_that("x equals rowSums(z) + rowSums(y)", {
  f <- io_single_country_fixture()
  result <- build_io_model(f$su, f$btd, f$cbs)

  z <- result$Z[[1]]
  y <- result$Y[[1]]
  x <- result$X[[1]]

  testthat::expect_equal(
    as.numeric(x),
    as.numeric(Matrix::rowSums(z) + Matrix::rowSums(y))
  )
})

testthat::test_that("z has no negative values", {
  f <- io_two_country_fixture()
  result <- build_io_model(f$su, f$btd, f$cbs)
  z <- result$Z[[1]]

  testthat::expect_true(all(z >= -1e-10))
})

testthat::test_that("labels have correct dimensions", {
  f <- io_two_country_fixture()
  result <- build_io_model(f$su, f$btd, f$cbs)
  labels <- result$labels[[1]]

  pointblank::expect_col_exists(
    labels,
    c("area_code", "item_cbs_code")
  )
  testthat::expect_equal(
    nrow(labels),
    length(result$X[[1]])
  )
})

testthat::test_that("build_io_model returns fd_labels with correct shape", {
  f <- io_two_country_fixture()
  result <- build_io_model(f$su, f$btd, f$cbs)
  fd_labs <- result$fd_labels[[1]]

  pointblank::expect_col_exists(
    fd_labs,
    c("area_code", "fd_col")
  )
  # 2 areas * 3 fd_cols (food, other_uses, stock_addition) = 6
  testthat::expect_equal(nrow(fd_labs), 6L)
  testthat::expect_equal(
    ncol(result$Y[[1]]),
    nrow(fd_labs)
  )
  pointblank::expect_col_vals_in_set(
    fd_labs,
    fd_col,
    set = c("food", "other_uses", "stock_addition")
  )
})

testthat::test_that("build_io_model validates missing columns", {
  f <- io_two_country_fixture()
  bad_su <- dplyr::select(f$su, -value)
  testthat::expect_error(
    build_io_model(bad_su, f$btd, f$cbs),
    "missing columns"
  )
})

testthat::test_that("build_io_model with years = NULL computes all years", {
  f <- io_two_country_fixture()
  result <- build_io_model(f$su, f$btd, f$cbs, years = NULL)

  testthat::expect_equal(nrow(result), 1)
  testthat::expect_equal(result$year[[1]], 2000)
})

testthat::test_that("build_io_model with years filters to specified years", {
  f <- io_two_country_fixture()
  result <- build_io_model(
    f$su,
    f$btd,
    f$cbs,
    years = c(2000)
  )

  testthat::expect_equal(nrow(result), 1)
  testthat::expect_equal(result$year[[1]], 2000)
})

testthat::test_that("build_io_model with invalid years raises error", {
  f <- io_two_country_fixture()
  testthat::expect_error(
    build_io_model(f$su, f$btd, f$cbs, years = c(2001)),
    "not available in data"
  )
})

testthat::test_that("build_io_model with non-numeric years raises error", {
  f <- io_two_country_fixture()
  testthat::expect_error(
    build_io_model(f$su, f$btd, f$cbs, years = "2000"),
    "must be numeric or NULL"
  )
})

# Private helpers -------------------------------------------------------

testthat::test_that(".block_diag builds correct block-diagonal", {
  a <- matrix(1:4, 2, 2)
  b <- matrix(5:7, 1, 3)
  result <- .block_diag(list(a, b))

  testthat::expect_equal(dim(result), c(3, 5))
  testthat::expect_equal(
    as.matrix(result[1:2, 1:2]),
    a
  )
  testthat::expect_equal(
    as.numeric(result[3, 3:5]),
    as.vector(b)
  )
  testthat::expect_equal(sum(result[1:2, 3:5]), 0)
  testthat::expect_equal(sum(result[3, 1:2]), 0)
})

testthat::test_that(".row_normalize normalises rows to sum 1", {
  m <- matrix(c(10, 20, 30, 60), nrow = 2)
  result <- .row_normalize(m)

  testthat::expect_equal(
    as.numeric(Matrix::rowSums(result)),
    c(1, 1)
  )
})

testthat::test_that(".row_normalize handles zero rows gracefully", {
  m <- matrix(c(0, 5, 0, 10), nrow = 2)
  result <- .row_normalize(m)

  testthat::expect_equal(result[1, ], c(0, 0))
  testthat::expect_equal(sum(result[2, ]), 1)
})

testthat::test_that(".build_shares_matrix produces correct shares", {
  shares <- list(
    "10" = matrix(
      c(0.9, 0.1, 0.05, 0.95),
      nrow = 2
    ),
    "20" = diag(2)
  )
  mat <- .build_shares_matrix(
    shares,
    c(10, 20),
    2,
    2
  )

  testthat::expect_equal(dim(mat), c(4, 2))
  # Row 1 = area 1, item 10 => shares[["10"]][1, ]
  testthat::expect_equal(
    mat[1, ],
    c(0.9, 0.05)
  )
  # Row 2 = area 1, item 20 => shares[["20"]][1, ]
  testthat::expect_equal(mat[2, ], c(1, 0))
  # Row 3 = area 2, item 10 => shares[["10"]][2, ]
  testthat::expect_equal(
    mat[3, ],
    c(0.1, 0.95)
  )
})

testthat::test_that(".fix_negative_output returns both X and Y", {
  z <- matrix(c(5, 0, 0, 3), nrow = 2)
  y <- matrix(c(10, 20), ncol = 1)
  result <- .fix_negative_output(z, y)

  testthat::expect_true(is.list(result))
  testthat::expect_true("X" %in% names(result))
  testthat::expect_true("Y" %in% names(result))
  testthat::expect_equal(
    result$X,
    rowSums(z) + rowSums(y)
  )
})
