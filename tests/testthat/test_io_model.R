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

io_multi_output_fixture <- function() {
  # One processing process turns 80 of crop 10 into co-products 20 (mass 90)
  # and 21 (mass 10). Export prices 1 and 9 give value shares 90:90 = 50:50,
  # vs mass shares 90:10.
  su <- tibble::tribble(
    ~year, ~area_code, ~proc_group, ~proc_cbs_code, ~item_cbs_code, ~type, ~value,
    2000, 1, "crop_production", 10, 10, "supply", 100,
    2000, 1, "processing", 10, 10, "use", 80,
    2000, 1, "processing", 10, 20, "supply", 90,
    2000, 1, "processing", 10, 21, "supply", 10,
  )
  btd <- tibble::tibble(
    year = rep(2000L, 3),
    item_cbs_code = c(10, 20, 21),
    bilateral_trade = list(matrix(0, 1, 1), matrix(0, 1, 1), matrix(0, 1, 1))
  )
  cbs <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~production, ~import, ~export, ~food, ~other_uses, ~stock_withdrawal, ~stock_addition,
    2000, 1, 10, 100, 0, 0, 20, 0, 0, 0,
    2000, 1, 20, 90, 0, 0, 80, 0, 0, 0,
    2000, 1, 21, 10, 0, 0, 9, 0, 0, 0,
  )
  prices <- tibble::tribble(
    ~year, ~element, ~item_cbs_code, ~price,
    2000, "export", 20, 1,
    2000, "export", 21, 9,
  )
  list(su = su, btd = btd, cbs = cbs, prices = prices)
}

testthat::test_that("mass is the default and value allocation reweights co-products", {
  f <- io_multi_output_fixture()
  z_default <- whep::build_io_model(f$su, f$btd, f$cbs)$Z[[1]]
  z_mass <- whep::build_io_model(f$su, f$btd, f$cbs, method = "mass")$Z[[1]]
  z_value <- whep::build_io_model(
    f$su,
    f$btd,
    f$cbs,
    method = "value",
    prices = f$prices
  )$Z[[1]]

  # default == mass (regression guard)
  testthat::expect_equal(as.matrix(z_mass), as.matrix(z_default))
  # crop 10's use (80) splits across co-products 20, 21 (sorted cols 2, 3)
  testthat::expect_equal(unname(Matrix::colSums(z_mass)[2:3]), c(72, 8))
  testthat::expect_equal(unname(Matrix::colSums(z_value)[2:3]), c(40, 40))
})

testthat::test_that("value allocation falls back to mass when a price is missing", {
  f <- io_multi_output_fixture()
  z_mass <- whep::build_io_model(f$su, f$btd, f$cbs, method = "mass")$Z[[1]]
  z_partial <- whep::build_io_model(
    f$su,
    f$btd,
    f$cbs,
    method = "value",
    prices = dplyr::filter(f$prices, item_cbs_code == 20)
  )$Z[[1]]

  testthat::expect_equal(as.matrix(z_partial), as.matrix(z_mass))
})

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

testthat::test_that("X uses reported production output", {
  f <- io_single_country_fixture()
  result <- build_io_model(f$su, f$btd, f$cbs)

  x <- result$X[[1]]

  testthat::expect_equal(
    as.numeric(x),
    f$cbs$production
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
    c(
      "area_code",
      "polity_area_code",
      "reporting_polity_code",
      "reporting_polity_name",
      "reporting_polity_has_geometry",
      "item_cbs_code"
    )
  )
  pointblank::expect_col_vals_not_null(labels, reporting_polity_code)
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
    c(
      "area_code",
      "polity_area_code",
      "reporting_polity_code",
      "reporting_polity_name",
      "reporting_polity_has_geometry",
      "fd_col"
    )
  )
  pointblank::expect_col_vals_not_null(fd_labs, reporting_polity_code)
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
    "missing column"
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

testthat::test_that("IO default build helpers scope cache keys by requested years", {
  testthat::expect_null(.io_build_years(NULL))
  testthat::expect_equal(.io_build_years(c(2001, 1999)), 1999:2001)
  testthat::expect_true(.io_years_are_contiguous(c(2001, 1999, 2000)))
  testthat::expect_false(.io_years_are_contiguous(c(2001, 1999)))
  testthat::expect_null(.io_context_years(NULL))
  testthat::expect_equal(.io_context_years(2001:2005), 2001:2005)
  testthat::expect_equal(.io_context_years(2013), 2011:2013)
  testthat::expect_equal(.io_context_years(2016:2020), 2011:2020)
  testthat::expect_equal(
    .io_cache_key("primary_prod", NULL),
    "primary_prod"
  )
  testthat::expect_equal(
    .io_cache_key("primary_prod", c(2001, 1999)),
    "primary_prod__1999__2001"
  )
})

testthat::test_that("sparse default IO builds run requested years independently", {
  f <- io_two_country_fixture()

  testthat::expect_true(.io_should_build_sparse_years(
    c(1999, 2001),
    supply_use = NULL,
    cbs = f$cbs
  ))
  testthat::expect_false(.io_should_build_sparse_years(
    c(1999, 2001),
    supply_use = f$su,
    cbs = f$cbs
  ))
  testthat::expect_false(.io_should_build_sparse_years(
    c(1999, 2000, 2001),
    supply_use = NULL,
    cbs = NULL
  ))
})

testthat::test_that(".build_output_vector falls back when production is absent", {
  dims <- list(
    areas = 1L,
    items = c(10L, 20L)
  )
  cbs <- tibble::tribble(
    ~area_code, ~item_cbs_code, ~production,
    1L, 10L, 100,
    1L, 20L, 0
  )

  result <- .build_output_vector(cbs, dims, fallback = c(80, 70))

  testthat::expect_equal(result, c(100, 70))
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

testthat::test_that(".extract_trade_matrix preserves off-diagonal named trade", {
  trade <- matrix(
    c(
      0,
      10,
      2,
      5,
      0,
      3,
      7,
      4,
      0
    ),
    nrow = 3,
    byrow = TRUE,
    dimnames = list(c("1", "2", "3"), c("1", "2", "3"))
  )

  result <- .extract_trade_matrix(trade, areas = c(2L, 1L))

  testthat::expect_equal(
    result,
    matrix(
      c(0, 5, 10, 0),
      nrow = 2,
      byrow = TRUE
    )
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
