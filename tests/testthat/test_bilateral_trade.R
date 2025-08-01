testthat::test_that(".prefer_flow_direction chooses preferred trade data", {
  bilateral_trade <- tibble::tribble(
    ~from_code, ~to_code, ~year, ~item_cbs_code, ~element, ~value,
    1, 2, 2000, 1, "Import", 0,
    1, 2, 2000, 1, "Export", 0,
    1, 2, 2000, 2, "Export", 0,
    1, 3, 2000, 2, "Import", 0,
    2, 3, 2000, 2, "Import", 0,
    2, 3, 2001, 2, "Export", 0,
    2, 3, 2001, 2, "Import", 0,
  ) |>
    dplyr::arrange(from_code, to_code, year, item_cbs_code)

  brute_group_by_result <- bilateral_trade |>
    dplyr::group_by(from_code, to_code, year, item_cbs_code) |>
    dplyr::filter(dplyr::n() == 1 | element == "Import") |>
    dplyr::ungroup() |>
    dplyr::arrange(from_code, to_code, year, item_cbs_code)

  my_result <- .prefer_flow_direction(bilateral_trade, "Import") |>
    dplyr::arrange(from_code, to_code, year, item_cbs_code)

  expected_import_result <- tibble::tribble(
    ~from_code, ~to_code, ~year, ~item_cbs_code, ~element, ~value,
    1, 2, 2000, 1, "Import", 0,
    1, 2, 2000, 2, "Export", 0,
    1, 3, 2000, 2, "Import", 0,
    2, 3, 2000, 2, "Import", 0,
    2, 3, 2001, 2, "Import", 0,
  ) |>
    dplyr::arrange(from_code, to_code, year, item_cbs_code)

  testthat::expect_equal(my_result, brute_group_by_result)
  testthat::expect_equal(my_result, expected_import_result)

  brute_group_by_result <- bilateral_trade |>
    dplyr::group_by(from_code, to_code, year, item_cbs_code) |>
    dplyr::filter(dplyr::n() == 1 | element == "Export") |>
    dplyr::ungroup() |>
    dplyr::arrange(from_code, to_code, year, item_cbs_code)

  my_result <- .prefer_flow_direction(bilateral_trade, "Export") |>
    dplyr::arrange(from_code, to_code, year, item_cbs_code)

  expected_export_result <- tibble::tribble(
    ~from_code, ~to_code, ~year, ~item_cbs_code, ~element, ~value,
    1, 2, 2000, 1, "Export", 0,
    1, 2, 2000, 2, "Export", 0,
    1, 3, 2000, 2, "Import", 0,
    2, 3, 2000, 2, "Import", 0,
    2, 3, 2001, 2, "Export", 0,
  ) |>
    dplyr::arrange(from_code, to_code, year, item_cbs_code)

  testthat::expect_equal(my_result, brute_group_by_result)
  testthat::expect_equal(my_result, expected_export_result)
})

testthat::test_that(".estimate_bilateral_trade creates expected matrix", {
  exports <- c(5, 0, 4)
  imports <- c(1, 3, 0)
  expected <- matrix(
    # fmt: skip
    c(
      0.9027778, 2.708333, 0,
      0.0000000, 0.000000, 0,
      0.7222222, 2.166667, 0
    ),
    byrow = TRUE,
    ncol = 3
  )
  result <- .estimate_bilateral_trade(exports, imports)
  testthat::expect_equal(result, expected, tolerance = 1e-6)

  # Martin' slide example
  exports <- c(500, 300, 100, 0, 0, 0)
  imports <- c(200, 150, 120, 200, 190, 30)
  expected <- matrix(
    # fmt: skip
    c(
      112, 84, 67, 112, 106, 17,
      67, 50, 40, 67, 64, 10,
      22, 17, 13, 22, 21, 3,
      0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0
    ),
    byrow = TRUE,
    ncol = 6
  )
  result <- .estimate_bilateral_trade(exports, imports)
  testthat::expect_equal(result, expected, tolerance = 1)

  # No data imports sum 0
  exports <- c(5, 0, 4)
  imports <- c(0, 0, 0)
  expected <- matrix(
    c(
      0, 0, 0,
      0, 0, 0,
      0, 0, 0
    ),
    byrow = TRUE,
    ncol = 3
  )
  result <- .estimate_bilateral_trade(exports, imports)
  testthat::expect_equal(result, expected, tolerance = 1)

  # No data exports sum 0
  exports <- c(0, 0, 0)
  imports <- c(1, 3, 0)
  expected <- matrix(
    c(
      0, 0, 0,
      0, 0, 0,
      0, 0, 0
    ),
    byrow = TRUE,
    ncol = 3
  )
  result <- .estimate_bilateral_trade(exports, imports)
  testthat::expect_equal(result, expected, tolerance = 1)

  # No data both sum 0
  exports <- c(0, 0, 0)
  imports <- c(0, 0, 0)
  expected <- matrix(
    c(
      0, 0, 0,
      0, 0, 0,
      0, 0, 0
    ),
    byrow = TRUE,
    ncol = 3
  )
  result <- .estimate_bilateral_trade(exports, imports)
  testthat::expect_equal(result, expected, tolerance = 1)
})

testthat::test_that(".fill_missing_trade only fills NA entries of matrix", {
  original <- matrix(
    c(
      140, NA, NA,
      50, 100, NA,
      NA, NA, NA
    ),
    byrow = TRUE,
    ncol = 3
  )

  expected <- matrix(
    c(
      140.00, 7.65, 2.45,
      50.00, 100.00, 2.96,
      3.64, 4.55, 1.46
    ),
    byrow = TRUE,
    ncol = 3
  )
  total_trade <- tibble::tribble(
    ~area_code, ~export, ~import,
    4, 250, 200,
    6, 200, 250,
    7, 100, 80
  ) |>
    .balance_total_trade()

  original |>
    .fill_missing_trade(total_trade) |>
    testthat::expect_equal(expected, tolerance = 1e-2)
})

testthat::test_that(".fill_missing_trade does nothing for non-NA matrices", {
  original <- matrix(
    c(
      140, 40, 30,
      50, 100, 77,
      11, 324, 23
    ),
    byrow = TRUE,
    ncol = 3
  )
  total_trade <- tibble::tribble(
    ~area_code, ~export, ~import,
    4, 250, 250,
    6, 300, 550,
    7, 450, 150
  ) |>
    .balance_total_trade()

  original |>
    .fill_missing_trade(total_trade) |>
    testthat::expect_equal(original, tolerance = 1e-2)
})

testthat::test_that(
  ".fill_missing_trade fills with 0s if row sum is already past CBS report",
  {
    original <- matrix(
      c(
        140, NA,
        NA, 100
      ),
      byrow = TRUE,
      ncol = 2
    )
    expected <- matrix(
      c(
        140, 0,
        0, 100
      ),
      byrow = TRUE,
      ncol = 2
    )
    total_trade <- tibble::tribble(
      ~area_code, ~export, ~import,
      4, 130, 200,
      6, 90, 100,
    ) |>
      .balance_total_trade()

    original |>
      .fill_missing_trade(total_trade) |>
      testthat::expect_equal(expected, tolerance = 1e-2)
  }
)

testthat::test_that(".balance_matrix makes rows and columns have target sum", {
  total_trade <- tibble::tibble(
    area_code = c(4, 6, 7, 9, 10, 75),
    export = c(500, 300, 100, 0, 0, 0),
    import = c(200, 150, 120, 200, 190, 30)
  ) |>
    .balance_total_trade()

  trade_matrix <- matrix(
    c(
      140, 30, 34, 140, 120, 8,
      50, 100, 20, 50, 60, 5,
      11, 8, 50, 11, 11, 2,
      0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0
    ),
    byrow = TRUE,
    ncol = 6
  )

  # Rescaling exports to match total sum of 890 imports
  balanced_total_exports <- c(494.44, 296.67, 98.89, 0, 0, 0)
  balanced_total_imports <- c(200, 150, 120, 200, 190, 30)

  result <- .balance_matrix(trade_matrix, total_trade)
  testthat::expect_equal(
    rowSums(result),
    balanced_total_exports,
    tolerance = 1e-2
  )
  testthat::expect_equal(
    colSums(result),
    balanced_total_imports,
    tolerance = 1e-2
  )
})

testthat::test_that(".build_trade_matrix completes missing countries", {
  codes <- factor(c(1, 2, 4, 5, 999))
  btd <- tibble::tribble(
    ~from_code, ~to_code, ~value,
    1, 2, 1,
    1, 4, 2,
    4, 2, 1,
    5, 4, 2
  ) |>
    dplyr::mutate(
      from_code = factor(from_code, levels = codes),
      to_code = factor(to_code, levels = codes),
    )
  expected <- matrix(
    c(
      NA, 1, 2, NA, NA,
      NA, NA, NA, NA, NA,
      NA, 1, NA, NA, NA,
      NA, NA, 2, NA, NA,
      NA, NA, NA, NA, NA
    ),
    byrow = TRUE,
    ncol = 5,
    dimnames = list(sort(codes), sort(codes))
  )

  btd |>
    .build_trade_matrix(codes) |>
    testthat::expect_equal(expected)
})
