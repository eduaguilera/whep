# Integration tests ------------------------------------------------------------

testthat::test_that("build_trade_prices example returns expected structure", {
  result <- build_trade_prices(example = TRUE)

  testthat::expect_s3_class(result, "tbl_df")
  testthat::expect_named(
    result,
    c(
      "year",
      "item_trade",
      "item_code_trade",
      "element",
      "kdollars",
      "tonnes",
      "price"
    )
  )
  testthat::expect_equal(nrow(result), 10)
})

testthat::test_that("build_trade_prices example has valid content", {
  result <- build_trade_prices(example = TRUE)

  testthat::expect_true(all(result$element %in% c("import", "export")))
  testthat::expect_true(all(result$price > 0))
  testthat::expect_true(all(result$tonnes > 0))
  testthat::expect_true(all(result$kdollars > 0))
})

testthat::test_that("build_primary_prices example returns expected structure", {
  result <- build_primary_prices(example = TRUE)

  testthat::expect_s3_class(result, "tbl_df")
  testthat::expect_named(result, c("year", "item_prod_code", "price"))
  testthat::expect_equal(nrow(result), 10)
  testthat::expect_true(all(result$price > 0))
})

testthat::test_that("build_cbs_prices example returns expected structure", {
  result <- build_cbs_prices(example = TRUE)

  testthat::expect_s3_class(result, "tbl_df")
  testthat::expect_named(
    result,
    c("year", "element", "item_cbs_code", "price")
  )
  testthat::expect_equal(nrow(result), 10)
  testthat::expect_true(all(result$element %in% c("import", "export")))
  testthat::expect_true(all(result$price > 0))
})
