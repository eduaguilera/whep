# Integration tests ------------------------------------------------------------

testthat::test_that("build_detailed_trade example returns expected structure", {
  result <- build_detailed_trade(example = TRUE)

  testthat::expect_s3_class(result, "tbl_df")
  testthat::expect_named(
    result,
    c(
      "year",
      "area_code",
      "area_code_partner",
      "element",
      "item_cbs_code",
      "unit",
      "value",
      "country_share"
    )
  )
  testthat::expect_equal(nrow(result), 10)
})

testthat::test_that("build_detailed_trade example has valid content", {
  result <- build_detailed_trade(example = TRUE)

  testthat::expect_true(all(result$element %in% c("import", "export")))
  testthat::expect_true(all(result$unit == "tonnes"))
  testthat::expect_true(all(result$value > 0))
  testthat::expect_true(all(
    result$country_share > 0 & result$country_share <= 1
  ))
  testthat::expect_true(all(
    result$area_code != result$area_code_partner
  ))
})
