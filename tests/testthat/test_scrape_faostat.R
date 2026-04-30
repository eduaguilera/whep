testthat::test_that(".faostat_converter returns correct codes for livestock", {
  result <- .faostat_converter("livestock")

  testthat::expect_type(result, "list")
  testthat::expect_equal(result$FAOSTAT_code, "EMN")
  testthat::expect_equal(result$FAOSTAT_param, "stocks")
})

testthat::test_that(".faostat_converter returns correct codes for crop types", {
  area <- .faostat_converter("crop_area")
  testthat::expect_equal(area$FAOSTAT_code, "QCL")
  testthat::expect_equal(area$FAOSTAT_param, "area_harvested")

  yield <- .faostat_converter("crop_yield")
  testthat::expect_equal(yield$FAOSTAT_code, "QCL")
  testthat::expect_equal(yield$FAOSTAT_param, "yield")

  prod <- .faostat_converter("crop_production")
  testthat::expect_equal(prod$FAOSTAT_code, "QCL")
  testthat::expect_equal(prod$FAOSTAT_param, "production")
})

testthat::test_that(".faostat_converter errors on invalid activity_data", {
  testthat::expect_error(.faostat_converter("invalid"))
  testthat::expect_error(.faostat_converter(c("livestock", "crop_area")))
})

testthat::test_that(".activity_data_choices returns expected values", {
  choices <- .activity_data_choices()

  testthat::expect_type(choices, "character")
  testthat::expect_length(choices, 4)
  testthat::expect_true("livestock" %in% choices)
  testthat::expect_true("crop_area" %in% choices)
  testthat::expect_true("crop_yield" %in% choices)
  testthat::expect_true("crop_production" %in% choices)
})

testthat::test_that(".bad_activity_data_param_error returns helpful message", {
  msg <- .bad_activity_data_param_error()

  testthat::expect_type(msg, "character")
  testthat::expect_true(
    stringr::str_detect(msg, "activity_data")
  )
  testthat::expect_true(
    stringr::str_detect(msg, "livestock")
  )
})
