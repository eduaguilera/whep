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

testthat::test_that(".populate_iso3_code keeps China aggregate distinct", {
  testthat::skip_if_not_installed("FAOSTAT")

  df <- tibble::tribble(
    ~area, ~value,
    "China", 100,
    "China, mainland", 90,
    "Portugal", 5
  ) |>
    as.data.frame()

  # fillCountryCode warns about unmatched / ambiguous China codes.
  result <- suppressWarnings(.populate_iso3_code(df))

  # "China, mainland" (area 41) is the mapped Chinese producer.
  mainland_iso <- result[result$area == "China, mainland", "ISO3_CODE"]
  testthat::expect_equal(mainland_iso, "CHN")

  # The aggregate "China" (area 351) must stay unmapped so it is not
  # summed together with "China, mainland".
  aggregate_iso <- result[result$area == "China", "ISO3_CODE"]
  testthat::expect_true(is.na(aggregate_iso))

  # No ISO3 code may be shared by two distinct source areas.
  mapped <- result[!is.na(result$ISO3_CODE), ]
  dup_codes <- mapped$ISO3_CODE[duplicated(mapped$ISO3_CODE)]
  testthat::expect_length(dup_codes, 0)
})

testthat::test_that("get_faostat_data(example = TRUE) returns offline fixture", {
  result <- whep::get_faostat_data(example = TRUE)

  testthat::expect_s3_class(result, "tbl_df")
  testthat::expect_named(
    result,
    c("area", "item", "element", "year", "value", "unit", "ISO3_CODE")
  )
  testthat::expect_type(result$value, "double")
  testthat::expect_type(result$year, "integer")
  testthat::expect_gt(nrow(result), 0)
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
