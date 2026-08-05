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

testthat::test_that(".fao_country_profile reads the dataset when unattached", {
  testthat::skip_if_not_installed("FAOSTAT")
  skip_if_faostat_attached()

  profile <- whep:::.fao_country_profile(c("ISO3_CODE", "SHORT_NAME"))

  testthat::expect_s3_class(profile, "tbl_df")
  testthat::expect_named(profile, c("ISO3_CODE", "SHORT_NAME"))
  testthat::expect_gt(nrow(profile), 200)
  testthat::expect_true("PRT" %in% profile$ISO3_CODE)
})

testthat::test_that(".fao_country_profile aborts on a missing column", {
  testthat::skip_if_not_installed("FAOSTAT")

  testthat::expect_error(
    whep:::.fao_country_profile("NOT_A_PROFILE_COLUMN"),
    "NOT_A_PROFILE_COLUMN"
  )
})

# Regression test for #520: `.populate_iso3_code()` used to delegate to
# FAOSTAT::fillCountryCode(), which reads `FAOcountryProfile` as a free
# variable and therefore only worked while package:FAOSTAT was attached. Every
# assertion below fails with "object 'FAOcountryProfile' not found" before the
# fix. No network is needed; the country profile ships inside FAOSTAT.
testthat::test_that(".populate_iso3_code resolves ISO3 codes unattached", {
  testthat::skip_if_not_installed("FAOSTAT")
  skip_if_faostat_attached()

  df <- tibble::tribble(
    ~area, ~value,
    "Portugal", 1,
    "Spain", 2,
    "T\u00FCrkiye", 3
  ) |>
    as.data.frame()

  search_before <- search()
  result <- whep:::.populate_iso3_code(df)

  testthat::expect_equal(result$ISO3_CODE, c("PRT", "ESP", "TUR"))
  # Resolving codes must not attach (nor later unload) FAOSTAT.
  testthat::expect_identical(search(), search_before)
})

testthat::test_that(".populate_iso3_code keeps input rows and their order", {
  testthat::skip_if_not_installed("FAOSTAT")

  # fillCountryCode() merged on the area name, which sorted the rows.
  df <- data.frame(
    area = c("Spain", "Portugal", "Spain"),
    value = 1:3,
    stringsAsFactors = FALSE
  )

  result <- whep:::.populate_iso3_code(df)

  testthat::expect_equal(result$area, df$area)
  testthat::expect_equal(result$value, df$value)
  testthat::expect_equal(result$ISO3_CODE, c("ESP", "PRT", "ESP"))
})

testthat::test_that(".populate_iso3_code warns and returns NA if unmatched", {
  testthat::skip_if_not_installed("FAOSTAT")

  df <- data.frame(
    area = c("Portugal", "World", "Not A Country"),
    stringsAsFactors = FALSE
  )

  testthat::expect_warning(
    result <- whep:::.populate_iso3_code(df),
    "Could not match"
  )
  testthat::expect_equal(result$ISO3_CODE, c("PRT", NA, NA))
  testthat::expect_type(result$ISO3_CODE, "character")
})

testthat::test_that(".fao_area_iso3_lookup leaves ambiguous names unmatched", {
  testthat::skip_if_not_installed("FAOSTAT")

  lookup <- whep:::.fao_area_iso3_lookup()

  testthat::expect_named(lookup, c("fao_area_name", "iso3_code"))
  testthat::expect_false(any(duplicated(lookup$fao_area_name)))

  # "China" names three profile rows (the aggregate 351, mainland 41 and
  # mainland + Taiwan 357), so it must not resolve to a single ISO3 code.
  china <- lookup$iso3_code[lookup$fao_area_name == "China"]
  testthat::expect_length(china, 1)
  testthat::expect_true(is.na(china))

  # An unambiguous name still resolves.
  mainland <- lookup$iso3_code[lookup$fao_area_name == "China mainland"]
  testthat::expect_equal(mainland, "CHN")
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
