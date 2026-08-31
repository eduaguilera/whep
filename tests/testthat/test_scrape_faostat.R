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

# Regression test for #520: `.populate_iso3_code()` used to delegate to
# FAOSTAT::fillCountryCode(), which reads `FAOcountryProfile` as a free
# variable and therefore only worked while package:FAOSTAT was attached. Since
# #541 the codes come off `polity_area_crosswalk` and FAOSTAT's profile is not
# read at all, so the assertion is now that resolving stays independent of the
# search path.
testthat::test_that(".populate_iso3_code resolves ISO3 codes unattached", {
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

testthat::test_that(".fao_area_iso3_lookup is one row per FAO area name", {
  lookup <- whep:::.fao_area_iso3_lookup()

  testthat::expect_named(lookup, c("fao_area_name", "iso3_code"))
  # The lookup is used with match(), so a duplicated name would silently pick
  # whichever row came first. Rows with no `area_code` are dropped precisely
  # because they reuse a parent area's name.
  testthat::expect_false(any(duplicated(lookup$fao_area_name)))
  testthat::expect_gt(nrow(lookup), 200)

  # The "China" aggregate (area 351) carries no `area_iso3c` upstream and so
  # must not resolve to a single ISO3 code (#158, #313).
  china <- lookup$iso3_code[lookup$fao_area_name == "China"]
  testthat::expect_length(china, 1)
  testthat::expect_true(is.na(china))

  # Its components do resolve, each on its own FAOSTAT label.
  mainland <- lookup$iso3_code[lookup$fao_area_name == "China, mainland"]
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

# Regression test for #541: the ISO3 codes came from FAOSTAT's vendored
# `FAOcountryProfile` name table, which is stale relative to the labels FAOSTAT
# publishes today, plus a hand-maintained fix block in whep. Eight of the 211
# real FAOSTAT labels in `regions_full$FAOSTAT_name` came out `NA`. All but the
# "China" aggregate fail before the fix.
testthat::test_that(".populate_iso3_code resolves renamed FAO reporters", {
  df <- data.frame(
    area = c("Eswatini", "North Macedonia", "China, Taiwan Province of"),
    stringsAsFactors = FALSE
  )

  result <- whep:::.populate_iso3_code(df)

  # ISO 3166-1 alpha-3, cross-checked against the UN M49 ISO-alpha3 column
  # (SWZ, MKD) and the countrycode package (TWN); see the PR body.
  testthat::expect_equal(result$ISO3_CODE, c("SWZ", "MKD", "TWN"))
})

testthat::test_that(".populate_iso3_code resolves former FAO areas", {
  df <- data.frame(
    area = c("Belgium-Luxembourg", "Ethiopia PDR", "Sudan (former)", "USSR"),
    stringsAsFactors = FALSE
  )

  result <- whep:::.populate_iso3_code(df)

  # Territorial attributions inherited from `polity_area_crosswalk`, not
  # decided here: BLX is the trade-database code for the Belgium-Luxembourg
  # Economic Union and SUN is ISO 3166-1's reserved code for the USSR.
  testthat::expect_equal(result$ISO3_CODE, c("BLX", "ETH", "SDN", "SUN"))
})

testthat::test_that("every FAOSTAT reporter but the China aggregate resolves", {
  labels <- setdiff(unique(whep::regions_full$FAOSTAT_name), c(NA, "", "#N/A"))
  # Non-vacuous: an empty or tiny label set would pass every assertion below.
  testthat::expect_gt(length(labels), 200)

  result <- suppressWarnings(
    whep:::.populate_iso3_code(data.frame(area = labels))
  )

  # The 351 "China" aggregate must stay unmapped (#158 / #313); it is the only
  # FAOSTAT reporter label allowed to.
  testthat::expect_equal(result$area[is.na(result$ISO3_CODE)], "China")
})

testthat::test_that("the retired manual ISO3 fix block is redundant", {
  # These seven names were patched by hand in `.populate_iso3_code()`. The
  # crosswalk carries all of them, which is why the block could be deleted.
  patched <- c(
    "China, mainland" = "CHN",
    "Türkiye" = "TUR",
    "Netherlands (Kingdom of the)" = "NLD",
    "Sudan" = "SDN",
    "South Sudan" = "SSD",
    "Czechia" = "CZE",
    "Lao People's Democratic Republic" = "LAO"
  )

  lookup <- whep:::.fao_area_iso3_lookup()
  resolved <- lookup$iso3_code[match(names(patched), lookup$fao_area_name)]

  testthat::expect_equal(resolved, unname(patched))
})
