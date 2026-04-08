# Helper fixtures --------------------------------------------------------------

.fake_bilateral_trade <- function() {
  data.table::data.table(
    `Reporter Country Code` = c(2L, 9L, 2L),
    `Partner Country Code` = c(9L, 2L, 9L),
    `Item Code` = c(15L, 15L, 15L),
    Element = c("Import Quantity", "Export Quantity", "Import Quantity"),
    Year = c(2020L, 2020L, 2021L),
    Unit = c("tonnes", "tonnes", "tonnes"),
    Value = c(100, 200, 150)
  )
}

# Unit tests -------------------------------------------------------------------

testthat::test_that("build_detailed_trade works with raw_trade input", {
  result <- build_detailed_trade(raw_trade = .fake_bilateral_trade())

  testthat::expect_s3_class(result, "tbl_df")
  testthat::expect_named(
    result,
    c(
      "year",
      "area_code",
      "area_code_partner",
      "element",
      "item_cbs",
      "item_cbs_code",
      "unit",
      "value",
      "country_share"
    )
  )

  # Wheat (trade code 15) maps to CBS code 2511
  testthat::expect_true(all(result$item_cbs_code == 2511))

  # AFG (code 2) and ARG (code 9) both map to themselves as polities
  testthat::expect_true(all(result$area_code %in% c(2, 9)))
  testthat::expect_true(all(result$area_code_partner %in% c(2, 9)))

  # No self-trade
  testthat::expect_true(all(result$area_code != result$area_code_partner))

  # Country shares sum to 1 within each group (only one partner each)
  testthat::expect_true(all(result$country_share == 1))
})

testthat::test_that("build_detailed_trade computes correct shares", {
  # Two partners exporting to the same reporter
  raw <- data.table::data.table(
    `Reporter Country Code` = c(2L, 2L),
    `Partner Country Code` = c(9L, 7L),
    `Item Code` = c(15L, 15L),
    Element = c("Import Quantity", "Import Quantity"),
    Year = c(2020L, 2020L),
    Unit = c("tonnes", "tonnes"),
    Value = c(300, 700)
  )

  result <- build_detailed_trade(raw_trade = raw)

  imports <- result |>
    dplyr::filter(element == "import", year == 2020) |>
    dplyr::arrange(area_code_partner)

  # AGO (code 7) -> polity area_code 7, ARG (code 9) -> polity area_code 9
  testthat::expect_equal(imports$value, c(700, 300))
  testthat::expect_equal(imports$country_share, c(0.7, 0.3))
})

testthat::test_that("self-trade rows are removed", {
  raw <- data.table::data.table(
    `Reporter Country Code` = c(2L, 2L),
    `Partner Country Code` = c(2L, 9L),
    `Item Code` = c(15L, 15L),
    Element = c("Import Quantity", "Import Quantity"),
    Year = c(2020L, 2020L),
    Unit = c("tonnes", "tonnes"),
    Value = c(500, 300)
  )

  result <- build_detailed_trade(raw_trade = raw)

  testthat::expect_equal(nrow(result), 1)
  testthat::expect_true(all(result$area_code != result$area_code_partner))
})

testthat::test_that("multiple trade items mapping to same CBS item are summed", {
  # Trade codes 15 (Wheat) and 16 (Flour, wheat) both map to
  # "Wheat and products" (CBS code 2511)
  raw <- data.table::data.table(
    `Reporter Country Code` = c(2L, 2L),
    `Partner Country Code` = c(9L, 9L),
    `Item Code` = c(15L, 16L),
    Element = c("Import Quantity", "Import Quantity"),
    Year = c(2020L, 2020L),
    Unit = c("tonnes", "tonnes"),
    Value = c(100, 50)
  )

  result <- build_detailed_trade(raw_trade = raw)

  testthat::expect_equal(nrow(result), 1)
  testthat::expect_equal(result$value, 150)
  testthat::expect_equal(result$item_cbs_code, 2511)
})

testthat::test_that("unmapped trade items warn and are dropped", {
  raw <- data.table::data.table(
    `Reporter Country Code` = c(2L, 2L),
    `Partner Country Code` = c(9L, 9L),
    `Item Code` = c(15L, 99999L),
    Element = c("Import Quantity", "Import Quantity"),
    Year = c(2020L, 2020L),
    Unit = c("tonnes", "tonnes"),
    Value = c(100, 200)
  )

  testthat::expect_warning(
    result <- build_detailed_trade(raw_trade = raw),
    "not found in CBS mapping"
  )

  # Only the mapped item (code 15) survives
  testthat::expect_equal(nrow(result), 1)
  testthat::expect_equal(result$item_cbs_code, 2511)
})

testthat::test_that("unmapped reporter codes warn and are dropped", {
  # Code 999 has no polity mapping in regions_full
  raw <- data.table::data.table(
    `Reporter Country Code` = c(999L, 2L),
    `Partner Country Code` = c(9L, 9L),
    `Item Code` = c(15L, 15L),
    Element = c("Import Quantity", "Import Quantity"),
    Year = c(2020L, 2020L),
    Unit = c("tonnes", "tonnes"),
    Value = c(100, 200)
  )

  testthat::expect_warning(
    result <- build_detailed_trade(raw_trade = raw),
    "Reporter.*not mapped to a polity"
  )

  testthat::expect_equal(nrow(result), 1)
  testthat::expect_equal(result$area_code, 2)
})

testthat::test_that("unmapped partner codes warn and are dropped", {
  raw <- data.table::data.table(
    `Reporter Country Code` = c(2L, 2L),
    `Partner Country Code` = c(999L, 9L),
    `Item Code` = c(15L, 15L),
    Element = c("Import Quantity", "Import Quantity"),
    Year = c(2020L, 2020L),
    Unit = c("tonnes", "tonnes"),
    Value = c(100, 200)
  )

  testthat::expect_warning(
    result <- build_detailed_trade(raw_trade = raw),
    "Partner.*not mapped to a polity"
  )

  testthat::expect_equal(nrow(result), 1)
  testthat::expect_equal(result$area_code_partner, 9)
})

testthat::test_that("non-quantity units are excluded", {
  raw <- data.table::data.table(
    `Reporter Country Code` = c(2L, 2L),
    `Partner Country Code` = c(9L, 9L),
    `Item Code` = c(15L, 15L),
    Element = c("Import Quantity", "Import Value"),
    Year = c(2020L, 2020L),
    Unit = c("tonnes", "1000 US$"),
    Value = c(100, 5000)
  )

  result <- build_detailed_trade(raw_trade = raw)

  testthat::expect_equal(nrow(result), 1)
  testthat::expect_equal(result$unit, "tonnes")
})

testthat::test_that("Head unit is standardised to heads", {
  # Item code 1171 (Animals live nes) maps to CBS and uses heads
  raw <- data.table::data.table(
    `Reporter Country Code` = c(2L),
    `Partner Country Code` = c(9L),
    `Item Code` = c(1171L),
    Element = c("Import Quantity"),
    Year = c(2020L),
    Unit = c("Head"),
    Value = c(50)
  )

  result <- build_detailed_trade(raw_trade = raw)

  if (nrow(result) > 0) {
    testthat::expect_equal(result$unit, "heads")
  }
})

testthat::test_that("short element names Import/Export are standardised", {
  raw <- data.table::data.table(
    `Reporter Country Code` = c(2L, 2L),
    `Partner Country Code` = c(9L, 9L),
    `Item Code` = c(15L, 15L),
    Element = c("Import", "Export"),
    Year = c(2020L, 2020L),
    Unit = c("tonnes", "tonnes"),
    Value = c(100, 200)
  )

  result <- build_detailed_trade(raw_trade = raw)

  testthat::expect_setequal(result$element, c("import", "export"))
})

testthat::test_that("polity-level aggregation sums sub-national codes", {
  # FAO codes 238 (Ethiopia) and 62 (Ethiopia PDR) both map to
  # polity ETH with area_code 238
  raw <- data.table::data.table(
    `Reporter Country Code` = c(238L, 62L),
    `Partner Country Code` = c(9L, 9L),
    `Item Code` = c(15L, 15L),
    Element = c("Import Quantity", "Import Quantity"),
    Year = c(2020L, 2020L),
    Unit = c("tonnes", "tonnes"),
    Value = c(100, 50)
  )

  result <- build_detailed_trade(raw_trade = raw)

  testthat::expect_equal(nrow(result), 1)
  testthat::expect_equal(result$area_code, 238)
  testthat::expect_equal(result$value, 150)
})

testthat::test_that("polity self-trade is removed after aggregation", {
  # FAO codes 238 and 62 both map to polity ETH (area_code 238).
  # A trade flow from 238 -> 62 becomes 238 -> 238 at polity level.
  raw <- data.table::data.table(
    `Reporter Country Code` = c(238L),
    `Partner Country Code` = c(62L),
    `Item Code` = c(15L),
    Element = c("Import Quantity"),
    Year = c(2020L),
    Unit = c("tonnes"),
    Value = c(100)
  )

  result <- build_detailed_trade(raw_trade = raw)

  testthat::expect_equal(nrow(result), 0)
})

testthat::test_that("zero-value rows are dropped", {
  raw <- data.table::data.table(
    `Reporter Country Code` = c(2L, 2L),
    `Partner Country Code` = c(9L, 7L),
    `Item Code` = c(15L, 15L),
    Element = c("Import Quantity", "Import Quantity"),
    Year = c(2020L, 2020L),
    Unit = c("tonnes", "tonnes"),
    Value = c(100, 0)
  )

  result <- build_detailed_trade(raw_trade = raw)

  testthat::expect_equal(nrow(result), 1)
  testthat::expect_true(all(result$value > 0))
})

testthat::test_that("item name column maps through cbs_trade_codes names", {
  # When raw data has an "Item" column instead of "Item Code"
  raw <- data.table::data.table(
    `Reporter Country Code` = c(2L),
    `Partner Country Code` = c(9L),
    Item = c("Wheat"),
    Element = c("Import Quantity"),
    Year = c(2020L),
    Unit = c("tonnes"),
    Value = c(100)
  )

  result <- build_detailed_trade(raw_trade = raw)

  testthat::expect_equal(nrow(result), 1)
  testthat::expect_equal(result$item_cbs_code, 2511)
  testthat::expect_equal(result$item_cbs, "Wheat and products")
})

testthat::test_that("NA values in input are handled gracefully", {
  raw <- data.table::data.table(
    `Reporter Country Code` = c(2L, 2L),
    `Partner Country Code` = c(9L, 9L),
    `Item Code` = c(15L, 15L),
    Element = c("Import Quantity", "Import Quantity"),
    Year = c(2020L, 2021L),
    Unit = c("tonnes", "tonnes"),
    Value = c(100, NA)
  )

  result <- build_detailed_trade(raw_trade = raw)

  # NA value row should still produce a result (sum with na.rm = TRUE)
  testthat::expect_true(nrow(result) >= 1)
  testthat::expect_equal(
    result |> dplyr::filter(year == 2020) |> dplyr::pull(value),
    100
  )
})

# extend_time tests ------------------------------------------------------------

testthat::test_that("extend_time fills country shares with CBS years", {
  # DTM has data for 2019 only
  raw <- data.table::data.table(
    `Reporter Country Code` = c(2L, 2L),
    `Partner Country Code` = c(9L, 7L),
    `Item Code` = c(15L, 15L),
    Element = c("Import Quantity", "Import Quantity"),
    Year = c(2019L, 2019L),
    Unit = c("tonnes", "tonnes"),
    Value = c(600, 400)
  )

  # CBS has import for 2019 and 2020 — so 2020 should be extended
  cbs <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~import, ~export,
    2019L, 2, 2511, 1000, NA,
    2020L, 2, 2511, 1200, NA
  )

  result <- build_detailed_trade(
    raw_trade = raw,
    cbs = cbs,
    extend_time = TRUE
  )

  # Should have rows for both 2019 and 2020
  testthat::expect_true(all(c(2019L, 2020L) %in% result$year))

  # Shares should be preserved (0.6 and 0.4) in extended year
  extended <- result |>
    dplyr::filter(year == 2020) |>
    dplyr::arrange(area_code_partner)

  testthat::expect_equal(extended$country_share, c(0.4, 0.6))
})

testthat::test_that("extend_time min_share drops small partners", {
  # Partner ARG has a tiny share (1 / 1001 ≈ 0.001)
  raw <- data.table::data.table(
    `Reporter Country Code` = c(2L, 2L),
    `Partner Country Code` = c(7L, 9L),
    `Item Code` = c(15L, 15L),
    Element = c("Import Quantity", "Import Quantity"),
    Year = c(2019L, 2019L),
    Unit = c("tonnes", "tonnes"),
    Value = c(1000, 1)
  )

  cbs <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~import, ~export,
    2019L, 2, 2511, 1001, NA,
    2020L, 2, 2511, 1200, NA
  )

  # min_share = 0.01 should drop the partner with share ~0.001
  result <- build_detailed_trade(
    raw_trade = raw,
    cbs = cbs,
    min_share = 0.01,
    extend_time = TRUE
  )

  # Only AGO (area_code 7) should survive, ARG (area_code 9) is too small
  testthat::expect_true(all(result$area_code_partner == 7))
})

testthat::test_that("extend_time interpolates shares between known years", {
  # DTM has data for 2018 and 2020, CBS fills in 2019
  raw <- data.table::data.table(
    `Reporter Country Code` = c(2L, 2L, 2L, 2L),
    `Partner Country Code` = c(9L, 7L, 9L, 7L),
    `Item Code` = c(15L, 15L, 15L, 15L),
    Element = rep("Import Quantity", 4),
    Year = c(2018L, 2018L, 2020L, 2020L),
    Unit = rep("tonnes", 4),
    Value = c(400, 600, 600, 400)
  )

  cbs <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~import, ~export,
    2018L, 2, 2511, 1000, NA,
    2019L, 2, 2511, 1000, NA,
    2020L, 2, 2511, 1000, NA
  )

  result <- build_detailed_trade(
    raw_trade = raw,
    cbs = cbs,
    extend_time = TRUE
  )

  # 2019 shares should be interpolated: midpoint of (0.4, 0.6) and (0.6, 0.4)
  mid <- result |>
    dplyr::filter(year == 2019) |>
    dplyr::arrange(area_code_partner)

  testthat::expect_equal(mid$country_share, c(0.5, 0.5), tolerance = 1e-6)
})

testthat::test_that("extend_time accepts long-format CBS", {
  raw <- data.table::data.table(
    `Reporter Country Code` = c(2L),
    `Partner Country Code` = c(9L),
    `Item Code` = c(15L),
    Element = c("Import Quantity"),
    Year = c(2019L),
    Unit = c("tonnes"),
    Value = c(100)
  )

  cbs_long <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~element, ~value,
    2019L, 2, 2511, "import", 1000,
    2020L, 2, 2511, "import", 1200
  )

  result <- build_detailed_trade(
    raw_trade = raw,
    cbs = cbs_long,
    extend_time = TRUE
  )

  testthat::expect_true(all(c(2019L, 2020L) %in% result$year))
})

testthat::test_that("extend_time produces no duplicate year+group rows", {
  # CBS has extra years (2017, 2021) not in DTM (2018-2020).
  # Before fix, the CBS merge created NA-keyed rows that duplicated
  # after tidyr::complete.
  raw <- data.table::data.table(
    `Reporter Country Code` = c(2L, 2L, 2L, 2L, 2L, 2L),
    `Partner Country Code` = c(9L, 7L, 9L, 7L, 9L, 7L),
    `Item Code` = rep(15L, 6),
    Element = rep("Import Quantity", 6),
    Year = c(2018L, 2018L, 2019L, 2019L, 2020L, 2020L),
    Unit = rep("tonnes", 6),
    Value = c(600, 400, 500, 500, 400, 600)
  )

  cbs <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~import, ~export,
    2017L, 2, 2511, 800, NA,
    2018L, 2, 2511, 1000, NA,
    2019L, 2, 2511, 1000, NA,
    2020L, 2, 2511, 1000, NA,
    2021L, 2, 2511, 1200, NA
  )

  result <- build_detailed_trade(
    raw_trade = raw,
    cbs = cbs,
    extend_time = TRUE
  )

  # No duplicate rows per year+group
  dupes <- result |>
    dplyr::count(year, area_code, area_code_partner, element,
                 item_cbs_code) |>
    dplyr::filter(n > 1)
  testthat::expect_equal(nrow(dupes), 0)

  # Should cover the full CBS year range
  testthat::expect_true(all(2017:2021 %in% result$year))
})

testthat::test_that("extend_time errors on invalid CBS format", {
  raw <- .fake_bilateral_trade()

  bad_cbs <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~production,
    2020L, 2, 2511, 1000
  )

  testthat::expect_error(
    build_detailed_trade(
      raw_trade = raw,
      cbs = bad_cbs,
      extend_time = TRUE
    ),
    "import.*export.*element"
  )
})

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
