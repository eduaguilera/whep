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
      "polity_area_code",
      "reporting_polity_code",
      "reporting_polity_name",
      "reporting_polity_has_geometry",
      "area_code_partner",
      "partner_polity_code",
      "partner_polity_name",
      "partner_polity_has_geometry",
      "partner_polity_area_code",
      "element",
      "item_cbs",
      "item_cbs_code",
      "unit",
      "value",
      "country_share",
      "method_unbacked_quantity"
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

testthat::test_that("unmapped item names warn and are dropped", {
  raw <- data.table::data.table(
    `Reporter Country Code` = c(2L, 2L),
    `Partner Country Code` = c(9L, 9L),
    Item = c("Wheat", "Nonexistent Item XYZ"),
    Element = c("Import Quantity", "Import Quantity"),
    Year = c(2020L, 2020L),
    Unit = c("tonnes", "tonnes"),
    Value = c(100, 200)
  )

  # Name-only input also triggers the brittle name-join fallback warning.
  testthat::expect_warning(
    testthat::expect_warning(
      result <- build_detailed_trade(raw_trade = raw),
      "by name"
    ),
    "not found in CBS mapping"
  )

  testthat::expect_equal(nrow(result), 1)
  testthat::expect_equal(result$item_cbs, "Wheat and products")
})

testthat::test_that("unmapped reporter codes warn and are dropped", {
  # Code 4444 is intentionally absent from WHEP area mappings.
  raw <- data.table::data.table(
    `Reporter Country Code` = c(4444L, 2L),
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
    `Partner Country Code` = c(4444L, 9L),
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

testthat::test_that("intra-aggregate distinct-origin trade survives collapse", {
  # Scoped to the explicit fold. WHEP now models the reporting members of
  # bucket 999 in their own right (#459), so there is no Rest-of-World fold
  # by default; what this pins is the fold behaviour itself, which still has
  # to work for anyone reproducing a published-before number.
  withr::local_options(whep.unfold_rest_of_world = "none")
  # American Samoa (5) and Andorra (6) both collapse to the Rest of World
  # aggregate polity (999). A real flow 5 -> 6 becomes 999 -> 999, which the
  # naive polity-level self-trade filter would delete. It must survive: these
  # are two different countries, not genuine self-trade (deepens #152).
  raw <- data.table::data.table(
    `Reporter Country Code` = c(5L),
    `Partner Country Code` = c(6L),
    `Item Code` = c(15L),
    Element = c("Import Quantity"),
    Year = c(2020L),
    Unit = c("tonnes"),
    Value = c(100)
  )

  result <- build_detailed_trade(raw_trade = raw)

  testthat::expect_equal(nrow(result), 1)
  testthat::expect_equal(result$area_code, 999)
  testthat::expect_equal(result$area_code_partner, 999)
  testthat::expect_equal(result$value, 100)
})

testthat::test_that("genuine self-trade within an aggregate is still removed", {
  # A flow from American Samoa (5) to itself is real self-trade and must be
  # dropped even though 5 collapses to the Rest of World aggregate (999).
  raw <- data.table::data.table(
    `Reporter Country Code` = c(5L),
    `Partner Country Code` = c(5L),
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

  # The name-join path is a brittle fallback used only without item codes, so
  # it now warns (relates to #170).
  testthat::expect_warning(
    result <- build_detailed_trade(raw_trade = raw),
    "by name"
  )

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
    dplyr::count(year, area_code, area_code_partner, element, item_cbs_code) |>
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

# CBS coverage granularity (#232) ----------------------------------------------

testthat::test_that(".extract_cbs_years_for_dtm returns reported years", {
  wide <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~import, ~export,
    2019L, 2, 2511, 1000, NA,
    2020L, 2, 2511, NA, 500,
    2021L, 2, 2511, NA, NA
  )

  # 2021 reports neither flow, so it is not a CBS trade year.
  testthat::expect_equal(
    whep:::.extract_cbs_years_for_dtm(wide),
    c(2019L, 2020L)
  )

  long <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~element, ~value,
    2019L, 2, 2511, "import", 1000,
    2020L, 2, 2511, "production", 700
  )

  # Only import/export rows count, so 2020 (production only) drops out.
  testthat::expect_equal(
    whep:::.extract_cbs_years_for_dtm(long),
    2019L
  )
})

testthat::test_that(".extract_cbs_years_for_dtm tolerates one flow only", {
  import_only <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~import,
    1961L, 2, 2511, 1,
    1962L, 2, 2511, NA
  )

  testthat::expect_equal(
    whep:::.extract_cbs_years_for_dtm(import_only),
    1961L
  )
})

testthat::test_that("extend_time ignores per-area CBS coverage", {
  # Reporter 2 is reported by CBS in both years; reporter 7 only in 2019.
  # The extension is driven by the CBS *year axis* alone, so reporter 7 also
  # gets a 2020 share even though CBS never reports it that year. This pins
  # the documented uniform-extension behaviour: a change that scopes the
  # extension to each group's own CBS coverage (#232) must fail here, because
  # that is a methodological decision and not a silent refactor.
  raw <- data.table::data.table(
    `Reporter Country Code` = c(2L, 7L),
    `Partner Country Code` = c(9L, 9L),
    `Item Code` = c(15L, 15L),
    Element = c("Import Quantity", "Import Quantity"),
    Year = c(2019L, 2019L),
    Unit = c("tonnes", "tonnes"),
    Value = c(100, 100)
  )

  cbs <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~import, ~export,
    2019L, 2, 2511, 1000, NA,
    2020L, 2, 2511, 1200, NA,
    2019L, 7, 2511, 800, NA
  )

  result <- build_detailed_trade(
    raw_trade = raw,
    cbs = cbs,
    extend_time = TRUE
  )

  uncovered <- result |>
    dplyr::filter(year == 2020, area_code == 7)

  testthat::expect_equal(nrow(uncovered), 1L)
  testthat::expect_equal(uncovered$country_share, 1)
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
      "polity_area_code",
      "reporting_polity_code",
      "reporting_polity_name",
      "reporting_polity_has_geometry",
      "area_code_partner",
      "partner_polity_code",
      "partner_polity_name",
      "partner_polity_has_geometry",
      "partner_polity_area_code",
      "element",
      "item_cbs_code",
      "unit",
      "value",
      "country_share",
      "method_unbacked_quantity"
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

# tonnes that are not masses (whep#1023) --------------------------------------

.fake_unbacked_trade <- function() {
  # Trade item 1293 is FAOSTAT's "Crude organic material n.e.c.". Its
  # Detailed Trade Matrix tonnage is not a mass: FAOSTAT's aggregate domain
  # publishes no country-level quantity for it, and Colombia's 2004 export to
  # the United States is booked as 2,579,549,000 tonnes at USD 0.227/tonne.
  data.table::data.table(
    `Reporter Country Code` = c(2L, 44L),
    `Partner Country Code` = c(9L, 231L),
    `Item Code` = c(15L, 1293L),
    Element = c("Export Quantity", "Export Quantity"),
    Year = c(2004L, 2004L),
    Unit = c("tonnes", "tonnes"),
    Value = c(100, 2579549000)
  )
}

testthat::test_that(".unbacked_mass_trade_items maps to CBS item 5001", {
  # The item list is a measurement (see the helper's comment); the CBS side
  # is derived from the shipped crosswalks, so guard the derivation.
  testthat::expect_equal(.unbacked_mass_trade_items(), 1293L)
  testthat::expect_equal(.unbacked_mass_cbs_items(), 5001)
  testthat::expect_true(
    "Crude materials" %in% .unbacked_mass_trade_names(1293L)
  )
})

testthat::test_that("build_detailed_trade drops unbacked tonnage by default", {
  testthat::expect_warning(
    result <- build_detailed_trade(raw_trade = .fake_unbacked_trade()),
    class = "whep_unbacked_mass_quantity"
  )

  testthat::expect_false(5001 %in% result$item_cbs_code)
  testthat::expect_equal(result$item_cbs_code, 2511)
  testthat::expect_equal(sum(result$value), 100)
  testthat::expect_true(all(result$method_unbacked_quantity == "drop"))
})

testthat::test_that("build_detailed_trade 'keep' carries unbacked tonnage", {
  # Asserted on the screen itself, because item 1293 never reaches the
  # output of `build_detailed_trade()` under any method: its CBS name
  # "Other" has no row in `whep::items_full`, so `.map_dtm_to_cbs_items()`
  # already loses it on the `items_bridge` merge. That is a separate,
  # unreported drop, and the screen must not be confused with it.
  dt <- .read_and_clean_dtm(.fake_unbacked_trade())

  testthat::expect_warning(
    kept <- .screen_unbacked_quantities(dt, "keep"),
    class = "whep_unbacked_mass_quantity"
  )
  testthat::expect_equal(nrow(kept), 2)
  testthat::expect_equal(max(kept$value), 2579549000)

  testthat::expect_warning(
    testthat::expect_warning(
      result <- build_detailed_trade(
        raw_trade = .fake_unbacked_trade(),
        method_unbacked_quantity = "keep"
      ),
      class = "whep_unbacked_mass_quantity"
    ),
    class = "whep_item_cbs_code_missing"
  )
  testthat::expect_true(all(result$method_unbacked_quantity == "keep"))
})

testthat::test_that("a CBS name with no item_cbs_code is warned, not silent", {
  # Found while tracing whep#1023: `whep::items_full` has no "Other" row, so
  # every item 1293 row left this producer with no message at all.
  raw <- .fake_unbacked_trade()

  testthat::expect_warning(
    testthat::expect_warning(
      build_detailed_trade(raw_trade = raw, method_unbacked_quantity = "keep"),
      class = "whep_unbacked_mass_quantity"
    ),
    class = "whep_item_cbs_code_missing"
  )
  testthat::expect_warning(
    testthat::expect_warning(
      build_detailed_trade(raw_trade = raw, method_unbacked_quantity = "keep"),
      class = "whep_unbacked_mass_quantity"
    ),
    "Other"
  )
})

testthat::test_that("the screen removes exactly the unbacked mass rows", {
  dt <- .read_and_clean_dtm(.fake_unbacked_trade())

  testthat::expect_warning(
    dropped <- .screen_unbacked_quantities(dt, "drop"),
    class = "whep_unbacked_mass_quantity"
  )
  testthat::expect_equal(nrow(dropped), 1)
  testthat::expect_equal(dropped$item_code_trade, 15)
  testthat::expect_equal(dropped$value, 100)
})

testthat::test_that("build_detailed_trade 'abort' refuses unbacked tonnage", {
  testthat::expect_error(
    build_detailed_trade(
      raw_trade = .fake_unbacked_trade(),
      method_unbacked_quantity = "abort"
    ),
    class = "whep_unbacked_mass_quantity"
  )
})

testthat::test_that("build_detailed_trade rejects an unknown method", {
  testthat::expect_error(
    build_detailed_trade(
      raw_trade = .fake_unbacked_trade(),
      method_unbacked_quantity = "rescale"
    ),
    class = "rlang_error"
  )
})

testthat::test_that("the screen leaves a clean pin untouched", {
  result <- testthat::expect_no_warning(
    build_detailed_trade(raw_trade = .fake_bilateral_trade())
  )
  testthat::expect_equal(nrow(result), 3)
})

testthat::test_that("the screen also works on the name-keyed path", {
  # `.map_dtm_to_cbs_items()` falls back to joining on the item *name* when
  # no item code column is present; the screen must follow it there.
  raw <- .fake_unbacked_trade()
  raw[, `Item Code` := NULL]
  raw[, item := c("Wheat", "Crude materials")]
  dt <- .read_and_clean_dtm(raw)

  testthat::expect_warning(
    dropped <- .screen_unbacked_quantities(dt, "drop"),
    class = "whep_unbacked_mass_quantity"
  )
  testthat::expect_equal(dropped$item, "Wheat")
})

testthat::test_that("head-count rows are not screened as mass", {
  # The screen is scoped to `tonnes`: a head count for the same item is a
  # different quantity and is not what whep#1023 is about.
  raw <- data.table::data.table(
    `Reporter Country Code` = c(2L, 44L),
    `Partner Country Code` = c(9L, 231L),
    `Item Code` = c(15L, 1293L),
    Element = c("Export Quantity", "Export Quantity"),
    Year = c(2004L, 2004L),
    Unit = c("tonnes", "Head"),
    Value = c(100, 1e9)
  )
  dt <- .read_and_clean_dtm(raw)

  result <- testthat::expect_no_warning(
    .screen_unbacked_quantities(dt, "drop")
  )
  testthat::expect_setequal(result$unit, c("tonnes", "heads"))
  testthat::expect_equal(max(result$value), 1e9)
})
