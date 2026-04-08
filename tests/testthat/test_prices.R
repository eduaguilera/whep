# Helper fixtures --------------------------------------------------------------

# Minimal FAOSTAT trade totals data. The .read_fao_trade() output has
# columns: year, item_trade, item_code_trade, unit, element, value
# (plus area_code, area from polity aggregation — not needed for prices
# since .compute_trade_prices aggregates globally).
.fake_trade_totals <- function() {
  data.table::data.table(
    year = rep(2020L, 4),
    item_trade = rep("Wheat", 4),
    item_code_trade = rep(15, 4),
    unit = c("kdollars", "tonnes", "kdollars", "tonnes"),
    element = c("import", "import", "export", "export"),
    value = c(500, 1000, 300, 600)
  )
}

# Pre-computed trade prices (the output of build_trade_prices)
.fake_trade_prices <- function() {
  data.table::data.table(
    year = c(2020L, 2020L),
    item_trade = c("Wheat", "Wheat"),
    item_code_trade = c(15, 15),
    element = c("import", "export"),
    kdollars = c(500, 300),
    tonnes = c(1000, 600),
    price = c(0.5, 0.5)
  )
}

# build_trade_prices tests -----------------------------------------------------

testthat::test_that("build_trade_prices computes price from raw data", {
  result <- build_trade_prices(raw_trade = .fake_trade_totals())

  testthat::expect_s3_class(result, "data.table")
  testthat::expect_true(all(
    c("year", "item_trade", "item_code_trade", "element",
      "kdollars", "tonnes", "price") %in% names(result)
  ))

  import_row <- result[element == "import"]
  testthat::expect_equal(import_row$price, 0.5)

  export_row <- result[element == "export"]
  testthat::expect_equal(export_row$price, 0.5)
})

testthat::test_that("build_trade_prices drops zero and NA values", {
  raw <- data.table::data.table(
    year = rep(2020L, 6),
    item_trade = rep("Wheat", 6),
    item_code_trade = rep(15, 6),
    unit = c("kdollars", "tonnes", "kdollars", "tonnes",
             "kdollars", "tonnes"),
    element = rep("import", 6),
    value = c(500, 1000, 0, 0, NA, NA)
  )

  result <- build_trade_prices(raw_trade = raw)

  # Only the first pair (500/1000) should contribute
  testthat::expect_equal(nrow(result), 1)
  testthat::expect_equal(result$kdollars, 500)
  testthat::expect_equal(result$tonnes, 1000)
})

testthat::test_that("build_trade_prices aggregates across countries", {
  raw <- data.table::data.table(
    year = rep(2020L, 4),
    item_trade = rep("Wheat", 4),
    item_code_trade = rep(15, 4),
    unit = c("kdollars", "tonnes", "kdollars", "tonnes"),
    element = rep("export", 4),
    value = c(100, 200, 300, 800),
    area_code = c(2, 2, 9, 9)
  )

  result <- build_trade_prices(raw_trade = raw)

  testthat::expect_equal(result$kdollars, 400)
  testthat::expect_equal(result$tonnes, 1000)
  testthat::expect_equal(result$price, 0.4)
})

testthat::test_that("build_trade_prices filters non-quantity units", {
  raw <- data.table::data.table(
    year = rep(2020L, 3),
    item_trade = rep("Wheat", 3),
    item_code_trade = rep(15, 3),
    unit = c("kdollars", "tonnes", "heads"),
    element = rep("import", 3),
    value = c(500, 1000, 50)
  )

  result <- build_trade_prices(raw_trade = raw)

  # heads should be dropped — only kdollars and tonnes kept
  testthat::expect_equal(nrow(result), 1)
  testthat::expect_equal(result$price, 0.5)
})

testthat::test_that("build_trade_prices standardises unit names", {
  raw <- data.table::data.table(
    year = rep(2020L, 2),
    item_trade = rep("Wheat", 2),
    item_code_trade = rep(15, 2),
    unit = c("1000 US$", "tonnes"),
    element = rep("export", 2),
    value = c(300, 600)
  )

  result <- build_trade_prices(raw_trade = raw)

  testthat::expect_equal(result$kdollars, 300)
  testthat::expect_equal(result$price, 0.5)
})

testthat::test_that("build_trade_prices returns empty when no monetary data", {
  # Only quantity rows, no "1000 US$" or "kdollars" — should produce 0 rows
  raw <- data.table::data.table(
    year = c(2020L),
    item_trade = c("Wheat"),
    item_code_trade = c(15),
    unit = c("tonnes"),
    element = c("import"),
    value = c(1000)
  )

  result <- build_trade_prices(raw_trade = raw)

  testthat::expect_equal(nrow(result), 0)
})

testthat::test_that("build_trade_prices handles 1000 USD unit variant", {
  raw <- data.table::data.table(
    year = rep(2020L, 2),
    item_trade = rep("Wheat", 2),
    item_code_trade = rep(15, 2),
    unit = c("1000 USD", "tonnes"),
    element = rep("export", 2),
    value = c(300, 600)
  )

  result <- build_trade_prices(raw_trade = raw)

  testthat::expect_equal(result$kdollars, 300)
  testthat::expect_equal(result$price, 0.5)
})

testthat::test_that("build_trade_prices keeps multiple items separate", {
  raw <- data.table::data.table(
    year = rep(2020L, 4),
    item_trade = c("Wheat", "Wheat", "Rice", "Rice"),
    item_code_trade = c(15, 15, 27, 27),
    unit = c("kdollars", "tonnes", "kdollars", "tonnes"),
    element = rep("export", 4),
    value = c(500, 1000, 200, 100)
  )

  result <- build_trade_prices(raw_trade = raw)

  testthat::expect_equal(nrow(result), 2)
  wheat <- result[item_code_trade == 15]
  rice <- result[item_code_trade == 27]
  testthat::expect_equal(wheat$price, 0.5)
  testthat::expect_equal(rice$price, 2.0)
})

testthat::test_that("build_trade_prices drops infinite prices", {
  raw <- data.table::data.table(
    year = rep(2020L, 2),
    item_trade = rep("Wheat", 2),
    item_code_trade = rep(15, 2),
    unit = c("kdollars", "tonnes"),
    element = rep("export", 2),
    value = c(500, 0)
  )

  # 0 tonnes rows are dropped as zero values, leaving no tonnes →
  # no price row after dcast
  result <- build_trade_prices(raw_trade = raw)

  testthat::expect_equal(nrow(result), 0)
})

# build_primary_prices tests ---------------------------------------------------

testthat::test_that("build_primary_prices uses export trade prices", {
  primary_prod <- tibble::tribble(
    ~year, ~area_code, ~item_prod_code, ~unit, ~value,
    2020L, 2, "15", "tonnes", 5000,
    2020L, 9, "15", "tonnes", 3000
  )

  trade_prices <- .fake_trade_prices()

  result <- build_primary_prices(
    primary_prod = primary_prod,
    trade_prices = trade_prices
  )

  testthat::expect_s3_class(result, "data.table")
  testthat::expect_true(all(c("year", "item_prod_code", "price") %in%
    names(result)))

  # Should use export price (0.5)
  wheat <- result[item_prod_code == "15"]
  testthat::expect_equal(wheat$price, 0.5)
})

testthat::test_that("build_primary_prices falls back to production value", {
  primary_prod <- tibble::tribble(
    ~year, ~area_code, ~item_prod_code, ~unit, ~value,
    2020L, 2, "15", "tonnes", 5000
  )

  # No trade prices for this item
  trade_prices <- data.table::data.table(
    year = integer(),
    item_trade = character(),
    item_code_trade = numeric(),
    element = character(),
    kdollars = numeric(),
    tonnes = numeric(),
    price = numeric()
  )

  # Value of production with FAOSTAT column names
  vop <- data.table::data.table(
    Item.Code = "15",
    Area.Code = 2L,
    Element = "Gross Production Value (constant 2014-2016 thousand US$)",
    Unit = "1000 US$",
    Year = 2020L,
    Value = 2500
  )

  result <- build_primary_prices(
    primary_prod = primary_prod,
    value_of_production = vop,
    trade_prices = trade_prices
  )

  wheat <- result[item_prod_code == "15"]
  # price_prod = 2500 / 5000 = 0.5
  testthat::expect_equal(wheat$price, 0.5)
})

testthat::test_that("build_primary_prices prefers export over production", {
  primary_prod <- tibble::tribble(
    ~year, ~area_code, ~item_prod_code, ~unit, ~value,
    2020L, 2, "15", "tonnes", 5000
  )

  trade_prices <- .fake_trade_prices()

  vop <- data.table::data.table(
    Item.Code = "15",
    Area.Code = 2L,
    Element = "Gross Production Value (constant 2014-2016 thousand US$)",
    Unit = "1000 US$",
    Year = 2020L,
    Value = 10000
  )

  result <- build_primary_prices(
    primary_prod = primary_prod,
    value_of_production = vop,
    trade_prices = trade_prices
  )

  wheat <- result[item_prod_code == "15"]
  # Export price is 0.5, production price would be 10000/5000=2.0
  # Should prefer export
  testthat::expect_equal(wheat$price, 0.5)
})

testthat::test_that("build_primary_prices handles space-separated VoP columns", {
  # Pin data has "Item Code" not "Item.Code"
  primary_prod <- tibble::tribble(
    ~year, ~area_code, ~item_prod_code, ~unit, ~value,
    2020L, 2, "15", "tonnes", 5000
  )

  trade_prices <- data.table::data.table(
    year = integer(), item_trade = character(),
    item_code_trade = numeric(), element = character(),
    kdollars = numeric(), tonnes = numeric(), price = numeric()
  )

  vop <- data.table::data.table(
    `Item Code` = 15L,
    `Area Code` = 2L,
    Element = "Gross Production Value (constant 2014-2016 thousand US$)",
    Unit = "1000 US$",
    Year = 2020L,
    Value = 2500
  )

  result <- build_primary_prices(
    primary_prod = primary_prod,
    value_of_production = vop,
    trade_prices = trade_prices
  )

  wheat <- result[item_prod_code == "15"]
  testthat::expect_equal(wheat$price, 0.5)
})

testthat::test_that("build_primary_prices does not mutate caller's VoP", {
  primary_prod <- tibble::tribble(
    ~year, ~area_code, ~item_prod_code, ~unit, ~value,
    2020L, 2, "15", "tonnes", 5000
  )

  trade_prices <- data.table::data.table(
    year = integer(), item_trade = character(),
    item_code_trade = numeric(), element = character(),
    kdollars = numeric(), tonnes = numeric(), price = numeric()
  )

  vop <- tibble::tribble(
    ~`Item Code`, ~`Area Code`, ~Element, ~Unit, ~Year, ~Value,
    15L, 2L,
    "Gross Production Value (constant 2014-2016 thousand US$)",
    "1000 US$", 2020L, 2500
  )

  original_names <- names(vop)

  build_primary_prices(
    primary_prod = primary_prod,
    value_of_production = vop,
    trade_prices = trade_prices
  )

  # Caller's tibble should be unchanged
  testthat::expect_equal(names(vop), original_names)
})

testthat::test_that("build_primary_prices VoP works on second call", {
  primary_prod <- tibble::tribble(
    ~year, ~area_code, ~item_prod_code, ~unit, ~value,
    2020L, 2, "15", "tonnes", 5000
  )

  trade_prices <- data.table::data.table(
    year = integer(), item_trade = character(),
    item_code_trade = numeric(), element = character(),
    kdollars = numeric(), tonnes = numeric(), price = numeric()
  )

  vop <- tibble::tribble(
    ~`Item Code`, ~`Area Code`, ~Element, ~Unit, ~Year, ~Value,
    15L, 2L,
    "Gross Production Value (constant 2014-2016 thousand US$)",
    "1000 US$", 2020L, 2500
  )

  # Call twice with same vop — second call should not fail
  r1 <- build_primary_prices(
    primary_prod = primary_prod,
    value_of_production = vop,
    trade_prices = trade_prices
  )
  r2 <- build_primary_prices(
    primary_prod = primary_prod,
    value_of_production = vop,
    trade_prices = trade_prices
  )

  testthat::expect_equal(r1, r2)
})

testthat::test_that("build_primary_prices gap-fills missing years", {
  primary_prod <- tibble::tribble(
    ~year, ~area_code, ~item_prod_code, ~unit, ~value,
    2018L, 2, "15", "tonnes", 5000,
    2019L, 2, "15", "tonnes", 5000,
    2020L, 2, "15", "tonnes", 5000
  )

  trade_prices <- data.table::data.table(
    year = c(2018L, 2020L),
    item_trade = c("Wheat", "Wheat"),
    item_code_trade = c(15, 15),
    element = c("export", "export"),
    kdollars = c(2500, 5000),
    tonnes = c(5000, 5000),
    price = c(0.5, 1.0)
  )

  result <- build_primary_prices(
    primary_prod = primary_prod,
    trade_prices = trade_prices
  )

  prices <- result[order(year)]$price
  # 2018=0.5, 2020=1.0 → 2019 interpolated to 0.75
  testthat::expect_equal(prices, c(0.5, 0.75, 1.0))
})

# build_cbs_prices tests -------------------------------------------------------

testthat::test_that("build_cbs_prices computes prices from trade prices", {
  trade_prices <- data.table::data.table(
    year = c(2020L, 2020L),
    item_trade = c("Wheat", "Wheat"),
    item_code_trade = c(15, 15),
    element = c("import", "export"),
    kdollars = c(500, 300),
    tonnes = c(1000, 600),
    price = c(0.5, 0.5)
  )

  cbs <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code,
    2020L, 2, 2511
  )

  result <- build_cbs_prices(
    cbs = cbs,
    trade_prices = trade_prices
  )

  testthat::expect_s3_class(result, "tbl_df")
  testthat::expect_true(all(
    c("year", "element", "item_cbs_code", "price") %in% names(result)
  ))

  # Wheat and products (2511) should have prices
  wheat <- result[result$item_cbs_code == 2511, ]
  testthat::expect_true(nrow(wheat) > 0)
  testthat::expect_true(all(wheat$price > 0))
})

testthat::test_that("build_cbs_prices adds residue prices", {
  trade_prices <- data.table::data.table(
    year = c(2020L, 2020L),
    item_trade = c("Wheat", "Wheat"),
    item_code_trade = c(15, 15),
    element = c("import", "export"),
    kdollars = c(500, 300),
    tonnes = c(1000, 600),
    price = c(0.5, 0.5)
  )

  cbs <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code,
    2020L, 2, 2511
  )

  result <- build_cbs_prices(
    cbs = cbs,
    trade_prices = trade_prices,
    residue_price_factor = 0.1
  )

  # Wheat is Cereals → residue is "Straw"
  straw_code <- whep::items_full |>
    dplyr::filter(item_cbs == "Straw") |>
    dplyr::pull(item_cbs_code) |>
    unique()

  if (length(straw_code) > 0) {
    straw <- result[result$item_cbs_code == straw_code, ]
    testthat::expect_true(nrow(straw) > 0)
  }
})

testthat::test_that("build_cbs_prices residue_price_factor scales prices", {
  trade_prices <- data.table::data.table(
    year = c(2020L),
    item_trade = c("Wheat"),
    item_code_trade = c(15),
    element = c("export"),
    kdollars = c(500),
    tonnes = c(1000),
    price = c(0.5)
  )

  cbs <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code,
    2020L, 2, 2511
  )

  result_low <- build_cbs_prices(
    cbs = cbs,
    trade_prices = trade_prices,
    residue_price_factor = 0.1
  )
  result_high <- build_cbs_prices(
    cbs = cbs,
    trade_prices = trade_prices,
    residue_price_factor = 0.5
  )

  straw_code <- whep::items_full |>
    dplyr::filter(item_cbs == "Straw") |>
    dplyr::pull(item_cbs_code) |>
    unique()

  if (length(straw_code) > 0) {
    straw_low <- result_low |>
      dplyr::filter(item_cbs_code == straw_code, element == "export")
    straw_high <- result_high |>
      dplyr::filter(item_cbs_code == straw_code, element == "export")

    if (nrow(straw_low) > 0 && nrow(straw_high) > 0) {
      testthat::expect_true(straw_high$price > straw_low$price)
    }
  }
})

testthat::test_that("build_cbs_prices adds proxy prices for missing items", {
  # Wheat trade price should generate Brans proxy at 0.2x
  trade_prices <- data.table::data.table(
    year = c(2020L),
    item_trade = c("Wheat"),
    item_code_trade = c(15),
    element = c("export"),
    kdollars = c(500),
    tonnes = c(1000),
    price = c(0.5)
  )

  brans_code <- 2111L
  cbs <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code,
    2020L, 2, 2511,
    2020L, 2, brans_code
  )

  result <- build_cbs_prices(
    cbs = cbs,
    trade_prices = trade_prices
  )

  brans <- result[result$item_cbs_code == brans_code, ]
  testthat::expect_true(nrow(brans) > 0)
})

testthat::test_that("build_cbs_prices handles Fibres category without duplication", {
  # Fibres has both Woody and Herbaceous in items_prod_full.
  # This caused a cartesian join before the fix.
  # Cotton lint (item_code_trade 767) maps to CBS code 2661
  trade_prices <- data.table::data.table(
    year = c(2020L, 2020L),
    item_trade = c("Cotton lint", "Cotton lint"),
    item_code_trade = c(767, 767),
    element = c("import", "export"),
    kdollars = c(1000, 800),
    tonnes = c(500, 400),
    price = c(2.0, 2.0)
  )

  cbs <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code,
    2020L, 2, 2661
  )

  # Should not error from cartesian join
  result <- build_cbs_prices(
    cbs = cbs,
    trade_prices = trade_prices
  )

  # Cotton lint (2661) should appear without duplicate rows per element
  cotton <- result |>
    dplyr::filter(item_cbs_code == 2661)
  cotton_per_elem <- cotton |>
    dplyr::count(year, element)
  testthat::expect_true(all(cotton_per_elem$n == 1))
})

testthat::test_that("build_cbs_prices multiple trade items aggregate to CBS", {
  # Trade codes 15 (Wheat) and 16 (Flour, wheat) both map to
  # CBS "Wheat and products" (2511)
  trade_prices <- data.table::data.table(
    year = c(2020L, 2020L),
    item_trade = c("Wheat", "Flour, wheat"),
    item_code_trade = c(15, 16),
    element = c("export", "export"),
    kdollars = c(500, 300),
    tonnes = c(1000, 600),
    price = c(0.5, 0.5)
  )

  cbs <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code,
    2020L, 2, 2511
  )

  result <- build_cbs_prices(
    cbs = cbs,
    trade_prices = trade_prices
  )

  wheat <- result |>
    dplyr::filter(item_cbs_code == 2511, element == "export")
  # Should have one row with aggregated price = (500+300)/(1000+600) = 0.5
  testthat::expect_equal(nrow(wheat), 1)
  testthat::expect_equal(wheat$price, 0.5)
})

testthat::test_that("build_cbs_prices proxy prefers original over estimated", {
  # Provide both Wheat (source for Brans proxy) AND direct Brans trade data
  trade_prices <- data.table::data.table(
    year = c(2020L, 2020L),
    item_trade = c("Wheat", "Bran, wheat"),
    item_code_trade = c(15, 17),
    element = c("export", "export"),
    kdollars = c(500, 100),
    tonnes = c(1000, 200),
    price = c(0.5, 0.5)
  )

  brans_code <- 2111L
  cbs <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code,
    2020L, 2, 2511,
    2020L, 2, brans_code
  )

  result <- build_cbs_prices(
    cbs = cbs,
    trade_prices = trade_prices
  )

  brans <- result |>
    dplyr::filter(item_cbs_code == brans_code, element == "export")

  # Bran, wheat maps to "Wheat and products" via cbs_trade_codes,
  # not directly to Brans. So proxy (from Wheat @ 0.2x) should be used.
  # The key point: no duplication from both original + estimated existing.
  testthat::expect_equal(nrow(brans), 1)
})

testthat::test_that("build_cbs_prices gap-fills prices across years", {
  trade_prices <- data.table::data.table(
    year = c(2018L, 2020L),
    item_trade = c("Wheat", "Wheat"),
    item_code_trade = c(15, 15),
    element = c("export", "export"),
    kdollars = c(500, 1000),
    tonnes = c(1000, 1000),
    price = c(0.5, 1.0)
  )

  cbs <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code,
    2018L, 2, 2511,
    2019L, 2, 2511,
    2020L, 2, 2511
  )

  result <- build_cbs_prices(
    cbs = cbs,
    trade_prices = trade_prices
  )

  wheat <- result |>
    dplyr::filter(item_cbs_code == 2511, element == "export") |>
    dplyr::arrange(year)

  testthat::expect_equal(nrow(wheat), 3)
  # 2019 should be interpolated: 0.75
  testthat::expect_equal(wheat$price, c(0.5, 0.75, 1.0))
})

# Integration tests (example mode) --------------------------------------------

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
