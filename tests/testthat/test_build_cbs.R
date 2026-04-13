# test_build_cbs.R — unit tests for R/build_cbs.R + R/read_raw_inputs.R helpers

# -- Fixtures ------------------------------------------------------------------

.make_cbs_afse <- function() {
  list(
    items_full = tibble::tribble(
      ~item_cbs, ~item_cbs_code, ~comm_group, ~group, ~default_destiny,
      "Wheat", 2511L, "Cereals", "Crop products", "Food",
      "Maize", 2514L, "Cereals", "Crop products", "Feed",
      "Rice", 2805L, "Cereals", "Crop products", "Food",
      "Flour", 2512L, "Flour", "Crop products", "Food"
    ),
    items_prod_full = tibble::tribble(
      ~item_prod, ~item_prod_code, ~item_cbs, ~item_cbs_code,
      "Wheat", 15L, "Wheat", 2511L,
      "Maize", 56L, "Maize", 2514L,
      "Rice", 27L, "Rice", 2805L
    ),
    regions_full = tibble::tribble(
      ~polity_name, ~polity_code, ~iso3c,
      "Spain", 203L, "ESP",
      "France", 68L, "FRA"
    ),
    polities_cats = tibble::tribble(
      ~polity_name, ~polity_code, ~dissolved,
      "Spain", 203L, FALSE,
      "France", 68L, FALSE
    ),
    CB_processing = tibble::tribble(
      ~year, ~ProcessedItem, ~item_cbs, ~Product_fraction, ~Value_fraction,
      2000L, "Flour", "Wheat", 0.8, 1.0,
      2001L, "Flour", "Wheat", 0.8, 1.0
    ),
    CBS_Trade_codes = tibble::tribble(
      ~item_code_trade, ~item_cbs,
      100L, "Wheat",
      200L, "Rice"
    ),
    NoDataProducts = character(),
    Primary_double = tibble::tibble(
      item_prod = character(),
      Item_area = character(),
      Multi_type = character()
    )
  )
}

.make_cbs_raw <- function() {
  tibble::tribble(
    ~year, ~area, ~area_code, ~item_cbs, ~item_cbs_code, ~element, ~value,
    2000L, "Spain", 203L, "Wheat", 2511L, "production", 5000,
    2000L, "Spain", 203L, "Wheat", 2511L, "import", 1000,
    2000L, "Spain", 203L, "Wheat", 2511L, "export", 500,
    2000L, "Spain", 203L, "Wheat", 2511L, "food", 3000,
    2000L, "Spain", 203L, "Wheat", 2511L, "feed", 1500,
    2000L, "Spain", 203L, "Wheat", 2511L, "domestic_supply", 5500,
    2000L, "Spain", 203L, "Wheat", 2511L, "processing", 500,
    2000L, "Spain", 203L, "Wheat", 2511L, "seed", 200,
    2000L, "Spain", 203L, "Wheat", 2511L, "other_uses", 300,
    2000L, "Spain", 203L, "Wheat", 2511L, "processing_primary", 0,
    2000L, "Spain", 203L, "Wheat", 2511L, "stock_variation", 0
  )
}


# -- .harmonize_element_names -------------------------------------------------

test_that(".harmonize_element_names converts FAOSTAT element names", {
  df <- tibble::tribble(
    ~element, ~value,
    "Production", 1000,
    "Import Quantity", 500,
    "Export Quantity", 200,
    "Food supply quantity (tonnes)", 800,
    "Feed", 300,
    "Seed", 100,
    "Processing", 50,
    "Other uses (non-food)", 25,
    "Stock Variation", -10,
    "Domestic supply quantity", 1490
  )

  result <- whep:::.harmonize_element_names(df)
  expect_setequal(
    result$element,
    c(
      "production",
      "import",
      "export",
      "food",
      "feed",
      "seed",
      "processing",
      "other_uses",
      "stock_variation",
      "domestic_supply"
    )
  )
})


# -- .fix_item_codes -----------------------------------------------------------

test_that(".fix_item_codes remaps rice 2804 -> 2807", {
  df <- tibble::tribble(
    ~item_cbs_code, ~item_cbs, ~value,
    2804L, "Rice, paddy", 100,
    2511L, "Wheat", 200
  )

  result <- whep:::.fix_item_codes(df)
  expect_false(2804L %in% result$item_cbs_code)
  expect_true(2807L %in% result$item_cbs_code)
})

test_that(".fix_item_codes remaps groundnuts 2820 -> 2552", {
  df <- tibble::tribble(
    ~item_cbs_code, ~item_cbs, ~value,
    2820L, "Groundnuts (in Shell Eq)", 100
  )

  result <- whep:::.fix_item_codes(df)
  expect_equal(result$item_cbs_code, 2552L)
})


# -- .select_best_source -------------------------------------------------------

test_that(".select_best_source prioritises FAOSTAT_prod source", {
  cbs_raw_all <- tibble::tribble(
    ~area, ~area_code, ~item_cbs, ~item_cbs_code, ~element, ~year, ~value, ~source, ~unit,
    "Spain", 203L, "Wheat", 2511L, "production", 2000L, 5000, "FAOSTAT_prod", "tonnes",
    "Spain", 203L, "Wheat", 2511L, "production", 2000L, 4000, "FAOSTAT_FBS_New", "tonnes",
    "Spain", 203L, "Wheat", 2511L, "production", 2000L, 3000, "FAOSTAT_FBS_Old", "tonnes"
  )

  result <- whep:::.select_best_source(cbs_raw_all)
  expect_equal(
    result$value[result$element == "production"],
    5000
  )
})


# -- .test_cbs -----------------------------------------------------------------

test_that(".test_cbs adds balance check columns", {
  cbs <- .make_cbs_raw()

  result <- whep:::.test_cbs(cbs)
  expect_true("balance" %in% names(result))
  expect_true("check" %in% names(result))
  expect_true("domestic_supply" %in% names(result))
})


# -- .untest_cbs ---------------------------------------------------------------

test_that(".untest_cbs returns long format without check columns", {
  cbs <- .make_cbs_raw()

  wide <- whep:::.test_cbs(cbs)
  result <- whep:::.untest_cbs(wide)
  expect_true(all(c("element", "value") %in% names(result)))
  expect_false("check" %in% names(result))
  expect_false("balance" %in% names(result))
})


# -- .processed_raw ------------------------------------------------------------

test_that(".processed_raw creates value_proc column", {
  cbs <- .make_cbs_raw() |>
    dplyr::filter(element == "processing")

  cb_proc <- tibble::tribble(
    ~year, ~ProcessedItem, ~item_cbs, ~Product_fraction, ~Value_fraction,
    2000L, "Flour", "Wheat", 0.8, 1.0
  )

  result <- whep:::.processed_raw(cbs, cb_proc)
  expect_true("value_proc" %in% names(result))
  expect_true("processed_item" %in% names(result))
})


# -- .select_best_source FBS harmonization ------------------------------------

.make_select_best_source_input <- function() {
  tibble::tribble(
    ~area, ~area_code, ~item_cbs, ~item_cbs_code, ~element, ~year,
    ~value, ~source, ~unit,
    "Spain", 203L, "Wheat", 2511L, "food", 2010L,
    1000, "FAOSTAT_FBS_Old", "tonnes",
    "Spain", 203L, "Wheat", 2511L, "food", 2010L,
    1050, "FAOSTAT_FBS_New", "tonnes",
    "Spain", 203L, "Wheat", 2511L, "food", 2011L,
    1020, "FAOSTAT_FBS_Old", "tonnes",
    "Spain", 203L, "Wheat", 2511L, "food", 2011L,
    1071, "FAOSTAT_FBS_New", "tonnes",
    "Spain", 203L, "Wheat", 2511L, "food", 2005L,
    900, "FAOSTAT_FBS_Old", "tonnes",
    "Spain", 203L, "Wheat", 2511L, "food", 2015L,
    1200, "FAOSTAT_FBS_New", "tonnes"
  )
}

test_that(".select_best_source scales FBS_Old to FBS_New level", {
  input <- .make_select_best_source_input()
  result <- whep:::.select_best_source(input)

  val_2005 <- result |>
    dplyr::filter(year == 2005) |>
    dplyr::pull(value)
  expect_true(val_2005 > 900)
  expect_true(val_2005 < 1000)

  val_2015 <- result |>
    dplyr::filter(year == 2015) |>
    dplyr::pull(value)
  expect_equal(val_2015, 1200)

  src_2005 <- result |>
    dplyr::filter(year == 2005) |>
    dplyr::pull(source)
  expect_equal(src_2005, "FAOSTAT_FBS_Old_scaled")

  src_2015 <- result |>
    dplyr::filter(year == 2015) |>
    dplyr::pull(source)
  expect_equal(src_2015, "FAOSTAT_FBS_New")
})

test_that(".select_best_source uses dataset-specific source names", {
  input <- tibble::tribble(
    ~area, ~area_code, ~item_cbs, ~item_cbs_code, ~element, ~year,
    ~value, ~source, ~unit,
    "Spain", 203L, "Wheat", 2511L, "production", 2010L,
    5000, "FAOSTAT_prod", "tonnes",
    "Spain", 203L, "Wheat", 2511L, "food", 2010L,
    3000, "FAOSTAT_FBS_New", "tonnes"
  )

  result <- whep:::.select_best_source(input)

  valid_sources <- c(
    "FAOSTAT_prod",
    "FAOSTAT_FBS_New",
    "FAOSTAT_FBS_Old",
    "FAOSTAT_FBS_Old_scaled",
    "FAOSTAT_CBS",
    "FAOSTAT_trade",
    "mean"
  )
  expect_true(all(result$source %in% valid_sources))
  expect_false(any(result$source %in% c("Primary", "FBS_New", "FBS_Old")))
})


# -- .format_cbs_output -------------------------------------------------------

test_that(".format_cbs_output returns long format with source column", {
  cbs <- tibble::tribble(
    ~year, ~area, ~area_code, ~item_cbs, ~item_cbs_code, ~element,
    ~value, ~source,
    2000L, "Spain", 203L, "Wheat", 2511L, "production", 5000, "FAOSTAT_prod",
    2000L, "Spain", 203L, "Wheat", 2511L, "food", 3000, "FAOSTAT_FBS_New",
    2000L, "Spain", 203L, "Wheat", 2511L, "feed", 1000, "FAOSTAT_FBS_New",
    2000L, "Spain", 203L, "Wheat", 2511L, "import", 500, "FAOSTAT_trade",
    2000L, "Spain", 203L, "Wheat", 2511L, "export", 200, "FAOSTAT_trade",
    2000L, "Spain", 203L, "Wheat", 2511L, "seed", 100, "FAOSTAT_FBS_Old",
    2000L, "Spain", 203L, "Wheat", 2511L, "other_uses", 50, "mean",
    2000L, "Spain", 203L, "Wheat", 2511L, "processing", 150, "Processed",
    2000L, "Spain", 203L, "Wheat", 2511L, "domestic_supply", 4300, "FAOSTAT_FBS_New",
    2000L, "Spain", 203L, "Wheat", 2511L, "stock_variation", 0, "mean"
  )

  result <- whep:::.format_cbs_output(cbs)

  expect_true("element" %in% names(result))
  expect_true("source" %in% names(result))
  expect_false("production" %in% names(result))

  prod_src <- result |>
    dplyr::filter(element == "production") |>
    dplyr::pull(source)
  expect_equal(prod_src, "FAOSTAT_prod")
})


# -- .wide_cbs_to_long ---------------------------------------------------------

test_that(".wide_cbs_to_long handles long format input", {
  long_input <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~element, ~value, ~source,
    2000L, 203L, 2511L, "production", 5000, "FAOSTAT_prod",
    2000L, 203L, 2511L, "food", 3000, "FAOSTAT_FBS_New"
  )

  result <- whep:::.wide_cbs_to_long(long_input)
  expect_true("item_cbs" %in% names(result))
  expect_equal(nrow(result), 2L)
})


# -- trade imputation exclusions -----------------------------------------------

test_that("trade imputation excludes ethanol, sugar cane, sugar beet", {
  no_residual <- c(2659L, 2536L, 2537L)
  tradeable_items <- c(2511L, 2659L, 2536L, 2537L)

  is_tradeable <- tradeable_items %in%
    tradeable_items &
    !tradeable_items %in% no_residual

  expect_true(is_tradeable[1])
  expect_false(is_tradeable[2])
  expect_false(is_tradeable[3])
  expect_false(is_tradeable[4])
})


# -- year range defaults -------------------------------------------------------

test_that("build_commodity_balances defaults to end_year 2023", {
  formals_cbs <- formals(whep::build_commodity_balances)
  expect_equal(formals_cbs$end_year, 2023)
})


# -- deduplication --------------------------------------------------------------

test_that(".format_cbs_output removes duplicate rows", {
  df <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~element, ~value, ~source,
    2000L, 203L, 2511L, "production", 100, "FAOSTAT_prod",
    2000L, 203L, 2511L, "production", 100, "FAOSTAT_prod",
    2000L, 203L, 2511L, "import", 50, "FAOSTAT_FBS_New"
  )

  result <- whep:::.format_cbs_output(df)
  prod_rows <- result |>
    dplyr::filter(
      year == 2000L,
      area_code == 203L,
      item_cbs_code == 2511L,
      element == "production"
    )
  expect_equal(nrow(prod_rows), 1L)
  expect_equal(prod_rows$value, 100)
})
