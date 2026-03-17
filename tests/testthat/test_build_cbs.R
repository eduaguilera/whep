# test_build_cbs.R — unit tests for R/build_cbs.R + R/read_raw_inputs.R helpers

# -- Fixtures ------------------------------------------------------------------

.make_cbs_afse <- function() {
  list(
    items_full = tibble::tribble(
      ~item_cbs, ~item_code_cbs, ~comm_group, ~group,
      ~default_destiny,
      "Wheat", 2511L, "Cereals", "Crop products", "Food",
      "Maize", 2514L, "Cereals", "Crop products", "Feed",
      "Rice", 2805L, "Cereals", "Crop products", "Food",
      "Flour", 2512L, "Flour", "Crop products", "Food"
    ),
    items_prod_full = tibble::tribble(
      ~item_prod, ~item_code_prod, ~item_cbs, ~item_code_cbs,
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
      ~year, ~ProcessedItem, ~item_cbs,
      ~Product_fraction, ~Value_fraction,
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
    ~year, ~area, ~area_code,
    ~item_cbs, ~item_code_cbs, ~element, ~value,
    2000L, "Spain", 203L, "Wheat", 2511L,
    "production", 5000,
    2000L, "Spain", 203L, "Wheat", 2511L,
    "import", 1000,
    2000L, "Spain", 203L, "Wheat", 2511L,
    "export", 500,
    2000L, "Spain", 203L, "Wheat", 2511L,
    "food", 3000,
    2000L, "Spain", 203L, "Wheat", 2511L,
    "feed", 1500,
    2000L, "Spain", 203L, "Wheat", 2511L,
    "domestic_supply", 5500,
    2000L, "Spain", 203L, "Wheat", 2511L,
    "processing", 500,
    2000L, "Spain", 203L, "Wheat", 2511L,
    "seed", 200,
    2000L, "Spain", 203L, "Wheat", 2511L,
    "other_uses", 300,
    2000L, "Spain", 203L, "Wheat", 2511L,
    "processing_primary", 0,
    2000L, "Spain", 203L, "Wheat", 2511L,
    "stock_variation", 0
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
    ~item_code_cbs, ~item_cbs, ~value,
    2804L, "Rice, paddy", 100,
    2511L, "Wheat", 200
  )

  result <- whep:::.fix_item_codes(df)
  expect_false(2804L %in% result$item_code_cbs)
  expect_true(2807L %in% result$item_code_cbs)
})

test_that(".fix_item_codes remaps groundnuts 2820 -> 2552", {
  df <- tibble::tribble(
    ~item_code_cbs, ~item_cbs, ~value,
    2820L, "Groundnuts (in Shell Eq)", 100
  )

  result <- whep:::.fix_item_codes(df)
  expect_equal(result$item_code_cbs, 2552L)
})


# -- .select_best_source -------------------------------------------------------

test_that(".select_best_source prioritises Primary source", {
  cbs_raw_all <- tibble::tribble(
    ~area, ~area_code, ~item_cbs, ~item_code_cbs,
    ~element, ~year, ~value, ~source, ~unit,
    "Spain", 203L, "Wheat", 2511L,
    "production", 2000L, 5000, "Primary", "tonnes",
    "Spain", 203L, "Wheat", 2511L,
    "production", 2000L, 4000, "FBS_New", "tonnes",
    "Spain", 203L, "Wheat", 2511L,
    "production", 2000L, 3000, "FBS_Old", "tonnes"
  )

  result <- whep:::.select_best_source(cbs_raw_all)
  expect_equal(
    result$value[result$element == "production"],
    5000
  )
})


# -- .test_cbs -----------------------------------------------------------------

test_that(".test_cbs adds balance check columns", {
  afse <- .make_cbs_afse()

  cbs <- .make_cbs_raw()

  result <- whep:::.test_cbs(cbs, afse)
  expect_true("balance" %in% names(result))
  expect_true("check" %in% names(result))
  expect_true("domestic_supply" %in% names(result))
})


# -- .untest_cbs ---------------------------------------------------------------

test_that(".untest_cbs returns long format without check columns", {
  afse <- .make_cbs_afse()
  cbs <- .make_cbs_raw()

  wide <- whep:::.test_cbs(cbs, afse)
  result <- whep:::.untest_cbs(wide)
  expect_true(all(c("element", "value") %in% names(result)))
  expect_false("check" %in% names(result))
  expect_false("balance" %in% names(result))
})


# -- .processed_raw ------------------------------------------------------------

test_that(".processed_raw creates value_proc column", {
  afse <- .make_cbs_afse()
  cbs <- .make_cbs_raw() |>
    dplyr::filter(element == "processing")

  cb_proc <- tibble::tribble(
    ~year, ~ProcessedItem, ~item_cbs,
    ~Product_fraction, ~Value_fraction,
    2000L, "Flour", "Wheat", 0.8, 1.0
  )

  result <- whep:::.processed_raw(cbs, cb_proc)
  expect_true("value_proc" %in% names(result))
  expect_true("processed_item" %in% names(result))
})


# -- .compute_splice_ratios ---------------------------------------------------

.make_overlap_data <- function() {
  tibble::tribble(
    ~area, ~area_code, ~item_cbs, ~item_code_cbs,
    ~element, ~year, ~value, ~source, ~unit,
    "Spain", 203L, "Wheat", 2511L,
    "production", 2010L, 100, "FBS_Old", "tonnes",
    "Spain", 203L, "Wheat", 2511L,
    "production", 2010L, 120, "FBS_New", "tonnes",
    "Spain", 203L, "Wheat", 2511L,
    "production", 2011L, 200, "FBS_Old", "tonnes",
    "Spain", 203L, "Wheat", 2511L,
    "production", 2011L, 240, "FBS_New", "tonnes",
    "Spain", 203L, "Wheat", 2511L,
    "production", 2005L, 80, "FBS_Old", "tonnes",
    "Spain", 203L, "Wheat", 2511L,
    "production", 2015L, 300, "FBS_New", "tonnes"
  )
}

test_that(".compute_splice_ratios returns correct median ratio", {
  ratios <- .make_overlap_data() |>
    whep:::.compute_splice_ratios()

  expect_equal(nrow(ratios), 1L)
  expect_equal(ratios$splice_ratio, 1.2)
  expect_equal(ratios$n_overlap, 2L)
})

test_that(".compute_splice_ratios returns empty tibble when no overlap", {
  no_overlap <- tibble::tribble(
      ~area, ~area_code, ~item_cbs, ~item_code_cbs,
      ~element, ~year, ~value, ~source, ~unit,
      "Spain", 203L, "Wheat", 2511L,
      "production", 2005L, 100, "FBS_Old", "tonnes",
      "Spain", 203L, "Wheat", 2511L,
      "production", 2015L, 300, "FBS_New", "tonnes"
    )

  ratios <- whep:::.compute_splice_ratios(no_overlap)
  expect_equal(nrow(ratios), 0L)
})


# -- .apply_splice -------------------------------------------------------------

test_that(".apply_splice rescales FBS_Old and leaves others", {
  data <- .make_overlap_data()
  ratios <- whep:::.compute_splice_ratios(data)
  result <- whep:::.apply_splice(data, ratios)

  old_rows <- result |>
    dplyr::filter(source == "FBS_Old")
  new_rows <- result |>
    dplyr::filter(source == "FBS_New")

  # FBS_Old values should be multiplied by 1.2
  expect_equal(
    old_rows |>
      dplyr::filter(year == 2005L) |>
      dplyr::pull(value),
    96
  )
  expect_equal(
    old_rows |>
      dplyr::filter(year == 2010L) |>
      dplyr::pull(value),
    120
  )

  # FBS_New values should be unchanged
  expect_equal(
    new_rows |>
      dplyr::filter(year == 2015L) |>
      dplyr::pull(value),
    300
  )
})

test_that(".apply_splice uses fallback ratio 1 for no overlap", {
  data <- tibble::tribble(
    ~area, ~area_code, ~item_cbs, ~item_code_cbs,
    ~element, ~year, ~value, ~source, ~unit,
    "Spain", 203L, "Rice", 2805L,
    "production", 2005L, 50, "FBS_Old", "tonnes",
    "Spain", 203L, "Rice", 2805L,
    "production", 2015L, 90, "FBS_New", "tonnes"
  )

  ratios <- whep:::.compute_splice_ratios(data)
  result <- whep:::.apply_splice(data, ratios)

  # With no overlap, FBS_Old should be unchanged (ratio = 1)
  expect_equal(
    result |>
      dplyr::filter(source == "FBS_Old") |>
      dplyr::pull(value),
    50
  )
})

test_that("splice + .select_best_source integration works", {
  data <- tibble::tribble(
    ~area, ~area_code, ~item_cbs, ~item_code_cbs,
    ~element, ~year, ~value, ~source, ~unit,
    "Spain", 203L, "Wheat", 2511L,
    "production", 2010L, 100, "FBS_Old", "tonnes",
    "Spain", 203L, "Wheat", 2511L,
    "production", 2010L, 120, "FBS_New", "tonnes",
    "Spain", 203L, "Wheat", 2511L,
    "production", 2010L, 130, "Primary", "tonnes",
    "Spain", 203L, "Wheat", 2511L,
    "production", 2015L, 300, "FBS_New", "tonnes",
    "Spain", 203L, "Wheat", 2511L,
    "production", 2015L, 300, "Primary", "tonnes"
  )

  ratios <- whep:::.compute_splice_ratios(data)
  spliced <- whep:::.apply_splice(data, ratios)
  result <- whep:::.select_best_source(spliced)

  expect_true(all(c("value", "element") %in% names(result)))
  expect_true("source" %in% names(result))
  expect_equal(nrow(result), 2L)
})
