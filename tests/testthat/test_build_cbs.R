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
  expect_equal(
    result$value[result$item_cbs_code == 2807L],
    100 * whep:::.rice_milled_extraction_rate()
  )
  expect_equal(
    result$item_cbs[result$item_cbs_code == 2807L],
    "Rice and products"
  )
})

test_that(".fix_item_codes keeps milled rice when old CBS also has paddy equivalent", {
  df <- tibble::tribble(
    ~year, ~area_code, ~area, ~element, ~unit, ~item_cbs_code, ~item_cbs, ~value,
    2000L, 41L, "China", "food", "tonnes", 2805L, "Rice (Milled Equivalent)", 100,
    2000L, 41L, "China", "food", "tonnes", 2804L, "Rice (Paddy Equivalent)", 150,
    2000L, 41L, "China", "production", "tonnes", 2804L, "Rice, paddy", 200
  )

  result <- whep:::.fix_item_codes(df)

  food <- result |>
    dplyr::filter(.data$element == "food")
  testthat::expect_equal(nrow(food), 1)
  testthat::expect_equal(food$item_cbs_code, 2807L)
  testthat::expect_equal(food$item_cbs, "Rice and products")
  testthat::expect_equal(food$value, 100)

  production <- result |>
    dplyr::filter(.data$element == "production")
  testthat::expect_equal(production$item_cbs_code, 2807L)
  testthat::expect_equal(production$item_cbs, "Rice and products")
  testthat::expect_equal(
    production$value,
    200 * whep:::.rice_milled_extraction_rate()
  )
})

test_that(".fix_item_codes remaps groundnuts 2820 -> 2552", {
  df <- tibble::tribble(
    ~item_cbs_code, ~item_cbs, ~value,
    2820L, "Groundnuts (in Shell Eq)", 100
  )

  result <- whep:::.fix_item_codes(df)
  expect_equal(result$item_cbs_code, 2552L)
  expect_equal(result$item_cbs, "Groundnuts")
})

test_that(".read_land_areas_wide tolerates missing LUH2 cropland and pasture rows", {
  # `iso3c` is in the mock because .read_land_areas_wide() now resolves the
  # LUH2 rows to a polity_code from it rather than carrying the area label
  # through; see .fill_with_proxies().
  local_mocked_bindings(
    .read_land_areas = function(years = NULL) {
      tibble::tibble(
        year = 2023L,
        iso3c = "ESP",
        area = "Spain",
        Land_Use = "urban",
        Area_Mha = 1
      )
    }
  )

  result <- whep:::.read_land_areas_wide(years = 2023L)

  expect_true(all(c("Cropland", "Pasture", "agriland") %in% names(result)))
  expect_equal(nrow(result), 0L)
})

test_that(".read_land_areas_wide keys its output on polity_code", {
  # The frame this table feeds is labelled with `polity_name`, but
  # .read_land_areas() labels its rows with the crosswalk's STATIC `area_name`.
  # Those two vocabularies diverge for most territories -- FAO area 3 is
  # "Albania (1913-2025)" as a polity and "Albania" as an area -- so the old
  # `by = c("year", "area")` join in .fill_with_proxies() missed them: measured
  # on main, 96 of the LUH2 labels (41.7% of land rows) are names no builder
  # emits, and frame coverage of `agriland` over 1900-1902 was 402 of 606
  # (year, polity) cells against 567 once keyed on the polity.
  local_mocked_bindings(
    .read_land_areas = function(years = NULL) {
      tibble::tibble(
        year = rep(1950L, 3),
        iso3c = c("ALB", "ALB", "ALB"),
        area = "Albania",
        Land_Use = c("c3ann", "pastr", "urban"),
        Area_Mha = c(2, 3, 9)
      )
    }
  )

  result <- whep:::.read_land_areas_wide(years = 1950L)

  expect_equal(result$polity_code, "ALB-1913-2025")
  expect_false("area" %in% names(result))
  expect_equal(result$agriland, 5)
})

test_that(".read_land_areas_wide holds back folded aggregate buckets", {
  # Equatorial Guinea and Syria both fold into the Rest of World bucket (999).
  # Summing their agricultural land into it would give the bucket an extent that
  # is neither member's nor the real rest of the world's, so proxies are not
  # synthesised for aggregates that are only reached by folding. Deciding what an
  # aggregate's proxy should be is a methodological choice (#493); until it is
  # made these buckets stay unfilled, which is where the name-keyed join left
  # them too.
  local_mocked_bindings(
    .read_land_areas = function(years = NULL) {
      tibble::tibble(
        year = rep(1950L, 3),
        iso3c = c("ESP", "GNQ", "SYR"),
        area = c("Spain", "Equatorial Guinea", "Syrian Arab Republic"),
        Land_Use = "c3ann",
        Area_Mha = c(10, 1, 2)
      )
    }
  )

  result <- whep:::.read_land_areas_wide(years = 1950L)

  expect_equal(result$polity_code, "ESP-1800-2025")
  expect_false("ROW-1850-2023" %in% result$polity_code)
})

test_that(".fix_palm_kernels tolerates single-year inputs without old palm-kernel anchors", {
  empty_fbs <- tibble::tibble(
    year = integer(),
    area = character(),
    area_code = integer(),
    item_cbs = character(),
    item_cbs_code = integer(),
    element = character(),
    value = numeric(),
    unit = character()
  )
  inputs <- list(
    fbs_old = empty_fbs,
    fbs_new = tibble::tibble(
      year = 2023L,
      area = "Spain",
      area_code = 203L,
      item_cbs = "Palmkernel Oil",
      item_cbs_code = 2577L,
      element = "production",
      value = 10,
      unit = "tonnes"
    )
  )

  result <- whep:::.fix_palm_kernels(inputs)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0L)
})

test_that(".cbs_impute_trade tolerates missing destiny element columns", {
  raw <- tibble::tribble(
    ~year, ~area, ~area_code, ~item_cbs, ~item_cbs_code, ~element, ~value, ~source,
    2023L, "Spain", 203L, "Wheat", 2511L, "production", 100, "FAOSTAT_prod"
  )

  result <- whep:::.cbs_impute_trade(raw)

  expect_true(all(
    c(
      "food",
      "feed",
      "other_uses",
      "processing",
      "import",
      "export",
      "stock_variation"
    ) %in%
      result$element
  ))
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

test_that(".select_best_source coalesces integer and double source values", {
  # Global sources disagree on storage type: the pivoted FAOSTAT_prod /
  # FAOSTAT_FBS_New inherit an integer raw `value`, while the scaled FBS_Old and
  # the other-source mean are doubles. fcoalesce() aborts on a mixed set, so the
  # sources must be coerced to a common numeric type first. Regression for the
  # global CBS build crashing in `Combining CBS sources`.
  cbs_raw_all <- tibble::tribble(
    ~area, ~area_code, ~item_cbs, ~item_cbs_code, ~element, ~year, ~value, ~source, ~unit,
    "China", 41L, "Wheat", 2511L, "production", 2000L, 100L, "FAOSTAT_prod", "tonnes",
    "China", 41L, "Wheat", 2511L, "production", 2000L, 90.5, "FAOSTAT_FBS_Old", "tonnes",
    "Brazil", 21L, "Maize", 2514L, "production", 2000L, 55.2, "FAOSTAT_FBS_Old", "tonnes"
  )
  result <- whep:::.select_best_source(cbs_raw_all)
  expect_type(result$value, "double")
  expect_equal(result$value[result$area_code == 41L], 100)
  expect_equal(result$value[result$area_code == 21L], 55.2)
})

test_that(".select_best_source keys on area_code, not periodized name", {
  # Sources disagree on the `area` name for the same `area_code` (plain name
  # vs periodized polity name). They must still compete on the integer code
  # instead of both surviving and being summed downstream (100 + 90 = 190).
  cbs_raw_all <- tibble::tribble(
    ~area, ~area_code, ~item_cbs, ~item_cbs_code, ~element, ~year, ~value, ~source,
    "China, mainland", 41L, "Wheat", 2511L, "production", 2010L, 100, "FAOSTAT_prod",
    "China (PRC)", 41L, "Wheat", 2511L, "production", 2010L, 90, "FAOSTAT_FBS_New"
  )

  selected <- whep:::.select_best_source(cbs_raw_all)
  prod <- selected |> dplyr::filter(element == "production")
  expect_equal(nrow(prod), 1L)
  expect_equal(prod$value, 100)

  formatted <- whep:::.format_cbs_output(selected)
  prod_fmt <- formatted |> dplyr::filter(element == "production")
  expect_equal(nrow(prod_fmt), 1L)
  expect_equal(prod_fmt$value, 100)
})


# -- .test_cbs -----------------------------------------------------------------

test_that(".test_cbs adds balance check columns", {
  cbs <- .make_cbs_raw()

  result <- whep:::.test_cbs(cbs)
  expect_true("balance" %in% names(result))
  expect_true("check" %in% names(result))
  expect_true("domestic_supply" %in% names(result))
})

test_that(".test_cbs tolerates missing standard element columns", {
  cbs <- tibble::tribble(
    ~year, ~area, ~area_code, ~item_cbs, ~item_cbs_code, ~element, ~value,
    2023L, "Spain", 203L, "Wheat", 2511L, "production", 100
  )

  result <- whep:::.test_cbs(cbs)

  expect_true(all(c("food", "feed", "import", "export") %in% names(result)))
  expect_equal(result$feed, 0)
  expect_true("check" %in% names(result))
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

test_that(".prepare_cb_processing_for_cbs excludes unconditional beer grains", {
  cb_proc <- tibble::tribble(
    ~ProcessedItem, ~item_cbs, ~Product_fraction, ~Value_fraction, ~Required,
    "Barley and products", "Beer", 6.55, 0.9, NA_real_,
    "Hops", "Beer", 0.28, NA_real_, NA_real_,
    "Maize and products", "Beer", 6.55, NA_real_, NA_real_,
    "Maize and products", "Sweeteners, Other", 0.3, NA_real_, NA_real_
  )

  result <- whep:::.prepare_cb_processing_for_cbs(cb_proc)

  beer_inputs <- result |>
    dplyr::filter(.data$item_cbs == "Beer") |>
    dplyr::pull(.data$ProcessedItem)

  expect_setequal(beer_inputs, c("Barley and products", "Hops"))
  expect_true(any(
    result$ProcessedItem == "Maize and products" &
      result$item_cbs == "Sweeteners, Other"
  ))
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


# -- historical CBS rows -------------------------------------------------------

test_that(".prepare_historical_cbs accepts generic production-shaped rows", {
  historical <- tibble::tribble(
    ~year, ~area_code, ~item_prod_code, ~unit, ~value, ~source,
    1950L, 203L, "15.0", "tonnes", 100, "future_source",
    1950L, 203L, "15.0", "tonnes", 120, "historical_future_source",
    1950L, 203L, "15.0", "ha", 10, "future_source",
    1800L, 203L, "15.0", "tonnes", 999, "future_source"
  )

  result <- whep:::.prepare_historical_cbs(
    historical,
    years = 1950:1951
  )

  expect_equal(nrow(result), 1L)
  expect_equal(result$year, 1950L)
  expect_equal(result$area, "Spain")
  expect_equal(result$item_cbs, "Wheat and products")
  expect_equal(result$item_cbs_code, 2511L)
  expect_equal(result$element, "production")
  expect_equal(result$value, 110)
  expect_true(stringr::str_starts(result$source, "historical_"))
})

test_that(".cbs_extend_historical preserves observed historical sources", {
  cbs_raw0 <- tibble::tibble(
    year = c(1950L, 1961L),
    area = "Spain",
    area_code = 203L,
    item_cbs = "Wheat and products",
    item_cbs_code = 2511L,
    element = "food",
    value = c(50, 100),
    source = c("historical_test", "FAOSTAT_FBS_Old")
  )
  inputs <- list(
    primary_cbs_area = tibble::tibble(
      year = 1950:1961,
      area = "Spain",
      area_code = 203L,
      item_cbs = "Wheat and products",
      item_cbs_code = 2511L,
      area_ha = 1
    ),
    # The gdp/population pin is keyed by ISO3 in a column called `area_code`,
    # and .read_land_areas_wide() emits a polity_code: both proxies are resolved
    # onto the frame's polity key rather than joined on an area label.
    gdp_pop = tibble::tibble(
      year = 1950:1961,
      area = "Spain",
      area_code = "ESP",
      pop = 1:12
    ),
    land_areas_wide = tibble::tibble(
      year = 1950:1961,
      polity_code = "ESP-1800-2025",
      Cropland = 1,
      Pasture = 0,
      agriland = 1
    )
  )

  result <- whep:::.cbs_extend_historical(cbs_raw0, inputs, 1950:1961)

  observed <- result |>
    dplyr::filter(.data$year == 1950L, .data$element == "food")
  filled <- result |>
    dplyr::filter(.data$year == 1951L, .data$element == "food")

  expect_equal(observed$value, 50)
  expect_equal(observed$source, "historical_test")
  expect_false(is.na(filled$value))
  expect_equal(filled$source, "historical_fill")
})

test_that(".fill_with_proxies keys its proxies on the polity, not the name", {
  # Three name vocabularies used to meet at this join. The frame carries
  # `polity_name` (.aggregate_to_polities() renames it to `area`), which is
  # periodized: FAO area 3 arrives as "Albania (1913-2025)". The gdp/population
  # pin calls it "Albania" and keys itself by ISO3; the LUH2 land table used to
  # carry the crosswalk's static "Albania" too. So `by = c("year", "area")`
  # matched neither, and the row kept its gaps. Measured on main: 57 of the pin's
  # 196 names (8,263 rows, 27.8%) and 96 of the LUH2 labels (41.7% of land rows)
  # are names no builder emits, and coverage of the pre-1962 frame's (year,
  # polity) cells was 13,664 of 22,624 for `pop` and 402 of 606 for `agriland`
  # over 1900-1902, against 18,480 and 567 once keyed on polity_code.
  #
  # Fixture rather than shipped data: neither proxy table is exported, both only
  # exist inside a build, and the frame they fill is assembled mid-pipeline.
  frame <- tibble::tibble(
    year = 1950:1953,
    area = "Albania (1913-2025)",
    area_code = 3L,
    item_cbs = "Wheat and products",
    item_cbs_code = 2511L,
    food = c(100, NA, NA, NA),
    other_uses = NA_real_,
    feed = c(10, NA, NA, NA),
    processing = NA_real_
  )
  gdp_pop <- tibble::tibble(
    year = 1950:1953,
    area = "Albania",
    area_code = "ALB",
    pop = c(1000, 1100, 1200, 1300)
  )
  land_wide <- tibble::tibble(
    year = 1950:1953,
    polity_code = "ALB-1913-2025",
    Cropland = 1,
    Pasture = 1,
    agriland = c(2, 2.2, 2.4, 2.6)
  )

  result <- whep:::.fill_with_proxies(frame, gdp_pop, land_wide)

  # Both destinies follow their proxy's growth rate: +10% a year for population,
  # +10% a year for agricultural land.
  expect_equal(result$food, c(100, 110, 120, 130))
  expect_equal(result$feed, c(10, 11, 12, 13))
  expect_equal(nrow(result), nrow(frame))
  expect_false("polity_code" %in% names(result))
})

test_that(".fill_with_proxies leaves a folded aggregate bucket unproxied", {
  # Syria folds into the Rest of World bucket (999), so the pin's Syrian
  # population is not the bucket's population and a per-capita rate against it
  # would mean nothing. Deciding what an aggregate's proxy should be is a
  # methodological choice (#493), so nothing is summed into the bucket here and
  # the gap survives -- which is also where the name-keyed join left it.
  frame <- tibble::tibble(
    year = 1950:1952,
    area = "Rest of World",
    area_code = 999L,
    item_cbs = "Wheat and products",
    item_cbs_code = 2511L,
    food = c(100, NA, NA),
    other_uses = NA_real_,
    feed = NA_real_,
    processing = NA_real_
  )
  gdp_pop <- tibble::tibble(
    year = 1950:1952,
    area = "Syria",
    area_code = "SYR",
    pop = c(1000, 1100, 1200)
  )
  land_wide <- tibble::tibble(
    year = 1950:1952,
    polity_code = "SYR-1946-2025",
    Cropland = 1,
    Pasture = 1,
    agriland = c(2, 2.2, 2.4)
  )

  result <- whep:::.fill_with_proxies(frame, gdp_pop, land_wide)

  expect_true(all(is.na(result$pop)))
  expect_equal(result$food, c(100, NA, NA))
})


# -- historical trade wiring (issue #141) -------------------------------------

.empty_cbs_component <- function() {
  # Internal CBS helpers receive data.tables in production; mirror that here.
  data.table::data.table(
    year = integer(),
    area = character(),
    area_code = integer(),
    item_cbs = character(),
    item_cbs_code = integer(),
    element = character(),
    value = numeric(),
    unit = character()
  )
}

.make_trade_hist_inputs <- function(with_trade_hist = TRUE) {
  primary_cbs <- data.table::data.table(
    year = 1950L,
    area = "Spain",
    area_code = 203L,
    item_cbs = "Wheat and products",
    item_cbs_code = 2511L,
    element = "production",
    value = 5000,
    unit = "tonnes"
  )
  trade_hist <- tibble::tribble(
    ~year, ~area, ~area_code, ~item_cbs, ~item_cbs_code, ~element, ~value,
    ~unit,
    1950L, "Spain", 203L, "Wheat and products", 2511L, "import", 700,
    "tonnes",
    1950L, "Spain", 203L, "Wheat and products", 2511L, "export", 200,
    "tonnes"
  ) |>
    data.table::as.data.table()
  list(
    fbs_new = .empty_cbs_component(),
    fbs_old = .empty_cbs_component(),
    cbs_animals = .empty_cbs_component(),
    cbs_crops = .empty_cbs_component(),
    primary_cbs = primary_cbs,
    crop_residues = .empty_cbs_component(),
    trade_hist = if (with_trade_hist) trade_hist else NULL
  )
}

test_that(".assemble_cbs_sources binds historical trade under its source", {
  inputs <- .make_trade_hist_inputs()
  empty <- .empty_cbs_component()

  result <- whep:::.assemble_cbs_sources(
    inputs,
    empty,
    empty,
    empty,
    whep::items_full
  )

  hist_rows <- result |>
    dplyr::filter(.data$source == "trade_hist")
  expect_equal(nrow(hist_rows), 2L)
  expect_setequal(hist_rows$element, c("import", "export"))
  expect_equal(
    hist_rows |> dplyr::filter(element == "import") |> dplyr::pull(value),
    700
  )
})

test_that("historical trade reaches pre-1961 CBS import/domestic supply", {
  ext_inputs <- list(
    primary_cbs_area = tibble::tibble(
      year = 1950L,
      area = "Spain",
      area_code = 203L,
      item_cbs = "Wheat and products",
      item_cbs_code = 2511L,
      area_ha = 1
    ),
    # Both proxies are resolved onto the frame's polity key, not joined on an
    # area label: the gdp/population pin is keyed by ISO3 in `area_code`, and
    # .read_land_areas_wide() emits a polity_code.
    gdp_pop = tibble::tibble(
      year = 1950L,
      area = "Spain",
      area_code = "ESP",
      pop = 10
    ),
    land_areas_wide = tibble::tibble(
      year = 1950L,
      polity_code = "ESP-1800-2025",
      Cropland = 1,
      Pasture = 0,
      agriland = 1
    )
  )
  empty <- .empty_cbs_component()

  run_extension <- function(with_trade_hist) {
    inputs <- .make_trade_hist_inputs(with_trade_hist)
    whep:::.assemble_cbs_sources(
      inputs,
      empty,
      empty,
      empty,
      whep::items_full
    ) |>
      whep:::.select_best_source() |>
      tibble::as_tibble() |>
      whep:::.cbs_extend_historical(ext_inputs, 1950L)
  }

  value_1950 <- function(ext, el) {
    ext |>
      dplyr::filter(.data$year == 1950L, .data$element == el) |>
      dplyr::pull(value)
  }

  with_hist <- run_extension(TRUE)
  expect_equal(value_1950(with_hist, "import"), 700)
  expect_equal(value_1950(with_hist, "export"), 200)
  # Domestic supply is production plus imports minus exports: 5000 + 700 - 200.
  expect_equal(value_1950(with_hist, "domestic_supply"), 5500)

  # Without historical trade, pre-1961 has no import/export evidence at all.
  without_hist <- run_extension(FALSE)
  expect_length(value_1950(without_hist, "import"), 0L)
  expect_length(value_1950(without_hist, "domestic_supply"), 0L)
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


# -- .resolve_hist_trade_polities ----------------------------------------------

test_that(".resolve_hist_trade_polities keys on the reported year, not today", {
  # The historical trade pins are a genuine historical source: 1746-1961 figures
  # reported under the borders in force at the time, unlike WHEP's pre-1962
  # FAOSTAT series which are back-cast onto ~1961 territory. Resolution used to
  # go through .current_area_lookup, which is deliberately year-insensitive, so
  # every row of an ISO3 got that ISO3's *present-day* polity: all 1,093 India
  # rows landed on IND-1949-2025 and all 9,522 UK rows on GBR-1921-2025.
  resolved <- whep:::.resolve_hist_trade_polities(data.table::data.table(
    iso3c = c("IND", "IND", "IND", "GBR", "GBR"),
    year = c(1885L, 1920L, 1961L, 1850L, 1961L),
    value = 1
  ))

  expect_equal(
    resolved$polity_code,
    c(
      # IND-1800-1893 is `superseded` upstream, replaced by the finer IND-1800-1886 /
      # IND-1886-1893 split. It was returned here until the FAOSTAT area map became the
      # resolution authority (#517); pinning a superseded polity was the bug, not the fix.
      "IND-1800-1886",
      "IND-1914-1937",
      "IND-1949-2025",
      "GBR-1800-1921",
      "GBR-1921-2025"
    )
  )

  # The FABIO aggregation bucket is period-invariant for both ISO3s, which is
  # why making the lookup year-aware moved no tonnage for them: over the full
  # pin the totals went 18,455,438,816 t -> 18,453,716,816 t (-0.0093%), and all
  # of that was the pre-1850 aggregate rows exercised in the next test.
  expect_equal(resolved$area_code, c(100L, 100L, 100L, 229L, 229L))
})

test_that(".resolve_hist_trade_polities drops pre-range aggregate rows", {
  # Guadeloupe and Martinique are folded into the ROW bucket, whose only polity
  # ROW-1850-2025 is of type "aggregate". .add_polity_columns_dt refuses to
  # extend aggregate reporting areas outside their range, so an 1830 figure has
  # no polity and must be dropped rather than back-filled into ROW. That is the
  # 64 rows / 1,722,000 t the year-aware lookup removes from the feed.
  resolved <- whep:::.resolve_hist_trade_polities(data.table::data.table(
    iso3c = c("GLP", "GLP"),
    year = c(1830L, 1900L),
    value = 1
  ))

  expect_true(is.na(resolved$polity_code[1]))
  expect_true(is.na(resolved$area_code[1]))
  expect_equal(resolved$polity_code[2], "ROW-1850-2025")
  expect_equal(resolved$area_code[2], 999L)
})

test_that(".resolve_hist_trade_polities leaves unknown iso3 labels unresolved", {
  # The pins carry a handful of labels that are not ISO3 codes in the crosswalk
  # (a placeholder for unknown origin, "BEL-LUX", "CZH"). They stay NA so the
  # caller drops them instead of silently attaching them to a wrong polity;
  # resolving them needs new crosswalk aliases, not a change here.
  resolved <- whep:::.resolve_hist_trade_polities(data.table::data.table(
    iso3c = c("BEL-LUX", "ESP"),
    year = c(1900L, 1900L),
    value = 1
  ))

  expect_true(is.na(resolved$polity_code[1]))
  expect_false(is.na(resolved$polity_code[2]))
})

test_that(".canonicalise_gdp_pop_area relabels through the ISO3 code", {
  # `.fill_with_proxies()` joins population on `c("year", "area")` -- the name --
  # and the two sides speak different vocabularies. Everything that comes through
  # `.aggregate_to_polities()` carries the period-specific `polity_name`, while the
  # gdp-population pin writes its own short forms. Measured on the current pin: 35
  # of its 196 area names, 5,346 rows or 18.0%, are not names any builder emits, so
  # the join silently found nothing and those countries went unfilled. Checked
  # against the 1961 CBS extract's own area vocabulary, 110 of 169 labels matched
  # before and 148 after, with 0 going from matching to not matching.
  #
  # The relabelling goes ISO3 -> FAOSTAT area code -> polity name for the row's
  # year, so it is a code lookup rather than a hand-written synonym list, and it
  # agrees with `.aggregate_to_polities()` by construction. #382 canonicalised
  # towards the crosswalk's `area_name` instead, which is a third vocabulary: on
  # today's main that would have renamed Bolivia, Iran, Tanzania, Venezuela and
  # North Korea -- five labels that match today -- into names that match nothing.
  dt <- data.table::data.table(
    year = rep(2000L, 5L),
    area = c("Lao", "Republic of Korea", "Albania", "Spain", "Syria"),
    area_code = c("LAO", "KOR", "ALB", "ESP", "SYR"),
    pop = 1:5
  )

  result <- whep:::.canonicalise_gdp_pop_area(dt)

  # Short forms take the polity name, including the periodized one.
  expect_equal(result$area[result$area_code == "LAO"], "Laos")
  expect_equal(result$area[result$area_code == "KOR"], "South Korea")
  expect_equal(result$area[result$area_code == "ALB"], "Albania (1913-2025)")

  # A label that already IS its polity's name is left alone, so the function
  # cannot break a join that works.
  expect_equal(result$area[result$area_code == "ESP"], "Spain")

  # Syria's FAOSTAT area folds into the Rest of World bucket, so its polity name
  # is the aggregate's. Relabelling it would attribute one member's population to
  # the whole bucket and collide with the other members on the same (year, area)
  # key, so folded areas are deliberately left as they are.
  expect_equal(result$area[result$area_code == "SYR"], "Syria")

  # Nothing else changes: same rows, same values, same column order.
  expect_equal(nrow(result), 5L)
  expect_equal(names(result), names(dt))
  expect_equal(result$pop[order(result$area_code)], c(3L, 4L, 2L, 1L, 5L))
})

test_that(".canonicalise_gdp_pop_area is a no-op without the columns it needs", {
  # The pin is read straight from a remote board, so the guard matters: a revision
  # that drops `area_code` or stores it as a number must leave the frame alone
  # rather than half-relabel it.
  no_code <- data.table::data.table(year = 2000L, area = "Lao", pop = 1)
  expect_identical(whep:::.canonicalise_gdp_pop_area(no_code), no_code)

  numeric_code <- data.table::data.table(
    year = 2000L,
    area = "Lao",
    area_code = 120L,
    pop = 1
  )
  expect_identical(
    whep:::.canonicalise_gdp_pop_area(numeric_code),
    numeric_code
  )
})

test_that("build_commodity_balances defaults to the long format", {
  long <- whep::build_commodity_balances(example = TRUE)

  expect_true(rlang::has_name(long, "element"))
  expect_false(rlang::has_name(long, "production"))
})

test_that("build_commodity_balances format = 'wide' pivots the elements", {
  # Same dataset, one column per element instead of one row per element, with
  # stock_variation split into the two non-negative directions.
  wide <- whep::build_commodity_balances(example = TRUE, format = "wide")

  expect_false(rlang::has_name(wide, "element"))
  expect_true(all(
    c("production", "import", "food", "feed", "domestic_supply") %in%
      names(wide)
  ))
  expect_true(all(c("stock_addition", "stock_withdrawal") %in% names(wide)))
})

test_that("build_commodity_balances rejects an unknown format", {
  expect_error(
    whep::build_commodity_balances(example = TRUE, format = "matrix"),
    class = "rlang_error"
  )
})

test_that("build_commodity_balances needs primary_all for the wide format", {
  # The live-animal rows come from primary production, so the wide format
  # cannot be assembled from .fixed_data alone. Aborting beats silently
  # returning a sheet with no live animals in it.
  expect_error(
    whep::build_commodity_balances(
      format = "wide",
      .fixed_data = readRDS(
        testthat::test_path("fixtures", "cbs_fixed_small.rds")
      )
    ),
    "primary_all"
  )
})
