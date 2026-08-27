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

test_that(".fix_item_codes converts new-FBS rice, which is paddy basis", {
  # faostat-fbs-new reports item 2807 "Rice and products" in paddy (rough-rice)
  # equivalent: India 2010 production is 143,963 kt there against 96,023 kt for
  # the milled item 2805 in faostat-fbs-old. WHEP's contract for this item is
  # milled equivalent, so the extract path must convert it (#751).
  df <- tibble::tribble(
    ~item_cbs_code, ~item_cbs,           ~value,
    2807L,          "Rice and products", 100
  )

  result <- whep:::.fix_item_codes(
    df,
    paddy_rice_names = whep:::.paddy_rice_names("faostat")
  )

  expect_equal(result$item_cbs_code, 2807L)
  expect_equal(result$value, 100 * whep:::.rice_milled_extraction_rate())
})

test_that(".fix_item_codes leaves an already-labelled rice row alone", {
  # .prepare_historical_cbs() relabels rows from the items_full lookup before
  # calling this, so "Rice and products" there is the canonical label and says
  # nothing about the mass basis. The default must not convert it, or that path
  # would be double-converted at 0.67^2.
  df <- tibble::tribble(
    ~item_cbs_code, ~item_cbs,           ~value,
    2807L,          "Rice and products", 100
  )

  result <- whep:::.fix_item_codes(df)

  expect_equal(result$value, 100)
})

test_that(".fix_item_codes never converts milled rice", {
  df <- tibble::tribble(
    ~item_cbs_code, ~item_cbs,                  ~value,
    2805L,          "Rice (Milled Equivalent)", 100
  )

  result <- whep:::.fix_item_codes(
    df,
    paddy_rice_names = whep:::.paddy_rice_names("faostat")
  )

  expect_equal(result$item_cbs_code, 2807L)
  expect_equal(result$value, 100)
})

test_that("a paddy source converts even once the row is relabelled", {
  # The complement of `.fix_item_codes leaves an already-labelled rice row
  # alone` (#778). `.prepare_historical_cbs()` relabels every 2807 row "Rice
  # and products", so the NAME cannot say what the basis is -- but the SOURCE
  # can, and at that ingest boundary it is the only thing that can. Keyed on
  # source, the row converts.
  df <- tibble::tribble(
    ~item_cbs_code, ~item_cbs,           ~source,               ~value,
    2807L,          "Rice and products", "historical_mysource", 100
  )

  result <- whep:::.fix_item_codes(df, paddy_by_source = TRUE)

  expect_equal(result$item_cbs_code, 2807L)
  expect_equal(
    result$value,
    100 * whep:::.rice_milled_extraction_rate()
  )
})

test_that("a milled-basis source is left alone when keying on source", {
  # `.rice_source_is_paddy()` is a whitelist, so a CBS-derived source -- which
  # is already milled equivalent -- must not be converted even with the
  # source rule switched on.
  df <- tibble::tribble(
    ~item_cbs_code, ~item_cbs,           ~source,             ~value,
    2807L,          "Rice and products", "FAOSTAT_FBS_Old",   100
  )

  result <- whep:::.fix_item_codes(df, paddy_by_source = TRUE)

  expect_equal(result$value, 100)
})

test_that("one historical rice row yields one tonnage in both pipelines", {
  # whep#778: `.read_historical_production` is the single reader behind the
  # public `historical_data` argument of BOTH `build_primary_production()` and
  # `build_commodity_balances()`. Item 2807 is milled equivalent throughout
  # WHEP (validation/rice_mass_basis.R enforces it across FBS vintages), so
  # the same input row must arrive at the same tonnage down either path.
  # Before the fix the production path returned 67 t and the CBS path 100 t.
  hist <- tibble::tribble(
    ~year, ~area_code, ~item_prod_code, ~element,     ~unit,    ~value, ~source,
    1900L, 100L,       "27",            "production", "tonnes", 100,    "mysource"
  )

  cbs <- whep:::.prepare_historical_cbs(hist, years = 1900L)
  prod <- whep:::.prepare_historical_production(hist, years = 1900L) |>
    whep:::.fix_rice_milled_equiv()

  expect_equal(cbs$item_cbs_code, 2807L)
  expect_equal(prod$item_cbs_code, 2807L)
  expect_equal(cbs$value, prod$value)
  expect_equal(cbs$value, 100 * whep:::.rice_milled_extraction_rate())
})

test_that("converting historical rice keeps the supply-use identity", {
  # whep#778: the conversion must be uniform across elements, or it turns a
  # balanced sheet into an unbalanced one. Scaling every element of a balanced
  # rice row by the same rate leaves supply == use exactly, which is the
  # invariant worth asserting rather than any single tonnage.
  hist <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~element,     ~unit,    ~value, ~source,
    1900L, 100L,       2807L,          "production", "tonnes", 100,    "mysrc",
    1900L, 100L,       2807L,          "import",     "tonnes", 20,     "mysrc",
    1900L, 100L,       2807L,          "export",     "tonnes", 30,     "mysrc",
    1900L, 100L,       2807L,          "food",       "tonnes", 70,     "mysrc",
    1900L, 100L,       2807L,          "feed",       "tonnes", 20,     "mysrc"
  )

  wide <- whep:::.prepare_historical_cbs(hist, years = 1900L) |>
    tidyr::pivot_wider(
      id_cols = c("year", "area_code", "item_cbs_code"),
      names_from = "element",
      values_from = "value",
      values_fill = 0
    ) |>
    whep::ensure_columns(tibble::tibble(
      import = numeric(),
      export = numeric(),
      food = numeric(),
      feed = numeric(),
      seed = numeric(),
      processing = numeric(),
      other_uses = numeric(),
      stock_withdrawal = numeric(),
      stock_addition = numeric()
    )) |>
    dplyr::mutate(dplyr::across(
      dplyr::everything(),
      \(x) tidyr::replace_na(x, 0)
    ))

  rate <- whep:::.rice_milled_extraction_rate()
  expect_equal(wide$production, 100 * rate)
  expect_equal(wide$food, 70 * rate)

  balance <- whep::check_supply_use_balance(wide)
  expect_true(all(balance$balanced))
})

test_that("historical wheat is not rescaled by the rice rule", {
  # Control: the conversion must be keyed on the item as well as the source.
  hist <- tibble::tribble(
    ~year, ~area_code, ~item_prod_code, ~element,     ~unit,    ~value, ~source,
    1900L, 100L,       "15",            "production", "tonnes", 100,    "mysource"
  )

  cbs <- whep:::.prepare_historical_cbs(hist, years = 1900L)

  expect_equal(cbs$item_cbs_code, 2511L)
  expect_equal(cbs$value, 100)
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

test_that(".read_land_areas_wide keys its output on the reporting bucket", {
  # The frame this table feeds is labelled with `polity_name`, but
  # .read_land_areas() labels its rows with the crosswalk's STATIC `area_name`.
  # Those two vocabularies diverge for most territories -- FAO area 3 is
  # "Albania (1913-2025)" as a polity and "Albania" as an area -- so the old
  # `by = c("year", "area")` join in .fill_with_proxies() missed them: measured
  # on main, 96 of the LUH2 labels (41.7% of land rows) are names no builder
  # emits, and frame coverage of `agriland` over 1900-1902 was 402 of 606
  # (year, polity) cells against 567 once keyed on the polity.
  #
  # The key is `polity_area_code`, renamed to `area_code`, not `polity_code`:
  # the frame this fills is aggregated to buckets and its `area_code` IS the
  # bucket, so this is the key that needs no label to reach it (whep#698).
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

  expect_equal(result$area_code, 3L)
  expect_false("area" %in% names(result))
  expect_false("polity_code" %in% names(result))
  expect_equal(result$agriland, 5)
})

test_that(".read_land_areas_wide holds back folded aggregate buckets", {
  # Scoped to the explicit fold. WHEP now models the reporting members of
  # bucket 999 in their own right (#459), so there is no Rest-of-World fold
  # by default; what this pins is the fold behaviour itself, which still has
  # to work for anyone reproducing a published-before number.
  withr::local_options(whep.unfold_rest_of_world = "none")
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

  expect_equal(result$area_code, 203L)
  # NO Rest-of-World BUCKET AT ALL. This named the polity code until whep#698
  # re-keyed the table on the reporting bucket; 999 is that bucket's code and
  # is what a folded member would land on now.
  expect_false(999L %in% result$area_code)
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

test_that(".cbs_impute_trade imputes production from destinies when missing", {
  # Item with reported destinies + trade but NO production row: the
  # domestic-supply residual should be imputed into production, not
  # dumped into a large negative stock_variation (#142).
  raw <- tibble::tribble(
    ~year, ~area, ~area_code, ~item_cbs, ~item_cbs_code, ~element, ~value, ~source,
    2023L, "Spain", 203L, "Wheat", 2511L, "food", 3000, "FAOSTAT_FBS_New",
    2023L, "Spain", 203L, "Wheat", 2511L, "import", 500, "FAOSTAT_trade",
    2023L, "Spain", 203L, "Wheat", 2511L, "export", 200, "FAOSTAT_trade"
  )

  result <- whep:::.cbs_impute_trade(raw)

  production <- result |>
    dplyr::filter(element == "production") |>
    dplyr::pull(value)
  stock_variation <- result |>
    dplyr::filter(element == "stock_variation") |>
    dplyr::pull(value)

  # The imputed production is the domestic-supply residual: supply 3000, less
  # imports 500, plus exports 200, less a zero stock variation, giving 2700.
  expect_equal(production, 2700)
  # Balance closes: no spurious negative stock change.
  expect_equal(stock_variation, 0)
})

test_that(".cbs_impute_trade balances a traded item with no production row", {
  # Trade but neither a production row nor any destiny. `.reestimate_domestic
  # _supply()` derives the supply residual from `production + import - export`,
  # which is NA while production is, and `dplyr::if_else(NA, ...)` is NA, so
  # both `domestic_supply` and `stock_variation` came out NA. The rows then
  # vanished downstream on the `value != 0` filters instead of balancing.
  # This is the shape every row recovered from the trade record will have
  # (#762), so the hole has to close before those rows can be created.
  raw <- tibble::tribble(
    ~year, ~area, ~area_code, ~item_cbs, ~item_cbs_code, ~element, ~value, ~source,
    2023L, "Spain", 203L, "Wheat", 2511L, "import", 500, "FAOSTAT_trade",
    2023L, "Spain", 203L, "Wheat", 2511L, "export", 200, "FAOSTAT_trade"
  )

  result <- whep:::.cbs_impute_trade(raw)
  value_of <- function(x) {
    dplyr::pull(dplyr::filter(result, element == x), value)
  }

  expect_false(any(is.na(result$value)))
  # Nothing is produced and nothing is used, so the whole net import is the
  # domestic supply and the stock is untouched.
  expect_equal(value_of("domestic_supply"), 300)
  expect_equal(value_of("production"), 0)
  expect_equal(value_of("stock_variation"), 0)
})

test_that(".cbs_impute_trade balances a net-exported item with no production", {
  # Same hole, mirrored: a re-exporting row whose export exceeds its import.
  # The supply residual is negative, so domestic supply is zero and the
  # production imputation of #142 supplies the 30 the exports need. That
  # imputation is deliberate, and this test pins it only to keep the identity
  # closing; whether it should fire for a row recovered from trade alone is
  # the open question in #762, not something settled here.
  raw <- tibble::tribble(
    ~year, ~area, ~area_code, ~item_cbs, ~item_cbs_code, ~element, ~value, ~source,
    2023L, "Singapore", 200L, "Wheat", 2511L, "import", 50, "FAOSTAT_trade",
    2023L, "Singapore", 200L, "Wheat", 2511L, "export", 80, "FAOSTAT_trade"
  )

  result <- whep:::.cbs_impute_trade(raw)
  value_of <- function(x) {
    dplyr::pull(dplyr::filter(result, element == x), value)
  }

  expect_false(any(is.na(result$value)))
  expect_equal(value_of("domestic_supply"), 0)
  expect_equal(value_of("production"), 30)
  expect_equal(value_of("stock_variation"), 0)
})


# -- trade recovery (#762) -----------------------------------------------------

# Singapore wheat production in two years, and a trade record that reaches
# further than that row set does. Every exclusion the recovery makes is
# represented here, so a single fixture can pin all of them. The CBS covers
# 2009 as well as 2010 on purpose: without it, the year window would be
# unfalsifiable, because the area-label join would drop the 2009 row anyway.
.recovery_cbs <- function() {
  tibble::tribble(
    ~year, ~area, ~area_code, ~item_cbs, ~item_cbs_code, ~element, ~value, ~source,
    2009, "Singapore", 200L, "Wheat and products", 2511, "production", 0, "FAOSTAT_prod",
    2010, "Singapore", 200L, "Wheat and products", 2511, "production", 0, "FAOSTAT_prod"
  )
}

.recovery_trade <- function() {
  tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~element, ~value,
    2010, 200L, 2511, "import", 400617,
    2010, 200L, 2807, "import", 620863,
    2010, 200L, 2807, "export", 91318,
    2010, 200L, 2656, "import", 100,
    2010, 200L, 2656, "export", 5000,
    2010, 200L, 2602, "import", 5000,
    2010, 200L, 2602, "export", 5000,
    2010, 200L, 1049, "import", 290549,
    2010, 13L, 2807, "import", 1000,
    2009, 200L, 2531, "import", 700
  )
}

test_that(".cbs_trade_recovery_rows creates the row the trade join cannot", {
  # The defect: `.cbs_impute_trade()` LEFT-joins trade onto the CBS, so an
  # import whose (year, area, item) has no CBS row is dropped outright. Rice
  # is exactly that shape for Singapore, and it is the largest single item in
  # the 2010 measurement in #762.
  result <- whep:::.cbs_trade_recovery_rows(
    .recovery_cbs(),
    .recovery_trade(),
    years = 2010
  )

  expect_equal(
    dplyr::arrange(result, element)$element,
    c("export", "import")
  )
  expect_equal(unique(result$item_cbs_code), 2807)
  expect_equal(sum(result$value), 620863 + 91318)
  # Provenance, so no consumer reads a recovered row as a balance-sheet one.
  expect_equal(unique(result$source), "FAOSTAT_trade")
})

test_that(".cbs_trade_recovery_rows leaves every excluded key alone", {
  result <- whep:::.cbs_trade_recovery_rows(
    .recovery_cbs(),
    .recovery_trade(),
    years = 2010
  )

  # Wheat: the CBS already carries the key, so the existing left join fills it.
  expect_false(2511 %in% result$item_cbs_code)
  # Beer: a net exporter. A created row has no production, so balancing would
  # invent some to cover the export (#762 keeps that decision open).
  expect_false(2656 %in% result$item_cbs_code)
  # Onions: import and export are equal, so the row would carry no supply at
  # all. The boundary is strict: a created row exists to hold a net import.
  expect_false(2602 %in% result$item_cbs_code)
  # Pigs: `items_cbs$item_type` says live animals are counted in heads, and
  # `get_livestock_cbs()` already supplies that key in the wide CBS.
  expect_false(1049 %in% result$item_cbs_code)
  # Bahrain: no CBS row in 2010, so the bucket has no `area` label to read.
  expect_false(13L %in% result$area_code)
  # 2009 is outside the requested window, though the CBS does cover it.
  expect_false(2009 %in% result$year)
})

test_that(".cbs_trade_recovery_rows labels rows from the right vocabulary", {
  # The `area` label is a property of the (year, area_code) bucket and is read
  # from the CBS itself; a year-free lookup would relabel a merged bucket
  # (whep#563). The item label is year-free, so it comes from `items_cbs` and
  # works for an item the CBS names nowhere -- Meat Meal is in 107 areas'
  # trade records and in no CBS row at all.
  trade <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~element, ~value,
    2010, 200L, 2112, "import", 5000
  )

  result <- whep:::.cbs_trade_recovery_rows(
    .recovery_cbs(),
    trade,
    years = 2010
  )

  expect_equal(result$area, "Singapore")
  expect_equal(result$item_cbs, "Meat Meal")
})

test_that(".cbs_trade_recovery_rows tolerates an empty trade record", {
  empty <- .recovery_trade()[0, ]

  expect_equal(
    nrow(whep:::.cbs_trade_recovery_rows(.recovery_cbs(), NULL, 2010)),
    0L
  )
  expect_equal(
    nrow(whep:::.cbs_trade_recovery_rows(.recovery_cbs(), empty, 2010)),
    0L
  )
  # Non-empty, but every row excluded.
  expect_equal(
    nrow(whep:::.cbs_trade_recovery_rows(
      .recovery_cbs(),
      dplyr::filter(.recovery_trade(), item_cbs_code == 2511),
      2010
    )),
    0L
  )
})

test_that(".cbs_trade_recovery_rows aborts on a fanned-out trade key", {
  # A trade record with two rows on one (year, area, item, element) would be
  # summed into a single created row by the reshape, which is how a crosswalk
  # fan-out turns into a double count (whep#164, whep#240). It must abort
  # rather than reshape.
  fanned <- dplyr::bind_rows(
    .recovery_trade(),
    tibble::tibble(
      year = 2010,
      area_code = 200L,
      item_cbs_code = 2807,
      element = "import",
      value = 1
    )
  )

  expect_error(
    whep:::.cbs_trade_recovery_rows(.recovery_cbs(), fanned, 2010),
    "not unique"
  )
})

test_that(".cbs_trade_recovery_rows aborts on a split area label", {
  # One `area_code` must carry one `area` label (whep#563). If the CBS itself
  # disagrees, the inner join would emit the created row twice, once per label.
  split_cbs <- dplyr::bind_rows(
    .recovery_cbs(),
    dplyr::mutate(.recovery_cbs(), area = "Singapore (former)")
  )

  expect_error(
    whep:::.cbs_trade_recovery_rows(split_cbs, .recovery_trade(), 2010),
    "more than one"
  )
})

test_that("a recovered row balances and invents no production", {
  # End to end through the imputation the recovered rows feed. The created row
  # must close the balance identity on its own terms: its whole net import is
  # domestic supply, nothing is produced, and the stock is untouched. The
  # destiny split of that supply happens later, in `.cbs_fill_destinies()`.
  recovered <- whep:::.cbs_trade_recovery_rows(
    .recovery_cbs(),
    .recovery_trade(),
    years = 2010
  )
  bound <- whep:::.cbs_bind_recovered(.recovery_cbs(), recovered)

  result <- whep:::.cbs_impute_trade(bound)
  rice <- dplyr::filter(result, item_cbs_code == 2807)
  value_of <- function(x) dplyr::pull(dplyr::filter(rice, element == x), value)

  expect_false(any(is.na(result$value)))
  expect_equal(value_of("domestic_supply"), 620863 - 91318)
  # Nothing produced, and nothing taken from stock: the identity closes on the
  # trade record alone. A net-exported row is what would force the cascade to
  # invent one of these, which is why the recovery does not create those.
  expect_equal(value_of("production"), 0)
  expect_equal(value_of("stock_variation"), 0)
  expect_equal(
    value_of("production") +
      value_of("import") -
      value_of("export") -
      value_of("stock_variation"),
    value_of("domestic_supply")
  )
})

test_that("binding recovered rows adds keys and changes no existing one", {
  # Row-and-key accounting, not just tonnage: the recovery may only ADD keys.
  # A created key that already existed would double the item in the wide CBS.
  cbs <- .recovery_cbs()
  recovered <- whep:::.cbs_trade_recovery_rows(cbs, .recovery_trade(), 2010)
  bound <- tibble::as_tibble(whep:::.cbs_bind_recovered(cbs, recovered))

  key <- c("year", "area_code", "item_cbs_code", "element")
  expect_equal(nrow(bound), nrow(cbs) + nrow(recovered))
  expect_equal(
    nrow(dplyr::distinct(bound, dplyr::pick(dplyr::all_of(key)))),
    nrow(bound)
  )
  expect_equal(
    as.data.frame(dplyr::semi_join(bound, cbs, by = key)),
    as.data.frame(cbs),
    ignore_attr = TRUE
  )
})

test_that("recovered sources reach the frozen source lookup", {
  # `src_lookup` is extracted before the rows exist, so without this the
  # recovered rows -- and every element derived from them -- ship source NA.
  cbs <- .recovery_cbs()
  recovered <- whep:::.cbs_trade_recovery_rows(cbs, .recovery_trade(), 2010)
  src <- whep:::.extract_source_lookup(data.table::as.data.table(cbs))

  result <- tibble::as_tibble(whep:::.add_recovered_sources(src, recovered))

  key <- c("year", "area_code", "item_cbs_code", "element")
  expect_equal(nrow(result), nrow(src) + nrow(recovered))
  expect_equal(sum(duplicated(result[, key])), 0L)
  expect_equal(
    sort(unique(result$source)),
    c("FAOSTAT_prod", "FAOSTAT_trade")
  )
  expect_equal(whep:::.add_recovered_sources(src, recovered[0, ]), src)
})

test_that(".fix_cbs wires trade recovery through the whole cascade", {
  # The only end-to-end coverage of `.fix_cbs()`, and the only place the
  # recovery's wiring is visible: its placement, the source lookup it has to
  # extend (frozen one step earlier, so without that every recovered element
  # ships `source = NA`), and what the destiny cascade does with the created
  # supply. No pin and no network -- the whole chain runs on this tribble.
  raw <- tibble::tribble(
    ~year, ~area, ~area_code, ~item_cbs, ~item_cbs_code, ~element, ~value, ~source,
    2010, "Singapore", 200L, "Wheat and products", 2511, "production", 1000, "FAOSTAT_prod",
    2010, "Singapore", 200L, "Wheat and products", 2511, "food", 800, "FAOSTAT_prod"
  )
  attr(raw, ".years") <- 2010L
  attr(raw, ".fao_trade") <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~element, ~value,
    2010, 200L, 2807, "import", 620863,
    2010, 200L, 2807, "export", 91318
  )

  off <- whep:::.fix_cbs(raw)
  on <- whep:::.fix_cbs(raw, trade_recovery = "net_import")

  # The defect: the trade join can only fill, so rice never reaches the CBS.
  expect_false(2807 %in% off$item_cbs_code)
  rice <- dplyr::filter(on, item_cbs_code == 2807)
  value_of <- function(x) dplyr::pull(dplyr::filter(rice, element == x), value)
  expect_equal(value_of("import"), 620863)
  expect_equal(value_of("export"), 91318)
  expect_equal(value_of("domestic_supply"), 620863 - 91318)
  # Provenance survives the frozen source lookup.
  expect_equal(
    dplyr::filter(rice, element %in% c("import", "export"))$source,
    c("FAOSTAT_trade", "FAOSTAT_trade")
  )
  # The wheat rows the CBS already had are untouched.
  expect_equal(
    dplyr::filter(on, item_cbs_code == 2511),
    dplyr::filter(off, item_cbs_code == 2511)
  )
  # WHAT THE CASCADE DOES WITH THE CREATED SUPPLY, pinned because it is an
  # allocation rule and not an identity: with no destiny of its own the row
  # falls to the item's default destiny, which for rice is processing, not
  # food. This is the open decision in #762 -- pinned so that changing it is
  # a visible change, not a silent one.
  expect_equal(value_of("processing"), 620863 - 91318)
})

test_that("build_commodity_balances validates trade_recovery", {
  expect_error(
    build_commodity_balances(example = TRUE, trade_recovery = "everything"),
    class = "rlang_error"
  )
  expect_warning(
    build_commodity_balances(
      .fixed_data = tibble::tibble(
        year = c(2010L, 2011L),
        area = "Spain",
        area_code = 203L,
        item_cbs = "Wheat and products",
        item_cbs_code = 2511L,
        element = "import",
        value = c(1, 2),
        source = "FAOSTAT_trade"
      ),
      trade_recovery = "net_import"
    ),
    "ignored"
  )
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

test_that("a duplicated (key, source) pair is summed, not counted", {
  # whep#557: with no `fun.aggregate`, dcast falls back to a row count, and it
  # applies that to EVERY cell, so one duplicate anywhere turns the whole table
  # into counts. The two buckets that used to duplicate, 206 and 999, no longer do,
  # which is why this guard is pinned by a test instead of by a measurement.
  cbs_raw_all <- tibble::tribble(
    ~area, ~area_code, ~item_cbs, ~item_cbs_code, ~element, ~year, ~value, ~source, ~unit,
    "Bucket", 206L, "Wheat", 2511L, "production", 2010L, 244000, "FAOSTAT_FBS_Old", "tonnes",
    "Bucket", 206L, "Wheat", 2511L, "production", 2010L, 3103000, "FAOSTAT_FBS_Old", "tonnes",
    "Bucket", 206L, "Maize", 2514L, "production", 2010L, 500, "FAOSTAT_FBS_Old", "tonnes"
  )

  result <- whep:::.select_best_source(cbs_raw_all)
  wheat <- result |> dplyr::filter(item_cbs_code == 2511L)
  maize <- result |> dplyr::filter(item_cbs_code == 2514L)

  expect_equal(nrow(wheat), 1L)
  expect_equal(wheat$value, 3347000)
  # The unduplicated key must keep its tonnes, not become its row count (1).
  expect_equal(maize$value, 500)
})


# -- .cbs_area_labels (whep#580) ----------------------------------------------

# One code, whose `area` label legitimately changes at a period boundary: `area`
# is the periodized polity name, so a multi-year build offers several labels for
# one code. On a real 1850-2023 `cbs_raw_all`, 75 of the 216 codes do (up to
# four labels each) and shuffling the rows flipped the label for 13.
.period_rows <- function() {
  tibble::tribble(
    ~area,                ~area_code, ~item_cbs, ~item_cbs_code, ~element,     ~year, ~value, ~source,
    "Utopia (1900-1950)", 300L,       "Wheat",   2511L,          "production", 1940L, 100,    "FAOSTAT_FBS_Old",
    "Utopia (1950-2025)", 300L,       "Wheat",   2511L,          "production", 1990L, 200,    "FAOSTAT_FBS_Old"
  )
}

test_that("a code's area label survives any reordering of the input", {
  # whep#580: the lookup kept the FIRST row seen for an `area_code`, so which
  # period named the code for the whole build was decided by what happened to
  # sort first, and nothing pinned it.
  rows <- .period_rows()

  forward <- whep:::.select_best_source(rows)
  reversed <- whep:::.select_best_source(rows[c(2L, 1L), ])

  expect_equal(unique(forward$area), "Utopia (1900-1950)")
  expect_equal(unique(reversed$area), unique(forward$area))
})

test_that("the label comes from the highest-priority source, not the first row", {
  # The order `.assemble_cbs_sources()` binds its sources in is what actually
  # decided the label, and it is now stated instead of implied: FBS_New outranks
  # FBS_Old wherever it reports the code, however the rows arrive.
  rows <- tibble::tribble(
    ~area,          ~area_code, ~item_cbs, ~item_cbs_code, ~element,     ~year, ~value, ~source,
    "Old vintage",  300L,       "Wheat",   2511L,          "production", 1990L, 100,    "FAOSTAT_FBS_Old",
    "New vintage",  300L,       "Wheat",   2511L,          "production", 1990L, 90,     "FAOSTAT_FBS_New"
  )

  expect_equal(unique(whep:::.select_best_source(rows)$area), "New vintage")
  expect_equal(
    unique(whep:::.select_best_source(rows[c(2L, 1L), ])$area),
    "New vintage"
  )
})

test_that("within one source the earliest year names the code", {
  # The second criterion, and the one whep#580 is about: a code's label is a
  # period name, so which YEAR is consulted decides it. Reversing the rows used
  # to swap the answer.
  rows <- tibble::tribble(
    ~area,                ~area_code, ~item_cbs, ~item_cbs_code, ~element,     ~year, ~value, ~source,
    "Utopia (1950-2025)", 300L,       "Wheat",   2511L,          "production", 1990L, 200,    "FAOSTAT_FBS_New",
    "Utopia (1900-1950)", 300L,       "Wheat",   2511L,          "production", 1940L, 100,    "FAOSTAT_FBS_New"
  )

  expect_equal(
    unique(whep:::.select_best_source(rows)$area),
    "Utopia (1900-1950)"
  )
  expect_equal(
    unique(whep:::.select_best_source(rows[c(2L, 1L), ])$area),
    "Utopia (1900-1950)"
  )
})

test_that("an unranked source still labels a code it alone reports", {
  # `trade_hist` is not in the source order, so it ranks last -- but a code only
  # that source reports must still come out labelled. `area` is a join key, and
  # an NA there drops the code from four inner joins.
  rows <- tibble::tribble(
    ~area,      ~area_code, ~item_cbs, ~item_cbs_code, ~element, ~year, ~value, ~source,
    "Zedland",  400L,       "Wheat",   2511L,          "import", 1950L, 100,    "trade_hist",
    "Aardland", 400L,       "Wheat",   2511L,          "import", 1950L, 90,     "trade_hist"
  )

  expect_equal(unique(whep:::.select_best_source(rows)$area), "Aardland")
  expect_equal(
    unique(whep:::.select_best_source(rows[c(2L, 1L), ])$area),
    "Aardland"
  )
})

test_that("one area_code still yields exactly one area label", {
  # The invariant the value-neutrality checks cannot see (whep#563): a second
  # label for one code splits every join keyed on the area label and its code,
  # without moving a single value.
  labels <- whep:::.cbs_area_labels(
    data.table::as.data.table(.period_rows())
  )

  expect_equal(nrow(labels), 1L)
  expect_equal(labels$area_code, 300L)
  expect_false(anyNA(labels$area))
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

# -- Dairy processing pathway (#757) ------------------------------------------

# FAOSTAT's new FBS reports a `processing` destiny for milk that is the milk
# churned into butter and ghee. Without a pathway for it,
# .cbs_redistribute_notprocessed splits that mass onto food/feed/export and
# deletes the processing row, inflating 2010 world milk food by 30.5%.

test_that("cb_processing carries the milk to butter pathway (#757)", {
  dairy <- whep::cb_processing |>
    dplyr::filter(.data$ProcessedItem == "Milk - Excluding Butter")

  expect_equal(nrow(dairy), 1L)
  expect_equal(dairy$item_cbs, "Butter, Ghee")
  # FAO (1997) Technical Conversion Factors, "Butter of Cow Milk" extraction
  # rates: median 4.5% over 69 reporting countries (range 3.3-7.3%).
  expect_equal(dairy$Product_fraction, 0.045)
  expect_equal(dairy$Value_fraction, 1)
})

test_that(".processed_raw turns milk processing into butter (#757)", {
  cbs <- tibble::tribble(
    ~year, ~area, ~area_code, ~item_cbs, ~item_cbs_code, ~element, ~value,
    2010L, "Spain", 203L, "Milk - Excluding Butter", 2848L, "processing", 1000,
    2010L, "Spain", 203L, "Milk - Excluding Butter", 2848L, "food", 4000
  )

  result <- whep:::.processed_raw(cbs, whep::cb_processing)

  expect_equal(nrow(result), 1L)
  expect_equal(result$item_cbs, "Butter, Ghee")
  expect_equal(result$element, "production")
  expect_equal(result$value_proc, 45)
})

test_that(".cbs_redistribute_notprocessed keeps matched processing (#757)", {
  cbs <- tibble::tribble(
    ~year, ~area, ~area_code, ~item_cbs, ~item_cbs_code, ~element, ~value,
    2010L, "Spain", 203L, "Milk", 2848L, "processing", 200,
    2010L, "Spain", 203L, "Milk", 2848L, "food", 500,
    2010L, "Spain", 203L, "Milk", 2848L, "feed", 60,
    2010L, "Spain", 203L, "Milk", 2848L, "export", 240,
    2010L, "Spain", 203L, "Milk", 2848L, "domestic_supply", 760
  ) |>
    dplyr::mutate(source = "FAOSTAT_FBS_New")

  matched <- tibble::tribble(
    ~year, ~area, ~area_code, ~processed_item,
    2010L, "Spain", 203L, "Milk"
  )

  kept <- whep:::.cbs_redistribute_notprocessed(cbs, matched)

  expect_equal(
    dplyr::filter(kept, .data$element == "processing")$value,
    200
  )
  expect_equal(dplyr::filter(kept, .data$element == "food")$value, 500)
  expect_equal(dplyr::filter(kept, .data$element == "export")$value, 240)

  # Negative control: with no pathway the processing is split onto the other
  # destinies and the processing row disappears. This is the #757 mechanism,
  # and the reason the dairy pathway above has to exist.
  unmatched <- matched[0L, ]
  split <- whep:::.cbs_redistribute_notprocessed(cbs, unmatched)

  expect_equal(nrow(dplyr::filter(split, .data$element == "processing")), 0L)
  expect_gt(dplyr::filter(split, .data$element == "food")$value, 500)
})


test_that(".resolve_processed_production keeps read production (#757)", {
  items <- tibble::tribble(
    ~item_cbs, ~item_cbs_code, ~group,
    "Butter, Ghee", 2740L, "Livestock products",
    "Soyabean Oil", 2571L, "Crop products"
  )

  observed <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~element, ~value,
    2000L, 203L, 2740L, "production", 7000,
    2010L, 203L, 2740L, "production", 9000
  )

  # The old FBS records a trace of milk processing, so the pathway emits a
  # butter row worth nothing in 2000. It must not cancel the read value.
  processed <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~element, ~value,
    2000L, 203L, 2740L, "production", 0,
    2010L, 203L, 2740L, "production", 8900
  )

  result <- whep:::.resolve_processed_production(observed, processed, items)

  expect_equal(result$observed$year, 2000L)
  expect_equal(result$observed$value, 7000)
  expect_equal(result$processed$year, 2010L)
  expect_equal(result$processed$value, 8900)
})

test_that(".resolve_processed_production leaves crop products alone (#757)", {
  items <- tibble::tribble(
    ~item_cbs, ~item_cbs_code, ~group,
    "Soyabean Oil", 2571L, "Crop products"
  )

  # Crop production is already dropped wholesale upstream, so `observed` never
  # carries it. A zero-valued crop estimate must still survive, because that
  # is the pre-#757 behaviour for every existing pathway.
  processed <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~element, ~value,
    2010L, 203L, 2571L, "production", 0
  )

  result <- whep:::.resolve_processed_production(
    processed[0L, ],
    processed,
    items
  )

  expect_equal(nrow(result$processed), 1L)
  expect_equal(result$processed$value, 0)
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
    # and .read_land_areas_wide() emits the reporting bucket: both proxies are
    # resolved onto the frame's own `area_code` rather than joined on a label.
    gdp_pop = tibble::tibble(
      year = 1950:1961,
      area = "Spain",
      area_code = "ESP",
      pop = 1:12
    ),
    land_areas_wide = tibble::tibble(
      year = 1950:1961,
      area_code = 203L,
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
    area_code = 3L,
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

test_that(".fill_with_proxies reaches a bucket its label misnames", {
  # whep#698. The frame's `area` is ONE label for the whole build, so it names
  # one period of a code; the proxy side resolves per year with the pre-1962
  # back-cast anchored at 1961. For FAO area 29 those are two different
  # polities -- the label says `BDI-1962-2025`, the anchor says
  # `BDI-1922-1962` -- and while the fill was keyed on `polity_code` the two
  # never met, so Burundi's whole pre-1962 series went through the historical
  # fill with no population proxy at all. Measured on a real
  # `build_commodity_balances(prim, 1955, 1965)` frame: 35 of 1,267
  # (`area_code`, `area`, `year`) keys resolved to a different polity that way
  # and 70 more to none.
  #
  # Keyed on the reporting bucket both sides say 29 and the label is not
  # consulted at all.
  frame <- tibble::tibble(
    year = 1955:1957,
    area = "Burundi",
    area_code = 29L,
    item_cbs = "Wheat and products",
    item_cbs_code = 2511L,
    food = c(100, NA, NA),
    other_uses = NA_real_,
    feed = NA_real_,
    processing = NA_real_
  )
  gdp_pop <- tibble::tibble(
    year = 1955:1957,
    area = "Burundi",
    area_code = "BDI",
    pop = c(1000, 1100, 1210)
  )
  land_wide <- tibble::tibble(
    year = 1955:1957,
    area_code = 29L,
    Cropland = 1,
    Pasture = 1,
    agriland = 2
  )

  result <- whep:::.fill_with_proxies(frame, gdp_pop, land_wide)

  expect_false(any(is.na(result$pop)))
  expect_equal(result$food, c(100, 110, 121))
})

test_that(".fill_with_proxies gives a promoted member its own population", {
  # The other half of whep#698, and the reason re-keying on `polity_code` would
  # have been wrong. `.unfold_rest_of_world()` promotes a member's
  # `polity_area_code` but NOT its `polity_code`, so every promoted member
  # still answers `ROW-1850-2025` and carries the shared label "Rest of World".
  # Keyed on the polity, all of them collapsed onto one proxy row holding the
  # SUM of their populations -- measured on the real pin, four pin rows shared
  # `(year, ROW-1850-2025)` while no two shared a `(year, polity_area_code)` --
  # and four buckets of a real 1955-1965 frame were being grown on that sum.
  #
  # Equatorial Guinea's population is flat here and Syria's quadruples, so the
  # bucket-keyed fill leaves food flat while the polity-keyed one would have
  # nearly doubled it.
  frame <- tibble::tibble(
    year = 1955:1957,
    area = "Rest of World",
    area_code = 61L,
    item_cbs = "Wheat and products",
    item_cbs_code = 2511L,
    food = c(100, NA, NA),
    other_uses = NA_real_,
    feed = NA_real_,
    processing = NA_real_
  )
  gdp_pop <- tibble::tibble(
    year = rep(1955:1957, 2L),
    area = rep(c("Equatorial Guinea", "Syria"), each = 3L),
    area_code = rep(c("GNQ", "SYR"), each = 3L),
    pop = c(100, 100, 100, 1000, 2000, 4000)
  )
  land_wide <- tibble::tibble(
    year = 1955:1957,
    area_code = 61L,
    Cropland = 1,
    Pasture = 1,
    agriland = 2
  )

  result <- whep:::.fill_with_proxies(frame, gdp_pop, land_wide)

  expect_equal(result$pop, c(100, 100, 100))
  expect_equal(result$food, c(100, 100, 100))
})

test_that(".fill_with_proxies leaves a folded aggregate bucket unproxied", {
  # Scoped to the explicit fold. WHEP now models the reporting members of
  # bucket 999 in their own right (#459), so there is no Rest-of-World fold
  # by default; what this pins is the fold behaviour itself, which still has
  # to work for anyone reproducing a published-before number.
  withr::local_options(whep.unfold_rest_of_world = "none")
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
  # Keyed on Syria's OWN reporting area, which is what `.read_land_areas_wide()`
  # emits for it -- the fold sends the bucket's code to 999, so nothing here
  # reaches the frame.
  land_wide <- tibble::tibble(
    year = 1950:1952,
    area_code = 212L,
    Cropland = 1,
    Pasture = 1,
    agriland = c(2, 2.2, 2.4)
  )

  result <- whep:::.fill_with_proxies(frame, gdp_pop, land_wide)

  expect_true(all(is.na(result$pop)))
  expect_true(all(is.na(result$agriland)))
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
    # Both proxies are resolved onto the frame's own `area_code`, not joined on
    # an area label: the gdp/population pin is keyed by ISO3 in `area_code`, and
    # .read_land_areas_wide() emits the reporting bucket.
    gdp_pop = tibble::tibble(
      year = 1950L,
      area = "Spain",
      area_code = "ESP",
      pop = 10
    ),
    land_areas_wide = tibble::tibble(
      year = 1950L,
      area_code = 203L,
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
  # Scoped to the explicit fold. WHEP now models the reporting members of
  # bucket 999 in their own right (#459), so there is no Rest-of-World fold
  # by default; what this pins is the fold behaviour itself, which still has
  # to work for anyone reproducing a published-before number.
  withr::local_options(whep.unfold_rest_of_world = "none")
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

test_that("a promoted member's pre-1850 trade resolves instead of dropping", {
  # THE OTHER SIDE OF THE TEST ABOVE, under the default. The 64 rows the fold
  # drops are dropped because `ROW-1850-2025` starts in 1850 and is an
  # aggregate, which `.add_polity_columns_dt()` refuses to extend -- rightly, a
  # figure for 1830 Guadeloupe must not be booked to a bucket that did not
  # exist. Once Guadeloupe carries `GLP-1816-2025` (whep#717) the year is
  # inside a real period of its own and the row resolves.
  #
  # This is the one place the identity change moves a published quantity, and
  # the direction is recovery: the historical trade feed goes from
  # 18,453,716,816 t to 18,455,438,816 t (+0.0093%), all of it 64 pre-1850 rows
  # of Guadeloupe (87) and Martinique (135).
  resolved <- whep:::.resolve_hist_trade_polities(data.table::data.table(
    iso3c = c("GLP", "MTQ", "GLP"),
    year = c(1830L, 1830L, 1900L),
    value = 1
  ))

  expect_equal(
    resolved$polity_code,
    c(
      "GLP-1816-2025",
      "MTQ-1816-2025",
      "GLP-1816-2025"
    )
  )
  expect_equal(resolved$area_code, c(87L, 135L, 87L))
  # And a year before even the member's own period still drops, so this is not
  # a licence to back-fill: `GLP-1816-2025` starts in 1816.
  early <- whep:::.resolve_hist_trade_polities(data.table::data.table(
    iso3c = "GLP",
    year = 1700L,
    value = 1
  ))
  expect_equal(early$polity_code, "GLP-1816-2025")
  expect_equal(early$area_code, 87L)
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

test_that("an ISO3 naming two areas resolves to the canonical one", {
  # Issue whep#719, the same defect as whep#586 and whep#718. The ISO3 bridge
  # used to be built here by taking the first row of each ISO3, and the area
  # lookup orders by `area_code`, so `ETH` entered as the LOWEST code: 62, the
  # "Ethiopia PDR" entity dissolved in 1993, rather than 238, plain
  # "Ethiopia". Two things followed. The label came out "Ethiopia PDR" on an
  # `area_code` whose own name is "Ethiopia", in every year including 2015;
  # and because the polity resolution is year-aware on the code it is handed,
  # a 2015 row resolved to `ETH-1952-1993` -- a polity that had ended 22
  # years earlier.
  resolved <- whep:::.resolve_hist_trade_polities(data.table::data.table(
    iso3c = c("ETH", "ETH"),
    year = c(1980L, 2015L),
    value = 1
  ))

  expect_equal(resolved$area_code, c(238L, 238L))
  expect_equal(resolved$polity_code, c("ETH-1952-1993", "ETH-1993-2025"))
  expect_false(any(resolved$area == "Ethiopia PDR"))
})

test_that("hist trade gives one area label per area_code and year", {
  # The invariant behind whep#719, over EVERY ISO3 the crosswalk knows rather
  # than the two that were measured. `area_code` here is the aggregation
  # bucket, so the label has to be a property of the bucket -- the rule
  # `.aggregate_to_polities()` and `.read_crop_residues()` already follow.
  # Carrying it in from the member row instead gave bucket 206 two labels in
  # the same year, "Sudan (former)" from SDN and "South Sudan" from SSD, which
  # is the vocabulary split that dropped 702,166 rows in whep#382.
  lookup <- whep:::.current_area_lookup(include_unmapped = FALSE)
  iso3 <- sort(unique(stats::na.omit(lookup$area_iso3c)))
  resolved <- whep:::.resolve_hist_trade_polities(data.table::data.table(
    expand.grid(
      iso3c = iso3,
      year = c(1900L, 1950L, 1980L, 2015L),
      stringsAsFactors = FALSE
    ),
    value = 1
  ))
  resolved <- resolved[!is.na(resolved$area_code), ]

  labels_per_key <- resolved |>
    dplyr::summarise(
      n_labels = dplyr::n_distinct(.data$area),
      .by = c("area_code", "year")
    ) |>
    dplyr::filter(.data$n_labels > 1L)

  expect_equal(labels_per_key$area_code, integer(0))
})

test_that("hist trade still returns the area column with nothing to label", {
  # The label now arrives by an update join on the resolved bucket, so the
  # branch where NO row resolves has to keep emitting `area` rather than drop
  # the column the caller's `keep` set names.
  none <- whep:::.resolve_hist_trade_polities(data.table::data.table(
    iso3c = c("BEL-LUX", "ZZZ"),
    year = c(1900L, 1900L),
    value = 1
  ))
  empty <- whep:::.resolve_hist_trade_polities(data.table::data.table(
    iso3c = character(0),
    year = integer(0),
    value = numeric(0)
  ))

  expect_true(all(is.na(none$area)))
  expect_true("area" %in% names(none))
  expect_true("area" %in% names(empty))
  expect_equal(nrow(empty), 0L)
})

test_that(".read_gdp_pop renames only the year column", {
  # whep#721. This reader used to relabel the pin's `area` into the polity-name
  # vocabulary for a name-keyed proxy join. That join is gone -- commit 2210d05d
  # keyed `.fill_with_proxies()` on the reporting bucket -- so the relabelling
  # rewrote a column no consumer reads. The reader now hands the pin over as
  # published, with `Year` renamed to `year` and nothing else touched.
  pin <- data.table::data.table(
    Year = rep(2000L, 4L),
    area = c("Lao", "Republic of Korea", "Albania", "Spain"),
    area_code = c("LAO", "KOR", "ALB", "ESP"),
    pop = 1:4
  )
  testthat::local_mocked_bindings(
    .read_input = function(pin_alias, years = NULL, year_col = NULL) {
      data.table::copy(pin)
    }
  )

  result <- whep:::.read_gdp_pop(years = 2000L)

  expect_identical(result$area, pin$area)
  expect_identical(names(result), c("year", "area", "area_code", "pop"))
  expect_identical(result$pop, pin$pop)
})

test_that(".fill_with_proxies ignores the population pin's area label", {
  # The guard that makes the relabelling's removal safe: the label is inert, so
  # garbling it must not move a filled number. If a future change keys a proxy
  # on the name again, this fails instead of silently going unfilled.
  frame <- tibble::tibble(
    year = 1950:1953,
    area = "Albania (1913-2025)",
    area_code = 3L,
    item_cbs = "Wheat and products",
    item_cbs_code = 2511L,
    food = c(100, NA, NA, NA),
    other_uses = NA_real_,
    feed = NA_real_,
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
    area_code = 3L,
    Cropland = 1,
    Pasture = 1,
    agriland = 2
  )

  canonical <- whep:::.fill_with_proxies(frame, gdp_pop, land_wide)
  garbled <- whep:::.fill_with_proxies(
    frame,
    gdp_pop |> dplyr::mutate(area = "not a polity name"),
    land_wide
  )

  expect_equal(canonical$food, c(100, 110, 120, 130))
  expect_equal(garbled$food, canonical$food)
  expect_equal(garbled$pop, canonical$pop)
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

# -- .cbs_fix_final_balance (issue #162) --------------------------------------

test_that(".cbs_fix_final_balance clamps DS then export, no negatives", {
  dt <- data.table::data.table(
    production = c(0, 100, 50),
    import = c(10, 5, 5),
    stock_variation = c(20, 0, 0),
    domestic_supply = c(-5, -30, 40),
    export = c(0, 10, 15),
    balance = c(-3, -2, 1)
  )

  result <- whep:::.cbs_fix_final_balance(dt)

  # Domestic supply is clamped at 0 before the export fix reads it.
  expect_true(all(result$domestic_supply >= 0))
  # No negative exports survive.
  expect_true(all(result$export >= 0))
  # Row 1: 0 + 10 - 20 - 0 = -10 -> clamped to 0.
  expect_equal(result$export[1], 0)
  # Row 2: reads clamped DS (0), 100 + 5 - 0 - 0 = 105 (not 135 pre-clamp).
  expect_equal(result$export[2], 105)
  # Row 3: balance >= 0, export left untouched.
  expect_equal(result$export[3], 15)
})


# -- FBS scaling ratio bounds (issue #161) ------------------------------------

test_that(".select_best_source clamps extreme FBS scaling ratio", {
  input <- tibble::tribble(
    ~area, ~area_code, ~item_cbs, ~item_cbs_code, ~element, ~year,
    ~value, ~source, ~unit,
    "Spain", 203L, "Wheat", 2511L, "food", 2010L,
    10, "FAOSTAT_FBS_Old", "tonnes",
    "Spain", 203L, "Wheat", 2511L, "food", 2010L,
    1000, "FAOSTAT_FBS_New", "tonnes",
    "Spain", 203L, "Wheat", 2511L, "food", 2011L,
    10, "FAOSTAT_FBS_Old", "tonnes",
    "Spain", 203L, "Wheat", 2511L, "food", 2011L,
    1000, "FAOSTAT_FBS_New", "tonnes",
    "Spain", 203L, "Wheat", 2511L, "food", 2005L,
    100, "FAOSTAT_FBS_Old", "tonnes"
  )

  result <- whep:::.select_best_source(input)

  val_2005 <- result |>
    dplyr::filter(year == 2005) |>
    dplyr::pull(value)
  # Raw ratio is 100x; clamp caps it at 5x -> 100 * 5 = 500, not 10000.
  expect_equal(val_2005, 500)

  src_2005 <- result |>
    dplyr::filter(year == 2005) |>
    dplyr::pull(source)
  expect_equal(src_2005, "FAOSTAT_FBS_Old_scaled")
})

test_that(".select_best_source leaves FBS unscaled when overlap is thin", {
  input <- tibble::tribble(
    ~area, ~area_code, ~item_cbs, ~item_cbs_code, ~element, ~year,
    ~value, ~source, ~unit,
    "Spain", 203L, "Wheat", 2511L, "food", 2010L,
    10, "FAOSTAT_FBS_Old", "tonnes",
    "Spain", 203L, "Wheat", 2511L, "food", 2010L,
    1000, "FAOSTAT_FBS_New", "tonnes",
    "Spain", 203L, "Wheat", 2511L, "food", 2005L,
    100, "FAOSTAT_FBS_Old", "tonnes"
  )

  result <- whep:::.select_best_source(input)

  val_2005 <- result |>
    dplyr::filter(year == 2005) |>
    dplyr::pull(value)
  # A single overlap year is not extrapolated: value stays unscaled.
  expect_equal(val_2005, 100)

  src_2005 <- result |>
    dplyr::filter(year == 2005) |>
    dplyr::pull(source)
  expect_equal(src_2005, "FAOSTAT_FBS_Old")
})

# whep#709: `area` is the periodized polity name and it was a KEY in the
# pre-1962 extension. The public `historical_data` argument reaches that key --
# `.prepare_historical_cbs()` names its rows from the crosswalk's static
# `area_name` while `.select_best_source()` stamps the periodized polity name,
# and for 97 of the 262 codes in that lookup the static name is not any of the
# code's polity names. Two labels for one code, and the whep#563 shape follows.

.two_label_frame <- function() {
  dplyr::bind_rows(
    tibble::tribble(
      ~year, ~area,                 ~area_code, ~item_cbs,            ~item_cbs_code, ~element,     ~value, ~source,
      1950L, "Algeria (1919-1962)", 4L,         "Wheat and products", 2511L,          "production", 100,    "FAOSTAT_prod",
      1955L, "Algeria (1919-1962)", 4L,         "Wheat and products", 2511L,          "production", 200,    "FAOSTAT_prod"
    ),
    tibble::tribble(
      ~year, ~area,     ~area_code, ~item_cbs,            ~item_cbs_code, ~element,     ~value, ~source,
      1950L, "Algeria", 4L,         "Wheat and products", 2511L,          "production", 140,    "historical_test",
      1955L, "Algeria", 4L,         "Wheat and products", 2511L,          "production", 240,    "historical_test"
    )
  )
}

.algeria_hist_inputs <- function() {
  list(
    primary_cbs_area = tibble::tibble(
      year = integer(),
      area = character(),
      area_code = integer(),
      item_cbs = character(),
      item_cbs_code = integer(),
      area_ha = numeric()
    ),
    gdp_pop = tibble::tibble(
      year = 1950:1960,
      area = "Algeria",
      area_code = "DZA",
      pop = 1:11
    ),
    # Keyed on the reporting bucket, matching what `.read_land_areas_wide()`
    # emits since whep#698 re-keyed it off the label. `.two_label_frame()`'s
    # code is Algeria's 4, and the point of this fixture is that its TWO labels
    # must not split that one code -- so the land proxy has to reach it by code.
    land_areas_wide = tibble::tibble(
      year = 1950:1960,
      area_code = 4L,
      Cropland = 1,
      Pasture = 0,
      agriland = 1
    )
  )
}

test_that("two labels for one code collapse to one observation", {
  # Before whep#709 `.collapse_cbs_observations()` keyed on `area`, so the two
  # rows were two territories: nothing collapsed, both survived, and
  # `.format_cbs_output()` summed them into 240 t for a cell whose answer is
  # 140 t.
  result <- whep:::.collapse_cbs_observations(.two_label_frame())

  expect_equal(nrow(result), 2L)
  expect_equal(unique(result$area), "Algeria (1919-1962)")
  expect_equal(
    result |> dplyr::filter(.data$year == 1950L) |> dplyr::pull(.data$value),
    140
  )
  # Pre-1961 a `historical_` source outranks FAOSTAT (`.cbs_source_rank()`), so
  # reconciling on the code picks it rather than keeping both.
  expect_equal(
    result |> dplyr::filter(.data$year == 1950L) |> dplyr::pull(.data$source),
    "historical_test"
  )
})

test_that("a second area label does not double the historical skeleton", {
  # The whep#563 shape, and the one that multiplies rows rather than only
  # mislabelling them: `.cbs_complete_year_nesting_dt()` crosses its id_cols
  # with the year axis, so a code with two labels used to get two full year
  # skeletons -- 154 rows over 77 keys, every value counted twice.
  result <- whep:::.cbs_extend_historical(
    .two_label_frame(),
    .algeria_hist_inputs(),
    1950:1961
  )

  keys <- result |>
    dplyr::distinct(
      .data$year,
      .data$area_code,
      .data$item_cbs_code,
      .data$element
    )
  expect_equal(nrow(result), nrow(keys))

  production <- result |>
    dplyr::filter(.data$element == "production", .data$year == 1950L)
  expect_equal(nrow(production), 1L)
  expect_equal(production$value, 140)
})

test_that("the historical extension keeps one area label per code", {
  # The invariant a value comparison cannot see (whep#563). It has to hold on
  # the frame that leaves the extension, not only on the lookup that built it.
  result <- whep:::.cbs_extend_historical(
    .two_label_frame(),
    .algeria_hist_inputs(),
    1950:1961
  )

  per_code <- result |>
    dplyr::summarise(
      n_labels = dplyr::n_distinct(.data$area),
      .by = "area_code"
    )

  expect_equal(per_code$n_labels, 1L)
  expect_equal(unique(result$area), "Algeria (1919-1962)")
})

# -- Destiny-share interpolation (whep#691) ------------------------------------

# One code, two labels: the balance side carries the current FAOSTAT name and
# the destiny side the periodized one. Every real caller hands
# `.interpolate_destiny_shares()` two filters of ONE frame, so today the labels
# agree by construction -- but that is the caller's invariant, not this
# function's, and an unmatched key here is a dropped row, not an error.
.two_label_destiny_frames <- function() {
  balance <- tibble::tribble(
    ~year, ~area, ~area_code, ~item_cbs, ~item_cbs_code, ~element, ~value,
    2000L, "Algeria", 4L, "Wheat", 2511L, "domestic_supply", 100,
    2001L, "Algeria", 4L, "Wheat", 2511L, "domestic_supply", 200,
    2002L, "Algeria", 4L, "Wheat", 2511L, "domestic_supply", 300
  ) |>
    dplyr::mutate(elem_cat = "balance", source = "FAOSTAT_FBS_New")

  destiny <- tibble::tribble(
    ~year, ~area, ~area_code, ~item_cbs, ~item_cbs_code, ~element, ~value,
    2000L, "Algeria (1919-1962)", 4L, "Wheat", 2511L, "food", 60,
    2000L, "Algeria (1919-1962)", 4L, "Wheat", 2511L, "feed", 40
  ) |>
    dplyr::mutate(
      elem_cat = "destiny",
      source = "FAOSTAT_FBS_New",
      dest_share = value / 100
    )

  list(balance = balance, destiny = destiny)
}

test_that("a second area label does not drop a destiny-share skeleton", {
  # Before whep#691 the skeleton join keyed on `area` as well as `area_code`,
  # so the two labels were two territories: no target year matched, the
  # skeleton collapsed to the single observed year and 2001-2002 lost their
  # shares entirely.
  frames <- .two_label_destiny_frames()

  result <- whep:::.interpolate_destiny_shares(
    frames$balance,
    frames$destiny
  )

  expect_equal(nrow(result), 6L)
  expect_setequal(unique(result$year), 2000:2002)
  expect_equal(
    result |>
      dplyr::filter(.data$element == "food") |>
      dplyr::arrange(.data$year) |>
      dplyr::pull(.data$dest_share),
    c(0.6, 0.6, 0.6)
  )
})

test_that("the destiny-share skeleton keeps one area label per code", {
  # The invariant a value comparison cannot see (whep#563): the label is
  # re-attached from the code once, so a disagreeing input label cannot leave
  # two territories behind.
  frames <- .two_label_destiny_frames()

  result <- whep:::.interpolate_destiny_shares(
    frames$balance,
    frames$destiny
  )

  per_code <- result |>
    dplyr::summarise(
      n_labels = dplyr::n_distinct(.data$area),
      .by = "area_code"
    )

  expect_equal(per_code$n_labels, 1L)
})

test_that("agreeing labels leave the destiny-share skeleton unchanged", {
  # What the real build hands it: one label per code on both sides. Dropping
  # the label from the key must move nothing here.
  frames <- .two_label_destiny_frames()
  frames$destiny$area <- "Algeria"

  result <- whep:::.interpolate_destiny_shares(
    frames$balance,
    frames$destiny
  )

  expect_equal(nrow(result), 6L)
  expect_equal(unique(result$area), "Algeria")
  expect_equal(
    result |>
      dplyr::filter(.data$year == 2002L) |>
      dplyr::arrange(.data$element) |>
      dplyr::pull(.data$dest_share),
    c(0.4, 0.6)
  )
})

# -- .read_historical_trade row order ------------------------------------------

# The pre-1961 trade source. Its two pins are read through arrow's
# multi-threaded scanner and the aggregation at the end of
# `.read_historical_trade()` emits groups in order of first appearance, so the
# table came back in a session-dependent order: 2 distinct orders over 7
# sessions on the real pins at 1950-1965, always the same 45,871 rows with the
# same values (whep#420). Feeding the same rows in two orders reproduces that
# here with no pin and no network.
.hist_trade_fixture <- function() {
  tibble::tribble(
    ~iso3, ~year, ~item_code, ~measurement, ~value,
    "ESP", 1950L, 15,         "1000 MT",    10,
    "ESP", 1950L, 16,         "1000 MT",    4,
    "FRA", 1950L, 15,         "1000 MT",    7,
    "FRA", 1951L, 15,         "1000 MT",    8,
    "ESP", 1951L, 15,         "1000 MT",    11
  ) |>
    data.table::as.data.table()
}

test_that(".read_historical_trade row order does not depend on the read", {
  fixture <- .hist_trade_fixture()

  read_in_order <- function(rows) {
    testthat::local_mocked_bindings(
      .read_input = function(pin_alias, years = NULL, year_col = NULL) {
        data.table::copy(rows)
      }
    )
    whep:::.read_historical_trade() |>
      as.data.frame()
  }

  forward <- read_in_order(fixture)
  reversed <- read_in_order(fixture[rev(seq_len(nrow(fixture)))])

  expect_gt(nrow(forward), 1L)
  expect_identical(forward, reversed)
})

# Issue whep#833, the other half. `.cbs_fill_destinies()` splits domestic
# supply with the area's own observed split, carried across the year axis, and
# falls back to the world average split for a key that has no split anywhere
# in the frame. On a truncated axis a key whose only observation lies outside the
# window takes the fallback instead of its own answer, which hands it a
# `processing` destiny it never reported -- and `.cbs_second_processed_round()`
# turns that into oil and cake rows the full-range build does not have.
.destiny_axis_fixture <- function(years) {
  destinies <- c("food", "feed", "seed", "other_uses", "processing")
  anchor <- tidyr::expand_grid(year = years, element = destinies) |>
    dplyr::mutate(
      area = "Anchorland",
      area_code = 991L,
      item_cbs = "Coconuts - Incl Copra",
      item_cbs_code = 2560L,
      value = dplyr::if_else(year == 1995L & element == "food", 1000, 0),
      source = "FBS"
    )
  # A second area with a stable half-food, half-crush split, so the world
  # average the fallback reads is not the anchor area's own.
  world <- tidyr::expand_grid(year = years, element = destinies) |>
    dplyr::mutate(
      area = "Worldland",
      area_code = 992L,
      item_cbs = "Coconuts - Incl Copra",
      item_cbs_code = 2560L,
      value = dplyr::if_else(element %in% c("food", "processing"), 500, 0),
      source = "FBS"
    )
  supply <- tidyr::expand_grid(
    year = years,
    tibble::tribble(
      ~area, ~area_code,
      "Anchorland", 991L,
      "Worldland", 992L
    )
  ) |>
    dplyr::mutate(
      item_cbs = "Coconuts - Incl Copra",
      item_cbs_code = 2560L,
      element = "domestic_supply",
      value = 1000,
      source = "FBS"
    )
  dplyr::bind_rows(anchor, world, supply)
}

.destiny_axis_processing <- function(years) {
  whep:::.cbs_fill_destinies(.destiny_axis_fixture(years)) |>
    tibble::as_tibble() |>
    dplyr::filter(
      area_code == 991L,
      year == 2010L,
      element == "processing"
    ) |>
    dplyr::pull(value)
}

test_that(".cbs_fill_destinies carries one observed split across the axis", {
  expect_length(.destiny_axis_processing(1990:2015), 0L)
})

test_that(".cbs_fill_destinies invents a crush off-anchor (whep#833)", {
  # The defect, pinned offline: the same area and year gets no `processing` at
  # all on an axis holding its 1995 split, and half its domestic supply crushed
  # on one that does not. Fixing #833 makes both branches emit nothing, and
  # this expectation must then be replaced by the empty one above.
  expect_equal(.destiny_axis_processing(2005:2015), 500)
})


# -- trade units (#865) --------------------------------------------------------

# `.read_fao_trade()` carries `unit`, and the FAOSTAT trade record denominates
# live animals in `An` / `1000 An` and bees in `No`, not in tonnes. This fixture
# holds one row of each unit the real pin uses, on real crosswalk keys: trade
# item 15 -> CBS 2511 (wheat, `t`), 1034 -> 1049 (pigs, `An`), 1057 -> 1053
# (broilers, `1000 An`), 1181 -> 1181 (bees, `No`).
.trade_unit_rows <- function() {
  tibble::tribble(
    ~year, ~area_code, ~unit, ~element, ~item_trade, ~item_code_trade, ~value,
    2010, 200L, "t", "import", "Wheat", 15, 400617,
    2010, 200L, "An", "import", "Swine / pigs", 1034, 290549,
    2010, 200L, "1000 An", "import", "Chickens", 1057, 1500,
    2010, 200L, "No", "import", "Bees", 1181, 4645
  ) |>
    data.table::as.data.table()
}

test_that(".aggregate_fao_trade_to_cbs keeps the unit of what it sums", {
  # The defect: the aggregation dropped `unit` and summed across it, so head
  # counts landed in the same tonnes-denominated `value` column as mass. On the
  # real pin that is 135.3 M head/number against 2.85 Gt at 2010 (#865).
  result <- whep:::.aggregate_fao_trade_to_cbs(.trade_unit_rows())

  expect_true("unit" %in% names(result))
  expect_setequal(result$unit, c("t", "An", "An", "No"))
})

test_that(".aggregate_fao_trade_to_cbs rescales 1000 An to An", {
  # `.normalise_units()` rescales "1000 tonnes" but not "1000 An", so poultry
  # trade arrived a thousandfold low in its own unit. Item 1150 even switches
  # between the two labels across years on the real pin, which would put a
  # 1000x step in one series.
  result <- whep:::.aggregate_fao_trade_to_cbs(.trade_unit_rows())
  broilers <- result[result$item_cbs_code == 1053, ]

  expect_equal(broilers$unit, "An")
  expect_equal(broilers$value, 1500 * 1000)
})

test_that(".mass_only_trade keeps mass and drops the head counts", {
  agg <- whep:::.aggregate_fao_trade_to_cbs(.trade_unit_rows())

  expect_warning(
    kept <- whep:::.mass_only_trade(agg, "faostat-trade-totals"),
    "not denominated in mass"
  )
  expect_false("unit" %in% names(kept))
  expect_equal(kept$item_cbs_code, 2511)
  expect_equal(sum(kept$value), 400617)
})

test_that(".mass_only_trade accepts either mass label and stays keyed", {
  # FAOSTAT trade reports "t" and FishStat is normalised to "tonnes", so both
  # are mass; collapsing them must not leave two rows on one key.
  both <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~element, ~unit, ~value,
    2010, 200L, 2511, "import", "t", 10,
    2010, 200L, 2511, "import", "tonnes", 5
  ) |>
    data.table::as.data.table()

  kept <- whep:::.mass_only_trade(both, "test")

  expect_equal(nrow(kept), 1L)
  expect_equal(kept$value, 15)
})

test_that(".mass_only_trade is silent when everything is mass", {
  mass <- whep:::.aggregate_fao_trade_to_cbs(.trade_unit_rows()[unit == "t"])

  expect_no_warning(kept <- whep:::.mass_only_trade(mass, "test"))
  expect_equal(nrow(kept), 1L)
})

test_that(".mass_only_trade tolerates an empty record", {
  empty <- whep:::.aggregate_fao_trade_to_cbs(.trade_unit_rows()[0L])

  expect_no_warning(kept <- whep:::.mass_only_trade(empty, "test"))
  expect_equal(nrow(kept), 0L)
  expect_false("unit" %in% names(kept))
})

test_that(".mass_only_trade aborts on a frame with no unit", {
  # A trade aggregate that lost its unit is exactly the shape of #865, so this
  # is an abort and not a silent pass-through.
  unitless <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~element, ~value,
    2010, 200L, 2511, "import", 10
  ) |>
    data.table::as.data.table()

  expect_error(
    whep:::.mass_only_trade(unitless, "faostat-trade-totals"),
    "has no"
  )
})


# -- trade recovery must not duplicate a territory (#884) ----------------------

# Belgium's shape: FishStat keys the territory 255 from 1976, while every
# FAOSTAT product keys it 15 (Belgium-Luxembourg) until 1999. The CBS here
# covers area 255 in 1990 on purpose -- that is what makes the restriction
# falsifiable, because the area-label join alone would otherwise drop the row
# and the test would pass for the wrong reason. It is also the state #867's
# proposal to create absent area-years would produce.
.belgium_cbs <- function() {
  tibble::tribble(
    ~year, ~area, ~area_code, ~item_cbs, ~item_cbs_code, ~element, ~value, ~source,
    1990, "Belgium", 255L, "Wheat and products", 2511, "production", 0, "FAOSTAT_prod",
    2010, "Belgium", 255L, "Wheat and products", 2511, "production", 0, "FAOSTAT_prod"
  )
}

.belgium_trade <- function() {
  tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~element, ~value,
    1990, 255L, 2761, "import", 265281,
    2010, 255L, 2761, "import", 300000
  )
}

test_that("trade recovery creates no row for an off-window area-year", {
  # 1990: FishStat says 255, the CBS vocabulary says 15, so a created 255 row
  # would sit beside the Belgium-Luxembourg row covering the same territory in
  # the same year -- a duplicated territory, not a filled gap (whep#884).
  result <- whep:::.cbs_trade_recovery_rows(
    .belgium_cbs(),
    .belgium_trade(),
    years = c(1990, 2010)
  )

  expect_equal(result$year, 2010)
  expect_false(1990 %in% result$year)
})

test_that("binding an off-window recovered row aborts", {
  # The filter above is the policy; this is the guard that makes lifting it
  # impossible to do silently.
  recovered <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~element, ~area, ~item_cbs, ~value, ~source,
    1990, 255L, 2761, "import", "Belgium", "Freshwater Fish", 265281, "FAOSTAT_trade"
  )

  expect_error(
    whep:::.cbs_bind_recovered(.belgium_cbs(), recovered),
    class = "whep_error_off_window_area_year"
  )
})
