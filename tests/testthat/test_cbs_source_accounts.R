test_that("FAO source account components retain units and observation flags", {
  raw <- tibble::tribble(
    ~`Area Code`, ~Area, ~`Item Code`, ~Item, ~Element, ~Unit,
    ~Year, ~Value, ~Flag,
    203L, "Spain", 2563L, "Olives", "Stock Variation", "1000 t",
    2015L, 0, "I",
    203L, "Spain", 2563L, "Olives", "Losses", "1000 t",
    2015L, 402, "I",
    203L, "Spain", 2563L, "Olives", "Residuals", "1000 t",
    2015L, -3113, "I",
    203L, "Spain", 2563L, "Olives", "Tourist consumption", "1000 t",
    2015L, 1, "I"
  )
  testthat::local_mocked_bindings(
    .read_input = function(...) data.table::as.data.table(raw),
    .package = "whep"
  )
  actual <- whep:::.extract_fao("faostat-fbs-new", keep_members = TRUE)
  expect_setequal(
    actual$element,
    c("stock_variation", "losses", "residuals", "tourist_consumption")
  )
  expect_equal(actual$value[actual$element == "stock_variation"], 0)
  expect_equal(actual$value[actual$element == "residuals"], -3113000)
  expect_true(all(actual$unit == "tonnes"))
  expect_true(all(actual$fao_flag == "I"))
  expect_true(all(actual$reporting_polity_code == "ESP-1800-2025"))
})

test_that("stock addition enters missing-production reconstruction positively", {
  # production + import - export - stock_variation = domestic_supply, with
  # the stock change an addition: production = 100 - 10 + 5 + 2 = 97.
  accounts <- tibble::tribble(
    ~production, ~import, ~export, ~stock_variation, ~domestic_supply,
    ~food, ~feed, ~seed, ~processing, ~processing_primary, ~other_uses,
    NA_real_, 10, 5, 2, 100, 60, 30, 5, 5, 0, 0
  )
  actual <- whep:::.reestimate_domestic_supply(accounts)
  expect_equal(actual$production, 97)
  expect_equal(actual$stock_variation, 2)
})

test_that("source ledger precedes unit conversion and retains missingness", {
  raw <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~element, ~unit, ~value, ~fao_flag,
    2015L, 203L, 2563L, "Stock Variation", "1000 t", 0, "I",
    2020L, 203L, 2563L, "Stock Variation", "1000 t", NA_real_, "I",
    2015L, 203L, 2563L, "Residuals", "1000 t", -3113, "I"
  )
  ledger <- whep:::.cbs_input_account_records(raw, "faostat-fbs-new")
  expect_equal(nrow(ledger), 3L)
  expect_equal(ledger$source_value[ledger$source_element == "Residuals"], -3113)
  expect_true(all(ledger$source_unit == "1000 t"))
  expect_true(all(ledger$reporting_polity_code == "ESP-1800-2025"))
  expect_equal(sum(is.na(ledger$source_value)), 1L)
  expect_false(anyNA(ledger$source_version))
})

test_that("public account restoration keeps losses and source stock separate", {
  model <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~element, ~value, ~source, ~fao_flag,
    2015L, 203L, 2563L, "production", 100, "model", NA_character_,
    2015L, 203L, 2563L, "import", 10, "model", NA_character_,
    2015L, 203L, 2563L, "export", 5, "model", NA_character_,
    2015L, 203L, 2563L, "stock_variation", 5, "model", NA_character_,
    2015L, 203L, 2563L, "food", 100, "model", NA_character_,
    2015L, 203L, 2563L, "domestic_supply", 100, "model", NA_character_
  )
  observations <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~element, ~value, ~source, ~fao_flag,
    2015L, 203L, 2563L, "stock_variation", 0, "FAOSTAT_FBS_New", "I",
    2015L, 203L, 2563L, "losses", 4, "FAOSTAT_FBS_New", "I",
    2015L, 203L, 2563L, "residuals", -2, "FAOSTAT_FBS_New", "I"
  )
  model$reporting_polity_code <- "ESP-1800-2025"
  observations$reporting_polity_code <- "ESP-1800-2025"
  actual <- whep:::.cbs_restore_accounts(model, observations)
  expect_equal(actual$value[actual$element == "stock_variation"], 0)
  expect_equal(actual$value[actual$element == "losses"], 4)
  expect_equal(actual$value[actual$element == "residuals"], -2)
  expect_equal(actual$value[actual$element == "balance_discrepancy"], 3)
  # Total physical use: food plus losses; the residual is not a use.
  expect_equal(actual$value[actual$element == "domestic_supply"], 104)
  expect_true(all(actual$method_cbs_account == "source"))
  expect_equal(actual$value[actual$element == "food"], 100)
})

test_that("physical balance and statistical closure are different diagnostics", {
  account <- tibble::tibble(
    year = 2020L,
    area_code = 203L,
    item_cbs_code = 2511L,
    production = 100,
    import = 0,
    stock_withdrawal = 0,
    export = 0,
    food = 100,
    feed = 0,
    seed = 0,
    processing = 0,
    other_uses = 0,
    stock_addition = 0,
    losses = 10,
    tourist_consumption = 5,
    residuals = -15,
    balance_discrepancy = 0
  )
  actual <- whep::check_supply_use_balance(account)
  expect_false(actual$balanced)
  expect_equal(actual$use, 115)
  expect_true(actual$accounting_balanced)
  account$food <- 85
  account$residuals <- 0
  expect_true(whep::check_supply_use_balance(account)$balanced)
})

test_that("old FAO withdrawal-positive stocks normalize without losing raw sign", {
  raw <- tibble::tribble(
    ~`Area Code`, ~Area, ~`Item Code`, ~Item, ~Element, ~Unit,
    ~Year, ~Value, ~Flag,
    203L, "Spain", 2511L, "Wheat and products", "Stock Variation", "1000 tonnes",
    2000L, -1157, "S"
  )
  testthat::local_mocked_bindings(
    .read_input = function(...) data.table::as.data.table(raw),
    .package = "whep"
  )
  actual <- whep:::.extract_fao("faostat-fbs-old", keep_members = TRUE)
  expect_equal(actual$value, 1157000)
  expect_equal(attr(actual, "source_accounts")$source_value, -1157)
})

test_that("old cached extracts cannot bypass the stock-sign/source contract", {
  old <- tibble::tibble()
  attr(old, "bucket_members") <- tibble::tibble()
  extracts <- rlang::rep_named(
    c("fbs_new", "fbs_old", "cbs_crops", "cbs_animals"),
    list(old)
  )
  expect_false(whep:::.cbs_reuse_extracts(extracts))
})

test_that("public account policies do not mutate supplied data or its ledger", {
  raw <- readr::read_csv(
    testthat::test_path("fixtures", "fbs_account_spain_2015_2020.csv"),
    show_col_types = FALSE
  )
  testthat::local_mocked_bindings(
    .read_input = function(...) data.table::as.data.table(raw),
    .package = "whep"
  )
  observations <- whep:::.extract_fao("faostat-fbs-new", keep_members = TRUE) |>
    tibble::as_tibble() |>
    dplyr::mutate(source = "FAOSTAT_FBS_New")
  model <- whep:::.cbs_impute_trade(observations)
  attr(model, "source_accounts") <- observations
  frozen <- serialize(model, NULL)
  first <- whep::build_commodity_balances(
    start_year = 2015,
    end_year = 2020,
    .fixed_data = model,
    method_cbs_account = "balance_residual"
  )
  expect_identical(serialize(model, NULL), frozen)
  repeated <- whep::build_commodity_balances(
    start_year = 2015,
    end_year = 2020,
    .fixed_data = model,
    method_cbs_account = "balance_residual"
  )
  expect_identical(serialize(first, NULL), serialize(repeated, NULL))
  expect_identical(serialize(model, NULL), frozen)
})

test_that("an unobserved stock stays NA while the gap stays visible", {
  model <- tibble::tribble(
    ~year, ~item_cbs_code, ~element, ~value, ~source,
    2015L, 2542L, "production", 100, "model",
    2015L, 2542L, "import", 10, "model",
    2015L, 2542L, "export", 5, "model",
    2015L, 2542L, "stock_variation", 15, "model",
    2015L, 2542L, "food", 90, "model",
    2015L, 2542L, "domestic_supply", 90, "model"
  ) |>
    dplyr::mutate(area_code = 110L, reporting_polity_code = "JPN-1952-2025")
  observations <- model[0, ]
  actual <- whep:::.cbs_restore_accounts(model, observations)
  stock <- dplyr::filter(actual, element == "stock_variation")
  expect_true(is.na(stock$value))
  expect_equal(stock$source, "unobserved_stock")
  gap <- dplyr::filter(actual, element == "balance_discrepancy")
  expect_equal(gap$value, 15)
  expect_equal(gap$source, "WHEP_account_reconciliation")
  expect_equal(actual$value[actual$element == "domestic_supply"], 90)
})

test_that("source accounts outside the balance keys are reported, not grown", {
  model <- tibble::tribble(
    ~year, ~item_cbs_code, ~element, ~value, ~source,
    2015L, 2513L, "production", 10, "model",
    2015L, 2513L, "food", 10, "model"
  ) |>
    dplyr::mutate(area_code = 45L, reporting_polity_code = "COM-1975-2025")
  observations <- tibble::tribble(
    ~year, ~item_cbs_code, ~element, ~value, ~source, ~reporting_polity_code,
    2015L, 2513L, "losses", 1, "FAOSTAT_FBS_New", "COM-1975-2025",
    2015L, 2515L, "residuals", -3, "FAOSTAT_FBS_New", "COM-1975-2025"
  )
  actual <- suppressMessages(
    whep:::.cbs_restore_accounts(model, observations)
  )
  expect_equal(nrow(dplyr::count(actual, year, item_cbs_code, element)), 6L)
  expect_false(anyNA(actual$area_code))
  expect_false(2515L %in% actual$item_cbs_code)
  unmatched <- attr(actual, "unmatched_source_accounts")
  expect_equal(unmatched$item_cbs_code, 2515L)
  expect_equal(unmatched$value, -3)
  expect_message(
    whep:::.cbs_restore_accounts(model, observations),
    "outside the calibrated balance keys"
  )
})

test_that("the balance_residual alternative leaves the model untouched", {
  model <- tibble::tribble(
    ~year, ~item_cbs_code, ~element, ~value, ~source,
    2015L, 2563L, "production", 100, "model",
    2015L, 2563L, "stock_variation", -3, "model"
  ) |>
    dplyr::mutate(area_code = 203L, reporting_polity_code = "ESP-1800-2025")
  actual <- whep:::.cbs_restore_accounts(
    model,
    model[0, ],
    "balance_residual"
  )
  expect_equal(dplyr::select(actual, -method_cbs_account), model)
  expect_true(all(actual$method_cbs_account == "balance_residual"))
})

test_that("account-only elements are the three the calibration never sees", {
  expect_setequal(
    whep:::.cbs_account_only_elements(),
    c("losses", "residuals", "tourist_consumption")
  )
  expect_setequal(
    whep:::.cbs_account_elements(),
    c("stock_variation", "losses", "residuals", "tourist_consumption")
  )
})

test_that("the published balance keeps the supplied stock and the residual", {
  raw <- readr::read_csv(
    testthat::test_path("fixtures", "fbs_account_spain_2015_2020.csv"),
    show_col_types = FALSE
  )
  testthat::local_mocked_bindings(
    .read_input = function(...) data.table::as.data.table(raw),
    .package = "whep"
  )
  observations <- whep:::.extract_fao("faostat-fbs-new", keep_members = TRUE) |>
    tibble::as_tibble() |>
    dplyr::mutate(source = "FAOSTAT_FBS_New")
  model <- whep:::.cbs_impute_trade(observations)
  attr(model, "source_accounts") <- observations
  olives <- function(method) {
    whep::build_commodity_balances(
      start_year = 2015,
      end_year = 2020,
      .fixed_data = model,
      method_cbs_account = method
    ) |>
      dplyr::filter(item_cbs_code == 2563, year == 2015) |>
      dplyr::select(element, value, source)
  }
  source <- suppressMessages(olives("source"))
  value <- function(data, el) data$value[data$element == el]
  # FAOSTAT's own Stock Variation is zero; the 3.1 Mt gap is its Residuals.
  expect_equal(value(source, "stock_variation"), 0)
  expect_equal(value(source, "losses"), 402000)
  expect_equal(value(source, "residuals"), -3113000)
  # Physical use: calibrated food 34 kt + processing 8,219 kt + losses.
  expect_equal(value(source, "domestic_supply"), 34000 + 8219000 + 402000)
  # What FAOSTAT's own rounding leaves, after its residual: 2 kt.
  expect_equal(value(source, "balance_discrepancy"), 2000)
  # The old contract booked the whole gap, losses included, as a stock draw:
  # 402 kt losses - 3,113 kt residual + 2 kt rounding = -2,709 kt.
  residual <- suppressMessages(olives("balance_residual"))
  expect_equal(value(residual, "stock_variation"), 402000 - 3113000 + 2000)
  expect_false(any(c("losses", "residuals") %in% residual$element))
})
