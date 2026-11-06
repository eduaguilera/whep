# FishStat net product weight at the live-weight CBS boundary (whep#1617).
#
# The fixture is the case the issue traced: Peru's 2017 exports of Marine
# Fish, Other (2764), 0 t in FAOSTAT's live-weight balance sheet and
# 1,253,750 t of net product weight in the legacy `fishstat-trade` pin.

.legacy_fishstat_pin <- function() {
  data.table::data.table(
    Year = 2017L,
    `Area Code` = 170L,
    `Item Code` = c(2764L, 2763L),
    Element = c("Export quantity", "Import quantity"),
    Value = c(1253750, 95467),
    Unit = "tonnes"
  )
}

.mock_fishstat_pin <- function(pin, env = parent.frame()) {
  testthat::local_mocked_bindings(
    .read_input = function(pin_alias, years = NULL, year_col = NULL) {
      data.table::copy(pin)
    },
    .package = "whep",
    .env = env
  )
}

# Peru 2017, item 2764, as FAOSTAT's balance sheet reports it: no export.
.peru_balance_zero_export <- function() {
  tibble::tribble(
    ~year, ~area, ~area_code, ~item_cbs, ~item_cbs_code, ~element, ~value, ~source,
    2017L, "Peru", 170L, "Marine Fish, Other", 2764, "production", 50000, "FAOSTAT_FBS_New",
    2017L, "Peru", 170L, "Marine Fish, Other", 2764, "food", 59000, "FAOSTAT_FBS_New",
    2017L, "Peru", 170L, "Marine Fish, Other", 2764, "import", 9000, "FAOSTAT_FBS_New",
    2017L, "Peru", 170L, "Marine Fish, Other", 2764, "export", 0, "FAOSTAT_FBS_New"
  )
}

.export_of <- function(balance) {
  balance |>
    dplyr::filter(.data$element == "export") |>
    dplyr::pull(value)
}

testthat::test_that("product-weight tonnes pass the unit gate but not the basis", {
  pin <- .legacy_fishstat_pin()
  .mock_fishstat_pin(pin)
  # The unit gate is blind to the weight basis: both are tonnes, so it keeps
  # every product-weight tonne. That is why the basis needs its own guard.
  as_mass <- whep:::.mass_only_trade(
    data.table::data.table(
      year = pin$Year,
      area_code = pin$`Area Code`,
      item_cbs_code = pin$`Item Code`,
      element = c("export", "import"),
      value = pin$Value,
      unit = pin$Unit
    ),
    "fishstat-trade"
  )
  testthat::expect_equal(sum(as_mass$value), sum(pin$Value))
  testthat::expect_message(
    out <- whep:::.read_fishstat_trade(years = 2017L),
    class = "whep_fishstat_withheld"
  )
  testthat::expect_null(out)
})

testthat::test_that("the withheld message names the product-weight tonnage", {
  .mock_fishstat_pin(.legacy_fishstat_pin())
  testthat::expect_message(
    whep:::.read_fishstat_trade(years = 2017L),
    "95,467 t of imports and 1,253,750 t of exports"
  )
})

testthat::test_that("a balance-sheet zero is not filled with product weight", {
  .mock_fishstat_pin(.legacy_fishstat_pin())
  testthat::expect_message(
    withheld <- whep:::.read_fishstat_trade(years = 2017L),
    class = "whep_fishstat_withheld"
  )
  kept <- whep:::.cbs_impute_trade(
    .peru_balance_zero_export(),
    fao_trade_cbs = withheld
  )
  testthat::expect_equal(.export_of(kept), 0)

  # The pre-whep#1617 behaviour, still selectable, fills the zero with the
  # pin's product weight tonne for tonne.
  testthat::expect_message(
    legacy <- whep:::.read_fishstat_trade(
      years = 2017L,
      fishstat_trade = "as_live_weight"
    ),
    class = "whep_fishstat_product_weight"
  )
  testthat::expect_message(
    filled <- whep:::.cbs_impute_trade(
      .peru_balance_zero_export(),
      fao_trade_cbs = legacy
    ),
    "1 key, 1253750 tonnes"
  )
  testthat::expect_equal(.export_of(filled), 1253750)
})

testthat::test_that("as_live_weight keeps the legacy rows and says what it is", {
  .mock_fishstat_pin(.legacy_fishstat_pin())
  testthat::expect_message(
    out <- whep:::.read_fishstat_trade(
      years = 2017L,
      fishstat_trade = "as_live_weight"
    ),
    "not a conversion"
  )
  out <- dplyr::arrange(tibble::as_tibble(out), .data$item_cbs_code)
  testthat::expect_equal(out$item_cbs_code, c(2763, 2764))
  testthat::expect_equal(out$element, c("import", "export"))
  testthat::expect_equal(out$value, c(95467, 1253750))
})

testthat::test_that("an empty year range withholds nothing and says nothing", {
  .mock_fishstat_pin(.legacy_fishstat_pin()[0])
  testthat::expect_no_message(
    out <- whep:::.read_fishstat_trade(years = 1970L)
  )
  testthat::expect_null(out)
})

testthat::test_that("a raw candidate still aborts under either method", {
  pin <- .legacy_fishstat_pin()
  pin[, `Weight Basis` := "net_product_weight"]
  .mock_fishstat_pin(pin)
  for (method in whep:::.fishstat_trade_choices()) {
    testthat::expect_error(
      whep:::.read_fishstat_trade(years = 2017L, fishstat_trade = method),
      class = "whep_fishstat_unconverted"
    )
  }
})

testthat::test_that("an unknown fishstat_trade method aborts", {
  testthat::expect_error(
    whep:::.read_fishstat_trade(fishstat_trade = "convert"),
    class = "rlang_error"
  )
  testthat::expect_error(
    whep:::.fishstat_weight_basis(
      data.table::data.table(element = "import", value = 1),
      "convert"
    ),
    class = "rlang_error"
  )
})

testthat::test_that("withhold is the default of the CBS build", {
  testthat::expect_identical(
    eval(formals(whep::build_commodity_balances)$fishstat_trade)[[1]],
    "withhold"
  )
  testthat::expect_identical(
    eval(formals(whep:::.cbs_read_inputs)$fishstat_trade)[[1]],
    "withhold"
  )
})

testthat::test_that("a supplied balance ignores fishstat_trade and says so", {
  fixed <- tibble::tribble(
    ~year, ~element,          ~value,
    1962L, "production",         100,
    1962L, "domestic_supply",    100,
    1962L, "food",               100
  ) |>
    dplyr::mutate(
      area = "United States",
      area_code = 231L,
      item_cbs = "Tobacco",
      item_cbs_code = 2671L,
      source = "historical_fill"
    )
  testthat::expect_warning(
    whep::build_commodity_balances(
      .fixed_data = fixed,
      fishstat_trade = "as_live_weight"
    ),
    "fishstat_trade.*ignored"
  )
  testthat::expect_no_warning(
    whep::build_commodity_balances(.fixed_data = fixed),
    message = "fishstat_trade"
  )
})
