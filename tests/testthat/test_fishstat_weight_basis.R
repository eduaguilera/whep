# FishStat net product weight at the live-weight CBS boundary (whep#1617).
#
# The fixture is the case the issue traced: Peru's 2017 exports of Marine
# Fish, Other (2764), 0 t in FAOSTAT's live-weight balance sheet and
# 1,253,750 t of net product weight in the legacy `fishstat-trade` pin. Its
# Pelagic Fish (2763) rows fall where the balance sheet books its own trade,
# so they never reach the balance under either option.

.legacy_fishstat_pin <- function() {
  data.table::data.table(
    Year = 2017L,
    `Area Code` = 170L,
    `Item Code` = c(2764L, 2763L, 2763L),
    Element = c("Export quantity", "Import quantity", "Export quantity"),
    Value = c(1253750, 95467, 40000),
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

# Peru 2017 as FAOSTAT's balance sheet reports it: no export of 2764, its own
# trade of 2763.
.peru_balance <- function() {
  tibble::tribble(
    ~year, ~area, ~area_code, ~item_cbs, ~item_cbs_code, ~element, ~value, ~source,
    2017L, "Peru", 170L, "Marine Fish, Other", 2764, "production", 50000, "FAOSTAT_FBS_New",
    2017L, "Peru", 170L, "Marine Fish, Other", 2764, "food", 59000, "FAOSTAT_FBS_New",
    2017L, "Peru", 170L, "Marine Fish, Other", 2764, "import", 9000, "FAOSTAT_FBS_New",
    2017L, "Peru", 170L, "Marine Fish, Other", 2764, "export", 0, "FAOSTAT_FBS_New",
    2017L, "Peru", 170L, "Pelagic Fish", 2763, "production", 4e6, "FAOSTAT_FBS_New",
    2017L, "Peru", 170L, "Pelagic Fish", 2763, "food", 1e5, "FAOSTAT_FBS_New",
    2017L, "Peru", 170L, "Pelagic Fish", 2763, "import", 120000, "FAOSTAT_FBS_New",
    2017L, "Peru", 170L, "Pelagic Fish", 2763, "export", 4530000, "FAOSTAT_FBS_New"
  )
}

.export_of <- function(balance, item) {
  balance |>
    dplyr::filter(.data$element == "export", .data$item_cbs_code == item) |>
    dplyr::pull(value)
}

.read_pin_rows <- function() {
  .mock_fishstat_pin(.legacy_fishstat_pin())
  whep:::.read_fishstat_trade(years = 2017L)
}

testthat::test_that("product-weight tonnes pass the unit gate but not the basis", {
  rows <- .read_pin_rows()
  # The unit gate is blind to the weight basis: both are tonnes, so it keeps
  # every product-weight tonne. That is why the basis needs its own guard.
  as_mass <- whep:::.mass_only_trade(rows, "fishstat-trade")
  testthat::expect_equal(sum(as_mass$value), sum(.legacy_fishstat_pin()$Value))
  testthat::expect_message(
    out <- whep:::.fishstat_weight_basis(rows, "withhold", .peru_balance()),
    class = "whep_fishstat_withheld"
  )
  testthat::expect_null(out)
})

testthat::test_that("the withheld message states what would have entered", {
  rows <- .read_pin_rows()
  msg <- testthat::capture_messages(
    whep:::.fishstat_weight_basis(rows, "withhold", .peru_balance())
  ) |>
    paste(collapse = " ") |>
    stringr::str_squish()
  # Only the 2764 export meets a balance-sheet zero; both 2763 flows fall
  # where the balance sheet books its own trade.
  testthat::expect_match(
    msg,
    "0 t of imports and 1,253,750 t of exports in net product weight, at keys where the balance sheet books no trade",
    fixed = TRUE
  )
  testthat::expect_match(
    msg,
    "The pin holds 95,467 t and 1,293,750 t in all",
    fixed = TRUE
  )
})

testthat::test_that("a key with no balance row counts as unbooked", {
  rows <- .read_pin_rows()
  balance <- dplyr::filter(.peru_balance(), .data$item_cbs_code != 2763)
  msg <- testthat::capture_messages(
    whep:::.fishstat_weight_basis(rows, "withhold", balance)
  ) |>
    paste(collapse = " ") |>
    stringr::str_squish()
  testthat::expect_match(
    msg,
    "95,467 t of imports and 1,293,750 t of exports",
    fixed = TRUE
  )
})

testthat::test_that("a balance-sheet zero is not filled with product weight", {
  rows <- .read_pin_rows()
  testthat::expect_message(
    withheld <- whep:::.fishstat_weight_basis(
      rows,
      "withhold",
      .peru_balance()
    ),
    class = "whep_fishstat_withheld"
  )
  kept <- whep:::.cbs_impute_trade(.peru_balance(), fao_trade_cbs = withheld)
  testthat::expect_equal(.export_of(kept, 2764), 0)

  # The pre-whep#1617 behaviour, still selectable, fills the zero with the
  # pin's product weight tonne for tonne, and only the zero: the message
  # figure is exactly what entered.
  testthat::expect_message(
    legacy <- whep:::.fishstat_weight_basis(
      rows,
      "as_live_weight",
      .peru_balance()
    ),
    "0 t of imports and 1,253,750 t of exports",
    class = "whep_fishstat_product_weight"
  )
  testthat::expect_message(
    filled <- whep:::.cbs_impute_trade(
      .peru_balance(),
      fao_trade_cbs = legacy
    ),
    "1 key, 1253750 tonnes"
  )
  testthat::expect_equal(.export_of(filled, 2764), 1253750)
  testthat::expect_equal(.export_of(filled, 2763), 4530000)
})

testthat::test_that("as_live_weight keeps the legacy rows and says what it is", {
  rows <- .read_pin_rows()
  testthat::expect_message(
    out <- whep:::.fishstat_weight_basis(
      rows,
      "as_live_weight",
      .peru_balance()
    ),
    "not a conversion"
  )
  testthat::expect_identical(out, rows)
})

testthat::test_that("an empty or absent pin withholds nothing and says nothing", {
  .mock_fishstat_pin(.legacy_fishstat_pin()[0])
  empty <- whep:::.read_fishstat_trade(years = 1970L)
  for (rows in list(empty, NULL)) {
    testthat::expect_no_message(
      out <- whep:::.fishstat_weight_basis(rows, "withhold", .peru_balance())
    )
    testthat::expect_null(out)
    testthat::expect_no_message(
      out <- whep:::.fishstat_weight_basis(
        rows,
        "as_live_weight",
        .peru_balance()
      )
    )
    testthat::expect_identical(out, rows)
  }
})

testthat::test_that("a raw candidate aborts before any method applies", {
  pin <- .legacy_fishstat_pin()
  pin[, `Weight Basis` := "net_product_weight"]
  .mock_fishstat_pin(pin)
  testthat::expect_error(
    whep:::.read_fishstat_trade(years = 2017L),
    class = "whep_fishstat_unconverted"
  )
})

testthat::test_that("an unknown fishstat_trade method aborts", {
  testthat::expect_error(
    whep:::.fishstat_weight_basis(
      data.table::data.table(element = "import", value = 1),
      "convert",
      .peru_balance()
    ),
    class = "rlang_error"
  )
  testthat::expect_error(
    whep::build_commodity_balances(fishstat_trade = "convert"),
    class = "rlang_error"
  )
})

testthat::test_that("withhold is the default of the CBS build", {
  testthat::expect_identical(
    eval(formals(whep::build_commodity_balances)$fishstat_trade)[[1]],
    "withhold"
  )
  testthat::expect_identical(
    eval(formals(whep:::.read_cbs)$fishstat_trade)[[1]],
    "withhold"
  )
})

testthat::test_that("the method is stamped on fish rows only", {
  rows <- tibble::tribble(
    ~item_cbs_code, ~element,
    2764,           "export",
    2781,           "food",
    2511,           "export"
  )
  for (method in c(whep:::.fishstat_trade_choices(), "not_applied")) {
    out <- whep:::.cbs_stamp_fish_trade(rows, method)
    testthat::expect_equal(
      out$method_fish_trade,
      c(method, method, NA_character_)
    )
  }
})

# The build from source combination to output, with only the pin readers and
# the balancing stubbed. `.fix_cbs()` is replaced by a probe that keeps the
# trade the imputation would read.
.run_fish_build <- function(fishstat_trade, env = parent.frame()) {
  probe <- new.env()
  empty_trade <- data.table::data.table(
    year = integer(),
    area_code = integer(),
    item_cbs_code = double(),
    element = character(),
    value = double(),
    unit = character()
  )
  testthat::local_mocked_bindings(
    .cbs_read_inputs = function(...) {
      list(fishstat_trade = .read_pin_rows(), fao_trade = empty_trade)
    },
    .cbs_combine_sources = function(inputs, bucket_coverage) {
      .peru_balance()
    },
    .prepare_historical_cbs = function(...) tibble::tibble(),
    .cbs_extend_historical = function(x, ...) x,
    .park_polity_identity = function(...) NULL,
    .aggregate_fao_trade_to_cbs = function(x, ...) x,
    .fix_cbs = function(df, ...) {
      probe$trade <- attr(df, ".fao_trade")
      attributes(df) <- attributes(df)[c("names", "row.names", "class")]
      df
    },
    .attach_polity_identity = function(x, ...) x,
    .package = "whep",
    .env = env
  )
  long <- whep::build_commodity_balances(
    tibble::tibble(),
    start_year = 2017L,
    end_year = 2017L,
    fishstat_trade = fishstat_trade
  )
  list(long = long, trade = probe$trade)
}

testthat::test_that("the build stamps and applies either method", {
  testthat::expect_message(
    default <- .run_fish_build("withhold"),
    class = "whep_fishstat_withheld"
  )
  pointblank::expect_col_vals_equal(
    default$long,
    "method_fish_trade",
    "withhold"
  )
  testthat::expect_equal(nrow(default$trade), 0L)

  testthat::expect_message(
    legacy <- .run_fish_build("as_live_weight"),
    class = "whep_fishstat_product_weight"
  )
  pointblank::expect_col_vals_equal(
    legacy$long,
    "method_fish_trade",
    "as_live_weight"
  )
  testthat::expect_equal(sum(legacy$trade$value), 1293750 + 95467)
})

testthat::test_that("a supplied balance ignores fishstat_trade and says so", {
  fixed <- tibble::tribble(
    ~year, ~item_cbs_code, ~item_cbs,             ~element,          ~value,
    1962L, 2671L,          "Tobacco",             "production",         100,
    1962L, 2671L,          "Tobacco",             "domestic_supply",    100,
    1962L, 2671L,          "Tobacco",             "food",               100,
    1962L, 2764L,          "Marine Fish, Other",  "production",          10,
    1962L, 2764L,          "Marine Fish, Other",  "food",                10
  ) |>
    dplyr::mutate(
      area = "United States",
      area_code = 231L,
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
    long <- whep::build_commodity_balances(.fixed_data = fixed),
    message = "fishstat_trade"
  )
  fish <- dplyr::filter(long, .data$item_cbs_code == 2764)
  other <- dplyr::filter(long, .data$item_cbs_code != 2764)
  testthat::expect_gt(nrow(fish), 0L)
  pointblank::expect_col_vals_equal(fish, "method_fish_trade", "not_applied")
  testthat::expect_true(all(is.na(other$method_fish_trade)))
})
