# The "correct" setting of `hist_trade_scale` (whep#1085, whep#1117).
#
# A synthetic miniature of the three populations the real pins hold above the
# world bound: a USA cotton-lint series with two ten-fold years interleaved
# with correct ones, a tobacco-products row that no partner books at all, and a
# British import that is consistent with its own mirror.
.x10_rows <- function() {
  tibble::tribble(
    ~year, ~iso3c, ~item_code_trade, ~element, ~value,
    1900L, "USA",  767L,             "export", 2000e3,
    1901L, "USA",  767L,             "export", 20000e3,
    1902L, "USA",  767L,             "export", 2100e3,
    1903L, "USA",  767L,             "export", 21000e3,
    1904L, "USA",  767L,             "export", 2200e3,
    1900L, "GBR",  767L,             "import", 2500e3,
    1901L, "GBR",  767L,             "import", 2500e3,
    1902L, "GBR",  767L,             "import", 2500e3,
    1903L, "GBR",  767L,             "import", 2500e3,
    1904L, "GBR",  767L,             "import", 2500e3,
    1903L, "USA",  831L,             "export", 115000e3,
    1901L, "GBR",  773L,             "import", 500e3,
    1901L, "RUS",  773L,             "export", 600e3
  ) |>
    data.table::as.data.table()
}

.x10_reference <- function() {
  tibble::tribble(
    ~item_code_trade, ~element, ~world_max,
    767L,             "export", 9000e3,
    767L,             "import", 9000e3,
    831L,             "export", 725e3,
    773L,             "import", 300e3,
    773L,             "export", 900e3
  ) |>
    data.table::as.data.table()
}

.x10_classified <- function() {
  rows <- .x10_rows()
  rows[
    .x10_reference(),
    world_max := i.world_max,
    on = c(
      "item_code_trade",
      "element"
    )
  ]
  whep:::.classify_hist_trade_scale(rows)
}

test_that("a ten-fold year proven by its mirror and neighbours is x10", {
  cls <- .x10_classified()
  usa <- cls[iso3c == "USA" & item_code_trade == 767L]

  expect_equal(
    usa[year %in% c(1901L, 1903L), hist_trade_class],
    c(
      "x10",
      "x10"
    )
  )
  # The correct years in between are within the bound and never classified.
  expect_true(all(is.na(usa[!year %in% c(1901L, 1903L), hist_trade_class])))
  # Partner side is GBR's import; the neighbours are the clean USA years.
  expect_equal(usa[year == 1901L, mirror], 2500e3)
  expect_equal(usa[year == 1901L, neighbour], 2100e3)
})

test_that("a row no partner books is not a mass", {
  cls <- .x10_classified()

  expect_equal(cls[item_code_trade == 831L, hist_trade_class], "not_mass")
  expect_equal(cls[item_code_trade == 831L, mirror], 0)
})

test_that("a row consistent with its own mirror is unexplained", {
  cls <- .x10_classified()
  gbr <- cls[iso3c == "GBR" & item_code_trade == 773L]

  expect_equal(gbr$hist_trade_class, "unexplained")
})

test_that("a ten-fold row with no clean neighbour is not corrected", {
  rows <- .x10_rows()[
    !(iso3c == "USA" &
      item_code_trade == 767L &
      year %in% c(1900L, 1902L, 1904L))
  ]
  rows[
    .x10_reference(),
    world_max := i.world_max,
    on = c(
      "item_code_trade",
      "element"
    )
  ]
  cls <- whep:::.classify_hist_trade_scale(rows)

  expect_equal(
    cls[iso3c == "USA" & item_code_trade == 767L, hist_trade_class],
    c("unexplained", "unexplained")
  )
})

test_that("a neighbour ratio that is not one power of ten is not x10", {
  rows <- .x10_rows()
  # 100x its neighbours: ten-fold would still leave it ten times too large.
  rows[
    iso3c == "USA" & year == 1901L & item_code_trade == 767L,
    value := 8500e3 * 10
  ]
  rows[
    .x10_reference(),
    world_max := i.world_max,
    on = c(
      "item_code_trade",
      "element"
    )
  ]
  cls <- whep:::.classify_hist_trade_scale(rows)

  expect_true(
    cls[iso3c == "USA" & year == 1901L, hist_trade_class] != "x10"
  )
})

test_that("correct divides the x10 rows by ten and drops the rest", {
  expect_message(
    expect_warning(
      out <- whep:::.screen_hist_trade_scale(
        .x10_rows(),
        .x10_reference(),
        method = "correct"
      ),
      class = "whep_hist_trade_scale"
    ),
    class = "whep_hist_trade_correct"
  )

  usa <- out[iso3c == "USA" & item_code_trade == 767L][order(year)]
  expect_equal(usa$value, c(2000e3, 2000e3, 2100e3, 2100e3, 2200e3))
  expect_false(831L %in% out$item_code_trade)
  expect_false(any(out$iso3c == "GBR" & out$item_code_trade == 773L))
  # The unflagged rows, including the whole partner side, are untouched.
  expect_equal(sum(out[iso3c == "GBR"]$value), 5 * 2500e3)
  expect_setequal(names(out), names(.x10_rows()))
})

test_that("correct records per row what it did", {
  suppressMessages(suppressWarnings(
    out <- whep:::.screen_hist_trade_scale(
      .x10_rows(),
      .x10_reference(),
      method = "correct"
    )
  ))
  log <- attr(out, "hist_trade_scale_log")

  expect_s3_class(log, "tbl_df")
  expect_equal(nrow(log), 4L)
  expect_setequal(
    log$hist_trade_scale_action,
    c("divided_by_10", "dropped_not_mass", "dropped_unexplained")
  )
  x10 <- log[log$hist_trade_scale_action == "divided_by_10", ]
  expect_equal(x10$value_used, x10$value_published / 10)
  expect_true(all(
    log$value_used[log$hist_trade_scale_action != "divided_by_10"] == 0
  ))
  expect_true(all(log$method_hist_trade_scale == "correct"))
})

test_that("report and drop log their flagged rows without classifying", {
  suppressWarnings({
    kept <- whep:::.screen_hist_trade_scale(
      .x10_rows(),
      .x10_reference(),
      method = "report"
    )
    dropped <- whep:::.screen_hist_trade_scale(
      .x10_rows(),
      .x10_reference(),
      method = "drop"
    )
  })

  expect_equal(sum(kept$value), sum(.x10_rows()$value))
  expect_true(all(
    attr(kept, "hist_trade_scale_log")$hist_trade_scale_action == "kept"
  ))
  expect_true(all(
    attr(dropped, "hist_trade_scale_log")$hist_trade_scale_action == "dropped"
  ))
  expect_true(all(is.na(attr(kept, "hist_trade_scale_log")$hist_trade_class)))
  expect_equal(nrow(dropped), nrow(.x10_rows()) - 4L)
})

test_that("correct classifies on neighbours outside the kept years", {
  suppressMessages(suppressWarnings(
    out <- whep:::.screen_hist_trade_scale(
      .x10_rows(),
      .x10_reference(),
      method = "correct",
      keep_years = 1901L
    )
  ))

  expect_equal(unique(out$year), 1901L)
  expect_equal(out[iso3c == "USA" & item_code_trade == 767L, value], 2000e3)
  expect_equal(attr(out, "hist_trade_scale_log")$year, c(1901L, 1901L))
})

test_that(".hist_trade_read_years widens only for correct", {
  expect_equal(whep:::.hist_trade_read_years(1950:1960, "report"), 1950:1960)
  expect_null(whep:::.hist_trade_read_years(NULL, "correct"))
  expect_equal(whep:::.hist_trade_read_years(1950:1960, "correct"), 1945:1965)
})

test_that(".read_historical_trade reads wider and trims back for correct", {
  asked <- NULL
  pin <- tibble::tribble(
    ~iso3, ~year, ~item_code, ~measurement, ~value,
    "USA", 1900L, 767,        "1000 MT",    2000,
    "USA", 1901L, 767,        "1000 MT",    20000,
    "USA", 1902L, 767,        "1000 MT",    2100
  )
  partner <- tibble::tribble(
    ~iso3, ~year, ~item_code, ~measurement, ~value,
    "GBR", 1900L, 767,        "1000 MT",    2500,
    "GBR", 1901L, 767,        "1000 MT",    2500,
    "GBR", 1902L, 767,        "1000 MT",    2500
  )
  testthat::local_mocked_bindings(
    .read_input = function(pin_alias, years = NULL, year_col = NULL) {
      asked <<- years
      rows <- if (pin_alias == "historical-trade-exports") pin else partner
      data.table::as.data.table(rows[rows$year %in% years, ])
    }
  )

  suppressMessages(suppressWarnings(
    out <- whep:::.read_historical_trade(
      years = 1901L,
      reference = .x10_reference(),
      scale_screen = "correct"
    )
  ))

  expect_equal(asked, 1896:1906)
  expect_equal(unique(out$year), 1901L)
  usa <- out[out$area_code == 231L & out$element == "export", ]
  expect_equal(sum(usa$value), 2000e3)
  log <- attr(out, "hist_trade_scale_log")
  expect_equal(log$hist_trade_scale_action, "divided_by_10")
})

test_that("\"correct\" is the default hist_trade_scale", {
  expect_identical(whep:::.hist_trade_scale_choices()[[1]], "correct")
  expect_identical(
    eval(formals(whep::build_commodity_balances)$hist_trade_scale)[[1]],
    "correct"
  )
})
