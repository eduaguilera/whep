.trade_provenance_cbs <- function() {
  tibble::tribble(
    ~element, ~value,
    "production", 7,
    "food", 62,
    "feed", 1,
    "import", 9,
    "export", 0,
    "stock_variation", -47
  ) |>
    dplyr::mutate(
      year = 2017L,
      area = "Peru",
      area_code = 170L,
      item_cbs = "Marine Fish, Other",
      item_cbs_code = 2764L,
      source = "FAOSTAT_FBS_New",
      fao_flag = "I"
    )
}

.trade_provenance_record <- function() {
  tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~element, ~value, ~source, ~fao_flag,
    2017L, 170L, 2764L, "export", 1253.75, "FishStat_trade", NA_character_
  )
}

.trade_provenance_row <- function(data, element = "export") {
  dplyr::filter(data, .data$element == .env$element)
}

test_that("trade replacement keeps the winning record's provenance", {
  cbs <- .trade_provenance_cbs()
  record <- .trade_provenance_record()
  out <- whep:::.cbs_impute_trade(cbs, record)
  row <- .trade_provenance_row(out)
  expect_equal(row$value, record$value)
  expect_identical(row$source, "FishStat_trade")
  expect_true(is.na(row$fao_flag))
  expect_identical(.trade_provenance_row(out, "import")$fao_flag, "I")

  record$source <- "FAOSTAT_trade"
  record$fao_flag <- "A"
  cbs$value[cbs$element == "export"] <- NA_real_
  row <- whep:::.cbs_impute_trade(cbs, record) |> .trade_provenance_row()
  expect_equal(row$value, record$value)
  expect_identical(row$source, "FAOSTAT_trade")
  expect_identical(row$fao_flag, "A")
})

test_that("retained CBS trade keeps its own source and flag", {
  cbs <- .trade_provenance_cbs()
  record <- .trade_provenance_record()
  row <- whep:::.cbs_impute_trade(cbs, record, trade_zero = "keep") |>
    .trade_provenance_row()
  expect_equal(row$value, 0)
  expect_identical(row$source, "FAOSTAT_FBS_New")
  expect_identical(row$fao_flag, "I")
  cbs$value[cbs$element == "export"] <- 15
  row <- whep:::.cbs_impute_trade(cbs, record) |> .trade_provenance_row()
  expect_equal(row$value, 15)
  expect_identical(row$source, "FAOSTAT_FBS_New")
  expect_identical(row$fao_flag, "I")
})

test_that("missing record metadata never inherits a displaced observation", {
  record <- .trade_provenance_record() |>
    dplyr::select(-source, -fao_flag)
  row <- whep:::.cbs_impute_trade(.trade_provenance_cbs(), record) |>
    .trade_provenance_row()
  expect_equal(row$value, record$value)
  expect_true(is.na(row$source))
  expect_true(is.na(row$fao_flag))
})

test_that("trade provenance survives the complete fixing cascade", {
  cbs <- .trade_provenance_cbs()
  attr(cbs, ".years") <- 2017L
  attr(cbs, ".fao_trade") <- .trade_provenance_record()
  expect_warning(
    fixed <- whep:::.fix_cbs(cbs),
    class = "whep_export_share_overflow"
  )
  row <- .trade_provenance_row(fixed)
  expect_equal(row$value, 1253.75)
  expect_identical(row$source, "FishStat_trade")
  expect_true(is.na(row$fao_flag))
})

test_that("derived trade cannot masquerade as a reported quantity", {
  cbs <- .trade_provenance_cbs() |>
    dplyr::filter(!.data$element %in% c("import", "export"))
  record <- .trade_provenance_record() |>
    dplyr::mutate(area_code = 1L)
  out <- whep:::.cbs_impute_trade(cbs, record)
  import <- .trade_provenance_row(out, "import")
  export <- .trade_provenance_row(out)
  expect_equal(import$value, 56)
  expect_identical(import$source, "trade_balance_residual")
  expect_true(is.na(import$fao_flag))
  expect_equal(export$value, 0)
  expect_identical(export$source, "trade_default_zero")
  expect_true(is.na(export$fao_flag))
})

test_that("FAOSTAT trade aggregation retains agreeing observation flags", {
  trade <- tibble::tribble(
    ~year, ~area_code, ~unit, ~element, ~item_code_trade, ~value, ~fao_flag,
    2017L, 170L, "t", "import", 15, 10, "A",
    2017L, 170L, "t", "import", 16, 20, "A"
  ) |>
    data.table::as.data.table()
  out <- whep:::.aggregate_fao_trade_to_cbs(trade)
  expect_equal(out$value, 30)
  expect_identical(out$source, "FAOSTAT_trade")
  expect_identical(out$fao_flag, "A")
  trade$fao_flag[2] <- "E"
  out <- whep:::.aggregate_fao_trade_to_cbs(trade)
  expect_equal(out$value, 30)
  expect_true(is.na(out$fao_flag))
  trade$fao_flag[2] <- NA_character_
  out <- whep:::.aggregate_fao_trade_to_cbs(trade)
  expect_equal(out$value, 30)
  expect_true(is.na(out$fao_flag))
})

test_that("mass aggregation retains the source composition without inventing flags", {
  trade <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~element, ~unit, ~value, ~source, ~fao_flag,
    2017L, 170L, 2764L, "export", "t", 10, "FAOSTAT_trade", "A",
    2017L, 170L, 2764L, "export", "tonnes", 20, "FishStat_trade", NA_character_
  ) |>
    data.table::as.data.table()
  original <- data.table::copy(trade)
  out <- whep:::.mass_only_trade(trade, "test")
  expect_equal(out$value, 30)
  expect_identical(out$source, "FAOSTAT_trade + FishStat_trade")
  expect_true(is.na(out$fao_flag))
  expect_identical(trade, original)
})

test_that("CBS input combination preserves both trade datasets", {
  fao <- tibble::tribble(
    ~year, ~area_code, ~unit, ~element, ~item_code_trade, ~value, ~fao_flag,
    2017L, 170L, "t", "import", 15, 30, "A"
  ) |>
    data.table::as.data.table()
  fish <- .trade_provenance_record() |>
    dplyr::select(-source, -fao_flag) |>
    dplyr::mutate(unit = "tonnes") |>
    data.table::as.data.table()
  out <- whep:::.cbs_all_trade(fao, fish)
  expect_equal(out$value, c(30, 1253.75))
  expect_identical(out$source, c("FAOSTAT_trade", "FishStat_trade"))
  expect_identical(out$fao_flag, c("A", NA_character_))
  expect_identical(
    whep:::.cbs_all_trade(fao, NULL),
    whep:::.cbs_all_trade(fao, fish[0L])
  )
  fish$unit <- "An"
  expect_warning(
    kept <- whep:::.cbs_all_trade(fao, fish),
    "not denominated in mass"
  )
  expect_identical(kept, whep:::.cbs_all_trade(fao, NULL))
})

test_that("empty and unknown-source aggregates keep an honest schema", {
  trade <- .trade_provenance_record() |>
    dplyr::mutate(unit = "t", source = NA_character_)
  out <- whep:::.mass_only_trade(trade, "test")
  expect_true(is.na(out$source))
  expect_true(is.na(out$fao_flag))
  expect_equal(out$value, trade$value)
  empty <- whep:::.mass_only_trade(trade[0L, ], "test")
  expect_equal(nrow(empty), 0L)
  expect_identical(empty$source, character())
  expect_identical(empty$fao_flag, character())
})
