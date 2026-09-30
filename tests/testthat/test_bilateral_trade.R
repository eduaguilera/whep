# Integration / regression tests -----------------------------------------------

testthat::test_that("get_bilateral_trade returns expected structure", {
  result <- get_bilateral_trade(example = TRUE)

  testthat::expect_s3_class(result, "tbl_df")
  testthat::expect_named(
    result,
    c(
      "year",
      "item_cbs_code",
      "bilateral_trade",
      "has_cbs_totals",
      "method_items_not_in_cbs",
      "unit",
      "method_seed_unit"
    )
  )
  testthat::expect_true(all(result$has_cbs_totals))
  testthat::expect_true(all(result$method_items_not_in_cbs == "drop"))
  testthat::expect_equal(nrow(result), 10)
  testthat::expect_true(all(
    purrr::map_lgl(result$bilateral_trade, is.matrix)
  ))

  # Every matrix must be square 187x187
  purrr::walk(result$bilateral_trade, function(m) {
    testthat::expect_equal(nrow(m), 187)
    testthat::expect_equal(ncol(m), 187)
  })
})

testthat::test_that("get_bilateral_trade example has expected content", {
  result <- get_bilateral_trade(example = TRUE)

  # All 10 groups should have matrices filled with 1s
  purrr::walk(result$bilateral_trade, function(m) {
    testthat::expect_true(all(m == 1))
  })

  # Check known year/item combinations exist
  testthat::expect_true(
    any(result$year == 2003 & result$item_cbs_code == 2552)
  )
  testthat::expect_true(
    any(result$year == 2015 & result$item_cbs_code == 2672)
  )
})

# Unit tests -------------------------------------------------------------------

testthat::test_that(".prefer_flow_direction chooses preferred trade data", {
  bilateral_trade <- tibble::tribble(
    ~from_code, ~to_code, ~year, ~item_cbs_code, ~element, ~value,
    1, 2, 2000, 1, "Import", 0,
    1, 2, 2000, 1, "Export", 0,
    1, 2, 2000, 2, "Export", 0,
    1, 3, 2000, 2, "Import", 0,
    2, 3, 2000, 2, "Import", 0,
    2, 3, 2001, 2, "Export", 0,
    2, 3, 2001, 2, "Import", 0,
  ) |>
    dplyr::arrange(from_code, to_code, year, item_cbs_code)

  brute_group_by_result <- bilateral_trade |>
    dplyr::group_by(from_code, to_code, year, item_cbs_code) |>
    dplyr::filter(dplyr::n() == 1 | element == "Import") |>
    dplyr::ungroup() |>
    dplyr::arrange(from_code, to_code, year, item_cbs_code)

  my_result <- .prefer_flow_direction(bilateral_trade, "Import") |>
    dplyr::arrange(from_code, to_code, year, item_cbs_code)

  expected_import_result <- tibble::tribble(
    ~from_code, ~to_code, ~year, ~item_cbs_code, ~element, ~value,
    1, 2, 2000, 1, "Import", 0,
    1, 2, 2000, 2, "Export", 0,
    1, 3, 2000, 2, "Import", 0,
    2, 3, 2000, 2, "Import", 0,
    2, 3, 2001, 2, "Import", 0,
  ) |>
    dplyr::arrange(from_code, to_code, year, item_cbs_code)

  testthat::expect_equal(my_result, brute_group_by_result)
  testthat::expect_equal(my_result, expected_import_result)

  brute_group_by_result <- bilateral_trade |>
    dplyr::group_by(from_code, to_code, year, item_cbs_code) |>
    dplyr::filter(dplyr::n() == 1 | element == "Export") |>
    dplyr::ungroup() |>
    dplyr::arrange(from_code, to_code, year, item_cbs_code)

  my_result <- .prefer_flow_direction(bilateral_trade, "Export") |>
    dplyr::arrange(from_code, to_code, year, item_cbs_code)

  expected_export_result <- tibble::tribble(
    ~from_code, ~to_code, ~year, ~item_cbs_code, ~element, ~value,
    1, 2, 2000, 1, "Export", 0,
    1, 2, 2000, 2, "Export", 0,
    1, 3, 2000, 2, "Import", 0,
    2, 3, 2000, 2, "Import", 0,
    2, 3, 2001, 2, "Export", 0,
  ) |>
    dplyr::arrange(from_code, to_code, year, item_cbs_code)

  testthat::expect_equal(my_result, brute_group_by_result)
  testthat::expect_equal(my_result, expected_export_result)
})

testthat::test_that(".estimate_bilateral_trade creates expected matrix", {
  exports <- c(5, 0, 4)
  imports <- c(1, 3, 0)
  expected <- matrix(
    # fmt: skip
    c(
      0.9027778, 2.708333, 0,
      0.0000000, 0.000000, 0,
      0.7222222, 2.166667, 0
    ),
    byrow = TRUE,
    ncol = 3
  )
  result <- .estimate_bilateral_trade(exports, imports)
  testthat::expect_equal(result, expected, tolerance = 1e-6)

  # Martin' slide example
  exports <- c(500, 300, 100, 0, 0, 0)
  imports <- c(200, 150, 120, 200, 190, 30)
  expected <- matrix(
    # fmt: skip
    c(
      112, 84, 67, 112, 106, 17,
      67, 50, 40, 67, 64, 10,
      22, 17, 13, 22, 21, 3,
      0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0
    ),
    byrow = TRUE,
    ncol = 6
  )
  result <- .estimate_bilateral_trade(exports, imports)
  testthat::expect_equal(result, expected, tolerance = 1)

  # No data imports sum 0
  exports <- c(5, 0, 4)
  imports <- c(0, 0, 0)
  expected <- matrix(
    # fmt: skip
    c(
      0, 0, 0,
      0, 0, 0,
      0, 0, 0
    ),
    byrow = TRUE,
    ncol = 3
  )
  result <- .estimate_bilateral_trade(exports, imports)
  testthat::expect_equal(result, expected, tolerance = 1)

  # No data exports sum 0
  exports <- c(0, 0, 0)
  imports <- c(1, 3, 0)
  expected <- matrix(
    # fmt: skip
    c(
      0, 0, 0,
      0, 0, 0,
      0, 0, 0
    ),
    byrow = TRUE,
    ncol = 3
  )
  result <- .estimate_bilateral_trade(exports, imports)
  testthat::expect_equal(result, expected, tolerance = 1)

  # No data both sum 0
  exports <- c(0, 0, 0)
  imports <- c(0, 0, 0)
  expected <- matrix(
    # fmt: skip
    c(
      0, 0, 0,
      0, 0, 0,
      0, 0, 0
    ),
    byrow = TRUE,
    ncol = 3
  )
  result <- .estimate_bilateral_trade(exports, imports)
  testthat::expect_equal(result, expected, tolerance = 1)
})

testthat::test_that(".fill_missing_trade only fills NA entries of matrix", {
  original <- matrix(
    # fmt: skip
    c(
      140, NA, NA,
      50, 100, NA,
      NA, NA, NA
    ),
    byrow = TRUE,
    ncol = 3
  )

  # Regression for #152: a country never trades with itself, so the
  # diagonal is always forced to 0 -- including the pre-existing 140/100
  # values at [1,1]/[2,2], which .fill_missing_trade() now treats as
  # self-trade rather than ordinary (non-NA, left-alone) cells.
  expected <- matrix(
    # fmt: skip
    c(
      0.00, 7.65, 2.45,
      50.00, 0.00, 2.96,
      3.64, 4.55, 0.00
    ),
    byrow = TRUE,
    ncol = 3
  )
  total_trade <- tibble::tribble(
    ~area_code, ~export, ~import,
    4, 250, 200,
    6, 200, 250,
    7, 100, 80
  ) |>
    .balance_total_trade()

  original |>
    .fill_missing_trade(total_trade) |>
    testthat::expect_equal(expected, tolerance = 1e-2)
})

testthat::test_that(".fill_missing_trade zeroes the diagonal even when the rest is unchanged", {
  original <- matrix(
    # fmt: skip
    c(
      140, 40, 30,
      50, 100, 77,
      11, 324, 23
    ),
    byrow = TRUE,
    ncol = 3
  )
  # Regression for #152: off-diagonal non-NA cells stay untouched, but the
  # diagonal (self-trade) is always forced to 0.
  expected <- matrix(
    # fmt: skip
    c(
      0, 40, 30,
      50, 0, 77,
      11, 324, 0
    ),
    byrow = TRUE,
    ncol = 3
  )
  total_trade <- tibble::tribble(
    ~area_code, ~export, ~import,
    4, 250, 250,
    6, 300, 550,
    7, 450, 150
  ) |>
    .balance_total_trade()

  original |>
    .fill_missing_trade(total_trade) |>
    testthat::expect_equal(expected, tolerance = 1e-2)
})

testthat::test_that(".fill_missing_trade fills with 0s if row sum is already past CBS report", {
  original <- matrix(
    # fmt: skip
    c(
      140, NA,
      NA, 100
    ),
    byrow = TRUE,
    ncol = 2
  )
  # Regression for #152: both diagonal entries are forced to 0 (self-trade),
  # on top of the pre-existing "row sum already past target" 0-fill.
  expected <- matrix(
    # fmt: skip
    c(
      0, 0,
      0, 0
    ),
    byrow = TRUE,
    ncol = 2
  )
  total_trade <- tibble::tribble(
    ~area_code, ~export, ~import,
    4, 130, 200,
    6, 90, 100,
  ) |>
    .balance_total_trade()

  original |>
    .fill_missing_trade(total_trade) |>
    testthat::expect_equal(expected, tolerance = 1e-2)
})

testthat::test_that(".balance_matrix makes rows and columns have target sum", {
  total_trade <- tibble::tibble(
    area_code = c(4, 6, 7, 9, 10, 75),
    export = c(500, 300, 100, 0, 0, 0),
    import = c(200, 150, 120, 200, 190, 30)
  ) |>
    .balance_total_trade()

  trade_matrix <- matrix(
    # fmt: skip
    c(
      140, 30, 34, 140, 120, 8,
      50, 100, 20, 50, 60, 5,
      11, 8, 50, 11, 11, 2,
      0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0
    ),
    byrow = TRUE,
    ncol = 6
  )
  dimnames(trade_matrix) <- list(
    as.character(total_trade$area_code),
    as.character(total_trade$area_code)
  )

  # Rescaling exports to match total sum of 890 imports
  balanced_total_exports <- c(494.44, 296.67, 98.89, 0, 0, 0)
  balanced_total_imports <- c(200, 150, 120, 200, 190, 30)

  result <- .balance_matrix(trade_matrix, total_trade)
  testthat::expect_equal(dimnames(result), dimnames(trade_matrix))
  testthat::expect_equal(
    as.numeric(rowSums(result)),
    balanced_total_exports,
    tolerance = 1e-2
  )
  testthat::expect_equal(
    as.numeric(colSums(result)),
    balanced_total_imports,
    tolerance = 1e-2
  )
})

testthat::test_that(".balance_matrix aligns targets by country code", {
  total_trade <- tibble::tibble(
    area_code = factor(c(2, 3, 1)),
    export = c(0, 0, 10),
    import = c(10, 0, 0)
  ) |>
    .balance_total_trade()

  trade_matrix <- matrix(
    1,
    nrow = 3,
    ncol = 3,
    dimnames = list(c("1", "2", "3"), c("1", "2", "3"))
  )

  result <- .balance_matrix(trade_matrix, total_trade)

  testthat::expect_equal(as.numeric(rowSums(result)), c(10, 0, 0))
  testthat::expect_equal(as.numeric(colSums(result)), c(0, 10, 0))
})

testthat::test_that(".balance_matrix never allocates self-trade on the diagonal", {
  # Regression for #152: .fill_missing_trade()'s na_mask includes the
  # diagonal, so a large trader (big exports AND big imports) previously got
  # a spurious i -> i flow seeded by .estimate_bilateral_trade() and then
  # preserved (often inflated) by IPF's sub[sub == 0] <- 1 seeding step,
  # stealing mass from its real trading partners. Uses a matrix that
  # reproduces the exact reported failure mode: country 1 is the largest
  # trader (1000 export / 900 import) with a mostly-unobserved (NA) row.
  n <- 4
  code_int <- c(10L, 20L, 30L, 40L)
  btd <- tibble::tribble(
    ~from_code, ~to_code, ~value,
    10L, 20L, 500,
    10L, 30L, 300,
    20L, 10L, 400,
    30L, 10L, 200,
    40L, 20L, 50
  )
  total_trade <- tibble::tribble(
    ~area_code, ~export, ~import, ~balanced_export, ~balanced_import,
    10L, 1000, 900, 1000, 900,
    20L, 200, 700, 200, 700,
    30L, 150, 350, 150, 350,
    40L, 80, 100, 80, 100
  )

  result <- btd |>
    .build_trade_matrix(n, code_int) |>
    .fill_missing_trade(total_trade) |>
    .balance_matrix(total_trade)

  testthat::expect_equal(as.numeric(diag(result)), rep(0, n))
})

testthat::test_that(".build_trade_matrix completes missing countries", {
  code_int <- c(1L, 2L, 4L, 5L, 999L)
  code_levels <- as.character(code_int)
  n <- length(code_int)
  btd <- tibble::tribble(
    ~from_code, ~to_code, ~value,
    1L, 2L, 1,
    1L, 4L, 2,
    4L, 2L, 1,
    5L, 4L, 2
  )
  expected <- matrix(
    # fmt: skip
    c(
      NA, 1, 2, NA, NA,
      NA, NA, NA, NA, NA,
      NA, 1, NA, NA, NA,
      NA, NA, 2, NA, NA,
      NA, NA, NA, NA, NA
    ),
    byrow = TRUE,
    ncol = 5,
    dimnames = list(code_levels, code_levels)
  )

  btd |>
    .build_trade_matrix(n, code_int) |>
    testthat::expect_equal(expected)
})

testthat::test_that(".build_trade_matrix sums duplicate country pairs", {
  code_int <- c(1L, 2L, 3L)
  n <- length(code_int)
  btd <- tibble::tribble(
    ~from_code, ~to_code, ~value,
    1L, 2L, 3,
    1L, 2L, 4,
    2L, 3L, 5
  )

  result <- .build_trade_matrix(btd, n, code_int)

  testthat::expect_equal(result["1", "2"], 7)
  testthat::expect_equal(result["2", "3"], 5)
})

testthat::test_that(".ipf_2d converges to target margins", {
  seed <- matrix(1, nrow = 3, ncol = 3)
  target_rows <- c(10, 20, 30)
  target_cols <- c(15, 25, 20)
  result <- .ipf_2d(seed, target_rows, target_cols)

  testthat::expect_equal(
    rowSums(result),
    target_rows,
    tolerance = 0.1
  )
  testthat::expect_equal(
    colSums(result),
    target_cols,
    tolerance = 0.1
  )
})

testthat::test_that(".ipf_2d handles unequal seed values", {
  seed <- matrix(
    c(5, 1, 3, 2, 4, 1, 1, 2, 6),
    nrow = 3,
    byrow = TRUE
  )
  target_rows <- c(100, 80, 120)
  target_cols <- c(90, 110, 100)
  result <- .ipf_2d(seed, target_rows, target_cols)

  testthat::expect_equal(
    rowSums(result),
    target_rows,
    tolerance = 0.1
  )
  testthat::expect_equal(
    colSums(result),
    target_cols,
    tolerance = 0.1
  )
})

testthat::test_that(".ipf_2d handles zero rows gracefully", {
  seed <- matrix(
    c(1, 1, 0, 0, 1, 1),
    nrow = 2,
    byrow = TRUE
  )
  target_rows <- c(10, 0)
  target_cols <- c(5, 5, 0)
  result <- .ipf_2d(seed, target_rows, target_cols)

  testthat::expect_equal(
    rowSums(result),
    target_rows,
    tolerance = 0.1
  )
  testthat::expect_equal(
    result[2, ],
    c(0, 0, 0),
    tolerance = 0.1
  )
})

testthat::test_that(".complete_total_trade fills missing codes", {
  codes <- factor(c(10, 20, 30))
  total_trade <- tibble::tribble(
    ~year, ~item_cbs_code, ~area_code, ~export, ~import,
    2000, 1, codes[1], 5, 3,
    2000, 1, codes[2], 2, 4
  )

  result <- .complete_total_trade(total_trade, codes)

  testthat::expect_equal(nrow(result), 3)
  result_30 <- result |>
    dplyr::filter(area_code == 30)
  testthat::expect_equal(
    result_30$export,
    0
  )
  testthat::expect_equal(
    result_30$import,
    0
  )
})

testthat::test_that(".complete_total_trade handles multiple year-item combos", {
  codes <- factor(c(10, 20))
  total_trade <- tibble::tribble(
    ~year, ~item_cbs_code, ~area_code, ~export, ~import,
    2000, 1, codes[1], 5, 3,
    2001, 2, codes[1], 8, 1
  )
  result <- .complete_total_trade(total_trade, codes)

  testthat::expect_equal(nrow(result), 4)
})

testthat::test_that(".get_all_country_codes returns unique sorted", {
  btd <- tibble::tibble(
    from_code = c(1, 2, 3),
    to_code = c(2, 4, 5)
  )
  cbs <- tibble::tibble(area_code = c(1, 6))
  result <- .get_all_country_codes(btd, cbs)

  testthat::expect_equal(
    as.integer(result),
    c(1, 2, 3, 4, 5, 6)
  )
  testthat::expect_s3_class(result, "factor")
})

testthat::test_that(".filter_only_items_in_cbs removes items not in cbs", {
  btd <- tibble::tribble(
    ~item_cbs_code, ~value,
    1, 10,
    2, 20,
    3, 30
  )
  cbs <- tibble::tibble(item_cbs_code = c(1, 3))
  testthat::expect_warning(
    result <- .filter_only_items_in_cbs(btd, cbs),
    "no commodity balance"
  )

  testthat::expect_equal(nrow(result), 2)
  items <- result |> dplyr::pull(item_cbs_code)
  testthat::expect_true(all(items %in% c(1, 3)))
})

# Items with no CBS row (whep#943) -------------------------------------------

testthat::test_that(".filter_only_items_in_cbs reports the code, tonnage and share dropped", {
  # Regression for whep#943: the drop used to be silent, so 26% of the
  # pin's traded tonnage left the pipeline with no message at all.
  btd <- tibble::tribble(
      ~item_cbs_code, ~value,
      2511, 250,
      5001, 750
    )
  cbs <- tibble::tibble(item_cbs_code = 2511)

  testthat::expect_warning(
    .filter_only_items_in_cbs(btd, cbs),
    "5001"
  )
  testthat::expect_warning(
    .filter_only_items_in_cbs(btd, cbs),
    "75 percent"
  )
})

testthat::test_that(".filter_only_items_in_cbs stays silent when all match", {
  btd <- tibble::tribble(
    ~item_cbs_code, ~value,
    2511, 250,
    2514, 750
  )
  cbs <- tibble::tibble(item_cbs_code = c(2511, 2514))

  result <- testthat::expect_no_warning(
    .filter_only_items_in_cbs(btd, cbs)
  )
  testthat::expect_equal(nrow(result), 2)
})

testthat::test_that(".filter_only_items_in_cbs keeps the items on 'keep'", {
  # 750 stands in for an unanchored item whose tonnes really are masses.
  # Item 5001 no longer can: see the refusal tests below (whep#1023).
  btd <- tibble::tribble(
    ~item_cbs_code, ~value,
    2511, 250,
    750, 750
  )
  cbs <- tibble::tibble(item_cbs_code = 2511)

  testthat::expect_warning(
    result <- .filter_only_items_in_cbs(btd, cbs, "keep"),
    "no commodity balance"
  )
  testthat::expect_equal(sum(result$value), 1000)
  testthat::expect_setequal(result$item_cbs_code, c(2511, 750))
})

# tonnes that are not masses (whep#1023) --------------------------------------

testthat::test_that(".unbacked_mass_cbs_items resolves item 1293 to CBS 5001", {
  # Guards the derivation, not a hardcoded number: if `cbs_trade_codes` or
  # `items_full` remaps FAOSTAT trade item 1293 ("Crude materials"), the
  # refusal below must follow it rather than keep pointing at 5001.
  testthat::expect_equal(.unbacked_mass_trade_items(), 1293L)
  testthat::expect_equal(.unbacked_mass_cbs_items(), 5001)
})

testthat::test_that("'keep' refuses an item whose tonnes are not masses", {
  # Regression for whep#1023. `"keep"` takes the matrix margins from the
  # reported flows, so it would have carried Colombia's 2.58 Gt 2004 cell
  # into the output verbatim.
  btd <- tibble::tribble(
    ~item_cbs_code, ~unit, ~value,
    2511, "tonnes", 250,
    5001, "tonnes", 2579549887
  )
  cbs <- tibble::tibble(item_cbs_code = 2511)

  testthat::expect_error(
    suppressWarnings(.filter_only_items_in_cbs(btd, cbs, "keep")),
    class = "whep_unbacked_mass_trade"
  )
})

testthat::test_that("'keep' refusal names the item and its tonnage", {
  btd <- tibble::tribble(
    ~item_cbs_code, ~unit, ~value,
    2511, "tonnes", 250,
    5001, "tonnes", 2579549887,
    5001, "heads", 1e9
  )
  cbs <- tibble::tibble(item_cbs_code = 2511)

  testthat::expect_error(
    suppressWarnings(.filter_only_items_in_cbs(btd, cbs, "keep")),
    "5001"
  )
  # The head-count row must not be added into the reported tonnage.
  testthat::expect_error(
    suppressWarnings(.filter_only_items_in_cbs(btd, cbs, "keep")),
    "2.58e\\+09"
  )
})

testthat::test_that("'drop' and 'abort' are unchanged by the refusal", {
  btd <- tibble::tribble(
    ~item_cbs_code, ~unit, ~value,
    2511, "tonnes", 250,
    5001, "tonnes", 2579549887
  )
  cbs <- tibble::tibble(item_cbs_code = 2511)

  testthat::expect_warning(
    result <- .filter_only_items_in_cbs(btd, cbs, "drop"),
    "no commodity balance"
  )
  testthat::expect_equal(result$item_cbs_code, 2511)
  testthat::expect_error(
    .filter_only_items_in_cbs(btd, cbs, "abort"),
    "no commodity balance"
  )
})

testthat::test_that("'keep' still works when no kept item is unbacked", {
  btd <- tibble::tribble(
    ~item_cbs_code, ~unit, ~value,
    2511, "tonnes", 250,
    750, "tonnes", 750
  )
  cbs <- tibble::tibble(item_cbs_code = 2511)

  testthat::expect_warning(
    result <- .filter_only_items_in_cbs(btd, cbs, "keep"),
    "no commodity balance"
  )
  testthat::expect_setequal(result$item_cbs_code, c(2511, 750))
})

testthat::test_that("an unbacked item with a CBS row is refused too", {
  # Regression for whep#1023. The only thing keeping the pin's 2.58 Gt cell
  # out of a published number was that item 5001 happens to have no CBS row,
  # so `"drop"` removed it -- "luck, not design", in the issue's words. Give
  # the same item a CBS row and the refusal has to still hold: otherwise the
  # tonnage is balanced against CBS margins and pushed through IPF, silently,
  # on the default method.
  btd <- tibble::tribble(
    ~item_cbs_code, ~unit, ~value,
    2511, "tonnes", 250,
    5001, "tonnes", 2579549887
  )
  cbs <- tibble::tibble(item_cbs_code = c(2511, 5001))

  testthat::expect_error(
    .filter_only_items_in_cbs(btd, cbs, "drop"),
    class = "whep_unbacked_mass_trade"
  )
  testthat::expect_error(
    .filter_only_items_in_cbs(btd, cbs, "abort"),
    class = "whep_unbacked_mass_trade"
  )
  testthat::expect_error(
    .filter_only_items_in_cbs(btd, cbs, "keep"),
    class = "whep_unbacked_mass_trade"
  )
})

testthat::test_that("the refusal ignores an unbacked item being dropped", {
  # Mirror of the test above: on `"drop"` an unbacked item with no CBS row is
  # removed, so it must not trigger the refusal. Only a kept one does.
  btd <- tibble::tribble(
    ~item_cbs_code, ~unit, ~value,
    2511, "tonnes", 250,
    5001, "tonnes", 2579549887
  )
  cbs <- tibble::tibble(item_cbs_code = 2511)

  testthat::expect_warning(
    result <- .filter_only_items_in_cbs(btd, cbs, "drop"),
    "no commodity balance"
  )
  testthat::expect_equal(result$item_cbs_code, 2511)
})

testthat::test_that(".filter_only_items_in_cbs aborts on 'abort'", {
  btd <- tibble::tribble(
    ~item_cbs_code, ~value,
    2511, 250,
    5001, 750
  )
  cbs <- tibble::tibble(item_cbs_code = 2511)

  testthat::expect_error(
    .filter_only_items_in_cbs(btd, cbs, "abort"),
    "no commodity balance"
  )
})

testthat::test_that("get_bilateral_trade rejects an unknown method", {
  testthat::expect_error(
    get_bilateral_trade(method_items_not_in_cbs = "map_to_residual"),
    class = "rlang_error"
  )
})

testthat::test_that(".own_margin_totals uses the item's own reported flows", {
  codes <- factor(c(10, 20, 30))
  flows <- tibble::tribble(
    ~from_code, ~to_code, ~value,
    10L, 20L, 40,
    10L, 30L, 60,
    20L, 30L, 25
  )

  result <- .own_margin_totals(flows, codes)

  testthat::expect_equal(as.character(result$area_code), c("10", "20", "30"))
  testthat::expect_equal(result$export, c(100, 25, 0))
  testthat::expect_equal(result$import, c(0, 40, 85))
  # Self-derived margins agree by construction, so balancing is the identity.
  testthat::expect_equal(result$balanced_export, result$export)
  testthat::expect_equal(result$balanced_import, result$import)
})

testthat::test_that(".attach_total_trade drops unanchored groups by default", {
  nested <- tibble::tribble(
    ~year, ~item_cbs_code, ~bilateral_trade,
    2010, 2511, tibble::tibble(from_code = 10L, to_code = 20L, value = 100),
    2010, 5001, tibble::tibble(from_code = 10L, to_code = 20L, value = 750)
  )
  codes <- factor(c(10, 20))
  cbs <- tibble::tribble(
    ~year, ~item_cbs_code, ~area_code, ~export, ~import,
    2010, 2511, 10, 100, 0,
    2010, 2511, 20, 0, 100
  ) |>
    dplyr::mutate(area_code = factor(area_code, levels = codes))

  result <- .attach_total_trade(nested, cbs, codes, "drop")

  testthat::expect_equal(result$item_cbs_code, 2511)
  testthat::expect_true(all(result$has_cbs_totals))
})

testthat::test_that(".attach_total_trade flags self-derived margins on 'keep'", {
  nested <- tibble::tribble(
    ~year, ~item_cbs_code, ~bilateral_trade,
    2010, 2511, tibble::tibble(from_code = 10L, to_code = 20L, value = 100),
    2010, 5001, tibble::tibble(from_code = 10L, to_code = 20L, value = 750)
  )
  codes <- factor(c(10, 20))
  cbs <- tibble::tribble(
    ~year, ~item_cbs_code, ~area_code, ~export, ~import,
    2010, 2511, 10, 100, 0,
    2010, 2511, 20, 0, 100
  ) |>
    dplyr::mutate(area_code = factor(area_code, levels = codes))

  result <- .attach_total_trade(nested, cbs, codes, "keep")

  testthat::expect_equal(result$item_cbs_code, c(2511, 5001))
  testthat::expect_equal(result$has_cbs_totals, c(TRUE, FALSE))
  unanchored <- result$total_trade[[2]]
  testthat::expect_equal(unanchored$export, c(750, 0))
  testthat::expect_equal(unanchored$import, c(0, 750))
})

testthat::test_that("'keep' carries the unanchored tonnage into the matrix", {
  # The invariant that matters: a kept item's balanced matrix must still
  # hold the tonnage the pin reported for it, and a dropped one must not
  # appear at all. `.balance_matrix()` renormalises to the margins, which
  # for a kept item are its own row/column sums.
  # 750 stands in for an unanchored item whose tonnes really are masses;
  # item 5001's do not, and `"keep"` refuses it (whep#1023).
  btd <- tibble::tribble(
    ~year, ~item_cbs_code, ~from_code, ~to_code, ~unit, ~value,
    2010, 2511, 10L, 20L, "tonnes", 100,
    2010, 750, 10L, 20L, "tonnes", 750,
    2010, 750, 20L, 10L, "tonnes", 250
  )
  cbs <- tibble::tribble(
    ~year, ~item_cbs_code, ~area_code, ~unit, ~export, ~import,
    2010, 2511, 10L, "tonnes", 100, 0,
    2010, 2511, 20L, "tonnes", 0, 100
  )
  codes <- factor(c(10L, 20L))

  testthat::expect_warning(
    nested_drop <- .nest_by_year_item_code(btd, cbs, codes, "drop"),
    "no commodity balance"
  )
  dropped <- .process_bilateral_trade(nested_drop, codes)
  testthat::expect_equal(dropped$item_cbs_code, 2511)

  testthat::expect_warning(
    nested_keep <- .nest_by_year_item_code(btd, cbs, codes, "keep"),
    "no commodity balance"
  )
  kept <- .process_bilateral_trade(nested_keep, codes)

  testthat::expect_equal(kept$item_cbs_code, c(750, 2511))
  other <- kept$bilateral_trade[[1]]
  testthat::expect_equal(sum(other), 1000)
  testthat::expect_equal(other["10", "20"], 750)
  testthat::expect_equal(other["20", "10"], 250)
})

# Fixture with several year-item groups: enough to be split across workers,
# and unbalanced enough that `.balance_matrix()` does real work on each.
.worker_invariance_fixture <- function() {
  countries <- c(10L, 20L, 30L, 40L)
  keys <- tidyr::expand_grid(
    year = 2010:2012,
    item_cbs_code = c(2511L, 2531L)
  )
  flows <- tidyr::expand_grid(keys, from_code = countries, to_code = countries)
  btd <- flows |>
    dplyr::filter(from_code != to_code) |>
    dplyr::mutate(
      unit = "tonnes",
      # Deterministic, asymmetric, and zero for some pairs so the
      # missing-flow estimator is exercised too.
      value = ((from_code * 7L + to_code * 3L + year + item_cbs_code) %% 11L) *
        10
    )
  cbs <- tidyr::expand_grid(keys, area_code = countries) |>
    dplyr::mutate(
      unit = "tonnes",
      export = ((area_code + year) %% 5L) * 100 + 50,
      import = ((area_code * 3L + item_cbs_code) %% 7L) * 100 + 50
    )
  list(btd = btd, cbs = cbs, codes = factor(countries))
}

testthat::test_that(".process_bilateral_trade output is worker-invariant", {
  # `.process_bilateral_trade()` claims its result does not depend on the
  # worker count. Assert it: each group is balanced from its own inputs and
  # mclapply preserves input order, so 1, 2 and N workers must agree
  # bit-for-bit.
  fixture <- .worker_invariance_fixture()
  nested <- .nest_by_year_item_code(
    fixture$btd,
    fixture$cbs,
    fixture$codes,
    "drop"
  )
  testthat::expect_gt(nrow(nested), 2L)

  # `mclapply(mc.cores > 1)` stops outright on Windows, so the only honest
  # multi-worker comparison there is none: run serially and let the assertion
  # below be trivially true rather than erroring. `.parallel_workers()` already
  # forces 1 on Windows, so the invariant it guards cannot be violated there.
  worker_counts <- if (.is_windows()) {
    1L
  } else if (!.core_limit_in_force() && isTRUE(parallel::detectCores() >= 4L)) {
    c(1L, 2L, 4L)
  } else {
    c(1L, 2L)
  }

  runs <- purrr::map(worker_counts, function(workers) {
    testthat::local_mocked_bindings(
      .parallel_workers = function(...) workers
    )
    .process_bilateral_trade(nested, fixture$codes)
  })

  purrr::walk(runs[-1], function(run) {
    testthat::expect_identical(run, runs[[1]])
  })
  # Guard the guard: the comparison would be vacuous on empty matrices.
  testthat::expect_true(all(purrr::map_lgl(
    runs[[1]]$bilateral_trade,
    function(m) is.matrix(m) && sum(m) > 0
  )))
})

testthat::test_that(".process_bilateral_trade obeys the check core limit", {
  # Regression guard for #1039: `R CMD check --as-cran` sets
  # `_R_CHECK_LIMIT_CORES_`, and parallel's own guard then aborts above two
  # processes. Before the fix this asked for half the host's cores, so it
  # errored on any machine with more than four.
  withr::local_envvar(c("_R_CHECK_LIMIT_CORES_" = "TRUE"))
  requested <- NULL
  testthat::local_mocked_bindings(
    # `mc.cores` arrives in the dots so the stub's own formals can stay
    # snake_case; it is the value parallel's guard would have vetted.
    mclapply = function(values, fn, ...) {
      requested <<- list(...)$mc.cores
      lapply(values, fn)
    },
    .package = "parallel"
  )

  fixture <- .worker_invariance_fixture()
  nested <- .nest_by_year_item_code(
    fixture$btd,
    fixture$cbs,
    fixture$codes,
    "drop"
  )
  result <- .process_bilateral_trade(nested, fixture$codes)

  testthat::expect_lte(requested, 2L)
  testthat::expect_equal(nrow(result), nrow(nested))
})

testthat::test_that(".downscale_estimate_matrix scales rows exceeding balance", {
  estimates <- matrix(
    c(6, 4, 3, 7),
    nrow = 2,
    byrow = TRUE
  )
  balances <- c(5, 10)
  result <- .downscale_estimate_matrix(estimates, balances)

  testthat::expect_equal(rowSums(result), c(5, 10))
  testthat::expect_equal(
    result[1, 1],
    5 * 6 / 10,
    tolerance = 1e-6
  )
})

testthat::test_that(".downscale_estimate_matrix leaves small rows unchanged", {
  estimates <- matrix(
    c(2, 1, 3, 4),
    nrow = 2,
    byrow = TRUE
  )
  balances <- c(10, 20)
  result <- .downscale_estimate_matrix(estimates, balances)

  testthat::expect_equal(result, estimates)
})

testthat::test_that(".prefer_flow_direction numeric key handles large codes", {
  bilateral_trade <- tibble::tribble(
    ~from_code, ~to_code, ~year, ~item_cbs_code, ~element, ~value,
    999, 998, 2025, 2999, "Import", 10,
    999, 998, 2025, 2999, "Export", 20,
    1,   2,   1961, 2500, "Import", 5,
  )

  result <- .prefer_flow_direction(bilateral_trade, "Export") |>
    dplyr::arrange(from_code, to_code, year, item_cbs_code)

  testthat::expect_equal(nrow(result), 2)
  testthat::expect_equal(result$value, c(5, 20))
})

testthat::test_that(".prefer_flow_direction keeps all when no conflict", {
  bilateral_trade <- tibble::tribble(
    ~from_code, ~to_code, ~year, ~item_cbs_code, ~element, ~value,
    1, 2, 2000, 1, "Export", 10,
    3, 4, 2000, 1, "Import", 20,
    5, 6, 2001, 2, "Export", 30,
  )

  result <- .prefer_flow_direction(bilateral_trade, "Export")
  testthat::expect_equal(nrow(result), 3)
})

testthat::test_that(".ipf_2d converges on larger matrix", {
  set.seed(42)
  n <- 50
  seed <- matrix(abs(rnorm(n * n)), nrow = n, ncol = n)
  target_rows <- abs(rnorm(n, 100, 30))
  target_cols <- target_rows * sum(target_rows) / sum(target_rows)
  # Rescale so sums match
  target_cols <- target_cols * sum(target_rows) / sum(target_cols)

  result <- .ipf_2d(seed, target_rows, target_cols)

  testthat::expect_equal(rowSums(result), target_rows, tolerance = 0.1)
  testthat::expect_equal(colSums(result), target_cols, tolerance = 0.1)
})

testthat::test_that(".balance_matrix returns zero matrix when all trade is zero", {
  total_trade <- tibble::tibble(
    area_code = c(1, 2, 3),
    export = c(0, 0, 0),
    import = c(0, 0, 0)
  ) |>
    .balance_total_trade()

  trade_matrix <- matrix(0, nrow = 3, ncol = 3)
  result <- .balance_matrix(trade_matrix, total_trade)

  testthat::expect_equal(result, matrix(0, nrow = 3, ncol = 3))
})

testthat::test_that(".balance_matrix keeps dimnames when all trade is zero", {
  # Regression for #235: the all-zero short circuit used to return a matrix
  # with no dimnames, so name indexing (the documented contract) failed.
  codes <- c("1", "2", "3")
  total_trade <- tibble::tibble(
    area_code = factor(codes),
    export = c(0, 0, 0),
    import = c(0, 0, 0)
  ) |>
    .balance_total_trade()

  trade_matrix <- matrix(
    0,
    nrow = 3,
    ncol = 3,
    dimnames = list(codes, codes)
  )

  result <- .balance_matrix(trade_matrix, total_trade)

  testthat::expect_equal(dimnames(result), list(codes, codes))
  testthat::expect_equal(result["1", "2"], 0)
})

testthat::test_that(".balance_matrix handles single active country", {
  total_trade <- tibble::tibble(
    area_code = c(1, 2, 3),
    export = c(10, 0, 0),
    import = c(0, 10, 0)
  ) |>
    .balance_total_trade()

  trade_matrix <- matrix(1, nrow = 3, ncol = 3)
  result <- .balance_matrix(trade_matrix, total_trade)

  testthat::expect_equal(
    sum(result),
    sum(total_trade$balanced_export),
    tolerance = 0.1
  )
})

testthat::test_that(".estimate_bilateral_trade uses tcrossprod correctly", {
  exports <- c(10, 20)
  imports <- c(5, 15)
  result <- .estimate_bilateral_trade(exports, imports)

  sum_exp <- sum(exports)
  sum_imp <- sum(imports)
  scale <- (1 / sum_imp + 1 / sum_exp) / 2
  expected <- outer(exports, imports) * scale

  testthat::expect_equal(result, expected, tolerance = 1e-10)
})

testthat::test_that(".fill_missing_trade handles all-NA matrix", {
  original <- matrix(NA_real_, nrow = 3, ncol = 3)
  total_trade <- tibble::tribble(
    ~area_code, ~export, ~import,
    1, 100, 80,
    2, 50, 120,
    3, 50, 0
  ) |>
    .balance_total_trade()

  result <- .fill_missing_trade(original, total_trade)

  testthat::expect_true(all(!is.na(result)))
  testthat::expect_true(all(result >= 0))
})


testthat::test_that(".balance_total_trade rescales larger side", {
  total_trade <- tibble::tribble(
    ~area_code, ~export, ~import,
    1, 100, 50,
    2, 200, 100
  )
  result <- .balance_total_trade(total_trade)

  pointblank::expect_col_exists(
    result,
    columns = c("balanced_export", "balanced_import")
  )
  testthat::expect_equal(
    sum(result$balanced_export),
    sum(result$balanced_import),
    tolerance = 1e-6
  )
  testthat::expect_equal(
    result$balanced_import,
    c(50, 100)
  )
})

# .mass_only_bilateral_trade / .nest_by_year_item_code -------------------------

# One tonnes item (2511) and one live-animal item (1049) whose CBS margins are
# head counts, as `get_livestock_cbs()` emits them.
.head_margin_cbs_fixture <- function() {
  tibble::tribble(
    ~year, ~item_cbs_code, ~area_code, ~unit,    ~export, ~import,
    2010,  2511,           10L,        "tonnes", 100,     0,
    2010,  2511,           20L,        "tonnes", 0,       100,
    2010,  1049,           10L,        "heads",  5000000, 0,
    2010,  1049,           20L,        "heads",  0,       5000000,
  )
}

testthat::test_that(".mass_only_bilateral_trade drops non-tonnes rows with a warning", {
  btd <- tibble::tribble(
      ~item_cbs_code, ~unit,    ~value,
      1,              "tonnes", 100,
      1,              "heads",  5000000,
      2,              "tonnes", 40,
    )

  testthat::expect_warning(
    result <- .mass_only_bilateral_trade(btd),
    "not denominated in mass"
  )

  testthat::expect_false("unit" %in% names(result))
  testthat::expect_equal(nrow(result), 2)
  testthat::expect_equal(sum(result$value), 140)
})

testthat::test_that(".mass_only_bilateral_trade keeps quiet if all tonnes", {
  btd <- tibble::tribble(
    ~item_cbs_code, ~unit,    ~value,
    1,              "tonnes", 100,
    2,              "tonnes", 40,
  )

  result <- testthat::expect_no_warning(.mass_only_bilateral_trade(btd))

  testthat::expect_equal(sum(result$value), 140)
})

testthat::test_that(
  paste(
    ".nest_by_year_item_code does not sum head counts into the tonnes",
    "column (whep#962)"
  ),
  {
    # Regression for whep#962: .build_trade_matrix() sums `value` with no
    # unit dimension, so a head-denominated row used to be added straight
    # into the tonnes-denominated bilateral trade matrix.
    btd <- tibble::tribble(
      ~year, ~item_cbs_code, ~from_code, ~to_code, ~unit, ~value,
      2010,  1,              10L,        20L,      "tonnes", 100,
      2010,  1,              10L,        20L,      "heads",  5000000,
    )
    cbs <- tibble::tribble(
      ~year, ~item_cbs_code, ~area_code, ~unit,    ~export, ~import,
      2010,  1,              10L,        "tonnes", 100,     0,
      2010,  1,              20L,        "tonnes", 0,       100,
    )
    codes <- factor(c(10L, 20L))

    testthat::expect_warning(
      result <- .nest_by_year_item_code(btd, cbs, codes, "drop", "tonnes"),
      "not denominated in mass"
    )

    testthat::expect_equal(sum(result$bilateral_trade[[1]]$value), 100)
  }
)

testthat::test_that(
  paste(
    ".nest_by_year_item_code will not lose a head-only matrix in",
    "silence (whep#962)"
  ),
  {
    # The mass-only filter empties a year-item group whose whole seed is head
    # counts, and the inner join then drops that group from the result. On the
    # 20250714 pin that is 283 groups over 11 live-animal items and 1986-2013,
    # 80,104 observed seed cells. Those items are balanced onto head-count
    # targets from the same rows, so what goes is real partner structure and
    # not a unit mixup -- it must at least be visible.
    btd <- tibble::tribble(
      ~year, ~item_cbs_code, ~from_code, ~to_code, ~unit,    ~value,
      2010,  2511,           10L,        20L,      "tonnes", 100,
      2010,  1049,           10L,        20L,      "heads",  5000000,
    )
    cbs <- .head_margin_cbs_fixture()
    codes <- factor(c(10L, 20L))

    testthat::expect_warning(
      testthat::expect_warning(
        result <- .nest_by_year_item_code(btd, cbs, codes, "drop", "tonnes"),
        "not denominated in mass"
      ),
      "lost every seed cell"
    )

    # The group really is gone: the warning is the only trace it leaves.
    testthat::expect_equal(result$item_cbs_code, 2511)
  }
)

# method_seed_unit (whep#1031) -------------------------------------------------

testthat::test_that("the 'target' seed keeps a head-only matrix and labels it in heads", {
  # The 283 head-only groups of the 20250714 pin: seed and margins are both
  # head counts, so the observed partner structure must survive.
  btd <- tibble::tribble(
      ~year, ~item_cbs_code, ~from_code, ~to_code, ~unit,    ~value,
      2010,  2511,           10L,        20L,      "tonnes", 100,
      2010,  1049,           10L,        20L,      "heads",  5000000,
    )
  codes <- factor(c(10L, 20L))

  result <- testthat::expect_no_warning(
    .nest_by_year_item_code(btd, .head_margin_cbs_fixture(), codes)
  )

  testthat::expect_equal(result$item_cbs_code, c(2511, 1049))
  testthat::expect_equal(result$unit, c("tonnes", "heads"))
  testthat::expect_equal(result$method_seed_unit, c("target", "target"))
  testthat::expect_equal(sum(result$bilateral_trade[[2]]$value), 5000000)
})

testthat::test_that("the 'target' seed drops rows in the other unit, loudly", {
  # From 2014 FAOSTAT reports live animals in both units. The tonnes row of
  # a head-count matrix is the one that goes, and the head count, not the
  # sum of the two, is what seeds it.
  btd <- tibble::tribble(
      ~year, ~item_cbs_code, ~from_code, ~to_code, ~unit,    ~value,
      2010,  2511,           10L,        20L,      "tonnes", 100,
      2010,  1049,           10L,        20L,      "heads",  5000000,
      2010,  1049,           10L,        20L,      "tonnes", 2500,
    )
  codes <- factor(c(10L, 20L))

  testthat::expect_warning(
    result <- .nest_by_year_item_code(btd, .head_margin_cbs_fixture(), codes),
    "not in the unit of the"
  )

  heads <- dplyr::filter(result, item_cbs_code == 1049)
  testthat::expect_equal(heads$unit, "heads")
  testthat::expect_equal(heads$bilateral_trade[[1]]$value, 5000000)
})

testthat::test_that("a 'tonnes'-seeded matrix balanced onto head counts is labelled heads", {
  # IPF imposes the margins' level, so whatever seeds it, a matrix
  # balanced onto head counts holds head counts.
  btd <- tibble::tribble(
      ~year, ~item_cbs_code, ~from_code, ~to_code, ~unit,    ~value,
      2010,  1049,           10L,        20L,      "heads",  5000000,
      2010,  1049,           10L,        20L,      "tonnes", 2500,
    )
  codes <- factor(c(10L, 20L))

  testthat::expect_warning(
    nested <- .nest_by_year_item_code(
      btd,
      .head_margin_cbs_fixture(),
      codes,
      "drop",
      "tonnes"
    ),
    "not denominated in mass"
  )
  result <- .process_bilateral_trade(nested, codes)

  heads <- dplyr::filter(result, item_cbs_code == 1049)
  testthat::expect_equal(heads$unit, "heads")
  testthat::expect_equal(heads$method_seed_unit, "tonnes")
  testthat::expect_equal(sum(heads$bilateral_trade[[1]]), 5000000)
})

testthat::test_that("the two seed methods agree on every tonnes matrix", {
  fixture <- .worker_invariance_fixture()
  run <- function(seed_method) {
    fixture$btd |>
      .nest_by_year_item_code(
        fixture$cbs,
        fixture$codes,
        "drop",
        seed_method
      ) |>
      .process_bilateral_trade(fixture$codes) |>
      dplyr::select(-method_seed_unit)
  }

  testthat::expect_identical(run("target"), run("tonnes"))
})

testthat::test_that("an unanchored 'keep' matrix is seeded and labelled tonnes", {
  btd <- tibble::tribble(
    ~year, ~item_cbs_code, ~from_code, ~to_code, ~unit,    ~value,
    2010,  2511,           10L,        20L,      "tonnes", 100,
    2010,  750,            10L,        20L,      "tonnes", 750,
    2010,  750,            20L,        10L,      "heads",  9e6,
  )
  codes <- factor(c(10L, 20L))

  testthat::expect_warning(
    testthat::expect_warning(
      result <- .nest_by_year_item_code(
        btd,
        dplyr::filter(.head_margin_cbs_fixture(), item_cbs_code == 2511),
        codes,
        "keep"
      ),
      "no commodity balance"
    ),
    "not in the unit of the"
  )

  kept <- dplyr::filter(result, item_cbs_code == 750)
  testthat::expect_false(kept$has_cbs_totals)
  testthat::expect_equal(kept$unit, "tonnes")
  testthat::expect_equal(kept$bilateral_trade[[1]]$value, 750)
})

testthat::test_that("the seed refuses CBS margins it cannot read a unit from", {
  btd <- tibble::tribble(
    ~year, ~item_cbs_code, ~from_code, ~to_code, ~unit,    ~value,
    2010,  2511,           10L,        20L,      "tonnes", 100,
  )
  codes <- factor(c(10L, 20L))
  unlabelled <- dplyr::select(.head_margin_cbs_fixture(), -unit)
  mixed <- .head_margin_cbs_fixture() |>
    dplyr::mutate(unit = c("tonnes", "heads", "heads", "heads"))

  testthat::expect_error(
    .nest_by_year_item_code(btd, unlabelled, codes),
    "no .*unit.* column"
  )
  testthat::expect_error(.nest_by_year_item_code(btd, mixed, codes))
})

testthat::test_that("get_bilateral_trade rejects an unknown seed method", {
  testthat::expect_error(
    get_bilateral_trade(method_seed_unit = "liveweight"),
    class = "rlang_error"
  )
})

# .match_btd_item_codes / .clean_bilateral_trade -------------------------------

testthat::test_that(".match_btd_item_codes resolves CBS item names", {
  items <- whep::items_cbs |>
    dplyr::slice_head(n = 3)

  testthat::expect_equal(
    .match_btd_item_codes(items$item_cbs_name),
    items$item_cbs_code
  )
})

testthat::test_that(".match_btd_item_codes warns on unmatched items", {
  known_name <- whep::items_cbs$item_cbs_name[[1]]
  known_code <- whep::items_cbs$item_cbs_code[[1]]
  # "Wheat" is a raw FAOSTAT trade name; the CBS name is "Wheat and products".
  item <- c(known_name, "Wheat", "Wheat", "Maize (corn)")

  testthat::expect_warning(
    codes <- .match_btd_item_codes(item),
    "3 rows will be dropped"
  )
  testthat::expect_equal(codes, c(known_code, NA, NA, NA))
})

testthat::test_that(".clean_bilateral_trade resolves every pin item", {
  # The `bilateral_trade` pin ships pre-harmonized CBS item names, so the
  # name match must resolve every row. A refreshed pin carrying raw FAOSTAT
  # trade names ("Wheat" instead of "Wheat and products") would trip this.
  btd <- tibble::tribble(
    ~Year, ~area_code, ~area_code_p, ~Element,  ~item,                 ~Unit,    ~Value,
    2010,  68,         203,          "Export",  "Wheat and products",  "tonnes", 5,
    2010,  203,        68,           "Import",  "Barley and products", "tonnes", 7,
    2010,  68,         203,          "Export",  "Sheep",               "Head",   3
  )

  result <- testthat::expect_no_warning(.clean_bilateral_trade(btd))

  testthat::expect_false(any(is.na(result$item_cbs_code)))
  testthat::expect_equal(sort(result$unit), c("heads", "tonnes", "tonnes"))
  testthat::expect_equal(nrow(result), 3)
})

testthat::test_that(".clean_bilateral_trade reads both published schemas", {
  # The live pin and `build_detailed_trade()` share exactly one column name,
  # `area_code` (whep#1122): the pin is CamelCase, keys the partner as
  # `area_code_p` and carries a CBS item *name*; the producer is snake_case,
  # keys the partner as `area_code_partner` and carries `item_cbs_code`. The
  # same flows expressed either way must clean to the same six columns, or a
  # regenerated pin cannot be published without breaking its reader.
  pin <- tibble::tribble(
    ~Year, ~area_code, ~area_code_p, ~Element,  ~item,                 ~Unit,    ~Value,
    2010,  68,         203,          "Export",  "Wheat and products",  "tonnes", 5,
    2010,  203,        68,           "Import",  "Barley and products", "tonnes", 7,
    2010,  68,         203,          "Export",  "Sheep",               "Head",   3
  )
  producer <- tibble::tribble(
    ~year, ~area_code, ~area_code_partner, ~element, ~item_cbs, ~item_cbs_code, ~unit,    ~value,
    2010,  68,         203,                "export", "Wheat and products",  2511, "tonnes", 5,
    2010,  203,        68,                 "import", "Barley and products", 2513, "tonnes", 7,
    2010,  68,         203,                "export", "Sheep",                976, "heads",  3
  )

  from_pin <- testthat::expect_no_warning(.clean_bilateral_trade(pin))
  from_producer <- testthat::expect_no_warning(
    .clean_bilateral_trade(producer)
  )

  testthat::expect_equal(from_producer, from_pin)
})

testthat::test_that(".clean_bilateral_trade prefers exports in either case", {
  # `Element` is capitalised on the pin and lower case on the producer, and
  # the export-preferred deduplication keys on it. Matching only one spelling
  # would keep both sides of every mirrored flow.
  producer <- tibble::tribble(
    ~year, ~area_code, ~area_code_partner, ~element, ~item_cbs_code, ~unit,    ~value,
    2010,  68,         203,                "export", 2511,           "tonnes", 5,
    2010,  203,        68,                 "import", 2511,           "tonnes", 9
  )

  result <- .clean_bilateral_trade(producer)

  testthat::expect_equal(nrow(result), 1)
  testthat::expect_equal(result$value, 5)
  testthat::expect_equal(result$from_code, 68L)
  testthat::expect_equal(result$to_code, 203L)
})

testthat::test_that(".clean_bilateral_trade reports a missing item code", {
  # An `NA` code is dropped by the CBS inner join exactly as an unresolved
  # item name is, so the code-keyed schema must report it too.
  producer <- tibble::tribble(
    ~year, ~area_code, ~area_code_partner, ~element, ~item_cbs_code, ~unit,    ~value,
    2010,  68,         203,                "export", 2511,           "tonnes", 5,
    2010,  68,         203,                "export", NA,             "tonnes", 7
  )

  testthat::expect_warning(
    .clean_bilateral_trade(producer),
    class = "whep_btd_item_code_missing"
  )
})

testthat::test_that(".clean_bilateral_trade refuses an unknown schema", {
  no_partner <- tibble::tribble(
    ~year, ~area_code, ~element, ~item_cbs_code, ~unit,    ~value,
    2010,  68,         "export", 2511,           "tonnes", 5
  )
  no_item <- tibble::tribble(
    ~year, ~area_code, ~area_code_partner, ~element, ~unit,    ~value,
    2010,  68,         203,                "export", "tonnes", 5
  )

  testthat::expect_error(
    .clean_bilateral_trade(no_partner),
    class = "whep_btd_schema"
  )
  testthat::expect_error(
    .clean_bilateral_trade(no_item),
    class = "whep_btd_schema"
  )
})

testthat::test_that("the dropped-tonnage report ignores head-count rows", {
  # The CBS-item filter runs before `.mass_only_bilateral_trade()` (whep#962,
  # PR #1017), so its frame still carries `heads`. Summing those as tonnes
  # would be the collapse of whep#865/#962 reappearing inside the very
  # warning that reports the loss.
  btd <- tibble::tribble(
    ~item_cbs_code, ~unit,    ~value,
    2511,           "tonnes",    250,
    5001,           "tonnes",    750,
    5001,           "heads",  1e9
  )
  cbs <- tibble::tibble(item_cbs_code = 2511)

  # 750 of 1000 tonnes is 75 percent; including the billion head would
  # report ~100 percent instead.
  testthat::expect_warning(
    .filter_only_items_in_cbs(btd, cbs),
    "75 percent"
  )
})
