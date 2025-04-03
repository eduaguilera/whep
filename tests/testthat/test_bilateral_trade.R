testthat::test_that("get_bilateral_trade has consistent data", {
  bilateral_trade_alias <- "bilateral_trade"
  test_file_path <- file.path(
    .get_destdir(),
    stringr::str_glue("test_file_{bilateral_trade_alias}.csv")
  )
  testthat::expect_false(file.exists(test_file_path))
  testthat::local_mocked_bindings(.get_destfile = function(...) test_file_path)

  bilateral_trade_alias |>
    get_file_path() |>
    get_bilateral_trade() |>
    is.na() |>
    any() |>
    testthat::expect_false()

  file.remove(test_file_path)
})

testthat::test_that(".prefer_flow_direction chooses preferred trade data", {
  bilateral_trade <- tibble::tribble(
    ~from_code, ~to_code, ~year, ~item, ~element, ~value,
    1, 2, 2000, "item_1", "Import", 0,
    1, 2, 2000, "item_1", "Export", 0,
    1, 2, 2000, "item_2", "Export", 0,
    1, 3, 2000, "item_2", "Import", 0,
    2, 3, 2000, "item_2", "Import", 0,
    2, 3, 2001, "item_2", "Export", 0,
    2, 3, 2001, "item_2", "Import", 0,
  ) |>
    dplyr::arrange(from_code, to_code, year, item)

  brute_group_by_result <-
    bilateral_trade |>
    dplyr::group_by(from_code, to_code, year, item) |>
    dplyr::filter(dplyr::n() == 1 | element == "Import") |>
    dplyr::ungroup() |>
    dplyr::arrange(from_code, to_code, year, item)

  my_result <- .prefer_flow_direction(bilateral_trade, "Import") |>
    dplyr::arrange(from_code, to_code, year, item)

  expected_import_result <- tibble::tribble(
    ~from_code, ~to_code, ~year, ~item, ~element, ~value,
    1, 2, 2000, "item_1", "Import", 0,
    1, 2, 2000, "item_2", "Export", 0,
    1, 3, 2000, "item_2", "Import", 0,
    2, 3, 2000, "item_2", "Import", 0,
    2, 3, 2001, "item_2", "Import", 0,
  ) |>
    dplyr::arrange(from_code, to_code, year, item)

  testthat::expect_equal(my_result, brute_group_by_result)
  testthat::expect_equal(my_result, expected_import_result)

  brute_group_by_result <-
    bilateral_trade |>
    dplyr::group_by(from_code, to_code, year, item) |>
    dplyr::filter(dplyr::n() == 1 | element == "Export") |>
    dplyr::ungroup() |>
    dplyr::arrange(from_code, to_code, year, item)

  my_result <- .prefer_flow_direction(bilateral_trade, "Export") |>
    dplyr::arrange(from_code, to_code, year, item)

  expected_export_result <- tibble::tribble(
    ~from_code, ~to_code, ~year, ~item, ~element, ~value,
    1, 2, 2000, "item_1", "Export", 0,
    1, 2, 2000, "item_2", "Export", 0,
    1, 3, 2000, "item_2", "Import", 0,
    2, 3, 2000, "item_2", "Import", 0,
    2, 3, 2001, "item_2", "Export", 0,
  ) |>
    dplyr::arrange(from_code, to_code, year, item)

  testthat::expect_equal(my_result, brute_group_by_result)
  testthat::expect_equal(my_result, expected_export_result)
})
