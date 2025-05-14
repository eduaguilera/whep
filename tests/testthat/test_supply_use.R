testthat::test_that(
  ".add_supply_use_for_processing works for 'processing' use items",
  {
    processes_table <- tibble::tribble(
      ~proc, ~item_to_process, ~item_processed, ~type,
      "p1", "i1", "i2", "processing",
      "p1", "i1", "i3", "processing",
      "p1", "i4", "i2", "processing",
      "p2", "i1", "i5", "processing",
      "ignore", "ignore", "ignore", "seedwaste",
      "ignore", "ignore", "ignore", "feed",
    )
    coeffs <- tibble::tribble(
      ~year, ~area, ~item_to_process, ~value_to_process,
      ~item_processed, ~final_value_processed,
      2000, "a1", "i1", 10, "i2", 20,
      2000, "a1", "i1", 10, "i3", 30,
      2000, "a1", "i4", 20, "i2", 40,
      2000, "a1", "i1", 30, "i5", 20
    )
    expected <- tibble::tribble(
      ~year, ~area, ~proc, ~item, ~value, ~type,
      2000, "a1", "p1", "i1", 10, "use",
      2000, "a1", "p1", "i4", 20, "use",
      2000, "a1", "p2", "i1", 30, "use",
      2000, "a1", "p1", "i2", 60, "supply",
      2000, "a1", "p1", "i3", 30, "supply",
      2000, "a1", "p2", "i5", 20, "supply",
    ) |>
      dplyr::arrange(year, area, proc, item, type)

    .add_supply_use_for_processing(processes_table, coeffs) |>
      dplyr::arrange(year, area, proc, item, type) |>
      testthat::expect_equal(expected)
  }
)

testthat::test_that(".add_use_for_seed works for 'seedwaste' use items", {
  processes_table <- tibble::tribble(
    ~proc, ~item_to_process, ~item_code_to_process, ~item_processed, ~type,
    "p1", "i1", 1, "i1", "seedwaste",
    "p2", "i2", 2, "i3", "seedwaste",
    "ignore", "ignore", 3, "ignore", "slaughtering",
    "ignore", "ignore", 4, "ignore", "feed",
  )
  cbs <- tibble::tribble(
    ~year, ~area, ~item, ~item_code, ~seed,
    2000, "a1", "i1", 1, 10,
    2000, "a1", "i2", 2, 20,
  )
  expected <- tibble::tribble(
    ~year, ~area, ~proc, ~item, ~value, ~type,
    2000, "a1", "p1", "i1", 10, "use",
    2000, "a1", "p2", "i2", 20, "use",
  ) |>
    dplyr::arrange(year, area, proc, item, type)

  .add_use_for_seed(processes_table, cbs) |>
    dplyr::arrange(year, area, proc, item, type) |>
    testthat::expect_equal(expected)
})
