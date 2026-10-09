# items_prod keyed Citrus Fruit, Total on 1807, the code of Sheep and Goat
# Meat (whep issue 1484), so any join by code duplicated those rows.

testthat::test_that("items_prod item_prod_code is unique", {
  testthat::expect_false(anyDuplicated(whep::items_prod$item_prod_code) > 0)
})

testthat::test_that("items_prod codes agree with items_prod_full_raw", {
  raw <- system.file(
    "extdata",
    "harmonization",
    "items_prod_full_raw.csv",
    package = "whep"
  ) |>
    readr::read_csv(show_col_types = FALSE) |>
    dplyr::mutate(item_prod_code = as.character(.data$item_prod_code))
  shared <- whep::items_prod |>
    dplyr::mutate(item_prod_code = as.character(.data$item_prod_code)) |>
    dplyr::inner_join(
      dplyr::distinct(
        raw,
        item_prod_name = .data$item_prod,
        .data$item_prod_code
      ),
      by = "item_prod_name",
      suffix = c("", "_raw")
    )
  testthat::expect_gt(nrow(shared), 0)
  differ <- shared |>
    dplyr::filter(.data$item_prod_code != .data$item_prod_code_raw) |>
    dplyr::pull(.data$item_prod_name)
  # Known, separate disagreement: 17530 here vs 1753 in the raw table.
  testthat::expect_equal(differ, "Fibre Crops, Fibre Equivalent")
})
