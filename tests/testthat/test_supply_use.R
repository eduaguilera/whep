.expect_equal_unordered <- function(actual, expected) {
  actual <- actual |>
    dplyr::select(sort(names(actual))) |>
    dplyr::arrange(dplyr::across(everything()))

  expected <- expected |>
    dplyr::select(sort(names(actual))) |>
    dplyr::arrange(dplyr::across(everything()))

  testthat::expect_equal(actual, expected)
}

testthat::test_that(".build_processing works for processed items", {
  coeffs <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code_to_process, ~value_to_process,
    ~item_cbs_code_processed, ~final_value_processed,
    2000, 1, 1, 10, 2, 20,
    2000, 1, 1, 10, 3, 30,
    2000, 1, 4, 20, 2, 40,
    2000, 2, 5, 30, 6, 20
  )

  expected <- tibble::tribble(
    ~year, ~area_code, ~proc_group, ~proc_cbs_code, ~item_cbs_code,
    ~value, ~type,
    2000, 1, "processing", 1, 2, 20, "supply",
    2000, 1, "processing", 1, 3, 30, "supply",
    2000, 1, "processing", 1, 1, 10, "use",
    2000, 1, "processing", 4, 2, 40, "supply",
    2000, 1, "processing", 4, 4, 20, "use",
    2000, 2, "processing", 5, 6, 20, "supply",
    2000, 2, "processing", 5, 5, 30, "use"
  )

  coeffs |>
    .build_processing() |>
    .expect_equal_unordered(expected)
})

testthat::test_that(".build_use_husbandry gives feed intake needs for animal husbandry", {
  feed_intake <- tibble::tribble(
    ~year, ~area_code, ~live_anim_code, ~item_cbs_code, ~supply,
    2000, 1, 1, 2, 20,
    2000, 1, 1, 3, 40,
    2000, 2, 4, 6, 40,
    2000, 2, 5, 6, 20,
    2001, 2, 5, 7, 20
  )

  expected <- tibble::tribble(
    ~year, ~area_code, ~proc_cbs_code, ~item_cbs_code, ~value, ~type,
    2000, 1, 1, 2, 20, "use",
    2000, 1, 1, 3, 40, "use",
    2000, 2, 4, 6, 40, "use",
    2000, 2, 5, 6, 20, "use",
    2001, 2, 5, 7, 20, "use"
  )

  feed_intake |>
    .build_use_husbandry() |>
    .expect_equal_unordered(expected)
})

testthat::test_that(".build_supply_husbandry gives livestock and their products", {
  husbandry_items <- tibble::tibble(live_anim_code = c(1, 2, 4))

  primary_prod <- tibble::tribble(
    ~year, ~area_code, ~item_prod_code, ~item_cbs_code, ~live_anim_code,
    ~unit, ~value,
    2000, 1, 1, 2, NA, "LU", 20,
    2000, 1, 3, 4, NA, "LU", 30,
    2000, 1, 5, 6, 1, "tonnes", 40,
    2000, 1, 7, 8, 1, "tonnes", 45,
    2001, 1, 1, 2, NA, "LU", 40,
    2001, 1, 7, 8, 1, "tonnes", 50,
    2001, 1, 7, 9, NA, "tonnes", 50,
    2002, 1, 7, 2, 3, "tonnes", 50
  )

  expected <- tibble::tribble(
    ~year, ~area_code, ~proc_cbs_code, ~item_cbs_code, ~value, ~type,
    2000, 1, 2, 2, 13, "supply",
    2000, 1, 4, 4, 19.5, "supply",
    2000, 1, 1, 6, 40, "supply",
    2000, 1, 1, 8, 45, "supply",
    2001, 1, 2, 2, 26, "supply",
    2001, 1, 1, 8, 50, "supply"
  )

  husbandry_items |>
    .build_supply_husbandry(primary_prod) |>
    .expect_equal_unordered(expected)
})

testthat::test_that(".build_use_crop_production gives seed use for crop production", {
  cbs_items <- tibble::tibble(item_cbs_code = c(1, 3, 8))

  cbs <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~seed,
    2000, 1, 1, 20,
    2000, 1, 3, 25,
    2001, 1, 8, 30,
    2001, 1, 9, 30
  )

  expected <- tibble::tribble(
    ~year, ~area_code, ~proc_cbs_code, ~item_cbs_code, ~value, ~type,
    2000, 1, 1, 1, 20, "use",
    2000, 1, 3, 3, 25, "use",
    2001, 1, 8, 8, 30, "use"
  )

  cbs_items |>
    .build_use_crop_production(cbs) |>
    .expect_equal_unordered(expected)
})

testthat::test_that(".build_supply_crop_production gives crops and their residues", {
  crop_prod_items <- tibble::tibble(item_prod_code = c(1, 2, 4))

  primary_prod <- tibble::tribble(
    ~year, ~area_code, ~item_prod_code, ~item_cbs_code, ~live_anim_code,
    ~unit, ~value,
    2000, 1, 1, 3, NA, "tonnes", 40,
    2000, 1, 2, 3, NA, "tonnes", 45,
    2000, 1, 4, 5, NA, "tonnes", 50,
    2001, 1, 4, 5, NA, "tonnes", 60,
    2001, 1, 6, 3, NA, "tonnes", 60
  )

  crop_residues <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code_crop, ~item_cbs_code_residue, ~value,
    2000, 1, 3, 10, 40,
    2000, 1, 5, 11, 45,
    2001, 1, 5, 12, 60,
    2001, 1, 4, 10, 80
  )

  expected <- tibble::tribble(
    ~year, ~area_code, ~proc_cbs_code, ~item_cbs_code, ~value, ~type,
    2000, 1, 3, 3, 85, "supply",
    2000, 1, 5, 5, 50, "supply",
    2001, 1, 5, 5, 60, "supply",
    2000, 1, 3, 10, 40, "supply",
    2000, 1, 5, 11, 45, "supply",
    2001, 1, 5, 12, 60, "supply"
  )

  crop_prod_items |>
    .build_supply_crop_production(primary_prod, crop_residues) |>
    .expect_equal_unordered(expected)
})
