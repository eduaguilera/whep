testthat::test_that(".use_seed_cbs_item remaps known codes", {
  crop_residues <- tibble::tribble(
    ~item_cbs_code_crop, ~value,
    328, 100,
    248, 200,
    254, 300,
    999, 400
  )

  result <- .use_seed_cbs_item(crop_residues)

  expected_codes <- result |>
    dplyr::pull(item_cbs_code_crop)
  testthat::expect_equal(
    expected_codes, c(2559, 2560, 2562, 999)
  )
  testthat::expect_equal(result$value, c(100, 200, 300, 400))
})

testthat::test_that(
  ".use_seed_cbs_item leaves non-mapped codes unchanged",
  {
    crop_residues <- tibble::tibble(
      item_cbs_code_crop = c(1000, 2000, 3000),
      value = c(10, 20, 30)
    )

    result <- .use_seed_cbs_item(crop_residues)

    testthat::expect_equal(
      result$item_cbs_code_crop, c(1000, 2000, 3000)
    )
  }
)
