testthat::test_that(".use_crop_process_cbs_item keeps crop process codes", {
  crop_residues <- tibble::tribble(
    ~item_cbs_code_crop, ~value,
    328, 100,
    248, 200,
    254, 300,
    310, 350,
    772, 360,
    776, 370,
    999, 400
  )

  result <- .use_crop_process_cbs_item(crop_residues)

  expected_codes <- result |>
    dplyr::pull(item_cbs_code_crop)
  testthat::expect_equal(
    expected_codes,
    c(328, 248, 254, 310, 772, 776, 999)
  )
  testthat::expect_equal(result$value, c(100, 200, 300, 350, 360, 370, 400))
})

testthat::test_that(".use_crop_process_cbs_item leaves non-mapped codes unchanged", {
  crop_residues <- tibble::tibble(
    item_cbs_code_crop = c(1000, 2000, 3000),
    value = c(10, 20, 30)
  )

  result <- .use_crop_process_cbs_item(crop_residues)

  testthat::expect_equal(
    result$item_cbs_code_crop,
    c(1000, 2000, 3000)
  )
})
