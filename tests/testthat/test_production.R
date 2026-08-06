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

testthat::test_that(".warn_residues_no_area names rows that resolved to no area", {
  # `get_primary_residues()` is the only builder that resolves its areas by NAME,
  # through `add_area_code()`, and it left `area_code` as NA wherever no name
  # matched. Those rows travelled to the output with NA polity columns and on into
  # `build_supply_use()` in complete silence. Measured on the current pin: 44,985
  # of 475,688 rows (9.5%) over 14 labels and years 1961-2021, which is 3,937 rows
  # of the builder's own aggregated output.
  #
  # Every one of the 14 is a short form of an area the crosswalk holds under a
  # FAOSTAT long form -- "Tanzania" against "United Republic of Tanzania" -- so the
  # codes are reachable and the spellings are not. Repairing that join is a
  # separate change; this is the diagnostic that says the gap is there.
  dt <- tibble::tribble(
    ~year, ~area, ~area_code,
    1961L, "Tanzania", NA_integer_,
    1962L, "Tanzania", NA_integer_,
    1961L, "Spain", 203L
  )

  testthat::expect_warning(
    result <- .warn_residues_no_area(dt),
    "crop-residue"
  )
  # Reports rather than drops: whether an unattributable residue row should be
  # removed is a modelling question, so the frame comes back untouched.
  testthat::expect_identical(result, dt)
})

testthat::test_that(".warn_residues_no_area stays quiet when every row resolved", {
  dt <- tibble::tribble(
    ~year, ~area, ~area_code,
    1961L, "Spain", 203L
  )

  testthat::expect_no_warning(.warn_residues_no_area(dt))
})
