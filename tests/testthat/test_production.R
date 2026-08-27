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

# get_primary_production / get_primary_residues --------------------------------

testthat::test_that("get_primary_production(example = TRUE) needs no remote", {
  out <- whep::get_primary_production(example = TRUE)

  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_true(nrow(out) > 0)
  testthat::expect_true(
    all(
      c("year", "area_code", "item_prod_code", "unit", "value") %in%
        names(out)
    )
  )
})

# The `crop_residues` pin, in the mixed-case schema the builder lowercases.
# "Tanzania" is deliberately a label the polity crosswalk does not hold, which
# is how the unresolved-area branch is reached.
residues_pin_fixture <- function() {
  tibble::tribble(
    ~Area,      ~Product_residue, ~Item_cbs,             ~Prod_ygpit_Mg,
    "Spain",    "Residue",        "Straw",               100,
    "Spain",    "Residue",        "Straw",               50,
    "Spain",    "Product",        "Straw",               999,
    "Spain",    "Residue",        "Other crop residues", 0,
    "Tanzania", "Residue",        "Straw",               7
  ) |>
    dplyr::mutate(Year = 2000L, Item_cbs_crop = "Wheat and products")
}

testthat::test_that("get_primary_residues aggregates residues on codes", {
  local_mocked_bindings(whep_read_file = function(name, ...) {
    residues_pin_fixture()
  })

  testthat::expect_warning(
    out <- whep::get_primary_residues(),
    "crop-residue"
  )

  spain <- out |> dplyr::filter(area_code == 203L)
  # The two Spanish straw rows are summed, the "Product" row (999) is not a
  # residue and the zero-tonne residue row is dropped.
  testthat::expect_equal(nrow(spain), 1)
  testthat::expect_equal(spain$value, 150)
  testthat::expect_equal(spain$item_cbs_code_crop, 2511)
  testthat::expect_equal(spain$item_cbs_code_residue, 2105)
  testthat::expect_false(any(out$value == 999))
})

testthat::test_that("get_primary_residues ignores NA rows within a group", {
  # whep#167: `prod_ygpit_mg` carries real NAs in the source pin (2,898 of
  # 475,688 rows measured on the current pin). Without `na.rm = TRUE`, one NA
  # sibling turns the whole `(year, area_code, item_cbs_code_crop,
  # item_cbs_code_residue)` sum into NA, and `filter(value > 0)` then silently
  # drops the group -- erasing the real, non-NA rows summed into it, not just
  # the missing one.
  local_mocked_bindings(whep_read_file = function(name, ...) {
    tibble::tribble(
      ~Area,   ~Product_residue, ~Item_cbs, ~Prod_ygpit_Mg,
      "Spain", "Residue",        "Straw",   100,
      "Spain", "Residue",        "Straw",   NA_real_
    ) |>
      dplyr::mutate(Year = 2000L, Item_cbs_crop = "Wheat and products")
  })

  out <- whep::get_primary_residues()

  spain <- out |> dplyr::filter(area_code == 203L)
  # The valid 100-tonne row must survive; only the NA sibling is ignored.
  testthat::expect_equal(nrow(spain), 1)
  testthat::expect_equal(spain$value, 100)
})

testthat::test_that("get_primary_residues keeps unresolved areas visible", {
  local_mocked_bindings(whep_read_file = function(name, ...) {
    residues_pin_fixture()
  })

  out <- suppressWarnings(whep::get_primary_residues())

  # The row whose area label did not resolve is reported, not dropped, so the
  # gap stays visible downstream instead of silently shrinking the totals.
  unresolved <- out |> dplyr::filter(is.na(area_code))
  testthat::expect_equal(unresolved$value, 7)
  testthat::expect_true(is.na(unresolved$reporting_polity_code))
  testthat::expect_equal(sum(out$value), 157)
})

testthat::test_that("get_primary_residues(example = TRUE) needs no remote", {
  out <- whep::get_primary_residues(example = TRUE)

  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_true(
    all(c("year", "area_code", "item_cbs_code_crop", "value") %in% names(out))
  )
})
