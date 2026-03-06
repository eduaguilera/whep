# Example output tests ---------------------------------------------------------
# These call exported functions with example = TRUE to verify the
# example tibbles returned by the .example_* helpers in toy_examples.R.

testthat::test_that("build_supply_use example returns valid tibble", {
  result <- build_supply_use(example = TRUE)

  testthat::expect_s3_class(result, "tbl_df")
  testthat::expect_equal(nrow(result), 10)
  pointblank::expect_col_exists(
    result,
    columns = c(
      "year",
      "area_code",
      "proc_group",
      "proc_cbs_code",
      "item_cbs_code",
      "type",
      "value"
    )
  )
  pointblank::expect_col_vals_in_set(
    result,
    columns = "type",
    set = c("use", "supply")
  )
  pointblank::expect_col_vals_in_set(
    result,
    columns = "proc_group",
    set = c("husbandry", "crop_production", "processing")
  )
  pointblank::expect_col_vals_not_null(result, "year")
  pointblank::expect_col_vals_not_null(result, "value")
})

testthat::test_that("get_feed_intake example returns valid tibble", {
  result <- get_feed_intake(example = TRUE)

  testthat::expect_s3_class(result, "tbl_df")
  testthat::expect_equal(nrow(result), 10)
  pointblank::expect_col_exists(
    result,
    columns = c(
      "year",
      "area_code",
      "live_anim_code",
      "item_cbs_code",
      "feed_type",
      "supply",
      "intake",
      "intake_dry_matter",
      "loss",
      "loss_share"
    )
  )
  pointblank::expect_col_vals_not_null(result, "year")
  pointblank::expect_col_vals_not_null(result, "supply")
})

testthat::test_that("get_primary_production example returns valid tibble", {
  result <- get_primary_production(example = TRUE)

  testthat::expect_s3_class(result, "tbl_df")
  testthat::expect_equal(nrow(result), 10)
  pointblank::expect_col_exists(
    result,
    columns = c(
      "year",
      "area_code",
      "item_prod_code",
      "item_cbs_code",
      "live_anim_code",
      "unit",
      "value"
    )
  )
  pointblank::expect_col_vals_in_set(
    result,
    columns = "unit",
    set = c(
      "tonnes",
      "ha",
      "t_ha",
      "heads",
      "LU",
      "t_head",
      "t_LU"
    )
  )
  pointblank::expect_col_vals_not_null(result, "value")
})

testthat::test_that("get_primary_residues example returns valid tibble", {
  result <- get_primary_residues(example = TRUE)

  testthat::expect_s3_class(result, "tbl_df")
  testthat::expect_equal(nrow(result), 10)
  pointblank::expect_col_exists(
    result,
    columns = c(
      "year",
      "area_code",
      "item_cbs_code_crop",
      "item_cbs_code_residue",
      "value"
    )
  )
  pointblank::expect_col_vals_not_null(result, "value")
})

testthat::test_that("get_processing_coefs example returns valid tibble", {
  result <- get_processing_coefs(example = TRUE)

  testthat::expect_s3_class(result, "tbl_df")
  testthat::expect_equal(nrow(result), 10)
  pointblank::expect_col_exists(
    result,
    columns = c(
      "year",
      "area_code",
      "item_cbs_code_to_process",
      "value_to_process",
      "item_cbs_code_processed",
      "initial_conversion_factor",
      "initial_value_processed",
      "conversion_factor_scaling",
      "final_conversion_factor",
      "final_value_processed"
    )
  )
  pointblank::expect_col_vals_not_null(
    result,
    "final_value_processed"
  )
})

testthat::test_that("get_wide_cbs example returns valid tibble", {
  result <- get_wide_cbs(example = TRUE)

  testthat::expect_s3_class(result, "tbl_df")
  testthat::expect_equal(nrow(result), 10)
  pointblank::expect_col_exists(
    result,
    columns = c(
      "year",
      "area_code",
      "item_cbs_code",
      "domestic_supply",
      "food",
      "production",
      "feed",
      "seed",
      "import",
      "export",
      "other_uses",
      "processing",
      "stock_retrieval"
    )
  )
  pointblank::expect_col_vals_not_null(result, "year")
  pointblank::expect_col_vals_not_null(result, "production")
})

testthat::test_that("create_n_prov_destiny example returns valid tibble", {
  result <- create_n_prov_destiny(example = TRUE)

  testthat::expect_s3_class(result, "tbl_df")
  testthat::expect_equal(nrow(result), 10)
  pointblank::expect_col_exists(
    result,
    columns = c(
      "year",
      "province_name",
      "item",
      "irrig_cat",
      "box",
      "origin",
      "destiny",
      "mg_n"
    )
  )
  pointblank::expect_col_vals_not_null(result, "year")
  pointblank::expect_col_vals_not_null(result, "mg_n")
})

testthat::test_that("create_n_nat_destiny example returns valid tibble", {
  result <- create_n_nat_destiny(example = TRUE)

  testthat::expect_s3_class(result, "tbl_df")
  testthat::expect_equal(nrow(result), 10)
  pointblank::expect_col_exists(
    result,
    columns = c(
      "year",
      "item",
      "irrig_cat",
      "box",
      "origin",
      "destiny",
      "mg_n",
      "province_name"
    )
  )
  pointblank::expect_col_vals_not_null(result, "mg_n")
})

testthat::test_that("create_n_soil_inputs example returns valid tibble", {
  result <- create_n_soil_inputs(example = TRUE)

  testthat::expect_s3_class(result, "tbl_df")
  testthat::expect_equal(nrow(result), 10)
  pointblank::expect_col_exists(
    result,
    columns = c(
      "year",
      "province_name",
      "item",
      "irrig_cat",
      "box",
      "deposition",
      "fixation",
      "synthetic",
      "manure",
      "urban"
    )
  )
  pointblank::expect_col_vals_not_null(result, "year")
  pointblank::expect_col_vals_not_null(
    result,
    "deposition"
  )
})

testthat::test_that("create_n_production example returns valid tibble", {
  result <- create_n_production(example = TRUE)

  testthat::expect_s3_class(result, "tbl_df")
  testthat::expect_equal(nrow(result), 10)
  pointblank::expect_col_exists(
    result,
    columns = c(
      "year",
      "province_name",
      "item",
      "box",
      "prod"
    )
  )
  pointblank::expect_col_vals_not_null(result, "prod")
})

testthat::test_that("calculate_nue_crops example returns valid tibble", {
  result <- calculate_nue_crops(example = TRUE)

  testthat::expect_s3_class(result, "tbl_df")
  testthat::expect_equal(nrow(result), 10)
  pointblank::expect_col_exists(
    result,
    columns = c(
      "year",
      "province_name",
      "item",
      "box",
      "nue"
    )
  )
  pointblank::expect_col_vals_not_null(result, "nue")
})

testthat::test_that("calculate_nue_livestock example returns valid tibble", {
  result <- calculate_nue_livestock(example = TRUE)

  testthat::expect_s3_class(result, "tbl_df")
  testthat::expect_equal(nrow(result), 10)
  pointblank::expect_col_exists(
    result,
    columns = c(
      "year",
      "province_name",
      "livestock_cat",
      "item",
      "prod_n",
      "feed_n",
      "excretion_n",
      "nue",
      "mass_balance"
    )
  )
  pointblank::expect_col_vals_not_null(result, "nue")
})

testthat::test_that("calculate_system_nue example returns valid tibble", {
  result <- calculate_system_nue(example = TRUE)

  testthat::expect_s3_class(result, "tbl_df")
  testthat::expect_equal(nrow(result), 10)
  pointblank::expect_col_exists(
    result,
    columns = c(
      "year",
      "province_name",
      "total_prod",
      "inputs",
      "nue_system"
    )
  )
  pointblank::expect_col_vals_not_null(
    result,
    "nue_system"
  )
})
