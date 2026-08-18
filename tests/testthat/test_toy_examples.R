# Example output tests ---------------------------------------------------------
# These call exported functions with example = TRUE to verify the
# example tibbles returned by the .example_* helpers in toy_examples.R.

testthat::test_that("build_supply_use example returns valid tibble", {
  result <- build_supply_use(example = TRUE)

  testthat::expect_s3_class(result, "tbl_df")
  testthat::expect_equal(nrow(result), 9)
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
  # All five documented process groups, and every (group, type) combination the
  # real builder emits: the fixture used to show three of the five.
  groups <- c(
    "crop_production",
    "husbandry",
    "animal_draught",
    "slaughtering",
    "processing"
  )
  pointblank::expect_col_vals_in_set(
    result,
    columns = "proc_group",
    set = groups
  )
  testthat::expect_setequal(unique(result$proc_group), groups)
  pointblank::expect_col_vals_not_null(result, "year")
  # whep#417: the fixture used to carry a row with no `area_code` at all.
  pointblank::expect_col_vals_not_null(result, "area_code")
  pointblank::expect_col_vals_not_null(result, "value")
})

testthat::test_that("get_feed_intake example returns valid tibble", {
  result <- get_feed_intake(example = TRUE)

  testthat::expect_s3_class(result, "tbl_df")
  testthat::expect_equal(nrow(result), 11)
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
  testthat::expect_true("grass" %in% result$feed_type)
  pointblank::expect_col_vals_not_null(result, "year")
  # whep#417: the fixture used to carry two rows with no `area_code` at all.
  pointblank::expect_col_vals_not_null(result, "area_code")
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
      "stock_withdrawal",
      "stock_addition"
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

testthat::test_that("grafs Spain typologies example returns valid tibble", {
  result <- whep::create_typologies_grafs_spain(example = TRUE)

  testthat::expect_s3_class(result, "tbl_df")
  testthat::expect_equal(nrow(result), 10)
  pointblank::expect_col_exists(
    result,
    columns = c("Province_name", "Typologie")
  )
  pointblank::expect_col_vals_in_set(
    result,
    columns = "Typologie",
    set = c(
      "Specialized cropping system",
      "Extensive cropping system",
      "Extensive mixed crop-livestock system",
      "Intensive mixed crop-livestock system",
      "Specialized livestock-farming system"
    )
  )
})

testthat::test_that("Josette typologies example returns the data elements", {
  result <- whep::create_typologies_of_josette(example = TRUE)

  testthat::expect_type(result, "list")
  testthat::expect_named(
    result,
    c("typologies_df", "n_input_df", "imported_feed_share_df")
  )
  purrr::walk(result, ~ testthat::expect_s3_class(.x, "tbl_df"))
  purrr::walk(result, ~ testthat::expect_equal(nrow(.x), 10))

  pointblank::expect_col_exists(
    result$typologies_df,
    columns = c("Year", "Province_name", "Typology")
  )
  pointblank::expect_col_exists(
    result$n_input_df,
    columns = c(
      "Year",
      "Province_name",
      "item",
      "irrig_cat",
      "Box",
      "MgN_dep",
      "MgN_fix",
      "MgN_syn",
      "MgN_manure",
      "MgN_urban"
    )
  )
  pointblank::expect_col_exists(
    result$imported_feed_share_df,
    columns = c(
      "Year",
      "Province_name",
      "LU_total",
      "Feed_import_MgN",
      "Domestic_feed_MgN",
      "Total_feed_MgN",
      "Imported_feed_share"
    )
  )
  # The share is a share, and the feed total is the sum of its two parts.
  pointblank::expect_col_vals_between(
    result$imported_feed_share_df,
    columns = "Imported_feed_share",
    left = 0,
    right = 1
  )
  testthat::expect_equal(
    result$imported_feed_share_df$Total_feed_MgN,
    result$imported_feed_share_df$Feed_import_MgN +
      result$imported_feed_share_df$Domestic_feed_MgN,
    tolerance = 1e-3
  )
})
