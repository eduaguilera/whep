# A compact but fully coherent injected input set for the non-example path: one
# country, two cells, two crops, laid out (like the packaged fixture) so every
# module join is non-empty. One crop-cell sits above its critical surplus
# (an Exceedance) and one below it (Within_boundary).
.sjos_nitrogen_test_data <- function() {
  list(
    balance = tibble::tribble(
      ~lon,
      ~lat,
      ~area_code,
      ~item_cbs_code,
      ~year,
      ~area_ha,
      ~n_input_std_t,
      ~prod_n_t,
      ~used_residue_n_t,
      ~grazed_weeds_n_t,
      ~burnt_residue_n_t,
      ~n_balance_t,
      ~nh3_n_t,
      ~no3_n_t,
      0.25, 0.25, 1L, 2511L, 2010L, 100, 50, 20, 5, 0, 0, 25, 3.0, 5.0,
      0.75, 0.25, 1L, 2513L, 2010L, 50, 10, 8, 1, 0, 0, 1, 0.5, 0.8
    ),
    critical = tibble::tribble(
      ~lon, ~lat, ~value,
      0.25, 0.25, 50,
      0.75, 0.25, 120
    ) |>
      dplyr::mutate(
        critical_var = "critical_n_surplus",
        critical_land_use = "ara"
      ),
    critical_loads = list(
      crit_nh3_emission = tibble::tribble(
        ~lon, ~lat, ~value,
        0.25, 0.25, 20,
        0.75, 0.25, 25
      ) |>
        dplyr::mutate(critical_var = "crit_nh3_emission"),
      crit_leaching_gw = tibble::tribble(
        ~lon, ~lat, ~value,
        0.25, 0.25, 30,
        0.75, 0.25, 40
      ) |>
        dplyr::mutate(critical_var = "crit_leaching_gw"),
      crit_load_sw = tibble::tribble(
        ~lon, ~lat, ~value,
        0.25, 0.25, 40,
        0.75, 0.25, 20
      ) |>
        dplyr::mutate(critical_var = "crit_load_sw")
    ),
    cbs_food = tibble::tribble(
      ~year, ~area_code, ~item_cbs_code, ~food_t,
      2010L, 1L, 2511L, 1.5e8,
      2010L, 1L, 2513L, 5.0e7
    ),
    population = tibble::tribble(
      ~year, ~area_code, ~population,
      2010L, 1L, 1.0e9
    ),
    n_inputs = tibble::tribble(
      ~year, ~area_code, ~fert_type, ~n_input_t,
      2010L, 1L, "synthetic", 3.0e7,
      2010L, 1L, "bnf", 1.5e7,
      2010L, 1L, "manure", 1.0e7
    ),
    biomass_coefs = tibble::tribble(
      ~Name_biomass,
      ~Edible_N_kgFM,
      ~N_kgN_kgFM,
      ~Product_kgN_kgDM,
      ~Product_kgDM_kgFM,
      ~GE_product_edible_portion_MJ_kgFM,
      ~GE_product_MJ_kgFM,
      "Wheat", 0.020, NA, NA, NA, 13.0, NA,
      "Barley", 0.018, NA, NA, NA, 12.5, NA
    ),
    items_full = tibble::tribble(
      ~item_cbs_code, ~Name_biomass,
      2511L, "Wheat",
      2513L, "Barley"
    ),
    fp_flows = tibble::tribble(
      ~year,
      ~origin_area,
      ~origin_item,
      ~target_area,
      ~target_item,
      ~target_fd,
      ~value,
      2010L, 1L, 2511L, 1L, 2511L, "food", 20,
      2010L, 1L, 2513L, 1L, 2513L, "food", 0
    )
  )
}

# All seven output tables, treating the nested boundary_surplus and footprint
# lists as leaves so "non-empty" can be asserted on each tibble.
.sjos_nitrogen_tables <- function(out) {
  list(
    surplus = out$surplus,
    boundary_surplus_grid = out$boundary_surplus$grid,
    boundary_surplus_country = out$boundary_surplus$country,
    boundary_pathway = out$boundary_pathway,
    nourishment = out$nourishment,
    scatter = out$scatter,
    sjos_class = out$sjos_class,
    footprint_all = out$footprint$fp_all,
    footprint_food = out$footprint$fp_food
  )
}

testthat::test_that("build_sjos_nitrogen(example = TRUE) returns every table", {
  out <- whep::build_sjos_nitrogen(example = TRUE)
  testthat::expect_named(
    out,
    c(
      "surplus",
      "boundary_surplus",
      "boundary_pathway",
      "nourishment",
      "scatter",
      "sjos_class",
      "footprint"
    )
  )
  testthat::expect_named(out$boundary_surplus, c("grid", "country"))
  testthat::expect_named(out$footprint, c("fp_all", "fp_food"))
})

testthat::test_that("every SJOS-N output table is non-empty", {
  out <- whep::build_sjos_nitrogen(example = TRUE)
  tables <- .sjos_nitrogen_tables(out)
  for (nm in names(tables)) {
    testthat::expect_s3_class(tables[[nm]], "tbl_df")
    testthat::expect_gt(nrow(tables[[nm]]), 0)
  }
})

testthat::test_that("per-crop item_cbs_code survives through the chain", {
  out <- whep::build_sjos_nitrogen(example = TRUE)
  pointblank::expect_col_exists(out$surplus, "item_cbs_code")
  pointblank::expect_col_exists(out$boundary_surplus$grid, "item_cbs_code")
  pointblank::expect_col_exists(out$sjos_class, "item_cbs_code")
  # The footprint extension path carries item_cbs_code to the consumed crop.
  pointblank::expect_col_exists(out$footprint$fp_all, "item_cbs_code")
})

testthat::test_that("the scatter carries both normalized axes", {
  out <- whep::build_sjos_nitrogen(example = TRUE)
  pointblank::expect_col_exists(out$scatter, c("nourish_norm", "boundary_norm"))
  testthat::expect_true(all(is.finite(out$scatter$nourish_norm)))
  testthat::expect_true(all(is.finite(out$scatter$boundary_norm)))
})

testthat::test_that("the footprint conserves the country exceedance total", {
  out <- whep::build_sjos_nitrogen(example = TRUE)
  country_total <- sum(out$boundary_surplus$country$exceedance_n_t)
  footprint_total <- sum(out$footprint$fp_all$impact_u)
  testthat::expect_gt(country_total, 0)
  testthat::expect_equal(footprint_total, country_total)
})

testthat::test_that("sjos_class values are valid sjos_levels", {
  out <- whep::build_sjos_nitrogen(example = TRUE)
  testthat::expect_s3_class(out$sjos_class$sjos_class, "factor")
  testthat::expect_true(all(!is.na(out$sjos_class$sjos_class)))
  testthat::expect_true(all(
    as.character(out$sjos_class$sjos_class) %in% whep::sjos_levels$level
  ))
  # The fixture is built to exercise both boundary sides.
  testthat::expect_setequal(
    unique(out$sjos_class$boundary_side),
    c("Exceedance", "Within_boundary")
  )
})

testthat::test_that("an injected coherent data fixture composes cleanly", {
  out <- whep::build_sjos_nitrogen(data = .sjos_nitrogen_test_data())
  tables <- .sjos_nitrogen_tables(out)
  for (nm in names(tables)) {
    testthat::expect_gt(nrow(tables[[nm]]), 0)
  }
  # The one exceeding crop propagates from the grid to the classification.
  pointblank::expect_col_exists(out$sjos_class, "item_cbs_code")
  testthat::expect_true(any(out$sjos_class$boundary_side == "Exceedance"))
  testthat::expect_true(any(out$sjos_class$boundary_side == "Within_boundary"))
  # Conservation holds on the injected fixture too.
  testthat::expect_equal(
    sum(out$footprint$fp_all$impact_u),
    sum(out$boundary_surplus$country$exceedance_n_t)
  )
})

testthat::test_that("a real call without IO or traced flows aborts", {
  data <- .sjos_nitrogen_test_data()
  data$fp_flows <- NULL
  testthat::expect_error(
    whep::build_sjos_nitrogen(data = data),
    "IO model|fp_flows|domestic"
  )
})
