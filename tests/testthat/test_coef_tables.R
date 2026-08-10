test_that("bio_coefs loads, is item-keyed and unique", {
  x <- whep::whep_coef_table("bio_coefs")
  testthat::expect_true(
    all(c("item_prod_code", "item_prod_name", "name_biomass") %in% names(x))
  )
  testthat::expect_equal(nrow(x), dplyr::n_distinct(x$item_prod_code))
})

test_that("residue_feed_fraction has a global fallback row", {
  x <- whep::whep_coef_table("residue_feed_fraction")
  testthat::expect_true(any(x$region_un_sub == "Global"))
})

test_that("natural-grain ipcc tables share the snake_case ipcc_crop key", {
  res <- whep::whep_coef_table("ipcc_residue_coefs")
  root <- whep::whep_coef_table("ipcc_root_coefs")
  testthat::expect_true("ipcc_crop" %in% names(res))
  testthat::expect_true("ipcc_crop" %in% names(root))
})

test_that("legacy below-ground fields stay retired from biomass_coefs", {
  legacy <- c(
    "BG_Biomass_kgDM_ha",
    "Root_Shoot_ratio",
    "Root_kgC_kgDM",
    "Rhizodeposits_mass_kgC_kgDM",
    "Rhizodeposits_N_kgN_kgRootN"
  )
  source_path <- system.file(
    "extdata",
    "harmonization",
    "biomass_coefs.csv",
    package = "whep"
  )
  source_coefs <- readr::read_csv(
    source_path,
    show_col_types = FALSE
  ) |>
    dplyr::select(!dplyr::starts_with("..."))

  testthat::expect_equal(dim(source_coefs), c(421L, 63L))
  testthat::expect_identical(names(source_coefs), names(whep::biomass_coefs))
  testthat::expect_false(any(legacy %in% names(source_coefs)))
  testthat::expect_false(any(legacy %in% names(whep::biomass_coefs)))

  bio <- whep::whep_coef_table("bio_coefs")
  modern <- c(
    "bg_biomass_dm_kg_ha",
    "root_shoot_ratio",
    "root_c_kgdm",
    "rhizodeposit_mass_c_kgdm",
    "rhizodeposit_n_kgn_krootn"
  )
  testthat::expect_true(all(modern %in% names(bio)))

  mapping <- whep::whep_coef_table("ipcc_crop_mapping")
  ipcc <- whep::whep_coef_table("ipcc_root_coefs")
  input <- tibble::tibble(
    item_prod_code = c("15", "417"),
    product_dm_t = 10,
    residue_dm_t = 20,
    area_ha = 2
  )
  root_shoot <- whep::calculate_crop_roots(input, method = "root_shoot")
  reference <- whep::calculate_crop_roots(input, method = "reference")
  wheat_ipcc <- mapping$ipcc_crop[mapping$item_prod_code == "15"]
  wheat_root <- ipcc[ipcc$ipcc_crop == wheat_ipcc, ]
  fallback <- bio[bio$item_prod_code == "417", ]

  testthat::expect_equal(root_shoot$root_dm_t[1], 30 * wheat_root$rs_default)
  testthat::expect_equal(
    reference$root_dm_t[1],
    2 * wheat_root$bg_ref_dm_t_ha
  )
  testthat::expect_equal(
    root_shoot$root_dm_t[2],
    30 * fallback$root_shoot_ratio
  )
  testthat::expect_equal(
    reference$root_dm_t[2],
    2 * fallback$bg_biomass_dm_kg_ha / 1000
  )
})

test_that("unknown coefficient table errors", {
  testthat::expect_error(
    whep::whep_coef_table("does_not_exist"),
    "Unknown coefficient table"
  )
})
