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

test_that("no row has an out-of-range Edible_portion", {
  # Edible_portion is a fraction of fresh matter, so (0, 1] is the whole valid
  # range. One row used to break it: "ANIMAL PRODUCTS" at 4.0. That row is NOT
  # a data defect to clean upstream -- in afsetools' Biomass_coefs.xlsx it is
  # the VLOOKUP column-index vector the Coefs sheet depends on by absolute
  # address, so editing it there breaks the workbook (#752). It is dropped at
  # ingestion instead, so the packaged table now satisfies the bound outright.
  coefs <- whep::biomass_coefs
  out_of_range <- coefs[
    !is.na(coefs$Edible_portion) &
      (coefs$Edible_portion <= 0 | coefs$Edible_portion > 1),
  ]

  testthat::expect_identical(nrow(out_of_range), 0L)
  testthat::expect_false(
    "ANIMAL PRODUCTS" %in% whep::items_full$Name_biomass
  )
})

test_that("the proximate columns exceed dry matter on a known 72 rows", {
  # Protein + carbohydrate + lipid + fibre cannot exceed dry matter. 72 of 421
  # rows do (#752). Some are feed additives where a composition block is not
  # applicable (Urea, Lysine), but ordinary foods are affected too, so the
  # columns cannot be used to reason about a row's basis. A tripwire, not a
  # target: it fails if a new row joins the set, and it fails when the set is
  # genuinely repaired -- at which point lower the number deliberately.
  coefs <- whep::biomass_coefs
  proximate <- rowSums(
    cbind(
      coefs$N_kgN_kgFM * 6.25 * 1000,
      coefs$Carbohydrates_g_kgFM,
      coefs$Lipids_g_kgFM,
      coefs$Fiber_g_kgFM
    ),
    na.rm = TRUE
  )
  dry_matter <- coefs$Product_kgDM_kgFM * 1000
  # Compare with a tolerance: three rows (Melon, Strawberry, Duck eggs) close
  # exactly against dry matter and exceed it only by ~1e-14, which is float
  # noise, not a data defect. Of the 72 that remain, 71 exceed by more than
  # 1 g/kg, so the set is not tolerance-sensitive in any other way.
  over <- !is.na(dry_matter) &
    proximate > 0 &
    proximate > dry_matter + 1e-6

  testthat::expect_equal(sum(over), 72L)
  testthat::expect_true("Barley" %in% coefs$Name_biomass[over])
})

test_that("unknown coefficient table errors", {
  testthat::expect_error(
    whep::whep_coef_table("does_not_exist"),
    "Unknown coefficient table"
  )
})
