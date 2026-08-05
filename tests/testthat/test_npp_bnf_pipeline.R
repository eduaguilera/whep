# End-to-end integration: the migrated crop-NPP, BNF and residue-destiny
# functions must compose into a single pipeline with the column names flowing
# correctly between steps. Per-function value correctness (faithful port of the
# afsetools formulae) is covered in the per-function test files; this locks the
# composition and the cross-function invariants.

test_that("crop NPP -> carbon/nitrogen -> BNF -> residue destinies composes", {
  crops <- tibble::tibble(
    item_prod_code = c("15", "176"),
    production_t = c(1000, 200),
    area_ha = c(400, 80),
    year = 2000,
    # The chain carries three regional groupings because they key three
    # different coefficient tables: region_krausmann the recovery rate,
    # region_hanpp the modern-variety adoption share, and region_un_sub the
    # residue feed-use fraction (M49 sub-regions, see #405). Keeping all three
    # here is what .sci_crop_prod_wide() actually supplies.
    region_krausmann = "West Europe",
    region_hanpp = "Western Europe",
    region_un_sub = "Western Europe",
    sub_territory = "ESP"
  )

  npp <- crops |>
    whep::calculate_crop_npp() |>
    whep::calculate_npp_carbon_nitrogen()
  testthat::expect_equal(
    npp$crop_npp_dm_t,
    npp$product_dm_t + npp$residue_dm_t + npp$root_dm_t
  )
  testthat::expect_true(all(c("crop_npp_n_t", "crop_npp_c_t") %in% names(npp)))

  bnf <- whep::calculate_crop_bnf(npp)
  testthat::expect_equal(bnf$crop_bnf_t[bnf$item_prod_code == "15"], 0)
  testthat::expect_gt(bnf$crop_bnf_t[bnf$item_prod_code == "176"], 0)

  dest <- whep::calculate_residue_destinies(npp)
  testthat::expect_equal(
    dest$residue_feed_dm_t + dest$residue_burn_dm_t + dest$residue_soil_dm_t,
    dest$residue_dm_t
  )

  avail <- whep::build_residue_feed_avail(dest)
  testthat::expect_true(
    all(c("item_cbs_code", "avail_dm_t", "feed_quality") %in% names(avail))
  )
  testthat::expect_true(all(avail$feed_quality == "residues"))
})
