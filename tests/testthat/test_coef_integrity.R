# Coefficient-table integrity. These check STRUCTURE and PHYSICAL RANGES, not
# exact values, so they survive legitimate coefficient updates while catching
# corruption: NA in a key column, an out-of-range fraction, a duplicate key, or
# a dropped fallback / flag.

test_that("bio_coefs is item-keyed, unique, and physically bounded", {
  x <- whep::whep_coef_table("bio_coefs")
  testthat::expect_false(any(is.na(x$item_prod_code)))
  testthat::expect_equal(nrow(x), dplyr::n_distinct(x$item_prod_code))
  pdm <- x$product_dm_kgfm[!is.na(x$product_dm_kgfm)]
  testthat::expect_true(all(pdm > 0 & pdm <= 1))
  rdm <- x$residue_dm_kgfm[!is.na(x$residue_dm_kgfm)]
  testthat::expect_true(all(rdm >= 0 & rdm <= 1))
  rsr <- x$root_shoot_ratio[!is.na(x$root_shoot_ratio)]
  testthat::expect_true(all(rsr >= 0))
  for (col in c("product_c_kgdm", "residue_c_kgdm", "root_mass_c_kgdm")) {
    v <- x[[col]][!is.na(x[[col]])]
    testthat::expect_true(all(v >= 0 & v <= 1), info = col)
  }
  rhizo_c <- x$rhizodeposit_mass_c_kgdm[
    !is.na(x$rhizodeposit_mass_c_kgdm)
  ]
  testthat::expect_true(all(rhizo_c >= 0))
  root_c <- x |>
    dplyr::filter(!is.na(root_c_kgdm), !is.na(root_mass_c_kgdm))
  testthat::expect_true(
    all(root_c$root_c_kgdm >= root_c$root_mass_c_kgdm),
    info = "root_c_kgdm includes root tissue plus rhizodeposit carbon"
  )
})

test_that("BNF parameters are within physical bounds", {
  b <- whep::whep_coef_table("bnf")
  for (col in c("ndfa", "leguminous_share")) {
    v <- b[[col]][!is.na(b[[col]])]
    testthat::expect_true(all(v >= 0 & v <= 1), info = col)
  }
  nb <- b$nonsymbiotic_base_kg_ha[!is.na(b$nonsymbiotic_base_kg_ha)]
  testthat::expect_true(all(nb >= 0))
})

test_that("residue recovery and feed fractions are in [0, 1] with a fallback", {
  ff <- whep::whep_coef_table("residue_feed_fraction")
  testthat::expect_true(all(
    ff$feed_use_fraction >= 0 & ff$feed_use_fraction <= 1
  ))
  testthat::expect_true("Global" %in% ff$region_hanpp)
  rk <- whep::whep_coef_table("residue_krausmann")
  rr <- rk$recovery_rates[!is.na(rk$recovery_rates)]
  testthat::expect_true(all(rr >= 0 & rr <= 1))
})

test_that("Spain-specific tables carry the to_be_revised flag", {
  for (tbl in c("weed_npp_scaling", "residue_shares")) {
    x <- whep::whep_coef_table(tbl)
    testthat::expect_true("to_be_revised" %in% names(x), info = tbl)
    testthat::expect_true(all(x$to_be_revised), info = tbl)
  }
  rs <- whep::whep_coef_table("residue_shares")
  for (col in c("use_share", "burn_share")) {
    v <- rs[[col]][!is.na(rs[[col]])]
    testthat::expect_true(all(v >= 0 & v <= 1), info = col)
  }
})

test_that("every coefficient table loads with its key column and rows", {
  keys <- c(
    bio_coefs = "item_prod_code",
    ipcc_residue_coefs = "ipcc_crop",
    ipcc_root_coefs = "ipcc_crop",
    ipcc_crop_mapping = "item_prod_code",
    npp_model_coefs = "model",
    bnf = "name_bnf",
    names_bnf = "item_prod_code",
    crop_residue_item_map = "item_prod_code",
    weed_coefs = "root_shoot_ratio_weed"
  )
  for (nm in names(keys)) {
    x <- whep::whep_coef_table(nm)
    testthat::expect_true(keys[[nm]] %in% names(x), info = nm)
    testthat::expect_gt(nrow(x), 0)
  }
})

test_that("crop -> residue item map uses straw / other-residue codes only", {
  m <- whep::whep_coef_table("crop_residue_item_map")
  testthat::expect_false(any(is.na(m$residue_item_cbs_code)))
  testthat::expect_true(all(m$residue_item_cbs_code %in% c(2105, 2106)))
})
