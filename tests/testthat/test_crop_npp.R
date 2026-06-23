.npp_coef_ref <- function(coefs, model, param, component = NULL) {
  m <- coefs$model == model & coefs$parameter == param
  if (!is.null(component)) {
    m <- m & coefs$component == component
  }
  coefs$value[m]
}

test_that("calculate_potential_npp miami matches the afsetools formula", {
  x <- tibble::tibble(temp_c = 15, water_input_mm = 600, aet_mm = 450)
  out <- whep::calculate_potential_npp(x, method = "miami")

  testthat::expect_true(
    all(c("npp_potential_dm_t_ha", "method_npp_potential") %in% names(out))
  )
  testthat::expect_equal(out$method_npp_potential, "miami")

  coefs <- whep::whep_coef_table("npp_model_coefs")
  t_max <- .npp_coef_ref(coefs, "Miami", "Max_gCm2yr", "F_MAT")
  t_mid <- .npp_coef_ref(coefs, "Miami", "Midpoint", "F_MAT")
  t_rate <- .npp_coef_ref(coefs, "Miami", "Rate", "F_MAT")
  p_max <- .npp_coef_ref(coefs, "Miami", "Max_gCm2yr", "F_MAP")
  p_rate <- .npp_coef_ref(coefs, "Miami", "Rate", "F_MAP")
  expected <- pmin(
    (t_max / (1 + exp(t_mid - t_rate * 15))) / 100,
    (p_max * (1 - exp(-p_rate * 600))) / 100
  )
  testthat::expect_equal(out$npp_potential_dm_t_ha, expected)
})

test_that("calculate_potential_npp nceas uses non-tree ANPP", {
  x <- tibble::tibble(temp_c = 15, water_input_mm = 600, aet_mm = 450)
  out <- whep::calculate_potential_npp(x, method = "nceas")

  coefs <- whep::whep_coef_table("npp_model_coefs")
  carbon_fraction <- whep::whep_coef_table("weed_coefs")$residue_c_kgdm_weed
  a_max <- .npp_coef_ref(coefs, "NCEAS_nontree_ANPP", "Max_gCm2yr")
  a_rate <- .npp_coef_ref(coefs, "NCEAS_nontree_ANPP", "Rate")
  expected <- a_max * (1 - exp(-a_rate * 600)) / (100 * carbon_fraction)

  testthat::expect_equal(out$npp_potential_dm_t_ha, expected)
  testthat::expect_gt(out$npp_potential_dm_t_ha, 0)
})

test_that("lpjml potential-NPP method is not yet wired", {
  testthat::expect_error(
    whep::calculate_potential_npp(
      tibble::tibble(temp_c = 1),
      method = "lpjml"
    ),
    "not yet wired"
  )
})

test_that("calculate_crop_residues ipcc matches the IPCC linear model", {
  x <- tibble::tibble(item_prod_code = "15", production_t = 100, area_ha = 40)
  out <- whep::calculate_crop_residues(x, method = "ipcc")

  bc <- whep::whep_coef_table("bio_coefs")
  rc <- whep::whep_coef_table("ipcc_residue_coefs")
  mp <- whep::whep_coef_table("ipcc_crop_mapping")
  pd <- bc$product_dm_kgfm[bc$item_prod_code == "15"]
  ic <- mp$ipcc_crop[mp$item_prod_code == "15"]
  sl <- rc$slope_ag[rc$ipcc_crop == ic]
  it <- rc$intercept_ag_dm_t_ha[rc$ipcc_crop == ic]
  expected <- pmax(sl * (100 * pd / 40) + it, 0) * 40

  testthat::expect_equal(out$residue_dm_t, expected)
  testthat::expect_equal(out$method_residue, "ipcc")
  testthat::expect_equal(out$product_dm_t, 100 * pd)
})

test_that("calculate_crop_residues ratio uses the bio_coefs ratio", {
  x <- tibble::tibble(item_prod_code = "15", production_t = 100, area_ha = 40)
  out <- whep::calculate_crop_residues(x, method = "ratio")
  bc <- whep::whep_coef_table("bio_coefs")
  rr <- bc$residue_kg_product_fm_kg[bc$item_prod_code == "15"]
  rd <- bc$residue_dm_kgfm[bc$item_prod_code == "15"]
  testthat::expect_equal(out$residue_dm_t, 100 * rr * rd)
})

test_that("calculate_crop_residues ensemble blends ipcc and ratio", {
  x <- tibble::tibble(item_prod_code = "15", production_t = 100, area_ha = 40)
  ens <- whep::calculate_crop_residues(x, method = "ensemble")$residue_dm_t
  ipcc <- whep::calculate_crop_residues(x, method = "ipcc")$residue_dm_t
  ratio <- whep::calculate_crop_residues(x, method = "ratio")$residue_dm_t
  testthat::expect_equal(ens, 0.5 * ipcc + 0.5 * ratio)
})

test_that("calculate_crop_residues errors on missing columns", {
  testthat::expect_error(
    whep::calculate_crop_residues(tibble::tibble(item_prod_code = "15")),
    "missing required"
  )
})

test_that("calculate_crop_roots root_shoot uses the IPCC RS ratio", {
  x <- tibble::tibble(
    item_prod_code = "15",
    product_dm_t = 87.9,
    residue_dm_t = 135.75,
    area_ha = 40
  )
  out <- whep::calculate_crop_roots(x, method = "root_shoot")
  rk <- whep::whep_coef_table("ipcc_root_coefs")
  mp <- whep::whep_coef_table("ipcc_crop_mapping")
  rs <- rk$rs_default[rk$ipcc_crop == mp$ipcc_crop[mp$item_prod_code == "15"]]
  testthat::expect_equal(out$root_dm_t, (87.9 + 135.75) * rs)
  testthat::expect_equal(out$method_root, "root_shoot")
})

test_that("calculate_crop_roots reference uses BG ref per hectare", {
  x <- tibble::tibble(
    item_prod_code = "15",
    product_dm_t = 87.9,
    residue_dm_t = 135.75,
    area_ha = 40
  )
  out <- whep::calculate_crop_roots(x, method = "reference")
  rk <- whep::whep_coef_table("ipcc_root_coefs")
  mp <- whep::whep_coef_table("ipcc_crop_mapping")
  bg <- rk$bg_ref_dm_t_ha[
    rk$ipcc_crop == mp$ipcc_crop[mp$item_prod_code == "15"]
  ]
  testthat::expect_equal(out$root_dm_t, bg * 40)
})

test_that("calculate_crop_roots ensemble blends RS and reference", {
  x <- tibble::tibble(
    item_prod_code = "15",
    product_dm_t = 87.9,
    residue_dm_t = 135.75,
    area_ha = 40
  )
  ens <- whep::calculate_crop_roots(x, method = "ensemble")$root_dm_t
  rs <- whep::calculate_crop_roots(x, method = "root_shoot")$root_dm_t
  ref <- whep::calculate_crop_roots(x, method = "reference")$root_dm_t
  testthat::expect_equal(ens, 0.5 * rs + 0.5 * ref)
})
