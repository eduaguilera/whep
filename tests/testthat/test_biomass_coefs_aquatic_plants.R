# The `Aquatic Plants` coefficient row and what it reaches (whep#970). CBS
# item 2775 used to bridge to the upstream placeholder Name_biomass "0", which
# matched no coefficient row, so its feed mass left availability and its food
# mass carried no protein or energy. The row's values are sourced in
# R/datasets.R (biomass_coefs); these pin them to the sources and to the FBS
# oracle, and show each consumer now reaches them.

.aquatic_row <- function() {
  whep::biomass_coefs |>
    dplyr::filter(.data$Name_biomass == "Aquatic Plants")
}

testthat::test_that("the aquatic-plant row carries its sourced values", {
  row <- .aquatic_row()
  testthat::expect_equal(nrow(row), 1L)
  # FAO (2001), Food balance sheets: a handbook, Annex I, PDF page 68
  # (printed 64), row AQUATIC PLANTS: 54 kcal, 2.8 g protein and 0.6 g fat
  # per 100 g of the commodity as recorded (fresh).
  testthat::expect_equal(row$N_kgN_kgFM * 6.25 * 100, 2.8)
  testthat::expect_equal(row$Edible_portion, 1)
  testthat::expect_equal(row$GE_product_MJ_kgFM, 540 * 4.184 / 1000)
  testthat::expect_equal(row$Lipids_g_kgFM, 6)
  # Heuze et al. (2017), Feedipedia node 78, "Laminaria and Saccharina
  # seaweeds" (fresh): dry matter 15.4% as fed (n = 56), crude protein 9.8%
  # of DM (n = 50).
  testthat::expect_equal(row$Product_kgDM_kgFM, 0.154)
  testthat::expect_equal(row$Product_kgN_kgDM * 6.25 * 100, 9.8)
})

testthat::test_that("aquatic-plant food protein agrees with the FBS oracle", {
  # FAOSTAT FBS item 2775, World (area 5000), 2010: protein supply 361,644.59
  # t (element 671) over food 13,631 thousand t (element 5142), 26.53 g/kg.
  # The 2001 factor (28 g/kg) sits 5.5% above it.
  fbs_g_kg <- 361644.59 / 13631e3 * 1000
  whep_g_kg <- whep:::.food_nutrition_lookup(
    whep::items_full,
    whep::biomass_coefs,
    protein_basis = "edible_portion"
  ) |>
    dplyr::filter(.data$item_cbs_code == 2775) |>
    dplyr::pull(.data$protein_frac_kgfm) *
    1000
  testthat::expect_equal(whep_g_kg, 28)
  testthat::expect_lt(abs(whep_g_kg / fbs_g_kg - 1), 0.1)
})

testthat::test_that("aquatic-plant feed reaches availability with no warning", {
  cbs <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~feed,
    2018L, 110L,       2775L,          130430
  )
  testthat::expect_no_warning(
    out <- whep:::.build_feed_avail_national(cbs, feed_loss = "none")
  )
  testthat::expect_equal(out$item_cbs_code, 2775L)
  testthat::expect_equal(out$avail_dm_t, 130430 * 0.154)
})

testthat::test_that("aquatic-plant feed nitrogen resolves through bio_coefs", {
  n <- whep:::.feed_n_content_lookup() |>
    dplyr::filter(.data$item_cbs_code == 2775L) |>
    dplyr::pull(.data$feed_n_kgn_kgdm)
  testthat::expect_equal(n, 0.098 / 6.25)
})
