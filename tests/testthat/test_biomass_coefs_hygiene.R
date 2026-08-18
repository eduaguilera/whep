# Hygiene invariants on the PACKAGED nutrition coefficients (#500 J2.4).
#
# These read whep::biomass_coefs, not a fixture, so a coefficient edit has to
# come past them. None of them is a clean bill of health: each records what the
# shipped table actually violates today, so the violations are visible and the
# lists can only shrink. A test that passed by asserting nothing would be worse
# than no test.

.bch_proximate <- function() {
  whep::biomass_coefs |>
    dplyr::mutate(
      protein_g_kgfm = .data$N_kgN_kgFM * 6.25 * 1000,
      proximate_g_kgfm = rowSums(
        cbind(
          .data$protein_g_kgfm,
          .data$Lipids_g_kgFM,
          .data$Carbohydrates_g_kgFM,
          .data$Fiber_g_kgFM
        ),
        na.rm = TRUE
      ),
      dry_matter_g_kgfm = .data$Product_kgDM_kgFM * 1000
    ) |>
    dplyr::filter(
      .data$proximate_g_kgfm > 0,
      !is.na(.data$dry_matter_g_kgfm),
      .data$dry_matter_g_kgfm > 0
    )
}

testthat::test_that("Edible_portion is a fraction, and one row is not", {
  # An edible portion above 1 means more edible matter than matter. The single
  # offender is the label row ANIMAL PRODUCTS, which carries 4.0 -- and 3.0 kg
  # of nitrogen per kg of fresh matter beside it. It is a header that leaked
  # into the data, not a commodity.
  bad <- whep::biomass_coefs |>
    dplyr::filter(!is.na(.data$Edible_portion)) |>
    dplyr::filter(.data$Edible_portion <= 0 | .data$Edible_portion > 1)
  testthat::expect_equal(bad$Name_biomass, "ANIMAL PRODUCTS")
  testthat::expect_equal(bad$Edible_portion, 4)
  # The rest really are fractions, so the guard is not vacuous.
  ok <- whep::biomass_coefs |>
    dplyr::filter(
      !is.na(.data$Edible_portion),
      .data$Name_biomass != "ANIMAL PRODUCTS"
    )
  testthat::expect_true(all(ok$Edible_portion > 0 & ok$Edible_portion <= 1))
  testthat::expect_gt(nrow(ok), 200L)
})

testthat::test_that("ANIMAL PRODUCTS is unreachable from any item code", {
  # This is why the 4.0 has never moved a published number: no item_cbs_code
  # bridges to it. If that ever changes, a row with 3 kg N per kg of food
  # becomes live, so the unreachability is asserted rather than assumed.
  reachable <- whep::items_full |>
    dplyr::filter(.data$Name_biomass == "ANIMAL PRODUCTS")
  testthat::expect_equal(nrow(reachable), 0L)
})

testthat::test_that("the nutrition lookup drops the label row even if reached", {
  # Belt and braces for the assertion above: if a future items_full DID bridge
  # to it, the food path must still not multiply food tonnes by 3 kg N/kg.
  items <- tibble::tribble(
    ~item_cbs_code, ~Name_biomass,
    2511L,          "ANIMAL PRODUCTS"
  )
  coefs <- tibble::tribble(
    ~Name_biomass,     ~N_kgN_kgFM, ~Product_kgN_kgDM, ~Product_kgDM_kgFM,
    "ANIMAL PRODUCTS", 3,           3,                 1
  ) |>
    dplyr::mutate(
      Edible_portion = 4,
      GE_product_edible_portion_MJ_kgFM = 15,
      GE_product_MJ_kgFM = 15
    )
  lookup <- whep:::.food_nutrition_lookup(items, coefs, "edible_portion")
  testthat::expect_true(all(is.na(lookup$protein_frac_kgfm)))
})

testthat::test_that("no food carries more than a kilogram per kilogram", {
  # The physically impossible bound: protein + lipid + carbohydrate + fibre
  # cannot exceed 1000 g in 1 kg of fresh matter. Fourteen rows do. Urea and
  # Lysine are feed additives where nitrogen times 6.25 is not protein at all;
  # the rest are real coefficient defects, White sugar among them.
  impossible <- .bch_proximate() |>
    dplyr::filter(.data$proximate_g_kgfm > 1000) |>
    dplyr::pull(.data$Name_biomass)
  testthat::expect_setequal(
    impossible,
    c(
      "Urea",
      "Carob",
      "Lysine",
      "Hemp seed",
      "Barley",
      "Barley old",
      "Winter cereals, other",
      "Canary grass",
      "Poppy seeds",
      "White sugar",
      "Triticale",
      "Linseed",
      "Cereals nes",
      "Grain, mixed"
    )
  )
})

testthat::test_that("the proximate sum against dry matter can only improve", {
  # The tighter chemical bound: the constituents must fit inside the dry
  # matter. 75 rows fail it, but only 32 fail once fibre is left out, so about
  # forty of them are fibre being counted inside carbohydrate as well as beside
  # it rather than a coefficient error. Both counts are ratchets: lower them
  # when a row is fixed, never raise them to admit a new one.
  prox <- .bch_proximate()
  with_fibre <- sum(prox$proximate_g_kgfm > prox$dry_matter_g_kgfm)
  without_fibre <- sum(
    prox$proximate_g_kgfm - dplyr::coalesce(prox$Fiber_g_kgFM, 0) >
      prox$dry_matter_g_kgfm
  )
  testthat::expect_lte(with_fibre, 75L)
  testthat::expect_lte(without_fibre, 32L)
})
