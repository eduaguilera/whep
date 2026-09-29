# The food-protein coefficient against FAOSTAT FBS's own implied density
# (whep#796). This is a coefficient check, not a build check: it needs no
# tonnage, so it runs offline against a committed fixture.
#
# The oracle in fixtures/fbs_protein_density_2010.csv is FBS element 671
# (protein supply, tonnes) over element 5142 (food, 1000 t), summed over the
# 2010 country-item cells where both are reported, FAO aggregates excluded and
# items with fewer than 20 reporting countries dropped. FBS derives element
# 671 by applying food-composition factors to the processed products actually
# eaten and dividing by the standardised primary-equivalent food quantity
# (FAO, Food Balance Sheets: A Handbook, section III), which is exactly the
# construct build_food_supply() needs: its `food_t` is that same standardised
# quantity. So the oracle is not "FAOSTAT is right"; it is "these two numbers
# are defined the same way and must therefore agree".
#
# Regenerate with `Rscript --vanilla validation/food_protein_density.R 2010
# --write`, which reads the faostat-fbs-new pin. What lives here is the
# fixture, not the read, so the suite stays offline.

.fpd_oracle <- function() {
  testthat::test_path("fixtures", "fbs_protein_density_2010.csv") |>
    readr::read_csv(show_col_types = FALSE)
}

# WHEP's protein density per FBS item, in grams per kg of fresh matter, on the
# default `edible_portion` basis: the same arithmetic build_food_supply() does.
.fpd_whep <- function() {
  whep::items_full |>
    dplyr::distinct(.data$item_cbs_code, .data$Name_biomass) |>
    dplyr::inner_join(
      whep::biomass_coefs |>
        dplyr::distinct(.data$Name_biomass, .keep_all = TRUE) |>
        dplyr::transmute(
          Name_biomass = .data$Name_biomass,
          nitrogen = dplyr::coalesce(
            .data$N_kgN_kgFM,
            .data$Product_kgN_kgDM * .data$Product_kgDM_kgFM
          ) *
            dplyr::coalesce(.data$Edible_portion, 1)
        ),
      by = "Name_biomass"
    ) |>
    dplyr::transmute(
      item_cbs_code = as.integer(.data$item_cbs_code),
      Name_biomass = .data$Name_biomass,
      whep_protein_g_kgfm = .data$nitrogen * 6.25 * 1000
    ) |>
    dplyr::filter(!is.na(.data$whep_protein_g_kgfm))
}

.fpd_paired <- function() {
  dplyr::inner_join(.fpd_whep(), .fpd_oracle(), by = "item_cbs_code")
}

# Items whose density is more than 20% and more than 5 g/kg away from the
# oracle. The band is deliberately loose: one global coefficient cannot carry
# the country-by-country milling and processing pattern that FBS reflects, so
# anything inside it is not evidence of a defect.
.fpd_out_of_band <- function() {
  .fpd_paired() |>
    dplyr::filter(
      abs(.data$whep_protein_g_kgfm - .data$fbs_protein_g_kgfm) > 5,
      .data$whep_protein_g_kgfm > 1.2 * .data$fbs_protein_g_kgfm |
        .data$whep_protein_g_kgfm < 0.8 * .data$fbs_protein_g_kgfm
    ) |>
    dplyr::arrange(.data$item_cbs_code)
}

testthat::test_that("the FBS density oracle still joins the coefficients", {
  # Guards the two invariants below against passing on an empty or shrunken
  # join, which is how a renamed Name_biomass would hide a real regression.
  paired <- .fpd_paired()
  testthat::expect_gte(nrow(paired), 75L)
  testthat::expect_true(all(paired$fbs_protein_g_kgfm > 0))
  testthat::expect_true(2511L %in% paired$item_cbs_code)
})

testthat::test_that("wheat protein density agrees with FAOSTAT FBS", {
  # whep#796: `Wheat` carried the agronomic whole-grain nitrogen, 118.45 g of
  # protein per kg, against an FBS implied density of 96.60 -- and 1.27x FBS
  # world wheat protein on a real 2010 build. It now carries the workbook's
  # own wheat-flour figure, 93 g/kg. This assertion fails on the old value.
  wheat <- dplyr::filter(.fpd_paired(), .data$item_cbs_code == 2511L)
  testthat::expect_equal(nrow(wheat), 1L)
  testthat::expect_equal(wheat$whep_protein_g_kgfm, 93)
  testthat::expect_lt(
    abs(wheat$whep_protein_g_kgfm / wheat$fbs_protein_g_kgfm - 1),
    0.1
  )
})

testthat::test_that("the items outside the FBS band are the known set", {
  # Pinned, not tolerated: this is the triage surface for the rest of the
  # nourishment axis, and it is what makes the wheat fix a measured change
  # rather than a lucky one. Nuts 2551 is whep#797 (an FBS basket of ten
  # species reached through Almonds); rice 2807 is a milled density over a
  # mass already converted to milled equivalent, settled in #751/#755, so its
  # 1.54 here is the density alone and not the axis error; the meat, fish and
  # root entries are open in #500. Olives 2563 left it in #1096. A row leaving
  # this set is progress; a row
  # joining it is a regression, and either way the test says so.
  known <- c(
    2513L,
    2514L,
    2531L,
    2534L,
    2535L,
    2541L,
    2551L,
    2557L,
    2558L,
    2560L,
    2570L,
    2605L,
    2614L,
    2630L,
    2633L,
    2640L,
    2645L,
    2732L,
    2733L,
    2734L,
    2735L,
    2737L,
    2762L,
    2764L,
    2765L,
    2766L,
    2767L,
    2769L,
    2807L
  )
  testthat::expect_equal(.fpd_out_of_band()$item_cbs_code, known)
})

testthat::test_that("olive, rye and honey carry their food-composition row", {
  # whep#1096: in the source workbook these three rows had their protein cut
  # loose from the food-composition row that fills the rest of their nutrition
  # block. Olive and honey held bare literals (0.0038 and 0) and rye the
  # agronomic whole-grain nitrogen. Each now carries its own
  # `Conversores_Dieta` `Proteinas` value: `Aceituna de mesa` 8 g/kg, `Miel`
  # 5 g/kg, `Harina de Centeno` 100 g/kg. Olive is 0.8 edible, hence 6.4.
  paired <- .fpd_paired()
  olive <- dplyr::filter(paired, .data$item_cbs_code == 2563L)
  rye <- dplyr::filter(paired, .data$item_cbs_code == 2515L)
  honey <- dplyr::filter(paired, .data$item_cbs_code == 2745L)
  testthat::expect_equal(olive$whep_protein_g_kgfm, 6.4)
  testthat::expect_equal(rye$whep_protein_g_kgfm, 100)
  testthat::expect_equal(honey$whep_protein_g_kgfm, 5)
  # Honey's whole block was a hand-typed placeholder (carbohydrate 1000 g/kg
  # in 785 g/kg of dry matter); the `Miel` row's protein and carbohydrate sum
  # to exactly that dry matter.
  honey_row <- dplyr::filter(whep::biomass_coefs, .data$Name_biomass == "Honey")
  testthat::expect_equal(
    honey_row$N_kgN_kgFM * 6250 + honey_row$Carbohydrates_g_kgFM,
    honey_row$Product_kgDM_kgFM * 1000
  )
  # Olive was 3.2x the FBS density and rye 0.90x; both land within 10%.
  testthat::expect_lt(
    abs(olive$whep_protein_g_kgfm / olive$fbs_protein_g_kgfm - 1),
    0.1
  )
  testthat::expect_lt(
    abs(rye$whep_protein_g_kgfm / rye$fbs_protein_g_kgfm - 1),
    0.1
  )
})

testthat::test_that("foods FAOSTAT credits with protein but WHEP zeroes are known", {
  # The band test above cannot see a zero on a low-protein item: honey at
  # 0 g/kg against FBS 2.99 sat inside its 5 g/kg absolute floor (whep#1096).
  # Pinned, not tolerated, like the band set: a row leaving it is progress, a
  # row joining it is a regression. The four that remain are refined sugar,
  # oil, fat and alcoholic-beverage items whose workbook food-composition row
  # is a genuine 0; whether FBS's 1.8-11.8 g/kg for them should be matched is
  # #500's question, not this one.
  zero <- .fpd_paired() |>
    dplyr::filter(
      .data$fbs_protein_g_kgfm > 1,
      .data$whep_protein_g_kgfm <= 0
    ) |>
    dplyr::arrange(.data$item_cbs_code)
  testthat::expect_equal(zero$item_cbs_code, c(2541L, 2586L, 2658L, 2737L))
})

testthat::test_that("the cereals still on agronomic nitrogen are known", {
  # whep#1096: five cereal rows carried the agronomic whole-grain nitrogen
  # (Product_kgN_kgDM * Product_kgDM_kgFM) because the workbook never derived
  # a nitrogen value for their food-composition rows. Wheat (#796) and rye now
  # take their flour protein. Maize and oats do not: their workbook food rows
  # (87 and 117 g/kg) move away from the FBS density (62.2 and 72.6) where the
  # agronomic value (78.0 and 83.8) is closer, and rice's basis is #751/#755.
  # Those three are an open expert call. Pinned so that changing any of them
  # is a decision someone makes, not a drift nobody sees.
  agronomic <- whep::biomass_coefs |>
    dplyr::filter(
      .data$Name_biomass %in% c("Wheat", "Rye", "Oats", "Maize", "Rice"),
      abs(
        .data$N_kgN_kgFM - .data$Product_kgN_kgDM * .data$Product_kgDM_kgFM
      ) <
        1e-12
    ) |>
    dplyr::pull(.data$Name_biomass)
  testthat::expect_setequal(agronomic, c("Oats", "Maize", "Rice"))
})
