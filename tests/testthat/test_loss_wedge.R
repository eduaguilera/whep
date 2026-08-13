# Every fixture is injected or reads the PACKAGED coefficient tables, so the
# suite is offline.

.lw_supply <- function() {
  tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~protein_t,
    2010L, 10L,        2511L,          100,
    2010L, 10L,        2605L,          100,
    2010L, 20L,        2511L,          300
  )
}

testthat::test_that("the half-of-minimum wedge composes the two retail steps", {
  # Cereals take the across-region minimum of Annex 4's Distribution (2%) and
  # Consumption (1%) columns, each halved, composed as
  # 1 - (1 - d/2)(1 - c/2) = 1 - 0.99 * 0.995. Adding the steps instead would
  # give 0.015 and overstate the wedge.
  out <- whep::build_loss_wedge(
    data = list(
      protein_supply = dplyr::filter(
        .lw_supply(),
        .data$area_code == 20L
      )
    )
  )
  testthat::expect_equal(out$omega, 1 - 0.99 * 0.995)
  testthat::expect_false(isTRUE(all.equal(out$omega, 0.01 + 0.005)))
})

testthat::test_that("the wedge is protein-weighted across groups", {
  # Equal protein from cereals (1.495%) and vegetables (6.400%) must give their
  # mean, not either one and not their sum.
  out <- whep::build_loss_wedge(
    data = list(
      protein_supply = dplyr::filter(
        .lw_supply(),
        .data$area_code == 10L
      )
    )
  )
  cereals <- 1 - 0.99 * 0.995
  veg <- 1 - 0.96 * 0.975
  testthat::expect_equal(out$omega, (cereals + veg) / 2)
})

testthat::test_that("gustavsson_min is the unhalved minimum and is larger", {
  half <- whep::build_loss_wedge(
    data = list(protein_supply = .lw_supply())
  )
  full <- whep::build_loss_wedge(
    data = list(protein_supply = .lw_supply()),
    method = "gustavsson_min"
  )
  cereals_full <- 1 - 0.98 * 0.99
  testthat::expect_equal(
    dplyr::filter(full, .data$area_code == 20L)$omega,
    cereals_full
  )
  testthat::expect_true(all(full$omega > half$omega))
})

testthat::test_that("none is a real zero, not a missing wedge", {
  out <- whep::build_loss_wedge(
    data = list(protein_supply = .lw_supply()),
    method = "none"
  )
  testthat::expect_equal(unique(out$omega), 0)
  testthat::expect_equal(unique(out$floor_divisor), 1)
  testthat::expect_equal(unique(out$method_loss_wedge), "none")
})

testthat::test_that("the floor divisor inverts the wedge", {
  out <- whep::build_loss_wedge(
    data = list(protein_supply = .lw_supply())
  )
  testthat::expect_equal(out$floor_divisor, 1 / (1 - out$omega))
})

testthat::test_that("items Annex 2 does not name are excluded and reported", {
  # Eggs (2744) carry protein but Gustavsson's Annex 2 gives them no commodity
  # group. They must not silently take a neighbouring group's rate, and their
  # share must be visible rather than absorbed.
  supply <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~protein_t,
    2010L, 10L,        2511L,          75,
    2010L, 10L,        2744L,          25
  )
  out <- whep::build_loss_wedge(data = list(protein_supply = supply))
  testthat::expect_equal(out$omega, 1 - 0.99 * 0.995)
  testthat::expect_equal(out$protein_grouped_share, 0.75)
})

testthat::test_that("a country-year with no grouped protein gets NA, not zero", {
  supply <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~protein_t,
    2010L, 10L,        2744L,          25
  )
  testthat::expect_warning(
    whep::build_loss_wedge(data = list(protein_supply = supply)),
    "commodity group"
  )
  out <- suppressWarnings(
    whep::build_loss_wedge(data = list(protein_supply = supply))
  )
  testthat::expect_true(is.na(out$omega))
  testthat::expect_equal(out$protein_grouped_share, 0)
})

testthat::test_that("food tonnes and injected protein agree", {
  # Deriving the weights from cbs_food must go through the same nutrition
  # lookup build_food_supply() uses, so the wedge is weighted on the very
  # supply the floor is compared against.
  coefs <- tibble::tribble(
    ~Name_biomass, ~N_kgN_kgFM, ~Product_kgN_kgDM, ~Product_kgDM_kgFM,
    "Wheat",       0.02,        0.02,              0.87,
    "Artichoke",   0.004,       0.004,             0.1
  ) |>
    dplyr::mutate(
      Edible_portion = 1,
      GE_product_edible_portion_MJ_kgFM = 15,
      GE_product_MJ_kgFM = 15
    )
  items <- tibble::tribble(
    ~item_cbs_code, ~Name_biomass,
    2511L,          "Wheat",
    2605L,          "Artichoke"
  )
  food <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~food_t,
    2010L, 10L,        2511L,          1000,
    2010L, 10L,        2605L,          1000
  )
  from_food <- whep::build_loss_wedge(
    data = list(cbs_food = food, biomass_coefs = coefs, items_full = items)
  )
  protein <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~protein_t,
    2010L, 10L,        2511L,          1000 * 0.02 * 6.25,
    2010L, 10L,        2605L,          1000 * 0.004 * 6.25
  )
  from_protein <- whep::build_loss_wedge(
    data = list(protein_supply = protein)
  )
  testthat::expect_equal(from_food$omega, from_protein$omega)
})

testthat::test_that("a missing input column aborts", {
  testthat::expect_error(
    whep::build_loss_wedge(
      data = list(protein_supply = dplyr::select(.lw_supply(), -"protein_t"))
    ),
    "protein_t"
  )
  testthat::expect_error(whep::build_loss_wedge(), "protein_supply|cbs_food")
})

testthat::test_that("unknown method values are rejected", {
  testthat::expect_error(
    whep::build_loss_wedge(
      data = list(protein_supply = .lw_supply()),
      method = "unep"
    ),
    "arg_match|must be one of|unep"
  )
})

testthat::test_that("the PACKAGED Annex 4 table is locked", {
  # This is the coefficient lock: it reads the shipped CSV, not a fixture, so
  # an edit to inst/extdata/coefs/food_loss_wedge.csv fails here.
  wedge <- whep::whep_coef_table("food_loss_wedge")
  testthat::expect_equal(nrow(wedge), 98L)
  testthat::expect_setequal(wedge$step, c("distribution", "consumption"))
  testthat::expect_equal(dplyr::n_distinct(wedge$region), 7L)
  testthat::expect_equal(dplyr::n_distinct(wedge$loss_group), 7L)
  testthat::expect_true(all(wedge$loss_pct > 0 & wedge$loss_pct <= 100))
  # The seven half-of-minimum group wedges the SJOS-N floor is built on.
  expected <- c(
    cereals = 0.014950,
    roots_tubers = 0.024850,
    oilseeds_pulses = 0.0099750,
    fruits_vegetables = 0.064000,
    meat = 0.029800,
    fish_seafood = 0.054550,
    milk = 0.0029988
  )
  got <- whep:::.lw_group_wedge("gustavsson_half_min", wedge)
  testthat::expect_equal(
    got$omega_group[match(names(expected), got$loss_group)],
    unname(expected),
    tolerance = 1e-6
  )
})

testthat::test_that("the PACKAGED item mapping stays inside Annex 2", {
  groups <- whep::whep_coef_table("food_loss_item_groups")
  wedge <- whep::whep_coef_table("food_loss_wedge")
  testthat::expect_equal(anyDuplicated(groups$item_cbs_code), 0L)
  testthat::expect_true(
    all(stats::na.omit(groups$loss_group) %in% unique(wedge$loss_group))
  )
  # A group assignment must carry the Annex 2 clause that licenses it, and an
  # unnamed item must carry no group at all.
  testthat::expect_setequal(
    unique(groups$annex2_basis),
    c("enumerated", "group_title", "not_named")
  )
  testthat::expect_true(
    all(is.na(groups$loss_group[groups$annex2_basis == "not_named"]))
  )
  testthat::expect_false(
    any(is.na(groups$loss_group[groups$annex2_basis != "not_named"]))
  )
})
