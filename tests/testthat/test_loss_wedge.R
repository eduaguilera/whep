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

.lw_cereals <- function(area) {
  tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~protein_t,
    2010L, area,       2511L,          100
  )
}

testthat::test_that("the regional method uses a region's own unhalved rates", {
  # Spain is Annex 1 Region 1. Europe's cereal rates are 2% distribution and
  # 25% consumption, composed whole -- neither minimised nor halved.
  out <- whep::build_loss_wedge(
    data = list(protein_supply = .lw_cereals(203L)),
    method = "gustavsson_regional_actual"
  )
  testthat::expect_equal(out$omega, 1 - 0.98 * 0.75)
  testthat::expect_equal(out$method_region, "annex1")
})

testthat::test_that("regional rates differ by region for the same basket", {
  # sub-Saharan Africa's cereal consumption rate is 1% against Europe's 25%.
  # A wedge that did not vary here would not be regional at all.
  spain <- whep::build_loss_wedge(
    data = list(protein_supply = .lw_cereals(203L)),
    method = "gustavsson_regional_actual"
  )
  nigeria <- whep::build_loss_wedge(
    data = list(protein_supply = .lw_cereals(159L)),
    method = "gustavsson_regional_actual"
  )
  testthat::expect_equal(nigeria$omega, 1 - 0.98 * 0.99)
  testthat::expect_lt(nigeria$omega, spain$omega)
})

testthat::test_that("both China codes reach Industrialized Asia", {
  # Annex 1 lists "China" without disambiguating, and WHEP splits it: area 41
  # is `CHN` while the aggregate 351, which is what the FBS pin reports food
  # on, carries no iso3c at all. Keying on iso3c alone would silently drop a
  # fifth of world food protein.
  out <- whep::build_loss_wedge(
    data = list(
      protein_supply = dplyr::bind_rows(
        .lw_cereals(41L),
        .lw_cereals(351L)
      )
    ),
    method = "gustavsson_regional_actual"
  )
  testthat::expect_equal(nrow(out), 2L)
  testthat::expect_equal(unique(out$omega), 1 - 0.98 * 0.80)
  testthat::expect_equal(unique(out$method_region), "annex1")
})

testthat::test_that("areas outside Annex 1 take the global mean, and say so", {
  # Madagascar is in no Annex 1 region. Its wedge must be visibly a fallback,
  # never indistinguishable from a listed country's.
  out <- whep::build_loss_wedge(
    data = list(protein_supply = .lw_cereals(129L)),
    method = "gustavsson_regional_actual"
  )
  # Mean distribution 2,2,2,2,4,2,4 -> 18/7; mean consumption
  # 25,27,20,1,12,3,10 -> 98/7 = 14.
  mean_dist <- mean(c(2, 2, 2, 2, 4, 2, 4)) / 100
  mean_cons <- mean(c(25, 27, 20, 1, 12, 3, 10)) / 100
  testthat::expect_equal(out$omega, 1 - (1 - mean_dist) * (1 - mean_cons))
  testthat::expect_equal(out$method_region, "global_mean")
})

testthat::test_that("annex1_only refuses to fill instead of filling", {
  testthat::expect_warning(
    whep::build_loss_wedge(
      data = list(protein_supply = .lw_cereals(129L)),
      method = "gustavsson_regional_actual",
      coverage = "annex1_only"
    ),
    "Annex 1 lists no region"
  )
  out <- suppressWarnings(
    whep::build_loss_wedge(
      data = list(protein_supply = .lw_cereals(129L)),
      method = "gustavsson_regional_actual",
      coverage = "annex1_only"
    )
  )
  testthat::expect_true(is.na(out$omega))
  testthat::expect_true(is.na(out$method_region))
})

testthat::test_that("the region-invariant methods say they use no region", {
  out <- whep::build_loss_wedge(
    data = list(protein_supply = .lw_cereals(203L))
  )
  testthat::expect_equal(out$method_region, "region_invariant")
  testthat::expect_equal(out$omega, 1 - 0.99 * 0.995)
})

testthat::test_that("the PACKAGED region table resolves one region per area", {
  regions <- whep::whep_coef_table("food_loss_regions")
  wedge <- whep::whep_coef_table("food_loss_wedge")
  testthat::expect_equal(nrow(regions), 153L)
  testthat::expect_setequal(unique(regions$region), unique(wedge$region))
  # Every row is keyed exactly one way, by iso3c or by area code, never both
  # and never neither.
  keyed <- xor(!is.na(regions$iso3c), !is.na(regions$area_code))
  testthat::expect_true(all(keyed))
  # Resolution must not hand one area two regions.
  resolved <- whep:::.lw_area_regions(regions)
  testthat::expect_equal(anyDuplicated(resolved$area_code), 0L)
  testthat::expect_true(all(c(41L, 351L) %in% resolved$area_code))
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

testthat::test_that("an area in two Annex 1 regions aborts, not double-weights", {
  # Found by review. The packaged tables give one region per area, but
  # `data$food_loss_regions` is injectable: two ISO3 codes folded into the same
  # WHEP area and placed in different regions would match both wedge rows and
  # weight the whole basket twice, at two different rates.
  regions <- tibble::tribble(
    ~iso3c,        ~area_code, ~region,
    NA_character_, 10L,        "Europe",
    NA_character_, 10L,        "Sub-Saharan Africa"
  )
  testthat::expect_error(
    whep::build_loss_wedge(
      data = list(
        protein_supply = tibble::tribble(
          ~year, ~area_code, ~item_cbs_code, ~protein_t,
          2010L, 10L,        2511L,          100
        ),
        food_loss_regions = regions
      ),
      method = "gustavsson_regional_actual"
    ),
    "more than one region"
  )
})

testthat::test_that("two clashing areas are named, not a cli crash", {
  # Same defect as the single-area case above, one step further: the abort
  # interpolates the integer area codes as cli's pluralisation quantity, which
  # cli accepts only at length 1. With two clashing areas the guard aborted
  # with "length(object) == 1 is not TRUE" instead of naming them.
  regions <- tibble::tribble(
    ~iso3c,        ~area_code, ~region,
    NA_character_, 10L,        "Europe",
    NA_character_, 10L,        "Sub-Saharan Africa",
    NA_character_, 20L,        "Europe",
    NA_character_, 20L,        "South and Southeast Asia"
  )
  testthat::expect_error(
    whep::build_loss_wedge(
      data = list(
        protein_supply = .lw_supply(),
        food_loss_regions = regions
      ),
      method = "gustavsson_regional_actual"
    ),
    "Area codes: 10 and 20"
  )
})

testthat::test_that("two ungrouped areas are named, not a cli crash", {
  # An area whose whole basket falls outside Annex 2 gets no wedge, and the
  # warning that says so carried the same length-1 pluralisation quantity.
  groups <- tibble::tribble(
    ~item_cbs_code, ~loss_group,
    9999L,          "Cereals"
  )
  testthat::expect_warning(
    whep::build_loss_wedge(
      data = list(
        protein_supply = .lw_supply(),
        food_loss_item_groups = groups
      )
    ),
    "Area codes: 10 and 20"
  )
})

testthat::test_that("two unregioned areas are named, not a cli crash", {
  # Under `coverage = "annex1_only"` an area Annex 1 does not list gets no
  # wedge on purpose, and must still be reported. Third instance of the same
  # length-1 quantity.
  regions <- tibble::tribble(
    ~iso3c, ~area_code, ~region,
    "ESP",  999L,       "Europe"
  )
  testthat::expect_warning(
    whep::build_loss_wedge(
      data = list(
        protein_supply = .lw_supply(),
        food_loss_regions = regions
      ),
      method = "gustavsson_regional_actual",
      coverage = "annex1_only"
    ),
    "Area codes: 10 and 20"
  )
})
