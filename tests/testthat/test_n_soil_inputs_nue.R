# .assign_items
test_that(".assign_items returns expected item groups", {
  items <- .assign_items()

  expect_true("semi_natural_agroecosystems" %in% names(items))
  expect_true("Firewood_biomass" %in% names(items))

  expect_true("Dehesa" %in% items$semi_natural_agroecosystems)
  expect_true("Holm oak" %in% items$Firewood_biomass)
})

# .calculate_n_soil_inputs
test_that(".calculate_n_soil_inputs aggregates soil inputs correctly", {
  n_balance <- tibble::tribble(
    ~Year, ~Province_name, ~Name_biomass, ~LandUse, ~Irrig_cat,
    ~Deposition, ~BNF, ~Synthetic, ~Solid, ~Liquid, ~Urban,
    2000, "A", "Wheat", "Cropland", "irrig", 1, 2, 3, 1, 1, 2
  )

  names_biomass_cb <- tibble::tribble(
    ~Name_biomass, ~Item,
    "Wheat", "Wheat"
  )

  out <- .calculate_n_soil_inputs(n_balance, names_biomass_cb)

  expect_equal(out$deposition, 1)
  expect_equal(out$fixation, 2)
  expect_equal(out$synthetic, 3)
  expect_equal(out$manure, 2) # Solid + Liquid
  expect_equal(out$urban, 2)
})

test_that(".calculate_n_soil_inputs assigns Firewood correctly", {
  n_balance <- tibble::tribble(
    ~Year, ~Province_name, ~Name_biomass, ~LandUse, ~Irrig_cat,
    ~Deposition, ~BNF, ~Synthetic, ~Solid, ~Liquid, ~Urban,
    2000, "A", "Holm oak", "Forest_low", NA, 1, 0, 0, 0, 0, 0
  )

  names_biomass_cb <- tibble::tribble(
    ~Name_biomass, ~Item,
    "Holm oak", "Holm oak"
  )

  out <- .calculate_n_soil_inputs(n_balance, names_biomass_cb)

  expect_equal(unique(out$Item), "Firewood")
})

# .calculate_n_production
test_that(".calculate_n_production sums production correctly", {
  grafs <- tibble::tribble(
    ~Year, ~Province_name, ~Item, ~Box, ~Destiny, ~MgN,
    2000, "A", "Wheat", "Cropland", "population_food", 5,
    2000, "A", "Wheat", "Cropland", "population_other_uses", 0,
    2000, "A", "Wheat", "Cropland", "livestock_rum", 2,
    2000, "A", "Wheat", "Cropland", "livestock_mono", 0,
    2000, "A", "Wheat", "Cropland", "export", 3
  )

  out <- .calculate_n_production(grafs)

  expect_equal(out$prod, 10)
})

# calculate_nue_crops
test_that("calculate_nue_crops computes NUE correctly", {
  n_soil_inputs <- tibble::tribble(
    ~Year, ~Province_name, ~Item, ~Box,
    ~deposition, ~fixation, ~synthetic, ~manure, ~urban,
    2000, "A", "Wheat", "Cropland", 1, 1, 2, 1, 0
  )

  n_prod <- tibble::tribble(
    ~Year, ~Province_name, ~Item, ~Box, ~prod,
    2000, "A", "Wheat", "Cropland", 5
  )

  nue <- dplyr::inner_join(
    n_soil_inputs,
    n_prod,
    by = c("Year", "Province_name", "Item", "Box")
  ) |>
    dplyr::mutate(
      inputs = deposition + fixation + synthetic + manure + urban,
      nue = prod / inputs * 100
    )

  expect_equal(nue$nue, 5 / 5 * 100)
})

# calculate_system_nue
test_that("calculate_system_nue computes system NUE correctly", {
  soil_inputs <- tibble::tribble(
    ~Year, ~Province_name,
    ~deposition, ~fixation, ~synthetic, ~manure, ~urban,
    2000, "A", 1, 1, 2, 1, 0
  )

  total_outputs <- tibble::tribble(
    ~Year, ~Province_name, ~Prod_MgN,
    2000, "A", 5
  )

  inputs <- sum(c(1, 1, 2, 1, 0))
  nue_expected <- 5 / inputs * 100

  out <- soil_inputs |>
    dplyr::mutate(
      inputs = inputs,
      total_prod = 5,
      nue_system = nue_expected
    )

  expect_equal(out$nue_system, nue_expected)
})
