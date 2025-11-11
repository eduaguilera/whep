#' Tests for GRAFS N Inputs
library(testthat)
library(dplyr)
library(tidyr)

#Test: .assign_items ----------------------------------------------------------
test_that(".assign_items returns expected categories", {
  cats <- .assign_items()

  expect_true("semi_natural_agroecosystems" %in% names(cats))
  expect_true("Firewood_biomass" %in% names(cats))
  expect_true("Dehesa" %in% cats$semi_natural_agroecosystems)
  expect_true("Holm oak" %in% cats$Firewood_biomass)
})

#Test: .calculate_n_soil_inputs -----------------------------------------------
test_that(".calculate_n_soil_inputs aggregates N inputs correctly", {
  n_balance_ygpit_all <- tibble(
    Year = c(2000, 2000, 2000, 2000),
    Province_name = c("Madrid", "Madrid", "Madrid", "Madrid"),
    Name_biomass = c("Dehesa", "Holm oak", "Other crop residues", "Manure"),
    Name_biomass = c(
      "Dehesa",
      "Holm oak",
      "Other crop residues",
      "Manure"
    ),
    LandUse = c("Dehesa", "Holm oak", "Cropland", "Livestock"),
    Irrig_cat = NA_character_,
    Deposition = c(1, 2, 3, 4),
    BNF = c(0.5, 0.2, 0.1, 0),
    Synthetic = c(0, 0, 1, 0),
    Urban = c(0, 0, 0, 1),
    Solid = c(NA, NA, NA, 1),
    Liquid = c(NA, NA, NA, 2),
    Irrig_cat = c("Irrigated", NA, "Rainfed", NA)
  )

  codes_coefs <- tibble(
    Name_biomass = c("Dehesa", "Holm oak", "Other crop residues", "Manure"),
    Item = c("Dehesa_item", "Firewood", "Residue", "Manure")
  )

  result <- .calculate_n_soil_inputs(n_balance_ygpit_all, codes_coefs)

  expect_true(all(
    c(
      "Year",
      "Province_name",
      "Item",
      "Box",
      "deposition",
      "fixation",
      "synthetic",
      "manure",
      "urban"
    ) %in%
      names(result)
  ))

  manure_val <- result |> filter(Item == "Manure") |> pull(manure)
  expect_equal(manure_val, 1 + 2)
  expect_true(all(result$deposition >= 0))

  # Check Manure calculation (sum of Solid + Liquid)
  manure_val <- result |>
    dplyr::filter(Item == "Manure") |>
    dplyr::pull(manure)

  expect_equal(manure_val, 1 + 2)
})

#Test: .calculate_n_production -----------------------------------------------
test_that(".calculate_n_production calculates production and import correctly", {
  grafs_prod_destiny <- tibble(
    Year = rep(2000, 5),
    Province_name = rep("Madrid", 5),
    Item = rep("Dehesa_item", 5),
    Box = rep("semi_natural_agroecosystems", 5),
    Box_destiny = rep("cropland", 5),
    Box_destiny = rep("semi_natural_agroecosystems", 5),
    Destiny = c("food", "feed", "other_uses", "export", "import"),
    MgN = c(10, 5, 4, 3, 2)
  )

  result <- .calculate_n_production(grafs_prod_destiny)

  expect_true(all(c("prod", "import") %in% colnames(result)))
  expect_equal(sum(result$prod), (10 + 5 + 4 + 3) - 2)
  expect_equal(sum(result$import), 2)
})


test_that("calculate_nue_crops output structure is correct", {
  testthat::skip_on_ci()
#Test: calculate_nue_livestock -----------------------------------------------
test_that("calculate_nue_livestock calculates NUE and mass balance correctly", {
  intake_n <- tibble(
    Year = 2000,
    Province_name = "Madrid",
    Livestock_cat = "Cattle",
    feed_n = 100
  )

  prod_n <- tibble(
    Year = 2000,
    Province_name = "Madrid",
    Livestock_cat = "Cattle",
    Item = "Milk",
    prod_n = 30
  )

  excretion_n <- tibble(
    Year = 2000,
    Province_name = "Madrid",
    Livestock_cat = "Cattle",
    excretion_n = 60
  )

  nue_livestock <- intake_n |>
    inner_join(prod_n, by = c("Year", "Province_name", "Livestock_cat")) |>
    left_join(excretion_n, by = c("Year", "Province_name", "Livestock_cat")) |>
    mutate(
      nue = prod_n / feed_n * 100,
      mass_balance = (prod_n + excretion_n) / feed_n * 100
    )

  expect_equal(nue_livestock$nue, 30)
  expect_equal(nue_livestock$mass_balance, 90)
})

#Test: calculate_system_nue --------------------------------------------------
test_that("calculate_system_nue calculates system-level NUE correctly", {
  n_soil_inputs <- tibble(
    Year = 2000,
    Province_name = "Madrid",
    deposition = 10,
    fixation = 5,
    synthetic = 15,
    manure = 20,
    urban = 0
  )

  total_outputs <- tibble(
    Year = 2000,
    Province_name = "Madrid",
    total_prod = 40
  )

  system_nue <- total_outputs |>
    left_join(n_soil_inputs, by = c("Year", "Province_name")) |>
    mutate(
      inputs = deposition + fixation + synthetic + manure + urban,
      nue_system = total_prod / inputs * 100
    ) |>
    select(Year, Province_name, total_prod, inputs, nue_system)

  expect_equal(system_nue$nue_system, 40 / (10 + 5 + 15 + 20 + 0) * 100)
})
