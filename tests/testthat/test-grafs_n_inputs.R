#' Tests for GRAFS N Inputs

test_that("._assign_items returns expected categories", {
  cats <- .assign_items()

  expect_true("semi_natural_agroecosystems" %in% names(cats))
  expect_true("Firewood_biomass" %in% names(cats))

  expect_true("Dehesa" %in% cats$semi_natural_agroecosystems)
  expect_true("Holm oak" %in% cats$Firewood_biomass)
})

test_that("._calculate_n_inputs calculates N soil inputs correctly", {
  # Sample data for n_balance_ygpit_all
  n_balance_ygpit_all <- tibble::tibble(
    Year = c(2000, 2000, 2000, 2000),
    Province_name = c("Madrid", "Madrid", "Madrid", "Madrid"),
    Name_biomass = c("Dehesa", "Holm oak", "Other crop residues", "Excreta"),
    LandUse = c("Dehesa", "Holm oak", "Cropland", "Livestock"),
    Deposition = c(1, 2, 3, 4),
    BNF = c(0.5, 0.2, 0.1, 0),
    Synthetic = c(0, 0, 1, 0),
    Urban = c(0, 0, 0, 1),
    Excreta = c(NA, NA, NA, 5),
    Solid = c(NA, NA, NA, 1),
    Liquid = c(NA, NA, NA, 2)
  )

  codes_coefs <- tibble::tibble(
    Name_biomass = c("Dehesa", "Holm oak", "Other crop residues", "Excreta"),
    Item = c("Dehesa_item", "Firewood", "Residue", "Manure")
  )

  result <- .calculate_n_inputs(n_balance_ygpit_all, codes_coefs)

  expect_true(all(c(
    "Year", "Province_name", "Item", "Box",
    "MgN_dep", "MgN_fix", "MgN_syn", "MgN_manure", "MgN_urban"
  ) %in% names(result)))

  expect_true(all(result$MgN_dep >= 0))

  # Check Manure calculation (sum of Excreta + Solid + Liquid)
  manure_val <- result |>
    dplyr::filter(Item == "Manure") |>
    dplyr::pull(MgN_manure)

  expect_equal(manure_val, 5 + 1 + 2)
})

test_that("._summarise_production combines inputs and production correctly", {
  n_inputs <- tibble::tibble(
    Year = 2000,
    Province_name = "Madrid",
    Item = "Dehesa_item",
    Box = "semi_natural_agroecosystems",
    MgN_dep = 1,
    MgN_fix = 0.5,
    MgN_syn = 0,
    MgN_manure = 0,
    MgN_urban = 0
  )

  grafs_prod_destiny <- tibble::tibble(
    Year = rep(2000, 5),
    Province_name = rep("Madrid", 5),
    Item = rep("Dehesa_item", 5),
    Box = rep("semi_natural_agroecosystems", 5),
    Destiny = c("Food", "Feed", "Other_uses", "Export", "Import"),
    MgN = c(10, 5, 4, 3, 2)
  )

  combined <- .summarise_production(grafs_prod_destiny, n_inputs)

  expect_true(all(c("Prod_MgN", "Import_MgN") %in% colnames(combined)))
  expect_true(all(!is.na(combined$Box)))
  expect_true("Madrid" %in% combined$Province_name)
})

test_that("._calculate_nue calculates NUE correctly", {
  n_inputs_combined <- tibble::tibble(
    Year = 2000,
    Province_name = "Madrid",
    Item = "Dehesa_item",
    Box = "semi_natural_agroecosystems",
    MgN_dep = 1,
    MgN_fix = 0.5,
    MgN_syn = 0,
    MgN_manure = 0,
    MgN_urban = 0,
    Import_MgN = 2,
    Prod_MgN = 15
  )

  nue <- .calculate_nue(n_inputs_combined)

  expect_true("nue" %in% colnames(nue))
  expect_true(all(!is.na(nue$nue)))

  nue_other <- nue
  nue_other$Box[1] <- "Fish"
  nue_other$nue[1] <- NA_real_
  expect_true(is.na(nue_other$nue[1]))
})
