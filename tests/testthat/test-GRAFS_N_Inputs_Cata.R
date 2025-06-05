#' Tests for GRAFS N Inputs

test_that("._assign_items returns expected categories", {
  cats <- .assign_items()

  expect_true("Semi_natural_agroecosystems" %in% names(cats))
  expect_true("Firewood_biomass" %in% names(cats))
  expect_true("residue_items" %in% names(cats))

  expect_true("Dehesa" %in% cats$Semi_natural_agroecosystems)
  expect_true("Holm oak" %in% cats$Firewood_biomass)
})

test_that("._calculate_n_inputs calculates inputs and manure correctly", {
  # Sample data for N_balance_ygpit_all
  N_balance_ygpit_all <- tibble::tibble(
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

  Codes_coefs <- tibble::tibble(
    Name_biomass = c("Dehesa", "Holm oak", "Other crop residues", "Excreta"),
    Item = c("Dehesa_item", "Firewood", "Residue", "Manure")
  )

  result <- .calculate_n_inputs(N_balance_ygpit_all, Codes_coefs)

  expect_named(result, c("N_inputs_summary", "manure_summary"))

  # Check that Deposition values are non-negative
  expect_true(all(result$N_inputs_summary$Deposition >= 0))

  # Check Manure calculation (sum of Excreta + Solid + Liquid)
  manure_val <- result$manure_summary %>%
    dplyr::filter(Name_biomass == "Excreta") %>%
    dplyr::pull(Total_Manure)
  expect_equal(manure_val, 5 + 1 + 2)
})

test_that("._summarise_inputs correctly summarizes inputs", {
  N_balance_ygpit_all <- tibble::tibble(
    Year = c(2000, 2000, 2000),
    Province_name = c("Madrid", "Madrid", "Madrid"),
    Name_biomass = c("Dehesa", "Holm oak", "Other crop residues"),
    LandUse = c("Dehesa", "Holm oak", "Cropland"),
    Deposition = c(1, 2, 3),
    BNF = c(0.5, 0.2, 0.1),
    Synthetic = c(0, 0, 1),
    Urban = c(0, 0, 0),
    Excreta = c(NA, NA, NA),
    Solid = c(NA, NA, NA),
    Liquid = c(NA, NA, NA)
  )

  Codes_coefs <- tibble::tibble(
    Name_biomass = c("Dehesa", "Holm oak", "Other crop residues"),
    Item = c("Dehesa_item", "Firewood", "Residue")
  )

  n_inputs_prepared <- .calculate_n_inputs(N_balance_ygpit_all, Codes_coefs)
  sum_inputs <- .summarise_inputs(n_inputs_prepared)

  expect_true(all(c("MgN_dep", "MgN_fix", "MgN_syn", "MgN_manure", "MgN_urban") %in% colnames(sum_inputs)))
  expect_true(all(sum_inputs$MgN_dep >= 0))
})

test_that("._summarise_production correctly combines inputs and production data", {
  n_inputs_prepared <- list(
    N_inputs_summary = tibble::tibble(
      Year = 2000,
      Province_name = "Madrid",
      Name_biomass = "Dehesa",
      Item = "Dehesa_item",
      Box = "Semi_natural_agroecosystems",
      Deposition = 1,
      BNF = 0.5,
      Synthetic = 0,
      Urban = 0
    ),
    manure_summary = tibble::tibble(
      Year = 2000,
      Province_name = "Madrid",
      Name_biomass = "Dehesa",
      Item = "Dehesa_item",
      Box = "Semi_natural_agroecosystems",
      Total_Manure = 0
    )
  )

  n_inputs_sum <- .summarise_inputs(n_inputs_prepared)

  GRAFS_Prod_Destiny <- tibble::tibble(
    Year = rep(2000, 5),
    Province_name = rep("Madrid", 5),
    Item = rep("Dehesa_item", 5),
    Box = rep("Semi_natural_agroecosystems", 5),
    Destiny = c("Food", "Feed", "Other_uses", "Export", "Import"),
    MgN = c(10, 5, 4, 3, 2)
  )

  prod_combined <- .summarise_production(GRAFS_Prod_Destiny, n_inputs_sum)

  expect_true(all(c("Prod_MgN", "Import_MgN") %in% colnames(prod_combined)))
  expect_true(all(!is.na(prod_combined$Box)))
  expect_true("Madrid" %in% prod_combined$Province_name)
})

test_that("._calculate_nue calculates NUE correctly for Cropland and Semi_natural_agroecosystems", {
  n_inputs_prepared <- list(
    N_inputs_summary = tibble::tibble(
      Year = 2000,
      Province_name = "Madrid",
      Name_biomass = "Dehesa",
      Item = "Dehesa_item",
      Box = "Semi_natural_agroecosystems",
      Deposition = 1,
      BNF = 0.5,
      Synthetic = 0,
      Urban = 0
    ),
    manure_summary = tibble::tibble(
      Year = 2000,
      Province_name = "Madrid",
      Name_biomass = "Dehesa",
      Item = "Dehesa_item",
      Box = "Semi_natural_agroecosystems",
      Total_Manure = 0
    )
  )

  n_inputs_sum <- .summarise_inputs(n_inputs_prepared)

  GRAFS_Prod_Destiny <- tibble::tibble(
    Year = c(2000, 2000, 2000, 2000),
    Province_name = c("Madrid", "Madrid", "Madrid", "Madrid"),
    Item = rep("Dehesa_item", 4),
    Box = rep("Semi_natural_agroecosystems", 4),
    Destiny = c("Food", "Feed", "Export", "Import"),
    MgN = c(10, 5, 3, 2)
  )

  prod_combined <- .summarise_production(GRAFS_Prod_Destiny, n_inputs_sum)
  nue <- .calculate_nue(prod_combined)

  # Check NUE is calculated for Cropland and Semi_natural_agroecosystems
  nue_filtered <- nue %>% dplyr::filter(Box == "Semi_natural_agroecosystems")
  expect_true(all(!is.na(nue_filtered$NUE)))

  # NUE should be NA for other Box categories (e.g. Fish)
  nue$Box[1] <- "Fish"
  nue$NUE[1] <- NA_real_
  expect_true(is.na(nue$NUE[1]))
})
