#' Tests for GRAFS N Inputs

test_that("._assign_items returns expected categories", {
  cats <- .assign_items()

  expect_true("semi_natural_agroecosystems" %in% names(cats))
  expect_true("Firewood_biomass" %in% names(cats))

  expect_true("Dehesa" %in% cats$semi_natural_agroecosystems)
  expect_true("Holm oak" %in% cats$Firewood_biomass)
})

test_that("_.calculate_n_soil_inputs calculates N soil inputs correctly", {
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

  result <- .calculate_n_soil_inputs(n_balance_ygpit_all, codes_coefs)

  expect_true(all(c(
    "Year", "Province_name", "Item", "Box",
    "deposition", "fixation", "synthetic", "manure", "urban"
  ) %in% names(result)))

  expect_true(all(result$deposition >= 0))

  # Check Manure calculation (sum of Excreta + Solid + Liquid)
  manure_val <- result |>
    dplyr::filter(Item == "Manure") |>
    dplyr::pull(manure)

  expect_equal(manure_val, 5 + 1 + 2)
})

test_that(".calculate_n_production computes production correctly", {
  grafs_prod_destiny <- tibble::tibble(
    Year = rep(2000, 5),
    Province_name = rep("Madrid", 5),
    Item = rep("Dehesa_item", 5),
    Box = rep("semi_natural_agroecosystems", 5),
    Destiny = c("Food", "Feed", "Other_uses", "Export", "Import"),
    MgN = c(10, 5, 4, 3, 2)
  )

  result <- .calculate_n_production(grafs_prod_destiny)

  expect_true(all(c("prod", "import") %in% colnames(result)))
  expect_equal(result$prod, (10 + 5 + 4 + 3) - 2)
  expect_equal(result$import, 2)
})

test_that("calculate_nue_crops calculates NUE correctly", {
  n_soil_inputs <- tibble::tibble(
    Year = 2000,
    Province_name = "Madrid",
    Item = "Dehesa_item",
    Box = "semi_natural_agroecosystems",
    deposition = 1,
    fixation = 0.5,
    synthetic = 0,
    manure = 0,
    urban = 0
  )

  n_prod_data <- tibble::tibble(
    Year = 2000,
    Province_name = "Madrid",
    Item = "Dehesa_item",
    Box = "semi_natural_agroecosystems",
    import = 2,
    prod = 15
  )


  nue <- calculate_nue_crops()

  expect_true("nue" %in% colnames(nue))
  expect_true(all(!is.na(nue$nue)))

  nue_other <- nue
  nue_other$Box[1] <- "Fish"
  nue_other$nue[1] <- NA_real_
  expect_true(is.na(nue_other$nue[1]))
})
