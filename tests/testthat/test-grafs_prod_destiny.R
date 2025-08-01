#' Tests for N production and destinies in Spain

fake_crop_areanpp_ygpit_all <- data.frame(
  Name_biomass = c("BiomassA", "BiomassB"),
  Area_ygpit_ha = c(100, 200),
  LandUse = c("Cropland", "Cropland"),
  Year = c(2020, 2020),
  Province_name = c("Province1", "Province2")
)

fake_npp_ygpit_csv <- data.frame(
  Name_biomass = c("BiomassA", "BiomassB"),
  GrazedWeeds_MgDM = c(10, 20),
  LandUse = c("Cropland", "Cropland"),
  Item = c("ItemA", "ItemB"),
  Year = c(2020, 2020),
  Province_name = c("Province1", "Province2")
)

fake_codes_coefs <- data.frame(
  Name_biomass = c("BiomassA", "BiomassB"),
  Item = c("ItemA", "ItemB")
)

# Test: .merge_items_biomass --------------------------------------------------
test_that(".merge_items_biomass merges correctly", {
  testthat::skip_on_ci()

  result <- .merge_items_biomass(
    fake_crop_areanpp_ygpit_all,
    fake_npp_ygpit_csv, fake_codes_coefs
  )

  expect_true("crop_area_npp_merged" %in% names(result))
  expect_true("npp_ygpit_merged" %in% names(result))
  expect_equal(nrow(result$crop_area_npp_merged), 2)
  expect_equal(result$crop_area_npp_merged$Item[1], "ItemA")
})

# Test: .summarise_crops_residues ---------------------------------------------
fake_crop_input <- data.frame(
  Year = c(2020, 2020),
  Province_name = c("Province1", "Province1"),
  Name_biomass = c("BiomassA", "BiomassA"),
  Prod_ygpit_Mg = c(50, 50),
  Product_residue = c("Product", "Residue"),
  Item = c("ItemA", "ItemA")
)

test_that(".summarise_crops_residues summarizes correctly", {
  testthat::skip_on_ci()

  result <- .summarise_crops_residues(fake_crop_input)

  expect_true("production_fm" %in% names(result))
  expect_equal(result$production_fm[1], 100)
  expect_equal(result$Box[1], "Cropland")
})

# Test: .aggregate_grazed_cropland --------------------------------------------
fake_grazed <- data.frame(
  Year = 2020,
  Province_name = "Province1",
  Name_biomass = "Fallow",
  Item = "Fallow",
  GrazedWeeds_MgDM = 15,
  LandUse = "Cropland"
)

fake_crop_residue <- data.frame(
  Year = 2020,
  Province_name = "Province1",
  Name_biomass = "Fallow",
  Item = "Fallow",
  production_fm = 50,
  Box = "Cropland"
)

test_that(".aggregate_grazed_cropland combines grazed and crop residue data", {
  testthat::skip_on_ci()

  result <- .aggregate_grazed_cropland(fake_grazed, fake_crop_residue)

  expect_true("GrazedWeeds_MgDM" %in% names(result))
  expect_equal(result$GrazedWeeds_MgDM[1], 15)
  expect_equal(result$production_fm[1], 50)
})

# Test: Adding feed correctly
test_that(".adding_feed correctly aggregates FM_Mg", {
  testthat::skip_on_ci()

  input <- data.frame(
    Year = c(2020, 2020, 2021),
    Province_name = c("A", "A", "B"),
    Item = c("Wheat", "Wheat", "Barley"),
    FM_Mg = c(10, 5, 20)
  )

  result <- .adding_feed(input)
  expect_equal(nrow(result), 2)
  expect_equal(result$FM_Mg_total[result$Year == 2020], 15)
})

# Test: Calculating population share correctly
test_that(".calculate_population_share calculates correct share", {
  testthat::skip_on_ci()

  input <- data.frame(
    Year = c(2020, 2020),
    Province_name = c("A", "B"),
    Pop_Mpeop_yg = c(100, 300)
  )

  result <- .calculate_population_share(input)
  expect_equal(result$Pop_share[result$Province_name == "A"], 0.25)
  expect_equal(result$Pop_share[result$Province_name == "B"], 0.75)
})

# Test: Adding food correctly
test_that(".adding_food multiplies population share with total food", {
  testthat::skip_on_ci()

  pie_data <- data.frame(
    Year = c(2020, 2020),
    Item = c("Wheat", "Wheat"),
    Destiny = "Food",
    Element = "Domestic_supply",
    Value_destiny = c(100, 300)
  )

  pop_share <- data.frame(
    Year = 2020,
    Province_name = c("A", "B"),
    Pop_Mpeop_yg = c(50, 150),
    Pop_share = c(0.25, 0.75)
  )

  result <- .adding_food(pie_data, pop_share)
  expect_equal(nrow(result), 2)
  expect_equal(result$Food_Mg[result$Province_name == "A"], 100)
  expect_equal(result$Food_Mg[result$Province_name == "B"], 300)
})

# Test: Adding other uses correctly
test_that(".adding_other_uses works same as food with other uses", {
  testthat::skip_on_ci()

  pie_data <- data.frame(
    Year = c(2020, 2020),
    Item = c("Wheat", "Wheat"),
    Destiny = "Other_uses",
    Element = "Domestic_supply",
    Value_destiny = c(100, 300)
  )

  pop_share <- data.frame(
    Year = 2020,
    Province_name = c("A", "B"),
    Pop_Mpeop_yg = c(50, 150),
    Pop_share = c(0.25, 0.75)
  )

  result <- .adding_other_uses(pie_data, pop_share)
  expect_equal(result$OtherUses_Mg[result$Province_name == "A"], 100)
  expect_equal(result$OtherUses_Mg[result$Province_name == "B"], 300)
})

# Test: combine destinies correctly
test_that(".combine_destinies joins correctly", {
  testthat::skip_on_ci()

  prod <- data.frame(
    Year = 2020, Province_name = "A", Item = "Wheat",
    Box = "Cropland", production_n = 500
  )
  food <- data.frame(
    Year = 2020, Province_name = "A", Item = "Wheat",
    Food_Mg = 100
  )
  other <- data.frame(
    Year = 2020, Province_name = "A", Item = "Wheat",
    OtherUses_Mg = 50
  )
  feed <- data.frame(
    Year = 2020, Province_name = "A", Item = "Wheat",
    FM_Mg_total = 25
  )

  result <- .combine_destinies(prod, feed, food, other)
  expect_equal(result$Food_MgFM, 100)
  expect_equal(result$OtherUses_MgFM, 50)
  expect_equal(result$Feed_MgFM, 25)
})

# Test: convert to N correctly
test_that(".convert_to_items_n applies conversions correctly", {
  testthat::skip_on_ci()

  input <- data.frame(
    Year = 2020, Province_name = "A", Item = "Wheat",
    Box = "Cropland", production_n = 500,
    Food_MgFM = 100, OtherUses_MgFM = 50, Feed_MgFM = 25
  )

  code_map <- data.frame(item = "Wheat", Name_biomass = "BiomassWheat")
  coefs <- data.frame(
    Name_biomass = "BiomassWheat",
    Product_kgDM_kgFM = 0.8, Product_kgN_kgDM = 0.02
  )

  result <- .convert_to_items_n(input, code_map, coefs)
  expect_equal(result$food, 1.6)
  expect_equal(result$other_uses, 0.8)
  expect_equal(result$feed, 0.4)
})

# Test: calculate trade correctly
test_that(".calculate_trade computes export/import properly", {
  testthat::skip_on_ci()

  input <- data.frame(
    Year = 2020, Province_name = "A", Item = "Wheat",
    Name_biomass = "BiomassWheat", Box = "Cropland",
    production_n = 10, food = 2, other_uses = 3, feed = 1
  )

  result <- .calculate_trade(input)
  expect_equal(result$consumption, 6)
  expect_equal(result$net_trade, 4)
  expect_equal(result$export, 4)
  expect_equal(result$import, 0)
})

# Test: finalising dataset correctly
test_that(".finalize_prod_destiny completes with pivot and recoding", {
  testthat::skip_on_ci()

  input <- data.frame(
    Year = 2020, Province_name = "A", Item = "Wheat", Box = NA,
    Name_biomass = "BiomassWheat",
    food = 1.5, other_uses = 0.5, feed = 1.0,
    export = 0.2, import = 0.1
  )

  code_map <- data.frame(item = "Wheat", group = "Primary crops")

  suppressMessages({
    result <- .finalize_prod_destiny(input, code_map)
  })

  expect_true(all(c("Destiny", "MgN") %in% names(result)))
  expect_equal(dplyr::n_distinct(result$Destiny), 5)
})
