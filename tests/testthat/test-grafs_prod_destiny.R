# Tests for n_prov_destiny

library(testthat)
library(dplyr)
library(tibble)

# Test: .summarise_crops_residues ---------------------------------------------
test_that(".summarise_crops_residues aggregates correctly", {
  fake_crop <- tibble(
    Year = c(2020, 2020),
    Province_name = c("Province1", "Province1"),
    Name_biomass = c("BiomassA", "BiomassA"),
    Item = c("ItemA", "ItemA"),
    Product_residue = c("Product", "Residue"),
    Prod_ygpit_Mg = c(50, 30),
    Irrig_cat = c("Irrigated", "Irrigated")
  )

  result <- .summarise_crops_residues(fake_crop)

  expect_equal(nrow(result), 2)
  expect_equal(result$production_fm[1], 50)
  expect_equal(result$production_fm[2], 30)
  expect_equal(unique(result$Box), "Cropland")
})

# Test: .aggregate_crop_seminatural -------------------------------------------
test_that(".aggregate_crop_seminatural combines correctly", {
  fake_crop_residue <- tibble(
    Year = 2020,
    Province_name = "Province1",
    Name_biomass = "BiomassA",
    Item = "ItemA",
    prod_type = "Product",
    production_fm = 50,
    LandUse = "Cropland",
    Irrig_cat = "Irrigated",
    Box = "Cropland"
  )

  fake_npp_merged <- tibble(
    Year = 2020,
    Province_name = "Province1",
    Name_biomass = c("Fallow", "Shrubland"),
    Item = c("Fallow", "Shrub"),
    LandUse = c("Cropland", "Semi-natural"),
    Irrig_cat = c("Irrigated", "Rainfed"),
    Prod_ygpit_Mg = c(0, 40),
    Used_Residue_MgFM = c(0, 10),
    GrazedWeeds_MgDM = c(15, 5)
  )

  result <- .aggregate_crop_seminatural(fake_npp_merged, fake_crop_residue)

  expect_true(any(result$Item == "Fallow"))
  expect_true(any(result$Item == "Shrub"))
  expect_equal(sum(result$production_fm), 50 + 15 + 40 + 10 + 5)
})

# Test: .combine_production_boxes ---------------------------------------------
test_that(".combine_production_boxes binds correctly", {
  fake_crop <- tibble(
    Year = 2020,
    Province_name = "Province1",
    Item = "ItemA",
    production_fm = 50,
    Box = "Cropland"
  )

  fake_livestock <- tibble(
    Year = 2020,
    Province_name = "Province1",
    Item = "Cow",
    Prod_Mg = 100,
    Box = "Livestock",
    prod_type = "Product"
  )

  result <- .combine_production_boxes(fake_crop, fake_livestock)

  expect_equal(nrow(result), 2)
  expect_equal(result$production_fm[result$Item == "Cow"], 100)
})

# Test: .remove_seeds_from_system --------------------------------------------
test_that(".remove_seeds_from_system subtracts seeds correctly", {
  fake_npp <- tibble(
    Year = 2020,
    Province_name = "Province1",
    Item = "Wheat",
    LandUse = "Cropland",
    Area_ygpit_ha = 100
  )

  fake_pie <- tibble(
    Year = 2020,
    Item = "Wheat",
    Element = "Domestic_supply",
    Destiny = "Seed",
    Value_destiny = 50
  )

  fake_prod <- tibble(
    Year = 2020,
    Province_name = "Province1",
    Item = "Wheat",
    production_fm = 500,
    Box = "Cropland"
  )

  result <- .remove_seeds_from_system(fake_npp, fake_pie, fake_prod)

  expect_equal(result$production_fm[1], 500 - (50 / 100 * 100))
})


# Test: .add_grass_wood ---------------------------------------------
test_that(".add_grass_wood transforms items correctly", {
  fake_data <- tibble(
    Year = 2020,
    Province_name = "Province1",
    Name_biomass = c("Fallow", "Grass", "Holm oak forest"),
    Item = c("OldItem1", "OldItem2", "OldItem3"),
    Box = c("Cropland", "Semi-natural", "Semi-natural"),
    LandUse = c("Cropland", "Semi-natural", "Semi-natural"),
    Irrig_cat = c("Irrigated", "Rainfed", "Rainfed"),
    prod_type = c("Grass", "Grass", "Residue"),
    production_fm = c(10, 20, 30)
  )

  result <- .add_grass_wood(fake_data)

  expect_true(any(result$Item == "Fallow"))
  expect_true(any(result$Item == "Grassland"))
  expect_true(any(result$Item == "Firewood"))

  expect_equal(result$production_fm[result$Item == "Grassland"], 20 / 0.2)
})

# Test: .prepare_processed_data ---------------------------------------
test_that(".prepare_processed_data sums processed items correctly", {
  fake_processed <- tibble(
    Year = c(2020, 2020),
    Province_name = c("Province1", "Province1"),
    Name_biomass = c("Wheat", "Wheat"),
    Item = c("Wheat", "Wheat"),
    ProcessedItem = c("Alcohol", "Alcohol"),
    ProcessedItem_amount = c(5, 15)
  )

  result <- .prepare_processed_data(fake_processed)

  expect_equal(nrow(result), 1)
  expect_equal(result$production_fm[1], 20)
  expect_equal(result$Item[1], "Alcohol")
  expect_equal(result$Box[1], "Cropland")
})

# Test: .prepare_prod_data --------------------------------------------
test_that(".prepare_prod_data combines grafs and processed data", {
  grafs_data <- tibble(
    Year = 2020,
    Province_name = "Province1",
    Name_biomass = "Grass",
    Item = "Grassland",
    Box = "Semi-natural",
    LandUse = "Semi-natural",
    Irrig_cat = "Rainfed",
    prod_type = "Grass",
    production_fm = 50
  )

  processed_data <- tibble(
    Year = 2020,
    Province_name = "Province1",
    Name_biomass = "Wheat",
    Item = "Alcohol",
    Box = "Cropland",
    production_fm = 20,
    prod_type = "Product"
  )

  codes_cb <- tibble(
    item = c("Grassland", "Alcohol"),
    Name_biomass = c("Grass", "Wheat")
  )

  result <- .prepare_prod_data(grafs_data, processed_data, codes_cb)

  expect_equal(nrow(result), 2)
  expect_true(all(c("Grassland", "Alcohol") %in% result$Item))
  expect_true("Name_biomass" %in% colnames(result))
})

# Test: .convert_fm_dm_n ---------------------------------------------
test_that(".convert_fm_dm_n calculates N correctly", {
  fake_prod <- tibble(
    Year = 2020,
    Province_name = "Province1",
    Name_biomass = "Grass",
    Name_biomass_primary = "Grass",
    Item = "Grassland",
    Box = "Semi-natural",
    LandUse = "Semi-natural",
    Irrig_cat = "Rainfed",
    prod_type = "Grass",
    production_fm = 10
  )

  biomass_coefs <- tibble(
    Name_biomass = "Grass",
    Product_kgDM_kgFM = 0.5,
    Residue_kgDM_kgFM = 0.2,
    Product_kgN_kgDM = 0.03,
    Residue_kgN_kgDM = 0.05
  )

  result <- .convert_fm_dm_n(fake_prod, biomass_coefs)

  expect_equal(nrow(result), 1)
  expect_equal(result$production_n[1], 10 * 0.2 * 0.05)
})

# Test: .adding_feed ------------------------------------------------
test_that(".adding_feed calculates feed and shares correctly", {
  fake_feed <- tibble(
    Year = 2020,
    Province_name = "Province1",
    Item = c("Wheat", "Wheat", "Wheat", "Wheat"),
    FM_Mg = c(50, 10, 5, 7),
    Livestock_cat = c("Cattle_meat", "Sheep", "Pets", "Pigs")
  )

  result <- .adding_feed(fake_feed)

  feed_df <- result$feed_intake
  share_df <- result$feed_share_rum_mono

  expect_equal(sum(feed_df$feed[feed_df$Livestock_type == "ruminant"]), 60)
  expect_equal(sum(feed_df$feed[feed_df$Livestock_type == "monogastric"]), 7)
  expect_equal(sum(feed_df$food_pets), 5)

  wheat_share <- share_df %>%
    filter(Item == "Wheat", Province_name == "Province1", Year == 2020) %>%
    slice(1)

  expect_equal(wheat_share$share_rum, 60 / (60 + 7))
  expect_equal(wheat_share$share_mono, 7 / (60 + 7))
})


# Test: .calculate_population_share ---------------------------------
test_that(".calculate_population_share calculates shares correctly", {
  pop <- tibble(
    Year = 2020,
    Province_name = c("Province1", "Province2"),
    Pop_Mpeop_yg = c(100, 300)
  )

  result <- .calculate_population_share(pop)

  expect_equal(result$Pop_share[1], 100 / 400)
  expect_equal(result$Pop_share[2], 300 / 400)
})

# Test: .calculate_food_and_other_uses -------------------------------
test_that(".calculate_food_and_other_uses scales by population share", {
  pop_share <- tibble(
    Year = 2020,
    Province_name = c("Province1", "Province2"),
    Pop_share = c(0.25, 0.75)
  )
  pie <- tibble(
    Year = 2020,
    Item = "Wheat",
    Destiny = c("Food", "Other_uses"),
    Element = "Domestic_supply",
    Value_destiny = c(100, 200)
  )

  result <- .calculate_food_and_other_uses(pop_share, pie)

  expect_equal(result$food[result$Province_name == "Province1"], 100 * 0.25)
  expect_equal(
    result$other_uses[result$Province_name == "Province2"],
    200 * 0.75
  )
})

# Test: .combine_destinies -------------------------------------------
test_that(".combine_destinies merges correctly", {
  grafs <- tibble(
    Year = 2020,
    Province_name = "Province1",
    Item = "Wheat",
    Box = "Cropland",
    Irrig_cat = "Irrigated",
    production_n = 100
  )
  feed <- tibble(
    Year = 2020,
    Province_name = "Province1",
    Item = "Wheat",
    feed = 50,
    food_pets = 10
  )
  food_other <- tibble(
    Year = 2020,
    Province_name = "Province1",
    Item = "Wheat",
    food = 30,
    other_uses = 20
  )

  result <- .combine_destinies(grafs, feed, food_other)

  expect_equal(result$food, 30 + 10)
  expect_equal(result$feed, 50)
  expect_equal(result$other_uses, 20)
})

# Test: .convert_to_items_n ------------------------------------------
test_that(".convert_to_items_n calculates n_value correctly", {
  grafs_combined <- tibble(
    Year = 2020,
    Province_name = "Province1",
    Item = "Wheat",
    Box = "Cropland",
    Irrig_cat = "Irrigated",
    production_n = 100,
    food = 50,
    other_uses = 30,
    feed = 20
  )
  codes_cb <- tibble(item = "Wheat", Name_biomass = "Wheat")
  biomass_coefs <- tibble(
    Name_biomass = "Wheat",
    Product_kgDM_kgFM = 0.5,
    Product_kgN_kgDM = 0.03,
    Residue_kgDM_kgFM = 0.2,
    Residue_kgN_kgDM = 0.05
  )

  result <- .convert_to_items_n(grafs_combined, codes_cb, biomass_coefs)

  expect_equal(result$food, 50 * 0.5 * 0.03)
  expect_equal(result$feed, 20 * 0.5 * 0.03)
})

# Test: .calculate_trade ---------------------------------------------
test_that(".calculate_trade calculates export/import correctly", {
  grafs_prod_item_n <- tibble(
    Year = 2020,
    Province_name = "A",
    Item = c("Wheat", "Milk"),
    Box = c("Cropland", "Livestock"),
    Irrig_cat = c("Irrigated", NA),
    food = c(30, 2),
    other_uses = c(10, 1),
    feed = c(20, 2),
    production_n = c(60, 5)
  )

  result <- .calculate_trade(grafs_prod_item_n) |>
    arrange(Item)

  expected_order <- result$Item

  expected_food <- c(2, 30)[order(c("Milk", "Wheat"))]
  expected_other <- c(1, 10)[order(c("Milk", "Wheat"))]
  expected_feed <- c(2, 20)[order(c("Milk", "Wheat"))]
  expected_prod <- c(5, 60)[order(c("Milk", "Wheat"))]

  expect_equal(result$food, expected_food)
  expect_equal(result$other_uses, expected_other)
  expect_equal(result$feed, expected_feed)
  expect_equal(result$production_n, expected_prod)

  expected_export <- pmax(
    result$production_n - (result$food + result$other_uses + result$feed),
    0
  )
  expected_import <- pmax(
    -(result$production_n - (result$food + result$other_uses + result$feed)),
    0
  )

  expect_equal(result$export, expected_export)
  expect_equal(result$import, expected_import)
})


# Test: .finalize_prod_destiny -------------------------------------------------
test_that(".finalize_prod_destiny splits local, import (Outside) and export correctly", {
  grafs_prod_item_trade <- tibble(
    Year = 2020,
    Province_name = "A",
    Item = c("Wheat", "Milk", "Fish"),
    Box = c(NA, NA, "Fish"),
    Irrig_cat = c("Irrigated", NA, NA),
    food = c(30, 2, 1),
    feed = c(20, 2, 0),
    other_uses = c(10, 1, 0),
    production_n = c(60, 5, 1),
    export = c(5, 0, 0),
    import = c(0, 5, 0)
  )

  codes_coefs_items_full <- tibble(
    item = c("Wheat", "Milk", "Fish"),
    group = c("Crop products", "Livestock products", "Fish")
  )

  feed_share_rum_mono <- tibble(
    Year = 2020,
    Province_name = "A",
    Item = c("Wheat", "Milk", "Fish"),
    share_rum = c(0.5, 0.5, 0),
    share_mono = c(0.5, 0.5, 0)
  )

  result <- .finalize_prod_destiny(
    grafs_prod_item_trade,
    codes_coefs_items_full,
    n_soil_inputs = NULL,
    feed_share_rum_mono = feed_share_rum_mono
  )

  wheat_local <- result |>
    filter(Item == "Wheat", Destiny != "export", Origin != "Outside") |>
    pull(MgN)
  expect_true(all(wheat_local > 0))

  milk_import <- result |>
    filter(Item == "Milk", Origin == "Outside") |>
    pull(MgN)
  expect_equal(sum(milk_import), 5)

  wheat_export <- result |>
    filter(Item == "Wheat", Destiny == "export") |>
    pull(MgN)
  expect_equal(sum(wheat_export), 5)

  fish_origin <- result |>
    filter(Item == "Fish") |>
    pull(Origin)
  expect_true(all(fish_origin == "Fish"))
})

# Test: .add_n_soil_inputs ---------------------------------------------------
test_that(".add_n_soil_inputs adds soil N inputs in long format", {
  grafs_final <- tibble(
    Year = 2020,
    Province_name = "Province1",
    Item = "Wheat",
    Irrig_cat = "Irrigated",
    Origin = "Cropland",
    Destiny = "population_food",
    MgN = 100
  )

  soil_inputs <- tibble(
    Year = 2020,
    Province_name = "Province1",
    Item = "Wheat",
    Box = "Cropland",
    Irrig_cat = "Irrigated",
    deposition = 5,
    fixation = 3,
    synthetic = 2,
    manure = 10,
    urban = 7
  )

  result <- .add_n_soil_inputs(grafs_final, soil_inputs)

  expect_true(all(
    c("Deposition", "Fixation", "Synthetic", "Livestock", "People") %in%
      result$Origin
  ))

  expect_gt(nrow(result), nrow(grafs_final))

  deposition_val <- result[result$Origin == "Deposition", "MgN"][[1]]
  expect_equal(deposition_val, 5)
})
