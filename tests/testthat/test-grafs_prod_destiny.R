# Test: .summarise_crops_residues ---------------------------------------------
testthat::test_that(".summarise_crops_residues aggregates correctly", {
  fake_crop <- tibble::tibble(
    Year = c(2020, 2020),
    Province_name = c("Province1", "Province1"),
    Name_biomass = c("BiomassA", "BiomassA"),
    Item = c("ItemA", "ItemA"),
    Product_residue = c("Product", "Residue"),
    Prod_ygpit_Mg = c(50, 30),
    Irrig_cat = c("Irrigated", "Irrigated")
  )

  result <- .summarise_crops_residues(fake_crop)

  testthat::expect_equal(base::nrow(result), 2)
  testthat::expect_equal(result$production_fm[1], 50)
  testthat::expect_equal(result$production_fm[2], 30)
  testthat::expect_equal(base::unique(result$Box), "Cropland")
})

# Test: .aggregate_crop_seminatural -------------------------------------------
testthat::test_that(".aggregate_crop_seminatural combines correctly", {
  fake_crop_residue <- tibble::tibble(
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

  fake_npp_merged <- tibble::tibble(
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

  testthat::expect_true(base::any(result$Item == "Fallow"))
  testthat::expect_true(base::any(result$Item == "Shrub"))
  testthat::expect_equal(base::sum(result$production_fm), 50 + 15 + 40 + 10 + 5)
})

# Test: .combine_production_boxes ---------------------------------------------
testthat::test_that(".combine_production_boxes binds correctly", {
  fake_crop <- tibble::tibble(
    Year = 2020,
    Province_name = "Province1",
    Item = "ItemA",
    production_fm = 50,
    Box = "Cropland"
  )

  fake_livestock <- tibble::tibble(
    Year = 2020,
    Province_name = "Province1",
    Item = "Cow",
    Prod_Mg = 100,
    Box = "Livestock",
    prod_type = "Product"
  )

  result <- .combine_production_boxes(fake_crop, fake_livestock)

  testthat::expect_equal(base::nrow(result), 2)
  testthat::expect_equal(result$production_fm[result$Item == "Cow"], 100)
})

# Test: .remove_seeds_from_system --------------------------------------------
testthat::test_that(".remove_seeds_from_system subtracts seeds correctly", {
  fake_npp <- tibble::tibble(
    Year = 2020,
    Province_name = "Province1",
    Item = "Wheat",
    LandUse = "Cropland",
    Area_ygpit_ha = 100
  )

  fake_pie <- tibble::tibble(
    Year = 2020,
    Item = "Wheat",
    Element = "Domestic_supply",
    Destiny = "Seed",
    Value_destiny = 50
  )

  fake_prod <- tibble::tibble(
    Year = 2020,
    Province_name = "Province1",
    Item = "Wheat",
    production_fm = 500,
    Box = "Cropland"
  )

  result <- .remove_seeds_from_system(fake_npp, fake_pie, fake_prod)

  testthat::expect_equal(result$production_fm[1], 500 - (50 / 100 * 100))
})

# Test: .add_grass_wood ---------------------------------------------
testthat::test_that(".add_grass_wood transforms items correctly", {
  fake_data <- tibble::tibble(
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

  testthat::expect_true(base::any(result$Item == "Fallow"))
  testthat::expect_true(base::any(result$Item == "Grassland"))
  testthat::expect_true(base::any(result$Item == "Firewood"))
  testthat::expect_equal(
    result$production_fm[result$Item == "Grassland"],
    20 / 0.2
  )
})

# Test: .prepare_processed_data ---------------------------------------
testthat::test_that(".prepare_processed_data sums processed items correctly", {
  fake_processed <- tibble::tibble(
    Year = c(2020, 2020),
    Province_name = c("Province1", "Province1"),
    Name_biomass = c("Wheat", "Wheat"),
    Item = c("Wheat", "Wheat"),
    ProcessedItem = c("Alcohol", "Alcohol"),
    ProcessedItem_amount = c(5, 15)
  )

  result <- .prepare_processed_data(fake_processed)

  testthat::expect_equal(base::nrow(result), 1)
  testthat::expect_equal(result$production_fm[1], 20)
  testthat::expect_equal(result$Item[1], "Alcohol")
  testthat::expect_equal(result$Box[1], "Cropland")
})

# Test: .prepare_prod_data --------------------------------------------
testthat::test_that(".prepare_prod_data combines grafs and processed data", {
  grafs_data <- tibble::tibble(
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

  processed_data <- tibble::tibble(
    Year = 2020,
    Province_name = "Province1",
    Name_biomass = "Wheat",
    Item = "Alcohol",
    Box = "Cropland",
    production_fm = 20,
    prod_type = "Product"
  )

  codes_cb <- tibble::tibble(
    item = c("Grassland", "Alcohol"),
    Name_biomass = c("Grass", "Wheat")
  )

  result <- .prepare_prod_data(grafs_data, processed_data, codes_cb)

  testthat::expect_equal(base::nrow(result), 2)
  testthat::expect_true(base::all(c("Grassland", "Alcohol") %in% result$Item))
  testthat::expect_true("Name_biomass" %in% base::colnames(result))
})

# Test: .convert_fm_dm_n ---------------------------------------------
testthat::test_that(".convert_fm_dm_n calculates N correctly", {
  fake_prod <- tibble::tibble(
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

  biomass_coefs <- tibble::tibble(
    Name_biomass = "Grass",
    Product_kgDM_kgFM = 0.5,
    Residue_kgDM_kgFM = 0.2,
    Product_kgN_kgDM = 0.03,
    Residue_kgN_kgDM = 0.05
  )

  result <- .convert_fm_dm_n(fake_prod, biomass_coefs)

  testthat::expect_equal(base::nrow(result), 1)
  testthat::expect_equal(result$production_n[1], 10 * 0.2 * 0.05)
})

# Test: .adding_feed ------------------------------------------------
testthat::test_that(".adding_feed calculates feed and shares correctly", {
  fake_feed <- tibble::tibble(
    Year = 2020,
    Province_name = "Province1",
    Item = c("Wheat", "Wheat", "Wheat", "Wheat"),
    FM_Mg = c(50, 10, 5, 7),
    Livestock_cat = c("Cattle_meat", "Sheep", "Pets", "Pigs")
  )

  result <- .adding_feed(fake_feed)

  feed_df <- result$feed_intake
  share_df <- result$feed_share_rum_mono

  testthat::expect_equal(
    base::sum(feed_df$feed[feed_df$Livestock_type == "ruminant"], na.rm = TRUE),
    60
  )
  testthat::expect_equal(
    base::sum(
      feed_df$feed[feed_df$Livestock_type == "monogastric"],
      na.rm = TRUE
    ),
    7
  )

  testthat::expect_equal(base::sum(feed_df$food_pets), 5)

  wheat_share <- share_df |>
    dplyr::filter(
      Item == "Wheat",
      Province_name == "Province1",
      Year == 2020
    ) |>
    dplyr::slice(1)

  testthat::expect_equal(wheat_share$share_rum, 60 / (60 + 7))
  testthat::expect_equal(wheat_share$share_mono, 7 / (60 + 7))
})

# Test: .calculate_population_share ---------------------------------
testthat::test_that(".calculate_population_share calculates shares correctly", {
  pop <- tibble::tibble(
    Year = 2020,
    Province_name = c("Province1", "Province2"),
    Pop_Mpeop_yg = c(100, 300)
  )

  result <- .calculate_population_share(pop)

  testthat::expect_equal(result$Pop_share[1], 100 / 400)
  testthat::expect_equal(result$Pop_share[2], 300 / 400)
})

# Test: .calculate_food_and_other_uses -------------------------------
testthat::test_that(".calculate_food_and_other_uses scales by population share", {
  pop_share <- tibble::tibble(
    Year = 2020,
    Province_name = c("Province1", "Province2"),
    Pop_share = c(0.25, 0.75)
  )
  pie <- tibble::tibble(
    Year = 2020,
    Item = "Wheat",
    Destiny = c("Food", "Other_uses"),
    Element = "Domestic_supply",
    Value_destiny = c(100, 200)
  )

  result <- .calculate_food_and_other_uses(pop_share, pie)

  testthat::expect_equal(
    result$food[result$Province_name == "Province1"],
    100 * 0.25
  )
  testthat::expect_equal(
    result$other_uses[result$Province_name == "Province2"],
    200 * 0.75
  )
})

# Test: .combine_destinies -------------------------------------------
testthat::test_that(".combine_destinies merges correctly", {
  grafs <- tibble::tibble(
    Year = 2020,
    Province_name = "Province1",
    Item = "Wheat",
    Box = "Cropland",
    Irrig_cat = "Irrigated",
    production_n = 100
  )
  feed <- tibble::tibble(
    Year = 2020,
    Province_name = "Province1",
    Item = "Wheat",
    feed = 50,
    food_pets = 10
  )
  food_other <- tibble::tibble(
    Year = 2020,
    Province_name = "Province1",
    Item = "Wheat",
    food = 30,
    other_uses = 20
  )

  result <- .combine_destinies(grafs, feed, food_other)

  testthat::expect_equal(result$food, 30 + 10)
  testthat::expect_equal(result$feed, 50)
  testthat::expect_equal(result$other_uses, 20)
})

# Test: .convert_to_items_n ------------------------------------------
testthat::test_that(".convert_to_items_n calculates n_value correctly", {
  grafs_combined <- tibble::tibble(
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
  codes_cb <- tibble::tibble(item = "Wheat", Name_biomass = "Wheat")
  biomass_coefs <- tibble::tibble(
    Name_biomass = "Wheat",
    Product_kgDM_kgFM = 0.5,
    Product_kgN_kgDM = 0.03,
    Residue_kgDM_kgFM = 0.2,
    Residue_kgN_kgDM = 0.05
  )

  result <- .convert_to_items_n(grafs_combined, codes_cb, biomass_coefs)

  testthat::expect_equal(result$food, 50 * 0.5 * 0.03)
  testthat::expect_equal(result$feed, 20 * 0.5 * 0.03)
})
