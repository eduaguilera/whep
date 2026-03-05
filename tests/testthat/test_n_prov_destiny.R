# Tests for n_prov_destiny.R functions
testthat::local_edition(3)


# .merge_items_biomass ---------------------------------------------------------

test_that(".merge_items_biomass joins Item from biomass codes", {
  npp <- tibble::tribble(
    ~Year, ~Province_name, ~Name_biomass, ~LandUse,
    2000, "A", "Wheat", "Cropland",
    2000, "A", "Barley", "Cropland"
  )

  codes <- tibble::tribble(
    ~Name_biomass, ~Item,
    "Wheat", "Wheat and products",
    "Barley", "Barley and products"
  )

  out <- .merge_items_biomass(npp, codes)

  expect_equal(nrow(out), 2)
  expect_equal(
    out$Item,
    c("Wheat and products", "Barley and products")
  )
})

test_that(".merge_items_biomass returns NA for unmatched biomass", {
  npp <- tibble::tribble(
    ~Year, ~Province_name, ~Name_biomass, ~LandUse,
    2000, "A", "Unknown", "Cropland"
  )

  codes <- tibble::tribble(
    ~Name_biomass, ~Item,
    "Wheat", "Wheat and products"
  )

  out <- .merge_items_biomass(npp, codes)

  expect_true(is.na(out$Item))
})


# .summarise_crops_residues ----------------------------------------------------

test_that(".summarise_crops_residues groups and sums correctly", {
  crop_data <- tibble::tribble(
    ~Year, ~Province_name, ~Name_biomass, ~Item, ~Product_residue,
    ~LandUse, ~Irrig_cat, ~Prod_ygpit_Mg,
    2000, "A", "Wheat", "Wheat", "Product",
    "Cropland", "irrig", 100,
    2000, "A", "Wheat", "Wheat", "Product",
    "Cropland", "irrig", 50,
    2000, "A", "Wheat", "Wheat", "Residue",
    "Cropland", "rainfed", 30
  )

  out <- .summarise_crops_residues(crop_data)

  expect_equal(out$Box |> unique(), "Cropland")
  expect_true("prod_type" %in% names(out))

  product_row <- out |>
    dplyr::filter(prod_type == "Product")

  expect_equal(product_row$production_fm, 150)

  residue_row <- out |>
    dplyr::filter(prod_type == "Residue")

  expect_equal(residue_row$production_fm, 30)
})


# .aggregate_crop_seminatural --------------------------------------------------

test_that(".aggregate_crop_seminatural combines fallow, semi-natural, crops", {
  npp_merged <- tibble::tribble(
    ~Year, ~Province_name, ~Name_biomass, ~Item, ~LandUse,
    ~Irrig_cat, ~GrazedWeeds_MgDM, ~Prod_ygpit_Mg,
    ~Used_Residue_MgFM,
    2000, "A", "Fallow", "Fallow", "Cropland",
    "rainfed", 10, 0, 0,
    2000, "A", "Grass", "Grassland", "Dehesa",
    NA, 20, 5, 3
  )

  crop_prod <- tibble::tribble(
    ~Year, ~Province_name, ~Name_biomass, ~Item, ~prod_type,
    ~LandUse, ~Irrig_cat, ~production_fm, ~Box,
    2000, "A", "Wheat", "Wheat", "Product",
    "Cropland", "irrig", 100, "Cropland"
  )

  out <- .aggregate_crop_seminatural(npp_merged, crop_prod)

  # crops from crop_prod
  expect_true("Cropland" %in% out$Box)
  # semi-natural from npp_merged
  expect_true("semi_natural_agroecosystems" %in% out$Box)
  # fallow grazed
  fallow <- out |>
    dplyr::filter(Item == "Fallow", prod_type == "Grass")
  expect_equal(fallow$production_fm, 10)
  expect_equal(fallow$Box, "Cropland")
})


# .prepare_livestock_production ------------------------------------------------

test_that(".prepare_livestock_production structures livestock data", {
  livestock <- tibble::tribble(
    ~Year, ~Province_name, ~Item, ~Name_biomass, ~Prod_Mg,
    2000, "A", "Beef", "Cattle", 500,
    2000, "A", "Milk", "Cattle", 300
  )

  out <- .prepare_livestock_production(livestock)

  expect_equal(nrow(out), 2)
  expect_true(all(out$Box == "Livestock"))
  expect_true(all(out$prod_type == "Product"))
  expect_equal(out$Prod_Mg, c(500, 300))
})


# .combine_production_boxes ---------------------------------------------------

test_that(".combine_production_boxes binds crop and livestock data", {
  crops <- tibble::tribble(
    ~Year, ~Province_name, ~Item, ~production_fm, ~Box,
    2000, "A", "Wheat", 100, "Cropland"
  )

  livestock <- tibble::tribble(
    ~Year, ~Province_name, ~Item, ~Prod_Mg, ~Box,
    2000, "A", "Beef", 500, "Livestock"
  )

  out <- .combine_production_boxes(crops, livestock)

  expect_equal(nrow(out), 2)
  expect_setequal(out$Box, c("Cropland", "Livestock"))
  expect_true("production_fm" %in% names(out))
})


# .add_grass_wood --------------------------------------------------------------

test_that(".add_grass_wood reclassifies grass items and converts DM to FM", {
  input <- tibble::tribble(
    ~Year, ~Province_name, ~Name_biomass, ~Item, ~Box,
    ~LandUse, ~Irrig_cat, ~prod_type, ~production_fm,
    2000, "A", "Fallow", "Fallow", "Cropland",
    "Cropland", "rainfed", "Grass", 10,
    2000, "A", "SomeGrass", "SomeItem", "semi_natural_agroecosystems",
    "Dehesa", NA, "Grass", 20,
    2000, "A", "Wheat", "Wheat", "Cropland",
    "Cropland", "irrig", "Product", 100
  )

  out <- .add_grass_wood(input)

  # Fallow grass stays as Fallow item
  fallow <- out |> dplyr::filter(Item == "Fallow")
  expect_equal(fallow$production_fm, 10)
  expect_equal(fallow$Name_biomass, "Fallow")

  # Non-fallow grass becomes Grassland, DM → FM (/ 0.2)
  grassland <- out |> dplyr::filter(Item == "Grassland")
  expect_equal(grassland$production_fm, 20 / 0.2)
  expect_equal(grassland$Name_biomass, "Grass")

  # Regular product unchanged
  wheat <- out |> dplyr::filter(Item == "Wheat")
  expect_equal(wheat$production_fm, 100)
})

test_that(".add_grass_wood reclassifies firewood from semi-natural residues", {
  input <- tibble::tribble(
    ~Year, ~Province_name, ~Name_biomass, ~Item, ~Box,
    ~LandUse, ~Irrig_cat, ~prod_type, ~production_fm,
    2000, "A", "Holm oak forest", "Holm oak", "semi_natural_agroecosystems",
    "Forest_low", NA, "Residue", 50,
    2000, "A", "Conifers", "Conifers", "semi_natural_agroecosystems",
    "Forest_low", NA, "Residue", 30
  )

  out <- .add_grass_wood(input)

  expect_true(all(out$Item == "Firewood"))
  expect_true(all(out$Name_biomass == "Firewood"))
})

test_that(".add_grass_wood filters out NA production", {
  input <- tibble::tribble(
    ~Year, ~Province_name, ~Name_biomass, ~Item, ~Box,
    ~LandUse, ~Irrig_cat, ~prod_type, ~production_fm,
    2000, "A", "Wheat", "Wheat", "Cropland",
    "Cropland", "irrig", "Product", NA_real_,
    2000, "A", "Barley", "Barley", "Cropland",
    "Cropland", "irrig", "Product", 50
  )

  out <- .add_grass_wood(input)

  expect_equal(nrow(out), 1)
  expect_equal(out$Item, "Barley")
})


# .prepare_processed_data -----------------------------------------------------

test_that(".prepare_processed_data summarises processed items", {
  processed <- tibble::tribble(
    ~Year, ~Province_name, ~Name_biomass, ~Item, ~ProcessedItem,
    ~ProcessedItem_amount,
    2000, "A", "Wheat", "Wheat", "Flour", 40,
    2000, "A", "Wheat", "Wheat", "Flour", 10,
    2000, "A", "Wheat", "Wheat", "Bran", 20
  )

  out <- .prepare_processed_data(processed)

  expect_equal(nrow(out), 2)
  expect_true(all(out$Box == "Cropland"))
  expect_true(all(out$prod_type == "Product"))

  flour <- out |> dplyr::filter(Item == "Flour")
  expect_equal(flour$production_fm, 50)

  bran <- out |> dplyr::filter(Item == "Bran")
  expect_equal(bran$production_fm, 20)
})


# .prepare_prod_data -----------------------------------------------------------

test_that(".prepare_prod_data merges biomass names from codes", {
  grafs_added <- tibble::tribble(
    ~Year, ~Province_name, ~Name_biomass, ~Item, ~Box,
    ~LandUse, ~Irrig_cat, ~prod_type, ~production_fm,
    2000, "A", "Wheat", "WheatItem", "Cropland",
    "Cropland", "irrig", "Product", 100
  )

  processed <- tibble::tribble(
    ~Year, ~Province_name, ~Name_biomass, ~Item, ~Box,
    ~production_fm, ~prod_type,
    2000, "A", "Wheat", "Flour", "Cropland", 30, "Product"
  )

  codes <- tibble::tribble(
    ~item, ~Name_biomass,
    "WheatItem", "Wheat_biomass",
    "Flour", "Flour_biomass"
  )

  out <- .prepare_prod_data(grafs_added, processed, codes)

  expect_true("Name_biomass_primary" %in% names(out))
  expect_true("Name_biomass" %in% names(out))
  expect_equal(nrow(out), 2)

  # Name_biomass should be from codes when available
  wheat_row <- out |> dplyr::filter(Item == "WheatItem")
  expect_equal(wheat_row$Name_biomass, "Wheat_biomass")
})

test_that(".prepare_prod_data falls back to primary when code is missing", {
  grafs_added <- tibble::tribble(
    ~Year, ~Province_name, ~Name_biomass, ~Item, ~Box,
    ~LandUse, ~Irrig_cat, ~prod_type, ~production_fm,
    2000, "A", "OrigBiomass", "UnknownItem", "Cropland",
    "Cropland", "irrig", "Product", 50
  )

  processed <- tibble::tibble(
    Year = integer(),
    Province_name = character(),
    Name_biomass = character(),
    Item = character(),
    Box = character(),
    production_fm = numeric(),
    prod_type = character()
  )

  codes <- tibble::tribble(
    ~item, ~Name_biomass,
    "OtherItem", "OtherBiomass"
  )

  out <- .prepare_prod_data(grafs_added, processed, codes)

  # Should fall back to Name_biomass_primary
  expect_equal(out$Name_biomass, "OrigBiomass")
})


# .convert_fm_dm_n -------------------------------------------------------------

test_that(".convert_fm_dm_n converts FM to DM to N correctly", {
  merged <- tibble::tribble(
    ~Year, ~Province_name, ~Item, ~Box, ~LandUse, ~Irrig_cat,
    ~prod_type, ~production_fm, ~Name_biomass_primary, ~Name_biomass,
    2000, "A", "Wheat", "Cropland", "Cropland", "irrig",
    "Product", 1000, "Wheat", "Wheat",
    2000, "A", "Straw", "Cropland", "Cropland", "irrig",
    "Residue", 500, "Wheat", "Wheat"
  )

  coefs <- tibble::tribble(
    ~Name_biomass, ~Product_kgDM_kgFM, ~Residue_kgDM_kgFM,
    ~Product_kgN_kgDM, ~Residue_kgN_kgDM,
    "Wheat", 0.88, 0.85, 0.02, 0.005
  )

  out <- .convert_fm_dm_n(merged, coefs)

  product <- out |>
    dplyr::filter(prod_type == "Product")
  expected_n_product <- 1000 * 0.88 * 0.02
  expect_equal(product$production_n, expected_n_product)

  residue <- out |>
    dplyr::filter(prod_type == "Residue")
  expected_n_residue <- 500 * 0.85 * 0.005
  expect_equal(residue$production_n, expected_n_residue)
})

test_that(".convert_fm_dm_n uses primary biomass for special items", {
  merged <- tibble::tribble(
    ~Year, ~Province_name, ~Item, ~Box, ~LandUse, ~Irrig_cat,
    ~prod_type, ~production_fm, ~Name_biomass_primary, ~Name_biomass,
    2000, "A", "Nuts and products", "Cropland", "Cropland",
    "irrig", "Product", 100, "Almond", "NutsGeneric"
  )

  coefs <- tibble::tribble(
    ~Name_biomass, ~Product_kgDM_kgFM, ~Residue_kgDM_kgFM,
    ~Product_kgN_kgDM, ~Residue_kgN_kgDM,
    "Almond", 0.9, 0.8, 0.03, 0.01,
    "NutsGeneric", 0.5, 0.5, 0.01, 0.01
  )

  out <- .convert_fm_dm_n(merged, coefs)

  # Should use Almond (primary) coefs, not NutsGeneric
  expect_equal(out$production_n, 100 * 0.9 * 0.03)
})

test_that(".convert_fm_dm_n filters NA Item with zero production", {
  merged <- tibble::tribble(
    ~Year, ~Province_name, ~Item, ~Box, ~LandUse, ~Irrig_cat,
    ~prod_type, ~production_fm, ~Name_biomass_primary, ~Name_biomass,
    2000, "A", NA_character_, "Cropland", "Cropland", "irrig",
    "Product", 0, "Something", "Something",
    2000, "A", "Wheat", "Cropland", "Cropland", "irrig",
    "Product", 100, "Wheat", "Wheat"
  )

  coefs <- tibble::tribble(
    ~Name_biomass, ~Product_kgDM_kgFM, ~Residue_kgDM_kgFM,
    ~Product_kgN_kgDM, ~Residue_kgN_kgDM,
    "Something", 0.5, 0.5, 0.01, 0.01,
    "Wheat", 0.88, 0.85, 0.02, 0.005
  )

  out <- .convert_fm_dm_n(merged, coefs)

  # NA Item with 0 production should be filtered out
  expect_equal(nrow(out), 1)
  expect_equal(out$Item, "Wheat")
})


# .add_feed --------------------------------------------------------------------

test_that(".add_feed classifies livestock and computes feed shares", {
  intake <- tibble::tribble(
    ~Year, ~Province_name, ~Item, ~Livestock_cat, ~FM_Mg,
    2000, "A", "Wheat", "Cattle_meat", 60,
    2000, "A", "Wheat", "Pigs", 30,
    2000, "A", "Wheat", "Pets", 10,
    2000, "A", "Wheat", "Aquaculture", 5
  )

  out <- .add_feed(intake)

  expect_named(out, c("feed_intake", "feed_share_rum_mono"))

  # feed_intake
  fi <- out$feed_intake
  expect_equal(fi$feed, 60 + 30 + 5)
  expect_equal(fi$food_pets, 10)

  # feed shares
  fs <- out$feed_share_rum_mono
  expect_equal(fs$share_rum, 60 / 95, tolerance = 1e-12)
  expect_equal(fs$share_mono, 30 / 95, tolerance = 1e-12)
})

test_that(".add_feed handles zero feed total gracefully", {
  intake <- tibble::tribble(
    ~Year, ~Province_name, ~Item, ~Livestock_cat, ~FM_Mg,
    2000, "A", "Wheat", "Cattle_meat", 0,
    2000, "A", "Wheat", "Pigs", 0,
    2000, "A", "Wheat", "Aquaculture", 0,
    2000, "A", "Wheat", "Pets", 10
  )

  out <- .add_feed(intake)

  expect_equal(out$feed_intake$feed, 0)
  expect_equal(out$feed_share_rum_mono$share_rum, 0)
  expect_equal(out$feed_share_rum_mono$share_mono, 0)
})

test_that(".add_feed maps all livestock types correctly", {
  intake <- tibble::tribble(
    ~Year, ~Province_name, ~Item, ~Livestock_cat, ~FM_Mg,
    2000, "A", "Grain", "Sheep", 10,
    2000, "A", "Grain", "Goats", 10,
    2000, "A", "Grain", "Horses", 10,
    2000, "A", "Grain", "Donkeys_mules", 10,
    2000, "A", "Grain", "Poultry", 20,
    2000, "A", "Grain", "Rabbits", 10,
    2000, "A", "Grain", "Fur animals", 5,
    2000, "A", "Grain", "Other", 5,
    2000, "A", "Grain", "Aquaculture", 0,
    2000, "A", "Grain", "Pets", 0
  )

  out <- .add_feed(intake)

  fi <- out$feed_intake
  # rum = 40 (Sheep+Goats+Horses+Donkeys), mono = 40 (Poul+Rab+Fur+Other)
  expect_equal(fi$feed, 80)

  fs <- out$feed_share_rum_mono
  expect_equal(fs$share_rum, 40 / 80)
  expect_equal(fs$share_mono, 40 / 80)
})


# .calculate_population_share -------------------------------------------------

test_that(".calculate_population_share computes correct shares", {
  pop <- tibble::tribble(
    ~Year, ~Province_name, ~Pop_Mpeop_yg,
    2000, "A", 3,
    2000, "B", 7
  )

  out <- .calculate_population_share(pop)

  expect_equal(
    out$Pop_share[out$Province_name == "A"],
    0.3
  )
  expect_equal(
    out$Pop_share[out$Province_name == "B"],
    0.7
  )
})


# .calculate_food_and_other_uses -----------------------------------------------

test_that(".calculate_food_and_other_uses distributes by pop share", {
  pop_share <- tibble::tribble(
    ~Year, ~Province_name, ~Pop_Mpeop_yg, ~Pop_share,
    2000, "A", 3, 0.3,
    2000, "B", 7, 0.7
  )

  pie_data <- tibble::tribble(
    ~Year, ~Item, ~Element, ~Destiny, ~Value_destiny,
    2000, "Wheat", "Domestic_supply", "Food", 100,
    2000, "Wheat", "Domestic_supply", "Other_uses", 50
  )

  out <- .calculate_food_and_other_uses(pop_share, pie_data)

  a_row <- out |> dplyr::filter(Province_name == "A")
  expect_equal(a_row$food, 30)
  expect_equal(a_row$other_uses, 15)

  b_row <- out |> dplyr::filter(Province_name == "B")
  expect_equal(b_row$food, 70)
  expect_equal(b_row$other_uses, 35)
})


# .combine_destinies -----------------------------------------------------------

test_that(".combine_destinies merges production with consumption data", {
  prod <- tibble::tribble(
    ~Year, ~Province_name, ~Item, ~Box, ~Irrig_cat,
    ~production_n, ~prod_type,
    2000, "A", "Wheat", "Cropland", "irrig", 60, "Product",
    2000, "A", "Wheat", "Cropland", "rainfed", 40, "Product"
  )

  feed <- tibble::tribble(
    ~Year, ~Province_name, ~Item, ~feed, ~food_pets,
    2000, "A", "Wheat", 30, 0
  )

  food_other <- tibble::tribble(
    ~Year, ~Province_name, ~Item, ~food, ~other_uses,
    2000, "A", "Wheat", 50, 20
  )

  out <- .combine_destinies(prod, feed, food_other)

  # production_share: irrig = 60/100, rainfed = 40/100
  irrig <- out |> dplyr::filter(Irrig_cat == "irrig")
  expect_equal(irrig$food, 50 * 0.6)
  expect_equal(irrig$feed, 30 * 0.6)
  expect_equal(irrig$other_uses, 20 * 0.6)

  rainfed <- out |> dplyr::filter(Irrig_cat == "rainfed")
  expect_equal(rainfed$food, 50 * 0.4)
  expect_equal(rainfed$feed, 30 * 0.4)
})

test_that(".combine_destinies adds food_pets to food", {
  prod <- tibble::tribble(
    ~Year, ~Province_name, ~Item, ~Box, ~Irrig_cat,
    ~production_n, ~prod_type,
    2000, "A", "FishMeal", "Fish", NA, 10, "Product"
  )

  feed <- tibble::tribble(
    ~Year, ~Province_name, ~Item, ~feed, ~food_pets,
    2000, "A", "FishMeal", 0, 5
  )

  food_other <- tibble::tribble(
    ~Year, ~Province_name, ~Item, ~food, ~other_uses,
    2000, "A", "FishMeal", 10, 0
  )

  out <- .combine_destinies(prod, feed, food_other)

  # food should include food_pets
  expect_equal(out$food, (10 + 5) * 1)
})


# .convert_to_items_n ----------------------------------------------------------

test_that(".convert_to_items_n converts consumption FM to N", {
  combined <- tibble::tribble(
    ~Year, ~Province_name, ~Item, ~Box, ~Irrig_cat,
    ~production_n, ~food, ~other_uses, ~feed,
    2000, "A", "Wheat", "Cropland", "irrig",
    100, 50, 20, 30
  )

  codes <- tibble::tribble(
    ~item, ~Name_biomass,
    "Wheat", "Wheat"
  )

  coefs <- tibble::tribble(
    ~Name_biomass, ~Product_kgDM_kgFM, ~Product_kgN_kgDM,
    ~Residue_kgDM_kgFM, ~Residue_kgN_kgDM,
    "Wheat", 0.88, 0.02, 0.85, 0.005
  )

  out <- .convert_to_items_n(combined, codes, coefs)

  # Wheat is "Product" type → uses Product coefs
  conv_factor <- 0.88 * 0.02
  expect_equal(out$food, 50 * conv_factor)
  expect_equal(out$other_uses, 20 * conv_factor)
  expect_equal(out$feed, 30 * conv_factor)
})

test_that(".convert_to_items_n uses residue coefs for Grass items", {
  combined <- tibble::tribble(
    ~Year, ~Province_name, ~Item, ~Box, ~Irrig_cat,
    ~production_n, ~food, ~other_uses, ~feed,
    2000, "A", "Grassland", "semi_natural_agroecosystems",
    NA, 0, 0, 0, 100
  )

  codes <- tibble::tribble(
    ~item, ~Name_biomass,
    "Grassland", "Grass"
  )

  coefs <- tibble::tribble(
    ~Name_biomass, ~Product_kgDM_kgFM, ~Product_kgN_kgDM,
    ~Residue_kgDM_kgFM, ~Residue_kgN_kgDM,
    "Grass", 0.3, 0.01, 0.2, 0.025
  )

  out <- .convert_to_items_n(combined, codes, coefs)

  # Grass → uses Residue coefs
  expect_equal(out$feed, 100 * 0.2 * 0.025)
})


# .calculate_trade -------------------------------------------------------------

test_that(".calculate_trade computes export when production > consumption", {
  trade_input <- tibble::tribble(
    ~Year, ~Province_name, ~Item, ~Box, ~Irrig_cat,
    ~Name_biomass, ~prod_type, ~production_n,
    ~food, ~other_uses, ~feed,
    2000, "A", "Wheat", "Cropland", "irrig",
    "Wheat", "Product", 100, 30, 10, 20
  )

  out <- .calculate_trade(trade_input)

  # consumption = 30 + 10 + 20 = 60, net_trade = 100 - 60 = 40
  expect_equal(out$export, 40)
  expect_equal(out$import, 0)
})

test_that(".calculate_trade computes import when consumption > production", {
  trade_input <- tibble::tribble(
    ~Year, ~Province_name, ~Item, ~Box, ~Irrig_cat,
    ~Name_biomass, ~prod_type, ~production_n,
    ~food, ~other_uses, ~feed,
    2000, "A", "Wheat", "Cropland", "irrig",
    "Wheat", "Product", 30, 50, 10, 20
  )

  out <- .calculate_trade(trade_input)

  # consumption = 80, net_trade = 30 - 80 = -50
  expect_equal(out$export, 0)
  expect_equal(out$import, 50)
})


# .prep_final_ds ---------------------------------------------------------------

test_that(".prep_final_ds assigns Box from group codes", {
  trade <- tibble::tribble(
    ~Year, ~Province_name, ~Item, ~Box, ~Irrig_cat,
    ~food, ~other_uses, ~feed, ~production_n, ~export, ~import,
    2000, "A", "Sugar", NA, "irrig",
    10, 5, 0, 15, 0, 0,
    2000, "A", "Beef", NA, NA,
    10, 0, 0, 10, 0, 0,
    2000, "A", "FishProd", NA, NA,
    5, 0, 0, 5, 0, 0,
    2000, "A", "Additive", NA, NA,
    2, 0, 0, 2, 0, 0
  )

  codes <- tibble::tribble(
    ~item, ~group,
    "Sugar", "Crop products",
    "Beef", "Livestock products",
    "FishProd", "Fish",
    "Additive", "Additives"
  )

  out <- .prep_final_ds(trade, codes)

  expect_equal(
    out$Box[out$Item == "Sugar"],
    "Cropland"
  )
  expect_equal(
    out$Box[out$Item == "Beef"],
    "Livestock"
  )
  expect_equal(
    out$Box[out$Item == "FishProd"],
    "Fish"
  )
  # Additives → recoded to Agro-industry
  expect_equal(
    out$Box[out$Item == "Additive"],
    "Agro-industry"
  )
  # Irrig_cat NA for non-Cropland
  expect_true(is.na(out$Irrig_cat[out$Item == "Beef"]))
})

test_that(".prep_final_ds recognises Fallow and Acorns", {
  trade <- tibble::tribble(
    ~Year, ~Province_name, ~Item, ~Box, ~Irrig_cat,
    ~food, ~other_uses, ~feed, ~production_n, ~export, ~import,
    2000, "A", "Fallow", NA, NA, 0, 0, 5, 5, 0, 0,
    2000, "A", "Acorns", NA, NA, 0, 0, 3, 3, 0, 0
  )

  codes <- tibble::tribble(
    ~item, ~group,
    "Fallow", "Other",
    "Acorns", "Other"
  )

  out <- .prep_final_ds(trade, codes)

  expect_equal(out$Box[out$Item == "Fallow"], "Cropland")
  expect_equal(
    out$Box[out$Item == "Acorns"],
    "semi_natural_agroecosystems"
  )
})


# .calculate_consumption_shares ------------------------------------------------

test_that(".calculate_consumption_shares computes correct shares", {
  data <- tibble::tribble(
    ~Year, ~Province_name, ~Item, ~Irrig_cat,
    ~food, ~other_uses, ~feed,
    2000, "A", "Wheat", "irrig", 60, 20, 20
  )

  out <- .calculate_consumption_shares(data)

  expect_equal(out$food_share, 0.6)
  expect_equal(out$other_uses_share, 0.2)
  expect_equal(out$feed_share, 0.2)
  expect_equal(out$consumption_total, 100)
})

test_that(".calculate_consumption_shares returns 0 when total is 0", {
  data <- tibble::tribble(
    ~Year, ~Province_name, ~Item, ~Irrig_cat,
    ~food, ~other_uses, ~feed,
    2000, "A", "Wheat", "irrig", 0, 0, 0
  )

  out <- .calculate_consumption_shares(data)

  expect_equal(out$food_share, 0)
  expect_equal(out$other_uses_share, 0)
  expect_equal(out$feed_share, 0)
})


# .split_local_consumption -----------------------------------------------------

test_that(".split_local_consumption splits by shares and feed type", {
  local_import <- tibble::tribble(
    ~Year, ~Province_name, ~Item, ~Box, ~Irrig_cat,
    ~local_consumption, ~import_consumption,
    ~food_share, ~feed_share, ~other_uses_share,
    2000, "A", "Wheat", "Cropland", "irrig",
    100, 0, 0.5, 0.3, 0.2
  )

  feed_shares <- tibble::tribble(
    ~Year, ~Province_name, ~Item, ~share_rum, ~share_mono,
    2000, "A", "Wheat", 0.6, 0.4
  )

  out <- .split_local_consumption(local_import, feed_shares)

  vals <- out |>
    dplyr::select(Destiny, MgN) |>
    tibble::deframe()

  expect_equal(vals[["population_food"]], 50)
  expect_equal(vals[["population_other_uses"]], 20)
  expect_equal(vals[["livestock_rum"]], 100 * 0.3 * 0.6)
  expect_equal(vals[["livestock_mono"]], 100 * 0.3 * 0.4)
  expect_equal(unique(out$Origin), "Cropland")
})


# .split_import_consumption ----------------------------------------------------

test_that(".split_import_consumption limits imports and splits", {
  local_vs_import <- tibble::tribble(
    ~Year, ~Province_name, ~Item, ~Box,
    ~local_consumption, ~import_consumption,
    ~food_share, ~feed_share, ~other_uses_share,
    2000, "A", "Wheat", "Cropland", 50, 30, 0.6, 0.3, 0.1,
    2000, "A", "FishProd", "Fish", 20, 10, 0.5, 0.4, 0.1
  )

  feed_share_rum_mono <- tibble::tribble(
    ~Year, ~Province_name, ~Item, ~share_rum, ~share_mono,
    2000, "A", "Wheat", 0.7, 0.3,
    2000, "A", "FishProd", 0.6, 0.4
  )

  out <- .split_import_consumption(
    local_vs_import,
    feed_share_rum_mono
  )

  # Wheat: food (pmin applies: min(30, 50) = 30)
  wheat_food <- out |>
    dplyr::filter(Item == "Wheat", Destiny == "population_food")
  expect_equal(sum(wheat_food$MgN), 18, tolerance = 1e-12)
  expect_equal(unique(wheat_food$Origin), "Outside")
  expect_true(all(is.na(wheat_food$Irrig_cat)))

  # Wheat: feed split
  wheat_feed <- out |>
    dplyr::filter(
      Item == "Wheat",
      Destiny %in% c("livestock_rum", "livestock_mono")
    ) |>
    dplyr::group_by(Destiny) |>
    dplyr::summarise(MgN = sum(MgN), .groups = "drop") |>
    tibble::deframe()

  expect_equal(
    unname(wheat_feed["livestock_rum"]),
    9 * 0.7,
    tolerance = 1e-12
  )
  expect_equal(
    unname(wheat_feed["livestock_mono"]),
    9 * 0.3,
    tolerance = 1e-12
  )

  # Fish: food (no pmin, full import used)
  fish_food <- out |>
    dplyr::filter(
      Item == "FishProd",
      Destiny == "population_food"
    ) |>
    dplyr::summarise(MgN = sum(MgN), .groups = "drop") |>
    dplyr::pull(MgN)
  expect_equal(fish_food, 10 * 0.5, tolerance = 1e-12)
})

test_that(".split_import_consumption aggregates duplicates from Irrig_cat", {
  # Two Irrig_cat rows that become NA → should be aggregated
  local_vs_import <- tibble::tribble(
    ~Year, ~Province_name, ~Item, ~Box, ~Irrig_cat,
    ~local_consumption, ~import_consumption,
    ~food_share, ~feed_share, ~other_uses_share,
    2000, "A", "Wheat", "Cropland", "irrig",
    40, 20, 0.5, 0.5, 0.0,
    2000, "A", "Wheat", "Cropland", "rainfed",
    60, 30, 0.5, 0.5, 0.0
  )

  feed_shares <- tibble::tribble(
    ~Year, ~Province_name, ~Item, ~share_rum, ~share_mono,
    2000, "A", "Wheat", 1.0, 0.0
  )

  out <- .split_import_consumption(local_vs_import, feed_shares)

  # All imports should be aggregated under NA Irrig_cat
  expect_true(all(is.na(out$Irrig_cat)))
  food_total <- out |>
    dplyr::filter(Destiny == "population_food") |>
    dplyr::pull(MgN) |>
    sum()
  expect_equal(food_total, 25)
})


# .add_exports -----------------------------------------------------------------

test_that(".add_exports creates export rows with correct structure", {
  data <- tibble::tribble(
    ~Year, ~Province_name, ~Item, ~Irrig_cat, ~Box, ~export,
    ~food, ~other_uses, ~feed, ~production_n, ~import,
    2000, "A", "Wheat", "irrig", "Cropland", 40,
    30, 10, 20, 100, 0,
    2000, "A", "Wheat", "rainfed", "Cropland", 10,
    10, 5, 5, 30, 0
  )

  out <- .add_exports(data)

  expect_true(all(out$Destiny == "export"))
  expect_true(all(out$Origin == "Cropland"))

  total <- out |>
    dplyr::summarise(MgN = sum(MgN)) |>
    dplyr::pull(MgN)
  expect_equal(total, 50)
})


# .add_n_soil_inputs -----------------------------------------------------------

test_that(".add_n_soil_inputs pivots soil inputs and preserves totals", {
  base <- tibble::tribble(
    ~Year, ~Province_name, ~Item, ~Irrig_cat,
    2000, "A", "Wheat", "irrig"
  )

  soil_inputs <- tibble::tribble(
    ~Year, ~Province_name, ~Item, ~Irrig_cat, ~Box,
    ~deposition, ~fixation, ~synthetic, ~manure, ~urban,
    2000, "A", "Wheat", "irrig", "Cropland",
    1, 2, 3, 4, 5
  )

  out <- .add_n_soil_inputs(base, soil_inputs)

  sums <- out |>
    dplyr::filter(Item == "Wheat") |>
    dplyr::group_by(Origin) |>
    dplyr::summarise(MgN = sum(MgN), .groups = "drop") |>
    tibble::deframe()

  expect_setequal(
    names(sums),
    c("Deposition", "Fixation", "Synthetic", "Livestock", "People")
  )

  expect_equal(
    unname(sums[c(
      "Deposition",
      "Fixation",
      "Synthetic",
      "Livestock",
      "People"
    )]),
    c(1, 2, 3, 4, 5)
  )
})

test_that(".add_n_soil_inputs does not create duplicate flows", {
  base <- tibble::tribble(
    ~Year, ~Province_name, ~Item, ~Irrig_cat,
    2000, "A", "Wheat", "irrig"
  )

  soil_inputs <- tibble::tribble(
    ~Year, ~Province_name, ~Item, ~Irrig_cat, ~Box,
    ~deposition, ~fixation, ~synthetic, ~manure, ~urban,
    2000, "A", "Wheat", "irrig", "Cropland",
    1, 2, 3, 4, 5
  )

  out <- .add_n_soil_inputs(base, soil_inputs)

  expect_false(any(duplicated(
    out |>
      dplyr::select(Year, Province_name, Item, Origin, Destiny)
  )))
})

test_that(".add_n_soil_inputs filters out zero values", {
  base <- tibble::tribble(
    ~Year, ~Province_name, ~Item, ~Irrig_cat,
    2000, "A", "Wheat", "irrig"
  )

  soil_inputs <- tibble::tribble(
    ~Year, ~Province_name, ~Item, ~Irrig_cat, ~Box,
    ~deposition, ~fixation, ~synthetic, ~manure, ~urban,
    2000, "A", "Wheat", "irrig", "Cropland",
    0, 0, 3, 0, 0
  )

  out <- .add_n_soil_inputs(base, soil_inputs)

  # Only Synthetic should remain (the rest are 0)
  soil_rows <- out |>
    dplyr::filter(
      Origin %in%
        c(
          "Deposition",
          "Fixation",
          "Synthetic",
          "Livestock",
          "People"
        )
    )
  expect_equal(nrow(soil_rows), 1)
  expect_equal(soil_rows$Origin, "Synthetic")
})


# .remove_seeds_from_system ----------------------------------------------------

test_that(".remove_seeds_from_system subtracts seed and applies 50% cap", {
  npp <- tibble::tribble(
    ~Year, ~Province_name, ~Item, ~Area_ygpit_ha, ~LandUse,
    2000, "A", "Wheat", 100, "Cropland",
    2000, "B", "Wheat", 10, "Cropland"
  )

  pie_seed <- tibble::tribble(
    ~Year, ~Item, ~Element, ~Destiny, ~Value_destiny,
    2000, "Wheat", "Domestic_supply", "Seed", 220
  )

  prod <- tibble::tribble(
    ~Year, ~Province_name, ~Item, ~production_fm,
    2000, "A", "Wheat", 300,
    2000, "B", "Wheat", 30
  )

  res <- .remove_seeds_from_system(npp, pie_seed, prod)

  summed <- res |>
    dplyr::group_by(Province_name) |>
    dplyr::summarise(
      production_fm = sum(production_fm),
      .groups = "drop"
    )

  expect_equal(
    summed$production_fm[summed$Province_name == "A"],
    150
  )
  expect_equal(
    summed$production_fm[summed$Province_name == "B"],
    15
  )
})

test_that(".remove_seeds_from_system caps at 50% when seeds exceed prod", {
  npp <- tibble::tribble(
    ~Year, ~Province_name, ~Item, ~Area_ygpit_ha, ~LandUse,
    2000, "A", "Wheat", 1, "Cropland"
  )

  pie_seed <- tibble::tribble(
    ~Year, ~Item, ~Element, ~Destiny, ~Value_destiny,
    2000, "Wheat", "Domestic_supply", "Seed", 1000
  )

  prod <- tibble::tribble(
    ~Year, ~Province_name, ~Item, ~production_fm,
    2000, "A", "Wheat", 10
  )

  out <- .remove_seeds_from_system(npp, pie_seed, prod)

  expect_equal(out$production_fm, 5)
})

test_that(".remove_seeds_from_system leaves non-seed items unchanged", {
  npp <- tibble::tribble(
    ~Year, ~Province_name, ~Item, ~Area_ygpit_ha, ~LandUse,
    2000, "A", "Barley", 100, "Cropland"
  )

  pie_seed <- tibble::tribble(
    ~Year, ~Item, ~Element, ~Destiny, ~Value_destiny,
    2000, "Wheat", "Domestic_supply", "Seed", 100
  )

  prod <- tibble::tribble(
    ~Year, ~Province_name, ~Item, ~production_fm,
    2000, "A", "Barley", 200
  )

  out <- .remove_seeds_from_system(npp, pie_seed, prod)

  expect_equal(out$production_fm, 200)
})


# .finalize_prod_destiny -------------------------------------------------------

test_that(".finalize_prod_destiny combines local, import, export flows", {
  trade_data <- tibble::tribble(
    ~Year, ~Province_name, ~Item, ~Box, ~Irrig_cat,
    ~food, ~other_uses, ~feed, ~production_n, ~export, ~import,
    2000, "A", "Wheat", "Cropland", "irrig",
    30, 10, 20, 100, 40, 0,
    2000, "A", "Fish", "Fish", NA,
    5, 0, 0, 0, 0, 5
  )

  codes <- tibble::tribble(
    ~item, ~group,
    "Wheat", "Primary crops",
    "Fish", "Fish"
  )

  soil <- tibble::tribble(
    ~Year, ~Province_name, ~Item, ~Irrig_cat, ~Box,
    ~deposition, ~fixation, ~synthetic, ~manure, ~urban,
    2000, "A", "Wheat", "irrig", "Cropland",
    1, 0, 0, 0, 0
  )

  feed_shares <- tibble::tribble(
    ~Year, ~Province_name, ~Item, ~share_rum, ~share_mono,
    2000, "A", "Wheat", 0.7, 0.3,
    2000, "A", "Fish", 0.0, 0.0
  )

  out <- .finalize_prod_destiny(
    trade_data,
    codes,
    soil,
    feed_shares
  )

  # Should have local consumption, export, and import rows
  destinies <- unique(out$Destiny)
  expect_true("export" %in% destinies)
  expect_true("population_food" %in% destinies)
  # All MgN should be positive (filtered)
  expect_true(all(out$MgN > 0))
})
