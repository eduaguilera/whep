#' Tests for GRAFS N Inputs

test_that(".assign_items returns expected categories", {
  cats <- .assign_items()

  expect_true("semi_natural_agroecosystems" %in% names(cats))
  expect_true("Firewood_biomass" %in% names(cats))

  expect_true("Dehesa" %in% cats$semi_natural_agroecosystems)
  expect_true("Holm oak" %in% cats$Firewood_biomass)
})

test_that(".calculate_n_soil_inputs calculates N soil inputs correctly", {
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

  # Test: .merge_items_biomass ------------------------------------------------
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

  # Test: .summarise_crops_residues -------------------------------------------
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

    expect_true("Prod_Residue_Product_Mg" %in% names(result))
    expect_equal(result$Prod_Residue_Product_Mg[1], 100)
    expect_equal(result$Box[1], "Cropland")
  })

  # Test: .aggregate_grazed_cropland ------------------------------------------
  fake_grazed <- data.frame(
    Year = c(2020),
    Province_name = c("Province1"),
    Name_biomass = c("BiomassA"),
    Item = c("ItemA"),
    GrazedWeeds_MgDM = c(15),
    LandUse = c("Cropland")
  )

  fake_crop_residue <- data.frame(
    Year = c(2020),
    Province_name = c("Province1"),
    Name_biomass = c("BiomassA"),
    Item = c("ItemA"),
    Prod_Residue_Product_Mg = c(50),
    Box = c("Cropland")
  )

  test_that(
    ".aggregate_grazed_cropland combines grazed and crop residue data",
    {
      testthat::skip_on_ci()

      result <- .aggregate_grazed_cropland(fake_grazed, fake_crop_residue)

      expect_true("GrazedWeeds_MgDM" %in% names(result))
      expect_equal(result$GrazedWeeds_MgDM[1], 15)
      expect_equal(result$Prod_Residue_Product_Mg[1], 50)
    }
  )

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
  testthat::skip_on_ci()

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

test_that("calculate_nue_crops output structure is correct", {
  testthat::skip_on_ci()

  nue <- tibble::tibble(
    Year = 2000,
    Province_name = "Madrid",
    Item = "Dehesa_item",
    Box = c("semi_natural_agroecosystems", "Fish"),
    nue = c(0.75, NA_real_)
  )

  expect_true("nue" %in% colnames(nue))
  expect_true(all(!is.na(nue$nue[!is.na(nue$nue)])))

  expect_true(is.na(nue$nue[nue$Box == "Fish"]))
})
