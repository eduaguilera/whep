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
    ~Year, ~Province_name, ~Name_biomass, ~item_cbs, ~Product_residue, ~LandUse, ~Irrig_cat, ~Prod_ygpit_Mg,
    2000, "A", "Wheat", "Wheat", "Product", "Cropland", "irrig", 100,
    2000, "A", "Wheat", "Wheat", "Product", "Cropland", "irrig", 50,
    2000, "A", "Wheat", "Wheat", "Residue", "Cropland", "rainfed", 30
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
    ~Year, ~Province_name, ~Name_biomass, ~Item, ~LandUse, ~Irrig_cat, ~GrazedWeeds_MgDM, ~GrazedAcorns_MgDM, ~GrazedFodder_MgDM, ~Prod_ygpit_Mg, ~Used_Residue_MgFM,
    2000, "A", "Fallow", "Fallow", "Cropland", "rainfed", 10, 0, 0, 0, 0,
    2000, "A", "Grass", "Grassland", "Dehesa", NA, 20, 0, 0, 5, 3
  )

  crop_prod <- tibble::tribble(
    ~Year, ~Province_name, ~Name_biomass, ~Item, ~prod_type, ~LandUse, ~Irrig_cat, ~production_fm, ~Box,
    2000, "A", "Wheat", "Wheat", "Product", "Cropland", "irrig", 100, "Cropland"
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
    ~Year, ~Province_name, ~item_cbs, ~Name_biomass, ~Prod_Mg,
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
    ~Year, ~Province_name, ~Name_biomass, ~Item, ~Box, ~LandUse, ~Irrig_cat, ~prod_type, ~production_fm,
    2000, "A", "Fallow", "Fallow", "Cropland", "Cropland", "rainfed", "Grass", 10,
    2000, "A", "SomeGrass", "SomeItem", "semi_natural_agroecosystems", "Dehesa", NA, "Grass", 20,
    2000, "A", "Wheat", "Wheat", "Cropland", "Cropland", "irrig", "Product", 100
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
    ~Year, ~Province_name, ~Name_biomass, ~Item, ~Box, ~LandUse, ~Irrig_cat, ~prod_type, ~production_fm,
    2000, "A", "Holm oak forest", "Holm oak", "semi_natural_agroecosystems", "Forest_low", NA, "Residue", 50,
    2000, "A", "Conifers", "Conifers", "semi_natural_agroecosystems", "Forest_low", NA, "Residue", 30
  )

  out <- .add_grass_wood(input)

  expect_true(all(out$Item == "Firewood"))
  expect_true(all(out$Name_biomass == "Firewood"))
})

test_that(".add_grass_wood filters out NA production", {
  input <- tibble::tribble(
    ~Year, ~Province_name, ~Name_biomass, ~Item, ~Box, ~LandUse, ~Irrig_cat, ~prod_type, ~production_fm,
    2000, "A", "Wheat", "Wheat", "Cropland", "Cropland", "irrig", "Product", NA_real_,
    2000, "A", "Barley", "Barley", "Cropland", "Cropland", "irrig", "Product", 50
  )

  out <- .add_grass_wood(input)

  expect_equal(nrow(out), 1)
  expect_equal(out$Item, "Barley")
})


# .spain_processing_coefs -------------------------------------------------------

test_that(".spain_processing_coefs filters to Spain and names item codes", {
  # Mirrors get_processing_coefs(): both sides arrive as CBS codes, Spain is
  # selected on area_code (203), and value_to_process is carried directly
  # rather than recovered from value_proc / cf as the old pin required.
  processing_coefs <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code_to_process, ~item_cbs_code_processed,
    2000L, 203L, 2620L, 2655L,
    2000L, 2L, 2620L, 2655L
  ) |>
    dplyr::mutate(value_to_process = 100, final_conversion_factor = 0.5)

  out <- .spain_processing_coefs(processing_coefs)

  expect_equal(nrow(out), 1)
  expect_equal(out$Item, "Grapes and products (excl wine)")
  expect_equal(out$ProcessedItem, "Wine")
  expect_equal(out$value_to_process, 100)
  expect_equal(out$cf, 0.5)
  expect_equal(out$Year, 2000L)
})

test_that(".spain_processing_coefs drops non-positive or missing cf", {
  processing_coefs <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code_to_process, ~item_cbs_code_processed,
    ~value_to_process, ~final_conversion_factor,
    2000L, 203L, 2620L, 2655L, 100, 0.5,
    2000L, 203L, 2563L, 2580L, 0, 0,
    2000L, 203L, 2511L, 2592L, 10, NA_real_
  )

  out <- .spain_processing_coefs(processing_coefs)

  expect_equal(nrow(out), 1)
  expect_equal(out$Item, "Grapes and products (excl wine)")
})


# .national_item_production ------------------------------------------------------

test_that(".national_item_production sums production across provinces", {
  prod <- tibble::tribble(
    ~Year, ~Item, ~Province_name, ~production_fm,
    2000, "Grapes", "A", 100,
    2000, "Grapes", "B", 50,
    2000, "Wheat", "A", 30
  )

  out <- .national_item_production(prod)

  expect_equal(
    out$national_production_fm[out$Item == "Grapes"],
    150
  )
  expect_equal(
    out$national_production_fm[out$Item == "Wheat"],
    30
  )
})


# .calculate_processing_shares ---------------------------------------------------

test_that(".calculate_processing_shares computes the national share", {
  spain_coefs <- tibble::tribble(
    ~Year, ~Item, ~ProcessedItem, ~value_to_process, ~cf,
    2000, "Grapes", "Wine", 40, 0.5,
    2000, "Grapes", "Juice", 40, 0.3
  )

  national_production <- tibble::tribble(
    ~Year, ~Item, ~national_production_fm,
    2000, "Grapes", 100
  )

  out <- .calculate_processing_shares(spain_coefs, national_production)

  expect_equal(nrow(out), 1)
  expect_equal(out$share_processing, 0.4)
})

test_that(".calculate_processing_shares collapses to one row per Item even when co-products carry slightly different value_to_process (rounding noise)", {
  spain_coefs <- tibble::tribble(
    ~Year, ~Item, ~ProcessedItem, ~value_to_process, ~cf,
    2000, "Wheat", "Flour", 100, 0.7,
    2000, "Wheat", "Bran", 102, 0.2,
    2000, "Wheat", "DDGS", 98, 0.1
  )

  national_production <- tibble::tribble(
    ~Year, ~Item, ~national_production_fm,
    2000, "Wheat", 200
  )

  out <- .calculate_processing_shares(spain_coefs, national_production)

  expect_equal(nrow(out), 1)
  expect_equal(out$share_processing, 0.5)
})

test_that(".calculate_processing_shares caps the share at 1", {
  spain_coefs <- tibble::tribble(
    ~Year, ~Item, ~ProcessedItem, ~value_to_process, ~cf,
    2000, "Grapes", "Wine", 150, 0.5
  )

  national_production <- tibble::tribble(
    ~Year, ~Item, ~national_production_fm,
    2000, "Grapes", 100
  )

  out <- .calculate_processing_shares(spain_coefs, national_production)

  expect_equal(out$share_processing, 1)
})

test_that(".calculate_processing_shares returns 0 for zero national production", {
  spain_coefs <- tibble::tribble(
    ~Year, ~Item, ~ProcessedItem, ~value_to_process, ~cf,
    2000, "Grapes", "Wine", 50, 0.5
  )

  national_production <- tibble::tribble(
    ~Year, ~Item, ~national_production_fm,
    2000, "Grapes", 0
  )

  out <- .calculate_processing_shares(spain_coefs, national_production)

  expect_equal(out$share_processing, 0)
})


# .expand_processed_items ---------------------------------------------------------

test_that(".expand_processed_items multiplies processed mass by cf", {
  with_share <- tibble::tribble(
    ~Year, ~Province_name, ~Name_biomass, ~Item, ~processed_fm,
    2000, "A", "Grape", "Grapes", 40
  )

  spain_coefs <- tibble::tribble(
    ~Year, ~Item, ~ProcessedItem, ~cf,
    2000, "Grapes", "Wine", 0.5
  )

  out <- .expand_processed_items(with_share, spain_coefs)

  expect_equal(out$Item, "Wine")
  expect_equal(out$production_fm, 20)
  expect_equal(out$Box, "Cropland")
  expect_equal(out$prod_type, "Product")
})


# .backfill_processing_shares -----------------------------------------------------

test_that(".backfill_processing_shares copies the earliest share backward", {
  processing_shares <- tibble::tribble(
    ~Year, ~Item, ~share_processing,
    1961, "Grapes", 0.9,
    1962, "Grapes", 0.92
  )

  out <- .backfill_processing_shares(processing_shares, first_year = 1959)

  expect_equal(nrow(out), 4)
  backfilled <- out |> dplyr::filter(Year < 1961) |> dplyr::arrange(Year)
  expect_equal(backfilled$Year, c(1959, 1960))
  expect_equal(backfilled$share_processing, c(0.9, 0.9))
})

test_that(".backfill_processing_shares does nothing when data already starts at first_year", {
  processing_shares <- tibble::tribble(
    ~Year, ~Item, ~share_processing,
    1961, "Grapes", 0.9
  )

  out <- .backfill_processing_shares(processing_shares, first_year = 1961)

  expect_equal(nrow(out), 1)
})


# .backfill_processing_cf -----------------------------------------------------

test_that(".backfill_processing_cf copies the earliest cf mapping backward", {
  spain_coefs <- tibble::tribble(
    ~Year, ~Item, ~ProcessedItem, ~value_to_process, ~cf,
    1961, "Grapes", "Wine", 100, 0.67,
    1961, "Grapes", "Alcohol, Non-Food", 100, 0.006,
    1962, "Grapes", "Wine", 105, 0.68
  )

  out <- .backfill_processing_cf(spain_coefs, first_year = 1959)

  backfilled <- out |>
    dplyr::filter(Year < 1961) |>
    dplyr::arrange(Year, ProcessedItem)

  expect_equal(nrow(backfilled), 4)
  expect_setequal(unique(backfilled$Year), c(1959, 1960))
  expect_setequal(
    unique(backfilled$ProcessedItem),
    c("Wine", "Alcohol, Non-Food")
  )
  expect_equal(
    backfilled$cf[backfilled$ProcessedItem == "Wine" & backfilled$Year == 1959],
    0.67
  )
})


# .forwardfill_processing_shares -----------------------------------------------------

test_that(".forwardfill_processing_shares copies the latest share forward", {
  processing_shares <- tibble::tribble(
    ~Year, ~Item, ~share_processing,
    2020, "Grapes", 0.9,
    2021, "Grapes", 0.92
  )

  out <- .forwardfill_processing_shares(processing_shares, last_year = 2023)

  expect_equal(nrow(out), 4)
  forwardfilled <- out |> dplyr::filter(Year > 2021) |> dplyr::arrange(Year)
  expect_equal(forwardfilled$Year, c(2022, 2023))
  expect_equal(forwardfilled$share_processing, c(0.92, 0.92))
})

test_that(".forwardfill_processing_shares does nothing when data already ends at last_year", {
  processing_shares <- tibble::tribble(
    ~Year, ~Item, ~share_processing,
    2021, "Grapes", 0.9
  )

  out <- .forwardfill_processing_shares(processing_shares, last_year = 2021)

  expect_equal(nrow(out), 1)
})


# .forwardfill_processing_cf -----------------------------------------------------

test_that(".forwardfill_processing_cf copies the latest cf mapping forward", {
  spain_coefs <- tibble::tribble(
    ~Year, ~Item, ~ProcessedItem, ~value_to_process, ~cf,
    2020, "Grapes", "Wine", 100, 0.67,
    2021, "Grapes", "Wine", 105, 0.68,
    2021, "Grapes", "Alcohol, Non-Food", 105, 0.006
  )

  out <- .forwardfill_processing_cf(spain_coefs, last_year = 2023)

  forwardfilled <- out |>
    dplyr::filter(Year > 2021) |>
    dplyr::arrange(Year, ProcessedItem)

  expect_equal(nrow(forwardfilled), 4)
  expect_setequal(unique(forwardfilled$Year), c(2022, 2023))
  expect_setequal(
    unique(forwardfilled$ProcessedItem),
    c("Wine", "Alcohol, Non-Food")
  )
  expect_equal(
    forwardfilled$cf[
      forwardfilled$ProcessedItem == "Wine" & forwardfilled$Year == 2022
    ],
    0.68
  )
})


# .forwardfill_population -----------------------------------------------------

test_that(".forwardfill_population copies the latest province row forward", {
  population_yg <- tibble::tribble(
    ~Year, ~Province_name, ~Pop_Mpeop_yg,
    2020, "Huesca", 0.22,
    2021, "Huesca", 0.221
  )

  out <- .forwardfill_population(population_yg, last_year = 2023)

  expect_equal(nrow(out), 4)
  forwardfilled <- out |> dplyr::filter(Year > 2021) |> dplyr::arrange(Year)
  expect_equal(forwardfilled$Year, c(2022, 2023))
  expect_equal(forwardfilled$Pop_Mpeop_yg, c(0.221, 0.221))
})

test_that(".forwardfill_population does nothing when data already ends at last_year", {
  population_yg <- tibble::tribble(
    ~Year, ~Province_name, ~Pop_Mpeop_yg,
    2021, "Huesca", 0.221
  )

  out <- .forwardfill_population(population_yg, last_year = 2021)

  expect_equal(nrow(out), 1)
})


# .calculate_processed_amounts -----------------------------------------------------

# Coefficient fixtures shared by the processing-conservation tests. n_per_fm is
# Product_kgDM_kgFM * Product_kgN_kgDM, i.e. tonnes N per tonne fresh matter.
.test_processing_coefs <- function() {
  list(
    items = tibble::tribble(
      ~item, ~Name_biomass,
      "Grapes", "grape_bm",
      "Wine", "wine_bm",
      "Juice", "juice_bm",
      "Sunflower seed", "sunflower_bm",
      "Sunflower Cake", "cake_bm",
      "Sunflower Oil", "oil_bm",
      "Beef", "beef_bm"
    ),
    biomass = tibble::tribble(
      ~Name_biomass, ~Product_kgDM_kgFM, ~Product_kgN_kgDM,
      "grape_bm", 0.2, 0.01, # 0.002   N/t FM
      "wine_bm", 0.02, 0.005, # 0.0001  N/t FM (nearly N-free)
      "juice_bm", 0.1, 0.01, # 0.001   N/t FM
      "sunflower_bm", 0.93, 0.028, # 0.02604 N/t FM
      "cake_bm", 0.9, 0.08, # 0.072   N/t FM (N-concentrating)
      "oil_bm", 1.0, 0.0, # 0       N/t FM
      "beef_bm", 0.3, 0.03
    )
  )
}

.test_n_per_fm <- function(item) {
  coefs <- .test_processing_coefs()
  bm <- coefs$items$Name_biomass[coefs$items$item == item]
  row <- coefs$biomass[coefs$biomass$Name_biomass == bm, ]
  row$Product_kgDM_kgFM * row$Product_kgN_kgDM
}

test_that(".calculate_processed_amounts splits and expands production", {
  prod <- tibble::tribble(
    ~Year, ~Province_name, ~Name_biomass, ~Item, ~Box, ~prod_type, ~production_fm,
    2000, "A", "grape_bm", "Grapes", "Cropland", "Product", 100,
    2000, "A", "beef_bm", "Beef", "Livestock", "Product", 50
  )

  processing_shares <- tibble::tribble(
    ~Year, ~Item, ~share_processing,
    2000, "Grapes", 0.4,
    2000, "Beef", 0.9
  )

  spain_coefs <- tibble::tribble(
    ~Year, ~Item, ~ProcessedItem, ~value_to_process, ~cf,
    2000, "Grapes", "Wine", 40, 0.5,
    2000, "Grapes", "Juice", 40, 0.3
  )

  out <- .calculate_processed_amounts(
    prod,
    processing_shares,
    spain_coefs,
    .test_processing_coefs()
  )

  # Non-cropland rows pass through unchanged, even if a share exists.
  beef_row <- out$non_processed |> dplyr::filter(Item == "Beef")
  expect_equal(beef_row$production_fm, 50)

  # Wine and juice are N-poorer than grapes, so the outputs stand as computed
  # and only the N-equivalent mass leaves the primary item.
  wine_row <- out$processed_items |> dplyr::filter(Item == "Wine")
  expect_equal(wine_row$production_fm, 100 * 0.4 * 0.5)

  juice_row <- out$processed_items |> dplyr::filter(Item == "Juice")
  expect_equal(juice_row$production_fm, 100 * 0.4 * 0.3)

  expect_true(all(out$processed_items$Box == "Cropland"))
  expect_true(all(out$processed_items$prod_type == "Product"))
})

test_that(".calculate_processed_amounts conserves N when outputs are N-poor", {
  prod <- tibble::tribble(
    ~Year, ~Province_name, ~Name_biomass, ~Item, ~Box, ~prod_type, ~production_fm,
    2000, "A", "grape_bm", "Grapes", "Cropland", "Product", 100
  )
  processing_shares <- tibble::tribble(
    ~Year, ~Item, ~share_processing,
    2000, "Grapes", 0.4
  )
  spain_coefs <- tibble::tribble(
    ~Year, ~Item, ~ProcessedItem, ~value_to_process, ~cf,
    2000, "Grapes", "Wine", 40, 0.5,
    2000, "Grapes", "Juice", 40, 0.3
  )

  out <- .calculate_processed_amounts(
    prod,
    processing_shares,
    spain_coefs,
    .test_processing_coefs()
  )

  n_added <- sum(
    out$processed_items$production_fm *
      vapply(out$processed_items$Item, .test_n_per_fm, numeric(1))
  )
  removed_fm <- 100 - out$non_processed$production_fm
  n_removed <- removed_fm * .test_n_per_fm("Grapes")

  expect_equal(n_removed, n_added)
  # Grapes keep the N the named outputs cannot account for, rather than
  # losing it: only 17.5% of the diverted mass is actually accounted for.
  expect_equal(removed_fm, 40 * (n_added / (40 * .test_n_per_fm("Grapes"))))
  expect_lt(removed_fm, 40)
})

test_that(".calculate_processed_amounts never creates N when outputs are N-rich", {
  prod <- tibble::tribble(
    ~Year, ~Province_name, ~Name_biomass, ~Item, ~Box, ~prod_type, ~production_fm,
    2000, "A", "sunflower_bm", "Sunflower seed", "Cropland", "Product", 100
  )
  processing_shares <- tibble::tribble(
    ~Year, ~Item, ~share_processing,
    2000, "Sunflower seed", 1
  )
  # Unscaled, the cake alone would carry 3.24 t N against the seed's 2.604.
  spain_coefs <- tibble::tribble(
    ~Year, ~Item, ~ProcessedItem, ~value_to_process, ~cf,
    2000, "Sunflower seed", "Sunflower Cake", 100, 0.45,
    2000, "Sunflower seed", "Sunflower Oil", 100, 0.4
  )

  out <- .calculate_processed_amounts(
    prod,
    processing_shares,
    spain_coefs,
    .test_processing_coefs()
  )

  n_in <- 100 * .test_n_per_fm("Sunflower seed")
  n_added <- sum(
    out$processed_items$production_fm *
      vapply(out$processed_items$Item, .test_n_per_fm, numeric(1))
  )

  expect_equal(n_added, n_in)
  expect_equal(out$non_processed$production_fm, 0)
  # All outputs are scaled by the same factor, so their mix is preserved.
  cake <- out$processed_items$production_fm[
    out$processed_items$Item == "Sunflower Cake"
  ]
  oil <- out$processed_items$production_fm[
    out$processed_items$Item == "Sunflower Oil"
  ]
  expect_equal(cake / oil, 0.45 / 0.4)
  expect_lt(cake, 100 * 0.45)
})

test_that(".calculate_processed_amounts drops substitutions it cannot price in N", {
  prod <- tibble::tribble(
    ~Year, ~Province_name, ~Name_biomass, ~Item, ~Box, ~prod_type, ~production_fm,
    2000, "A", "unknown_bm", "Mystery crop", "Cropland", "Product", 100
  )
  processing_shares <- tibble::tribble(
    ~Year, ~Item, ~share_processing,
    2000, "Mystery crop", 0.5
  )
  spain_coefs <- tibble::tribble(
    ~Year, ~Item, ~ProcessedItem, ~value_to_process, ~cf,
    2000, "Mystery crop", "Juice", 50, 0.5
  )

  expect_warning(
    out <- .calculate_processed_amounts(
      prod,
      processing_shares,
      spain_coefs,
      .test_processing_coefs()
    ),
    "no usable product N coefficient"
  )

  expect_equal(out$non_processed$production_fm, 100)
  expect_equal(sum(out$processed_items$production_fm), 0)
})

test_that(".processing_n_scaling leaves an exactly balanced substitution alone", {
  # Outputs carry precisely the input's N: both scales must be 1.
  candidate <- tibble::tribble(
    ~Year, ~Province_name, ~Name_biomass, ~Item, ~processed_fm,
    2000, "A", "grape_bm", "Grapes", 100
  )
  outputs <- tibble::tribble(
    ~Year, ~Province_name, ~Name_biomass, ~Item, ~from_item, ~production_fm,
    2000, "A", "grape_bm", "Juice", "Grapes", 200
  )

  out <- .processing_n_scaling(candidate, outputs, .test_processing_coefs())

  expect_equal(out$output_scale, 1)
  expect_equal(out$removal_scale, 1)
})


# .prepare_prod_data -----------------------------------------------------------

test_that(".prepare_prod_data merges biomass names from codes", {
  grafs_added <- tibble::tribble(
    ~Year, ~Province_name, ~Name_biomass, ~Item, ~Box, ~LandUse, ~Irrig_cat, ~prod_type, ~production_fm,
    2000, "A", "Wheat", "WheatItem", "Cropland", "Cropland", "irrig", "Product", 100
  )

  processed <- tibble::tribble(
    ~Year, ~Province_name, ~Name_biomass, ~Item, ~Box, ~production_fm, ~prod_type,
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
    ~Year, ~Province_name, ~Name_biomass, ~Item, ~Box, ~LandUse, ~Irrig_cat, ~prod_type, ~production_fm,
    2000, "A", "OrigBiomass", "UnknownItem", "Cropland", "Cropland", "irrig", "Product", 50
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
    ~Year, ~Province_name, ~Item, ~Box, ~LandUse, ~Irrig_cat, ~prod_type, ~production_fm, ~Name_biomass_primary, ~Name_biomass,
    2000, "A", "Wheat", "Cropland", "Cropland", "irrig", "Product", 1000, "Wheat", "Wheat",
    2000, "A", "Straw", "Cropland", "Cropland", "irrig", "Residue", 500, "Wheat", "Wheat"
  )

  coefs <- tibble::tribble(
    ~Name_biomass, ~Product_kgDM_kgFM, ~Residue_kgDM_kgFM, ~Product_kgN_kgDM, ~Residue_kgN_kgDM,
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
    ~Year, ~Province_name, ~Item, ~Box, ~LandUse, ~Irrig_cat, ~prod_type, ~production_fm, ~Name_biomass_primary, ~Name_biomass,
    2000, "A", "Nuts and products", "Cropland", "Cropland", "irrig", "Product", 100, "Almond", "NutsGeneric"
  )

  coefs <- tibble::tribble(
    ~Name_biomass, ~Product_kgDM_kgFM, ~Residue_kgDM_kgFM, ~Product_kgN_kgDM, ~Residue_kgN_kgDM,
    "Almond", 0.9, 0.8, 0.03, 0.01,
    "NutsGeneric", 0.5, 0.5, 0.01, 0.01
  )

  out <- .convert_fm_dm_n(merged, coefs)

  # Should use Almond (primary) coefs, not NutsGeneric
  expect_equal(out$production_n, 100 * 0.9 * 0.03)
})

test_that(".convert_fm_dm_n filters NA Item with zero production", {
  merged <- tibble::tribble(
    ~Year, ~Province_name, ~Item, ~Box, ~LandUse, ~Irrig_cat, ~prod_type, ~production_fm, ~Name_biomass_primary, ~Name_biomass,
    2000, "A", NA_character_, "Cropland", "Cropland", "irrig", "Product", 0, "Something", "Something",
    2000, "A", "Wheat", "Cropland", "Cropland", "irrig", "Product", 100, "Wheat", "Wheat"
  )

  coefs <- tibble::tribble(
    ~Name_biomass, ~Product_kgDM_kgFM, ~Residue_kgDM_kgFM, ~Product_kgN_kgDM, ~Residue_kgN_kgDM,
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
    ~Year, ~Province_name, ~item_cbs, ~Livestock_cat, ~intake_MgFM,
    2000, "A", "Wheat", "Cattle_meat", 60,
    2000, "A", "Wheat", "Pigs", 30,
    2000, "A", "Wheat", "Pets", 10
  )

  out <- .add_feed(intake)

  expect_named(out, c("feed_intake", "feed_share_rum_mono"))

  # feed_intake. feed = 60 (Cattle_meat) + 30 (Pigs).
  fi <- out$feed_intake
  expect_equal(fi$feed, 60 + 30)
  expect_equal(fi$food_pets, 10)

  # Shares are over total feed (90).
  fs <- out$feed_share_rum_mono
  expect_equal(fs$share_rum, 60 / 90, tolerance = 1e-12)
  expect_equal(fs$share_mono, 30 / 90, tolerance = 1e-12)
})

test_that(".add_feed handles zero feed total gracefully", {
  intake <- tibble::tribble(
    ~Year, ~Province_name, ~item_cbs, ~Livestock_cat, ~intake_MgFM,
    2000, "A", "Wheat", "Cattle_meat", 0,
    2000, "A", "Wheat", "Pigs", 0,
    2000, "A", "Wheat", "Pets", 10
  )

  out <- .add_feed(intake)

  expect_equal(out$feed_intake$feed, 0)
  expect_equal(out$feed_share_rum_mono$share_rum, 0)
  expect_equal(out$feed_share_rum_mono$share_mono, 0)
})

test_that(".add_feed maps all livestock types correctly", {
  intake <- tibble::tribble(
    ~Year, ~Province_name, ~item_cbs, ~Livestock_cat, ~intake_MgFM,
    2000, "A", "Grain", "Sheep", 10,
    2000, "A", "Grain", "Goats", 10,
    2000, "A", "Grain", "Horses", 10,
    2000, "A", "Grain", "Donkeys_mules", 10,
    2000, "A", "Grain", "Poultry", 20,
    2000, "A", "Grain", "Rabbits", 10,
    2000, "A", "Grain", "Fur animals", 5,
    2000, "A", "Grain", "Other", 5,
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
    ~Year, ~Province_name, ~Item, ~Box, ~Irrig_cat, ~production_n, ~prod_type,
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
    ~Year, ~Province_name, ~Item, ~Box, ~Irrig_cat, ~production_n, ~prod_type,
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
    ~Year, ~Province_name, ~Item, ~Box, ~Irrig_cat, ~production_n, ~food, ~other_uses, ~feed,
    2000, "A", "Wheat", "Cropland", "irrig", 100, 50, 20, 30
  )

  codes <- tibble::tribble(
    ~item, ~Name_biomass,
    "Wheat", "Wheat"
  )

  coefs <- tibble::tribble(
    ~Name_biomass, ~Product_kgDM_kgFM, ~Product_kgN_kgDM, ~Residue_kgDM_kgFM, ~Residue_kgN_kgDM,
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
    ~Year, ~Province_name, ~Item, ~Box, ~Irrig_cat, ~production_n, ~food, ~other_uses, ~feed,
    2000, "A", "Grassland", "semi_natural_agroecosystems", NA, 0, 0, 0, 100
  )

  codes <- tibble::tribble(
    ~item, ~Name_biomass,
    "Grassland", "Grass"
  )

  coefs <- tibble::tribble(
    ~Name_biomass, ~Product_kgDM_kgFM, ~Product_kgN_kgDM, ~Residue_kgDM_kgFM, ~Residue_kgN_kgDM,
    "Grass", 0.3, 0.01, 0.2, 0.025
  )

  out <- .convert_to_items_n(combined, codes, coefs)

  # Grass → uses Residue coefs
  expect_equal(out$feed, 100 * 0.2 * 0.025)
})


# .calculate_trade -------------------------------------------------------------

test_that(".calculate_trade computes export when production > consumption", {
  trade_input <- tibble::tribble(
    ~Year, ~Province_name, ~Item, ~Box, ~Irrig_cat, ~Name_biomass, ~prod_type, ~production_n, ~food, ~other_uses, ~feed,
    2000, "A", "Wheat", "Cropland", "irrig", "Wheat", "Product", 100, 30, 10, 20
  )

  out <- .calculate_trade(trade_input)

  # consumption = 30 + 10 + 20 = 60, net_trade = 100 - 60 = 40
  expect_equal(out$export, 40)
  expect_equal(out$import, 0)
})

test_that(".calculate_trade computes import when consumption > production", {
  trade_input <- tibble::tribble(
    ~Year, ~Province_name, ~Item, ~Box, ~Irrig_cat, ~Name_biomass, ~prod_type, ~production_n, ~food, ~other_uses, ~feed,
    2000, "A", "Wheat", "Cropland", "irrig", "Wheat", "Product", 30, 50, 10, 20
  )

  out <- .calculate_trade(trade_input)

  # consumption = 80, net_trade = 30 - 80 = -50
  expect_equal(out$export, 0)
  expect_equal(out$import, 50)
})


# .prep_final_ds ---------------------------------------------------------------

test_that(".prep_final_ds assigns Box from group codes", {
  trade <- tibble::tribble(
    ~Year, ~Province_name, ~Item, ~Box, ~Irrig_cat, ~food, ~other_uses, ~feed, ~production_n, ~export, ~import,
    2000, "A", "Sugar", NA, "irrig", 10, 5, 0, 15, 0, 0,
    2000, "A", "Beef", NA, NA, 10, 0, 0, 10, 0, 0,
    2000, "A", "FishProd", NA, NA, 5, 0, 0, 5, 0, 0,
    2000, "A", "Additive", NA, NA, 2, 0, 0, 2, 0, 0
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
    ~Year, ~Province_name, ~Item, ~Box, ~Irrig_cat, ~food, ~other_uses, ~feed, ~production_n, ~export, ~import,
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
    ~Year, ~Province_name, ~Item, ~Box, ~Irrig_cat, ~production_n, ~food, ~other_uses, ~feed,
    2000, "A", "Wheat", "Cropland", "irrig", 100, 60, 20, 20
  )

  out <- .calculate_consumption_shares(data)

  expect_equal(out$food_share, 0.6)
  expect_equal(out$other_uses_share, 0.2)
  expect_equal(out$feed_share, 0.2)
  expect_equal(out$local_total, 100)
})

test_that(".calculate_consumption_shares returns 0 when total is 0", {
  data <- tibble::tribble(
    ~Year, ~Province_name, ~Item, ~Box, ~Irrig_cat, ~production_n, ~food, ~other_uses, ~feed,
    2000, "A", "Wheat", "Cropland", "irrig", 0, 0, 0, 0
  )

  out <- .calculate_consumption_shares(data)

  expect_equal(out$food_share, 0)
  expect_equal(out$other_uses_share, 0)
  expect_equal(out$feed_share, 0)
})


# .split_local_consumption -----------------------------------------------------

test_that(".split_local_consumption splits by shares and feed type", {
  local_import <- tibble::tribble(
    ~Year, ~Province_name, ~Item, ~Box, ~Irrig_cat, ~local_consumption, ~import_consumption, ~food_share, ~feed_share, ~other_uses_share,
    2000, "A", "Wheat", "Cropland", "irrig", 100, 0, 0.5, 0.3, 0.2
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
  # Total demand (food/other_uses/feed) and local shares are chosen so the
  # import gap shares work out to food = 0.6, other = 0.1, feed = 0.3 for
  # Wheat and food = 0.5 for FishProd.
  local_vs_import <- tibble::tribble(
    ~Year, ~Province_name, ~Item, ~Box, ~local_consumption, ~import_consumption, ~food, ~other_uses, ~feed, ~food_share, ~feed_share, ~other_uses_share,
    2000, "A", "Wheat", "Cropland", 50, 30, 36, 6, 18, 0.6, 0.3, 0.1,
    2000, "A", "FishProd", "Fish", 20, 10, 15, 3, 12, 0.5, 0.4, 0.1
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
    ~Year, ~Province_name, ~Item, ~Box, ~Irrig_cat, ~local_consumption, ~import_consumption, ~food, ~other_uses, ~feed, ~food_share, ~feed_share, ~other_uses_share,
    2000, "A", "Wheat", "Cropland", "irrig", 40, 20, 30, 0, 30, 0.5, 0.5, 0.0,
    2000, "A", "Wheat", "Cropland", "rainfed", 60, 30, 40, 0, 40, 0.5, 0.5, 0.0
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
    ~Year, ~Province_name, ~Item, ~Irrig_cat, ~Box, ~export, ~food, ~other_uses, ~feed, ~production_n, ~import,
    2000, "A", "Wheat", "irrig", "Cropland", 40, 30, 10, 20, 100, 0,
    2000, "A", "Wheat", "rainfed", "Cropland", 10, 10, 5, 5, 30, 0
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
    ~Year, ~Province_name, ~Item, ~Irrig_cat, ~Box, ~deposition, ~fixation, ~synthetic, ~manure, ~urban,
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
    ~Year, ~Province_name, ~Item, ~Irrig_cat, ~Box, ~deposition, ~fixation, ~synthetic, ~manure, ~urban,
    2000, "A", "Wheat", "irrig", "Cropland", 1, 2, 3, 4, 5
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
    ~Year, ~Province_name, ~Item, ~Irrig_cat, ~Box, ~deposition, ~fixation, ~synthetic, ~manure, ~urban,
    2000, "A", "Wheat", "irrig", "Cropland", 0, 0, 3, 0, 0
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
    ~Year, ~Province_name, ~Item, ~prod_type, ~production_fm,
    2000, "A", "Wheat", "Product", 300,
    2000, "B", "Wheat", "Product", 30
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
    ~Year, ~Province_name, ~Item, ~prod_type, ~production_fm,
    2000, "A", "Wheat", "Product", 10
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
    ~Year, ~Province_name, ~Item, ~prod_type, ~production_fm,
    2000, "A", "Barley", "Product", 200
  )

  out <- .remove_seeds_from_system(npp, pie_seed, prod)

  expect_equal(out$production_fm, 200)
})

test_that(".remove_seeds_from_system takes seed only from the Product row", {
  # Seed is grain. `grafs_prod_combined` carries Residue and Grass rows for the
  # same (Year, Province_name, Item), and the join attaches the same per-item
  # seed mass to each, so subtracting from all three removed ~3x the real seed
  # use here and understated residue and grass biomass (#147).
  npp <- tibble::tribble(
    ~Year, ~Province_name, ~Item, ~Area_ygpit_ha, ~LandUse,
    2000, "A", "Wheat", 100, "Cropland"
  )

  pie_seed <- tibble::tribble(
    ~Year, ~Item, ~Element, ~Destiny, ~Value_destiny,
    2000, "Wheat", "Domestic_supply", "Seed", 10
  )

  prod <- tibble::tribble(
    ~Year, ~Province_name, ~Item, ~prod_type, ~production_fm,
    2000, "A", "Wheat", "Product", 100,
    2000, "A", "Wheat", "Residue", 80,
    2000, "A", "Wheat", "Grass", 60
  )

  out <- .remove_seeds_from_system(npp, pie_seed, prod)
  got <- function(type) out$production_fm[out$prod_type == type]

  # Only the Product row loses the 10 Mg of seed.
  expect_equal(got("Product"), 90)
  expect_equal(got("Residue"), 80)
  expect_equal(got("Grass"), 60)

  # Total removed is the seed used exactly once, not once per prod_type.
  expect_equal(sum(prod$production_fm) - sum(out$production_fm), 10)
})

test_that(".remove_seeds_from_system caps against the Product row alone", {
  # The 50% cap must bind on the Product row's own production, not be applied
  # separately to each prod_type against its own denominator.
  npp <- tibble::tribble(
    ~Year, ~Province_name, ~Item, ~Area_ygpit_ha, ~LandUse,
    2000, "A", "Wheat", 1, "Cropland"
  )

  pie_seed <- tibble::tribble(
    ~Year, ~Item, ~Element, ~Destiny, ~Value_destiny,
    2000, "Wheat", "Domestic_supply", "Seed", 1000
  )

  prod <- tibble::tribble(
    ~Year, ~Province_name, ~Item, ~prod_type, ~production_fm,
    2000, "A", "Wheat", "Product", 10,
    2000, "A", "Wheat", "Residue", 40
  )

  out <- .remove_seeds_from_system(npp, pie_seed, prod)

  expect_equal(out$production_fm[out$prod_type == "Product"], 5)
  expect_equal(out$production_fm[out$prod_type == "Residue"], 40)
})


# .finalize_prod_destiny -------------------------------------------------------

test_that(".finalize_prod_destiny combines local, import, export flows", {
  trade_data <- tibble::tribble(
    ~Year, ~Province_name, ~Item, ~Box, ~Irrig_cat, ~food, ~other_uses, ~feed, ~production_n, ~export, ~import,
    2000, "A", "Wheat", "Cropland", "irrig", 30, 10, 20, 100, 40, 0,
    2000, "A", "Fish", "Fish", NA, 5, 0, 0, 0, 0, 5
  )

  codes <- tibble::tribble(
    ~item, ~group, ~Name_biomass,
    "Wheat", "Primary crops", "Wheat",
    "Fish", "Fish", "Fish"
  )

  soil <- tibble::tribble(
    ~Year, ~Province_name, ~Item, ~Irrig_cat, ~Box, ~deposition, ~fixation, ~synthetic, ~manure, ~urban,
    2000, "A", "Wheat", "irrig", "Cropland", 1, 0, 0, 0, 0
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
