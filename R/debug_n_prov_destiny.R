create_n_prov_destiny_debug <- function() {
  codes_coefs_items_full <- whep_read_file("codes_coefs_items_full")
  biomass_coefs <- whep_read_file("biomass_coefs")
  pie_full_destinies_fm <- whep_read_file("pie_full_destinies_fm")
  processed_prov_fixed <- whep_read_file("processed_prov_fixed")
  livestock_prod_ygps <- whep_read_file("livestock_prod_ygps")
  crop_area_npp_no_fallow <- whep_read_file("crop_area_npp_ygpitr_no_fallow")
  npp_ygpit <- whep_read_file("npp_ygpit")
  codes_coefs <- whep_read_file("codes_coefs")
  intake_ygiac <- whep_read_file("intake_ygiac")
  population_yg <- whep_read_file("population_yg")
  n_balance_ygpit_all <- whep_read_file("n_balance_ygpit_all")

  biomass_item_merged <- .merge_items_biomass(npp_ygpit, codes_coefs)

  prod_combined_boxes <- biomass_item_merged |>
    .aggregate_crop_seminatural(
      .summarise_crops_residues(crop_area_npp_no_fallow)
    ) |>
    .combine_production_boxes(
      .prepare_livestock_production(livestock_prod_ygps)
    )

  food_and_other_uses <- population_yg |>
    .calculate_population_share() |>
    .calculate_food_and_other_uses(pie_full_destinies_fm)

  adding_feed_output <- .adding_feed(intake_ygiac)

  grafs_prod_item_n <- biomass_item_merged |>
    .remove_seeds_from_system(pie_full_destinies_fm, prod_combined_boxes) |>
    .add_grass_wood() |>
    .prepare_prod_data(
      .prepare_processed_data(processed_prov_fixed),
      codes_coefs_items_full
    ) |>
    .convert_fm_dm_n(biomass_coefs) |>
    .combine_destinies(
      adding_feed_output$feed_intake,
      food_and_other_uses
    ) |>
    .convert_to_items_n(codes_coefs_items_full, biomass_coefs)

  n_soil_inputs <- .calculate_n_soil_inputs(
    n_balance_ygpit_all,
    codes_coefs
  )

  trade_data <- .calculate_trade(
    grafs_prod_item_n,
    pie_full_destinies_fm,
    biomass_coefs,
    codes_coefs_items_full
  )

  prod_destiny <- .finalize_prod_destiny(
    grafs_prod_item_trade = trade_data,
    codes_coefs_items_full = codes_coefs_items_full,
    n_soil_inputs = n_soil_inputs,
    feed_share_rum_mono = adding_feed_output$feed_share_rum_mono
  )

  prod_destiny <- .add_n_soil_inputs(prod_destiny, soil_inputs = n_soil_inputs)

  list(
    biomass_item_merged = biomass_item_merged,
    prod_combined_boxes = prod_combined_boxes,
    food_and_other_uses = food_and_other_uses,
    adding_feed_output = adding_feed_output,
    grafs_prod_item_n = grafs_prod_item_n,
    n_soil_inputs = n_soil_inputs,
    trade_data = trade_data,
    prod_destiny_final = prod_destiny,
    pie_full_destinies_fm = pie_full_destinies_fm,
    biomass_coefs = biomass_coefs,
    codes_coefs_items_full = codes_coefs_items_full
  )
}


# Check food
# Result: only small deviations
pie_food <- dbg$pie_full_destinies_fm |>
  filter(Element == "Domestic_supply", Destiny == "Food") |>
  group_by(Year, Item) |>
  summarise(national_food = sum(Value_destiny, na.rm = TRUE), .groups = "drop")

prov_food <- dbg$food_and_other_uses |>
  group_by(Year, Item) |>
  summarise(prov_food = sum(food, na.rm = TRUE), .groups = "drop")

check_food <- pie_food |>
  left_join(prov_food, by = c("Year", "Item")) |>
  mutate(
    prov_food = coalesce(prov_food, 0),
    diff = prov_food - national_food
  )

check_food |>
  filter(abs(diff) > 1e-6) |>
  arrange(desc(abs(diff)))

# Check feed
# Result: PIE feed values are much smaller then the values from Intake_ygiac
pie_feed <- dbg$pie_full_destinies_fm |>
  filter(Element == "Domestic_supply", Destiny == "Feed") |>
  group_by(Year, Item) |>
  summarise(national_feed = sum(Value_destiny, na.rm = TRUE), .groups = "drop")

prov_feed <- dbg$adding_feed_output$feed_intake |>
  group_by(Year, Item) |>
  summarise(prov_feed = sum(feed, na.rm = TRUE), .groups = "drop")

check_feed <- pie_feed |>
  left_join(prov_feed, by = c("Year", "Item")) |>
  mutate(
    prov_feed = coalesce(prov_feed, 0),
    diff = prov_feed - national_feed
  )

check_feed |>
  filter(abs(diff) > 1e-6) |>
  arrange(desc(abs(diff)))

# Check food final dataset and PIE
orig_food <- whep_read_file("pie_full_destinies_fm") |>
  filter(Element == "Domestic_supply", Destiny == "Food") |>
  left_join(
    whep_read_file("codes_coefs_items_full"),
    by = c("Item" = "item")
  ) |>
  left_join(whep_read_file("biomass_coefs"), by = "Name_biomass") |>
  mutate(N = Value_destiny * Product_kgDM_kgFM * Product_kgN_kgDM) |>
  group_by(Year) |>
  summarise(food_orig = sum(N, na.rm = TRUE))

final_food <- prov_destiny_n |>
  filter(Destiny == "population_food") |>
  group_by(Year) |>
  summarise(food_model = sum(MgN, na.rm = TRUE))

food_diff <- left_join(orig_food, final_food, by = "Year") |>
  mutate(diff = food_model - food_orig) |>
  View()

# Check food final dataset with pets food
orig_food <- whep_read_file("pie_full_destinies_fm") |>
  dplyr::filter(Element == "Domestic_supply", Destiny == "Food") |>
  dplyr::left_join(
    whep_read_file("codes_coefs_items_full"),
    by = c("Item" = "item")
  ) |>
  dplyr::left_join(whep_read_file("biomass_coefs"), by = "Name_biomass") |>
  dplyr::mutate(N = Value_destiny * Product_kgDM_kgFM * Product_kgN_kgDM) |>
  dplyr::group_by(Year) |>
  dplyr::summarise(food_orig = sum(N, na.rm = TRUE))

final_food <- prov_destiny_n |>
  dplyr::filter(Destiny == "population_food") |>
  dplyr::group_by(Year) |>
  dplyr::summarise(food_model = sum(MgN, na.rm = TRUE))

pets_food_fm <- dbg$adding_feed_output$feed_intake |>
  dplyr::group_by(Year) |>
  dplyr::summarise(food_pets_fm = sum(food_pets, na.rm = TRUE))

pets_food_n <- dbg$adding_feed_output$feed_intake |>
  dplyr::left_join(
    whep_read_file("codes_coefs_items_full"),
    by = c("Item" = "item")
  ) |>
  dplyr::left_join(whep_read_file("biomass_coefs"), by = "Name_biomass") |>
  dplyr::mutate(N = food_pets * Product_kgDM_kgFM * Product_kgN_kgDM) |>
  dplyr::group_by(Year) |>
  dplyr::summarise(food_pets = sum(N, na.rm = TRUE))

food_compare <- orig_food |>
  dplyr::left_join(final_food, by = "Year") |>
  dplyr::left_join(pets_food_n, by = "Year") |>
  dplyr::mutate(
    added_by_pets = dplyr::coalesce(food_pets, 0),
    food_model_no_pets = food_model - added_by_pets,
    diff_total = food_model - food_orig,
    diff_no_pets = food_model_no_pets - food_orig
  )

View(food_compare)
