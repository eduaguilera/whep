dbg <- create_n_prov_destiny_debug()

# Check food in FM
# Result: only small deviations in FM
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

# Check food final dataset and PIE in N
# Result: higher differences
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

final_food <- dbg$prod_destiny_final |>
  dplyr::filter(Destiny == "population_food") |>
  dplyr::group_by(Year) |>
  dplyr::summarise(food_model = sum(MgN, na.rm = TRUE))

food_diff_n <- dplyr::left_join(orig_food, final_food, by = "Year") |>
  dplyr::mutate(diff = food_model - food_orig)

View(food_diff_n)


# Check food final dataset with pets food
# Pets food in food does not make a big difference
orig_food <- whep_read_file("pie_full_destinies_fm") |>
  dplyr::filter(Element == "Domestic_supply", Destiny == "Food") |>
  dplyr::left_join(
    whep_read_file("codes_coefs_items_full"),
    by = c("Item" = "item")
  ) |>
  dplyr::left_join(
    whep_read_file("biomass_coefs"),
    by = "Name_biomass"
  ) |>
  dplyr::mutate(N = Value_destiny * Product_kgDM_kgFM * Product_kgN_kgDM) |>
  dplyr::group_by(Year) |>
  dplyr::summarise(food_orig = sum(N, na.rm = TRUE))


final_food_local <- dbg$prod_destiny_final |>
  dplyr::filter(
    Destiny == "population_food",
    Origin != "Outside"
  ) |>
  dplyr::group_by(Year) |>
  dplyr::summarise(food_model_local = sum(MgN, na.rm = TRUE))


pets_food_n <- dbg$adding_feed_output$feed_intake |>
  dplyr::left_join(
    whep_read_file("codes_coefs_items_full"),
    by = c("Item" = "item")
  ) |>
  dplyr::left_join(
    whep_read_file("biomass_coefs"),
    by = "Name_biomass"
  ) |>
  dplyr::mutate(N = food_pets * Product_kgDM_kgFM * Product_kgN_kgDM) |>
  dplyr::group_by(Year) |>
  dplyr::summarise(food_pets = sum(N, na.rm = TRUE))


food_compare <- orig_food |>
  dplyr::left_join(final_food_local, by = "Year") |>
  dplyr::left_join(pets_food_n, by = "Year") |>
  dplyr::mutate(
    food_pets = dplyr::coalesce(food_pets, 0),
    food_model_no_pets = food_model_local - food_pets,
    diff_total = food_model_local - food_orig,
    diff_no_pets = food_model_no_pets - food_orig,
    ratio_total = food_model_local / food_orig,
    ratio_no_pets = food_model_no_pets / food_orig
  )

View(food_compare)


# Check food and other uses
# Result: Higher values in my dataset
pie <- whep_read_file("pie_full_destinies_fm") |>
  dplyr::filter(
    Element == "Domestic_supply",
    Destiny %in% c("Food", "Other_uses")
  ) |>
  dplyr::left_join(
    whep_read_file("codes_coefs_items_full"),
    by = c("Item" = "item")
  ) |>
  dplyr::left_join(
    whep_read_file("biomass_coefs"),
    by = "Name_biomass"
  ) |>
  dplyr::mutate(
    N = Value_destiny * Product_kgDM_kgFM * Product_kgN_kgDM
  )

orig <- pie |>
  dplyr::group_by(Year, Destiny) |>
  dplyr::summarise(N = sum(N, na.rm = TRUE), .groups = "drop") |>
  tidyr::pivot_wider(
    names_from = Destiny,
    values_from = N,
    values_fill = 0
  ) |>
  dplyr::rename(
    food_orig = Food,
    other_orig = `Other_uses`
  ) |>
  dplyr::mutate(
    orig_total = food_orig + other_orig
  )

model <- dbg$prod_destiny_final |>
  dplyr::filter(
    Destiny %in% c("population_food", "population_other_uses")
  ) |>
  dplyr::group_by(Year, Destiny) |>
  dplyr::summarise(MgN = sum(MgN, na.rm = TRUE), .groups = "drop") |>
  tidyr::pivot_wider(
    names_from = Destiny,
    values_from = MgN,
    values_fill = 0
  ) |>
  dplyr::rename(
    food_model = population_food,
    other_model = population_other_uses
  ) |>
  dplyr::mutate(
    model_total = food_model + other_model
  )

compare <- orig |>
  dplyr::left_join(model, by = "Year") |>
  dplyr::mutate(
    diff_food = food_model - food_orig,
    diff_other = other_model - other_orig,
    diff_total = model_total - orig_total,
    ratio_food = food_model / food_orig,
    ratio_other = other_model / other_orig,
    ratio_total = model_total / orig_total
  )

View(compare)


# Compare items
# Result: Imports are the same, consumption of food items, produced in the
# province are higher
orig_item <- whep_read_file("pie_full_destinies_fm") |>
  dplyr::filter(Element == "Domestic_supply", Destiny == "Food") |>
  dplyr::left_join(
    whep_read_file("codes_coefs_items_full"),
    by = c("Item" = "item")
  ) |>
  dplyr::left_join(
    whep_read_file("biomass_coefs"),
    by = "Name_biomass"
  ) |>
  dplyr::mutate(
    N = Value_destiny * Product_kgDM_kgFM * Product_kgN_kgDM
  ) |>
  dplyr::group_by(Item) |>
  dplyr::summarise(
    MgN_orig = sum(N, na.rm = TRUE),
    .groups = "drop"
  )

model_item <- dbg$prod_destiny_final |>
  dplyr::filter(Destiny == "population_food") |>
  dplyr::group_by(Item) |>
  dplyr::summarise(
    MgN_model = sum(MgN, na.rm = TRUE),
    .groups = "drop"
  )

item_compare <- orig_item |>
  dplyr::left_join(model_item, by = "Item") |>
  dplyr::mutate(
    diff = MgN_model - MgN_orig,
    ratio = MgN_model / MgN_orig
  ) |>
  dplyr::arrange(dplyr::desc(ratio))

View(item_compare)


# Check fm to N coefs
pie_food_other_fm <- whep_read_file("pie_full_destinies_fm") |>
  dplyr::filter(
    Element == "Domestic_supply",
    Destiny %in% c("Food", "Other_uses")
  ) |>
  dplyr::group_by(Year, Item, Destiny) |>
  dplyr::summarise(
    Value_destiny = sum(Value_destiny, na.rm = TRUE),
    .groups = "drop"
  )

population_share <- whep_read_file("population_yg") |>
  .calculate_population_share()

prov_food_other_fm <- .calculate_food_and_other_uses(
  population_share,
  whep_read_file("pie_full_destinies_fm")
)

check_mass_fm <- prov_food_other_fm |>
  tidyr::pivot_longer(
    cols = c(food, other_uses),
    names_to = "Destiny",
    values_to = "Value_destiny"
  ) |>
  dplyr::group_by(Year, Item, Destiny) |>
  dplyr::summarise(
    Value_destiny_model = sum(Value_destiny, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::left_join(pie_food_other_fm, by = c("Year", "Item", "Destiny")) |>
  dplyr::mutate(
    diff_fm = Value_destiny_model - Value_destiny,
    ratio_fm = Value_destiny_model / Value_destiny
  )

# Compare food items
# Result: it's the same
codes_coefs_items_full <- whep_read_file("codes_coefs_items_full")
biomass_coefs <- whep_read_file("biomass_coefs")

orig_food_N_item <- pie_food_other_fm |>
  dplyr::filter(Destiny == "Food") |>
  dplyr::left_join(
    codes_coefs_items_full,
    by = c("Item" = "item")
  ) |>
  dplyr::left_join(
    biomass_coefs,
    by = "Name_biomass"
  ) |>
  dplyr::mutate(
    MgN_orig = Value_destiny *
      Product_kgDM_kgFM *
      Product_kgN_kgDM
  ) |>
  dplyr::group_by(Year, Item) |>
  dplyr::summarise(
    MgN_orig = sum(MgN_orig, na.rm = TRUE),
    .groups = "drop"
  )

model_food_N_item <- prov_food_other_fm |>
  dplyr::group_by(Year, Item) |>
  dplyr::summarise(
    food_fm_total = sum(food, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::left_join(
    codes_coefs_items_full,
    by = c("Item" = "item")
  ) |>
  dplyr::left_join(
    biomass_coefs,
    by = "Name_biomass"
  ) |>
  dplyr::mutate(
    MgN_model = food_fm_total *
      Product_kgDM_kgFM *
      Product_kgN_kgDM
  ) |>
  dplyr::select(Year, Item, MgN_model)


# Check if there are more then 1 items with the same combination
# Bug found: there are more then one item because of irrigated/rainfed.
# crop production shares need to be multiplied with consumption to distinguish
# between irrigated rainfed consumption.
grafs_prod_item_combined <- whep:::.combine_destinies(
  dbg$grafs_prod_item,
  dbg$adding_feed_output$feed_intake,
  dbg$food_and_other_uses
)

test_dups <- grafs_prod_item_combined |>
  dplyr::count(Year, Province_name, Item) |>
  dplyr::filter(n > 1)

test_dups
