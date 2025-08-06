#' @title GRAFS Nitrogen (N) production and their destinies
#'
#' @description
#' Provides N production of crops and livestock, categorized by their destinies:
#'  food, feed, other uses, exports, imports, which is the base of the
#'  GRAFS model. The dataset contains data in megagrams of N (MgN) for each
#'  year, province, item, and box (cropland, semi natural agroecosystems,
#'  livestock, fish, additives). Processed items, residues, woody crops,
#'  grazed weeds are taken into account.
#'
#' @return
#' A final tibble containing N production data by destiny.
#' It includes the following columns:
#'   - `year`: The year in which the recorded event occurred.
#'   - `province_name`: The Spanish province where the data is from.
#'   - `item`: The item which was produced, defined in `names_biomass_cb`.
#'   - `box`: One of the GRAFS model systems: cropland,
#'   Semi-natural agroecosystems, Livestock, Fish, or Additives.
#'   - `destiny`: The use category of the nitrogen: Food, Feed, Other_uses,
#'   Export, or Import.
#'   - `MgN`: Nitrogen amount in megagrams (Mg).
#'
#' @export
create_prod_and_destiny_grafs <- function() {
  data <- .load_data()
  biomass_item_merged <- .merge_items_biomass(
    data$npp_ygpit_csv, data$names_biomass_cb
  )
  data$npp_ygpit_csv <- biomass_item_merged$npp_ygpit_merged
  production_crops_residues <-
    .summarise_crops_residues(data$crop_area_npp_ygpitr_no_fallow)
  grazed_data_added <-
    .aggregate_grazed_cropland(
      biomass_item_merged$npp_ygpit_merged,
      production_crops_residues
    )
  livestock_data <- .prepare_livestock_production(data$livestock_prod_ygps)
  prod_combined_boxes <- .combine_production_boxes(
    grazed_data_added,
    livestock_data
  )
  seeds_removed <- .remove_seeds_from_system(
    biomass_item_merged$npp_ygpit_merged,
    data$pie_full_destinies_fm, prod_combined_boxes
  )
  grass_wood_added <- .adding_grass_wood(seeds_removed)
  prepared_processed_data <- .prepare_processed_data(data$processed_prov_fixed)
  prepared_prod_data <- .prepare_prod_data(
    grass_wood_added, prepared_processed_data, data$codes_coefs_items_full
  )
  converted_data_fm_dm_n <- .convert_fm_dm_n(
    prepared_prod_data,
    data$biomass_coefs
  )
  feed_data <- .adding_feed(data$feed_intake)
  population_share <- .calculate_population_share(data$population_share)
  food_data <- .adding_food(data$pie_full_destinies_fm, population_share)
  other_uses_data <- .adding_other_uses(
    data$pie_full_destinies_fm, population_share
  )
  combined_destinies <- .combine_destinies(
    converted_data_fm_dm_n, feed_data, food_data, other_uses_data
  )
  converted_items_n <- .convert_to_items_n(
    combined_destinies, data$codes_coefs_items_full, data$biomass_coefs
  )
  trade <- .calculate_trade(converted_items_n)

  .finalize_prod_destiny(trade, data$codes_coefs_items_full)
}

#' Load all required datasets from input directory -----------------------------
#' @return A named list including the required datasets.
#' @keywords internal
#' @noRd
.load_data <- function() {
  npp_ygpit_csv <- whep_read_file("npp_ygpit")
  crop_area_npp_ygpitr_no_fallow <- whep_read_file(
    "crop_area_npp_ygpitr_no_fallow"
  )
  pie_full_destinies_fm <- whep_read_file("pie_full_destinies_fm")
  feed_intake <- whep_read_file("intake_ygiac")
  population_share <- whep_read_file("population_yg")
  n_excretion_ygs <- whep_read_file("n_excretion_ygs")
  livestock_prod_ygps <- whep_read_file("livestock_prod_ygps")

  names_biomass_cb <- whep_read_file("codes_coefs")
  codes_coefs_items_full <- whep_read_file("codes_coefs_items_full")
  biomass_coefs <- whep_read_file("biomass_coefs")
  processed_prov_fixed <- whep_read_file("processed_prov_fixed")

  data_list <- list(
    npp_ygpit_csv = npp_ygpit_csv,
    crop_area_npp_ygpitr_no_fallow = crop_area_npp_ygpitr_no_fallow,
    pie_full_destinies_fm = pie_full_destinies_fm,
    feed_intake = feed_intake,
    population_share = population_share,
    n_excretion_ygs = n_excretion_ygs,
    livestock_prod_ygps = livestock_prod_ygps,
    names_biomass_cb = names_biomass_cb,
    codes_coefs_items_full = codes_coefs_items_full,
    biomass_coefs = biomass_coefs,
    processed_prov_fixed = processed_prov_fixed
  )

  data_list
}

#' @title Production of Cropland, Livestock, and Semi-natural agroecosystems
#' @description Merge items with biomasses.
#'
#' @param npp_ygpit_csv Dataframe with N data.
#' @param names_biomass_cb Dataframe with biomass names and associated item
#' names.
#'
#' @return A list with two merged dataframes: 'crop_area_npp_merged' and
#' 'npp_ygpit_merged'.
#' @keywords internal
#' @noRd
.merge_items_biomass <- function(
  npp_ygpit_csv,
  names_biomass_cb
) {
  npp_ygpit_merged <- npp_ygpit_csv |>
    dplyr::left_join(
      names_biomass_cb |> dplyr::select(Name_biomass, Item),
      by = "Name_biomass"
    )

  list(npp_ygpit_merged = npp_ygpit_merged)
}

#' @title Crops Production and Residues ----------------------------------------
#'
#' @param crop_area_npp_ygpitr_no_fallow Dataframe excluding fallow.
#'
#' @return A dataframe summarizing total crop production and residues per
#' province and year.
#' @keywords internal
#' @noRd
.summarise_crops_residues <- function(crop_area_npp_ygpitr_no_fallow) {
  crop_area_npp_prod_residue <- crop_area_npp_ygpitr_no_fallow |>
    dplyr::select(
      Year,
      Province_name,
      Name_biomass,
      Prod_ygpit_Mg,
      Product_residue,
      Item,
      Irrig_cat,
      Irrig_type
    ) |>
    dplyr::mutate(LandUse = "Cropland") |>
    dplyr::rename(prod_type = Product_residue) |>
    dplyr::group_by(
      Year,
      Province_name,
      Name_biomass,
      Item,
      prod_type,
      LandUse,
      Irrig_cat,
      Irrig_type
    ) |>
    dplyr::summarise(
      production_fm = sum(as.numeric(Prod_ygpit_Mg), na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(Box = "Cropland")

  crop_area_npp_prod_residue
}


#' @title Combining all plant production (harvested products and residues,
#' and grazed grass) ----------------------------------------------------------
#'
#' @param npp_ygpit_merged NPP merged data including all biomasses and items.
#'
#' @return A dataframe combining products, residues, and grazed biomass.
#' @keywords internal
#' @noRd
.aggregate_grazed_cropland <- function(
  npp_ygpit_merged,
  crop_area_npp_prod_residue
) {
  fallow_grazed <- npp_ygpit_merged |>
    dplyr::filter(
      LandUse == "Cropland",
      Item == "Fallow",
      Name_biomass == "Fallow"
    ) |>
    dplyr::group_by(
      Year,
      Province_name,
      Name_biomass,
      Item,
      LandUse,
      Irrig_cat,
      Irrig_type
    ) |>
    dplyr::summarise(
      production_fm = sum(GrazedWeeds_MgDM, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      prod_type = "Grass",
      Box = "Cropland"
    )

  semi_natural <- dplyr::bind_rows(
    npp_ygpit_merged |>
      dplyr::filter(LandUse != "Cropland") |>
      dplyr::select(
        Year, Province_name, Name_biomass, Item,
        LandUse, Irrig_cat, Irrig_type,
        production_fm = GrazedWeeds_MgDM
      ) |>
      dplyr::mutate(prod_type = "Grass"),
    npp_ygpit_merged |>
      dplyr::filter(LandUse != "Cropland") |>
      dplyr::select(
        Year, Province_name, Name_biomass, Item,
        LandUse, Irrig_cat, Irrig_type,
        production_fm = Prod_ygpit_Mg
      ) |>
      dplyr::mutate(prod_type = "Product"),
    npp_ygpit_merged |>
      dplyr::filter(LandUse != "Cropland") |>
      dplyr::select(
        Year, Province_name, Name_biomass, Item,
        LandUse, Irrig_cat, Irrig_type,
        production_fm = Used_Residue_MgFM
      ) |>
      dplyr::mutate(prod_type = "Residue")
  ) |>
    dplyr::mutate(Box = "Semi_natural_agroecosystems")

  combined_biomasses <- dplyr::bind_rows(
    crop_area_npp_prod_residue,
    fallow_grazed,
    semi_natural
  )

  combined_biomasses
}


#' @title Livestock Production -------------------------------------------------
#'
#' @param livestock_prod_ygps A dataframe including livestock production data.
#'
#' @return A dataframe formatted for integration with other production data.
#' @keywords internal
#' @noRd
.prepare_livestock_production <- function(
  livestock_prod_ygps
) {
  livestock <- livestock_prod_ygps |>
    dplyr::select(
      Year, Province_name, Item, Name_biomass, Prod_Mg
    ) |>
    dplyr::mutate(
      Box = "Livestock",
      prod_type = "Product"
    )

  livestock
}

#' @title Combine Cropland, Semi_natural_agroecosystems and Livestock ----------
#'
#' @param combined_biomasses Dataframe of crop production.
#' @param semi_natural_agroecosystems Dataframe of production from semi-natural
#' agroecosystems.
#' @param livestock Dataframe of livestock production.
#'
#' @return Combined dataframe of all production systems.
#' @keywords internal
#' @noRd
.combine_production_boxes <- function(
  combined_biomasses,
  livestock
) {
  grafs_prod_combined <- dplyr::bind_rows(
    combined_biomasses,
    livestock |>
      dplyr::rename(production_fm = Prod_Mg)
  )

  grafs_prod_combined
}

#' @title Seed production per province, based on national seed rate per Area
#' @description Calculates the amount of seeds used per province and subtracts
#' it from total production.
#'
#' @param npp_ygpit_csv Dataframe containing crop area by province.
#' @param pie_full_destinies_fm Dataframe containing domestic supply by
#' destiny, including seed usage.
#' @param grafs_prod_combined Dataframe with total production values.
#'
#' @return A dataframe with production values after subtracting seed usage.
#' @keywords internal
#' @noRd
.remove_seeds_from_system <- function(
  npp_ygpit_merged,
  pie_full_destinies_fm,
  grafs_prod_combined
) {
  seed_rates <- npp_ygpit_merged |>
    dplyr::filter(LandUse == "Cropland") |>
    dplyr::group_by(Year, Province_name, Item) |>
    dplyr::summarise(
      Area_ha = sum(Area_ygpit_ha, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::left_join(
      pie_full_destinies_fm |>
        dplyr::filter(Element == "Domestic_supply", Destiny == "Seed") |>
        dplyr::group_by(Year, Item) |>
        dplyr::summarise(
          Seed_total = sum(Value_destiny, na.rm = TRUE),
          .groups = "drop"
        ) |>
        dplyr::left_join(
          npp_ygpit_merged |>
            dplyr::filter(LandUse == "Cropland") |>
            dplyr::group_by(Year, Item) |>
            dplyr::summarise(
              National_area = sum(Area_ygpit_ha, na.rm = TRUE),
              .groups = "drop"
            ),
          by = c("Year", "Item")
        ) |>
        dplyr::mutate(Seed_rate_per_ha = Seed_total / National_area) |>
        dplyr::select(Year, Item, Seed_rate_per_ha),
      by = c("Year", "Item")
    ) |>
    dplyr::mutate(Seeds_used_MgFM = Area_ha * Seed_rate_per_ha) |>
    tidyr::drop_na()

  # Substracting the Seed data from Production in grafs_prod_combined
  grafs_prod_combined_no_seeds <- grafs_prod_combined |>
    dplyr::left_join(
      seed_rates |>
        dplyr::select(Year, Province_name, Item, Seeds_used_MgFM),
      by = c("Year", "Province_name", "Item")
    ) |>
    dplyr::mutate(
      production_fm = production_fm -
        dplyr::coalesce(Seeds_used_MgFM, 0)
    ) |>
    dplyr::select(-Seeds_used_MgFM)

  grafs_prod_combined_no_seeds
}

#' @title Structuring dataset (GrazedWeeds und Used_Residues in ProductionFM)
#' @description Replace production_fm with GrazedWeeds_MgDM (for Fallow).
#' QUESTION: Is Acorns Residue Acorns or Firewood?
#'
#' @param grafs_prod_combined_no_seeds Dataframe of production without seeds.
#'
#' @return A dataframe with added grass and wood production.
#' @keywords internal
#' @noRd
.adding_grass_wood <- function(grafs_prod_combined_no_seeds) {
  grafs_prod_added <- grafs_prod_combined_no_seeds |>
    dplyr::mutate(
      Item = dplyr::case_when(
        prod_type == "Grass" & Name_biomass == "Fallow" ~ "Fallow",
        prod_type == "Grass" ~ "Grassland",
        prod_type == "Residue" & Box != "Cropland" &
          Name_biomass %in% c(
            "Holm oak forest", "Conifers", "Mediterranean shrubland"
          ) ~ "Firewood",
        TRUE ~ Item
      ),
      Name_biomass = dplyr::case_when(
        prod_type == "Grass" & Item == "Grassland" ~ "Grass",
        Item == "Firewood" ~ "Firewood",
        TRUE ~ Name_biomass
      )
    ) |>
    dplyr::mutate(
      production_fm = dplyr::if_else(
        prod_type == "Grass" & Item == "Grassland" & !is.na(production_fm),
        production_fm / 0.2,
        production_fm
      )
    ) |>
    dplyr::filter(!is.na(production_fm)) |>
    dplyr::group_by(
      Year, Province_name, Name_biomass, Item, Box, LandUse,
      Irrig_cat, Irrig_type, prod_type
    ) |>
    dplyr::summarise(
      production_fm = sum(production_fm, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(Year, Province_name, Name_biomass, Item)

  grafs_prod_added
}


#' @title Processed Items ------------------------------------------------------
#' @description Summarise processed items by Year, Province, Biomass,
#' Item & ProcessedItem.
#'
#' @param processed_prov_fixed Dataframe containing data for processed items.
#'
#' @return A dataframe with processed item values structured for integration.
#' @keywords internal
#' @noRd
.prepare_processed_data <- function(
  processed_prov_fixed
) {
  processed_data <- processed_prov_fixed |>
    dplyr::group_by(Year, Province_name, Name_biomass, Item, ProcessedItem) |>
    dplyr::summarise(
      ProcessedItem_amount = sum(ProcessedItem_amount, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      Item = ProcessedItem,
      production_fm = ProcessedItem_amount,
      Box = "Cropland",
      prod_type = "Product"
    ) |>
    dplyr::select(
      Year, Province_name, Name_biomass, Item, Box, production_fm, prod_type
    )

  processed_data
}

#' @title Match structure of grafs_prod_combined_no_seeds ----------------------
#' @description Combines grass, wood and processed item data into a unified
#' structure and merges biomass names.
#'
#' @param grafs_prod_added Data with added grass and wood production.
#' @param processed_data Dataframe with processed item values.
#' @param codes_coefs_items_full Dataframe with item-to-biomass names.
#'
#' @return A unified dataframe with complete production data for items.
#' @keywords internal
#' @noRd
.prepare_prod_data <- function(
  grafs_prod_added,
  processed_data,
  codes_coefs_items_full
) {
  added_grass_wood_prepared <- grafs_prod_added |>
    dplyr::select(
      Year, Province_name, Name_biomass, Item, Box, LandUse,
      Irrig_cat, Irrig_type, prod_type, production_fm
    ) |>
    dplyr::bind_rows(processed_data) |>
    dplyr::arrange(Year, Province_name, Name_biomass, Item)

  # Merging Item and Name_biomass and creating Name_biomass_primary
  added_grass_wood_merged <- added_grass_wood_prepared |>
    dplyr::rename(Name_biomass_primary = Name_biomass) |>
    dplyr::left_join(
      codes_coefs_items_full |>
        dplyr::select(item, Name_biomass),
      by = c("Item" = "item")
    ) |>
    dplyr::mutate(Name_biomass = dplyr::if_else(!is.na(Name_biomass),
      Name_biomass, Name_biomass_primary
    )) |>
    dplyr::relocate(Name_biomass, .after = Name_biomass_primary)

  added_grass_wood_merged
}

#' @title Convert Fresh Matter (FM) to Dry Matter (DM) and finally to N
#' @description Define a list of special items that require using the primary
#' biomass name for selecting conversion coefficients.
#'
#' @param added_grass_wood_merged Dataframe with production values and biomass.
#' @param biomass_coefs Dataframe with FM→DM and DM→N conversion coefficients
#' for each biomass.
#'
#' @return A dataframe with total dry matter and N production.
#' @keywords internal
#' @noRd
.convert_fm_dm_n <- function(
  added_grass_wood_merged,
  biomass_coefs
) {
  special_items <- c(
    "Nuts and products", "Vegetables, Other", "Fruits, Other",
    "Cereals, Other", "Pulses, Other and products"
  )

  # Add a column to choose the appropriate biomass name for matching
  # conversion factors
  grazed_no_seeds_primary <- added_grass_wood_merged |>
    dplyr::mutate(
      Biomass_match = dplyr::if_else(Item %in% special_items,
        Name_biomass_primary, Name_biomass
      )
    )

  # Join with FM to DM conversion factors using Biomass_match, then calculate
  # Dry Matter production
  prod_grazed_no_seeds_dm <- grazed_no_seeds_primary |>
    dplyr::left_join(
      biomass_coefs |> dplyr::select(
        Name_biomass, Product_kgDM_kgFM, Residue_kgDM_kgFM
      ),
      by = c("Biomass_match" = "Name_biomass")
    ) |>
    dplyr::mutate(
      conversion_dm = dplyr::if_else(
        prod_type %in% c(
          "Residue", "Grass"
        ), Residue_kgDM_kgFM, Product_kgDM_kgFM
      ),
      production_dm = production_fm * conversion_dm
    )


  # Join with DM to N conversion factors using Biomass_match, then calculate
  # N production
  prod_grazed_no_seeds_n <- prod_grazed_no_seeds_dm |>
    dplyr::left_join(
      biomass_coefs |>
        dplyr::select(Name_biomass, Product_kgN_kgDM, Residue_kgN_kgDM),
      by = c("Biomass_match" = "Name_biomass")
    ) |>
    dplyr::mutate(
      conversion_n = dplyr::if_else(
        prod_type %in% c(
          "Residue", "Grass"
        ), Residue_kgN_kgDM, Product_kgN_kgDM
      ),
      production_n = production_dm * conversion_n
    ) |>
    # Remove intermediate columns and rename Biomass_match to Name_biomass
    dplyr::select(-Name_biomass) |>
    dplyr::select(
      Year, Province_name, Item, Box, LandUse, Irrig_cat,
      Irrig_type, prod_type, production_n
    ) |>
    dplyr::filter(!(is.na(Item) & production_n == 0)) |>
    dplyr::group_by(
      Year, Province_name, Item, Box, LandUse, Irrig_cat,
      Irrig_type, prod_type
    ) |>
    dplyr::summarise(
      production_n = sum(production_n, na.rm = TRUE),
      .groups = "drop"
    )

  # Summarize total Dry Matter and Nitrogen production per Item, Year,
  # Province, and Box
  grafs_prod_item <- prod_grazed_no_seeds_n |>
    dplyr::group_by(
      Year, Province_name, Item, Box, LandUse,
      Irrig_cat, Irrig_type, prod_type, production_n
    ) |>
    dplyr::summarise(
      production_n = sum(production_n, na.rm = TRUE),
      .groups = "drop"
    )

  grafs_prod_item
}

#' @title Consumption (Destinies) ---------------------------------------------
#'
#' @description Intake Livestock: sum all data (FM_Mg) for the same Year,
#' Province_name, Item.
#' Comment!!! Feed from all animals are summed together, also from pets.
#' Do they have to be assigned to humans?
#'
#' @param feed_intake A dataframe with feed intake data in FM.
#'
#' @return A dataframe with the total FM_Mg per year, province, and item.
#' @keywords internal
#' @noRd
.adding_feed <- function(
  feed_intake
) {
  feed_intake <- feed_intake |>
    dplyr::select(Year, Province_name, Item, FM_Mg) |>
    dplyr::group_by(Year, Province_name, Item) |>
    dplyr::summarise(FM_Mg_total = sum(FM_Mg, na.rm = TRUE), .groups = "drop")

  feed_intake
}

#' @title Population
#' @description Use column Pop_Mpeop_yg. Calculate the share of population
#' (population in each province divided through whole population in
#' Spain to get the share).
#'
#' @param population_share A dataframe with population data.
#'
#' @return A dataframe including population shares.
#' @keywords internal
#' @noRd
.calculate_population_share <- function(
  population_share
) {
  population_share <- population_share |>
    dplyr::select(Year, Province_name, Pop_Mpeop_yg) |>
    dplyr::group_by(Year) |>
    dplyr::mutate(
      Total_pop_spain = sum(Pop_Mpeop_yg, na.rm = TRUE),
      Pop_share = Pop_Mpeop_yg / Total_pop_spain
    ) |>
    dplyr::ungroup() |>
    dplyr::select(
      Year, Province_name, Pop_Mpeop_yg, Pop_share
    )

  population_share
}

#' @title Food -----------------------------------------------------------------
#' @description Sum all Elements for food and multiply with population share
#'
#' @param pie_full_destinies_fm A dataframe containing domestic supply food.
#' @param population_share A dataframe containing population share by province.
#'
#' @return A dataframe including food consumption per province and item.
#' @keywords internal
#' @noRd
.adding_food <- function(
  pie_full_destinies_fm,
  population_share
) {
  total_food <- pie_full_destinies_fm |>
    dplyr::filter(Destiny == "Food", Element == "Domestic_supply") |>
    dplyr::group_by(Year, Item) |>
    dplyr::summarise(
      Total_Food_value = sum(Value_destiny, na.rm = TRUE),
      .groups = "drop"
    )

  food_with_share <- dplyr::left_join(
    total_food,
    population_share,
    by = "Year",
    relationship = "many-to-many"
  ) |>
    dplyr::mutate(Food_Mg = Pop_share * Total_Food_value) |>
    dplyr::select(Year, Province_name, Item, Food_Mg)

  food_with_share
}

#' @title Other_uses -----------------------------------------------------------
#' @description Sum all elements for Other_uses and multiply with pop share.
#'
#' @param pie_full_destinies_fm A dataframe containing domestic supply.
#' other uses.
#' @param population_share A dataframe containing population share by province.
#'
#' @return A dataframe including other uses per province and item.
#' @keywords internal
#' @noRd
.adding_other_uses <- function(
  pie_full_destinies_fm,
  population_share
) {
  other_uses_with_share <- pie_full_destinies_fm |>
    dplyr::filter(Destiny == "Other_uses", Element == "Domestic_supply") |>
    dplyr::group_by(Year, Item) |>
    dplyr::summarise(
      Total_OtherUses_value = sum(Value_destiny, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::left_join(population_share, by = "Year") |>
    dplyr::mutate(OtherUses_Mg = Pop_share * Total_OtherUses_value) |>
    dplyr::select(Year, Province_name, Item, OtherUses_Mg)

  other_uses_with_share
}

#' @title Combine all destinies ------------------------------------------------
#' @description Merges food, feed, and other uses into one dataset.
#'
#' @param grafs_prod_item Dataframe production data for items.
#' @param feed_intake Feed intake values per province and item.
#' @param food_with_share Food values per province and item.
#' @param other_uses_with_share Other uses per province and item.
#'
#' @return A combined dataframe with food, feed, and other uses.
#' @keywords internal
#' @noRd
.combine_destinies <- function(
  grafs_prod_item,
  feed_intake,
  food_with_share,
  other_uses_with_share
) {
  grafs_prod_item_sum <- grafs_prod_item |>
    dplyr::select(Year, Province_name, Item, Box, production_n) |>
    dplyr::group_by(Year, Province_name, Item, Box) |>
    dplyr::summarise(production_n = sum(
      production_n,
      na.rm = TRUE
    ), .groups = "drop")

  grafs_prod_item_combined <- grafs_prod_item_sum |>
    dplyr::full_join(food_with_share |> dplyr::rename(Food_MgFM = Food_Mg),
      by = c("Year", "Province_name", "Item")
    ) |>
    dplyr::full_join(
      other_uses_with_share |>
        dplyr::rename(OtherUses_MgFM = OtherUses_Mg),
      by = c("Year", "Province_name", "Item")
    ) |>
    dplyr::full_join(feed_intake |> dplyr::rename(Feed_MgFM = FM_Mg_total),
      by = c("Year", "Province_name", "Item")
    )

  grafs_prod_item_combined
}

#' @title Finalizing data
#' @description Final merging of Item and Name_biomass and converting FM to DM,
#' and DM to N.
#'
#' @param grafs_prod_item_combined Dataframe with FM values for food, feed,
#' and other uses.
#' @param codes_coefs_items_full Dataframe linking items to biomass names.
#' @param biomass_coefs Dataframe including conversion factors
#' (DM/FM and N/DM).
#'
#' @return A dataframe with food, feed, and other uses in MgN.
#' @keywords internal
#' @noRd
.convert_to_items_n <- function(
  grafs_prod_item_combined,
  codes_coefs_items_full,
  biomass_coefs
) {
  grafs_prod_item_n <- grafs_prod_item_combined |>
    dplyr::left_join(
      codes_coefs_items_full |> dplyr::select(item, Name_biomass),
      by = c("Item" = "item")
    ) |>
    dplyr::mutate(
      prod_type = dplyr::case_when(
        Name_biomass %in% c("Grass", "Holm oak forest", "Fallow") ~ "Grass",
        Name_biomass == "Average wood" ~ "Residue",
        TRUE ~ "Product"
      )
    ) |>
    dplyr::left_join(
      biomass_coefs |> dplyr::select(
        Name_biomass,
        Product_kgDM_kgFM, Product_kgN_kgDM,
        Residue_kgDM_kgFM, Residue_kgN_kgDM
      ),
      by = "Name_biomass"
    ) |>
    dplyr::mutate(
      food_mg_dm = Food_MgFM * dplyr::if_else(
        prod_type == "Residue", Residue_kgDM_kgFM, Product_kgDM_kgFM
      ),
      other_uses_mg_dm = OtherUses_MgFM * dplyr::if_else(
        prod_type == "Residue", Residue_kgDM_kgFM, Product_kgDM_kgFM
      ),
      feed_mg_dm = Feed_MgFM * dplyr::if_else(
        prod_type == "Residue", Residue_kgDM_kgFM, Product_kgDM_kgFM
      ),
      food = food_mg_dm * dplyr::if_else(
        prod_type == "Residue", Residue_kgN_kgDM, Product_kgN_kgDM
      ),
      other_uses = other_uses_mg_dm * dplyr::if_else(
        prod_type == "Residue", Residue_kgN_kgDM, Product_kgN_kgDM
      ),
      feed = feed_mg_dm * dplyr::if_else(
        prod_type == "Residue", Residue_kgN_kgDM, Product_kgN_kgDM
      )
    ) |>
    dplyr::select(
      Year, Province_name, Item, Name_biomass, prod_type, Box, production_n,
      food, other_uses, feed
    )

  grafs_prod_item_n
}


#' @title Consumption and Trade
#' @description Calculation of consumption by destiny and trade
#' (export, import).
#'
#' @param grafs_prod_item_n A dataframe with N values (MgN) by destiny.
#'
#' @return A dataframe with consumption, exports, and imports in MgN.
#' @keywords internal
#' @noRd
.calculate_trade <- function(
  grafs_prod_item_n
) {
  grafs_prod_item_trade <- grafs_prod_item_n |>
    dplyr::group_by(Year, Province_name, Item, Name_biomass, Box) |>
    dplyr::mutate(
      consumption = rowSums(cbind(food, other_uses, feed),
        na.rm = TRUE
      ),
      production_n_tmp = tidyr::replace_na(production_n, 0),
      net_trade = production_n_tmp - consumption,
      export = ifelse(net_trade > 0, net_trade, 0),
      import = ifelse(net_trade < 0, -net_trade, 0)
    ) |>
    dplyr::select(-production_n_tmp) |>
    dplyr::ungroup()

  grafs_prod_item_trade
}

#' @title Finalize production and destiny output -------------------------------
#' @description Fills in missing box categories and transforms data into a
#' long format.
#'
#' @param grafs_prod_item_trade A dataframe containing N trade data.
#' @param codes_coefs_items_full A dataframe used to assign items to biomasses.
#'
#' @return A final dataframe with N data by year, province, box, destiny.
#' @keywords internal
#' @noRd
.finalize_prod_destiny <- function(
  grafs_prod_item_trade,
  codes_coefs_items_full
) {
  grafs_prod_destiny <- grafs_prod_item_trade |>
    dplyr::left_join(
      dplyr::select(codes_coefs_items_full, item, group),
      by = c("Item" = "item")
    ) |>
    dplyr::mutate(
      Box = dplyr::case_when(
        Item == "Acorns" ~ "Semi_natural_agroecosystems",
        is.na(Box) & Item == "Fallow" ~ "Cropland",
        is.na(Box) & group %in% c(
          "Crop products", "Primary crops",
          "crop residue"
        ) ~ "Cropland",
        is.na(Box) & group %in% c("Livestock products", "Livestock")
        ~ "Livestock",
        is.na(Box) & group %in% c("Additives", "Fish") ~ group,
        TRUE ~ Box
      )
    ) |>
    dplyr::select(-group)

  grafs_prod_destiny_final <- grafs_prod_destiny |>
    tidyr::pivot_longer(
      cols = c(food, other_uses, feed, export, import),
      names_to = "Destiny",
      values_to = "MgN"
    ) |>
    dplyr::mutate(
      Destiny = dplyr::recode(Destiny,
        food = "Food",
        other_uses = "Other_uses",
        feed = "Feed",
        export = "Export",
        import = "Import"
      )
    ) |>
    dplyr::select(
      Year, Province_name, Item, Box, Destiny, MgN
    )

  grafs_prod_destiny_final
}
