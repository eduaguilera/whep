#' @title GRAFS Nitrogen (N) production and their destinies
#'
#' @description
#' Provides N production of crops and livestock, categorized by their destinies:
#'  food, feed, other uses, exports, imports, which is the base of the
#'  GRAFS model. The dataset contains data in megagrams of N (MgN) for each
#'  year, province, item, and box (cropland, semi natural agroecosystems,
#'  livestock, fish, Agro-industry). Processed items, residues, woody crops,
#'  grazed weeds are taken into account.
#'
#' @return
#' A final tibble containing N production data by destiny.
#' It includes the following columns:
#'   - `year`: The year in which the recorded event occurred.
#'   - `province_name`: The Spanish province where the data is from.
#'   - `item`: The item which was produced, defined in `names_biomass_cb`.
#'   - `box`: One of the GRAFS model systems: cropland,
#'   Semi-natural agroecosystems, Livestock, Fish, or Agro-industry.
#'   - `destiny`: The use category of the nitrogen: Food, Feed, Other_uses,
#'   Export, or Import.
#'   - `MgN`: Nitrogen amount in megagrams (Mg).
#'
#' @export
create_prod_and_destiny_grafs <- function() {
  biomass_item_merged <- .merge_items_biomass(
    whep_read_file("npp_ygpit"),
    whep_read_file("codes_coefs")
  )

  production_crops_residues <- .summarise_crops_residues(
    whep_read_file("crop_area_npp_ygpitr_no_fallow")
  )

  crop_seminatural_data <- .aggregate_crop_seminatural(
    biomass_item_merged,
    production_crops_residues
  )

  livestock_data <- .prepare_livestock_production(
    whep_read_file("livestock_prod_ygps")
  )

  prod_combined_boxes <- .combine_production_boxes(
    crop_seminatural_data,
    livestock_data
  )

  seeds_removed <- .remove_seeds_from_system(
    biomass_item_merged,
    whep_read_file("pie_full_destinies_fm"),
    prod_combined_boxes
  )

  grass_wood_added <- .adding_grass_wood(seeds_removed)

  prepared_processed_data <- .prepare_processed_data(
    whep_read_file("processed_prov_fixed")
  )

  prepared_prod_data <- .prepare_prod_data(
    grass_wood_added,
    prepared_processed_data,
    whep_read_file("codes_coefs_items_full")
  )

  converted_data_fm_dm_n <- .convert_fm_dm_n(
    prepared_prod_data,
    whep_read_file("biomass_coefs")
  )

  feed_data <- .adding_feed(whep_read_file("intake_ygiac"))

  population_share <- .calculate_population_share(
    whep_read_file("population_yg")
  )

  food_other_uses_data <- .calculate_food_and_other_uses(
    whep_read_file("pie_full_destinies_fm"),
    population_share
  )

  combined_destinies <- .combine_destinies(
    converted_data_fm_dm_n,
    feed_data,
    food_other_uses_data
  )

  converted_items_n <- .convert_to_items_n(
    combined_destinies,
    whep_read_file("codes_coefs_items_full"),
    whep_read_file("biomass_coefs")
  )

  trade <- .calculate_trade(converted_items_n)

  .finalize_prod_destiny(
    trade,
    whep_read_file("codes_coefs_items_full")
  )
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
  npp_ygpit_csv |>
    dplyr::left_join(
      names_biomass_cb |> dplyr::select(Name_biomass, Item),
      by = "Name_biomass"
    )
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
.aggregate_crop_seminatural <- function(
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
        Year,
        Province_name,
        Name_biomass,
        Item,
        LandUse,
        Irrig_cat,
        Irrig_type,
        production_fm = GrazedWeeds_MgDM
      ) |>
      dplyr::mutate(prod_type = "Grass"),
    npp_ygpit_merged |>
      dplyr::filter(LandUse != "Cropland") |>
      dplyr::select(
        Year,
        Province_name,
        Name_biomass,
        Item,
        LandUse,
        Irrig_cat,
        Irrig_type,
        production_fm = Prod_ygpit_Mg
      ) |>
      dplyr::mutate(prod_type = "Product"),
    npp_ygpit_merged |>
      dplyr::filter(LandUse != "Cropland") |>
      dplyr::select(
        Year,
        Province_name,
        Name_biomass,
        Item,
        LandUse,
        Irrig_cat,
        Irrig_type,
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
      Year,
      Province_name,
      Item,
      Name_biomass,
      Prod_Mg
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

#' @title Structuring dataset (GrazedWeeds and Used_Residues in ProductionFM)
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
        prod_type == "Residue" &
          Box != "Cropland" &
          Name_biomass %in%
            c(
              "Holm oak forest",
              "Conifers",
              "Mediterranean shrubland"
            ) ~
          "Firewood",
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
      Year,
      Province_name,
      Name_biomass,
      Item,
      Box,
      LandUse,
      Irrig_cat,
      Irrig_type,
      prod_type
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
      Year,
      Province_name,
      Name_biomass,
      Item,
      Box,
      production_fm,
      prod_type
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
      Year,
      Province_name,
      Name_biomass,
      Item,
      Box,
      LandUse,
      Irrig_cat,
      Irrig_type,
      prod_type,
      production_fm
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
    dplyr::mutate(
      Name_biomass = dplyr::if_else(
        !is.na(Name_biomass),
        Name_biomass,
        Name_biomass_primary
      )
    ) |>
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
    "Nuts and products",
    "Vegetables, Other",
    "Fruits, Other",
    "Cereals, Other",
    "Pulses, Other and products"
  )

  # Add a column to choose the appropriate biomass name for matching
  # conversion factors
  grazed_no_seeds_primary <- added_grass_wood_merged |>
    dplyr::mutate(
      Biomass_match = dplyr::if_else(
        Item %in% special_items,
        Name_biomass_primary,
        Name_biomass
      )
    )

  # Join with FM to DM conversion factors using Biomass_match, then calculate
  # Dry Matter production
  prod_grazed_no_seeds_dm <- grazed_no_seeds_primary |>
    dplyr::left_join(
      biomass_coefs |>
        dplyr::select(
          Name_biomass,
          Product_kgDM_kgFM,
          Residue_kgDM_kgFM
        ),
      by = c("Biomass_match" = "Name_biomass")
    ) |>
    dplyr::mutate(
      conversion_dm = dplyr::if_else(
        prod_type %in%
          c(
            "Residue",
            "Grass"
          ),
        Residue_kgDM_kgFM,
        Product_kgDM_kgFM
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
        prod_type %in%
          c(
            "Residue",
            "Grass"
          ),
        Residue_kgN_kgDM,
        Product_kgN_kgDM
      ),
      production_n = production_dm * conversion_n
    ) |>
    # Remove intermediate columns and rename Biomass_match to Name_biomass
    dplyr::select(-Name_biomass) |>
    dplyr::select(
      Year,
      Province_name,
      Item,
      Box,
      LandUse,
      Irrig_cat,
      Irrig_type,
      prod_type,
      production_n
    ) |>
    dplyr::filter(!(is.na(Item) & production_n == 0)) |>
    dplyr::group_by(
      Year,
      Province_name,
      Item,
      Box,
      LandUse,
      Irrig_cat,
      Irrig_type,
      prod_type
    ) |>
    dplyr::summarise(
      production_n = sum(production_n, na.rm = TRUE),
      .groups = "drop"
    )

  # Summarize total Dry Matter and Nitrogen production per Item, Year,
  # Province, and Box
  grafs_prod_item <- prod_grazed_no_seeds_n |>
    dplyr::group_by(
      Year,
      Province_name,
      Item,
      Box,
      LandUse,
      Irrig_cat,
      Irrig_type,
      prod_type
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
    dplyr::group_by(Year, Province_name, Item) |>
    dplyr::summarise(feed = sum(FM_Mg, na.rm = TRUE), .groups = "drop")

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
      Year,
      Province_name,
      Pop_Mpeop_yg,
      Pop_share
    )

  population_share
}

#' @title Food and Other uses---------------------------------------------------
#' @description Sum all Elements for food and other uses and multiply by
#' population share
#'
#' @param pie_full_destinies_fm A dataframe containing domestic supply food and
#' other uses.
#' @param population_share A dataframe containing population share by province.
#'
#' @return A dataframe including food and other uses consumption per province
#' and item.
#' @keywords internal
#' @noRd
.calculate_food_and_other_uses <- function(
  pie_full_destinies_fm,
  population_share
) {
  total_food_other_uses <- pie_full_destinies_fm |>
    dplyr::filter(
      Destiny %in% c("Food", "Other_uses"), Element == "Domestic_supply"
    ) |>
    dplyr::group_by(Year, Item, Destiny) |>
    dplyr::summarise(
      Total_value = sum(Value_destiny, na.rm = TRUE),
      .groups = "drop"
    ) |>
    tidyr::pivot_wider(
      names_from = Destiny,
      values_from = Total_value
    )

  provincial_food_other_uses <- total_food_other_uses |>
    dplyr::left_join(
      population_share,
      by = "Year", relationship = "many-to-many"
    ) |>
    dplyr::mutate(
      Food = Pop_share * Food,
      Other_uses = Pop_share * Other_uses
    ) |>
    dplyr::rename(
      food = Food,
      other_uses = Other_uses
    ) |>
    dplyr::select(Year, Province_name, Item, food, other_uses)

  provincial_food_other_uses
}

#' @title Combine all destinies ------------------------------------------------
#' @description Merges food, feed, and other uses into one dataset.
#'
#' @param grafs_prod_item Dataframe production data for items.
#' @param feed_intake Feed intake values per province and item.
#' @param provincial_food_other_uses Food and Other uses per province and item.
#'
#' @return A combined dataframe with food, feed, and other uses.
#' @keywords internal
#' @noRd
.combine_destinies <- function(
  grafs_prod_item,
  feed_intake,
  provincial_food_other_uses
) {
  grafs_prod_item_sum <- grafs_prod_item |>
    dplyr::select(Year, Province_name, Item, Box, production_n) |>
    dplyr::group_by(Year, Province_name, Item, Box) |>
    dplyr::summarise(
      production_n = sum(
        production_n,
        na.rm = TRUE
      ),
      .groups = "drop"
    )

  grafs_prod_item_combined <- grafs_prod_item_sum |>
    dplyr::full_join(
      provincial_food_other_uses,
      by = c("Year", "Province_name", "Item")
    ) |>
    dplyr::full_join(
      feed_intake,
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
  grafs_prod_item_combined |>
    dplyr::left_join(
      codes_coefs_items_full |>
        dplyr::select(item, Name_biomass),
      by = c("Item" = "item")
    ) |>
    dplyr::mutate(
      prod_type = dplyr::case_when(
        Name_biomass %in% c("Grass", "Fallow") ~ "Grass",
        Name_biomass == "Average wood" ~ "Residue",
        TRUE ~ "Product"
      )
    ) |>
    tidyr::pivot_longer(
      cols = c(food, other_uses, feed),
      names_to = "destiny",
      values_to = "value_fm"
    ) |>
    dplyr::left_join(
      biomass_coefs |>
        dplyr::select(
          Name_biomass,
          Product_kgDM_kgFM,
          Product_kgN_kgDM,
          Residue_kgDM_kgFM,
          Residue_kgN_kgDM
        ),
      by = "Name_biomass"
    ) |>
    dplyr::mutate(
      n_value = dplyr::case_when(
        prod_type %in% c("Residue", "Grass")
        ~ value_fm * Residue_kgDM_kgFM * Residue_kgN_kgDM,
        prod_type == "Product" ~
          value_fm * Product_kgDM_kgFM * Product_kgN_kgDM,
        TRUE ~ NA_real_
      )
    ) |>
    tidyr::pivot_wider(
      names_from = destiny,
      values_from = n_value
    ) |>
    dplyr::select(
      Year,
      Province_name,
      Item,
      Name_biomass,
      prod_type,
      Box,
      production_n,
      food,
      other_uses,
      feed
    )
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
.calculate_trade <- function(grafs_prod_item_n) {
  grafs_prod_item_n |>
    dplyr::group_by(Year, Province_name, Item, Box) |>
    dplyr::summarise(
      food = sum(food, na.rm = TRUE),
      other_uses = sum(other_uses, na.rm = TRUE),
      feed = sum(feed, na.rm = TRUE),
      production_n = sum(production_n, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      consumption = food + other_uses + feed,
      net_trade = production_n - consumption,
      export = ifelse(net_trade > 0, net_trade, 0),
      import = ifelse(net_trade < 0, -net_trade, 0)
    ) |>
    dplyr::select(
      Year,
      Province_name,
      Item,
      Box,
      food,
      other_uses,
      feed,
      production_n,
      export,
      import
    )
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
  grafs_prod_destiny_final <- grafs_prod_item_trade |>
    dplyr::left_join(
      dplyr::select(codes_coefs_items_full, item, group),
      by = c("Item" = "item")
    ) |>
    dplyr::mutate(
      group = dplyr::recode(group, "Additives" = "Agro-industry"),
      Box = dplyr::case_when(
        Item == "Acorns" ~ "Semi_natural_agroecosystems",
        is.na(Box) & Item == "Fallow" ~ "Cropland",
        is.na(Box) &
          group %in%
            c(
              "Crop products",
              "Primary crops",
              "crop residue"
            ) ~
          "Cropland",
        is.na(Box) &
          group %in%
            c(
              "Livestock products",
              "Livestock"
            ) ~
          "Livestock",
        is.na(Box) &
          group %in%
            c(
              "Agro-industry",
              "Fish"
            ) ~
          group,
        TRUE ~ Box
      )
    ) |>
    dplyr::select(-group) |>
    tidyr::pivot_longer(
      cols = c(food, other_uses, feed, export, import),
      names_to = "Destiny",
      values_to = "MgN"
    ) |>
    dplyr::group_by(Year, Province_name, Item, Box, Destiny) |>
    dplyr::summarise(MgN = sum(MgN, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(
      Destiny = factor(
        Destiny,
        levels = c(
          "food",
          "other_uses",
          "feed",
          "export",
          "import"
        )
      )
    ) |>
    dplyr::mutate(
      Box_destiny = dplyr::case_when(
        Box == "Cropland" & Destiny == "export" ~ "crop_export",
        Box == "Livestock" & Destiny == "export" ~ "livestock_export",
        Box == "Livestock" & Destiny %in% c(
          "food", "other_uses"
        ) ~ "livestock_to_pop",
        Box == "Cropland" & Destiny %in% c(
          "food", "other_uses"
        ) ~ "crops_to_pop",
        Box == "Grass" & Destiny == "feed" ~ "grass_to_livestock",
        Box == "Cropland" & Destiny == "feed" ~ "crops_to_livestock",
        Box == "Fish" & Destiny %in% c(
          "food", "other_uses"
        ) ~ "fish_to_pop",
        Box == "Fish" & Destiny == "feed" ~ "fish_to_livestock",
        Box == "Agro-industry" & Destiny == "feed" ~ "additives_to_livestock",
        Box == "Agro-industry" & Destiny %in% c(
          "food", "other_uses"
        ) ~ "additives_to_pop",
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::arrange(
      Year,
      Province_name,
      Box,
      Item,
      Destiny
    ) |>
    dplyr::select(
      Year,
      Province_name,
      Item,
      Box,
      Destiny,
      Box_destiny,
      MgN
    )

  grafs_prod_destiny_final
}
