#' @title GRAFS production and destiny
#'
#' @description
#' This code is creating a dataset, based on N production data of crops and
#' livestock, which represents the following destinies:
#' food, feed, other uses, exports, imports.
#' This is the base of the GRAFS model.
#' The dataset contains data in MgN for each year, province, item, and box
#' (cropland, semi natural agroecosystems, livestock, fish, additives).
#' Processed items, residues, woody crops, grazed weeds are taken into account.
#' Seeds were subtracted from crop production.
#'
#' @return
#' A tibble with N data for each Year, Province, Item, Box, and Destiny
#'
#' @export
.create_prod_and_destiny_grafs <- function() {
  data <- .load_data()
  biomass_item_merged <- .merge_items_biomass(
    data$crop_area_npp_ygpit_all,
    data$npp_ygpit_csv, data$codes_coefs
  )
  data$crop_area_npp_ygpit_all <- biomass_item_merged$crop_area_npp_merged
  data$npp_ygpit_csv <- biomass_item_merged$npp_ygpit_merged
  production_crops_residues <-
    .summarise_crops_residues(data$crop_area_npp_ygpitr_no_fallow)
  grazed_data_added <-
    .aggregate_grazed_cropland(
      biomass_item_merged$npp_ygpit_merged,
      production_crops_residues
    )
  semi_natural_systems_data <-
    .aggregate_seminatural_system(data$npp_ygpit_csv)
  livestock_data <- .prepare_livestock_production(data$livestock_prod_ygps)
  prod_combined_boxes <- .combine_production_boxes(
    grazed_data_added,
    semi_natural_systems_data, livestock_data
  )
  seeds_removed <- .remove_seeds_from_system(
    data$crop_area_npp_ygpit_all,
    data$pie_full_destinies_fm, prod_combined_boxes
  )
  grass_wood_added <- .adding_grass_wood(seeds_removed)
  prepared_processed_data <- .prepare_processed_data(data$processed_prov_fixed)
  prepared_prod_data <- .prepare_prod_data(
    grass_wood_added,
    prepared_processed_data, data$codes_coefs_items_full
  )
  converted_data_fm_dm_n <- .convert_fm_dm_n(
    prepared_prod_data,
    data$biomass_coefs
  )
  feed_data <- .adding_feed(data$feed_intake)
  population_share <- .calculate_population_share(data$population_share)
  food_data <- .adding_food(data$pie_full_destinies_fm, population_share)
  other_uses_data <- .adding_other_uses(
    data$pie_full_destinies_fm,
    population_share
  )
  combined_destinies <- .combine_destinies(
    converted_data_fm_dm_n,
    feed_data, food_data, other_uses_data
  )
  converted_items_n <- .convert_to_items_n(
    combined_destinies,
    data$codes_coefs_items_full, data$biomass_coefs
  )
  trade <- .calculate_trade(converted_items_n)
  final_data <- .finalize_prod_destiny(trade, data$codes_coefs_items_full)


  list(
    data = data,
    biomass_item_merged = biomass_item_merged,
    production_crops_residues = production_crops_residues,
    grazed_data_added = grazed_data_added,
    semi_natural_systems_data = semi_natural_systems_data,
    prod_combined_boxes = prod_combined_boxes,
    seeds_removed = seeds_removed,
    grass_wood_added = grass_wood_added,
    prepared_processed_data = prepared_processed_data,
    converted_data_fm_dm_n = converted_data_fm_dm_n,
    feed_data = feed_data,
    population_share = population_share,
    food_data = food_data,
    other_uses_data = other_uses_data,
    combined_destinies = combined_destinies,
    converted_items_n = converted_items_n,
    trade = trade,
    final_data = final_data
  )
}


# Load all required datasets from input directory ---
.load_data <- function() {
  npp_ygpit_csv <- readr::read_csv(get_file_path("npp_ygpit"))
  feed_avail_all <- readRDS(get_file_path("feed_avail_all"))
  crop_area_npp_ygpitr_no_fallow <-
    readRDS(get_file_path("crop_area_npp_ygpitr_no_fallow"))
  crop_area_npp_ygpit_all <- readRDS(get_file_path("crop_area_npp_ygpit_all"))
  pie_full_destinies_fm <-
    readr::read_csv(get_file_path("pie_full_destinies_fm"))
  feed_intake <- readr::read_csv(get_file_path("intake_ygiac"))
  population_share <- readr::read_csv(get_file_path("population_yg"))
  n_excretion_ygs <- readRDS(get_file_path("n_excretion_ygs"))
  livestock_prod_ygps <- readr::read_csv(get_file_path("livestock_prod_ygps"))

  codes_coefs <-
    readxl::read_excel(get_file_path("codes_coefs"), sheet = "Names_biomass_CB")
  codes_coefs_items_full <- readxl::read_excel(get_file_path("codes_coefs"),
    sheet = "items_full"
  )
  biomass_coefs <- readxl::read_excel(get_file_path("biomass_coefs"), skip = 1)
  processed_prov_fixed <-
    readxl::read_excel(get_file_path("processed_prov_fixed"),
      sheet = "ProcessedItems_biomass"
    )

  data_list <- list(
    npp_ygpit_csv = npp_ygpit_csv,
    feed_avail_all = feed_avail_all,
    crop_area_npp_ygpitr_no_fallow = crop_area_npp_ygpitr_no_fallow,
    crop_area_npp_ygpit_all = crop_area_npp_ygpit_all,
    pie_full_destinies_fm = pie_full_destinies_fm,
    feed_intake = feed_intake,
    population_share = population_share,
    n_excretion_ygs = n_excretion_ygs,
    livestock_prod_ygps = livestock_prod_ygps,
    codes_coefs = codes_coefs,
    codes_coefs_items_full = codes_coefs_items_full,
    biomass_coefs = biomass_coefs,
    processed_prov_fixed = processed_prov_fixed
  )

  data_list
}

# Production of Cropland, Livestock, and Semi natural agroecosystems ---------
# Merge items with biomasses
# @keywords internal
.merge_items_biomass <- function(crop_area_npp_ygpit_all, npp_ygpit_csv,
                                 codes_coefs) {
  crop_area_npp_merged <- crop_area_npp_ygpit_all |>
    dplyr::left_join(
      codes_coefs |>
        dplyr::select(Name_biomass, Item),
      by = "Name_biomass"
    )

  npp_ygpit_merged <- npp_ygpit_csv |>
    dplyr::left_join(
      codes_coefs |>
        dplyr::select(Name_biomass, Item),
      by = "Name_biomass"
    )

  list(
    crop_area_npp_merged = crop_area_npp_merged,
    npp_ygpit_merged = npp_ygpit_merged
  )
}

# Crops Production and Residues ----------------------------------------------
# @keywords internal
.summarise_crops_residues <- function(crop_area_npp_ygpitr_no_fallow) {
  crop_area_npp_prod_residue <- crop_area_npp_ygpitr_no_fallow |>
    dplyr::select(
      Year,
      Province_name,
      Name_biomass,
      Prod_ygpit_Mg,
      Product_residue,
      Item
    ) |>
    dplyr::group_by(Year, Province_name, Name_biomass, Item, Product_residue) |>
    dplyr::summarise(
      Total_Mg = sum(as.numeric(Prod_ygpit_Mg), na.rm = TRUE),
      .groups = "drop"
    ) |>
    tidyr::pivot_wider(
      names_from = Product_residue,
      values_from = Total_Mg
    ) |>
    dplyr::mutate(
      Prod_Residue_Product_Mg = dplyr::coalesce(Product, 0) +
        dplyr::coalesce(Residue, 0),
      Box = "Cropland"
    ) |>
    dplyr::select(
      Year, Province_name, Name_biomass, Item, Prod_Residue_Product_Mg, Box
    )

  crop_area_npp_prod_residue
}


# Combining crops, residues, feed (grass, fallow) production -----------------
# @keywords internal
.aggregate_grazed_cropland <- function(npp_ygpit_merged,
                                       crop_area_npp_prod_residue) {
  grazed_data <- npp_ygpit_merged |>
    dplyr::filter(
      LandUse == "Cropland",
      !(Item == "Fallow" | Name_biomass == "Fallow")
    ) |>
    dplyr::select(Year, Province_name, GrazedWeeds_MgDM, Name_biomass, Item) |>
    dplyr::group_by(Year, Province_name, Name_biomass, Item) |>
    dplyr::summarise(
      GrazedWeeds_MgDM = sum(GrazedWeeds_MgDM, na.rm = TRUE),
      .groups = "drop"
    )

  # Merge `grazed_data` with `crop_area_npp_prod_residue`
  prod_residue_grass <- crop_area_npp_prod_residue |>
    dplyr::left_join(grazed_data, by = c(
      "Year", "Province_name", "Item",
      "Name_biomass"
    ))

  # Add missing Production data from NPP (Fallow)
  fallow_data <- npp_ygpit_merged |>
    dplyr::filter(
      LandUse == "Cropland",
      Item == "Fallow" | Name_biomass == "Fallow"
    ) |>
    dplyr::group_by(Year, Province_name, Name_biomass, Item) |>
    dplyr::summarise(
      GrazedWeeds_MgDM = sum(GrazedWeeds_MgDM, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      Prod_Residue_Product_Mg = 0,
      Box = "Cropland"
    ) |>
    dplyr::select(
      Year, Province_name, Name_biomass, Item,
      Prod_Residue_Product_Mg, GrazedWeeds_MgDM, Box
    )

  crop_area_npp_residue_grass <-
    dplyr::bind_rows(prod_residue_grass, fallow_data) |>
    dplyr::arrange(Year, Province_name, Name_biomass, Item)

  crops_residues_grazed <- crop_area_npp_residue_grass |>
    dplyr::mutate(across(
      c(Prod_Residue_Product_Mg, GrazedWeeds_MgDM),
      ~ tidyr::replace_na(., 0)
    )) |>
    dplyr::select(-Box, everything(), Box)

  crops_residues_grazed
}

# Semi_natural_agroecosystems: Aggregate Grazed Weeds and Production plus
# Used Residues from Forest, Shrubland, Dehesa, Other
#
# @param npp_ygpit_merged A data frame containing biomass data
#
# @return A tibble filtered and transformed with selected columns for
# semi-natural agroecosystems.
# @keywords internal
.aggregate_seminatural_system <- function(npp_ygpit_merged) {
  semi_natural_agroecosystems <- npp_ygpit_merged |>
    dplyr::ungroup() |>
    dplyr::filter(LandUse != "Cropland") |>
    dplyr::mutate(Box = "Semi_natural_agroecosystems") |>
    dplyr::select(
      Year, Province_name, Name_biomass, GrazedWeeds_MgDM,
      Prod_ygpit_Mg, Used_Residue_MgFM, Box, Item
    )

  semi_natural_agroecosystems
}

# Livestock Production -------------------------------------------------------
# @param livestock_prod_ygps A data frame including livestock production data.
# @keywords internal
.prepare_livestock_production <- function(livestock_prod_ygps) {
  livestock <- livestock_prod_ygps |>
    dplyr::select(
      Year,
      Province_name,
      Item,
      Name_biomass,
      Prod_Mg
    ) |>
    dplyr::mutate(
      Box = "Livestock"
    )

  livestock
}

# Combine Cropland, Semi_natural_agroecosystems and Livestock ----------------
# @keywords internal
.combine_production_boxes <- function(crops_residues_grazed,
                                      semi_natural_agroecosystems, livestock) {
  grafs_prod_combined <- dplyr::bind_rows(
    crops_residues_grazed |>
      dplyr::select(
        Year, Province_name, Name_biomass, Item,
        Prod_Residue_Product_Mg, GrazedWeeds_MgDM, Box
      ),
    semi_natural_agroecosystems |>
      dplyr::select(Year, Province_name, Name_biomass, Item,
        Prod_Residue_Product_Mg = Prod_ygpit_Mg, Used_Residue_MgFM,
        GrazedWeeds_MgDM, Box
      ),
    livestock |>
      dplyr::select(Year, Province_name, Name_biomass, Item,
        Prod_Residue_Product_Mg = Prod_Mg, Box
      )
  )

  grafs_prod_combined
}

# Seed production per province, based on the national seed share per Area ----
# @keywords internal
.remove_seeds_from_system <- function(crop_area_npp_ygpit_all,
                                      pie_full_destinies_fm,
                                      grafs_prod_combined) {
  seeds_substracted <- crop_area_npp_ygpit_all |>
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
          crop_area_npp_ygpit_all |>
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
      seeds_substracted |>
        dplyr::select(Year, Province_name, Item, Seeds_used_MgFM),
      by = c("Year", "Province_name", "Item")
    ) |>
    dplyr::mutate(
      Prod_Residue_Product_Mg = Prod_Residue_Product_Mg -
        dplyr::coalesce(Seeds_used_MgFM, 0)
    ) |>
    dplyr::select(-Seeds_used_MgFM)

  grafs_prod_combined_no_seeds
}

# Structuring dataset (GrazedWeeds und Used_Residues in ProductionFM) --------
# Rename Prod_Residue_Product_Mg to Production_FM and replace Production_FM
# with GrazedWeeds_MgDM (for Fallow)
# @keywords internal
.adding_grass_wood <- function(grafs_prod_combined_no_seeds, biomass_coefs) {
  grafs_prod_structured <- grafs_prod_combined_no_seeds |>
    dplyr::rename(Production_FM = Prod_Residue_Product_Mg) |>
    dplyr::mutate(
      Production_FM = dplyr::if_else(
        Name_biomass == "Fallow" | Item == "Fallow",
        GrazedWeeds_MgDM,
        Production_FM
      )
    )

  # Create 'Grass' rows for Holm oak using GrazedWeeds_MgDM
  holm_oak_grass <- grafs_prod_structured |>
    dplyr::filter(Name_biomass == "Holm oak") |>
    dplyr::distinct(Year, Province_name, Name_biomass, Box, GrazedWeeds_MgDM) |>
    dplyr::mutate(
      Item = "Grass",
      Production_FM = GrazedWeeds_MgDM
    ) |>
    dplyr::select(Year, Province_name, Name_biomass, Item, Production_FM, Box)

  # Create 'Firewood' rows for Holm oak using Used_Residue_MgFM
  holm_oak_firewood <- grafs_prod_structured |>
    dplyr::filter(Name_biomass == "Holm oak") |>
    dplyr::distinct(
      Year, Province_name, Name_biomass, Box,
      Used_Residue_MgFM
    ) |>
    dplyr::mutate(
      Item = "Firewood",
      Production_FM = Used_Residue_MgFM
    ) |>
    dplyr::select(Year, Province_name, Name_biomass, Item, Production_FM, Box)

  # Combine the two into one dataframe
  holm_oak_extra <- dplyr::bind_rows(holm_oak_grass, holm_oak_firewood)

  # Create Firewood rows for other biomass types using Used_Residue_MgFM
  firewood_extra <- grafs_prod_structured |>
    dplyr::filter(Name_biomass %in% c(
      "Conifers", "Holm oak forest",
      "Mediterranean shrubland"
    )) |>
    dplyr::filter(!is.na(Used_Residue_MgFM) & Used_Residue_MgFM > 0) |>
    dplyr::mutate(
      Item = "Firewood",
      Production_FM = Used_Residue_MgFM
    ) |>
    dplyr::select(Year, Province_name, Name_biomass, Item, Production_FM, Box)

  # Create Grass rows for other biomass types using GrazedWeeds_MgDM
  grass_extra <- grafs_prod_structured |>
    dplyr::filter(Name_biomass %in% c(
      "Conifers", "Holm oak forest",
      "Mediterranean shrubland"
    )) |>
    dplyr::filter(!is.na(GrazedWeeds_MgDM) & GrazedWeeds_MgDM > 0) |>
    dplyr::mutate(
      Item = "Grass",
      Production_FM = GrazedWeeds_MgDM
    ) |>
    dplyr::select(Year, Province_name, Name_biomass, Item, Production_FM, Box)

  # Combine all new rows with existing data
  grafs_prod_added_grass_wood <- grafs_prod_structured |>
    dplyr::select(
      Year, Province_name, Name_biomass, Item,
      Production_FM, Box
    ) |>
    dplyr::bind_rows(holm_oak_extra, firewood_extra, grass_extra) |>
    dplyr::filter(!is.na(Production_FM)) |>
    # Convert only Grass rows from DM to FM using 20% coefficient from
    # biomass_coefs
    dplyr::mutate(
      Production_FM = dplyr::if_else(
        Item == "Grass" & !is.na(Production_FM),
        Production_FM / 0.2,
        Production_FM
      ),
      Item = dplyr::if_else(Item == "Grass", "Grassland", Item),
      Name_biomass = dplyr::if_else(Item == "Grassland", "Grass", Name_biomass)
    ) |>
    # Aggregate Grassland rows across same Year/Province_name/Item/Box
    dplyr::group_by(Year, Province_name, Name_biomass, Item, Box) |>
    dplyr::summarise(
      Production_FM = sum(Production_FM, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(Year, Province_name, Name_biomass, Item)

  grafs_prod_added_grass_wood
}

# Processed Items ------------------------------------------------------------
# Summarise processed items by Year, Province, Biomass, Item, and ProcessedItem
# @keywords internal
.prepare_processed_data <- function(processed_prov_fixed) {
  processed_data <- processed_prov_fixed |>
    dplyr::group_by(Year, Province_name, Name_biomass, Item, ProcessedItem) |>
    dplyr::summarise(
      ProcessedItem_amount = sum(ProcessedItem_amount, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      Item = ProcessedItem,
      Production_FM = ProcessedItem_amount,
      Box = "Cropland"
    ) |>
    dplyr::select(Year, Province_name, Name_biomass, Item, Box, Production_FM)

  processed_data
}

# Match structure of grafs_prod_combined_no_seeds ----------------------------
# @keywords internal
.prepare_prod_data <- function(grafs_prod_added_grass_wood,
                               processed_data, codes_coefs_items_full) {
  added_grass_wood_prepared <- grafs_prod_added_grass_wood |>
    dplyr::select(
      Year, Province_name, Name_biomass, Item, Box,
      Production_FM
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

# Convert Fresh Matter (FM) to Dry Matter (DM) and finally to Nitrogen (N) ---
# Define a list of special items that require using the primary biomass name
# for selecting conversion coefficients
# @keywords internal
.convert_fm_dm_n <- function(added_grass_wood_merged,
                             biomass_coefs) {
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
      biomass_coefs |> dplyr::select(Name_biomass, Product_kgDM_kgFM),
      by = c("Biomass_match" = "Name_biomass")
    ) |>
    dplyr::mutate(
      Production_DM = Production_FM * Product_kgDM_kgFM
    )

  # Join with DM to N conversion factors using Biomass_match, then calculate
  # Nitrogen production
  prod_grazed_no_seeds_n <- prod_grazed_no_seeds_dm |>
    dplyr::left_join(
      biomass_coefs |>
        dplyr::select(Name_biomass, Product_kgN_kgDM),
      by = c("Biomass_match" = "Name_biomass")
    ) |>
    dplyr::mutate(
      Production_N = Production_DM * Product_kgN_kgDM
    ) |>
    # Remove intermediate columns and rename Biomass_match to Name_biomass
    dplyr::select(
      -Production_FM, -Product_kgDM_kgFM, -Product_kgN_kgDM,
      -Name_biomass
    ) |>
    dplyr::rename(Name_biomass = Biomass_match)

  # Summarize total Dry Matter and Nitrogen production per Item, Year,
  # Province, and Box
  grafs_prod_item <- prod_grazed_no_seeds_n |>
    dplyr::group_by(Year, Province_name, Item, Box) |>
    dplyr::summarise(
      Production_DM = sum(Production_DM, na.rm = TRUE),
      Production_N = sum(Production_N, na.rm = TRUE),
      .groups = "drop"
    )

  grafs_prod_item
}

# Consumption (Destinies) ----------------------------------------------------
# Intake Livestock: sum all values (FM_Mg) for the same Year,
# Province_name and Item
# Comment!!! Feed from all animals are summed together, also from pets.
# Do they have to be assigned to humans?
# @keywords internal
.adding_feed <- function(feed_intake) {
  feed_intake <- feed_intake |>
    dplyr::select(Year, Province_name, Item, FM_Mg) |>
    dplyr::group_by(Year, Province_name, Item) |>
    dplyr::summarise(FM_Mg_total = sum(FM_Mg, na.rm = TRUE), .groups = "drop")

  feed_intake
}

# Popoulation: use column Pop_Mpeop_yg. Calculate the share of population ----
# (population in each province divided through whole population in
# Spain to get the share)
# @keywords internal
.calculate_population_share <- function(population_share) {
  population_share <- population_share |>
    dplyr::select(Year, Province_name, Pop_Mpeop_yg) |>
    dplyr::group_by(Year) |>
    dplyr::mutate(
      Total_pop_spain = sum(Pop_Mpeop_yg, na.rm = TRUE),
      Pop_share = Pop_Mpeop_yg / Total_pop_spain
    ) |>
    dplyr::ungroup() |>
    dplyr::select(Year, Province_name, Pop_Mpeop_yg, Pop_share)

  population_share
}

# Food -----------------------------------------------------------------------
# Sum all Elements for Food and multiply with population share
# @keywords internal
.adding_food <- function(pie_full_destinies_fm, population_share) {
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

# Other_uses -----------------------------------------------------------------
# Sum all Elements for Other_uses and multiply with population share
# @keywords internal
.adding_other_uses <- function(pie_full_destinies_fm, population_share) {
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

# Putting all together -------------------------------------------------------
# @keywords internal
.combine_destinies <- function(grafs_prod_item, feed_intake, food_with_share,
                               other_uses_with_share) {
  grafs_prod_item_combined <- grafs_prod_item |>
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

# Converting Item and Name_biomass again and converting FM to DM, and DM to N
# @keywords internal
.convert_to_items_n <- function(grafs_prod_item_combined,
                                codes_coefs_items_full, biomass_coefs) {
  grafs_prod_item_n <- grafs_prod_item_combined |>
    dplyr::left_join(
      codes_coefs_items_full |> dplyr::select(item, Name_biomass),
      by = c("Item" = "item")
    ) |>
    dplyr::relocate(Name_biomass, .after = Item) |>
    # Convert the conversion factors into grafs_prod_item
    dplyr::left_join(
      biomass_coefs |> dplyr::select(
        Name_biomass, Product_kgDM_kgFM,
        Product_kgN_kgDM
      ),
      by = "Name_biomass"
    ) |>
    # Convert FM → DM → N for each use type
    dplyr::mutate(
      Food_MgDM = Food_MgFM * Product_kgDM_kgFM,
      OtherUses_MgDM = OtherUses_MgFM * Product_kgDM_kgFM,
      Feed_MgDM = Feed_MgFM * Product_kgDM_kgFM,
      Food_MgN = Food_MgDM * Product_kgN_kgDM,
      OtherUses_MgN = OtherUses_MgDM * Product_kgN_kgDM,
      Feed_MgN = Feed_MgDM * Product_kgN_kgDM
    ) |>
    dplyr::select(
      Year, Province_name, Item, Name_biomass, Box, Production_N,
      Food_MgN, OtherUses_MgN, Feed_MgN
    )

  grafs_prod_item_n
}

# Calculating Consumption and Trade ------------------------------------------
# @keywords internal
.calculate_trade <- function(grafs_prod_item_n) {
  grafs_prod_item_trade <- grafs_prod_item_n |>
    dplyr::group_by(Year, Province_name, Item, Name_biomass, Box) |>
    dplyr::mutate(
      Consumption_N = rowSums(cbind(Food_MgN, OtherUses_MgN, Feed_MgN),
        na.rm = TRUE
      ),
      Production_N_tmp = tidyr::replace_na(Production_N, 0),
      Net_trade = Production_N_tmp - Consumption_N,
      Export_MgN = ifelse(Net_trade > 0, Net_trade, 0),
      Import_MgN = ifelse(Net_trade < 0, -Net_trade, 0)
    ) |>
    dplyr::select(-Production_N_tmp) |>
    dplyr::ungroup()

  grafs_prod_item_trade
}

# Adding missing Boxes for Imports -------------------------------------------
# @keywords internal
.finalize_prod_destiny <- function(grafs_prod_item_trade,
                                   codes_coefs_items_full) {
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
      cols = c(Food_MgN, OtherUses_MgN, Feed_MgN, Export_MgN, Import_MgN),
      names_to = "Destiny",
      values_to = "MgN"
    ) |>
    dplyr::mutate(
      Destiny = dplyr::recode(Destiny,
        Food_MgN = "Food",
        OtherUses_MgN = "Other_uses",
        Feed_MgN = "Feed",
        Export_MgN = "Export",
        Import_MgN = "Import"
      )
    ) |>
    dplyr::select(Year, Province_name, Item, Box, Destiny, MgN)

  grafs_prod_destiny_final
}
