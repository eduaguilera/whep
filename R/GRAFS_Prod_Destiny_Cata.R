#' Create GRAFS Production and Destiny Outputs
#'
#' @description
#' Loads all relevant input data and processes production and destinies for the GRAFS model.
#'
#' @returns
#' A list containing all key data frames for further analysis or visualization.
#'
#' @export
create_production_and_destinies_grafs <- function(inputs_dir = "C:/PhD/GRAFS/Production Boxes/Final Files/Inputs") {
  data <- .load_data(inputs_dir)
  biomass_item_merged <- .merge_items_biomass(data$Crop_AreaNPP_ygpit_all, data$NPP_ygpit_csv, data$Codes_coefs)
  production_crops_residues <- .summarise_crops_residues(data$Crop_AreaNPP_ygpitr_NoFallow)
  grazed_data_added <- .aggregate_grazed_cropland(data$NPP_ygpit_csv, production_crops_residues)
  semi_natural_agroecosystems_data <- .aggregate_semi_natural_agroecosystems(data$NPP_ygpit_csv)
  livestock_data <- .prepare_livestock_production(data$Livestock_Prod_ygps)
  prod_combined_boxes <- .combine_production_boxes(grazed_data_added, semi_natural_agroecosystems_data, livestock_data)
  seeds_removed <- .remove_seeds_from_system(data$Crop_AreaNPP_ygpit_all, data$PIE_FullDestinies_FM, prod_combined_boxes)
  grass_wood_added <- .adding_grass_wood(seeds_removed)
  prepared_processed_data <- .prepare_processed_data(data$processed_prov_fixed)
  prepared_prod_data <- .prepare_prod_data(grass_wood_added, prepared_processed_data)
  converted_data_FM_DM_N <- .convert_FM_DM_N(data$Biomass_coefs, prepared_prod_data)
  feed_data <- .adding_feed(data$Feed_Intake)
  population_share <- .calculate_population_share(data$Population_share)
  food_data <- .adding_food(population_share, data$PIE_FullDestinies_FM)
  other_uses_data <- .adding_other_uses(population_share, data$PIE_FullDestinies_FM)
  combined_destinies <- .combine_destinies(converted_data_FM_DM_N, feed_data, food_data, other_uses_data)
  converted_items_N <- .convert_to_items_N(combined_destinies, data$Codes_coefs_items_full, data$Biomass_coefs)
  trade <- .calculate_trade(converted_items_N)
  final_data <- .finalize_prod_destiny(trade)


  return(list(
    data = data,
    biomass_item_merged = biomass_item_merged,
    production_crops_residues = production_crops_residues,
    grazed_data_added = grazed_data_added,
    semi_natural_agroecosystems_data = semi_natural_agroecosystems_data,
    prod_combined_boxes = prod_combined_boxes,
    seeds_removed = seeds_removed,
    grass_wood_added = grass_wood_added,
    prepared_processed_data = prepared_processed_data,
    converted_data_FM_DM_N = converted_data_FM_DM_N,
    feed_data = feed_data,
    population_share = population_share,
    food_data = food_data,
    other_uses_data = other_uses_data,
    combined_destinies = combined_destinies,
    converted_items_N = converted_items_N,
    trade = trade,
    final_data = final_data
  ))
}


#' Load all required datasets from input directory ---
.load_data <- function(inputs_dir) {
  NPP_ygpit_csv <- readr::read_csv(file.path(inputs_dir, "NPP_ygpit.csv.gz"))
  Feed_avail_all <- readRDS(file.path(inputs_dir, "Feed_avail_all.rds"))
  Crop_AreaNPP_ygpitr_NoFallow <- readRDS(file.path(inputs_dir, "Crop_AreaNPP_ygpitr_NoFallow.rds"))
  Crop_AreaNPP_ygpit_all <- readRDS(file.path(inputs_dir, "Crop_AreaNPP_ygpit_all.rds"))
  PIE_FullDestinies_FM <- readr::read_csv(file.path(inputs_dir, "PIE_FullDestinies_FM.csv"))
  Feed_Intake <- readr::read_csv(file.path(inputs_dir, "Intake_ygiac.csv.gz"))
  Population_share <- readr::read_csv(file.path(inputs_dir, "Population_yg.csv"))
  N_Excretion_ygs <- readRDS(file.path(inputs_dir, "N_Excretion_ygs.rds"))
  Livestock_Prod_ygps <- readr::read_csv(file.path(inputs_dir, "Livestock_Prod_ygps.csv"))

  Codes_coefs <- readxl::read_excel(file.path(inputs_dir, "Codes_coefs.xlsx"), sheet = "Names_biomass_CB")
  Codes_coefs_items_full <- readxl::read_excel(file.path(inputs_dir, "Codes_coefs.xlsx"), sheet = "items_full")
  Biomass_coefs <- readxl::read_excel(file.path(inputs_dir, "Biomass_coefs.xlsx"), skip = 1)
  processed_prov_fixed <- readxl::read_excel(file.path(inputs_dir, "processed_prov_fixed.xlsx"), sheet = "ProcessedItems_biomass")

  data_list <- list(
    NPP_ygpit_csv = NPP_ygpit_csv,
    Feed_avail_all = Feed_avail_all,
    Crop_AreaNPP_ygpitr_NoFallow = Crop_AreaNPP_ygpitr_NoFallow,
    Crop_AreaNPP_ygpit_all = Crop_AreaNPP_ygpit_all,
    PIE_FullDestinies_FM = PIE_FullDestinies_FM,
    Feed_Intake = Feed_Intake,
    Population_share = Population_share,
    N_Excretion_ygs = N_Excretion_ygs,
    Livestock_Prod_ygps = Livestock_Prod_ygps,
    Codes_coefs = Codes_coefs,
    Codes_coefs_items_full = Codes_coefs_items_full,
    Biomass_coefs = Biomass_coefs,
    processed_prov_fixed = processed_prov_fixed
  )

  return(data_list)
}


#' Production of Cropland, Livestock, and Semi natural agroecosystems --------------------------------------------------------------------
#' Merge items with biomasses
.merge_items_biomass <- function(Crop_AreaNPP_ygpit_all, NPP_ygpit_csv, Codes_coefs) {
  Crop_AreaNPP_merged <- Crop_AreaNPP_ygpit_all |>
    dplyr::left_join(
      Codes_coefs |>
        dplyr::select(Name_biomass, Item),
      by = "Name_biomass"
    )

  NPP_ygpit_merged <- NPP_ygpit_csv |>
    dplyr::left_join(
      Codes_coefs |>
        dplyr::select(Name_biomass, Item),
      by = "Name_biomass"
    )

  list(
    Crop_AreaNPP_merged = Crop_AreaNPP_merged,
    NPP_ygpit_merged = NPP_ygpit_merged
  )
}

#' Crops Production and Residues ---------------------------------------------------------------------------------
.summarise_crops_residues <- function(Crop_AreaNPP_ygpitr_NoFallow) {
  Crop_AreaNPP_prod_residue <- Crop_AreaNPP_ygpitr_NoFallow |>
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
    dplyr::rename(
      Prod_Residue_Product_Mg = Product,
      Residue_Mg = Residue
    ) |>
    dplyr::mutate(
      Prod_Residue_Product_Mg = dplyr::coalesce(Prod_Residue_Product_Mg, Residue_Mg)
    ) |>
    dplyr::select(-Residue_Mg) |>
    dplyr::mutate(Box = "Cropland")

  return(Crop_AreaNPP_prod_residue)
}

#' Combining crops, residues, feed (grass, fallow) production
#' Grazed: Aggregate GrazedWeeds_MgDM for Cropland (Fallow) ---
.aggregate_grazed_cropland <- function(NPP_ygpit_csv, Crop_AreaNPP_prod_residue) {
  grazed_data <- NPP_ygpit_csv |>
    dplyr::filter(LandUse == "Cropland", !(Item == "Fallow" | Name_biomass == "Fallow")) |>
    dplyr::select(Year, Province_name, GrazedWeeds_MgDM, Item, Name_biomass) |>
    dplyr::group_by(Year, Province_name, Item, Name_biomass) |>
    dplyr::summarise(GrazedWeeds_MgDM = sum(GrazedWeeds_MgDM, na.rm = TRUE), .groups = "drop")

  #' Merge `grazed_data` with `Crop_AreaNPP_prod_residue`
  Crop_AreaNPP_prod_residue_grass <- Crop_AreaNPP_prod_residue |>
    dplyr::left_join(grazed_data, by = c("Year", "Province_name", "Item", "Name_biomass"))

  #' Add missing Production data from NPP (Fallow)
  fallow_data <- NPP_ygpit_csv |>
    dplyr::filter(LandUse == "Cropland", Item == "Fallow" | Name_biomass == "Fallow") |>
    dplyr::group_by(Year, Province_name, Name_biomass, Item) |>
    dplyr::summarise(
      GrazedWeeds_MgDM = sum(GrazedWeeds_MgDM, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      Prod_Residue_Product_Mg = 0,
      Box = "Cropland"
    ) |>
    dplyr::select(Year, Province_name, Name_biomass, Item, Prod_Residue_Product_Mg, GrazedWeeds_MgDM, Box)

  Crop_AreaNPP_prod_residue_grazed <- dplyr::bind_rows(Crop_AreaNPP_prod_residue_grass, fallow_data) |>
    dplyr::arrange(Year, Province_name, Name_biomass, Item)

  crops_residues_grazed <- Crop_AreaNPP_prod_residue_grazed |>
    dplyr::mutate(across(c(Prod_Residue_Product_Mg, GrazedWeeds_MgDM), ~ tidyr::replace_na(., 0))) |>
    dplyr::select(-Box, everything(), Box)

  return(crops_residues_grazed)
}

#' Semi_natural_agroecosystems: Aggregate Grazed Weeds and Production plus Used Residues from Forest, Shrubland, Dehesa, Other ---
.aggregate_semi_natural_agroecosystems <- function(NPP_ygpit_csv) {
  semi_natural_agroecosystems <- NPP_ygpit_csv |>
    dplyr::ungroup() |>
    dplyr::filter(LandUse != "Cropland") |>
    dplyr::mutate(Box = "Semi_natural_agroecosystems") |>
    dplyr::select(Year, Province_name, Name_biomass, GrazedWeeds_MgDM, Prod_ygpit_Mg, Used_Residue_MgFM, Box, Item)

  return(semi_natural_agroecosystems)
}

#' Livestock Production ---------------------------------------------------------------------------------------------------------------
.prepare_livestock_production <- function(Livestock_Prod_ygps) {
  livestock <- Livestock_Prod_ygps |>
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

  return(livestock)
}

#' Combine Cropland, Semi_natural_agroecosystems and Livestock -----------------------------------------------------------------------------------------------------------
.combine_production_boxes <- function(crops_residues_grazed, semi_natural_agroecosystems, livestock) {
  GRAFS_prod_combined <- dplyr::bind_rows(
    crops_residues_grazed |>
      dplyr::select(Year, Province_name, Name_biomass, Item, Prod_Residue_Product_Mg, GrazedWeeds_MgDM, Box),
    semi_natural_agroecosystems |>
      dplyr::select(Year, Province_name, Name_biomass, Item, Prod_Residue_Product_Mg = Prod_ygpit_Mg, Used_Residue_MgFM, GrazedWeeds_MgDM, Box),
    livestock |>
      dplyr::select(Year, Province_name, Name_biomass, Item, Prod_Residue_Product_Mg = Prod_Mg, Box)
  )

  return(GRAFS_prod_combined)
}

#' Seed production per province, based on the national seed share per Area ------------------------------------------------------------------------------------------------
.remove_seeds_from_system <- function(Crop_AreaNPP_ygpit_all, PIE_FullDestinies_FM, GRAFS_prod_combined) {
  seeds_substracted <- Crop_AreaNPP_ygpit_all |>
    dplyr::filter(LandUse == "Cropland") |>
    dplyr::group_by(Year, Province_name, Item) |>
    dplyr::summarise(Area_ha = sum(Area_ygpit_ha, na.rm = TRUE), .groups = "drop") |>
    dplyr::left_join(
      PIE_FullDestinies_FM |>
        dplyr::filter(Element == "Domestic_supply", Destiny == "Seed") |>
        dplyr::group_by(Year, Item) |>
        dplyr::summarise(Seed_total = sum(Value_destiny, na.rm = TRUE), .groups = "drop") |>
        dplyr::left_join(
          Crop_AreaNPP_ygpit_all |>
            dplyr::filter(LandUse == "Cropland") |>
            dplyr::group_by(Year, Item) |>
            dplyr::summarise(National_area = sum(Area_ygpit_ha, na.rm = TRUE), .groups = "drop"),
          by = c("Year", "Item")
        ) |>
        dplyr::mutate(Seed_rate_per_ha = Seed_total / National_area) |>
        dplyr::select(Year, Item, Seed_rate_per_ha),
      by = c("Year", "Item")
    ) |>
    dplyr::mutate(Seeds_used_MgFM = Area_ha * Seed_rate_per_ha) |>
    tidyr::drop_na()

  #' Substracting the Seed data from Production in GRAFS_prod_combined
  GRAFS_prod_combined_no_seeds <- GRAFS_prod_combined |>
    dplyr::left_join(
      seeds_substracted |>
        dplyr::select(Year, Province_name, Item, Seeds_used_MgFM),
      by = c("Year", "Province_name", "Item")
    ) |>
    dplyr::mutate(
      Prod_Residue_Product_Mg = Prod_Residue_Product_Mg - dplyr::coalesce(Seeds_used_MgFM, 0)
    ) |>
    dplyr::select(-Seeds_used_MgFM)

  return(GRAFS_prod_combined_no_seeds)
}

#' Structuring dataset (GrazedWeeds und Used_Residues in ProductionFM) -------------------------------------------------------------------------------------------
#' Rename Prod_Residue_Product_Mg to Production_FM and replace Production_FM with GrazedWeeds_MgDM (for Fallow)
.adding_grass_wood <- function(GRAFS_prod_combined_no_seeds, Biomass_coefs) {
  GRAFS_prod_structured <- GRAFS_prod_combined_no_seeds |>
    dplyr::rename(Production_FM = Prod_Residue_Product_Mg) |>
    dplyr::mutate(
      Production_FM = dplyr::if_else(
        Name_biomass == "Fallow" | Item == "Fallow",
        GrazedWeeds_MgDM,
        Production_FM
      )
    )

  #' Create 'Grass' rows for Holm oak using GrazedWeeds_MgDM
  holm_oak_grass <- GRAFS_prod_structured |>
    dplyr::filter(Name_biomass == "Holm oak") |>
    dplyr::distinct(Year, Province_name, Name_biomass, Box, GrazedWeeds_MgDM) |>
    dplyr::mutate(
      Item = "Grass",
      Production_FM = GrazedWeeds_MgDM
    ) |>
    dplyr::select(Year, Province_name, Name_biomass, Item, Production_FM, Box)

  #' Create 'Firewood' rows for Holm oak using Used_Residue_MgFM
  holm_oak_firewood <- GRAFS_prod_structured |>
    dplyr::filter(Name_biomass == "Holm oak") |>
    dplyr::distinct(Year, Province_name, Name_biomass, Box, Used_Residue_MgFM) |>
    dplyr::mutate(
      Item = "Firewood",
      Production_FM = Used_Residue_MgFM
    ) |>
    dplyr::select(Year, Province_name, Name_biomass, Item, Production_FM, Box)

  #' Combine the two into one dataframe
  holm_oak_extra <- dplyr::bind_rows(holm_oak_grass, holm_oak_firewood)

  #' Create Firewood rows for other biomass types using Used_Residue_MgFM
  firewood_extra <- GRAFS_prod_structured |>
    dplyr::filter(Name_biomass %in% c("Conifers", "Holm oak forest", "Mediterranean shrubland")) |>
    dplyr::filter(!is.na(Used_Residue_MgFM) & Used_Residue_MgFM > 0) |>
    dplyr::mutate(
      Item = "Firewood",
      Production_FM = Used_Residue_MgFM
    ) |>
    dplyr::select(Year, Province_name, Name_biomass, Item, Production_FM, Box)

  #' Create Grass rows for other biomass types using GrazedWeeds_MgDM
  grass_extra <- GRAFS_prod_structured |>
    dplyr::filter(Name_biomass %in% c("Conifers", "Holm oak forest", "Mediterranean shrubland")) |>
    dplyr::filter(!is.na(GrazedWeeds_MgDM) & GrazedWeeds_MgDM > 0) |>
    dplyr::mutate(
      Item = "Grass",
      Production_FM = GrazedWeeds_MgDM
    ) |>
    dplyr::select(Year, Province_name, Name_biomass, Item, Production_FM, Box)

  #' Combine all new rows with existing data
  GRAFS_prod_added_grass_wood <- GRAFS_prod_structured |>
    dplyr::select(Year, Province_name, Name_biomass, Item, Production_FM, Box) |>
    dplyr::bind_rows(holm_oak_extra, firewood_extra, grass_extra) |>
    dplyr::filter(!is.na(Production_FM)) |>
    # Convert only Grass rows from DM to FM using 20% coefficient from Biomass_coefs
    dplyr::mutate(
      Production_FM = dplyr::if_else(
        Item == "Grass" & !is.na(Production_FM),
        Production_FM / 0.2,
        Production_FM
      ),
      Item = dplyr::if_else(Item == "Grass", "Grassland", Item),
      Name_biomass = dplyr::if_else(Item == "Grassland", "Grass", Name_biomass)
    ) |>
    #' Aggregate Grassland rows across same Year/Province_name/Item/Box
    dplyr::group_by(Year, Province_name, Name_biomass, Item, Box) |>
    dplyr::summarise(Production_FM = sum(Production_FM, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(Year, Province_name, Name_biomass, Item)

  return(GRAFS_prod_added_grass_wood)
}

#' Processed Items ----------------------------------------------------------------------------------------------------------------------------------------
#' Summarise processed items by Year, Province, Biomass, Item, and ProcessedItem
.prepare_processed_data <- function(processed_prov_fixed) {
  processed_data <- processed_prov_fixed |>
    dplyr::group_by(Year, Province_name, Name_biomass, Item, ProcessedItem) |>
    dplyr::summarise(ProcessedItem_amount = sum(ProcessedItem_amount, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(
      Item = ProcessedItem,
      Production_FM = ProcessedItem_amount,
      Box = "Cropland"
    ) |>
    dplyr::select(Year, Province_name, Name_biomass, Item, Box, Production_FM)

  return(processed_data)
}

#' Match structure of GRAFS_prod_combined_no_seeds ---------------------------------------------------------------------------------------------------------------------
.prepare_prod_data <- function(GRAFS_prod_added_grass_wood, processed_data) {
  GRAFS_prod_added_grass_wood_prepared <- GRAFS_prod_added_grass_wood |>
    dplyr::select(Year, Province_name, Name_biomass, Item, Box, Production_FM) |>
    dplyr::bind_rows(processed_data) |>
    dplyr::arrange(Year, Province_name, Name_biomass, Item)

  #' Merging Item and Name_biomass and creating Name_biomass_primary
  GRAFS_prod_added_grass_wood_merged <- GRAFS_prod_added_grass_wood_prepared |>
    dplyr::rename(Name_biomass_primary = Name_biomass) |>
    dplyr::left_join(Codes_coefs_items_full |> dplyr::select(item, Name_biomass), by = c("Item" = "item")) |>
    dplyr::mutate(Name_biomass = dplyr::if_else(!is.na(Name_biomass), Name_biomass, Name_biomass_primary)) |>
    dplyr::relocate(Name_biomass, .after = Name_biomass_primary)

  return(GRAFS_prod_added_grass_wood_merged)
}

#' Convert Fresh Matter (FM) to Dry Matter (DM) and Nitrogen Content (N) -------------------------------------------------------------------------------------
#' Define a list of special items that require using the primary biomass name for selecting conversion coefficients
.convert_FM_DM_N <- function(GRAFS_prod_added_grass_wood_merged, Biomass_coefs) {
  special_items <- c(
    "Nuts and products", "Vegetables, Other", "Fruits, Other",
    "Cereals, Other", "Pulses, Other and products"
  )

  #' Add a column to choose the appropriate biomass name for matching conversion factors
  GRAFS_prod_grazed_no_seeds_primary <- GRAFS_prod_added_grass_wood_merged |>
    dplyr::mutate(
      Biomass_match = dplyr::if_else(Item %in% special_items, Name_biomass_primary, Name_biomass)
    )

  #' Join with FM to DM conversion factors using Biomass_match, then calculate Dry Matter production
  GRAFS_prod_grazed_no_seeds_DM <- GRAFS_prod_grazed_no_seeds_primary |>
    dplyr::left_join(
      Biomass_coefs |> dplyr::select(Name_biomass, Product_kgDM_kgFM),
      by = c("Biomass_match" = "Name_biomass")
    ) |>
    dplyr::mutate(
      Production_DM = Production_FM * Product_kgDM_kgFM
    )

  #' Join with DM to N conversion factors using Biomass_match, then calculate Nitrogen production
  GRAFS_prod_grazed_no_seeds_N <- GRAFS_prod_grazed_no_seeds_DM |>
    dplyr::left_join(
      Biomass_coefs |>
        dplyr::select(Name_biomass, Product_kgN_kgDM),
      by = c("Biomass_match" = "Name_biomass")
    ) |>
    dplyr::mutate(
      Production_N = Production_DM * Product_kgN_kgDM
    ) |>
    #' Remove intermediate columns and rename Biomass_match to Name_biomass
    dplyr::select(-Production_FM, -Product_kgDM_kgFM, -Product_kgN_kgDM, -Name_biomass) |>
    dplyr::rename(Name_biomass = Biomass_match)

  #' Summarize total Dry Matter and Nitrogen production per Item, Year, Province, and Box
  GRAFS_prod_item <- GRAFS_prod_grazed_no_seeds_N |>
    dplyr::group_by(Year, Province_name, Item, Box) |>
    dplyr::summarise(
      Production_DM = sum(Production_DM, na.rm = TRUE),
      Production_N = sum(Production_N, na.rm = TRUE),
      .groups = "drop"
    )

  return(GRAFS_prod_item)
}

#' Consumption (Destinies) -----------------------------------------------------------------------------------------------------------------------------------------------------------
#' Intake Livestock: sum all values (FM_Mg) for the same Year, Province_name and Item -------------------------------------------------------------------------------------
.adding_feed <- function(Feed_Intake) {
  feed_intake <- Feed_Intake |>
    dplyr::select(Year, Province_name, Item, FM_Mg) |>
    dplyr::group_by(Year, Province_name, Item) |>
    dplyr::summarise(FM_Mg_total = sum(FM_Mg, na.rm = TRUE), .groups = "drop")

  return(feed_intake)
}

#' Popoulation: use column Pop_Mpeop_yg. Calculate the share of population -------------------------------------------------------------------------------------------------
#' (population in each province divided through whole population in Spain to get the share; multiply with Food from PIE full destiny)
.calculate_population_share <- function(Population_share) {
  population_share <- Population_share |>
    dplyr::select(Year, Province_name, Pop_Mpeop_yg) |>
    dplyr::group_by(Year) |>
    dplyr::mutate(
      Total_pop_spain = sum(Pop_Mpeop_yg, na.rm = TRUE),
      Pop_share = Pop_Mpeop_yg / Total_pop_spain
    ) |>
    dplyr::ungroup() |>
    dplyr::select(Year, Province_name, Pop_Mpeop_yg, Pop_share)

  return(population_share)
}

#' Food --------------------------------------------------------------------------------------------------------------------------------------------------------------------
#' Sum all Elements for Food and multiply with population share
.adding_food <- function(PIE_FullDestinies_FM, population_share) {
  food_with_share <- PIE_FullDestinies_FM |>
    dplyr::filter(Destiny == "Food") |>
    dplyr::filter(Element == "Domestic_supply") |>
    dplyr::group_by(Year, Item) |>
    dplyr::summarise(Total_Food_value = sum(Value_destiny, na.rm = TRUE), .groups = "drop") |>
    dplyr::left_join(population_share, by = "Year") |>
    dplyr::mutate(Food_Mg = Pop_share * Total_Food_value) |>
    dplyr::select(Year, Province_name, Item, Food_Mg)

  return(food_with_share)
}

#' Other_uses ---------------------------------------------------------------------------------------------------------------------------------------------------------------
#' Sum all Elements for Other_uses and multiply with population share
.adding_other_uses <- function(PIE_FullDestinies_FM, population_share) {
  other_uses_with_share <- PIE_FullDestinies_FM |>
    dplyr::filter(Destiny == "Other_uses") |>
    dplyr::filter(Element == "Domestic_supply") |>
    dplyr::group_by(Year, Item) |>
    dplyr::summarise(Total_OtherUses_value = sum(Value_destiny, na.rm = TRUE), .groups = "drop") |>
    dplyr::left_join(Population_share, by = "Year") |>
    dplyr::mutate(OtherUses_Mg = Pop_share * Total_OtherUses_value) |>
    dplyr::select(Year, Province_name, Item, OtherUses_Mg)

  return(other_uses_with_share)
}

#' Putting all together ---------------------------------------------------------------------------------------------------------------------------------------
.combine_destinies <- function(GRAFS_prod_item, feed_intake, other_uses_with_share, food_with_share) {
  GRAFS_prod_item_combined <- GRAFS_prod_item |>
    dplyr::full_join(food_with_share |> rename(Food_MgFM = Food_Mg),
      by = c("Year", "Province_name", "Item")
    ) |>
    dplyr::full_join(other_uses_with_share |> rename(OtherUses_MgFM = OtherUses_Mg),
      by = c("Year", "Province_name", "Item")
    ) |>
    dplyr::full_join(feed_intake |> rename(Feed_MgFM = FM_Mg_total),
      by = c("Year", "Province_name", "Item")
    )

  return(GRAFS_prod_item_combined)
}

#' Converting Item and Name_biomass again and converting FM to DM, and DM to N
.convert_to_items_N <- function(GRAFS_prod_item_combined, Codes_coefs_items_full, Biomass_coefs) {
  GRAFS_prod_item_N <- GRAFS_prod_item_combined |>
    dplyr::left_join(
      Codes_coefs_items_full |> dplyr::select(item, Name_biomass),
      by = c("Item" = "item")
    ) |>
    dplyr::relocate(Name_biomass, .after = Item) |>
    #' Convert the conversion factors into GRAFS_prod_item
    dplyr::left_join(
      Biomass_coefs |> dplyr::select(Name_biomass, Product_kgDM_kgFM, Product_kgN_kgDM),
      by = "Name_biomass"
    ) |>
    #' Convert FM → DM → N for each use type
    dplyr::mutate(
      Food_MgDM = Food_MgFM * Product_kgDM_kgFM,
      OtherUses_MgDM = OtherUses_MgFM * Product_kgDM_kgFM,
      Feed_MgDM = Feed_MgFM * Product_kgDM_kgFM,
      Food_MgN = Food_MgDM * Product_kgN_kgDM,
      OtherUses_MgN = OtherUses_MgDM * Product_kgN_kgDM,
      Feed_MgN = Feed_MgDM * Product_kgN_kgDM
    ) |>
    dplyr::select(Year, Province_name, Item, Name_biomass, Box, Production_N, Food_MgN, OtherUses_MgN, Feed_MgN)

  return(GRAFS_prod_item_N)
}

#' Calculating Consumption and Trade ---------------------------------------------------------------------------------------------------------------------------------------------
.calculate_trade <- function(GRAFS_prod_item_N) {
  GRAFS_prod_item_trade <- GRAFS_prod_item_N |>
    dplyr::group_by(Year, Province_name, Item, Name_biomass, Box) |>
    dplyr::mutate(
      Consumption_N = rowSums(across(c(Food_MgN, OtherUses_MgN, Feed_MgN)), na.rm = TRUE),
      Production_N_tmp = replace_na(Production_N, 0),
      Net_trade = Production_N_tmp - Consumption_N,
      Export_MgN = ifelse(Net_trade > 0, Net_trade, 0),
      Import_MgN = ifelse(Net_trade < 0, -Net_trade, 0)
    ) |>
    dplyr::select(-Production_N_tmp) |>
    dplyr::ungroup()

  return(GRAFS_prod_item_trade)
}

#' Adding missing Boxes for Imports ---------------------------------------------------------------------------------------------------------------------------------------------
#' Join group info from Codes_coefs to GRAFS where Box is NA
.finalize_prod_destiny <- function(GRAFS_prod_item_trade) {
  GRAFS_prod_destiny <- GRAFS_prod_item_trade |>
    dplyr::left_join(
      Codes_coefs_items_full |> select(item, group),
      by = c("Item" = "item")
    ) |>
    dplyr::mutate(
      Box = case_when(
        Item == "Acorns" ~ "Semi_natural_agroecosystems",
        is.na(Box) & Item == "Fallow" ~ "Cropland",
        is.na(Box) & group %in% c("Crop products", "Primary crops", "crop residue") ~ "Cropland",
        is.na(Box) & group %in% c("Livestock products", "Livestock") ~ "Livestock",
        is.na(Box) & group %in% c("Additives", "Fish") ~ group,
        TRUE ~ Box
      )
    ) |>
    dplyr::select(-group)

  GRAFS_prod_destiny_final <- GRAFS_prod_destiny |>
    tidyr::pivot_longer(
      cols = c(Food_MgN, OtherUses_MgN, Feed_MgN, Export_MgN, Import_MgN),
      names_to = "Destiny",
      values_to = "MgN"
    ) |>
    dplyr::mutate(
      Destiny = recode(Destiny,
        Food_MgN = "Food",
        OtherUses_MgN = "Other_uses",
        Feed_MgN = "Feed",
        Export_MgN = "Export",
        Import_MgN = "Import"
      )
    ) |>
    dplyr::select(Year, Province_name, Item, Box, Destiny, MgN)

  return(GRAFS_prod_destiny_final)

  write.csv(GRAFS_Prod_Destiny, "Outputs/GRAFS_Prod_Destiny.csv")
  GRAFS_Prod_Destiny <- readr::read_csv("Outputs/GRAFS_Prod_Destiny.csv")
  View(GRAFS_Prod_Destiny)
}


#' Plots
#' Plot Spain Destinies ----------------------------------------------------------------------------------------------------------------------
#' Summarize and transform MgN → GgN; make Imports negative
MgN_time_series <- GRAFS_Prod_Destiny |>
  dplyr::group_by(Year, Destiny) |>
  dplyr::summarise(
    MgN_total = sum(MgN, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    GgN_total = MgN_total / 1000,
    GgN_total = ifelse(Destiny == "Import", -GgN_total, GgN_total)
  )

#' Create a time series line plot in GgN
ggplot2::ggplot(MgN_time_series, aes(x = Year, y = GgN_total, color = Destiny)) +
  ggplot2::geom_line(size = 1) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  ggplot2::labs(
    title = "Nitrogen in Spain by Destiny",
    x = "Year",
    y = "Nitrogen (GgN)",
    color = "Destiny"
  ) +
  ggplot2::theme_minimal()

ggplot2::ggsave(
  filename = "N_Spain_Destiny.png",
  width = 16,
  height = 10,
  dpi = 300
)


#' Plot Provinces Destinies -------------------------------------------------------------------------------------------------------------------
#' Summarize and transform MgN to GgN; set Import to negative
MgN_time_series_province <- GRAFS_Prod_Destiny |>
  dplyr::filter(Province_name != "Sea") |>
  dplyr::group_by(Year, Province_name, Destiny) |>
  dplyr::summarise(MgN_total = sum(MgN, na.rm = TRUE), .groups = "drop") |>
  dplyr::mutate(
    GgN_total = MgN_total / 1000,
    GgN_total = ifelse(Destiny == "Import", -GgN_total, GgN_total)
  )

#' Plot with facet per province and horizontal zero line
ggplot2::ggplot(MgN_time_series_province, aes(x = Year, y = GgN_total, color = Destiny)) +
  ggplot2::geom_line(size = 0.8) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  ggplot2::facet_wrap(~Province_name, scales = "free_y") +
  ggplot2::labs(
    title = "Nitrogen Spain per Destiny and Province",
    x = "Year",
    y = "Gg N",
    color = "Destiny"
  ) +
  ggplot2::theme_minimal(base_size = 8) +
  ggplot2::theme(
    strip.text = element_text(size = 7),
    axis.text = element_text(size = 6),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 9),
    plot.title = element_text(size = 10, face = "bold"),
    legend.position = "bottom"
  )

ggplot2::ggsave(
  filename = "N_Spain_Provinces.png",
  width = 16,
  height = 10,
  dpi = 300
)

#' Plot per Box and provinces -----------------------------------------------------------------------------------------------------------------------
#' Summarize MgN per year, province, and box
MgN_time_series_box_province <- GRAFS_Prod_Destiny |>
  dplyr::filter(Province_name != "Sea") |>
  dplyr::group_by(Year, Province_name, Box) |>
  dplyr::summarise(MgN_total = sum(MgN, na.rm = TRUE), .groups = "drop") |>
  dplyr::mutate(
    GgN_total = MgN_total / 1000
  )


#' Faceted time series plot per province
ggplot2::ggplot(MgN_time_series_box_province, aes(x = Year, y = GgN_total, color = Box)) +
  ggplot2::geom_line(size = 0.8) +
  ggplot2::facet_wrap(~Province_name, scales = "free_y") +
  ggplot2::labs(
    title = "Nitrogen in Spain by Box and Province",
    x = "Year",
    y = "Gg N",
    color = "Box"
  ) +
  ggplot2::theme_minimal(base_size = 8) +
  ggplot2::theme(
    strip.text = element_text(size = 7),
    axis.text = element_text(size = 6),
    legend.title = element_text(size = 7),
    legend.text = element_text(size = 6),
    plot.title = element_text(size = 9, face = "bold"),
    legend.position = "bottom"
  )
ggplot2::ggsave(
  filename = "N_Spain_Provinces_Box.png",
  width = 16,
  height = 10,
  dpi = 300
)
