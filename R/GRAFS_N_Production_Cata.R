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
  # Load input data
  data <- .load_grafs_inputs(inputs_dir)

  # Prepare crop production and residues
  crop_prod <- .prepare_crop_production(data$Crop_AreaNPP_ygpitr_NoFallow)

  # Add grazed weeds and fallow
  crop_prod_feed <- .add_grazing_and_fallow(crop_prod, data$NPP_ygpit_csv)

  # Build GRAFS production table
  grafs_prod <- build_grafs_production(
    crop_data = crop_prod_feed,
    semi_natural = data$Feed_avail_all,
    livestock_data = data$Livestock_Prod_ygps
  )

  # Subtract seeds
  grafs_prod_substracted_seeds <- subtract_seeds(
    crop_area_all = data$Crop_AreaNPP_ygpit_all,
    destinies = data$PIE_FullDestinies_FM,
    production_data = grafs_prod
  )

  # Expand with grass, firewood, and processed products
  grafs_grass_firewood_processed <- expand_with_grass_firewood_processed(
    grafs_data = grafs_prod_substracted_seeds,
    processed_data = data$processed_prov_fixed,
    biomass_coefs = data$Biomass_coefs,
    codes_items = data$Codes_coefs_items_full
  )

  # Convert fresh matter to dry matter
  grafs_dm <- convert_fm_to_dm_n(
    df = grafs_grass_firewood_processed,
    biomass_coefs = data$Biomass_coefs
  )

  # Return list
  return(list(
    Crop_Production = crop_prod_feed,
    GRAFS_Production = grafs_prod,
    GRAFS_Production_Expanded = grafs_grass_firewood_processed,
    GRAFS_Production_DM = grafs_dm
  ))
}

#' Load all required datasets from input directory
#'
#' @param inputs_dir Path to the input folder containing data files.
#' @return A named list of all datasets loaded
load_data <- function(inputs_dir) {
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

#' Merge Items with Biomasses in Crop and NPP data
#'
#' @param crop_data Dataframe with crop biomass data
#' @param codes_coefs Dataframe with codes and coefficients
#' @return Crop data joined with Item information
merge_items_biomass <- function(crop_data, codes_coefs) {
  merged <- crop_data |>
    dplyr::left_join(
      codes_coefs |> dplyr::select(Name_biomass, Item),
      by = "Name_biomass"
    )
  return(merged)
}

#' Summarize Crops Production and Residues
#'
#' @param crop_data Dataframe containing crop area and production excluding fallow
#' @return Dataframe with summarized production and residue by Year, Province, biomass, and item
summarise_crops_production_residues <- function(crop_data) {
  sum_prod_residue <- crop_data |>
    dplyr::select(
      Year, Province_name, Name_biomass, Prod_ygpit_Mg, Product_residue, Item
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

  return(sum_prod_residue)
}

#' Aggregate Grazed Weeds for Cropland excluding Fallow
#'
#' @param npp_data NPP data with biomass info
#' @return Aggregated grazed weeds for cropland (excluding fallow)
aggregate_grazed_cropland <- function(npp_data) {
  grazed <- npp_data |>
    dplyr::filter(LandUse == "Cropland", !(Item == "Fallow" | Name_biomass == "Fallow")) |>
    dplyr::select(Year, Province_name, GrazedWeeds_MgDM, Item, Name_biomass) |>
    dplyr::group_by(Year, Province_name, Item, Name_biomass) |>
    dplyr::summarise(GrazedWeeds_MgDM = sum(GrazedWeeds_MgDM, na.rm = TRUE), .groups = "drop")

  return(grazed)
}

#' Add missing Fallow production data from NPP
#'
#' @param npp_data NPP data with biomass info
#' @return Dataframe with fallow production and grazed weeds data
add_fallow_data <- function(npp_data) {
  fallow <- npp_data |>
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

  return(fallow)
}

#' Combine Crop Production with Grazed Data including fallow
#'
#' @param crop_prod_residue Dataframe of crop production and residue
#' @param grazed_data Dataframe of aggregated grazed weeds data
#' @param fallow_data Dataframe of fallow production and grazed data
#' @return crops_combined dataframe with production, residues and grazed weeds
combine_crop_grazed_fallow <- function(crop_prod_residue, grazed_data, fallow_data) {
  crops_combined <- crop_prod_residue |>
    dplyr::left_join(grazed_data, by = c("Year", "Province_name", "Item", "Name_biomass")) |>
    dplyr::bind_rows(fallow_data) |>
    dplyr::arrange(Year, Province_name, Name_biomass, Item) |>
    dplyr::mutate(
      dplyr::across(c(Prod_Residue_Product_Mg, GrazedWeeds_MgDM), ~ tidyr::replace_na(., 0))
    ) |>
    dplyr::group_by(Year, Province_name, Name_biomass, Item, Box) |>
    dplyr::select(-Box, everything(), Box)

  return(crops_combined)
}

#' Aggregate Semi-natural Agroecosystems production and grazing
#'
#' @param npp_data NPP data with biomass info
#' @return Dataframe of semi-natural agroecosystems with production, grazing and residue
aggregate_semi_natural_agroecosystems <- function(npp_data) {
  semi_natural <- npp_data |>
    dplyr::ungroup() |>
    dplyr::filter(LandUse != "Cropland") |>
    dplyr::mutate(Box = "Semi_natural_agroecosystems") |>
    dplyr::select(Year, Province_name, Name_biomass, GrazedWeeds_MgDM, Prod_ygpit_Mg, Used_Residue_MgFM, Box, Item)

  return(semi_natural)
}

#' Prepare livestock production data
#'
#' @param livestock_data livestock production dataframe
#' @return Formatted livestock dataframe with following columns: Year, Province_name, Item, Name_biomass, Prod_Mg
prepare_livestock_production <- function(livestock_data) {
  livestock <- livestock_data |>
    dplyr::select(Year, Province_name, Item, Name_biomass, Prod_Mg) |>
    dplyr::mutate(Box = "Livestock")

  return(livestock)
}

#' Combine cropland, semi-natural and livestock production data
#'
#' @param cropland Cropland production dataframe
#' @param semi_natural Semi-natural ecosystem dataframe
#' @param livestock Livestock production dataframe
#' @return Combined dataframe of all production sources
#'
combine_all_production <- function(cropland, semi_natural, livestock) {
  combined <- dplyr::bind_rows(
    cropland |>
      dplyr::select(Year, Province_name, Name_biomass, Item, Prod_Residue_Product_Mg, GrazedWeeds_MgDM, Box),
    semi_natural |>
      dplyr::select(Year, Province_name, Name_biomass, Item,
        Prod_Residue_Product_Mg = Prod_ygpit_Mg, Used_Residue_MgFM,
        GrazedWeeds_MgDM, Box
      ),
    livestock |>
      dplyr::select(Year, Province_name, Name_biomass, Item,
        Prod_Residue_Product_Mg = Prod_Mg, Box
      )
  )

  return(combined)
}

#' Calculate and subtract seeds from crop production
#'
#' @param crop_data Crop dataframe
#' @param seed_data Seed dataframe
#' @return Seed-subtracted production dataframe
subtract_seed_use <- function(crop_data, seed_data) {
  seed_area <- crop_data |>
    dplyr::filter(LandUse == "Cropland") |>
    dplyr::group_by(Year, Province_name, Item) |>
    dplyr::summarise(Area_ha = sum(Area_ygpit_ha, na.rm = TRUE), .groups = "drop")

  seed_rates <- seed_data |>
    dplyr::filter(Element == "Domestic_supply", Destiny == "Seed") |>
    dplyr::group_by(Year, Item) |>
    dplyr::summarise(Seed_total = sum(Value_destiny, na.rm = TRUE), .groups = "drop")

  national_area <- crop_data |>
    dplyr::filter(LandUse == "Cropland") |>
    dplyr::group_by(Year, Item) |>
    dplyr::summarise(National_area = sum(Area_ygpit_ha, na.rm = TRUE), .groups = "drop")

  seeds_joined <- seed_rates |>
    dplyr::left_join(national_area, by = c("Year", "Item")) |>
    dplyr::mutate(Seed_rate_per_ha = Seed_total / National_area) |>
    dplyr::select(Year, Item, Seed_rate_per_ha)

  seeds_subtracted <- seed_area |>
    dplyr::left_join(seeds_joined, by = c("Year", "Item")) |>
    dplyr::mutate(Seeds_used_MgFM = Area_ha * Seed_rate_per_ha) |>
    tidyr::drop_na()

  return(seeds_subtracted)
}

#' Remove seeds from production data
#'
#' @param production Combined production dataframe
#' @param seeds Dataframe of seeds used per province
#' @return Production data without seeds
#'
remove_seed_production <- function(production, seeds) {
  production_no_seeds <- production |>
    dplyr::left_join(
      seeds |> dplyr::select(Year, Province_name, Item, Seeds_used_MgFM),
      by = c("Year", "Province_name", "Item")
    ) |>
    dplyr::mutate(
      Prod_Residue_Product_Mg = Prod_Residue_Product_Mg - dplyr::coalesce(Seeds_used_MgFM, 0)
    ) |>
    dplyr::select(-Seeds_used_MgFM)

  return(production_no_seeds)
}

#' Rename and adjust Production_FM, replacing it with GrazedWeeds_MgDM for Fallow
#'
#' @param data Input dataframe with Prod_Residue_Product_Mg and GrazedWeeds_MgDM
#' @return Updated dataframe with Production_FM column
#'
production_fallow <- function(data) {
  fallow <- data |>
    dplyr::rename(Production_FM = Prod_Residue_Product_Mg) |>
    dplyr::mutate(
      Production_FM = dplyr::if_else(
        Name_biomass == "Fallow" | Item == "Fallow",
        GrazedWeeds_MgDM,
        Production_FM
      )
    )
}

#' Create Holm oak rows for Grass and Firewood
#'
#' @param data Input dataframe including Holm oak entries
#' @return Dataframe with added Grass and Firewood entries for Holm oak
#'
create_holm_oak_rows <- function(data) {
  holm_oak_grass <- data |>
    dplyr::filter(Name_biomass == "Holm oak") |>
    dplyr::distinct(Year, Province_name, Name_biomass, Box, GrazedWeeds_MgDM) |>
    dplyr::mutate(Item = "Grass", Production_FM = GrazedWeeds_MgDM) |>
    dplyr::select(Year, Province_name, Name_biomass, Item, Production_FM, Box)

  holm_oak_firewood <- data |>
    dplyr::filter(Name_biomass == "Holm oak") |>
    dplyr::distinct(Year, Province_name, Name_biomass, Box, Used_Residue_MgFM) |>
    dplyr::mutate(Item = "Firewood", Production_FM = Used_Residue_MgFM) |>
    dplyr::select(Year, Province_name, Name_biomass, Item, Production_FM, Box)

  dplyr::bind_rows(holm_oak_grass, holm_oak_firewood)
}

#' Create Firewood and Grass rows for other biomass types
#'
#' @param data Input dataframe
#' @return Dataframe with additional Firewood and Grass rows
#'
create_additional_firewood_grass_rows <- function(data) {
  firewood <- data |>
    dplyr::filter(Name_biomass %in% c("Conifers", "Holm oak forest", "Mediterranean shrubland")) |>
    dplyr::filter(!is.na(Used_Residue_MgFM) & Used_Residue_MgFM > 0) |>
    dplyr::mutate(Item = "Firewood", Production_FM = Used_Residue_MgFM) |>
    dplyr::select(Year, Province_name, Name_biomass, Item, Production_FM, Box)

  grass <- data |>
    dplyr::filter(Name_biomass %in% c("Conifers", "Holm oak forest", "Mediterranean shrubland")) |>
    dplyr::filter(!is.na(GrazedWeeds_MgDM) & GrazedWeeds_MgDM > 0) |>
    dplyr::mutate(Item = "Grass", Production_FM = GrazedWeeds_MgDM) |>
    dplyr::select(Year, Province_name, Name_biomass, Item, Production_FM, Box)

  dplyr::bind_rows(firewood, grass)
}

#' Combine new production rows and convert Grass DM to FM
#'
#' @param combined_data Main dataset with production values
#' @param holm_oak_extra Holm oak production (grass and firewood)
#' @param firewood_extra Other firewood rows
#' @param grass_extra Other grass rows
#' @return Cleaned and aggregated production dataset
#'
combine_and_convert_production_data <- function(combined_data, holm_oak_extra, firewood_extra, grass_extra) {
  combined_data |>
    dplyr::select(Year, Province_name, Name_biomass, Item, Production_FM, Box) |>
    dplyr::bind_rows(holm_oak_extra, firewood_extra, grass_extra) |>
    dplyr::filter(!is.na(Production_FM)) |>
    dplyr::mutate(
      Production_FM = dplyr::if_else(
        Item == "Grass" & !is.na(Production_FM),
        Production_FM / 0.2,
        Production_FM
      ),
      Item = dplyr::if_else(Item == "Grass", "Grassland", Item),
      Name_biomass = dplyr::if_else(Item == "Grassland", "Grass", Name_biomass)
    ) |>
    dplyr::group_by(Year, Province_name, Name_biomass, Item, Box) |>
    dplyr::summarise(Production_FM = sum(Production_FM, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(Year, Province_name, Name_biomass, Item)
}

#' Process and format processed items to match GRAFS structure
#'
#' @param processed_data Data with processed items
#' @return Cleaned processed item dataframe
prepare_processed_items <- function(processed_data) {
  processed_data |>
    dplyr::group_by(Year, Province_name, Name_biomass, Item, ProcessedItem) |>
    dplyr::summarise(ProcessedItem_amount = sum(ProcessedItem_amount, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(
      Item = ProcessedItem,
      Production_FM = ProcessedItem_amount,
      Box = "Cropland"
    ) |>
    dplyr::select(Year, Province_name, Name_biomass, Item, Box, Production_FM)
}

#' Merge processed data into main dataset (combined_data)
#'
#' @param combined_data Production dataset before merging
#' @param processed_items Processed item dataset
#' @return Combined and arranged dataframe
merge_processed_data <- function(combined_data, processed_items) {
  combined_data |>
    dplyr::select(Year, Province_name, Name_biomass, Item, Box, Production_FM) |>
    dplyr::bind_rows(processed_items) |>
    dplyr::arrange(Year, Province_name, Name_biomass, Item)
}

#' Add primary biomass names and resolve mismatches with item-biomass dictionary
#'
#' @param data Input dataframe
#' @param item_lookup Lookup table
#' @return Dataframe with Name_biomass_primary and resolved Name_biomass
#'
resolve_biomass_names <- function(data, item_lookup) {
  data |>
    dplyr::rename(Name_biomass_primary = Name_biomass) |>
    dplyr::left_join(item_lookup |> dplyr::select(item, Name_biomass), by = c("Item" = "item")) |>
    dplyr::mutate(
      Name_biomass = dplyr::if_else(!is.na(Name_biomass), Name_biomass, Name_biomass_primary)
    ) |>
    dplyr::relocate(Name_biomass, .after = Name_biomass_primary)
}
