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
#' A final tibble (`n_prod_destiny`) containing N production data by destiny.
#' It includes the following columns:
#'   - `year`: The year in which the recorded event occurred.
#'   - `province_name`: The Spanish province where the data is from.
#'   - `item`: The item which was produced, defined in `codes_coefs`.
#'   - `box`: One of the GRAFS model systems: cropland,
#'            Semi-natural agroecosystems, Livestock, Fish, or Additives.
#'   - `destiny`: The use category of the nitrogen: Food, Feed, Other_uses,
#'                Export, or Import.
#'   - `MgN`: Nitrogen amount in megagrams (Mg).
#'
#' @export
create_prod_and_destiny_grafs <- function() {
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
  npp_ygpit_csv <- readr::read_csv(get_file_path("npp_ygpit"))
  feed_avail_all <- readRDS(get_file_path("feed_avail_all")) |> dplyr::ungroup()
  crop_area_npp_ygpitr_no_fallow <- readRDS(
    get_file_path("crop_area_npp_ygpitr_no_fallow")
  ) |> dplyr::ungroup()
  crop_area_npp_ygpit_all <- readRDS(
    get_file_path("crop_area_npp_ygpit_all")
  ) |>
    dplyr::ungroup()
  pie_full_destinies_fm <- readr::read_csv(
    get_file_path("pie_full_destinies_fm")
  )
  feed_intake <- readr::read_csv(get_file_path("intake_ygiac"))
  population_share <- readr::read_csv(get_file_path("population_yg"))
  n_excretion_ygs <- readRDS(
    get_file_path("n_excretion_ygs")
  ) |>
    dplyr::ungroup()
  livestock_prod_ygps <- readr::read_csv(get_file_path("livestock_prod_ygps"))

  codes_coefs <- readxl::read_excel(
    get_file_path("codes_coefs"),
    sheet = "Names_biomass_CB"
  )
  codes_coefs_items_full <- readxl::read_excel(
    get_file_path("codes_coefs"),
    sheet = "items_full"
  )
  biomass_coefs <- readxl::read_excel(get_file_path("biomass_coefs"), skip = 1)
  processed_prov_fixed <- readxl::read_excel(
    get_file_path("processed_prov_fixed"),
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

#' @title Production of Cropland, Livestock, and Semi-natural agroecosystems
#' @description Merge items with biomasses.
#'
#' @param crop_area_npp_ygpit_all Data frame with cropland area and N data.
#' @param npp_ygpit_csv Data frame with N data.
#' @param codes_coefs Data frame with biomass names and associated item names.
#'
#' @return A list with two merged data frames: 'crop_area_npp_merged' and
#' 'npp_ygpit_merged'.
#' @keywords internal
#' @noRd
.merge_items_biomass <- function(
  crop_area_npp_ygpit_all,
  npp_ygpit_csv,
  codes_coefs
) {
  crop_area_npp_merged <- crop_area_npp_ygpit_all |>
    dplyr::left_join(
      codes_coefs |>
        dplyr::select(Name_biomass, Item),
      by = "Name_biomass"
    )

  npp_ygpit_merged <- npp_ygpit_csv |>
    dplyr::left_join(
      codes_coefs |> dplyr::select(Name_biomass, Item),
      by = "Name_biomass"
    )

  list(
    crop_area_npp_merged = crop_area_npp_merged,
    npp_ygpit_merged = npp_ygpit_merged
  )
}

#' @title Crops Production and Residues ----------------------------------------
#'
#' @param crop_area_npp_ygpitr_no_fallow Data frame excluding fallow.
#'
#' @return A data frame summarizing total crop production and residues per
#' province and year.
#' @keywords internal
#' @noRd
.summarise_crops_residues <- function(
  crop_area_npp_ygpitr_no_fallow
) {
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


#' @title Combining crops, residues, feed (grass, fallow) production -----------
#'
#' @param npp_ygpit_merged NPP merged data including cropland and fallow.
#' @param crop_area_npp_prod_residue Data frame with crop production and
#' residue.
#'
#' @return A data frame combining production, residues, and grazed biomass.
#' @keywords internal
#' @noRd
.aggregate_grazed_cropland <- function(
  npp_ygpit_merged,
  crop_area_npp_prod_residue
) {
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

  # Merge 'grazed_data' with 'crop_area_npp_prod_residue'
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

#' @title Semi_natural_agroecosystems
#' @description Aggregate Grazed Weeds and Production plus Used Residues from
#' Forest, Shrubland, Dehesa, Other.
#'
#' @param npp_ygpit_merged A data frame containing biomass data.
#'
#' @return A tibble filtered and transformed with selected columns for
#' semi-natural agroecosystems.
#' @keywords internal
#' @noRd
.aggregate_seminatural_system <- function(
  npp_ygpit_merged
) {
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

#' @title Livestock Production -------------------------------------------------
#'
#' @param livestock_prod_ygps A data frame including livestock production data.
#'
#' @return A data frame formatted for integration with other production data.
#' @keywords internal
#' @noRd
.prepare_livestock_production <- function(
  livestock_prod_ygps
) {
  livestock <- livestock_prod_ygps |>
    dplyr::select(
      Year, Province_name, Item, Name_biomass, Prod_Mg
    ) |>
    dplyr::mutate(Box = "Livestock")

  livestock
}

#' @title Combine Cropland, Semi_natural_agroecosystems and Livestock ----------
#'
#' @param crops_residues_grazed Data frame of crop production.
#' @param semi_natural_agroecosystems Data frame of production from semi-natural
#' agroecosystems.
#' @param livestock Data frame of livestock production.
#'
#' @return Combined data frame of all production systems.
#' @keywords internal
#' @noRd
.combine_production_boxes <- function(
  crops_residues_grazed,
  semi_natural_agroecosystems,
  livestock
) {
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

#' @title Seed production per province, based on national seed share per Area
#' @description Calculates the amount of seeds used per province and subtracts
#' it from total production.
#'
#' @param crop_area_npp_ygpit_all Data frame containing crop data by province.
#' @param pie_full_destinies_fm Data frame containing domestic supply by
#' destiny, including seed usage.
#' @param grafs_prod_combined Data frame with total production values.
#'
#' @return A data frame with production values after subtracting seed usage.
#' @keywords internal
#' @noRd
.remove_seeds_from_system <- function(
  crop_area_npp_ygpit_all,
  pie_full_destinies_fm,
  grafs_prod_combined
) {
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

#' @title Structuring dataset (GrazedWeeds und Used_Residues in ProductionFM)
#' @description Rename Prod_Residue_Product_Mg to Production_FM and replace
#' Production_FM with GrazedWeeds_MgDM (for Fallow).
#'
#' @param grafs_prod_combined_no_seeds Data frame of production without seeds.
#' @param biomass_coefs Data frame containing conversion coefficients for
#' biomass (FM to DM and DM to N).
#'
#' @return A data frame with added grass and wood production.
#' @keywords internal
#' @noRd
.adding_grass_wood <- function(
  grafs_prod_combined_no_seeds,
  biomass_coefs
) {
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
    dplyr::select(
      Year, Province_name, Name_biomass, Item, Production_FM, Box
    )

  # Combine the two into one dataframe
  holm_oak_extra <- dplyr::bind_rows(holm_oak_grass, holm_oak_firewood)

  # Create Firewood rows for other biomass types using Used_Residue_MgFM
  firewood_extra <- grafs_prod_structured |>
    dplyr::filter(Name_biomass %in% c(
      "Conifers", "Holm oak forest", "Mediterranean shrubland"
    )) |>
    dplyr::filter(!is.na(Used_Residue_MgFM) & Used_Residue_MgFM > 0) |>
    dplyr::mutate(
      Item = "Firewood",
      Production_FM = Used_Residue_MgFM
    ) |>
    dplyr::select(
      Year, Province_name, Name_biomass, Item, Production_FM, Box
    )

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
    dplyr::select(
      Year, Province_name, Name_biomass, Item, Production_FM, Box
    )

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

#' @title Processed Items ------------------------------------------------------
#' @description Summarise processed items by Year, Province, Biomass,
#' Item & ProcessedItem.
#'
#' @param processed_prov_fixed Data frame containing data for processed items.
#'
#' @return A data frame with processed item values structured for integration.
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
      Production_FM = ProcessedItem_amount,
      Box = "Cropland"
    ) |>
    dplyr::select(
      Year, Province_name, Name_biomass, Item, Box, Production_FM
    )

  processed_data
}

#' @title Match structure of grafs_prod_combined_no_seeds ----------------------
#' @description Combines grass,wood and processed item data into a unified
#' structure and merges biomass names.
#'
#' @param grafs_prod_added_grass_wood Data with added grass and wood production.
#' @param processed_data Data frame with processed item values.
#' @param codes_coefs_items_full Data frame with item-to-biomass names.
#'
#' @return A unified data frame with complete production data for items.
#' @keywords internal
#' @noRd
.prepare_prod_data <- function(
  grafs_prod_added_grass_wood,
  processed_data,
  codes_coefs_items_full
) {
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

#' @title Convert Fresh Matter (FM) to Dry Matter (DM) and finally to N
#' @description Define a list of special items that require using the primary
#' biomass name for selecting conversion coefficients.
#'
#' @param added_grass_wood_merged Data frame with production values and biomass.
#' @param biomass_coefs Data frame with FM→DM and DM→N conversion coefficients
#' for each biomass.
#'
#' @return A data frame with total dry matter and N production.
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
      biomass_coefs |> dplyr::select(Name_biomass, Product_kgDM_kgFM),
      by = c("Biomass_match" = "Name_biomass")
    ) |>
    dplyr::mutate(
      Production_DM = Production_FM * Product_kgDM_kgFM
    )

  # Join with DM to N conversion factors using Biomass_match, then calculate
  # N production
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

#' @title Consumption (Destinies) ---------------------------------------------
#'
#' @description Intake Livestock: sum all data (FM_Mg) for the same Year,
#' Province_name, Item.
#' Comment!!! Feed from all animals are summed together, also from pets.
#' Do they have to be assigned to humans?
#'
#' @param feed_intake A data frame with feed intake data in FM.
#'
#' @return A data frame with the total FM_Mg per year, province, and item.
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

#' @title Popoulation
#' @description Use column Pop_Mpeop_yg. Calculate the share of population
#' (population in each province divided through whole population in
#' Spain to get the share).
#'
#' @param population_share A data frame with population data.
#'
#' @return A data frame including population shares.
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
#' @param pie_full_destinies_fm A data frame containing domestic supply food.
#' @param population_share A data frame containing population share by province.
#'
#' @return A data frame including food consumption per province and item.
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
#' @param pie_full_destinies_fm A data frame containing domestic supply.
#' other uses.
#' @param population_share A data frame containing population share by province.
#'
#' @return A data frame including other uses per province and item.
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
#' @param grafs_prod_item Data frame production data for items.
#' @param feed_intake Feed intake values per province and item.
#' @param food_with_share Food values per province and item.
#' @param other_uses_with_share Other uses per province and item.
#'
#' @return A combined data frame with food, feed, and other uses.
#' @keywords internal
#' @noRd
.combine_destinies <- function(
  grafs_prod_item,
  feed_intake,
  food_with_share,
  other_uses_with_share
) {
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

#' @title Finalizing data
#' @description Final merging of Item and Name_biomass and converting FM to DM,
#' and DM to N.
#'
#' @param grafs_prod_item_combined Data frame with FM values for food, feed,
#' and other uses.
#' @param codes_coefs_items_full Data frame linking items to biomass names.
#' @param biomass_coefs Data frame including conversion factors
#' (DM/FM and N/DM).
#'
#' @return A data frame with food, feed, and other uses in MgN.
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

#' @title Consumption and Trade
#' @description Calculation of consumption by destiny and trade
#' (export, import).
#'
#' @param grafs_prod_item_n A data frame with N values (MgN) by destiny.
#'
#' @return A data frame with consumption, exports, and imports in MgN.
#' @keywords internal
#' @noRd
.calculate_trade <- function(
  grafs_prod_item_n
) {
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

#' @title Finalize production and destiny output -------------------------------
#' @description Fills in missing box categories and transforms data into a
#' long format.
#'
#' @param grafs_prod_item_trade A data frame containing N trade data.
#' @param codes_coefs_items_full A data frame used to assign items to biomasses.
#'
#' @return A final data frame with N data by year, province, box, destiny.
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
    dplyr::select(
      Year, Province_name, Item, Box, Destiny, MgN
    )

  readr::write_csv(grafs_prod_destiny_final, "grafs_prod_destiny.csv")

  grafs_prod_destiny_final
}
