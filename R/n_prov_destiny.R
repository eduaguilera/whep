#' @title GRAFS Nitrogen (N) flows
#'
#' @description
#' Provides N flows of the spanish agro-food system on a provincial level
#' between 1860 and 2020. This dataset is the the base of the GRAFS model and
#' contains data in megagrams of N (MgN) for each year, province, item, origin
#' and destiny. Thereby, the origin column represents where N comes from, which
#' includes N soil inputs, imports and production. The destiny column shows
#' where N goes to, which includes export, population food, population other
#' uses and feed or cropland (in case of N soil inputs).
#' Processed items, residues, woody crops, grazed weeds are taken into account.
#'
#' @return
#' A final tibble containing N flow data by origin and destiny.
#' It includes the following columns:
#'   - `year`: The year in which the recorded event occurred.
#'   - `province_name`: The Spanish province where the data is from.
#'   - `item`: The item which was produced, defined in `names_biomass_cb`.
#'   - `irrig_cat`: Irrigation form (irrigated or rainfed)
#'   - `box`: One of the GRAFS model systems: cropland,
#'   Semi-natural agroecosystems, Livestock, Fish, or Agro-industry.
#'   - `origin`: The origin category of N: Cropland,
#'   Semi-natural agroecosystems, Livestock, Fish, Agro-industry, Deposition,
#'   Fixation, Synthetic, People (waste water), Livestock (manure).
#'   - `destiny`: The destiny category of N: population_food,
#'   population_other_uses, livestock_mono, livestock_rum (feed), export,
#'   Cropland (for N soil inputs).
#'   - `MgN`: Nitrogen amount in megagrams (Mg).
#'
#' @export
create_n_prov_destiny <- function() {
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
    whep_read_file("n_balance_ygpit_all"),
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

  prod_destiny
}

#' @title GRAFS Nitrogen (N) flows – National Spain
#'
#' @description
#' Provides N flows of the Spanish agro-food system on a national level
#' between 1860 and 2020. This dataset is the national equivalent of the
#' provincial GRAFS model and represents Spain as a single system without
#' internal trade between provinces. All production, consumption and soil
#' inputs are aggregated nationally before calculating trade with the
#' outside.
#'
#' @return
#' A final tibble containing national N flow data by origin and destiny.
#'
#' @export
create_n_nat_destiny <- function() {
  prov <- create_n_prov_destiny()

  nat_shares <- prov |>
    dplyr::filter(
      Destiny %in%
        c(
          "population_food",
          "population_other_uses",
          "livestock_rum",
          "livestock_mono"
        )
    ) |>
    dplyr::group_by(Year, Item, Destiny) |>
    dplyr::summarise(MgN = sum(MgN, na.rm = TRUE), .groups = "drop") |>
    dplyr::group_by(Year, Item) |>
    dplyr::mutate(share = MgN / sum(MgN, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::select(Year, Item, Destiny, share)

  nat_core <- prov |>
    dplyr::filter(Origin != "Outside", Destiny != "export") |>
    dplyr::group_by(
      Year,
      Item,
      Irrig_cat,
      Box,
      Origin,
      Destiny
    ) |>
    dplyr::summarise(
      MgN = sum(MgN, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(Province_name = "Spain")

  nat_balance <- prov |>
    dplyr::group_by(Year, Item, Box, Irrig_cat) |>
    dplyr::summarise(
      production = sum(MgN[Origin == Box], na.rm = TRUE),
      consumption = sum(
        MgN[
          Destiny %in%
            c(
              "population_food",
              "population_other_uses",
              "livestock_rum",
              "livestock_mono"
            )
        ],
        na.rm = TRUE
      ),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      export = pmax(production - consumption, 0),
      import = pmax(consumption - production, 0),
      Province_name = "Spain"
    )

  exports <- nat_balance |>
    dplyr::filter(export > 0) |>
    dplyr::transmute(
      Year,
      Province_name,
      Item,
      Irrig_cat,
      Box,
      Origin = Box,
      Destiny = "export",
      MgN = export
    )

  imports <- nat_balance |>
    dplyr::filter(import > 0) |>
    dplyr::left_join(nat_shares, by = c("Year", "Item")) |>
    dplyr::mutate(
      MgN = pmin(import, consumption) * share,
      Origin = "Outside",
      Irrig_cat = NA_character_
    ) |>
    dplyr::filter(MgN > 0) |>
    dplyr::select(
      Year,
      Province_name,
      Item,
      Irrig_cat,
      Box,
      Origin,
      Destiny,
      MgN
    )

  dplyr::bind_rows(
    nat_core,
    exports,
    imports
  ) |>
    dplyr::arrange(Year, Item, Origin, Destiny)
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
      Irrig_cat
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
      Irrig_cat
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
        production_fm = Used_Residue_MgFM
      ) |>
      dplyr::mutate(prod_type = "Residue")
  ) |>
    dplyr::mutate(Box = "semi_natural_agroecosystems")

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
#' COMMENT: in a few cases, seeds are higher then production, so that we get
#' negative values. When the share is over 50%, it is therefore set back to 50%.
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
      Seeds_used_capped = dplyr::if_else(
        dplyr::coalesce(Seeds_used_MgFM, 0) > 0.5 * production_fm,
        0.5 * production_fm,
        dplyr::coalesce(Seeds_used_MgFM, 0)
      ),
      production_fm = production_fm - Seeds_used_capped
    ) |>
    dplyr::select(-Seeds_used_MgFM, -Seeds_used_capped)

  grafs_prod_combined_no_seeds
}

#' @title Structuring dataset (GrazedWeeds and Used_Residues in ProductionFM)
#' @description Replace production_fm with GrazedWeeds_MgDM (for Fallow).
#'
#' @param grafs_prod_combined_no_seeds Dataframe of production without seeds.
#'
#' @return A dataframe with added grass and wood production.
#' @keywords internal
#' @noRd
.add_grass_wood <- function(grafs_prod_combined_no_seeds) {
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
    # 20% DM to FM for Grass
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

  grazed_no_seeds_primary <- added_grass_wood_merged |>
    dplyr::mutate(
      Biomass_match = dplyr::if_else(
        Item %in% special_items,
        Name_biomass_primary,
        Name_biomass
      )
    )

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
    dplyr::select(-Name_biomass) |>
    dplyr::select(
      Year,
      Province_name,
      Item,
      Box,
      LandUse,
      Irrig_cat,
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
      prod_type
    ) |>
    dplyr::summarise(
      production_n = sum(production_n, na.rm = TRUE),
      .groups = "drop"
    )

  grafs_prod_item <- prod_grazed_no_seeds_n |>
    dplyr::group_by(
      Year,
      Province_name,
      Item,
      Box,
      LandUse,
      Irrig_cat,
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
#' Province_name, Item. Calculates feed shares for ruminant and monogastric
#' animals.
#'
#' @param feed_intake A dataframe with feed intake data in FM.
#'
#' @return A dataframe with the total FM_Mg per year, province, and item.
#' @keywords internal
#' @noRd
.adding_feed <- function(feed_intake) {
  feed_intake <- feed_intake |>
    dplyr::mutate(
      Livestock_type = dplyr::case_when(
        Livestock_cat %in%
          c(
            "Cattle_meat",
            "Cattle_milk",
            "Goats",
            "Sheep",
            "Donkeys_mules",
            "Horses"
          ) ~
          "ruminant",
        Livestock_cat %in%
          c("Pigs", "Poultry", "Rabbits", "Fur animals", "Other") ~
          "monogastric"
      )
    ) |>
    dplyr::group_by(Year, Province_name, Item, Livestock_type) |>
    dplyr::summarise(
      feed = sum(FM_Mg[Livestock_cat != "Pets"], na.rm = TRUE),
      food_pets = sum(FM_Mg[Livestock_cat == "Pets"], na.rm = TRUE),
      .groups = "drop"
    )

  feed_share_rum_mono <- feed_intake |>
    tidyr::pivot_wider(
      names_from = Livestock_type,
      values_from = feed,
      values_fill = 0
    ) |>
    dplyr::mutate(
      feed_total = ruminant + monogastric,
      share_rum = dplyr::if_else(feed_total > 0, ruminant / feed_total, 0),
      share_mono = dplyr::if_else(feed_total > 0, monogastric / feed_total, 0)
    ) |>
    dplyr::select(Year, Province_name, Item, share_rum, share_mono)

  list(
    feed_intake = feed_intake,
    feed_share_rum_mono = feed_share_rum_mono
  )
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
  population_share,
  pie_full_destinies_fm
) {
  total_food_other_uses <- pie_full_destinies_fm |>
    dplyr::filter(
      Destiny %in% c("Food", "Other_uses"),
      Element == "Domestic_supply"
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

  prov_food_other_uses <- total_food_other_uses |>
    dplyr::left_join(
      population_share,
      by = "Year",
      relationship = "many-to-many"
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

  prov_food_other_uses
}

#' @title Combine all destinies ------------------------------------------------
#' @description Merges food, feed, and other uses into one dataset.
#'
#' @param grafs_prod_item Dataframe production data for items.
#' @param feed_intake Feed intake values per province and item.
#' @param prov_food_other_uses Food and Other uses per province and item.
#'
#' @return A combined dataframe with food, feed, and other uses.
#' @keywords internal
#' @noRd
#'
.combine_destinies <- function(
  grafs_prod_item,
  feed_intake,
  prov_food_other_uses
) {
  grafs_prod_item_sum <- grafs_prod_item |>
    dplyr::group_by(Year, Province_name, Item, Box, Irrig_cat) |>
    dplyr::summarise(
      production_n = sum(production_n, na.rm = TRUE),
      .groups = "drop"
    )

  # Pre-calculate production totals to avoid expensive group_by later
  production_totals <- grafs_prod_item_sum |>
    dplyr::group_by(Year, Province_name, Item) |>
    dplyr::summarise(
      production_total = sum(production_n, na.rm = TRUE),
      .groups = "drop"
    )

  feed_clean <- feed_intake |>
    dplyr::group_by(Year, Province_name, Item) |>
    dplyr::summarise(
      feed = sum(feed, na.rm = TRUE),
      food_pets = sum(food_pets, na.rm = TRUE),
      .groups = "drop"
    )

  prov_food_other_uses_clean <- prov_food_other_uses |>
    dplyr::group_by(Year, Province_name, Item) |>
    dplyr::summarise(
      food = sum(food, na.rm = TRUE),
      other_uses = sum(other_uses, na.rm = TRUE),
      .groups = "drop"
    )

  # Feed for pets is assigned to food, therefore we have for example DDGS in
  # human consumption
  grafs_prod_item_combined <- grafs_prod_item_sum |>
    dplyr::full_join(
      prov_food_other_uses_clean,
      by = c("Year", "Province_name", "Item")
    ) |>
    dplyr::full_join(
      feed_clean,
      by = c("Year", "Province_name", "Item")
    ) |>
    dplyr::left_join(
      production_totals,
      by = c("Year", "Province_name", "Item")
    ) |>
    dplyr::mutate(
      food = dplyr::coalesce(food, 0) + dplyr::coalesce(food_pets, 0),
      feed = dplyr::coalesce(feed, 0),
      other_uses = dplyr::coalesce(other_uses, 0),
      production_n = dplyr::coalesce(production_n, 0),
      production_total = dplyr::coalesce(production_total, 0)
    ) |>
    dplyr::select(-food_pets)

  # calculating production shares to distinguish between consumption of e.g.
  # rainfed vs. irrigated crops
  grafs_prod_item_combined <- grafs_prod_item_combined |>
    dplyr::mutate(
      production_share = dplyr::if_else(
        production_total > 0,
        production_n / production_total,
        1
      ),
      food = food * production_share,
      feed = feed * production_share,
      other_uses = other_uses * production_share
    ) |>
    dplyr::select(
      -production_total,
      -production_share
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
  grafs_prod_item_combined = whep_read_file(""),
  codes_coefs_items_full = whep_read_file("codes_coefs_items_full"),
  biomass_coefs = whep_read_file("biomass_coefs")
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
        prod_type %in% c("Residue", "Grass") ~
          value_fm * Residue_kgDM_kgFM * Residue_kgN_kgDM,
        prod_type == "Product" ~
          value_fm * Product_kgDM_kgFM * Product_kgN_kgDM,
        TRUE ~ NA_real_
      )
    ) |>
    dplyr::select(-value_fm) |>
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
      Irrig_cat,
      production_n,
      food,
      other_uses,
      feed
    )
}

#' @title Consumption and Trade
#' @description Calculation of consumption by destiny and trade
#' (export, import). National scaling can be activated, for analysis for whole
#' Spain. It should be deactivated for provincial analysis
#'
#' @param grafs_prod_item_n A dataframe with N values (MgN) by destiny.
#' @param pie_full_destinies_fm A data frame with destiny data.
#' @param biomass_coefs A data frame with biomass coefficients.
#' @param codes_coefs_items_full A lookup table with coefficients.
#'
#' @return A dataframe with consumption, exports, and imports in MgN.
#' @keywords internal
#' @noRd
.calculate_trade <- function(
  grafs_prod_item_n,
  pie_full_destinies_fm,
  biomass_coefs,
  codes_coefs_items_full
) {
  trade_data <- grafs_prod_item_n |>
    dplyr::group_by(Year, Province_name, Item, Box, Irrig_cat) |>
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
      Irrig_cat,
      food,
      other_uses,
      feed,
      production_n,
      export,
      import
    )

  trade_data
}


#' @title Prepare final dataset
#' @description Assigns Box to item groups and Irrig_cat to Cropland.
#' @param grafs_prod_item_trade A dataset containing consumptiom and trade data.
#' @param codes_coefs_items_full A dataset linking items to groups for Box
#' assignment.
#' @return A dataframe with Box and Irrig_cat columns assigned.
#' @keywords internal
#' @noRd
.prep_final_ds <- function(grafs_prod_item_trade, codes_coefs_items_full) {
  grafs_prod_item_trade |>
    dplyr::left_join(
      dplyr::select(codes_coefs_items_full, item, group),
      by = c("Item" = "item")
    ) |>
    dplyr::mutate(
      group = dplyr::recode(group, "Additives" = "Agro-industry"),
      Box = dplyr::case_when(
        Item == "Acorns" ~ "semi_natural_agroecosystems",
        is.na(Box) & Item == "Fallow" ~ "Cropland",
        is.na(Box) &
          group %in% c("Crop products", "Primary crops", "crop residue") ~
          "Cropland",
        is.na(Box) & group %in% c("Livestock products", "Livestock") ~
          "Livestock",
        group %in% c("Agro-industry", "Fish") ~ group,
        TRUE ~ Box
      ),
      Irrig_cat = dplyr::if_else(Box == "Cropland", Irrig_cat, NA_character_)
    ) |>
    dplyr::select(-group)
}


#' @title Calculate consumption shares
#' @description Calculates food, feed, and other uses shares for each item.
#' @param grafs_prod_destiny_final A dataset containing consumption and trade
#' per item, province, origin, and destiny.
#' @return A dataset with total consumption and consumption shares for food,
#' other uses, and feed.
#' @keywords internal
#' @noRd
.calculate_consumption_shares <- function(grafs_prod_destiny_final) {
  grafs_prod_destiny_final |>
    dplyr::mutate(
      consumption_total = food + other_uses + feed,
      food_share = dplyr::if_else(
        consumption_total > 0,
        food / consumption_total,
        0
      ),
      other_uses_share = dplyr::if_else(
        consumption_total > 0,
        other_uses / consumption_total,
        0
      ),
      feed_share = dplyr::if_else(
        consumption_total > 0,
        feed / consumption_total,
        0
      )
    ) |>
    dplyr::select(
      Year,
      Province_name,
      Item,
      Irrig_cat,
      consumption_total,
      food_share,
      other_uses_share,
      feed_share
    )
}

#' @title Split local consumption
#' @description Splits local consumption into population food, other uses,
#' and livestock. Livestock feed is split into livestock_rum (ruminants) and
#' livestock_mono (monogastric).
#' @param local_vs_import A dataset containing local and imported consumption.
#' @param feed_share_rum_mono A dataset with feed shares between ruminants
#' and monogastric animals.
#' @return A dataset with consumption split into population_food,
#' livestock_rum, livestock_mono, and population_other_uses.
#' @keywords internal
#' @noRd
.split_local_consumption <- function(local_vs_import, feed_share_rum_mono) {
  local_vs_import |>
    tidyr::pivot_longer(
      cols = c(food_share, feed_share, other_uses_share),
      names_to = "share_type",
      values_to = "share"
    ) |>
    dplyr::mutate(
      MgN = local_consumption * share,
      Destiny = dplyr::case_when(
        share_type == "food_share" ~ "population_food",
        share_type == "feed_share" ~ "livestock",
        share_type == "other_uses_share" ~ "population_other_uses"
      ),
      Origin = Box
    ) |>
    dplyr::group_by(
      Year,
      Province_name,
      Item,
      Irrig_cat,
      Box,
      Origin,
      Destiny
    ) |>
    dplyr::summarise(MgN = sum(MgN, na.rm = TRUE), .groups = "drop") |>
    dplyr::left_join(
      feed_share_rum_mono,
      by = c("Year", "Province_name", "Item")
    ) |>
    dplyr::mutate(
      MgN_rum = dplyr::if_else(Destiny == "livestock", MgN * share_rum, MgN),
      MgN_mono = dplyr::if_else(Destiny == "livestock", MgN * share_mono, 0)
    ) |>
    tidyr::pivot_longer(
      cols = c(MgN_rum, MgN_mono),
      names_to = "Destiny_feed",
      values_to = "MgN_feed"
    ) |>
    dplyr::mutate(
      Destiny = dplyr::case_when(
        Destiny == "livestock" & Destiny_feed == "MgN_rum" ~ "livestock_rum",
        Destiny == "livestock" & Destiny_feed == "MgN_mono" ~ "livestock_mono",
        TRUE ~ Destiny
      ),
      MgN = MgN_feed
    ) |>
    dplyr::select(-share_rum, -share_mono, -Destiny_feed, -MgN_feed)
}

#' @title Split imported consumption
#' @description Splits imports by consumption and assigns origins.
#' Livestock feed is split into livestock_rum (ruminants) and livestock_mono
#' (monogastric).
#' COMMENT: pmin prevents imported N for food and other uses from becoming
#' unrealistically high.
#' For human consumption, imports usually replace local supply instead of
#' adding to it. So I limited imported food and other uses to the smaller
#' value of imports or local use with pmin. Feed is treated differently because
#' imports can exceed local production. Fish and Agro-industry are excluded in
#' pmin because all of these values are considered as imports.
#' @param local_vs_import A dataset containing local and import consumption.
#' @param feed_share_rum_mono A dataset with feed shares split into ruminants
#' and monogastric animals.
#' @return A dataset with imported consumption, split into population_food,
#' livestock_rum, livestock_mono, and population_other_uses.
#' @keywords internal
#' @noRd
.split_import_consumption <- function(local_vs_import, feed_share_rum_mono) {
  local_vs_import |>
    dplyr::mutate(Box = Box) |>
    tidyr::pivot_longer(
      cols = c(food_share, feed_share, other_uses_share),
      names_to = "share_type",
      values_to = "share"
    ) |>
    dplyr::mutate(
      MgN = dplyr::case_when(
        Box %in%
          c("Fish", "Agro-industry") &
          share_type %in% c("food_share", "other_uses_share") ~
          import_consumption * share,
        share_type %in% c("food_share", "other_uses_share") ~
          pmin(import_consumption, local_consumption) * share,
        TRUE ~ import_consumption * share
      ),
      Destiny = dplyr::case_when(
        share_type == "food_share" ~ "population_food",
        share_type == "feed_share" ~ "livestock",
        share_type == "other_uses_share" ~ "population_other_uses"
      ),
      Origin = "Outside",
      Irrig_cat = NA_character_
    ) |>
    dplyr::group_by(
      Year,
      Province_name,
      Item,
      Irrig_cat,
      Box,
      Origin,
      Destiny
    ) |>
    dplyr::summarise(MgN = sum(MgN, na.rm = TRUE), .groups = "drop") |>
    dplyr::left_join(
      feed_share_rum_mono,
      by = c("Year", "Province_name", "Item")
    ) |>
    dplyr::mutate(
      MgN_rum = dplyr::if_else(Destiny == "livestock", MgN * share_rum, MgN),
      MgN_mono = dplyr::if_else(Destiny == "livestock", MgN * share_mono, 0)
    ) |>
    tidyr::pivot_longer(
      cols = c(MgN_rum, MgN_mono),
      names_to = "Destiny_feed",
      values_to = "MgN_feed"
    ) |>
    dplyr::mutate(
      Destiny = dplyr::case_when(
        Destiny == "livestock" & Destiny_feed == "MgN_rum" ~ "livestock_rum",
        Destiny == "livestock" & Destiny_feed == "MgN_mono" ~ "livestock_mono",
        TRUE ~ Destiny
      ),
      MgN = MgN_feed
    ) |>
    dplyr::select(-share_rum, -share_mono, -Destiny_feed, -MgN_feed)
}

#' @title Adding exports
#' @description Adds exports to the final dataset.
#' @param grafs_prod_destiny_final A dataset containing consumption and trade.
#' @return A dataset with added exports for each item and province.
#' @keywords internal
#' @noRd
.add_exports <- function(grafs_prod_destiny_final) {
  grafs_prod_destiny_final |>
    dplyr::transmute(
      Year,
      Province_name,
      Item,
      Irrig_cat,
      Destiny = "export",
      MgN = export,
      Origin = Box,
      Box = Box
    ) |>
    dplyr::group_by(
      Year,
      Province_name,
      Item,
      Irrig_cat,
      Box,
      Origin,
      Destiny
    ) |>
    dplyr::summarise(MgN = sum(MgN, na.rm = TRUE), .groups = "drop")
}


#' @title Finalize N flow output
#' @description Combines consumption, import, and export N flows.
#' @param grafs_prod_item_trade A dataset containing trade data.
#' @param codes_coefs_items_full An excel linking items to groups for
#' classification.
#' @param n_soil_inputs A dataset containing N soil inputs.
#' @param feed_share_rum_mono A dataset containing feed shares between ruminant
#' and monogastric animals.
#' @return A dataset containing the final nitrogen flows (MgN)  by
#' year, province, item, irrigation category, Box, origin, and destiny.
#' Includes local consumption, imports, and exports.
#' @keywords internal
#' @noRd
.finalize_prod_destiny <- function(
  grafs_prod_item_trade,
  codes_coefs_items_full,
  n_soil_inputs,
  feed_share_rum_mono
) {
  grafs_prod_destiny_final <- .prep_final_ds(
    grafs_prod_item_trade,
    codes_coefs_items_full
  )
  shares_import <- .calculate_consumption_shares(grafs_prod_destiny_final)

  local_vs_import <- grafs_prod_destiny_final |>
    dplyr::left_join(
      shares_import,
      by = c("Year", "Province_name", "Item", "Irrig_cat")
    ) |>
    dplyr::mutate(
      local_consumption = pmin(production_n, consumption_total),
      import_consumption = consumption_total - local_consumption
    )

  local_split <- .split_local_consumption(local_vs_import, feed_share_rum_mono)
  imports_split <- .split_import_consumption(
    local_vs_import,
    feed_share_rum_mono
  )
  exports <- .add_exports(grafs_prod_destiny_final)

  dplyr::bind_rows(local_split, imports_split, exports) |>
    dplyr::filter(MgN > 0) |>
    dplyr::group_by(
      Year,
      Province_name,
      Item,
      Irrig_cat,
      Box,
      Origin,
      Destiny
    ) |>
    dplyr::summarise(MgN = sum(MgN, na.rm = TRUE), .groups = "drop")
}

#' @title Add soil N inputs
#' @description Transforms soil N inputs (deposition, fixation, synthetic,
#' manure, urban) into long format and adds them to the production-destiny
#' dataframe.
#'
#' @param grafs_prod_destiny_final A tibble from `.finalize_prod_destiny()`
#'   containing destinies.
#' @param n_soil_inputs A dataframe with soil inputs.
#'
#' @return The input dataframe extended with soil N input flows.
#' @keywords internal
#' @noRd
.add_n_soil_inputs <- function(grafs_prod_destiny_final, soil_inputs) {
  soil_inputs_long <- soil_inputs |>
    tidyr::pivot_longer(
      cols = c(deposition, fixation, synthetic, manure, urban),
      names_to = "Origin",
      values_to = "MgN"
    ) |>
    dplyr::mutate(
      Destiny = dplyr::case_when(
        Origin %in% c("deposition", "fixation", "synthetic") ~ Box,
        Origin == "manure" ~ Box,
        Origin == "urban" ~ Box
      ),
      Origin = dplyr::case_when(
        Origin == "deposition" ~ "Deposition",
        Origin == "fixation" ~ "Fixation",
        Origin == "synthetic" ~ "Synthetic",
        Origin == "manure" ~ "Livestock",
        Origin == "urban" ~ "People"
      ),
      Box = Destiny
    ) |>
    dplyr::select(Year, Province_name, Item, Irrig_cat, Origin, Destiny, MgN)

  dplyr::bind_rows(
    grafs_prod_destiny_final,
    soil_inputs_long
  ) |>
    dplyr::filter(MgN != 0) |>
    dplyr::arrange(Year, Province_name, Item, Irrig_cat, Origin, Destiny)
}
