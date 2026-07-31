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
#' @param example If `TRUE`, return a small example output without downloading
#'   remote data. Default is `FALSE`.
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
#'   - `mg_n`: Nitrogen amount in megagrams (Mg).
#'
#' @export
#'
#' @examples
#' create_n_prov_destiny(example = TRUE)
create_n_prov_destiny <- function(example = FALSE) {
  if (example) {
    return(.example_create_n_prov_destiny())
  }
  codes_coefs_items_full <- whep_read_file("codes_coefs_items_full")
  biomass_coefs <- whep_read_file("biomass_coefs")
  pie_full_destinies_fm <- whep_read_file("pie_full_destinies_fm")
  processing_coefs <- whep_read_file("processing_coefs")
  livestock_prod_ygps <- whep_read_file("stock_prod_ygps")
  crop_area_npp_no_fallow <- whep_read_file("crop_area_npp_ygpitr_no_fallow")
  npp_ygpit <- whep_read_file("npp_ygpit")
  codes_coefs <- whep_read_file("codes_coefs")
  intake_ygiac <- whep_read_file("intake_ygiac")
  population_yg <- whep_read_file("population_yg")
  n_balance_ygpit_all <- whep_read_file("n_balance_ygpit_all") |>
    dplyr::filter(Year <= 2021)

  biomass_item_merged <- .merge_items_biomass(npp_ygpit, codes_coefs)
  n_soil_inputs <- .calculate_n_soil_inputs(n_balance_ygpit_all, codes_coefs)

  livestock_product_items <- codes_coefs_items_full |>
    dplyr::filter(group %in% c("Livestock products", "Livestock")) |>
    dplyr::pull(item)

  add_feed_output <- .add_feed(
    intake_ygiac |>
      dplyr::filter(!item_cbs %in% livestock_product_items)
  )

  prod_combined_boxes <- biomass_item_merged |>
    .aggregate_crop_seminatural(
      .summarise_crops_residues(crop_area_npp_no_fallow)
    ) |>
    .combine_production_boxes(
      .prepare_livestock_production(livestock_prod_ygps)
    )

  national_production <- .national_item_production(prod_combined_boxes)
  first_year <- min(national_production$Year, na.rm = TRUE)

  spain_coefs_observed <- .spain_processing_coefs(processing_coefs)
  spain_coefs <- .backfill_processing_cf(spain_coefs_observed, first_year)
  processing_shares <- .calculate_processing_shares(
    spain_coefs_observed,
    national_production
  ) |>
    .backfill_processing_shares(first_year)

  processed <- .calculate_processed_amounts(
    prod_combined_boxes,
    processing_shares,
    spain_coefs,
    coefs = list(
      items = codes_coefs_items_full,
      biomass = biomass_coefs
    )
  )

  food_and_other_uses <- population_yg |>
    .calculate_population_share() |>
    .calculate_food_and_other_uses(pie_full_destinies_fm)

  grafs_prod_item_trade <- biomass_item_merged |>
    .remove_seeds_from_system(
      pie_full_destinies_fm,
      processed$non_processed
    ) |>
    .add_grass_wood() |>
    .prepare_prod_data(
      processed$processed_items,
      codes_coefs_items_full
    ) |>
    .convert_fm_dm_n(biomass_coefs) |>
    .combine_destinies(add_feed_output$feed_intake, food_and_other_uses) |>
    .convert_to_items_n(codes_coefs_items_full, biomass_coefs) |>
    .calculate_trade() |>
    .finalize_prod_destiny(
      codes_coefs_items_full,
      n_soil_inputs,
      add_feed_output$feed_share_rum_mono
    ) |>
    .add_n_soil_inputs(n_soil_inputs) |>
    dplyr::select(
      year = Year,
      province_name = Province_name,
      item = Item,
      irrig_cat = Irrig_cat,
      box = Box,
      origin = Origin,
      destiny = Destiny,
      mg_n = MgN
    )
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
#' @param example If `TRUE`, return a small example output without downloading
#'   remote data. Default is `FALSE`.
#'
#' @return
#' A final tibble containing national N flow data by origin and destiny.
#' It includes the following columns:
#'   - `year`: The year in which the recorded event occurred.
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
#'   - `mg_n`: Nitrogen amount in megagrams (Mg).
#'   - `province_name`: Set to "Spain" for all national-level rows.
#'
#' @export
#'
#' @examples
#' create_n_nat_destiny(example = TRUE)
create_n_nat_destiny <- function(example = FALSE) {
  if (example) {
    return(.example_create_n_nat_destiny())
  }
  prov <- create_n_prov_destiny() |>
    dplyr::rename(
      Year = year,
      Province_name = province_name,
      Item = item,
      Irrig_cat = irrig_cat,
      Box = box,
      Origin = origin,
      Destiny = destiny,
      MgN = mg_n
    )

  prov_lookup <- prov |>
    dplyr::group_by(Item, Box, Irrig_cat) |>
    dplyr::summarise(weight = sum(MgN, na.rm = TRUE), .groups = "drop") |>
    dplyr::group_by(Item) |>
    dplyr::slice_max(weight, n = 1, with_ties = FALSE) |>
    dplyr::ungroup()

  nat_production_detail <- prov |>
    dplyr::filter(Origin == Box) |>
    dplyr::group_by(Year, Item, Box, Irrig_cat) |>
    dplyr::summarise(production = sum(MgN, na.rm = TRUE), .groups = "drop")

  nat_production <- nat_production_detail |>
    dplyr::group_by(Year, Item) |>
    dplyr::summarise(
      production = sum(production, na.rm = TRUE),
      .groups = "drop"
    )

  nat_consumption <- prov |>
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
    dplyr::summarise(
      consumption = sum(MgN, na.rm = TRUE),
      .groups = "drop"
    )

  nat_cons_wide <- nat_consumption |>
    tidyr::pivot_wider(
      names_from = Destiny,
      values_from = consumption,
      values_fill = 0
    )

  nat_total_consumption <- nat_consumption |>
    dplyr::group_by(Year, Item) |>
    dplyr::summarise(
      consumption = sum(consumption, na.rm = TRUE),
      .groups = "drop"
    )

  nat_balance <- nat_production |>
    dplyr::full_join(nat_total_consumption, by = c("Year", "Item")) |>
    dplyr::mutate(
      production = dplyr::coalesce(production, 0),
      consumption = dplyr::coalesce(consumption, 0),
      export = pmax(production - consumption, 0),
      import = pmax(consumption - production, 0)
    )

  nat_shares <- nat_cons_wide |>
    dplyr::left_join(nat_production, by = c("Year", "Item")) |>
    dplyr::mutate(
      production = dplyr::coalesce(production, 0),

      food = dplyr::coalesce(population_food, 0),
      other = dplyr::coalesce(population_other_uses, 0),
      feed_rum = dplyr::coalesce(livestock_rum, 0),
      feed_mono = dplyr::coalesce(livestock_mono, 0),
      feed = feed_rum + feed_mono,

      demand = food + other + feed,
      local = pmin(production, demand),

      food_local = dplyr::if_else(demand > 0, local * (food / demand), 0),
      other_local = dplyr::if_else(demand > 0, local * (other / demand), 0),
      feed_local = dplyr::if_else(demand > 0, local * (feed / demand), 0),

      food_gap = pmax(food - food_local, 0),
      other_gap = pmax(other - other_local, 0),
      feed_gap = pmax(feed - feed_local, 0),

      total_gap = food_gap + other_gap + feed_gap,

      share_food = dplyr::if_else(total_gap > 0, food_gap / total_gap, 0),
      share_other = dplyr::if_else(total_gap > 0, other_gap / total_gap, 0),
      share_feed = dplyr::if_else(total_gap > 0, feed_gap / total_gap, 0),

      share_rum = dplyr::if_else(feed > 0, feed_rum / feed, 0),
      share_mono = dplyr::if_else(feed > 0, feed_mono / feed, 0),

      share_feed_rum = share_feed * share_rum,
      share_feed_mono = share_feed * share_mono
    ) |>
    dplyr::select(
      Year,
      Item,
      share_food,
      share_other,
      share_feed_rum,
      share_feed_mono
    ) |>
    tidyr::pivot_longer(
      cols = c(
        share_food,
        share_other,
        share_feed_rum,
        share_feed_mono
      ),
      names_to = "Destiny",
      values_to = "share"
    ) |>
    dplyr::mutate(
      Destiny = dplyr::recode(
        Destiny,
        share_food = "population_food",
        share_other = "population_other_uses",
        share_feed_rum = "livestock_rum",
        share_feed_mono = "livestock_mono"
      )
    ) |>
    dplyr::ungroup()

  imports <- nat_balance |>
    dplyr::filter(import > 0) |>
    dplyr::left_join(nat_shares, by = c("Year", "Item")) |>
    dplyr::mutate(
      share = dplyr::coalesce(share, 0),
      MgN = import * share,
      Province_name = "Spain",
      Origin = "Outside"
    ) |>
    dplyr::left_join(prov_lookup, by = "Item") |>
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

  export_shares <- nat_production_detail |>
    dplyr::group_by(Year, Item) |>
    dplyr::mutate(
      total_production = sum(production, na.rm = TRUE),
      share = dplyr::if_else(
        total_production > 0,
        production / total_production,
        0
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::select(Year, Item, Box, Irrig_cat, share)

  exports <- nat_balance |>
    dplyr::filter(export > 0) |>
    dplyr::left_join(export_shares, by = c("Year", "Item")) |>
    dplyr::mutate(
      Province_name = "Spain",
      Origin = Box,
      Destiny = "export",
      MgN = export * dplyr::coalesce(share, 0)
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

  nat_soil_inputs <- prov |>
    dplyr::filter(
      Origin %in%
        c("Deposition", "Fixation", "Synthetic", "Livestock", "People"),
      Destiny %in% c("Cropland", "semi_natural_agroecosystems")
    ) |>
    dplyr::group_by(Year, Item, Irrig_cat, Box, Origin, Destiny) |>
    dplyr::summarise(MgN = sum(MgN, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(Province_name = "Spain")

  nat_local_total <- nat_balance |>
    dplyr::mutate(local = pmin(production, consumption)) |>
    dplyr::select(Year, Item, local)

  nat_destiny_shares <- nat_consumption |>
    dplyr::left_join(
      nat_total_consumption |> dplyr::rename(total = consumption),
      by = c("Year", "Item")
    ) |>
    dplyr::mutate(
      destiny_share = dplyr::if_else(total > 0, consumption / total, 0)
    ) |>
    dplyr::select(Year, Item, Destiny, destiny_share)

  nat_local_detail <- nat_local_total |>
    dplyr::left_join(nat_destiny_shares, by = c("Year", "Item")) |>
    dplyr::mutate(MgN_local = local * destiny_share) |>
    dplyr::left_join(export_shares, by = c("Year", "Item")) |>
    dplyr::mutate(
      MgN = MgN_local * share,
      Origin = Box,
      Province_name = "Spain"
    ) |>
    dplyr::filter(!is.na(Box), MgN > 0) |>
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

  dplyr::bind_rows(nat_local_detail, nat_soil_inputs, exports, imports) |>
    dplyr::arrange(Year, Item, Origin, Destiny) |>
    dplyr::rename(
      year = Year,
      province_name = Province_name,
      item = Item,
      irrig_cat = Irrig_cat,
      box = Box,
      origin = Origin,
      destiny = Destiny,
      mg_n = MgN
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
    dplyr::rename(Item = item_cbs) |>
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

  semi_natural <- npp_ygpit_merged |>
    dplyr::filter(LandUse != "Cropland") |>
    dplyr::mutate(
      GrazedWeeds_MgDM = GrazedWeeds_MgDM +
        GrazedAcorns_MgDM +
        GrazedFodder_MgDM
    ) |>
    dplyr::select(
      Year,
      Province_name,
      Name_biomass,
      Item,
      LandUse,
      Irrig_cat,
      GrazedWeeds_MgDM,
      Prod_ygpit_Mg,
      Used_Residue_MgFM
    ) |>
    tidyr::pivot_longer(
      cols = c(GrazedWeeds_MgDM, Prod_ygpit_Mg, Used_Residue_MgFM),
      names_to = "prod_source",
      values_to = "production_fm"
    ) |>
    dplyr::mutate(
      prod_type = dplyr::recode(
        prod_source,
        GrazedWeeds_MgDM = "Grass",
        Prod_ygpit_Mg = "Product",
        Used_Residue_MgFM = "Residue"
      )
    ) |>
    dplyr::select(-prod_source) |>
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
    dplyr::rename(Item = item_cbs) |>
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
  cropland_area <- npp_ygpit_merged |>
    dplyr::filter(LandUse == "Cropland") |>
    dplyr::summarise(
      Area_ha = sum(Area_ygpit_ha, na.rm = TRUE),
      .by = c("Year", "Province_name", "Item")
    )

  seed_reference <- pie_full_destinies_fm |>
    dplyr::filter(Element == "Domestic_supply", Destiny == "Seed") |>
    dplyr::summarise(
      Seed_total = sum(Value_destiny, na.rm = TRUE),
      .by = c("Year", "Item")
    ) |>
    dplyr::left_join(
      cropland_area |>
        dplyr::summarise(
          National_area = sum(Area_ha, na.rm = TRUE),
          .by = c("Year", "Item")
        ),
      by = c("Year", "Item")
    ) |>
    dplyr::mutate(
      Seed_rate_per_ha = dplyr::if_else(
        National_area > 0,
        Seed_total / National_area,
        0
      )
    ) |>
    dplyr::select(Year, Item, Seed_rate_per_ha)

  seed_rates <- cropland_area |>
    dplyr::left_join(seed_reference, by = c("Year", "Item")) |>
    dplyr::mutate(
      Seeds_used_MgFM = Area_ha * dplyr::coalesce(Seed_rate_per_ha, 0)
    )

  # Substracting the Seed data from Production in grafs_prod_combined.
  grafs_prod_combined_no_seeds <- grafs_prod_combined |>
    dplyr::left_join(
      seed_rates |>
        dplyr::select(Year, Province_name, Item, Seeds_used_MgFM),
      by = c("Year", "Province_name", "Item")
    ) |>
    dplyr::mutate(
      Seeds_used_MgFM = dplyr::coalesce(Seeds_used_MgFM, 0),
      Seeds_used_capped = dplyr::if_else(
        Seeds_used_MgFM > 0.5 * production_fm,
        0.5 * production_fm,
        Seeds_used_MgFM
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

#' @title Spain processing coefficients -----------------------------------------
#' @description Filters the cached national processing-coefficients pin
#' (`get_processing_coefs()`) down to Spain, using its own item names — the
#' pin's `ProcessedItem` column is actually the primary/input item (e.g.
#' `"Cottonseed"`) and its `item` column is the processed output (e.g.
#' `"Cottonseed Cake"`), so they are relabelled here to the Item/ProcessedItem
#' convention used throughout this file. `value_to_process` (the amount of
#' the primary item consumed by processing) isn't stored directly, but
#' `value_proc = value_to_process * cf`, so it is recovered by division.
#'
#' @param processing_coefs Output of `get_processing_coefs()`.
#'
#' @return A dataframe with Year, Item, ProcessedItem, value_to_process, cf.
#' @keywords internal
#' @noRd
.spain_processing_coefs <- function(processing_coefs) {
  processing_coefs |>
    dplyr::filter(area == "Spain", !is.na(cf), cf > 0) |>
    dplyr::transmute(
      Year,
      Item = ProcessedItem,
      ProcessedItem = item,
      value_to_process = value_proc / cf,
      cf
    )
}

#' @title National production by item ---------------------------------------
#' @description Sums province-level production to a national total per item
#' and year, used as the denominator for processing shares.
#'
#' @param prod_combined_boxes Dataframe with production_fm by province.
#'
#' @return A dataframe with Year, Item, national_production_fm.
#' @keywords internal
#' @noRd
.national_item_production <- function(prod_combined_boxes) {
  prod_combined_boxes |>
    dplyr::summarise(
      national_production_fm = sum(production_fm, na.rm = TRUE),
      .by = c(Year, Item)
    )
}

#' @title Processing shares by item -------------------------------------------
#' @description Computes the national fraction of each item's production
#' that goes to processing, capped at 1. An item can yield several
#' ProcessedItem outputs, each with its own (near-identical, up to rounding)
#' estimate of `value_to_process` — these are averaged down to a single
#' value per Year/Item so downstream joins don't duplicate rows.
#'
#' @param spain_coefs Output of `.spain_processing_coefs()`.
#' @param national_production Output of `.national_item_production()`.
#'
#' @return A dataframe with Year, Item, share_processing.
#' @keywords internal
#' @noRd
.calculate_processing_shares <- function(
  spain_coefs,
  national_production
) {
  spain_coefs |>
    dplyr::summarise(
      value_to_process = mean(value_to_process, na.rm = TRUE),
      .by = c(Year, Item)
    ) |>
    dplyr::left_join(national_production, by = c("Year", "Item")) |>
    dplyr::mutate(
      national_production_fm = dplyr::coalesce(national_production_fm, 0),
      share_processing = dplyr::if_else(
        national_production_fm > 0,
        pmin(value_to_process / national_production_fm, 1),
        0
      )
    ) |>
    dplyr::select(Year, Item, share_processing)
}

#' @title Backfill early-year processing shares -------------------------------
#' @description `processing_coefs` only covers years from 1961 onward. For
#' each Item, its earliest observed share_processing (1961) is copied back
#' to every year from `first_year` up to (but not including) that first
#' observed year, matching the original workflow's assumption that
#' pre-1961 processing behaved like 1961.
#'
#' @param processing_shares Output of `.calculate_processing_shares()`.
#' @param first_year Earliest year present in the production data.
#'
#' @return `processing_shares` extended with backfilled early-year rows.
#' @keywords internal
#' @noRd
.backfill_processing_shares <- function(processing_shares, first_year) {
  earliest <- processing_shares |>
    dplyr::group_by(Item) |>
    dplyr::slice_min(Year, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::filter(Year > first_year)

  backfilled <- earliest |>
    dplyr::mutate(backfill_years = purrr::map2(first_year, Year - 1, seq)) |>
    tidyr::unnest(backfill_years) |>
    dplyr::mutate(Year = backfill_years) |>
    dplyr::select(-backfill_years)

  dplyr::bind_rows(processing_shares, backfilled)
}

#' @title Backfill early-year processing cf/output mapping --------------------
#' @description `processing_coefs` only covers years from 1961 onward. For
#' each Item/ProcessedItem pair, its earliest observed row (1961) is copied
#' back to every year from `first_year` up to (but not including) that
#' first observed year. Without this, pre-1961 processed mass (subtracted
#' via the backfilled share) would have no ProcessedItem to convert into,
#' silently disappearing instead of showing up as output.
#'
#' @param spain_coefs Output of `.spain_processing_coefs()`.
#' @param first_year Earliest year present in the production data.
#'
#' @return `spain_coefs` extended with backfilled early-year rows.
#' @keywords internal
#' @noRd
.backfill_processing_cf <- function(spain_coefs, first_year) {
  earliest <- spain_coefs |>
    dplyr::group_by(Item, ProcessedItem) |>
    dplyr::slice_min(Year, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::filter(Year > first_year)

  backfilled <- earliest |>
    dplyr::mutate(backfill_years = purrr::map2(first_year, Year - 1, seq)) |>
    tidyr::unnest(backfill_years) |>
    dplyr::mutate(Year = backfill_years) |>
    dplyr::select(-backfill_years)

  dplyr::bind_rows(spain_coefs, backfilled)
}

#' @title Processed and non-processed production amounts -----------------------
#' @description Splits cropland production into a non-processed remainder
#' and, separately, the processed-item quantities it yields. Only Cropland
#' rows are considered for processing; other boxes pass through unchanged.
#'
#' The substitution is N-conserving by construction: the N removed from the
#' primary item is exactly the N credited to its processed outputs. See
#' `.processing_n_scaling()` for why that needs enforcing and what it costs.
#'
#' @param prod_combined_boxes Dataframe with production_fm by province.
#' @param processing_shares Output of `.calculate_processing_shares()`.
#' @param spain_coefs Output of `.spain_processing_coefs()`.
#' @param coefs Named list with `items` (`codes_coefs_items_full`) and
#' `biomass` (`biomass_coefs`), used to price each item's N per tonne FM.
#'
#' @return A list with 'non_processed' and 'processed_items' dataframes.
#' @keywords internal
#' @noRd
.calculate_processed_amounts <- function(
  prod_combined_boxes,
  processing_shares,
  spain_coefs,
  coefs
) {
  candidate <- prod_combined_boxes |>
    dplyr::left_join(processing_shares, by = c("Year", "Item")) |>
    dplyr::mutate(
      share_processing = dplyr::if_else(
        Box == "Cropland",
        dplyr::coalesce(share_processing, 0),
        0
      ),
      processed_fm = production_fm * share_processing
    )

  outputs <- .expand_processed_items(candidate, spain_coefs)
  scaling <- .processing_n_scaling(candidate, outputs, coefs)

  list(
    non_processed = .subtract_processed_mass(candidate, scaling),
    processed_items = .scale_processed_items(outputs, scaling)
  )
}

#' @title Expand processed input mass into processed item quantities -----------
#' @description Aggregates the processed input mass per item and converts it
#' into each of its processed-item outputs using the conversion factor.
#'
#' The conversion factors are per-output and are *not* co-product shares of a
#' single physical process: `R/supply_use.R` documents them as the outputs of
#' a *virtual* process (e.g. 'Wheat and products processing'), so they can sum
#' to well above 1 for one input item (5.2 for maize in Spain in 2000, where
#' beer and starch are water-diluted). Applying them all to the same input
#' mass therefore over-produces, which is why the caller rescales the result
#' via `.processing_n_scaling()`.
#'
#' @param candidate Production rows joined with share_processing and
#' processed_fm, as built by `.calculate_processed_amounts()`.
#' @param spain_coefs Output of `.spain_processing_coefs()`.
#'
#' @return A dataframe with Year, Province_name, Name_biomass, Item, Box,
#' production_fm, prod_type and the primary Item it came from (`from_item`) —
#' one row per processed item, per province.
#' @keywords internal
#' @noRd
.expand_processed_items <- function(candidate, spain_coefs) {
  candidate |>
    dplyr::summarise(
      processed_fm = sum(processed_fm, na.rm = TRUE),
      .by = c(Year, Province_name, Name_biomass, Item)
    ) |>
    dplyr::left_join(
      spain_coefs |> dplyr::select(Year, Item, ProcessedItem, cf),
      by = c("Year", "Item"),
      relationship = "many-to-many"
    ) |>
    dplyr::filter(!is.na(ProcessedItem)) |>
    dplyr::mutate(
      production_fm = processed_fm * cf,
      from_item = Item,
      Item = ProcessedItem,
      Box = "Cropland",
      prod_type = "Product"
    ) |>
    dplyr::select(
      Year,
      Province_name,
      Name_biomass,
      Item,
      from_item,
      Box,
      production_fm,
      prod_type
    )
}

#' @title N-conserving scaling for the processing substitution -----------------
#' @description Nitrogen is an element: milling, crushing and fermenting can
#' move it between products but cannot create or destroy it. The raw
#' conversion factors respect neither bound — measured against the pins, the
#' unscaled substitution created up to +7.8% of Spain's annual production N
#' (maize/wheat/barley, whose factors sum to ~5) and destroyed up to 2.8%
#' (grapes, olives and sugar beet, whose outputs — wine, oil, sugar — are
#' nearly N-free). This computes, per province-year and primary item, the two
#' factors that close the balance:
#'
#' - `output_scale` caps the processed outputs at the N actually available in
#'   the input, scaling all of an input's outputs equally so their relative
#'   mix is untouched.
#' - `removal_scale` removes from the primary item only the N that the named
#'   outputs actually account for.
#'
#' The second factor is the conservative half. When the outputs are N-poor,
#' the unaccounted N stays with the primary item rather than disappearing.
#' Physically that residue is by-product (grape pomace, olive cake, beet
#' pulp) or an agro-industry loss, and modelling it as unprocessed primary
#' crop is an approximation: it keeps the mass balance honest but attributes
#' the residual to the primary item's destinies. Routing it to explicit
#' by-product items is the open methodological question, tracked separately —
#' it needs a decision from the model owners, not a default chosen here.
#'
#' @param candidate Production rows with processed_fm, as built by
#' `.calculate_processed_amounts()`.
#' @param outputs Output of `.expand_processed_items()`.
#' @param coefs Named list with `items` and `biomass` coefficient tables.
#'
#' @return A dataframe with Year, Province_name, Name_biomass, Item,
#' output_scale and removal_scale.
#' @keywords internal
#' @noRd
.processing_n_scaling <- function(candidate, outputs, coefs) {
  key <- c("Year", "Province_name", "Name_biomass", "Item")

  n_in <- candidate |>
    dplyr::filter(processed_fm > 0) |>
    dplyr::summarise(
      processed_fm = sum(processed_fm, na.rm = TRUE),
      .by = dplyr::all_of(key)
    ) |>
    .add_product_n_per_fm(coefs) |>
    dplyr::mutate(n_in = processed_fm * n_per_fm) |>
    dplyr::select(dplyr::all_of(key), n_in)

  # Priced on the output item, then re-keyed to the primary item it came from.
  n_out <- outputs |>
    .add_product_n_per_fm(coefs) |>
    dplyr::select(-Item) |>
    dplyr::rename(Item = from_item) |>
    dplyr::summarise(
      n_out = sum(production_fm * n_per_fm, na.rm = TRUE),
      n_missing = sum(is.na(n_per_fm) & production_fm > 0),
      .by = dplyr::all_of(key)
    )

  n_in |>
    dplyr::full_join(n_out, by = key) |>
    dplyr::mutate(
      n_out = dplyr::coalesce(n_out, 0),
      # A missing input coefficient makes the input's N unknown, so any output
      # N would be created from nothing: drop the substitution for that item
      # rather than guess a coefficient.
      ratio = dplyr::if_else(!is.na(n_in) & n_in > 0, n_out / n_in, 0),
      output_scale = pmin(1, dplyr::if_else(ratio > 0, 1 / ratio, 0)),
      removal_scale = pmin(1, ratio)
    ) |>
    .warn_unpriced_processing() |>
    dplyr::select(dplyr::all_of(key), output_scale, removal_scale)
}

#' @title N content per tonne of fresh matter, product basis -------------------
#' @description Replicates the coefficient choice `.convert_fm_dm_n()` makes
#' for `prod_type == "Product"` rows, so the conservation scaling and the
#' final FM to N conversion cannot drift apart.
#'
#' @param df Dataframe with `Item` and `Name_biomass` (the primary biomass).
#' @param coefs Named list with `items` and `biomass` coefficient tables.
#'
#' @return `df` with an added `n_per_fm` column (tonnes N per tonne FM).
#' @keywords internal
#' @noRd
.add_product_n_per_fm <- function(df, coefs) {
  df |>
    dplyr::left_join(
      coefs$items |> dplyr::select(item, item_biomass = Name_biomass),
      by = c("Item" = "item")
    ) |>
    dplyr::mutate(
      biomass_match = dplyr::if_else(
        Item %in% .special_biomass_items(),
        Name_biomass,
        dplyr::coalesce(item_biomass, Name_biomass)
      )
    ) |>
    dplyr::left_join(
      coefs$biomass |>
        dplyr::select(
          Name_biomass,
          Product_kgDM_kgFM,
          Product_kgN_kgDM
        ) |>
        dplyr::distinct(),
      by = c("biomass_match" = "Name_biomass")
    ) |>
    dplyr::mutate(n_per_fm = Product_kgDM_kgFM * Product_kgN_kgDM) |>
    dplyr::select(
      -item_biomass,
      -biomass_match,
      -Product_kgDM_kgFM,
      -Product_kgN_kgDM
    )
}

#' @title Items whose conversion coefficients come from the primary biomass ----
#' @description Aggregate items that carry no biomass coefficients of their
#' own and fall back to the primary biomass of the row they came from.
#'
#' @return A character vector of item names.
#' @keywords internal
#' @noRd
.special_biomass_items <- function() {
  c(
    "Nuts and products",
    "Vegetables, Other",
    "Fruits, Other",
    "Cereals, Other",
    "Pulses, Other and products"
  )
}

#' @title Warn about processing flows that cannot be priced in N --------------
#' @description Surfaces the two cases where the conservation scaling has to
#' drop a substitution instead of balancing it, so a missing coefficient can
#' never silently zero a real flow.
#'
#' @param scaling Scaling table built inside `.processing_n_scaling()`.
#'
#' @return `scaling`, unchanged.
#' @keywords internal
#' @noRd
.warn_unpriced_processing <- function(scaling) {
  no_input_coef <- scaling |>
    dplyr::filter(is.na(n_in) | n_in <= 0, n_out > 0) |>
    dplyr::distinct(Item) |>
    dplyr::pull(Item)

  if (length(no_input_coef) > 0) {
    cli::cli_warn(c(
      "Dropped the processing substitution for {length(no_input_coef)} item{?s}
       with no usable product N coefficient.",
      i = "Item{?s}: {.val {no_input_coef}}."
    ))
  }

  unpriced_outputs <- scaling |>
    dplyr::filter(dplyr::coalesce(n_missing, 0L) > 0) |>
    dplyr::distinct(Item) |>
    dplyr::pull(Item)

  if (length(unpriced_outputs) > 0) {
    cli::cli_warn(c(
      "{length(unpriced_outputs)} primary item{?s} ha{?s/ve} processed outputs
       with no product N coefficient; their N is counted as zero.",
      i = "Item{?s}: {.val {unpriced_outputs}}."
    ))
  }

  scaling
}

#' @title Remove the accounted processed mass from the primary item -----------
#' @description Subtracts `removal_scale * processed_fm` from each production
#' row, so only the mass whose N is credited to a processed output leaves the
#' primary item.
#'
#' @param candidate Production rows with processed_fm.
#' @param scaling Output of `.processing_n_scaling()`.
#'
#' @return `candidate` with production_fm reduced and helper columns dropped.
#' @keywords internal
#' @noRd
.subtract_processed_mass <- function(candidate, scaling) {
  candidate |>
    dplyr::left_join(
      scaling |> dplyr::select(-output_scale),
      by = c("Year", "Province_name", "Name_biomass", "Item")
    ) |>
    dplyr::mutate(
      production_fm = production_fm -
        processed_fm * dplyr::coalesce(removal_scale, 0)
    ) |>
    dplyr::select(-share_processing, -processed_fm, -removal_scale)
}

#' @title Apply the N cap to the processed item quantities ---------------------
#' @description Scales each processed output so the N credited to an input's
#' outputs never exceeds the N the input actually carried.
#'
#' @param outputs Output of `.expand_processed_items()`.
#' @param scaling Output of `.processing_n_scaling()`.
#'
#' @return `outputs` with production_fm scaled and `from_item` dropped.
#' @keywords internal
#' @noRd
.scale_processed_items <- function(outputs, scaling) {
  outputs |>
    dplyr::left_join(
      scaling |> dplyr::select(-removal_scale),
      by = c(
        "Year",
        "Province_name",
        "Name_biomass",
        "from_item" = "Item"
      )
    ) |>
    dplyr::mutate(
      production_fm = production_fm * dplyr::coalesce(output_scale, 0)
    ) |>
    dplyr::select(-from_item, -output_scale)
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
  grazed_no_seeds_primary <- added_grass_wood_merged |>
    dplyr::mutate(
      Biomass_match = dplyr::if_else(
        Item %in% .special_biomass_items(),
        Name_biomass_primary,
        Name_biomass
      )
    )

  prod_grazed_no_seeds_n <- grazed_no_seeds_primary |>
    dplyr::left_join(
      biomass_coefs |>
        dplyr::select(
          Name_biomass,
          Product_kgDM_kgFM,
          Product_kgN_kgDM,
          Residue_kgDM_kgFM,
          Residue_kgN_kgDM
        ),
      by = c("Biomass_match" = "Name_biomass")
    ) |>
    dplyr::mutate(
      # Some residues (e.g. Straw) can miss residue-specific coefficients.
      # In that case, fall back to product coefficients to avoid dropping
      # production to zero.
      Residue_kgDM_kgFM = dplyr::coalesce(
        Residue_kgDM_kgFM,
        Product_kgDM_kgFM
      ),
      Residue_kgN_kgDM = dplyr::coalesce(
        Residue_kgN_kgDM,
        Product_kgN_kgDM
      ),
      conversion_dm = dplyr::if_else(
        prod_type %in%
          c(
            "Residue",
            "Grass"
          ),
        Residue_kgDM_kgFM,
        Product_kgDM_kgFM
      ),
      conversion_n_dm = dplyr::if_else(
        prod_type %in%
          c(
            "Residue",
            "Grass"
          ),
        Residue_kgN_kgDM,
        Product_kgN_kgDM
      ),
      production_n = production_fm * conversion_dm * conversion_n_dm
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
.add_feed <- function(feed_intake) {
  feed_wide <- feed_intake |>
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
          c(
            "Pigs",
            "Poultry",
            "Rabbits",
            "Fur animals",
            "Other",
            "Other_birds"
          ) ~
          "monogastric",
        Livestock_cat == "Pets" ~ "pets",
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::summarise(
      feed_amount = sum(intake_MgFM, na.rm = TRUE),
      .by = c("Year", "Province_name", "item_cbs", "Livestock_type")
    ) |>
    tidyr::pivot_wider(
      names_from = Livestock_type,
      values_from = feed_amount,
      values_fill = 0
    ) |>
    .ensure_livestock_cols() |>
    dplyr::mutate(
      ruminant = dplyr::coalesce(ruminant, 0),
      monogastric = dplyr::coalesce(monogastric, 0),
      pets = dplyr::coalesce(pets, 0),
      feed = ruminant + monogastric,
      food_pets = pets
    )

  feed_share_rum_mono <- feed_wide |>
    dplyr::mutate(
      feed_total = feed,
      share_rum = dplyr::if_else(feed_total > 0, ruminant / feed_total, 0),
      share_mono = dplyr::if_else(feed_total > 0, monogastric / feed_total, 0)
    )

  list(
    feed_intake = feed_wide |>
      dplyr::rename(Item = item_cbs) |>
      dplyr::select(Year, Province_name, Item, feed, food_pets),
    feed_share_rum_mono = feed_share_rum_mono |>
      dplyr::rename(Item = item_cbs) |>
      dplyr::select(Year, Province_name, Item, share_rum, share_mono)
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

  # Feed for pets is assigned to food.
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
      other_uses = dplyr::coalesce(other_uses, 0),
      feed = dplyr::coalesce(feed, 0),
      production_n = dplyr::coalesce(production_n, 0),
      production_total = dplyr::coalesce(production_total, 0)
    ) |>

    dplyr::select(-food_pets)

  # Split consumption proportionally across all Box/Irrig_cat rows by their
  # share of total item production. This handles both the irrigated/rainfed
  # split within Cropland AND items that span multiple boxes (e.g. Cropland +
  # semi_natural). Without this, the non-Cropland rows would each receive the
  # full consumption value, causing overcounting that grows with production.
  # When production_total = 0 (pure import items), there is only one row so
  # production_share = 1 is correct.
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
    dplyr::select(-production_total, -production_share)

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
.calculate_trade <- function(grafs_prod_item_n) {
  grafs_prod_item_n |>
    dplyr::mutate(
      food = dplyr::coalesce(food, 0),
      other_uses = dplyr::coalesce(other_uses, 0),
      feed = dplyr::coalesce(feed, 0),

      demand_total = food + other_uses + feed,

      import = pmax(demand_total - production_n, 0),
      export = pmax(production_n - demand_total, 0)
    ) |>
    dplyr::select(
      Year,
      Province_name,
      Item,
      Box,
      Irrig_cat,
      production_n,
      food,
      other_uses,
      feed,
      export,
      import
    )
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
        is.na(Box) & group %in% c("Agro-industry", "Fish") ~ group,
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
.calculate_consumption_shares <- function(df) {
  df |>
    dplyr::mutate(
      demand_total = food + other_uses + feed,

      local_total = pmin(production_n, demand_total),

      food_local = dplyr::if_else(
        demand_total > 0,
        local_total * (food / demand_total),
        0
      ),
      other_local = dplyr::if_else(
        demand_total > 0,
        local_total * (other_uses / demand_total),
        0
      ),
      feed_local = dplyr::if_else(
        demand_total > 0,
        local_total * (feed / demand_total),
        0
      ),

      food_share = dplyr::if_else(local_total > 0, food_local / local_total, 0),
      other_uses_share = dplyr::if_else(
        local_total > 0,
        other_local / local_total,
        0
      ),
      feed_share = dplyr::if_else(local_total > 0, feed_local / local_total, 0)
    ) |>
    dplyr::select(
      Year,
      Province_name,
      Item,
      Box,
      Irrig_cat,
      local_total,
      food_share,
      other_uses_share,
      feed_share
    )
}

#' @title Split local consumption
#' @description Splits local consumption into population food, other uses,
#' and livestock. Livestock feed is split into livestock_rum (ruminants)
#' and livestock_mono (monogastric).
#' @param local_vs_import A dataset containing local and imported consumption.
#' @param feed_share_rum_mono A dataset with feed shares between ruminants
#' and monogastric animals.
#' @return A dataset with consumption split into population_food,
#' livestock_rum, livestock_mono, and population_other_uses.
#' @keywords internal
#' @noRd
.split_local_consumption <- function(local_vs_import, feed_share_rum_mono) {
  local_vs_import |>
    dplyr::left_join(
      feed_share_rum_mono,
      by = c("Year", "Province_name", "Item")
    ) |>
    dplyr::mutate(
      share_rum = dplyr::coalesce(share_rum, 0),
      share_mono = dplyr::coalesce(share_mono, 0),
      share_total = share_rum + share_mono,
      share_rum = dplyr::if_else(is.na(share_rum), 0, share_rum),
      share_mono = dplyr::if_else(is.na(share_mono), 0, share_mono),

      local_food_raw = local_consumption * food_share,
      local_other_raw = local_consumption * other_uses_share,
      local_feed_raw = local_consumption * feed_share,

      total_local_alloc = local_food_raw + local_other_raw + local_feed_raw,

      scale_factor = dplyr::if_else(
        total_local_alloc > local_consumption & total_local_alloc > 0,
        local_consumption / total_local_alloc,
        1
      ),

      local_food = local_food_raw * scale_factor,
      local_other_uses = local_other_raw * scale_factor,
      local_feed = local_feed_raw * scale_factor,

      population_food = local_food,
      population_other_uses = local_other_uses,
      livestock_rum = local_feed * share_rum,
      livestock_mono = local_feed * share_mono,

      Origin = Box
    ) |>
    dplyr::select(
      -share_total,
      -local_food_raw,
      -local_other_raw,
      -local_feed_raw,
      -total_local_alloc,
      -scale_factor,
      -local_food,
      -local_other_uses,
      -local_feed
    ) |>
    tidyr::pivot_longer(
      cols = c(
        population_food,
        population_other_uses,
        livestock_rum,
        livestock_mono
      ),
      names_to = "Destiny",
      values_to = "MgN"
    )
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
.split_import_consumption <- function(
  local_vs_import,
  feed_share_rum_mono,
  shares_import_wide
) {
  local_vs_import |>
    dplyr::left_join(
      feed_share_rum_mono,
      by = c("Year", "Province_name", "Item")
    ) |>
    dplyr::mutate(
      share_rum = dplyr::coalesce(share_rum, 0),
      share_mono = dplyr::coalesce(share_mono, 0),

      food_local = local_consumption * food_share,
      other_local = local_consumption * other_uses_share,
      feed_local = local_consumption * feed_share,

      food_gap = pmax(food - food_local, 0),
      other_gap = pmax(other_uses - other_local, 0),
      feed_gap = pmax(feed - feed_local, 0),

      total_gap = food_gap + other_gap + feed_gap,

      share_food = dplyr::if_else(total_gap > 0, food_gap / total_gap, 0),
      share_other = dplyr::if_else(total_gap > 0, other_gap / total_gap, 0),
      share_feed = dplyr::if_else(total_gap > 0, feed_gap / total_gap, 0),

      population_food = import_consumption * share_food,
      population_other_uses = import_consumption * share_other,
      import_feed = import_consumption * share_feed,

      livestock_rum = import_feed * share_rum,
      livestock_mono = import_feed * share_mono,

      Origin = "Outside",
      Irrig_cat = NA_character_
    ) |>
    dplyr::select(
      -food_local,
      -other_local,
      -feed_local,
      -food_gap,
      -other_gap,
      -feed_gap,
      -total_gap,
      -share_food,
      -share_other,
      -share_feed,
      -import_feed
    ) |>
    dplyr::summarise(
      population_food = sum(population_food, na.rm = TRUE),
      population_other_uses = sum(population_other_uses, na.rm = TRUE),
      livestock_rum = sum(livestock_rum, na.rm = TRUE),
      livestock_mono = sum(livestock_mono, na.rm = TRUE),
      .by = c("Year", "Province_name", "Item", "Box", "Origin", "Irrig_cat")
    ) |>
    tidyr::pivot_longer(
      cols = c(
        population_food,
        population_other_uses,
        livestock_rum,
        livestock_mono
      ),
      names_to = "Destiny",
      values_to = "MgN"
    )
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
  biomass_coefs <- whep_read_file("biomass_coefs")
  grafs_prod_destiny_final <- .prep_final_ds(
    grafs_prod_item_trade,
    codes_coefs_items_full
  ) |>
    dplyr::group_by(Year, Province_name, Item, Box, Irrig_cat) |>
    dplyr::summarise(
      production_n = sum(production_n, na.rm = TRUE),
      food = sum(food, na.rm = TRUE),
      other_uses = sum(other_uses, na.rm = TRUE),
      feed = sum(feed, na.rm = TRUE),
      export = sum(export, na.rm = TRUE),
      import = sum(import, na.rm = TRUE),
      .groups = "drop"
    )

  shares_import <- .calculate_consumption_shares(grafs_prod_destiny_final)

  pie_imports_n <- whep_read_file("pie_full_destinies_fm") |>
    dplyr::filter(
      Element == "Import",
      Destiny %in% c("Food", "Other_uses", "Feed")
    ) |>
    dplyr::group_by(Year, Item, Destiny) |>
    dplyr::summarise(
      value_fm = sum(Value_destiny, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::left_join(
      codes_coefs_items_full |>
        dplyr::select(item, Name_biomass),
      by = c("Item" = "item")
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
      prod_type = dplyr::case_when(
        Name_biomass %in% c("Grass", "Fallow") ~ "Grass",
        Name_biomass == "Average wood" ~ "Residue",
        TRUE ~ "Product"
      )
    ) |>
    dplyr::mutate(
      value_n = dplyr::case_when(
        prod_type %in% c("Residue", "Grass") ~
          value_fm *
          dplyr::coalesce(Residue_kgDM_kgFM, Product_kgDM_kgFM) *
          dplyr::coalesce(Residue_kgN_kgDM, Product_kgN_kgDM),
        prod_type == "Product" ~
          value_fm *
          Product_kgDM_kgFM *
          Product_kgN_kgDM,

        TRUE ~ NA_real_
      )
    ) |>
    dplyr::group_by(Year, Item) |>
    dplyr::mutate(
      total = sum(value_n, na.rm = TRUE),
      share = dplyr::if_else(total > 0, value_n / total, 0)
    ) |>
    dplyr::ungroup()

  shares_import_wide <- pie_imports_n |>
    dplyr::select(Year, Item, Destiny, share) |>
    tidyr::pivot_wider(
      names_from = Destiny,
      values_from = share,
      names_prefix = "share_"
    )

  local_vs_import <- grafs_prod_destiny_final |>
    dplyr::left_join(
      shares_import,
      by = c("Year", "Province_name", "Item", "Box", "Irrig_cat")
    ) |>
    dplyr::mutate(
      local_consumption = pmin(production_n, food + other_uses + feed),
      import_consumption = pmax((food + other_uses + feed) - production_n, 0)
    )

  dplyr::bind_rows(
    .split_local_consumption(local_vs_import, feed_share_rum_mono),
    .split_import_consumption(
      local_vs_import,
      feed_share_rum_mono,
      shares_import_wide
    ),
    .add_exports(grafs_prod_destiny_final)
  ) |>
    dplyr::filter(MgN > 0)
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

.ensure_livestock_cols <- function(df) {
  required <- c("ruminant", "monogastric", "pets")
  missing <- setdiff(required, names(df))
  dplyr::mutate(df, !!!purrr::map(rlang::set_names(missing), ~0))
}
