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
  codes_coefs_items_full <- whep_read_file("codes_coefs_items_full")
  biomass_coefs <- whep_read_file("biomass_coefs")
  pie_full_destinies_fm <- whep_read_file("pie_full_destinies_fm")
  processed_prov_fixed <- whep_read_file("processed_prov_fixed")
  livestock_prod_ygps <- whep_read_file("livestock_prod_ygps")
  crop_area_npp_no_fallow <- whep_read_file("crop_area_npp_ygpitr_no_fallow")
  npp_ygpit <- whep_read_file("npp_ygpit")
  codes_coefs <- whep_read_file("codes_coefs")
  intake_ygiac <- whep_read_file("intake_ygiac")

  biomass_item_merged <- .merge_items_biomass(npp_ygpit, codes_coefs)

  prod_combined_boxes <- biomass_item_merged |>
    .aggregate_crop_seminatural(
      .summarise_crops_residues(crop_area_npp_no_fallow)
    ) |>
    .combine_production_boxes(
      .prepare_livestock_production(livestock_prod_ygps)
    )

  adding_feed_output <- .adding_feed(intake_ygiac)

  feed_share_nat <- intake_ygiac |>
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
            "Other"
          ) ~
          "monogastric",
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::filter(
      !is.na(Livestock_type),
      Livestock_cat != "Pets"
    ) |>
    dplyr::group_by(Year, Item, Livestock_type) |>
    dplyr::summarise(
      feed = sum(FM_Mg, na.rm = TRUE),
      .groups = "drop"
    ) |>
    tidyr::pivot_wider(
      names_from = Livestock_type,
      values_from = feed
    ) |>
    dplyr::mutate(
      feed_total = ruminant + monogastric,
      share_rum = ruminant / feed_total,
      share_mono = monogastric / feed_total
    ) |>
    dplyr::filter(feed_total > 0) |>
    dplyr::select(Year, Item, share_rum, share_mono)

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
      pie_full_destinies_fm |>
        dplyr::filter(
          Destiny %in% c("Food", "Other_uses"),
          Element == "Domestic_supply"
        ) |>
        dplyr::group_by(Year, Item, Destiny) |>
        dplyr::summarise(
          Value = sum(Value_destiny, na.rm = TRUE),
          .groups = "drop"
        ) |>
        tidyr::pivot_wider(names_from = Destiny, values_from = Value) |>
        dplyr::rename(food = Food, other_uses = Other_uses) |>
        dplyr::mutate(Province_name = "Spain")
    ) |>
    dplyr::group_by(Year, Item, Box, Irrig_cat) |>
    dplyr::summarise(
      production_n = sum(production_n, na.rm = TRUE),
      food = sum(food, na.rm = TRUE),
      feed = sum(feed, na.rm = TRUE),
      other_uses = sum(other_uses, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(Province_name = "Spain")

  n_soil_inputs <- .calculate_n_soil_inputs(
    whep_read_file("n_balance_ygpit_all"),
    codes_coefs
  ) |>
    dplyr::group_by(Year, Item, Irrig_cat, Box) |>
    dplyr::summarise(
      deposition = sum(deposition, na.rm = TRUE),
      fixation = sum(fixation, na.rm = TRUE),
      synthetic = sum(synthetic, na.rm = TRUE),
      manure = sum(manure, na.rm = TRUE),
      urban = sum(urban, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(Province_name = "Spain")

  trade_data <- grafs_prod_item_n |>
    dplyr::mutate(
      consumption = food + feed + other_uses,
      net_trade = production_n - consumption,
      export = dplyr::if_else(net_trade > 0, net_trade, 0),
      import = dplyr::if_else(net_trade < 0, -net_trade, 0)
    )

  local_base <- trade_data |>
    dplyr::mutate(
      food_share = dplyr::if_else(consumption > 0, food / consumption, 0),
      feed_share = dplyr::if_else(consumption > 0, feed / consumption, 0),
      other_uses_share = dplyr::if_else(
        consumption > 0,
        other_uses / consumption,
        0
      ),
      local_consumption = pmin(production_n, consumption)
    ) |>
    tidyr::pivot_longer(
      cols = c(food_share, feed_share, other_uses_share),
      names_to = "share_type",
      values_to = "share"
    ) |>
    dplyr::mutate(
      MgN = local_consumption * share,
      Origin = Box,
      Destiny = dplyr::case_when(
        share_type == "food_share" ~ "population_food",
        share_type == "feed_share" ~ "livestock",
        share_type == "other_uses_share" ~ "population_other_uses"
      )
    ) |>
    dplyr::filter(MgN > 0)

  local_non_livestock <- local_base |>
    dplyr::filter(Destiny != "livestock")

  local_livestock <- local_base |>
    dplyr::filter(Destiny == "livestock") |>
    dplyr::left_join(feed_share_nat, by = c("Year", "Item")) |>
    dplyr::mutate(
      MgN_rum = MgN * share_rum,
      MgN_mono = MgN * share_mono
    ) |>
    dplyr::select(-MgN) |>
    tidyr::pivot_longer(
      cols = c(MgN_rum, MgN_mono),
      names_to = "Destiny_feed",
      values_to = "MgN"
    ) |>
    dplyr::mutate(
      Destiny = dplyr::case_when(
        Destiny_feed == "MgN_rum" ~ "livestock_rum",
        Destiny_feed == "MgN_mono" ~ "livestock_mono"
      )
    ) |>
    dplyr::filter(!is.na(MgN), MgN > 0) |>
    dplyr::select(-Destiny_feed)

  local_split <- dplyr::bind_rows(local_non_livestock, local_livestock)

  export_flows <- trade_data |>
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

  import_base <- trade_data |>
    dplyr::mutate(
      food_share = dplyr::if_else(consumption > 0, food / consumption, 0),
      feed_share = dplyr::if_else(consumption > 0, feed / consumption, 0),
      other_uses_share = dplyr::if_else(
        consumption > 0,
        other_uses / consumption,
        0
      )
    ) |>
    tidyr::pivot_longer(
      cols = c(food_share, feed_share, other_uses_share),
      names_to = "share_type",
      values_to = "share"
    ) |>
    dplyr::mutate(
      MgN = import * share,
      Origin = "Outside",
      Destiny = dplyr::case_when(
        share_type == "food_share" ~ "population_food",
        share_type == "feed_share" ~ "livestock",
        share_type == "other_uses_share" ~ "population_other_uses"
      ),
      Irrig_cat = NA_character_
    ) |>
    dplyr::filter(MgN > 0)

  import_non_livestock <- import_base |>
    dplyr::filter(Destiny != "livestock")

  import_livestock <- import_base |>
    dplyr::filter(Destiny == "livestock") |>
    dplyr::left_join(feed_share_nat, by = c("Year", "Item")) |>
    dplyr::mutate(
      MgN_rum = MgN * share_rum,
      MgN_mono = MgN * share_mono
    ) |>
    dplyr::select(-MgN) |>
    tidyr::pivot_longer(
      cols = c(MgN_rum, MgN_mono),
      names_to = "Destiny_feed",
      values_to = "MgN"
    ) |>
    dplyr::mutate(
      Destiny = dplyr::if_else(
        Destiny_feed == "MgN_rum",
        "livestock_rum",
        "livestock_mono"
      )
    ) |>
    dplyr::filter(MgN > 0) |>
    dplyr::select(-Destiny_feed)

  dplyr::bind_rows(
    local_split,
    import_non_livestock,
    import_livestock,
    export_flows
  ) |>
    .add_n_soil_inputs(n_soil_inputs) |>
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
