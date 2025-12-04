create_n_prov_destiny_debug <- function() {
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
  n_balance_ygpit_all <- whep_read_file("n_balance_ygpit_all")

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
    n_balance_ygpit_all,
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

  list(
    biomass_item_merged = biomass_item_merged,
    prod_combined_boxes = prod_combined_boxes,
    food_and_other_uses = food_and_other_uses,
    adding_feed_output = adding_feed_output,
    grafs_prod_item_n = grafs_prod_item_n,
    n_soil_inputs = n_soil_inputs,
    trade_data = trade_data,
    prod_destiny_final = prod_destiny,
    pie_full_destinies_fm = pie_full_destinies_fm,
    biomass_coefs = biomass_coefs,
    codes_coefs_items_full = codes_coefs_items_full
  )
}
