prepare_lmdi_dataset <- function() {
  grafs_data <- create_n_nat_destiny()
  population_data <- whep_read_file("population_yg")

  df_veg <- grafs_data |>
    dplyr::filter(
      Origin %in%
        c("Cropland", "semi_natural_agroecosystems") |
        Origin %in%
          c("Deposition", "Fixation", "Synthetic", "Livestock", "People")
    )

  outputs <- df_veg |>
    dplyr::filter(
      Destiny %in%
        c(
          "population_food",
          "population_other_uses",
          "livestock_mono",
          "livestock_rum",
          "export"
        ),
      Origin %in% c("Cropland", "semi_natural_agroecosystems")
    ) |>
    dplyr::group_by(Year) |>
    dplyr::summarise(outputs = sum(MgN, na.rm = TRUE), .groups = "drop")

  inputs <- grafs_data |>
    dplyr::filter(
      Origin %in%
        c("Deposition", "Fixation", "Synthetic", "Livestock", "People"),
      Destiny %in% c("Cropland", "semi_natural_agroecosystems")
    ) |>
    dplyr::group_by(Year) |>
    dplyr::summarise(inputs = sum(MgN, na.rm = TRUE), .groups = "drop")

  surplus_df <- inputs |>
    dplyr::left_join(outputs, by = "Year") |>
    dplyr::mutate(
      outputs = dplyr::coalesce(outputs, 0),
      surplus = inputs - outputs
    )

  food_df <- df_veg |>
    dplyr::filter(
      Destiny == "population_food",
      Origin %in% c("Cropland", "semi_natural_agroecosystems")
    ) |>
    dplyr::group_by(Year) |>
    dplyr::summarise(food = sum(MgN, na.rm = TRUE), .groups = "drop")

  pop_df <- population_data |>
    dplyr::group_by(Year) |>
    dplyr::summarise(
      population = sum(Pop_Mpeop_yg, na.rm = TRUE),
      .groups = "drop"
    )

  surplus_df |>
    dplyr::left_join(food_df, by = "Year") |>
    dplyr::left_join(pop_df, by = "Year") |>
    dplyr::mutate(
      food = dplyr::coalesce(food, 0),
      population = dplyr::coalesce(population, 0),
      A = dplyr::if_else(population > 0, food / population, NA_real_),
      T = dplyr::if_else(food > 0, surplus / food, NA_real_)
    ) |>
    dplyr::select(Year, surplus, population, food, A, T) |>
    dplyr::arrange(Year)
}

prepare_lmdi_production_area <- function() {

  n_data <- create_n_nat_destiny()

  surplus <- n_data |>
    dplyr::group_by(Year) |>
    dplyr::summarise(
      surplus = sum(MgN, na.rm = TRUE),
      .groups = "drop"
    )

  production <- n_data |>
    dplyr::filter(
      Origin %in% c("Cropland", "semi_natural_agroecosystems"),
      Destiny %in% c(
        "livestock_mono",
        "livestock_rum",
        "population_food",
        "population_other_uses",
        "export"
      )
    ) |>
    dplyr::group_by(Year) |>
    dplyr::summarise(
      production = sum(MgN, na.rm = TRUE),
      .groups = "drop"
    )

  area <- whep_read_file("crop_area_npp_ygpitr_no_fallow") |>
    dplyr::group_by(Year) |>
    dplyr::summarise(
      area = sum(Area_ygpit_ha, na.rm = TRUE),
      .groups = "drop"
    )

  df <- surplus |>
    dplyr::left_join(production, by = "Year") |>
    dplyr::left_join(area, by = "Year") |>
    dplyr::mutate(
      yield = production / area,
      intensity = surplus / production
    ) |>
    dplyr::select(
      Year,
      surplus,
      area,
      yield,
      intensity
    )

  return(df)
}