prepare_lmdi_dataset <- function() {
  grafs_data <- create_n_nat_destiny()
  population_data <- whep_read_file("population_yg")

  df_veg <- grafs_data |>
    dplyr::filter(
      origin %in%
        c("Cropland", "semi_natural_agroecosystems") |
        origin %in%
          c("Deposition", "Fixation", "Synthetic", "Livestock", "People")
    )

  outputs <- df_veg |>
    dplyr::filter(
      destiny %in%
        c(
          "population_food",
          "population_other_uses",
          "livestock_mono",
          "livestock_rum",
          "export"
        ),
      origin %in% c("Cropland", "semi_natural_agroecosystems")
    ) |>
    dplyr::group_by(year) |>
    dplyr::summarise(outputs = sum(mg_n, na.rm = TRUE), .groups = "drop")

  inputs <- grafs_data |>
    dplyr::filter(
      origin %in%
        c("Deposition", "Fixation", "Synthetic", "Livestock", "People"),
      destiny %in% c("Cropland", "semi_natural_agroecosystems")
    ) |>
    dplyr::group_by(year) |>
    dplyr::summarise(inputs = sum(mg_n, na.rm = TRUE), .groups = "drop")

  surplus_df <- inputs |>
    dplyr::left_join(outputs, by = "year") |>
    dplyr::mutate(
      outputs = dplyr::coalesce(outputs, 0),
      surplus = inputs - outputs
    )

  food_df <- df_veg |>
    dplyr::filter(
      destiny == "population_food",
      origin %in% c("Cropland", "semi_natural_agroecosystems")
    ) |>
    dplyr::group_by(year) |>
    dplyr::summarise(food = sum(mg_n, na.rm = TRUE), .groups = "drop")

  pop_df <- population_data |>
    dplyr::rename_with(tolower) |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      population = sum(pop_mpeop_yg, na.rm = TRUE),
      .groups = "drop"
    )

  surplus_df |>
    dplyr::left_join(food_df, by = "year") |>
    dplyr::left_join(pop_df, by = "year") |>
    dplyr::mutate(
      food = dplyr::coalesce(food, 0),
      population = dplyr::coalesce(population, 0),
      A = dplyr::if_else(population > 0, food / population, NA_real_),
      t_ratio = dplyr::if_else(food > 0, surplus / food, NA_real_)
    ) |>
    dplyr::select(year, surplus, population, food, A, t_ratio) |>
    dplyr::arrange(year)
}

prepare_lmdi_production_area <- function() {
  n_data <- create_n_nat_destiny()

  surplus <- n_data |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      surplus = sum(mg_n, na.rm = TRUE),
      .groups = "drop"
    )

  production <- n_data |>
    dplyr::filter(
      origin %in% c("Cropland", "semi_natural_agroecosystems"),
      destiny %in%
        c(
          "livestock_mono",
          "livestock_rum",
          "population_food",
          "population_other_uses",
          "export"
        )
    ) |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      production = sum(mg_n, na.rm = TRUE),
      .groups = "drop"
    )

  area <- whep_read_file("crop-area-npp-ygpitr-no-fallow") |>
    dplyr::rename_with(tolower) |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      area = sum(area_ygpit_ha, na.rm = TRUE),
      .groups = "drop"
    )

  df <- surplus |>
    dplyr::left_join(production, by = "year") |>
    dplyr::left_join(area, by = "year") |>
    dplyr::mutate(
      yield = production / area,
      intensity = surplus / production
    ) |>
    dplyr::select(
      year,
      surplus,
      area,
      yield,
      intensity
    )

  df
}
