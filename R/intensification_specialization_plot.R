intensification_specialization_plot <- function() {
  # ---- Load data ----
  flows <- create_n_prov_destiny()
  npp_ygpit <- whep_read_file("npp_ygpit")

  # ---- Area per province ----
  area_df <- npp_ygpit |>
    dplyr::group_by(Year, Province_name) |>
    dplyr::summarise(
      area_ha = sum(Area_ygpit_ha, na.rm = TRUE),
      .groups = "drop"
    )

  # ---- New N soil inputs (MgN) ----
  n_inputs <- flows |>
    dplyr::filter(
      Origin %in%
        c("Synthetic", "Deposition", "Fixation"),
      Destiny %in% c("Cropland", "semi_natural_agroecosystems")
    ) |>
    dplyr::group_by(Year, Province_name) |>
    dplyr::summarise(
      total_input_mg = sum(MgN, na.rm = TRUE),
      .groups = "drop"
    )

  # ---- Synthetic share ----
  synthetic_share <- flows |>
    dplyr::filter(
      Destiny %in% c("Cropland", "semi_natural_agroecosystems"),
      Origin %in%
        c("Synthetic", "Livestock", "Fixation", "Deposition", "People")
    ) |>
    dplyr::group_by(Year, Province_name) |>
    dplyr::summarise(
      synthetic_share = sum(MgN[Origin == "Synthetic"], na.rm = TRUE) /
        sum(MgN, na.rm = TRUE),
      .groups = "drop"
    )

  # ---- Feed import share ----
  feed_import_share <- flows |>
    dplyr::filter(
      Destiny %in% c("livestock_mono", "livestock_rum"),
      Origin %in% c("Cropland", "semi_natural_agroecosystems", "Outside")
    ) |>
    dplyr::group_by(Year, Province_name) |>
    dplyr::summarise(
      feed_import_share = sum(MgN[Origin == "Outside"], na.rm = TRUE) /
        sum(MgN, na.rm = TRUE),
      .groups = "drop"
    )

  # ---- Combine indicators ----
  df <- n_inputs |>
    dplyr::left_join(area_df, by = c("Year", "Province_name")) |>
    dplyr::left_join(synthetic_share, by = c("Year", "Province_name")) |>
    dplyr::left_join(feed_import_share, by = c("Year", "Province_name")) |>
    dplyr::mutate(
      intensification = (total_input_mg * 1000) / area_ha,

      specialization = (synthetic_share + feed_import_share) / 2
    ) |>
    dplyr::filter(!is.na(intensification), !is.na(specialization))

  df <- df |>
    dplyr::left_join(
      create_typologies_timeseries_plot() |>
        dplyr::select(Year, Province_name, Typology_base) |>
        dplyr::rename(Typology = Typology_base),
      by = c("Year", "Province_name")
    ) |>
    dplyr::mutate(
      Typology = gsub(" \\(intensive\\)| \\(extensive\\)", "", Typology)
    ) |>
    dplyr::filter(!is.na(Typology))

  p <- ggplot2::ggplot(
    df,
    ggplot2::aes(
      x = intensification,
      y = specialization,
      group = Province_name
    )
  ) +
    ggplot2::geom_point(
      ggplot2::aes(
        color = Year,
        shape = Typology
      ),
      alpha = 0.8,
      size = 2
    ) +
    ggplot2::scale_color_viridis_c(
      option = "plasma",
      name = "Year"
    ) +
    ggplot2::labs(
      x = "Intensification (kgN/ha)",
      y = "Specialization (synthetic share, feed import share)",
      shape = "Typology",
      title = "Provincial intensification and specialization in Spain (1860–2021)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "right",
      plot.title = ggplot2::element_text(face = "bold")
    )

  print(p)

  return(list(plot = p, data = df))
}
