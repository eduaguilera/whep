typology_stacked_bars <- function() {
  indicators <- create_typologies_timeseries_plot()
  n_prov_destiny <- create_n_prov_destiny()

  typologies_df <- indicators |>
    dplyr::group_by(Year, Province_name) |>
    dplyr::summarise(Typology = dplyr::first(Typology), .groups = "drop")

  soil_inputs <- n_prov_destiny |>
    dplyr::filter(
      Origin %in%
        c("Deposition", "Fixation", "Synthetic", "Livestock", "People"),
      Destiny %in% c("Cropland", "semi_natural_agroecosystems")
    )

  import_inputs <- n_prov_destiny |>
    dplyr::filter(
      Origin == "Outside",
      Destiny %in%
        c(
          "livestock_mono",
          "livestock_rum",
          "population_food",
          "population_other_uses"
        )
    )

  n_inputs <- dplyr::bind_rows(soil_inputs, import_inputs) |>
    dplyr::group_by(Year, Province_name) |>
    dplyr::summarise(
      Total_N_input = sum(MgN, na.rm = TRUE),
      .groups = "drop"
    )

  df_total <- n_inputs |>
    dplyr::left_join(typologies_df, by = c("Year", "Province_name")) |>
    dplyr::group_by(Year, Typology) |>
    dplyr::summarise(
      Total_N_input = sum(Total_N_input, na.rm = TRUE) / 1000,
      .groups = "drop"
    )

  typology_colors <- c(
    "Semi-natural agroecosystems" = "#66a61e",
    "Specialized cropping systems (intensive)" = "#F7DD5A",
    "Specialized cropping systems (extensive)" = "#FFF7C2",
    "Specialized livestock systems (intensive)" = "#b3001b",
    "Specialized livestock systems (extensive)" = "#C94F6B",
    "Connected crop-livestock systems (intensive)" = "#7A4F20",
    "Connected crop-livestock systems (extensive)" = "#AF814B",
    "Disconnected crop-livestock systems (intensive)" = "#E67E00",
    "Disconnected crop-livestock systems (extensive)" = "#F6A640",
    "Urban systems" = "#6A5ACD"
  )

  df_total$Typology <- factor(
    df_total$Typology,
    levels = names(typology_colors)
  )

  year_breaks <- df_total$Year |>
    unique() |>
    sort()
  year_breaks <- year_breaks[year_breaks %% 20 == 0]

  p_total <- ggplot2::ggplot(
    df_total,
    ggplot2::aes(
      x = factor(Year),
      y = Total_N_input,
      fill = Typology
    )
  ) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::scale_x_discrete(breaks = year_breaks) +
    ggplot2::scale_fill_manual(values = typology_colors) +
    ggplot2::labs(
      title = "Total Nitrogen Inputs by Typology",
      x = "Year",
      y = "Total N Input (Gg N)",
      fill = "Typology"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  df_pct <- n_inputs |>
    dplyr::left_join(typologies_df, by = c("Year", "Province_name")) |>
    dplyr::group_by(Year, Typology) |>
    dplyr::summarise(
      Total_N_input = sum(Total_N_input, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::group_by(Year) |>
    dplyr::mutate(
      Total_all = sum(Total_N_input, na.rm = TRUE),
      Percent_N_input = Total_N_input / Total_all * 100
    ) |>
    dplyr::ungroup()

  df_pct$Typology <- factor(
    df_pct$Typology,
    levels = names(typology_colors)
  )

  year_breaks_pct <- df_pct$Year |>
    unique() |>
    sort()
  year_breaks_pct <- year_breaks_pct[year_breaks_pct %% 20 == 0]

  p_pct <- ggplot2::ggplot(
    df_pct,
    ggplot2::aes(
      x = factor(Year),
      y = Percent_N_input,
      fill = Typology
    )
  ) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::scale_x_discrete(breaks = year_breaks_pct) +
    ggplot2::scale_fill_manual(values = typology_colors) +
    ggplot2::labs(
      title = "Nitrogen Inputs by Typology (%)",
      x = "Year",
      y = "Share of Total N Input (%)",
      fill = "Typology"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  print(p_total)
  print(p_pct)

  list(total = df_total, pct = df_pct)
}
