typology_stacked_bars <- function() {
  indicators <- create_typo_ts_plot()
  n_prov_destiny <- create_n_prov_destiny() |>
    dplyr::filter(as.numeric(year) <= 2021)

  typologies_df <- indicators |>
    dplyr::group_by(year, province_name) |>
    dplyr::summarise(
      Typology = dplyr::first(Typology_base),
      .groups = "drop"
    )

  soil_inputs <- n_prov_destiny |>
    dplyr::filter(
      origin %in%
        c("Deposition", "Fixation", "Synthetic"),
      destiny %in% c("Cropland", "semi_natural_agroecosystems")
    )

  import_inputs <- n_prov_destiny |>
    dplyr::filter(
      origin == "Outside",
      destiny %in%
        c(
          "livestock_mono",
          "livestock_rum",
          "population_food",
          "population_other_uses"
        )
    )

  n_inputs <- dplyr::bind_rows(soil_inputs, import_inputs) |>
    dplyr::group_by(year, province_name) |>
    dplyr::summarise(
      Total_N_input = sum(mg_n, na.rm = TRUE),
      .groups = "drop"
    )

  df_total <- n_inputs |>
    dplyr::left_join(typologies_df, by = c("year", "province_name")) |>
    dplyr::group_by(year, Typology) |>
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
    "Disconnected crop-livestock systems (extensive)" = "#F6A640"
  )

  typology_order <- c(
    "Specialized cropping systems (intensive)",
    "Specialized cropping systems (extensive)",
    "Specialized livestock systems (intensive)",
    "Specialized livestock systems (extensive)",
    "Disconnected crop-livestock systems (intensive)",
    "Disconnected crop-livestock systems (extensive)",
    "Connected crop-livestock systems (intensive)",
    "Connected crop-livestock systems (extensive)",
    "Semi-natural agroecosystems"
  )

  df_total$Typology <- factor(
    df_total$Typology,
    levels = typology_order
  )

  year_breaks <- df_total$year |>
    unique() |>
    sort()
  year_breaks <- year_breaks[year_breaks %% 20 == 0]

  p_total <- ggplot2::ggplot(
    df_total,
    ggplot2::aes(
      x = factor(year),
      y = Total_N_input,
      fill = Typology
    )
  ) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::scale_x_discrete(breaks = year_breaks) +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(mult = c(0, 0.05))
    ) +
    ggplot2::scale_fill_manual(values = typology_colors) +
    ggplot2::labs(
      title = "Total N inputs by typology",
      x = "Year",
      y = "Total N input (Gg N)",
      fill = "Typology"
    ) +
    ggplot2::theme_minimal() +
    .stacked_bar_theme()

  df_pct <- n_inputs |>
    dplyr::left_join(typologies_df, by = c("year", "province_name")) |>
    dplyr::group_by(year, Typology) |>
    dplyr::summarise(
      Total_N_input = sum(Total_N_input, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::group_by(year) |>
    dplyr::mutate(
      Total_all = sum(Total_N_input, na.rm = TRUE),
      Percent_N_input = Total_N_input / Total_all * 100
    ) |>
    dplyr::ungroup()

  df_pct$Typology <- factor(
    df_pct$Typology,
    levels = typology_order
  )

  year_breaks_pct <- df_pct$year |>
    unique() |>
    sort()
  year_breaks_pct <- year_breaks_pct[year_breaks_pct %% 20 == 0]

  p_pct <- ggplot2::ggplot(
    df_pct,
    ggplot2::aes(
      x = factor(year),
      y = Percent_N_input,
      fill = Typology
    )
  ) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::scale_x_discrete(breaks = year_breaks_pct) +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(mult = c(0, 0.05))
    ) +
    ggplot2::scale_fill_manual(values = typology_colors) +
    ggplot2::labs(
      title = "Nitrogen inputs by typology (%)",
      x = "Year",
      y = "Share of total N input (%)",
      fill = "Typology"
    ) +
    ggplot2::theme_minimal() +
    .stacked_bar_theme()

  print(p_total)
  ggplot2::ggsave(
    "C:/PhD/Typologies/Typologies_spain/new_typologies/stacked_typologies_total.png",
    plot = p_total,
    width = 10,
    height = 6,
    dpi = 300
  )
  print(p_pct)

  list(total = df_total, pct = df_pct, p_total = p_total, p_pct = p_pct)
}

typology_area_stacked_bars <- function() {
  indicators <- create_typo_ts_plot()

  df_area <- indicators |>
    dplyr::group_by(year, Typology_base) |>
    dplyr::summarise(
      area_mha = sum(Area_ha, na.rm = TRUE) / 1e6,
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
    "Disconnected crop-livestock systems (extensive)" = "#F6A640"
  )

  typology_order <- c(
    "Specialized cropping systems (intensive)",
    "Specialized cropping systems (extensive)",
    "Specialized livestock systems (intensive)",
    "Specialized livestock systems (extensive)",
    "Disconnected crop-livestock systems (intensive)",
    "Disconnected crop-livestock systems (extensive)",
    "Connected crop-livestock systems (intensive)",
    "Connected crop-livestock systems (extensive)",
    "Semi-natural agroecosystems"
  )

  df_area$Typology_base <- factor(
    df_area$Typology_base,
    levels = typology_order
  )

  year_breaks <- df_area$year |>
    unique() |>
    sort()
  year_breaks <- year_breaks[year_breaks %% 20 == 0]

  ggplot2::ggplot(
    df_area,
    ggplot2::aes(x = factor(year), y = area_mha, fill = Typology_base)
  ) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::scale_x_discrete(breaks = year_breaks) +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(mult = c(0, 0.05))
    ) +
    ggplot2::scale_fill_manual(values = typology_colors) +
    ggplot2::labs(
      x = "Year",
      y = "Total area (Mha)",
      fill = "Typology"
    ) +
    ggplot2::theme_minimal() +
    .stacked_bar_theme()
}


.stacked_bar_theme <- function() {
  ggplot2::theme(
    axis.line = ggplot2::element_line(color = "grey70", linewidth = 0.4),
    axis.ticks = ggplot2::element_line(color = "grey70", linewidth = 0.4),
    axis.ticks.length = ggplot2::unit(3, "pt"),
    axis.text.x = ggplot2::element_text(
      angle = 45,
      hjust = 1,
      vjust = 1,
      margin = ggplot2::margin(t = 1)
    )
  )
}
