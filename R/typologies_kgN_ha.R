typology_kgha_lines <- function() {
  indicators <- create_typologies_timeseries_plot()
  npp_ygpit <- whep_read_file("npp_ygpit")

  typologies_df <- indicators |>
    dplyr::select(Year, Province_name, Typology)

  n_balance <- whep_read_file("n_balance_ygpit_all")

  soil_agri <- n_balance |>
    dplyr::filter(
      LandUse %in% c("Cropland", "Dehesa", "Pasture_Shrubland")
    ) |>
    dplyr::mutate(
      Total_N_Mg = Deposition + BNF + Synthetic + Solid + Liquid + Urban
    ) |>
    dplyr::group_by(Year, Province_name) |>
    dplyr::summarise(
      Total_N_Mg = sum(Total_N_Mg, na.rm = TRUE),
      .groups = "drop"
    )

  soil_all <- n_balance |>
    dplyr::mutate(
      Total_N_Mg = Deposition + BNF + Synthetic + Solid + Liquid + Urban
    ) |>
    dplyr::group_by(Year, Province_name) |>
    dplyr::summarise(
      Total_N_Mg = sum(Total_N_Mg, na.rm = TRUE),
      .groups = "drop"
    )

  area_agri <- npp_ygpit |>
    dplyr::filter(
      LandUse %in% c("Cropland", "Pasture_Shrubland", "Dehesa")
    ) |>
    dplyr::group_by(Year, Province_name) |>
    dplyr::summarise(
      Area_ha = sum(Area_ygpit_ha, na.rm = TRUE),
      .groups = "drop"
    )

  area_all <- npp_ygpit |>
    dplyr::group_by(Year, Province_name) |>
    dplyr::summarise(
      Area_ha = sum(Area_ygpit_ha, na.rm = TRUE),
      .groups = "drop"
    )

  build_kgha <- function(area_df, soil_df) {
    soil_df |>
      dplyr::left_join(area_df, by = c("Year", "Province_name")) |>
      dplyr::left_join(typologies_df, by = c("Year", "Province_name")) |>
      dplyr::mutate(
        kgN_ha = (Total_N_Mg * 1000) / Area_ha
      ) |>
      dplyr::filter(!is.na(kgN_ha), Area_ha > 0) |>
      dplyr::group_by(Year, Typology) |>
      dplyr::summarise(
        mean_kgN = mean(kgN_ha, na.rm = TRUE),
        sd_kgN = sd(kgN_ha, na.rm = TRUE),
        .groups = "drop"
      )
  }

  df_agri <- build_kgha(area_agri, soil_agri)
  df_all <- build_kgha(area_all, soil_all)

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

  df_agri$Typology <- factor(df_agri$Typology, levels = names(typology_colors))
  df_all$Typology <- factor(df_all$Typology, levels = names(typology_colors))

  plot_fun <- function(df, title_text) {
    ggplot2::ggplot(
      df,
      ggplot2::aes(
        x = Year,
        y = mean_kgN,
        color = Typology,
        fill = Typology
      )
    ) +
      ggplot2::geom_ribbon(
        ggplot2::aes(
          ymin = mean_kgN - sd_kgN,
          ymax = mean_kgN + sd_kgN
        ),
        alpha = 0.2,
        colour = NA
      ) +
      ggplot2::geom_line(linewidth = 1.2) +
      ggplot2::scale_color_manual(values = typology_colors) +
      ggplot2::scale_fill_manual(values = typology_colors) +
      ggplot2::labs(
        title = title_text,
        x = "Year",
        y = "kg N per ha",
        color = "Typology",
        fill = "Typology"
      ) +
      ggplot2::theme_minimal()
  }

  p1 <- plot_fun(df_agri, "Nitrogen Inputs per ha (cropland and grassland)")
  p2 <- plot_fun(df_all, "Nitrogen Inputs per ha (total land area)")

  print(p1)
  print(p2)

  return(list(
    p1 = p1,
    p2 = p2,
    agricultural_land = df_agri,
    total_land = df_all
  ))
}
