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
      Origin %in% c("Synthetic", "Deposition", "Fixation"),
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
      specialization = pmax(synthetic_share, feed_import_share)
    ) |>
    dplyr::filter(!is.na(intensification), !is.na(specialization))

  # ---- Add typology ----
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

  # ---- Define periods ----
  df <- df |>
    dplyr::mutate(
      Period = dplyr::case_when(
        Year < 1900 ~ "1860–1900",
        Year < 1950 ~ "1900–1950",
        Year < 1990 ~ "1950–1990",
        TRUE ~ "1990–2021"
      )
    )

  df$Period <- factor(
    df$Period,
    levels = c("1860–1900", "1900–1950", "1950–1990", "1990–2021")
  )

  # ---- Plot ----
  typology_colors <- c(
    "Semi-natural agroecosystems" = "#66a61e",
    "Specialized cropping systems" = "#F7DD5A",
    "Specialized livestock systems" = "#b3001b",
    "Connected crop-livestock systems" = "#7A4F20",
    "Disconnected crop-livestock systems" = "#E67E00"
  )

  df_sample <- df |> dplyr::sample_frac(0.2)

  p <- ggplot2::ggplot(
    df_sample,
    ggplot2::aes(
      x = intensification,
      y = specialization,
      color = Typology
    )
  ) +
    ggplot2::geom_point(
      alpha = 0.8,
      size = 2
    ) +
    ggplot2::facet_wrap(~Period) +
    ggplot2::scale_color_manual(values = typology_colors) +
    ggplot2::labs(
      x = "Intensification (kg N / ha)",
      y = "Specialization (max synthetic and feed import share)",
      color = "Typology",
      title = "Provincial intensification and specialization in Spain (1860–2021)"
    ) +
    ggplot2::theme_minimal()

  print(p)

  return(list(plot = p, data = df))
}


circularity_intensification_plot <- function() {
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

  # ---- New N soil inputs ----
  n_inputs <- flows |>
    dplyr::filter(
      Origin %in% c("Synthetic", "Deposition", "Fixation"),
      Destiny %in% c("Cropland", "semi_natural_agroecosystems")
    ) |>
    dplyr::group_by(Year, Province_name) |>
    dplyr::summarise(
      total_input_mg = sum(MgN, na.rm = TRUE),
      .groups = "drop"
    )

  # ---- Manure recycling share ----
  manure_share <- flows |>
    dplyr::filter(
      Destiny %in% c("Cropland", "semi_natural_agroecosystems"),
      Origin %in%
        c("Livestock", "Synthetic", "Fixation", "Deposition", "People")
    ) |>
    dplyr::group_by(Year, Province_name) |>
    dplyr::summarise(
      manure_share = sum(MgN[Origin == "Livestock"], na.rm = TRUE) /
        sum(MgN, na.rm = TRUE),
      .groups = "drop"
    )

  # ---- Urban recycling share ----
  urban_share <- flows |>
    dplyr::filter(
      Destiny %in% c("Cropland", "semi_natural_agroecosystems"),
      Origin %in%
        c("Livestock", "Synthetic", "Fixation", "Deposition", "People")
    ) |>
    dplyr::group_by(Year, Province_name) |>
    dplyr::summarise(
      urban_share = sum(MgN[Origin == "People"], na.rm = TRUE) /
        sum(MgN, na.rm = TRUE),
      .groups = "drop"
    )

  # ---- Local feed share ----
  local_feed_share <- flows |>
    dplyr::filter(
      Destiny %in% c("livestock_mono", "livestock_rum"),
      Origin %in% c("Cropland", "semi_natural_agroecosystems", "Outside")
    ) |>
    dplyr::group_by(Year, Province_name) |>
    dplyr::summarise(
      local_feed_share = sum(
        MgN[Origin %in% c("Cropland", "semi_natural_agroecosystems")],
        na.rm = TRUE
      ) /
        sum(MgN, na.rm = TRUE),
      .groups = "drop"
    )

  # ---- Combine indicators ----
  df <- n_inputs |>
    dplyr::left_join(area_df, by = c("Year", "Province_name")) |>
    dplyr::left_join(manure_share, by = c("Year", "Province_name")) |>
    dplyr::left_join(urban_share, by = c("Year", "Province_name")) |>
    dplyr::left_join(local_feed_share, by = c("Year", "Province_name")) |>
    dplyr::mutate(
      intensification = (total_input_mg * 1000) / area_ha,
      circularity = (manure_share + urban_share + local_feed_share) / 3
    ) |>
    dplyr::filter(!is.na(intensification), !is.na(circularity))

  # ---- Add typology ----
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

  # ---- Define periods ----
  df <- df |>
    dplyr::mutate(
      Period = dplyr::case_when(
        Year < 1900 ~ "1860–1900",
        Year < 1950 ~ "1900–1950",
        Year < 1990 ~ "1950–1990",
        TRUE ~ "1990–2021"
      )
    )

  df$Period <- factor(
    df$Period,
    levels = c("1860–1900", "1900–1950", "1950–1990", "1990–2021")
  )

  # ---- Typology colors ----
  typology_colors <- c(
    "Semi-natural agroecosystems" = "#66a61e",
    "Specialized cropping systems" = "#F7DD5A",
    "Specialized livestock systems" = "#b3001b",
    "Connected crop-livestock systems" = "#7A4F20",
    "Disconnected crop-livestock systems" = "#E67E00"
  )

  # ---- Plot ----
  p <- ggplot2::ggplot(
    df,
    ggplot2::aes(
      x = intensification,
      y = circularity,
      color = Typology
    )
  ) +
    ggplot2::geom_point(
      alpha = 0.8,
      size = 2
    ) +
    ggplot2::facet_wrap(~Period) +
    ggplot2::scale_color_manual(values = typology_colors) +
    ggplot2::labs(
      x = "Intensification (kg N / ha)",
      y = "Circularity index",
      color = "Typology",
      title = "Circularity and intensification in Spanish agro-food systems (1860–2021)"
    ) +
    ggplot2::theme_minimal()

  print(p)

  return(list(plot = p, data = df))
}

circularity_nue_trajectory_plot <- function() {
  # ---- Load data ----
  flows <- create_n_prov_destiny()
  npp_ygpit <- whep_read_file("npp_ygpit")

  # ---- Cropland area ----
  cropland_area <- npp_ygpit |>
    dplyr::filter(
      LandUse %in%
        c(
          "Cropland",
          "semi_natural_agroecosystems"
        )
    ) |>
    dplyr::group_by(Year) |>
    dplyr::summarise(
      cropland_area = sum(Area_ygpit_ha, na.rm = TRUE),
      .groups = "drop"
    )

  # ---- Crop production ----
  crop_production <- flows |>
    dplyr::filter(
      Origin %in%
        c(
          "Cropland",
          "semi_natural_agroecosystems"
        ),
      Destiny %in%
        c(
          "population_food",
          "livestock_rum",
          "livestock_mono",
          "export"
        )
    ) |>
    dplyr::group_by(Year) |>
    dplyr::summarise(
      crop_N = sum(MgN, na.rm = TRUE),
      .groups = "drop"
    )

  # ---- Total N inputs ----
  n_inputs <- flows |>
    dplyr::filter(
      Origin %in%
        c(
          "Synthetic",
          "Fixation",
          "Deposition",
          "Livestock",
          "People"
        ),
      Destiny %in%
        c(
          "Cropland",
          "semi_natural_agroecosystems"
        )
    ) |>
    dplyr::group_by(Year) |>
    dplyr::summarise(
      total_inputs = sum(MgN, na.rm = TRUE),
      .groups = "drop"
    )

  # ---- Manure recycling share ----
  manure_share <- flows |>
    dplyr::filter(
      Destiny %in% c("Cropland", "semi_natural_agroecosystems"),
      Origin %in%
        c("Livestock", "Synthetic", "Fixation", "Deposition", "People")
    ) |>
    dplyr::group_by(Year) |>
    dplyr::summarise(
      manure_share = sum(MgN[Origin == "Livestock"], na.rm = TRUE) /
        sum(MgN, na.rm = TRUE),
      .groups = "drop"
    )

  # ---- Urban recycling share ----
  urban_share <- flows |>
    dplyr::filter(
      Destiny %in% c("Cropland", "semi_natural_agroecosystems"),
      Origin %in%
        c("Livestock", "Synthetic", "Fixation", "Deposition", "People")
    ) |>
    dplyr::group_by(Year) |>
    dplyr::summarise(
      urban_share = sum(MgN[Origin == "People"], na.rm = TRUE) /
        sum(MgN, na.rm = TRUE),
      .groups = "drop"
    )

  # ---- Local feed share ----
  local_feed_share <- flows |>
    dplyr::filter(
      Destiny %in% c("livestock_mono", "livestock_rum"),
      Origin %in% c("Cropland", "semi_natural_agroecosystems", "Outside")
    ) |>
    dplyr::group_by(Year) |>
    dplyr::summarise(
      local_feed_share = sum(
        MgN[Origin %in% c("Cropland", "semi_natural_agroecosystems")],
        na.rm = TRUE
      ) /
        sum(MgN, na.rm = TRUE),
      .groups = "drop"
    )

  # ---- Combine indicators ----
  df <- cropland_area |>
    dplyr::left_join(crop_production, by = "Year") |>
    dplyr::left_join(n_inputs, by = "Year") |>
    dplyr::left_join(manure_share, by = "Year") |>
    dplyr::left_join(urban_share, by = "Year") |>
    dplyr::left_join(local_feed_share, by = "Year") |>
    dplyr::mutate(
      # NUE
      NUE = (crop_N / total_inputs) * 100,

      # Circularity index
      circularity = (manure_share + urban_share + local_feed_share) / 3
    )

  # ---- Plot ----
  p <- ggplot2::ggplot(
    df,
    ggplot2::aes(
      x = NUE,
      y = circularity,
      color = Year
    )
  ) +
    ggplot2::geom_point(size = 3) +
    ggplot2::scale_color_viridis_c(option = "plasma") +
    ggplot2::labs(
      x = "NUE (%)",
      y = "Circularity index",
      color = "Year",
      title = "Circularity and NUE in Spain (1860–2021)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold")
    )

  print(p)

  return(list(plot = p, data = df))
}

circularity_nue_cropland_timeseries_plot <- function() {
  # ---- Load data ----
  flows <- create_n_prov_destiny()

  # ---- Crop production ----
  crop_production <- flows |>
    dplyr::filter(
      Origin == "Cropland",
      Destiny %in%
        c(
          "population_food",
          "livestock_rum",
          "livestock_mono",
          "export"
        )
    ) |>
    dplyr::group_by(Year) |>
    dplyr::summarise(
      crop_N = sum(MgN, na.rm = TRUE),
      .groups = "drop"
    )

  # ---- Total N inputs to cropland ----
  n_inputs <- flows |>
    dplyr::filter(
      Origin %in%
        c(
          "Synthetic",
          "Fixation",
          "Deposition",
          "Livestock",
          "People"
        ),
      Destiny == "Cropland"
    ) |>
    dplyr::group_by(Year) |>
    dplyr::summarise(
      total_inputs = sum(MgN, na.rm = TRUE),
      .groups = "drop"
    )

  # ---- Manure recycling share ----
  manure_share <- flows |>
    dplyr::filter(
      Destiny == "Cropland",
      Origin %in%
        c(
          "Livestock",
          "Synthetic",
          "Fixation",
          "Deposition",
          "People"
        )
    ) |>
    dplyr::group_by(Year) |>
    dplyr::summarise(
      manure_share = sum(MgN[Origin == "Livestock"], na.rm = TRUE) /
        sum(MgN, na.rm = TRUE),
      .groups = "drop"
    )

  # ---- Urban recycling share ----
  urban_share <- flows |>
    dplyr::filter(
      Destiny == "Cropland",
      Origin %in%
        c(
          "Livestock",
          "Synthetic",
          "Fixation",
          "Deposition",
          "People"
        )
    ) |>
    dplyr::group_by(Year) |>
    dplyr::summarise(
      urban_share = sum(MgN[Origin == "People"], na.rm = TRUE) /
        sum(MgN, na.rm = TRUE),
      .groups = "drop"
    )

  # ---- Local feed share ----
  local_feed_share <- flows |>
    dplyr::filter(
      Box == "Cropland",
      Destiny %in%
        c(
          "livestock_mono",
          "livestock_rum"
        ),
      Origin %in%
        c(
          "Cropland",
          "Outside"
        )
    ) |>
    dplyr::group_by(Year) |>
    dplyr::summarise(
      local_feed_share = sum(MgN[Origin == "Cropland"], na.rm = TRUE) /
        sum(MgN, na.rm = TRUE),
      .groups = "drop"
    )

  # ---- Combine indicators ----
  df <- crop_production |>
    dplyr::left_join(n_inputs, by = "Year") |>
    dplyr::left_join(manure_share, by = "Year") |>
    dplyr::left_join(urban_share, by = "Year") |>
    dplyr::left_join(local_feed_share, by = "Year") |>
    dplyr::mutate(
      # Crop NUE
      NUE = (crop_N / total_inputs) * 100,

      # Circularity index
      circularity = ((manure_share + urban_share + local_feed_share) / 3) * 100
    )

  # ---- Long format ----
  df_long <- df |>
    tidyr::pivot_longer(
      cols = c(NUE, circularity),
      names_to = "indicator",
      values_to = "value"
    )

  # ---- Plot ----
  p <- ggplot2::ggplot(
    df_long,
    ggplot2::aes(
      x = Year,
      y = value,
      color = indicator
    )
  ) +
    ggplot2::geom_line(size = 1.3) +
    ggplot2::scale_color_manual(
      values = c(
        NUE = "#e41a1c",
        circularity = "#377eb8"
      ),
      labels = c(
        "Crop NUE",
        "Circularity"
      )
    ) +
    ggplot2::labs(
      x = "Year",
      y = "Percent (%)",
      color = "Indicator",
      title = "Crop nitrogen use efficiency and circularity in Spain (1860–2021)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold")
    )

  print(p)

  # ---- Save plot ----
  ggplot2::ggsave(
    "C:/PhD/Typologies/Typologies_spain/typology_plot/circularity_nue_cropland_timeseries.jpeg",
    plot = p,
    width = 10,
    height = 7,
    dpi = 300
  )

  return(list(plot = p, data = df))
}

intensification_specialization_timeseries <- function() {
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

  # ---- New N soil inputs ----
  n_inputs <- flows |>
    dplyr::filter(
      Origin %in% c("Synthetic", "Deposition", "Fixation"),
      Destiny %in% c("Cropland", "semi_natural_agroecosystems")
    ) |>
    dplyr::group_by(Year, Province_name) |>
    dplyr::summarise(
      total_input_mg = sum(MgN, na.rm = TRUE),
      .groups = "drop"
    )

  # ---- Synthetic fertilizer share ----
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
      # Intensification (kg N per ha)
      intensification = (total_input_mg * 1000) / area_ha,

      # Specialization index
      specialization = (synthetic_share + feed_import_share) / 2
    )

  # ---- National yearly averages ----
  df_ts <- df |>
    dplyr::group_by(Year) |>
    dplyr::summarise(
      intensification = mean(intensification, na.rm = TRUE),
      specialization = mean(specialization, na.rm = TRUE),
      .groups = "drop"
    )

  # ---- Index (1860 = 100) ----
  base <- df_ts |> dplyr::filter(Year == min(Year))

  df_ts <- df_ts |>
    dplyr::mutate(
      intensification_index = intensification / base$intensification * 100,

      specialization_index = specialization / base$specialization * 100
    )

  # ---- Convert to long format ----
  df_long <- df_ts |>
    tidyr::pivot_longer(
      cols = c(
        intensification_index,
        specialization_index
      ),
      names_to = "indicator",
      values_to = "value"
    )

  # ---- Plot ----
  p <- ggplot2::ggplot(
    df_long,
    ggplot2::aes(
      x = Year,
      y = value,
      color = indicator
    )
  ) +
    ggplot2::geom_line(size = 1.2) +
    ggplot2::scale_color_manual(
      values = c(
        "intensification_index" = "#e41a1c",
        "specialization_index" = "#377eb8"
      ),
      labels = c(
        "Intensification",
        "Specialization"
      )
    ) +
    ggplot2::labs(
      x = "Year",
      y = "Index (1860 = 100)",
      color = "Indicator",
      title = "Intensification and specialization in Spain (1860–2020)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      legend.position = "right"
    )

  print(p)

  # ---- Save plot ----
  ggplot2::ggsave(
    filename = "C:/PhD/Typologies/Typologies_spain/typology_plot/intensification_specialization_timeseries.jpeg",
    plot = p,
    width = 10,
    height = 7,
    dpi = 300
  )

  return(list(plot = p, data = df_ts))
}


yield_nue_trajectory_plot <- function() {
  # ---- Load data ----
  flows <- create_n_prov_destiny()
  npp_ygpit <- whep_read_file("npp_ygpit")

  # ---- Cropland area ----
  cropland_area <- npp_ygpit |>
    dplyr::filter(LandUse == "Cropland") |>
    dplyr::group_by(Year) |>
    dplyr::summarise(
      cropland_area = sum(Area_ygpit_ha, na.rm = TRUE),
      .groups = "drop"
    )

  # ---- Crop production ----
  crop_production <- flows |>
    dplyr::filter(
      Origin == "Cropland",
      Destiny %in%
        c(
          "population_food",
          "livestock_rum",
          "livestock_mono",
          "export"
        )
    ) |>
    dplyr::group_by(Year) |>
    dplyr::summarise(
      crop_N = sum(MgN, na.rm = TRUE),
      .groups = "drop"
    )

  # ---- Total N inputs ----
  n_inputs <- flows |>
    dplyr::filter(
      Origin %in%
        c(
          "Synthetic",
          "Fixation",
          "Deposition",
          "Livestock",
          "People"
        ),
      Destiny == "Cropland"
    ) |>
    dplyr::group_by(Year) |>
    dplyr::summarise(
      total_inputs = sum(MgN, na.rm = TRUE),
      .groups = "drop"
    )

  # ---- Combine ----
  df <- cropland_area |>
    dplyr::left_join(crop_production, by = "Year") |>
    dplyr::left_join(n_inputs, by = "Year") |>
    dplyr::mutate(
      # Crop productivity (kg N / ha)
      yield = (crop_N * 1000) / cropland_area,

      # Nitrogen Use Efficiency (%)
      NUE = (crop_N / total_inputs) * 100
    )

  # ---- Plot ----
  p <- ggplot2::ggplot(
    df,
    ggplot2::aes(
      x = NUE,
      y = yield,
      color = Year
    )
  ) +
    ggplot2::geom_point(size = 3) +
    ggplot2::scale_color_viridis_c(option = "plasma") +
    ggplot2::labs(
      x = "NUE (%)",
      y = "Crop productivity (kgN/ha)",
      color = "Year",
      title = "Crop productivity (intensification) and crop NUE in Spain (1860–2020)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold")
    )

  print(p)

  # ---- Save plot ----
  ggplot2::ggsave(
    "C:/PhD/Typologies/Typologies_spain/typology_plot/yield_nue_trajectory.jpeg",
    plot = p,
    width = 10,
    height = 7,
    dpi = 300
  )

  return(list(plot = p, data = df))
}


intensification_trajectory_plot <- function() {
  # ---- Load data ----
  flows <- create_n_prov_destiny()
  npp_ygpit <- whep_read_file("npp_ygpit")

  # ---- Cropland area ----
  cropland_area <- npp_ygpit |>
    dplyr::filter(LandUse == "Cropland") |>
    dplyr::group_by(Year) |>
    dplyr::summarise(
      cropland_area = sum(Area_ygpit_ha, na.rm = TRUE),
      .groups = "drop"
    )

  # ---- Crop production (N) ----
  crop_production <- flows |>
    dplyr::filter(
      Origin == "Cropland",
      Destiny %in%
        c(
          "population_food",
          "livestock_rum",
          "livestock_mono",
          "export"
        )
    ) |>
    dplyr::group_by(Year) |>
    dplyr::summarise(
      crop_N = sum(MgN, na.rm = TRUE),
      .groups = "drop"
    )

  # ---- Synthetic fertilizer ----
  fertilizer_input <- flows |>
    dplyr::filter(
      Origin == "Synthetic",
      Destiny == "Cropland"
    ) |>
    dplyr::group_by(Year) |>
    dplyr::summarise(
      fertilizer_N = sum(MgN, na.rm = TRUE),
      .groups = "drop"
    )

  # ---- Combine ----
  df <- cropland_area |>
    dplyr::left_join(crop_production, by = "Year") |>
    dplyr::left_join(fertilizer_input, by = "Year") |>
    dplyr::mutate(
      # Land productivity (kg N / ha)
      land_productivity = (crop_N * 1000) / cropland_area,

      # Nitrogen intensity (kg N / ha)
      nitrogen_intensity = (fertilizer_N * 1000) / cropland_area
    )

  # ---- Plot ----
  p <- ggplot2::ggplot(
    df,
    ggplot2::aes(
      x = nitrogen_intensity,
      y = land_productivity,
      color = Year
    )
  ) +
    ggplot2::geom_point(size = 3) +
    ggplot2::scale_color_viridis_c(option = "plasma") +
    ggplot2::labs(
      x = "Nitrogen input intensity (kg N / ha)",
      y = "Land productivity (kg N / ha)",
      color = "Year",
      title = "Agricultural intensification in Spain (1860–2020)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold")
    )

  print(p)

  # ---- Save plot ----
  ggplot2::ggsave(
    "C:/PhD/Typologies/Typologies_spain/typology_plot/intensification_trajectory.jpeg",
    plot = p,
    width = 10,
    height = 7,
    dpi = 300
  )

  return(list(plot = p, data = df))
}

intensification_timeseries_plot <- function() {
  # ---- Load data ----
  flows <- create_n_prov_destiny()
  npp_ygpit <- whep_read_file("npp_ygpit")

  # ---- Cropland area ----
  cropland_area <- npp_ygpit |>
    dplyr::filter(LandUse == "Cropland") |>
    dplyr::group_by(Year) |>
    dplyr::summarise(
      cropland_area = sum(Area_ygpit_ha, na.rm = TRUE),
      .groups = "drop"
    )

  # ---- Crop production ----
  crop_production <- flows |>
    dplyr::filter(
      Origin == "Cropland",
      Destiny %in%
        c(
          "population_food",
          "livestock_rum",
          "livestock_mono",
          "export"
        )
    ) |>
    dplyr::group_by(Year) |>
    dplyr::summarise(
      crop_N = sum(MgN, na.rm = TRUE),
      .groups = "drop"
    )

  # ---- Synthetic fertilizer ----
  fertilizer_input <- flows |>
    dplyr::filter(
      Origin == "Synthetic",
      Destiny == "Cropland"
    ) |>
    dplyr::group_by(Year) |>
    dplyr::summarise(
      fertilizer_N = sum(MgN, na.rm = TRUE),
      .groups = "drop"
    )

  # ---- Combine ----
  df <- cropland_area |>
    dplyr::left_join(crop_production, by = "Year") |>
    dplyr::left_join(fertilizer_input, by = "Year") |>
    dplyr::mutate(
      land_productivity = (crop_N * 1000) / cropland_area,
      nitrogen_intensity = (fertilizer_N * 1000) / cropland_area
    )

  # ---- Convert to long format ----
  df_long <- df |>
    tidyr::pivot_longer(
      cols = c(land_productivity, nitrogen_intensity),
      names_to = "indicator",
      values_to = "value"
    )

  # ---- Plot ----
  p <- ggplot2::ggplot(
    df_long,
    ggplot2::aes(
      x = Year,
      y = value,
      color = indicator
    )
  ) +
    ggplot2::geom_line(size = 1.3) +
    ggplot2::scale_color_manual(
      values = c(
        land_productivity = "#1b9e77",
        nitrogen_intensity = "#d95f02"
      ),
      labels = c(
        "Land productivity",
        "Nitrogen input intensity"
      )
    ) +
    ggplot2::labs(
      x = "Year",
      y = "kg N / ha",
      color = "Indicator",
      title = "N inputs and crop productivity in Spain (1860–2020)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      legend.position = "right"
    )

  print(p)

  # ---- Save plot ----
  ggplot2::ggsave(
    "C:/PhD/Typologies/Typologies_spain/typology_plot/intensification_timeseries.jpeg",
    plot = p,
    width = 10,
    height = 7,
    dpi = 300
  )

  return(list(plot = p, data = df))
}

nue_fertilizer_timeseries_plot <- function() {
  flows <- create_n_prov_destiny()

  # ---- Crop production ----
  crop_production <- flows |>
    dplyr::filter(
      Origin == "Cropland",
      Destiny %in%
        c(
          "population_food",
          "livestock_rum",
          "livestock_mono",
          "export"
        )
    ) |>
    dplyr::group_by(Year) |>
    dplyr::summarise(
      crop_N = sum(MgN, na.rm = TRUE),
      .groups = "drop"
    )

  # ---- Soil inputs ----
  inputs <- flows |>
    dplyr::filter(
      Origin %in%
        c(
          "Synthetic",
          "Livestock",
          "People",
          "Fixation",
          "Deposition"
        ),
      Destiny == "Cropland"
    ) |>
    dplyr::group_by(Year) |>
    dplyr::summarise(
      total_inputs = sum(MgN, na.rm = TRUE),

      synthetic = sum(MgN[Origin == "Synthetic"], na.rm = TRUE),

      organic = sum(
        MgN[Origin %in% c("Livestock", "People")],
        na.rm = TRUE
      ),

      fixation_dep = sum(
        MgN[Origin %in% c("Fixation", "Deposition")],
        na.rm = TRUE
      ),

      .groups = "drop"
    )

  # ---- Combine indicators ----
  df <- crop_production |>
    dplyr::left_join(inputs, by = "Year") |>
    dplyr::mutate(
      NUE = (crop_N / total_inputs) * 100,

      synthetic_share = (synthetic / total_inputs) * 100,

      organic_share = (organic / total_inputs) * 100,

      fertilizer_dependency = ((synthetic + organic) / total_inputs) * 100
    )

  # ---- Long format ----
  df_long <- df |>
    tidyr::pivot_longer(
      cols = c(
        NUE,
        synthetic_share,
        organic_share,
        fertilizer_dependency
      ),
      names_to = "indicator",
      values_to = "value"
    )

  # ---- Order ----
  df_long$indicator <- factor(
    df_long$indicator,
    levels = c(
      "NUE",
      "fertilizer_dependency",
      "synthetic_share",
      "organic_share"
    )
  )

  # ---- Plot ----
  p <- ggplot2::ggplot(
    df_long,
    ggplot2::aes(x = Year, y = value, color = indicator)
  ) +
    ggplot2::geom_line(size = 1.3) +
    ggplot2::scale_color_manual(
      values = c(
        NUE = "#7570b3",
        fertilizer_dependency = "#e7298a",
        synthetic_share = "#d95f02",
        organic_share = "#1b9e77"
      ),
      labels = c(
        NUE = "Nitrogen use efficiency (NUE)",
        fertilizer_dependency = "Fertilizer dependency",
        synthetic_share = "Mineral fertilizer share",
        organic_share = "Organic fertilizer share"
      )
    ) +
    ggplot2::labs(
      x = "Year",
      y = "Percent (%)",
      color = "Indicator",
      title = "NUE and fertilizer dependency in Spanish cropland (1860–2021)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold")
    )

  print(p)

  ggplot2::ggsave(
    filename = "C:/PhD/Typologies/Typologies_spain/typology_plot/nue_fertilizer_time.jpeg",
    plot = p,
    width = 12,
    height = 8,
    dpi = 300
  )

  return(list(plot = p, data = df))
}

nue_fertilizer_trajectory_plot <- function() {
  # ---- Load data ----
  flows <- create_n_prov_destiny()

  # ---- Crop production (N output) ----
  crop_production <- flows |>
    dplyr::filter(
      Origin == "Cropland",
      Destiny %in%
        c(
          "population_food",
          "livestock_rum",
          "livestock_mono",
          "export"
        )
    ) |>
    dplyr::group_by(Year) |>
    dplyr::summarise(
      crop_N = sum(MgN, na.rm = TRUE),
      .groups = "drop"
    )

  # ---- Soil N inputs ----
  inputs <- flows |>
    dplyr::filter(
      Origin %in%
        c(
          "Synthetic",
          "Livestock",
          "People",
          "Fixation",
          "Deposition"
        ),
      Destiny == "Cropland"
    ) |>
    dplyr::group_by(Year) |>
    dplyr::summarise(
      total_inputs = sum(MgN, na.rm = TRUE),

      fertilizer = sum(
        MgN[Origin %in% c("Synthetic", "Livestock", "People")],
        na.rm = TRUE
      ),

      .groups = "drop"
    )

  # ---- Combine indicators ----
  df <- crop_production |>
    dplyr::left_join(inputs, by = "Year") |>
    dplyr::mutate(
      # Nitrogen use efficiency
      NUE = (crop_N / total_inputs) * 100,

      # Fertilizer dependency
      fertilizer_dependency = (fertilizer / total_inputs) * 100
    )

  # ---- Plot ----
  p <- ggplot2::ggplot(
    df,
    ggplot2::aes(
      x = NUE,
      y = fertilizer_dependency,
      color = Year
    )
  ) +
    ggplot2::geom_point(size = 3) +
    ggplot2::scale_color_viridis_c(option = "plasma") +
    ggplot2::labs(
      x = "Nitrogen use efficiency (%)",
      y = "Fertilizer dependency (%)",
      color = "Year",
      title = "NUE and fertilizer dependency trajectory in Spanish cropland (1860–2021)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold")
    )

  print(p)

  # ---- Save plot ----
  ggplot2::ggsave(
    filename = "C:/PhD/Typologies/Typologies_spain/typology_plot/nue_fertilizer_dependency.jpeg",
    plot = p,
    width = 10,
    height = 7,
    dpi = 300
  )

  return(list(plot = p, data = df))
}


nrr_cropland_timeseries_plot <- function() {
  # ---- Load data ----
  flows <- create_n_prov_destiny()

  # ---- Crop production (N output) ----
  crop_outputs <- flows |>
    dplyr::filter(
      Origin == "Cropland",
      Destiny %in%
        c(
          "population_food",
          "livestock_rum",
          "livestock_mono",
          "export"
        )
    ) |>
    dplyr::group_by(Year) |>
    dplyr::summarise(
      crop_output = sum(MgN, na.rm = TRUE),
      .groups = "drop"
    )

  # ---- N soil inputs ----
  crop_inputs <- flows |>
    dplyr::filter(
      Origin %in%
        c(
          "Synthetic",
          "Livestock",
          "People",
          "Fixation",
          "Deposition"
        ),
      Destiny == "Cropland"
    ) |>
    dplyr::group_by(Year) |>
    dplyr::summarise(
      crop_input = sum(MgN, na.rm = TRUE),
      .groups = "drop"
    )

  # ---- Recycled N (manure + urban) ----
  recycled_N <- flows |>
    dplyr::filter(
      Origin %in% c("Livestock", "People"),
      Destiny == "Cropland"
    ) |>
    dplyr::group_by(Year) |>
    dplyr::summarise(
      recycled = sum(MgN, na.rm = TRUE),
      .groups = "drop"
    )

  # ---- Combine ----
  df <- crop_inputs |>
    dplyr::left_join(crop_outputs, by = "Year") |>
    dplyr::left_join(recycled_N, by = "Year") |>
    dplyr::mutate(
      crop_output = dplyr::coalesce(crop_output, 0),
      recycled = dplyr::coalesce(recycled, 0),

      surplus = crop_input - crop_output,

      NRR = recycled / (recycled + surplus) * 100
    )

  # ---- Plot ----
  p <- ggplot2::ggplot(
    df,
    ggplot2::aes(
      x = Year,
      y = NRR
    )
  ) +
    ggplot2::geom_line(
      color = "#1b9e77",
      size = 1.3
    ) +
    ggplot2::labs(
      x = "Year",
      y = "Nitrogen recycling rate (%)",
      title = "Nitrogen recycling rate (NRR) in Spanish cropland (1860–2021)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold")
    )

  print(p)

  # ---- Save ----
  ggplot2::ggsave(
    "C:/PhD/Typologies/Typologies_spain/typology_plot/nrr_cropland_timeseries.jpeg",
    plot = p,
    width = 10,
    height = 7,
    dpi = 300
  )

  return(list(plot = p, data = df))
}


production_diversity_plot <- function() {
  # ---- Load data ----
  flows <- create_n_prov_destiny()

  items <- readxl::read_excel(
    "C:/PhD/GRAFS/Inputs_SACO/Codes_coefs.xlsx",
    sheet = "Names_biomass_CB"
  )

  df <- dplyr::left_join(
    flows,
    items,
    by = c("Item" = "Name_biomass")
  )

  df <- df |>
    dplyr::filter(
      Destiny %in%
        c(
          "population_food",
          "population_other_uses",
          "livestock_rum",
          "livestock_mono",
          "export"
        ),
      Origin != "Outside",
      !is.na(Cat_1)
    )

  prod_group <- df |>
    dplyr::group_by(Year, Province_name, Cat_1) |>
    dplyr::summarise(
      value = sum(MgN, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::filter(value > 0)

  # ---- Shannon index ----
  shannon_index <- function(x) {
    p <- x / sum(x)
    p <- p[p > 0]
    -sum(p * log(p))
  }

  diversity <- prod_group |>
    dplyr::group_by(Year, Province_name) |>
    dplyr::summarise(
      H = shannon_index(value),
      n_groups = dplyr::n(),
      H_norm = dplyr::if_else(n_groups > 1, H / log(n_groups), NA_real_),
      .groups = "drop"
    )

  diversity_ts <- diversity |>
    dplyr::group_by(Year) |>
    dplyr::summarise(
      diversity = mean(H_norm, na.rm = TRUE),
      .groups = "drop"
    )

  # ---- Plot ----
  p <- ggplot2::ggplot(
    diversity_ts,
    ggplot2::aes(x = Year, y = diversity)
  ) +
    ggplot2::geom_line(
      color = "#1b9e77",
      size = 1.3
    ) +
    ggplot2::labs(
      x = "Year",
      y = "Shannon index",
      title = "Production diversity (Shannon index) in Spain (1860–2021)"
    ) +
    ggplot2::theme_minimal()

  print(p)

  return(list(plot = p, data = diversity_ts))
}

intensification_specialization_secondary_axis <- function() {
  flows <- create_n_prov_destiny()
  npp_ygpit <- whep_read_file("npp_ygpit")

  items <- readxl::read_excel(
    "C:/PhD/GRAFS/Inputs_SACO/Codes_coefs.xlsx",
    sheet = "Names_biomass_CB"
  )

  df_prod <- flows |>
    dplyr::left_join(items, by = c("Item" = "Name_biomass")) |>
    dplyr::filter(
      Destiny %in%
        c(
          "population_food",
          "population_other_uses",
          "livestock_rum",
          "livestock_mono",
          "export"
        ),
      Origin != "Outside",
      !is.na(Cat_1)
    )

  prod_group <- df_prod |>
    dplyr::group_by(Year, Province_name, Cat_1) |>
    dplyr::summarise(
      value = sum(MgN, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::filter(value > 0)

  shannon_index <- function(x) {
    p <- x / sum(x)
    p <- p[p > 0]
    -sum(p * log(p))
  }

  specialization_ts <- prod_group |>
    dplyr::group_by(Year, Province_name) |>
    dplyr::summarise(
      H = shannon_index(value),
      n_groups = dplyr::n(),
      H_norm = dplyr::if_else(n_groups > 1, H / log(n_groups), NA_real_),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      specialization = 1 - H_norm
    ) |>
    dplyr::group_by(Year) |>
    dplyr::summarise(
      specialization = mean(specialization, na.rm = TRUE),
      .groups = "drop"
    )

  area_df <- npp_ygpit |>
    dplyr::group_by(Year, Province_name) |>
    dplyr::summarise(
      area_ha = sum(Area_ygpit_ha, na.rm = TRUE),
      .groups = "drop"
    )

  intensification_ts <- flows |>
    dplyr::filter(
      Origin %in% c("Synthetic", "Deposition", "Fixation"),
      Destiny %in% c("Cropland", "semi_natural_agroecosystems")
    ) |>
    dplyr::group_by(Year, Province_name) |>
    dplyr::summarise(
      total_input_mg = sum(MgN, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::left_join(area_df, by = c("Year", "Province_name")) |>
    dplyr::mutate(
      intensification = (total_input_mg * 1000) / area_ha
    ) |>
    dplyr::group_by(Year) |>
    dplyr::summarise(
      intensification = mean(intensification, na.rm = TRUE),
      .groups = "drop"
    )

  df <- intensification_ts |>
    dplyr::left_join(specialization_ts, by = "Year")

  scale_factor <- max(df$intensification, na.rm = TRUE) /
    max(df$specialization, na.rm = TRUE)

  p <- ggplot2::ggplot(df, ggplot2::aes(x = Year)) +
    ggplot2::geom_line(
      ggplot2::aes(y = intensification, color = "Intensification"),
      size = 1.3
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = specialization * scale_factor, color = "Specialization"),
      size = 1.3
    ) +
    ggplot2::scale_y_continuous(
      name = "Intensification (kg N / ha)",
      sec.axis = ggplot2::sec_axis(
        ~ . / scale_factor,
        name = "Specialization (1 - Shannon)"
      )
    ) +
    ggplot2::scale_color_manual(
      values = c(
        "Intensification" = "#e41a1c",
        "Specialization" = "#377eb8"
      )
    ) +
    ggplot2::labs(
      x = "Year",
      color = "Indicator",
      title = "Intensification and production specialization in Spain (1860–2021)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold")
    )

  print(p)

  ggplot2::ggsave(
    "C:/PhD/Typologies/Typologies_spain/typology_plot/intensification_specialization_secondary.jpeg",
    plot = p,
    width = 10,
    height = 7,
    dpi = 300
  )

  return(list(plot = p, data = df))
}

spatial_diversity <- function() {
  flows <- create_n_prov_destiny()

  items <- readxl::read_excel(
    "C:/PhD/GRAFS/Inputs_SACO/Codes_coefs.xlsx",
    sheet = "Names_biomass_CB"
  )

  df <- flows |>
    dplyr::left_join(items, by = c("Item" = "Name_biomass")) |>
    dplyr::filter(
      Destiny %in%
        c(
          "population_food",
          "population_other_uses",
          "livestock_rum",
          "livestock_mono",
          "export"
        ),
      Origin != "Outside",
      !is.na(Cat_1)
    )

  df_sum <- df |>
    dplyr::group_by(Year, Province_name, Cat_1) |>
    dplyr::summarise(
      value = sum(MgN, na.rm = TRUE),
      .groups = "drop"
    )

  shannon_index <- function(x) {
    p <- x / sum(x)
    p <- p[p > 0]
    -sum(p * log(p))
  }

  diversity <- df_sum |>
    dplyr::group_by(Year) |>
    dplyr::summarise(
      H = shannon_index(value),
      n = dplyr::n(),
      H_norm = ifelse(n > 1, H / log(n), NA_real_),
      .groups = "drop"
    )

  p <- ggplot2::ggplot(
    diversity,
    ggplot2::aes(x = Year, y = H_norm)
  ) +
    ggplot2::geom_line(color = "#1b9e77", size = 1.3) +
    ggplot2::labs(
      x = "Year",
      y = "Spatial diversity (Shannon index)",
      title = "Spatial diversity of agricultural production in Spain"
    ) +
    ggplot2::theme_minimal()

  print(p)

  return(list(plot = p, data = diversity))
}


specialization_hhi_production_plot <- function() {
  # ---- Load data ----
  flows <- create_n_prov_destiny()

  # ---- Select production-related flows ----
  spec_df <- flows |>
    dplyr::filter(
      Destiny %in%
        c(
          "population_food",
          "population_other_uses",
          "livestock_rum",
          "livestock_mono",
          "export"
        ),
      Box %in%
        c(
          "Cropland",
          "Livestock",
          "semi_natural_agroecosystems"
        )
    ) |>
    dplyr::group_by(Year, Province_name, Box) |>
    dplyr::summarise(
      value = sum(MgN, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::filter(value > 0)

  # ---- Calculate shares ----
  spec_df <- spec_df |>
    dplyr::group_by(Year, Province_name) |>
    dplyr::mutate(
      share = value / sum(value, na.rm = TRUE)
    )

  # ---- Calculate HHI ----
  spec_index <- spec_df |>
    dplyr::group_by(Year, Province_name) |>
    dplyr::summarise(
      HHI = sum(share^2, na.rm = TRUE),
      .groups = "drop"
    )

  # ---- National average ----
  spec_ts <- spec_index |>
    dplyr::group_by(Year) |>
    dplyr::summarise(
      HHI = mean(HHI, na.rm = TRUE),
      .groups = "drop"
    )

  # ---- Plot ----
  p <- ggplot2::ggplot(
    spec_ts,
    ggplot2::aes(x = Year, y = HHI)
  ) +
    ggplot2::geom_line(
      color = "#377eb8",
      size = 1.3
    ) +
    ggplot2::labs(
      x = "Year",
      y = "HHI index (Specialization)",
      title = "Specialization of agricultural production systems (cropland, livestock, semi-natural agroecosystems) in Spain (1860–2021)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold")
    )

  print(p)

  return(list(plot = p, data = spec_ts))
}

system_shares_plot <- function() {
  flows <- create_n_prov_destiny()

  df <- flows |>
    dplyr::filter(
      Destiny %in%
        c(
          "population_food",
          "population_other_uses",
          "livestock_rum",
          "livestock_mono",
          "export"
        ),
      Box %in%
        c(
          "Cropland",
          "Livestock",
          "semi_natural_agroecosystems"
        )
    ) |>
    dplyr::group_by(Year, Province_name, Box) |>
    dplyr::summarise(
      value = sum(MgN, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::group_by(Year, Province_name) |>
    dplyr::mutate(
      share = value / sum(value, na.rm = TRUE)
    )

  # ---- National average ----
  df_ts <- df |>
    dplyr::group_by(Year, Box) |>
    dplyr::summarise(
      share = mean(share, na.rm = TRUE),
      .groups = "drop"
    )

  # ---- Plot ----
  p <- ggplot2::ggplot(
    df_ts,
    ggplot2::aes(
      x = Year,
      y = share,
      fill = Box
    )
  ) +
    ggplot2::geom_area() +
    ggplot2::scale_fill_manual(
      values = c(
        "semi_natural_agroecosystems" = "#66a61e",
        "Livestock" = "#b3001b",
        "Cropland" = "#F7DD5A"
      )
    ) +
    ggplot2::labs(
      x = "Year",
      y = "Share of production",
      fill = "System",
      title = "Relative contribution of production in cropland, livestock and semi-natural agroecosystems in Spain (1860–2021)"
    ) +
    ggplot2::theme_minimal()

  print(p)

  return(list(plot = p, data = df_ts))
}
