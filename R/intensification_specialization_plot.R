intens_spec_plot <- function() {
  # ---- Load data ----
  flows <- create_n_prov_destiny()
  npp_ygpit <- whep_read_file("npp_ygpit") |> dplyr::rename_with(tolower)

  # ---- Area per province ----
  area_df <- npp_ygpit |>
    dplyr::group_by(year, province_name) |>
    dplyr::summarise(
      area_ha = sum(area_ygpit_ha, na.rm = TRUE),
      .groups = "drop"
    )

  # ---- New N soil inputs (mg_n) ----
  n_inputs <- flows |>
    dplyr::filter(
      origin %in% c("Synthetic", "Deposition", "Fixation"),
      destiny %in% c("Cropland", "semi_natural_agroecosystems")
    ) |>
    dplyr::group_by(year, province_name) |>
    dplyr::summarise(
      total_input_mg = sum(mg_n, na.rm = TRUE),
      .groups = "drop"
    )

  # ---- Synthetic share ----
  synthetic_share <- flows |>
    dplyr::filter(
      destiny %in% c("Cropland", "semi_natural_agroecosystems"),
      origin %in%
        c("Synthetic", "Livestock", "Fixation", "Deposition", "People")
    ) |>
    dplyr::group_by(year, province_name) |>
    dplyr::summarise(
      synthetic_share = sum(mg_n[origin == "Synthetic"], na.rm = TRUE) /
        sum(mg_n, na.rm = TRUE),
      .groups = "drop"
    )

  # ---- Feed import share ----
  feed_import_share <- flows |>
    dplyr::filter(
      destiny %in% c("livestock_mono", "livestock_rum"),
      origin %in% c("Cropland", "semi_natural_agroecosystems", "Outside")
    ) |>
    dplyr::group_by(year, province_name) |>
    dplyr::summarise(
      feed_import_share = sum(mg_n[origin == "Outside"], na.rm = TRUE) /
        sum(mg_n, na.rm = TRUE),
      .groups = "drop"
    )

  # ---- Combine indicators ----
  df <- n_inputs |>
    dplyr::left_join(area_df, by = c("year", "province_name")) |>
    dplyr::left_join(synthetic_share, by = c("year", "province_name")) |>
    dplyr::left_join(feed_import_share, by = c("year", "province_name")) |>
    dplyr::mutate(
      intensification = (total_input_mg * 1000) / area_ha,
      specialization = pmax(synthetic_share, feed_import_share)
    ) |>
    dplyr::filter(!is.na(intensification), !is.na(specialization))

  # ---- Add typology ----
  df <- df |>
    dplyr::left_join(
      create_typo_ts_plot() |>
        dplyr::select(year, province_name, Typology_base) |>
        dplyr::rename(Typology = Typology_base),
      by = c("year", "province_name")
    ) |>
    dplyr::mutate(
      Typology = gsub(" \\(intensive\\)| \\(extensive\\)", "", Typology)
    ) |>
    dplyr::filter(!is.na(Typology))

  # ---- Define periods ----
  df <- df |>
    dplyr::mutate(
      Period = dplyr::case_when(
        year < 1900 ~ "1860-1900",
        year < 1950 ~ "1900-1950",
        year < 1990 ~ "1950-1990",
        TRUE ~ "1990-2021"
      )
    )

  df$Period <- factor(
    df$Period,
    levels = c("1860-1900", "1900-1950", "1950-1990", "1990-2021")
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
      title = "Provincial intensification and specialization in Spain (1860-2021)"
    ) +
    ggplot2::theme_minimal()

  print(p)

  list(plot = p, data = df)
}


circ_intens_plot <- function() {
  # ---- Load data ----
  flows <- create_n_prov_destiny()
  npp_ygpit <- whep_read_file("npp_ygpit") |> dplyr::rename_with(tolower)

  # ---- Area per province ----
  area_df <- npp_ygpit |>
    dplyr::group_by(year, province_name) |>
    dplyr::summarise(
      area_ha = sum(area_ygpit_ha, na.rm = TRUE),
      .groups = "drop"
    )

  # ---- New N soil inputs ----
  n_inputs <- flows |>
    dplyr::filter(
      origin %in% c("Synthetic", "Deposition", "Fixation"),
      destiny %in% c("Cropland", "semi_natural_agroecosystems")
    ) |>
    dplyr::group_by(year, province_name) |>
    dplyr::summarise(
      total_input_mg = sum(mg_n, na.rm = TRUE),
      .groups = "drop"
    )

  # ---- Manure recycling share ----
  manure_share <- flows |>
    dplyr::filter(
      destiny %in% c("Cropland", "semi_natural_agroecosystems"),
      origin %in%
        c("Livestock", "Synthetic", "Fixation", "Deposition", "People")
    ) |>
    dplyr::group_by(year, province_name) |>
    dplyr::summarise(
      manure_share = sum(mg_n[origin == "Livestock"], na.rm = TRUE) /
        sum(mg_n, na.rm = TRUE),
      .groups = "drop"
    )

  # ---- Urban recycling share ----
  urban_share <- flows |>
    dplyr::filter(
      destiny %in% c("Cropland", "semi_natural_agroecosystems"),
      origin %in%
        c("Livestock", "Synthetic", "Fixation", "Deposition", "People")
    ) |>
    dplyr::group_by(year, province_name) |>
    dplyr::summarise(
      urban_share = sum(mg_n[origin == "People"], na.rm = TRUE) /
        sum(mg_n, na.rm = TRUE),
      .groups = "drop"
    )

  # ---- Local feed share ----
  local_feed_share <- flows |>
    dplyr::filter(
      destiny %in% c("livestock_mono", "livestock_rum"),
      origin %in% c("Cropland", "semi_natural_agroecosystems", "Outside")
    ) |>
    dplyr::group_by(year, province_name) |>
    dplyr::summarise(
      local_feed_share = sum(
        mg_n[origin %in% c("Cropland", "semi_natural_agroecosystems")],
        na.rm = TRUE
      ) /
        sum(mg_n, na.rm = TRUE),
      .groups = "drop"
    )

  # ---- Combine indicators ----
  df <- n_inputs |>
    dplyr::left_join(area_df, by = c("year", "province_name")) |>
    dplyr::left_join(manure_share, by = c("year", "province_name")) |>
    dplyr::left_join(urban_share, by = c("year", "province_name")) |>
    dplyr::left_join(local_feed_share, by = c("year", "province_name")) |>
    dplyr::mutate(
      intensification = (total_input_mg * 1000) / area_ha,
      circularity = (manure_share + urban_share + local_feed_share) / 3
    ) |>
    dplyr::filter(!is.na(intensification), !is.na(circularity))

  # ---- Add typology ----
  df <- df |>
    dplyr::left_join(
      create_typo_ts_plot() |>
        dplyr::select(year, province_name, Typology_base) |>
        dplyr::rename(Typology = Typology_base),
      by = c("year", "province_name")
    ) |>
    dplyr::mutate(
      Typology = gsub(" \\(intensive\\)| \\(extensive\\)", "", Typology)
    ) |>
    dplyr::filter(!is.na(Typology))

  # ---- Define periods ----
  df <- df |>
    dplyr::mutate(
      Period = dplyr::case_when(
        year < 1900 ~ "1860-1900",
        year < 1950 ~ "1900-1950",
        year < 1990 ~ "1950-1990",
        TRUE ~ "1990-2021"
      )
    )

  df$Period <- factor(
    df$Period,
    levels = c("1860-1900", "1900-1950", "1950-1990", "1990-2021")
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
      title = "Circularity and intensification in Spanish agro-food systems (1860-2021)"
    ) +
    ggplot2::theme_minimal()

  print(p)

  list(plot = p, data = df)
}

circ_nue_traj_plot <- function() {
  # ---- Load data ----
  flows <- create_n_prov_destiny()
  npp_ygpit <- whep_read_file("npp_ygpit") |> dplyr::rename_with(tolower)

  # ---- Cropland area ----
  cropland_area <- npp_ygpit |>
    dplyr::filter(
      LandUse %in%
        c(
          "Cropland",
          "semi_natural_agroecosystems"
        )
    ) |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      cropland_area = sum(area_ygpit_ha, na.rm = TRUE),
      .groups = "drop"
    )

  # ---- Crop production ----
  crop_production <- flows |>
    dplyr::filter(
      origin %in%
        c(
          "Cropland",
          "semi_natural_agroecosystems"
        ),
      destiny %in%
        c(
          "population_food",
          "livestock_rum",
          "livestock_mono",
          "export"
        )
    ) |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      crop_N = sum(mg_n, na.rm = TRUE),
      .groups = "drop"
    )

  # ---- Total N inputs ----
  n_inputs <- flows |>
    dplyr::filter(
      origin %in%
        c(
          "Synthetic",
          "Fixation",
          "Deposition",
          "Livestock",
          "People"
        ),
      destiny %in%
        c(
          "Cropland",
          "semi_natural_agroecosystems"
        )
    ) |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      total_inputs = sum(mg_n, na.rm = TRUE),
      .groups = "drop"
    )

  # ---- Manure recycling share ----
  manure_share <- flows |>
    dplyr::filter(
      destiny %in% c("Cropland", "semi_natural_agroecosystems"),
      origin %in%
        c("Livestock", "Synthetic", "Fixation", "Deposition", "People")
    ) |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      manure_share = sum(mg_n[origin == "Livestock"], na.rm = TRUE) /
        sum(mg_n, na.rm = TRUE),
      .groups = "drop"
    )

  # ---- Urban recycling share ----
  urban_share <- flows |>
    dplyr::filter(
      destiny %in% c("Cropland", "semi_natural_agroecosystems"),
      origin %in%
        c("Livestock", "Synthetic", "Fixation", "Deposition", "People")
    ) |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      urban_share = sum(mg_n[origin == "People"], na.rm = TRUE) /
        sum(mg_n, na.rm = TRUE),
      .groups = "drop"
    )

  # ---- Local feed share ----
  local_feed_share <- flows |>
    dplyr::filter(
      destiny %in% c("livestock_mono", "livestock_rum"),
      origin %in% c("Cropland", "semi_natural_agroecosystems", "Outside")
    ) |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      local_feed_share = sum(
        mg_n[origin %in% c("Cropland", "semi_natural_agroecosystems")],
        na.rm = TRUE
      ) /
        sum(mg_n, na.rm = TRUE),
      .groups = "drop"
    )

  # ---- Combine indicators ----
  df <- cropland_area |>
    dplyr::left_join(crop_production, by = "year") |>
    dplyr::left_join(n_inputs, by = "year") |>
    dplyr::left_join(manure_share, by = "year") |>
    dplyr::left_join(urban_share, by = "year") |>
    dplyr::left_join(local_feed_share, by = "year") |>
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
      color = year
    )
  ) +
    ggplot2::geom_point(size = 3) +
    ggplot2::scale_color_viridis_c(option = "plasma") +
    ggplot2::labs(
      x = "NUE (%)",
      y = "Circularity index",
      color = "Year",
      title = "Circularity and NUE in Spain (1860-2021)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold")
    )

  print(p)

  list(plot = p, data = df)
}

circ_nue_crop_ts_plot <- function() {
  # ---- Load data ----
  flows <- create_n_prov_destiny()

  # ---- Crop production ----
  crop_production <- flows |>
    dplyr::filter(
      origin == "Cropland",
      destiny %in%
        c(
          "population_food",
          "livestock_rum",
          "livestock_mono",
          "export"
        )
    ) |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      crop_N = sum(mg_n, na.rm = TRUE),
      .groups = "drop"
    )

  # ---- Total N inputs to cropland ----
  n_inputs <- flows |>
    dplyr::filter(
      origin %in%
        c(
          "Synthetic",
          "Fixation",
          "Deposition",
          "Livestock",
          "People"
        ),
      destiny == "Cropland"
    ) |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      total_inputs = sum(mg_n, na.rm = TRUE),
      .groups = "drop"
    )

  # ---- Manure recycling share ----
  manure_share <- flows |>
    dplyr::filter(
      destiny == "Cropland",
      origin %in%
        c(
          "Livestock",
          "Synthetic",
          "Fixation",
          "Deposition",
          "People"
        )
    ) |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      manure_share = sum(mg_n[origin == "Livestock"], na.rm = TRUE) /
        sum(mg_n, na.rm = TRUE),
      .groups = "drop"
    )

  # ---- Urban recycling share ----
  urban_share <- flows |>
    dplyr::filter(
      destiny == "Cropland",
      origin %in%
        c(
          "Livestock",
          "Synthetic",
          "Fixation",
          "Deposition",
          "People"
        )
    ) |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      urban_share = sum(mg_n[origin == "People"], na.rm = TRUE) /
        sum(mg_n, na.rm = TRUE),
      .groups = "drop"
    )

  # ---- Local feed share ----
  local_feed_share <- flows |>
    dplyr::filter(
      box == "Cropland",
      destiny %in%
        c(
          "livestock_mono",
          "livestock_rum"
        ),
      origin %in%
        c(
          "Cropland",
          "Outside"
        )
    ) |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      local_feed_share = sum(mg_n[origin == "Cropland"], na.rm = TRUE) /
        sum(mg_n, na.rm = TRUE),
      .groups = "drop"
    )

  # ---- Combine indicators ----
  df <- crop_production |>
    dplyr::left_join(n_inputs, by = "year") |>
    dplyr::left_join(manure_share, by = "year") |>
    dplyr::left_join(urban_share, by = "year") |>
    dplyr::left_join(local_feed_share, by = "year") |>
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
      x = year,
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
      x = NULL,
      y = "",
      color = "Indicator",
      title = "Crop N use efficiency and circularity in Spain (1860-2021)"
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::label_percent(scale = 1),
      limits = c(0, 100)
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

  list(plot = p, data = df)
}

intens_spec_ts <- function() {
  # ---- Load data ----
  flows <- create_n_prov_destiny()
  npp_ygpit <- whep_read_file("npp_ygpit") |> dplyr::rename_with(tolower)

  # ---- Area per province ----
  area_df <- npp_ygpit |>
    dplyr::group_by(year, province_name) |>
    dplyr::summarise(
      area_ha = sum(area_ygpit_ha, na.rm = TRUE),
      .groups = "drop"
    )

  # ---- New N soil inputs ----
  n_inputs <- flows |>
    dplyr::filter(
      origin %in% c("Synthetic", "Deposition", "Fixation"),
      destiny %in% c("Cropland", "semi_natural_agroecosystems")
    ) |>
    dplyr::group_by(year, province_name) |>
    dplyr::summarise(
      total_input_mg = sum(mg_n, na.rm = TRUE),
      .groups = "drop"
    )

  # ---- Synthetic fertilizer share ----
  synthetic_share <- flows |>
    dplyr::filter(
      destiny %in% c("Cropland", "semi_natural_agroecosystems"),
      origin %in%
        c("Synthetic", "Livestock", "Fixation", "Deposition", "People")
    ) |>
    dplyr::group_by(year, province_name) |>
    dplyr::summarise(
      synthetic_share = sum(mg_n[origin == "Synthetic"], na.rm = TRUE) /
        sum(mg_n, na.rm = TRUE),
      .groups = "drop"
    )

  # ---- Feed import share ----
  feed_import_share <- flows |>
    dplyr::filter(
      destiny %in% c("livestock_mono", "livestock_rum"),
      origin %in% c("Cropland", "semi_natural_agroecosystems", "Outside")
    ) |>
    dplyr::group_by(year, province_name) |>
    dplyr::summarise(
      feed_import_share = sum(mg_n[origin == "Outside"], na.rm = TRUE) /
        sum(mg_n, na.rm = TRUE),
      .groups = "drop"
    )

  # ---- Combine indicators ----
  df <- n_inputs |>
    dplyr::left_join(area_df, by = c("year", "province_name")) |>
    dplyr::left_join(synthetic_share, by = c("year", "province_name")) |>
    dplyr::left_join(feed_import_share, by = c("year", "province_name")) |>
    dplyr::mutate(
      # Intensification (kg N per ha)
      intensification = (total_input_mg * 1000) / area_ha,

      # Specialization index
      specialization = (synthetic_share + feed_import_share) / 2
    )

  # ---- National yearly averages ----
  df_ts <- df |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      intensification = mean(intensification, na.rm = TRUE),
      specialization = mean(specialization, na.rm = TRUE),
      .groups = "drop"
    )

  # ---- Index (1860 = 100) ----
  base <- df_ts |> dplyr::filter(year == min(year))

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
      x = year,
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
      x = NULL,
      y = "Index (1860 = 100)",
      color = "Indicator",
      title = "Intensification and specialization in Spain (1860-2020)"
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

  list(plot = p, data = df_ts)
}


yield_nue_trajectory_plot <- function() {
  # ---- Load data ----
  flows <- create_n_prov_destiny()
  npp_ygpit <- whep_read_file("npp_ygpit") |> dplyr::rename_with(tolower)

  # ---- Cropland area ----
  cropland_area <- npp_ygpit |>
    dplyr::filter(landuse == "Cropland") |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      cropland_area = sum(area_ygpit_ha, na.rm = TRUE),
      .groups = "drop"
    )

  # ---- Crop production ----
  crop_production <- flows |>
    dplyr::filter(
      origin == "Cropland",
      destiny %in%
        c(
          "population_food",
          "livestock_rum",
          "livestock_mono",
          "export"
        )
    ) |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      crop_N = sum(mg_n, na.rm = TRUE),
      .groups = "drop"
    )

  # ---- Total N inputs ----
  n_inputs <- flows |>
    dplyr::filter(
      origin %in%
        c(
          "Synthetic",
          "Fixation",
          "Deposition",
          "Livestock",
          "People"
        ),
      destiny == "Cropland"
    ) |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      total_inputs = sum(mg_n, na.rm = TRUE),
      .groups = "drop"
    )

  # ---- Combine ----
  df <- cropland_area |>
    dplyr::left_join(crop_production, by = "year") |>
    dplyr::left_join(n_inputs, by = "year") |>
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
      color = year
    )
  ) +
    ggplot2::geom_point(size = 3) +
    ggplot2::scale_color_viridis_c(option = "plasma") +
    ggplot2::labs(
      x = "NUE (%)",
      y = "Crop productivity (kgN/ha)",
      color = "Year",
      title = "Crop productivity (intensification) and crop NUE in Spain (1860-2020)"
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

  list(plot = p, data = df)
}


intens_traj_plot <- function() {
  # ---- Load data ----
  flows <- create_n_prov_destiny()
  npp_ygpit <- whep_read_file("npp_ygpit") |> dplyr::rename_with(tolower)

  # ---- Cropland area ----
  cropland_area <- npp_ygpit |>
    dplyr::filter(landuse == "Cropland") |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      cropland_area = sum(area_ygpit_ha, na.rm = TRUE),
      .groups = "drop"
    )

  # ---- Crop production (N) ----
  crop_production <- flows |>
    dplyr::filter(
      origin == "Cropland",
      destiny %in%
        c(
          "population_food",
          "livestock_rum",
          "livestock_mono",
          "export"
        )
    ) |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      crop_N = sum(mg_n, na.rm = TRUE),
      .groups = "drop"
    )

  # ---- Synthetic fertilizer ----
  fertilizer_input <- flows |>
    dplyr::filter(
      origin == "Synthetic",
      destiny == "Cropland"
    ) |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      fertilizer_N = sum(mg_n, na.rm = TRUE),
      .groups = "drop"
    )

  # ---- Combine ----
  df <- cropland_area |>
    dplyr::rename(year_dup = year) |
    dplyr::left_join(crop_production, by = "year") |>
      dplyr::left_join(fertilizer_input, by = "year") |>
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
      color = year
    )
  ) +
    ggplot2::geom_point(size = 3) +
    ggplot2::scale_color_viridis_c(option = "plasma") +
    ggplot2::labs(
      x = "Nitrogen input intensity (kg N / ha)",
      y = "Land productivity (kg N / ha)",
      color = "Year",
      title = "Agricultural intensification in Spain (1860-2020)"
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

  list(plot = p, data = df)
}

intens_ts_plot <- function() {
  # ---- Load data ----
  flows <- create_n_prov_destiny()
  npp_ygpit <- whep_read_file("npp_ygpit") |> dplyr::rename_with(tolower)

  # ---- Cropland area ----
  cropland_area <- npp_ygpit |>
    dplyr::filter(landuse == "Cropland") |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      cropland_area = sum(area_ygpit_ha, na.rm = TRUE),
      .groups = "drop"
    )

  # ---- Crop production ----
  crop_production <- flows |>
    dplyr::filter(
      origin == "Cropland",
      destiny %in%
        c(
          "population_food",
          "livestock_rum",
          "livestock_mono",
          "export"
        )
    ) |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      crop_N = sum(mg_n, na.rm = TRUE),
      .groups = "drop"
    )

  # ---- Synthetic fertilizer ----
  fertilizer_input <- flows |>
    dplyr::filter(
      origin == "Synthetic",
      destiny == "Cropland"
    ) |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      fertilizer_N = sum(mg_n, na.rm = TRUE),
      .groups = "drop"
    )

  # ---- Combine ----
  df <- cropland_area |>
    dplyr::rename(year_dup = year) |
    dplyr::left_join(crop_production, by = "year") |>
      dplyr::left_join(fertilizer_input, by = "year") |>
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
      x = year,
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
      x = NULL,
      y = "kg N / ha",
      color = "Indicator",
      title = "N inputs and crop productivity in Spain (1860-2020)"
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

  list(plot = p, data = df)
}

nue_fertilizer_timeseries_plot <- function() {
  flows <- create_n_prov_destiny()

  # ---- Crop production ----
  crop_production <- flows |>
    dplyr::filter(
      origin == "Cropland",
      destiny %in%
        c(
          "population_food",
          "livestock_rum",
          "livestock_mono",
          "export"
        )
    ) |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      crop_N = sum(mg_n, na.rm = TRUE),
      .groups = "drop"
    )

  # ---- Soil inputs ----
  inputs <- flows |>
    dplyr::filter(
      origin %in%
        c(
          "Synthetic",
          "Livestock",
          "People",
          "Fixation",
          "Deposition"
        ),
      destiny == "Cropland"
    ) |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      total_inputs = sum(mg_n, na.rm = TRUE),

      synthetic = sum(mg_n[origin == "Synthetic"], na.rm = TRUE),

      organic = sum(
        mg_n[origin %in% c("Livestock", "People")],
        na.rm = TRUE
      ),

      fixation_dep = sum(
        mg_n[origin %in% c("Fixation", "Deposition")],
        na.rm = TRUE
      ),

      .groups = "drop"
    )

  # ---- Combine indicators ----
  df <- crop_production |>
    dplyr::left_join(inputs, by = "year") |>
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
      "fertilizer_dependency",
      "synthetic_share",
      "organic_share"
    )
  )

  # ---- Plot ----
  p <- ggplot2::ggplot(
    df_long,
    ggplot2::aes(x = year, y = value, color = indicator)
  ) +
    ggplot2::geom_line(size = 1.3) +
    ggplot2::scale_color_manual(
      values = c(
        fertilizer_dependency = "#e7298a",
        synthetic_share = "#d95f02",
        organic_share = "#1b9e77"
      ),
      labels = c(
        fertilizer_dependency = "Fertilizer dependency",
        synthetic_share = "Mineral fertilizer share",
        organic_share = "Organic fertilizer share"
      )
    ) +
    ggplot2::labs(
      x = NULL,
      y = "",
      color = "Indicator",
      title = "Fertilizer N dependency in Spanish cropland (1860-2021)"
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::label_percent(scale = 1),
      limits = c(0, 100)
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

  list(plot = p, data = df)
}

nue_fertilizer_trajectory_plot <- function() {
  # ---- Load data ----
  flows <- create_n_prov_destiny()

  # ---- Crop production (N output) ----
  crop_production <- flows |>
    dplyr::filter(
      origin == "Cropland",
      destiny %in%
        c(
          "population_food",
          "livestock_rum",
          "livestock_mono",
          "export"
        )
    ) |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      crop_N = sum(mg_n, na.rm = TRUE),
      .groups = "drop"
    )

  # ---- Soil N inputs ----
  inputs <- flows |>
    dplyr::filter(
      origin %in%
        c(
          "Synthetic",
          "Livestock",
          "People",
          "Fixation",
          "Deposition"
        ),
      destiny == "Cropland"
    ) |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      total_inputs = sum(mg_n, na.rm = TRUE),

      fertilizer = sum(
        mg_n[origin %in% c("Synthetic", "Livestock", "People")],
        na.rm = TRUE
      ),

      .groups = "drop"
    )

  # ---- Combine indicators ----
  df <- crop_production |>
    dplyr::left_join(inputs, by = "year") |>
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
      color = year
    )
  ) +
    ggplot2::geom_point(size = 3) +
    ggplot2::scale_color_viridis_c(option = "plasma") +
    ggplot2::labs(
      x = "N use efficiency",
      y = "Fertilizer dependency",
      color = "Year",
      title = "NUE and fertilizer dependency trajectory in Spanish cropland (1860-2021)"
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

  list(plot = p, data = df)
}


nrr_cropland_timeseries_plot <- function() {
  # ---- Load data ----
  flows <- create_n_prov_destiny()

  # ---- Crop production (N output) ----
  crop_outputs <- flows |>
    dplyr::filter(
      origin == "Cropland",
      destiny %in%
        c(
          "population_food",
          "livestock_rum",
          "livestock_mono",
          "export"
        )
    ) |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      crop_output = sum(mg_n, na.rm = TRUE),
      .groups = "drop"
    )

  # ---- N soil inputs ----
  crop_inputs <- flows |>
    dplyr::filter(
      origin %in%
        c(
          "Synthetic",
          "Livestock",
          "People",
          "Fixation",
          "Deposition"
        ),
      destiny == "Cropland"
    ) |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      crop_input = sum(mg_n, na.rm = TRUE),
      .groups = "drop"
    )

  # ---- Recycled N (manure + urban) ----
  recycled_n <- flows |>
    dplyr::filter(
      origin %in% c("Livestock", "People"),
      destiny == "Cropland"
    ) |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      recycled = sum(mg_n, na.rm = TRUE),
      .groups = "drop"
    )

  # ---- Combine ----
  df <- crop_inputs |>
    dplyr::left_join(crop_outputs, by = "year") |>
    dplyr::left_join(recycled_n, by = "year") |>
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
      x = year,
      y = NRR
    )
  ) +
    ggplot2::geom_line(
      color = "#1b9e77",
      size = 1.3
    ) +
    ggplot2::labs(
      x = NULL,
      y = "",
      title = "N recycling rate (NRR) in Spanish cropland (1860-2021)"
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::label_percent(scale = 1),
      limits = c(0, 100)
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

  list(plot = p, data = df)
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
    by = c("item" = "Name_biomass")
  )

  df <- df |>
    dplyr::filter(
      destiny %in%
        c(
          "population_food",
          "population_other_uses",
          "livestock_rum",
          "livestock_mono",
          "export"
        ),
      origin != "Outside"
    )

  prod_group <- df |>
    dplyr::group_by(year, province_name, item) |>
    dplyr::summarise(
      value = sum(mg_n, na.rm = TRUE),
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
    dplyr::group_by(year, province_name) |>
    dplyr::summarise(
      H = shannon_index(value),
      n_groups = dplyr::n(),
      H_norm = dplyr::if_else(n_groups > 1, H / log(n_groups), NA_real_),
      .groups = "drop"
    )

  diversity_ts <- diversity |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      diversity = mean(H_norm, na.rm = TRUE),
      .groups = "drop"
    )

  # ---- Plot ----
  p <- ggplot2::ggplot(
    diversity_ts,
    ggplot2::aes(x = year, y = diversity)
  ) +
    ggplot2::geom_line(
      color = "#1b9e77",
      size = 1.3
    ) +
    ggplot2::labs(
      x = NULL,
      y = "Shannon index",
      title = "Production diversity (Shannon index) in Spain (1860-2021)"
    ) +
    ggplot2::scale_y_continuous(limits = c(0, 1)) +
    ggplot2::theme_minimal()

  print(p)

  list(plot = p, data = diversity_ts)
}

intens_spec_sec_axis <- function() {
  flows <- create_n_prov_destiny()
  npp_ygpit <- whep_read_file("npp_ygpit") |> dplyr::rename_with(tolower)

  items <- readxl::read_excel(
    "C:/PhD/GRAFS/Inputs_SACO/Codes_coefs.xlsx",
    sheet = "Names_biomass_CB"
  )

  df_prod <- flows |>
    dplyr::filter(
      destiny %in%
        c(
          "population_food",
          "population_other_uses",
          "livestock_rum",
          "livestock_mono",
          "export"
        ),
      origin != "Outside"
    )

  prod_group <- df_prod |>
    dplyr::group_by(year, province_name, item) |>
    dplyr::summarise(
      value = sum(mg_n, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::filter(value > 0)

  shannon_index <- function(x) {
    p <- x / sum(x)
    p <- p[p > 0]
    -sum(p * log(p))
  }

  specialization_ts <- prod_group |>
    dplyr::group_by(year, province_name) |>
    dplyr::summarise(
      H = shannon_index(value),
      n_groups = dplyr::n(),
      H_norm = dplyr::if_else(n_groups > 1, H / log(n_groups), NA_real_),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      specialization = 1 - H_norm
    ) |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      specialization = mean(specialization, na.rm = TRUE),
      .groups = "drop"
    )

  area_df <- npp_ygpit |>
    dplyr::group_by(year, province_name) |>
    dplyr::summarise(
      area_ha = sum(area_ygpit_ha, na.rm = TRUE),
      .groups = "drop"
    )

  intensification_ts <- flows |>
    dplyr::filter(
      origin %in% c("Synthetic", "Deposition", "Fixation"),
      destiny %in% c("Cropland", "semi_natural_agroecosystems")
    ) |>
    dplyr::group_by(year, province_name) |>
    dplyr::summarise(
      total_input_mg = sum(mg_n, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::left_join(area_df, by = c("year", "province_name")) |>
    dplyr::mutate(
      intensification = (total_input_mg * 1000) / area_ha
    ) |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      intensification = mean(intensification, na.rm = TRUE),
      .groups = "drop"
    )

  df <- intensification_ts |>
    dplyr::left_join(specialization_ts, by = "year")

  scale_factor <- max(df$intensification, na.rm = TRUE) /
    max(df$specialization, na.rm = TRUE)

  p <- ggplot2::ggplot(df, ggplot2::aes(x = year)) +
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
      x = NULL,
      color = "Indicator",
      title = "Intensification and production specialization in Spain (1860-2021)"
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

  list(plot = p, data = df)
}

spatial_diversity <- function() {
  flows <- create_n_prov_destiny()

  items <- readxl::read_excel(
    "C:/PhD/GRAFS/Inputs_SACO/Codes_coefs.xlsx",
    sheet = "Names_biomass_CB"
  )

  df <- flows |>
    dplyr::left_join(items, by = c("item" = "Name_biomass")) |>
    dplyr::filter(
      destiny %in%
        c(
          "population_food",
          "population_other_uses",
          "livestock_rum",
          "livestock_mono",
          "export"
        ),
      origin != "Outside",
      !is.na(Cat_1)
    )

  df_sum <- df |>
    dplyr::group_by(year, province_name, Cat_1) |>
    dplyr::summarise(
      value = sum(mg_n, na.rm = TRUE),
      .groups = "drop"
    )

  shannon_index <- function(x) {
    p <- x / sum(x)
    p <- p[p > 0]
    -sum(p * log(p))
  }

  diversity <- df_sum |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      H = shannon_index(value),
      n = dplyr::n(),
      H_norm = ifelse(n > 1, H / log(n), NA_real_),
      .groups = "drop"
    )

  p <- ggplot2::ggplot(
    diversity,
    ggplot2::aes(x = year, y = H_norm)
  ) +
    ggplot2::geom_line(color = "#1b9e77", size = 1.3) +
    ggplot2::labs(
      x = NULL,
      y = "Spatial diversity (Shannon index)",
      title = "Spatial diversity of agricultural production in Spain"
    ) +
    ggplot2::theme_minimal()

  print(p)

  list(plot = p, data = diversity)
}


spec_hhi_prod_plot <- function() {
  # ---- Load data ----
  flows <- create_n_prov_destiny()

  # ---- Select production-related flows ----
  spec_df <- flows |>
    dplyr::filter(
      destiny %in%
        c(
          "population_food",
          "population_other_uses",
          "livestock_rum",
          "livestock_mono",
          "export"
        ),
      box %in%
        c(
          "Cropland",
          "Livestock",
          "semi_natural_agroecosystems"
        )
    ) |>
    dplyr::group_by(year, province_name, box) |>
    dplyr::summarise(
      value = sum(mg_n, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::filter(value > 0)

  # ---- Calculate shares ----
  spec_df <- spec_df |>
    dplyr::group_by(year, province_name) |>
    dplyr::mutate(
      share = value / sum(value, na.rm = TRUE)
    )

  # ---- Calculate HHI ----
  spec_index <- spec_df |>
    dplyr::group_by(year, province_name) |>
    dplyr::summarise(
      HHI = sum(share^2, na.rm = TRUE),
      .groups = "drop"
    )

  # ---- National average ----
  spec_ts <- spec_index |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      HHI = mean(HHI, na.rm = TRUE),
      .groups = "drop"
    )

  # ---- Plot ----
  p <- ggplot2::ggplot(
    spec_ts,
    ggplot2::aes(x = year, y = HHI)
  ) +
    ggplot2::geom_line(
      color = "#377eb8",
      size = 1.3
    ) +
    ggplot2::scale_y_continuous(limits = c(0, 1)) +
    ggplot2::labs(
      x = NULL,
      y = "HHI index",
      title = "HHI index of N production in cropland, livestock, semi-natural agroecosystems in Spain (1860-2021)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold")
    )

  print(p)

  list(plot = p, data = spec_ts)
}

system_shares_plot <- function() {
  flows <- create_n_prov_destiny()

  df <- flows |>
    dplyr::filter(
      destiny %in%
        c(
          "population_food",
          "population_other_uses",
          "livestock_rum",
          "livestock_mono",
          "export"
        ),
      box %in%
        c(
          "Cropland",
          "Livestock",
          "semi_natural_agroecosystems"
        )
    ) |>
    dplyr::group_by(year, province_name, box) |>
    dplyr::summarise(
      value = sum(mg_n, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::group_by(year, province_name) |>
    dplyr::mutate(
      share = value / sum(value, na.rm = TRUE)
    )

  # ---- National average ----
  df_ts <- df |>
    dplyr::group_by(year, box) |>
    dplyr::summarise(
      share = mean(share, na.rm = TRUE),
      .groups = "drop"
    )

  # ---- Plot ----
  p <- ggplot2::ggplot(
    df_ts,
    ggplot2::aes(
      x = year,
      y = share,
      fill = box
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
      x = NULL,
      y = "",
      fill = "System",
      title = "Relative contribution of production in cropland, livestock and semi-natural agroecosystems in Spain (1860-2021)"
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::percent_format(),
      limits = c(0, 1)
    ) +
    ggplot2::theme_minimal()

  print(p)

  list(plot = p, data = df_ts)
}


ext_dep_plot_national <- function() {
  # ---- Load national data ----
  flows <- create_n_nat_destiny()

  inputs <- flows |>
    dplyr::filter(
      # ---- Soil inputs ----
      (destiny %in%
        c("Cropland", "semi_natural_agroecosystems") &
        origin %in%
          c("Synthetic", "Fixation", "Deposition", "Livestock", "People")) |

        # ---- Livestock feed and population food ----
        (destiny %in%
          c(
            "livestock_mono",
            "livestock_rum",
            "population_food",
            "population_other_uses"
          ) &
          origin %in% c("Cropland", "semi_natural_agroecosystems", "Outside"))
    ) |>

    # ---- Classify inputs ----
    dplyr::mutate(
      input_type = dplyr::case_when(
        # External
        origin %in%
          c("Synthetic", "Fixation", "Deposition") &
          destiny %in%
            c("Cropland", "semi_natural_agroecosystems") ~
          "external",

        origin == "Outside" &
          destiny %in%
            c(
              "livestock_mono",
              "livestock_rum",
              "population_food",
              "population_other_uses"
            ) ~
          "external",

        # Internal
        origin %in%
          c("Livestock", "People") &
          destiny %in%
            c("Cropland", "semi_natural_agroecosystems") ~
          "internal",

        origin %in%
          c("Cropland", "semi_natural_agroecosystems") &
          destiny %in%
            c(
              "livestock_mono",
              "livestock_rum",
              "population_food",
              "population_other_uses"
            ) ~
          "internal",

        TRUE ~ NA_character_
      )
    ) |>
    dplyr::filter(!is.na(input_type))

  # ---- Aggregate ----
  df <- inputs |>
    dplyr::group_by(year, input_type) |>
    dplyr::summarise(
      value_MgN = sum(mg_n, na.rm = TRUE),
      .groups = "drop"
    ) |>
    tidyr::pivot_wider(
      names_from = input_type,
      values_from = value_MgN,
      values_fill = 0
    )

  df <- df |>
    dplyr::mutate(
      external_GgN = external / 1000,
      internal_GgN = internal / 1000,
      total_GgN = external_GgN + internal_GgN,
      external_dependency = external_GgN / total_GgN
    )

  # ---- Plot 1: Dependency ----
  p1 <- ggplot2::ggplot(
    df,
    ggplot2::aes(x = year, y = external_dependency)
  ) +
    ggplot2::geom_line(
      linewidth = 1.4,
      color = "#d95f02"
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::percent_format(),
      limits = c(0, 1)
    ) +
    ggplot2::labs(
      x = NULL,
      y = "External N dependency",
      title = "Dependence on external nitrogen inputs in Spain (1860-2021)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold")
    )

  print(p1)

  # ---- Plot 2: Composition ----
  df_long <- df |>
    dplyr::select(year, external_GgN, internal_GgN) |>
    dplyr::mutate(
      total = external_GgN + internal_GgN,
      external_share = external_GgN / total * 100,
      internal_share = internal_GgN / total * 100
    ) |>
    dplyr::filter(is.finite(external_share), is.finite(internal_share)) |>
    dplyr::select(year, external_share, internal_share) |>
    tidyr::pivot_longer(
      cols = c(external_share, internal_share),
      names_to = "type",
      values_to = "value"
    ) |>
    dplyr::mutate(
      type = dplyr::recode(
        type,
        external_share = "External inputs",
        internal_share = "Internal recycling"
      )
    )

  p2 <- ggplot2::ggplot(
    df_long,
    ggplot2::aes(x = year, y = value, fill = type)
  ) +
    ggplot2::geom_area(alpha = 0.9, na.rm = TRUE) +
    ggplot2::scale_fill_manual(
      values = c(
        "External inputs" = "#d95f02",
        "Internal recycling" = "#1b9e77"
      )
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::label_percent(scale = 1),
      limits = c(0, 100)
    ) +
    ggplot2::labs(
      x = NULL,
      y = "Share of N inputs",
      fill = "Input type",
      title = "Composition of nitrogen inputs in Spain"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold")
    )

  print(p2)

  list(
    dependency_plot = p1,
    composition_plot = p2,
    data = df
  )
}
