province_landuse_n_inputs_plot <- function(year_plot = 1980) {
  n_flows <- create_n_prov_destiny()
  area <- whep_read_file("npp_ygpit") |> dplyr::rename_with(tolower)
  indicators <- create_typologies_spain(make_map = FALSE)

  inputs <- n_flows |>
    dplyr::filter(
      year == year_plot,
      origin %in%
        c(
          "Deposition",
          "Fixation",
          "Synthetic",
          "Livestock",
          "People"
        ),
      destiny %in%
        c(
          "Cropland",
          "semi_natural_agroecosystems"
        )
    ) |>
    dplyr::mutate(
      Input = dplyr::case_when(
        origin == "Synthetic" ~ "Synthetic_fertilizer",
        origin == "Livestock" ~ "Manure",
        origin == "People" ~ "Urban",
        TRUE ~ origin
      ),
      LandUse = dplyr::if_else(
        destiny == "Cropland",
        "Cropland",
        "Semi_natural"
      )
    ) |>
    dplyr::group_by(
      province_name,
      LandUse,
      Input
    ) |>
    dplyr::summarise(
      MgN = sum(mg_n, na.rm = TRUE),
      .groups = "drop"
    )

  areas <- area |>
    dplyr::filter(year == year_plot) |>
    dplyr::mutate(
      LandUse = dplyr::if_else(
        landuse == "Cropland",
        "Cropland",
        "Semi_natural"
      )
    ) |>
    dplyr::group_by(province_name, LandUse) |>
    dplyr::summarise(
      Area_ha = sum(area_ygpit_ha, na.rm = TRUE),
      .groups = "drop"
    )

  df <- inputs |>
    dplyr::left_join(areas, by = c("province_name", "LandUse"))

  df_grouped <- df |>
    dplyr::group_by(province_name, LandUse, Input) |>
    dplyr::summarise(
      MgN = sum(MgN),
      Area_ha = sum(Area_ha),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      kgN_ha = (MgN * 1000) / Area_ha
    )

  df_total <- df |>
    dplyr::group_by(province_name, Input) |>
    dplyr::summarise(
      MgN = sum(MgN),
      .groups = "drop"
    ) |>
    dplyr::left_join(
      areas |>
        dplyr::group_by(province_name) |>
        dplyr::summarise(
          Area_ha = sum(Area_ha),
          .groups = "drop"
        ),
      by = "province_name"
    ) |>
    dplyr::mutate(
      LandUse = "Total",
      kgN_ha = (MgN * 1000) / Area_ha
    )

  plot_data <- dplyr::bind_rows(df_grouped, df_total)

  typologies <- indicators |>
    dplyr::filter(year == year_plot) |>
    dplyr::select(province_name, Typology) |>
    dplyr::mutate(
      Typology = stringr::str_remove(
        Typology,
        " \\(intensive\\)| \\(extensive\\)"
      )
    )

  plot_data <- plot_data |>
    dplyr::left_join(typologies, by = "province_name")

  plot_data$province_name <- factor(
    plot_data$province_name,
    levels = sort(unique(plot_data$province_name))
  )

  plot_data$LandUse <- factor(
    plot_data$LandUse,
    levels = c("Cropland", "Semi_natural", "Total")
  )

  input_colors <- c(
    Synthetic_fertilizer = "red4",
    Manure = "darkorange3",
    Urban = "darkorange4",
    Fixation = "olivedrab4",
    Deposition = "gray40"
  )

  typology_list <- unique(plot_data$Typology)

  plots <- lapply(typology_list, function(t) {
    ggplot2::ggplot(
      plot_data |>
        dplyr::filter(Typology == t),
      ggplot2::aes(
        x = LandUse,
        y = kgN_ha,
        fill = Input
      )
    ) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::facet_wrap(~Province_name, nrow = 1) +
      ggplot2::scale_fill_manual(values = input_colors) +
      ggplot2::labs(
        title = paste(
          "N inputs per hectare by province -",
          t,
          "(",
          year_plot,
          ")"
        ),
        x = "Land_use",
        y = "kg N ha^-1"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        strip.text = ggplot2::element_text(
          size = 10,
          face = "bold"
        ),
        panel.spacing = grid::unit(1.2, "lines"),
        axis.text.x = ggplot2::element_text(
          angle = 30,
          hjust = 1
        )
      )
  })

  names(plots) <- typology_list

  return(plots)
}


province_all_landuse_n_inputs_plot <- function(year_plot = 1980) {
  n_balance <- whep_read_file("n_balance_ygpit_all") |>
    dplyr::rename_with(tolower)
  area <- whep_read_file("npp_ygpit") |> dplyr::rename_with(tolower)
  indicators <- create_typologies_spain(make_map = FALSE)

  inputs <- n_balance |>
    dplyr::filter(year == year_plot) |>
    dplyr::mutate(
      LandUse = dplyr::case_when(
        landuse %in% c("Forest_high", "Forest_low") ~ "Forest",
        TRUE ~ landuse
      )
    ) |>
    dplyr::group_by(province_name, LandUse) |>
    dplyr::summarise(
      Deposition = sum(deposition, na.rm = TRUE),
      Fixation = sum(bnf, na.rm = TRUE),
      Synthetic_fertilizer = sum(synthetic, na.rm = TRUE),
      Manure = sum(solid + liquid, na.rm = TRUE),
      Urban = sum(urban, na.rm = TRUE),
      .groups = "drop"
    ) |>
    tidyr::pivot_longer(
      cols = c(
        Deposition,
        Fixation,
        Synthetic_fertilizer,
        Manure,
        Urban
      ),
      names_to = "Input",
      values_to = "MgN"
    )

  areas <- area |>
    dplyr::filter(year == year_plot) |>
    dplyr::mutate(
      LandUse = dplyr::case_when(
        landuse %in% c("Forest_high", "Forest_low") ~ "Forest",
        TRUE ~ landuse
      )
    ) |>
    dplyr::group_by(province_name, LandUse) |>
    dplyr::summarise(
      Area_ha = sum(area_ygpit_ha, na.rm = TRUE),
      .groups = "drop"
    )

  df <- inputs |>
    dplyr::left_join(
      areas,
      by = c("province_name", "LandUse")
    )

  df_grouped <- df |>
    dplyr::group_by(
      Province_name,
      LandUse,
      Input
    ) |>
    dplyr::summarise(
      MgN = sum(MgN, na.rm = TRUE),
      Area_ha = sum(Area_ha, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      kgN_ha = (MgN * 1000) / Area_ha
    )

  df_total <- df |>
    dplyr::group_by(
      Province_name,
      Input
    ) |>
    dplyr::summarise(
      MgN = sum(MgN, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::left_join(
      areas |>
        dplyr::group_by(province_name) |>
        dplyr::summarise(
          Area_ha = sum(Area_ha, na.rm = TRUE),
          .groups = "drop"
        ),
      by = "province_name"
    ) |>
    dplyr::mutate(
      LandUse = "Total",
      kgN_ha = (MgN * 1000) / Area_ha
    )

  plot_data <- dplyr::bind_rows(
    df_grouped,
    df_total
  )

  typologies <- indicators |>
    dplyr::filter(year == year_plot) |>
    dplyr::select(province_name, Typology) |>
    dplyr::mutate(
      Typology = stringr::str_remove(
        Typology,
        " \\(intensive\\)| \\(extensive\\)"
      )
    )

  plot_data <- plot_data |>
    dplyr::left_join(
      typologies,
      by = "province_name"
    )

  plot_data$province_name <- factor(
    plot_data$province_name,
    levels = sort(unique(plot_data$province_name))
  )

  plot_data$LandUse <- factor(
    plot_data$LandUse,
    levels = c(
      "Cropland",
      "Pasture_Shrubland",
      "Dehesa",
      "Forest",
      "Other",
      "Total"
    )
  )

  input_colors <- c(
    Synthetic_fertilizer = "red4",
    Manure = "darkorange3",
    Urban = "darkorange4",
    Fixation = "olivedrab4",
    Deposition = "gray40"
  )

  typology_list <- unique(plot_data$Typology)

  plots <- lapply(typology_list, function(t) {
    ggplot2::ggplot(
      plot_data |>
        dplyr::filter(Typology == t),
      ggplot2::aes(
        x = LandUse,
        y = kgN_ha,
        fill = Input
      )
    ) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::facet_wrap(~Province_name, nrow = 1) +
      ggplot2::scale_fill_manual(values = input_colors) +
      ggplot2::labs(
        title = paste(
          "N inputs per hectare by province -",
          t,
          "(",
          year_plot,
          ")"
        ),
        x = "Land use",
        y = "kg N ha^-1"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        strip.text = ggplot2::element_text(size = 10, face = "bold"),
        panel.spacing = grid::unit(1.2, "lines"),
        axis.text.x = ggplot2::element_text(angle = 30, hjust = 1)
      )
  })

  names(plots) <- typology_list

  return(plots)
}
