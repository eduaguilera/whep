create_typologies_plot <- function(
  n_prov_destiny = NULL,
  shapefile_path = "C:/PhD/GRAFS/Production Boxes/Final Files/Inputs/ne_10m_admin_1_states_provinces.shp",
  benchmark_years = seq(1860, 2020, by = 10)
) {
  if (is.null(n_prov_destiny)) {
    n_prov_destiny <- create_n_prov_destiny()
  }

  n_agg <- n_prov_destiny |>
    dplyr::group_by(Year, Province_name, Origin, Destiny, Box) |>
    dplyr::summarise(MgN = sum(MgN, na.rm = TRUE), .groups = "drop")

  indicators <- n_agg |>
    dplyr::group_by(Year, Province_name) |>
    dplyr::summarise(
      production_crops = sum(MgN[Origin == "Cropland"], na.rm = TRUE),
      production_seminatural = sum(
        MgN[Origin == "semi_natural_agroecosystems"],
        na.rm = TRUE
      ),
      production_total = sum(
        MgN[
          Origin %in%
            c("Cropland", "Livestock", "semi_natural_agroecosystems") &
            Destiny %in%
              c(
                "population_food",
                "population_other_uses",
                "livestock_mono",
                "livestock_rum",
                "export"
              )
        ],
        na.rm = TRUE
      ),
      pop_consumption = sum(
        MgN[Destiny == "population_food" & Origin != "Fish"],
        na.rm = TRUE
      ),
      production_livestock = sum(
        MgN[
          Origin == "Livestock" &
            Destiny %in%
              c(
                "export",
                "population_food",
                "population_other_uses",
                "livestock_mono",
                "livestock_rum"
              )
        ],
        na.rm = TRUE
      ),
      animal_ingestion = sum(
        MgN[Destiny %in% c("livestock_mono", "livestock_rum")],
        na.rm = TRUE
      ),
      imported_feed_share = sum(
        MgN[
          Origin == "Outside" &
            Destiny %in% c("livestock_mono", "livestock_rum")
        ],
        na.rm = TRUE
      ) /
        sum(MgN[
          Origin %in%
            c("Cropland", "semi_natural_agroecosystems", "Outside") &
            Destiny %in% c("livestock_mono", "livestock_rum")
        ]),
      local_feed_share = sum(
        MgN[
          Origin %in%
            c("Cropland", "semi_natural_agroecosystems") &
            Destiny %in% c("livestock_mono", "livestock_rum")
        ],
        na.rm = TRUE
      ) /
        sum(MgN[
          Origin %in%
            c("Cropland", "semi_natural_agroecosystems", "Outside") &
            Destiny %in% c("livestock_mono", "livestock_rum")
        ]),
      feed_from_seminatural_share = sum(
        MgN[
          Origin == "semi_natural_agroecosystems" &
            Destiny %in% c("livestock_mono", "livestock_rum")
        ],
        na.rm = TRUE
      ) /
        sum(MgN[
          Origin %in%
            c("Cropland", "semi_natural_agroecosystems", "Outside") &
            Destiny %in% c("livestock_mono", "livestock_rum")
        ]),
      Manure_share = sum(
        MgN[
          Origin == "Livestock" &
            Destiny %in% c("Cropland", "semi_natural_agroecosystems")
        ],
        na.rm = TRUE
      ) /
        sum(MgN[
          Origin %in%
            c("Livestock", "Synthetic", "Fixation", "Deposition") &
            Destiny %in% c("Cropland", "semi_natural_agroecosystems")
        ])
    ) |>
    dplyr::mutate(
      livestock_share = production_livestock / production_total,
      dplyr::across(dplyr::where(is.numeric), ~ tidyr::replace_na(., 0)),
      Year = as.numeric(Year)
    )

  indicators <- indicators |>
    dplyr::mutate(
      Typology = dplyr::case_when(
        pop_consumption > 2.3 * production_total ~ "Urban systems",
        production_seminatural > production_crops ~
          "Semi-natural agroecosystems",
        production_crops > 1.5 * animal_ingestion ~
          "Specialized cropping systems",
        livestock_share > 0.2 &
          imported_feed_share > 0.6 &
          feed_from_seminatural_share < 0.3 ~
          "Specialized livestock systems",
        local_feed_share > 0.4 & Manure_share > 0.15 ~
          "Connected crop-livestock systems",
        TRUE ~ "Disconnected crop-livestock systems"
      )
    )

  typology_levels <- c(
    "Urban systems",
    "Semi-natural agroecosystems",
    "Specialized cropping systems",
    "Specialized livestock systems",
    "Connected crop-livestock systems",
    "Disconnected crop-livestock systems"
  )

  indicators <- indicators |>
    dplyr::mutate(
      Typology = factor(Typology, levels = typology_levels)
    )

  layer_name <- tools::file_path_sans_ext(basename(shapefile_path))
  sf_provinces <- sf::st_read(
    shapefile_path,
    query = paste0(
      "SELECT * FROM ",
      layer_name,
      " WHERE iso_a2 = 'ES'"
    ),
    quiet = TRUE
  )
  province_col <- intersect(
    c("NAME_1", "name", "NAME", "province"),
    colnames(sf_provinces)
  )[1]
  sf_provinces <- sf_provinces |>
    dplyr::mutate(
      name_clean = stringi::stri_trans_general(
        .data[[province_col]],
        "Latin-ASCII"
      ),
      name_clean = gsub(" ", "_", name_clean)
    )
  sf_provinces$name_clean[sf_provinces$name_clean == "La_Rioja"] <- "Rioja"
  sf_provinces$name_clean[sf_provinces$name_clean == "Alava"] <- "Araba"
  sf_provinces$name_clean[sf_provinces$name_clean == "Lerida"] <- "Lleida"
  sf_provinces$name_clean[sf_provinces$name_clean == "Castellon"] <- "Castello"
  sf_provinces$name_clean[sf_provinces$name_clean == "La_Coruna"] <- "A_Coruna"
  sf_provinces$name_clean[sf_provinces$name_clean == "Orense"] <- "Ourense"
  sf_provinces$name_clean[sf_provinces$name_clean == "Gerona"] <- "Girona"
  sf_provinces <- sf_provinces[
    !sf_provinces$name_clean %in% c("Las_Palmas", "Tenerife"),
  ]

  typology_colors <- c(
    "Urban systems" = "#7570b3",
    "Semi-natural agroecosystems" = "#66a61e",
    "Specialized cropping systems" = "#FFF8B0",
    "Specialized livestock systems" = "#FF6666",
    "Connected crop-livestock systems" = "#C49A6C",
    "Disconnected crop-livestock systems" = "#FFA500"
  )

  map_list <- list()
  for (yr in benchmark_years) {
    filtered_indicators <- indicators |>
      dplyr::filter(Year == yr) |>
      dplyr::select(Province_name, Typology)

    typologies_map <- sf_provinces |>
      dplyr::inner_join(
        filtered_indicators,
        by = c("name_clean" = "Province_name")
      )

    p <- ggplot2::ggplot(typologies_map) +
      ggplot2::geom_sf(ggplot2::aes(fill = Typology), color = "black") +
      ggplot2::scale_fill_manual(values = typology_colors, drop = FALSE) +
      ggplot2::labs(title = paste("Year:", yr)) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 8)
      ) +
      ggplot2::coord_sf(datum = NA, lims_method = "geometry_bbox")

    map_list[[as.character(yr)]] <- p
  }

  plot_with_legend <- map_list[[length(map_list)]]

  map_list_no_legend <- lapply(
    map_list,
    function(p) p + ggplot2::guides(fill = "none")
  )

  map_list_no_legend[[length(map_list_no_legend)]] <- plot_with_legend

  combined_plot <- patchwork::wrap_plots(
    map_list_no_legend,
    ncol = 3,
    guides = "collect"
  ) +
    patchwork::plot_annotation(
      title = "Typologies in Spain 1860 - 2020"
    )

  print(combined_plot)

  return(indicators)
}
