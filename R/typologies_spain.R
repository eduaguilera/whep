#'
#'
create_typologies_spain <- function(
  n_prov_destiny = NULL,
  make_map = TRUE,
  shapefile_path = "C:/PhD/GRAFS/Production Boxes/Final Files/Inputs/ne_10m_admin_1_states_provinces.shp",
  map_year = 2020
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
        sum(
          MgN[
            Origin %in%
              c("Cropland", "semi_natural_agroecosystems", "Outside") &
              Destiny %in% c("livestock_mono", "livestock_rum")
          ]
        ),
      local_feed_share = sum(
        MgN[
          Origin %in%
            c("Cropland", "semi_natural_agroecosystems") &
            Destiny %in% c("livestock_mono", "livestock_rum")
        ],
        na.rm = TRUE
      ) /
        sum(
          MgN[
            Origin %in%
              c("Cropland", "semi_natural_agroecosystems", "Outside") &
              Destiny %in% c("livestock_mono", "livestock_rum")
          ],
          na.rm = TRUE
        ),
      feed_from_seminatural_share = sum(
        MgN[
          Origin == "semi_natural_agroecosystems" &
            Destiny %in% c("livestock_mono", "livestock_rum")
        ],
        na.rm = TRUE
      ) /
        sum(
          MgN[
            Origin %in%
              c("Cropland", "semi_natural_agroecosystems", "Outside") &
              Destiny %in% c("livestock_mono", "livestock_rum")
          ]
        ),
      Manure_share = sum(
        MgN[
          Origin == "Livestock" &
            Destiny %in% c("Cropland", "semi_natural_agroecosystems")
        ],
        na.rm = TRUE
      ) /
        sum(
          MgN[
            Origin %in%
              c("Livestock", "Synthetic", "Fixation", "Deposition") &
              Destiny %in% c("Cropland", "semi_natural_agroecosystems")
          ],
          na.rm = TRUE
        )
    ) |>
    dplyr::mutate(
      livestock_share = production_livestock / production_total,
      dplyr::across(dplyr::where(is.numeric), ~ tidyr::replace_na(., 0)),
      Year = as.numeric(Year)
    )

  indicators <- indicators |>
    dplyr::mutate(
      Typology_base = dplyr::case_when(
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

  indicators <- indicators |>
    dplyr::mutate(
      Typology = dplyr::case_when(
        pop_consumption > 2.5 * production_total ~ "Urban systems",
        TRUE ~ Typology_base
      )
    )

  if (make_map) {
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
    sf_provinces$name_clean[
      sf_provinces$name_clean == "Castellon"
    ] <- "Castello"
    sf_provinces$name_clean[
      sf_provinces$name_clean == "La_Coruna"
    ] <- "A_Coruna"
    sf_provinces$name_clean[sf_provinces$name_clean == "Orense"] <- "Ourense"
    sf_provinces$name_clean[sf_provinces$name_clean == "Gerona"] <- "Girona"

    sf_provinces <- sf_provinces[
      !sf_provinces$name_clean %in% c("Las_Palmas", "Tenerife"),
    ]

    filtered_indicators <- indicators |>
      dplyr::filter(Year == map_year) |>
      dplyr::select(Province_name, Typology, Typology_base) |>
      dplyr::mutate(
        pattern_type = ifelse(Typology == "Urban systems", "stripe", "none"),
        pattern_fill = "Urban systems"
      )

    typologies_map <- sf_provinces |>
      dplyr::inner_join(
        filtered_indicators,
        by = c("name_clean" = "Province_name")
      )

    typology_colors <- c(
      "Semi-natural agroecosystems" = "#66a61e",
      "Specialized cropping systems" = "#FFF8B0",
      "Specialized livestock systems" = "#FF6666",
      "Connected crop-livestock systems" = "#C49A6C",
      "Disconnected crop-livestock systems" = "#FFA500",
      "Urban systems" = "#7570b3"
    )

    typologies_map$Typology_base <- factor(
      typologies_map$Typology_base,
      levels = c(
        "Semi-natural agroecosystems",
        "Specialized cropping systems",
        "Specialized livestock systems",
        "Connected crop-livestock systems",
        "Disconnected crop-livestock systems"
      )
    )

    ggplot2::ggplot(typologies_map) +
      ggpattern::geom_sf_pattern(
        ggplot2::aes(
          fill = Typology_base,
          pattern = pattern_type,
          pattern_fill = pattern_fill
        ),
        color = "black",
        pattern_angle = 45,
        pattern_density = 0.5,
        pattern_spacing = 0.03
      ) +
      ggplot2::scale_fill_manual(
        values = typology_colors[names(typology_colors) != "Urban systems"],
        name = "Typologies",
        drop = TRUE,
        guide = ggplot2::guide_legend(
          order = 1,
          override.aes = list(pattern = "none")
        )
      ) +
      ggpattern::scale_pattern_fill_manual(
        values = c("Urban systems" = typology_colors["Urban systems"]),
        na.value = NA,
        guide = ggplot2::guide_legend(
          title = NULL,
          override.aes = list(
            fill = "white",
            pattern = "stripe"
          ),
          order = 2
        )
      ) +
      ggplot2::labs(title = paste("Typologies in Spain for", map_year)) +
      ggplot2::theme_minimal()
  }
}
