create_typologies_spain <- function(
  n_prov_destiny = NULL,
  make_map = TRUE,
  shapefile_path = "C:/PhD/GRAFS/Production Boxes/Final Files/Inputs/ne_10m_admin_1_states_provinces.shp",
  map_year = 1900
) {
  if (is.null(n_prov_destiny)) {
    n_prov_destiny <- create_n_prov_destiny()
  }

  n_agg <- n_prov_destiny |>
    dplyr::group_by(Year, Province_name, Origin, Destiny, Box) |>
    dplyr::summarise(MgN = base::sum(MgN, na.rm = TRUE), .groups = "drop")

  indicators <- n_agg |>
    dplyr::group_by(Year, Province_name) |>
    dplyr::summarise(
      production_crops = base::sum(MgN[Origin == "Cropland"], na.rm = TRUE),
      production_seminatural = base::sum(
        MgN[Origin == "semi_natural_agroecosystems"],
        na.rm = TRUE
      ),
      production_total = base::sum(
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
      pop_consumption = base::sum(
        MgN[Destiny == "population_food" & Origin != "Fish"],
        na.rm = TRUE
      ),
      animal_ingestion = base::sum(
        MgN[Destiny %in% c("livestock_mono", "livestock_rum")],
        na.rm = TRUE
      ),
      imported_feed_share = base::sum(
        MgN[
          Origin == "Outside" &
            Destiny %in% c("livestock_mono", "livestock_rum")
        ],
        na.rm = TRUE
      ) /
        base::sum(
          MgN[Destiny %in% c("livestock_mono", "livestock_rum")],
          na.rm = TRUE
        ),
      local_feed_share = base::sum(
        MgN[
          Origin %in%
            c("Cropland", "semi_natural_agroecosystems") &
            Destiny %in% c("livestock_mono", "livestock_rum")
        ],
        na.rm = TRUE
      ) /
        base::sum(
          MgN[
            Origin %in%
              c("Cropland", "semi_natural_agroecosystems", "Outside") &
              Destiny %in% c("livestock_mono", "livestock_rum")
          ],
          na.rm = TRUE
        ),
      Manure_share = base::sum(
        MgN[
          Origin == "Livestock" &
            Destiny %in% c("Cropland", "semi_natural_agroecosystems")
        ],
        na.rm = TRUE
      ) /
        base::sum(
          MgN[
            Origin %in%
              c("Livestock", "Synthetic", "Fixation", "Deposition") &
              Destiny %in% c("Cropland", "semi_natural_agroecosystems")
          ],
          na.rm = TRUE
        )
    ) |>
    dplyr::mutate(
      dplyr::across(dplyr::where(is.numeric), ~ tidyr::replace_na(., 0)),
      Year = base::as.numeric(Year)
    )

  indicators <- indicators |>
    dplyr::mutate(
      Typology = dplyr::case_when(
        pop_consumption > 1.5 * production_total ~ "Urban systems",
        production_seminatural > production_crops ~
          "Semi-natural agroecosystems",
        production_crops > 1.5 * animal_ingestion ~
          "Specialized cropping systems",
        local_feed_share > 0.4 & Manure_share > 0.15 ~
          "Connected crop-livestock systems",
        TRUE ~ "Disconnected crop-livestock systems"
      )
    )

  if (make_map) {
    layer_name <- tools::file_path_sans_ext(base::basename(shapefile_path))
    sf_provinces <- sf::st_read(
      shapefile_path,
      query = base::paste0(
        "SELECT * FROM ",
        layer_name,
        " WHERE iso_a2 = 'ES'"
      ),
      quiet = TRUE
    )

    province_col <- base::intersect(
      c("NAME_1", "name", "NAME", "province"),
      base::colnames(sf_provinces)
    )[1]

    sf_provinces <- sf_provinces |>
      dplyr::mutate(
        name_clean = stringi::stri_trans_general(
          .data[[province_col]],
          "Latin-ASCII"
        ),
        name_clean = base::gsub(" ", "_", name_clean)
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
      dplyr::select(Province_name, Typology)

    typologies_map <- sf_provinces |>
      dplyr::inner_join(
        filtered_indicators,
        by = c("name_clean" = "Province_name")
      )

    ggplot2::ggplot(typologies_map) +
      ggplot2::geom_sf(ggplot2::aes(fill = Typology), color = "black") +
      ggplot2::scale_fill_manual(
        values = c(
          "Urban systems" = "#7570b3",
          "Semi-natural agroecosystems" = "#66a61e",
          "Specialized cropping systems" = "#FFD700",
          "Connected crop-livestock systems" = "#FFFF99",
          "Disconnected crop-livestock systems" = "#FF6666"
        )
      ) +
      ggplot2::labs(title = base::paste("Typologies in Spain for", map_year)) +
      ggplot2::theme_minimal()
  }
}
