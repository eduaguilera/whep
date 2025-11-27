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

  livestock_prod_ygps <- whep_read_file("livestock_prod_ygps")
  npp_ygpit <- whep_read_file("npp_ygpit")

  lu_mapping <- tibble::tribble(
    ~Animal_class,   ~LU_head,
    "Dairy_cows",     1,
    "Cattle",         0.8,
    "Sheep_goats",    0.1,
    "Equines",        0.8,
    "Pigs",           0.3,
    "Hogs",           0.5,
    "Broilers",       0.007,
    "Hens",           0.014,
    "Other_birds",    0.03,
    "Turkeys",        0.03,
    "Ducks",          0.01,
    "Geese",          0.02,
    "Ostriches",      0.35,
    "Small_birds",    0.001,
    "Rabbits",        0.02,
    "Bees",           0.01
  )

  livestockcat_to_class <- tibble::tribble(
    ~Livestock_cat,    ~Animal_class,
    "Cattle_milk",     "Dairy_cows",
    "Cattle_meat",     "Cattle",
    "Sheep",           "Sheep_goats",
    "Goats",           "Sheep_goats",
    "Donkeys_mules",   "Equines",
    "Horses",          "Equines",
    "Pigs",            "Pigs",
    "Hogs",            "Hogs",
    "Poultry",         "Hens",
    "Rabbits",         "Rabbits",
    "Bees",            "Bees"
  )

  lu_df <- livestock_prod_ygps |>
    dplyr::select(Year, Province_name, Livestock_cat, Stock_Number) |>
    dplyr::left_join(livestockcat_to_class, by = "Livestock_cat") |>
    dplyr::left_join(lu_mapping, by = "Animal_class") |>
    dplyr::mutate(
      LU_head = tidyr::replace_na(LU_head, 0),
      Stock_Number = tidyr::replace_na(Stock_Number, 0),
      LU_total_row = Stock_Number * LU_head
    ) |>
    dplyr::group_by(Year, Province_name) |>
    dplyr::summarise(
      LU_total = sum(LU_total_row, na.rm = TRUE),
      .groups = "drop"
    )

  area_df <- npp_ygpit |>
    dplyr::group_by(Year, Province_name) |>
    dplyr::summarise(
      Area_ha = sum(Area_ygpit_ha, na.rm = TRUE),
      .groups = "drop"
    )

  livestock_density_df <- lu_df |>
    dplyr::left_join(area_df, by = c("Year", "Province_name")) |>
    dplyr::mutate(
      # avoid division by zero
      Area_ha = ifelse(Area_ha == 0 | is.na(Area_ha), NA_real_, Area_ha),
      Livestock_density = LU_total / Area_ha
    ) |>
    dplyr::select(Year, Province_name, LU_total, Area_ha, Livestock_density)

  productivity_df <- npp_ygpit |>
    dplyr::filter(LandUse == "Cropland") |>
    dplyr::group_by(Year, Province_name) |>
    dplyr::summarise(
      Prod_MgN_total = sum(Prod_MgN, na.rm = TRUE),
      Area_ha_crops = sum(Area_ygpit_ha, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      crop_productivity = Prod_MgN_total / Area_ha_crops
    )

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
        ),
      synthetic_share = sum(
        MgN[Origin == "Synthetic" & Destiny == "Cropland"],
        na.rm = TRUE
      ) /
        sum(
          MgN[
            Origin %in%
              c("Synthetic", "Livestock", "Fixation", "Deposition", "People") &
              Destiny == "Cropland"
          ],
          na.rm = TRUE
        )
    )

  indicators <- indicators |>
    dplyr::left_join(livestock_density_df, by = c("Year", "Province_name")) |>
    dplyr::left_join(productivity_df, by = c("Year", "Province_name")) |>
    dplyr::mutate(
      Livestock_density = tidyr::replace_na(Livestock_density, 0),
      synthetic_share = tidyr::replace_na(synthetic_share, 0),
      crop_productivity = tidyr::replace_na(crop_productivity, 0),
      dplyr::across(dplyr::where(is.numeric), ~ tidyr::replace_na(., 0)),
      Year = as.numeric(Year)
    )

  indicators <- indicators |>
    dplyr::mutate(
      Typology_base = dplyr::case_when(
        production_seminatural > production_crops ~
          "Semi-natural agroecosystems",
        production_crops > 1.5 * animal_ingestion &
          synthetic_share > 0.4 &
          crop_productivity >= 60 ~
          "Specialized cropping systems (intensive)",
        production_crops > 1.5 * animal_ingestion &
          synthetic_share <= 0.4 &
          crop_productivity < 60 ~
          "Specialized cropping systems (extensive)",
        Livestock_density > 0.8 &
          imported_feed_share > 0.6 &
          feed_from_seminatural_share < 0.3 ~
          "Specialized livestock systems (intensive)",
        Livestock_density > 0.5 &
          Livestock_density <= 0.8 &
          imported_feed_share > 0.6 &
          feed_from_seminatural_share < 0.3 ~
          "Specialized livestock systems (extensive)",
        local_feed_share > 0.4 &
          Manure_share > 0.15 &
          crop_productivity >= 60 ~
          "Connected crop-livestock systems (intensive)",
        local_feed_share > 0.4 &
          Manure_share > 0.15 &
          crop_productivity < 60 ~
          "Connected crop-livestock systems (extensive)",
        local_feed_share < 0.4 &
          Manure_share < 0.15 &
          synthetic_share > 0.4 ~
          "Disconnected crop-livestock systems (intensive)",
        TRUE ~ "Disconnected crop-livestock systems (extensive)"
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
      "Specialized cropping systems (intensive)" = "#fddc5c",
      "Specialized cropping systems (extensive)" = "#FFF8B0",
      "Specialized livestock systems (intensive)" = "#b3001b",
      "Specialized livestock systems (extensive)" = "#ea6a8e",
      "Connected crop-livestock systems (intensive)" = "#C49A6C",
      "Connected crop-livestock systems (extensive)" = "#D9C2A3",
      "Disconnected crop-livestock systems (intensive)" = "#FFA500",
      "Disconnected crop-livestock systems (extensive)" = "#FFD27F",
      "Urban systems" = "#7570b3"
    )

    typologies_map$Typology_base <- factor(
      typologies_map$Typology_base,
      levels = c(
        "Semi-natural agroecosystems",
        "Specialized cropping systems (intensive)",
        "Specialized cropping systems (extensive)",
        "Specialized livestock systems (intensive)",
        "Specialized livestock systems (extensive)",
        "Connected crop-livestock systems (intensive)",
        "Connected crop-livestock systems (extensive)",
        "Disconnected crop-livestock systems (intensive)",
        "Disconnected crop-livestock systems (extensive)"
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
