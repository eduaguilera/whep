library(dplyr)
library(tidyr)
library(ggplot2)
library(sf)
library(stringi)

create_typologies_spain <- function(
  n_prov_destiny = NULL,
  make_map = TRUE,
  shapefile_path = "C:/PhD/GRAFS/Production Boxes/Final Files/Inputs/ne_10m_admin_1_states_provinces.shp",
  map_year = 1920
) {
  if (is.null(n_prov_destiny)) {
    n_prov_destiny <- create_n_prov_destiny()
  }

  n_agg <- n_prov_destiny |>
    group_by(Year, Province_name, Origin, Destiny, Box) |>
    summarise(MgN = sum(MgN, na.rm = TRUE), .groups = "drop")

  indicators <- n_agg |>
    group_by(Year, Province_name) |>
    summarise(
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
          MgN[Destiny %in% c("livestock_mono", "livestock_rum")],
          na.rm = TRUE
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
    mutate(across(where(is.numeric), ~ replace_na(., 0)))

  indicators <- indicators |>
    mutate(
      Typology = case_when(
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
    layer_name <- tools::file_path_sans_ext(basename(shapefile_path))
    sf_provinces <- sf::st_read(
      shapefile_path,
      query = paste0("SELECT * FROM ", layer_name, " WHERE iso_a2 = 'ES'"),
      quiet = TRUE
    )

    province_col <- intersect(
      c("NAME_1", "name", "NAME", "province"),
      colnames(sf_provinces)
    )[1]

    sf_provinces <- sf_provinces |>
      mutate(
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

    typologies_map <- sf_provinces |>
      inner_join(
        indicators |>
          filter(Year == map_year) |>
          select(Province_name, Typology),
        by = c("name_clean" = "Province_name")
      )

    # Plot
    ggplot(typologies_map) +
      geom_sf(aes(fill = Typology), color = "black") +
      scale_fill_manual(
        values = c(
          "Urban systems" = "#7570b3",
          "Semi-natural agroecosystems" = "#66a61e",
          "Specialized cropping systems" = "#FFD700",
          "Connected crop-livestock systems" = "#FFFF99",
          "Disconnected crop-livestock systems" = "#FF6666"
        )
      ) +
      labs(title = paste("Typologies in Spain for", map_year)) +
      theme_minimal()
  }
}
