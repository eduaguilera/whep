create_typologies_timeseries_plot <- function(
  n_prov_destiny = NULL,
  shapefile_path = "C:/PhD/GRAFS/Production Boxes/Final Files/Inputs/ne_10m_admin_1_states_provinces.shp",
  benchmark_years = seq(1860, 2020, by = 20)
) {
  if (is.null(n_prov_destiny)) {
    n_prov_destiny <- create_n_prov_destiny()
  }

  livestock_prod_ygps <- whep_read_file("livestock_prod_ygps")
  npp_ygpit <- whep_read_file("npp_ygpit")

  lu_mapping <- tibble::tribble(
    ~Animal_class,   ~LU_head,
    "Dairy_cows",1, "Cattle",0.8, "Sheep_goats",0.1, "Equines",0.8,
    "Pigs",0.3, "Hogs",0.5, "Broilers",0.007, "Hens",0.014,
    "Other_birds",0.03, "Turkeys",0.03, "Ducks",0.01, "Geese",0.02,
    "Ostriches",0.35, "Small_birds",0.001, "Rabbits",0.02, "Bees",0.01
  )

  livestockcat_to_class <- tibble::tribble(
    ~Livestock_cat, ~Animal_class,
    "Cattle_milk","Dairy_cows",
    "Cattle_meat","Cattle",
    "Sheep","Sheep_goats",
    "Goats","Sheep_goats",
    "Donkeys_mules","Equines",
    "Horses","Equines",
    "Pigs","Pigs",
    "Hogs","Hogs",
    "Poultry","Hens",
    "Rabbits","Rabbits",
    "Bees","Bees"
  )

  lu_df <- livestock_prod_ygps |>
    dplyr::left_join(livestockcat_to_class, by = "Livestock_cat") |>
    dplyr::left_join(lu_mapping, by = "Animal_class") |>
    dplyr::mutate(
      LU_head = tidyr::replace_na(LU_head, 0),
      LU_total_row = Stock_Number * LU_head
    ) |>
    dplyr::group_by(Year, Province_name) |>
    dplyr::summarise(LU_total = sum(LU_total_row), .groups = "drop")

  area_df <- npp_ygpit |>
    dplyr::group_by(Year, Province_name) |>
    dplyr::summarise(Area_ha = sum(Area_ygpit_ha), .groups = "drop")

  livestock_density_df <- lu_df |>
    dplyr::left_join(area_df, by = c("Year", "Province_name")) |>
    dplyr::mutate(
      Area_ha = ifelse(Area_ha == 0, NA_real_, Area_ha),
      Livestock_density = LU_total / Area_ha
    )

  productivity_df <- npp_ygpit |>
    dplyr::filter(LandUse == "Cropland") |>
    dplyr::group_by(Year, Province_name) |>
    dplyr::summarise(
      Prod_MgN_total = sum(Prod_MgN),
      Area_ha_crops = sum(Area_ygpit_ha),
      .groups = "drop"
    ) |>
    dplyr::mutate(crop_productivity = Prod_MgN_total / Area_ha_crops * 1000)

  n_agg <- n_prov_destiny |>
    dplyr::group_by(Year, Province_name, Origin, Destiny, Box) |>
    dplyr::summarise(MgN = sum(MgN), .groups = "drop")

  indicators <- n_agg |>
    dplyr::group_by(Year, Province_name) |>
    dplyr::summarise(
      production_crops = sum(MgN[Origin == "Cropland"]),
      production_seminatural = sum(MgN[
        Origin == "semi_natural_agroecosystems"
      ]),
      animal_ingestion = sum(MgN[
        Destiny %in% c("livestock_mono", "livestock_rum")
      ]),
      pop_consumption = sum(MgN[Destiny == "population_food"]),
      production_total = sum(MgN[
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
      ]),
      imported_feed_share = sum(MgN[
        Origin == "Outside" & Destiny %in% c("livestock_mono", "livestock_rum")
      ]) /
        sum(MgN[
          Origin %in%
            c("Cropland", "semi_natural_agroecosystems", "Outside") &
            Destiny %in% c("livestock_mono", "livestock_rum")
        ]),
      local_feed_share = sum(MgN[
        Origin %in%
          c("Cropland", "semi_natural_agroecosystems") &
          Destiny %in% c("livestock_mono", "livestock_rum")
      ]) /
        sum(MgN[
          Origin %in%
            c("Cropland", "semi_natural_agroecosystems", "Outside") &
            Destiny %in% c("livestock_mono", "livestock_rum")
        ]),
      feed_from_seminatural_share = sum(MgN[
        Origin == "semi_natural_agroecosystems" &
          Destiny %in% c("livestock_mono", "livestock_rum")
      ]) /
        sum(MgN[
          Origin %in%
            c("Cropland", "semi_natural_agroecosystems", "Outside") &
            Destiny %in% c("livestock_mono", "livestock_rum")
        ]),
      Manure_share = sum(MgN[Origin == "Livestock"]) /
        sum(MgN[
          Origin %in% c("Livestock", "Synthetic", "Fixation", "Deposition")
        ]),
      synthetic_share = sum(MgN[Origin == "Synthetic"]) /
        sum(MgN[
          Origin %in% c("Synthetic", "Livestock", "Fixation", "Deposition")
        ]),
      .groups = "drop"
    ) |>
    tidyr::replace_na(list(
      imported_feed_share = 0,
      local_feed_share = 0,
      feed_from_seminatural_share = 0,
      Manure_share = 0,
      synthetic_share = 0
    ))

  indicators <- indicators |>
    dplyr::left_join(livestock_density_df, by = c("Year", "Province_name")) |>
    dplyr::left_join(productivity_df, by = c("Year", "Province_name")) |>
    dplyr::mutate(
      dplyr::across(dplyr::where(is.numeric), ~ tidyr::replace_na(., 0))
    )

  indicators <- indicators |>
    dplyr::mutate(
      Typology_base = dplyr::case_when(
        production_seminatural > production_crops ~
          "Semi-natural agroecosystems",
        production_crops > 1.5 * animal_ingestion &
          synthetic_share > 0.2 &
          crop_productivity >= 30 ~
          "Specialized cropping systems (intensive)",
        production_crops > 1.5 * animal_ingestion &
          synthetic_share <= 0.2 &
          crop_productivity < 30 ~
          "Specialized cropping systems (extensive)",
        Livestock_density > 1 &
          imported_feed_share > 0.6 &
          feed_from_seminatural_share < 0.3 ~
          "Specialized livestock systems (intensive)",
        Livestock_density > 0.8 &
          Livestock_density <= 1 &
          imported_feed_share > 0.6 &
          feed_from_seminatural_share < 0.3 ~
          "Specialized livestock systems (extensive)",
        local_feed_share > 0.4 &
          Manure_share > 0.15 &
          crop_productivity >= 30 ~
          "Connected crop-livestock systems (intensive)",
        local_feed_share > 0.4 &
          Manure_share > 0.15 &
          crop_productivity < 30 ~
          "Connected crop-livestock systems (extensive)",
        local_feed_share < 0.4 &
          Manure_share < 0.15 &
          synthetic_share > 0.2 ~
          "Disconnected crop-livestock systems (intensive)",
        TRUE ~ "Disconnected crop-livestock systems (extensive)"
      ),
      Typology = dplyr::case_when(
        pop_consumption > 2.5 * production_total ~ "Urban systems",
        TRUE ~ Typology_base
      )
    )

  layer_name <- tools::file_path_sans_ext(basename(shapefile_path))
  sf_provinces <- sf::st_read(
    shapefile_path,
    query = paste0(
      "SELECT * FROM ",
      layer_name,
      " WHERE iso_a2='ES'"
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

  # Minor fixes
  sf_provinces$name_clean[sf_provinces$name_clean == "La_Rioja"] <- "Rioja"
  sf_provinces <- sf_provinces[
    !sf_provinces$name_clean %in% c("Las_Palmas", "Tenerife"),
  ]

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

  map_list <- list()

  for (yr in benchmark_years) {
    df_yr <- indicators |>
      dplyr::filter(Year == yr) |>
      dplyr::select(Province_name, Typology)

    typology_map <- sf_provinces |>
      dplyr::inner_join(df_yr, by = c("name_clean" = "Province_name"))

    p <- ggplot2::ggplot(typology_map) +
      ggplot2::geom_sf(ggplot2::aes(fill = Typology), color = "black") +
      ggplot2::scale_fill_manual(values = typology_colors) +
      ggplot2::labs(title = paste("Year:", yr)) +
      ggplot2::theme_minimal() +
      ggplot2::theme(plot.title = ggplot2::element_text(size = 10))

    map_list[[as.character(yr)]] <- p
  }

  combined <- patchwork::wrap_plots(map_list, ncol = 3, guides = "collect") +
    patchwork::plot_annotation(
      title = "Typologies in Spain: 1860–2020"
    )

  print(combined)

  return(indicators)
}
