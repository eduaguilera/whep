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
      Area_ha = dplyr::na_if(Area_ha, 0),
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
        Origin == "Outside" &
          Destiny %in% c("livestock_mono", "livestock_rum")
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
    dplyr::mutate(dplyr::across(
      dplyr::where(is.numeric),
      ~ tidyr::replace_na(., 0)
    ))

  indicators <- indicators |>
    dplyr::mutate(
      Typology_base = dplyr::case_when(
        production_seminatural > production_crops ~
          "Semi-natural agroecosystems",
        production_crops > animal_ingestion &
          synthetic_share > 0.2 &
          crop_productivity >= 15 ~
          "Specialized cropping systems (intensive)",
        production_crops > animal_ingestion &
          synthetic_share <= 0.2 &
          crop_productivity < 15 ~
          "Specialized cropping systems (extensive)",
        Livestock_density > 1.3 &
          imported_feed_share > 0.6 &
          feed_from_seminatural_share < 0.4 ~
          "Specialized livestock systems (intensive)",
        Livestock_density > 1 &
          Livestock_density <= 1.3 &
          imported_feed_share > 0.6 &
          feed_from_seminatural_share < 0.4 ~
          "Specialized livestock systems (extensive)",
        local_feed_share > 0.2 & Manure_share > 0.13 & crop_productivity >= 40 ~
          "Connected crop-livestock systems (intensive)",
        local_feed_share > 0.2 & Manure_share > 0.13 & crop_productivity < 40 ~
          "Connected crop-livestock systems (extensive)",
        local_feed_share < 0.2 & Manure_share < 0.13 & synthetic_share > 0.4 ~
          "Disconnected crop-livestock systems (intensive)",
        TRUE ~ "Disconnected crop-livestock systems (extensive)"
      ),
      Typology = dplyr::case_when(
        pop_consumption > 2 * production_total ~ "Urban systems",
        TRUE ~ Typology_base
      )
    )

  layer_name <- tools::file_path_sans_ext(basename(shapefile_path))
  sf_provinces <- sf::st_read(
    shapefile_path,
    query = paste0("SELECT * FROM ", layer_name, " WHERE iso_a2='ES'"),
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

  typology_levels <- names(typology_colors)

  map_list <- list()

  for (yr in benchmark_years) {
    df_yr <- indicators |>
      dplyr::filter(Year == yr) |>
      dplyr::mutate(
        pattern_type = ifelse(Typology == "Urban systems", "stripe", "none"),
        pattern_fill = "Urban systems"
      )

    typology_map <- sf_provinces |>
      dplyr::inner_join(df_yr, by = c("name_clean" = "Province_name"))

    p <- ggplot2::ggplot(typology_map) +
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
        drop = TRUE
      ) +
      ggpattern::scale_pattern_fill_manual(
        values = c("Urban systems" = typology_colors["Urban systems"]),
        na.value = NA
      ) +
      ggplot2::labs(title = paste("Year", yr)) +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "none")

    map_list[[as.character(yr)]] <- p
  }

  block_width <- 0.01
  block_height <- 0.01
  block_gap <- 0.003

  n <- length(typology_levels)

  y_top <- seq(from = 1, by = -(block_height + block_gap), length.out = n)
  y_bottom <- y_top - block_height

  legend_df <- data.frame(
    Typology = typology_levels,
    Color = unname(typology_colors),
    is_urban = typology_levels == "Urban systems",
    ymin = y_bottom,
    ymax = y_top
  )

  n_stripes <- 18
  stripe_df <- subset(legend_df, is_urban)

  stripe_lines <- do.call(
    rbind,
    lapply(1:nrow(stripe_df), function(i) {
      xs <- seq(0, block_width, length.out = n_stripes)
      data.frame(
        x = xs,
        xend = xs,
        y = rep(stripe_df$ymin[i], n_stripes),
        yend = rep(stripe_df$ymax[i], n_stripes)
      )
    })
  )

  legend_plot <- ggplot2::ggplot(legend_df) +
    ggplot2::geom_text(
      ggplot2::aes(
        x = 0,
        y = max(y_top) + block_height + 0.02,
        label = "Typologies"
      ),
      hjust = 0,
      vjust = 0,
      size = 3.5
    ) +
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = 0,
        xmax = block_width,
        ymin = ymin,
        ymax = ymax,
        fill = ifelse(is_urban, "white", Color)
      ),
      color = "black"
    ) +
    ggplot2::geom_segment(
      data = stripe_lines,
      ggplot2::aes(
        x = x,
        xend = xend,
        y = y,
        yend = yend
      ),
      color = "#6A5ACD",
      linewidth = 0.28
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        x = block_width + 0.003,
        y = (ymin + ymax) / 2,
        label = Typology
      ),
      hjust = 0,
      size = 2.8
    ) +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_x_continuous(
      limits = c(0, 0.17),
      expand = c(0, 0)
    ) +
    ggplot2::scale_y_continuous(
      limits = c(min(y_bottom) - 0.02, max(y_top) + 0.05),
      expand = c(0, 0)
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.margin = ggplot2::margin(2, 2, 2, 2)
    )

  combined <- patchwork::wrap_plots(map_list, ncol = 3)

  final_plot <- (combined | legend_plot) +
    patchwork::plot_annotation(
      title = "Typologies in Spain (1860–2020)"
    )

  grid::grid.newpage()
  print(final_plot)

  return(indicators)
}
