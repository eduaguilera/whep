#' Typologies of Josette
#'
#' @name Typologies_Josette
NULL

create_typologies_of_josette <- function(
    make_map = TRUE, shapefile_path = "C:/PhD/GRAFS/Production Boxes/
    Final Files/Inputs/ne_10m_admin_1_states_provinces.shp", map_year = 2020) {
  inputs_dir <- "C:/PhD/GRAFS/Production Boxes/Final Files/Inputs"

  # Load datasets
  data <- .load_inputs_josette(inputs_dir, shapefile_path)

  consumption <- .calculate_consumption_prod(data$grafs_prod_destiny_git)
  food_consumption_df <- consumption$food_consumption
  production_df <- consumption$production

  crop_feed <- .calculate_crop_prod_feed(
    data$grafs_prod_destiny_git
  )
  cropland_prod_df <- crop_feed$cropland_prod
  animal_ingestion_df <- crop_feed$animal_ingestion

  intensive_list <- .calculate_imported_feed(
    data$Livestock_Prod_ygps,
    data$Codes_coefs,
    data$NPP_ygpit,
    data$PIE_FullDestinies_FM,
    data$grafs_prod_destiny_git
  )

  seminatural_df <- .calculate_natural_feed_share(
    data$grafs_prod_destiny_git
  )
  feed_domestic_df <- .calculate_feed_domestic_share(
    data$PIE_FullDestinies_FM, intensive_list$lu_totals
  )
  manure_share_df <- .calculate_manure_share(data$N_inputs)

  typologies_df <- food_consumption_df |>
    dplyr::left_join(production_df, by = c("Year", "Province_name")) |>
    dplyr::left_join(cropland_prod_df, by = c("Year", "Province_name")) |>
    dplyr::left_join(animal_ingestion_df, by = c("Year", "Province_name")) |>
    dplyr::left_join(intensive_list$livestock_density_df,
      by = c("Year", "Province_name")
    ) |>
    dplyr::left_join(intensive_list$imported_feed_share_df,
      by = c("Year", "Province_name")
    ) |>
    dplyr::left_join(seminatural_df, by = c("Year", "Province_name")) |>
    dplyr::left_join(feed_domestic_df, by = c("Year", "Province_name")) |>
    dplyr::left_join(manure_share_df, by = c("Year", "Province_name"))


  typologies_df <- .assign_typologies(typologies_df) |>
    dplyr::distinct(Year, Province_name, Typology)

  if (make_map) {
    .create_typologies_map_josette(typologies_df, shapefile_path, map_year)
  }

  typologies_df
}


# Load input datasets ---------------------------------------------------------
.load_inputs_josette <- function(inputs_dir, shapefile_path) {
  layer_name <- tools::file_path_sans_ext(basename(shapefile_path))

  sf_provinces_spain <- sf::st_read(shapefile_path, query = paste0(
    "SELECT * FROM ", layer_name, " WHERE iso_a2 = 'ES'"
  ))

  list(
    Livestock_Prod_ygps = readr::read_csv(file.path(
      inputs_dir, "Livestock_Prod_ygps.csv"
    )),
    Codes_coefs = readxl::read_excel(file.path(inputs_dir, "Codes_coefs.xlsx"),
      sheet = "Liv_LU_coefs"
    ),
    NPP_ygpit = readr::read_csv(file.path(inputs_dir, "NPP_ygpit.csv.gz")),
    grafs_prod_destiny_git = readr::read_csv(file.path(
      inputs_dir, "GRAFS_Prod_Destiny_git.csv"
    )),
    PIE_FullDestinies_FM = readr::read_csv(file.path(
      inputs_dir, "PIE_FullDestinies_FM.csv"
    )),
    sf_provinces_spain = sf_provinces_spain,
    N_inputs = readr::read_csv(file.path(inputs_dir, "N_Inputs_combined.csv"))
  )
}

#' Calculate food consumption and total production
#'
.calculate_consumption_prod <- function(grafs_prod_destiny_git) {
  # Food consumption
  food_consumption <- grafs_prod_destiny_git |>
    dplyr::filter(Destiny == "Food") |>
    dplyr::group_by(Year, Province_name) |>
    dplyr::summarise(Food_Consumption_MgN = sum(
      MgN,
      na.rm = TRUE
    ), .groups = "drop")

  # Agricultural production
  production <- grafs_prod_destiny_git |>
    dplyr::filter(Destiny %in% c(
      "Food", "Feed", "Other_uses",
      "Export", "Import"
    )) |>
    dplyr::group_by(Year, Province_name, Destiny) |>
    dplyr::summarise(MgN = sum(MgN, na.rm = TRUE), .groups = "drop") |>
    dplyr::group_by(Year, Province_name) |>
    dplyr::summarise(
      Production_MgN = sum(MgN[Destiny %in% c(
        "Food", "Feed", "Other_uses", "Export"
      )], na.rm = TRUE) -
        sum(MgN[Destiny == "Import"], na.rm = TRUE),
      .groups = "drop"
    )

  list(
    food_consumption = food_consumption,
    production = production
  )
}

.calculate_crop_prod_feed <- function(grafs_prod_destiny_git) {
  # Cropland  production
  cropland_prod <- grafs_prod_destiny_git |>
    dplyr::filter(Box == "Cropland", Destiny %in% c(
      "Food", "Feed", "Other_uses", "Export", "Import"
    )) |>
    dplyr::group_by(Year, Province_name, Destiny) |>
    dplyr::summarise(MgN = sum(MgN, na.rm = TRUE), .groups = "drop") |>
    tidyr::pivot_wider(
      names_from = Destiny,
      values_from = MgN, values_fill = 0
    ) |>
    dplyr::mutate(
      Cropland_Production_MgN = Food + Feed + `Other_uses` + Export - Import
    ) |>
    dplyr::select(Year, Province_name, Cropland_Production_MgN)

  # Animal ingestion (Feed)
  animal_ingestion <- grafs_prod_destiny_git |>
    dplyr::filter(Destiny == "Feed") |>
    dplyr::group_by(Year, Province_name) |>
    dplyr::summarise(Animal_Ingestion_MgN = sum(
      MgN,
      na.rm = TRUE
    ), .groups = "drop")

  list(
    cropland_prod = cropland_prod,
    animal_ingestion = animal_ingestion
  )
}

#' Decision: Livestock density > 1 LU/ha UAA & >33% animal feed from imports
#'
.calculate_imported_feed <- function(
    livestock_df, codes_coefs_df, npp_df, feed_df, destiny_df) {
  lu_coefs <- .prepare_lu_coefs(codes_coefs_df)
  lu_detailed <- .calculate_lu_totals(livestock_df, lu_coefs)
  lu_totals <- .aggregate_lu_totals(lu_detailed)
  area_uaa <- .aggregate_area_aa(npp_df)
  livestock_density_df <- .calculate_livestock_density(lu_totals, area_uaa)

  feed_import_by_province <- .calculate_feed_import_share(feed_df, lu_totals)

  domestic_feed_by_province <- destiny_df |>
    dplyr::filter(Destiny == "Feed", Box == "Cropland") |>
    dplyr::group_by(Year, Province_name) |>
    dplyr::summarise(
      Domestic_feed_MgN = sum(MgN, na.rm = TRUE),
      .groups = "drop"
    )

  imported_feed_share_df <- .calculate_imported_feed_share(
    feed_import_by_province, domestic_feed_by_province
  )

  list(
    lu_totals = lu_totals,
    livestock_density_df = livestock_density_df,
    imported_feed_share_df = imported_feed_share_df
  )
}

#' Decision: >50% animal feed from Semi-natural agroecosystems
#'
.calculate_natural_feed_share <- function(destiny_df) {
  seminatural_feed <- destiny_df |>
    dplyr::filter(Destiny == "Feed", Box == "Semi_natural_agroecosystems") |>
    dplyr::group_by(Year, Province_name) |>
    dplyr::summarise(SemiNatural_feed_MgN = sum(
      MgN,
      na.rm = TRUE
    ), .groups = "drop")

  total_feed <- destiny_df |>
    dplyr::filter(Destiny == "Feed") |>
    dplyr::group_by(Year, Province_name) |>
    dplyr::summarise(Total_feed_MgN = sum(MgN, na.rm = TRUE), .groups = "drop")

  seminatural_share_df <- seminatural_feed |>
    dplyr::left_join(total_feed, by = c("Year", "Province_name")) |>
    dplyr::mutate(
      SemiNatural_feed_share = SemiNatural_feed_MgN / Total_feed_MgN
    )

  seminatural_share_df
}

#' Decision: >25% animal feed from local crop ---------------------------------
#' Calculate domestic feed supply per province
#'
.calculate_feed_domestic_share <- function(feed_df, lu_df) {
  #' Filter relevant feed data: Production, Exports and
  #' Imports destined for feed use
  feed_summary <- feed_df |>
    dplyr::filter(
      Element %in% c("Production", "Export", "Import"),
      Destiny == "Feed"
    ) |>
    tidyr::pivot_wider(
      names_from = Element,
      values_from = Value_destiny,
      values_fill = 0
    )

  # Calculate total Livestock Units (LU) in Spain per year
  total_lu_spain <- lu_df |>
    dplyr::group_by(Year) |>
    dplyr::summarise(LU_total_spain = sum(
      LU_total,
      na.rm = TRUE
    ), .groups = "drop")

  # Calculate LU share per province
  lu_with_share <- lu_df |>
    dplyr::left_join(total_lu_spain, by = "Year") |>
    dplyr::mutate(LU_share = LU_total / LU_total_spain)

  #' Allocate Production and Import to provinces proportional to LU share
  #' Calculate local feed share per province: share of feed coming from domestic
  feed_prov <- lu_with_share |>
    dplyr::left_join(feed_summary, by = "Year") |>
    dplyr::mutate(
      Production_prov = LU_share * Production,
      Import_prov = LU_share * Import,
      Export_prov = LU_share * Export,
      local_feed_share = (Production_prov - Export_prov) /
        ((Production_prov - Export_prov) + Import_prov),
    ) |>
    dplyr::select(
      Year, Province_name, LU_total, LU_share,
      Production_prov, Import_prov, Export_prov, local_feed_share
    )

  feed_prov
}



#' Decision: >25% cropland N input from manure
#'
.calculate_manure_share <- function(n_input_df) {
  cropland_n_inputs <- n_input_df |>
    dplyr::filter(Box == "Cropland") |>
    dplyr::group_by(Year, Province_name) |>
    dplyr::summarise(
      MgN_manure = sum(MgN_manure, na.rm = TRUE),
      MgN_total = sum(
        MgN_dep + MgN_fix + MgN_syn + MgN_manure + MgN_urban,
        na.rm = TRUE
      ),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      Manure_share = MgN_manure / MgN_total
    )

  cropland_n_inputs
}

#' Assign Typologies
#'
.assign_typologies <- function(df) {
  df |>
    dplyr::mutate(
      Typology = dplyr::case_when(
        Food_Consumption_MgN > Production_MgN ~ "Urban system",
        Cropland_Production_MgN > 1.5 * Animal_Ingestion_MgN ~
          "Specialized stockless cropping system",
        Livestock_density > 1 &
          Imported_feed_share > 0.33 ~ "Specialized livestock system",
        SemiNatural_feed_share > 0.5 ~ "Grass-based crop & livestock system",
        local_feed_share > 0.05 &
          Manure_share > 0.05 ~ "Forage-based crop & livestock system",
        TRUE ~ "Disconnected crop & livestock system"
      )
    ) |>
    dplyr::select(Year, Province_name, Typology)
}

#' Create map
#'
.create_typologies_map_josette <- function(
    typologies_df, shapefile_path, map_year) {
  layer_name <- tools::file_path_sans_ext(basename(shapefile_path))

  sf_provinces <- sf::st_read(shapefile_path, query = paste0(
    "SELECT * FROM ", layer_name, " WHERE iso_a2 = 'ES'"
  ))

  sf_provinces <- sf_provinces |>
    mutate(
      name = stringi::stri_trans_general(name, "Latin-ASCII"),
      name = gsub(" ", "_", name),
      name = dplyr::case_when(
        name == "La_Rioja" ~ "Rioja",
        name == "Alava" ~ "Araba",
        name == "Lerida" ~ "Lleida",
        name == "Castellon" ~ "Castello",
        name == "La_Coruna" ~ "A_Coruna",
        name == "Orense" ~ "Ourense",
        name == "Gerona" ~ "Girona",
        TRUE ~ name
      )
    ) |>
    dplyr::filter(!name %in% c("Las_Palmas", "Tenerife", "Illes_Balears"))

  typologies_year <- typologies_df |>
    dplyr::filter(Year == map_year) |>
    dplyr::filter(!Province_name %in% c("Las_Palmas", "Tenerife", "Illes_Balears"))

  sf_provinces_filtered <- sf_provinces |>
    dplyr::filter(name %in% typologies_year$Province_name)

  map_data <- sf_provinces_filtered |>
    dplyr::rename(Province_name = name) |>
    dplyr::inner_join(typologies_year, by = "Province_name")

  map_typologies_josette <- ggplot(map_data) +
    geom_sf(aes(fill = Typology)) +
    scale_fill_manual(values = c(
      "Urban system" = "#FF6666",
      "Specialized stockless cropping system" = "#FFEB00",
      "Grass-based crop & livestock system" = "#66a61e",
      "Forage-based crop & livestock system" = "#FFA500",
      "Disconnected crop & livestock system" = "#FFFF99",
      "Specialized livestock system" = "#FF0000"
    )) +
    labs(title = paste("Typologies in Spain for", map_year)) +
    theme_minimal()

  print(map_typologies_josette)
}
