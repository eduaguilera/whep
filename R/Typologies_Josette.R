#' @title Typologies of Josette
#'
#' @description
#' Typologies of provinces in Spain based on nitrogen (N) production
#' data of crops and livestock, considering multiple data inputs and
#' producing classification maps and data frames.
#'
#' @param make_map If TRUE a map of the typologies will be created.
#'
#' @param shapefile_path Path to the shapefile used for mapping provinces.
#'
#' @param map_year The year for which the typology map is created.
#'
#' @return A tibble with the typology classification per year and province.
#'
#' @export
create_typologies_of_josette <- function(
  make_map = TRUE,
  shapefile_path = paste0(
    "C:/PhD/GRAFS/Production Boxes/",
    "Final Files/Inputs/ne_10m_admin_1_states_provinces.shp"
  ),
  map_year = 1980
) {
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
  manure_share_df <- .calculate_manure_share(data$n_input_df)

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

  df_inputs_plots <- .create_bar_plots_inputs(
    typologies_df = typologies_df,
    n_input_df = data$n_input_df,
    imported_feed_share_df = feed_domestic_df
  )

  return(list(
    typologies_df = typologies_df,
    n_input_df = data$n_input_df,
    imported_feed_share_df = intensive_list$imported_feed_share_df,
    df_inputs_plots = df_inputs_plots
  ))
}

#' @title Load input datasets -------------------------------------------------
#' @description Loads all necessary datasets, including shapefiles for
#' Spanish provinces.
#' @param shapefile_path The local path where the input data are located.
#' @param inputs_dir Path to the input data directory.
#' @keywords internal
#' @noRd
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
    n_input_df = readr::read_csv(file.path(inputs_dir, "n_inputs_combined.csv"))
  )
}

#' @title Calculate food consumption and total production----------------------
#' @description Calculated N from food consumption and agricultural
#' production per province and year.
#' @param grafs_prod_destiny_git Data frame containing nitrogen data by
#' destiny and box.
#' @return A list with two tibbles: 'food_consumption' and 'production'.
#' @keywords internal
#' @noRd
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

#' @title Calculate Crop Production and Animal Feed Ingestion------------------
#' @description Calculates total cropland N production and N ingested by animals
#' @param grafs_prod_destiny_git Data frame containing N data by destiny and box
#' @return A list with two tibbles: 'cropland_prod' and 'animal_ingestion'.
#' @keywords internal
#' @noRd
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

#' @title Decision: Livestock density > 1 LU/ha UAA & >33% animal feed
#' from imports
#'
#' @description
#' Calculates livestock unit totals, livestock density per UAA (Utilized
#' Agricultural Area), and the share of imported feed at the province level.
#'
#' @param livestock_df A data frame containing livestock production per year
#' and province.
#' @param codes_coefs_df A data frame with livestock unit (LU) coefficients for
#' different categories.
#' @param npp_df A data frame with net primary production (NPP) and agricultural
#'  area (UAA) data.
#' @param feed_df A data frame containing feed data including import, export,
#' and production.
#' @param destiny_df A data frame from GRAFS showing nitrogen fluxes by
#' destination and box.
#'
#' @return A list containing LU per province and year, Livestock Density,
#' share of imported feed
#' @keywords internal
#' @noRd
.calculate_imported_feed <- function(
  livestock_df, codes_coefs_df, npp_df, feed_df, destiny_df
) {
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

#' @title Decision: >50% animal feed from Semi-natural agroecosystems
#' @description Calculates the share of feed N that comes from semi-natural
#' agroecosystems.
#' @param destiny_df Data frame of N data by destiny and box.
#' @return A tibble with 'Year', 'Province_name', and 'SemiNatural_feed_share'.
#' @keywords internal
#' @noRd
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

#' @title Decision: >25% animal feed from local crop ---------------------------
#' @description Calculate domestic feed supply by province.
#' @param feed_df Data frame of feed data (PIE_FullDestinies_FM).
#' @param lu_df Data frame with livestock unit totals per province and year.
#' @return A tibble with domestic feed share.
#' @keywords internal
#' @noRd
.calculate_feed_domestic_share <- function(feed_df, lu_df) {
  # Filter relevant feed data: Production, Exports and
  # Imports destined for feed use
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

  # Allocate Production and Import to provinces proportional to LU share
  # Calculate local feed share per province: share of feed coming from domestic
  feed_prov <- lu_with_share |>
    dplyr::left_join(feed_summary, by = "Year") |>
    dplyr::mutate(
      Production_prov = LU_share * Production,
      Import_prov = LU_share * Import,
      Export_prov = LU_share * Export,
      Net_feed_import = Import_prov - Export_prov,
      local_feed_share = (Production_prov - Export_prov) /
        ((Production_prov - Export_prov) + Import_prov),
    ) |>
    dplyr::select(
      Year, Province_name, LU_total, LU_share, Production_prov,
      Import_prov, Export_prov, Net_feed_import, local_feed_share
    )

  feed_prov
}


#' @title Decision: >25% cropland N input from manure--------------------------
#' @description Calculates the share of cropland N inputs, coming from manure.
#' @param n_input_df Data frame of N inputs.
#' @return A tibble with 'Year', 'Province_name', and 'Manure_share'.
#' @keywords internal
#' @noRd
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

#' @title Assign Typologies----------------------------------------------------
#' @description Applies classifications to assign each province to one of the
#' typologies.
#' @param df A tibble containing all required indicator variables.
#' @return A tibble with 'Year', 'Province_name', and 'Typology'.
#' @keywords internal
#' @noRd
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

#' @title Create map-----------------------------------------------------------
#' @description Generates a map of typologies for Spanish provinces for a
#' specified year using ggplot2 and sf.
#' @param typologies_df A tibble with province-level typologies.
#' @param shapefile_path Path to the shapefile.
#' @param map_year Year for which the map should be drawn.
#' @return A ggplot2 object displaying the typology map.
#' @keywords internal
#' @noRd
.create_typologies_map_josette <- function(
  typologies_df, shapefile_path, map_year
) {
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
    dplyr::filter(!Province_name %in% c(
      "Las_Palmas", "Tenerife", "Illes_Balears"
    ))

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

#' @title Create stacked bar plots for N inputs --------------------------------
#' @description Stacked bar plots by province and typology, taking N deposition,
#' fixation, synthetic fertilizer, net feed import into account.
#'
#' @param typologies_df A data frame with columns Year, Province_name, Typology
#' @param n_input_df A data frame with N input values by Year and Province_name
#' @return A plot showing stacked bar plots of inputs by typology.
#' @param year The year to filter the data for plotting
#' @keywords internal
#' @noRd

.create_bar_plots_inputs <- function(
  typologies_df,
  n_input_df,
  imported_feed_share_df
) {
  # Define benchmark years
  benchmark_years <- c(1880, 1930, 1980, 2020)

  # Filter datasets to those years
  typologies_df_filtered <- typologies_df |>
    dplyr::filter(Year %in% benchmark_years)

  n_inputs_agg <- n_input_df |>
    dplyr::filter(Year %in% benchmark_years) |>
    dplyr::group_by(Year, Province_name) |>
    dplyr::summarise(
      Deposition = sum(MgN_dep, na.rm = TRUE),
      Fixation = sum(MgN_fix, na.rm = TRUE),
      Synthetic_fert = sum(MgN_syn, na.rm = TRUE),
      .groups = "drop"
    )

  feed_import_agg <- imported_feed_share_df |>
    dplyr::filter(Year %in% benchmark_years) |>
    dplyr::select(Year, Province_name, Net_feed_import)

  # Join all data
  df_joined <- n_inputs_agg |>
    dplyr::left_join(feed_import_agg, by = c("Year", "Province_name")) |>
    dplyr::left_join(typologies_df_filtered, by = c("Year", "Province_name"))

  # Convert to long format
  df_long <- df_joined |>
    tidyr::pivot_longer(
      cols = c("Deposition", "Fixation", "Synthetic_fert", "Net_feed_import"),
      names_to = "N_input_type",
      values_to = "N_input_value"
    )

  # Aggregate by typology and year, convert to GgN
  df_plot <- df_long |>
    dplyr::group_by(Year, Typology, N_input_type) |>
    dplyr::summarise(
      N_input_value = sum(N_input_value, na.rm = TRUE) / 1000,
      .groups = "drop"
    )

  # Create stacked bar plot
  p <- ggplot(df_plot, aes(
    x = factor(Year),
    y = N_input_value,
    fill = N_input_type
  )) +
    geom_bar(stat = "identity") +
    facet_wrap(~Typology, scales = "free_y") +
    labs(
      title = "Nitrogen Inputs by Typology",
      x = "Year",
      y = "Nitrogen Input (GgN)",
      fill = "Input Type"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  print(p)
  df_plot
}
