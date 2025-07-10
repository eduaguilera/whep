#' Typologies of Julia
#'
#' @description
#' Creates typologies of provinces in Spain based on nitrogen (N) production
#' data of crops and livestock, using various input datasets and generating
#' classification maps and data frames.
#'
#' @param make_map If TRUE a map of the typologies will be created.
#'
#' @param shapefile_path Path to the shapefile used for mapping provinces.
#'
#' @param map_year The year for which the typology map is created.
#'
#' @return A tibble with the typology classification for each year and province.
#'
#' @export
create_typologies_grafs_spain <- function(
  make_map = TRUE,
  shapefile_path = paste0(
    "C:/PhD/GRAFS/Production Boxes/",
    "Final Files/Inputs/ne_10m_admin_1_states_provinces.shp"
  ),
  map_year = 1980
) {
  inputs_dir <- "C:/PhD/GRAFS/Production Boxes/Final Files/Inputs"

  # Load datasets
  data <- .load_inputs_typologies_julia(inputs_dir, shapefile_path)

  data$sf_provinces$name <- stri_trans_general(
    data$sf_provinces$name,
    "Latin-ASCII"
  )
  data$Livestock_Prod_ygps$Province_name <- stri_trans_general(
    data$Livestock_Prod_ygps$Province_name, "Latin-ASCII"
  )
  data$sf_provinces$name <- gsub(" ", "_", data$sf_provinces$name)
  data$sf_provinces$name[data$sf_provinces$name == "La_Rioja"] <- "Rioja"
  data$sf_provinces$name[data$sf_provinces$name == "Alava"] <- "Araba"
  data$sf_provinces$name[data$sf_provinces$name == "Lerida"] <- "Lleida"
  data$sf_provinces$name[data$sf_provinces$name == "Castellon"] <- "Castello"
  data$sf_provinces$name[data$sf_provinces$name == "La_Coruna"] <- "A_Coruna"
  data$sf_provinces$name[data$sf_provinces$name == "Orense"] <- "Ourense"
  data$sf_provinces$name[data$sf_provinces$name == "Gerona"] <- "Girona"
  data$sf_provinces <- data$sf_provinces[!data$sf_provinces$name %in% c(
    "Las_Palmas", "Tenerife"
  ), ]

  # Prepare LU coefficients with Livestock_cat mapping
  lu_coefs_mapped <- .prepare_lu_coefs(data$Codes_coefs)

  # Merge livestock data with LU coefficients and calculate totals
  lu_totals_detailed <- .calculate_lu_totals(
    data$Livestock_Prod_ygps,
    lu_coefs_mapped
  )

  # Aggregate LU_total per Year, Province
  lu_aggregated <- .aggregate_lu_totals(lu_totals_detailed)

  # Aggregate Area
  area_aggregated <- .aggregate_area_aa(data$NPP_ygpit)

  # Calculate livestock density
  livestock_density <- .calculate_livestock_density(
    lu_aggregated,
    area_aggregated
  )

  # Calculate cropland productivity
  cropland_productivity <- .aggregate_crop_productivity(data$NPP_ygpit)

  # Aggregate feed from semi natural agroecosystems
  semi_natural_feed <- .aggregate_semi_nat_feed_mgn(data$GRAFS_Prod_Destiny_git)

  cropland_feed <- .aggregate_cropland_feed_mgn(data$GRAFS_Prod_Destiny_git)

  # Calculate feed share (semi_natural feed / total feed)
  feed_share <- .calculate_semi_nat_feed_share(data$GRAFS_Prod_Destiny_git)

  # Use feed supply from GRAFS_Prod_Destiny_git + LU data
  feed_domestic_prov <- .calculate_feed_domest_supply(
    data$GRAFS_Prod_Destiny_git, lu_aggregated
  )

  # Calculate feed import per province based on national imports & LU shares
  feed_import_by_province <- .calculate_feed_import_share(
    data$PIE_FullDestinies_FM, lu_aggregated
  )

  # Calculate imported feed share at province level
  feed_imported_share <- .calculate_imported_feed_share(
    feed_import_by_province,
    feed_domestic_prov
  )


  # Assign Typologies + map
  typologies_result <- .assign_decision_tree(
    livestock_density,
    cropland_productivity,
    feed_share,
    feed_imported_share,
    sf_provinces = data$sf_provinces,
    year = map_year
  )

  if (make_map) {
    typologies_df <- typologies_result$Typologies
    map_plot <- typologies_result$Typologies_map
  } else {
    typologies_df <- typologies_result$Typologies
    map_plot <- NULL
  }

  # Return the relevant map_years
  list(
    LU_detailed = lu_totals_detailed,
    LU_totals = lu_aggregated,
    Livestock_density = livestock_density,
    Cropland_productivity = cropland_productivity,
    Semi_natural_feed = semi_natural_feed,
    Cropland_feed = cropland_feed,
    Feed_share = feed_share,
    Feed_domestic_supply_province = feed_domestic_prov,
    Feed_import_by_province = feed_import_by_province,
    Feed_imported_share = feed_imported_share,
    Typologies = typologies_df,
    Typologies_map = map_plot,
    Typologies_all_years = typologies_result$Typologies_all_years
  )
}

#' Load input datasets ---------------------------------------------------------
#'
#' @param shapefile_path The local path where the input data are located.
#' @param inputs_dir Path to the input data directory.
#'
#' @keywords internal
.load_inputs <- function(inputs_dir, shapefile_path) {
  layer_name <- tools::file_path_sans_ext(basename(shapefile_path))

  sf_provinces_spain <- sf::st_read(shapefile_path, query = paste0(
    "SELECT * FROM ", layer_name, " WHERE iso_a2 = 'ES'"
  ))

  list(
    Livestock_Prod_ygps = readr::read_csv(file.path(
      inputs_dir,
      "Livestock_Prod_ygps.csv"
    )),
    Codes_coefs = readxl::read_excel(file.path(inputs_dir, "Codes_coefs.xlsx"),
      sheet = "Liv_LU_coefs"
    ),
    NPP_ygpit = readr::read_csv(file.path(inputs_dir, "NPP_ygpit.csv.gz")),
    GRAFS_Prod_Destiny_git = readr::read_csv(file.path(
      inputs_dir, "GRAFS_Prod_Destiny_git.csv"
    )),
    PIE_FullDestinies_FM = readr::read_csv(file.path(
      inputs_dir, "PIE_FullDestinies_FM.csv"
    )),
    sf_provinces_spain = sf_provinces_spain
  )
}

#' Mapping: Livestock_cat (from livestock data) to Animal_class
#' (from coefficients)
#' @keywords internal
.create_livestockcat_mapping <- function() {
  tibble::tribble(
    ~Livestock_cat,    ~Animal_class,
    "Cattle_milk",     "Dairy_cows",
    "Cattle_meat",     "Cattle",
    "Sheep",           "Sheep_goats",
    "Goats",           "Sheep_goats",
    "Donkeys_mules",   "Equines",
    "Horses",          "Equines",
    "Pigs",            "Pigs",
    "Poultry",         "Hens",
    "Rabbits",         "Rabbits"
  )
}

#' Prepare LU coefficients with Livestock_cat mapping --------------------------
#'
#' @param codes_coefs_df An excel file including coefficients.
#' @keywords internal
.prepare_lu_coefs <- function(codes_coefs_df) {
  mapping <- .create_livestockcat_mapping()

  mapping |>
    dplyr::inner_join(codes_coefs_df, by = "Animal_class") |>
    dplyr::select(Livestock_cat, Animal_class, LU_head) |>
    dplyr::distinct()
}

#' Calculate LU_total per row --------------------------------------------------
#'
#' @param livestock_df A data frame containing livestock data.
#' @param lu_coefs_df A data frame with livestock unit coefficients.
#'
#' @return A tibble with columns 'Year', 'Province_name', 'Livestock_cat',
#' 'Animal_class', Stock_Number', 'LU_head', and 'LU_total'.
#' @keywords internal
.calculate_lu_totals <- function(livestock_df, lu_coefs_df) {
  livestock_df |>
    dplyr::select(Year, Province_name, Livestock_cat, Stock_Number) |>
    dplyr::left_join(lu_coefs_df, by = "Livestock_cat") |>
    dplyr::mutate(LU_total = Stock_Number * LU_head) |>
    dplyr::select(
      Year, Province_name, Livestock_cat, Animal_class,
      Stock_Number, LU_head, LU_total
    ) |>
    dplyr::distinct()
}

#' Aggregate LU_total ----------------------------------------------------------
#' Aggregates total land use (LU_total) by year and province.
#'
#' @param lu_detailed_df A data frame containing columns `Year`,
#' `Province_name`, and `LU_total`.
#'
#' @return A tibble with total land use summed for each year and province,
#' sorted by year and province.
#' @keywords internal
.aggregate_lu_totals <- function(lu_detailed_df) {
  lu_aggregated <- lu_detailed_df |>
    dplyr::group_by(Year, Province_name) |>
    dplyr::summarise(LU_total = sum(
      LU_total,
      na.rm = TRUE
    ), .groups = "drop") |>
    dplyr::arrange(Year, Province_name)

  lu_aggregated
}

#' Aggregate Area AA -----------------------------------------------------------
#' Aggregates the area (`Area_ygpit_ha`) by year and province.
#'
#' @param npp_df A data frame containing the columns `Year`, `Province_name`,
#' and `Area_ygpit_ha`.
#'
#' @return A tibble with the sum of areas per year and province.
#' @keywords internal
.aggregate_area_aa <- function(npp_df) {
  npp_df |>
    dplyr::group_by(Year, Province_name) |>
    dplyr::summarise(Area_ha = sum(
      Area_ygpit_ha,
      na.rm = TRUE
    ), .groups = "drop")
}

#' Calculate livestock density -------------------------------------------------
#'
#' @param lu_totals_df A data frame containing livestock total data.
#' @param area_df A data frame containing area information.
#'
#' @return A tibble with columns 'Year', 'Province_name', 'LU_total', 'Area_ha',
#' and 'Livestock_density' (LU_total divided by Area_ha).
#' @keywords internal
.calculate_livestock_density <- function(lu_totals_df, area_df) {
  lu_totals_df |>
    dplyr::left_join(area_df, by = c("Year", "Province_name")) |>
    dplyr::mutate(Livestock_density = LU_total / Area_ha) |>
    dplyr::select(Year, Province_name, LU_total, Area_ha, Livestock_density) |>
    dplyr::arrange(Year, Province_name)
}

#' Aggregate Productivity for Cropland -----------------------------------------
#'
#' @param npp_df A data frame containing columns `Year`, `Province_name`,
#' `LandUse`,`Prod_MgN`, and `Area_ygpit_ha`.
#'
#' @return A tibble grouped by year and province with total production,
#' total cropland area, and productivity in kg N per hectare.
#' @keywords internal
.aggregate_crop_productivity <- function(npp_df) {
  cropland_prod <- npp_df |>
    dplyr::filter(LandUse == "Cropland") |>
    dplyr::group_by(Year, Province_name) |>
    dplyr::summarise(
      Prod_MgN_total = sum(Prod_MgN, na.rm = TRUE),
      Area_ha_cropland = sum(Area_ygpit_ha, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      Productivity_kgN_ha = Prod_MgN_total / Area_ha_cropland * 1000
    ) |>
    dplyr::arrange(Year, Province_name)

  cropland_prod
}

#' Aggregate Feed from Semi natural agroecosystems for Grassland > 60% of
#' Livestock intake from Grassland
#'
#' @param df A data frame containing columns `Year`, `Province_name`, `Box`,
#' `Destiny`, and `MgN`.
#'
#' @return A tibble grouped by year and province with the total feed nitrogen
#' (MgN) from semi-natural agroecosystems.
#' @keywords internal
.aggregate_semi_nat_feed_mgn <- function(df) {
  df |>
    dplyr::filter(Box == "Semi_natural_agroecosystems", Destiny == "Feed") |>
    dplyr::group_by(Year, Province_name) |>
    dplyr::summarise(Semi_nat_feed_MgN = sum(
      MgN,
      na.rm = TRUE
    ), .groups = "drop")
}

#' Aggregate Feed from Cropland -----------------------------------------------
#'
#' @param df A data frame containing columns `Year`, `Province_name`,
#' `Box`, `Destiny`, and `MgN`.
#'
#' @return A tibble grouped by year and province with the total cropland feed
#' nitrogen (MgN).
#' @keywords internal
.aggregate_cropland_feed_mgn <- function(df) {
  df |>
    dplyr::filter(Box == "Cropland", Destiny == "Feed") |>
    dplyr::group_by(Year, Province_name) |>
    dplyr::summarise(Cropland_feed_MgN = sum(
      MgN,
      na.rm = TRUE
    ), .groups = "drop")
}

#' Aggregate total feed from all boxes (Feed destiny) -------------------------
#'
#' @param df A data frame containing nitrogen data with columns including
#' `Year`, `Province_name`, `Destiny`, `Box`, and `MgN`.
#'
#' @return A tibble with total feed nitrogen (MgN) summed by year and province.
#' @keywords internal
.aggregate_total_feed_mgn <- function(df) {
  df |>
    dplyr::filter(
      Destiny == "Feed",
      Box %in% c("Semi_natural_agroecosystems", "Cropland")
    ) |>
    dplyr::group_by(Year, Province_name) |>
    dplyr::summarise(Total_feed_MgN = sum(
      MgN,
      na.rm = TRUE
    ), .groups = "drop") |>
    dplyr::arrange(Year, Province_name)
}

#' Calculate Feed share (between semi natural agroecosystems and total feed)
#'
#' @param df A data frame containing nitrogen data with columns including
#' `Year`, `Province_name`, `Destiny`, `Box`, and `MgN`.
#'
#' @return A data frame including the share of feed from semi natural
#' agroecosystems
#' @keywords internal
.calculate_semi_nat_feed_share <- function(df) {
  total_feed <- .aggregate_total_feed_mgn(df)
  semi_nat_feed <- .aggregate_semi_nat_feed_mgn(df)

  dplyr::left_join(total_feed, semi_nat_feed,
    by = c("Year", "Province_name")
  ) |>
    dplyr::mutate(
      Semi_nat_feed_MgN = ifelse(
        is.na(Semi_nat_feed_MgN), 0, Semi_nat_feed_MgN
      ),
      Semi_nat_share = Semi_nat_feed_MgN / Total_feed_MgN
    ) |>
    dplyr::arrange(Year, Province_name)
}

#' Calculate feed domestic supply ---------------------------------------------
#'
#' @param grafs_df A data frame containing GRAFS data with the columns Destiny',
#' 'Year', 'Province_name', and 'MgN'.
#' @param lu_df A data frame with land use data.
#'
#' @return A tibble with columns 'Year', 'Province_name', and
#' 'Domestic_feed_MgN' representing the total domestic feed supply in MgN.
#' @keywords internal
.calculate_feed_domest_supply <- function(grafs_df, lu_df) {
  domestic_feed <- grafs_df |>
    dplyr::filter(Destiny == "Feed") |>
    dplyr::group_by(Year, Province_name) |>
    dplyr::summarise(Domestic_feed_MgN = sum(
      MgN,
      na.rm = TRUE
    ), .groups = "drop")

  # Add LU_total for use in further steps
  domestic_feed |>
    dplyr::left_join(
      lu_df |> dplyr::select(Year, Province_name, LU_total),
      by = c("Year", "Province_name")
    )
}

#' Calculate feed import per province -----------------------------------------
#'
#' @param feed_df A data frame containing feed data.
#' @param lu_df A data frame with land use information.
#'
#' @return A tibble with columns 'Year', 'Province_name', 'LU_total',
#' 'LU_share', and 'Feed_import_MgN', where 'Feed_import_MgN' is the estimated
#'  feed import allocated to each province.
#' @keywords internal
.calculate_feed_import_share <- function(feed_df, lu_df) {
  feed_filtered <- feed_df |>
    dplyr::filter(Element == "Import", Destiny == "Feed") |>
    dplyr::group_by(Year) |>
    dplyr::summarise(Total_feed_import = sum(
      Value_destiny,
      na.rm = TRUE
    ), .groups = "drop")

  total_lu_spain <- lu_df |>
    dplyr::group_by(Year) |>
    dplyr::summarise(LU_total_spain = sum(
      LU_total,
      na.rm = TRUE
    ), .groups = "drop")

  lu_with_share <- lu_df |>
    dplyr::left_join(total_lu_spain, by = "Year") |>
    dplyr::mutate(LU_share = LU_total / LU_total_spain)

  feed_import_by_province <- lu_with_share |>
    dplyr::left_join(feed_filtered, by = "Year") |>
    dplyr::mutate(Feed_import_MgN = LU_share * Total_feed_import) |>
    dplyr::select(Year, Province_name, LU_total, LU_share, Feed_import_MgN)

  feed_import_by_province
}

#' Calculate feed share of imported/consumed feed -----------------------------
#'
#' @param feed_import_by_province A data frame containing imported feed data.
#' @param domestic_feed_by_province A data frame containing domestic feed data.
#'
#' @return A data frame with the imported feed share.
#' @keywords internal
.calculate_imported_feed_share <- function(
  feed_import_by_province,
  domestic_feed_by_province
) {
  feed_import_by_province |>
    dplyr::left_join(
      domestic_feed_by_province,
      by = c("Year", "Province_name")
    ) |>
    dplyr::mutate(
      Total_feed_MgN = Domestic_feed_MgN + Feed_import_MgN,
      Imported_feed_share = Feed_import_MgN / Total_feed_MgN,
      Imported_feed_share = ifelse(
        is.nan(Imported_feed_share), NA, Imported_feed_share
      )
    ) |>
    dplyr::select(
      Year,
      Province_name,
      LU_total = LU_total,
      Feed_import_MgN,
      Domestic_feed_MgN,
      Total_feed_MgN,
      Imported_feed_share
    )
}

#' Assign Typologies and optionally plot map
#'
#' @param livestock_density A data frame with livestock density values.
#' @param productivity A data frame with productivity (kgN/ha) values.
#' @param semi_nat_share A data frame with semi-natural agroecosystem share.
#' @param imported_feed_share A data frame with share of imported feed.
#' @param sf_provinces An sf object with province geometries.
#' @param year Integer specifying the year for which typologies are assigned.
#'
#' @return A tibble with province names and their assigned farming system
#' typology for the specified year.
#' @keywords internal
.assign_decision_tree <- function(
  livestock_density, productivity, semi_nat_share, imported_feed_share,
  sf_provinces, year
) {
  typologies <- livestock_density |>
    dplyr::inner_join(productivity, by = c("Year", "Province_name")) |>
    dplyr::inner_join(semi_nat_share, by = c("Year", "Province_name")) |>
    dplyr::inner_join(imported_feed_share, by = c("Year", "Province_name")) |>
    dplyr::filter(Year == year) |>
    dplyr::mutate(
      Typologie = dplyr::case_when(
        Livestock_density < 0.3 &
          Productivity_kgN_ha > 60 ~ "Specialized cropping system",
        Livestock_density < 0.3 &
          Productivity_kgN_ha <= 60 ~ "Extensive cropping system",
        Livestock_density >= 0.3 &
          Semi_nat_share > 0.6 ~ "Extensive mixed crop-livestock system",
        Livestock_density >= 0.3 &
          Semi_nat_share <= 0.6 &
          Imported_feed_share < 0.5 ~ "Intensive mixed crop-livestock system",
        Livestock_density >= 0.3 &
          Semi_nat_share <= 0.6 &
          Imported_feed_share >= 0.5 ~ "Specialized livestock-farming system",
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::select(Province_name, Typologie)

  sf_provinces_filtered <- sf_provinces |>
    dplyr::rename(Province_name = name) |>
    dplyr::inner_join(typologies, by = "Province_name")

  map <- ggplot2::ggplot(sf_provinces_filtered) +
    ggplot2::geom_sf(ggplot2::aes(fill = Typologie), color = "white") +
    ggplot2::scale_fill_manual(
      values = c(
        "Specialized cropping system" = "#FFD700",
        "Extensive cropping system" = "#FFFF99",
        "Extensive mixed crop-livestock system" = "#66a61e",
        "Intensive mixed crop-livestock system" = "#d95f02",
        "Specialized livestock-farming system" = "#7570b3"
      ),
      na.value = "grey80"
    ) +
    ggplot2::labs(
      title = paste("Typologies by Province -", year),
      fill = "Typologie"
    ) +
    ggplot2::theme_minimal()

  # Typologies for each year as a dataset
  typologies_all_years <- livestock_density |>
    dplyr::inner_join(productivity, by = c("Year", "Province_name")) |>
    dplyr::inner_join(semi_nat_share, by = c("Year", "Province_name")) |>
    dplyr::inner_join(imported_feed_share, by = c("Year", "Province_name")) |>
    dplyr::mutate(
      Typologie = dplyr::case_when(
        Livestock_density < 0.3 &
          Productivity_kgN_ha > 60 ~ "Specialized cropping system",
        Livestock_density < 0.3 &
          Productivity_kgN_ha <= 60 ~ "Extensive cropping system",
        Livestock_density >= 0.3 &
          Semi_nat_share > 0.6 ~ "Extensive mixed crop-livestock system",
        Livestock_density >= 0.3 &
          Semi_nat_share <= 0.6 &
          Imported_feed_share < 0.5 ~ "Intensive mixed crop-livestock system",
        Livestock_density >= 0.3 &
          Semi_nat_share <= 0.6 &
          Imported_feed_share >= 0.5 ~ "Specialized livestock-farming system",
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::select(Year, Province_name, Typologie) |>
    dplyr::arrange(Year, Province_name)

  list(
    Typologies = typologies,
    Typologies_map = map,
    Typologies_all_years = typologies_all_years
  )
}
