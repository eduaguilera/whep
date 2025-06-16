#' Typologies
#'
create_typologies_grafs_spain <- function() {
  inputs_dir <- "C:/PhD/GRAFS/Production Boxes/Final Files/Inputs"

  # Load datasets
  data <- .load_inputs(inputs_dir)

  # Prepare LU coefficients with Livestock_cat mapping
  lu_coefs_mapped <- .prepare_lu_coefs(data$Codes_coefs)

  # Merge livestock data with LU coefficients and calculate totals
  lu_totals_detailed <- .calculate_lu_totals(data$Livestock_Prod_ygps, lu_coefs_mapped)

  # Aggregate LU_total per Year, Province
  lu_aggregated <- .aggregate_lu_totals(lu_totals_detailed)

  # Aggregate Area
  area_aggregated <- .aggregate_area_aa(data$NPP_ygpit)

  # Calculate livestock density
  livestock_density <- .calculate_livestock_density(lu_aggregated, area_aggregated)

  # Calculate cropland productivity
  cropland_productivity <- .aggregate_cropland_productivity(data$NPP_ygpit)

  # Aggregate feed from semi natural agroecosystems
  semi_natural_feed <- .aggregate_semi_nat_feed_mgn(data$GRAFS_Prod_Destiny_git)

  # Aggregate feed from cropland
  cropland_feed <- .aggregate_cropland_feed_mgn(data$GRAFS_Prod_Destiny_git)

  # Calculate feed share (semi_natural feed / total feed)
  feed_share <- .calculate_semi_nat_feed_share(data$GRAFS_Prod_Destiny_git)

  View(feed_share)

  # Return the relevant results
  list(
    LU_detailed = lu_totals_detailed,
    LU_totals = lu_aggregated,
    Livestock_density = livestock_density,
    Cropland_productivity = cropland_productivity,
    Semi_natural_feed = semi_natural_feed,
    Cropland_feed = cropland_feed,
    Feed_share = feed_share
  )
}

# Load input datasets -------------------------------------------------------------------------------------------------------------------
.load_inputs <- function(inputs_dir) {
  list(
    Livestock_Prod_ygps = readr::read_csv(file.path(inputs_dir, "Livestock_Prod_ygps.csv")),
    Codes_coefs = readxl::read_excel(file.path(inputs_dir, "Codes_coefs.xlsx"), sheet = "Liv_LU_coefs"),
    NPP_ygpit = readr::read_csv(file.path(inputs_dir, "NPP_ygpit.csv.gz")),
    GRAFS_Prod_Destiny_git = readr::read_csv(file.path(inputs_dir, "GRAFS_Prod_Destiny_git.csv"))
  )
}

# Mapping: Livestock_cat (from livestock data) to Animal_class (from coefficients) ------------------------------------------------------
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

# Prepare LU coefficients with Livestock_cat mapping ------------------------------------------------------------------------------------
.prepare_lu_coefs <- function(codes_coefs_df) {
  mapping <- .create_livestockcat_mapping()

  mapping %>%
    dplyr::inner_join(codes_coefs_df, by = "Animal_class") %>%
    dplyr::select(Livestock_cat, Animal_class, LU_head) %>%
    dplyr::distinct()
}

# Calculate LU_total per row ----------------------------------------------------------------------------------------------------------
.calculate_lu_totals <- function(livestock_df, lu_coefs_df) {
  livestock_df %>%
    dplyr::select(Year, Province_name, Livestock_cat, Stock_Number) %>%
    dplyr::left_join(lu_coefs_df, by = "Livestock_cat") %>%
    dplyr::mutate(LU_total = Stock_Number * LU_head) %>%
    dplyr::select(Year, Province_name, Livestock_cat, Animal_class, Stock_Number, LU_head, LU_total) %>%
    dplyr::distinct()
}

# Aggregate LU_total ----------------------------------------------------------------------------------
.aggregate_lu_totals <- function(lu_detailed_df) {
  lu_aggregated <- lu_detailed_df %>%
    dplyr::group_by(Year, Province_name) %>%
    dplyr::summarise(LU_total = sum(LU_total, na.rm = TRUE), .groups = "drop") %>%
    dplyr::arrange(Year, Province_name)

  return(lu_aggregated)
}

# Aggregate Area AA ----------------------------------------------------------------------------------
.aggregate_area_aa <- function(npp_df) {
  npp_df %>%
    dplyr::group_by(Year, Province_name) %>%
    dplyr::summarise(Area_ha = sum(Area_ygpit_ha, na.rm = TRUE), .groups = "drop")
}

# Calculate livestock density ------------------------------------------------------------------------
.calculate_livestock_density <- function(lu_totals_df, area_df) {
  lu_totals_df %>%
    dplyr::left_join(area_df, by = c("Year", "Province_name")) %>%
    dplyr::mutate(Livestock_density = LU_total / Area_ha) %>%
    dplyr::select(Year, Province_name, LU_total, Area_ha, Livestock_density) %>%
    dplyr::arrange(Year, Province_name)
}

# Aggregate Productivity for Cropland ----------------------------------------------------------------------------------
.aggregate_cropland_productivity <- function(npp_df) {
  cropland_prod <- npp_df %>%
    dplyr::filter(LandUse == "Cropland") %>%
    dplyr::group_by(Year, Province_name) %>%
    dplyr::summarise(
      Prod_MgN_total = sum(Prod_MgN, na.rm = TRUE),
      Area_ha_cropland = sum(Area_ygpit_ha, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(Productivity_kgN_ha_yr = Prod_MgN_total / Area_ha_cropland * 1000) %>%
    dplyr::arrange(Year, Province_name)

  return(cropland_prod)
}

# Aggregate Feed from Semi natural agroecosystems for Grassland > 60% of Livestock intake from Grassland ----------------
.aggregate_semi_nat_feed_mgn <- function(df) {
  df %>%
    dplyr::filter(Box == "Semi_natural_agroecosystems", Destiny == "Feed") %>%
    dplyr::group_by(Year, Province_name) %>%
    dplyr::summarise(Semi_nat_feed_MgN = sum(MgN, na.rm = TRUE), .groups = "drop")
}

# Aggregate Feed from Cropland -----------------------------------------------------------------------------------------
.aggregate_cropland_feed_mgn <- function(df) {
  df %>%
    dplyr::filter(Box == "Cropland", Destiny == "Feed") %>%
    dplyr::group_by(Year, Province_name) %>%
    dplyr::summarise(Cropland_feed_MgN = sum(MgN, na.rm = TRUE), .groups = "drop")
}

# Aggregate total feed from all boxes (Feed destiny) -------------------------------------------------------------------
.aggregate_total_feed_mgn <- function(df) {
  df %>%
    dplyr::filter(Destiny == "Feed", Box %in% c("Semi_natural_agroecosystems", "Cropland")) %>%
    dplyr::group_by(Year, Province_name) %>%
    dplyr::summarise(Total_feed_MgN = sum(MgN, na.rm = TRUE), .groups = "drop") %>%
    dplyr::arrange(Year, Province_name)
}

# Calculate Feed share (between semi natural agroecosystems and total feed)
.calculate_semi_nat_feed_share <- function(df) {
  total_feed <- .aggregate_total_feed_mgn(df)
  semi_nat_feed <- .aggregate_semi_nat_feed_mgn(df)

  dplyr::left_join(total_feed, semi_nat_feed, by = c("Year", "Province_name")) %>%
    dplyr::mutate(
      Semi_nat_feed_MgN = ifelse(is.na(Semi_nat_feed_MgN), 0, Semi_nat_feed_MgN),
      Semi_nat_share = Semi_nat_feed_MgN / Total_feed_MgN
    ) %>%
    dplyr::arrange(Year, Province_name)
}
