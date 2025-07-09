#' @title Create N Inputs and NUE Dataset for GRAFS Spain
#'
#' @description
#' N Inputs, Production and NUE in Spain:
#' This code is creating a dataset with nitrogen (N) inputs (deposition,
#' fixation, synthetic, urban, manure)
#' and N production in Spain between 1860 and 2020 for the GRAFS model on a
#' provincial level
#'
#' @return
#' A named list with two elements:
#' \describe{
#'   \item{n_inputs_combined}{A tibble with nitrogen input and production data
#'   by year, province, item, and box.}
#'   \item{nue}{A tibble with calculated nitrogen use efficiency for cropland
#'   and semi-natural ecosystems.}
#' }
#'
#' @export
create_n_inputs_grafs_spain <- function() {
  # Load datasets
  data <- .load_inputs_n_inputs()

  # Calculate N inputs and manure
  n_inputs_prepared <- .calculate_n_inputs(
    data$n_balance_ygpit_all,
    data$codes_coefs
  )

  # Summarise inputs
  n_inputs_summary <- .summarise_inputs(n_inputs_prepared)

  # Summarise production
  n_inputs_combined <- .summarise_production(
    data$grafs_prod_destiny,
    n_inputs_summary
  )

  # Calculate NUE
  nue <- .calculate_nue(n_inputs_combined)

  # Return the loaded datasets
  list(
    n_inputs_combined = n_inputs_combined,
    nue = nue
  )
}

# N Inputs --------------------------------------------------------------------
# load data -------------------------------------------------------------------
# @keywords internal
.load_inputs_n_inputs <- function() {
  result <-
    list(
      n_Excretion_ygs = readRDS(get_file_path("n_excretion_ygs")),
      # TODO: Excretion need to be added to dataset as an input of Livestock
      n_balance_ygpit_all = readRDS(get_file_path("n_balance_ygpit_all")),
      grafs_prod_destiny = readr::read_csv(get_file_path(
        "GRAFS_prod_destiny_git"
      )),
      codes_coefs = readxl::read_excel(get_file_path("codes_coefs"),
        sheet = "Names_biomass_CB"
      )
    )
  result
}

# Assign some special items to Boxes ------------------------------------------
# @keywords internal
.assign_items <- function() {
  list(
    semi_natural_agroecosystems = c(
      "Dehesa", "Forest_high", "Forest_low",
      "Other", "Pasture_Shrubland"
    ),
    Firewood_biomass = c(
      "Holm oak", "Mediterranean shrubland", "Conifers",
      "Holm oak forest"
    ),
    residue_items = c("Other crop residues", "Straw", "Firewood")
  )
}

# Calculate n Inputs ----------------------------------------------------------
# @keywords internal
.calculate_n_inputs <- function(n_balance_ygpit_all, codes_coefs) {
  categories <- .assign_items()
  firewood_biomass <- categories$Firewood_biomass
  semi_natural_agroecosystems <- categories$semi_natural_agroecosystems

  # Merge Name_biomass with Item
  items <- codes_coefs |>
    dplyr::select(Name_biomass, Item)

  # Combine all necessary n Inputs
  n_inputs_summary <- n_balance_ygpit_all |>
    dplyr::left_join(items, by = "Name_biomass") |>
    dplyr::mutate(
      Item = ifelse(Name_biomass %in% firewood_biomass, "Firewood", Item),
      Box = ifelse(LandUse %in% semi_natural_agroecosystems,
        "semi_natural_agroecosystems", LandUse
      )
    ) |>
    dplyr::group_by(Year, Province_name, Name_biomass, Item, Box) |>
    dplyr::summarise(
      Deposition = sum(Deposition, na.rm = TRUE),
      BNF = sum(BNF, na.rm = TRUE),
      Synthetic = sum(Synthetic, na.rm = TRUE),
      Urban = sum(Urban, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::select(
      Year, Province_name, Name_biomass, Item, Box, Deposition,
      BNF, Synthetic, Urban
    )

  # Calculation of Manure from Excreta, Solid, Liquid
  item_lookup <- codes_coefs |>
    dplyr::select(Name_biomass, Item) |>
    dplyr::distinct()

  manure_selected <- n_balance_ygpit_all |>
    dplyr::left_join(item_lookup, by = "Name_biomass") |>
    dplyr::mutate(
      Item = ifelse(Name_biomass %in% firewood_biomass, "Firewood", Item),
      Box = ifelse(LandUse %in% semi_natural_agroecosystems,
        "semi_natural_agroecosystems", LandUse
      )
    ) |>
    dplyr::select(
      Year, Province_name, Name_biomass, Item, Box, Excreta,
      Solid, Liquid
    )

  manure_summary <- manure_selected |>
    dplyr::group_by(Year, Province_name, Name_biomass, Item, Box) |>
    dplyr::summarise(
      Total_Manure = sum(Excreta + Solid + Liquid, na.rm = TRUE),
      .groups = "drop"
    )

  list(
    n_inputs_summary = n_inputs_summary,
    manure_summary = manure_summary
  )
}

# Combine all Inputs ----------------------------------------------------------
# @keywords internal
.summarise_inputs <- function(n_inputs_prepared) {
  n_inputs <- dplyr::full_join(
    n_inputs_prepared$n_inputs_summary,
    n_inputs_prepared$manure_summary,
    by = c("Year", "Province_name", "Name_biomass", "Item", "Box")
  )

  # Summing Inputs for each Year, Province_name, Box
  n_inputs_sum <- n_inputs |>
    dplyr::group_by(Year, Province_name, Item, Box) |>
    dplyr::summarise(
      MgN_dep = sum(Deposition, na.rm = TRUE),
      MgN_fix = sum(BNF, na.rm = TRUE),
      MgN_syn = sum(Synthetic, na.rm = TRUE),
      MgN_manure = sum(Total_Manure, na.rm = TRUE),
      MgN_urban = sum(Urban, na.rm = TRUE),
      .groups = "drop"
    )
  n_inputs_sum
}

# GRAFS_Prod_Destiny ---------------------------------------------------------
# Summarize and calculate new columns: Prod_MgN
# Spread Destiny column to separate columns for Food, Feed, Other_uses, Export
# @keywords internal
.summarise_production <- function(grafs_prod_destiny, n_inputs_sum) {
  grafs_prod_destiny_summary <- grafs_prod_destiny |>
    tidyr::pivot_wider(
      names_from = Destiny, values_from = MgN, values_fn = sum,
      values_fill = list(MgN = 0)
    ) |>
    dplyr::mutate(
      Prod_MgN = (Food + Feed + Other_uses + Export) - Import,
      Import_MgN = Import,
      # Set Production to 0 for Fish Box
      Prod_MgN = ifelse(Box == "Fish", 0, Prod_MgN)
    ) |>
    dplyr::group_by(Year, Province_name, Item, Box) |>
    dplyr::summarise(
      Import_MgN = sum(Import_MgN, na.rm = TRUE),
      Prod_MgN = sum(Prod_MgN, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(Year, Province_name, Item, Box) |>
    dplyr::select(Year, Province_name, Item, Box, Import_MgN, Prod_MgN)

  # Combine with n_inputs dataset
  n_inputs_combined <- dplyr::full_join(n_inputs_sum,
    grafs_prod_destiny_summary,
    by = c("Year", "Province_name", "Item", "Box")
  ) |>
    dplyr::filter(!is.na(Box))

  n_inputs_combined
}

# NUE for Cropland and Semi-natural agroecosystems ----------------------------
# @keywords internal
.calculate_nue <- function(n_inputs_combined) {
  nue <- n_inputs_combined |>
    dplyr::mutate(
      Inputs_MgN = MgN_dep + MgN_fix + MgN_syn + MgN_manure + MgN_urban
    ) |>
    dplyr::mutate(
      nue = ifelse(Box %in% c("semi_natural_agroecosystems", "Cropland"),
        Prod_MgN / Inputs_MgN * 100,
        NA_real_
      )
    )

  nue
}
