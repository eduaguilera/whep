#' N Inputs, Production and NUE in Spain
#'
#' @description
#' This code is creating a dataset with nitrogen (N) inputs (deposition,
#' fixation, synthetic, urban, manure)
#' and N production in Spain between 1860 and 2020 for the GRAFS model on a
#' provincial level
#'
#' @returns
#' The output (N_Inputs_combined) contains data on N in MgN for the columns
#' MgN_dep,  MgN_fix, MgN_syn, MgN_manure, MgN_urban, Prod_MgN
#' for each Year, Province and Box (Cropland, Semi_natural_agroecosystems,
#' Livestock, Fish, Additives).
#' As a base, data from the files N_balance_ygpit_all.rds and
#' GRAFS_Prod_Destiny.csv were used.
#' The data are assigned to Items. For this purpose, definitions in
#' Name_biomasses were assigned to Items with the file
#' Codes_coefs.xlsx (sheet = Names_biomass_CB).
#' Manure was calculated by the sum of Excreta, Solid,
#' and Liquid from the file N_balance_ygpit_all.rds.
#' N production was calculated with data from the file
#' GRAFS_Prod_Destiny.csv withe the following formula:
#' (Prod_MgN = (Food + Feed + Other_uses + Export) - Import)
#' Nitrogen use efficiency (NUE) for Cropland and
#' Semi_natural_agroecosystems was calculated by the sum of Inputs
#' with the following formula: (NUE = (Prod_MgN / Inputs_MgN) * 100)


create_n_inputs_grafs_spain <- function() {
  # Load datasets
  data <- .load_inputs_N_inputs()

  # Calculate N inputs and manure
  n_inputs_prepared <- .calculate_n_inputs(
    data$N_balance_ygpit_all,
    data$Codes_coefs
  )

  # Summarise inputs
  n_inputs_summary <- .summarise_inputs(n_inputs_prepared)

  # Summarise production
  n_inputs_combined <- .summarise_production(
    data$GRAFS_Prod_Destiny,
    n_inputs_summary
  )

  # Calculate NUE
  nue <- .calculate_nue(n_inputs_combined)

  # Return the loaded datasets
  list(
    N_Inputs_combined = n_inputs_combined,
    NUE = nue
  )
}

# N Inputs --------------------------------------------------------------------
# load data -------------------------------------------------------------------
.load_inputs_N_inputs <- function() {
  result <-
    list(
      N_Excretion_ygs = readRDS(get_file_path("n_excretion_ygs")),
      # TODO: Excretion need to be added to dataset as an input of Livestock
      N_balance_ygpit_all = readRDS(get_file_path("n_balance_ygpit_all")),
      GRAFS_Prod_Destiny = readr::read_csv(
        get_file_path("GRAFS_prod_destiny_git")
      ),
      Codes_coefs = readxl::read_excel(get_file_path("codes_coefs"),
        sheet = "Names_biomass_CB"
      )
    )
  result
}

# Assign some special items to Boxes ------------------------------------------
.assign_items <- function() {
  list(
    Semi_natural_agroecosystems = c(
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

# Calculate N Inputs ----------------------------------------------------------
.calculate_n_inputs <- function(N_balance_ygpit_all, Codes_coefs) {
  categories <- .assign_items()
  firewood_biomass <- categories$Firewood_biomass
  Semi_natural_agroecosystems <- categories$Semi_natural_agroecosystems

  # Merge Name_biomass with Item
  Items <- Codes_coefs |>
    dplyr::select(Name_biomass, Item)

  # Combine all necessary N Inputs
  N_inputs_summary <- N_balance_ygpit_all |>
    dplyr::left_join(Items, by = "Name_biomass") |>
    dplyr::mutate(
      Item = ifelse(Name_biomass %in% firewood_biomass, "Firewood", Item),
      Box = ifelse(LandUse %in% Semi_natural_agroecosystems,
        "Semi_natural_agroecosystems", LandUse
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
      Year, Province_name, Name_biomass, Item, Box,
      Deposition, BNF, Synthetic, Urban
    )

  # Calculation of Manure from Excreta, Solid, Liquid
  item_lookup <- Codes_coefs |>
    dplyr::select(Name_biomass, Item) |>
    dplyr::distinct()

  manure_selected <- N_balance_ygpit_all |>
    dplyr::left_join(item_lookup, by = "Name_biomass") |>
    dplyr::mutate(
      Item = ifelse(Name_biomass %in% firewood_biomass, "Firewood", Item),
      Box = ifelse(LandUse %in% Semi_natural_agroecosystems,
        "Semi_natural_agroecosystems", LandUse
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
    N_inputs_summary = N_inputs_summary,
    manure_summary = manure_summary
  )
}

# Combine all Inputs ----------------------------------------------------------
.summarise_inputs <- function(n_inputs_prepared) {
  N_Inputs <- dplyr::full_join(
    n_inputs_prepared$N_inputs_summary,
    n_inputs_prepared$manure_summary,
    by = c("Year", "Province_name", "Name_biomass", "Item", "Box")
  )

  # Summing Inputs for each Year, Province_name, Box
  N_Inputs_sum <- N_Inputs |>
    dplyr::group_by(Year, Province_name, Item, Box) |>
    dplyr::summarise(
      MgN_dep = sum(Deposition, na.rm = TRUE),
      MgN_fix = sum(BNF, na.rm = TRUE),
      MgN_syn = sum(Synthetic, na.rm = TRUE),
      MgN_manure = sum(Total_Manure, na.rm = TRUE),
      MgN_urban = sum(Urban, na.rm = TRUE),
      .groups = "drop"
    )
  N_Inputs_sum
}

#' GRAFS_Prod_Destiny ---------------------------------------------------------
#' Summarize and calculate new columns: Prod_MgN
#' Spread the Destiny column to separate columns for Food,
#' Feed, Other_uses,Export

.summarise_production <- function(GRAFS_Prod_Destiny, N_Inputs_sum) {
  GRAFS_Prod_Destiny_summary <- GRAFS_Prod_Destiny |>
    tidyr::pivot_wider(
      names_from = Destiny, values_from = MgN,
      values_fn = sum, values_fill = list(MgN = 0)
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

  # Combine with N_Inputs dataset
  N_Inputs_combined <- dplyr::full_join(N_Inputs_sum,
    GRAFS_Prod_Destiny_summary,
    by = c("Year", "Province_name", "Item", "Box")
  ) |>
    dplyr::filter(!is.na(Box))

  N_Inputs_combined
}

# NUE for Cropland and Semi-natural agroecosystems ----------------------------
.calculate_nue <- function(N_Inputs_combined) {
  NUE <- N_Inputs_combined |>
    dplyr::mutate(
      Inputs_MgN = MgN_dep + MgN_fix + MgN_syn + MgN_manure + MgN_urban
    ) |>
    dplyr::mutate(
      NUE = ifelse(Box %in% c("Semi_natural_agroecosystems", "Cropland"),
        Prod_MgN / Inputs_MgN * 100,
        NA_real_
      )
    )

  NUE
}
