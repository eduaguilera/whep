#' @title Nitrogen (N) inputs and Nitrogen Use Efficiency (NUE) for GRAFS Spain
#'
#' @description
#' N inputs (deposition, fixation, synthetic fertilizers, urban sources, manure)
#' and N production in Spain between 1860 and 2020 for the GRAFS model at the
#' provincial level. Nitrogen use efficiency (NUE) is also calculated for
#' cropland and semi-natural ecosystems.
#'
#' @returns
#' A tibble containing nitrogen input, production, and NUE data.
#'   It includes the following columns:
#'   - `Year`: The year in which the recorded event occurred.
#'   - `Province_name`: The Spanish province where the data is from.
#'   - `Item`: The item which was produced, defined in `codes_coefs`.
#'   - `Box`: One of the two systems of the GRAFS model: cropland or
#'            semi-natural agroecosystems.
#'   - `MgN_dep`: Atmospheric nitrogen deposition in megagrams (Mg).
#'   - `MgN_fix`: Nitrogen fixation in megagrams (Mg).
#'   - `MgN_syn`: Synthetic nitrogen fertilizer applied to the land in megagrams
#'               (Mg).
#'   - `MgN_manure`: Nitrogen in manure applied to the land in megagrams (Mg).
#'   - `MgN_urban`: Nitrogen in wastewater from human sources in megagrams (Mg).
#'   - `Import_MgN`: Imported nitrogen in megagrams (Mg).
#'   - `Prod_MgN`: Produced nitrogen in megagrams (Mg).
#'   - `Inputs_MgN`: Total nitrogen inputs in megagrams (Mg).
#'   - `nue`: Nitrogen use efficiency, expressed in percent.
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
  .calculate_nue(n_inputs_combined)
}

#' @title N Inputs -------------------------------------------------------------
#' @description Loading all required datasets.
#' @return A named list including four datasets.
#' @keywords internal
#' @noRd
.load_inputs_n_inputs <- function() {
  result <-
    list(
      n_Excretion_ygs =
        readRDS(get_file_path("n_excretion_ygs")) |> dplyr::ungroup(),
      # TODO: Excretion need to be added to dataset as an input of Livestock
      n_balance_ygpit_all =
        readRDS(get_file_path("n_balance_ygpit_all")) |> dplyr::ungroup(),
      grafs_prod_destiny =
        readr::read_csv(get_file_path("GRAFS_prod_destiny_git")),
      codes_coefs =
        readxl::read_excel(
          get_file_path("codes_coefs"),
          sheet = "Names_biomass_CB"
        )
    )
  result
}

#' @title Assign some special items to Boxes -----------------------------------
#' @return A named list with assigned items.
#' @keywords internal
#' @noRd
.assign_items <- function() {
  list(
    semi_natural_agroecosystems = c(
      "Dehesa",
      "Forest_high",
      "Forest_low",
      "Other",
      "Pasture_Shrubland"
    ),
    Firewood_biomass = c(
      "Conifers",
      "Holm oak",
      "Holm oak forest",
      "Mediterranean shrubland"
    )
  )
}

#' @title Calculate N Inputs ---------------------------------------------------
#' @description Merges N balance data with items and aggregates deposition,
#' fixation, synthetic, urban, and manure inputs for each combination of year,
#' province, item, and box.
#'
#' @param n_balance_ygpit_all A data frame containing nitrogen balance data.
#' @param codes_coefs A data frame merging biomass names to item names.
#'
#' @return A list with two tibbles: 'n_inputs_summary' and 'manure_summary'.
#' @keywords internal
#' @noRd
.calculate_n_inputs <- function(
  n_balance_ygpit_all,
  codes_coefs
) {
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

#' @title Combine all Inputs ---------------------------------------------------
#' @description Combines different N input sources and calculates total values
#' per year, province, item, and box.
#'
#' @param n_inputs_prepared A list with 'n_inputs_summary' and 'manure_summary'
#'
#' @return A tibble summarising nitrogen input components by item and region.
#' @keywords internal
#' @noRd
.summarise_inputs <- function(
  n_inputs_prepared
) {
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
    ) |>
    dplyr::ungroup()

  n_inputs_sum
}

#' @title GRAFS_Prod_Destiny ---------------------------------------------------
#' @description Summarizes and calculates new columns: Prod_MgN
#' Spread Destiny column to separate columns for Food, Feed, Other_uses, Export.
#'
#' @param grafs_prod_destiny Data containing production values by destiny.
#' @param n_inputs_sum A tibble of summarized N inputs.
#'
#' @return A tibble with combined N input and production data.
#' @keywords internal
#' @noRd
.summarise_production <- function(
  grafs_prod_destiny,
  n_inputs_sum
) {
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
    dplyr::ungroup() |>
    dplyr::arrange(Year, Province_name, Item, Box) |>
    dplyr::select(Year, Province_name, Item, Box, Import_MgN, Prod_MgN)

  # Combine with n_inputs dataset
  n_inputs_combined <- dplyr::full_join(
    n_inputs_sum, grafs_prod_destiny_summary,
    by = c("Year", "Province_name", "Item", "Box")
  ) |>
    dplyr::filter(!is.na(Box)) |>
    dplyr::ungroup()

  n_inputs_combined
}

#' @title NUE for Cropland and Semi-natural agroecosystems ---------------------
#' @description Calculates NUE for cropland and semi-natural agroecosystems as
#' the ratio of production to total N input.
#'
#' @param n_inputs_combined A data frame combining N inputs and production.
#'
#' @return A tibble with calculated NUE values for cropland and semi-natural
#' agroecosystems.
#' @keywords internal
#' @noRd
.calculate_nue <- function(
  n_inputs_combined
) {
  nue <- n_inputs_combined |>
    dplyr::mutate(
      Inputs_MgN = MgN_dep + MgN_fix + MgN_syn + MgN_manure + MgN_urban
    ) |>
    dplyr::mutate(
      nue = ifelse(Box %in% c("semi_natural_agroecosystems", "Cropland"),
        Prod_MgN / Inputs_MgN * 100,
        NA_real_
      )
    ) |>
    dplyr::ungroup()

  nue
}
