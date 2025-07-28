#' @title Nitrogen (N) inputs and Nitrogen Use Efficiency (NUE) for GRAFS Spain
#'
#' @description
#' N inputs (deposition, fixation, synthetic fertilizers, urban sources, manure)
#' and N production in Spain from 1860 to the present for the GRAFS model at the
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
#'   semi-natural agroecosystems.
#'   - `deposition`: Atmospheric nitrogen deposition in megagrams (Mg).
#'   - `fixation`: Nitrogen fixation in megagrams (Mg).
#'   - `synthetic`: Synthetic nitrogen fertilizer applied to the land in
#'   megagrams (Mg).
#'   - `manure`: Nitrogen in manure applied to the land in megagrams (Mg).
#'   - `urban`: Nitrogen in wastewater from human sources in megagrams (Mg).
#'   - `import`: Imported nitrogen in megagrams (Mg).
#'   - `prod`: Produced nitrogen in megagrams (Mg).
#'   - `inputs`: Total nitrogen inputs in megagrams (Mg).
#'   - `nue`: Nitrogen use efficiency, expressed in percent.
#'
#' @export
create_n_inputs_grafs_spain <- function() {
  data <- .load_inputs_n_inputs()

  n_soil_inputs <- .calculate_n_inputs(
    data$n_balance_ygpit_all,
    data$codes_coefs
  )

  n_inputs_combined <- .summarise_production(
    data$grafs_prod_destiny,
    n_soil_inputs
  )

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
      # TODO: Excretion need to be added to dataset as an input of Livestock
      n_Excretion_ygs = whep_read_file("n_excretion_ygs") |>
        dplyr::ungroup(),
      n_balance_ygpit_all = whep_read_file("n_balance_ygpit_all") |>
        dplyr::ungroup(),
      grafs_prod_destiny = whep_read_file("grafs_prod_destiny"),
      codes_coefs = whep_read_file("codes_coefs")
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
#' @return One tibble: 'n_soil_inputs'
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
    dplyr::distinct(Name_biomass, Item)

  # Combine all necessary n Inputs
  n_soil_inputs <- n_balance_ygpit_all |>
    dplyr::left_join(items, by = "Name_biomass") |>
    dplyr::mutate(
      Item = ifelse(Name_biomass %in% firewood_biomass, "Firewood", Item),
      Box = ifelse(LandUse %in% semi_natural_agroecosystems,
        "semi_natural_agroecosystems", LandUse
      )
    ) |>
    dplyr::summarise(
      deposition = sum(Deposition, na.rm = TRUE),
      fixation = sum(BNF, na.rm = TRUE),
      synthetic = sum(Synthetic, na.rm = TRUE),
      manure = sum(Excreta + Solid + Liquid, na.rm = TRUE),
      urban = sum(Urban, na.rm = TRUE),
      .by = c(Year, Province_name, Item, Box)
    ) |>
    dplyr::arrange(Year, Province_name)

  n_soil_inputs
}


#' @title GRAFS_Prod_Destiny ---------------------------------------------------
#' @description Summarizes and calculates new columns: prod
#' Spread Destiny column to separate columns for Food, Feed, Other_uses, Export.
#'
#' @param grafs_prod_destiny Data containing production values by destiny.
#' @param n_soil_inputs A tibble of summarized N inputs.
#'
#' @return A tibble with combined N input and production data.
#' @keywords internal
#' @noRd
.summarise_production <- function(
  grafs_prod_destiny,
  n_soil_inputs
) {
  grafs_prod_destiny_summary <- grafs_prod_destiny |>
    dplyr::filter(!is.na(Box)) |>
    tidyr::pivot_wider(
      names_from = Destiny, values_from = MgN, values_fn = sum,
      values_fill = list(MgN = 0)
    ) |>
    dplyr::mutate(
      prod = (Food + Feed + Other_uses + Export) - Import,
      import = Import,
      # Set Production to 0 for Fish Box
      prod = ifelse(Box == "Fish", 0, prod)
    ) |>
    dplyr::group_by(Year, Province_name, Item, Box) |>
    dplyr::summarise(
      import = sum(import, na.rm = TRUE),
      prod = sum(prod, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::ungroup() |>
    dplyr::arrange(Year, Province_name, Item, Box) |>
    dplyr::select(Year, Province_name, Item, Box, import, prod)

  # Combine with n_inputs dataset
  n_inputs_combined <- dplyr::full_join(
    n_soil_inputs, grafs_prod_destiny_summary,
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
      inputs = deposition + fixation + synthetic + manure + urban
    ) |>
    dplyr::mutate(
      nue = ifelse(Box %in% c("semi_natural_agroecosystems", "Cropland"),
        prod / inputs * 100,
        NA_real_
      )
    ) |>
    dplyr::ungroup()

  nue
}
