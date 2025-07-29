#' @title Nitrogen (N) soil inputs and Nitrogen Use Efficiency (NUE) for
#' crops in Spain
#'
#' @description
#' N inputs (deposition, fixation, synthetic fertilizers, urban sources, manure)
#' and N production in Spain from 1860 to the present for the GRAFS model at the
#' provincial level.
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
#'
#' @export
create_n_soil_inputs <- function() {
  data <- .load_inputs_n_inputs()

  n_soil_inputs <- .calculate_n_soil_inputs(
    data$n_balance_ygpit_all,
    data$codes_coefs
  )

  n_soil_inputs
}

#' @title Soil N Inputs --------------------------------------------------------
#' @description Loading all required datasets.
#' @return A named list including four datasets.
#' @keywords internal
#' @noRd
.load_inputs_n_inputs <- function() {
  result <-
    list(
      # TODO: Feed intake need to be added to dataset as an input of Livestock
      n_Excretion_ygs = whep_read_file("n_excretion_ygs"),
      n_balance_ygpit_all = whep_read_file("n_balance_ygpit_all"),
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
.calculate_n_soil_inputs <- function(
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
#' @return A tibble with combined N input and production data.
#' @export
create_n_production <- function() {
  data <- .load_inputs_n_inputs()

  n_prod_data <- .calculate_n_production(
    data$grafs_prod_destiny
  )

  n_prod_data
}

#' @title Calculate N Production
#' @description Internal function to calculate nitrogen production.
#' @param grafs_prod_destiny A data frame with consumption, export, import data.
#' @return A tibble with production and import values.
#' @keywords internal
#' @noRd
.calculate_n_production <- function(
  grafs_prod_destiny
) {
  n_prod_data <- grafs_prod_destiny |>
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
    dplyr::arrange(Year, Province_name, Item, Box) |>
    dplyr::select(Year, Province_name, Item, Box, import, prod)

  n_prod_data
}

#' @title NUE for Cropland and Semi-natural agroecosystems ---------------------
#' @description Calculates NUE for cropland and semi-natural agroecosystems as
#' the ratio of production to total N soil input.
#'
#' @return A tibble with calculated NUE values for cropland and semi-natural
#' agroecosystems.
#'
#' @export
calculate_nue_crops <- function() {
  n_soil_inputs <- create_n_soil_inputs()
  n_prod_data <- create_n_production()

  nue <- dplyr::inner_join(
    n_soil_inputs, n_prod_data,
    by = c("Year", "Province_name", "Item", "Box")
  ) |>
    dplyr::filter(!is.na(Box)) |>
    dplyr::mutate(
      inputs = deposition + fixation + synthetic + manure + urban
    ) |>
    dplyr::filter(
      !is.na(prod), !is.na(inputs),
      prod > 0, inputs > 0
    ) |>
    dplyr::mutate(
      nue = prod / inputs * 100
    ) |>
    dplyr::select(Year, Province_name, Item, Box, nue)

  nue
}
