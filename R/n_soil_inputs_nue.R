#' @title Nitrogen (N) soil inputs for Spain ----------------------------------
#'
#' @description
#' Calculates total nitrogen inputs to soils in Spain at the provincial level.
#' This includes contributions from:
#' - Atmospheric deposition (`deposition`)
#' - Biological nitrogen fixation (`fixation`)
#' - Synthetic fertilizers (`synthetic`)
#' - Manure (excreta, solid, liquid) (`manure`)
#' - Urban sources (`urban`)
#'
#' Special land use categories and items are aggregated:
#' - Semi-natural agroecosystems (e.g., Dehesa, Pasture_Shrubland)
#' - Firewood biomass (e.g., Conifers, Holm oak)
#'
#' @return A tibble containing:
#'   - `Year`: Year
#'   - `Province_name`: Spanish province
#'   - `Item`: Crop, land use, or biomass item
#'   - `Box`: Land use or ecosystem box for aggregation
#'   - `deposition`: N input from atmospheric deposition (Mg)
#'   - `fixation`: N input from biological N fixation (Mg)
#'   - `synthetic`: N input from synthetic fertilizers (Mg)
#'   - `manure`: N input from livestock manure (Mg)
#'   - `urban`: N input from urban sources (Mg)
#'
#' @export
create_n_soil_inputs <- function() {
  .calculate_n_soil_inputs(
    whep_read_file("n_balance_ygpit_all"),
    whep_read_file("codes_coefs")
  )
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
#' @param names_biomass_cb A data frame merging biomass names to item names.
#'
#' @return One tibble: 'n_soil_inputs'
#' @keywords internal
#' @noRd
.calculate_n_soil_inputs <- function(
  n_balance_ygpit_all,
  names_biomass_cb
) {
  categories <- .assign_items()

  # Merge Name_biomass with Item
  items <- names_biomass_cb |>
    dplyr::distinct(Name_biomass, Item)

  # Create mapping tables
  firewood_biomass <- tibble::tibble(
    Name_biomass = categories$Firewood_biomass,
    item_firewood = "Firewood"
  )

  semi_natural_agroecosystems <- tibble::tibble(
    LandUse = categories$semi_natural_agroecosystems,
    Box_semi_natural_agroecosystems = "semi_natural_agroecosystems"
  )

  # Combine all necessary n Inputs
  n_soil_inputs <- n_balance_ygpit_all |>
    dplyr::left_join(items, by = "Name_biomass") |>
    dplyr::left_join(firewood_biomass, by = "Name_biomass") |>
    dplyr::left_join(semi_natural_agroecosystems, by = "LandUse") |>
    dplyr::mutate(
      Item = dplyr::coalesce(item_firewood, Item),
      Box = dplyr::coalesce(Box_semi_natural_agroecosystems, LandUse)
    ) |>
    dplyr::select(-item_firewood, -Box_semi_natural_agroecosystems) |>
    dplyr::summarise(
      deposition = sum(Deposition, na.rm = TRUE),
      fixation = sum(BNF, na.rm = TRUE),
      synthetic = sum(Synthetic, na.rm = TRUE),
      manure = sum(Excreta + Solid + Liquid, na.rm = TRUE),
      urban = sum(Urban, na.rm = TRUE),
      .by = c(Year, Province_name, Item, Irrig_cat, Box)
    ) |>
    dplyr::arrange(Year, Province_name)

  n_soil_inputs
}

#' @title N production for Spain -----------------------------------------------
#'
#' @description Calculates N production at the provincial level in Spain.
#' Production is derived from consumption, export, import, and other uses.
#'
#' @return A tibble containing:
#'   - `Year`: Year
#'   - `Province_name`: Spanish province
#'   - `Item`: Product item
#'   - `Box`: Ecosystem box
#'   - `Box_destiny`: Destination box
#'   - `import`: Imported N (Mg)
#'   - `prod`: Produced N (Mg)
#'
#' @export
create_n_production <- function() {
  grafs_prod_destiny_final <- create_n_prov_destiny()

  .calculate_n_production(grafs_prod_destiny_final)
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
    tidyr::replace_na(list(MgN = 0)) |>
    tidyr::pivot_wider(
      names_from = Destiny,
      values_from = MgN,
      values_fill = list(MgN = 0)
    ) |>
    dplyr::mutate(
      prod = (food + feed + other_uses + export) - import,
      # Set Production to 0 for Fish Box
      prod = ifelse(Box == "Fish", 0, prod)
    ) |>
    dplyr::summarise(
      import = sum(import, na.rm = TRUE),
      prod = sum(prod, na.rm = TRUE),
      .by = c(Year, Province_name, Item, Box, Box_destiny)
    ) |>
    dplyr::arrange(Year, Province_name, Item, Box, Box_destiny) |>
    dplyr::select(Year, Province_name, Item, Box, Box_destiny, import, prod)

  n_prod_data
}

#' @title N soil inputs and Nitrogen Use Efficiency (NUE) for crop -------------
#'
#' @description
#' N inputs (deposition, fixation, synthetic fertilizers, urban sources, manure)
#' and N production in Spain from 1860 to the present for the GRAFS model at the
#' provincial level.
#' The crop NUE is defined as the percentage of produced nitrogen relative to
#' the total nitrogen inputs to the soil.
#' Total soil inputs are calculated as:
#' inputs = deposition + fixation + synthetic + manure + urban
#'
#' @returns
#' A tibble containing nitrogen input, production, and NUE data.
#'   It includes the following columns:
#'   - `Year`: Year.
#'   - `Province_name`: The Spanish province.
#'   - `Item`: The item which was produced, defined in `names_biomass_cb`.
#'   - `Box`: One of the two systems of the GRAFS model: cropland or
#'            semi-natural agroecosystems.
#'   - `deposition`: Atmospheric nitrogen deposition in megagrams (Mg).
#'   - `fixation`: Nitrogen fixation in megagrams (Mg).
#'   - `synthetic`: Synthetic nitrogen fertilizer applied to the land in
#'                  megagrams (Mg).
#'   - `manure`: Nitrogen in manure applied to the land in megagrams (Mg).
#'   - `urban`: Nitrogen in wastewater from human sources in megagrams (Mg).
#'   - `prod`: Produced nitrogen in megagrams (Mg).
#'   - `inputs`: Total nitrogen inputs in megagrams (Mg).
#'
#'
#' @export
calculate_nue_crops <- function() {
  n_soil_inputs <- create_n_soil_inputs()
  n_prod_data <- create_n_production()

  nue <- dplyr::inner_join(
    n_soil_inputs,
    n_prod_data,
    by = c("Year", "Province_name", "Item", "Box")
  ) |>
    dplyr::filter(!is.na(Box)) |>
    dplyr::mutate(
      inputs = deposition + fixation + synthetic + manure + urban
    ) |>
    dplyr::filter(
      !is.na(prod),
      !is.na(inputs),
      prod > 0,
      inputs > 0
    ) |>
    dplyr::mutate(
      nue = prod / inputs * 100
    ) |>
    dplyr::select(Year, Province_name, Item, Box, Box_destiny, nue)

  nue
}


#' @title NUE for Livestock ----------------------------------------------------
#'
#' @description
#' Calculates Nitrogen Use Efficiency (NUE) for livestock categories
#' (excluding pets).
#'
#' The livestock NUE is defined as the percentage of nitrogen in livestock
#' products relative to the nitrogen in feed intake:
#' nue = prod_n / feed_n * 100
#'
#' Additionally, a mass balance is calculated to check the recovery of N in
#' products and excretion relative to feed intake:
#' mass_balance = (prod_n + excretion_n) / feed_n
#'
#' @return A tibble containing:
#'   - `Year`: Year
#'   - `Province_name`: Spanish province
#'   - `Livestock_cat`: Livestock category
#'   - `Item`: Produced item
#'   - `prod_n`: Nitrogen in livestock products (Mg)
#'   - `feed_n`: Nitrogen in feed intake (Mg)
#'   - `excretion_n`: Nitrogen excreted (Mg)
#'   - `nue`: Nitrogen Use Efficiency (%)
#'   - `mass_balance`: Mass balance ratio (%)
#'
#' @export
calculate_nue_livestock <- function() {
  intake_n <- whep_read_file("intake_ygiac") |>
    dplyr::filter(Livestock_cat != "Pets") |>
    dplyr::group_by(Year, Province_name, Livestock_cat) |>
    dplyr::summarise(
      feed_n = sum(N_MgN, na.rm = TRUE),
      .groups = "drop"
    )

  prod_n <- whep_read_file("livestock_prod_ygps") |>
    dplyr::filter(!is.na(Prod_MgN)) |>
    dplyr::group_by(Year, Province_name, Livestock_cat, Item) |>
    dplyr::summarise(
      prod_n = sum(Prod_MgN, na.rm = TRUE),
      .groups = "drop"
    )

  excretion_n <- whep_read_file("n_excretion_ygs") |>
    dplyr::group_by(Year, Province_name, Livestock_cat) |>
    dplyr::summarise(
      excretion_n = sum(Excr_MgN, na.rm = TRUE),
      .groups = "drop"
    )

  nue_livestock <- intake_n |>
    dplyr::inner_join(
      prod_n,
      by = c("Year", "Province_name", "Livestock_cat")
    ) |>
    dplyr::left_join(
      excretion_n,
      by = c("Year", "Province_name", "Livestock_cat")
    ) |>
    dplyr::mutate(
      nue = prod_n / feed_n * 100,
      mass_balance = (prod_n + excretion_n) / feed_n
    ) |>
    dplyr::select(
      Year,
      Province_name,
      Livestock_cat,
      Item,
      prod_n,
      feed_n,
      excretion_n,
      nue,
      mass_balance
    )

  nue_livestock
}

#' @title System NUE -----------------------------------------------------------
#'
#' @description
#' Calculates the NUE for Spain at the provincial level.
#' The system NUE is defined as the percentage of total nitrogen production
#' (`total_prod`) relative to the sum of all nitrogen inputs (`inputs`) into the
#' soil system.
#'
#' @param n_soil_inputs A tibble of nitrogen soil input (deposition, fixation,
#' synthetic, manure, urban)
#'
#' @return A tibble with the following columns:
#'   - `Year`: Year
#'   - `Province_name`: Spanish province
#'   - `total_prod`: Total nitrogen production (Mg)
#'   - `inputs`: Total nitrogen inputs (Mg)
#'   - `nue_system`: System-level Nitrogen Use Efficiency (%)
#'
#' @export
calculate_system_nue <- function(n_soil_inputs = create_n_soil_inputs()) {
  n_soil_inputs <- n_soil_inputs |>
    dplyr::group_by(Year, Province_name) |>
    dplyr::summarise(
      deposition = sum(deposition, na.rm = TRUE),
      fixation = sum(fixation, na.rm = TRUE),
      synthetic = sum(synthetic, na.rm = TRUE),
      manure = sum(manure, na.rm = TRUE),
      urban = sum(urban, na.rm = TRUE),
      .groups = "drop"
    )

  total_outputs <- dplyr::bind_rows(
    whep_read_file("n_balance_ygpit_all"),
    whep_read_file("livestock_prod_ygps")
  ) |>
    dplyr::group_by(Year, Province_name) |>
    dplyr::summarise(
      total_prod = sum(Prod_MgN, na.rm = TRUE),
      .groups = "drop"
    )

  system_nue <- total_outputs |>
    dplyr::left_join(n_soil_inputs, by = c("Year", "Province_name")) |>
    dplyr::mutate(
      inputs = deposition + fixation + synthetic + manure + urban,
      nue_system = total_prod / inputs * 100
    ) |>
    dplyr::select(Year, Province_name, total_prod, inputs, nue_system)

  system_nue
}
