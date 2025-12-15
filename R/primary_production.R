# Functions to extend primary production data to historical periods (1850-1961)
# Uses proxy variables (land use data) and historical yield estimates

#' Extend primary production to historical period
#'
#' @description
#' Extends FAOSTAT production data (typically starting in 1961) back to 1850
#' using land use data as a proxy for area changes and historical yield
#' estimates.
#'
#' @param production_data Tibble with processed FAOSTAT production data from
#'   `process_faostat_production()`.
#' @param land_use_data Tibble with historical cropland and pasture areas
#'   (e.g., from LUH2 dataset).
#' @param historical_yields Optional tibble with historical yield estimates.
#' @param start_year First year to extend to (default 1850).
#' @param faostat_start_year First year of FAOSTAT data (default 1961).
#'
#' @return A tibble with production data extended to the historical period.
#' @export
#'
#' @examples
#' \dontrun{
#'   extended <- extend_production_historical(
#'     production_data,
#'     land_use_data,
#'     historical_yields
#'   )
#' }
extend_production_historical <- function(
  production_data,
  land_use_data,
  historical_yields = NULL,
  start_year = 1850,
  faostat_start_year = 1961
) {
  # Classify items by land use type
  classified_data <- production_data |>
    dplyr::mutate(
      land_use_type = dplyr::case_when(
        unit %in% c("ha", "t_ha") ~ "cropland",
        unit == "tonnes" & is.na(live_anim_code) ~ "cropland",
        unit %in% c("heads", "LU", "t_head", "t_LU") ~ "pasture",
        unit == "tonnes" & !is.na(live_anim_code) ~ "pasture",
        TRUE ~ NA_character_
      )
    )

  # Create full year sequence
  years <- tibble::tibble(year = seq(start_year, max(production_data$year)))

  # Extend data
  extended <- classified_data |>
    dplyr::full_join(years, by = "year") |>
    tidyr::complete(
      year,
      tidyr::nesting(
        area, area_code, item_prod, item_prod_code,
        item_cbs_code, live_anim_code, unit, land_use_type
      )
    )

  # Join land use data
  extended <- extended |>
    dplyr::left_join(
      land_use_data |>
        dplyr::select(year, area_code, cropland, pasture, agriland),
      by = c("year", "area_code")
    )

  # Apply proxy-based filling for historical period
  extended <- .fill_historical_values(
    extended,
    faostat_start_year
  )

  # Apply historical yields if provided
  if (!is.null(historical_yields)) {
    extended <- .apply_historical_yields(extended, historical_yields)
  }

  extended |>
    dplyr::filter(!is.na(value), value != 0) |>
    dplyr::select(
      year, area, area_code, item_prod, item_prod_code,
      item_cbs_code, live_anim_code, unit, value
    )
}

#' Prepare land use data from LUH2 dataset
#'
#' @description
#' Processes LUH2 (Land Use Harmonization) data to create cropland and
#' pasture area time series for use as proxy variables.
#'
#' @param luh2_data Tibble with LUH2 land use categories.
#' @param cropland_categories Character vector of LUH2 cropland variable names.
#' @param pasture_categories Character vector of LUH2 pasture variable names.
#'
#' @return A tibble with year, area_code, cropland, pasture, and agriland
#'   (total agricultural land) in hectares.
#' @export
#'
#' @examples
#' \dontrun{
#'   land_use <- prepare_luh2_land_use(
#'     luh2_data,
#'     cropland_categories = c("c3ann", "c4ann", "c3per", "c4per", "c3nfx"),
#'     pasture_categories = c("pastr", "range")
#'   )
#' }
prepare_luh2_land_use <- function(
  luh2_data,
  cropland_categories,
  pasture_categories
) {
  luh2_data |>
    dplyr::mutate(
      land_use_type = dplyr::case_when(
        land_use %in% cropland_categories ~ "cropland",
        land_use %in% pasture_categories ~ "pasture",
        TRUE ~ "other"
      )
    ) |>
    dplyr::filter(land_use_type != "other") |>
    dplyr::summarize(
      area_mha = sum(area_mha, na.rm = TRUE),
      .by = c(year, area, area_code, land_use_type)
    ) |>
    tidyr::pivot_wider(
      names_from = land_use_type,
      values_from = area_mha
    ) |>
    dplyr::mutate(
      cropland = dplyr::coalesce(cropland, 0) * 1e6,
      pasture = dplyr::coalesce(pasture, 0) * 1e6,
      agriland = cropland + pasture
    )
}

#' Add grassland production from LUH2 pasture data
#'
#' @description
#' Creates grassland area entries from LUH2 pasture data categories.
#'
#' @param land_use_data Tibble from `prepare_luh2_land_use()`.
#' @param pasture_item_code Item code for managed pasture (default 3001).
#' @param range_item_code Item code for rangeland (default 3002).
#' @param grassland_cbs_code CBS code for grassland (default 3000).
#'
#' @return A tibble with grassland production entries.
#' @export
#'
#' @examples
#' \dontrun{
#'   grassland <- create_grassland_production(land_use_data)
#' }
create_grassland_production <- function(
  land_use_data,
  pasture_item_code = 3001,
  range_item_code = 3002,
  grassland_cbs_code = 3000
) {
  # This would need the detailed LUH2 data with pastr/range separation

  # For now, return pasture as a single category

  land_use_data |>
    dplyr::filter(pasture > 0) |>
    dplyr::transmute(
      year,
      area,
      area_code,
      item_prod = "Grassland",
      item_prod_code = pasture_item_code,
      item_cbs = "Grassland",
      item_cbs_code = grassland_cbs_code,
      unit = "ha",
      value = pasture
    )
}

#' Fill historical values using proxy variables
#'
#' @description
#' Fills pre-FAOSTAT values using land use data as a proxy.
#' Crop areas and yields are indexed to cropland; livestock parameters
#' are indexed to total agricultural land.
#'
#' @param data Tibble with production data and land use columns.
#' @param faostat_start_year First year of FAOSTAT data.
#'
#' @return Tibble with filled historical values.
#'
#' @keywords internal
.fill_historical_values <- function(data, faostat_start_year) {
  # Split into historical and FAOSTAT periods
  historical <- data |>
    dplyr::filter(year < faostat_start_year)

  faostat <- data |>
    dplyr::filter(year >= faostat_start_year)

  # Apply proxy filling to historical data
  historical_filled <- historical |>
    dplyr::mutate(
      proxy_var = dplyr::case_when(
        land_use_type == "cropland" ~ cropland,
        land_use_type == "pasture" & unit %in% c("t_head", "t_LU") ~ NA_real_,
        land_use_type == "pasture" ~ agriland,
        TRUE ~ NA_real_
      )
    ) |>
    dplyr::group_by(area, area_code, item_prod, unit) |>
    proxy_fill(
      var = value,
      proxy = proxy_var,
      time_index = year
    ) |>
    dplyr::ungroup()

  # For livestock yields, use linear fill (constant backwards)
  historical_filled <- historical_filled |>
    dplyr::group_by(area, area_code, item_prod, unit) |>
    linear_fill(
      var = value,
      time_index = year,
      interpolate = FALSE,
      fill_forward = FALSE,
      fill_backward = TRUE
    ) |>
    dplyr::ungroup()

  # Combine
dplyr::bind_rows(historical_filled, faostat)
}

#' Apply historical yield estimates
#'
#' @description
#' Adjusts historical production values using independent yield estimates.
#' Production is recalculated as area * yield when historical yields are
#' available.
#'
#' @param data Tibble with extended production data.
#' @param historical_yields Tibble with year, area_code, item_prod_code, yield.
#'
#' @return Tibble with adjusted historical production.
#'
#' @keywords internal
.apply_historical_yields <- function(data, historical_yields) {
  # Pivot to wide format for calculation
  wide_data <- data |>
    tidyr::pivot_wider(
      names_from = unit,
      values_from = value
    )

  # Join historical yields
  wide_data <- wide_data |>
    dplyr::left_join(
      historical_yields |>
        dplyr::select(year, area_code, item_prod_code, hist_yield = yield),
      by = c("year", "area_code", "item_prod_code")
    )

  # Apply historical yields with proxy fill
  wide_data <- wide_data |>
    dplyr::group_by(area, item_prod) |>
    proxy_fill(
      var = t_ha,
      proxy = hist_yield,
      time_index = year
    ) |>
    dplyr::ungroup()

  # Recalculate production
  wide_data <- wide_data |>
    dplyr::mutate(
      t_ha = dplyr::coalesce(t_ha, ha / tonnes),
      tonnes = dplyr::if_else(!is.na(ha), ha * t_ha, tonnes)
    )

  # Pivot back to long format
  wide_data |>
    tidyr::pivot_longer(
      cols = c(ha, tonnes, t_ha, LU, heads, t_LU, t_head),
      names_to = "unit",
      values_to = "value",
      values_drop_na = TRUE
    ) |>
    dplyr::filter(value != 0)
}

#' Filter data by country temporal validity
#'
#' @description
#' Removes data for countries in years when they did not exist
#' (e.g., Czechoslovakia after 1992, Czech Republic before 1993).
#'
#' @param data Tibble with production data.
#' @param country_changes Optional tibble with area_code, start_year, end_year.
#'   If NULL, uses built-in rules for common cases.
#'
#' @return Filtered tibble with temporally valid country data.
#' @export
#'
#' @examples
#' \dontrun{
#'   filtered <- filter_country_temporal_validity(production_data)
#' }
filter_country_temporal_validity <- function(data, country_changes = NULL) {
  # Default rules for common country changes
  if (is.null(country_changes)) {
    data |>
      dplyr::filter(
        !(area == "Czechoslovakia" & year > 1992),
        !(area %in% c("Czech Republic", "Czechia", "Slovakia") & year < 1993),
        !(area %in% c("Lithuania", "Latvia", "Estonia", "Slovenia", "Croatia") &
            year < 1992),
        !(area == "Belgium-Luxembourg" & year > 1999),
        !(area %in% c("Belgium", "Luxembourg") & year < 2000),
        !(area == "USSR" & year > 1991),
        !(area == "Yugoslavia" & year > 1991),
        !(area == "Serbia and Montenegro" & year > 2006),
        !(area == "Sudan (former)" & year > 2011)
      )
  } else {
    # Use provided country changes table
    data |>
      dplyr::left_join(country_changes, by = "area_code") |>
      dplyr::filter(
        is.na(start_year) | year >= start_year,
        is.na(end_year) | year <= end_year
      ) |>
      dplyr::select(-start_year, -end_year)
  }
}

#' Summarize primary production to final format
#'
#' @description
#' Prepares the final primary production dataset with all required columns
#' and proper formatting for WHEP package use.
#'
#' @param data Tibble with extended and processed production data.
#'
#' @return A tibble formatted for `get_primary_production()`.
#' @export
#'
#' @examples
#' \dontrun{
#'   final <- finalize_primary_production(extended_data)
#' }
finalize_primary_production <- function(data) {
  data |>
    dplyr::select(
      year,
      area_code,
      item_prod_code,
      item_cbs_code,
      live_anim_code,
      unit,
      value
    ) |>
    dplyr::filter(
      !is.na(value),
      !is.infinite(value),
      value != 0
    ) |>
    dplyr::distinct() |>
    dplyr::summarize(
      value = mean(value, na.rm = TRUE),
      .by = c(
        year, area_code, item_prod_code, item_cbs_code,
        live_anim_code, unit
      )
    ) |>
    dplyr::arrange(year, area_code, item_prod_code, unit)
}
