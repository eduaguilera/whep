# Functions to extract and process FAOSTAT production data
# These functions read from bulk downloaded FAOSTAT CSV files

#' Extract production data from FAOSTAT bulk download
#'
#' @description
#' Reads and processes the FAOSTAT Production_Crops_Livestock file,
#' standardizing column names and filtering valid areas.
#'
#' @param file_path Path to the FAOSTAT normalized CSV file.
#'
#' @return A tibble with standardized production data.
#' @export
#'
#' @examples
#' \dontrun{
#'   crop_liv <- extract_faostat_production(
#'     "Production_Crops_Livestock_E_All_Data_(Normalized).csv"
#'   )
#' }
extract_faostat_production <- function(file_path) {
  data.table::fread(file_path, check.names = TRUE) |>
    tibble::as_tibble() |>
    dplyr::rename(
      item_prod_code = Item.Code,
      item_prod = Item,
      area_code = Area.Code,
      area = Area,
      year = Year,
      element = Element,
      unit = Unit,
      value = Value
    ) |>
    .filter_valid_areas() |>
    dplyr::arrange(year, area, area_code, item_prod_code, item_prod, element)
}

#' Extract commodity balance data from FAOSTAT bulk download
#'
#' @description
#' Reads and processes FAOSTAT commodity balance sheet files
#' (FBS, CBS crops, CBS animals), standardizing column names.
#'
#' @param file_path Path to the FAOSTAT normalized CSV file.
#'
#' @return A tibble with standardized commodity balance data.
#' @export
#'
#' @examples
#' \dontrun{
#'   fbs <- extract_faostat_commodity_balance(
#'     "FoodBalanceSheets_E_All_Data_(Normalized).csv"
#'   )
#' }
extract_faostat_commodity_balance <- function(file_path) {
  data.table::fread(file_path, check.names = TRUE) |>
    tibble::as_tibble() |>
    dplyr::rename(
      item_cbs_code = Item.Code,
      item_cbs = Item,
      area_code = Area.Code,
      area = Area,
      year = Year,
      element = Element,
      unit = Unit,
      value = Value
    ) |>
    .filter_valid_areas()
}

#' Extract livestock stocks from FAOSTAT emissions dataset
#'
#' @description
#' Reads and processes the FAOSTAT Emissions_livestock file to extract
#' animal stock data using FAO TIER 1 source.
#'
#' @param file_path Path to the FAOSTAT normalized CSV file.
#'
#' @return A tibble with livestock stock data.
#' @export
#'
#' @examples
#' \dontrun{
#'   stocks <- extract_faostat_livestock_stocks(
#'     "Emissions_livestock_E_All_Data_(Normalized).csv"
#'   )
#' }
extract_faostat_livestock_stocks <- function(file_path) {
  data.table::fread(file_path, check.names = TRUE) |>
    tibble::as_tibble() |>
    dplyr::filter(
      Element == "Stocks",
      Source == "FAO TIER 1"
    ) |>
    dplyr::rename(
      item_cbs_code = Item.Code,
      item_cbs = Item,
      area_code = Area.Code,
      area = Area,
      year = Year,
      element = Element,
      unit = Unit,
      value = Value
    ) |>
    .filter_valid_areas() |>
    dplyr::arrange(year, area, area_code, item_cbs, item_cbs_code, element)
}

#' Combine commodity balance production data
#'
#' @description
#' Combines production data from multiple commodity balance sources
#' (FBS new, FBS old, CBS crops, CBS animals) and summarizes by item.
#'
#' @param fbs_new Tibble from new Food Balance Sheets.
#' @param fbs_old Tibble from historic Food Balance Sheets.
#' @param cbs_crops Tibble from Commodity Balances Crops.
#' @param cbs_animals Tibble from Commodity Balances Livestock/Fish.
#'
#' @return A tibble with combined production data from all CBS sources.
#' @export
#'
#' @examples
#' \dontrun{
#'   cbs_prod <- combine_cbs_production(fbs_new, fbs_old, cbs_crops, cbs_animals)
#' }
combine_cbs_production <- function(fbs_new, fbs_old, cbs_crops, cbs_animals) {
  dplyr::bind_rows(fbs_new, fbs_old, cbs_animals, cbs_crops) |>
    dplyr::filter(element == "Production") |>
    dplyr::summarize(
      value = mean(value, na.rm = TRUE),
      .by = c(area, area_code, year, item_cbs, item_cbs_code, element)
    ) |>
    dplyr::arrange(year, area, area_code, item_cbs, item_cbs_code, element)
}

#' Process crop and livestock production data
#'
#' @description
#' Main function to process FAOSTAT production data into a standardized format.
#' Normalizes units, fills gaps using linear interpolation, and calculates
#' yields.
#'
#' @param production_data Tibble from `extract_faostat_production()`.
#' @param items_mapping Tibble with item code mappings (item_prod_code to
#'   item_cbs_code).
#' @param cbs_production Optional tibble from `combine_cbs_production()` to
#'   fill gaps.
#'
#' @return A tibble with processed production data including filled gaps.
#' @export
#'
#' @examples
#' \dontrun{
#'   processed <- process_faostat_production(
#'     production_data,
#'     items_mapping,
#'     cbs_production
#'   )
#' }
process_faostat_production <- function(
  production_data,
  items_mapping,
  cbs_production = NULL
) {
  # Normalize units (convert 1000 An to heads)
  normalized <- production_data |>
    dplyr::mutate(
      value = dplyr::if_else(
        unit == "1000 An",
        value * 1000,
        value
      ),
      unit = dplyr::if_else(
        unit == "1000 An",
        "heads",
        unit
      )
    )

  # Filter relevant units and join item mappings
  result <- normalized |>
    dplyr::filter(unit %in% c("ha", "t", "heads", "hg/ha")) |>
    dplyr::left_join(
      items_mapping |> dplyr::select(item_prod_code, item_cbs_code),
      by = "item_prod_code"
    )

  # Calculate yields
  result <- .calculate_yields(result)

  # Fill gaps using CBS production if provided

  if (!is.null(cbs_production)) {
    result <- .fill_production_gaps(result, cbs_production)
  }

  result
}

#' Process livestock data with Livestock Unit conversion
#'
#' @description
#' Processes livestock stock data, converting heads to Livestock Units (LU)
#' using standard conversion factors.
#'
#' @param stock_data Tibble from `extract_faostat_livestock_stocks()`.
#' @param lu_coefficients Tibble with `item_cbs_code` and `lu_per_head`
#'   conversion factors.
#'
#' @return A tibble with livestock data in both heads and LU units.
#' @export
#'
#' @examples
#' \dontrun{
#'   livestock <- process_livestock_stocks(stock_data, lu_coefficients)
#' }
process_livestock_stocks <- function(stock_data, lu_coefficients) {
  stock_data |>
    dplyr::left_join(lu_coefficients, by = "item_cbs_code") |>
    dplyr::mutate(
      value_lu = value * lu_per_head,
      lu_per_head = NULL
    ) |>
    tidyr::pivot_longer(
      cols = c(value, value_lu),
      names_to = "unit_type",
      values_to = "value"
    ) |>
    dplyr::mutate(
      unit = dplyr::if_else(unit_type == "value", "heads", "LU"),
      unit_type = NULL
    )
}

#' Calculate yields from area and production data
#'
#' @description
#' Calculates crop yields (tonnes/ha) and livestock product yields
#' (tonnes/head or tonnes/LU) from production and area/stock data.
#'
#' @param data Tibble with production data in long format.
#'
#' @return A tibble with yield calculations added.
#'
#' @keywords internal
.calculate_yields <- function(data) {
  # Pivot to wide format for yield calculation
  wide_data <- data |>
    dplyr::filter(unit %in% c("ha", "t")) |>
    tidyr::pivot_wider(
      names_from = unit,
      values_from = value
    ) |>
    dplyr::mutate(
      yield = t / ha,
      yield = dplyr::if_else(
        is.infinite(yield) | is.nan(yield) | yield == 0,
        NA_real_,
        yield
      )
    )

  # Pivot back to long format
  wide_data |>
    tidyr::pivot_longer(
      cols = c(t, ha, yield),
      names_to = "unit",
      values_to = "value"
    ) |>
    dplyr::filter(!is.na(value), value != 0) |>
    dplyr::mutate(
      unit = dplyr::case_match(
        unit,
        "t" ~ "tonnes",
        "ha" ~ "ha",
        "yield" ~ "t_ha"
      )
    )
}

#' Fill production gaps using CBS data
#'
#' @description
#' Uses commodity balance sheet production data to fill gaps in the main
#' production dataset, applying linear interpolation.
#'
#' @param production_data Tibble with production data.
#' @param cbs_production Tibble with CBS production data.
#'
#' @return A tibble with filled production values.
#'
#' @keywords internal
.fill_production_gaps <- function(production_data, cbs_production) {
  # Join CBS production data
  result <- production_data |>
    dplyr::left_join(
      cbs_production |>
        dplyr::select(year, area_code, item_cbs_code, value_cbs = value),
      by = c("year", "area_code", "item_cbs_code")
    )

  # Calculate production/CBS ratio for gap filling
  result <- result |>
    dplyr::mutate(
      prod_cbs_ratio = value / value_cbs,
      .by = c(year, area_code, item_cbs_code)
    )

  # Fill ratio gaps using linear interpolation
  result |>
    linear_fill(
      var = prod_cbs_ratio,
      time_index = year,
      .by = c("area_code", "item_prod_code")
    ) |>
    dplyr::mutate(
      value = dplyr::coalesce(value, value_cbs * prod_cbs_ratio)
    ) |>
    dplyr::select(-value_cbs, -prod_cbs_ratio, -source_prod_cbs_ratio)
}

#' Filter valid geographic areas
#'
#' @description
#' Removes aggregated regions and keeps only country-level data.
#' Area codes above 5000 are typically aggregates in FAOSTAT.
#'
#' @param data Tibble with area_code column.
#'
#' @return Filtered tibble with only valid country areas.
#'
#' @keywords internal
.filter_valid_areas <- function(data) {
  data |>
    dplyr::filter(
      area_code < 5000,
      !is.na(area_code)
    )
}

#' Apply tea production correction
#'
#' @description
#' Corrects tea production values after 1990, when FAOSTAT changed
#' from dry weight to fresh matter reporting (factor ~4.37).
#'
#' @param data Tibble with production data.
#' @param tea_item_code The item_prod_code for tea (default 667).
#' @param correction_year Year from which to apply correction (default 1990).
#' @param correction_factor Conversion factor (default 4.37).
#'
#' @return Tibble with corrected tea values.
#' @export
#'
#' @examples
#' \dontrun{
#'   corrected <- correct_tea_production(production_data)
#' }
correct_tea_production <- function(
  data,
  tea_item_code = 667,
  correction_year = 1990,
  correction_factor = 4.37
) {
  data |>
    dplyr::mutate(
      value = dplyr::if_else(
        unit == "tonnes" &
          item_prod_code == tea_item_code &
          year > correction_year,
        value / correction_factor,
        value
      )
    )
}
