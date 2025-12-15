## Historical extension of primary production (1850-1960)
##
## This file mirrors the last section of `Global/R/crop_liv_prod.r`.
## It extends the FAOSTAT-period primary production series backwards using
## LUH2 land areas as a proxy and adds international historical yields.

#' Extend primary production to historical period
#'
#' @description
#' Extends FAOSTAT-period primary production data (typically starting in 1961)
#' back to 1850 using LUH2 land areas as proxy variables and (optionally)
#' international historical yield estimates.
#'
#' @param production_data Tibble with processed production data.
#'   It must contain at least: `year`, `area`, `area_code`, `item_prod`,
#'   `item_prod_code`, `item_cbs_code`, `live_anim_code`, `unit`, `value`.
#' @param stock_area_full Tibble with LUH2 land areas in long format.
#'   It must contain at least: `Year`, `area`, `area_code`, `Land_Use`,
#'   `Area_Mha`.
#' @param international_yields Optional tibble with international historical
#'   yields. It must contain at least: `Year`, `area`, `item_code_prod`,
#'   `yield`.
#' @param start_year First year to extend to (default 1850).
#' @param faostat_start_year First year of FAOSTAT data (default 1961).
#' @param cropland_varnames LUH2 variable names considered cropland.
#' @param pasture_varnames LUH2 variable names considered pasture.
#'
#' @return A tibble with production data extended to the historical period.
#' @export
#'
#' @examples
#' \dontrun{
#'   extended <- extend_production_historical(
#'     production_data,
#'     stock_area_full,
#'     international_yields
#'   )
#' }
extend_production_historical <- function(
  production_data,
  stock_area_full,
  international_yields = NULL,
  start_year = 1850,
  faostat_start_year = 1961,
  cropland_varnames = c("c3ann", "c3per", "c4ann", "c4per", "c3nfx"),
  pasture_varnames = c("pastr", "range")
) {
  land_areas_wide <- .build_land_areas_wide(
    stock_area_full = stock_area_full,
    cropland_varnames = cropland_varnames,
    pasture_varnames = pasture_varnames
  )

  grassland <- .build_grassland_from_luh2(
    stock_area_full = stock_area_full,
    pasture_varnames = pasture_varnames
  )

  extended <- production_data |>
    dplyr::mutate(
      land_use = dplyr::if_else(
        unit %in% c("ha", "t_ha") |
          (unit == "tonnes" & is.na(live_anim_code)),
        "Cropland",
        "Agriland"
      )
    ) |>
    dplyr::select(
      year,
      area,
      area_code,
      item_prod,
      item_prod_code,
      item_cbs_code,
      land_use,
      unit,
      live_anim_code,
      value
    ) |>
    .extend_pre_faostat_years(
      start_year = start_year,
      faostat_start_year = faostat_start_year,
      land_areas_wide = land_areas_wide
    ) |>
    dplyr::bind_rows(grassland) |>
    dplyr::filter(
      !is.na(area),
      area != "",
      !is.na(unit)
    )

  if (!is.null(international_yields)) {
    extended <- .apply_international_yields(
      data = extended,
      international_yields = international_yields,
      faostat_start_year = faostat_start_year
    )
  }

  extended |>
    filter_country_temporal_validity() |>
    dplyr::filter(!is.na(value), value != 0)
}

#' Prepare land areas from LUH2 dataset
#'
#' @description
#' Aggregates LUH2 land use categories to cropland, pasture and agriland.
#' This reproduces the `Land_areas_wide` object built in
#' `Global/R/crop_liv_prod.r`.
#'
#' @param stock_area_full Tibble with LUH2 land use categories.
#' @param cropland_varnames Character vector of LUH2 cropland variable names.
#' @param pasture_varnames Character vector of LUH2 pasture variable names.
#'
#' @return A tibble with `year`, `area`, `area_code`, `Cropland`, `Pasture`,
#'   and `Agriland` in hectares.
#' @export
#'
#' @examples
#' \dontrun{
#'   land_use <- prepare_luh2_land_use(
#'     stock_area_full,
#'     cropland_varnames = c("c3ann", "c3per", "c4ann", "c4per", "c3nfx"),
#'     pasture_varnames = c("pastr", "range")
#'   )
#' }
prepare_luh2_land_use <- function(
  stock_area_full,
  cropland_varnames,
  pasture_varnames
) {
  .build_land_areas_wide(
    stock_area_full = stock_area_full,
    cropland_varnames = cropland_varnames,
    pasture_varnames = pasture_varnames
  )
}

#' Add grassland areas from LUH2 pasture data
#'
#' @description
#' Creates grassland area entries from LUH2 pasture categories.
#' This reproduces the `grassland` object built in `Global/R/crop_liv_prod.r`.
#'
#' @param stock_area_full Tibble with LUH2 land use categories.
#' @param pasture_varnames Character vector of LUH2 pasture variable names.
#' @param grassland_cbs_code CBS code for grassland (default 3000).
#'
#' @return A tibble with grassland production entries.
#' @export
#'
#' @examples
#' \dontrun{
#'   grassland <- create_grassland_production(stock_area_full)
#' }
create_grassland_production <- function(
  stock_area_full,
  pasture_varnames = c("pastr", "range"),
  grassland_cbs_code = 3000
) {
  .build_grassland_from_luh2(
    stock_area_full = stock_area_full,
    pasture_varnames = pasture_varnames,
    grassland_cbs_code = grassland_cbs_code
  )
}

## Internal helpers ---------------------------------------------------------

.build_land_areas_wide <- function(
  stock_area_full,
  cropland_varnames,
  pasture_varnames
) {
  stock_area_full |>
    dplyr::mutate(
      land_use = dplyr::if_else(
        Land_Use %in% cropland_varnames,
        "Cropland",
        dplyr::if_else(Land_Use %in% pasture_varnames, "Pasture", "Other")
      )
    ) |>
    dplyr::filter(land_use != "Other") |>
    dplyr::summarise(
      Area_Mha = sum(Area_Mha, na.rm = TRUE),
      .by = c(Year, area, area_code, land_use)
    ) |>
    dplyr::filter(!is.na(area)) |>
    tidyr::pivot_wider(names_from = land_use, values_from = Area_Mha) |>
    dplyr::mutate(
      Cropland = dplyr::coalesce(Cropland, 0) * 1e6,
      Pasture = dplyr::coalesce(Pasture, 0) * 1e6,
      Agriland = Cropland + Pasture,
      year = Year
    ) |>
    dplyr::select(year, area, area_code, Cropland, Pasture, Agriland)
}

.build_grassland_from_luh2 <- function(
  stock_area_full,
  pasture_varnames,
  grassland_cbs_code = 3000
) {
  stock_area_full |>
    dplyr::filter(Land_Use %in% pasture_varnames) |>
    dplyr::mutate(
      item_prod = dplyr::if_else(Land_Use == "pastr", "Pasture", "range"),
      item_prod_code = dplyr::if_else(Land_Use == "pastr", 3001, 3002),
      item_cbs_code = grassland_cbs_code,
      live_anim_code = NA_real_,
      unit = "ha"
    ) |>
    dplyr::summarize(
      value = sum(Area_Mha, na.rm = TRUE) * 1e6,
      .by = c(
        Year,
        area,
        area_code,
        item_prod,
        item_prod_code,
        item_cbs_code,
        live_anim_code,
        unit
      )
    ) |>
    dplyr::rename(year = Year)
}

.extend_pre_faostat_years <- function(
  data,
  start_year,
  faostat_start_year,
  land_areas_wide
) {
  years_tbl <- tibble::tibble(year = seq(start_year, max(data$year)))

  data <- data |>
    dplyr::full_join(years_tbl, by = "year")

  dplyr::bind_rows(
    data |>
      dplyr::filter(year < faostat_start_year) |>
      tidyr::complete(
        year,
        tidyr::nesting(
          area,
          area_code,
          item_prod,
          item_prod_code,
          item_cbs_code,
          land_use,
          unit,
          live_anim_code
        )
      ) |>
      dplyr::left_join(land_areas_wide, by = c("year", "area", "area_code")) |>
      dplyr::mutate(
        value_cropland = value,
        value_agriland = value,
        value_livestock_yield = value
      ) |>
      proxy_fill(
        value_cropland,
        Cropland,
        year,
        .by = c("area", "area_code", "item_prod", "land_use", "unit")
      ) |>
      proxy_fill(
        value_agriland,
        Agriland,
        year,
        .by = c("area", "area_code", "item_prod", "land_use", "unit")
      ) |>
      linear_fill(
        value_livestock_yield,
        year,
        .by = c("area", "area_code", "item_prod", "land_use", "unit")
      ) |>
      dplyr::mutate(
        value = dplyr::if_else(
          land_use == "Cropland",
          value_cropland,
          dplyr::if_else(
            unit %in% c("t_head", "t_LU"),
            value_livestock_yield,
            value_agriland
          )
        )
      ) |>
      dplyr::select(
        year,
        area,
        area_code,
        item_prod,
        item_prod_code,
        item_cbs_code,
        land_use,
        unit,
        live_anim_code,
        value
      ),
    data |>
      dplyr::filter(year >= faostat_start_year)
  )
}

.apply_international_yields <- function(
  data,
  international_yields,
  faostat_start_year
) {
  yields_std <- international_yields |>
    dplyr::rename(
      year = Year,
      item_prod_code = item_code_prod
    ) |>
    dplyr::select(year, area_code, item_prod_code, yield)

  wide <- data |>
    tidyr::pivot_wider(names_from = unit, values_from = value)

  wide <- wide |>
    dplyr::left_join(
      yields_std,
      by = c("year", "area_code", "item_prod_code")
    ) |>
    dplyr::mutate(
      t_ha_raw = tonnes / ha,
      t_ha = dplyr::if_else(year < faostat_start_year, NA_real_, t_ha_raw)
    ) |>
    proxy_fill(t_ha, yield, year, .by = c("area", "item_prod")) |>
    dplyr::mutate(
      t_ha = dplyr::if_else(!is.na(t_ha), t_ha, t_ha_raw),
      tonnes = dplyr::if_else(!is.na(ha), ha * t_ha, tonnes)
    )

  wide |>
    dplyr::select(
      year,
      area,
      area_code,
      item_prod,
      item_prod_code,
      item_cbs_code,
      live_anim_code,
      ha,
      LU,
      heads,
      tonnes,
      t_ha,
      t_LU,
      t_head
    ) |>
    tidyr::pivot_longer(
      ha:t_head,
      names_to = "unit",
      values_to = "value"
    ) |>
    dplyr::filter(!is.na(value), value != 0) |>
    dplyr::distinct() |>
    dplyr::summarize(
      value = mean(value, na.rm = TRUE),
      .by = c(
        year,
        area,
        area_code,
        item_prod,
        item_prod_code,
        item_cbs_code,
        live_anim_code,
        unit
      )
    )
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
