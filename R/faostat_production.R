## FAOSTAT production inputs
##
## These functions read raw FAOSTAT bulk-download tables that are made
## available via the WHEP pinned inputs system (see `whep_read_file()` and
## `whep_inputs`).

#' Extract production data from FAOSTAT bulk download
#'
#' @description
#' Reads and standardises the FAOSTAT Production_Crops_Livestock table.
#' This is a low-level helper to reproduce the pipeline in
#' `Global/R/crop_liv_prod.r`.
#'
#' @param version File version to use as input. See [whep_inputs] for details.
#' @param file_alias Input file alias. Defaults to the WHEP entry
#'   `"faostat-production"`.
#'
#' @return A tibble with standardized production data.
#' @export
#'
#' @examples
#' \dontrun{
#'   crop_liv <- extract_faostat_production()
#' }
extract_faostat_production <- function(
  version = NULL,
  file_alias = "faostat-production"
) {
  file_alias |>
    whep_read_file(version = version) |>
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
#' Reads and standardises a FAOSTAT commodity balance sheet table.
#' Use different values of `file_alias` to read FBS/CBS variants.
#'
#' @param version File version to use as input. See [whep_inputs] for details.
#' @param file_alias Input file alias, e.g. `"faostat-fbs-new"`,
#'   `"faostat-fbs-old"`, `"faostat-cbs-old-crops"`.
#'
#' @return A tibble with standardized commodity balance data.
#' @export
#'
#' @examples
#' \dontrun{
#'   fbs <- extract_faostat_commodity_balance(file_alias = "faostat-fbs-new")
#' }
extract_faostat_commodity_balance <- function(version = NULL, file_alias) {
  file_alias |>
    whep_read_file(version = version) |>
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
#' @param version File version to use as input. See [whep_inputs] for details.
#' @param file_alias Input file alias for the FAOSTAT Emissions_livestock
#'   table.
#'
#' @return A tibble with livestock stock data.
#' @export
#'
#' @examples
#' \dontrun{
#'   stocks <- extract_faostat_livestock_stocks(
#'     file_alias = "faostat-emissions-livestock"
#'   )
#' }
extract_faostat_livestock_stocks <- function(version = NULL, file_alias) {
  file_alias |>
    whep_read_file(version = version) |>
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
      ),
      unit = dplyr::if_else(unit == "t", "tonnes", unit)
    )

  # Filter relevant units and join item mappings
  result <- normalized |>
    dplyr::filter(unit %in% c("ha", "tonnes", "heads", "hg/ha")) |>
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
    dplyr::filter(unit %in% c("ha", "tonnes")) |>
    tidyr::pivot_wider(
      names_from = unit,
      values_from = value
    ) |>
    dplyr::mutate(
      yield = tonnes / ha,
      yield = dplyr::if_else(
        is.infinite(yield) | is.nan(yield) | yield == 0,
        NA_real_,
        yield
      )
    )

  # Pivot back to long format
  wide_data |>
    tidyr::pivot_longer(
      cols = c(tonnes, ha, yield),
      names_to = "unit",
      values_to = "value"
    ) |>
    dplyr::filter(!is.na(value), value != 0) |>
    dplyr::mutate(
      unit = dplyr::case_match(
        unit,
        "tonnes" ~ "tonnes",
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
      prod_cbs_ratio,
      year,
      .by = c("area_code", "item_prod_code")
    ) |>
    dplyr::mutate(value = dplyr::coalesce(value, value_cbs * prod_cbs_ratio)) |>
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

#' Build FAOSTAT-period primary production
#'
#' @description
#' Reproduces the FAOSTAT-period processing logic in
#' `Global/R/crop_liv_prod.r` up to the `Primary_all_raw2` object.
#'
#' This function is intended for rebuilding the pinned `primary_prod` input.
#' It requires several mapping tables that are not defined in this file.
#'
#' @param production_data Tibble returned by [extract_faostat_production()].
#' @param livestock_stocks Tibble returned by
#'   [extract_faostat_livestock_stocks()].
#' @param cbs_production Optional tibble returned by
#'   [combine_cbs_production()]. Used to fill production gaps.
#' @param animals_codes Tibble defining livestock disaggregation.
#'   It must contain at least: `item_code_cbs`, `item_cbs`, `Item_Code`,
#'   `Livestock_name`, `Animal_class`.
#' @param livestock_lu_coefs Tibble defining LU per head conversion.
#'   It must contain at least: `item_cbs` and `LU_head`.
#' @param items_prod_full Tibble mapping production items to CBS items and
#'   defining product groups and livestock associations.
#'   It must contain at least: `item_code_prod`, `item_code_cbs`, `group`,
#'   `Live_anim`, `Live_anim_code`.
#' @param primary_double Tibble defining multi-output product handling.
#'   It must contain at least: `item_prod`, `item_code_prod`, `Multi_type`,
#'   `Item_area`.
#' @param items_primary Tibble mapping the final primary items.
#'   It must contain at least: `item_prod`, `item_code_prod`,
#'   `item_cbs`, `item_code_cbs`.
#'
#' @return A tibble with FAOSTAT-period primary production, including
#'   production, area/stocks and yields.
#' @export
#'
#' @examples
#' \dontrun{
#'   primary_raw2 <- build_primary_production_faostat_period(
#'     production_data,
#'     livestock_stocks,
#'     cbs_production,
#'     animals_codes,
#'     livestock_lu_coefs,
#'     items_prod_full,
#'     primary_double,
#'     items_primary
#'   )
#' }
build_primary_production_faostat_period <- function(
  production_data,
  livestock_stocks,
  cbs_production = NULL,
  animals_codes,
  livestock_lu_coefs,
  items_prod_full,
  primary_double,
  items_primary
) {
  crop_liv <- production_data |>
    dplyr::mutate(
      value = dplyr::if_else(unit == "t", value, value),
      unit = dplyr::if_else(unit == "t", "tonnes", unit)
    ) |>
    correct_tea_production()

  livestock <- .build_livestock_full_disagg(
    production_data = crop_liv,
    livestock_stocks = livestock_stocks,
    animals_codes = animals_codes,
    livestock_lu_coefs = livestock_lu_coefs
  )

  primary_all_raw <- dplyr::bind_rows(
    crop_liv |>
      dplyr::filter(element %in% c("Production", "Stocks")) |>
      dplyr::filter(unit %in% c("ha", "tonnes")) |>
      dplyr::select(
        year,
        area,
        area_code,
        item_prod,
        item_prod_code,
        unit,
        value
      ),
    livestock
  ) |>
    dplyr::summarize(
      value = sum(value, na.rm = TRUE),
      .by = c(year, area, area_code, item_prod, item_prod_code, unit)
    ) |>
    .add_game_assumptions()

  yield_all <- .build_and_fill_yields(
    primary_all_raw = primary_all_raw,
    items_prod_full = items_prod_full,
    primary_double = primary_double,
    cbs_production = cbs_production
  )

  .rebuild_primary_all_raw2(yield_all, items_primary)
}

.build_livestock_full_disagg <- function(
  production_data,
  livestock_stocks,
  animals_codes,
  livestock_lu_coefs
) {
  stocks_std <- livestock_stocks |>
    dplyr::rename(item_code_cbs = item_cbs_code)

  fao_liv_raw <- production_data |>
    dplyr::filter(element == "Stocks") |>
    dplyr::mutate(
      value = dplyr::if_else(unit == "1000 An", value * 1000, value),
      unit = dplyr::if_else(unit == "1000 An", "heads", unit)
    ) |>
    dplyr::select(year, area, area_code, item_prod_code, value) |>
    dplyr::inner_join(
      animals_codes |>
        dplyr::select(
          item_code_cbs,
          item_cbs,
          Item_Code,
          Livestock_name
        ) |>
        dplyr::rename(item_prod_code = Item_Code),
      by = "item_prod_code"
    ) |>
    dplyr::full_join(
      stocks_std |>
        dplyr::rename(value_st = value, item_st = item_cbs) |>
        dplyr::select(year, area, item_code_cbs, item_st, value_st),
      by = c("year", "area", "item_code_cbs")
    ) |>
    dplyr::mutate(
      share = value_st / sum(value_st, na.rm = TRUE),
      value_comb = dplyr::if_else(
        is.na(share) | share == 1,
        value,
        value * share
      ),
      .by = c(year, area, item_prod_code)
    ) |>
    dplyr::filter(!is.na(area_code)) |>
    dplyr::mutate(n = dplyr::n(), .by = c(area, item_cbs, item_code_cbs)) |>
    tidyr::complete(
      year,
      tidyr::nesting(area, area_code, item_cbs, item_code_cbs, Livestock_name)
    )

  fao_liv_raw <- fao_liv_raw |>
    linear_fill(n, year, .by = c("area", "item_cbs", "item_code_cbs")) |>
    dplyr::mutate(
      value = dplyr::if_else(
        !is.na(value_comb),
        value_comb,
        dplyr::if_else(n > 40, NA_real_, 0)
      ),
      .by = c(area, item_cbs, item_code_cbs)
    ) |>
    linear_fill(value, year, .by = c("area", "item_cbs", "item_code_cbs"))

  fao_liv_all <- fao_liv_raw |>
    dplyr::select(
      year,
      area,
      area_code,
      item_code_cbs,
      item_cbs,
      value,
      source_value
    ) |>
    dplyr::left_join(
      animals_codes |>
        dplyr::select(item_cbs, Animal_class),
      by = "item_cbs"
    ) |>
    dplyr::left_join(livestock_lu_coefs, by = "item_cbs") |>
    dplyr::mutate(
      value_lu = value * LU_head,
      element = "Stock"
    ) |>
    dplyr::transmute(
      year,
      area,
      area_code,
      item_prod = item_cbs,
      item_prod_code = item_code_cbs,
      unit = "LU",
      value = value_lu
    ) |>
    dplyr::bind_rows(
      fao_liv_raw |>
        dplyr::transmute(
          year,
          area,
          area_code,
          item_prod = item_cbs,
          item_prod_code = item_code_cbs,
          unit = "heads",
          value = value
        )
    )

  fao_liv_all
}

.add_game_assumptions <- function(primary_all_raw) {
  dplyr::bind_rows(
    primary_all_raw,
    primary_all_raw |>
      dplyr::filter(item_prod_code == 1163) |>
      dplyr::mutate(
        value = value * 3,
        unit = "LU",
        item_prod = "Game",
        item_prod_code = 1190
      ) |>
      dplyr::bind_rows(
        primary_all_raw |>
          dplyr::filter(item_prod_code == 1163) |>
          dplyr::mutate(
            value = value * 3 / 10,
            unit = "heads",
            item_prod = "Game",
            item_prod_code = 1190
          )
      )
  )
}

.build_and_fill_yields <- function(
  primary_all_raw,
  items_prod_full,
  primary_double,
  cbs_production
) {
  crop_yield <- primary_all_raw |>
    dplyr::filter(unit %in% c("ha", "tonnes")) |>
    tidyr::pivot_wider(names_from = unit, values_from = value) |>
    dplyr::mutate(
      Yield_c = tonnes / ha,
      unit = "t_ha",
      FU = ha,
      t = tonnes
    ) |>
    dplyr::select(year, area, area_code, item_prod, item_prod_code,
      unit, Yield_c, FU, t)

  livestock_yield <- primary_all_raw |>
    dplyr::filter(unit == "tonnes") |>
    dplyr::select(-unit) |>
    dplyr::right_join(
      items_prod_full |>
        dplyr::filter(!is.na(Live_anim)) |>
        dplyr::select(Live_anim, Live_anim_code, item_code_prod),
      by = c("item_prod_code" = "item_code_prod")
    ) |>
    dplyr::rename(t = value) |>
    tidyr::complete(year, tidyr::nesting(area, area_code, item_prod,
      item_prod_code, Live_anim, Live_anim_code)) |>
    dplyr::full_join(
      dplyr::bind_rows(
        primary_all_raw |>
          dplyr::filter(unit == "LU") |>
          dplyr::rename(Live_anim_code = item_prod_code) |>
          dplyr::select(year, area, area_code, Live_anim_code, unit, value),
        primary_all_raw |>
          dplyr::filter(unit == "heads") |>
          dplyr::rename(Live_anim_code = item_prod_code) |>
          dplyr::select(year, area, area_code, Live_anim_code, unit, value)
      ),
      by = c("year", "area", "area_code", "Live_anim_code")
    ) |>
    dplyr::rename(FU = value) |>
    dplyr::mutate(
      Yield_c = t / FU,
      unit = dplyr::if_else(unit == "LU", "t_LU", "t_head")
    ) |>
    dplyr::select(year, area, area_code, item_prod, item_prod_code,
      Live_anim, Live_anim_code, unit, Yield_c, FU, t)

  yield_all_raw <- dplyr::bind_rows(crop_yield, livestock_yield) |>
    dplyr::mutate(
      t = dplyr::na_if(t, 0),
      FU = dplyr::na_if(FU, 0),
      Yield_c = dplyr::if_else(
        Yield_c == 0 | is.infinite(Yield_c) | is.nan(Yield_c),
        NA_real_,
        Yield_c
      )
    ) |>
    tidyr::complete(
      year,
      tidyr::nesting(area, area_code, item_prod, item_prod_code,
        Live_anim, Live_anim_code, unit)
    )

  yield_all <- yield_all_raw |>
    dplyr::left_join(primary_double |>
      dplyr::select(item_prod, Multi_type), by = "item_prod") |>
    dplyr::group_by(area, area_code, item_prod, item_prod_code, unit) |>
    linear_fill(Yield_c, year) |>
    dplyr::ungroup() |>
    .add_global_yields() |>
    dplyr::left_join(
      items_prod_full |>
        dplyr::select(item_code_prod, item_code_cbs, group),
      by = c("item_prod_code" = "item_code_prod")
    )

  if (!is.null(cbs_production)) {
    yield_all <- yield_all |>
      dplyr::left_join(
        cbs_production |>
          dplyr::rename(t_cbs = value) |>
          dplyr::select(year, area_code, item_cbs_code, t_cbs),
        by = c("year", "area_code", "item_code_cbs" = "item_cbs_code")
      ) |>
      dplyr::mutate(
        prod_cbs_ratio = t / t_cbs,
        sumprod_cbs_ratio = sum(t, na.rm = TRUE) / t_cbs,
        .by = c(year, area, area_code, item_code_cbs)
      ) |>
      linear_fill(
        prod_cbs_ratio,
        year,
        .by = c("area_code", "item_prod_code", "Live_anim_code", "group")
      )
  }

  yield_all
}

.add_global_yields <- function(yield_all) {
  global_yields <- yield_all |>
    dplyr::summarize(
      t = sum(t, na.rm = TRUE),
      FU = sum(FU, na.rm = TRUE),
      .by = c(year, item_prod_code, Live_anim_code, unit)
    ) |>
    dplyr::mutate(
      Yield_glo = t / FU,
      Yield_glo = dplyr::if_else(
        Yield_glo == 0 | is.infinite(Yield_glo) | is.nan(Yield_glo),
        NA_real_,
        Yield_glo
      )
    ) |>
    linear_fill(Yield_glo, year, .by = c("item_prod_code", "Live_anim_code",
      "unit")) |>
    dplyr::select(year, item_prod_code, Live_anim_code, unit, Yield_glo)

  yield_all |>
    dplyr::left_join(
      global_yields,
      by = c("year", "item_prod_code", "Live_anim_code", "unit")
    ) |>
    dplyr::mutate(Yield = dplyr::coalesce(Yield_c, Yield_glo))
}

.rebuild_primary_all_raw2 <- function(yield_all, items_primary) {
  out <- dplyr::bind_rows(
    yield_all |>
      dplyr::filter(unit == "t_ha") |>
      dplyr::mutate(unit = "ha") |>
      dplyr::transmute(
        year,
        area,
        area_code,
        item_prod_code,
        live_anim_code = Live_anim_code,
        unit,
        value = FU
      ),
    yield_all |>
      dplyr::filter(unit %in% c("t_ha", "t_head")) |>
      dplyr::mutate(unit = "tonnes") |>
      dplyr::transmute(
        year,
        area,
        area_code,
        item_prod_code,
        live_anim_code = Live_anim_code,
        unit,
        value = t
      ),
    yield_all |>
      dplyr::transmute(
        year,
        area,
        area_code,
        item_prod_code,
        live_anim_code = Live_anim_code,
        unit,
        value = Yield
      )
  ) |>
    dplyr::left_join(
      items_primary |>
        dplyr::select(item_prod, item_code_prod, item_cbs, item_code_cbs),
      by = c("item_prod_code" = "item_code_prod")
    ) |>
    dplyr::filter(
      !is.na(value),
      !is.infinite(value),
      value != 0
    )

  out
}
