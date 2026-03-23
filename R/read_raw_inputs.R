# Internal helpers to read and pre-process raw FAOSTAT inputs.
# These replace Global's ExtractFAO(), ExtractCB(), filter_areas()
# and harmonize_countries() in whep conventions.

# -- Reading helpers -----------------------------------------------------------

.filter_years <- function(df, years) {
  if (is.null(years)) {
    return(df)
  }
  dplyr::filter(df, year %in% years)
}

.read_faostat_csv <- function(path) {
  readr::read_csv(path, show_col_types = FALSE, name_repair = "universal")
}

# -- FAOSTAT extraction --------------------------------------------------------

.harmonize_element_names <- function(df) {
  lookup <- c(
    "Domestic supply quantity" = "domestic_supply",
    "Stock Variation" = "stock_variation",
    "Other uses (non-food)" = "other_uses",
    "Other uses" = "other_uses",
    "Food supply quantity (tonnes)" = "food",
    "Food" = "food",
    "Import Quantity" = "import",
    "Import quantity" = "import",
    "Export Quantity" = "export",
    "Export quantity" = "export",
    "Feed" = "feed",
    "Seed" = "seed",
    "Processing" = "processing",
    "Production" = "production"
  )
  df |>
    dplyr::mutate(
      element = dplyr::coalesce(
        unname(lookup[element]),
        element
      )
    )
}

.normalise_units <- function(df) {
  df |>
    dplyr::mutate(
      value = dplyr::if_else(
        unit %in% c("1000 tonnes", "1000 t"),
        value * 1000,
        value
      ),
      unit = dplyr::if_else(
        unit %in% c("1000 tonnes", "1000 t"),
        "tonnes",
        dplyr::if_else(unit == "1000 US$", "kdollars", as.character(unit))
      )
    )
}

.fix_item_codes <- function(df) {
  df |>
    dplyr::mutate(
      item_code_cbs = dplyr::if_else(
        item_code_cbs == 2804L,
        2807L,
        dplyr::if_else(item_code_cbs == 2820L, 2552L, item_code_cbs)
      )
    )
}

.aggregate_to_polities <- function(df, ...) {
  regions <- whep::regions_full

  df |>
    dplyr::inner_join(
      regions |>
        dplyr::rename(area_code = code) |>
        dplyr::select(
          area_code,
          polity_code,
          polity_name
        ),
      by = "area_code"
    ) |>
    dplyr::summarise(
      value = sum(value, na.rm = TRUE),
      .by = c(year, polity_code, polity_name, unit, element, ...)
    ) |>
    dplyr::rename(
      area_code = polity_code,
      area = polity_name
    )
}

.extract_fao <- function(pin_alias, years = NULL, version = NULL) {
  cb_elements <- c(
    "production",
    "import",
    "export",
    "stock_variation",
    "domestic_supply",
    "food",
    "feed",
    "seed",
    "processing",
    "other_uses"
  )

  whep_read_file(pin_alias, version = version) |>
    dplyr::rename(
      item_code_cbs = `Item Code`,
      item_cbs = Item,
      area = Area,
      area_code = `Area Code`,
      unit = Unit,
      element = Element,
      year = Year,
      value = Value
    ) |>
    .filter_years(years) |>
    .harmonize_element_names() |>
    .normalise_units() |>
    .fix_item_codes() |>
    dplyr::filter(element %in% cb_elements) |>
    dplyr::select(
      area,
      area_code,
      item_cbs,
      item_code_cbs,
      element,
      unit,
      year,
      value
    ) |>
    .aggregate_to_polities(
      item_cbs,
      item_code_cbs
    )
}

.extract_cb <- function(pin_alias, years = NULL, version = NULL) {
  .extract_fao(pin_alias, years = years, version = version) |>
    dplyr::inner_join(
      whep::items_full |>
        dplyr::select(item_cbs, item_code_cbs),
      by = c("item_cbs", "item_code_cbs")
    )
}

# -- Processing helpers (from comdat_global) -----------------------------------

.processed_raw <- function(df, cb_processing_eq) {
  rhs <- cb_processing_eq |>
    dplyr::rename(processed_item = ProcessedItem) |>
    dplyr::select(-Value_fraction) |>
    dplyr::filter(!is.na(Product_fraction))

  join_keys <- "processed_item"
  if ("year" %in% names(rhs)) {
    join_keys <- c(join_keys, "year")
  }

  df |>
    dplyr::rename(processed_item = item_cbs) |>
    dplyr::filter(element == "processing") |>
    dplyr::inner_join(rhs, by = join_keys) |>
    dplyr::mutate(
      value_proc = value * Product_fraction,
      element = "production"
    )
}

.correct_processed <- function(
  processed_df,
  cbs,
  no_data_products = character()
) {
  cb_proc <- whep::cb_processing

  processed_df |>
    dplyr::summarise(
      value_proc = sum(value_proc, na.rm = TRUE),
      .by = c(area, area_code, year, item_cbs, element)
    ) |>
    dplyr::left_join(
      cb_proc |>
        dplyr::summarise(
          required = sum(Required, na.rm = TRUE),
          .by = item_cbs
        ) |>
        dplyr::filter(required > 0),
      by = "item_cbs"
    ) |>
    dplyr::left_join(
      cbs |>
        dplyr::summarise(
          value = sum(value, na.rm = TRUE),
          item_code_cbs = dplyr::first(item_code_cbs),
          .by = c(area, area_code, year, item_cbs, element)
        ),
      by = c("area", "area_code", "year", "item_cbs", "element")
    ) |>
    dplyr::mutate(
      scaling_raw = value / value_proc,
      scaling_raw = dplyr::if_else(
        scaling_raw == 0,
        NA_real_,
        scaling_raw
      )
    ) |>
    fill_linear(
      scaling_raw,
      time_col = year,
      .by = c("area", "area_code", "item_cbs", "element")
    ) |>
    dplyr::select(-item_code_cbs) |>
    dplyr::mutate(
      scaling = dplyr::if_else(
        is.na(scaling_raw),
        dplyr::if_else(
          !is.na(required) | item_cbs %in% no_data_products,
          1,
          0
        ),
        dplyr::if_else(
          source_scaling_raw == "Original",
          scaling_raw,
          dplyr::if_else(
            scaling_raw > 5,
            5,
            dplyr::if_else(scaling_raw < 0.2, 0.2, scaling_raw)
          )
        )
      ),
      value_final = value_proc * scaling
    )
}

# -- CBS testing helpers -------------------------------------------------------

.test_cbs <- function(df) {
  items_prod <- whep::items_prod_full
  prim_double <- whep::primary_double

  df |>
    tidyr::pivot_wider(
      names_from = "element",
      values_from = "value",
      values_fill = 0,
      names_sort = TRUE
    ) |>
    dplyr::mutate(
      ds_destinies = round(
        domestic_supply -
          food -
          feed -
          seed -
          processing -
          processing_primary -
          other_uses,
        4
      ),
      balance = round(
        production +
          import -
          export -
          stock_variation -
          food -
          feed -
          seed -
          processing -
          processing_primary -
          other_uses,
        4
      ),
      balance2 = round(
        production + import - export - stock_variation - domestic_supply,
        4
      )
    ) |>
    dplyr::left_join(
      prim_double |>
        dplyr::filter(is.na(Item_area)) |>
        dplyr::left_join(items_prod, by = "item_prod") |>
        dplyr::select(item_code_cbs, Multi_type),
      by = "item_code_cbs"
    ) |>
    dplyr::mutate(
      multi_type = dplyr::coalesce(Multi_type, "Single"),
      destiny_replacement = dplyr::if_else(
        multi_type != "Single",
        "none",
        dplyr::if_else(
          ds_destinies == balance,
          "default_prone",
          "none"
        )
      ),
      check = dplyr::if_else(
        multi_type != "Single",
        ds_destinies == balance,
        balance == 0
      )
    ) |>
    dplyr::select(-Multi_type)
}

.untest_cbs <- function(df) {
  df |>
    dplyr::select(
      -ds_destinies,
      -balance,
      -balance2,
      -multi_type,
      -dplyr::any_of("default_destiny"),
      -destiny_replacement,
      -check
    ) |>
    tidyr::pivot_longer(
      domestic_supply:stock_variation,
      names_to = "element",
      values_to = "value"
    )
}
