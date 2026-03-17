# Internal helpers to read and pre-process raw FAOSTAT inputs.
# These replace Global's ExtractFAO(), ExtractCB(), filter_areas()
# and harmonize_countries() in whep conventions.

# -- Reading helpers -----------------------------------------------------------

.load_afse_tables <- function(tables = NULL) {
  # load_general_data() assigns into parent.frame(), so we call it
  # from a dedicated environment that inherits from globalenv() so
  # that `::` and other base primitives are available.
  env <- new.env(parent = globalenv())
  evalq(
    afsetools::load_general_data(load_vectors = TRUE),
    envir = env
  )
  objs <- as.list(env)
  if (!is.null(tables)) {
    objs <- objs[intersect(names(objs), tables)]
  }
  objs
}

.read_faostat_csv <- function(path) {
  data.table::fread(path, check.names = TRUE) |>
    tibble::as_tibble()
}

# -- FAOSTAT extraction --------------------------------------------------------

.harmonize_element_names <- function(df) {
  df |>
    dplyr::mutate(
      element = stringr::str_replace(
        element,
        "Domestic supply quantity",
        "domestic_supply"
      ),
      element = stringr::str_replace(
        element,
        "Stock Variation",
        "stock_variation"
      ),
      element = stringr::str_replace(
        element,
        "Other uses \\(non-food\\)",
        "other_uses"
      ),
      element = stringr::str_replace(
        element,
        "Other uses",
        "other_uses"
      ),
      element = stringr::str_replace(
        element,
        "Food supply quantity \\(tonnes\\)",
        "food"
      ),
      element = stringr::str_replace(
        element,
        " Quantity",
        ""
      ),
      element = stringr::str_replace(
        element,
        " quantity",
        ""
      ),
      element = stringr::str_replace(
        element,
        " Value",
        ""
      ),
      element = stringr::str_replace(
        element,
        "Production",
        "production"
      ),
      element = stringr::str_replace(
        element,
        "Import",
        "import"
      ),
      element = stringr::str_replace(
        element,
        "Export",
        "export"
      ),
      element = stringr::str_replace(
        element,
        "Food",
        "food"
      ),
      element = stringr::str_replace(
        element,
        "Feed",
        "feed"
      ),
      element = stringr::str_replace(
        element,
        "Seed",
        "seed"
      ),
      element = stringr::str_replace(
        element,
        "Processing",
        "processing"
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
      unit = dplyr::case_when(
        unit %in% c("1000 tonnes", "1000 t") ~ "tonnes",
        unit == "1000 US$" ~ "kdollars",
        TRUE ~ unit
      )
    )
}

.fix_item_codes <- function(df) {
  df |>
    dplyr::mutate(
      item_code_cbs = dplyr::case_when(
        item_code_cbs == 2804 ~ 2807L,
        item_code_cbs == 2820 ~ 2552L,
        TRUE ~ item_code_cbs
      )
    )
}

.aggregate_to_polities <- function(df, ..., afse = NULL) {
  regions <- afse$regions_full %||%
    .load_afse_tables("regions_full")$regions_full

  df |>
    dplyr::right_join(
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

.extract_fao <- function(pin_alias, afse = NULL, version = NULL) {
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
      item_code_cbs = Item.Code,
      item_cbs = Item,
      area_code = Area.Code,
      unit = Unit,
      element = Element,
      year = Year,
      value = Value
    ) |>
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
      item_code_cbs,
      afse = afse
    )
}

.extract_cb <- function(pin_alias, afse = NULL, version = NULL) {
  items <- afse$items_full %||%
    .load_afse_tables("items_full")$items_full

  .extract_fao(pin_alias, afse = afse, version = version) |>
    dplyr::inner_join(
      items |>
        dplyr::select(item_cbs, item_code_cbs),
      by = c("item_cbs", "item_code_cbs")
    )
}

# -- Processing helpers (from comdat_global) -----------------------------------

.processed_raw <- function(df, cb_processing_eq) {
  df |>
    dplyr::rename(processed_item = item_cbs) |>
    dplyr::filter(element == "processing") |>
    dplyr::inner_join(
      cb_processing_eq |>
        dplyr::rename(processed_item = ProcessedItem) |>
        dplyr::select(-Value_fraction) |>
        dplyr::filter(!is.na(Product_fraction)),
      by = c("processed_item", "year")
    ) |>
    dplyr::mutate(
      value_proc = value * Product_fraction,
      element = "production"
    )
}

.correct_processed <- function(processed_df, cbs, afse = NULL) {
  cb_proc <- afse$CB_processing %||%
    .load_afse_tables("CB_processing")$CB_processing

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
    dplyr::left_join(cbs, by = c("area", "area_code", "year", "item_cbs")) |>
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
      .by = c("area", "area_code", "item_cbs")
    ) |>
    dplyr::select(-item_code_cbs) |>
    dplyr::mutate(
      scaling = dplyr::case_when(
        is.na(scaling_raw) &
          (!is.na(required) | item_cbs %in% no_data_products) ~
          1,
        is.na(scaling_raw) ~ 0,
        source_scaling_raw == "Original" ~ scaling_raw,
        scaling_raw > 5 ~ 5,
        scaling_raw < 0.2 ~ 0.2,
        TRUE ~ scaling_raw
      ),
      value_final = value_proc * scaling
    )
}

# -- CBS testing helpers -------------------------------------------------------

.test_cbs <- function(df, afse = NULL) {
  items_prod <- afse$items_prod_full %||%
    .load_afse_tables("items_prod_full")$items_prod_full
  prim_double <- afse$Primary_double %||%
    .load_afse_tables("Primary_double")$Primary_double

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
