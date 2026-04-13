#' Build commodity balance sheets
#'
#' @description
#' Construct commodity balance sheets (CBS) from raw FAOSTAT data.
#' This is a convenience wrapper that chains the three pipeline steps:
#'
#' 1. `.read_cbs()` — read & reformat FAOSTAT CBS data.
#' 2. `.fix_cbs()` — processing calibration, trade imputation,
#'    destiny filling, and final balancing.
#' 3. `.qc_cbs()` — flag data-quality anomalies.
#'
#' @param primary_all A tibble of primary production, as returned by
#'   [build_primary_production()].
#' @param start_year Integer. First year to include. Default `1850`.
#' @param end_year Integer. Last year to include. Default `2023`.
#' @param smooth_carry_forward Logical. If `TRUE`, carry-forward tails
#'   are replaced with a linear trend. Default `FALSE`.
#' @param example Logical. If `TRUE`, return a small hardcoded example
#'   tibble instead of reading remote data. Default `FALSE`.
#' @param raw_data Optional tibble with the same structure as the output
#'   of `.read_cbs()`. When provided, this replaces the remote data
#'   read, allowing tests to inject small fixtures.
#'
#' @returns A tibble in long format with columns: `year`,
#'   `area_code`, `item_cbs_code`, `element` (e.g.
#'   `"production"`, `"import"`, `"food"`), `value`,
#'   `source`, `fao_flag`.
#'
#' @export
#'
#' @examples
#' build_commodity_balances(example = TRUE)
build_commodity_balances <- function(
  primary_all,
  start_year = 1850,
  end_year = 2023,
  smooth_carry_forward = FALSE,
  example = FALSE,
  raw_data = NULL
) {
  if (example) {
    return(.example_build_commodity_bal())
  }
  raw <- raw_data %||% .read_cbs(primary_all, start_year, end_year)
  raw |>
    .fix_cbs() |>
    .qc_cbs(smooth = smooth_carry_forward) |>
    .format_cbs_output()
}

.format_cbs_output <- function(df) {
  df <- tibble::as_tibble(df)

  cbs_elements <- c(
    "domestic_supply",
    "production",
    "import",
    "export",
    "stock_variation",
    "food",
    "feed",
    "seed",
    "other_uses",
    "processing"
  )

  has_flag <- "fao_flag" %in% names(df)

  df |>
    dplyr::filter(element %in% cbs_elements) |>
    dplyr::select(
      year,
      area_code,
      item_cbs_code,
      element,
      value,
      source,
      dplyr::any_of("fao_flag")
    ) |>
    .normalise_cbs_values() |>
    dplyr::distinct() |>
    dplyr::summarise(
      value = .sum_if_any_cbs(value),
      source = dplyr::first(source),
      fao_flag = if (has_flag) dplyr::first(fao_flag) else NA_character_,
      .by = c(year, area_code, item_cbs_code, element)
    )
}

.normalise_cbs_values <- function(df) {
  if (!is.list(df$value)) {
    return(df)
  }

  df |>
    dplyr::mutate(value = purrr::map_dbl(value, .sum_if_any_cbs))
}

.sum_if_any_cbs <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }

  sum(x, na.rm = TRUE)
}


#' Step 1: Read and reformat FAOSTAT CBS data
#'
#' @description
#' Read raw FAOSTAT food balance sheets, commodity balances, trade data,
#' crop residues and primary production, combine sources (selecting the
#' best value among FBS-new, FBS-old, CBS-crops, CBS-animals), and
#' extend the series back to 1850 using historical trade data.
#'
#' No processing calibration or balancing is applied at this stage.
#' For the fully processed CBS, pipe into `.fix_cbs()`.
#'
#' The returned tibble carries `years` as an attribute so that
#' `.fix_cbs()` can reuse it without reloading.
#'
#' @param primary_all A tibble of primary production, as returned by
#'   [build_primary_production()].
#' @param start_year Integer. First year to include. Default `1850`.
#' @param end_year Integer. Last year to include. Default `2023`.
#'
#' @returns A tibble in long format with columns:
#'   `year`, `area`, `area_code`, `item_cbs`, `item_cbs_code`,
#'   `element`, `source`, `value`.
#'
#'   The `source` column indicates data provenance:
#'   * `"Primary"` — direct FAOSTAT production data.
#'   * `"FBS_New"`, `"FBS_Old"`, `"mean"` — food balance sheet selection.
#'   * `"historical_fill"` — pre-1961 historical extension.
#'
#' @keywords internal
#' @noRd
.read_cbs <- function(
  primary_all,
  start_year = 1850,
  end_year = 2023
) {
  output_years <- start_year:end_year

  # FAOSTAT CBS data begins at 1961. When historical extension is needed,
  # we must also read the FAOSTAT anchor years for destiny-share estimation.
  # The output is trimmed to output_years at the end.
  needs_historical <- start_year < 1962L
  years <- if (needs_historical) {
    start_year:max(end_year, 1965L)
  } else {
    output_years
  }

  # 1. Read inputs
  cli::cli_progress_step("Reading CBS inputs")
  inputs <- .cbs_read_inputs(
    primary_all,
    years
  )

  # 2. Build first raw CBS (combine sources, select best)
  cli::cli_progress_step("Combining CBS sources")
  cbs_raw0 <- .cbs_combine_sources(inputs)

  # 3. Historical extension
  cli::cli_progress_step("Extending CBS historical series")
  cbs_raw <- .cbs_extend_historical(
    cbs_raw0,
    inputs,
    years
  )

  # Trim to requested years and attach context for downstream
  cbs_raw <- .filter_years(cbs_raw, output_years)
  attr(cbs_raw, ".years") <- output_years

  # Aggregate FAOSTAT + FishStat trade to CBS item level for imputation
  fao_trade_agg <- .aggregate_fao_trade_to_cbs(inputs$fao_trade)
  fishstat_agg <- inputs$fishstat_trade # already aggregated to CBS level
  if (!is.null(fishstat_agg) && nrow(fishstat_agg) > 0L) {
    # Keep only the columns common with fao_trade_agg
    common_cols <- c("year", "area_code", "item_cbs_code", "element", "value")
    all_trade <- data.table::rbindlist(
      list(
        fao_trade_agg[, common_cols, with = FALSE],
        fishstat_agg[, common_cols, with = FALSE]
      ),
      use.names = TRUE
    )
    # Sum in case of overlaps (shouldn't happen: different item ranges)
    all_trade <- all_trade[,
      .(value = sum(value, na.rm = TRUE)),
      by = .(year, area_code, item_cbs_code, element)
    ]
  } else {
    all_trade <- fao_trade_agg
  }
  attr(cbs_raw, ".fao_trade") <- all_trade
  cbs_raw
}


#' Step 2: Apply processing calibration and balancing to CBS
#'
#' @description
#' Applies the data corrections and balancing steps that were originally
#' in `Global/R/commodity_balances.r`.
#'
#' **Steps applied:**
#' * Processing coefficient calibration (global).
#' * Re-estimate processed products.
#' * Redistribute non-processed items.
#' * Trade and domestic supply imputation.
#' * Destiny gap-filling (food, feed, processing, other_uses).
#' * Second round of processed products.
#' * Reclassify processing → other_uses.
#' * Final balance (clamp DS ≥ 0, fix exports, default destiny).
#'
#' @param df A tibble from `.read_cbs()`. Expects `.years`
#'   attribute (set automatically by `.read_cbs()`).
#'
#' @returns The same tibble with calibrated, imputed, and balanced values.
#'
#' @keywords internal
#' @noRd
.fix_cbs <- function(df) {
  years <- attr(df, ".years") %||% 1850:2023
  fao_trade_cbs <- attr(df, ".fao_trade")
  cbs_raw <- df

  # Strip attributes to avoid carrying large objects downstream
  attr(cbs_raw, ".years") <- NULL
  attr(cbs_raw, ".fao_trade") <- NULL

  # 4. Processing coefficients (global calibration)
  cli::cli_progress_step("Calibrating processing coefficients")
  proc_result <- .cbs_calibrate_processing(cbs_raw)

  # 5. Re-estimate processed products
  cli::cli_progress_step("Adding processed products")
  cbs_raw2 <- .cbs_add_processed(
    cbs_raw,
    proc_result,
    years
  )

  # 6. Redistribute non-processed items
  cli::cli_progress_step("Redistributing non-processed items")
  cbs_raw3 <- .cbs_redistribute_notprocessed(
    cbs_raw2,
    proc_result$processd_raw
  )

  # 7. Impute trade and domestic supply
  cli::cli_progress_step("Imputing trade and domestic supply")
  cbs_raw4 <- .cbs_impute_trade(cbs_raw3, fao_trade_cbs)

  # 8. Fill destiny gaps
  cli::cli_progress_step("Filling destiny gaps")
  cbs_raw5 <- .cbs_fill_destinies(cbs_raw4)

  # 9. Second round of processed products
  cli::cli_progress_step("Second round of processed products")
  cbs_raw6 <- .cbs_second_processed_round(
    cbs_raw5,
    proc_result
  )

  # 10. Reclassify processing
  cli::cli_progress_step("Reclassifying processing")
  cbs_raw7 <- .cbs_reclassify_processing(
    cbs_raw6,
    proc_result$cb_processing_glo
  )

  # 11. Final balancing
  cli::cli_progress_step("Final balancing")
  .cbs_final_balance(cbs_raw7, years)
}


#' Step 3: Flag CBS data-quality anomalies
#'
#' @description
#' Detects and optionally smooths data-quality issues in the CBS
#' dataset. Adds a `qc_flag` character column.
#'
#' **Flags applied:**
#' * `carry_forward` — constant-value tail (≥ `min_run` identical final
#'   years). Likely FAOSTAT carry-forward imputation.
#' * `spike` — year-on-year change exceeding `spike_ratio`.
#'
#' @param df A tibble from `.fix_cbs()` (or `.read_cbs()`).
#' @param smooth Logical. Replace carry-forward tails with a linear
#'   trend? Default `FALSE`.
#' @param anchor_years Integer. Years for trend anchor. Default `5`.
#' @param spike_ratio Numeric. Threshold. Default `10`.
#' @param spike_min Numeric. Minimum value. Default `1000`.
#' @param min_run Integer. Minimum carry-forward run. Default `3`.
#'
#' @returns The input tibble plus a `qc_flag` column.
#'
#' @keywords internal
#' @noRd
.qc_cbs <- function(
  df,
  smooth = FALSE,
  anchor_years = 5L,
  spike_ratio = 10,
  spike_min = 1000,
  min_run = 3L
) {
  by_cols <- c(
    "area",
    "area_code",
    "item_cbs",
    "item_cbs_code",
    "element"
  )

  df <- df |>
    .flag_cf_and_spikes(
      by = by_cols,
      min_run = min_run,
      spike_ratio = spike_ratio,
      spike_min = spike_min
    )

  if (smooth) {
    df <- .smooth_carry_forward(
      df,
      by = by_cols,
      anchor_years = anchor_years
    )
  }

  df <- .collapse_qc_flags(df)
  .qc_summary(df, "Commodity Balance Sheets")
  df
}

#' Build processing coefficients
#'
#' @description
#' Extract the final calibrated processing coefficients from the CBS
#' building pipeline. These can be used independently for footprint
#' calculations.
#'
#' @param cbs A tibble of final CBS in wide format, as returned by
#'   [build_commodity_balances()].
#' @param start_year Integer. First year to include. Default `1850`.
#' @param end_year Integer. Last year to include. Default `2023`.
#' @param example Logical. If `TRUE`, return a small hardcoded dataset
#'   for illustration without downloading data. Default `FALSE`.
#'
#' @returns A tibble with columns: `year`, `area_code`,
#'   `item_cbs_code_to_process`, `value_to_process`,
#'   `item_cbs_code_processed`, `initial_conversion_factor`,
#'   `initial_value_processed`, `conversion_factor_scaling`,
#'   `final_conversion_factor`, `final_value_processed`.
#'
#' @export
#'
#' @examples
#' build_processing_coefs(example = TRUE)
build_processing_coefs <- function(
  cbs,
  start_year = 1850,
  end_year = 2023,
  example = FALSE
) {
  if (example) {
    return(.example_build_proc_coefs())
  }
  cb_proc <- whep::cb_processing
  years <- start_year:end_year

  # Convert wide CBS (from build_commodity_balances()) back to long with names
  cbs <- .wide_cbs_to_long(cbs) |>
    .filter_years(years)

  cbs_glob <- cbs |>
    dplyr::summarise(
      value = sum(value, na.rm = TRUE),
      .by = c(year, item_cbs, element)
    )

  processd_raw_glo <- .processed_raw(cbs_glob, cb_proc)

  processed_agg_glo <- .calibrate_global_scaling(
    processd_raw_glo,
    cbs_glob
  )

  cb_processing_glo <- .build_global_proc_coefs(
    cb_proc,
    processed_agg_glo
  )

  proc_scaling <- cbs |>
    .processed_raw(cb_processing_glo) |>
    dplyr::summarise(
      value_proc = sum(value_proc, na.rm = TRUE),
      .by = c(area_code, year, item_cbs, element)
    ) |>
    dplyr::left_join(
      cbs,
      by = c("area_code", "year", "item_cbs", "element")
    ) |>
    dplyr::mutate(scaling = value / value_proc)

  cbs |>
    .processed_raw(cb_processing_glo) |>
    dplyr::rename(value_proc_raw = value_proc) |>
    dplyr::left_join(
      proc_scaling |>
        dplyr::select(
          year,
          area_code,
          item_cbs,
          scaling
        ),
      by = c("year", "area_code", "item_cbs")
    ) |>
    dplyr::mutate(
      cf = Product_fraction * scaling,
      value_proc = value * cf
    ) |>
    dplyr::filter(
      !is.na(value),
      !is.na(value_proc),
      value != 0
    ) |>
    .format_proc_output()
}

.wide_cbs_to_long <- function(df) {
  items <- whep::items_full

  # CBS output is now long format — just add item_cbs name
  if ("element" %in% names(df)) {
    return(
      df |>
        dplyr::left_join(
          items |> dplyr::select(item_cbs_code, item_cbs),
          by = "item_cbs_code"
        )
    )
  }

  # Legacy wide format support
  src_cols <- grep("^source", names(df), value = TRUE)
  df |>
    dplyr::select(-dplyr::any_of(src_cols)) |>
    dplyr::mutate(
      stock_variation = -stock_retrieval,
      .keep = "unused"
    ) |>
    tidyr::pivot_longer(
      cols = -c(year, area_code, item_cbs_code),
      names_to = "element",
      values_to = "value"
    ) |>
    dplyr::left_join(
      items |> dplyr::select(item_cbs_code, item_cbs),
      by = "item_cbs_code"
    )
}

.format_proc_output <- function(df) {
  items <- whep::items_full

  tibble::as_tibble(df) |>
    dplyr::left_join(
      items |>
        dplyr::select(
          item_cbs,
          item_cbs_code_processed = item_cbs_code
        ),
      by = "item_cbs"
    ) |>
    dplyr::select(
      year,
      area_code,
      item_cbs_code_to_process = item_cbs_code,
      value_to_process = value,
      item_cbs_code_processed,
      initial_conversion_factor = Product_fraction,
      initial_value_processed = value_proc_raw,
      conversion_factor_scaling = scaling,
      final_conversion_factor = cf,
      final_value_processed = value_proc
    )
}

# -- Input reading for CBS -----------------------------------------------------

.cbs_read_inputs <- function(
  primary_all,
  years
) {
  # Reuse CB extracts from production build if available
  cb <- attr(primary_all, ".cb_extracts")
  if (!is.null(cb)) {
    fbs_new <- cb$fbs_new
    fbs_old <- cb$fbs_old
    cbs_crops <- cb$cbs_crops
    cbs_animals <- cb$cbs_animals
  } else {
    fbs_new <- .extract_cb("faostat-fbs-new", years = years)
    fbs_old <- .extract_cb("faostat-fbs-old", years = years)
    cbs_crops <- .extract_cb("faostat-cbs-old-crops", years = years)
    cbs_animals <- .extract_cb("faostat-cbs-old-animal", years = years)
  }
  cbs_new <- .extract_fao(
    "faostat-cbs-new",
    years = years
  )

  # Trade
  fao_trade <- .read_fao_trade(years = years)
  fishstat_trade <- .read_fishstat_trade(years = years)
  trade_hist <- .read_historical_trade(years = years)

  # GDP over population
  gdp_pop <- .read_gdp_pop(years = years)

  # Primary production as CBS
  primary_cbs <- .primary_to_cbs(primary_all)
  primary_cbs_area <- .primary_to_cbs_area(primary_all)

  # Crop residues
  crop_residues <- .read_crop_residues(years = years)

  # Land areas
  land_areas_wide <- .read_land_areas_wide(years = years)

  list(
    fbs_new = fbs_new,
    fbs_old = fbs_old,
    cbs_crops = cbs_crops,
    cbs_animals = cbs_animals,
    cbs_new = cbs_new,
    fao_trade = fao_trade,
    fishstat_trade = fishstat_trade,
    trade_hist = trade_hist,
    gdp_pop = gdp_pop,
    primary_cbs = primary_cbs,
    primary_cbs_area = primary_cbs_area,
    crop_residues = crop_residues,
    land_areas_wide = land_areas_wide
  )
}

.read_gdp_pop <- function(years = NULL) {
  dt <- .read_input("gdp-population", years = years, year_col = "Year")
  if ("Year" %in% names(dt)) {
    data.table::setnames(dt, "Year", "year")
  }
  dt
}

# Aggregate fao_trade (which uses item_code_trade) to CBS item codes.
# Returns a data.table with: year, area_code, item_cbs_code, element, value.
.aggregate_fao_trade_to_cbs <- function(fao_trade) {
  cbs_tc <- data.table::as.data.table(whep::cbs_trade_codes)
  items <- data.table::as.data.table(whep::items_full)[,
    .(item_cbs, item_cbs_code)
  ]
  items <- unique(items, by = "item_cbs")

  trade_bridge <- merge(
    cbs_tc[, .(item_code_trade, item_cbs)],
    items,
    by = "item_cbs",
    all.x = TRUE
  )
  trade_bridge <- unique(
    trade_bridge[!is.na(item_cbs_code), .(item_code_trade, item_cbs_code)]
  )

  dt <- data.table::copy(fao_trade)
  dt <- merge(dt, trade_bridge, by = "item_code_trade", all.x = TRUE)
  dt <- dt[!is.na(item_cbs_code) & element %in% c("import", "export")]
  dt[,
    .(value = sum(value, na.rm = TRUE)),
    by = .(year, area_code, item_cbs_code, element)
  ]
}

# Read FishStat trade data (pre-aggregated to CBS items) from pins.
# Returns a data.table: year, area_code, item_cbs_code, element, value.
# Returns NULL if the pin is not registered yet.
.read_fishstat_trade <- function(years = NULL) {
  # Check if fishstat-trade is registered in whep_inputs
  if (!"fishstat-trade" %in% whep::whep_inputs$alias) {
    cli::cli_alert_info("fishstat-trade pin not registered, skipping")
    return(NULL)
  }

  dt <- .read_input("fishstat-trade", years = years, year_col = "Year")
  data.table::setnames(
    dt,
    c("Area Code", "Item Code", "Element", "Year", "Value", "Unit"),
    c("area_code", "item_cbs_code", "element", "year", "value", "unit")
  )
  dt[,
    element := data.table::fifelse(
      element == "Import quantity",
      "import",
      "export"
    )
  ]
  dt <- dt[element %in% c("import", "export")]

  .aggregate_to_polities(dt, item_cbs_code)
}

.read_fao_trade <- function(years = NULL) {
  dt <- .extract_fao("faostat-trade", years = years)
  if (!data.table::is.data.table(dt)) {
    data.table::setDT(dt)
  }
  data.table::setnames(
    dt,
    c("item_cbs", "item_cbs_code"),
    c("item_trade", "item_code_trade")
  )
  dt
}

.read_historical_trade <- function(years = NULL) {
  items <- data.table::as.data.table(whep::items_full)[,
    .(item_cbs, item_cbs_code)
  ]
  cbs_trade <- data.table::as.data.table(whep::cbs_trade_codes)
  regions <- data.table::as.data.table(whep::regions_full)
  polities <- data.table::as.data.table(whep::polities)[,
    .(iso3c, area_code)
  ]

  exports <- .read_input(
    "historical-trade-exports",
    years = years,
    year_col = "year"
  )
  exports[, element := "import"]

  imports <- .read_input(
    "historical-trade-imports",
    years = years,
    year_col = "year"
  )
  imports[, element := "export"]

  dt <- data.table::rbindlist(
    list(exports, imports),
    use.names = TRUE,
    fill = TRUE
  )
  dt <- dt[
    !is.na(iso3) & measurement %in% c("1000 MT", "1000 tons")
  ]
  dt <- dt[,
    .(value = sum(value, na.rm = TRUE) * 1000),
    by = c("year", "iso3", "item_code", "element")
  ]
  data.table::setnames(
    dt,
    c("iso3", "item_code"),
    c("iso3c", "item_code_trade")
  )

  region_bridge <- unique(
    regions[, .(iso3c, area = polity_name, polity_code)],
    by = "iso3c"
  )
  dt <- merge(dt, region_bridge, by = "iso3c", all.x = TRUE)
  dt <- merge(dt, polities, by.x = "polity_code", by.y = "iso3c", all.x = TRUE)
  dt[, polity_code := NULL]
  dt <- merge(dt, cbs_trade, by = "item_code_trade", all.x = TRUE)
  dt <- merge(dt, items, by = "item_cbs", all.x = TRUE)

  dt <- dt[,
    .(value = sum(value, na.rm = TRUE)),
    by = c("year", "area", "area_code", "item_cbs", "item_cbs_code", "element")
  ]
  dt[, unit := "tonnes"]
  dt[!is.na(area)]
}

# Enrich codes-only primary output with names needed by the CBS pipeline.
.enrich_primary_with_names <- function(primary_all) {
  primary_all |>
    add_area_name() |>
    dplyr::rename(area = area_name) |>
    add_item_cbs_name(code_column = "item_cbs_code") |>
    dplyr::rename(item_cbs = item_cbs_name)
}

.primary_to_cbs <- function(primary_all) {
  fodder_codes <- c(2000L, 2001L, 2002L, 2003L, 3002L)

  dt <- .enrich_primary_with_names(primary_all)
  if (!data.table::is.data.table(dt)) {
    data.table::setDT(dt)
  }
  dt[, element := "production"]
  dt <- dt[!is.na(item_cbs_code)]
  by_cols <- c(
    "year",
    "area",
    "area_code",
    "item_cbs",
    "item_cbs_code",
    "unit",
    "element"
  )
  dt <- dt[, .(value = sum(value, na.rm = TRUE)), by = by_cols]
  dt <- dt[!is.na(area)]

  feed_dt <- dt[item_cbs_code %in% fodder_codes]
  feed_dt <- data.table::copy(feed_dt)
  feed_dt[, element := "feed"]

  dt <- data.table::rbindlist(list(dt, feed_dt), use.names = TRUE)
  dt[unit == "tonnes"]
}

.primary_to_cbs_area <- function(primary_all) {
  dt <- .enrich_primary_with_names(primary_all)
  if (!data.table::is.data.table(dt)) {
    data.table::setDT(dt)
  }
  dt[, element := "production"]
  dt <- dt[!is.na(item_cbs_code)]
  by_cols <- c(
    "year",
    "area",
    "area_code",
    "item_cbs",
    "item_cbs_code",
    "unit",
    "element"
  )
  dt <- dt[, .(value = sum(value, na.rm = TRUE)), by = by_cols]
  dt <- dt[unit == "ha" & element == "production"]
  data.table::setnames(dt, "value", "area_ha")
  dt[, c("unit", "element") := NULL]
  dt
}

.read_crop_residues <- function(years = NULL) {
  items_prod <- whep::items_prod_full

  res <- get_primary_residues() |>
    .filter_years(years)

  # Map back to item_cbs names for CBS integration
  res <- res |>
    dplyr::mutate(element = "production") |>
    add_item_cbs_name(code_column = "item_cbs_code_residue") |>
    dplyr::rename(item_cbs = item_cbs_name)

  dt <- data.table::as.data.table(res)
  dt_extra <- data.table::copy(dt)
  dt_extra[,
    element := data.table::fifelse(
      item_cbs %in% c("Straw", "Other crop residues"),
      "feed",
      "other_uses"
    )
  ]
  dt <- data.table::rbindlist(list(dt, dt_extra), use.names = TRUE, fill = TRUE)
  dt <- dt[!is.na(item_cbs)]

  items_bridge <- data.table::as.data.table(items_prod)[,
    .(item_cbs, item_cbs_code)
  ]
  items_bridge <- unique(items_bridge, by = c("item_cbs", "item_cbs_code"))
  dt <- merge(dt, items_bridge, by = "item_cbs", all.x = TRUE)

  dt <- dt[,
    .(value = sum(value, na.rm = TRUE)),
    by = c("year", "area_code", "item_cbs", "item_cbs_code", "element")
  ]

  regions <- data.table::as.data.table(whep::regions_full)[
    !is.na(code),
    .(area_code = code, polity_code, polity_name)
  ]
  polities <- data.table::as.data.table(whep::polities)[,
    .(iso3c, polity_area_code = area_code)
  ]

  dt <- merge(dt, regions, by = "area_code", all = FALSE)
  dt <- merge(dt, polities, by.x = "polity_code", by.y = "iso3c", all.x = TRUE)

  dt <- dt[,
    .(value = sum(value, na.rm = TRUE)),
    by = c(
      "year",
      "polity_area_code",
      "polity_name",
      "item_cbs",
      "item_cbs_code",
      "element"
    )
  ]
  data.table::setnames(
    dt,
    c("polity_area_code", "polity_name"),
    c("area_code", "area")
  )
  dt
}

.read_land_areas_wide <- function(years = NULL) {
  land_areas <- .read_land_areas(years = years)
  varnames_cropland <- c(
    "c3ann",
    "c3per",
    "c4ann",
    "c4per",
    "c3nfx"
  )
  varnames_pasture <- c("pastr", "range")

  land_areas |>
    dplyr::mutate(
      land_use = dplyr::if_else(
        Land_Use %in% varnames_cropland,
        "Cropland",
        dplyr::if_else(Land_Use %in% varnames_pasture, "Pasture", "Other")
      )
    ) |>
    dplyr::filter(land_use != "Other") |>
    dplyr::summarise(
      area_mha = sum(Area_Mha, na.rm = TRUE),
      .by = c(year, area, land_use)
    ) |>
    dplyr::filter(!is.na(area)) |>
    tidyr::pivot_wider(
      names_from = land_use,
      values_from = area_mha
    ) |>
    dplyr::mutate(agriland = Cropland + Pasture)
}

# -- Source combination --------------------------------------------------------

.cbs_combine_sources <- function(inputs) {
  items <- whep::items_full
  cbs_trade <- whep::cbs_trade_codes

  # Palm kernels fix
  palm_kernels <- .fix_palm_kernels(inputs)

  # Traded residues fix
  traded_residues <- .get_traded_residues(
    inputs$fao_trade,
    cbs_trade,
    items
  )

  # Fibre and tobacco from new CBS
  fiber_tobacco <- .get_fiber_tobacco(
    inputs$cbs_new,
    cbs_trade,
    items
  )

  # Combine all sources
  cbs_raw_all <- .assemble_cbs_sources(
    inputs,
    palm_kernels,
    traded_residues,
    fiber_tobacco,
    items
  )

  # Select best value among sources
  .select_best_source(cbs_raw_all)
}

.fix_palm_kernels <- function(inputs) {
  dplyr::bind_rows(
    inputs$fbs_old |>
      dplyr::mutate(source = "FAOSTAT_FBS_Old"),
    inputs$fbs_new |>
      dplyr::mutate(source = "FAOSTAT_FBS_New") |>
      dplyr::filter(year > 2013)
  ) |>
    dplyr::filter(
      (item_cbs == "Palm kernels" &
        element == "processing" &
        source == "FAOSTAT_FBS_Old") |
        (item_cbs == "Palmkernel Oil" & element == "production")
    ) |>
    dplyr::select(-item_cbs_code, -element) |>
    tidyr::pivot_wider(
      names_from = item_cbs,
      values_from = value,
      values_fill = NA
    ) |>
    dplyr::rename_with(~ stringr::str_replace(., " ", "_")) |>
    fill_proxy_growth(
      value_col = Palm_kernels,
      proxy_col = "Palmkernel_Oil",
      time_col = year,
      .by = c("area", "area_code"),
      verbose = FALSE
    ) |>
    dplyr::rename(processing = Palm_kernels) |>
    dplyr::select(year, area, area_code, processing) |>
    dplyr::mutate(
      item_cbs = "Palm kernels",
      item_cbs_code = 2562,
      unit = "tonnes"
    ) |>
    .fill_palm_kernel_destinies(inputs$fbs_old) |>
    dplyr::filter(year > 2013, !is.na(value)) |>
    .add_palm_kernel_ds()
}

.fill_palm_kernel_destinies <- function(pk, fbs_old) {
  pk |>
    dplyr::left_join(
      fbs_old |>
        dplyr::filter(
          item_cbs == "Palm kernels",
          element %in% c("food", "other_uses")
        ) |>
        tidyr::pivot_wider(
          names_from = "element",
          values_from = "value"
        ),
      by = c("year", "area", "area_code", "item_cbs", "item_cbs_code", "unit")
    ) |>
    fill_proxy_growth(
      value_col = food,
      proxy_col = "processing",
      time_col = year,
      .by = c("area", "area_code"),
      verbose = FALSE
    ) |>
    fill_proxy_growth(
      value_col = other_uses,
      proxy_col = "processing",
      time_col = year,
      .by = c("area", "area_code"),
      verbose = FALSE
    ) |>
    dplyr::select(
      year,
      area,
      area_code,
      item_cbs,
      item_cbs_code,
      unit,
      food,
      processing,
      other_uses
    ) |>
    tidyr::pivot_longer(
      food:other_uses,
      names_to = "element",
      values_to = "value"
    )
}

.add_palm_kernel_ds <- function(df) {
  dplyr::bind_rows(
    df,
    df |>
      dplyr::summarise(
        value = sum(value, na.rm = TRUE),
        .by = c(
          year,
          area,
          area_code,
          item_cbs,
          item_cbs_code,
          unit
        )
      ) |>
      dplyr::mutate(element = "domestic_supply")
  )
}

.get_traded_residues <- function(fao_trade, cbs_trade, items) {
  fao_trade |>
    dplyr::filter(year > 2013) |>
    dplyr::left_join(
      cbs_trade |>
        dplyr::select(item_code_trade, item_cbs),
      by = "item_code_trade"
    ) |>
    dplyr::left_join(
      items |>
        dplyr::select(item_cbs, item_cbs_code, comm_group),
      by = "item_cbs"
    ) |>
    dplyr::filter(
      comm_group == "Oil cakes" |
        item_cbs %in%
          c(
            "Molasses",
            "Jute",
            "Jute-Like Fibres",
            "Sisal",
            "Hard Fibres, Other",
            "Soft Fibres, Other",
            "Palm kernels",
            "Hides and skins"
          )
    ) |>
    dplyr::summarise(
      value = sum(value, na.rm = TRUE),
      .by = c(
        year,
        area,
        area_code,
        item_cbs,
        item_cbs_code,
        element
      )
    )
}

.get_fiber_tobacco <- function(cbs_new, cbs_trade, items) {
  cbs_new |>
    dplyr::rename(
      item_trade = item_cbs,
      item_code_trade = item_cbs_code
    ) |>
    dplyr::left_join(
      cbs_trade |>
        dplyr::select(item_code_trade, item_cbs),
      by = "item_code_trade"
    ) |>
    dplyr::left_join(
      items |> dplyr::select(item_cbs, item_cbs_code),
      by = "item_cbs"
    ) |>
    dplyr::summarise(
      value = sum(value, na.rm = TRUE),
      .by = c(
        year,
        area,
        area_code,
        element,
        item_cbs,
        item_cbs_code
      )
    ) |>
    dplyr::filter(year > 2013)
}

.assemble_cbs_sources <- function(
  inputs,
  palm_kernels,
  traded_res,
  fiber_tobacco,
  items
) {
  fbs_new <- if (data.table::is.data.table(inputs$fbs_new)) {
    data.table::copy(inputs$fbs_new)
  } else {
    data.table::as.data.table(inputs$fbs_new)
  }
  fbs_new <- fbs_new[
    !(element %in%
      c("processing", "domestic_supply", "stock_variation") &
      item_cbs_code == 2635)
  ]
  fbs_new <- fbs_new[
    !(element %in%
      c(
        "production",
        "domestic_supply",
        "processing",
        "stock_variation",
        "other_uses",
        "food"
      ) &
      item_cbs == "Palm kernels")
  ]
  if (!data.table::is.data.table(palm_kernels)) {
    data.table::setDT(palm_kernels)
  }
  fbs_new <- data.table::rbindlist(
    list(fbs_new, palm_kernels),
    use.names = TRUE,
    fill = TRUE
  )
  fbs_new[, source := "FAOSTAT_FBS_New"]

  fbs_old <- if (data.table::is.data.table(inputs$fbs_old)) {
    data.table::copy(inputs$fbs_old)
  } else {
    data.table::as.data.table(inputs$fbs_old)
  }
  fbs_old[, source := "FAOSTAT_FBS_Old"]

  cbs <- data.table::rbindlist(
    list(
      inputs$cbs_animals,
      inputs$cbs_crops,
      fiber_tobacco
    ),
    use.names = TRUE,
    fill = TRUE
  )
  cbs[, source := "FAOSTAT_CBS"]

  primary <- data.table::rbindlist(
    list(
      inputs$primary_cbs,
      inputs$crop_residues
    ),
    use.names = TRUE,
    fill = TRUE
  )
  primary[, source := "FAOSTAT_prod"]

  if (!data.table::is.data.table(traded_res)) {
    data.table::setDT(traded_res)
  }
  trade <- traded_res
  trade[, source := "FAOSTAT_trade"]

  dt <- data.table::rbindlist(
    list(fbs_new, fbs_old, cbs, primary, trade),
    use.names = TRUE,
    fill = TRUE
  )
  dt <- dt[!is.na(year) & !is.na(area) & !is.na(element)]

  pp_items <- c(
    "Oil, palm fruit",
    "Hops",
    "Seed cotton",
    "Coconuts",
    "Hemp",
    "Kapok fruit",
    "Linum"
  )
  dt_pp <- dt[item_cbs %in% pp_items & source != "trade_hist"]
  dt_pp[, element := "processing_primary"]

  data.table::rbindlist(list(dt, dt_pp), use.names = TRUE, fill = TRUE)
}

.select_best_source <- function(cbs_raw_all) {
  # Pivot sources into columns — avoids grouped summarise + nth overhead.
  key_cols <- c(
    "area",
    "area_code",
    "year",
    "item_cbs",
    "item_cbs_code",
    "element"
  )
  group_cols <- c(
    "area",
    "area_code",
    "item_cbs",
    "item_cbs_code",
    "element"
  )

  wide <- cbs_raw_all |>
    dplyr::filter(!is.na(area)) |>
    dplyr::select(dplyr::all_of(key_cols), source, value) |>
    tidyr::pivot_wider(names_from = source, values_from = value)

  # Ensure expected columns exist even if a source is absent
  for (col in c("FAOSTAT_prod", "FAOSTAT_FBS_New", "FAOSTAT_FBS_Old")) {
    if (!col %in% names(wide)) wide[[col]] <- NA_real_
  }

  # --- Harmonize old and new FBS ---
  # Trust FBS_New as the reference. In the overlap period (2010-2013),
  # compute a scaling ratio to align FBS_Old to FBS_New level, then
  # apply it to all FBS_Old years for a smooth transition.
  overlap_ratio <- wide |>
    dplyr::filter(
      year >= 2010L,
      year <= 2013L,
      !is.na(FAOSTAT_FBS_New),
      !is.na(FAOSTAT_FBS_Old),
      FAOSTAT_FBS_Old != 0
    ) |>
    dplyr::summarise(
      scale_new_old = stats::median(
        FAOSTAT_FBS_New / FAOSTAT_FBS_Old,
        na.rm = TRUE
      ),
      .by = dplyr::all_of(group_cols)
    ) |>
    dplyr::filter(is.finite(scale_new_old))

  wide <- dplyr::left_join(
    wide,
    overlap_ratio,
    by = group_cols
  )

  # Compute fallback from other sources (CBS, Trade, etc.)
  other_src_cols <- setdiff(
    names(wide),
    c(
      key_cols,
      "FAOSTAT_prod",
      "FAOSTAT_FBS_New",
      "FAOSTAT_FBS_Old",
      "scale_new_old"
    )
  )
  if (length(other_src_cols) > 0L) {
    other_mat <- as.matrix(wide[other_src_cols])
    other_n <- rowSums(!is.na(other_mat))
    other_s <- rowSums(other_mat, na.rm = TRUE)
    wide[["other_mean"]] <- ifelse(other_n > 0, other_s / other_n, NA_real_)
  } else {
    wide[["other_mean"]] <- NA_real_
  }

  wide <- wide |>
    dplyr::mutate(
      # Scale FBS_Old to match FBS_New level where a ratio exists
      FBS_Old_scaled = dplyr::if_else(
        !is.na(scale_new_old) & !is.na(FAOSTAT_FBS_Old),
        FAOSTAT_FBS_Old * scale_new_old,
        FAOSTAT_FBS_Old
      ),
      # Source selection: Primary > FBS_New > scaled FBS_Old > other
      value = dplyr::case_when(
        !is.na(FAOSTAT_prod) ~ FAOSTAT_prod,
        !is.na(FAOSTAT_FBS_New) ~ FAOSTAT_FBS_New,
        !is.na(FBS_Old_scaled) ~ FBS_Old_scaled,
        !is.na(other_mean) ~ other_mean,
        TRUE ~ NA_real_
      ),
      source = dplyr::case_when(
        !is.na(FAOSTAT_prod) ~ "FAOSTAT_prod",
        !is.na(FAOSTAT_FBS_New) ~ "FAOSTAT_FBS_New",
        !is.na(FBS_Old_scaled) & !is.na(scale_new_old) & scale_new_old != 1 ~
          "FAOSTAT_FBS_Old_scaled",
        !is.na(FAOSTAT_FBS_Old) ~ "FAOSTAT_FBS_Old",
        TRUE ~ "mean"
      ),
      value = dplyr::if_else(
        element %in%
          c(
            "production",
            "import",
            "export",
            "domestic_supply",
            "food",
            "feed",
            "seed",
            "processing",
            "other_uses"
          ) &
          (value < 0 | is.infinite(value)),
        0,
        value
      )
    ) |>
    dplyr::select(
      area,
      area_code,
      item_cbs,
      item_cbs_code,
      element,
      year,
      source,
      value
    )
}

# -- Historical extension for CBS ---------------------------------------------

.cbs_extend_historical <- function(
  cbs_raw0,
  inputs,
  years
) {
  items <- whep::items_full

  cbs_hist <- cbs_raw0 |>
    dplyr::filter(year %in% years, year < 1962) |>
    dplyr::select(-dplyr::any_of("source"))

  cbs_hist <- .cbs_complete_year_nesting_dt(
    cbs_hist,
    id_cols = c("area", "area_code", "item_cbs", "item_cbs_code", "element")
  ) |>
    .fill_historical_destinies(
      inputs$primary_cbs_area,
      inputs$gdp_pop,
      inputs$land_areas_wide,
      items
    )

  dplyr::bind_rows(
    cbs_hist |>
      dplyr::filter(year < 1961) |>
      dplyr::mutate(source = "historical_fill"),
    cbs_raw0 |> dplyr::filter(year > 1960)
  )
}

.cbs_complete_year_nesting_dt <- function(df, id_cols) {
  dt <- data.table::as.data.table(df)
  years_dt <- data.table::data.table(year = sort(unique(dt$year)))
  keys_dt <- unique(dt[, id_cols, with = FALSE])
  years_dt[, .cross_key := 1L]
  keys_dt[, .cross_key := 1L]
  skeleton <- merge(
    years_dt,
    keys_dt,
    by = ".cross_key",
    allow.cartesian = TRUE
  )
  skeleton[, .cross_key := NULL]

  merge(
    skeleton,
    dt,
    by = c("year", id_cols),
    all.x = TRUE
  )
}

.fill_historical_destinies <- function(
  df,
  primary_area,
  gdp_pop,
  land_wide,
  items
) {
  expected_elements <- c(
    "domestic_supply",
    "production",
    "import",
    "export",
    "food",
    "feed",
    "other_uses",
    "processing",
    "processing_primary",
    "seed",
    "stock_variation"
  )

  df |>
    tidyr::pivot_wider(
      names_from = element,
      values_from = value
    ) |>
    (\(d) {
      for (col in setdiff(expected_elements, names(d))) {
        d[[col]] <- NA_real_
      }
      d
    })() |>
    dplyr::mutate(
      domestic_supply = dplyr::coalesce(
        domestic_supply,
        dplyr::if_else(
          !is.na(production) & !is.na(import) & !is.na(export),
          production + import - export,
          NA_real_
        )
      ),
      food_share = food / domestic_supply,
      feed_share = feed / domestic_supply,
      other_uses_share = other_uses / domestic_supply,
      processing_share = processing / domestic_supply,
      processing_primary_share = processing_primary / domestic_supply
    ) |>
    dplyr::left_join(
      primary_area,
      by = c("year", "area", "area_code", "item_cbs", "item_cbs_code")
    ) |>
    dplyr::mutate(seed_rate = seed / area_ha) |>
    .fill_share_columns() |>
    .apply_filled_shares() |>
    .fill_with_proxies(gdp_pop, land_wide) |>
    .finalise_historical(items)
}

.fill_share_columns <- function(df) {
  # Batch-fill all share columns using fully vectorized nafill approach.
  cols <- c(
    "food_share",
    "feed_share",
    "other_uses_share",
    "processing_share",
    "processing_primary_share",
    "seed_rate"
  )
  by_cols <- c(
    "area",
    "area_code",
    "item_cbs",
    "item_cbs_code"
  )

  if (!data.table::is.data.table(df)) {
    data.table::setDT(df)
  }
  data.table::setorderv(df, c(by_cols, "year"))

  grp <- data.table::rleidv(df, cols = by_cols)
  tm <- df[["year"]]
  nn <- length(tm)

  for (col in cols) {
    val <- df[[col]]
    is_na <- is.na(val)
    if (!any(is_na)) {
      data.table::set(
        df,
        j = paste0("source_", col),
        value = rep("Original", nn)
      )
      next
    }

    locf_raw <- data.table::nafill(val, "locf")
    nocb_raw <- data.table::nafill(val, "nocb")

    valid_idx <- ifelse(!is_na, seq_len(nn), NA_integer_)
    locf_idx <- data.table::nafill(valid_idx, "locf")
    nocb_idx <- data.table::nafill(valid_idx, "nocb")

    locf_ok <- !is.na(locf_idx) & grp[locf_idx] == grp
    nocb_ok <- !is.na(nocb_idx) & grp[nocb_idx] == grp

    locf <- data.table::fifelse(locf_ok, locf_raw, NA_real_)
    nocb <- data.table::fifelse(nocb_ok, nocb_raw, NA_real_)

    filled <- val
    source <- data.table::fifelse(is_na, "Gap not filled", "Original")

    # Interpolation
    time_valid <- data.table::fifelse(!is_na, tm, NA_real_)
    tl <- data.table::fifelse(
      locf_ok,
      data.table::nafill(time_valid, "locf"),
      NA_real_
    )
    tn <- data.table::fifelse(
      nocb_ok,
      data.table::nafill(time_valid, "nocb"),
      NA_real_
    )
    denom <- tn - tl
    frac <- data.table::fifelse(
      !is.na(denom) & denom != 0,
      (tm - tl) / denom,
      NA_real_
    )
    interp <- locf + (nocb - locf) * frac
    mask <- is_na & !is.na(interp) & locf_ok & nocb_ok
    filled[mask] <- interp[mask]
    source[mask] <- "Linear interpolation"

    # Carry backward
    mask <- is_na & !locf_ok & nocb_ok
    filled[mask] <- nocb[mask]
    source[mask] <- "First value carried backwards"

    # Carry forward
    mask <- is_na & locf_ok & !nocb_ok
    filled[mask] <- locf[mask]
    source[mask] <- "Last value carried forward"

    data.table::set(df, j = col, value = filled)
    data.table::set(df, j = paste0("source_", col), value = source)
  }
  df
}

.apply_filled_shares <- function(df) {
  df |>
    dplyr::mutate(
      food = dplyr::coalesce(
        food,
        domestic_supply * food_share
      ),
      feed = dplyr::coalesce(
        feed,
        domestic_supply * feed_share
      ),
      other_uses = dplyr::coalesce(
        other_uses,
        domestic_supply * other_uses_share
      ),
      processing = dplyr::coalesce(
        processing,
        domestic_supply * processing_share
      ),
      processing_primary = dplyr::coalesce(
        processing_primary,
        domestic_supply * processing_primary_share
      ),
      seed = dplyr::coalesce(
        seed,
        production * seed_rate
      )
    )
}

.fill_with_proxies <- function(df, gdp_pop, land_wide) {
  df |>
    dplyr::left_join(
      gdp_pop |> dplyr::select(year, area, pop),
      by = c("year", "area")
    ) |>
    dplyr::left_join(
      land_wide,
      by = c("year", "area")
    ) |>
    fill_proxy_growth(
      value_col = food,
      proxy_col = "pop",
      time_col = year,
      .by = c("area", "item_cbs"),
      verbose = FALSE
    ) |>
    fill_proxy_growth(
      value_col = other_uses,
      proxy_col = "pop",
      time_col = year,
      .by = c("area", "item_cbs"),
      verbose = FALSE
    ) |>
    fill_proxy_growth(
      value_col = feed,
      proxy_col = "agriland",
      time_col = year,
      .by = c("area", "item_cbs"),
      verbose = FALSE
    ) |>
    fill_proxy_growth(
      value_col = processing,
      proxy_col = "pop",
      time_col = year,
      .by = c("area", "item_cbs"),
      verbose = FALSE
    )
}

.finalise_historical <- function(df, items) {
  df |>
    dplyr::left_join(
      items |> dplyr::select(item_cbs, group),
      by = "item_cbs"
    ) |>
    dplyr::mutate(
      production = dplyr::coalesce(
        production,
        dplyr::if_else(
          group == "Crop products",
          NA_real_,
          0
        )
      ),
      dplyr::across(
        c(
          food,
          other_uses,
          feed,
          processing,
          processing_primary,
          seed
        ),
        ~ tidyr::replace_na(., 0)
      )
    ) |>
    dplyr::select(
      year,
      area,
      area_code,
      item_cbs,
      item_cbs_code,
      production,
      domestic_supply,
      import,
      export,
      food,
      feed,
      other_uses,
      processing,
      processing_primary,
      seed
    ) |>
    tidyr::pivot_longer(
      production:seed,
      names_to = "element",
      values_to = "value"
    ) |>
    dplyr::filter(!is.na(value))
}

# -- Processing calibration ----------------------------------------------------

.cbs_calibrate_processing <- function(cbs_raw) {
  cb_proc <- whep::cb_processing
  items <- whep::items_full

  cbs_glob <- cbs_raw |>
    dplyr::summarise(
      value = sum(value, na.rm = TRUE),
      .by = c(year, item_cbs, element)
    )

  processd_raw_glo <- .processed_raw(cbs_glob, cb_proc)

  processed_agg_glo <- .calibrate_global_scaling(
    processd_raw_glo,
    cbs_glob
  )

  cb_processing_glo <- .build_global_proc_coefs(
    cb_proc,
    processed_agg_glo
  )

  no_data_products <- processed_agg_glo |>
    dplyr::summarise(
      scaling_raw = sum(scaling_raw, na.rm = TRUE),
      .by = item_cbs
    ) |>
    dplyr::filter(scaling_raw == 0) |>
    dplyr::pull(item_cbs)

  processd_raw <- .processed_raw(cbs_raw, cb_processing_glo)

  processed_agg <- .correct_processed(
    processd_raw,
    cbs_raw,
    no_data_products = no_data_products
  ) |>
    dplyr::select(
      year,
      area,
      area_code,
      element,
      item_cbs,
      value_final
    ) |>
    dplyr::rename(value = value_final) |>
    dplyr::mutate(
      value = dplyr::if_else(
        is.na(value) | is.infinite(value),
        0,
        value
      )
    )

  list(
    cb_processing_glo = cb_processing_glo,
    cbs_glob = cbs_glob,
    processd_raw = processd_raw,
    processed_agg = processed_agg,
    no_data_products = no_data_products
  )
}

.calibrate_global_scaling <- function(
  processd_raw_glo,
  cbs_glob
) {
  items <- whep::items_full

  processd_raw_glo |>
    dplyr::summarise(
      value_proc = sum(value_proc, na.rm = TRUE),
      .by = c(year, item_cbs, element)
    ) |>
    dplyr::left_join(
      cbs_glob,
      by = c("year", "item_cbs", "element")
    ) |>
    dplyr::mutate(scaling_raw = value / value_proc) |>
    dplyr::left_join(
      items |>
        dplyr::select(item_cbs, item_cbs_code, comm_group),
      by = "item_cbs"
    ) |>
    dplyr::filter(item_cbs != "Flour") |>
    dplyr::mutate(
      scaling = dplyr::if_else(
        comm_group == "Other processing residues" |
          is.na(scaling_raw),
        1,
        scaling_raw
      ),
      scaling = dplyr::if_else(
        year < 1961,
        NA_real_,
        scaling
      )
    ) |>
    fill_linear(
      scaling,
      time_col = year,
      .by = c("item_cbs")
    )
}

.build_global_proc_coefs <- function(
  cb_processing,
  processed_agg_glo
) {
  cb_processing |>
    dplyr::left_join(
      processed_agg_glo |>
        dplyr::select(year, item_cbs, scaling),
      by = "item_cbs",
      relationship = "many-to-many"
    ) |>
    dplyr::mutate(
      Product_fraction = Product_fraction * scaling
    ) |>
    dplyr::select(
      year,
      ProcessedItem,
      item_cbs,
      Product_fraction,
      Value_fraction
    ) |>
    dplyr::filter(!is.na(year))
}

# -- Add processed products ----------------------------------------------------

.cbs_add_processed <- function(
  cbs_raw,
  proc_result,
  years
) {
  items <- whep::items_full

  dplyr::bind_rows(
    cbs_raw |>
      dplyr::left_join(
        items |>
          dplyr::select(item_cbs, item_cbs_code, group),
        by = c("item_cbs", "item_cbs_code")
      ) |>
      dplyr::filter(!(group == "Crop products" & element == "production")),
    proc_result$processed_agg |>
      dplyr::left_join(
        items |> dplyr::select(item_cbs, item_cbs_code),
        by = "item_cbs"
      ) |>
      dplyr::mutate(source = "Processed")
  ) |>
    dplyr::filter(
      year %in% years,
      !is.na(area),
      !is.na(element)
    ) |>
    dplyr::select(
      year,
      area,
      area_code,
      item_cbs,
      item_cbs_code,
      element,
      source,
      value
    )
}

# -- Redistribute non-processed ------------------------------------------------

.cbs_redistribute_notprocessed <- function(
  cbs_raw2,
  processd_raw
) {
  # Save source provenance before internal processing
  src_lookup <- cbs_raw2 |>
    dplyr::distinct(
      year,
      area_code,
      item_cbs_code,
      element,
      .keep_all = TRUE
    ) |>
    dplyr::select(year, area_code, item_cbs_code, element, source)
  cbs_raw2 <- cbs_raw2 |>
    dplyr::select(-dplyr::any_of("source"))

  not_processed <- cbs_raw2 |>
    dplyr::filter(element == "processing", value > 0) |>
    dplyr::select(-element) |>
    dplyr::anti_join(
      processd_raw |>
        dplyr::summarise(
          value = sum(value),
          .by = c(year, area, area_code, processed_item)
        ) |>
        dplyr::select(-value) |>
        dplyr::rename(item_cbs = processed_item),
      by = c("year", "area", "area_code", "item_cbs")
    ) |>
    dplyr::left_join(
      cbs_raw2 |>
        dplyr::filter(
          element %in%
            c(
              "food",
              "feed",
              "other_uses",
              "export"
            )
        ) |>
        dplyr::mutate(
          share = value / sum(value),
          .by = c(year, area, area_code, item_cbs)
        ) |>
        dplyr::select(-value),
      by = c(
        "year",
        "area",
        "area_code",
        "item_cbs",
        "item_cbs_code"
      )
    ) |>
    dplyr::mutate(value = value * share) |>
    dplyr::select(-share)

  items <- cbs_raw2 |>
    dplyr::left_join(
      not_processed |>
        dplyr::summarise(
          value = sum(value),
          .by = c(year, area, area_code, item_cbs)
        ) |>
        dplyr::select(-value) |>
        dplyr::mutate(has_np = TRUE),
      by = c("year", "area", "area_code", "item_cbs")
    )

  ok <- items |> dplyr::filter(is.na(has_np))

  fixed <- items |>
    dplyr::filter(!is.na(has_np)) |>
    dplyr::select(-has_np) |>
    dplyr::bind_rows(not_processed) |>
    dplyr::summarise(
      value = sum(value, na.rm = TRUE),
      .by = c(
        year,
        area,
        area_code,
        item_cbs,
        item_cbs_code,
        element
      )
    ) |>
    dplyr::filter(
      !is.na(element),
      element != "processing"
    ) |>
    tidyr::pivot_wider(
      names_from = element,
      values_from = value,
      values_fill = 0
    ) |>
    dplyr::mutate(
      domestic_supply = feed + food + seed + other_uses
    ) |>
    tidyr::pivot_longer(
      dplyr::any_of(c(
        "domestic_supply",
        "production",
        "export",
        "import",
        "stock_variation",
        "food",
        "feed",
        "seed",
        "other_uses"
      )),
      names_to = "element",
      values_to = "value"
    ) |>
    dplyr::summarise(
      value = sum(value, na.rm = TRUE),
      .by = c(year, area, area_code, item_cbs, element)
    )

  dplyr::bind_rows(
    ok |> dplyr::select(-has_np),
    fixed
  ) |>
    dplyr::select(-dplyr::any_of(c("has_np", "item_cbs_code"))) |>
    dplyr::left_join(
      dplyr::distinct(
        cbs_raw2,
        item_cbs,
        item_cbs_code
      ),
      by = "item_cbs"
    ) |>
    dplyr::left_join(
      src_lookup,
      by = c(
        "year",
        "area_code",
        "item_cbs_code",
        "element"
      )
    )
}

# -- Impute trade + domestic supply --------------------------------------------

.cbs_impute_trade <- function(cbs_raw3, fao_trade_cbs = NULL) {
  # Save source provenance before pivot cycle
  src_lookup <- cbs_raw3 |>
    dplyr::distinct(
      year,
      area_code,
      item_cbs_code,
      element,
      .keep_all = TRUE
    ) |>
    dplyr::select(year, area_code, item_cbs_code, element, source)

  destiny_list <- c(
    "food",
    "feed",
    "seed",
    "other_uses",
    "processing",
    "processing_primary"
  )

  # --- Tier 1: FAOSTAT trade dataset (aggregated to CBS items) ---
  # Prepare import/export lookup from standalone FAOSTAT trade data.
  if (!is.null(fao_trade_cbs) && nrow(fao_trade_cbs) > 0L) {
    fao_imp <- fao_trade_cbs[fao_trade_cbs$element == "import", ]
    fao_exp <- fao_trade_cbs[fao_trade_cbs$element == "export", ]

    fao_imp_lookup <- fao_imp |>
      dplyr::select(
        year,
        area_code,
        item_cbs_code,
        fao_trade_import = value
      )
    fao_exp_lookup <- fao_exp |>
      dplyr::select(
        year,
        area_code,
        item_cbs_code,
        fao_trade_export = value
      )

    # Items that exist anywhere in FAOSTAT trade (for tier 2 eligibility)
    tradeable_items <- unique(fao_trade_cbs$item_cbs_code)
  } else {
    fao_imp_lookup <- NULL
    fao_exp_lookup <- NULL
    tradeable_items <- integer(0)
  }

  wide <- cbs_raw3 |>
    dplyr::select(-dplyr::any_of("source")) |>
    tidyr::pivot_wider(
      names_from = element,
      values_from = value,
      values_fill = NA
    ) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::any_of(destiny_list),
        ~ tidyr::replace_na(., 0)
      ),
      # Domestic supply from destiny sums
      domestic_supply = food +
        other_uses +
        feed +
        processing +
        processing_primary +
        seed
    )

  # Join FAOSTAT trade lookups
  if (!is.null(fao_imp_lookup)) {
    wide <- wide |>
      dplyr::left_join(
        fao_imp_lookup,
        by = c("year", "area_code", "item_cbs_code")
      ) |>
      dplyr::left_join(
        fao_exp_lookup,
        by = c("year", "area_code", "item_cbs_code")
      )
  } else {
    wide <- wide |>
      dplyr::mutate(
        fao_trade_import = NA_real_,
        fao_trade_export = NA_real_
      )
  }

  wide <- wide |>
    dplyr::mutate(
      # Tier 1: use FAOSTAT trade when CBS has no value
      import = dplyr::coalesce(import, fao_trade_import),
      export = dplyr::coalesce(export, fao_trade_export),
      # Tier 2: DS-production residual, only for tradeable items
      # (items that appear somewhere in FAOSTAT trade)
      # Items eligible for tier 2 residual imputation: must appear in
      # FAOSTAT/FishStat trade, and must not be ethanol (2659, use
      # BACI/Comtrade instead) or raw sugar cane/beet (2536/2537, rarely
      # traded -- FAOSTAT trade only).
      is_tradeable = item_cbs_code %in%
        tradeable_items &
        !item_cbs_code %in% c(2659L, 2536L, 2537L),
      has_reliable_anchor = !is.na(production) &
        production > 0 &
        domestic_supply > 0,
      net_trade = domestic_supply -
        dplyr::coalesce(production, 0),
      import = dplyr::case_when(
        !is.na(import) ~ import,
        is_tradeable & has_reliable_anchor & net_trade > 0 ~ net_trade,
        TRUE ~ 0
      ),
      export = dplyr::case_when(
        !is.na(export) ~ export,
        is_tradeable & has_reliable_anchor & net_trade < 0 ~ -net_trade,
        TRUE ~ 0
      ),
      production = tidyr::replace_na(production, 0),
      stock_variation = tidyr::replace_na(
        stock_variation,
        0
      )
    ) |>
    dplyr::select(
      -fao_trade_import,
      -fao_trade_export,
      -is_tradeable,
      -has_reliable_anchor,
      -net_trade
    ) |>
    .reestimate_domestic_supply() |>
    dplyr::mutate(
      food = dplyr::if_else(
        item_cbs_code == 2635,
        domestic_supply,
        food
      )
    ) |>
    dplyr::select(
      year,
      area,
      area_code,
      item_cbs,
      item_cbs_code,
      domestic_supply,
      production,
      export,
      import,
      stock_variation,
      food,
      feed,
      seed,
      other_uses,
      processing,
      processing_primary
    ) |>
    tidyr::pivot_longer(
      domestic_supply:processing_primary,
      names_to = "element",
      values_to = "value"
    ) |>
    dplyr::left_join(
      src_lookup,
      by = c(
        "year",
        "area_code",
        "item_cbs_code",
        "element"
      )
    )
}

.reestimate_domestic_supply <- function(df) {
  df |>
    dplyr::mutate(
      ds2 = rowSums(
        dplyr::pick(
          food,
          feed,
          seed,
          other_uses,
          processing,
          processing_primary
        ),
        na.rm = TRUE
      ),
      net_bal1 = production + import - export,
      net_bal2 = production + import - export - stock_variation,
      ds3 = dplyr::if_else(
        !is.na(domestic_supply) & domestic_supply != 0,
        domestic_supply,
        dplyr::if_else(
          !is.na(ds2) & ds2 != 0,
          ds2,
          dplyr::if_else(
            net_bal1 > 0,
            net_bal1,
            dplyr::if_else(net_bal2 > 0, net_bal2, 0)
          )
        )
      ),
      prod_calc = domestic_supply -
        import +
        export -
        stock_variation,
      production = dplyr::if_else(
        is.na(production),
        pmax(prod_calc, 0),
        production
      ),
      stock_variation = production + import - export - ds3,
      domestic_supply = ds3
    ) |>
    dplyr::select(
      -ds2,
      -net_bal1,
      -net_bal2,
      -prod_calc,
      -ds3
    )
}

# -- Fill destiny gaps ---------------------------------------------------------

.cbs_fill_destinies <- function(cbs_raw4) {
  destiny_list <- c(
    "food",
    "feed",
    "seed",
    "other_uses",
    "processing",
    "processing_primary"
  )

  cli::cli_progress_step("Computing destiny shares")
  balance <- dplyr::filter(cbs_raw4, !element %in% destiny_list) |>
    dplyr::mutate(elem_cat = "balance")

  destiny <- dplyr::filter(cbs_raw4, element %in% destiny_list) |>
    dplyr::mutate(elem_cat = "destiny") |>
    .compute_destiny_shares()

  cli::cli_progress_step("Interpolating destiny shares")
  destiny_filled <- .interpolate_destiny_shares(balance, destiny)

  cli::cli_progress_step("Assembling destiny output")
  .assemble_cbs_destinies(balance, destiny_filled)
}

# Compute each destiny element's share of total destinies per (year, area, item).
# "Other processing residues" items are special-cased: feed = 1, all others = 0.
.compute_destiny_shares <- function(destiny) {
  items <- data.table::as.data.table(whep::items_full)[,
    .(item_cbs, comm_group)
  ]
  items <- unique(items, by = "item_cbs")

  dt <- data.table::as.data.table(destiny)
  dt <- merge(dt, items, by = "item_cbs", all.x = TRUE)

  grp <- c("year", "area", "area_code", "item_cbs", "item_cbs_code")
  dt[, sum_dests := sum(value, na.rm = TRUE), by = grp]
  dt[sum_dests == 0, sum_dests := NA_real_]

  dt[,
    dest_share := data.table::fifelse(
      comm_group == "Other processing residues",
      as.numeric(element == "feed"),
      value / sum_dests
    )
  ]

  dt[, c("sum_dests", "comm_group") := NULL]
  dt
}

# Fill dest_share across time using a sparse skeleton:
# interpolate/extrapolate shares at all years with domestic_supply data.
.interpolate_destiny_shares <- function(balance, destiny) {
  by_cols <- c(
    "area",
    "area_code",
    "item_cbs",
    "item_cbs_code",
    "element",
    "elem_cat"
  )

  if (!data.table::is.data.table(balance)) {
    data.table::setDT(balance)
  }
  if (!data.table::is.data.table(destiny)) {
    data.table::setDT(destiny)
  }
  dt_bal <- balance
  dt_dest <- destiny

  # Target years: where domestic_supply exists (shares needed here).
  ds_keys <- unique(dt_bal[
    element == "domestic_supply",
    .(year, area, area_code, item_cbs, item_cbs_code)
  ])

  dest_elem <- unique(dt_dest[,
    .(area, area_code, item_cbs, item_cbs_code, element, elem_cat)
  ])

  target_rows <- ds_keys[
    dest_elem,
    on = c("area", "area_code", "item_cbs", "item_cbs_code"),
    nomatch = 0,
    allow.cartesian = TRUE
  ]

  # Anchor years: known shares outside the target set.
  anchor_rows <- unique(dt_dest[
    !is.na(dest_share),
    c("year", by_cols),
    with = FALSE
  ])

  anti_keys <- c(
    "year",
    "area",
    "area_code",
    "item_cbs",
    "item_cbs_code",
    "element"
  )
  anchor_rows <- anchor_rows[!target_rows, on = anti_keys]

  # Deduplicate destiny: multiple rows per key can exist when
  # different sources contribute to one key.
  join_keys <- c(
    "year",
    "area",
    "area_code",
    "item_cbs",
    "item_cbs_code",
    "element"
  )

  destiny_dedup <- dt_dest[,
    .(
      dest_share = mean(dest_share, na.rm = TRUE),
      value = sum(value, na.rm = TRUE),
      source = source[1L]
    ),
    by = join_keys
  ]
  destiny_dedup[is.nan(dest_share), dest_share := NA_real_]

  skeleton <- data.table::rbindlist(
    list(
      target_rows[, c("year", by_cols), with = FALSE],
      anchor_rows[, c("year", by_cols), with = FALSE]
    ),
    use.names = TRUE,
    fill = TRUE
  )

  out <- merge(skeleton, destiny_dedup, by = join_keys, all.x = TRUE)
  fill_linear(out, dest_share, time_col = year, .by = by_cols)
}

# Apply filled destiny shares to domestic supply and return final CBS rows.
.assemble_cbs_destinies <- function(balance, destiny_filled) {
  if (!data.table::is.data.table(balance)) {
    data.table::setDT(balance)
  }
  if (!data.table::is.data.table(destiny_filled)) {
    data.table::setDT(destiny_filled)
  }
  dt <- data.table::rbindlist(
    list(balance, destiny_filled),
    use.names = TRUE,
    fill = TRUE
  )

  dt <- .add_global_destiny_shares(dt)

  dt[,
    value := data.table::fifelse(
      elem_cat == "destiny",
      data.table::fcoalesce(
        domestic_supply * dest_share,
        domestic_supply * dest_share_global
      ),
      data.table::fcoalesce(value, 0)
    )
  ]
  dt[, value := data.table::fcoalesce(value, 0)]

  dt <- dt[
    value != 0 & area != "",
    .(year, area, area_code, item_cbs, item_cbs_code, element, source, value)
  ]

  dt
}

.add_global_destiny_shares <- function(df) {
  if (!data.table::is.data.table(df)) {
    data.table::setDT(df)
  }
  dt <- df

  global_shares <- dt[,
    .(value = sum(value, na.rm = TRUE)),
    by = c("year", "item_cbs", "item_cbs_code", "element", "elem_cat")
  ]
  global_shares[,
    dest_share_global := value / sum(value, na.rm = TRUE),
    by = c("year", "item_cbs", "item_cbs_code", "elem_cat")
  ]
  global_shares[, value := NULL]

  ds <- dt[
    element == "domestic_supply",
    .(year, area, area_code, item_cbs, item_cbs_code, domestic_supply = value)
  ]

  out <- merge(
    dt,
    global_shares,
    by = c("year", "item_cbs", "item_cbs_code", "element", "elem_cat"),
    all.x = TRUE
  )

  merge(
    out,
    ds,
    by = c("year", "area", "area_code", "item_cbs", "item_cbs_code"),
    all.x = TRUE
  )
}

# -- Second round of processed products ---------------------------------------

.cbs_second_processed_round <- function(
  cbs_raw5,
  proc_result
) {
  cb_proc_glo <- proc_result$cb_processing_glo
  cbs_glob <- proc_result$cbs_glob

  processd_raw2 <- .processed_raw(cbs_raw5, cb_proc_glo)

  processed_agg_raw2 <- .correct_processed(
    processd_raw2,
    cbs_raw5,
    no_data_products = proc_result$no_data_products
  ) |>
    dplyr::left_join(
      proc_result$processed_agg |>
        dplyr::select(
          year,
          area,
          area_code,
          item_cbs,
          value
        ) |>
        dplyr::rename(value_final_old = value),
      by = c("year", "area", "area_code", "item_cbs")
    ) |>
    dplyr::filter(
      is.na(value_final_old),
      value_proc != 0
    ) |>
    dplyr::mutate(
      scaling = dplyr::if_else(
        is.na(scaling_raw),
        1,
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

  processed_new_bal <- .build_new_processed_balance(
    processed_agg_raw2,
    cbs_glob
  )

  join_keys <- c(
    "year",
    "area",
    "area_code",
    "item_cbs",
    "item_cbs_code",
    "element"
  )

  # Add processed values to existing rows (keeps original source),
  # then append genuinely new groups — avoids grouped summarise + first().
  dplyr::bind_rows(
    cbs_raw5 |>
      dplyr::left_join(
        processed_new_bal |>
          dplyr::select(dplyr::all_of(join_keys), value_new = value),
        by = join_keys
      ) |>
      dplyr::mutate(
        value = value + dplyr::coalesce(value_new, 0)
      ) |>
      dplyr::select(-value_new),
    processed_new_bal |>
      dplyr::anti_join(cbs_raw5, by = join_keys) |>
      dplyr::mutate(source = "Processed_round2")
  )
}

.build_new_processed_balance <- function(
  processed_agg_raw2,
  cbs_glob
) {
  items <- whep::items_full

  export_share <- cbs_glob |>
    dplyr::filter(
      element %in% c("production", "import")
    ) |>
    dplyr::summarise(
      gross_avail = sum(value, na.rm = TRUE),
      .by = c(year, item_cbs)
    ) |>
    dplyr::left_join(
      cbs_glob |>
        dplyr::filter(element == "export") |>
        dplyr::rename(export_val = value) |>
        dplyr::select(-element),
      by = c("year", "item_cbs")
    ) |>
    dplyr::mutate(
      export_share = export_val / gross_avail
    ) |>
    dplyr::select(year, item_cbs, export_share)

  dest_shares <- cbs_glob |>
    dplyr::filter(
      element %in% c("other_uses", "food", "feed")
    ) |>
    dplyr::mutate(
      dest_share = value / sum(value, na.rm = TRUE),
      .by = c(year, item_cbs)
    ) |>
    dplyr::select(year, item_cbs, element, dest_share)

  base <- processed_agg_raw2 |>
    dplyr::rename(production = value_final) |>
    dplyr::select(
      year,
      area,
      area_code,
      item_cbs,
      production
    ) |>
    dplyr::left_join(
      export_share,
      by = c("year", "item_cbs")
    ) |>
    dplyr::mutate(
      export_share = tidyr::replace_na(
        export_share,
        0
      ),
      export = production * export_share,
      domestic_supply = production - export
    ) |>
    dplyr::select(
      year,
      area,
      area_code,
      item_cbs,
      domestic_supply,
      production,
      export
    ) |>
    tidyr::pivot_longer(
      domestic_supply:export,
      names_to = "element",
      values_to = "value"
    )

  ds_vals <- base |>
    dplyr::filter(element == "domestic_supply") |>
    dplyr::select(-element)

  destiny_rows <- ds_vals |>
    dplyr::left_join(
      dest_shares,
      by = c("year", "item_cbs")
    ) |>
    dplyr::mutate(value = value * dest_share)

  dplyr::bind_rows(base, destiny_rows) |>
    dplyr::left_join(
      items |> dplyr::select(item_cbs, item_cbs_code),
      by = "item_cbs"
    )
}

# -- Reclassify processing ----------------------------------------------------

.cbs_reclassify_processing <- function(
  cbs_raw6,
  cb_processing_glo
) {
  # Save source provenance before pivot cycle
  src_lookup <- cbs_raw6 |>
    dplyr::distinct(
      year,
      area_code,
      item_cbs_code,
      element,
      .keep_all = TRUE
    ) |>
    dplyr::select(year, area_code, item_cbs_code, element, source)

  proc_scaling <- cbs_raw6 |>
    .processed_raw(cb_processing_glo) |>
    dplyr::summarise(
      value_proc = sum(value_proc, na.rm = TRUE),
      .by = c(
        area,
        area_code,
        year,
        item_cbs,
        element
      )
    ) |>
    dplyr::left_join(
      cbs_raw6,
      by = c(
        "area",
        "area_code",
        "year",
        "item_cbs",
        "element"
      )
    ) |>
    dplyr::mutate(scaling = value / value_proc)

  proc_coefs_raw <- cbs_raw6 |>
    .processed_raw(cb_processing_glo) |>
    dplyr::rename(value_proc_raw = value_proc) |>
    dplyr::left_join(
      proc_scaling |>
        dplyr::select(
          year,
          area,
          area_code,
          item_cbs,
          scaling
        ),
      by = c("year", "area", "area_code", "item_cbs")
    ) |>
    dplyr::mutate(
      cf = Product_fraction * scaling,
      value_proc = value * cf
    ) |>
    dplyr::filter(
      !is.na(value),
      !is.na(value_proc),
      value != 0
    )

  reclassify_proc <- proc_coefs_raw |>
    dplyr::summarise(
      value = mean(value),
      value_proc_raw = sum(value_proc_raw),
      value_proc = sum(value_proc),
      .by = c(
        year,
        area,
        area_code,
        processed_item,
        item_cbs_code,
        element
      )
    ) |>
    dplyr::mutate(
      proc_scaling = value_proc / value_proc_raw,
      min_scaling = 0.3,
      value_scaling = proc_scaling / min_scaling,
      reclassified_value = dplyr::if_else(
        value_scaling < 1,
        value * (1 - value_scaling),
        0
      )
    )

  cbs_filtered <- cbs_raw6 |>
    dplyr::filter(!is.na(element)) |>
    dplyr::select(-dplyr::any_of("source"))

  # Ensure processing and other_uses rows exist for groups that need

  # reclassification, without expensive pivot_wider/pivot_longer roundtrip.
  reclass_agg <- reclassify_proc |>
    dplyr::summarise(
      reclassified_value = sum(reclassified_value, na.rm = TRUE),
      .by = c(year, area_code, item_cbs_code)
    )

  needed_keys <- reclass_agg |>
    dplyr::filter(reclassified_value != 0) |>
    dplyr::select(year, area_code, item_cbs_code) |>
    dplyr::inner_join(
      cbs_filtered |>
        dplyr::distinct(year, area, area_code, item_cbs, item_cbs_code),
      by = c("year", "area_code", "item_cbs_code")
    )

  for (elem in c("processing", "other_uses")) {
    missing <- needed_keys |>
      dplyr::anti_join(
        cbs_filtered |> dplyr::filter(element == elem),
        by = c("year", "area_code", "item_cbs_code")
      ) |>
      dplyr::mutate(element = elem, value = 0)
    cbs_filtered <- dplyr::bind_rows(cbs_filtered, missing)
  }

  cbs_filtered |>
    dplyr::left_join(
      reclass_agg,
      by = c("year", "area_code", "item_cbs_code")
    ) |>
    dplyr::mutate(
      reclassified_value = tidyr::replace_na(
        reclassified_value,
        0
      ),
      value2 = dplyr::if_else(
        element == "processing",
        value - reclassified_value,
        dplyr::if_else(
          element == "other_uses",
          value + reclassified_value,
          value
        )
      )
    ) |>
    dplyr::filter(value2 != 0, area != "") |>
    dplyr::left_join(
      src_lookup,
      by = c(
        "year",
        "area_code",
        "item_cbs_code",
        "element"
      )
    ) |>
    dplyr::select(
      year,
      area,
      area_code,
      item_cbs,
      item_cbs_code,
      element,
      source,
      value = value2
    )
}

# -- Final balance -------------------------------------------------------------

.cbs_final_balance <- function(cbs_raw7, years) {
  items <- whep::items_full

  # Save source provenance before test_cbs pivot cycles
  src_lookup <- cbs_raw7 |>
    dplyr::distinct(
      year,
      area_code,
      item_cbs_code,
      element,
      .keep_all = TRUE
    ) |>
    dplyr::select(year, area_code, item_cbs_code, element, source)

  cbs_raw8 <- cbs_raw7 |>
    dplyr::filter(year %in% years) |>
    dplyr::select(-dplyr::any_of("source")) |>
    .test_cbs() |>
    dplyr::left_join(
      items |> dplyr::select(item_cbs, default_destiny),
      by = "item_cbs"
    ) |>
    dplyr::mutate(
      domestic_supply = pmax(domestic_supply, 0),
      export = dplyr::if_else(
        balance < 0,
        production + import - stock_variation - domestic_supply,
        export
      )
    ) |>
    .untest_cbs()

  cbs_raw8 |>
    .test_cbs() |>
    dplyr::left_join(
      items |> dplyr::select(item_cbs, default_destiny),
      by = "item_cbs"
    ) |>
    dplyr::mutate(
      feed = dplyr::if_else(
        check == FALSE &
          destiny_replacement == "default_prone" &
          default_destiny == "Feed",
        domestic_supply,
        feed
      ),
      food = dplyr::if_else(
        check == FALSE &
          destiny_replacement == "default_prone" &
          default_destiny == "Food",
        domestic_supply,
        food
      ),
      other_uses = dplyr::if_else(
        check == FALSE &
          destiny_replacement == "default_prone" &
          default_destiny == "Other_uses",
        domestic_supply,
        other_uses
      ),
      processing = dplyr::if_else(
        check == FALSE &
          destiny_replacement == "default_prone" &
          default_destiny == "Processing",
        domestic_supply,
        processing
      )
    ) |>
    .untest_cbs() |>
    dplyr::filter(value != 0) |>
    dplyr::left_join(
      src_lookup,
      by = c(
        "year",
        "area_code",
        "item_cbs_code",
        "element"
      )
    )
}
