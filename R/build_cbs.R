#' Build commodity balance sheets
#'
#' @description
#' Construct commodity balance sheets (CBS) from raw FAOSTAT data.
#' This is a convenience wrapper that chains the three pipeline steps:
#'
#' 1. [read_cbs()] — read & reformat FAOSTAT CBS data.
#' 2. [fix_cbs()] — processing calibration, trade imputation,
#'    destiny filling, and final balancing.
#' 3. [qc_cbs()] — flag data-quality anomalies.
#'
#' Each step can also be called independently for inspection or debugging.
#'
#' @inheritParams read_cbs
#' @param smooth_carry_forward Logical. If `TRUE`, carry-forward tails
#'   are replaced with a linear trend. Default `FALSE`.
#'
#' @returns A tibble in long format (see [read_cbs()] for column
#'   descriptions), plus a character `qc_flag` column from
#'   [qc_cbs()].
#'
#' @export
#'
#' @examples
#' \dontrun{
#' primary <- build_primary_production()
#' cbs <- build_commodity_balances(primary)
#' }
build_commodity_balances <- function(
  primary_all,
  start_year = 1850,
  end_year = 2021,
  version = NULL,
  smooth_carry_forward = FALSE
) {
  read_cbs(primary_all, start_year, end_year, version) |>
    fix_cbs() |>
    qc_cbs(smooth = smooth_carry_forward)
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
#' For the fully processed CBS, pipe into [fix_cbs()].
#'
#' The returned tibble carries `years` as an attribute so that
#' [fix_cbs()] can reuse it without reloading.
#'
#' @param primary_all A tibble of primary production, as returned by
#'   [build_primary_production()].
#' @param start_year Integer. First year to include. Default `1850`.
#' @param end_year Integer. Last year to include. Default `2021`.
#' @param version File version for input pins. `NULL` (default) uses the
#'   frozen version from [whep_inputs].
#'
#' @returns A tibble in long format with columns:
#'   `year`, `area`, `area_code`, `item_cbs`, `item_code_cbs`,
#'   `element`, `source`, `value`.
#'
#'   The `source` column indicates data provenance:
#'   * `"Primary"` — direct FAOSTAT production data.
#'   * `"FBS_New"`, `"FBS_Old"`, `"mean"` — food balance sheet selection.
#'   * `"historical_fill"` — pre-1961 historical extension.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' primary <- build_primary_production()
#' raw_cbs <- read_cbs(primary)
#' }
read_cbs <- function(
  primary_all,
  start_year = 1850,
  end_year = 2021,
  version = NULL
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
    years,
    version
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
#' @param df A tibble from [read_cbs()]. Expects `.years`
#'   attribute (set automatically by `read_cbs()`).
#'
#' @returns The same tibble with calibrated, imputed, and balanced values.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' read_cbs(primary) |> fix_cbs()
#' }
fix_cbs <- function(df) {
  years <- attr(df, ".years") %||% 1850:2021
  cbs_raw <- df

  # Strip attributes to avoid carrying large objects downstream
  attr(cbs_raw, ".years") <- NULL

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
  cbs_raw4 <- .cbs_impute_trade(cbs_raw3)

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
#' @param df A tibble from [fix_cbs()] (or [read_cbs()]).
#' @param smooth Logical. Replace carry-forward tails with a linear
#'   trend? Default `FALSE`.
#' @param anchor_years Integer. Years for trend anchor. Default `5`.
#' @param spike_ratio Numeric. Threshold. Default `10`.
#' @param spike_min Numeric. Minimum value. Default `1000`.
#' @param min_run Integer. Minimum carry-forward run. Default `3`.
#'
#' @returns The input tibble plus a `qc_flag` column.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' read_cbs(primary) |> fix_cbs() |> qc_cbs()
#' }
qc_cbs <- function(
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
    "item_code_cbs",
    "element"
  )

  df <- df |>
    .flag_carry_forward(by = by_cols, min_run = min_run) |>
    .flag_spikes(
      by = by_cols,
      spike_ratio = spike_ratio,
      min_value = spike_min
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
#' @param cbs A tibble of final CBS, as returned by
#'   [build_commodity_balances()].
#' @param start_year Integer. First year to include. Default `1850`.
#' @param end_year Integer. Last year to include. Default `2021`.
#' @param version File version for input pins.
#'
#' @returns A tibble with processing coefficients per country, year and
#'   item.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' primary <- build_primary_production()
#' cbs <- build_commodity_balances(primary)
#' coefs <- build_processing_coefs(cbs)
#' }
build_processing_coefs <- function(
  cbs,
  start_year = 1850,
  end_year = 2021,
  version = NULL
) {
  cb_proc <- whep::cb_processing
  years <- start_year:end_year

  cbs <- .filter_years(cbs, years)

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
      .by = c(area, area_code, year, item_cbs, element)
    ) |>
    dplyr::left_join(
      cbs,
      by = c("area", "area_code", "year", "item_cbs", "element")
    ) |>
    dplyr::mutate(scaling = value / value_proc)

  cbs |>
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
}

# -- Input reading for CBS -----------------------------------------------------

.cbs_read_inputs <- function(
  primary_all,
  years,
  version
) {
  # Commodity balances from FAOSTAT
  fbs_new <- .extract_cb(
    "faostat-fbs-new",
    years = years, version = version
  )
  fbs_old <- .extract_cb(
    "faostat-fbs-old",
    years = years, version = version
  )
  cbs_crops <- .extract_cb(
    "faostat-cbs-old-crops",
    years = years, version = version
  )
  cbs_animals <- .extract_cb(
    "faostat-cbs-old-animal",
    years = years, version = version
  )
  cbs_new <- .extract_fao(
    "faostat-cbs-new",
    years = years, version = version
  )

  # Processed production
  prod_processed <- whep_read_file(
    "faostat-production-processed",
    version = version
  )

  # Trade
  fao_trade <- .read_fao_trade(years = years, version = version)
  trade_hist <- .read_historical_trade(years = years, version = version)

  # GDP / population
  gdp_pop <- whep_read_file(
    "gdp-population",
    version = version
  ) |>
    dplyr::rename(year = Year) |>
    .filter_years(years)

  # Primary production as CBS
  primary_cbs <- .primary_to_cbs(primary_all)
  primary_cbs_area <- .primary_to_cbs_area(primary_all)

  # Crop residues
  crop_residues <- .read_crop_residues(years = years, version = version)

  # Land areas
  land_areas_wide <- .read_land_areas_wide(years = years, version = version)

  list(
    fbs_new = fbs_new,
    fbs_old = fbs_old,
    cbs_crops = cbs_crops,
    cbs_animals = cbs_animals,
    cbs_new = cbs_new,
    fao_trade = fao_trade,
    trade_hist = trade_hist,
    gdp_pop = gdp_pop,
    primary_cbs = primary_cbs,
    primary_cbs_area = primary_cbs_area,
    crop_residues = crop_residues,
    land_areas_wide = land_areas_wide
  )
}

.read_fao_trade <- function(years = NULL, version = NULL) {
  .extract_fao("faostat-trade", years = years, version = version) |>
    dplyr::rename(
      item_trade = item_cbs,
      item_code_trade = item_code_cbs
    )
}

.read_historical_trade <- function(years = NULL, version = NULL) {
  items <- whep::items_full
  regions <- whep::regions_full
  cbs_trade <- whep::cbs_trade_codes

  exports <- whep_read_file(
    "historical-trade-exports",
    version = version
  ) |>
    dplyr::mutate(element = "import")

  imports <- whep_read_file(
    "historical-trade-imports",
    version = version
  ) |>
    dplyr::mutate(element = "export")

  dplyr::bind_rows(exports, imports) |>
    .filter_years(years) |>
    dplyr::filter(
      !is.na(iso3),
      measurement %in% c("1000 MT", "1000 tons")
    ) |>
    dplyr::summarise(
      value = sum(value, na.rm = TRUE) * 1000,
      .by = c(year, iso3, item_code, element)
    ) |>
    dplyr::rename(
      iso3c = iso3,
      item_code_trade = item_code
    ) |>
    dplyr::left_join(
      regions |>
        dplyr::distinct(iso3c, .keep_all = TRUE) |>
        dplyr::select(
          iso3c,
          area = polity_name,
          area_code = polity_code
        ),
      by = "iso3c"
    ) |>
    dplyr::left_join(cbs_trade, by = "item_code_trade") |>
    dplyr::left_join(
      items |> dplyr::select(item_cbs, item_code_cbs),
      by = "item_cbs"
    ) |>
    dplyr::summarise(
      value = sum(value, na.rm = TRUE),
      .by = c(
        year,
        area,
        area_code,
        item_cbs,
        item_code_cbs,
        element
      )
    ) |>
    dplyr::mutate(unit = "tonnes") |>
    dplyr::filter(!is.na(area))
}

.primary_to_cbs <- function(primary_all) {
  primary_all |>
    dplyr::mutate(element = "production") |>
    dplyr::filter(!is.na(item_cbs)) |>
    dplyr::summarise(
      value = sum(value, na.rm = TRUE),
      .by = c(
        year,
        area,
        area_code,
        item_cbs,
        item_code_cbs,
        unit,
        element
      )
    ) |>
    dplyr::filter(!is.na(area)) |>
    (\(df) {
      dplyr::bind_rows(
        df,
        df |>
          dplyr::mutate(element = "feed") |>
          dplyr::filter(
            item_cbs %in%
              c(
                "Fodder cereal and grasses",
                "Fodder legumes",
                "Fodder mix",
                "Fodder vegetables and roots",
                "Temporary grassland"
              )
          )
      )
    })() |>
    dplyr::filter(unit == "tonnes")
}

.primary_to_cbs_area <- function(primary_all) {
  primary_all |>
    dplyr::mutate(element = "production") |>
    dplyr::filter(!is.na(item_cbs)) |>
    dplyr::summarise(
      value = sum(value, na.rm = TRUE),
      .by = c(
        year,
        area,
        area_code,
        item_cbs,
        item_code_cbs,
        unit,
        element
      )
    ) |>
    dplyr::filter(
      unit == "ha",
      element == "production"
    ) |>
    dplyr::rename(area_ha = value) |>
    dplyr::select(-unit, -element)
}

.read_crop_residues <- function(years = NULL, version = NULL) {
  items_prod <- whep::items_prod_full
  polities <- whep::polities_cats

  res <- get_primary_residues(version = version) |>
    .filter_years(years)

  # Map back to item_cbs names for CBS integration
  res |>
    dplyr::mutate(element = "production") |>
    add_item_cbs_name(code_column = "item_cbs_code_residue") |>
    dplyr::rename(item_cbs = item_cbs_name) |>
    add_area_name() |>
    dplyr::rename(area = area_name) |>
    (\(df) {
      dplyr::bind_rows(
        df,
        df |>
          dplyr::mutate(
            element = dplyr::if_else(
              item_cbs %in%
                c(
                  "Straw",
                  "Other crop residues"
                ),
              "feed",
              "other_uses"
            )
          )
      )
    })() |>
    dplyr::filter(!is.na(item_cbs)) |>
    dplyr::left_join(
      items_prod |>
        dplyr::distinct(item_cbs, item_code_cbs),
      by = "item_cbs"
    ) |>
    dplyr::summarise(
      value = sum(value, na.rm = TRUE),
      .by = c(year, area, area_code, item_cbs, item_code_cbs, element)
    )
}

.read_land_areas_wide <- function(years = NULL, version = NULL) {
  land_areas <- .read_land_areas(years = years, version = version)
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
        Land_Use %in% varnames_cropland, "Cropland",
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
      dplyr::mutate(source = "FBS_Old"),
    inputs$fbs_new |>
      dplyr::mutate(source = "FBS_New") |>
      dplyr::filter(year > 2013)
  ) |>
    dplyr::filter(
      (item_cbs == "Palm kernels" &
        element == "processing" &
        source == "FBS_Old") |
        (item_cbs == "Palmkernel Oil" &
          element == "production")
    ) |>
    dplyr::select(-item_code_cbs, -element) |>
    tidyr::pivot_wider(
      names_from = item_cbs,
      values_from = value,
      values_fill = NA
    ) |>
    dplyr::rename_with(
      ~ stringr::str_replace(., " ", "_")
    ) |>
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
      item_code_cbs = 2562,
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
      by = c("year", "area", "area_code", "item_cbs", "item_code_cbs", "unit")
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
      item_code_cbs,
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
          item_code_cbs,
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
        dplyr::select(item_cbs, item_code_cbs, comm_group),
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
        item_code_cbs,
        element
      )
    )
}

.get_fiber_tobacco <- function(cbs_new, cbs_trade, items) {
  cbs_new |>
    dplyr::rename(
      item_trade = item_cbs,
      item_code_trade = item_code_cbs
    ) |>
    dplyr::left_join(
      cbs_trade |>
        dplyr::select(item_code_trade, item_cbs),
      by = "item_code_trade"
    ) |>
    dplyr::left_join(
      items |> dplyr::select(item_cbs, item_code_cbs),
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
        item_code_cbs
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
  dplyr::bind_rows(
    inputs$fbs_new |>
      dplyr::filter(
        !(element %in%
          c(
            "processing",
            "domestic_supply",
            "stock_variation"
          ) &
          item_code_cbs == 2635),
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
      ) |>
      dplyr::bind_rows(palm_kernels) |>
      dplyr::mutate(source = "FBS_New"),
    inputs$fbs_old |> dplyr::mutate(source = "FBS_Old"),
    dplyr::bind_rows(
      inputs$cbs_animals,
      inputs$cbs_crops,
      fiber_tobacco
    ) |>
      dplyr::mutate(source = "CBS"),
    dplyr::bind_rows(
      inputs$primary_cbs,
      inputs$crop_residues
    ) |>
      dplyr::mutate(source = "Primary"),
    traded_res |> dplyr::mutate(source = "Trade")
  ) |>
    dplyr::filter(
      !is.na(year),
      !is.na(area),
      !is.na(element)
    ) |>
    (\(df) {
      dplyr::bind_rows(
        df,
        df |>
          dplyr::filter(
            item_cbs %in%
              c(
                "Oil, palm fruit",
                "Hops",
                "Seed cotton",
                "Coconuts",
                "Hemp",
                "Kapok fruit",
                "Linum"
              ),
            source != "trade_hist"
          ) |>
          dplyr::mutate(
            element = "processing_primary"
          )
      )
    })()
}

.select_best_source <- function(cbs_raw_all) {
  # Pivot sources into columns — avoids grouped summarise + nth overhead.
  key_cols <- c("area", "area_code", "year", "item_cbs", "item_code_cbs", "element")

  wide <- cbs_raw_all |>
    dplyr::filter(!is.na(area)) |>
    dplyr::select(dplyr::all_of(key_cols), source, value) |>
    tidyr::pivot_wider(names_from = source, values_from = value)

  # Ensure expected columns exist even if a source is absent
  for (col in c("Primary", "FBS_New", "FBS_Old")) {
    if (!col %in% names(wide)) wide[[col]] <- NA_real_
  }

  # Vectorized row-wise stats across all source columns
  src_cols <- setdiff(names(wide), key_cols)
  src_mat <- as.matrix(wide[src_cols])
  n <- rowSums(!is.na(src_mat))
  s <- rowSums(src_mat, na.rm = TRUE)
  ss <- rowSums(src_mat^2, na.rm = TRUE)

  wide[["n"]] <- n
  wide[["mean_val"]] <- ifelse(n > 0, s / n, NA_real_)
  wide[["sd"]] <- ifelse(n > 1L, sqrt(pmax(0, ss - s^2 / n) / (n - 1)), NA_real_)

  wide |>
    dplyr::mutate(
      cv = dplyr::if_else(
        mean_val != 0 & !is.na(mean_val), sd / mean_val, NA_real_
      ),
      fbs_comp = FBS_New / FBS_Old,
      .use_mean = n == 1 | cv < 0.01 | mean_val == 0 | is.na(fbs_comp),
      .use_fbs_new = fbs_comp > 0.9 & fbs_comp < 1.1,
      value = dplyr::if_else(
        !is.na(Primary), Primary,
        dplyr::if_else(
          .use_mean, mean_val,
          dplyr::if_else(.use_fbs_new, FBS_New, FBS_Old)
        )
      ),
      source = dplyr::if_else(
        !is.na(Primary), "Primary",
        dplyr::if_else(
          .use_mean, "mean",
          dplyr::if_else(.use_fbs_new, "FBS_New", "FBS_Old")
        )
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
      item_code_cbs,
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
    dplyr::select(-dplyr::any_of("source")) |>
    tidyr::complete(
      year,
      tidyr::nesting(
        area,
        area_code,
        item_cbs,
        item_code_cbs,
        element
      )
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

.fill_historical_destinies <- function(
  df,
  primary_area,
  gdp_pop,
  land_wide,
  items
) {
  expected_elements <- c(
    "domestic_supply", "production", "import", "export",
    "food", "feed", "other_uses", "processing",
    "processing_primary", "seed", "stock_variation"
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
      by = c("year", "area", "area_code", "item_cbs", "item_code_cbs")
    ) |>
    dplyr::mutate(seed_rate = seed / area_ha) |>
    .fill_share_columns() |>
    .apply_filled_shares() |>
    .fill_with_proxies(gdp_pop, land_wide) |>
    .finalise_historical(items)
}

.fill_share_columns <- function(df) {
  # Batch-fill all share columns in a single grouped pass.
  # Avoids re-sorting and re-computing group boundaries 6 times.
  cols <- c(
    "food_share", "feed_share", "other_uses_share",
    "processing_share", "processing_primary_share", "seed_rate"
  )
  by_cols <- c("area", "item_cbs")

  # Sort once
  grp_id <- vctrs::vec_group_id(df[by_cols])
  times <- df[["year"]]
  n <- nrow(df)
  ord <- order(grp_id, times)
  inv_ord <- integer(n)
  inv_ord[ord] <- seq_len(n)
  grp_sorted <- grp_id[ord]
  times_sorted <- times[ord]

  # Group boundaries
  breaks <- which(diff(grp_sorted) != 0L)
  starts <- c(1L, breaks + 1L)
  ends <- c(breaks, n)

  for (col in cols) {
    orig_vals <- df[[col]][ord]
    filled <- orig_vals
    sources <- ifelse(is.na(orig_vals), "Gap not filled", "Original")

    for (g in seq_along(starts)) {
      i1 <- starts[g]
      i2 <- ends[g]
      rng <- i1:i2
      ov <- orig_vals[rng]
      tv <- times_sorted[rng]
      m <- length(rng)

      valid <- which(!is.na(ov))
      if (length(valid) == 0L) next
      first_v <- valid[1L]
      last_v <- valid[length(valid)]

      # Carry backward
      if (first_v > 1L) {
        na_left <- which(is.na(ov[seq_len(first_v - 1L)]))
        if (length(na_left) > 0L) {
          filled[i1 - 1L + na_left] <- ov[first_v]
          sources[i1 - 1L + na_left] <- "First value carried backwards"
        }
      }
      # Carry forward
      if (last_v < m) {
        tail_idx <- (last_v + 1L):m
        na_right <- tail_idx[is.na(ov[tail_idx])]
        if (length(na_right) > 0L) {
          filled[i1 - 1L + na_right] <- ov[last_v]
          sources[i1 - 1L + na_right] <- "Last value carried forward"
        }
      }
      # Interpolate
      if (length(valid) >= 2L) {
        mid_idx <- (first_v + 1L):(last_v - 1L)
        if (length(mid_idx) > 0L) {
          na_mid <- mid_idx[is.na(ov[mid_idx])]
          if (length(na_mid) > 0L) {
            interp <- .safe_na_approx(ov, x = tv, na.rm = FALSE)
            if (length(interp) == m) {
              fill_mask <- na_mid[!is.na(interp[na_mid])]
              if (length(fill_mask) > 0L) {
                filled[i1 - 1L + fill_mask] <- interp[fill_mask]
                sources[i1 - 1L + fill_mask] <- "Linear interpolation"
              }
            }
          }
        }
      }
      # Preserve originals
      has_orig <- which(!is.na(ov))
      filled[i1 - 1L + has_orig] <- ov[has_orig]
      sources[i1 - 1L + has_orig] <- "Original"
    }

    df[[col]] <- filled[inv_ord]
    df[[paste0("source_", col)]] <- sources[inv_ord]
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
      item_code_cbs,
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
        dplyr::select(item_cbs, item_code_cbs, comm_group),
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
          dplyr::select(item_cbs, item_code_cbs, group),
        by = c("item_cbs", "item_code_cbs")
      ) |>
      dplyr::filter(
        !(group == "Crop products" &
          element == "production")
      ),
    proc_result$processed_agg |>
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
      item_code_cbs,
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
    dplyr::distinct(year, area_code, item_code_cbs, element,
                    .keep_all = TRUE) |>
    dplyr::select(year, area_code, item_code_cbs, element, source)
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
        "item_code_cbs"
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
        item_code_cbs,
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
    dplyr::select(-dplyr::any_of(c("has_np", "item_code_cbs"))) |>
    dplyr::left_join(
      dplyr::distinct(
        cbs_raw2,
        item_cbs,
        item_code_cbs
      ),
      by = "item_cbs"
    ) |>
    dplyr::left_join(
      src_lookup,
      by = c(
        "year",
        "area_code",
        "item_code_cbs",
        "element"
      )
    )
}

# -- Impute trade + domestic supply --------------------------------------------

.cbs_impute_trade <- function(cbs_raw3) {
  # Save source provenance before pivot cycle
  src_lookup <- cbs_raw3 |>
    dplyr::distinct(year, area_code, item_code_cbs, element,
                    .keep_all = TRUE) |>
    dplyr::select(year, area_code, item_code_cbs, element, source)

  destiny_list <- c(
    "food",
    "feed",
    "seed",
    "other_uses",
    "processing",
    "processing_primary"
  )

  cbs_raw3 |>
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
      domestic_supply = food +
        other_uses +
        feed +
        processing +
        processing_primary +
        seed,
      net_trade = domestic_supply -
        dplyr::coalesce(production, 0),
      net_export = pmax(-net_trade, 0),
      net_import = pmax(net_trade, 0),
      import = dplyr::coalesce(import, net_import),
      export = dplyr::coalesce(export, net_export),
      production = tidyr::replace_na(production, 0),
      export = tidyr::replace_na(export, 0),
      import = tidyr::replace_na(import, 0),
      stock_variation = tidyr::replace_na(
        stock_variation,
        0
      )
    ) |>
    .reestimate_domestic_supply() |>
    dplyr::mutate(
      food = dplyr::if_else(
        item_code_cbs == 2635,
        domestic_supply,
        food
      )
    ) |>
    dplyr::select(
      year,
      area,
      area_code,
      item_cbs,
      item_code_cbs,
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
    dplyr::rename(
      stock_variation = stock_variation
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
        "item_code_cbs",
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
          !is.na(ds2) & ds2 != 0, ds2,
          dplyr::if_else(
            net_bal1 > 0, net_bal1,
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
      -net_trade,
      -net_export,
      -net_import,
      -prod_calc,
      -ds3
    )
}

# -- Fill destiny gaps ---------------------------------------------------------

.cbs_fill_destinies <- function(cbs_raw4) {
  destiny_list <- c(
    "food", "feed", "seed", "other_uses", "processing", "processing_primary"
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
  items <- whep::items_full

  destiny |>
    dplyr::left_join(
      items |> dplyr::select(item_cbs, comm_group),
      by = "item_cbs"
    ) |>
    dplyr::mutate(
      sum_dests = dplyr::na_if(sum(value, na.rm = TRUE), 0),
      .by = c(year, area, area_code, item_cbs, item_code_cbs)
    ) |>
    dplyr::mutate(
      dest_share = dplyr::case_when(
        comm_group == "Other processing residues" ~ as.numeric(element == "feed"),
        !is.na(sum_dests)                          ~ value / sum_dests
      )
    ) |>
    dplyr::select(-sum_dests, -comm_group)
}

# Fill dest_share across time using a sparse skeleton:
# interpolate/extrapolate shares at all years with domestic_supply data.
.interpolate_destiny_shares <- function(balance, destiny) {
  by_cols <- c("area", "area_code", "item_cbs", "item_code_cbs", "element", "elem_cat")

  # Target years: where domestic_supply exists (shares needed here)
  ds_keys <- balance |>
    dplyr::filter(element == "domestic_supply") |>
    dplyr::distinct(year, area, area_code, item_cbs, item_code_cbs)

  dest_elem <- dplyr::distinct(destiny, area, area_code, item_cbs, item_code_cbs, element, elem_cat)

  target_rows <- dplyr::inner_join(
    ds_keys, dest_elem,
    by = c("area", "area_code", "item_cbs", "item_code_cbs"),
    relationship = "many-to-many"
  )

  # Anchor years: known shares outside the target set (needed for interpolation)
  anchor_rows <- destiny |>
    dplyr::filter(!is.na(dest_share)) |>
    dplyr::select(dplyr::all_of(c("year", by_cols))) |>
    dplyr::anti_join(
      target_rows,
      by = c("year", "area", "area_code", "item_cbs", "item_code_cbs", "element")
    )

  dplyr::bind_rows(target_rows, anchor_rows) |>
    dplyr::left_join(
      destiny |> dplyr::select(year, area, area_code, item_cbs, item_code_cbs,
                                element, dest_share, value, source),
      by = c("year", "area", "area_code", "item_cbs", "item_code_cbs", "element")
    ) |>
    fill_linear(dest_share, time_col = year, .by = by_cols)
}

# Apply filled destiny shares to domestic supply and return final CBS rows.
.assemble_cbs_destinies <- function(balance, destiny_filled) {
  dplyr::bind_rows(balance, destiny_filled) |>
    .add_global_destiny_shares() |>
    dplyr::mutate(
      value = dplyr::if_else(
        elem_cat == "destiny",
        dplyr::coalesce(
          domestic_supply * dest_share,
          domestic_supply * dest_share_global
        ),
        tidyr::replace_na(value, 0)
      ),
      value = tidyr::replace_na(value, 0)
    ) |>
    dplyr::select(year, area, area_code, item_cbs, item_code_cbs, element, source, value) |>
    dplyr::filter(value != 0, area != "")
}

.add_global_destiny_shares <- function(df) {
  global_shares <- df |>
    dplyr::summarise(
      value = sum(value, na.rm = TRUE),
      .by = c(
        year,
        item_cbs,
        item_code_cbs,
        element,
        elem_cat
      )
    ) |>
    dplyr::mutate(
      dest_share_global = value /
        sum(
          value,
          na.rm = TRUE
        ),
      .by = c(year, item_cbs, item_code_cbs, elem_cat)
    ) |>
    dplyr::select(-value)

  ds <- df |>
    dplyr::filter(element == "domestic_supply") |>
    dplyr::select(
      year,
      area,
      area_code,
      item_cbs,
      item_code_cbs,
      domestic_supply = value
    )

  df |>
    dplyr::left_join(
      global_shares,
      by = c(
        "year",
        "item_cbs",
        "item_code_cbs",
        "element",
        "elem_cat"
      )
    ) |>
    dplyr::left_join(
      ds,
      by = c(
        "year",
        "area",
        "area_code",
        "item_cbs",
        "item_code_cbs"
      )
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
        is.na(scaling_raw), 1,
        dplyr::if_else(
          source_scaling_raw == "Original", scaling_raw,
          dplyr::if_else(
            scaling_raw > 5, 5,
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

  join_keys <- c("year", "area", "area_code", "item_cbs", "item_code_cbs", "element")

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
      items |> dplyr::select(item_cbs, item_code_cbs),
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
    dplyr::distinct(year, area_code, item_code_cbs, element,
                    .keep_all = TRUE) |>
    dplyr::select(year, area_code, item_code_cbs, element, source)

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
        item_code_cbs,
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
      .by = c(year, area_code, item_code_cbs)
    )

  needed_keys <- reclass_agg |>
    dplyr::filter(reclassified_value != 0) |>
    dplyr::select(year, area_code, item_code_cbs) |>
    dplyr::inner_join(
      cbs_filtered |>
        dplyr::distinct(year, area, area_code, item_cbs, item_code_cbs),
      by = c("year", "area_code", "item_code_cbs")
    )

  for (elem in c("processing", "other_uses")) {
    missing <- needed_keys |>
      dplyr::anti_join(
        cbs_filtered |> dplyr::filter(element == elem),
        by = c("year", "area_code", "item_code_cbs")
      ) |>
      dplyr::mutate(element = elem, value = 0)
    cbs_filtered <- dplyr::bind_rows(cbs_filtered, missing)
  }

  cbs_filtered |>
    dplyr::left_join(
      reclass_agg,
      by = c("year", "area_code", "item_code_cbs")
    ) |>
    dplyr::mutate(
      reclassified_value = tidyr::replace_na(
        reclassified_value,
        0
      ),
      value2 = dplyr::if_else(
        element == "processing", value - reclassified_value,
        dplyr::if_else(
          element == "other_uses", value + reclassified_value, value
        )
      )
    ) |>
    dplyr::filter(value2 != 0, area != "") |>
    dplyr::left_join(
      src_lookup,
      by = c(
        "year",
        "area_code",
        "item_code_cbs",
        "element"
      )
    ) |>
    dplyr::select(
      year,
      area,
      area_code,
      item_cbs,
      item_code_cbs,
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
    dplyr::distinct(year, area_code, item_code_cbs, element,
                    .keep_all = TRUE) |>
    dplyr::select(year, area_code, item_code_cbs, element, source)

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
        "item_code_cbs",
        "element"
      )
    )
}
