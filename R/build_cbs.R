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
  max_year = 2021,
  version = NULL,
  smooth_carry_forward = FALSE
) {
  read_cbs(primary_all, max_year, version) |>
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
#' @param max_year Integer. Latest year of FAOSTAT data. Default `2021`.
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
  max_year = 2021,
  version = NULL
) {
  years <- 1850:max_year

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

  # Attach context for downstream pipeline steps
  attr(cbs_raw, ".years") <- years
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
#' @param max_year Integer. Latest year. Default `2021`.
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
  max_year = 2021,
  version = NULL
) {
  cb_proc <- whep::cb_processing

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
    version
  )
  fbs_old <- .extract_cb(
    "faostat-fbs-old",
    version
  )
  cbs_crops <- .extract_cb(
    "faostat-cbs-old-crops",
    version
  )
  cbs_animals <- .extract_cb(
    "faostat-cbs-old-animal",
    version
  )
  cbs_new <- .extract_fao(
    "faostat-cbs-new",
    version
  )

  # Processed production
  prod_processed <- whep_read_file(
    "faostat-production-processed",
    version = version
  )

  # Trade
  fao_trade <- .read_fao_trade(version)
  trade_hist <- .read_historical_trade(version)

  # GDP / population
  gdp_pop <- whep_read_file(
    "gdp-population",
    version = version
  ) |>
    dplyr::rename(year = Year)

  # Primary production as CBS
  primary_cbs <- .primary_to_cbs(primary_all)
  primary_cbs_area <- .primary_to_cbs_area(primary_all)

  # Crop residues
  crop_residues <- .read_crop_residues(version)

  # Land areas
  land_areas_wide <- .read_land_areas_wide(version)

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

.read_fao_trade <- function(version) {
  .extract_fao("faostat-trade", version) |>
    dplyr::rename(
      item_trade = item_cbs,
      item_code_trade = item_code_cbs
    )
}

.read_historical_trade <- function(version) {
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

.read_crop_residues <- function(version) {
  items_prod <- whep::items_prod_full
  polities <- whep::polities_cats

  res <- get_primary_residues(version = version)

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

.read_land_areas_wide <- function(version) {
  land_areas <- .read_land_areas(version)
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
      land_use = dplyr::case_when(
        Land_Use %in% varnames_cropland ~ "Cropland",
        Land_Use %in% varnames_pasture ~ "Pasture",
        TRUE ~ "Other"
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
  cbs_stat <- cbs_raw_all |>
    dplyr::summarise(
      n = dplyr::n(),
      value = mean(value, na.rm = TRUE),
      sd = dplyr::if_else(
        dplyr::n() > 1,
        stats::sd(value, na.rm = TRUE),
        NA_real_
      ),
      .by = c(
        area,
        area_code,
        year,
        item_cbs,
        item_code_cbs,
        element
      )
    ) |>
    dplyr::mutate(
      cv = dplyr::if_else(
        value != 0 & !is.na(value),
        sd / value,
        NA_real_
      )
    )

  cbs_raw_all |>
    dplyr::mutate(unit = "tonnes") |>
    tidyr::pivot_wider(
      names_from = source,
      values_from = value,
      values_fill = NA
    ) |>
    dplyr::left_join(
      cbs_stat |>
        dplyr::select(
          area,
          area_code,
          item_cbs,
          item_code_cbs,
          element,
          year,
          value,
          sd,
          cv,
          n
        ),
      by = c(
        "area",
        "area_code",
        "item_cbs",
        "item_code_cbs",
        "element",
        "year"
      )
    ) |>
    dplyr::mutate(
      fbs_comp = FBS_New / FBS_Old,
      value_sel = dplyr::case_when(
        !is.na(Primary) ~ Primary,
        n == 1 | cv < 0.01 | value == 0 | is.na(fbs_comp) ~ value,
        fbs_comp > 0.9 & fbs_comp < 1.1 ~ FBS_New,
        TRUE ~ FBS_Old
      ),
      source = dplyr::case_when(
        !is.na(Primary) ~ "Primary",
        n == 1 | cv < 0.01 | value == 0 | is.na(fbs_comp) ~ "mean",
        fbs_comp > 0.9 & fbs_comp < 1.1 ~ "FBS_New",
        TRUE ~ "FBS_Old"
      )
    ) |>
    dplyr::filter(!is.na(area)) |>
    dplyr::select(
      area,
      area_code,
      item_cbs,
      item_code_cbs,
      element,
      year,
      source,
      value = value_sel
    ) |>
    dplyr::mutate(
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
  df |>
    tidyr::pivot_wider(
      names_from = element,
      values_from = value
    ) |>
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
  df |>
    fill_linear(
      food_share,
      time_col = year,
      .by = c("area", "item_cbs")
    ) |>
    fill_linear(
      feed_share,
      time_col = year,
      .by = c("area", "item_cbs")
    ) |>
    fill_linear(
      other_uses_share,
      time_col = year,
      .by = c("area", "item_cbs")
    ) |>
    fill_linear(
      processing_share,
      time_col = year,
      .by = c("area", "item_cbs")
    ) |>
    fill_linear(
      processing_primary_share,
      time_col = year,
      .by = c("area", "item_cbs")
    ) |>
    fill_linear(
      seed_rate,
      time_col = year,
      .by = c("area", "item_cbs")
    )
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
      by = "item_cbs"
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
    dplyr::filter(year %in% years) |>
    dplyr::select(
      year,
      area,
      area_code,
      item_cbs,
      item_code_cbs,
      element,
      source,
      value
    ) |>
    dplyr::filter(!is.na(area), !is.na(element)) |>
    dplyr::summarise(
      value = mean(value, na.rm = TRUE),
      source = dplyr::first(source),
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

# -- Redistribute non-processed ------------------------------------------------

.cbs_redistribute_notprocessed <- function(
  cbs_raw2,
  processd_raw
) {
  # Save source provenance before internal processing
  src_lookup <- cbs_raw2 |>
    dplyr::select(
      year,
      area_code,
      item_code_cbs,
      element,
      source
    ) |>
    dplyr::distinct()
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
    dplyr::select(
      year,
      area_code,
      item_code_cbs,
      element,
      source
    ) |>
    dplyr::distinct()

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
      ds3 = dplyr::case_when(
        !is.na(domestic_supply) &
          domestic_supply != 0 ~
          domestic_supply,
        !is.na(ds2) & ds2 != 0 ~ ds2,
        net_bal1 > 0 ~ net_bal1,
        net_bal2 > 0 ~ net_bal2,
        TRUE ~ 0
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
  items <- whep::items_full
  destiny_list <- c(
    "food",
    "feed",
    "seed",
    "other_uses",
    "processing",
    "processing_primary"
  )

  cbs_all <- cbs_raw4 |>
    dplyr::mutate(
      elem_cat = dplyr::if_else(
        element %in% destiny_list,
        "destiny",
        "balance"
      )
    ) |>
    dplyr::left_join(
      items |> dplyr::select(item_cbs, comm_group),
      by = "item_cbs"
    ) |>
    dplyr::mutate(
      sum_dests = dplyr::if_else(
        sum(value, na.rm = TRUE) == 0,
        NA_real_,
        sum(value, na.rm = TRUE)
      ),
      dest_share = dplyr::case_when(
        comm_group == "Other processing residues" &
          element == "feed" ~
          1,
        comm_group == "Other processing residues" ~ 0,
        !is.na(sum_dests) ~ value / sum_dests,
        TRUE ~ NA_real_
      ),
      .by = c(
        year,
        area,
        area_code,
        item_cbs,
        item_code_cbs,
        elem_cat
      )
    ) |>
    tidyr::complete(
      year,
      tidyr::nesting(
        area,
        area_code,
        item_cbs,
        item_code_cbs,
        element,
        elem_cat
      )
    ) |>
    fill_linear(
      dest_share,
      time_col = year,
      .by = c(
        "area",
        "area_code",
        "item_cbs",
        "item_code_cbs",
        "element",
        "elem_cat"
      )
    ) |>
    .add_global_destiny_shares() |>
    dplyr::mutate(
      value2 = dplyr::if_else(
        elem_cat == "destiny",
        dplyr::coalesce(
          domestic_supply * dest_share,
          domestic_supply * dest_share_global
        ),
        tidyr::replace_na(value, 0)
      ),
      value2 = tidyr::replace_na(value2, 0)
    )

  cbs_all |>
    dplyr::select(
      year,
      area,
      area_code,
      item_cbs,
      item_code_cbs,
      element,
      source,
      value = value2
    ) |>
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
      scaling = dplyr::case_when(
        is.na(scaling_raw) ~ 1,
        source_scaling_raw == "Original" ~ scaling_raw,
        scaling_raw > 5 ~ 5,
        scaling_raw < 0.2 ~ 0.2,
        TRUE ~ scaling_raw
      ),
      value_final = value_proc * scaling
    )

  processed_new_bal <- .build_new_processed_balance(
    processed_agg_raw2,
    cbs_glob
  )

  cbs_raw5 |>
    dplyr::bind_rows(
      processed_new_bal |>
        dplyr::mutate(source = "Processed_round2")
    ) |>
    dplyr::summarise(
      value = sum(value, na.rm = TRUE),
      source = dplyr::first(source),
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
    dplyr::select(
      year,
      area_code,
      item_code_cbs,
      element,
      source
    ) |>
    dplyr::distinct()

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

  cbs_raw6 |>
    dplyr::filter(!is.na(element)) |>
    dplyr::select(-dplyr::any_of("source")) |>
    tidyr::pivot_wider(
      names_from = element,
      values_from = value,
      values_fill = 0,
      names_sort = TRUE
    ) |>
    tidyr::pivot_longer(
      domestic_supply:dplyr::last_col(),
      names_to = "element",
      values_to = "value"
    ) |>
    dplyr::left_join(
      reclassify_proc |>
        dplyr::summarise(
          reclassified_value = sum(reclassified_value, na.rm = TRUE),
          .by = c(year, area_code, item_code_cbs)
        ),
      by = c("year", "area_code", "item_code_cbs")
    ) |>
    dplyr::mutate(
      reclassified_value = tidyr::replace_na(
        reclassified_value,
        0
      ),
      value2 = dplyr::case_when(
        element == "processing" ~ value - reclassified_value,
        element == "other_uses" ~ value + reclassified_value,
        TRUE ~ value
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
    dplyr::select(
      year,
      area_code,
      item_code_cbs,
      element,
      source
    ) |>
    dplyr::distinct()

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
