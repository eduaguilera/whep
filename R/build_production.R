#' Build primary production dataset
#'
#' @description
#' Construct the full primary production dataset from raw FAOSTAT inputs.
#' This is a convenience wrapper that chains the three pipeline steps:
#'
#' 1. `.read_production()` — read & reformat FAOSTAT data.
#' 2. `.fix_production()` — apply Global-ported corrections.
#' 3. `.qc_production()` — flag data-quality anomalies.
#'
#' @param start_year Integer. First year to include. Default `1850`.
#' @param end_year Integer. Last year to include. Default `2021`.
#' @param version File version for input pins. `NULL` (default) uses the
#'   frozen version from [whep_inputs].
#' @param smooth_carry_forward Logical. If `TRUE`, carry-forward tails
#'   are replaced with a linear trend. Default `FALSE`.
#'
#' @returns A tibble in long format (see `.read_production()` for column
#'   descriptions), plus a character `qc_flag` column from
#'   `.qc_production()`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' primary <- build_primary_production()
#' }
build_primary_production <- function(
  start_year = 1850,
  end_year = 2021,
  version = NULL,
  smooth_carry_forward = FALSE
) {
  cli::cli_h1("Building primary production")
  .read_production(start_year, end_year, version) |>
    .fix_production() |>
    .qc_production(smooth = smooth_carry_forward)
}


#' Step 1: Read and reformat FAOSTAT production data
#'
#' @description
#' Read raw FAOSTAT inputs (crops, livestock, fodder, emission-based
#' stocks), gap-fill yields, handle double products, extend back to 1850
#' using LUH2 land-use areas, and add grassland.
#'
#' No data corrections are applied at this stage. The output preserves
#' FAOSTAT values as-is (including known issues such as un-corrected tea
#' tonnages and the absence of game-meat stocks). For the corrected
#' version, pipe into `.fix_production()`.
#'
#' @param start_year Integer. First year to include. Default `1850`.
#' @param end_year Integer. Last year to include. Default `2021`.
#' @param version File version for input pins. `NULL` (default) uses the
#'   frozen version from [whep_inputs].
#'
#' @returns A tibble in long format with columns:
#'   `year`, `area`, `area_code`, `item_prod`, `item_code_prod`,
#'   `item_cbs`, `item_code_cbs`, `Live_anim`, `Live_anim_code`,
#'   `unit`, `value`, `source`.
#'
#'   The `source` column indicates data provenance:
#'   `"FAOSTAT"` (original), `"EU_AgriDB"` (European AgriDB fodder),
#'   `"DM_yield_estimate"` (dry-matter yield imputation),
#'   `"fill_linear"` (interpolation), `"imputed_yield"` (yield × area),
#'   `"imputed_cbs_ratio"` (CBS ratio imputation),
#'   `"LUH2_cropland"` / `"LUH2_agriland"` (LUH2 proxy),
#'   `"fill_linear_historical"` (pre-1962 linear extrapolation),
#'   `"LUH2"` (grassland), `"Estimated"` (double-product estimation).
#'
#' @keywords internal
#' @noRd
.read_production <- function(
  start_year = 1850,
  end_year = 2021,
  version = NULL
) {
  output_years <- start_year:end_year
  years_df <- tibble::tibble(year = output_years)

  # FAOSTAT data begins at 1961. When historical extension is needed,
  # we must also read the FAOSTAT anchor years to extrapolate from.
  # All reads use `years` (which may extend beyond output_years);
  # the output is trimmed to `output_years` at the end.
  needs_historical <- start_year < 1962L
  years <- if (needs_historical) {
    start_year:max(end_year, 1965L)
  } else {
    output_years
  }

  # 1. Read commodity balances (for gap-filling)
  cbs_prod_raw <- .read_cbs_production(years = years, version = version)

  # 2. Read and process FAOSTAT crop/livestock production
  fao_crop_liv <- .read_fao_crop_liv(years = years, version = version)

  # 3. Fodder crops (year 2013 excluded — known bad data in old source)
  fodder <- .build_fodder(fao_crop_liv, years = years, version = version)

  # 4. Combine FAO + fodder (no tea correction — see .fix_production)
  fao_combined <- dplyr::bind_rows(fao_crop_liv, fodder)

  # 5. Livestock stocks
  fao_liv_all <- .build_livestock_stocks(
    fao_combined,
    years = years,
    version = version
  )

  # 6. Primary dataset (crops + livestock, no game meat — see .fix_production)
  primary_raw <- .combine_primary_raw(fao_combined, fao_liv_all)

  # 7. Yield calculation + gap-filling
  yield_all <- .compute_yields(
    primary_raw,
    cbs_prod_raw
  )

  # 8. Assemble to final format (no dissolved-country filter — see .fix_production)
  primary_raw2 <- .assemble_production_raw(yield_all)

  # 9. Historical extension
  land_areas <- .read_land_areas(years = years, version = version)
  int_yields <- .read_int_yields(years = years, version = version)

  primary_ext <- .extend_historical(
    primary_raw2,
    years_df,
    land_areas
  )

  # 10. Add grassland + historical yields
  grassland <- .build_grassland(land_areas)

  primary_ext |>
    dplyr::bind_rows(grassland) |>
    .add_historical_yields(int_yields) |>
    .finalise_primary() |>
    .filter_years(output_years)
}


#' Step 2: Apply Global-ported corrections to production data
#'
#' @description
#' Applies the data corrections that were originally in
#' `Global/R/crop_liv_prod.r`. These modify values but do not add
#' QC metadata. Each correction is documented inline.
#'
#' **Corrections applied:**
#' * **Tea ÷ 4.37** — FAOSTAT reports tea in fresh-leaf weight;
#'   this converts to made-tea weight for years after 1990. Affects
#'   both `tonnes` and yield units (`t_ha`, `t_head`, `t_LU`).
#' * **Game meat stocks** — creates synthetic `LU` and `heads` rows
#'   for item "Game" (1190) from game-meat production tonnes (1163).
#' * **Dissolved countries** — removes overlapping country/year
#'   observations (e.g. Czechoslovakia after 1992).
#'
#' @param df A tibble from `.read_production()`.
#'
#' @returns The same tibble with corrected values.
#'
#' @keywords internal
#' @noRd
.fix_production <- function(df) {
  df |>
    .correct_tea_final() |>
    .add_game_meat_final() |>
    .filter_dissolved_countries()
}


#' Step 3: Flag production data-quality anomalies
#'
#' @description
#' Detects and optionally smooths data-quality issues in the production
#' dataset. Adds a `qc_flag` character column.
#'
#' **Flags applied:**
#' * `carry_forward` — constant-value tail (≥ `min_run` identical final
#'   years). Likely FAOSTAT carry-forward imputation.
#' * `spike` — year-on-year change exceeding `spike_ratio`.
#' * `fodder_break` — the 1984/1985 EU fodder reporting discontinuity.
#'
#' @param df A tibble from `.fix_production()` (or `.read_production()`).
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
.qc_production <- function(
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
    "item_prod",
    "item_code_prod",
    "unit"
  )

  df <- df |>
    .flag_carry_forward(by = by_cols, min_run = min_run) |>
    .flag_spikes(
      by = by_cols,
      spike_ratio = spike_ratio,
      min_value = spike_min
    ) |>
    .flag_fodder_break(item_col = "item_prod")

  if (smooth) {
    df <- .smooth_carry_forward(
      df,
      by = by_cols,
      anchor_years = anchor_years
    )
  }

  df <- .collapse_qc_flags(df)
  .qc_summary(df, "Primary Production")
  df
}

# -- Input reading helpers -----------------------------------------------------

.read_cbs_production <- function(years = NULL, version = NULL) {
  cli::cli_progress_step("Reading CBS production")
  fbs_new <- .extract_cb("faostat-fbs-new", years = years, version = version)
  fbs_old <- .extract_cb("faostat-fbs-old", years = years, version = version)
  cbs_anim <- .extract_cb(
    "faostat-cbs-old-animal",
    years = years,
    version = version
  )
  cbs_crops <- .extract_cb(
    "faostat-cbs-old-crops",
    years = years,
    version = version
  )

  dplyr::bind_rows(fbs_new, fbs_old, cbs_anim, cbs_crops) |>
    dplyr::filter(element == "production") |>
    dplyr::summarise(
      t_cbs = mean(value, na.rm = TRUE),
      .by = c(
        area,
        area_code,
        year,
        item_cbs,
        item_code_cbs,
        element
      )
    ) |>
    dplyr::select(
      year,
      area_code,
      item_code_cbs,
      item_cbs,
      t_cbs
    )
}

.read_fao_crop_liv <- function(years = NULL, version = NULL) {
  cli::cli_progress_step("Reading FAO crops/livestock")
  whep_read_file("faostat-production", version = version) |>
    dplyr::rename(
      item_code_prod = `Item Code`,
      item_prod = Item,
      area_code = `Area Code`,
      unit = Unit,
      element = Element,
      year = Year,
      value = Value
    ) |>
    .filter_years(years) |>
    dplyr::mutate(item_code_prod = as.character(item_code_prod)) |>
    .aggregate_to_polities(
      item_code_prod,
      item_prod
    ) |>
    dplyr::arrange(
      year,
      area,
      area_code,
      item_code_prod,
      item_prod,
      element,
      unit
    )
}

.read_land_areas <- function(years = NULL, version = NULL) {
  cli::cli_progress_step("Reading land areas")
  regions <- whep::regions_full

  whep_read_file("luh2-areas", version = version) |>
    dplyr::rename(iso3c = ISO3, year = Year) |>
    .filter_years(years) |>
    dplyr::left_join(
      regions |>
        dplyr::select(iso3c, area = polity_name, area_code = polity_code),
      by = "iso3c"
    ) |>
    dplyr::filter(year > 1849)
}

.read_int_yields <- function(years = NULL, version = NULL) {
  cli::cli_progress_step("Reading international yields")
  regions <- whep::regions_full

  whep_read_file("international-yields", version = version) |>
    .filter_years(years) |>
    dplyr::mutate(item_code_prod = as.character(item_code)) |>
    dplyr::rename(
      code = area_code,
      year = year
    ) |>
    dplyr::select(-item_code) |>
    dplyr::filter(
      year < 1962,
      !is.na(yield),
      yield != 0,
      yield < 100
    ) |>
    dplyr::left_join(
      regions |>
        dplyr::select(code, polity_name),
      by = "code"
    ) |>
    dplyr::rename(area = polity_name) |>
    dplyr::summarise(
      yield = mean(yield, na.rm = TRUE),
      .by = c(year, area, item_code_prod)
    ) |>
    dplyr::filter(!is.nan(yield))
}

# -- Fodder --------------------------------------------------------------------

.build_fodder <- function(fao_crop_liv, years = NULL, version = NULL) {
  cli::cli_progress_step("Building fodder dataset")
  items_prod <- whep::items_prod_full
  items <- whep::items_full
  crops_eu <- whep::crops_eurostat
  biomass <- whep::biomass_coefs
  regions <- whep::regions_full

  # Old FAO fodder data
  i_fodder <- .read_fodder_old(years = years, version = version)

  # EU AgriDB fodder
  fodder_euadb <- .read_fodder_euadb(years = years, version = version)

  # DM yields for imputing fodder areas
  dm_yield <- .compute_dm_yield(
    fao_crop_liv,
    items_prod,
    biomass
  )

  # Build complete fodder dataset
  .combine_fodder(
    i_fodder,
    fodder_euadb,
    dm_yield,
    items_prod,
    biomass
  )
}

.read_fodder_old <- function(years = NULL, version = NULL) {
  items_prod <- whep::items_prod_full
  items <- whep::items_full

  whep_read_file("faostat-production-old", version = version) |>
    dplyr::rename(
      area_code = AreaCode,
      item_code_prod = ItemCode,
      item_prod = ItemName,
      year = Year,
      value = Value
    ) |>
    .filter_years(years) |>
    dplyr::mutate(
      element = "production",
      unit = "t",
      item_code_prod = as.character(item_code_prod)
    ) |>
    .aggregate_to_polities(
      item_code_prod,
      item_prod
    ) |>
    dplyr::left_join(
      items_prod |> dplyr::select(item_code_prod, item_cbs),
      by = "item_code_prod"
    ) |>
    dplyr::left_join(
      items |> dplyr::select(item_cbs, Cat_1),
      by = "item_cbs"
    ) |>
    dplyr::filter(Cat_1 == "Fodder_green")
}

.read_fodder_euadb <- function(years = NULL, version = NULL) {
  crops_eu <- whep::crops_eurostat
  regions <- whep::regions_full

  whep_read_file("eu-agridb-fodder", version = version) |>
    dplyr::rename(year = Year) |>
    .filter_years(years) |>
    dplyr::left_join(crops_eu, by = "Crop") |>
    dplyr::rename(adb_region = Region) |>
    dplyr::left_join(
      regions |>
        dplyr::select(
          adb_region = ADB_Region,
          polity_name,
          polity_code
        ),
      by = "adb_region"
    ) |>
    dplyr::rename(
      area_code = polity_code,
      area = polity_name
    ) |>
    dplyr::select(
      year,
      area,
      area_code,
      Name_Eurostat,
      Label,
      Unit,
      value = Value
    )
}

.compute_dm_yield <- function(fao_crop_liv, items_prod, biomass) {
  crops_dm <- items_prod |>
    dplyr::left_join(
      biomass |> dplyr::select(Name_biomass, Product_kgDM_kgFM),
      by = "Name_biomass"
    ) |>
    dplyr::select(item_code_prod, Product_kgDM_kgFM) |>
    dplyr::filter(!is.na(Product_kgDM_kgFM))

  fao_crop_liv |>
    dplyr::filter(unit %in% c("ha", "t")) |>
    tidyr::pivot_wider(
      names_from = "unit",
      values_from = "value"
    ) |>
    dplyr::left_join(crops_dm, by = "item_code_prod") |>
    dplyr::mutate(dm = t * Product_kgDM_kgFM) |>
    dplyr::summarise(
      ha = sum(ha, na.rm = TRUE),
      dm = sum(dm, na.rm = TRUE),
      .by = c(year, area, area_code)
    ) |>
    dplyr::mutate(yield_dm = dm * 4 / ha)
}

.combine_fodder <- function(
  i_fodder,
  fodder_euadb,
  dm_yield,
  items_prod,
  biomass
) {
  crops_dm <- items_prod |>
    dplyr::left_join(
      biomass |> dplyr::select(Name_biomass, Product_kgDM_kgFM),
      by = "Name_biomass"
    ) |>
    dplyr::select(item_code_prod, Product_kgDM_kgFM) |>
    dplyr::filter(!is.na(Product_kgDM_kgFM))

  fodder_all <- i_fodder |>
    dplyr::filter(year != 2013) |>
    dplyr::mutate(
      value = dplyr::if_else(value == 0, NA_real_, value)
    ) |>
    dplyr::right_join(
      dm_yield |> dplyr::select(year, area_code, yield_dm),
      by = c("year", "area_code")
    ) |>
    dplyr::left_join(crops_dm, by = "item_code_prod") |>
    dplyr::rename(t = value) |>
    dplyr::mutate(
      t_dm = t * Product_kgDM_kgFM,
      ha = t_dm / yield_dm
    ) |>
    .merge_euadb_fodder(fodder_euadb, items_prod) |>
    .fill_fodder_gaps(dm_yield, items_prod, biomass)

  fodder_all |>
    dplyr::select(
      year,
      area,
      area_code,
      item_prod,
      item_code_prod,
      source,
      t_2,
      ha
    ) |>
    dplyr::rename(t = t_2) |>
    tidyr::pivot_longer(
      t:ha,
      names_to = "unit",
      values_to = "value"
    )
}

.merge_euadb_fodder <- function(
  fodder,
  fodder_euadb,
  items_prod
) {
  euadb_area <- fodder_euadb |>
    dplyr::filter(Unit == "Mha") |>
    dplyr::mutate(ha_euadb = value * 1e6) |>
    dplyr::select(
      year,
      area,
      area_code,
      Name_Eurostat,
      ha_euadb
    ) |>
    dplyr::left_join(
      items_prod |>
        dplyr::select(item_prod, item_code_prod, Name_Eurostat),
      by = "Name_Eurostat"
    )

  euadb_yield <- fodder_euadb |>
    dplyr::filter(Label == "Yield") |>
    dplyr::mutate(kgnha_euadb = value) |>
    dplyr::select(year, area, Name_Eurostat, kgnha_euadb)

  fodder |>
    dplyr::full_join(
      euadb_area,
      by = c(
        "year",
        "area",
        "area_code",
        "item_prod",
        "item_code_prod"
      )
    ) |>
    dplyr::left_join(
      euadb_yield,
      by = c("year", "area", "Name_Eurostat")
    ) |>
    dplyr::mutate(
      ha_tot = sum(ha, na.rm = TRUE),
      .by = c(year, area, area_code)
    ) |>
    dplyr::mutate(
      sum_ha = sum(ha, na.rm = TRUE),
      ha_share = dplyr::if_else(
        ha_tot == 0,
        NA_real_,
        dplyr::if_else(sum_ha == 0, 1, ha / sum_ha)
      ),
      .by = c(year, area, area_code, Name_Eurostat)
    )
}

.fill_fodder_gaps <- function(
  fodder,
  dm_yield,
  items_prod,
  biomass
) {
  fodder |>
    dplyr::filter(!is.na(area)) |>
    tidyr::complete(
      year,
      tidyr::nesting(
        area,
        area_code,
        item_prod,
        item_code_prod,
        Name_Eurostat
      )
    ) |>
    fill_linear(
      ha_share,
      time_col = year,
      .by = c(
        "area",
        "area_code",
        "item_prod",
        "item_code_prod",
        "Name_Eurostat"
      )
    ) |>
    fill_linear(
      kgnha_euadb,
      time_col = year,
      .by = c(
        "area",
        "area_code",
        "item_prod",
        "item_code_prod",
        "Name_Eurostat"
      )
    ) |>
    dplyr::mutate(
      ha = dplyr::if_else(
        is.na(ha_euadb),
        ha,
        ha_euadb * ha_share
      ),
      .by = c(
        area,
        area_code,
        item_prod,
        item_code_prod,
        Name_Eurostat
      )
    ) |>
    fill_linear(
      ha,
      time_col = year,
      .by = c(
        "area",
        "area_code",
        "item_prod",
        "item_code_prod",
        "Name_Eurostat"
      )
    ) |>
    dplyr::select(
      year,
      area,
      area_code,
      item_prod,
      item_code_prod,
      t,
      t_dm,
      ha_share,
      ha,
      kgnha_euadb
    ) |>
    dplyr::left_join(
      dm_yield |> dplyr::select(year, area_code, yield_dm),
      by = c("year", "area_code")
    ) |>
    dplyr::left_join(
      items_prod |> dplyr::select(item_prod, Name_biomass),
      by = "item_prod"
    ) |>
    dplyr::left_join(
      biomass |>
        dplyr::select(
          Name_biomass,
          Product_kgDM_kgFM,
          Product_kgN_kgDM
        ),
      by = "Name_biomass"
    ) |>
    dplyr::mutate(
      t_euadb = ha *
        kgnha_euadb /
        (Product_kgN_kgDM * Product_kgDM_kgFM * 1000),
      t_dmbased = ha * yield_dm / Product_kgDM_kgFM,
      t_2 = dplyr::if_else(
        !is.na(t_euadb),
        t_euadb,
        t_dmbased
      ),
      source = dplyr::case_when(
        !is.na(t) ~ "FAOSTAT",
        !is.na(t_euadb) ~ "EU_AgriDB",
        TRUE ~ "DM_yield_estimate"
      )
    ) |>
    dplyr::filter(!is.na(item_prod), !is.na(t_2))
}

.correct_tea <- function(df) {
  df |>
    dplyr::mutate(
      value = dplyr::if_else(
        unit == "t" &
          item_prod == "Tea leaves" &
          year > 1990,
        value / 4.37,
        value
      )
    )
}

# -- Livestock stocks ----------------------------------------------------------

.build_livestock_stocks <- function(
  fao_combined,
  years = NULL,
  version = NULL
) {
  cli::cli_progress_step("Building livestock stocks")
  animals <- whep::animals_codes
  liv_lu <- whep::liv_lu_coefs

  fao_stocks <- .read_livestock_stocks(years = years, version = version)

  fao_liv_raw <- .combine_livestock(
    fao_combined,
    fao_stocks,
    animals
  )

  .finalise_livestock(fao_liv_raw, animals, liv_lu)
}

.read_livestock_stocks <- function(years = NULL, version = NULL) {
  whep_read_file(
    "faostat-emissions-livestock",
    version = version
  ) |>
    dplyr::rename(
      item_code_cbs = `Item Code`,
      item_cbs = Item,
      area_code = `Area Code`,
      unit = Unit,
      element = Element,
      year = Year,
      value = Value
    ) |>
    .filter_years(years) |>
    dplyr::filter(
      element == "Stocks",
      Source == "FAO TIER 1"
    ) |>
    .aggregate_to_polities(
      item_code_cbs,
      item_cbs
    )
}

.combine_livestock <- function(
  fao_combined,
  fao_stocks,
  animals
) {
  fao_combined |>
    dplyr::filter(element == "Stocks") |>
    dplyr::mutate(
      value = dplyr::if_else(
        unit == "1000 An",
        value * 1000,
        value
      )
    ) |>
    dplyr::select(
      year,
      area,
      area_code,
      item_code_prod,
      value
    ) |>
    dplyr::inner_join(
      animals |>
        dplyr::select(
          item_code_cbs,
          item_cbs,
          Item_Code,
          Livestock_name
        ) |>
        dplyr::mutate(item_code_prod = as.character(Item_Code)) |>
        dplyr::select(-Item_Code),
      by = "item_code_prod"
    ) |>
    dplyr::full_join(
      fao_stocks |>
        dplyr::rename(
          value_st = value,
          item_st = item_cbs
        ) |>
        dplyr::select(
          year,
          area,
          item_code_cbs,
          item_st,
          value_st
        ),
      by = c("year", "area", "item_code_cbs")
    ) |>
    dplyr::mutate(
      share = value_st / sum(value_st),
      value_comb = dplyr::if_else(
        is.na(share) | share == 1,
        value,
        value * share
      ),
      .by = c(year, area, item_code_prod)
    ) |>
    dplyr::filter(!is.na(area_code)) |>
    dplyr::mutate(
      n = dplyr::n(),
      .by = c(area, item_cbs, item_code_cbs)
    ) |>
    tidyr::complete(
      year,
      tidyr::nesting(
        area,
        area_code,
        item_cbs,
        item_code_cbs,
        Livestock_name
      )
    ) |>
    fill_linear(
      n,
      time_col = year,
      .by = c(
        "area",
        "area_code",
        "item_cbs",
        "item_code_cbs",
        "Livestock_name"
      )
    ) |>
    dplyr::mutate(
      value = dplyr::if_else(
        !is.na(value_comb),
        value_comb,
        dplyr::if_else(n > 40, NA_real_, 0)
      ),
      .by = c(area, area_code, item_cbs, item_code_cbs, Livestock_name)
    ) |>
    fill_linear(
      value,
      time_col = year,
      .by = c(
        "area",
        "area_code",
        "item_cbs",
        "item_code_cbs",
        "Livestock_name"
      )
    )
}

.finalise_livestock <- function(fao_liv_raw, animals, liv_lu) {
  fao_liv_raw |>
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
      animals |> dplyr::select(item_cbs, Animal_class),
      by = "item_cbs"
    ) |>
    dplyr::left_join(liv_lu, by = "Animal_class") |>
    dplyr::mutate(value_lu = value * LU_head) |>
    (\(df) {
      dplyr::bind_rows(
        df |>
          dplyr::mutate(value = value_lu, unit = "LU"),
        df |>
          dplyr::mutate(unit = "heads")
      )
    })() |>
    dplyr::mutate(element = "Stock") |>
    dplyr::rename(
      item_prod = item_cbs,
      item_code_prod = item_code_cbs
    ) |>
    dplyr::mutate(item_code_prod = as.character(item_code_prod)) |>
    dplyr::mutate(
      source = dplyr::if_else(
        source_value == "Original",
        "FAOSTAT",
        "fill_linear"
      )
    ) |>
    dplyr::select(
      year,
      area,
      area_code,
      item_code_prod,
      item_prod,
      unit,
      value,
      source
    )
}

# -- Primary combination & yields ---------------------------------------------

.combine_primary_raw <- function(fao_combined, fao_liv_all) {
  cli::cli_progress_step("Combining primary raw dataset")
  dplyr::bind_rows(
    fao_combined |>
      dplyr::filter(unit %in% c("ha", "t")) |>
      dplyr::mutate(
        source = dplyr::if_else(
          is.na(source),
          "FAOSTAT",
          source
        )
      ),
    fao_liv_all
  ) |>
    dplyr::summarise(
      value = sum(value),
      source = source[1L],
      .by = c(
        year,
        area,
        area_code,
        item_prod,
        item_code_prod,
        unit
      )
    )
}

.add_game_meat <- function(df) {
  game <- df |>
    dplyr::filter(item_code_prod == 1163) |>
    dplyr::mutate(
      value = value * 3,
      unit = "LU",
      item_prod = "Game",
      item_code_prod = 1190
    )

  game_heads <- game |>
    dplyr::mutate(value = value / 10, unit = "heads")

  dplyr::bind_rows(df, game, game_heads)
}

.compute_yields <- function(primary_raw, cbs_prod_raw) {
  cli::cli_progress_step("Computing yields")
  items_prod <- whep::items_prod_full

  yield_raw <- .calculate_raw_yields(primary_raw, items_prod)
  yield_double <- .handle_double_products(
    yield_raw,
    whep::primary_double
  )

  dplyr::bind_rows(
    yield_raw |>
      dplyr::filter(
        !item_prod %in% whep::primary_double$item_prod
      ),
    yield_double
  ) |>
    .fill_yields(items_prod, cbs_prod_raw)
}

.calculate_raw_yields <- function(primary_raw, items_prod) {
  crop_yield <- primary_raw |>
    dplyr::filter(unit %in% c("ha", "t")) |>
    tidyr::pivot_wider(
      id_cols = c(year, area, area_code, item_prod, item_code_prod),
      names_from = unit,
      values_from = value
    ) |>
    dplyr::mutate(yield_c = t / ha, unit = "t_ha") |>
    dplyr::rename(fu = ha)

  liv_yield <- primary_raw |>
    dplyr::filter(unit == "t") |>
    dplyr::select(-unit) |>
    dplyr::right_join(
      items_prod |>
        dplyr::filter(!is.na(Live_anim)) |>
        dplyr::select(
          Live_anim,
          Live_anim_code,
          item_code_prod
        ) |>
        dplyr::mutate(Live_anim_code = as.character(Live_anim_code)),
      by = "item_code_prod"
    ) |>
    dplyr::rename(t = value) |>
    tidyr::complete(
      year,
      tidyr::nesting(
        area,
        area_code,
        item_prod,
        item_code_prod,
        Live_anim,
        Live_anim_code
      )
    ) |>
    dplyr::left_join(
      dplyr::bind_rows(
        primary_raw |>
          dplyr::filter(unit == "LU") |>
          dplyr::rename(Live_anim_code = item_code_prod) |>
          dplyr::select(
            year,
            area,
            area_code,
            Live_anim_code,
            unit,
            value
          ),
        primary_raw |>
          dplyr::filter(unit == "heads") |>
          dplyr::rename(Live_anim_code = item_code_prod) |>
          dplyr::select(
            year,
            area,
            area_code,
            Live_anim_code,
            unit,
            value
          )
      ) |>
        dplyr::summarise(
          value = sum(value, na.rm = TRUE),
          .by = c(year, area, area_code, Live_anim_code, unit)
        ),
      by = c(
        "year",
        "area",
        "area_code",
        "Live_anim_code"
      )
    ) |>
    dplyr::rename(fu = value) |>
    dplyr::mutate(
      yield_c = t / fu,
      unit = dplyr::if_else(
        unit == "LU",
        "t_LU",
        "t_head"
      )
    )

  dplyr::bind_rows(crop_yield, liv_yield) |>
    dplyr::mutate(
      t = dplyr::if_else(t == 0, NA_real_, t),
      fu = dplyr::if_else(fu == 0, NA_real_, fu),
      yield_c = dplyr::if_else(
        yield_c == 0 | is.infinite(yield_c) | is.nan(yield_c),
        NA_real_,
        yield_c
      )
    ) |>
    tidyr::complete(
      year,
      tidyr::nesting(
        area,
        area_code,
        item_prod,
        item_code_prod,
        Live_anim,
        Live_anim_code,
        unit
      )
    )
}

.handle_double_products <- function(yield_raw, primary_double) {
  yield_double_all <- yield_raw |>
    dplyr::right_join(
      primary_double |> dplyr::select(item_prod, Multi_type),
      by = "item_prod"
    ) |>
    .suppress_empty_series() |>
    .build_double_combined(primary_double) |>
    .deduplicate_doubles()

  yield_double_all |>
    dplyr::select(
      year,
      area,
      area_code,
      item_prod,
      item_code_prod,
      Multi_type,
      unit,
      yield_c,
      fu,
      t
    )
}

.suppress_empty_series <- function(df) {
  df |>
    dplyr::mutate(
      check_t = sum(t, na.rm = TRUE),
      check_fu = sum(fu, na.rm = TRUE),
      check = check_t + check_fu,
      .by = c(area, area_code, item_prod, item_code_prod)
    ) |>
    dplyr::filter(check != 0)
}

.build_double_combined <- function(df, primary_double) {
  multi_stripped <- df |>
    dplyr::mutate(
      fu = dplyr::if_else(
        Multi_type == "Multi",
        NA_real_,
        fu
      ),
      yield_c = dplyr::if_else(
        Multi_type == "Multi",
        NA_real_,
        yield_c
      )
    )

  combined <- df |>
    dplyr::filter(Multi_type %in% c("Multi", "Primary")) |>
    dplyr::select(-Multi_type, -item_code_prod) |>
    dplyr::left_join(
      primary_double |>
        dplyr::select(item_prod, Item_area) |>
        dplyr::left_join(
          primary_double |>
            dplyr::select(
              item_prod,
              item_code_prod,
              Multi_type
            ) |>
            dplyr::mutate(
              item_code_prod = as.character(item_code_prod)
            ) |>
            dplyr::rename(Item_area = item_prod),
          by = "Item_area"
        ),
      by = "item_prod"
    ) |>
    dplyr::mutate(item_prod = Item_area) |>
    dplyr::summarise(
      t = sum(t, na.rm = TRUE),
      fu_sum = sum(fu, na.rm = TRUE),
      fu_mean = mean(fu, na.rm = TRUE),
      distinct = dplyr::n_distinct(fu),
      .by = c(
        year,
        item_prod,
        item_code_prod,
        area,
        area_code,
        unit,
        Multi_type
      )
    ) |>
    dplyr::mutate(
      fu = dplyr::if_else(
        distinct == 1,
        fu_mean,
        fu_sum
      ),
      fu = dplyr::if_else(is.nan(fu), NA_real_, fu),
      yield_c = t / fu,
      source = "Estimated"
    )

  dplyr::bind_rows(multi_stripped, combined)
}

.deduplicate_doubles <- function(df) {
  df |>
    dplyr::filter(!is.na(t)) |>
    dplyr::mutate(
      n = dplyr::n(),
      .by = c(
        year,
        area,
        area_code,
        item_prod,
        item_code_prod,
        unit
      )
    ) |>
    dplyr::filter(n == 1 | (n == 2 & is.na(source)))
}

.fill_yields <- function(yield_all, items_prod, cbs_prod_raw) {
  yield_all |>
    fill_linear(
      yield_c,
      time_col = year,
      .by = c(
        "area",
        "area_code",
        "item_prod",
        "item_code_prod",
        "Live_anim_code",
        "unit"
      )
    ) |>
    .add_global_yields() |>
    dplyr::left_join(
      items_prod |>
        dplyr::select(item_prod, item_code_prod, item_code_cbs, group),
      by = c("item_prod", "item_code_prod")
    ) |>
    dplyr::left_join(
      cbs_prod_raw,
      by = c("year", "area_code", "item_code_cbs")
    ) |>
    .compute_cbs_ratios() |>
    .impute_missing_values()
}

.add_global_yields <- function(df) {
  global <- df |>
    dplyr::summarise(
      t = sum(t, na.rm = TRUE),
      fu = sum(fu, na.rm = TRUE),
      .by = c(year, item_code_prod, Live_anim_code, unit)
    ) |>
    dplyr::mutate(
      yield_glo = t / fu,
      yield_glo = dplyr::if_else(
        yield_glo == 0 | is.infinite(yield_glo) | is.nan(yield_glo),
        NA_real_,
        yield_glo
      )
    ) |>
    fill_linear(
      yield_glo,
      time_col = year,
      .by = c("item_code_prod", "Live_anim_code", "unit")
    ) |>
    dplyr::select(
      year,
      item_code_prod,
      Live_anim_code,
      unit,
      yield_glo
    )

  df |>
    dplyr::left_join(
      global,
      by = c(
        "year",
        "item_code_prod",
        "Live_anim_code",
        "unit"
      )
    )
}

.compute_cbs_ratios <- function(df) {
  df |>
    dplyr::mutate(
      prod_cbs_ratio = t / t_cbs,
      prod_cbs_count = dplyr::n(),
      sumprod_cbs_ratio = sum(t, na.rm = TRUE) / t_cbs,
      .by = c(year, area, area_code, item_code_cbs)
    ) |>
    fill_linear(
      prod_cbs_ratio,
      time_col = year,
      .by = c(
        "area_code",
        "item_prod",
        "item_code_prod",
        "Live_anim_code",
        "unit",
        "group"
      )
    )
}

.impute_missing_values <- function(df) {
  df |>
    dplyr::mutate(
      yield = dplyr::if_else(
        !is.na(yield_c),
        yield_c,
        yield_glo
      ),
      t2 = dplyr::if_else(
        is.na(t),
        dplyr::if_else(
          is.na(fu * yield) &
            sumprod_cbs_ratio < 0.9 &
            is.na(Multi_type),
          t_cbs * prod_cbs_ratio,
          fu * yield
        ),
        t
      ),
      fu2 = dplyr::if_else(
        is.na(fu),
        t2 / yield,
        fu
      ),
      source = dplyr::case_when(
        !is.na(source) ~ source,
        !is.na(t) ~ "FAOSTAT",
        !is.na(fu * yield) ~ "imputed_yield",
        sumprod_cbs_ratio < 0.9 &
          is.na(Multi_type) ~
          "imputed_cbs_ratio",
        TRUE ~ "imputed_yield"
      )
    ) |>
    dplyr::mutate(
      remove = dplyr::if_else(
        (is.na(group) | group == "Crop products") |
          (group == "Livestock products" & unit == "t_ha") |
          is.na(t2) |
          t2 == 0,
        "remove",
        NA_character_
      )
    ) |>
    dplyr::filter(is.na(remove))
}

# -- Assembly ------------------------------------------------------------------

.assemble_production_raw <- function(yield_all) {
  cli::cli_progress_step("Assembling production")
  items <- whep::items_full

  ha_df <- yield_all |>
    dplyr::filter(unit == "t_ha") |>
    dplyr::mutate(unit = "ha") |>
    dplyr::select(
      year,
      area,
      area_code,
      item_prod,
      item_code_prod,
      Live_anim,
      Live_anim_code,
      unit,
      source,
      value = fu2
    )

  tonnes_df <- yield_all |>
    dplyr::filter(unit %in% c("t_ha", "t_head")) |>
    dplyr::mutate(unit = "t") |>
    dplyr::select(
      year,
      area,
      area_code,
      item_prod,
      item_code_prod,
      Live_anim,
      Live_anim_code,
      unit,
      source,
      value = t2
    )

  yield_df <- yield_all |>
    dplyr::select(
      year,
      area,
      area_code,
      item_prod,
      item_code_prod,
      Live_anim,
      Live_anim_code,
      unit,
      source,
      value = yield
    )

  live_anim_df <- yield_all |>
    dplyr::filter(unit %in% c("t_LU", "t_head")) |>
    dplyr::summarise(
      value = mean(fu2, na.rm = TRUE),
      source = source[1L],
      .by = c(
        year,
        area,
        area_code,
        Live_anim_code,
        unit
      )
    ) |>
    dplyr::mutate(
      unit = dplyr::case_match(
        unit,
        "t_LU" ~ "LU",
        "t_head" ~ "heads"
      )
    ) |>
    dplyr::rename(item_code_prod = Live_anim_code) |>
    dplyr::left_join(
      items |>
        dplyr::select(item_cbs, item_code_cbs) |>
        dplyr::mutate(item_code_prod = as.character(item_code_cbs)),
      by = "item_code_prod"
    ) |>
    dplyr::mutate(item_prod = item_cbs)

  dplyr::bind_rows(ha_df, tonnes_df, yield_df) |>
    dplyr::select(-item_prod) |>
    dplyr::left_join(
      whep::items_prim |>
        dplyr::select(
          item_prod,
          item_code_prod,
          item_cbs,
          item_code_cbs
        ),
      by = "item_code_prod"
    ) |>
    dplyr::filter(
      !is.na(value),
      !is.infinite(value),
      value != 0
    ) |>
    dplyr::bind_rows(live_anim_df) |>
    dplyr::mutate(
      unit = dplyr::if_else(
        unit == "t",
        "tonnes",
        as.character(unit)
      )
    )
}

.filter_dissolved_countries <- function(df) {
  force(df)
  cli::cli_progress_step("Filtering dissolved countries")
  df |>
    dplyr::filter(
      !(area == "Czechoslovakia" & year > 1992),
      !(area %in%
        c(
          "Czech Republic",
          "Czechia",
          "Slovakia"
        ) &
        year < 1993),
      !(area %in%
        c(
          "Lithuania",
          "Latvia",
          "Estonia",
          "Slovenia",
          "Croatia"
        ) &
        year < 1992),
      !(area == "Belgium-Luxembourg" & year > 1999),
      !(area %in% c("Belgium", "Luxembourg") & year < 2000)
    )
}

# -- Post-processing corrections (used by .fix_production) --------------------

#' Correct tea fresh-leaf weight to made-tea weight (final format)
#' @details FAOSTAT reports tea in fresh-leaf weight. Made-tea weight is
#'   fresh / 4.37. Applied to `tonnes` and yield units for "Tea leaves"
#'   from 1991 onward.
#' @keywords internal
#' @noRd
.correct_tea_final <- function(df) {
  force(df)
  cli::cli_progress_step("Correcting tea weights")
  tea_units <- c("tonnes", "t_ha", "t_head", "t_LU")
  df |>
    dplyr::mutate(
      value = dplyr::if_else(
        item_prod == "Tea leaves" &
          year > 1990 &
          unit %in% tea_units,
        value / 4.37,
        value
      )
    )
}

#' Add game-meat livestock units and heads (final format)
#' @details Creates `LU` and `heads` rows for item "Game" (1190) from
#'   game-meat production tonnes (item 1163). Conversion:
#'   LU = tonnes * 3, heads = tonnes * 3 / 10.
#' @keywords internal
#' @noRd
.add_game_meat_final <- function(df) {
  force(df)
  cli::cli_progress_step("Adding game meat stocks")
  game_tonnes <- df |>
    dplyr::filter(item_code_prod == 1163, unit == "tonnes")

  if (nrow(game_tonnes) == 0L) {
    return(df)
  }

  game_lu <- game_tonnes |>
    dplyr::mutate(
      value = value * 3,
      unit = "LU",
      item_prod = "Game",
      item_code_prod = 1190
    )

  game_heads <- game_lu |>
    dplyr::mutate(
      value = value / 10,
      unit = "heads"
    )

  dplyr::bind_rows(df, game_lu, game_heads)
}

# -- Historical extension -----------------------------------------------------

.extend_historical <- function(
  primary_raw2,
  years_df,
  land_areas
) {
  cli::cli_progress_step("Extending historical series")
  varnames_cropland <- c(
    "c3ann",
    "c3per",
    "c4ann",
    "c4per",
    "c3nfx"
  )
  varnames_pasture <- c("pastr", "range")

  land_wide <- land_areas |>
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

  primary_raw2 |>
    dplyr::mutate(
      land_use = dplyr::if_else(
        unit %in% c("ha", "t_ha") | (unit == "tonnes" & is.na(Live_anim_code)),
        "Cropland",
        "Agriland"
      )
    ) |>
    dplyr::full_join(years_df, by = "year") |>
    .fill_pre_faostat(land_wide) |>
    dplyr::filter(
      !is.na(area),
      area != "",
      !is.na(unit)
    )
}

.fill_pre_faostat <- function(df, land_wide) {
  pre <- df |>
    dplyr::filter(year < 1962) |>
    dplyr::select(-dplyr::any_of("source")) |>
    tidyr::complete(
      year,
      tidyr::nesting(
        area,
        area_code,
        item_prod,
        item_code_prod,
        item_cbs,
        item_code_cbs,
        land_use,
        Live_anim,
        Live_anim_code,
        unit
      )
    ) |>
    dplyr::full_join(land_wide, by = c("year", "area")) |>
    dplyr::mutate(
      value_cropland = value,
      value_agriland = value,
      value_livestockyield = value
    ) |>
    fill_proxy_growth(
      value_col = value_cropland,
      proxy_col = "Cropland",
      time_col = year,
      .by = c("area", "area_code", "item_prod", "land_use", "unit"),
      verbose = FALSE
    ) |>
    fill_proxy_growth(
      value_col = value_agriland,
      proxy_col = "agriland",
      time_col = year,
      .by = c("area", "area_code", "item_prod", "land_use", "unit"),
      verbose = FALSE
    ) |>
    fill_linear(
      value_livestockyield,
      time_col = year,
      .by = c("area", "area_code", "item_prod", "land_use", "unit")
    ) |>
    dplyr::mutate(
      value = dplyr::case_when(
        land_use == "Cropland" ~ value_cropland,
        unit %in% c("t_head", "t_LU") ~ value_livestockyield,
        TRUE ~ value_agriland
      ),
      source = dplyr::case_when(
        land_use == "Cropland" ~ "LUH2_cropland",
        unit %in% c("t_head", "t_LU") ~ "fill_linear_historical",
        TRUE ~ "LUH2_agriland"
      )
    )

  post <- df |> dplyr::filter(year > 1961)

  dplyr::bind_rows(pre, post)
}

.build_grassland <- function(land_areas) {
  cli::cli_progress_step("Building grassland")
  varnames_pasture <- c("pastr", "range")

  land_areas |>
    dplyr::filter(Land_Use %in% varnames_pasture) |>
    dplyr::mutate(
      item_prod = dplyr::if_else(
        Land_Use == "pastr",
        "Pasture",
        "range"
      ),
      item_code_prod = dplyr::if_else(
        Land_Use == "pastr",
        "3001",
        "3002"
      ),
      item_cbs = "Grassland",
      item_code_cbs = 3000L
    ) |>
    dplyr::summarise(
      value = sum(Area_Mha, na.rm = TRUE) * 1e6,
      .by = c(
        year,
        area,
        area_code,
        item_prod,
        item_code_prod,
        item_cbs,
        item_code_cbs
      )
    ) |>
    dplyr::mutate(unit = "ha", source = "LUH2")
}

.add_historical_yields <- function(df, int_yields) {
  force(df)
  cli::cli_progress_step("Adding historical yields")
  # Capture source per key (take the source from tonnes/t rows as
  # the primary source indicator)
  src_lookup <- df |>
    dplyr::filter(unit %in% c("tonnes", "t")) |>
    dplyr::select(
      year,
      area,
      area_code,
      item_prod,
      item_code_prod,
      source
    ) |>
    dplyr::distinct()

  df |>
    dplyr::select(
      year,
      area,
      area_code,
      item_prod,
      item_code_prod,
      item_cbs,
      item_code_cbs,
      land_use,
      unit,
      Live_anim,
      Live_anim_code,
      value
    ) |>
    tidyr::pivot_wider(
      names_from = unit,
      values_from = value
    ) |>
    dplyr::left_join(
      src_lookup,
      by = c(
        "year",
        "area",
        "area_code",
        "item_prod",
        "item_code_prod"
      )
    ) |>
    dplyr::left_join(
      int_yields,
      by = c("year", "area", "item_code_prod")
    ) |>
    dplyr::mutate(
      t_ha_raw = tonnes / ha,
      t_ha = dplyr::if_else(year < 1961, NA_real_, t_ha_raw)
    ) |>
    fill_proxy_growth(
      value_col = t_ha,
      proxy_col = "yield",
      time_col = year,
      .by = c("area", "item_prod"),
      verbose = FALSE
    ) |>
    dplyr::mutate(
      t_ha = dplyr::if_else(
        !is.na(t_ha),
        t_ha,
        t_ha_raw
      ),
      tonnes = dplyr::if_else(
        !is.na(ha),
        ha * t_ha,
        tonnes
      )
    )
}

.finalise_primary <- function(df) {
  force(df)
  cli::cli_progress_step("Finalising primary production")
  df |>
    dplyr::select(
      year,
      area,
      area_code,
      item_prod,
      item_code_prod,
      item_cbs,
      item_code_cbs,
      Live_anim,
      Live_anim_code,
      source,
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
    dplyr::filter(value != 0, !is.na(value)) |>
    dplyr::summarise(
      value = mean(value, na.rm = TRUE),
      source = source[1L],
      .by = c(
        year,
        area,
        area_code,
        item_prod,
        item_code_prod,
        item_cbs,
        item_code_cbs,
        Live_anim,
        Live_anim_code,
        unit
      )
    )
}
