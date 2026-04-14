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
#' @param end_year Integer. Last year to include. Default `2023`.
#' @param smooth_carry_forward Logical. If `TRUE`, carry-forward tails
#'   are replaced with a linear trend. Default `FALSE`.
#' @param example Logical. If `TRUE`, return a small hardcoded example
#'   tibble instead of reading remote data. Default `FALSE`.
#' @param show_duplicates Logical. If `TRUE`, return only the rows that
#'   have competing sources in wide format (one column per source) for
#'   diagnostic comparison. Default `FALSE`.
#' @param .raw_data Optional tibble with the same structure as the output
#'   of the internal `.read_production()` step. When supplied, the
#'   remote-data read is skipped entirely and the pipeline starts from
#'   `.fix_production()`. Columns required: `year`, `area`, `area_code`,
#'   `item_prod`, `item_prod_code`, `item_cbs`, `item_cbs_code`,
#'   `live_anim`, `live_anim_code`, `unit`, `value`, `source`.
#'   Default `NULL`.
#'
#' @returns A tibble with the same columns as [get_primary_production()]:
#'   `year`, `area_code` (numeric FAOSTAT), `item_prod_code`,
#'   `item_cbs_code`, `live_anim_code`, `unit`, `value`.
#'   Names can be recovered via [add_area_name()], [add_item_prod_name()], etc.
#'   When `show_duplicates = TRUE`, returns a wide tibble with one
#'   column per source showing the competing values.
#'
#' @export
#'
#' @examples
#' build_primary_production(example = TRUE)
build_primary_production <- function(
  start_year = 1850,
  end_year = 2023,
  smooth_carry_forward = FALSE,
  example = FALSE,
  show_duplicates = FALSE,
  .raw_data = NULL
) {
  if (example) {
    return(.example_build_primary_prod())
  }
  cli::cli_h1("Building primary production")
  if (is.null(.raw_data)) {
    raw <- .read_production(start_year, end_year)
  } else {
    raw <- .raw_data
  }
  cb_extracts <- attr(raw, ".cb_extracts")

  clean <- raw |>
    .fix_production() |>
    .qc_production(smooth = smooth_carry_forward) |>
    tibble::as_tibble()

  if (show_duplicates) {
    return(.show_prod_duplicates(clean))
  }

  result <- clean |>
    .dedup_production() |>
    dplyr::mutate(
      item_prod_code = as.numeric(item_prod_code),
      live_anim_code = as.numeric(live_anim_code)
    ) |>
    dplyr::select(
      year,
      area_code,
      item_prod_code,
      item_cbs_code,
      live_anim_code,
      unit,
      value,
      source,
      dplyr::any_of("fao_flag")
    )

  attr(result, ".cb_extracts") <- cb_extracts
  result
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
#' @param end_year Integer. Last year to include. Default `2023`.
#'
#' @returns A tibble in long format with columns:
#'   `year`, `area`, `area_code`, `item_prod`, `item_prod_code`,
#'   `item_cbs`, `item_cbs_code`, `live_anim`, `live_anim_code`,
#'   `unit`, `value`, `source`.
#'
#'   The `source` column indicates data provenance:
#'   `"FAOSTAT_prod"` (original FAOSTAT production), `"EuropeAgriDB"` (European AgriDB fodder),
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
  end_year = 2023
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
  cbs_prod_raw <- .read_cbs_production(years = years)

  # 2. Read and process FAOSTAT crop/livestock production
  fao_crop_liv <- .read_fao_crop_liv(years = years)

  # 3. Fodder crops (year 2013 excluded — known bad data in old source)
  fodder <- .build_fodder(fao_crop_liv, years = years)

  # 4. Combine FAO + fodder (no tea correction — see .fix_production)
  fao_combined <- dplyr::bind_rows(fao_crop_liv, fodder)

  # 5. Livestock stocks
  fao_liv_all <- .build_livestock_stocks(
    fao_combined,
    years = years
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
  land_areas <- .read_land_areas(years = years)
  int_yields <- .read_int_yields(years = years)

  primary_ext <- .extend_historical(
    primary_raw2,
    years_df,
    land_areas
  )

  # 10. Add grassland + historical yields
  grassland <- .build_grassland(land_areas)

  cb_extracts <- attr(cbs_prod_raw, ".cb_extracts")

  result <- primary_ext |>
    dplyr::bind_rows(grassland) |>
    .add_historical_yields(int_yields) |>
    .finalise_primary() |>
    .filter_years(output_years)

  attr(result, ".cb_extracts") <- cb_extracts
  result
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
    "item_prod_code",
    "unit"
  )

  df <- df |>
    .flag_cf_and_spikes(
      by = by_cols,
      min_run = min_run,
      spike_ratio = spike_ratio,
      spike_min = spike_min
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

.read_cbs_production <- function(years = NULL) {
  cli::cli_progress_step("Reading CBS production")
  fbs_new <- .extract_cb("faostat-fbs-new", years = years)
  fbs_old <- .extract_cb("faostat-fbs-old", years = years)
  cbs_anim <- .extract_cb(
    "faostat-cbs-old-animal",
    years = years
  )
  cbs_crops <- .extract_cb(
    "faostat-cbs-old-crops",
    years = years
  )

  dt <- data.table::rbindlist(
    list(
      data.table::as.data.table(fbs_new),
      data.table::as.data.table(fbs_old),
      data.table::as.data.table(cbs_anim),
      data.table::as.data.table(cbs_crops)
    ),
    use.names = TRUE,
    fill = TRUE
  )
  dt <- dt[element == "production"]
  dt <- dt[,
    .(t_cbs = mean(value, na.rm = TRUE)),
    by = c("area", "area_code", "year", "item_cbs", "item_cbs_code", "element")
  ]
  dt <- dt[, .(year, area_code, item_cbs_code, item_cbs, t_cbs)]

  # Attach CB extracts so CBS build can reuse them
  attr(dt, ".cb_extracts") <- list(
    fbs_new = fbs_new,
    fbs_old = fbs_old,
    cbs_crops = cbs_crops,
    cbs_animals = cbs_anim
  )
  dt
}

.read_fao_crop_liv <- function(years = NULL) {
  cli::cli_progress_step("Reading FAO crops/livestock")
  dt <- .read_input("faostat-production", years = years, year_col = "Year")
  data.table::setnames(
    dt,
    c("Item Code", "Item", "Area Code", "Unit", "Element", "Year", "Value"),
    c(
      "item_prod_code",
      "item_prod",
      "area_code",
      "unit",
      "element",
      "year",
      "value"
    )
  )
  # Rename FAOSTAT flag so .aggregate_to_polities carries it through
  if ("Flag" %in% names(dt)) {
    data.table::setnames(dt, "Flag", "fao_flag")
  }
  dt[, item_prod_code := as.character(item_prod_code)]
  dt <- .aggregate_to_polities(dt, item_prod_code, item_prod)
  data.table::setorderv(
    dt,
    c(
      "year",
      "area",
      "area_code",
      "item_prod_code",
      "item_prod",
      "element",
      "unit"
    )
  )
  dt
}

.read_land_areas <- function(years = NULL) {
  cli::cli_progress_step("Reading land areas")
  regions <- unique(
    data.table::as.data.table(whep::regions_full)[,
      .(iso3c, area = polity_name, polity_code)
    ],
    by = "iso3c"
  )
  polities <- data.table::as.data.table(whep::polities)[,
    .(iso3c, area_code)
  ]

  dt <- .read_input("luh2-areas", years = years, year_col = "Year")
  data.table::setnames(dt, c("ISO3", "Year"), c("iso3c", "year"))
  dt <- merge(dt, regions, by = "iso3c", all.x = TRUE, sort = FALSE)
  unmatched <- unique(dt[is.na(area), iso3c])
  if (length(unmatched) > 0) {
    cli::cli_warn(
      "LUH2 ISO3 codes not found in regions_full, dropping: {unmatched}"
    )
  }
  dt <- dt[!is.na(area)]
  dt <- merge(
    dt,
    polities,
    by.x = "polity_code",
    by.y = "iso3c",
    all.x = TRUE,
    sort = FALSE
  )
  dt[, polity_code := NULL]
  dt <- dt[year > 1849]
  dt
}

.read_int_yields <- function(years = NULL) {
  cli::cli_progress_step("Reading international yields")
  regions <- data.table::as.data.table(whep::regions_full)[,
    .(code, polity_name)
  ]

  dt <- .read_input("international-yields", years = years, year_col = "year")
  dt[, item_prod_code := as.character(item_code)]
  data.table::setnames(dt, "area_code", "code")
  dt[, item_code := NULL]

  dt <- dt[
    year < 1962 & !is.na(yield) & yield != 0 & yield < 100
  ]
  dt <- merge(dt, regions, by = "code", all.x = TRUE, sort = FALSE)
  data.table::setnames(dt, "polity_name", "area")
  dt <- dt[,
    .(yield = mean(yield, na.rm = TRUE)),
    by = c("year", "area", "item_prod_code")
  ]
  dt <- dt[!is.nan(yield)]
  dt
}

# -- Fodder --------------------------------------------------------------------

.build_fodder <- function(fao_crop_liv, years = NULL) {
  cli::cli_progress_step("Building fodder dataset")
  items_prod <- whep::items_prod_full
  items <- whep::items_full
  crops_eu <- whep::crops_eurostat
  biomass <- whep::biomass_coefs
  regions <- whep::regions_full

  # Old FAO fodder data
  i_fodder <- .read_fodder_old(years = years)

  # EU AgriDB fodder
  fodder_euadb <- .read_fodder_euadb(years = years)

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

.read_fodder_old <- function(years = NULL) {
  items_prod <- data.table::as.data.table(whep::items_prod_full)
  items <- data.table::as.data.table(whep::items_full)

  dt <- .read_input("faostat-production-old", years = years, year_col = "Year")
  data.table::setnames(
    dt,
    c("AreaCode", "ItemCode", "ItemName", "Year", "Value"),
    c("area_code", "item_prod_code", "item_prod", "year", "value")
  )
  dt[, `:=`(
    element = "production",
    unit = "t",
    item_prod_code = as.character(item_prod_code)
  )]
  dt <- .aggregate_to_polities(dt, item_prod_code, item_prod)
  dt <- merge(
    dt,
    items_prod[, .(item_prod_code, item_cbs)],
    by = "item_prod_code",
    all.x = TRUE,
    sort = FALSE
  )
  dt <- merge(
    dt,
    unique(items[, .(item_cbs, Cat_1)]),
    by = "item_cbs",
    all.x = TRUE,
    sort = FALSE
  )
  dt[Cat_1 == "Fodder_green"]
}

.read_fodder_euadb <- function(years = NULL) {
  crops_eu <- whep::crops_eurostat
  regions <- whep::regions_full

  polities <- whep::polities

  .read_input("eu-agridb-fodder", years = years, year_col = "Year") |>
    dplyr::rename(year = Year) |>
    dplyr::left_join(crops_eu, by = "Crop") |>
    dplyr::rename(adb_region = Region) |>
    dplyr::left_join(
      regions |>
        dplyr::select(
          adb_region = ADB_Region,
          area = polity_name,
          polity_code
        ),
      by = "adb_region"
    ) |>
    dplyr::left_join(
      polities |> dplyr::select(iso3c, area_code),
      by = c("polity_code" = "iso3c")
    ) |>
    dplyr::select(-polity_code) |>
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
    dplyr::select(item_prod_code, Product_kgDM_kgFM) |>
    dplyr::filter(!is.na(Product_kgDM_kgFM))

  fao_crop_liv |>
    dplyr::filter(unit %in% c("ha", "t")) |>
    tidyr::pivot_wider(
      names_from = "unit",
      values_from = "value"
    ) |>
    dplyr::left_join(crops_dm, by = "item_prod_code") |>
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
    dplyr::select(item_prod_code, Product_kgDM_kgFM) |>
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
    dplyr::left_join(crops_dm, by = "item_prod_code") |>
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
      item_prod_code,
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
        dplyr::select(item_prod, item_prod_code, Name_Eurostat),
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
        "item_prod_code"
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
  grp_cols <- c(
    "area",
    "area_code",
    "item_prod",
    "item_prod_code",
    "Name_Eurostat"
  )

  dt <- data.table::as.data.table(fodder)
  dt <- dt[!is.na(area)]
  dt <- dt[,
    .(
      t = .sum_if_any(t),
      t_dm = .sum_if_any(t_dm),
      ha = .sum_if_any(ha),
      ha_euadb = .sum_if_any(ha_euadb),
      ha_share = .mean_if_any(ha_share),
      kgnha_euadb = .mean_if_any(kgnha_euadb)
    ),
    by = c("year", grp_cols)
  ]

  # tidyr::complete equivalent: cross join all years x all group combos
  dt <- merge(
    .cross_join(
      dt[, .(year = unique(year))],
      unique(dt[, ..grp_cols])
    ),
    dt,
    by = c("year", grp_cols),
    all.x = TRUE,
    sort = FALSE
  )

  # Three fill_linear calls sharing one sort — first call sets key,
  # subsequent calls see key already set and skip setkeyv.
  dt <- fill_linear(
    dt,
    ha_share,
    time_col = year,
    .by = grp_cols,
    .copy = FALSE
  )
  dt <- fill_linear(
    dt,
    kgnha_euadb,
    time_col = year,
    .by = grp_cols,
    .copy = FALSE
  )
  dt[, ha := data.table::fifelse(is.na(ha_euadb), ha, ha_euadb * ha_share)]
  dt <- fill_linear(dt, ha, time_col = year, .by = grp_cols, .copy = FALSE)

  dt <- dt[,
    .(
      year,
      area,
      area_code,
      item_prod,
      item_prod_code,
      t,
      t_dm,
      ha_share,
      ha,
      kgnha_euadb
    )
  ]

  dm_dt <- data.table::as.data.table(dm_yield)[, .(year, area_code, yield_dm)]
  items_dt <- data.table::as.data.table(items_prod)[, .(
    item_prod,
    Name_biomass
  )]
  bio_dt <- data.table::as.data.table(biomass)[,
    .(Name_biomass, Product_kgDM_kgFM, Product_kgN_kgDM)
  ]

  dt <- merge(
    dt,
    dm_dt,
    by = c("year", "area_code"),
    all.x = TRUE,
    sort = FALSE
  )
  dt <- merge(dt, items_dt, by = "item_prod", all.x = TRUE, sort = FALSE)
  dt <- merge(dt, bio_dt, by = "Name_biomass", all.x = TRUE, sort = FALSE)

  dt[, `:=`(
    t_euadb = ha * kgnha_euadb / (Product_kgN_kgDM * Product_kgDM_kgFM * 1000),
    t_dmbased = ha * yield_dm / Product_kgDM_kgFM
  )]
  dt[, t_2 := data.table::fifelse(!is.na(t_euadb), t_euadb, t_dmbased)]
  dt[,
    source := data.table::fcase(
      !is.na(t)       , "FAOSTAT_prod" ,
      !is.na(t_euadb) , "EuropeAgriDB" ,
      default = "DM_yield_estimate"
    )
  ]

  tibble::as_tibble(dt[!is.na(item_prod) & !is.na(t_2)])
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
  years = NULL
) {
  cli::cli_progress_step("Building livestock stocks")
  animals <- whep::animals_codes
  liv_lu <- whep::liv_lu_coefs

  fao_stocks <- .read_livestock_stocks(years = years)

  fao_liv_raw <- .combine_livestock(
    fao_combined,
    fao_stocks,
    animals
  )

  .finalise_livestock(fao_liv_raw, animals, liv_lu)
}

.read_livestock_stocks <- function(years = NULL) {
  dt <- .read_input(
    "faostat-emissions-livestock",
    years = years,
    year_col = "Year"
  )
  data.table::setnames(
    dt,
    c("Item Code", "Item", "Area Code", "Unit", "Element", "Year", "Value"),
    c(
      "item_cbs_code",
      "item_cbs",
      "area_code",
      "unit",
      "element",
      "year",
      "value"
    )
  )
  dt <- dt[element == "Stocks" & Source == "FAO TIER 1"]
  .aggregate_to_polities(dt, item_cbs_code, item_cbs)
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
      item_prod_code,
      value
    ) |>
    dplyr::inner_join(
      animals |>
        dplyr::select(
          item_cbs_code,
          item_cbs,
          Item_Code,
          Livestock_name
        ) |>
        dplyr::mutate(item_prod_code = as.character(Item_Code)) |>
        dplyr::select(-Item_Code),
      by = "item_prod_code"
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
          item_cbs_code,
          item_st,
          value_st
        ),
      by = c("year", "area", "item_cbs_code")
    ) |>
    dplyr::mutate(
      share = value_st / sum(value_st),
      value_comb = dplyr::if_else(
        is.na(share) | share == 1,
        value,
        value * share
      ),
      .by = c(year, area, item_prod_code)
    ) |>
    dplyr::filter(!is.na(area_code)) |>
    dplyr::mutate(
      n = dplyr::n(),
      .by = c(area, item_cbs, item_cbs_code)
    ) |>
    tidyr::complete(
      year,
      tidyr::nesting(
        area,
        area_code,
        item_cbs,
        item_cbs_code,
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
        "item_cbs_code",
        "Livestock_name"
      )
    ) |>
    dplyr::mutate(
      value = dplyr::if_else(
        !is.na(value_comb),
        value_comb,
        dplyr::if_else(n > 40, NA_real_, 0)
      ),
      .by = c(area, area_code, item_cbs, item_cbs_code, Livestock_name)
    ) |>
    fill_linear(
      value,
      time_col = year,
      .by = c(
        "area",
        "area_code",
        "item_cbs",
        "item_cbs_code",
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
      item_cbs_code,
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
      item_prod_code = item_cbs_code
    ) |>
    dplyr::mutate(item_prod_code = as.character(item_prod_code)) |>
    dplyr::mutate(
      source = dplyr::if_else(
        source_value == "Original",
        "FAOSTAT_prod",
        "fill_linear"
      )
    ) |>
    dplyr::select(
      year,
      area,
      area_code,
      item_prod_code,
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
          "FAOSTAT_prod",
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
        item_prod_code,
        unit
      )
    )
}

.add_game_meat <- function(df) {
  game <- df |>
    dplyr::filter(item_prod_code == 1163) |>
    dplyr::mutate(
      value = value * 3,
      unit = "LU",
      item_prod = "Game",
      item_prod_code = 1190
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
  crop_dt <- data.table::as.data.table(primary_raw)[unit %in% c("ha", "t")]
  crop_yield <- data.table::dcast(
    crop_dt,
    year + area + area_code + item_prod + item_prod_code ~ unit,
    value.var = "value"
  )
  crop_yield[, `:=`(yield_c = t / ha, unit = "t_ha")]
  data.table::setnames(crop_yield, "ha", "fu")
  crop_yield <- tibble::as_tibble(crop_yield)

  liv_yield <- primary_raw |>
    dplyr::filter(unit == "t") |>
    dplyr::select(-unit) |>
    dplyr::right_join(
      items_prod |>
        dplyr::filter(!is.na(live_anim)) |>
        dplyr::select(
          live_anim,
          live_anim_code,
          item_prod_code
        ) |>
        dplyr::mutate(live_anim_code = as.character(live_anim_code)),
      by = "item_prod_code"
    ) |>
    dplyr::rename(t = value) |>
    tidyr::complete(
      year,
      tidyr::nesting(
        area,
        area_code,
        item_prod,
        item_prod_code,
        live_anim,
        live_anim_code
      )
    ) |>
    dplyr::left_join(
      dplyr::bind_rows(
        primary_raw |>
          dplyr::filter(unit == "LU") |>
          dplyr::rename(live_anim_code = item_prod_code) |>
          dplyr::select(
            year,
            area,
            area_code,
            live_anim_code,
            unit,
            value
          ),
        primary_raw |>
          dplyr::filter(unit == "heads") |>
          dplyr::rename(live_anim_code = item_prod_code) |>
          dplyr::select(
            year,
            area,
            area_code,
            live_anim_code,
            unit,
            value
          )
      ) |>
        dplyr::summarise(
          value = sum(value, na.rm = TRUE),
          .by = c(year, area, area_code, live_anim_code, unit)
        ),
      by = c(
        "year",
        "area",
        "area_code",
        "live_anim_code"
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
        item_prod_code,
        live_anim,
        live_anim_code,
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
      item_prod_code,
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
      .by = c(area, area_code, item_prod, item_prod_code)
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
    dplyr::select(-Multi_type, -item_prod_code) |>
    dplyr::left_join(
      primary_double |>
        dplyr::select(item_prod, Item_area) |>
        dplyr::left_join(
          primary_double |>
            dplyr::select(
              item_prod,
              item_prod_code,
              Multi_type
            ) |>
            dplyr::mutate(
              item_prod_code = as.character(item_prod_code)
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
        item_prod_code,
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
        item_prod_code,
        unit
      )
    ) |>
    dplyr::filter(n == 1 | (n == 2 & is.na(source)))
}

.fill_yields <- function(yield_all, items_prod, cbs_prod_raw) {
  yield_all |>
    .collapse_yield_rows() |>
    fill_linear(
      yield_c,
      time_col = year,
      .by = c(
        "area",
        "area_code",
        "item_prod",
        "item_prod_code",
        "live_anim_code",
        "unit"
      )
    ) |>
    .add_global_yields() |>
    dplyr::left_join(
      items_prod |>
        dplyr::select(item_prod_code, item_cbs_code, group) |>
        dplyr::distinct(item_prod_code, .keep_all = TRUE),
      by = "item_prod_code"
    ) |>
    dplyr::left_join(
      cbs_prod_raw,
      by = c("year", "area_code", "item_cbs_code")
    ) |>
    .compute_cbs_ratios() |>
    .impute_missing_values()
}

.collapse_yield_rows <- function(df) {
  if (!data.table::is.data.table(df)) {
    data.table::setDT(df)
  }

  df[, `:=`(.t_ok = as.double(!is.na(t)), .fu_ok = as.double(!is.na(fu)))]

  by_cols <- c(
    "year",
    "area",
    "area_code",
    "item_prod",
    "item_prod_code",
    "live_anim_code",
    "unit"
  )

  # GForce-optimized numeric aggregation
  out <- df[,
    .(
      t = sum(t, na.rm = TRUE),
      .t_n = sum(.t_ok),
      fu = sum(fu, na.rm = TRUE),
      .fu_n = sum(.fu_ok),
      yield_c = mean(yield_c, na.rm = TRUE)
    ),
    keyby = by_cols
  ]

  out[.t_n == 0, t := NA_real_]
  out[.fu_n == 0, fu := NA_real_]
  out[is.nan(yield_c), yield_c := NA_real_]
  out[, c(".t_n", ".fu_n") := NULL]

  # Character columns: vectorised first-non-NA via unique()
  # (avoids per-group R expression evaluation)
  chars <- .first_non_na_chars(
    df,
    by_cols,
    c(
      "source",
      "Multi_type",
      "live_anim"
    )
  )

  out <- out[chars]
  df[, c(".t_ok", ".fu_ok") := NULL]
  out
}

.add_global_yields <- function(df) {
  if (!data.table::is.data.table(df)) {
    data.table::setDT(df)
  }
  dt <- df

  global <- dt[,
    .(
      t = sum(t, na.rm = TRUE),
      fu = sum(fu, na.rm = TRUE)
    ),
    by = c("year", "item_prod_code", "live_anim_code", "unit")
  ]

  global[, yield_glo := t / fu]
  global[
    yield_glo == 0 | is.infinite(yield_glo) | is.nan(yield_glo),
    yield_glo := NA_real_
  ]

  global <- fill_linear(
    global,
    yield_glo,
    time_col = year,
    .by = c("item_prod_code", "live_anim_code", "unit"),
    .copy = FALSE
  )
  if (!data.table::is.data.table(global)) {
    data.table::setDT(global)
  }
  global <- global[, .(year, item_prod_code, live_anim_code, unit, yield_glo)]

  merge(
    dt,
    global,
    by = c("year", "item_prod_code", "live_anim_code", "unit"),
    all.x = TRUE,
    sort = FALSE
  )
}

.compute_cbs_ratios <- function(df) {
  if (!data.table::is.data.table(df)) {
    data.table::setDT(df)
  } else {
    data.table::setalloccol(df)
  }
  df[,
    `:=`(
      prod_cbs_ratio = t / t_cbs,
      prod_cbs_count = .N,
      sumprod_cbs_ratio = sum(t, na.rm = TRUE) / t_cbs[1L]
    ),
    by = c("year", "area", "area_code", "item_cbs_code")
  ]

  df |>
    .collapse_cbs_ratio_rows() |>
    fill_linear(
      prod_cbs_ratio,
      time_col = year,
      .by = c(
        "area",
        "area_code",
        "item_prod",
        "item_prod_code",
        "item_cbs",
        "item_cbs_code",
        "live_anim",
        "live_anim_code",
        "unit",
        "group"
      )
    )
}

.collapse_cbs_ratio_rows <- function(df) {
  if (!data.table::is.data.table(df)) {
    data.table::setDT(df)
  }

  df[, .tcbs_ok := as.double(!is.na(t_cbs))]

  by_cols <- c(
    "year",
    "area",
    "area_code",
    "item_prod",
    "item_prod_code",
    "item_cbs",
    "item_cbs_code",
    "live_anim",
    "live_anim_code",
    "unit",
    "group"
  )

  # GForce-optimized numeric aggregation
  out <- df[,
    .(
      t = sum(t, na.rm = TRUE),
      fu = sum(fu, na.rm = TRUE),
      yield_c = mean(yield_c, na.rm = TRUE),
      yield_glo = mean(yield_glo, na.rm = TRUE),
      t_cbs = sum(t_cbs, na.rm = TRUE),
      .tcbs_n = sum(.tcbs_ok),
      prod_cbs_ratio = mean(prod_cbs_ratio, na.rm = TRUE),
      prod_cbs_count = mean(prod_cbs_count, na.rm = TRUE),
      sumprod_cbs_ratio = mean(sumprod_cbs_ratio, na.rm = TRUE)
    ),
    keyby = by_cols
  ]

  out[.tcbs_n == 0, t_cbs := NA_real_]
  out[is.nan(yield_c), yield_c := NA_real_]
  out[is.nan(yield_glo), yield_glo := NA_real_]
  out[is.nan(prod_cbs_ratio), prod_cbs_ratio := NA_real_]
  out[is.nan(sumprod_cbs_ratio), sumprod_cbs_ratio := NA_real_]
  out[, .tcbs_n := NULL]

  # Character columns: vectorised first-non-NA via unique()
  chars <- .first_non_na_chars(
    df,
    by_cols,
    c(
      "source",
      "Multi_type",
      "source_yield_c"
    )
  )

  out <- out[chars]
  df[, .tcbs_ok := NULL]
  out
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
        !is.na(t) ~ "FAOSTAT_prod",
        !is.na(fu * yield) &
          !is.na(source_yield_c) &
          source_yield_c != "Original" ~
          paste0("imputed_yield:", source_yield_c),
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
      item_prod_code,
      live_anim,
      live_anim_code,
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
      item_prod_code,
      live_anim,
      live_anim_code,
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
      item_prod_code,
      live_anim,
      live_anim_code,
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
        live_anim_code,
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
    dplyr::rename(item_prod_code = live_anim_code) |>
    dplyr::left_join(
      items |>
        dplyr::select(item_cbs, item_cbs_code) |>
        dplyr::mutate(item_prod_code = as.character(item_cbs_code)),
      by = "item_prod_code"
    ) |>
    dplyr::mutate(item_prod = item_cbs)

  dplyr::bind_rows(ha_df, tonnes_df, yield_df) |>
    dplyr::select(-item_prod) |>
    dplyr::left_join(
      whep::items_prim |>
        dplyr::select(
          item_prod,
          item_prod_code,
          item_cbs,
          item_cbs_code
        ),
      by = "item_prod_code"
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
    dplyr::filter(item_prod_code == 1163, unit == "tonnes")

  if (nrow(game_tonnes) == 0L) {
    return(df)
  }

  game_lu <- game_tonnes |>
    dplyr::mutate(
      value = value * 3,
      unit = "LU",
      item_prod = "Game",
      item_prod_code = 1190
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
        unit %in% c("ha", "t_ha") | (unit == "tonnes" & is.na(live_anim_code)),
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
  pre_base <- df |>
    dplyr::filter(year < 1962) |>
    dplyr::select(-dplyr::any_of("source"))

  pre <- .complete_year_nesting_dt(
    pre_base,
    id_cols = c(
      "area",
      "area_code",
      "item_prod",
      "item_prod_code",
      "item_cbs",
      "item_cbs_code",
      "land_use",
      "live_anim",
      "live_anim_code",
      "unit"
    )
  )

  pre <- merge(
    data.table::as.data.table(pre),
    data.table::as.data.table(land_wide),
    by = c("year", "area"),
    all.x = TRUE,
    sort = FALSE
  )
  pre[, `:=`(
    value_cropland = value,
    value_agriland = value,
    value_livestockyield = value
  )]

  livestock_units <- c("t_head", "t_LU")

  pre_liv <- pre |>
    dplyr::filter(unit %in% livestock_units) |>
    fill_linear(
      value_livestockyield,
      time_col = year,
      .by = c("area", "area_code", "item_prod", "land_use", "unit")
    )

  pre_crop <- pre |>
    dplyr::filter(!(unit %in% livestock_units), land_use == "Cropland") |>
    fill_proxy_growth(
      value_col = value_cropland,
      proxy_col = "Cropland",
      time_col = year,
      .by = c("area", "area_code", "item_prod", "land_use", "unit"),
      verbose = FALSE
    )

  pre_agri <- pre |>
    dplyr::filter(
      !(unit %in% livestock_units),
      land_use != "Cropland" | is.na(land_use)
    ) |>
    fill_proxy_growth(
      value_col = value_agriland,
      proxy_col = "agriland",
      time_col = year,
      .by = c("area", "area_code", "item_prod", "land_use", "unit"),
      verbose = FALSE
    )

  pre <- dplyr::bind_rows(pre_liv, pre_crop, pre_agri) |>
    dplyr::mutate(
      value = dplyr::case_when(
        land_use == "Cropland" ~ value_cropland,
        unit %in% livestock_units ~ value_livestockyield,
        TRUE ~ value_agriland
      ),
      source = dplyr::case_when(
        land_use == "Cropland" ~ "LUH2_cropland",
        unit %in% livestock_units ~ "fill_linear_historical",
        TRUE ~ "LUH2_agriland"
      )
    )

  post <- df |> dplyr::filter(year > 1961)

  dplyr::bind_rows(pre, post)
}

.complete_year_nesting_dt <- function(df, id_cols) {
  dt <- data.table::as.data.table(df)
  years_dt <- data.table::data.table(year = sort(unique(dt$year)))
  keys_dt <- unique(dt[, id_cols, with = FALSE])
  years_dt[, .cross_key := 1L]
  keys_dt[, .cross_key := 1L]
  skeleton <- merge(
    years_dt,
    keys_dt,
    by = ".cross_key",
    allow.cartesian = TRUE,
    sort = FALSE
  )
  skeleton[, .cross_key := NULL]

  merge(
    skeleton,
    dt,
    by = c("year", id_cols),
    all.x = TRUE,
    sort = FALSE
  )
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
      item_prod_code = dplyr::if_else(
        Land_Use == "pastr",
        "3001",
        "3002"
      ),
      item_cbs = "Grassland",
      item_cbs_code = 3000L
    ) |>
    dplyr::summarise(
      value = sum(Area_Mha, na.rm = TRUE) * 1e6,
      .by = c(
        year,
        area,
        area_code,
        item_prod,
        item_prod_code,
        item_cbs,
        item_cbs_code
      )
    ) |>
    dplyr::mutate(unit = "ha", source = "LUH2_grassland")
}

.add_historical_yields <- function(df, int_yields) {
  force(df)
  cli::cli_progress_step("Adding historical yields")
  # Capture source per key (take the source from tonnes/t rows as
  # the primary source indicator)
  if (!data.table::is.data.table(df)) {
    data.table::setDT(df)
  }

  src_lookup <- df[
    unit %in% c("tonnes", "t"),
    .(
      source = data.table::first(source)
    ),
    by = c("year", "area", "area_code", "item_prod", "item_prod_code")
  ]

  agg <- df[,
    .(value = sum(value, na.rm = TRUE)),
    by = c(
      "year",
      "area",
      "area_code",
      "item_prod",
      "item_prod_code",
      "item_cbs",
      "item_cbs_code",
      "land_use",
      "unit",
      "live_anim",
      "live_anim_code"
    )
  ]

  wide <- data.table::dcast(
    agg,
    year +
      area +
      area_code +
      item_prod +
      item_prod_code +
      item_cbs +
      item_cbs_code +
      land_use +
      live_anim +
      live_anim_code ~
      unit,
    value.var = "value"
  )

  wide <- merge(
    wide,
    src_lookup,
    by = c("year", "area", "area_code", "item_prod", "item_prod_code"),
    all.x = TRUE,
    sort = FALSE
  )
  wide <- merge(
    wide,
    if (data.table::is.data.table(int_yields)) {
      int_yields
    } else {
      data.table::as.data.table(int_yields)
    },
    by = c("year", "area", "item_prod_code"),
    all.x = TRUE,
    sort = FALSE
  )

  wide[, t_ha_raw := tonnes / ha]
  wide[, t_ha := data.table::fifelse(year < 1961, NA_real_, t_ha_raw)]

  wide <- fill_proxy_growth(
    wide,
    value_col = t_ha,
    proxy_col = "yield",
    time_col = year,
    .by = c("area", "item_prod"),
    verbose = FALSE
  )
  if (!data.table::is.data.table(wide)) {
    data.table::setDT(wide)
  }
  wide[, t_ha := data.table::fifelse(!is.na(t_ha), t_ha, t_ha_raw)]
  wide[, tonnes := data.table::fifelse(!is.na(ha), ha * t_ha, tonnes)]
  wide
}

.first_non_missing <- function(x) {
  idx <- which.min(is.na(x))
  if (is.na(x[idx])) NA_character_ else x[idx]
}

# Vectorised first-non-NA for character columns.
# Uses unique() (C-level) instead of per-group R evaluation.
.first_non_na_chars <- function(df, by_cols, char_cols) {
  data.table::setkeyv(df, by_cols)
  present_cols <- intersect(char_cols, names(df))
  # Start from full set of group keys
  result <- unique(df[, by_cols, with = FALSE], by = by_cols)
  for (col in present_cols) {
    cols <- c(by_cols, col)
    part <- unique(
      df[!is.na(df[[col]]), cols, with = FALSE],
      by = by_cols
    )
    result <- merge(result, part, by = by_cols, all.x = TRUE, sort = FALSE)
  }
  result
}

.sum_if_any <- function(x) {
  if (anyNA(x) && all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)
}

.mean_if_any <- function(x) {
  if (anyNA(x) && all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
}

# Cross join two data.tables (cartesian product of all rows).
.cross_join <- function(x, y) {
  x[, .cross := 1L]
  y[, .cross := 1L]
  out <- merge(x, y, by = ".cross", allow.cartesian = TRUE, sort = FALSE)
  out[, .cross := NULL]
  out
}

.finalise_primary <- function(df) {
  force(df)
  cli::cli_progress_step("Finalising primary production")
  if (!data.table::is.data.table(df)) {
    data.table::setDT(df)
  }
  dt <- df[,
    .(
      year,
      area,
      area_code,
      item_prod,
      item_prod_code,
      item_cbs,
      item_cbs_code,
      live_anim,
      live_anim_code,
      source,
      ha,
      LU,
      heads,
      tonnes,
      t_ha,
      t_LU,
      t_head
    )
  ]

  long <- data.table::melt(
    dt,
    measure.vars = c("ha", "LU", "heads", "tonnes", "t_ha", "t_LU", "t_head"),
    variable.name = "unit",
    value.name = "value"
  )

  long <- long[value != 0 & !is.na(value)]

  out <- long[,
    .(
      value = mean(value, na.rm = TRUE),
      source = source[1L]
    ),
    by = c(
      "year",
      "area",
      "area_code",
      "item_prod",
      "item_prod_code",
      "item_cbs",
      "item_cbs_code",
      "live_anim",
      "live_anim_code",
      "unit"
    )
  ]

  out
}

.prod_source_rank <- function(source) {
  dplyr::case_when(
    source == "FAOSTAT_prod" ~ 1L,
    source == "EuropeAgriDB" ~ 2L,
    stringr::str_starts(source, "imputed_yield") ~ 3L,
    source == "imputed_cbs_ratio" ~ 4L,
    source == "DM_yield_estimate" ~ 5L,
    source == "fill_linear" ~ 6L,
    source == "fill_linear_historical" ~ 7L,
    source == "LUH2_cropland" ~ 8L,
    source == "LUH2_agriland" ~ 9L,
    source == "LUH2_grassland" ~ 10L,
    source == "Estimated" ~ 11L,
    TRUE ~ 12L
  )
}

.dedup_production <- function(df) {
  dt <- data.table::as.data.table(df)
  dt[, `:=`(
    .src_rank = .prod_source_rank(source),
    .orig_row = .I
  )]
  by_cols <- c("year", "area_code", "item_prod_code", "unit")
  data.table::setorderv(dt, c(by_cols, ".src_rank"))
  dt <- dt[dt[, .I[1L], by = by_cols]$V1]
  data.table::setorderv(dt, ".orig_row")
  dt[, c(".src_rank", ".orig_row") := NULL]
  tibble::as_tibble(dt)
}

.show_prod_duplicates <- function(df) {
  key_cols <- c("year", "area_code", "item_prod_code", "unit")
  dupes <- df |>
    dplyr::add_count(
      dplyr::across(dplyr::all_of(key_cols)),
      name = ".n"
    ) |>
    dplyr::filter(.n > 1L) |>
    dplyr::select(!.n)

  n_keys <- dplyr::n_distinct(
    dupes$year,
    dupes$area_code,
    dupes$item_prod_code,
    dupes$unit
  )
  cli::cli_alert_info(
    "{n_keys} key{?s} with competing sources found."
  )

  dupes |>
    dplyr::select(
      dplyr::all_of(key_cols),
      source,
      value
    ) |>
    dplyr::mutate(
      .src_rank = .prod_source_rank(source)
    ) |>
    dplyr::arrange(
      dplyr::across(dplyr::all_of(key_cols)),
      .src_rank
    ) |>
    dplyr::select(!.src_rank) |>
    tidyr::pivot_wider(
      names_from = source,
      values_from = value
    )
}
