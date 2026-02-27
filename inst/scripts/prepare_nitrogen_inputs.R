# -----------------------------------------------------------------------
# prepare_nitrogen_inputs.R
#
# Generates crop-specific nitrogen, phosphorus, and potassium input
# estimates per country and year, combining multiple data sources:
#
#   1. FAOSTAT country-level totals (FAO, 2024):
#      - Synthetic fertilizer: Inputs_FertilizersNutrient (RFN)
#        Element 5157 = "Agricultural Use"
#        Items: 3102 (N), 3103 (P2O5), 3104 (K2O)
#      - Manure N: Environment_LivestockManure (EMN)
#        "Manure applied to soils" + "Manure left on pasture"
#
#   2. Cropland/Grassland split:
#      - EuroAgriDB v1.0 (Einarsson et al. 2019, Sci. Data)
#        doi:10.1038/s41597-019-0283-z
#      - Lassaletta et al. 2014: global grassland share
#        doi:10.1088/1748-9326/9/10/105011
#
#   3. Crop-specific distribution (base year ~2000, then scaled):
#      a. Mueller et al. 2012: Synthetic N rates (Excel matrix)
#         doi:10.1038/nature11420
#      b. West et al. 2014: Gridded manure N (170+ crops, NetCDF)
#         5 arc-min rasters at L_files/Manure_Westetal2014/N/
#      c. EarthStat Crop Specific Fertilizers: N/P/K rates for
#         17 major crops (Mueller et al. 2012), country averages
#      d. Global/input/Crops_manure_N.csv: country-level manure
#         (West et al. aggregated, CSV fallback)
#
#   4. Atmospheric N deposition:
#      - HaNi (Tian et al. 2022): NHx + NOy, 1850–2020, 5 arcmin
#        doi:10.5194/essd-14-4551-2022
#
#   5. Biological N fixation (BNF):
#      - NSBNF: 13 kgN/ha (Herridge et al. 2008)
#
#   6. Coello et al. 2025:
#      - Crop-group fertilization rates, 13 groups, N/P/K, 1961–2019
#        doi:10.1038/s41597-024-04215-x
#      - Country-level CSV from companion GitHub repo
#        (Rscript inst/scripts/download_coello_data.R)
#      - Used as fallback N rate in crop distribution cascade, and
#        as proportional weights for P/K crop-level distribution.
#
# Outputs (to L_files/whep/inputs/):
#   - nitrogen_inputs.parquet: crop × country × year × fert_type
#     (includes N, NSBNF, Deposition, and P2O5/K2O when Coello available)
#   - n_deposition.parquet: country × year deposition rates
#   - pk_totals.parquet: P and K country × year totals
#   - coello_crop_rates.parquet: Coello crop-group rates (cached)
#
# Requires: FAOSTAT CSVs, EuroAgriDB results, Global/input/,
# HaNi NetCDFs, West manure NetCDFs, EarthStat fertilizer TIFs.
# Optional: Coello2025/Prediction_corrected.csv
# -----------------------------------------------------------------------

library(dplyr, warn.conflicts = FALSE)

# ==== Configuration ====================================================

l_files_dir <- "C:/Users/53272530E/OneDrive/L_files"
global_dir <- "C:/Users/53272530E/OneDrive/Documents/GitHub/Global"
output_dir <- file.path(l_files_dir, "whep", "inputs")

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# WHEP reference tables
regions <- readr::read_csv(
  system.file("extdata", "regions.csv", package = "whep"),
  show_col_types = FALSE
)

items_prod <- readr::read_csv(
  system.file("extdata", "items_prod.csv", package = "whep"),
  show_col_types = FALSE
)

# Items that are CBS-level "Grassland" or similar
grassland_items <- c("Pasture", "range")


# ==== 1. Read FAOSTAT country-level N totals ===========================

.read_faostat_totals <- function(l_files_dir, regions) {
  cli::cli_h2("FAOSTAT N totals")

  fert_file <- file.path(
    l_files_dir,
    "FAOSTAT/Inputs_FertilizersNutrient_E_All_Data_(Normalized).csv"
  )
  manure_file <- file.path(
    l_files_dir,
    "FAOSTAT/Environment_LivestockManure_E_All_Data_(Normalized).csv"
  )

  if (!file.exists(fert_file) || !file.exists(manure_file)) {
    cli::cli_abort("FAOSTAT files not found in {l_files_dir}/FAOSTAT/")
  }

  # -- Synthetic fertilizer: N agricultural use by country ---
  fert_raw <- data.table::fread(fert_file) |>
    tibble::as_tibble() |>
    filter(
      Element == "Agricultural Use",
      Item == "Nutrient nitrogen N (total)"
    ) |>
    transmute(
      area_code = `Area Code`,
      year = as.integer(Year),
      fert_type = "Synthetic",
      mg_n = Value  # tonnes
    )

  # -- Manure N applied to soils ---
  manure_raw <- data.table::fread(manure_file) |>
    tibble::as_tibble()

  manure_applied <- manure_raw |>
    filter(
      Element == "Manure applied to soils (N content)",
      Item == "All Animals"
    ) |>
    transmute(
      area_code = `Area Code`,
      year = as.integer(Year),
      fert_type = "Manure",
      mg_n = Value / 1000  # Original in kg → Mg (tonnes)
    )

  manure_pasture <- manure_raw |>
    filter(
      Element == "Manure left on pasture (N content)",
      Item == "All Animals"
    ) |>
    transmute(
      area_code = `Area Code`,
      year = as.integer(Year),
      fert_type = "Grassland_excretion",
      mg_n = Value / 1000
    )

  n_totals <- bind_rows(fert_raw, manure_applied, manure_pasture) |>
    inner_join(regions |> select(area_code, area_name), by = "area_code") |>
    filter(!is.na(mg_n), mg_n >= 0)

  cli::cli_alert_success(
    "FAOSTAT totals: {n_distinct(n_totals$area_code)} countries, ",
    "{n_distinct(n_totals$year)} years"
  )

  n_totals
}


# ==== 1b. P and K country-level totals ================================

.read_faostat_pk_totals <- function(l_files_dir, regions) {
  cli::cli_h2("FAOSTAT P and K totals")

  fert_file <- file.path(
    l_files_dir,
    "FAOSTAT/Inputs_FertilizersNutrient_E_All_Data_(Normalized).csv"
  )
  if (!file.exists(fert_file)) {
    cli::cli_alert_warning("FAOSTAT fertilizer file not found")
    return(NULL)
  }

  fert_pk <- data.table::fread(fert_file) |>
    tibble::as_tibble() |>
    dplyr::filter(
      Element == "Agricultural Use",
      Item %in% c(
        "Nutrient phosphate P2O5 (total)",
        "Nutrient potash K2O (total)"
      )
    ) |>
    dplyr::transmute(
      area_code = `Area Code`,
      year = as.integer(Year),
      nutrient = dplyr::if_else(
        grepl("phosphate", Item), "P", "K"
      ),
      mg_nutrient = Value
    ) |>
    dplyr::inner_join(
      regions |> dplyr::select(area_code, area_name),
      by = "area_code"
    ) |>
    dplyr::filter(!is.na(mg_nutrient), mg_nutrient >= 0)

  n_pk_countries <- n_distinct(fert_pk$area_code)
  n_p_obs <- sum(fert_pk$nutrient == "P")
  n_k_obs <- sum(fert_pk$nutrient == "K")
  cli::cli_alert_success(
    "P/K totals: {n_pk_countries} countries, P={n_p_obs} obs, K={n_k_obs} obs"
  )

  fert_pk
}


# ==== 2. Cropland/Grassland split =====================================

.read_cropland_grassland_split <- function(l_files_dir, regions) {
  cli::cli_h2("Cropland/Grassland N split")

  # -- EuroAgriDB (EU countries, Einarsson et al. 2019) ---
  euadb_dir <- file.path(
    l_files_dir,
    "EuropeAgriDB-v1.0-results/main_results/tables"
  )

  euadb <- NULL
  if (dir.exists(euadb_dir)) {
    synth_eu <- data.table::fread(
      file.path(euadb_dir, "synthetic_fertilizer.csv")
    ) |>
      tibble::as_tibble() |>
      filter(Symbol %in% c("Q_C", "Q_PG")) |>
      mutate(
        land_use = if_else(Symbol == "Q_C", "Cropland", "Grassland"),
        fert_type = "Synthetic"
      )

    manure_eu <- data.table::fread(
      file.path(euadb_dir, "manure_flows.csv")
    ) |>
      tibble::as_tibble() |>
      filter(Symbol %in% c("A_C", "A_PG")) |>
      mutate(
        land_use = if_else(Symbol == "A_C", "Cropland", "Grassland"),
        fert_type = "Manure"
      )

    euadb <- bind_rows(synth_eu, manure_eu) |>
      rename(iso2c = Region) |>
      mutate(mg_n_lu = Value * 1000) |>  # kt → Mg
      left_join(
        regions |>
          select(iso3c, area_code, area_name) |>
          mutate(iso2c = substr(iso3c, 1, 2)),
        by = "iso2c"
      ) |>
      filter(!is.na(area_code)) |>
      select(year = Year, area_code, area_name, land_use, fert_type,
             mg_n_lu) |>
      mutate(
        lu_share_eu = mg_n_lu / sum(mg_n_lu),
        .by = c(year, area_code, fert_type)
      )

    cli::cli_alert_info(
      "EuroAgriDB: {n_distinct(euadb$area_code)} EU countries"
    )
  } else {
    cli::cli_alert_warning("EuroAgriDB not found at {euadb_dir}")
  }

  # -- Lassaletta et al. 2014 (global grassland share of synthetic N) ---
  lass_file <- file.path(
    global_dir, "input", "Synthetic_N_Grassland_share.csv"
  )

  lass <- NULL
  if (file.exists(lass_file)) {
    lass_raw <- data.table::fread(lass_file, header = TRUE) |>
      tibble::as_tibble()

    # Columns: Country, 1961..2009 (numeric year names)
    year_cols <- grep("^(X?\\d{4})$", names(lass_raw), value = TRUE)

    lass <- lass_raw |>
      tidyr::pivot_longer(
        all_of(year_cols),
        names_to = "year",
        values_to = "grass_share_pct"
      ) |>
      mutate(
        year = as.integer(gsub("X", "", year)),
        grass_share = grass_share_pct / 100
      ) |>
      rename(lassaletta_name = Country) |>
      # Match to WHEP regions — joining via ISO3 if possible
      left_join(
        regions |> select(iso3c, area_code, area_name),
        by = c("lassaletta_name" = "area_name")
      ) |>
      filter(!is.na(area_code)) |>
      select(year, area_code, grass_share)

    n_lass_ctry <- n_distinct(lass$area_code)
    lass_yr_range <- paste0(min(lass$year), "\u2013", max(lass$year))
    cli::cli_alert_info(
      "Lassaletta: {n_lass_ctry} countries, {lass_yr_range}"
    )
  } else {
    cli::cli_alert_warning(
      "Lassaletta grassland share file not found: {lass_file}"
    )
  }

  list(euadb = euadb, lassaletta = lass)
}


# ==== 3. Crop-specific base-year rates ================================

.read_crop_base_rates <- function(l_files_dir, global_dir, regions) {
  cli::cli_h2("Crop-specific base-year N rates")

  # -- West et al. manure by crop (2000) ---
  manure_file <- file.path(global_dir, "input", "Crops_manure_N.csv")
  crop_manure <- NULL
  if (file.exists(manure_file)) {
    crop_manure <- data.table::fread(manure_file) |>
      tibble::as_tibble() |>
      rename(
        crop_name = Crop_name,
        iso3c = ISO,
        manure_mg_n = Manure_N_Mg
      ) |>
      left_join(
        regions |> select(iso3c, area_code),
        by = "iso3c"
      ) |>
      filter(!is.na(area_code)) |>
      summarize(manure_mg_n = sum(manure_mg_n), .by = c(area_code, crop_name))

    n_crops <- n_distinct(crop_manure$crop_name)
    n_ctry <- n_distinct(crop_manure$area_code)
    cli::cli_alert_info("West et al. manure: {n_crops} crops × {n_ctry} countries")
  } else {
    cli::cli_alert_warning("West et al. manure file not found: {manure_file}")
  }

  # -- Mueller et al. 2012 synthetic N rates ---
  synth_file <- file.path(
    global_dir, "input", "Nfertilizationmatrix_filled_nodup.xlsx"
  )
  crop_synthetic <- NULL
  if (file.exists(synth_file)) {
    if (!requireNamespace("openxlsx", quietly = TRUE)) {
      cli::cli_alert_info("Installing {.pkg openxlsx}...")
      install.packages("openxlsx", quiet = TRUE)
    }
    synth_raw <- openxlsx::read.xlsx(
      synth_file,
      sheet = "N fertilization",
      cols = c(1, 2, 5:197),
      rows = c(2:64),
      na.strings = "NaN",
      check.names = TRUE
    )

    crop_synthetic <- synth_raw |>
      tidyr::pivot_longer(
        -c(Proc.Code, Process),
        names_to = "iso3c",
        values_to = "kg_n_ha_synth"
      ) |>
      rename(proc_code = Proc.Code, crop_name = Process) |>
      # Fix non-standard ISO3 codes from Mueller dataset
      mutate(iso3c = recode(iso3c,
        "SRM" = "SCG", "GUA" = "GTM", "BZE" = "BLZ",
        "COS" = "CRI", "ELS" = "SLV", "HAI" = "HTI",
        "HON" = "HND", "ROM" = "ROU", "TRI" = "TTO",
        "ZAR" = "COD", "BHA" = "BHS", "BAR" = "BRB",
        "DMI" = "DMA", "STL" = "LCA"
      )) |>
      left_join(
        regions |> select(iso3c, area_code),
        by = "iso3c"
      ) |>
      filter(!is.na(area_code), !is.na(kg_n_ha_synth))

    n_crops <- n_distinct(crop_synthetic$crop_name)
    n_ctry <- n_distinct(crop_synthetic$area_code)
    cli::cli_alert_info(
      "Mueller et al. synthetic: {n_crops} crops × {n_ctry} countries"
    )
  } else {
    cli::cli_alert_warning("Mueller et al. file not found: {synth_file}")
  }

  list(crop_manure = crop_manure, crop_synthetic = crop_synthetic)
}


# ==== 3b. West et al. 2014 gridded manure N ===========================

# Reads a curated subset of West et al. gridded manure N NetCDFs
# (17 major crops matching EarthStat coverage) and aggregates to
# country-level average rates (kg N/ha).
# If output_dir is provided, also saves pixel-level rates for
# sub-national spatial index computation.

.read_west_gridded_manure <- function(l_files_dir, country_grid,
                                      items_prod, target_res = 0.5,
                                      output_dir = NULL) {
  cli::cli_h2("West et al. 2014 gridded manure N (NetCDF)")

  west_dir <- file.path(l_files_dir, "Manure_Westetal2014", "N")
  if (!dir.exists(west_dir)) {
    cli::cli_alert_warning("West manure dir not found: {west_dir}")
    return(NULL)
  }

  # Map curated crop names -> FAOSTAT item_prod_codes
  crop_map <- tibble::tribble(
    ~west_crop,    ~item_prod_code,
    "barley",      44L,
    "cassava",    340L,
    "cotton",     274L,
    "groundnut",  242L,
    "maize",       56L,
    "millet",      79L,
    "oilpalm",    217L,
    "potato",     328L,
    "rapeseed",   223L,
    "rice",        27L,
    "rye",         71L,
    "sorghum",     83L,
    "soybean",    216L,
    "sugarbeet",  157L,
    "sugarcane",  780L,
    "sunflower",  222L,
    "wheat",       15L
  )

  cli::cli_alert_info("Reading {nrow(crop_map)} West manure N rasters")

  results <- purrr::map(seq_len(nrow(crop_map)), \(i) {
    crop <- crop_map$west_crop[i]
    code <- crop_map$item_prod_code[i]
    nc_path <- file.path(
      west_dir,
      paste0("NappliedperHA", crop, "Westetal.nc")
    )
    if (!file.exists(nc_path)) return(tibble::tibble())

    tryCatch({
      r <- terra::rast(nc_path)
      src_res <- terra::res(r)[1]
      agg_factor <- max(1L, as.integer(round(target_res / src_res)))
      if (agg_factor > 1) {
        r_agg <- terra::aggregate(
          r, fact = agg_factor, fun = "mean", na.rm = TRUE
        )
      } else {
        r_agg <- r
      }
      coords <- terra::xyFromCell(r_agg, seq_len(terra::ncell(r_agg)))
      tibble::tibble(
        lon = round(coords[, 1], 2),
        lat = round(coords[, 2], 2),
        kg_n_ha_manure = terra::values(r_agg)[, 1]
      ) |>
        dplyr::filter(!is.na(kg_n_ha_manure), kg_n_ha_manure > 0) |>
        dplyr::mutate(item_prod_code = code)
    }, error = \(e) {
      cli::cli_alert_warning("Failed: {crop} \u2014 {e$message}")
      tibble::tibble()
    })
  }) |>
    dplyr::bind_rows()

  if (nrow(results) == 0) return(NULL)

  # Join with country grid (single join for both gridded + country)
  results_geo <- results |>
    dplyr::inner_join(country_grid, by = c("lon", "lat"))

  # Save pixel-level rates for sub-national spatial index
  if (!is.null(output_dir)) {
    gridded_path <- file.path(output_dir, "gridded_west_manure.parquet")
    nanoparquet::write_parquet(
      dplyr::select(
        results_geo, lon, lat, area_code, item_prod_code, kg_n_ha_manure
      ),
      gridded_path
    )
    cli::cli_alert_success(
      "Gridded West manure: {nrow(results_geo)} pixels \u2192 {gridded_path}"
    )
  }

  # Aggregate to country-level averages
  country_rates <- results_geo |>
    dplyr::summarize(
      kg_n_ha_manure_west = mean(kg_n_ha_manure, na.rm = TRUE),
      .by = c("area_code", "item_prod_code")
    ) |>
    dplyr::left_join(
      dplyr::select(items_prod, item_prod_code, item_prod_name),
      by = "item_prod_code"
    ) |>
    dplyr::rename(crop_name = item_prod_name) |>
    dplyr::filter(!is.na(crop_name))

  n_crops <- dplyr::n_distinct(country_rates$item_prod_code)
  n_ctry <- dplyr::n_distinct(country_rates$area_code)
  cli::cli_alert_success(
    "West gridded manure: {n_crops} crops \u00d7 {n_ctry} countries"
  )

  country_rates
}


# ==== 3c. EarthStat spatial fertilizer rates ===========================

# Reads 17-crop EarthStat N application rate TIFs and averages to
# country level for use as supplementary base-year synthetic N rates.
# If output_dir is provided, also saves pixel-level rates for
# sub-national spatial index computation.

.read_earthstat_country_rates <- function(l_files_dir, country_grid,
                                          items_prod, target_res = 0.5,
                                          output_dir = NULL) {
  cli::cli_h2("EarthStat Crop Specific Fertilizer rates")

  fert_dir <- file.path(
    l_files_dir, "EarthStat - Crop Specific Fertilizers"
  )
  if (!dir.exists(fert_dir)) {
    cli::cli_alert_warning(
      "EarthStat fertilizer dir not found: {fert_dir}"
    )
    return(NULL)
  }

  crop_map <- tibble::tribble(
    ~es_crop,      ~item_prod_code,
    "barley",      44L,
    "cassava",    340L,
    "cotton",     274L,
    "groundnut",  242L,
    "maize",       56L,
    "millet",      79L,
    "oilpalm",    217L,
    "potato",     328L,
    "rapeseed",   223L,
    "rice",        27L,
    "rye",         71L,
    "sorghum",     83L,
    "soybean",    216L,
    "sugarbeet",  157L,
    "sugarcane",  780L,
    "sunflower",  222L,
    "wheat",       15L
  )

  cli::cli_alert_info(
    "Reading {nrow(crop_map)} EarthStat N application rate rasters"
  )

  results <- purrr::map(seq_len(nrow(crop_map)), \(i) {
    crop <- crop_map$es_crop[i]
    code <- crop_map$item_prod_code[i]
    tif_path <- file.path(
      fert_dir, paste0("Fertilizer_", crop),
      paste0(crop, "_NitrogenApplication_Rate.tif")
    )
    if (!file.exists(tif_path)) return(tibble::tibble())

    tryCatch({
      r <- terra::rast(tif_path)
      src_res <- terra::res(r)[1]
      agg_factor <- max(1L, as.integer(round(target_res / src_res)))
      if (agg_factor > 1) {
        r_agg <- terra::aggregate(
          r, fact = agg_factor, fun = "mean", na.rm = TRUE
        )
      } else {
        r_agg <- r
      }
      coords <- terra::xyFromCell(r_agg, seq_len(terra::ncell(r_agg)))
      tibble::tibble(
        lon = round(coords[, 1], 2),
        lat = round(coords[, 2], 2),
        kg_n_ha_synth_es = terra::values(r_agg)[, 1]
      ) |>
        dplyr::filter(!is.na(kg_n_ha_synth_es), kg_n_ha_synth_es > 0) |>
        dplyr::mutate(item_prod_code = code)
    }, error = \(e) {
      tibble::tibble()
    })
  }) |>
    dplyr::bind_rows()

  if (nrow(results) == 0) return(NULL)

  # Join with country grid (single join for both gridded + country)
  results_geo <- results |>
    dplyr::inner_join(country_grid, by = c("lon", "lat"))

  # Save pixel-level rates for sub-national spatial index
  if (!is.null(output_dir)) {
    gridded_path <- file.path(output_dir, "gridded_earthstat_synth.parquet")
    nanoparquet::write_parquet(
      dplyr::select(
        results_geo, lon, lat, area_code, item_prod_code, kg_n_ha_synth_es
      ),
      gridded_path
    )
    cli::cli_alert_success(
      "Gridded EarthStat synth: {nrow(results_geo)} pixels \u2192 {gridded_path}"
    )
  }

  # Aggregate to country-level averages
  country_rates <- results_geo |>
    dplyr::summarize(
      kg_n_ha_synth_es = mean(kg_n_ha_synth_es, na.rm = TRUE),
      .by = c("area_code", "item_prod_code")
    ) |>
    dplyr::left_join(
      dplyr::select(items_prod, item_prod_code, item_prod_name),
      by = "item_prod_code"
    ) |>
    dplyr::rename(crop_name = item_prod_name) |>
    dplyr::filter(!is.na(crop_name))

  n_crops <- dplyr::n_distinct(country_rates$item_prod_code)
  n_ctry <- dplyr::n_distinct(country_rates$area_code)
  cli::cli_alert_success(
    "EarthStat N rates: {n_crops} crops \u00d7 {n_ctry} countries"
  )

  country_rates
}


# ==== 3d. Coello et al. 2025 crop-group fertilization ==================

# Coello et al. (2025) "A global gridded crop-specific fertilization
# dataset from 1961 to 2019". doi:10.1038/s41597-024-04215-x
#
# Country-level ML-predicted N, P2O5 and K2O application rates (kg/ha)
# for 13 crop groups, 1961-2019.  Predictions are corrected to match
# FAOSTAT country totals (IFA + IFASTAT).
#
# Source: Prediction_corrected.csv from companion GitHub repository
#   https://github.com/STAN-UAntwerp/fertilizers_use_by_crop
# Download: Rscript inst/scripts/download_coello_data.R
#
# Crop groups:
#   1_1 Wheat, 1_2 Maize, 1_3 Rice, 1_4 Other cereals,
#   2_1 Soybean, 2_2 Palm, 2_3 Other oilseeds,
#   3_1 Vegetables, 3_2 Fruits,
#   4 Roots and tubers, 5 Sugar crops, 6 Fiber crops, 7 Other crops

.prepare_coello_inputs <- function(l_files_dir, regions, output_dir) {
  cli::cli_h2("Coello et al. 2025 crop-group fertilization")

  # -- Check for pre-computed output ---
  coello_file <- file.path(output_dir, "coello_crop_rates.parquet")
  if (file.exists(coello_file)) {
    cli::cli_alert_info("Reading pre-computed Coello rates: {coello_file}")
    return(nanoparquet::read_parquet(coello_file))
  }

  # -- Check for raw CSV ---
  coello_dir <- file.path(l_files_dir, "Coello2025")
  csv_file <- file.path(coello_dir, "Prediction_corrected.csv")

  if (!file.exists(csv_file)) {
    cli::cli_alert_info(
      "Coello et al. 2025 data not found at {csv_file}"
    )
    cli::cli_alert_info(
      "  Run: Rscript inst/scripts/download_coello_data.R"
    )
    return(NULL)
  }

  # -- Read raw predictions ---
  cli::cli_alert("Reading {csv_file}")
  raw <- data.table::fread(csv_file) |>
    tibble::as_tibble()

  # Standardize column names
  # Expected: FAOStat_area_code, Crop_Code, Year,
  #   predicted_N_avg_app_cor, predicted_P2O5_avg_app_cor,
  #   predicted_K2O_avg_app_cor
  coello <- raw |>
    transmute(
      area_code = as.integer(FAOStat_area_code),
      coello_crop_code = as.character(Crop_Code),
      year = as.integer(Year),
      kg_n_ha_coello = as.numeric(predicted_N_avg_app_cor),
      kg_p2o5_ha_coello = as.numeric(predicted_P2O5_avg_app_cor),
      kg_k2o_ha_coello = as.numeric(predicted_K2O_avg_app_cor)
    ) |>
    filter(!is.na(area_code), !is.na(year))

  cli::cli_alert_info(
    "Raw Coello: {nrow(coello)} rows, ",
    "{n_distinct(coello$area_code)} countries, ",
    "{min(coello$year)}\u2013{max(coello$year)}"
  )

  # -- Load crop-group mapping ---
  mapping_file <- system.file(
    "extdata", "coello_mapping.csv", package = "whep"
  )
  if (!nzchar(mapping_file)) {
    # Fallback: read from inst/ directly (e.g. during development)
    mapping_file <- file.path(
      getwd(), "inst", "extdata", "coello_mapping.csv"
    )
  }
  if (!file.exists(mapping_file)) {
    cli::cli_alert_warning("coello_mapping.csv not found — returning raw rates")
    return(coello)
  }

  coello_map <- readr::read_csv(mapping_file, show_col_types = FALSE) |>
    select(item_prod_code, coello_crop_code, coello_crop_name)

  n_mapped <- n_distinct(coello_map$item_prod_code)
  n_groups <- n_distinct(coello_map$coello_crop_code)
  cli::cli_alert_info(
    "Coello mapping: {n_mapped} FAOSTAT items \u2192 {n_groups} crop groups"
  )

  # -- Expand crop-group rates to individual FAOSTAT items ---
  # Each item in a crop group gets the same rate (kg/ha) as the group.
  coello_items <- coello |>
    inner_join(coello_map, by = "coello_crop_code", relationship = "many-to-many") |>
    left_join(
      regions |> select(area_code, area_name),
      by = "area_code"
    ) |>
    filter(!is.na(area_name))

  # Clamp negative predictions to zero (can occur from correction)
  coello_items <- coello_items |>
    mutate(
      kg_n_ha_coello = pmax(kg_n_ha_coello, 0),
      kg_p2o5_ha_coello = pmax(kg_p2o5_ha_coello, 0),
      kg_k2o_ha_coello = pmax(kg_k2o_ha_coello, 0)
    )

  # -- Join item names from items_prod ---
  items_ref <- readr::read_csv(
    system.file("extdata", "items_prod.csv", package = "whep"),
    show_col_types = FALSE
  ) |>
    select(item_prod_code, item_prod_name)

  coello_items <- coello_items |>
    left_join(items_ref, by = "item_prod_code") |>
    select(
      year, area_code, area_name,
      item_prod_code, crop_name = item_prod_name,
      coello_crop_code, coello_crop_name,
      kg_n_ha_coello, kg_p2o5_ha_coello, kg_k2o_ha_coello
    )

  # -- Save parquet ---
  nanoparquet::write_parquet(coello_items, coello_file)
  sz <- round(file.size(coello_file) / 1024 / 1024, 1)

  n_crops <- n_distinct(coello_items$crop_name)
  n_ctry <- n_distinct(coello_items$area_code)
  n_yrs <- paste0(min(coello_items$year), "\u2013", max(coello_items$year))

  cli::cli_alert_success(
    "Coello rates: {n_crops} crops \u00d7 {n_ctry} countries \u00d7 {n_yrs}"
  )
  cli::cli_alert_success(
    "  Saved: {coello_file} ({sz} MB, {nrow(coello_items)} rows)"
  )

  coello_items
}


# ==== 4. Atmospheric N deposition (HaNi) ==============================

.prepare_n_deposition <- function(l_files_dir, regions, output_dir) {
  cli::cli_h2("Atmospheric N deposition (HaNi)")

  hani_dir <- file.path(l_files_dir, "HaNi")
  if (!dir.exists(hani_dir)) {
    cli::cli_alert_warning("HaNi directory not found: {hani_dir}")
    return(NULL)
  }

  # Check for pre-computed output
  dep_file <- file.path(output_dir, "n_deposition.parquet")
  if (file.exists(dep_file)) {
    cli::cli_alert_info("Reading pre-computed deposition: {dep_file}")
    return(nanoparquet::read_parquet(dep_file))
  }

  # Or check for pre-computed CSV from Global repo
  global_dep <- file.path(l_files_dir, "Global/output/Global_N_deposition.csv")
  if (file.exists(global_dep)) {
    cli::cli_alert_info("Reading Global repo deposition: {global_dep}")
    dep_raw <- data.table::fread(global_dep) |>
      tibble::as_tibble()

    if ("Deposit_kgNha" %in% names(dep_raw)) {
      dep <- dep_raw |>
        select(year = Year, iso3c = ISO3, deposit_kg_n_ha = Deposit_kgNha) |>
        left_join(regions |> select(iso3c, area_code), by = "iso3c") |>
        filter(!is.na(area_code), !is.na(deposit_kg_n_ha)) |>
        select(year, area_code, deposit_kg_n_ha)

      nanoparquet::write_parquet(dep, dep_file)
      cli::cli_alert_success(
        "n_deposition: {n_distinct(dep$area_code)} countries, ",
        "{min(dep$year)}–{max(dep$year)}"
      )
      return(dep)
    }
  }

  # Extract from HaNi rasters directly
  cli::cli_alert("Extracting N deposition from HaNi rasters (slow)...")

  if (!requireNamespace("raster", quietly = TRUE)) {
    cli::cli_alert_info("Installing {.pkg raster}...")
    install.packages("raster", quiet = TRUE)
  }

  # Country boundaries
  countries <- rnaturalearth::ne_countries(
    scale = "medium", returnclass = "sf"
  ) |>
    select(ISO3 = adm0_a3)

  .extract_hani <- function(zip_path) {
    cli::cli_alert("  Extracting {basename(zip_path)}...")
    br <- raster::brick(utils::unzip(zip_path, exdir = tempdir()))
    vals <- tibble::as_tibble(
      raster::extract(br, countries, fun = mean, na.rm = TRUE)
    )
    names(vals) <- names(br)
    vals |> mutate(ISO3 = countries$ISO3, file = basename(zip_path))
  }

  zip_files <- list.files(hani_dir, pattern = "\\.zip$", full.names = TRUE)
  if (length(zip_files) == 0) {
    cli::cli_alert_warning("No .zip files in {hani_dir}")
    return(NULL)
  }

  dep_raw <- lapply(zip_files, .extract_hani) |> bind_rows()

  dep <- dep_raw |>
    tidyr::pivot_longer(
      starts_with("X"),
      names_to = "layer",
      values_to = "value"
    ) |>
    mutate(
      parameter = gsub(".*ndep_(\\w+)\\.zip", "\\1", file),
      layer_num = as.integer(gsub("X", "", layer)),
      year = 1850L + layer_num,
      value = value / 10000000  # Unit conversion
    ) |>
    filter(!is.na(value), !is.na(year)) |>
    select(year, ISO3, parameter, value) |>
    tidyr::pivot_wider(names_from = parameter, values_from = value) |>
    mutate(deposit_kg_n_ha = nhx + noy) |>
    rename(iso3c = ISO3) |>
    left_join(regions |> select(iso3c, area_code), by = "iso3c") |>
    filter(!is.na(area_code)) |>
    select(year, area_code, deposit_kg_n_ha)

  nanoparquet::write_parquet(dep, dep_file)
  cli::cli_alert_success(
    "n_deposition: {n_distinct(dep$area_code)} countries, ",
    "{min(dep$year)}–{max(dep$year)}"
  )
  dep
}


# ==== 5. Distribute N among crops =====================================

.distribute_n_to_crops <- function(
  n_totals, lu_split, base_rates, crop_areas, regions
) {
  cli::cli_h2("Distributing N among crops")

  euadb <- lu_split$euadb
  lass <- lu_split$lassaletta
  crop_manure <- base_rates$crop_manure
  crop_synthetic <- base_rates$crop_synthetic
  west_rates <- base_rates$west_manure_rates
  es_rates <- base_rates$earthstat_synth_rates
  coello <- base_rates$coello_rates

  # -- Step 1: Split N between cropland and grassland ---
  # For each country-year, compute the LU share of each N type
  # Priority: Grassland_excretion → 100% grassland
  # Synth/Manure: EuroAgriDB (EU) → Lassaletta (rest) → default 0% grassland

  n_by_lu <- n_totals |>
    # Expand to both LU types
    cross_join(tibble(land_use = c("Cropland", "Grassland"))) |>
    left_join(
      euadb |>
        select(year, area_code, land_use, fert_type, lu_share_eu),
      by = c("year", "area_code", "land_use", "fert_type")
    ) |>
    left_join(lass, by = c("year", "area_code")) |>
    mutate(
      # Grassland excretion → 100% grassland
      lu_share = case_when(
        fert_type == "Grassland_excretion" & land_use == "Grassland" ~ 1.0,
        fert_type == "Grassland_excretion" & land_use == "Cropland" ~ 0.0,
        # EuroAgriDB takes priority
        !is.na(lu_share_eu) ~ lu_share_eu,
        # Lassaletta: grass_share for grassland, 1-grass_share for cropland
        !is.na(grass_share) & land_use == "Grassland" ~ grass_share,
        !is.na(grass_share) & land_use == "Cropland" ~ 1 - grass_share,
        # Default: all to cropland
        land_use == "Cropland" ~ 1.0,
        TRUE ~ 0.0
      ),
      mg_n_lu = mg_n * lu_share
    ) |>
    select(year, area_code, area_name, fert_type, land_use, mg_n, mg_n_lu)

  cli::cli_alert_info("LU split done for {n_distinct(n_by_lu$area_code)} countries")

  # -- Step 2: Distribute cropland N among individual crops ---
  #
  # Strategy: three-layer approach
  #   Layer 1 — Mueller/West/EarthStat give crop-specific AND
  #     country-specific differentiation at a base year (~2000).
  #   Layer 2 — Coello et al. (2025) give year-specific temporal

  #     trends per crop group × country (1961–2019).  We compute a
  #     temporal index = coello(year) / coello(base_year) and multiply
  #     the base-year rates to propagate them across all years.
  #   Layer 3 — Scale everything to match FAOSTAT country totals
  #     (which are the authoritative annual constraint).
  #
  # This preserves item-level and geographic differentiation from the
  # detailed spatial datasets while letting Coello drive the historical
  # trajectory.

  if (is.null(crop_areas) || nrow(crop_areas) == 0) {
    cli::cli_alert_warning("No crop area data available — returning LU-level only")
    return(n_by_lu)
  }

  # Cropland N total per country-year-fert_type
  cropland_n <- n_by_lu |>
    filter(land_use == "Cropland", fert_type %in% c("Synthetic", "Manure"))

  # Rate limit: max 4× median to avoid outliers
  rate_limit <- 4

  # ---- Layer 1: Base-year rates (crop × country, ~2000) ----

  # Build a base-rate table with one row per (area_code, crop_name,
  # fert_type).  These are time-invariant snapshots circa 2000.

  # Start with crop areas (unique crop × country combos)
  crop_ids <- crop_areas |>
    distinct(area_code, crop_name)

  # Expand to both fert types
  base_tbl <- crop_ids |>
    cross_join(tibble(fert_type = c("Synthetic", "Manure")))

  # Join Mueller synthetic rates
  if (!is.null(crop_synthetic)) {
    base_tbl <- base_tbl |>
      left_join(
        crop_synthetic |>
          summarize(
            kg_n_ha_synth = mean(kg_n_ha_synth, na.rm = TRUE),
            .by = c(area_code, crop_name)
          ),
        by = c("area_code", "crop_name")
      )
  } else {
    base_tbl$kg_n_ha_synth <- NA_real_
  }

  # Join West manure rates
  if (!is.null(crop_manure)) {
    base_tbl <- base_tbl |>
      left_join(
        crop_manure |> select(area_code, crop_name, manure_mg_n),
        by = c("area_code", "crop_name")
      )
  } else {
    base_tbl$manure_mg_n <- NA_real_
  }

  # Join West gridded manure rates (kg/ha from NetCDF spatial means)
  if (!is.null(west_rates)) {
    base_tbl <- base_tbl |>
      left_join(
        west_rates |> select(area_code, crop_name, kg_n_ha_manure_west),
        by = c("area_code", "crop_name")
      )
  } else {
    base_tbl$kg_n_ha_manure_west <- NA_real_
  }

  # Join EarthStat spatial synthetic N rates (country-level averages)
  if (!is.null(es_rates)) {
    base_tbl <- base_tbl |>
      left_join(
        es_rates |> select(area_code, crop_name, kg_n_ha_synth_es),
        by = c("area_code", "crop_name")
      )
  } else {
    base_tbl$kg_n_ha_synth_es <- NA_real_
  }

  # We also need a reference area_ha for converting manure_mg_n.
  # Take the median area_ha around the base year as a stable reference.
  base_year <- 2000L
  base_area <- crop_areas |>
    filter(year >= base_year - 2L, year <= base_year + 2L,
           !is.na(area_ha), area_ha > 0) |>
    summarize(area_ha_base = median(area_ha), .by = c(area_code, crop_name))

  base_tbl <- base_tbl |>
    left_join(base_area, by = c("area_code", "crop_name"))

  # Priority cascade for base-year rate
  # Mueller/CSV > West gridded > EarthStat spatial
  base_tbl <- base_tbl |>
    mutate(
      kg_n_ha_base = case_when(
        fert_type == "Manure" & !is.na(manure_mg_n) & !is.na(area_ha_base) ~
          manure_mg_n * 1000 / area_ha_base,
        fert_type == "Manure" & !is.na(kg_n_ha_manure_west) ~
          kg_n_ha_manure_west,
        fert_type == "Synthetic" & !is.na(kg_n_ha_synth) ~
          kg_n_ha_synth,
        fert_type == "Synthetic" & !is.na(kg_n_ha_synth_es) ~
          kg_n_ha_synth_es,
        TRUE ~ NA_real_
      )
    )

  # Impute missing base rates with cascading medians
  base_tbl <- base_tbl |>
    mutate(
      kg_n_ha_global_crop = median(kg_n_ha_base, na.rm = TRUE),
      .by = c("crop_name", "fert_type")
    ) |>
    mutate(
      kg_n_ha_country_med = median(kg_n_ha_base, na.rm = TRUE),
      .by = c("area_code", "fert_type")
    ) |>
    mutate(
      kg_n_ha_global = median(kg_n_ha_base, na.rm = TRUE),
      .by = "fert_type"
    ) |>
    mutate(
      # Cap outliers
      kg_n_ha_capped = case_when(
        is.na(kg_n_ha_base) ~ NA_real_,
        kg_n_ha_base == 0 ~ kg_n_ha_country_med / rate_limit,
        kg_n_ha_base > kg_n_ha_country_med * rate_limit ~
          kg_n_ha_country_med * rate_limit,
        TRUE ~ kg_n_ha_base
      ),
      # Imputation cascade
      kg_n_ha_ref = coalesce(
        kg_n_ha_capped,
        kg_n_ha_country_med,
        kg_n_ha_global_crop,
        kg_n_ha_global
      )
    )

  n_base_filled <- sum(!is.na(base_tbl$kg_n_ha_base))
  n_base_total <- nrow(base_tbl)
  cli::cli_alert_info(
    "Base-year rates: {n_base_filled}/{n_base_total} filled before imputation"
  )

  base_ref <- base_tbl |>
    select(area_code, crop_name, fert_type, kg_n_ha_ref)

  # ---- Layer 2: Coello temporal index ----
  # For each crop group × country, compute an index relative to
  # the base year: idx(y) = coello_rate(y) / coello_rate(base_year).
  # This captures the historical trajectory without overriding the
  # crop-level differentiation from Layer 1.

  coello_idx <- NULL
  if (!is.null(coello)) {
    # Load mapping to get Coello crop group for each FAOSTAT item
    mapping_file <- system.file(
      "extdata", "coello_mapping.csv", package = "whep"
    )
    if (!nzchar(mapping_file)) {
      mapping_file <- file.path(
        getwd(), "inst", "extdata", "coello_mapping.csv"
      )
    }
    if (file.exists(mapping_file)) {
      coello_map <- readr::read_csv(mapping_file, show_col_types = FALSE) |>
        select(item_prod_code, coello_crop_code)

      # Coello rates at crop-group level (one rate per group × country × year)
      coello_group <- coello |>
        distinct(year, area_code, coello_crop_code, kg_n_ha_coello)

      # Reference value at base year (or nearest available)
      coello_base <- coello_group |>
        filter(year >= base_year - 2L, year <= base_year + 2L) |>
        summarize(
          kg_n_ha_coello_ref = median(kg_n_ha_coello, na.rm = TRUE),
          .by = c(area_code, coello_crop_code)
        ) |>
        filter(!is.na(kg_n_ha_coello_ref), kg_n_ha_coello_ref > 0)

      # Temporal index: rate(year) / rate(base_year)
      coello_idx <- coello_group |>
        inner_join(coello_base, by = c("area_code", "coello_crop_code")) |>
        mutate(
          coello_temporal_idx = kg_n_ha_coello / kg_n_ha_coello_ref
        ) |>
        select(year, area_code, coello_crop_code, coello_temporal_idx)

      # Map items to crop groups
      items_ref <- readr::read_csv(
        system.file("extdata", "items_prod.csv", package = "whep"),
        show_col_types = FALSE
      ) |>
        select(item_prod_code, item_prod_name)

      item_group <- coello_map |>
        left_join(items_ref, by = "item_prod_code") |>
        select(crop_name = item_prod_name, coello_crop_code)

      n_idx_ctry <- n_distinct(coello_idx$area_code)
      n_idx_yrs <- paste0(min(coello_idx$year), "\u2013", max(coello_idx$year))
      cli::cli_alert_info(
        "Coello temporal index: {n_idx_ctry} countries, {n_idx_yrs}"
      )
    }
  }

  # ---- Combine: base rate × temporal index ----

  n_crop <- crop_areas |>
    filter(!is.na(area_ha), area_ha > 0) |>
    left_join(
      cropland_n |> select(year, area_code, fert_type, land_use, mg_n_lu),
      by = c("year", "area_code"),
      relationship = "many-to-many"
    ) |>
    filter(!is.na(mg_n_lu)) |>
    left_join(base_ref, by = c("area_code", "crop_name", "fert_type"))

  # Apply Coello temporal scaling
  if (!is.null(coello_idx)) {
    n_crop <- n_crop |>
      left_join(item_group, by = "crop_name") |>
      left_join(coello_idx,
                by = c("year", "area_code", "coello_crop_code")) |>
      mutate(
        # Scale base rate by temporal index; default to 1.0 if no index
        kg_n_ha_trended = kg_n_ha_ref * coalesce(coello_temporal_idx, 1.0)
      ) |>
      select(-coello_crop_code, -coello_temporal_idx)
  } else {
    # Without Coello, base rates are used as-is for all years
    n_crop <- n_crop |>
      mutate(kg_n_ha_trended = kg_n_ha_ref)
  }

  # ---- Layer 3: Scale to FAOSTAT country totals ----

  n_crop <- n_crop |>
    mutate(
      mg_n_raw = kg_n_ha_trended * area_ha / 1000,
      mg_n_raw_total = sum(mg_n_raw, na.rm = TRUE),
      scaling = if_else(mg_n_raw_total > 0, mg_n_lu / mg_n_raw_total, 0),
      kg_n_ha = kg_n_ha_trended * scaling,
      mg_n_crop = kg_n_ha * area_ha / 1000,
      .by = c("year", "area_code", "fert_type")
    )

  n_crop_items <- n_distinct(n_crop$crop_name)
  n_crop_ctry <- n_distinct(n_crop$area_code)
  cli::cli_alert_success(
    "Crop-level N: {n_crop_items} crops, {n_crop_ctry} countries"
  )

  n_crop |>
    select(
      year, area_code, area_name, crop_name, land_use, fert_type,
      area_ha, mg_n = mg_n_crop, kg_n_ha
    )
}


# ==== 6. NSBNF (non-symbiotic BNF) ====================================

.compute_nsbnf <- function(crop_areas) {
  cli::cli_h2("Non-symbiotic BNF")
  # Default 13 kgN/ha (Herridge et al. 2008)
  nsbnf_rate <- 13

  result <- crop_areas |>
    filter(!is.na(area_ha), area_ha > 0) |>
    mutate(
      fert_type = "NSBNF",
      kg_n_ha = nsbnf_rate,
      mg_n = kg_n_ha * area_ha / 1000,
      land_use = "Cropland"
    ) |>
    select(year, area_code, area_name, crop_name, land_use, fert_type,
           area_ha, mg_n, kg_n_ha)

  total_mg <- round(sum(result$mg_n, na.rm = TRUE))
  cli::cli_alert_success("NSBNF: {nsbnf_rate} kgN/ha × {nrow(result)} obs = {total_mg} Mg total")
  result
}


# ==== 7. Spatial N rate index ==========================================

# Computes a sub-national spatial index from pixel-level West (manure)
# and EarthStat (synthetic) gridded rates.  For each pixel × crop ×
# fert_type:
#   spatial_n_index = pixel_rate / country_mean_rate
# This captures within-country spatial variation that would otherwise
# be lost when aggregating to country averages.

.compute_spatial_n_index <- function(output_dir) {
  cli::cli_h2("Computing spatial N rate index")

  west_path <- file.path(output_dir, "gridded_west_manure.parquet")
  es_path <- file.path(output_dir, "gridded_earthstat_synth.parquet")

  parts <- list()

  # --- West manure spatial index ---
  if (file.exists(west_path)) {
    west <- nanoparquet::read_parquet(west_path)

    west_means <- west |>
      dplyr::summarize(
        country_mean = mean(kg_n_ha_manure, na.rm = TRUE),
        .by = c("area_code", "item_prod_code")
      ) |>
      dplyr::filter(country_mean > 0)

    west_idx <- west |>
      dplyr::inner_join(west_means, by = c("area_code", "item_prod_code")) |>
      dplyr::mutate(
        spatial_n_index = kg_n_ha_manure / country_mean,
        fert_type = "Manure"
      ) |>
      dplyr::select(lon, lat, item_prod_code, fert_type, spatial_n_index)

    parts <- c(parts, list(west_idx))
    cli::cli_alert_info(
      "West manure spatial index: {nrow(west_idx)} pixels"
    )
  }

  # --- EarthStat synthetic spatial index ---
  if (file.exists(es_path)) {
    es <- nanoparquet::read_parquet(es_path)

    es_means <- es |>
      dplyr::summarize(
        country_mean = mean(kg_n_ha_synth_es, na.rm = TRUE),
        .by = c("area_code", "item_prod_code")
      ) |>
      dplyr::filter(country_mean > 0)

    es_idx <- es |>
      dplyr::inner_join(es_means, by = c("area_code", "item_prod_code")) |>
      dplyr::mutate(
        spatial_n_index = kg_n_ha_synth_es / country_mean,
        fert_type = "Synthetic"
      ) |>
      dplyr::select(lon, lat, item_prod_code, fert_type, spatial_n_index)

    parts <- c(parts, list(es_idx))
    cli::cli_alert_info(
      "EarthStat synth spatial index: {nrow(es_idx)} pixels"
    )
  }

  if (length(parts) == 0) {
    cli::cli_alert_warning("No gridded rate data for spatial index")
    return(NULL)
  }

  spatial_idx <- dplyr::bind_rows(parts)

  # Cap extreme values (0.1–10× country mean)
  spatial_idx <- spatial_idx |>
    dplyr::mutate(
      spatial_n_index = pmax(0.1, pmin(10, spatial_n_index))
    )

  idx_path <- file.path(output_dir, "spatial_n_index.parquet")
  nanoparquet::write_parquet(spatial_idx, idx_path)
  cli::cli_alert_success(
    "Spatial N index: {nrow(spatial_idx)} pixels \u2192 {idx_path}"
  )

  spatial_idx
}


# ==== Main execution ==================================================

cli::cli_h1("Preparing nitrogen inputs")

# -- Load crop areas from FAOSTAT primary production ---
# This should match what prepare_spatialize_inputs.R uses
crop_areas_file <- file.path(output_dir, "country_areas.parquet")
crop_areas <- NULL
if (file.exists(crop_areas_file)) {
  cli::cli_alert("Reading crop areas from {crop_areas_file}")
  crop_areas_raw <- nanoparquet::read_parquet(crop_areas_file)
  # Adapt columns: the country_areas parquet has area_code, year,
  # item_prod_code, area_ha (among others)
  crop_areas <- crop_areas_raw |>
    left_join(regions |> select(area_code, area_name), by = "area_code") |>
    left_join(items_prod |> select(item_prod_code, item_prod_name),
              by = "item_prod_code") |>
    transmute(
      year,
      area_code,
      area_name,
      crop_name = item_prod_name,
      area_ha = harvested_area_ha
    ) |>
    filter(!is.na(area_ha), area_ha > 0)
  n_ca_crops <- n_distinct(crop_areas$crop_name)
  n_ca_ctry <- n_distinct(crop_areas$area_code)
  ca_yr_range <- paste0(min(crop_areas$year), "\u2013", max(crop_areas$year))
  cli::cli_alert_info(
    "Crop areas: {n_ca_crops} crops, {n_ca_ctry} countries, {ca_yr_range}"
  )
} else {
  cli::cli_alert_warning(
    "country_areas.parquet not found — run prepare_spatialize_inputs.R first"
  )
  cli::cli_alert_info("Proceeding with country-level-only N estimates")
}

# -- Step 1: FAOSTAT totals ---
n_totals <- .read_faostat_totals(l_files_dir, regions)

# -- Step 1b: P and K totals ---
pk_totals <- .read_faostat_pk_totals(l_files_dir, regions)

# -- Step 2: Cropland/Grassland split ---
lu_split <- .read_cropland_grassland_split(l_files_dir, regions)

# -- Step 3: Base-year crop-specific rates ---
base_rates <- .read_crop_base_rates(l_files_dir, global_dir, regions)

# -- Step 3b: Supplementary spatial base rates ---
# Load country grid for spatial aggregation
country_grid_file <- file.path(output_dir, "country_grid.parquet")
if (file.exists(country_grid_file)) {
  country_grid <- nanoparquet::read_parquet(country_grid_file)

  # West gridded manure N (17 major crops, NetCDF -> country averages)
  # Also saves pixel-level rates for sub-national spatial index
  west_rates <- .read_west_gridded_manure(
    l_files_dir, country_grid, items_prod, 0.5, output_dir
  )
  base_rates$west_manure_rates <- west_rates

  # EarthStat spatial N rates (17 crops -> country averages)
  # Also saves pixel-level rates for sub-national spatial index
  es_rates <- .read_earthstat_country_rates(
    l_files_dir, country_grid, items_prod, 0.5, output_dir
  )
  base_rates$earthstat_synth_rates <- es_rates

  # Compute sub-national spatial N rate index from pixel-level data
  .compute_spatial_n_index(output_dir)
} else {
  cli::cli_alert_info(
    "country_grid.parquet not found \u2014 ",
    "skipping spatial rate enhancement"
  )
}

# -- Step 3c: Coello et al. 2025 crop-group rates ---
coello_rates <- .prepare_coello_inputs(l_files_dir, regions, output_dir)
if (!is.null(coello_rates)) {
  base_rates$coello_rates <- coello_rates
}

# -- Step 4: N deposition ---
n_deposition <- .prepare_n_deposition(l_files_dir, regions, output_dir)

# -- Step 5: Distribute to crops ---
if (!is.null(crop_areas)) {
  n_crops <- .distribute_n_to_crops(
    n_totals, lu_split, base_rates, crop_areas, regions
  )
} else {
  n_crops <- NULL
}

# -- Step 6: NSBNF ---
if (!is.null(crop_areas)) {
  nsbnf <- .compute_nsbnf(crop_areas)
} else {
  nsbnf <- NULL
}

# -- Step 7: Add deposition to crop-level results ---
n_dep_crop <- NULL
if (!is.null(crop_areas) && !is.null(n_deposition)) {
  n_dep_crop <- crop_areas |>
    left_join(n_deposition, by = c("year", "area_code")) |>
    filter(!is.na(deposit_kg_n_ha)) |>
    mutate(
      fert_type = "Deposition",
      kg_n_ha = deposit_kg_n_ha,
      mg_n = kg_n_ha * area_ha / 1000,
      land_use = "Cropland"
    ) |>
    select(year, area_code, area_name, crop_name, land_use, fert_type,
           area_ha, mg_n, kg_n_ha)
}

# -- Step 8: Distribute P/K to crops using Coello rates ---
pk_crop <- NULL
if (!is.null(crop_areas) && !is.null(coello_rates) && !is.null(pk_totals)) {
  cli::cli_h2("Distributing P/K among crops (Coello)")

  # Coello rates give crop-group-specific P2O5 and K2O kg/ha.
  # Use these as proportional weights to distribute FAOSTAT country
  # totals to individual crops.
  pk_crop_raw <- crop_areas |>
    filter(!is.na(area_ha), area_ha > 0) |>
    left_join(
      coello_rates |>
        select(year, area_code, crop_name,
               kg_p2o5_ha_coello, kg_k2o_ha_coello),
      by = c("year", "area_code", "crop_name")
    ) |>
    filter(!is.na(kg_p2o5_ha_coello) | !is.na(kg_k2o_ha_coello))

  if (nrow(pk_crop_raw) > 0) {
    # pk_totals is long-form: year, area_code, nutrient (P/K), mg_nutrient
    pk_ref <- pk_totals |>
      filter(!is.na(mg_nutrient), mg_nutrient > 0)

    # Distribute P2O5 to crops
    pk_p <- pk_crop_raw |>
      filter(!is.na(kg_p2o5_ha_coello)) |>
      mutate(
        mg_raw = kg_p2o5_ha_coello * area_ha / 1000,
        mg_raw_total = sum(mg_raw, na.rm = TRUE),
        .by = c("year", "area_code")
      ) |>
      left_join(
        pk_ref |>
          filter(nutrient == "P") |>
          select(year, area_code, mg_total = mg_nutrient),
        by = c("year", "area_code")
      ) |>
      filter(!is.na(mg_total), mg_raw_total > 0) |>
      mutate(
        scaling = mg_total / mg_raw_total,
        kg_ha = kg_p2o5_ha_coello * scaling,
        mg = kg_ha * area_ha / 1000,
        fert_type = "Synthetic_P2O5",
        land_use = "Cropland"
      ) |>
      select(year, area_code, area_name, crop_name, land_use, fert_type,
             area_ha, mg_n = mg, kg_n_ha = kg_ha)

    # Distribute K2O to crops
    pk_k <- pk_crop_raw |>
      filter(!is.na(kg_k2o_ha_coello)) |>
      mutate(
        mg_raw = kg_k2o_ha_coello * area_ha / 1000,
        mg_raw_total = sum(mg_raw, na.rm = TRUE),
        .by = c("year", "area_code")
      ) |>
      left_join(
        pk_ref |>
          filter(nutrient == "K") |>
          select(year, area_code, mg_total = mg_nutrient),
        by = c("year", "area_code")
      ) |>
      filter(!is.na(mg_total), mg_raw_total > 0) |>
      mutate(
        scaling = mg_total / mg_raw_total,
        kg_ha = kg_k2o_ha_coello * scaling,
        mg = kg_ha * area_ha / 1000,
        fert_type = "Synthetic_K2O",
        land_use = "Cropland"
      ) |>
      select(year, area_code, area_name, crop_name, land_use, fert_type,
             area_ha, mg_n = mg, kg_n_ha = kg_ha)

    pk_crop <- bind_rows(pk_p, pk_k)
    n_pk_crops <- n_distinct(pk_crop$crop_name)
    n_pk_ctry <- n_distinct(pk_crop$area_code)
    cli::cli_alert_success(
      "P/K crop distribution: {n_pk_crops} crops \u00d7 {n_pk_ctry} countries"
    )
  }
}

# -- Combine all nutrient inputs ---
all_parts <- list(n_crops, nsbnf, n_dep_crop, pk_crop)
all_parts <- all_parts[!sapply(all_parts, is.null)]

if (length(all_parts) > 0) {
  nitrogen_inputs <- bind_rows(all_parts) |>
    arrange(year, area_code, crop_name, fert_type)

  nanoparquet::write_parquet(
    nitrogen_inputs,
    file.path(output_dir, "nitrogen_inputs.parquet")
  )

  n_crops_u <- n_distinct(nitrogen_inputs$crop_name)
  n_ctry <- n_distinct(nitrogen_inputs$area_code)
  n_years <- n_distinct(nitrogen_inputs$year)
  n_types <- n_distinct(nitrogen_inputs$fert_type)
  sz <- round(file.size(file.path(output_dir, "nitrogen_inputs.parquet")) /
                1024 / 1024, 1)

  cli::cli_h1("Summary")
  cli::cli_alert_success(
    "nitrogen_inputs.parquet: {nrow(nitrogen_inputs)} rows ({sz} MB)"
  )
  cli::cli_alert_info(
    "  {n_crops_u} crops × {n_ctry} countries × {n_years} years × {n_types} fert types"
  )
  cli::cli_alert_info(
    "  Types: {paste(unique(nitrogen_inputs$fert_type), collapse = ', ')}"
  )
} else {
  cli::cli_alert_warning("No nitrogen input data generated")
}

# -- Save P/K totals ---
if (!is.null(pk_totals) && nrow(pk_totals) > 0) {
  nanoparquet::write_parquet(
    pk_totals,
    file.path(output_dir, "pk_totals.parquet")
  )
  cli::cli_alert_success(
    "pk_totals.parquet: {nrow(pk_totals)} rows saved"
  )
}

cli::cli_alert_success("Done!")

# --- Future extensions (TODO) ---
# - Coello et al. 2025 gridded data (doi:10.6084/m9.figshare.25435432):
#   5-arcmin GeoTIFF maps for sub-national spatial disaggregation.
#   Country-level CSV is now integrated (Steps 3c and 8).
# - afsetools::Calc_N_fix() for full BNF (crop + weed + NS components).
#   Currently only NSBNF is computed.
# - Refine P/K crop-level pipeline: EarthStat P/K rate TIFs could
#   supplement Coello for crops outside the 13-group mapping.
