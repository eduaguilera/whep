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
#   6. Coello et al. 2025 (planned):
#      - Crop-group fertilization rates, 13 groups, N/P/K, 1961–2019
#        doi:10.1038/s41597-024-04215-x
#
# Outputs (to L_files/whep/inputs/):
#   - nitrogen_inputs.parquet: crop × country × year × fert_type
#   - n_deposition.parquet: country × year deposition rates
#   - pk_totals.parquet: P and K country × year totals
#
# Requires: FAOSTAT CSVs, EuroAgriDB results, Global/input/,
# HaNi NetCDFs, West manure NetCDFs, EarthStat fertilizer TIFs.
# -----------------------------------------------------------------------

library(dplyr, warn.conflicts = FALSE)

# ==== Configuration ====================================================

l_files_dir <- "WHEP_L_FILES_DIR_PLACEHOLDER"
global_dir <- "WHEP_GLOBAL_DIR_PLACEHOLDER"
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

  cli::cli_alert_success(
    "P/K totals: {n_distinct(fert_pk$area_code)} countries, ",
    "P={sum(fert_pk$nutrient == 'P')} obs, ",
    "K={sum(fert_pk$nutrient == 'K')} obs"
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
    lass_raw <- data.table::fread(lass_file) |>
      tibble::as_tibble()

    # Semicolon-separated, columns: Country, X1961..X2009
    # (or just 1961..2009 as numbers)
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

    cli::cli_alert_info(
      "Lassaletta: {n_distinct(lass$area_code)} countries, ",
      "{min(lass$year)}–{max(lass$year)}"
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

.read_west_gridded_manure <- function(l_files_dir, country_grid,
                                      items_prod, target_res = 0.5) {
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

  # Aggregate to country-level weighted averages
  country_rates <- results |>
    dplyr::inner_join(country_grid, by = c("lon", "lat")) |>
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

.read_earthstat_country_rates <- function(l_files_dir, country_grid,
                                          items_prod, target_res = 0.5) {
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

  country_rates <- results |>
    dplyr::inner_join(country_grid, by = c("lon", "lat")) |>
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


# ==== 3d. Coello et al. 2025 (future integration) =====================

# Coello et al. (2025) "A global gridded crop-specific fertilization
# dataset from 1961 to 2019". doi:10.1038/s41597-024-04215-x
# Coverage: 13 crop groups, N/P/K, 1961-2019, gridded.
# Crop groups: Cereals excl. rice, Rice, Roots & tubers, Sugar crops,
# Pulses, Oil crops, Vegetables, Fruits, Stimulant crops, Spices,
# Fibre crops, Tobacco, Rubber.

.prepare_coello_inputs <- function(l_files_dir) {
  cli::cli_h2("Coello et al. 2025 crop-group fertilization")

  coello_dir <- file.path(l_files_dir, "Coello2025")
  if (!dir.exists(coello_dir)) {
    cli::cli_alert_info(
      "Coello et al. 2025 data not available at {coello_dir}"
    )
    cli::cli_alert_info(
      "  doi:10.1038/s41597-024-04215-x \u2014 download to {coello_dir}"
    )
    return(NULL)
  }

  # TODO: Read Coello gridded N/P/K time-series when data available
  cli::cli_alert_info("Coello data reading not yet implemented")
  NULL
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
  # Uses base-year rates from Mueller (synthetic) and West (manure),
  # then scales to match FAOSTAT country totals

  if (is.null(crop_areas) || nrow(crop_areas) == 0) {
    cli::cli_alert_warning("No crop area data available — returning LU-level only")
    return(n_by_lu)
  }

  # Cropland N total per country-year-fert_type
  cropland_n <- n_by_lu |>
    filter(land_use == "Cropland", fert_type %in% c("Synthetic", "Manure"))

  # Rate limit: max 4× median to avoid outliers
  rate_limit <- 4

  n_crop <- crop_areas |>
    filter(!is.na(area_ha), area_ha > 0) |>
    left_join(cropland_n |> select(-mg_n), by = c("year", "area_code")) |>
    filter(!is.na(mg_n_lu))

  # Join base-year rates
  if (!is.null(crop_manure)) {
    n_crop <- n_crop |>
      left_join(
        crop_manure |> select(area_code, crop_name, manure_mg_n),
        by = c("area_code", "crop_name")
      )
  }
  if (!is.null(crop_synthetic)) {
    n_crop <- n_crop |>
      left_join(
        crop_synthetic |>
          summarize(
            kg_n_ha_synth = mean(kg_n_ha_synth, na.rm = TRUE),
            .by = c(area_code, crop_name)
          ),
        by = c("area_code", "crop_name")
      )
  }

  # Join West gridded manure rates (kg/ha from NetCDF spatial means)
  if (!is.null(west_rates)) {
    n_crop <- n_crop |>
      left_join(
        west_rates |> select(area_code, crop_name, kg_n_ha_manure_west),
        by = c("area_code", "crop_name")
      )
  } else {
    n_crop$kg_n_ha_manure_west <- NA_real_
  }

  # Join EarthStat spatial synthetic N rates (country-level averages)
  if (!is.null(es_rates)) {
    n_crop <- n_crop |>
      left_join(
        es_rates |> select(area_code, crop_name, kg_n_ha_synth_es),
        by = c("area_code", "crop_name")
      )
  } else {
    n_crop$kg_n_ha_synth_es <- NA_real_
  }

  # Compute base-year rate for each crop
  # Priority: Mueller/CSV > West gridded > EarthStat spatial
  n_crop <- n_crop |>
    mutate(
      kg_n_ha_base = case_when(
        fert_type == "Manure" & !is.na(manure_mg_n) ~
          manure_mg_n * 1000 / area_ha,
        fert_type == "Manure" & !is.na(kg_n_ha_manure_west) ~
          kg_n_ha_manure_west,
        fert_type == "Synthetic" & !is.na(kg_n_ha_synth) ~
          kg_n_ha_synth,
        fert_type == "Synthetic" & !is.na(kg_n_ha_synth_es) ~
          kg_n_ha_synth_es,
        TRUE ~ NA_real_
      )
    )

  # Impute missing rates with cascading medians
  n_crop <- n_crop |>
    mutate(
      # Global crop-specific median
      kg_n_ha_global_crop = median(kg_n_ha_base, na.rm = TRUE),
      .by = c("crop_name", "fert_type")
    ) |>
    mutate(
      # Country median across all crops
      kg_n_ha_country_med = median(kg_n_ha_base, na.rm = TRUE),
      .by = c("year", "area_code", "fert_type")
    ) |>
    mutate(
      # Global all-crop median
      kg_n_ha_global = median(kg_n_ha_base, na.rm = TRUE),
      .by = "fert_type"
    ) |>
    mutate(
      # Cap outliers at rate_limit × median
      kg_n_ha_capped = case_when(
        is.na(kg_n_ha_base) ~ NA_real_,
        kg_n_ha_base == 0 ~ kg_n_ha_country_med / rate_limit,
        kg_n_ha_base > kg_n_ha_country_med * rate_limit ~
          kg_n_ha_country_med * rate_limit,
        TRUE ~ kg_n_ha_base
      ),
      # Imputation cascade
      kg_n_ha_imputed = coalesce(
        kg_n_ha_capped,
        kg_n_ha_country_med,
        kg_n_ha_global_crop,
        kg_n_ha_global
      )
    )

  # Scale to match FAOSTAT country totals
  n_crop <- n_crop |>
    mutate(
      mg_n_raw = kg_n_ha_imputed * area_ha / 1000,
      mg_n_raw_total = sum(mg_n_raw, na.rm = TRUE),
      scaling = if_else(mg_n_raw_total > 0, mg_n_lu / mg_n_raw_total, 0),
      kg_n_ha = kg_n_ha_imputed * scaling,
      mg_n_crop = kg_n_ha * area_ha / 1000,
      .by = c("year", "area_code", "fert_type")
    )

  cli::cli_alert_success(
    "Crop-level N: {n_distinct(n_crop$crop_name)} crops, ",
    "{n_distinct(n_crop$area_code)} countries"
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
      area_ha
    ) |>
    filter(!is.na(area_ha), area_ha > 0)
  cli::cli_alert_info(
    "Crop areas: {n_distinct(crop_areas$crop_name)} crops, ",
    "{n_distinct(crop_areas$area_code)} countries, ",
    "{min(crop_areas$year)}–{max(crop_areas$year)}"
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
  west_rates <- .read_west_gridded_manure(
    l_files_dir, country_grid, items_prod, 0.5
  )
  base_rates$west_manure_rates <- west_rates

  # EarthStat spatial N rates (17 crops -> country averages)
  es_rates <- .read_earthstat_country_rates(
    l_files_dir, country_grid, items_prod, 0.5
  )
  base_rates$earthstat_synth_rates <- es_rates
} else {
  cli::cli_alert_info(
    "country_grid.parquet not found \u2014 ",
    "skipping spatial rate enhancement"
  )
}

# -- Step 3c: Coello et al. 2025 (future) ---
.prepare_coello_inputs(l_files_dir)

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

# -- Combine all nitrogen inputs ---
all_parts <- list(n_crops, nsbnf, n_dep_crop)
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
# - Coello et al. 2025 (doi:10.1038/s41597-024-04215-x):
#   Download and integrate gridded N/P/K time-series (13 crop groups,
#   1961-2019) when data repository is accessible.
# - afsetools::Calc_N_fix() for full BNF (crop + weed + NS components).
#   Currently only NSBNF is computed.
# - Distribute P and K to individual crops (similar to N pipeline)
#   using EarthStat P/K rate TIFs as crop-specific base rates.
