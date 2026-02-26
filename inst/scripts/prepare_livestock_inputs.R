# -----------------------------------------------------------------------
# prepare_livestock_inputs.R
#
# Processes FAOSTAT livestock stock data and computes manure nitrogen
# production per country, year, and species group.  Distributes
# manure N spatially using LUH2 pasture/rangeland and cropland
# patterns as a proxy for livestock density.
#
# Data sources and references:
#   1. FAOSTAT Production_Livestock (FAO, 2024)
#      Element 5111 = "Stocks" (number of live animals, heads)
#      22 species from 1961–present, ~200 countries.
#      https://www.fao.org/faostat/en/#data/QCL
#
#   2. FAOSTAT Environment_LivestockManure (manure N totals,
#      used as cross-validation reference)
#
#   3. IPCC 2006 Guidelines Vol 4 Ch 10, Tables 10.19 / 10.21
#      Default Nex rates (kg N/head/year) by species and region.
#      https://www.ipcc-nggip.iges.or.jp/public/2006gl/vol4.html
#
#   4. LUH2 v2h (Hurtt et al. 2020) — pasture + rangeland fractions
#      as spatial proxy for grazing livestock distribution.
#      doi:10.5194/gmd-13-5425-2020
#
#   5. Gridded Livestock of the World v3 (Gilbert et al. 2018)
#      doi:10.1371/journal.pone.0217754
#      NOT currently available locally — download instructions
#      included for future enhancement.
#
# Pre-FAOSTAT methodology (before 1961):
#   Livestock stocks are back-extrapolated using 1961 species
#   composition scaled by LUH2 pastureland trends per country.
#
# Outputs (to L_files/whep/inputs/):
#   - livestock_stocks.parquet:
#       year, area_code, area_name, species, species_group,
#       heads, nex_kg_n_head, manure_n_mg
#   - gridded_livestock.parquet:
#       lon, lat, year, area_code, species_group,
#       manure_n_mg, grazing_n_mg, applied_n_mg
# -----------------------------------------------------------------------

library(dplyr, warn.conflicts = FALSE)

# ==== Configuration ====================================================

l_files_dir <- "C:/Users/53272530E/OneDrive/L_files"
output_dir  <- file.path(l_files_dir, "whep", "inputs")

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# WHEP reference tables
regions <- readr::read_csv(
  system.file("extdata", "regions.csv", package = "whep"),
  show_col_types = FALSE
)


# ==== Species group mapping ============================================

# Map FAOSTAT livestock item codes to aggregated species groups.
# Nex rates from IPCC 2006 Vol 4 Ch 10 Tables 10.19 & 10.21
# (global approximate defaults, kg N per head per year).
# grazing_frac = fraction of manure deposited on pasture vs applied.

.livestock_species_mapping <- function() {
  tibble::tribble(
    ~item_code, ~species,              ~species_group, ~nex_kg_n, ~grazing_frac,
    866L,       "Cattle",              "Cattle",          50.0,      0.45,
    946L,       "Buffaloes",           "Buffalo",         34.0,      0.50,
    976L,       "Sheep",               "Sheep & Goats",   12.0,      0.80,
    1016L,      "Goats",               "Sheep & Goats",   12.0,      0.80,
    1034L,      "Pigs",                "Pigs",            16.0,      0.05,
    1057L,      "Chickens",            "Poultry",          0.6,      0.10,
    1068L,      "Ducks",               "Poultry",          0.6,      0.10,
    1072L,      "Geese and guinea fowls", "Poultry",       0.6,      0.10,
    1079L,      "Turkeys",             "Poultry",          1.1,      0.10,
    1083L,      "Pigeons, other birds","Poultry",          0.3,      0.10,
    1096L,      "Horses",              "Equines",         26.0,      0.60,
    1107L,      "Asses",               "Equines",         26.0,      0.60,
    1110L,      "Mules",               "Equines",         26.0,      0.60,
    1126L,      "Camels",              "Camels",          46.0,      0.90,
    1157L,      "Camelids, other",     "Camels",          28.0,      0.90,
    1140L,      "Rabbits and hares",   "Other",            2.0,      0.05,
    1150L,      "Rodents, other",      "Other",            2.0,      0.05,
    1171L,      "Animals live nes",    "Other",           10.0,      0.30
  )
}


# ==== 1. Read FAOSTAT livestock stocks =================================

.read_faostat_livestock <- function(l_files_dir, regions) {
  cli::cli_h2("FAOSTAT livestock stocks")

  csv_path <- file.path(
    l_files_dir,
    "FAOSTAT/Production_Livestock_E_All_Data_(Normalized).csv"
  )

  if (!file.exists(csv_path)) {
    cli::cli_abort("FAOSTAT livestock file not found: {csv_path}")
  }

  species_map <- .livestock_species_mapping()

  fao_raw <- data.table::fread(csv_path) |>
    tibble::as_tibble()

  stocks <- fao_raw |>
    dplyr::filter(
      `Element Code` == 5111L,  # Stocks (live animals)
      `Item Code` %in% species_map$item_code,
      !is.na(Value),
      Value > 0
    ) |>
    dplyr::transmute(
      area_code = as.integer(`Area Code`),
      item_code = as.integer(`Item Code`),
      year      = as.integer(Year),
      heads     = Value
    ) |>
    # Remove regional aggregates (code >= 5000)
    dplyr::filter(area_code < 5000L)

  # Join species metadata
  stocks <- stocks |>
    dplyr::inner_join(
      dplyr::select(species_map, item_code, species, species_group,
                     nex_kg_n, grazing_frac),
      by = "item_code"
    ) |>
    dplyr::inner_join(
      dplyr::select(regions, area_code, area_name),
      by = "area_code"
    )

  # Exclude aggregate items (Cattle and Buffaloes, Poultry Birds, etc.)
  # These are FAOSTAT-computed totals — we use individual species instead.
  aggregate_codes <- c(1746L, 2029L, 1749L, 1181L)
  stocks <- stocks |>
    dplyr::filter(!item_code %in% aggregate_codes)

  n_sp   <- dplyr::n_distinct(stocks$species)
  n_ctry <- dplyr::n_distinct(stocks$area_code)
  n_yr   <- dplyr::n_distinct(stocks$year)
  cli::cli_alert_success(
    "Livestock stocks: {n_sp} species, {n_ctry} countries, ",
    "{min(stocks$year)}\u2013{max(stocks$year)}"
  )

  stocks
}


# ==== 2. Compute manure N production ===================================

.compute_manure_n <- function(stocks) {
  cli::cli_h2("Computing manure nitrogen production")

  # Manure N (Mg) = heads × Nex (kg N/head/yr) / 1000
  result <- stocks |>
    dplyr::mutate(
      manure_n_mg   = heads * nex_kg_n / 1000,
      grazing_n_mg  = manure_n_mg * grazing_frac,
      applied_n_mg  = manure_n_mg * (1 - grazing_frac)
    )

  total_mg <- round(sum(result$manure_n_mg, na.rm = TRUE) / 1e6, 1)
  cli::cli_alert_success(
    "Total manure N: {total_mg} Tg (all years combined)"
  )

  result
}


# ==== 3. Validate against FAOSTAT Emissions ============================

.validate_with_emissions <- function(livestock_n, l_files_dir, regions) {
  cli::cli_h2("Cross-validating with FAOSTAT Emissions_LivestockManure")

  emi_path <- file.path(
    l_files_dir,
    "FAOSTAT/Environment_LivestockManure_E_All_Data_(Normalized).csv"
  )
  if (!file.exists(emi_path)) {
    cli::cli_alert_info(
      "Emissions_LivestockManure not found \u2014 skipping validation"
    )
    return(invisible(NULL))
  }

  emi_raw <- data.table::fread(emi_path) |>
    tibble::as_tibble()

  # Total manure N applied + pasture from FAOSTAT Emissions
  fao_manure <- emi_raw |>
    dplyr::filter(
      Item == "All Animals",
      Element %in% c(
        "Manure applied to soils (N content)",
        "Manure left on pasture (N content)"
      )
    ) |>
    dplyr::transmute(
      area_code = as.integer(`Area Code`),
      year      = as.integer(Year),
      mg_n_fao  = Value / 1000  # kg -> Mg
    ) |>
    dplyr::summarize(
      mg_n_fao = sum(mg_n_fao, na.rm = TRUE),
      .by = c(area_code, year)
    ) |>
    dplyr::filter(area_code < 5000L)

  # Our computed totals
  our_totals <- livestock_n |>
    dplyr::summarize(
      mg_n_ours = sum(manure_n_mg, na.rm = TRUE),
      .by = c(area_code, year)
    )

  # Compare
  comparison <- dplyr::inner_join(
    fao_manure, our_totals,
    by = c("area_code", "year")
  ) |>
    dplyr::mutate(ratio = mg_n_ours / mg_n_fao) |>
    dplyr::filter(!is.na(ratio), is.finite(ratio))

  if (nrow(comparison) > 0) {
    med_ratio <- round(median(comparison$ratio, na.rm = TRUE), 2)
    cli::cli_alert_info(
      "Median ratio (ours / FAOSTAT Emissions): {med_ratio}"
    )
  }

  invisible(comparison)
}


# ==== 4. Pre-FAOSTAT backfill (before 1961) ============================

.backfill_livestock <- function(livestock_n, l_files_dir,
                                country_grid, year_range) {
  cli::cli_h2("Pre-FAOSTAT livestock backfill")

  fao_first_year <- 1961L
  backfill_years <- year_range[year_range < fao_first_year]

  if (length(backfill_years) == 0) {
    cli::cli_alert_info("No pre-FAOSTAT years to backfill")
    return(livestock_n)
  }

  # Use LUH2 pastureland trends to scale 1961 livestock backward
  luh2_dir <- file.path(l_files_dir, "LUH2", "LUH2 v2h")
  if (!dir.exists(luh2_dir)) {
    cli::cli_alert_warning("LUH2 not found \u2014 skipping backfill")
    return(livestock_n)
  }

  cli::cli_alert_info(
    "Backfilling {length(backfill_years)} pre-FAOSTAT years ",
    "using LUH2 pasture trends"
  )

  # Read LUH2 pasture + rangeland for needed years
  states_path <- file.path(luh2_dir, "states.nc")
  carea_ha_r <- terra::rast(
    file.path(luh2_dir, "staticData_quarterdeg.nc"),
    subds = "carea"
  ) * 100  # km2 -> ha

  target_res <- 0.5
  agg_factor <- as.integer(target_res / 0.25)

  needed_years <- c(fao_first_year, backfill_years)

  pasture_by_year <- purrr::map(needed_years, \(yr) {
    time_idx <- yr - 850L + 1L
    nc <- ncdf4::nc_open(states_path)
    on.exit(ncdf4::nc_close(nc))

    pastr_vals <- ncdf4::ncvar_get(
      nc, "pastr",
      start = c(1, 1, time_idx), count = c(-1, -1, 1)
    )
    range_vals <- ncdf4::ncvar_get(
      nc, "range",
      start = c(1, 1, time_idx), count = c(-1, -1, 1)
    )

    lat <- ncdf4::ncvar_get(nc, "lat")
    pasture_frac <- pastr_vals + range_vals
    pasture_frac[is.na(pasture_frac)] <- 0

    r <- terra::rast(
      t(pasture_frac),
      extent = terra::ext(-180, 180, -90, 90)
    )
    if (lat[1] < lat[length(lat)]) {
      r <- terra::flip(r, direction = "vertical")
    }

    pasture_ha <- r * carea_ha_r
    pasture_ha_agg <- terra::aggregate(
      pasture_ha, fact = agg_factor, fun = "sum", na.rm = TRUE
    )

    coords <- terra::xyFromCell(
      pasture_ha_agg, seq_len(terra::ncell(pasture_ha_agg))
    )
    tibble::tibble(
      lon = round(coords[, 1], 2),
      lat = round(coords[, 2], 2),
      pasture_ha = terra::values(pasture_ha_agg)[, 1]
    ) |>
      dplyr::filter(!is.na(pasture_ha), pasture_ha > 0) |>
      dplyr::inner_join(country_grid, by = c("lon", "lat")) |>
      dplyr::summarize(
        pasture_ha = sum(pasture_ha),
        .by = "area_code"
      ) |>
      dplyr::mutate(year = yr)
  }) |>
    dplyr::bind_rows()

  # Compute ratio relative to 1961
  base_pasture <- pasture_by_year |>
    dplyr::filter(year == fao_first_year) |>
    dplyr::select(area_code, base_ha = pasture_ha)

  country_ratios <- pasture_by_year |>
    dplyr::filter(year %in% backfill_years) |>
    dplyr::inner_join(base_pasture, by = "area_code") |>
    dplyr::mutate(
      ratio = dplyr::if_else(base_ha > 0, pasture_ha / base_ha, 1)
    ) |>
    dplyr::select(year, area_code, ratio)

  # 1961 template
  base_stocks <- livestock_n |>
    dplyr::filter(year == fao_first_year) |>
    dplyr::select(
      area_code, area_name, species, species_group,
      item_code, nex_kg_n, grazing_frac,
      base_heads = heads
    )

  # Scale backward
  backfill <- country_ratios |>
    dplyr::inner_join(
      base_stocks, by = "area_code",
      relationship = "many-to-many"
    ) |>
    dplyr::mutate(
      heads        = base_heads * ratio,
      manure_n_mg  = heads * nex_kg_n / 1000,
      grazing_n_mg = manure_n_mg * grazing_frac,
      applied_n_mg = manure_n_mg * (1 - grazing_frac)
    ) |>
    dplyr::filter(heads > 0) |>
    dplyr::select(
      year, area_code, area_name, item_code, species,
      species_group, nex_kg_n, grazing_frac, heads,
      manure_n_mg, grazing_n_mg, applied_n_mg
    )

  cli::cli_alert_success(
    "Backfilled {nrow(backfill)} livestock-year-species records"
  )

  dplyr::bind_rows(livestock_n, backfill) |>
    dplyr::arrange(year, area_code, species_group, species)
}


# ==== 5. Spatial livestock distribution ================================

# Distribute country-level livestock manure N to a 0.5° grid using
# LUH2 pasture+rangeland as proxy for grazing animals and LUH2
# cropland for confined animals (pigs, poultry).
#
# Future enhancement: use Gridded Livestock of the World v3
# (Gilbert et al. 2018) for species-specific density grids.

.distribute_livestock_spatially <- function(
  livestock_n, l_files_dir, country_grid, target_res, year_sample
) {
  cli::cli_h2("Spatial livestock distribution (LUH2 proxy)")

  luh2_dir <- file.path(l_files_dir, "LUH2", "LUH2 v2h")
  if (!dir.exists(luh2_dir)) {
    cli::cli_alert_warning("LUH2 not found — skipping spatialization")
    return(NULL)
  }

  states_path <- file.path(luh2_dir, "states.nc")
  carea_ha_r <- terra::rast(
    file.path(luh2_dir, "staticData_quarterdeg.nc"),
    subds = "carea"
  ) * 100  # km2 -> ha

  agg_factor <- as.integer(target_res / 0.25)

  # Confined species groups → use cropland as proxy
  confined_groups <- c("Pigs", "Poultry")

  cli::cli_alert_info(
    "Distributing over {length(year_sample)} sample years"
  )

  gridded <- purrr::map(year_sample, \(yr) {
    if (yr %% 50 == 0) cli::cli_alert("  Year {yr}")

    time_idx <- yr - 850L + 1L
    nc <- ncdf4::nc_open(states_path)
    on.exit(ncdf4::nc_close(nc))

    lat <- ncdf4::ncvar_get(nc, "lat")
    lat_desc <- lat[1] > lat[length(lat)]

    .read_var <- function(vname) {
      vals <- ncdf4::ncvar_get(
        nc, vname,
        start = c(1, 1, time_idx), count = c(-1, -1, 1)
      )
      vals[is.na(vals)] <- 0
      r <- terra::rast(
        t(vals), extent = terra::ext(-180, 180, -90, 90)
      )
      if (!lat_desc) r <- terra::flip(r, direction = "vertical")
      r
    }

    # Pasture + rangeland for grazing animals
    pastr_r <- .read_var("pastr")
    range_r <- .read_var("range")
    grazing_ha <- (pastr_r + range_r) * carea_ha_r
    grazing_ha <- terra::aggregate(
      grazing_ha, fact = agg_factor, fun = "sum", na.rm = TRUE
    )

    # Cropland for confined animals
    crop_vars <- c("c3ann", "c4ann", "c3per", "c4per", "c3nfx")
    crop_r <- purrr::map(crop_vars, .read_var)
    cropland_frac <- Reduce(`+`, crop_r)
    cropland_ha <- cropland_frac * carea_ha_r
    cropland_ha <- terra::aggregate(
      cropland_ha, fact = agg_factor, fun = "sum", na.rm = TRUE
    )

    # Build grids
    g_coords <- terra::xyFromCell(
      grazing_ha, seq_len(terra::ncell(grazing_ha))
    )
    grazing_tbl <- tibble::tibble(
      lon = round(g_coords[, 1], 2),
      lat = round(g_coords[, 2], 2),
      grazing_ha = terra::values(grazing_ha)[, 1],
      cropland_ha = terra::values(cropland_ha)[, 1]
    ) |>
      dplyr::inner_join(country_grid, by = c("lon", "lat")) |>
      dplyr::mutate(
        grazing_ha  = dplyr::if_else(
          is.na(grazing_ha), 0, grazing_ha
        ),
        cropland_ha = dplyr::if_else(
          is.na(cropland_ha), 0, cropland_ha
        )
      ) |>
      dplyr::filter(grazing_ha > 0 | cropland_ha > 0)

    # Country-level totals for weighting
    ctry_totals <- grazing_tbl |>
      dplyr::summarize(
        total_grazing  = sum(grazing_ha),
        total_cropland = sum(cropland_ha),
        .by = "area_code"
      )

    # Country-level livestock N for this year, by species group
    yr_livestock <- livestock_n |>
      dplyr::filter(year == yr) |>
      dplyr::summarize(
        grazing_n_mg = sum(grazing_n_mg, na.rm = TRUE),
        applied_n_mg = sum(applied_n_mg, na.rm = TRUE),
        manure_n_mg  = sum(manure_n_mg, na.rm = TRUE),
        .by = c(area_code, species_group)
      )

    # Separate grazing and confined species
    grazing_sp <- yr_livestock |>
      dplyr::filter(!species_group %in% confined_groups)
    confined_sp <- yr_livestock |>
      dplyr::filter(species_group %in% confined_groups)

    # Distribute grazing species by pasture+range share
    gridded_grazing <- NULL
    if (nrow(grazing_sp) > 0) {
      gridded_grazing <- grazing_tbl |>
        dplyr::filter(grazing_ha > 0) |>
        dplyr::inner_join(ctry_totals, by = "area_code") |>
        dplyr::mutate(
          share = grazing_ha / total_grazing
        ) |>
        dplyr::inner_join(
          grazing_sp, by = "area_code",
          relationship = "many-to-many"
        ) |>
        dplyr::mutate(
          manure_n_mg  = manure_n_mg * share,
          grazing_n_mg = grazing_n_mg * share,
          applied_n_mg = applied_n_mg * share
        ) |>
        dplyr::select(
          lon, lat, area_code, species_group,
          manure_n_mg, grazing_n_mg, applied_n_mg
        )
    }

    # Distribute confined species by cropland share
    gridded_confined <- NULL
    if (nrow(confined_sp) > 0) {
      gridded_confined <- grazing_tbl |>
        dplyr::filter(cropland_ha > 0) |>
        dplyr::inner_join(ctry_totals, by = "area_code") |>
        dplyr::mutate(
          share = cropland_ha / total_cropland
        ) |>
        dplyr::inner_join(
          confined_sp, by = "area_code",
          relationship = "many-to-many"
        ) |>
        dplyr::mutate(
          manure_n_mg  = manure_n_mg * share,
          grazing_n_mg = grazing_n_mg * share,
          applied_n_mg = applied_n_mg * share
        ) |>
        dplyr::select(
          lon, lat, area_code, species_group,
          manure_n_mg, grazing_n_mg, applied_n_mg
        )
    }

    dplyr::bind_rows(gridded_grazing, gridded_confined) |>
      dplyr::mutate(year = yr)
  }) |>
    dplyr::bind_rows()

  if (nrow(gridded) > 0) {
    n_cells <- dplyr::n_distinct(paste(gridded$lon, gridded$lat))
    cli::cli_alert_success(
      "Gridded livestock: {nrow(gridded)} rows, {n_cells} cells"
    )
  }

  gridded
}


# ==== 6. GLW3 download helper =========================================

# Gridded Livestock of the World v3 (Gilbert et al. 2018) provides
# species-specific density maps at ~1 km resolution.  These can
# replace the LUH2 pasture proxy for more accurate spatial dist.
#
# Download from:
#   https://dataverse.harvard.edu/dataverse/glw
#   Individual species TIFs (~250 MB each):
#   - Cattle, Buffaloes, Sheep, Goats, Pigs, Chickens, Ducks, Horses

.download_glw3 <- function(dest_dir) {
  cli::cli_h2("Gridded Livestock of the World v3 (GLW3)")

  if (!dir.exists(dest_dir)) {
    dir.create(dest_dir, recursive = TRUE)
  }

  # GLW3 Dataverse DOIs
  cli::cli_alert_info(
    "GLW3 data must be downloaded manually from Harvard Dataverse:"
  )
  cli::cli_alert_info(
    "  https://dataverse.harvard.edu/dataverse/glw"
  )
  cli::cli_alert_info(
    "  Gilbert et al. 2018, doi:10.1371/journal.pone.0217754"
  )
  cli::cli_alert_info("  Place TIF files in: {dest_dir}")
  cli::cli_alert_info(
    "  Required files: 5_Ct_2010_Da.tif (Cattle), ",
    "5_Bf_2010_Da.tif (Buffaloes), etc."
  )

  invisible(NULL)
}


# ==== Main execution ==================================================

cli::cli_h1("Preparing livestock inputs")

# Configuration
year_range <- 1850L:2022L
target_res <- 0.5

# -- Load country grid ---
country_grid_file <- file.path(output_dir, "country_grid.parquet")
country_grid <- NULL
if (file.exists(country_grid_file)) {
  country_grid <- nanoparquet::read_parquet(country_grid_file)
  cli::cli_alert_success(
    "Country grid: {nrow(country_grid)} cells loaded"
  )
} else {
  cli::cli_alert_warning(
    "country_grid.parquet not found \u2014 ",
    "run prepare_spatialize_inputs.R first"
  )
}

# -- Step 1: Read FAOSTAT livestock stocks ---
stocks <- .read_faostat_livestock(l_files_dir, regions)

# -- Step 2: Compute manure N production ---
livestock_n <- .compute_manure_n(stocks)

# -- Step 3: Cross-validate with FAOSTAT Emissions ---
.validate_with_emissions(livestock_n, l_files_dir, regions)

# -- Step 4: Backfill pre-FAOSTAT years ---
if (!is.null(country_grid) && min(year_range) < 1961L) {
  livestock_n <- .backfill_livestock(
    livestock_n, l_files_dir, country_grid, year_range
  )
}

# -- Step 5: Save country-level livestock stocks ---
stocks_out <- livestock_n |>
  dplyr::select(
    year, area_code, area_name, species, species_group,
    heads, nex_kg_n_head = nex_kg_n, manure_n_mg,
    grazing_n_mg, applied_n_mg
  )

nanoparquet::write_parquet(
  stocks_out,
  file.path(output_dir, "livestock_stocks.parquet")
)

n_sp   <- dplyr::n_distinct(stocks_out$species)
n_grp  <- dplyr::n_distinct(stocks_out$species_group)
n_ctry <- dplyr::n_distinct(stocks_out$area_code)
n_yr   <- dplyr::n_distinct(stocks_out$year)
sz <- round(
  file.size(file.path(output_dir, "livestock_stocks.parquet")) /
    1024 / 1024, 1
)

cli::cli_h2("Country-level summary")
cli::cli_alert_success(
  "livestock_stocks.parquet: {nrow(stocks_out)} rows ({sz} MB)"
)
cli::cli_alert_info(
  "  {n_sp} species / {n_grp} groups \u00d7 {n_ctry} countries \u00d7 {n_yr} years"
)

# -- Step 6: Spatial distribution ---
if (!is.null(country_grid)) {
  # Use decadal sample for efficiency (full years very slow)
  spatial_years <- year_range[
    year_range %% 10 == 0 | year_range == max(year_range)
  ]

  gridded <- .distribute_livestock_spatially(
    livestock_n, l_files_dir, country_grid, target_res, spatial_years
  )

  if (!is.null(gridded) && nrow(gridded) > 0) {
    nanoparquet::write_parquet(
      gridded,
      file.path(output_dir, "gridded_livestock.parquet")
    )

    sz2 <- round(
      file.size(file.path(output_dir, "gridded_livestock.parquet")) /
        1024 / 1024, 1
    )
    cli::cli_alert_success(
      "gridded_livestock.parquet: {nrow(gridded)} rows ({sz2} MB)"
    )
  }
} else {
  cli::cli_alert_info(
    "Skipping spatial distribution (no country_grid)"
  )
}

# -- GLW3 download instructions ---
glw_dir <- file.path(l_files_dir, "GLW3")
if (!dir.exists(glw_dir)) {
  .download_glw3(glw_dir)
}

cli::cli_h1("Livestock processing complete")
cli::cli_alert_success("Done!")
