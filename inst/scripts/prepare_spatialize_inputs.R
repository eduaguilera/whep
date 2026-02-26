# -----------------------------------------------------------------------
# prepare_spatialize_inputs.R
#
# Reads raw spatial data from L_files and prepares the four input
# tibbles required by build_gridded_landuse(). Each output is saved
# as parquet to L_files/whep/inputs/.
#
# Raw data expected in L_files:
#   FAOSTAT, EarthStat, LUH2 v2h, and NaturalEarth shapefiles.
# -----------------------------------------------------------------------

library(dplyr, warn.conflicts = FALSE)

# ==== Private helpers =================================================

.find_extdata_file <- function(filename) {
  local_path <- file.path("inst", "extdata", filename)
  if (file.exists(local_path)) return(local_path)
  pkg_path <- system.file("extdata", filename, package = "whep")
  if (nchar(pkg_path) > 0) return(pkg_path)
  cli::cli_abort("Cannot find {filename} in inst/extdata or package.")
}

.read_cft_mapping <- function() {
  .find_extdata_file("cft_mapping.csv") |>
    readr::read_csv(show_col_types = FALSE)
}

.read_earthstat_mapping <- function() {
  .find_extdata_file("earthstat_mapping.csv") |>
    readr::read_csv(
      show_col_types = FALSE,
      col_types = readr::cols(
        earthstat_name = readr::col_character(),
        item_prod_code = readr::col_integer(),
        item_prod_name = readr::col_character()
      )
    )
}

.raster_to_tibble <- function(r, value_name) {
  coords <- terra::xyFromCell(r, seq_len(terra::ncell(r)))
  tibble::tibble(
    lon = round(coords[, 1], 2),
    lat = round(coords[, 2], 2),
    value = terra::values(r)[, 1]
  ) |>
    rlang::set_names(c("lon", "lat", value_name))
}

.read_one_earthstat_crop <- function(earthstat_dir, crop_name,
                                     item_prod_code, target_res) {
  tif_path <- file.path(
    earthstat_dir, crop_name,
    paste0(crop_name, "_HarvestedAreaFraction.tif")
  )

  if (!file.exists(tif_path)) {
    cli::cli_alert_warning("Missing: {tif_path}")
    return(tibble::tibble())
  }

  r <- terra::rast(tif_path)

  # Aggregate 5 arc-min to target_res (0.5 deg -> factor = 6)
  agg_factor <- as.integer(target_res / (5 / 60))
  r_agg <- terra::aggregate(
    r, fact = agg_factor, fun = "mean", na.rm = TRUE
  )

  .raster_to_tibble(r_agg, "harvest_fraction") |>
    dplyr::filter(
      !is.na(harvest_fraction),
      harvest_fraction > 0
    ) |>
    dplyr::mutate(item_prod_code = item_prod_code)
}

.read_luh2_static <- function(luh2_dir, var_name) {
  static_path <- file.path(luh2_dir, "staticData_quarterdeg.nc")
  nc <- ncdf4::nc_open(static_path)
  on.exit(ncdf4::nc_close(nc))

  lat <- ncdf4::ncvar_get(nc, "lat")
  vals <- ncdf4::ncvar_get(nc, var_name)

  # t(vals) -> rows = lat, cols = lon. When lat is descending
  # (90 to -90), row 1 = north, which matches terra convention.
  r <- terra::rast(
    t(vals),
    extent = terra::ext(-180, 180, -90, 90)
  )
  if (lat[1] < lat[length(lat)]) {
    r <- terra::flip(r, direction = "vertical")
  }
  r
}

.read_luh2_carea <- function(luh2_dir) {
  .read_luh2_static(luh2_dir, "carea")
}

# CFT-to-LUH2 crop functional type mapping
.cft_to_luh2_mapping <- function() {
  tibble::tribble(
    ~cft_name,             ~luh2_type,
    "temperate_cereals",   "c3ann",
    "rice",                "c3ann",
    "oil_crops_rapeseed",  "c3ann",
    "oil_crops_sunflower", "c3ann",
    "oil_crops_other",     "c3ann",
    "others_annual",       "c3ann",
    "temperate_roots",     "c3ann",
    "tropical_roots",      "c3ann",
    "maize",               "c4ann",
    "tropical_cereals",    "c4ann",
    "sugarcane",           "c4ann",
    "others_perennial",    "c3per",
    "oil_crops_soybean",   "c3nfx",
    "oil_crops_groundnut", "c3nfx",
    "pulses",              "c3nfx"
  )
}

.read_luh2_variables <- function(nc_path, var_names, time_idx) {
  nc <- ncdf4::nc_open(nc_path)
  on.exit(ncdf4::nc_close(nc))

  lon <- ncdf4::ncvar_get(nc, "lon")
  lat <- ncdf4::ncvar_get(nc, "lat")
  n_lon <- length(lon)
  n_lat <- length(lat)
  lat_desc <- lat[1] > lat[length(lat)]

  purrr::map(var_names, \(vname) {
    vals <- ncdf4::ncvar_get(
      nc, vname,
      start = c(1, 1, time_idx),
      count = c(n_lon, n_lat, 1)
    )
    r <- terra::rast(
      t(vals),
      extent = terra::ext(-180, 180, -90, 90)
    )
    if (!lat_desc) {
      r <- terra::flip(r, direction = "vertical")
    }
    r[is.nan(terra::values(r))] <- 0
    r[is.na(terra::values(r))] <- 0
    r
  }) |>
    rlang::set_names(var_names)
}

.read_luh2_year <- function(luh2_dir, yr, crop_vars,
                            carea_rast, target_res) {
  time_idx <- yr - 850L + 1L

  states_path <- file.path(luh2_dir, "states.nc")
  mgmt_path <- file.path(luh2_dir, "management.nc")
  irrig_vars <- paste0("irrig_", crop_vars)

  crop_rasters <- .read_luh2_variables(
    states_path, crop_vars, time_idx
  )
  irrig_rasters <- .read_luh2_variables(
    mgmt_path, irrig_vars, time_idx
  )

  # Total cropland = sum of crop fractions * cell area
  cropland_frac <- Reduce(`+`, crop_rasters)
  irrig_frac <- Reduce(`+`, purrr::map2(
    irrig_rasters, crop_rasters,
    \(irrig, crop) irrig * crop
  ))

  # Convert fractions to hectares (carea km^2 * 100 = ha)
  cropland_ha <- cropland_frac * carea_rast * 100
  irrigated_ha <- irrig_frac * carea_rast * 100

  # Aggregate 0.25 deg -> target_res
  agg_factor <- as.integer(target_res / 0.25)
  cropland_ha <- terra::aggregate(
    cropland_ha, fact = agg_factor, fun = "sum", na.rm = TRUE
  )
  irrigated_ha <- terra::aggregate(
    irrigated_ha, fact = agg_factor, fun = "sum", na.rm = TRUE
  )

  cropland_tbl <- .raster_to_tibble(cropland_ha, "cropland_ha")
  irrigated_tbl <- .raster_to_tibble(irrigated_ha, "irrigated_ha")

  cropland_tbl |>
    dplyr::left_join(irrigated_tbl, by = c("lon", "lat")) |>
    dplyr::filter(!is.na(cropland_ha), cropland_ha > 0) |>
    dplyr::mutate(
      year = yr,
      irrigated_ha = dplyr::if_else(
        is.na(irrigated_ha), 0, irrigated_ha
      )
    )
}

# ==== Preparation functions ===========================================

# ---- 1. Country grid ------------------------------------------------
# Rasterize NaturalEarth to a 0.5-degree grid with WHEP area_code.

prepare_country_grid <- function(l_files_dir, target_res) {
  cli::cli_h1("Preparing country grid")

  shp_path <- file.path(
    l_files_dir, "NaturalEarth", "Countries_shape",
    "ne_10m_admin_0_countries.shp"
  )
  countries <- terra::vect(shp_path)

  polities <- whep::polities

  # NaturalEarth has ISO_A3 = "-99" for some countries
  # (e.g., France, Norway). Fall back to ISO_A3_EH then ADM0_A3.
  iso_raw <- as.character(countries$ISO_A3)
  iso_eh <- as.character(countries$ISO_A3_EH)
  iso_adm <- as.character(countries$ADM0_A3)
  iso3c <- dplyr::if_else(
    iso_raw != "-99", iso_raw,
    dplyr::if_else(iso_eh != "-99", iso_eh, iso_adm)
  )

  ne_data <- tibble::tibble(iso3c = iso3c) |>
    dplyr::left_join(
      dplyr::select(polities, "iso3c", "area_code"),
      by = "iso3c"
    )

  countries$area_code <- ne_data$area_code

  ref <- terra::rast(
    resolution = target_res,
    xmin = -180, xmax = 180,
    ymin = -90, ymax = 90
  )

  cli::cli_alert_info(
    "Rasterizing shapefile to {target_res}-degree grid"
  )
  grid_rast <- terra::rasterize(
    countries, ref, field = "area_code"
  )

  .raster_to_tibble(grid_rast, "area_code") |>
    dplyr::filter(!is.na(area_code)) |>
    dplyr::mutate(area_code = as.integer(area_code))
}

# ---- 2. Country areas (FAOSTAT + LUH2 irrigation) ------------------
# Read FAOSTAT production CSV (element 5312: Area harvested) and
# derive per-crop irrigated area from LUH2 management layer.
# For years before FAOSTAT (pre-1961), backfill using 1961 crop
# proportions applied to LUH2 country-level cropland totals.

.luh2_country_totals <- function(
  luh2_dir,
  year_range,
  country_grid,
  target_res
) {
  cli::cli_alert_info(
    "Computing country-level cropland & irrigation from LUH2"
  )

  crop_vars <- c("c3ann", "c4ann", "c3per", "c4per", "c3nfx")
  irrig_vars <- paste0("irrig_", crop_vars)
  states_path <- file.path(luh2_dir, "states.nc")
  mgmt_path <- file.path(luh2_dir, "management.nc")

  # Cell area in hectares (km2 * 100) at 0.25 deg
  carea_ha_r <- .read_luh2_carea(luh2_dir) * 100
  agg_factor <- as.integer(target_res / 0.25)

  purrr::map(year_range, \(yr) {
    if (yr %% 10 == 0) {
      cli::cli_alert("LUH2 country totals: year {yr}")
    }
    time_idx <- yr - 850L + 1L

    crop_r <- .read_luh2_variables(
      states_path, crop_vars, time_idx
    )
    irrig_r <- .read_luh2_variables(
      mgmt_path, irrig_vars, time_idx
    )

    # Per-type cropland & irrigated ha at 0.25deg, aggregate
    purrr::map(crop_vars, \(cv) {
      iv <- paste0("irrig_", cv)
      crop_ha_r <- crop_r[[cv]] * carea_ha_r
      irrig_ha_r <- irrig_r[[iv]] * crop_r[[cv]] * carea_ha_r
      crop_agg <- terra::aggregate(
        crop_ha_r, fact = agg_factor,
        fun = "sum", na.rm = TRUE
      )
      irrig_agg <- terra::aggregate(
        irrig_ha_r, fact = agg_factor,
        fun = "sum", na.rm = TRUE
      )
      crop_tbl <- .raster_to_tibble(crop_agg, "crop_ha")
      irrig_tbl <- .raster_to_tibble(irrig_agg, "irrig_ha")
      dplyr::left_join(
        crop_tbl, irrig_tbl, by = c("lon", "lat")
      ) |>
        dplyr::filter(
          !is.na(.data$crop_ha), .data$crop_ha > 0
        ) |>
        dplyr::mutate(
          irrig_ha = dplyr::if_else(
            is.na(.data$irrig_ha), 0, .data$irrig_ha
          ),
          luh2_type = cv
        )
    }) |>
      dplyr::bind_rows() |>
      dplyr::inner_join(country_grid, by = c("lon", "lat")) |>
      dplyr::summarise(
        crop_ha = sum(.data$crop_ha),
        irrig_ha = sum(.data$irrig_ha),
        .by = c("area_code", "luh2_type")
      ) |>
      dplyr::mutate(year = yr)
  }) |>
    dplyr::bind_rows() |>
    # Merge c4per into c3per (both map to others_perennial)
    dplyr::mutate(
      luh2_type = dplyr::if_else(
        .data$luh2_type == "c4per", "c3per", .data$luh2_type
      )
    ) |>
    dplyr::summarise(
      crop_ha = sum(.data$crop_ha),
      irrig_ha = sum(.data$irrig_ha),
      .by = c("year", "area_code", "luh2_type")
    )
}

# Predecessor -> successor country mappings.
# Historical entities that dissolved into modern states.
.predecessor_successors <- function() {
  tibble::tribble(
    ~predecessor_code, ~successor_code,
    # USSR (1961-1991) -> 15 successor states
    228L,   1L,   # Armenia
    228L,  52L,   # Azerbaijan
    228L,  57L,   # Belarus
    228L,  63L,   # Estonia
    228L,  73L,   # Georgia
    228L, 108L,   # Kazakhstan
    228L, 113L,   # Kyrgyzstan
    228L, 119L,   # Latvia
    228L, 126L,   # Lithuania
    228L, 146L,   # Republic of Moldova
    228L, 185L,   # Russian Federation
    228L, 208L,   # Tajikistan
    228L, 213L,   # Turkmenistan
    228L, 230L,   # Ukraine
    228L, 235L,   # Uzbekistan
    # Yugoslav SFR (1961-1991) -> 6 successor states
    248L,  80L,   # Bosnia and Herzegovina
    248L,  98L,   # Croatia
    248L, 154L,   # North Macedonia
    248L, 198L,   # Slovenia
    248L, 272L,   # Serbia
    248L, 273L,   # Montenegro
    # Czechoslovakia (1961-1992) -> 2 successor states
     51L, 167L,   # Czech Republic
     51L, 199L,   # Slovakia
    # Belgium-Luxembourg (1961-1999) -> 2 successor states
     15L, 255L,   # Belgium
     15L, 256L,   # Luxembourg
    # Serbia and Montenegro (1992-2005) -> 2 successor states
    186L, 272L,   # Serbia
    186L, 273L,   # Montenegro
    # Ethiopia PDR (1961-1992) -> 2 successor states
     62L, 238L,   # Ethiopia
     62L, 178L,   # Eritrea
    # Sudan (former) (1961-2011) -> 2 successor states
    206L, 276L,   # Sudan
    206L, 277L    # South Sudan
  )
}

# Redistribute historical predecessor data to successor states
# using LUH2 cropland shares as weights.
.redistribute_predecessors <- function(crop_areas, luh2_totals) {
  pred_map <- .predecessor_successors()
  pred_codes <- unique(pred_map$predecessor_code)

  pred_rows <- crop_areas |>
    dplyr::filter(.data$area_code %in% pred_codes)

  if (nrow(pred_rows) == 0) return(crop_areas)

  other_rows <- crop_areas |>
    dplyr::filter(!.data$area_code %in% pred_codes)

  # Country-level LUH2 cropland for weighting
  luh2_ctry <- luh2_totals |>
    dplyr::summarise(
      crop_ha = sum(.data$crop_ha),
      .by = c("year", "area_code")
    )

  redistributed <- pred_rows |>
    dplyr::inner_join(
      pred_map,
      by = c("area_code" = "predecessor_code"),
      relationship = "many-to-many"
    ) |>
    # Get LUH2 cropland for each successor in each year
    dplyr::left_join(
      luh2_ctry,
      by = c("year", "successor_code" = "area_code")
    ) |>
    dplyr::mutate(
      crop_ha = dplyr::if_else(
        is.na(.data$crop_ha), 0, .data$crop_ha
      )
    ) |>
    # Share = successor cropland / total successor cropland
    dplyr::mutate(
      group_total = sum(.data$crop_ha),
      share = dplyr::if_else(
        .data$group_total > 0,
        .data$crop_ha / .data$group_total,
        1 / dplyr::n()
      ),
      .by = c("year", "area_code", "item_prod_code")
    ) |>
    dplyr::mutate(
      harvested_area_ha = .data$harvested_area_ha * .data$share,
      area_code = .data$successor_code
    ) |>
    dplyr::filter(.data$harvested_area_ha > 0) |>
    dplyr::select(
      "year", "area_code", "item_prod_code",
      "harvested_area_ha"
    )

  n_pred <- length(unique(pred_rows$area_code))
  n_succ <- length(unique(redistributed$area_code))
  cli::cli_alert_success(
    "Redistributed {n_pred} predecessor entities to ",
    "{n_succ} successor states"
  )

  # Combine: successor rows may overlap with existing direct data
  # (e.g., Russia has direct FAO data from 1992+; predecessor
  # redistribution adds rows for 1961-1991). No deduplication needed
  # since they cover different year ranges.
  dplyr::bind_rows(other_rows, redistributed)
}

prepare_country_areas <- function(
  l_files_dir, year_range, country_grid, target_res
) {
  cli::cli_h1("Preparing FAOSTAT country areas")

  csv_path <- file.path(
    l_files_dir, "FAOSTAT",
    "Production_Crops_Livestock_E_All_Data_(Normalized).csv"
  )

  cli::cli_alert_info(
    "Reading FAOSTAT CSV (this may take a moment)"
  )
  fao_raw <- readr::read_csv(
    csv_path,
    col_types = readr::cols(
      `Area Code` = readr::col_integer(),
      `Item Code` = readr::col_integer(),
      `Element Code` = readr::col_integer(),
      Year = readr::col_integer(),
      Value = readr::col_double(),
      .default = readr::col_character()
    ),
    show_col_types = FALSE
  )

  element_area_harvested <- 5312L
  cft_mapping <- .read_cft_mapping()
  cft_luh2 <- .cft_to_luh2_mapping()
  fao_first_year <- 1961L

  # FAOSTAT crop areas for available years
  fao_years <- year_range[year_range >= fao_first_year]
  crop_areas <- fao_raw |>
    dplyr::filter(
      .data$`Element Code` == element_area_harvested,
      .data$Year %in% fao_years,
      .data$`Item Code` %in% cft_mapping$item_prod_code
    ) |>
    dplyr::transmute(
      year = .data$Year,
      area_code = .data$`Area Code`,
      item_prod_code = .data$`Item Code`,
      harvested_area_ha = dplyr::if_else(
        is.na(Value), 0, Value
      )
    ) |>
    dplyr::filter(harvested_area_ha > 0)

  # ---- Filter FAO aggregate codes ----
  # Codes >= 5000 are regional aggregates (World, Africa, etc.)
  crop_areas <- crop_areas |>
    dplyr::filter(.data$area_code < 5000L)

  # LUH2 country-level cropland + irrigation for all years
  luh2_dir <- file.path(l_files_dir, "LUH2", "LUH2 v2h")
  luh2_totals <- .luh2_country_totals(
    luh2_dir, year_range, country_grid, target_res
  )

  # ---- Redistribute predecessor entities ----
  # Historical entities (USSR, Yugoslavia, etc.) have FAO data but
  # no grid cells. Redistribute to successor states proportionally
  # to each successor's LUH2 cropland.
  crop_areas <- .redistribute_predecessors(
    crop_areas, luh2_totals
  )

  # ---- Backfill pre-FAOSTAT years (before 1961) ----
  # Use 1961 FAOSTAT crop areas scaled by country-level
  # LUH2 cropland trends: backfill_ha = fao_1961 * ratio
  # where ratio = LUH2_year / LUH2_1961 per country.
  backfill_years <- year_range[year_range < fao_first_year]

  if (length(backfill_years) > 0) {
    cli::cli_alert_info(
      "Backfilling {length(backfill_years)} pre-FAOSTAT years ",
      "using {fao_first_year} crop proportions"
    )

    # 1961 crop areas as backfill template
    fao_base <- crop_areas |>
      dplyr::filter(.data$year == fao_first_year) |>
      dplyr::select(
        "area_code", "item_prod_code",
        base_ha = "harvested_area_ha"
      )

    # Country-level LUH2 cropland: total across all types
    luh2_ctry <- luh2_totals |>
      dplyr::summarise(
        total_ha = sum(.data$crop_ha),
        .by = c("year", "area_code")
      )

    luh2_base <- luh2_ctry |>
      dplyr::filter(.data$year == fao_first_year) |>
      dplyr::select("area_code", base_total = "total_ha")

    # Ratio: LUH2_year / LUH2_1961 per country
    country_ratios <- luh2_ctry |>
      dplyr::filter(.data$year %in% backfill_years) |>
      dplyr::inner_join(luh2_base, by = "area_code") |>
      dplyr::mutate(
        ratio = dplyr::if_else(
          .data$base_total > 0,
          .data$total_ha / .data$base_total, 1
        )
      ) |>
      dplyr::select("year", "area_code", "ratio")

    # Scale 1961 crop values by country LUH2 trend
    backfill_areas <- country_ratios |>
      dplyr::inner_join(
        fao_base,
        by = "area_code",
        relationship = "many-to-many"
      ) |>
      dplyr::mutate(
        harvested_area_ha = .data$base_ha * .data$ratio
      ) |>
      dplyr::filter(.data$harvested_area_ha > 0) |>
      dplyr::select(
        "year", "area_code", "item_prod_code",
        "harvested_area_ha"
      )

    crop_areas <- dplyr::bind_rows(backfill_areas, crop_areas)
  }

  # ---- Add LUH2 type mapping ----
  type_map <- dplyr::select(
    cft_mapping, "item_prod_code", "cft_name"
  ) |>
    dplyr::left_join(
      dplyr::select(cft_luh2, "cft_name", "luh2_type"),
      by = "cft_name"
    ) |>
    dplyr::select("item_prod_code", "luh2_type")

  crop_areas <- crop_areas |>
    dplyr::left_join(type_map, by = "item_prod_code")

  # ---- Distribute irrigation ----
  # Priority: MIRCA2000 crop-specific fractions (static ~2000),
  #           scaled by LUH2 total irrigation trend.
  # Fallback: LUH2-proportional within crop type (original method).

  luh2_irrig <- luh2_totals |>
    dplyr::select("year", "area_code", "luh2_type", "irrig_ha")

  # Check for MIRCA irrigation fractions
  mirca_file <- file.path(l_files_dir, "whep", "inputs",
                          "mirca_irrigation_country.parquet")
  has_mirca <- file.exists(mirca_file)

  if (has_mirca) {
    cli::cli_alert_info(
      "Using MIRCA2000 crop-specific irrigation fractions"
    )
    mirca <- nanoparquet::read_parquet(mirca_file) |>
      dplyr::select("area_code", "item_prod_code", "irrig_frac")

    # Compute total LUH2 irrigated area per country-year (all types)
    luh2_total_irrig <- luh2_totals |>
      dplyr::summarize(
        total_irrig_ha = sum(.data$irrig_ha, na.rm = TRUE),
        .by = c("year", "area_code")
      )

    crop_areas <- crop_areas |>
      dplyr::left_join(mirca, by = c("area_code", "item_prod_code")) |>
      dplyr::left_join(luh2_total_irrig, by = c("year", "area_code"))

    # For crops with MIRCA data: use MIRCA fraction × harvested area,
    # then scale so total irrigated across crops matches LUH2 total.
    # For crops without MIRCA: fall back to LUH2 type-proportional.
    crop_areas <- crop_areas |>
      dplyr::mutate(
        mirca_irrig_raw = dplyr::if_else(
          !is.na(.data$irrig_frac),
          .data$irrig_frac * .data$harvested_area_ha,
          NA_real_
        ),
        # Sum of MIRCA-based irrigation per country-year
        mirca_total = sum(.data$mirca_irrig_raw, na.rm = TRUE),
        .by = c("year", "area_code")
      ) |>
      dplyr::mutate(
        total_irrig_ha = dplyr::if_else(
          is.na(.data$total_irrig_ha), 0, .data$total_irrig_ha
        ),
        # Scale MIRCA to match LUH2 total irrigation
        mirca_scale = dplyr::if_else(
          .data$mirca_total > 0,
          .data$total_irrig_ha / .data$mirca_total, 1
        ),
        irrigated_area_ha = dplyr::case_when(
          !is.na(.data$mirca_irrig_raw) ~
            .data$mirca_irrig_raw * .data$mirca_scale,
          TRUE ~ NA_real_  # Will be filled below for non-MIRCA crops
        )
      )

    # Fallback for crops without MIRCA: LUH2 type-proportional
    needs_fallback <- is.na(crop_areas$irrigated_area_ha)
    if (any(needs_fallback)) {
      fallback <- crop_areas |>
        dplyr::filter(is.na(.data$irrigated_area_ha)) |>
        dplyr::left_join(luh2_irrig,
                         by = c("year", "area_code", "luh2_type")) |>
        dplyr::mutate(
          irrig_ha = dplyr::if_else(
            is.na(.data$irrig_ha), 0, .data$irrig_ha
          ),
          cft_total = sum(.data$harvested_area_ha),
          crop_share = .data$harvested_area_ha / .data$cft_total,
          irrigated_area_ha = .data$crop_share * .data$irrig_ha,
          .by = c("year", "area_code", "luh2_type")
        )

      crop_areas$irrigated_area_ha[needs_fallback] <-
        fallback$irrigated_area_ha
    }
  } else {
    cli::cli_alert_info(
      "MIRCA not found — using LUH2-proportional irrigation allocation"
    )
    crop_areas <- crop_areas |>
      dplyr::mutate(
        cft_total_ha = sum(.data$harvested_area_ha),
        crop_share = .data$harvested_area_ha /
          .data$cft_total_ha,
        .by = c("year", "area_code", "luh2_type")
      ) |>
      dplyr::left_join(
        luh2_irrig,
        by = c("year", "area_code", "luh2_type")
      ) |>
      dplyr::mutate(
        irrig_ha = dplyr::if_else(
          is.na(.data$irrig_ha), 0, .data$irrig_ha
        ),
        irrigated_area_ha = .data$crop_share * .data$irrig_ha
      )
  }

  crop_areas |>
    dplyr::mutate(
      irrigated_area_ha = dplyr::if_else(
        is.na(.data$irrigated_area_ha), 0, .data$irrigated_area_ha
      ),
      irrigated_area_ha = pmin(
        .data$irrigated_area_ha, .data$harvested_area_ha
      )
    ) |>
    dplyr::select(
      "year", "area_code", "item_prod_code",
      "harvested_area_ha", "irrigated_area_ha"
    )
}

# ---- 3. Crop patterns (EarthStat / Monfreda) ------------------------
# Read HarvestedAreaFraction GeoTIFFs, aggregate to 0.5 degrees.

prepare_crop_patterns <- function(l_files_dir, target_res) {
  cli::cli_h1("Preparing EarthStat crop patterns")

  earthstat_dir <- file.path(
    l_files_dir,
    "HarvestedAreaYield175Crops_Geotiff", "GeoTiff"
  )
  xwalk <- .read_earthstat_mapping() |>
    dplyr::filter(!is.na(item_prod_code))

  cli::cli_alert_info("Processing {nrow(xwalk)} crops from EarthStat")

  purrr::imap(
    rlang::set_names(
      xwalk$earthstat_name, xwalk$earthstat_name
    ),
    \(crop_name, idx) {
      code <- xwalk$item_prod_code[
        xwalk$earthstat_name == crop_name
      ]
      .read_one_earthstat_crop(
        earthstat_dir, crop_name, code, target_res
      )
    }
  ) |>
    dplyr::bind_rows() |>
    # Multiple EarthStat crops can map to the same
    # item_prod_code. Sum their fractions.
    dplyr::summarise(
      harvest_fraction = sum(harvest_fraction),
      .by = c(lon, lat, item_prod_code)
    )
}

# ---- 4. Gridded cropland (LUH2) -------------------------------------
# Read LUH2 crop fractions + irrigation, convert to hectares.

prepare_gridded_cropland <- function(l_files_dir, year_range,
                                     target_res) {
  cli::cli_h1("Preparing LUH2 gridded cropland")


  luh2_dir <- file.path(l_files_dir, "LUH2", "LUH2 v2h")
  carea_rast <- .read_luh2_carea(luh2_dir)

  crop_vars <- c("c3ann", "c4ann", "c3per", "c4per", "c3nfx")

  cli::cli_alert_info("Processing {length(year_range)} years of LUH2 data")

  purrr::map(year_range, \(yr) {
    if (yr %% 10 == 0) {
      cli::cli_alert("Processing year {yr}")
    }
    .read_luh2_year(
      luh2_dir, yr, crop_vars, carea_rast, target_res
    )
  }) |>
    dplyr::bind_rows()
}

# ==== Main execution ==================================================

l_files_dir <- "C:/Users/53272530E/OneDrive/L_files"
output_dir <- file.path(l_files_dir, "whep", "inputs")
year_range <- 1850L:2022L
target_res <- 0.5

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

cli::cli_h1("Preparing spatialize inputs")
cli::cli_alert_info("Output directory: {output_dir}")

country_grid <- prepare_country_grid(l_files_dir, target_res)
nanoparquet::write_parquet(
  country_grid,
  file.path(output_dir, "country_grid.parquet")
)
cli::cli_alert_success(
  "country_grid: {nrow(country_grid)} cells saved"
)

country_areas <- prepare_country_areas(
  l_files_dir, year_range, country_grid, target_res
)
nanoparquet::write_parquet(
  country_areas,
  file.path(output_dir, "country_areas.parquet")
)
cli::cli_alert_success(
  "country_areas: {nrow(country_areas)} rows saved"
)

crop_patterns <- prepare_crop_patterns(l_files_dir, target_res)
nanoparquet::write_parquet(
  crop_patterns,
  file.path(output_dir, "crop_patterns.parquet")
)
cli::cli_alert_success(
  "crop_patterns: {nrow(crop_patterns)} rows saved"
)

gridded_cropland <- prepare_gridded_cropland(
  l_files_dir, year_range, target_res
)
nanoparquet::write_parquet(
  gridded_cropland,
  file.path(output_dir, "gridded_cropland.parquet")
)
cli::cli_alert_success(
  "gridded_cropland: {nrow(gridded_cropland)} rows saved"
)

cli::cli_alert_success("All inputs prepared in {output_dir}")
