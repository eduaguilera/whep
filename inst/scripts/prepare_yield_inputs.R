# -----------------------------------------------------------------------
# prepare_yield_inputs.R
#
# Generates two inputs for yield spatialization:
#   1. country_yields.parquet   — FAOSTAT country × crop × year yield
#   2. spatial_yield_index.parquet — EarthStat sub-national yield index
#
# Prerequisites: prepare_spatialize_inputs.R must have been run first
# (need country_grid.parquet and crop_patterns.parquet).
#
# Data sources:
#   - FAOSTAT Production_Crops_Livestock element 5419 (Yield, hg/ha)
#     and element 5510 (Production, tonnes).
#   - EarthStat HarvestedAreaYield175Crops (Monfreda et al., 2008)
#     *_YieldPerHectare.tif rasters at 5 arc-min.
#
# Outputs (to L_files/whep/inputs/):
#   country_yields.parquet, spatial_yield_index.parquet
# -----------------------------------------------------------------------

library(dplyr, warn.conflicts = FALSE)

# ==== Helpers ===========================================================

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

# Read one EarthStat *_YieldPerHectare.tif and aggregate to 0.5 deg
.read_one_earthstat_yield <- function(earthstat_dir, crop_name,
                                      item_prod_code, target_res) {
  tif_path <- file.path(
    earthstat_dir, crop_name,
    paste0(crop_name, "_YieldPerHectare.tif")
  )

  if (!file.exists(tif_path)) {
    return(tibble::tibble())
  }

  r <- terra::rast(tif_path)

  # Aggregate 5 arc-min (0.0833 deg) to target_res (0.5 deg) -> factor 6
  agg_factor <- as.integer(target_res / (5 / 60))
  r_agg <- terra::aggregate(
    r, fact = agg_factor, fun = "mean", na.rm = TRUE
  )

  .raster_to_tibble(r_agg, "yield_t_ha") |>
    dplyr::filter(
      !is.na(yield_t_ha),
      yield_t_ha > 0
    ) |>
    dplyr::mutate(item_prod_code = item_prod_code)
}

# ==== Configuration =====================================================

l_files_dir <- "C:/Users/53272530E/OneDrive/L_files"
input_dir <- file.path(l_files_dir, "whep", "inputs")
target_res <- 0.5

cft_mapping <- .read_cft_mapping()
year_range <- 1961L:2022L

# ==== Part 1: Country-level yields from FAOSTAT ========================

cli::cli_h1("Preparing country-level yields from FAOSTAT")

csv_path <- file.path(
  l_files_dir, "FAOSTAT",
  "Production_Crops_Livestock_E_All_Data_(Normalized).csv"
)

cli::cli_alert_info("Reading FAOSTAT CSV...")
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

# Element 5419 = Yield (hg/ha), 5510 = Production (tonnes),
# 5312 = Area harvested (ha)
# Strategy: use 5510/5312 (production/area) to get yield in t/ha
# This is more reliable than 5419 which is in hg/ha and sometimes
# inconsistent.

element_production <- 5510L
element_area <- 5312L

# Production in tonnes
production <- fao_raw |>
  dplyr::filter(
    .data$`Element Code` == element_production,
    .data$Year %in% year_range,
    .data$`Item Code` %in% cft_mapping$item_prod_code,
    .data$`Area Code` < 5000L
  ) |>
  dplyr::transmute(
    year = .data$Year,
    area_code = .data$`Area Code`,
    item_prod_code = .data$`Item Code`,
    production_t = dplyr::if_else(is.na(Value), 0, Value)
  ) |>
  dplyr::filter(production_t > 0)

# Area harvested in hectares
area_harvested <- fao_raw |>
  dplyr::filter(
    .data$`Element Code` == element_area,
    .data$Year %in% year_range,
    .data$`Item Code` %in% cft_mapping$item_prod_code,
    .data$`Area Code` < 5000L
  ) |>
  dplyr::transmute(
    year = .data$Year,
    area_code = .data$`Area Code`,
    item_prod_code = .data$`Item Code`,
    harvested_area_ha = dplyr::if_else(is.na(Value), 0, Value)
  ) |>
  dplyr::filter(harvested_area_ha > 0)

rm(fao_raw)
gc()

# Compute yield = production / area
country_yields <- production |>
  dplyr::inner_join(
    area_harvested,
    by = c("year", "area_code", "item_prod_code")
  ) |>
  dplyr::mutate(
    yield_t_ha = production_t / harvested_area_ha
  ) |>
  dplyr::filter(
    is.finite(yield_t_ha),
    yield_t_ha > 0
  ) |>
  dplyr::select(year, area_code, item_prod_code, yield_t_ha)

cli::cli_alert_success(
  "country_yields: {nrow(country_yields)} rows, ",
  "{n_distinct(country_yields$area_code)} countries, ",
  "{n_distinct(country_yields$item_prod_code)} crops"
)

# Sanity check: global average yield by major crop
sanity <- country_yields |>
  dplyr::inner_join(
    dplyr::select(cft_mapping, item_prod_code, cft_name),
    by = "item_prod_code"
  ) |>
  dplyr::filter(year == 2020L) |>
  dplyr::summarise(
    mean_yield = round(mean(yield_t_ha, na.rm = TRUE), 2),
    median_yield = round(median(yield_t_ha, na.rm = TRUE), 2),
    .by = cft_name
  ) |>
  dplyr::arrange(dplyr::desc(mean_yield))

cli::cli_alert_info("2020 mean country yields by CFT (t/ha):")
print(as.data.frame(sanity), row.names = FALSE)

# Save
yield_path <- file.path(input_dir, "country_yields.parquet")
nanoparquet::write_parquet(country_yields, yield_path)
cli::cli_alert_success("Saved {yield_path}")

rm(production, area_harvested)
gc()

# ==== Part 2: Spatial yield index from EarthStat =======================

cli::cli_h1("Building spatial yield index from EarthStat")

earthstat_dir <- file.path(
  l_files_dir, "HarvestedAreaYield175Crops_Geotiff", "GeoTiff"
)

xwalk <- .read_earthstat_mapping() |>
  dplyr::filter(!is.na(item_prod_code))

cli::cli_alert_info(
  "Reading yield rasters for {nrow(xwalk)} EarthStat crops..."
)

# Read all yield TIFs
raw_yields <- purrr::pmap(
  list(
    crop_name = xwalk$earthstat_name,
    item_prod_code = xwalk$item_prod_code
  ),
  \(crop_name, item_prod_code) {
    .read_one_earthstat_yield(
      earthstat_dir, crop_name, item_prod_code, target_res
    )
  },
  .progress = TRUE
) |>
  dplyr::bind_rows()

cli::cli_alert_success(
  "Raw yield pixels: {nrow(raw_yields)} rows, ",
  "{n_distinct(raw_yields$item_prod_code)} crops"
)

# When multiple EarthStat crops map to the same item_prod_code,
# take the area-weighted mean. We use harvest_fraction from
# crop_patterns as weight.
crop_patterns <- nanoparquet::read_parquet(
  file.path(input_dir, "crop_patterns.parquet")
)

# For multi-mapped codes, weight by harvest fraction
raw_yields <- raw_yields |>
  dplyr::summarise(
    yield_t_ha = mean(yield_t_ha, na.rm = TRUE),
    .by = c(lon, lat, item_prod_code)
  )

# Load country grid to compute country mean yields
country_grid <- nanoparquet::read_parquet(
  file.path(input_dir, "country_grid.parquet")
)

# Join with country grid
yields_with_country <- raw_yields |>
  dplyr::inner_join(country_grid, by = c("lon", "lat"))

# Compute country mean yield per crop (weighted by harvest fraction
# if available, otherwise simple mean)
yields_weighted <- yields_with_country |>
  dplyr::left_join(
    crop_patterns,
    by = c("lon", "lat", "item_prod_code")
  ) |>
  dplyr::mutate(
    weight = dplyr::coalesce(harvest_fraction, 1.0)
  )

country_mean_yields <- yields_weighted |>
  dplyr::summarise(
    country_mean = sum(yield_t_ha * weight, na.rm = TRUE) /
      sum(weight, na.rm = TRUE),
    .by = c(area_code, item_prod_code)
  ) |>
  dplyr::filter(country_mean > 0)

# Compute yield index = pixel_yield / country_mean_yield
spatial_yield_index <- yields_with_country |>
  dplyr::inner_join(
    country_mean_yields,
    by = c("area_code", "item_prod_code")
  ) |>
  dplyr::mutate(
    spatial_yield_index = yield_t_ha / country_mean
  ) |>
  dplyr::select(lon, lat, item_prod_code, spatial_yield_index)

# Clip extreme values (0.1 to 5.0) to prevent unrealistic spikes
spatial_yield_index <- spatial_yield_index |>
  dplyr::mutate(
    spatial_yield_index = pmin(pmax(spatial_yield_index, 0.1), 5.0)
  )

cli::cli_alert_success(
  "spatial_yield_index: {nrow(spatial_yield_index)} pixels, ",
  "{n_distinct(spatial_yield_index$item_prod_code)} crops"
)

# Summary statistics
idx_stats <- spatial_yield_index |>
  dplyr::summarise(
    mean_idx = round(mean(spatial_yield_index), 3),
    sd_idx = round(sd(spatial_yield_index), 3),
    min_idx = round(min(spatial_yield_index), 3),
    max_idx = round(max(spatial_yield_index), 3),
    pct_below_half = round(
      100 * mean(spatial_yield_index < 0.5), 1
    ),
    pct_above_two = round(
      100 * mean(spatial_yield_index > 2.0), 1
    )
  )
cli::cli_alert_info("Yield index statistics:")
print(as.data.frame(idx_stats), row.names = FALSE)

# Save
idx_path <- file.path(input_dir, "spatial_yield_index.parquet")
nanoparquet::write_parquet(spatial_yield_index, idx_path)
cli::cli_alert_success("Saved {idx_path}")

# ==== Done ==============================================================

cli::cli_h1("Yield input preparation complete")
cli::cli_alert_success(
  "country_yields: {nrow(country_yields)} rows"
)
cli::cli_alert_success(
  "spatial_yield_index: {nrow(spatial_yield_index)} pixels"
)
cli::cli_alert_info(
  "Run run_spatialize.R to produce gridded_yields.parquet"
)
