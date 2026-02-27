# -----------------------------------------------------------------------
# run_spatialize.R
#
# Runner script that loads preprocessed inputs, calls
# build_gridded_landuse(), and saves the output. Run
# prepare_spatialize_inputs.R first.
#
# Outputs are saved to L_files/whep/ as parquet.
# -----------------------------------------------------------------------

library(dplyr, warn.conflicts = FALSE)
devtools::load_all(".")

# ---- Configuration ---------------------------------------------------

l_files_dir <- "C:/Users/53272530E/OneDrive/L_files"
input_dir <- file.path(l_files_dir, "whep", "inputs")
output_dir <- file.path(l_files_dir, "whep")

# Subset of years to process (must be within the range used
# in prepare_spatialize_inputs.R)
year_range <- 1850L:2022L

# ---- Load inputs -----------------------------------------------------

cli::cli_h1("Loading preprocessed inputs")

country_grid <- nanoparquet::read_parquet(
  file.path(input_dir, "country_grid.parquet")
)
cli::cli_alert_success(
  "country_grid: {nrow(country_grid)} cells"
)

country_areas <- nanoparquet::read_parquet(
  file.path(input_dir, "country_areas.parquet")
) |>
  dplyr::filter(year %in% year_range)
cli::cli_alert_success(
  "country_areas: {nrow(country_areas)} rows
    ({min(year_range)}-{max(year_range)})"
)

crop_patterns <- nanoparquet::read_parquet(
  file.path(input_dir, "crop_patterns.parquet")
)
cli::cli_alert_success(
  "crop_patterns: {nrow(crop_patterns)} rows"
)

gridded_cropland <- nanoparquet::read_parquet(
  file.path(input_dir, "gridded_cropland.parquet")
) |>
  dplyr::filter(year %in% year_range)
cli::cli_alert_success(
  "gridded_cropland: {nrow(gridded_cropland)} rows"
)

# ---- Load type-level cropland (LUH2 per crop type) -------------------

type_cl_path <- file.path(input_dir, "type_cropland.parquet")
if (file.exists(type_cl_path)) {
  type_cropland <- nanoparquet::read_parquet(type_cl_path) |>
    dplyr::filter(year %in% year_range)
  cli::cli_alert_success(
    "type_cropland: {nrow(type_cropland)} rows ({dplyr::n_distinct(type_cropland$luh2_type)} types)"
  )
} else {
  type_cropland <- NULL
  cli::cli_alert_info(
    "No type_cropland.parquet — using total-cropland allocation"
  )
}

# ---- Load CFT mapping ------------------------------------------------

cft_path <- file.path("inst", "extdata", "cft_mapping.csv")
if (!file.exists(cft_path)) {
  cft_path <- system.file(
    "extdata", "cft_mapping.csv", package = "whep"
  )
}
cft_mapping <- readr::read_csv(cft_path, show_col_types = FALSE)
cli::cli_alert_success(
  "cft_mapping: {nrow(cft_mapping)} item-to-CFT entries"
)

# ---- Ensure only mapped items remain --------------------------------
# Keep only crops that appear in both country_areas and cft_mapping

mapped_items <- cft_mapping$item_prod_code
country_areas <- dplyr::filter(
  country_areas, item_prod_code %in% mapped_items
)
crop_patterns <- dplyr::filter(
  crop_patterns, item_prod_code %in% mapped_items
)

# ---- Run spatialization -----------------------------------------------

cli::cli_h1("Running build_gridded_landuse()")
cli::cli_alert_info(
  "Years: {min(year_range)}-{max(year_range)}"
)

t_start <- proc.time()

# ---- Run at individual crop level (no CFT aggregation) ----------------

cli::cli_h2("Individual crop spatialization")

result_crops <- whep::build_gridded_landuse(
  country_areas = country_areas,
  crop_patterns = crop_patterns,
  gridded_cropland = gridded_cropland,
  country_grid = country_grid,
  cft_mapping = NULL,            # No aggregation → individual crops
  type_cropland = type_cropland, # Per-type LUH2 cropland (NULL = fallback)
  type_mapping = cft_mapping     # item_prod_code → luh2_type for type-aware
)

elapsed <- (proc.time() - t_start)[["elapsed"]]
cli::cli_alert_success(
  "Individual crops done in {round(elapsed / 60, 1)} minutes"
)
cli::cli_alert_info(
  "Result: {nrow(result_crops)} rows, ",
  "{dplyr::n_distinct(result_crops$item_prod_code)} crops"
)

# ---- Save individual crop output --------------------------------------

crop_path <- file.path(output_dir, "gridded_landuse_crops.parquet")
nanoparquet::write_parquet(result_crops, crop_path)
cli::cli_alert_success("Individual crops saved to {crop_path}")

# ---- Aggregate to CFT level -------------------------------------------

cli::cli_h2("CFT aggregation")

result <- result_crops |>
  dplyr::inner_join(
    dplyr::select(cft_mapping, item_prod_code, cft_name),
    by = "item_prod_code"
  ) |>
  dplyr::summarise(
    rainfed_ha = sum(rainfed_ha, na.rm = TRUE),
    irrigated_ha = sum(irrigated_ha, na.rm = TRUE),
    .by = c(lon, lat, year, cft_name)
  )

cli::cli_alert_info(
  "CFT result: {nrow(result)} rows, ",
  "{dplyr::n_distinct(result$cft_name)} CFTs"
)

# ---- Save CFT output --------------------------------------------------

out_path <- file.path(output_dir, "gridded_landuse.parquet")
nanoparquet::write_parquet(result, out_path)
cli::cli_alert_success("CFT output saved to {out_path}")

# ---- Summary by year and CFT ------------------------------------------

summary_tbl <- result |>
  dplyr::summarise(
    total_rainfed_ha = sum(rainfed_ha, na.rm = TRUE),
    total_irrigated_ha = sum(irrigated_ha, na.rm = TRUE),
    n_cells = dplyr::n_distinct(paste(lon, lat)),
    .by = c(year, cft_name)
  ) |>
  dplyr::arrange(year, cft_name)

summary_path <- file.path(output_dir, "landuse_summary.csv")
readr::write_csv(summary_tbl, summary_path)
cli::cli_alert_success("Summary saved to {summary_path}")

# ---- Irrigated/rainfed ratio tables -----------------------------------
# Literature-based irrigated/rainfed ratios per CFT for:
#   - Yield gaps: irrigated yield / rainfed yield
#   - N rate split: irrigated N rate / rainfed N rate
#
# Sources: Siebert & Döll (2010), Mueller et al. (2012),
#   Lassaletta et al. (2014), AQUASTAT yield tables,
#   Zhang et al. (2015).

irrig_rf_ratios <- tibble::tribble(
  ~cft_name,              ~yield_ratio, ~n_rate_ratio,
  "temperate_cereals",    1.3,          1.3,
  "rice",                 1.6,          1.4,
  "maize",                1.5,          1.4,
  "tropical_cereals",     1.3,          1.3,
  "pulses",               1.3,          1.2,
  "oil_crops_soybean",    1.3,          1.2,
  "oil_crops_groundnut",  1.3,          1.3,
  "oil_crops_sunflower",  1.3,          1.3,
  "oil_crops_rapeseed",   1.3,          1.3,
  "oil_crops_other",      1.3,          1.3,
  "sugarcane",            1.2,          1.3,
  "temperate_roots",      1.3,          1.3,
  "tropical_roots",       1.2,          1.2,
  "others_annual",        1.3,          1.3,
  "others_perennial",     1.2,          1.2
)

# Map ratios from CFT to item_prod_code for cell-level joins
item_ratios <- cft_mapping |>
  dplyr::select(item_prod_code, cft_name) |>
  dplyr::inner_join(irrig_rf_ratios, by = "cft_name")

cli::cli_alert_success(
  "Irrigated/rainfed ratios: {nrow(irrig_rf_ratios)} CFTs mapped to {nrow(item_ratios)} items"
)

# ---- Spatialize crop yields (if available) ----------------------------

yields_file <- file.path(input_dir, "country_yields.parquet")
yield_idx_file <- file.path(input_dir, "spatial_yield_index.parquet")

if (file.exists(yields_file)) {
  cli::cli_h2("Spatializing crop yields")

  country_yields <- nanoparquet::read_parquet(yields_file)

  # Load spatial yield index (EarthStat sub-national variation)
  if (file.exists(yield_idx_file)) {
    spatial_yield_idx <- nanoparquet::read_parquet(yield_idx_file)
    cli::cli_alert_success(
      "spatial_yield_index: {nrow(spatial_yield_idx)} pixels loaded"
    )
  } else {
    spatial_yield_idx <- NULL
    cli::cli_alert_info(
      "No spatial_yield_index — using uniform country yields"
    )
  }

  # Join gridded areas with country-level yields
  gridded_y <- result_crops |>
    dplyr::inner_join(country_grid, by = c("lon", "lat")) |>
    dplyr::inner_join(
      country_yields,
      by = c("year", "area_code", "item_prod_code")
    )

  # Apply spatial yield index (sub-national variation)
  if (!is.null(spatial_yield_idx)) {
    gridded_y <- gridded_y |>
      dplyr::left_join(
        spatial_yield_idx,
        by = c("lon", "lat", "item_prod_code")
      ) |>
      dplyr::mutate(
        spatial_yield_index = dplyr::coalesce(spatial_yield_index, 1.0)
      )

    # Renormalize to preserve country production totals
    gridded_y <- gridded_y |>
      dplyr::mutate(
        total_ha = rainfed_ha + irrigated_ha,
        weighted_sum = sum(
          spatial_yield_index * total_ha, na.rm = TRUE
        ),
        ha_sum = sum(total_ha, na.rm = TRUE),
        renorm = dplyr::if_else(
          weighted_sum > 0, ha_sum / weighted_sum, 1.0
        ),
        yield_t_ha = yield_t_ha * spatial_yield_index * renorm,
        .by = c("year", "area_code", "item_prod_code")
      ) |>
      dplyr::select(
        -spatial_yield_index, -total_ha, -weighted_sum,
        -ha_sum, -renorm
      )

    cli::cli_alert_success("Applied sub-national yield spatial index")
  }

  # Split into irrigated/rainfed using yield ratios
  # Conservation: yield_rf * rf_ha + yield_irr * irr_ha = yield * total_ha
  # Ratio: yield_irr = R * yield_rf
  # Solve: yield_rf = yield * total_ha / (rf_ha + R * irr_ha)
  gridded_y <- gridded_y |>
    dplyr::left_join(
      dplyr::select(item_ratios, item_prod_code, yield_ratio),
      by = "item_prod_code"
    ) |>
    dplyr::mutate(
      yield_ratio = dplyr::coalesce(yield_ratio, 1.0),
      total_ha = rainfed_ha + irrigated_ha,
      denom = rainfed_ha + yield_ratio * irrigated_ha,
      yield_rainfed = dplyr::if_else(
        denom > 0, yield_t_ha * total_ha / denom, yield_t_ha
      ),
      yield_irrigated = yield_ratio * yield_rainfed,
      rainfed_prod_t = yield_rainfed * rainfed_ha,
      irrigated_prod_t = yield_irrigated * irrigated_ha
    ) |>
    dplyr::select(
      lon, lat, year, area_code, item_prod_code,
      rainfed_ha, irrigated_ha,
      yield_rainfed, yield_irrigated,
      rainfed_prod_t, irrigated_prod_t
    )

  y_path <- file.path(output_dir, "gridded_yields.parquet")
  nanoparquet::write_parquet(gridded_y, y_path)
  cli::cli_alert_success(
    "Gridded yields: {nrow(gridded_y)} rows \u2192 {y_path}"
  )
} else {
  cli::cli_alert_info(
    "No country_yields.parquet — skipping yield spatialization"
  )
}

# ---- Spatialize fertilizer inputs (if available) ----------------------

n_inputs_file <- file.path(input_dir, "nitrogen_inputs.parquet")
if (file.exists(n_inputs_file)) {
  cli::cli_h2("Spatializing nitrogen inputs")

  n_inputs <- nanoparquet::read_parquet(n_inputs_file)

  # Load sub-national spatial N index (if available)
  spatial_idx_file <- file.path(input_dir, "spatial_n_index.parquet")
  if (file.exists(spatial_idx_file)) {
    spatial_n_idx <- nanoparquet::read_parquet(spatial_idx_file)
    cli::cli_alert_success(
      "spatial_n_index: {nrow(spatial_n_idx)} pixels loaded"
    )
  } else {
    spatial_n_idx <- NULL
    cli::cli_alert_info(
      "No spatial_n_index.parquet — using uniform country rates"
    )
  }

  # Join gridded crop areas with N rates per crop
  # Both use item_prod_code (via crop_name -> items_prod mapping)
  n_rates <- n_inputs |>
    dplyr::filter(!is.na(kg_n_ha), kg_n_ha > 0) |>
    dplyr::summarize(
      kg_n_ha = sum(kg_n_ha, na.rm = TRUE),
      .by = c(year, area_code, crop_name, fert_type)
    )

  # Map crop names back to item_prod_code using items_prod
  items_prod <- readr::read_csv(
    system.file("extdata", "items_prod.csv", package = "whep"),
    show_col_types = FALSE
  )

  n_rates <- n_rates |>
    dplyr::left_join(
      dplyr::select(items_prod, item_prod_code, item_prod_name),
      by = c("crop_name" = "item_prod_name")
    ) |>
    dplyr::filter(!is.na(item_prod_code))

  # Join with gridded areas to get N application per cell
  gridded_n <- result_crops |>
    dplyr::inner_join(country_grid, by = c("lon", "lat")) |>
    dplyr::inner_join(
      n_rates,
      by = c("year", "area_code", "item_prod_code")
    )

  # Apply sub-national spatial variation (West/EarthStat pixel index)
  if (!is.null(spatial_n_idx)) {
    gridded_n <- gridded_n |>
      dplyr::left_join(
        spatial_n_idx,
        by = c("lon", "lat", "item_prod_code", "fert_type")
      ) |>
      dplyr::mutate(
        spatial_n_index = dplyr::coalesce(spatial_n_index, 1.0)
      )

    # Renormalize within (year, area_code, item_prod_code, fert_type)
    # so that country totals are preserved after applying the index
    gridded_n <- gridded_n |>
      dplyr::mutate(
        total_ha = rainfed_ha + irrigated_ha,
        weighted_sum = sum(spatial_n_index * total_ha, na.rm = TRUE),
        ha_sum = sum(total_ha, na.rm = TRUE),
        renorm = dplyr::if_else(
          weighted_sum > 0, ha_sum / weighted_sum, 1.0
        ),
        kg_n_ha = kg_n_ha * spatial_n_index * renorm,
        .by = c("year", "area_code", "item_prod_code", "fert_type")
      ) |>
      dplyr::select(
        -spatial_n_index, -total_ha, -weighted_sum, -ha_sum, -renorm
      )

    cli::cli_alert_success("Applied sub-national N rate spatial index")
  }

  # Split N rates between irrigated/rainfed using crop-specific ratios
  # Conservation: n_rf * rf_ha + n_irr * irr_ha = n * total_ha
  # Ratio: n_irr = R * n_rf
  # Solve: n_rf = n * total_ha / (rf_ha + R * irr_ha)
  gridded_n <- gridded_n |>
    dplyr::left_join(
      dplyr::select(item_ratios, item_prod_code, n_rate_ratio),
      by = "item_prod_code"
    ) |>
    dplyr::mutate(
      n_rate_ratio = dplyr::coalesce(n_rate_ratio, 1.0),
      total_ha = rainfed_ha + irrigated_ha,
      denom = rainfed_ha + n_rate_ratio * irrigated_ha,
      kg_n_ha_rainfed = dplyr::if_else(
        denom > 0, kg_n_ha * total_ha / denom, kg_n_ha
      ),
      kg_n_ha_irrigated = n_rate_ratio * kg_n_ha_rainfed,
      rainfed_n_mg = rainfed_ha * kg_n_ha_rainfed / 1000,
      irrigated_n_mg = irrigated_ha * kg_n_ha_irrigated / 1000
    ) |>
    dplyr::select(
      lon, lat, year, area_code, item_prod_code,
      fert_type, kg_n_ha, kg_n_ha_rainfed, kg_n_ha_irrigated,
      rainfed_ha, irrigated_ha,
      rainfed_n_mg, irrigated_n_mg
    )

  n_grid_path <- file.path(output_dir, "gridded_nitrogen.parquet")
  nanoparquet::write_parquet(gridded_n, n_grid_path)
  cli::cli_alert_success(
    "Gridded nitrogen: {nrow(gridded_n)} rows → {n_grid_path}"
  )

  # Also aggregate to CFT level
  gridded_n_cft <- gridded_n |>
    dplyr::inner_join(
      dplyr::select(cft_mapping, item_prod_code, cft_name),
      by = "item_prod_code"
    ) |>
    dplyr::summarize(
      rainfed_n_mg = sum(rainfed_n_mg, na.rm = TRUE),
      irrigated_n_mg = sum(irrigated_n_mg, na.rm = TRUE),
      rainfed_ha = sum(rainfed_ha, na.rm = TRUE),
      irrigated_ha = sum(irrigated_ha, na.rm = TRUE),
      .by = c(lon, lat, year, cft_name, fert_type)
    ) |>
    dplyr::mutate(
      total_ha = rainfed_ha + irrigated_ha,
      kg_n_ha = dplyr::if_else(
        total_ha > 0,
        (rainfed_n_mg + irrigated_n_mg) * 1000 / total_ha, 0
      ),
      kg_n_ha_rainfed = dplyr::if_else(
        rainfed_ha > 0, rainfed_n_mg * 1000 / rainfed_ha, 0
      ),
      kg_n_ha_irrigated = dplyr::if_else(
        irrigated_ha > 0, irrigated_n_mg * 1000 / irrigated_ha, 0
      )
    )

  n_cft_path <- file.path(output_dir, "gridded_nitrogen_cft.parquet")
  nanoparquet::write_parquet(gridded_n_cft, n_cft_path)
  cli::cli_alert_success(
    "CFT nitrogen: {nrow(gridded_n_cft)} rows → {n_cft_path}"
  )
} else {
  cli::cli_alert_warning(
    "nitrogen_inputs.parquet not found — skipping fertilizer spatialization"
  )
  cli::cli_alert_info(
    "  Run prepare_nitrogen_inputs.R first to enable this step."
  )
}
