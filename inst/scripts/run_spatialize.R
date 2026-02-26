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
  cft_mapping = NULL  # No aggregation → individual item_prod_codes
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

# ---- Spatialize fertilizer inputs (if available) ----------------------

n_inputs_file <- file.path(input_dir, "nitrogen_inputs.parquet")
if (file.exists(n_inputs_file)) {
  cli::cli_h2("Spatializing nitrogen inputs")

  n_inputs <- nanoparquet::read_parquet(n_inputs_file)

  # Join gridded crop areas with N rates per crop
  # Both use item_prod_code (via crop_name → items_prod mapping)
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
    ) |>
    dplyr::mutate(
      rainfed_n_mg = rainfed_ha * kg_n_ha / 1000,
      irrigated_n_mg = irrigated_ha * kg_n_ha / 1000
    ) |>
    dplyr::select(
      lon, lat, year, area_code, item_prod_code,
      fert_type, kg_n_ha, rainfed_ha, irrigated_ha,
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
        total_ha > 0, (rainfed_n_mg + irrigated_n_mg) * 1000 / total_ha, 0
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
