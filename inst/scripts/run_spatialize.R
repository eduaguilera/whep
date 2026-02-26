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
year_range <- 2000L:2015L

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

result <- whep::build_gridded_landuse(
  country_areas = country_areas,
  crop_patterns = crop_patterns,
  gridded_cropland = gridded_cropland,
  country_grid = country_grid,
  cft_mapping = cft_mapping
)

elapsed <- (proc.time() - t_start)[["elapsed"]]
cli::cli_alert_success(
  "Done in {round(elapsed / 60, 1)} minutes"
)
cli::cli_alert_info(
  "Result: {nrow(result)} rows, {ncol(result)} columns"
)

# ---- Save output ------------------------------------------------------

out_path <- file.path(output_dir, "gridded_landuse.parquet")
nanoparquet::write_parquet(result, out_path)
cli::cli_alert_success("Output saved to {out_path}")

# Also save a CSV summary by year and CFT
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
