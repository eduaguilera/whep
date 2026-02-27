# -----------------------------------------------------------------------
# update_country_areas_mirca.R
#
# Re-generates country_areas.parquet with MIRCA2000 irrigation fractions.
# This is a lightweight alternative to re-running the full
# prepare_spatialize_inputs.R — it only regenerates the country_areas
# output, using the existing MIRCA, LUH2, and FAOSTAT data.
#
# Prerequisites:
#   - mirca_irrigation_country.parquet (from prepare_mirca_inputs.R)
#   - country_grid.parquet (from prepare_spatialize_inputs.R)
# -----------------------------------------------------------------------

library(dplyr, warn.conflicts = FALSE)

l_files_dir <- "C:/Users/53272530E/OneDrive/L_files"
output_dir <- file.path(l_files_dir, "whep", "inputs")
year_range <- 1850L:2022L
target_res <- 0.5

# Source the prepare functions (but not the main execution block)
# We extract just what we need: prepare_country_areas and its helpers
local({
  # Read the whole script source
  lines <- readLines("inst/scripts/prepare_spatialize_inputs.R")

  # Find the Main execution line and cut before it
  main_line <- grep("^# ==== Main execution", lines)
  if (length(main_line) > 0) {
    lines <- lines[seq_len(main_line[1] - 1)]
  }

  # Source the functions only
  tmp <- tempfile(fileext = ".R")
  writeLines(lines, tmp)
  source(tmp, local = globalenv())
  unlink(tmp)
})

cli::cli_h1("Updating country_areas with MIRCA2000 irrigation")

# Load existing country_grid
country_grid <- nanoparquet::read_parquet(
  file.path(output_dir, "country_grid.parquet")
)
cli::cli_alert_info("country_grid: {nrow(country_grid)} cells")

# Check MIRCA is available
mirca_path <- file.path(output_dir, "mirca_irrigation_country.parquet")
if (!file.exists(mirca_path)) {
  cli::cli_abort("mirca_irrigation_country.parquet not found. Run prepare_mirca_inputs.R first.")
}
cli::cli_alert_success("MIRCA irrigation fractions available")

# Re-generate country_areas with MIRCA
country_areas <- prepare_country_areas(
  l_files_dir, year_range, country_grid, target_res
)

nanoparquet::write_parquet(
  country_areas,
  file.path(output_dir, "country_areas.parquet")
)

# Quick stats
irrig_stats <- country_areas |>
  summarize(
    n = n(),
    pct_irrigated = mean(irrigated_area_ha > 0) * 100,
    total_irrig_mha = sum(irrigated_area_ha) / 1e6,
    total_harvest_mha = sum(harvested_area_ha) / 1e6,
    global_irrig_pct = total_irrig_mha / total_harvest_mha * 100
  )

cli::cli_alert_success(
  "country_areas: {irrig_stats$n} rows saved"
)
cli::cli_alert_info(
  "{round(irrig_stats$pct_irrigated, 1)}% of crop-country-year records have irrigation > 0"
)
cli::cli_alert_info(
  "Global irrigation: {round(irrig_stats$total_irrig_mha, 1)} Mha / {round(irrig_stats$total_harvest_mha, 1)} Mha = {round(irrig_stats$global_irrig_pct, 1)}%"
)
