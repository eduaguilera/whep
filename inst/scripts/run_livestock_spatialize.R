# -----------------------------------------------------------------------
# run_livestock_spatialize.R
#
# Runner script: loads prepared inputs and calls
# build_gridded_livestock() to produce gridded livestock data.
#
# Prerequisites:
#   1. Run prepare_spatialize_inputs.R       (country_grid, gridded_cropland)
#   2. Run prepare_livestock_spatialize_inputs.R (livestock_country_data,
#      gridded_pasture, manure_pattern)
#
# Outputs (to L_files/whep/):
#   - gridded_livestock_emissions.parquet
#   - livestock_summary.csv
# -----------------------------------------------------------------------

library(dplyr, warn.conflicts = FALSE)
devtools::load_all(".")

# ==== Configuration ====================================================

l_files_dir <- "WHEP_L_FILES_DIR_PLACEHOLDER"
input_dir <- file.path(l_files_dir, "whep", "inputs")
output_dir <- file.path(l_files_dir, "whep")


# ==== Load inputs ======================================================

cli::cli_h1("Livestock spatialization")
cli::cli_h2("Loading inputs")

# -- Livestock country data ---
livestock_data <- nanoparquet::read_parquet(
  file.path(input_dir, "livestock_country_data.parquet")
)
cli::cli_alert_success(
  "livestock_country_data: {nrow(livestock_data)} rows, ",
  "{n_distinct(livestock_data$species_group)} groups, ",
  "{n_distinct(livestock_data$year)} years"
)

# -- Gridded pasture ---
gridded_pasture <- nanoparquet::read_parquet(
  file.path(input_dir, "gridded_pasture.parquet")
)
cli::cli_alert_success(
  "gridded_pasture: {nrow(gridded_pasture)} rows"
)

# -- Gridded cropland (from crop spatialization pipeline) ---
gridded_cropland <- nanoparquet::read_parquet(
  file.path(input_dir, "gridded_cropland.parquet")
)
cli::cli_alert_success(
  "gridded_cropland: {nrow(gridded_cropland)} rows"
)

# -- Country grid ---
country_grid <- nanoparquet::read_parquet(
  file.path(input_dir, "country_grid.parquet")
)
cli::cli_alert_success(
  "country_grid: {nrow(country_grid)} cells"
)

# -- Livestock mapping (for species_proxy) ---
mapping_path <- system.file(
  "extdata", "livestock_mapping.csv", package = "whep"
)
if (nchar(mapping_path) == 0) {
  mapping_path <- file.path("inst", "extdata", "livestock_mapping.csv")
}
livestock_mapping <- readr::read_csv(mapping_path, show_col_types = FALSE)

species_proxy <- livestock_mapping |>
  distinct(species_group, spatial_proxy)

cli::cli_alert_success(
  "species_proxy: {nrow(species_proxy)} groups"
)

# -- Manure pattern (optional, West et al. 2014) ---
manure_pattern_path <- file.path(input_dir, "manure_pattern.parquet")
manure_pattern <- NULL
if (file.exists(manure_pattern_path)) {
  manure_pattern <- nanoparquet::read_parquet(manure_pattern_path)
  cli::cli_alert_success(
    "manure_pattern: {nrow(manure_pattern)} cells (West et al. 2014)"
  )
} else {
  cli::cli_alert_info(
    "No manure_pattern available - using LUH2 only"
  )
}

# -- GLW3 (optional) ---
glw_density <- NULL
# GLW3 not currently available; will be integrated when downloaded.
# Future: read species-specific density TIF files.


# ==== Run spatialization ===============================================

cli::cli_h2("Running build_gridded_livestock()")

# Process in annual chunks to manage memory
years <- sort(unique(livestock_data$year))
n_years <- length(years)

# Chunk into groups of ~20 years
chunk_size <- 20L
year_chunks <- split(years, ceiling(seq_along(years) / chunk_size))

cli::cli_alert_info(
  "Processing {n_years} years in {length(year_chunks)} chunks"
)

result_list <- list()

for (i in seq_along(year_chunks)) {
  chunk_years <- year_chunks[[i]]
  cli::cli_alert(
    "  Chunk {i}/{length(year_chunks)}: ",
    "{min(chunk_years)}-{max(chunk_years)}"
  )

  chunk_data <- filter(livestock_data, year %in% chunk_years)
  chunk_pasture <- filter(gridded_pasture, year %in% chunk_years)
  chunk_cropland <- filter(gridded_cropland, year %in% chunk_years)

  chunk_result <- build_gridded_livestock(
    livestock_data = chunk_data,
    gridded_pasture = chunk_pasture,
    gridded_cropland = chunk_cropland,
    country_grid = country_grid,
    species_proxy = species_proxy,
    manure_pattern = manure_pattern,
    glw_density = glw_density
  )

  result_list[[i]] <- chunk_result
}

gridded_livestock <- bind_rows(result_list)


# ==== Conservation check ===============================================

cli::cli_h2("Conservation validation")

# Check that gridded totals match country totals
country_totals <- livestock_data |>
  summarise(
    total_heads = sum(heads, na.rm = TRUE),
    total_enteric = sum(enteric_ch4_kt, na.rm = TRUE),
    .by = year
  )

grid_totals <- gridded_livestock |>
  summarise(
    grid_heads = sum(heads, na.rm = TRUE),
    grid_enteric = sum(enteric_ch4_kt, na.rm = TRUE),
    .by = year
  )

check <- inner_join(country_totals, grid_totals, by = "year") |>
  mutate(
    heads_ratio = grid_heads / total_heads,
    enteric_ratio = if_else(
      total_enteric > 0, grid_enteric / total_enteric, 1
    )
  )

heads_range <- range(check$heads_ratio, na.rm = TRUE)
cli::cli_alert_info(
  "Heads conservation ratio: {round(heads_range[1], 4)} - ",
  "{round(heads_range[2], 4)}"
)

if (all(abs(check$heads_ratio - 1) < 0.01, na.rm = TRUE)) {
  cli::cli_alert_success("Conservation check PASSED (< 1% error)")
} else {
  lost_years <- check |>
    filter(abs(heads_ratio - 1) >= 0.01) |>
    pull(year)
  cli::cli_alert_warning(
    "Conservation > 1% error in {length(lost_years)} years"
  )
}


# ==== Save output ======================================================

cli::cli_h2("Saving output")

# Save gridded livestock emissions
nanoparquet::write_parquet(
  gridded_livestock,
  file.path(output_dir, "gridded_livestock_emissions.parquet")
)

sz <- round(
  file.size(
    file.path(output_dir, "gridded_livestock_emissions.parquet")
  ) / 1024 / 1024, 1
)
n_cells <- n_distinct(paste(
  gridded_livestock$lon, gridded_livestock$lat
))
cli::cli_alert_success(
  "gridded_livestock_emissions.parquet: ",
  "{nrow(gridded_livestock)} rows, {n_cells} cells ({sz} MB)"
)

# Summary CSV
summary_tbl <- gridded_livestock |>
  summarise(
    total_heads = round(sum(heads, na.rm = TRUE)),
    enteric_ch4_kt = round(sum(enteric_ch4_kt, na.rm = TRUE), 2),
    manure_ch4_kt = round(sum(manure_ch4_kt, na.rm = TRUE), 2),
    manure_n2o_kt = round(sum(manure_n2o_kt, na.rm = TRUE), 2),
    manure_n_mg = round(sum(manure_n_mg, na.rm = TRUE), 1),
    n_cells = n_distinct(paste(lon, lat)),
    .by = c(year, species_group)
  ) |>
  arrange(year, species_group)

readr::write_csv(
  summary_tbl,
  file.path(output_dir, "livestock_summary.csv")
)
cli::cli_alert_success(
  "livestock_summary.csv: {nrow(summary_tbl)} rows"
)


# ==== Final summary ====================================================

cli::cli_h1("Livestock spatialization complete")

yr_range <- range(gridded_livestock$year)
groups <- sort(unique(gridded_livestock$species_group))

cli::cli_alert_success(
  "{yr_range[1]}-{yr_range[2]}, {length(groups)} groups, ",
  "{n_cells} cells"
)
cli::cli_alert_info("Groups: {paste(groups, collapse = ', ')}")

# Print 2020 snapshot if available
if (2020L %in% unique(gridded_livestock$year)) {
  snap <- gridded_livestock |>
    filter(year == 2020L) |>
    summarise(
      heads_M = round(sum(heads, na.rm = TRUE) / 1e6, 1),
      enteric_Mt = round(
        sum(enteric_ch4_kt, na.rm = TRUE) / 1e3, 2
      ),
      manure_ch4_Mt = round(
        sum(manure_ch4_kt, na.rm = TRUE) / 1e3, 2
      ),
      .by = species_group
    ) |>
    arrange(desc(heads_M))
  cli::cli_alert_info("2020 snapshot:")
  print(as.data.frame(snap), row.names = FALSE)
}

cli::cli_alert_success("Done!")
