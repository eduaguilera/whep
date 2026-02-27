# -----------------------------------------------------------------------
# figure_validate_spatialized.R
#
# Validation figures comparing spatialized gridded outputs against the
# original country-level FAOSTAT totals (country_areas.parquet).
#
# The core check is area conservation: the sum of gridded rainfed +
# irrigated areas should exactly reproduce the country-level harvested
# and irrigated totals that went into build_gridded_landuse().
#
# Outputs saved to L_files/whep/figures/:
#   1. val_global_totals.png               — global total comparison
#   2. val_cft_totals.png                  — per-CFT total comparison
#   3. val_scatter_country_<year>.png       — country scatter (1:1 line)
#   4. val_relative_error_distribution.png  — histogram of country errors
#   5. val_timeseries_top_deviations.png    — worst-case country trends
#   6. val_irrigated_totals.png             — irrigated area validation
#   7. val_country_panel.png                — grid of large countries
#
# Requirements: ggplot2, dplyr, tidyr, patchwork, scales, viridis,
#               nanoparquet, readr, cli
# -----------------------------------------------------------------------

library(dplyr, warn.conflicts = FALSE)
library(ggplot2)
library(tidyr)
library(patchwork)

devtools::load_all(".")

# ---- Configuration ---------------------------------------------------

l_files_dir <- "C:/Users/53272530E/OneDrive/L_files"
whep_dir <- file.path(l_files_dir, "whep")
input_dir <- file.path(whep_dir, "inputs")
fig_dir <- file.path(whep_dir, "figures")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

val_dpi <- 300

# ---- Load data --------------------------------------------------------

cli::cli_h1("Loading data for validation")

# Original country-level inputs
country_areas <- nanoparquet::read_parquet(
  file.path(input_dir, "country_areas.parquet")
)
cli::cli_alert_success(
  "country_areas: {nrow(country_areas)} rows"
)

# CFT mapping
cft_mapping <- readr::read_csv(
  file.path("inst", "extdata", "cft_mapping.csv"),
  show_col_types = FALSE
)

# Gridded output
gridded <- nanoparquet::read_parquet(
  file.path(whep_dir, "gridded_landuse.parquet")
)
cli::cli_alert_success(
  "gridded_landuse: {nrow(gridded)} rows"
)

# Country grid mapping
country_grid <- nanoparquet::read_parquet(
  file.path(input_dir, "country_grid.parquet")
)

# Polities for country names
polities <- whep::polities |>
  dplyr::select(area_code, area_name, continent) |>
  dplyr::distinct()

# ---- Prepare reference totals ----------------------------------------

cli::cli_h2("Preparing reference totals")

# Country-CFT level reference: sum original areas by CFT
ref_cft <- country_areas |>
  dplyr::inner_join(
    dplyr::select(cft_mapping, item_prod_code, cft_name),
    by = "item_prod_code"
  ) |>
  dplyr::summarise(
    ref_harvested_ha = sum(harvested_area_ha, na.rm = TRUE),
    ref_irrigated_ha = sum(irrigated_area_ha, na.rm = TRUE),
    .by = c(year, area_code, cft_name)
  )

# Global-year-CFT reference
ref_global_cft <- ref_cft |>
  dplyr::summarise(
    ref_harvested_mha = sum(ref_harvested_ha) / 1e6,
    ref_irrigated_mha = sum(ref_irrigated_ha) / 1e6,
    .by = c(year, cft_name)
  )

# Global-year reference
ref_global <- ref_cft |>
  dplyr::summarise(
    ref_harvested_mha = sum(ref_harvested_ha) / 1e6,
    ref_irrigated_mha = sum(ref_irrigated_ha) / 1e6,
    .by = year
  )

# ---- Prepare gridded totals ------------------------------------------

cli::cli_h2("Aggregating gridded totals")

# Gridded to country-CFT totals
grid_country_cft <- gridded |>
  dplyr::inner_join(country_grid, by = c("lon", "lat")) |>
  dplyr::summarise(
    grid_harvested_ha = sum(rainfed_ha + irrigated_ha, na.rm = TRUE),
    grid_irrigated_ha = sum(irrigated_ha, na.rm = TRUE),
    .by = c(year, area_code, cft_name)
  )

# Global-year-CFT gridded
grid_global_cft <- grid_country_cft |>
  dplyr::summarise(
    grid_harvested_mha = sum(grid_harvested_ha) / 1e6,
    grid_irrigated_mha = sum(grid_irrigated_ha) / 1e6,
    .by = c(year, cft_name)
  )

# Global-year gridded
grid_global <- grid_country_cft |>
  dplyr::summarise(
    grid_harvested_mha = sum(grid_harvested_ha) / 1e6,
    grid_irrigated_mha = sum(grid_irrigated_ha) / 1e6,
    .by = year
  )

# ---- Figure 1: Global total comparison --------------------------------

cli::cli_h2("Figure 1: Global total comparison")

comp_global <- ref_global |>
  dplyr::inner_join(grid_global, by = "year") |>
  tidyr::pivot_longer(
    cols = -year,
    names_to = c("source", "type"),
    names_pattern = "(ref|grid)_(harvested|irrigated)_mha"
  ) |>
  dplyr::mutate(
    source = dplyr::if_else(source == "ref", "FAOSTAT input", "Gridded output"),
    type = dplyr::if_else(type == "harvested", "Harvested", "Irrigated")
  )

p1 <- ggplot(comp_global, aes(x = year, y = value, colour = source)) +
  geom_line(linewidth = 0.7) +
  facet_wrap(~type, scales = "free_y", ncol = 2) +
  scale_colour_manual(
    values = c("FAOSTAT input" = "#2c7bb6", "Gridded output" = "#d7191c")
  ) +
  scale_x_continuous(breaks = seq(1850, 2020, 25)) +
  scale_y_continuous(labels = scales::label_comma()) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold", size = 12)
  ) +
  labs(
    title = "Validation: global totals — FAOSTAT input vs gridded output",
    subtitle = "Lines should overlap if area conservation holds",
    x = "Year", y = "Area (million ha)", colour = NULL
  )

ggsave(
  file.path(fig_dir, "val_global_totals.png"),
  p1, width = 12, height = 5, dpi = val_dpi
)
cli::cli_alert_success("Saved val_global_totals.png")

# ---- Figure 2: Per-CFT total comparison --------------------------------

cli::cli_h2("Figure 2: Per-CFT total comparison")

comp_cft <- ref_global_cft |>
  dplyr::inner_join(grid_global_cft, by = c("year", "cft_name")) |>
  dplyr::select(year, cft_name, ref_harvested_mha, grid_harvested_mha)

comp_cft_long <- comp_cft |>
  tidyr::pivot_longer(
    cols = c(ref_harvested_mha, grid_harvested_mha),
    names_to = "source",
    values_to = "area_mha"
  ) |>
  dplyr::mutate(
    source = dplyr::if_else(
      grepl("ref", source), "FAOSTAT input", "Gridded output"
    )
  )

p2 <- ggplot(comp_cft_long, aes(x = year, y = area_mha, colour = source)) +
  geom_line(linewidth = 0.5) +
  facet_wrap(~cft_name, scales = "free_y", ncol = 3) +
  scale_colour_manual(
    values = c("FAOSTAT input" = "#2c7bb6", "Gridded output" = "#d7191c")
  ) +
  scale_x_continuous(breaks = seq(1850, 2020, 50)) +
  theme_minimal(base_size = 9) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold")
  ) +
  labs(
    title = "Validation by crop functional type: FAOSTAT vs gridded",
    x = "Year", y = "Area (million ha)", colour = NULL
  )

ggsave(
  file.path(fig_dir, "val_cft_totals.png"),
  p2, width = 14, height = 10, dpi = val_dpi
)
cli::cli_alert_success("Saved val_cft_totals.png")

# ---- Figure 3: Country scatter (1:1 line) ----------------------------

cli::cli_h2("Figure 3: Country scatter plots")

# Country-year totals (all CFTs summed)
ref_country <- ref_cft |>
  dplyr::summarise(
    ref_mha = sum(ref_harvested_ha) / 1e6,
    ref_irr_mha = sum(ref_irrigated_ha) / 1e6,
    .by = c(year, area_code)
  )

grid_country <- grid_country_cft |>
  dplyr::summarise(
    grid_mha = sum(grid_harvested_ha) / 1e6,
    grid_irr_mha = sum(grid_irrigated_ha) / 1e6,
    .by = c(year, area_code)
  )

comp_country <- ref_country |>
  dplyr::inner_join(grid_country, by = c("year", "area_code")) |>
  dplyr::inner_join(polities, by = "area_code")

scatter_years <- c(1960L, 1980L, 2000L, 2020L)

for (yr in scatter_years) {
  if (!yr %in% comp_country$year) next

  df <- dplyr::filter(comp_country, year == yr, ref_mha > 0)

  max_val <- max(c(df$ref_mha, df$grid_mha), na.rm = TRUE) * 1.05

  p <- ggplot(df, aes(x = ref_mha, y = grid_mha)) +
    geom_abline(
      slope = 1, intercept = 0,
      linetype = "dashed", colour = "grey50"
    ) +
    geom_point(aes(colour = continent), alpha = 0.7, size = 2) +
    # Label outliers
    ggrepel::geom_text_repel(
      data = dplyr::slice_max(df, abs(grid_mha - ref_mha), n = 8),
      aes(label = area_name),
      size = 2.5, max.overlaps = 15
    ) +
    scale_colour_brewer(palette = "Set2") +
    coord_fixed(xlim = c(0, max_val), ylim = c(0, max_val)) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold")
    ) +
    labs(
      title = paste0(
        "Country harvested area: FAOSTAT vs gridded — ", yr
      ),
      subtitle = "Points on the dashed 1:1 line indicate perfect conservation",
      x = "FAOSTAT input (million ha)",
      y = "Gridded output (million ha)",
      colour = "Continent"
    )

  fpath <- file.path(
    fig_dir, paste0("val_scatter_country_", yr, ".png")
  )
  ggsave(fpath, p, width = 8, height = 8, dpi = val_dpi)
  cli::cli_alert_success("Saved {fpath}")
}

# ---- Figure 4: Relative error distribution ----------------------------

cli::cli_h2("Figure 4: Relative error distribution")

# Compute relative errors per (country, year)
errors <- comp_country |>
  dplyr::filter(ref_mha > 0.001) |>
  dplyr::mutate(
    rel_error_pct = (grid_mha - ref_mha) / ref_mha * 100,
    abs_error_pct = abs(rel_error_pct)
  )

# Summary statistics
error_stats <- errors |>
  dplyr::summarise(
    median_pct = median(rel_error_pct, na.rm = TRUE),
    mean_pct = mean(rel_error_pct, na.rm = TRUE),
    p95_abs = quantile(abs_error_pct, 0.95, na.rm = TRUE),
    p99_abs = quantile(abs_error_pct, 0.99, na.rm = TRUE),
    max_abs = max(abs_error_pct, na.rm = TRUE),
    frac_below_1pct = mean(abs_error_pct < 1, na.rm = TRUE),
    frac_below_5pct = mean(abs_error_pct < 5, na.rm = TRUE),
    n = dplyr::n()
  )

cli::cli_alert_info("Error stats:")
cli::cli_alert_info("  Median:  {round(error_stats$median_pct, 4)}%")
cli::cli_alert_info("  Mean:    {round(error_stats$mean_pct, 4)}%")
cli::cli_alert_info("  P95 abs: {round(error_stats$p95_abs, 4)}%")
cli::cli_alert_info("  P99 abs: {round(error_stats$p99_abs, 4)}%")
cli::cli_alert_info(
  "  <1% error: {round(error_stats$frac_below_1pct*100, 1)}% of obs"
)
cli::cli_alert_info(
  "  <5% error: {round(error_stats$frac_below_5pct*100, 1)}% of obs"
)

# Clip extreme tails for plotting
errors_plot <- errors |>
  dplyr::mutate(
    rel_error_clipped = pmax(pmin(rel_error_pct, 10), -10)
  )

p4 <- ggplot(errors_plot, aes(x = rel_error_clipped)) +
  geom_histogram(
    bins = 100, fill = "#2c7bb6", colour = "white", linewidth = 0.1
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "red") +
  scale_x_continuous(
    breaks = seq(-10, 10, 2),
    labels = paste0(seq(-10, 10, 2), "%")
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold")) +
  labs(
    title = "Distribution of relative error (gridded vs FAOSTAT, per country-year)",
    subtitle = paste0(
      "Median = ", round(error_stats$median_pct, 3), "% | ",
      round(error_stats$frac_below_1pct * 100, 1),
      "% of observations within \u00b11%"
    ),
    x = "Relative error (%, clipped to \u00b110%)",
    y = "Count (country-year pairs)"
  )

ggsave(
  file.path(fig_dir, "val_relative_error_distribution.png"),
  p4, width = 10, height = 5, dpi = val_dpi
)
cli::cli_alert_success("Saved val_relative_error_distribution.png")

# ---- Figure 5: Time series of worst-case deviations -------------------

cli::cli_h2("Figure 5: Worst-case country deviations")

# Countries with largest average absolute errors across years
worst <- errors |>
  dplyr::summarise(
    mean_abs_err = mean(abs_error_pct, na.rm = TRUE),
    mean_ref_mha = mean(ref_mha, na.rm = TRUE),
    .by = c(area_code, area_name)
  ) |>
  # Only countries with meaningful area
  dplyr::filter(mean_ref_mha > 0.5) |>
  dplyr::slice_max(mean_abs_err, n = 12)

worst_ts <- errors |>
  dplyr::filter(area_code %in% worst$area_code) |>
  dplyr::select(year, area_name, ref_mha, grid_mha)

worst_long <- worst_ts |>
  tidyr::pivot_longer(
    cols = c(ref_mha, grid_mha),
    names_to = "source",
    values_to = "area_mha"
  ) |>
  dplyr::mutate(
    source = dplyr::if_else(
      source == "ref_mha", "FAOSTAT input", "Gridded output"
    )
  )

p5 <- ggplot(
  worst_long,
  aes(x = year, y = area_mha, colour = source)
) +
  geom_line(linewidth = 0.6) +
  facet_wrap(~area_name, scales = "free_y", ncol = 3) +
  scale_colour_manual(
    values = c("FAOSTAT input" = "#2c7bb6", "Gridded output" = "#d7191c")
  ) +
  theme_minimal(base_size = 9) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold")
  ) +
  labs(
    title = "Countries with largest mean deviations (>0.5 Mha average area)",
    x = "Year", y = "Harvested area (million ha)", colour = NULL
  )

ggsave(
  file.path(fig_dir, "val_timeseries_top_deviations.png"),
  p5, width = 14, height = 10, dpi = val_dpi
)
cli::cli_alert_success("Saved val_timeseries_top_deviations.png")

# ---- Figure 6: Irrigated area validation ------------------------------

cli::cli_h2("Figure 6: Irrigated area validation")

comp_irr <- ref_global |>
  dplyr::inner_join(grid_global, by = "year") |>
  dplyr::select(year, ref_irrigated_mha, grid_irrigated_mha) |>
  tidyr::pivot_longer(
    cols = -year,
    names_to = "source",
    values_to = "irrigated_mha"
  ) |>
  dplyr::mutate(
    source = dplyr::if_else(
      grepl("ref", source), "FAOSTAT input", "Gridded output"
    )
  )

p6 <- ggplot(comp_irr, aes(x = year, y = irrigated_mha, colour = source)) +
  geom_line(linewidth = 0.8) +
  scale_colour_manual(
    values = c("FAOSTAT input" = "#2c7bb6", "Gridded output" = "#d7191c")
  ) +
  scale_x_continuous(breaks = seq(1850, 2020, 25)) +
  scale_y_continuous(labels = scales::label_comma()) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  ) +
  labs(
    title = "Validation: global irrigated area",
    subtitle = "FAOSTAT country totals vs sum of gridded irrigated cells",
    x = "Year", y = "Irrigated area (million ha)", colour = NULL
  )

ggsave(
  file.path(fig_dir, "val_irrigated_totals.png"),
  p6, width = 10, height = 5, dpi = val_dpi
)
cli::cli_alert_success("Saved val_irrigated_totals.png")

# ---- Figure 7: Country panel for large producers ----------------------

cli::cli_h2("Figure 7: Country panel for major producers")

# Use 12 largest countries for a detailed panel
top12 <- comp_country |>
  dplyr::filter(year == max(year)) |>
  dplyr::slice_max(ref_mha, n = 12) |>
  dplyr::pull(area_name)

panel_data <- comp_country |>
  dplyr::filter(area_name %in% top12) |>
  dplyr::select(year, area_name, ref_mha, grid_mha) |>
  tidyr::pivot_longer(
    cols = c(ref_mha, grid_mha),
    names_to = "source",
    values_to = "area_mha"
  ) |>
  dplyr::mutate(
    source = dplyr::if_else(
      source == "ref_mha", "FAOSTAT input", "Gridded output"
    ),
    area_name = factor(area_name, levels = top12)
  )

p7 <- ggplot(panel_data, aes(x = year, y = area_mha, colour = source)) +
  geom_line(linewidth = 0.5) +
  facet_wrap(~area_name, scales = "free_y", ncol = 3) +
  scale_colour_manual(
    values = c("FAOSTAT input" = "#2c7bb6", "Gridded output" = "#d7191c")
  ) +
  scale_x_continuous(breaks = seq(1850, 2020, 50)) +
  theme_minimal(base_size = 9) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold")
  ) +
  labs(
    title = "Validation: top 12 countries by harvested area",
    subtitle = "Blue = FAOSTAT input, Red = gridded output (overlap = perfect match)",
    x = "Year", y = "Area (million ha)", colour = NULL
  )

ggsave(
  file.path(fig_dir, "val_country_panel.png"),
  p7, width = 14, height = 10, dpi = val_dpi
)
cli::cli_alert_success("Saved val_country_panel.png")

# ---- Summary table output ---------------------------------------------

cli::cli_h2("Summary statistics")

# Write a CSV with per-country per-year validation stats
val_detail <- comp_country |>
  dplyr::filter(ref_mha > 0) |>
  dplyr::mutate(
    abs_diff_mha = grid_mha - ref_mha,
    rel_error_pct = abs_diff_mha / ref_mha * 100,
    irr_diff_mha = grid_irr_mha - ref_irr_mha,
    irr_rel_error_pct = dplyr::if_else(
      ref_irr_mha > 0.001,
      irr_diff_mha / ref_irr_mha * 100,
      NA_real_
    )
  ) |>
  dplyr::select(
    year, area_code, area_name, continent,
    ref_mha, grid_mha, abs_diff_mha, rel_error_pct,
    ref_irr_mha, grid_irr_mha, irr_diff_mha, irr_rel_error_pct
  ) |>
  dplyr::arrange(year, desc(abs(abs_diff_mha)))

readr::write_csv(
  val_detail,
  file.path(fig_dir, "validation_detail.csv")
)
cli::cli_alert_success(
  "Validation detail saved to {file.path(fig_dir, 'validation_detail.csv')}"
)

# Print quick summary
n_perfect <- sum(abs(val_detail$rel_error_pct) < 0.01, na.rm = TRUE)
n_total <- nrow(val_detail)
cli::cli_alert_info(
  "Perfect match (<0.01% error): {n_perfect}/{n_total} ({round(n_perfect/n_total*100,1)}%)"
)

cli::cli_h1("Validation complete — figures saved to {fig_dir}")
