# -----------------------------------------------------------------------
# figure_validate_livestock.R
#
# Validation figures comparing gridded livestock totals against
# the FAOSTAT country-level input data. Produces 7 figures + CSV.
#
# Prerequisites:
#   - run_livestock_spatialize.R (gridded_livestock_emissions.parquet)
#   - prepare_livestock_spatialize_inputs.R (livestock_country_data.parquet)
#
# Figures:
#   1. val_livestock_global_totals     — FAOSTAT vs gridded heads overlay
#   2. val_livestock_species_totals    — Per-species comparison
#   3. val_livestock_scatter_country   — 1:1 scatter (4 years)
#   4. val_livestock_error_distribution — Histogram of relative errors
#   5. val_livestock_top_deviations    — Worst-case time series
#   6. val_livestock_emissions_totals  — CH4 emission validation
#   7. val_livestock_country_panel     — Top-12 country panels
#   + validation_livestock_detail.csv
# -----------------------------------------------------------------------

library(dplyr, warn.conflicts = FALSE)
library(ggplot2)

devtools::load_all(".")

# ==== Configuration =====================================================

l_files_dir <- "C:/Users/53272530E/OneDrive/L_files"
input_dir <- file.path(l_files_dir, "whep", "inputs")
output_dir <- file.path(l_files_dir, "whep", "figures")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

map_width <- 14
map_height <- 7
trend_width <- 12
trend_height <- 7

# ==== Load data =========================================================

cli::cli_h1("Loading data for validation")

# Gridded livestock
gridded <- nanoparquet::read_parquet(
  file.path(l_files_dir, "whep", "gridded_livestock_emissions.parquet")
)

# Country grid
country_grid <- nanoparquet::read_parquet(
  file.path(input_dir, "country_grid.parquet")
)

# Original FAOSTAT-based country data (the input to spatialization)
faostat <- nanoparquet::read_parquet(
  file.path(input_dir, "livestock_country_data.parquet")
)

# Polities for names
polities <- whep::polities |>
  select(area_code, area_name) |>
  distinct()

cli::cli_alert_success(
  "Loaded: gridded={nrow(gridded)} rows, ",
  "country_input={nrow(faostat)} rows"
)

# ==== Aggregate gridded to country level ================================

cli::cli_h2("Aggregating gridded data to country level")

gridded_by_country <- gridded |>
  inner_join(country_grid, by = c("lon", "lat")) |>
  summarise(
    grid_heads = sum(heads, na.rm = TRUE),
    grid_enteric_ch4 = sum(enteric_ch4_kt, na.rm = TRUE),
    grid_manure_ch4 = sum(manure_ch4_kt, na.rm = TRUE),
    grid_manure_n2o = sum(manure_n2o_kt, na.rm = TRUE),
    grid_manure_n = sum(manure_n_mg, na.rm = TRUE),
    .by = c(year, area_code, species_group)
  )

# Match with FAOSTAT input
faostat_totals <- faostat |>
  transmute(
    year,
    area_code,
    species_group,
    fao_heads = heads,
    fao_enteric_ch4 = enteric_ch4_kt,
    fao_manure_ch4 = manure_ch4_kt,
    fao_manure_n2o = manure_n2o_kt,
    fao_manure_n = manure_n_mg
  )

comparison <- gridded_by_country |>
  inner_join(
    faostat_totals,
    by = c("year", "area_code", "species_group")
  ) |>
  mutate(
    heads_diff = grid_heads - fao_heads,
    heads_rel_error = if_else(
      fao_heads > 0,
      abs(grid_heads - fao_heads) / fao_heads * 100,
      0
    )
  ) |>
  inner_join(polities, by = "area_code")

cli::cli_alert_success(
  "Matched {nrow(comparison)} country-species-year observations"
)

# Summary
n_perfect <- sum(comparison$heads_rel_error < 0.01)
n_good <- sum(comparison$heads_rel_error < 1)
n_total <- nrow(comparison)
cli::cli_alert_info(
  "Perfect match (<0.01%): {n_perfect}/{n_total} ({round(100*n_perfect/n_total,1)}%)"
)
cli::cli_alert_info(
  "Good match (<1%): {n_good}/{n_total} ({round(100*n_good/n_total,1)}%)"
)

# ==== Save validation CSV ===============================================

csv_path <- file.path(output_dir, "validation_livestock_detail.csv")
readr::write_csv(comparison, csv_path)
cli::cli_alert_success("Saved {basename(csv_path)}")

# ==== Figure 1: Global totals ==========================================

cli::cli_h2("Figure 1: Global totals comparison")

global_fao <- faostat |>
  summarise(
    fao_heads_B = sum(heads, na.rm = TRUE) / 1e9,
    .by = year
  )

global_grid <- gridded |>
  summarise(
    grid_heads_B = sum(heads, na.rm = TRUE) / 1e9,
    .by = year
  )

global_comp <- inner_join(global_fao, global_grid, by = "year") |>
  tidyr::pivot_longer(
    cols = c(fao_heads_B, grid_heads_B),
    names_to = "source",
    values_to = "heads_B"
  ) |>
  mutate(
    source = case_match(
      source,
      "fao_heads_B" ~ "Country input",
      "grid_heads_B" ~ "Gridded"
    )
  )

p <- ggplot(
  global_comp,
  aes(x = year, y = heads_B, color = source, linetype = source)
) +
  geom_line(linewidth = 1.1) +
  scale_color_manual(
    values = c("Country input" = "#1f77b4", "Gridded" = "#d62728"),
    name = ""
  ) +
  scale_linetype_manual(
    values = c("Country input" = "solid", "Gridded" = "dashed"),
    name = ""
  ) +
  scale_x_continuous(breaks = seq(1850, 2020, 20)) +
  labs(
    title = "Global livestock: country input vs gridded totals",
    x = "Year",
    y = "Heads (billions)"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")

fname <- file.path(output_dir, "val_livestock_global_totals.png")
ggsave(fname, p, width = trend_width, height = trend_height, dpi = 200)
cli::cli_alert_success("Saved {basename(fname)}")

# ==== Figure 2: Per-species totals =====================================

cli::cli_h2("Figure 2: Per-species comparison")

species_comp <- comparison |>
  summarise(
    fao = sum(fao_heads, na.rm = TRUE) / 1e9,
    gridded = sum(grid_heads, na.rm = TRUE) / 1e9,
    .by = c(year, species_group)
  ) |>
  tidyr::pivot_longer(
    cols = c(fao, gridded),
    names_to = "source",
    values_to = "heads_B"
  )

p <- ggplot(
  species_comp,
  aes(x = year, y = heads_B, color = source, linetype = source)
) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~species_group, scales = "free_y") +
  scale_color_manual(
    values = c("fao" = "#1f77b4", "gridded" = "#d62728"),
    name = "",
    labels = c("fao" = "Country input", "gridded" = "Gridded")
  ) +
  scale_linetype_manual(
    values = c("fao" = "solid", "gridded" = "dashed"),
    name = "",
    labels = c("fao" = "Country input", "gridded" = "Gridded")
  ) +
  scale_x_continuous(breaks = seq(1850, 2020, 40)) +
  labs(
    title = "Livestock by species: country input vs gridded",
    x = "Year",
    y = "Heads (billions)"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

fname <- file.path(output_dir, "val_livestock_species_totals.png")
ggsave(
  fname, p,
  width = trend_width * 1.1, height = trend_height,
  dpi = 200
)
cli::cli_alert_success("Saved {basename(fname)}")

# ==== Figure 3: Country scatter (4 years) ===============================

cli::cli_h2("Figure 3: Country 1:1 scatter")

scatter_years <- c(1960L, 1980L, 2000L, 2020L)

# Aggregate across species for country totals
country_scatter <- comparison |>
  summarise(
    fao_M = sum(fao_heads, na.rm = TRUE) / 1e6,
    grid_M = sum(grid_heads, na.rm = TRUE) / 1e6,
    .by = c(year, area_code, area_name)
  )

for (yr in scatter_years) {
  if (!yr %in% unique(country_scatter$year)) next

  yr_data <- country_scatter |>
    filter(year == yr, fao_M > 0)

  # Label top deviations
  yr_data <- yr_data |>
    mutate(
      rel_err = abs(grid_M - fao_M) / fao_M * 100,
      label = if_else(
        rank(-rel_err) <= 8 | rank(-fao_M) <= 5,
        area_name, NA_character_
      )
    )

  p <- ggplot(yr_data, aes(x = fao_M, y = grid_M)) +
    geom_abline(
      slope = 1, intercept = 0,
      linetype = "dashed", color = "grey50"
    ) +
    geom_point(
      aes(size = fao_M),
      alpha = 0.6, color = "#2ca02c"
    ) +
    ggrepel::geom_text_repel(
      aes(label = label),
      size = 3, max.overlaps = 15,
      segment.color = "grey60"
    ) +
    scale_x_log10() +
    scale_y_log10() +
    labs(
      title = paste("Country livestock validation", yr),
      x = "Country input (million heads)",
      y = "Gridded total (million heads)"
    ) +
    theme_minimal(base_size = 13) +
    theme(legend.position = "none")

  fname <- file.path(
    output_dir, paste0("val_livestock_scatter_country_", yr, ".png")
  )
  ggsave(fname, p, width = 9, height = 8, dpi = 200)
  cli::cli_alert_success("Saved {basename(fname)}")
}

# ==== Figure 4: Relative error distribution =============================

cli::cli_h2("Figure 4: Relative error distribution")

# Per country-year (across species)
error_data <- comparison |>
  filter(fao_heads > 100) |>
  summarise(
    fao = sum(fao_heads),
    grid = sum(grid_heads),
    .by = c(year, area_code, area_name)
  ) |>
  mutate(rel_error = (grid - fao) / fao * 100)

p <- ggplot(error_data, aes(x = rel_error)) +
  geom_histogram(
    bins = 100, fill = "#1f77b4", alpha = 0.7, color = "white"
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  coord_cartesian(xlim = c(-5, 5)) +
  labs(
    title = "Distribution of relative errors (gridded vs country input)",
    subtitle = paste0(
      nrow(error_data), " country-year observations, ",
      round(100 * mean(abs(error_data$rel_error) < 0.01), 1),
      "% within \u00b10.01%"
    ),
    x = "Relative error (%)",
    y = "Count"
  ) +
  theme_minimal(base_size = 14)

fname <- file.path(
  output_dir, "val_livestock_error_distribution.png"
)
ggsave(fname, p, width = trend_width, height = trend_height * 0.8, dpi = 200)
cli::cli_alert_success("Saved {basename(fname)}")

# ==== Figure 5: Top deviations time series ==============================

cli::cli_h2("Figure 5: Worst-case country time series")

# Find countries with largest errors
worst <- error_data |>
  group_by(area_code, area_name) |>
  summarise(
    max_err = max(abs(rel_error), na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(max_err)) |>
  head(12)

if (nrow(worst) > 0) {
  worst_ts <- comparison |>
    filter(area_code %in% worst$area_code) |>
    summarise(
      fao_M = sum(fao_heads) / 1e6,
      grid_M = sum(grid_heads) / 1e6,
      .by = c(year, area_code, area_name)
    )

  worst_long <- worst_ts |>
    tidyr::pivot_longer(
      cols = c(fao_M, grid_M),
      names_to = "source",
      values_to = "heads_M"
    ) |>
    mutate(
      source = case_match(
        source,
        "fao_M" ~ "Country input",
        "grid_M" ~ "Gridded"
      )
    )

  p <- ggplot(
    worst_long,
    aes(x = year, y = heads_M, color = source, linetype = source)
  ) +
    geom_line(linewidth = 0.7) +
    facet_wrap(~area_name, scales = "free_y", ncol = 4) +
    scale_color_manual(
      values = c("Country input" = "#1f77b4", "Gridded" = "#d62728"),
      name = ""
    ) +
    scale_linetype_manual(
      values = c("Country input" = "solid", "Gridded" = "dashed"),
      name = ""
    ) +
    labs(
      title = "Countries with largest relative errors",
      x = "Year",
      y = "Heads (millions)"
    ) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom")

  fname <- file.path(
    output_dir, "val_livestock_top_deviations.png"
  )
  ggsave(
    fname, p,
    width = trend_width * 1.1, height = trend_height * 1.0,
    dpi = 200
  )
  cli::cli_alert_success("Saved {basename(fname)}")
}

# ==== Figure 6: Emissions validation ====================================

cli::cli_h2("Figure 6: Emissions validation")

emissions_comp <- comparison |>
  summarise(
    fao_ch4 = sum(fao_enteric_ch4, na.rm = TRUE) / 1e3,
    grid_ch4 = sum(grid_enteric_ch4, na.rm = TRUE) / 1e3,
    fao_n_Tg = sum(fao_manure_n, na.rm = TRUE) / 1e6,
    grid_n_Tg = sum(grid_manure_n, na.rm = TRUE) / 1e6,
    .by = year
  )

em_long <- emissions_comp |>
  tidyr::pivot_longer(
    cols = c(fao_ch4, grid_ch4),
    names_to = "source",
    values_to = "Mt_CH4"
  ) |>
  mutate(
    source = case_match(
      source,
      "fao_ch4" ~ "Country input",
      "grid_ch4" ~ "Gridded"
    )
  )

p1 <- ggplot(
  em_long,
  aes(x = year, y = Mt_CH4, color = source, linetype = source)
) +
  geom_line(linewidth = 1.0) +
  scale_color_manual(
    values = c("Country input" = "#1f77b4", "Gridded" = "#d62728"),
    name = ""
  ) +
  scale_linetype_manual(
    values = c("Country input" = "solid", "Gridded" = "dashed"),
    name = ""
  ) +
  labs(
    title = expression("Enteric CH"[4] ~ " validation"),
    x = "Year",
    y = expression("Mt CH"[4])
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

n_long <- emissions_comp |>
  tidyr::pivot_longer(
    cols = c(fao_n_Tg, grid_n_Tg),
    names_to = "source",
    values_to = "Tg_N"
  ) |>
  mutate(
    source = case_match(
      source,
      "fao_n_Tg" ~ "Country input",
      "grid_n_Tg" ~ "Gridded"
    )
  )

p2 <- ggplot(
  n_long,
  aes(x = year, y = Tg_N, color = source, linetype = source)
) +
  geom_line(linewidth = 1.0) +
  scale_color_manual(
    values = c("Country input" = "#1f77b4", "Gridded" = "#d62728"),
    name = ""
  ) +
  scale_linetype_manual(
    values = c("Country input" = "solid", "Gridded" = "dashed"),
    name = ""
  ) +
  labs(
    title = "Manure N validation",
    x = "Year",
    y = "Tg N"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

p_combined <- patchwork::wrap_plots(p1, p2, ncol = 2)
fname <- file.path(output_dir, "val_livestock_emissions_totals.png")
ggsave(
  fname, p_combined,
  width = trend_width * 1.2, height = trend_height * 0.7,
  dpi = 200
)
cli::cli_alert_success("Saved {basename(fname)}")

# ==== Figure 7: Top-12 country panels ==================================

cli::cli_h2("Figure 7: Country panels (top 12)")

latest_yr <- max(comparison$year)
top12 <- comparison |>
  filter(year == latest_yr) |>
  summarise(
    total = sum(fao_heads, na.rm = TRUE),
    .by = c(area_code, area_name)
  ) |>
  arrange(desc(total)) |>
  head(12)

panel_data <- comparison |>
  filter(area_code %in% top12$area_code) |>
  summarise(
    fao_M = sum(fao_heads) / 1e6,
    grid_M = sum(grid_heads) / 1e6,
    .by = c(year, area_code, area_name)
  ) |>
  mutate(
    area_name = factor(area_name, levels = top12$area_name)
  ) |>
  tidyr::pivot_longer(
    cols = c(fao_M, grid_M),
    names_to = "source",
    values_to = "heads_M"
  ) |>
  mutate(
    source = case_match(
      source,
      "fao_M" ~ "Country input",
      "grid_M" ~ "Gridded"
    )
  )

p <- ggplot(
  panel_data,
  aes(x = year, y = heads_M, color = source, linetype = source)
) +
  geom_line(linewidth = 0.7) +
  facet_wrap(~area_name, scales = "free_y", ncol = 4) +
  scale_color_manual(
    values = c("Country input" = "#1f77b4", "Gridded" = "#d62728"),
    name = ""
  ) +
  scale_linetype_manual(
    values = c("Country input" = "solid", "Gridded" = "dashed"),
    name = ""
  ) +
  labs(
    title = "Top 12 livestock countries: input vs gridded",
    x = "Year",
    y = "Heads (millions)"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

fname <- file.path(output_dir, "val_livestock_country_panel.png")
ggsave(
  fname, p,
  width = trend_width * 1.1, height = trend_height * 1.0,
  dpi = 200
)
cli::cli_alert_success("Saved {basename(fname)}")

# ==== Done ==============================================================

cli::cli_h1("Livestock validation complete")
cli::cli_alert_success(
  "{n_total} observations: {round(100*n_perfect/n_total,1)}% perfect match"
)
cli::cli_alert_success(
  "All figures saved to {output_dir}"
)
