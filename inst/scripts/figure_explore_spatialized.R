# -----------------------------------------------------------------------
# figure_explore_spatialized.R
#
# Generate exploratory maps and aggregated trend figures from the
# spatialized gridded landuse data produced by run_spatialize.R.
#
# Outputs saved to L_files/whep/figures/:
#   1. map_total_cropland_<year>.png  — global total cropland map
#   2. map_cft_panel_<year>.png       — faceted map by CFT
#   3. map_irrigated_share_<year>.png — irrigated share map
#   4. trend_global_area.png          — global rainfed/irrigated trends
#   5. trend_cft_area.png             — per-CFT area time series
#   6. trend_top_countries.png        — top-10 country area trends
#   7. trend_irrigated_share.png      — global irrigated share over time
#
# Requirements: ggplot2, sf, rnaturalearth, rnaturalearthdata,
#               patchwork, viridis, dplyr, tidyr, nanoparquet, readr,
#               cli, scales
# -----------------------------------------------------------------------

library(dplyr, warn.conflicts = FALSE)
library(ggplot2)
library(sf)
library(patchwork)
library(viridis)
library(tidyr)

devtools::load_all(".")

# ---- Configuration ---------------------------------------------------

l_files_dir <- "WHEP_L_FILES_DIR_PLACEHOLDER"
whep_dir <- file.path(l_files_dir, "whep")
fig_dir <- file.path(whep_dir, "figures")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

# Snapshot years for maps
map_years <- c(1900L, 1950L, 1980L, 2000L, 2020L)

# Map theme defaults
map_width <- 14
map_height <- 7
map_dpi <- 300

# ---- Load data --------------------------------------------------------

cli::cli_h1("Loading data")

gridded <- nanoparquet::read_parquet(
  file.path(whep_dir, "gridded_landuse.parquet")
)
cli::cli_alert_success(
  "gridded_landuse: {nrow(gridded)} rows, {dplyr::n_distinct(gridded$year)} years"
)

summary_tbl <- readr::read_csv(
  file.path(whep_dir, "landuse_summary.csv"),
  show_col_types = FALSE
)
cli::cli_alert_success(
  "landuse_summary: {nrow(summary_tbl)} rows"
)

# Country grid for aggregations
country_grid <- nanoparquet::read_parquet(
  file.path(whep_dir, "inputs", "country_grid.parquet")
)

# World basemap
world <- rnaturalearth::ne_countries(
  scale = "medium", returnclass = "sf"
) |>
  sf::st_transform("+proj=robin")

# ---- Helpers ----------------------------------------------------------

.make_raster_df <- function(data, value_col, year_val) {
  data |>
    dplyr::filter(year == year_val) |>
    dplyr::summarise(
      value = sum(.data[[value_col]], na.rm = TRUE),
      .by = c(lon, lat)
    ) |>
    dplyr::filter(value > 0)
}

.plot_global_map <- function(
  df,
  fill_label,
  title,
  log_scale = TRUE,
  palette = "viridis"
) {
  pts <- sf::st_as_sf(df, coords = c("lon", "lat"), crs = 4326) |>
    sf::st_transform("+proj=robin")

  p <- ggplot() +
    geom_sf(data = world, fill = "grey92", colour = "grey70", linewidth = 0.15) +
    geom_sf(
      data = pts, aes(colour = value),
      size = 0.15, alpha = 0.7
    ) +
    theme_minimal(base_size = 11) +
    theme(
      legend.position = "bottom",
      legend.key.width = unit(2.5, "cm"),
      panel.grid = element_line(colour = "grey95"),
      plot.title = element_text(face = "bold", size = 13)
    ) +
    labs(title = title, colour = fill_label)

  if (log_scale) {
    p <- p + scale_colour_viridis_c(
      option = palette, trans = "log10",
      labels = scales::label_comma(),
      na.value = "transparent"
    )
  } else {
    p <- p + scale_colour_viridis_c(
      option = palette,
      labels = scales::label_comma(),
      na.value = "transparent"
    )
  }

  p
}

# ---- Figure 1: Total cropland maps -----------------------------------

cli::cli_h2("Figure 1: Total cropland maps")

for (yr in map_years) {
  if (!yr %in% gridded$year) {
    cli::cli_alert_warning("Year {yr} not in data, skipping")
    next
  }

  df <- gridded |>
    dplyr::filter(year == yr) |>
    dplyr::summarise(
      value = sum(rainfed_ha + irrigated_ha, na.rm = TRUE),
      .by = c(lon, lat)
    ) |>
    dplyr::filter(value > 0)

  p <- .plot_global_map(
    df,
    fill_label = "Cropland (ha)",
    title = paste0("Total cropland area per grid cell — ", yr)
  )

  fpath <- file.path(fig_dir, paste0("map_total_cropland_", yr, ".png"))
  ggsave(fpath, p, width = map_width, height = map_height, dpi = map_dpi)
  cli::cli_alert_success("Saved {fpath}")
}

# ---- Figure 2: Faceted CFT map (year 2000) ----------------------------

cli::cli_h2("Figure 2: CFT panel map")

world_ll <- rnaturalearth::ne_countries(
  scale = "medium", returnclass = "sf"
)

for (yr in c(2000L, 2020L)) {
  if (!yr %in% gridded$year) next

  df_cft <- gridded |>
    dplyr::filter(year == yr, (rainfed_ha + irrigated_ha) > 0) |>
    dplyr::mutate(total_ha = rainfed_ha + irrigated_ha)

  p <- ggplot() +
    geom_sf(
      data = world_ll, fill = "grey92", colour = "grey70", linewidth = 0.1
    ) +
    geom_point(
      data = df_cft, aes(x = lon, y = lat, colour = total_ha),
      size = 0.05, alpha = 0.6
    ) +
    facet_wrap(~cft_name, ncol = 3) +
    scale_colour_viridis_c(
      option = "mako", trans = "log10",
      labels = scales::label_comma()
    ) +
    coord_sf(expand = FALSE) +
    theme_minimal(base_size = 9) +
    theme(
      legend.position = "bottom",
      legend.key.width = unit(2, "cm"),
      strip.text = element_text(face = "bold", size = 8),
      panel.grid = element_line(colour = "grey95")
    ) +
    labs(
      title = paste0("Crop functional type distribution — ", yr),
      colour = "Area (ha)"
    )

  fpath <- file.path(fig_dir, paste0("map_cft_panel_", yr, ".png"))
  ggsave(fpath, p, width = 16, height = 14, dpi = map_dpi)
  cli::cli_alert_success("Saved {fpath}")
}

# ---- Figure 3: Irrigated share map -----------------------------------

cli::cli_h2("Figure 3: Irrigated share map")

for (yr in c(1960L, 2000L, 2020L)) {
  if (!yr %in% gridded$year) next

  df_irr <- gridded |>
    dplyr::filter(year == yr) |>
    dplyr::summarise(
      total = sum(rainfed_ha + irrigated_ha, na.rm = TRUE),
      irrigated = sum(irrigated_ha, na.rm = TRUE),
      .by = c(lon, lat)
    ) |>
    dplyr::filter(total > 100) |>
    dplyr::mutate(value = irrigated / total)

  pts <- sf::st_as_sf(df_irr, coords = c("lon", "lat"), crs = 4326) |>
    sf::st_transform("+proj=robin")

  p <- ggplot() +
    geom_sf(
      data = world, fill = "grey92", colour = "grey70", linewidth = 0.15
    ) +
    geom_sf(
      data = pts, aes(colour = value),
      size = 0.15, alpha = 0.7
    ) +
    scale_colour_viridis_c(
      option = "inferno",
      labels = scales::label_percent(),
      limits = c(0, 1)
    ) +
    theme_minimal(base_size = 11) +
    theme(
      legend.position = "bottom",
      legend.key.width = unit(2.5, "cm"),
      panel.grid = element_line(colour = "grey95"),
      plot.title = element_text(face = "bold", size = 13)
    ) +
    labs(
      title = paste0("Irrigated share of harvested area — ", yr),
      colour = "Irrigated fraction"
    )

  fpath <- file.path(fig_dir, paste0("map_irrigated_share_", yr, ".png"))
  ggsave(fpath, p, width = map_width, height = map_height, dpi = map_dpi)
  cli::cli_alert_success("Saved {fpath}")
}

# ---- Figure 4: Global area trends ------------------------------------

cli::cli_h2("Figure 4: Global rainfed/irrigated trends")

global_trend <- summary_tbl |>
  dplyr::summarise(
    rainfed_mha = sum(total_rainfed_ha, na.rm = TRUE) / 1e6,
    irrigated_mha = sum(total_irrigated_ha, na.rm = TRUE) / 1e6,
    .by = year
  ) |>
  tidyr::pivot_longer(
    cols = c(rainfed_mha, irrigated_mha),
    names_to = "type",
    values_to = "area_mha"
  ) |>
  dplyr::mutate(
    type = dplyr::case_when(
      type == "rainfed_mha" ~ "Rainfed",
      type == "irrigated_mha" ~ "Irrigated"
    )
  )

p_global <- ggplot(global_trend, aes(x = year, y = area_mha, fill = type)) +
  geom_area(alpha = 0.8) +
  scale_fill_manual(
    values = c("Rainfed" = "#2c7bb6", "Irrigated" = "#d7191c")
  ) +
  scale_x_continuous(
    breaks = seq(1850, 2020, 25),
    expand = expansion(mult = 0.01)
  ) +
  scale_y_continuous(labels = scales::label_comma()) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  ) +
  labs(
    title = "Global harvested area (1850–2022)",
    x = "Year", y = "Area (million ha)", fill = NULL
  )

ggsave(
  file.path(fig_dir, "trend_global_area.png"),
  p_global, width = 10, height = 5, dpi = map_dpi
)
cli::cli_alert_success("Saved trend_global_area.png")

# ---- Figure 5: Per-CFT area trends -----------------------------------

cli::cli_h2("Figure 5: CFT area time series")

cft_trend <- summary_tbl |>
  dplyr::mutate(
    total_mha = (total_rainfed_ha + total_irrigated_ha) / 1e6
  )

p_cft <- ggplot(cft_trend, aes(x = year, y = total_mha, colour = cft_name)) +
  geom_line(linewidth = 0.6) +
  facet_wrap(~cft_name, scales = "free_y", ncol = 3) +
  scale_colour_viridis_d(option = "turbo") +
  scale_x_continuous(breaks = seq(1850, 2020, 50)) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 9),
    plot.title = element_text(face = "bold")
  ) +
  labs(
    title = "Harvested area by crop functional type (1850–2022)",
    x = "Year", y = "Area (million ha)"
  )

ggsave(
  file.path(fig_dir, "trend_cft_area.png"),
  p_cft, width = 14, height = 10, dpi = map_dpi
)
cli::cli_alert_success("Saved trend_cft_area.png")

# ---- Figure 6: Top-10 countries by total area (post-1961) ------------

cli::cli_h2("Figure 6: Top country trends")

polities <- whep::polities |>
  dplyr::select(area_code, area_name, continent) |>
  dplyr::distinct()

# Aggregate gridded to country-year
country_totals <- gridded |>
  dplyr::filter(year >= 1961L) |>
  dplyr::inner_join(country_grid, by = c("lon", "lat")) |>
  dplyr::summarise(
    total_mha = sum(rainfed_ha + irrigated_ha, na.rm = TRUE) / 1e6,
    .by = c(year, area_code)
  ) |>
  dplyr::inner_join(polities, by = "area_code")

# Identify top 10 countries by 2020 area (or latest year)
latest_yr <- max(country_totals$year)
top10 <- country_totals |>
  dplyr::filter(year == latest_yr) |>
  dplyr::slice_max(total_mha, n = 10) |>
  dplyr::pull(area_name)

p_countries <- country_totals |>
  dplyr::filter(area_name %in% top10) |>
  dplyr::mutate(
    area_name = factor(area_name, levels = top10)
  ) |>
  ggplot(aes(x = year, y = total_mha, colour = area_name)) +
  geom_line(linewidth = 0.7) +
  scale_colour_viridis_d(option = "turbo") +
  scale_x_continuous(breaks = seq(1960, 2020, 10)) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold")
  ) +
  labs(
    title = paste0("Top 10 countries by harvested area (1961–", latest_yr, ")"),
    x = "Year", y = "Total harvested area (million ha)",
    colour = "Country"
  )

ggsave(
  file.path(fig_dir, "trend_top_countries.png"),
  p_countries, width = 12, height = 6, dpi = map_dpi
)
cli::cli_alert_success("Saved trend_top_countries.png")

# ---- Figure 7: Global irrigated share trend ---------------------------

cli::cli_h2("Figure 7: Irrigated share trend")

irr_share <- summary_tbl |>
  dplyr::summarise(
    total = sum(total_rainfed_ha + total_irrigated_ha, na.rm = TRUE),
    irrigated = sum(total_irrigated_ha, na.rm = TRUE),
    .by = year
  ) |>
  dplyr::mutate(share = irrigated / total)

p_irr <- ggplot(irr_share, aes(x = year, y = share)) +
  geom_line(colour = "#d7191c", linewidth = 0.8) +
  geom_point(
    data = dplyr::filter(irr_share, year %% 20 == 0),
    colour = "#d7191c", size = 2
  ) +
  scale_y_continuous(
    labels = scales::label_percent(),
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_x_continuous(breaks = seq(1850, 2020, 25)) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold")) +
  labs(
    title = "Global irrigated share of harvested area (1850–2022)",
    x = "Year", y = "Irrigated share"
  )

ggsave(
  file.path(fig_dir, "trend_irrigated_share.png"),
  p_irr, width = 10, height = 5, dpi = map_dpi
)
cli::cli_alert_success("Saved trend_irrigated_share.png")

# ---- Figure 8: Continental stacked area ------------------------------

cli::cli_h2("Figure 8: Continental area trends")

continental <- gridded |>
  dplyr::filter(year >= 1850L) |>
  dplyr::inner_join(country_grid, by = c("lon", "lat")) |>
  dplyr::inner_join(
    dplyr::select(polities, area_code, continent),
    by = "area_code"
  ) |>
  dplyr::summarise(
    total_mha = sum(rainfed_ha + irrigated_ha, na.rm = TRUE) / 1e6,
    .by = c(year, continent)
  )

p_cont <- ggplot(
  continental,
  aes(x = year, y = total_mha, fill = continent)
) +
  geom_area(alpha = 0.8) +
  scale_fill_brewer(palette = "Set2") +
  scale_x_continuous(
    breaks = seq(1850, 2020, 25),
    expand = expansion(mult = 0.01)
  ) +
  scale_y_continuous(labels = scales::label_comma()) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  ) +
  labs(
    title = "Global harvested area by continent (1850–2022)",
    x = "Year", y = "Area (million ha)", fill = NULL
  )

ggsave(
  file.path(fig_dir, "trend_continental_area.png"),
  p_cont, width = 10, height = 5, dpi = map_dpi
)
cli::cli_alert_success("Saved trend_continental_area.png")

# ---- Done -------------------------------------------------------------

cli::cli_h1("All exploratory figures saved to {fig_dir}")
