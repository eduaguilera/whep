# -----------------------------------------------------------------------
# figure_explore_livestock.R
#
# Exploratory maps and trend figures for spatialized livestock data.
# Produces 8 figures to L_files/whep/figures/.
#
# Prerequisites:
#   run_livestock_spatialize.R (gridded_livestock_emissions.parquet)
#
# Figures:
#   1. map_livestock_heads      — Total heads density (3 years)
#   2. map_species_panel        — Per-species panels (2020)
#   3. map_enteric_ch4          — Enteric CH4 density (3 years)
#   4. trend_global_heads       — Stacked area by species
#   5. trend_species_heads      — Per-species trends
#   6. trend_top_countries      — Top-10 countries by heads
#   7. trend_emissions          — CH4 + N2O totals over time
#   8. trend_continental_heads  — Continental breakdown
# -----------------------------------------------------------------------

library(dplyr, warn.conflicts = FALSE)
library(ggplot2)

# ==== Configuration =====================================================

l_files_dir <- "WHEP_L_FILES_DIR_PLACEHOLDER"
output_dir <- file.path(l_files_dir, "whep", "figures")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Plot config
map_width <- 14
map_height <- 7
trend_width <- 12
trend_height <- 7

# Nice species colors
species_colors <- c(
  "cattle" = "#d62728",
  "buffalo" = "#8c564b",
  "pigs" = "#ff7f0e",
  "sheep_goats" = "#2ca02c",
  "equines" = "#9467bd",
  "camels" = "#e377c2",
  "other" = "#7f7f7f"
)

# ==== Load data =========================================================

cli::cli_h1("Loading livestock data")

gridded <- nanoparquet::read_parquet(
  file.path(l_files_dir, "whep", "gridded_livestock_emissions.parquet")
)
cli::cli_alert_success(
  "gridded_livestock: {nrow(gridded)} rows, ",
  "{n_distinct(gridded$year)} years, ",
  "{n_distinct(gridded$species_group)} species"
)

summary_csv <- readr::read_csv(
  file.path(l_files_dir, "whep", "livestock_summary.csv"),
  show_col_types = FALSE
)

# Country grid for country-level aggregation
country_grid <- nanoparquet::read_parquet(
  file.path(l_files_dir, "whep", "inputs", "country_grid.parquet")
)

# Load world basemap
world <- rnaturalearth::ne_countries(
  scale = "medium", returnclass = "sf"
)

# ==== Helper function ===================================================

robinson_theme <- function() {
  theme_minimal(base_size = 12) +
    theme(
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      plot.title = element_text(hjust = 0.5, face = "bold")
    )
}

# ==== Figure 1: Total heads density maps ================================

cli::cli_h2("Figure 1: Total heads density maps")

for (yr in c(1900L, 1960L, 2020L)) {
  if (!yr %in% unique(gridded$year)) next

  map_data <- gridded |>
    filter(year == yr) |>
    summarise(
      total_heads = sum(heads, na.rm = TRUE),
      .by = c(lon, lat)
    ) |>
    mutate(
      log_heads = log10(pmax(total_heads, 1))
    )

  p <- ggplot() +
    geom_sf(
      data = world, fill = "grey92", color = "grey70", linewidth = 0.2
    ) +
    geom_point(
      data = map_data,
      aes(x = lon, y = lat, color = log_heads),
      size = 0.15, shape = 15
    ) +
    scale_color_viridis_c(
      name = expression(log[10] ~ "(heads)"),
      option = "inferno",
      limits = c(0, 7),
      na.value = "transparent"
    ) +
    coord_sf(
      crs = "+proj=robin", expand = FALSE,
      xlim = c(-170, 180), ylim = c(-60, 85)
    ) +
    labs(title = paste("Total livestock heads", yr)) +
    robinson_theme()

  fname <- file.path(
    output_dir, paste0("map_livestock_heads_", yr, ".png")
  )
  ggsave(fname, p, width = map_width, height = map_height, dpi = 200)
  cli::cli_alert_success("Saved {basename(fname)}")
}

# ==== Figure 2: Per-species panel map (2020) ============================

cli::cli_h2("Figure 2: Species panel map")

yr <- 2020L
if (yr %in% unique(gridded$year)) {
  panel_data <- gridded |>
    filter(year == yr, heads > 0) |>
    mutate(
      log_heads = log10(pmax(heads, 1))
    )

  # Order species by total heads
  sp_order <- panel_data |>
    summarise(tot = sum(heads), .by = species_group) |>
    arrange(desc(tot)) |>
    pull(species_group)

  panel_data <- panel_data |>
    mutate(
      species_group = factor(species_group, levels = sp_order)
    )

  p <- ggplot() +
    geom_sf(
      data = world, fill = "grey92", color = "grey70", linewidth = 0.1
    ) +
    geom_point(
      data = panel_data,
      aes(x = lon, y = lat, color = log_heads),
      size = 0.08, shape = 15
    ) +
    facet_wrap(~species_group, ncol = 4) +
    scale_color_viridis_c(
      name = expression(log[10] ~ "(heads)"),
      option = "inferno",
      limits = c(0, 7),
      na.value = "transparent"
    ) +
    coord_sf(
      crs = "+proj=robin", expand = FALSE,
      xlim = c(-170, 180), ylim = c(-60, 85)
    ) +
    labs(title = paste("Livestock distribution by species", yr)) +
    robinson_theme() +
    theme(
      strip.text = element_text(face = "bold", size = 10)
    )

  fname <- file.path(
    output_dir, paste0("map_species_panel_", yr, ".png")
  )
  ggsave(
    fname, p,
    width = map_width * 1.2, height = map_height * 1.0,
    dpi = 200
  )
  cli::cli_alert_success("Saved {basename(fname)}")
}

# ==== Figure 3: Enteric CH4 density maps ================================

cli::cli_h2("Figure 3: Enteric CH4 density maps")

for (yr in c(1960L, 2000L, 2020L)) {
  if (!yr %in% unique(gridded$year)) next

  map_data <- gridded |>
    filter(year == yr) |>
    summarise(
      total_ch4 = sum(enteric_ch4_kt, na.rm = TRUE),
      .by = c(lon, lat)
    ) |>
    filter(total_ch4 > 0) |>
    mutate(
      log_ch4 = log10(total_ch4)
    )

  p <- ggplot() +
    geom_sf(
      data = world, fill = "grey92", color = "grey70", linewidth = 0.2
    ) +
    geom_point(
      data = map_data,
      aes(x = lon, y = lat, color = log_ch4),
      size = 0.15, shape = 15
    ) +
    scale_color_viridis_c(
      name = expression(log[10] ~ "(kt CH"[4] * ")"),
      option = "magma",
      na.value = "transparent"
    ) +
    coord_sf(
      crs = "+proj=robin", expand = FALSE,
      xlim = c(-170, 180), ylim = c(-60, 85)
    ) +
    labs(title = paste("Enteric methane emissions", yr)) +
    robinson_theme()

  fname <- file.path(
    output_dir, paste0("map_enteric_ch4_", yr, ".png")
  )
  ggsave(fname, p, width = map_width, height = map_height, dpi = 200)
  cli::cli_alert_success("Saved {basename(fname)}")
}

# ==== Figure 4: Global heads stacked area ===============================

cli::cli_h2("Figure 4: Global heads stacked area")

trend_data <- summary_csv |>
  mutate(
    species_group = factor(
      species_group,
      levels = rev(names(species_colors))
    ),
    heads_B = total_heads / 1e9
  )

p <- ggplot(
  trend_data,
  aes(x = year, y = heads_B, fill = species_group)
) +
  geom_area(position = "stack", alpha = 0.85) +
  scale_fill_manual(
    values = species_colors,
    name = "Species"
  ) +
  scale_x_continuous(
    breaks = seq(1850, 2020, 20),
    expand = c(0.01, 0)
  ) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(
    title = "Global livestock population (1850\u20132022)",
    x = "Year",
    y = "Heads (billions)"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "right")

fname <- file.path(output_dir, "trend_global_livestock.png")
ggsave(fname, p, width = trend_width, height = trend_height, dpi = 200)
cli::cli_alert_success("Saved {basename(fname)}")

# ==== Figure 5: Per-species trends ======================================

cli::cli_h2("Figure 5: Per-species trends")

p <- ggplot(
  summary_csv |>
    mutate(heads_M = total_heads / 1e6),
  aes(x = year, y = heads_M, color = species_group)
) +
  geom_line(linewidth = 0.9) +
  scale_color_manual(values = species_colors, name = "Species") +
  facet_wrap(
    ~species_group, scales = "free_y", ncol = 4
  ) +
  scale_x_continuous(breaks = seq(1850, 2020, 40)) +
  labs(
    title = "Livestock population by species group",
    x = "Year",
    y = "Heads (millions)"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

fname <- file.path(output_dir, "trend_species_heads.png")
ggsave(
  fname, p,
  width = trend_width * 1.1, height = trend_height * 0.8,
  dpi = 200
)
cli::cli_alert_success("Saved {basename(fname)}")

# ==== Figure 6: Top-10 countries by heads (2020) ========================

cli::cli_h2("Figure 6: Top countries by heads")

devtools::load_all(".")
polities <- whep::polities |>
  select(area_code, area_name) |>
  distinct()

yr_range <- c(1961L, max(gridded$year))
country_heads <- gridded |>
  filter(year >= yr_range[1]) |>
  inner_join(country_grid, by = c("lon", "lat")) |>
  summarise(
    total_heads = sum(heads, na.rm = TRUE),
    .by = c(year, area_code)
  ) |>
  inner_join(polities, by = "area_code")

# Top 10 by 2020
latest_yr <- max(country_heads$year)
top10 <- country_heads |>
  filter(year == latest_yr) |>
  arrange(desc(total_heads)) |>
  head(10) |>
  pull(area_name)

country_top <- country_heads |>
  filter(area_name %in% top10) |>
  mutate(
    area_name = factor(area_name, levels = top10),
    heads_M = total_heads / 1e6
  )

p <- ggplot(
  country_top,
  aes(x = year, y = heads_M, color = area_name)
) +
  geom_line(linewidth = 0.9) +
  scale_x_continuous(breaks = seq(1960, 2020, 10)) +
  labs(
    title = paste0(
      "Top 10 countries by livestock population (",
      yr_range[1], "\u2013", latest_yr, ")"
    ),
    x = "Year",
    y = "Heads (millions)",
    color = "Country"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "right")

fname <- file.path(output_dir, "trend_top_livestock_countries.png")
ggsave(fname, p, width = trend_width, height = trend_height, dpi = 200)
cli::cli_alert_success("Saved {basename(fname)}")

# ==== Figure 7: Emissions trends ========================================

cli::cli_h2("Figure 7: Emissions trends")

emission_data <- summary_csv |>
  summarise(
    enteric_ch4_Mt = sum(enteric_ch4_kt, na.rm = TRUE) / 1e3,
    manure_ch4_Mt = sum(manure_ch4_kt, na.rm = TRUE) / 1e3,
    manure_n2o_kt = sum(manure_n2o_kt, na.rm = TRUE),
    manure_n_Tg = sum(manure_n_mg, na.rm = TRUE) / 1e6,
    .by = year
  )

# Combine CH4 sources
ch4_long <- emission_data |>
  select(year, enteric_ch4_Mt, manure_ch4_Mt) |>
  tidyr::pivot_longer(
    cols = c(enteric_ch4_Mt, manure_ch4_Mt),
    names_to = "source",
    values_to = "Mt_CH4"
  ) |>
  mutate(
    source = dplyr::case_match(
      source,
      "enteric_ch4_Mt" ~ "Enteric fermentation",
      "manure_ch4_Mt" ~ "Manure management"
    )
  )

p1 <- ggplot(
  ch4_long,
  aes(x = year, y = Mt_CH4, fill = source)
) +
  geom_area(position = "stack", alpha = 0.85) +
  scale_fill_manual(
    values = c(
      "Enteric fermentation" = "#d62728",
      "Manure management" = "#ff9896"
    ),
    name = ""
  ) +
  scale_x_continuous(
    breaks = seq(1850, 2020, 20),
    expand = c(0.01, 0)
  ) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(
    title = expression("Livestock CH"[4] ~ "emissions"),
    x = "Year",
    y = expression("Mt CH"[4])
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

p2 <- ggplot(
  emission_data,
  aes(x = year, y = manure_n_Tg)
) +
  geom_line(color = "#1f77b4", linewidth = 0.9) +
  geom_area(fill = "#1f77b4", alpha = 0.3) +
  scale_x_continuous(
    breaks = seq(1850, 2020, 20),
    expand = c(0.01, 0)
  ) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(
    title = "Manure nitrogen excretion",
    x = "Year",
    y = "Tg N"
  ) +
  theme_minimal(base_size = 13)

p_combined <- patchwork::wrap_plots(p1, p2, ncol = 2)
fname <- file.path(output_dir, "trend_livestock_emissions.png")
ggsave(
  fname, p_combined,
  width = trend_width * 1.2, height = trend_height * 0.7,
  dpi = 200
)
cli::cli_alert_success("Saved {basename(fname)}")

# ==== Figure 8: Continental breakdown ==================================

cli::cli_h2("Figure 8: Continental heads breakdown")

# Use whep::polities which already has continent column
polity_continent <- whep::polities |>
  select(area_code, continent) |>
  distinct() |>
  filter(!is.na(continent))

# Merge country heads with continents
country_continent <- country_heads |>
  inner_join(polity_continent, by = "area_code") |>
  filter(!is.na(continent))

continental <- country_continent |>
  summarise(
    heads_B = sum(total_heads, na.rm = TRUE) / 1e9,
    .by = c(year, continent)
  )

p <- ggplot(
  continental,
  aes(x = year, y = heads_B, fill = continent)
) +
  geom_area(position = "stack", alpha = 0.85) +
  scale_fill_brewer(palette = "Set1", name = "Continent") +
  scale_x_continuous(
    breaks = seq(1960, 2020, 10),
    expand = c(0.01, 0)
  ) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(
    title = "Livestock population by continent",
    x = "Year",
    y = "Heads (billions)"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "right")

fname <- file.path(output_dir, "trend_continental_livestock.png")
ggsave(fname, p, width = trend_width, height = trend_height, dpi = 200)
cli::cli_alert_success("Saved {basename(fname)}")

# ==== Done ==============================================================

cli::cli_h1("Livestock exploration figures complete")
cli::cli_alert_success(
  "All figures saved to {output_dir}"
)
