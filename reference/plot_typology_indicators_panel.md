# Plot four N indicators as time series per typology

Creates a four-panel figure showing the temporal evolution of external N
dependency, Finn Cycling Index, pollution (soil + livestock surplus per
ha), and intensification (synthetic + feed imports per ha) as mean lines
per typology with interquartile ribbons.

## Usage

``` r
plot_typology_indicators_panel(
  finn_data = NULL,
  n_prov_destiny = NULL,
  area_df = NULL,
  typo_df = NULL
)
```

## Arguments

- finn_data:

  Pre-computed tibble from
  [`create_finn_indicator()`](https://eduaguilera.github.io/whep/reference/create_finn_indicator.md).
  If `NULL`, computed automatically (slow).

- n_prov_destiny:

  Nitrogen flows tibble from
  [`create_n_prov_destiny()`](https://eduaguilera.github.io/whep/reference/create_n_prov_destiny.md).
  If `NULL`, loaded automatically.

- area_df:

  Cropland area per year and province, with columns `year`,
  `province_name` and `area_ha`. If `NULL`, read from the `npp_ygpit`
  pin.

- typo_df:

  Typology assignment per year and province, with columns `year`,
  `province_name` and `Typology_base`. If `NULL`, derived from
  `create_typo_ts_plot()`.

## Value

A patchwork ggplot object.

## Examples

``` r
# Two provinces at two dates is enough to exercise the four panels; the
# real figure spans 50 provinces and 1860-2023.
flows <- tibble::tribble(
  ~year, ~province_name, ~box, ~origin, ~destiny, ~mg_n,
  1960, "A", "Cropland", "Synthetic", "Cropland", 900,
  1960, "A", "Cropland", "Outside", "livestock_mono", 300,
  1960, "A", "Cropland", "Cropland", "population_food", 500,
  1960, "B", "Cropland", "Synthetic", "Cropland", 400,
  1960, "B", "Cropland", "Outside", "livestock_mono", 100,
  2000, "A", "Cropland", "Synthetic", "Cropland", 2600,
  2000, "A", "Cropland", "Outside", "livestock_mono", 1800,
  2000, "A", "Cropland", "Cropland", "population_food", 700,
  2000, "B", "Cropland", "Synthetic", "Cropland", 900,
  2000, "B", "Cropland", "Outside", "livestock_mono", 500
)
area_df <- tibble::tribble(
  ~year, ~province_name, ~area_ha,
  1960, "A", 10000,
  1960, "B", 8000,
  2000, "A", 9000,
  2000, "B", 7000
)
typo_df <- tibble::tribble(
  ~year, ~province_name, ~Typology_base,
  1960, "A", "Specialized cropping systems",
  1960, "B", "Semi-natural agroecosystems",
  2000, "A", "Specialized cropping systems",
  2000, "B", "Semi-natural agroecosystems"
)
finn_data <- tibble::tribble(
  ~year, ~province_name, ~finn_index,
  1960, "A", 0.12,
  1960, "B", 0.18,
  2000, "A", 0.07,
  2000, "B", 0.09
)
panel <- plot_typology_indicators_panel(
  finn_data = finn_data,
  n_prov_destiny = flows,
  area_df = area_df,
  typo_df = typo_df
)
```
