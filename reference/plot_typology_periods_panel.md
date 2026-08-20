# Plot four N indicators as period comparisons per typology

Creates a four-panel figure comparing external N dependency, Finn
Cycling Index, pollution (soil + livestock surplus per ha), and
intensification (synthetic + feed imports per ha) across four reference
periods (1860-1870, 1920-1930, 1960-1970, 2010-2020), analogous to the
periods panel from
[`plot_finn_circularity()`](https://eduaguilera.github.io/whep/reference/plot_finn_circularity.md).
Each panel facets by typology and adds a "Spain (national)" facet
computed from the national GRAFS dataset
([`create_n_nat_destiny()`](https://eduaguilera.github.io/whep/reference/create_n_nat_destiny.md)),
shown as a single black point per period since there is only one
national observation per period.

## Usage

``` r
plot_typology_periods_panel(
  finn_data = NULL,
  n_prov_destiny = NULL,
  n_nat_destiny = NULL,
  panel_data = NULL
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

- n_nat_destiny:

  National nitrogen flows tibble from
  [`create_n_nat_destiny()`](https://eduaguilera.github.io/whep/reference/create_n_nat_destiny.md).
  If `NULL`, computed automatically (slow).

- panel_data:

  Named list overriding the two frames otherwise read from pins:
  `area_df` (`year`, `province_name`, `area_ha`) and `typo_df` (`year`,
  `province_name`, `Typology_base`). Missing elements are loaded
  automatically.

## Value

A patchwork ggplot object.

## Examples

``` r
# The four reference periods are 1860-1870, 1920-1930, 1960-1970 and
# 2010-2020, so an example needs at least one year inside two of them.
flows <- tibble::tribble(
  ~year, ~province_name, ~box, ~origin, ~destiny, ~mg_n,
  1865, "A", "Cropland", "Synthetic", "Cropland", 900,
  1865, "A", "Cropland", "Outside", "livestock_mono", 300,
  1865, "B", "Cropland", "Synthetic", "Cropland", 400,
  1965, "A", "Cropland", "Synthetic", "Cropland", 2600,
  1965, "A", "Cropland", "Outside", "livestock_mono", 1800,
  1965, "B", "Cropland", "Synthetic", "Cropland", 900
)
panel_data <- list(
  area_df = tibble::tribble(
    ~year, ~province_name, ~area_ha,
    1865, "A", 10000,
    1865, "B", 8000,
    1965, "A", 9000,
    1965, "B", 7000
  ),
  typo_df = tibble::tribble(
    ~year, ~province_name, ~Typology_base,
    1865, "A", "Specialized cropping systems",
    1865, "B", "Semi-natural agroecosystems",
    1965, "A", "Specialized cropping systems",
    1965, "B", "Semi-natural agroecosystems"
  )
)
finn_data <- tibble::tribble(
  ~year, ~province_name, ~finn_index,
  1865, "A", 0.12,
  1865, "B", 0.18,
  1965, "A", 0.07,
  1965, "B", 0.09
)
panel <- plot_typology_periods_panel(
  finn_data = finn_data,
  n_prov_destiny = flows,
  n_nat_destiny = flows,
  panel_data = panel_data
)
```
