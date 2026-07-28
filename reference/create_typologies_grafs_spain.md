# Typologies of Julia

Typologies of provinces in Spain based on nitrogen (N) production data
of crops and livestock, using various input datasets and generating
classification maps and data frames.

## Usage

``` r
create_typologies_grafs_spain(
  make_map = TRUE,
  shapefile_path = NULL,
  map_year = 1980
)
```

## Arguments

- make_map:

  If TRUE a map of the typologies will be created.

- shapefile_path:

  Optional path to a Natural Earth 10m admin-1 states/provinces
  shapefile. When `NULL` (default) the layer is downloaded from
  <https://www.naturalearthdata.com> on first use and cached locally;
  set `options(whep.provinces_shapefile = )` to point at an existing
  copy instead.

- map_year:

  The year for which the typology map is created.

## Value

A tibble with the classification of Spanish provinces into typologies.
It contains the following columns:

- `year`: The year in which the classification is performed.

- `province_name`: The name of the Spanish province.

- `livestock_density`: Livestock units (LU) per hectare of agricultural
  area (UAA). Reflects the intensity of animal farming.

- `productivity_kgN_ha`: Crop N productivity, in kilograms of N
  harvested per hectare of cropland.

- `semi_nat_share`: Share of total feed coming from semi-natural
  agroecosystems. Expressed between 0 and 1.

- `feed_imported_share`: Share of feed that is imported. Expressed
  between 0 and 1.

- `typology`: Assigned typology category for each province. This is
  based on thresholds in livestock density, crop productivity, and feed
  patterns. The typologies include: - `Specialized cropping system` -
  `Extensive cropping system` -
  `Extensive mixed crop-livestock system` -
  `Intensive mixed crop-livestock system` -
  `Specialized livestock-farming system`
