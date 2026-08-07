# Typologies of Julia

Typologies of provinces in Spain based on nitrogen (N) production data
of crops and livestock, using various input datasets and generating
classification maps and data frames.

## Usage

``` r
create_typologies_grafs_spain(
  make_map = TRUE,
  shapefile_path = NULL,
  map_year = 1980,
  example = FALSE
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

- example:

  If `TRUE`, return a small example output without reading the remote
  inputs or the Natural Earth layer. Default is `FALSE`.

## Value

A tibble with the classification of Spanish provinces into typologies
for `map_year`. It contains the following columns:

- `Province_name`: The name of the Spanish province.

- `Typologie`: Assigned typology category for each province. This is
  based on thresholds in livestock density (livestock units per hectare
  of agricultural area), crop N productivity (kg N harvested per hectare
  of cropland), and the semi-natural and imported shares of feed. The
  typologies are:

  - `Specialized cropping system`

  - `Extensive cropping system`

  - `Extensive mixed crop-livestock system`

  - `Intensive mixed crop-livestock system`

  - `Specialized livestock-farming system`

## Examples

``` r
create_typologies_grafs_spain(example = TRUE)
#> # A tibble: 10 × 2
#>    Province_name Typologie                           
#>    <chr>         <chr>                               
#>  1 Albacete      Extensive cropping system           
#>  2 Alicante      Extensive cropping system           
#>  3 Araba         Extensive cropping system           
#>  4 Asturias      Specialized livestock-farming system
#>  5 Avila         Extensive cropping system           
#>  6 Huelva        Extensive cropping system           
#>  7 Jaen          Extensive cropping system           
#>  8 Leon          Extensive cropping system           
#>  9 Lleida        Specialized livestock-farming system
#> 10 Teruel        Extensive cropping system           
```
