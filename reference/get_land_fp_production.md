# Get land footprint data for local production.

Read and clean the `land_fp` input file, returning only land-use entries
from local production (`Impact == "Land"` and `Origin == "Production"`).
Derived product rows from `land_fp` are excluded because they encode
land impacts on product output rather than physical land occupation;
processed products and animal products should inherit crop and grass
land through the input-output model.

## Usage

``` r
get_land_fp_production(
  example = FALSE,
  grassland_metric = c("occupation", "active_grazing", "both"),
  usable_grass_yield_dm_t_ha = 2.06
)
```

## Arguments

- example:

  If `TRUE`, return a small example output without downloading remote
  data. Default is `FALSE`.

- grassland_metric:

  Grassland land-extension metric. `"occupation"` returns the current
  LUH2 pasture+rangeland occupation area. `"active_grazing"` replaces
  grassland rows with the area required to supply modelled grazed grass
  intake at `usable_grass_yield_dm_t_ha`, capped by occupation area.
  `"both"` returns two scoped copies for comparing both metrics.

- usable_grass_yield_dm_t_ha:

  Usable grazed-grass dry-matter yield in tonnes per hectare. Only used
  when `grassland_metric` is `"active_grazing"` or `"both"`.

## Value

A tibble with columns:

- `year`: Year.

- `area_code`: Numeric area code.

- `item_cbs_code`: Commodity balance sheet item code.

- `impact`: Impact category.

- `element`: Source element.

- `origin`: Origin type.

- `group`: Group category.

- `impact_u`: Land footprint value (TODO: find which unit).

- `extension_scope`: Land-extension scope. This is `"occupation"` by
  default and distinguishes the two copies when
  `grassland_metric = "both"`.

## Examples

``` r
get_land_fp_production(example = TRUE)
#> # A tibble: 3 × 13
#>    year area_code polity_area_code reporting_polity_code reporting_polity_name
#>   <int>     <int>            <int> <chr>                 <chr>                
#> 1  2020        32               32 CMR-1961-2025         Cameroon             
#> 2  2020         9                9 ARG-1800-2025         Argentina            
#> 3  2020        32               32 CMR-1961-2025         Cameroon             
#> # ℹ 8 more variables: reporting_polity_has_geometry <lgl>, item_cbs_code <int>,
#> #   impact <chr>, element <chr>, origin <chr>, group <chr>, impact_u <dbl>,
#> #   extension_scope <chr>
```
