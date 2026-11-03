# Read native reporting support without renormalizing its physical domain

Read native reporting support without renormalizing its physical domain

## Usage

``` r
read_reporting_support(
  bundle,
  role = c("partition", "footprints"),
  require_layers = TRUE
)
```

## Arguments

- bundle:

  A bundle returned by
  [`build_reporting_support()`](https://eduaguilera.github.io/whep/reference/build_reporting_support.md),
  or the path to a complete bundle saved with
  [`saveRDS()`](https://rdrr.io/r/base/readRDS.html). The canonical
  geometries, inventory and domain remain together in that artifact.

- role:

  Select the disjoint reporting `partition` or all inventory
  `footprints`, including explicitly excluded alternatives. Footprints
  are marked as overlap and must not be summed as a physical partition.

- require_layers:

  Require supplied water and ice, as in
  [`read_polycell_support()`](https://eduaguilera.github.io/whep/reference/read_polycell_support.md).
  Set `FALSE` explicitly for geometry-only study.

## Value

A tibble carrying `domain_land_area_ha` alongside polity areas.

## Examples

``` r
if (requireNamespace("sf", quietly = TRUE)) {
  g <- polycell_example_geometries()
  d <- sf::st_sf(domain_source = "example", year = 2010L,
                  geometry = sf::st_union(g))
  i <- tibble::tibble(source = "example", year = 2010L,
                     reporting_polity_code = g$polity_code)
  b <- suppressWarnings(build_reporting_support(i, d, g))
  read_reporting_support(b, require_layers = FALSE)
}
#> # A tibble: 6 × 35
#>   polycell_id      cell_id   lon   lat polity_code area_code start_year end_year
#>   <chr>              <int> <dbl> <dbl> <chr>           <int>      <int>    <int>
#> 1 AAA-2000-2020@3…  380269  10.2  44.8 AAA-2000-2…        11       2010     2011
#> 2 AAA-2000-2020@3…  380270  10.2  45.2 AAA-2000-2…        11       2010     2011
#> 3 AAA-2000-2020@3…  381269  10.8  44.8 AAA-2000-2…        11       2010     2011
#> 4 AAA-2000-2020@3…  381270  10.8  45.2 AAA-2000-2…        11       2010     2011
#> 5 AAA-2000-2020@3…  382269  11.2  44.8 AAA-2000-2…        11       2010     2011
#> 6 AAA-2000-2020@3…  382270  11.2  45.2 AAA-2000-2…        11       2010     2011
#> # ℹ 27 more variables: cell_area_ha <dbl>, polity_area_ha <dbl>,
#> #   land_area_ha <dbl>, inland_water_ha <dbl>, ice_area_ha <dbl>,
#> #   geometry_source <chr>, polygon_status <chr>, split_method <chr>,
#> #   coverage_status <chr>, support_role <chr>, area_engine <chr>,
#> #   luh2_vintage <chr>, layers_supplied <chr>, domain_area_ha <dbl>,
#> #   domain_land_area_ha <dbl>, domain_water_ha <dbl>, domain_ice_free_ha <dbl>,
#> #   domain_water_excess_ha <dbl>, water_fraction_ice_free <dbl>, …
```
