# Build source-specific reporting support on a complete physical domain

Preserves canonical polity footprints while selecting an explicit
source/year inventory. Selected footprints must not overlap. The
physical denominator is measured independently from the supplied domain,
including territory with no selected reporting row. Neither source codes
nor polity types set priority. Water supplied as whole-cell fractions is
allocated uniformly over ice-free territory. This is an explicit
within-cell spatial assumption, not an observed water-polygon
distribution. Domain water beyond available ice-free territory remains
recorded as excess. After geometric checks, a numerical excess within
the existing polycell resolution (1e-6 ha per cell) is reduced
proportionally. `measured_land_area_ha`, `area_adjustment_ha` and
`area_adjustment_method` retain that operation; the independent domain
is unchanged. Larger excesses abort, and genuinely unrepresented
territory is never redistributed. These checks establish computational
consistency, not historical boundary truth. Refs \#1310, \#1483 and
\#458.

## Usage

``` r
build_reporting_support(inventory, domain, geometries = NULL, layers = list())
```

## Arguments

- inventory:

  Tibble with `source`, numeric whole `year`, and
  `reporting_polity_code`. Optional `use` is `reporting` or
  `alternative`; alternatives require a nonempty `basis` and remain in
  the returned inventory.

- domain:

  An `sf` polygon table covering the complete physical domain, with a
  nonempty `domain_source` provenance column and `year` matching the
  inventory. Overlapping domain polygons are unioned, not summed.
  Completeness is a supplied contract, not inferred from reporting rows;
  the returned object preserves the domain for review.

- geometries:

  Canonical polity geometries; defaults to
  [`get_polity_geometries()`](https://eduaguilera.github.io/whep/reference/get_polity_geometries.md).

- layers:

  Named list with optional `water` and `ice` inputs as accepted by
  [`build_polycell_support()`](https://eduaguilera.github.io/whep/reference/build_polycell_support.md).
  Absent inputs remain stamped as absent.

## Value

A reporting-support bundle with `support`, `footprints`,
`canonical_geometries`, `physical_domain`, `domain_support`,
`inventory`, `domain`, and per-cell `audit`. Use
[`read_reporting_support()`](https://eduaguilera.github.io/whep/reference/read_reporting_support.md)
to retrieve support retaining the independent domain denominator.

## Examples

``` r
if (requireNamespace("sf", quietly = TRUE)) {
  g <- polycell_example_geometries()
  d <- sf::st_sf(domain_source = "example", year = 2010L,
                  geometry = sf::st_union(g))
  i <- tibble::tibble(source = "example", year = 2010L,
                     reporting_polity_code = g$polity_code)
  suppressWarnings(build_reporting_support(i, d, g))
}
#> $schema_version
#> [1] 1
#> 
#> $support
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
#> 
#> $footprints
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
#> 
#> $canonical_geometries
#> Simple feature collection with 1 feature and 7 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 10.1 ymin: 44.9 xmax: 11.4 ymax: 45.4
#> Geodetic CRS:  WGS 84
#>     polity_code polity_type wiki_status polygon_status start_year end_year
#> 1 AAA-2000-2020    national    reviewed       assigned       2000     2020
#>   area_code                           geom
#> 1        11 POLYGON ((10.1 44.9, 11.4 4...
#> 
#> $inventory
#> # A tibble: 1 × 5
#>   source   year reporting_polity_code use       basis
#>   <chr>   <int> <chr>                 <chr>     <chr>
#> 1 example  2010 AAA-2000-2020         reporting NA   
#> 
#> $domain
#> Simple feature collection with 1 feature and 2 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 10.1 ymin: 44.9 xmax: 11.4 ymax: 45.4
#> Geodetic CRS:  WGS 84
#>   domain_source year                       geometry
#> 1       example 2010 POLYGON ((10.1 44.9, 11.4 4...
#> 
#> $physical_domain
#> Simple feature collection with 1 feature and 5 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 10.1 ymin: 44.9 xmax: 11.4 ymax: 45.4
#> Geodetic CRS:  WGS 84
#>           polity_code area_code domain_geometry_status start_year end_year
#> 1 __physical_domain__        NA                     ok       2010     2011
#>                         geometry
#> 1 POLYGON ((10.1 44.9, 11.4 4...
#> 
#> $domain_support
#> # A tibble: 6 × 22
#>   polycell_id      cell_id   lon   lat polity_code area_code start_year end_year
#>   <chr>              <int> <dbl> <dbl> <chr>           <int>      <int>    <int>
#> 1 __physical_doma…  380269  10.2  44.8 __physical…        NA       2010     2011
#> 2 __physical_doma…  380270  10.2  45.2 __physical…        NA       2010     2011
#> 3 __physical_doma…  381269  10.8  44.8 __physical…        NA       2010     2011
#> 4 __physical_doma…  381270  10.8  45.2 __physical…        NA       2010     2011
#> 5 __physical_doma…  382269  11.2  44.8 __physical…        NA       2010     2011
#> 6 __physical_doma…  382270  11.2  45.2 __physical…        NA       2010     2011
#> # ℹ 14 more variables: cell_area_ha <dbl>, polity_area_ha <dbl>,
#> #   land_area_ha <dbl>, inland_water_ha <dbl>, ice_area_ha <dbl>,
#> #   geometry_source <chr>, polygon_status <chr>, split_method <chr>,
#> #   coverage_status <chr>, support_role <chr>, area_engine <chr>,
#> #   luh2_vintage <chr>, layers_supplied <chr>, water_excess_ha <dbl>
#> 
#> $geometry_checks
#> $geometry_checks$containment
#>   area_engine polity_area_ha
#> 1          s2   0.000000e+00
#> 2          s2   9.380766e-24
#> 
#> 
#> $audit
#> # A tibble: 6 × 7
#>   cell_id domain_land_area_ha domain_water_excess_ha reporting_land_ha
#>     <int>               <dbl>                  <dbl>             <dbl>
#> 1  380269              34757.                      0            34757.
#> 2  380270             139639.                      0           139639.
#> 3  381269              43065.                      0            43065.
#> 4  381270             174928.                      0           174928.
#> 5  382269              34757.                      0            34757.
#> 6  382270             139639.                      0           139639.
#> # ℹ 3 more variables: measured_reporting_land_ha <dbl>,
#> #   area_adjustment_ha <dbl>, unrepresented_land_ha <dbl>
#> 
#> attr(,"class")
#> [1] "whep_reporting_support"
```
