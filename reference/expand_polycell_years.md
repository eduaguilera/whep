# Expand the interval-keyed polycell support to one row per year

Repeats every polycell interval over the calendar years it covers,
adding a `year` column. `start_year` is inclusive; `end_year` is
**exclusive at a succession**, so a boundary year resolves to the
successor alone and is never counted twice, and **inclusive at the open
end**, so the last year the table covers still resolves to the polity
nothing succeeds instead of to nothing at all.

## Usage

``` r
expand_polycell_years(support, years)
```

## Arguments

- support:

  A
  [`build_polycell_support()`](https://eduaguilera.github.io/whep/reference/build_polycell_support.md)
  table in the interval grain, carrying `cell_id`, `polity_code`,
  `start_year` and `end_year`. The first two are what identify
  successive intervals of one polity in one cell, and without them the
  open end cannot be told from a succession.

- years:

  Integer vector of calendar years.

## Value

A `tibble` with one row per polycell-year, `year` placed after
`area_code`.

## Examples

``` r
if (requireNamespace("sf", quietly = TRUE)) {
  build_polycell_support(geometries = polycell_example_geometries()) |>
    expand_polycell_years(2010L:2012L)
}
#> Warning: No `ice` layer was supplied, so ice_area_ha is identically zero.
#> ✖ Every lake, river and glacier inside a polity is therefore booked as LAND,
#>   and the identity `polity_area_ha == land_area_ha + inland_water_ha +
#>   ice_area_ha` still holds, so no downstream check can see it.
#> ℹ This is correct for a smoke build and wrong for a published pin (#885).
#>   Supply the layer, or state in the publishing commit that ice_area_ha is zero
#>   by construction.
#> Warning: No `water` layer was supplied, so inland_water_ha is identically zero.
#> ✖ Every lake, river and glacier inside a polity is therefore booked as LAND,
#>   and the identity `polity_area_ha == land_area_ha + inland_water_ha +
#>   ice_area_ha` still holds, so no downstream check can see it.
#> ℹ This is correct for a smoke build and wrong for a published pin (#885).
#>   Supply the layer, or state in the publishing commit that inland_water_ha is
#>   zero by construction.
#> # A tibble: 18 × 22
#>    polycell_id        cell_id   lon   lat polity_code area_code  year start_year
#>    <chr>                <int> <dbl> <dbl> <chr>           <int> <int>      <int>
#>  1 AAA-2000-2020@380…  380269  10.2  44.8 AAA-2000-2…        11  2010       2000
#>  2 AAA-2000-2020@380…  380270  10.2  45.2 AAA-2000-2…        11  2010       2000
#>  3 AAA-2000-2020@381…  381269  10.8  44.8 AAA-2000-2…        11  2010       2000
#>  4 AAA-2000-2020@381…  381270  10.8  45.2 AAA-2000-2…        11  2010       2000
#>  5 AAA-2000-2020@382…  382269  11.2  44.8 AAA-2000-2…        11  2010       2000
#>  6 AAA-2000-2020@382…  382270  11.2  45.2 AAA-2000-2…        11  2010       2000
#>  7 AAA-2000-2020@380…  380269  10.2  44.8 AAA-2000-2…        11  2011       2000
#>  8 AAA-2000-2020@380…  380270  10.2  45.2 AAA-2000-2…        11  2011       2000
#>  9 AAA-2000-2020@381…  381269  10.8  44.8 AAA-2000-2…        11  2011       2000
#> 10 AAA-2000-2020@381…  381270  10.8  45.2 AAA-2000-2…        11  2011       2000
#> 11 AAA-2000-2020@382…  382269  11.2  44.8 AAA-2000-2…        11  2011       2000
#> 12 AAA-2000-2020@382…  382270  11.2  45.2 AAA-2000-2…        11  2011       2000
#> 13 AAA-2000-2020@380…  380269  10.2  44.8 AAA-2000-2…        11  2012       2000
#> 14 AAA-2000-2020@380…  380270  10.2  45.2 AAA-2000-2…        11  2012       2000
#> 15 AAA-2000-2020@381…  381269  10.8  44.8 AAA-2000-2…        11  2012       2000
#> 16 AAA-2000-2020@381…  381270  10.8  45.2 AAA-2000-2…        11  2012       2000
#> 17 AAA-2000-2020@382…  382269  11.2  44.8 AAA-2000-2…        11  2012       2000
#> 18 AAA-2000-2020@382…  382270  11.2  45.2 AAA-2000-2…        11  2012       2000
#> # ℹ 14 more variables: end_year <int>, cell_area_ha <dbl>,
#> #   polity_area_ha <dbl>, land_area_ha <dbl>, inland_water_ha <dbl>,
#> #   ice_area_ha <dbl>, geometry_source <chr>, polygon_status <chr>,
#> #   split_method <chr>, coverage_status <chr>, support_role <chr>,
#> #   area_engine <chr>, luh2_vintage <chr>, water_excess_ha <dbl>
```
