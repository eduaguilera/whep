# Convert IPCC per-animal livestock emissions from kilograms to kilotonnes.

Bridge the two unit families the package carries. The IPCC calculators
([`calculate_livestock_emissions()`](https://eduaguilera.github.io/whep/reference/calculate_livestock_emissions.md)
and friends) emit kilograms of gas in
`enteric_ch4_tier1`/`enteric_ch4_tier2`, `manure_ch4_tier1`/
`manure_ch4_tier2` and `manure_n2o_total`. The FAOSTAT-shaped gridded
and national artifacts carry kilotonnes in `enteric_ch4_kt`,
`manure_ch4_kt` and `manure_n2o_kt`. Nothing converted between them, so
the two families never met; this is the one place that conversion
happens.

The factor is exact: 1 kilotonne is 1e6 kilograms.

## Usage

``` r
livestock_emissions_to_kt(data, tier = 2)
```

## Arguments

- data:

  A tibble from the IPCC livestock calculators, carrying the kilogram
  columns for the requested `tier`. A missing column aborts rather than
  being treated as zero.

- tier:

  IPCC tier the kilogram columns come from, `2` (default) or `1`.

## Value

`data` with `enteric_ch4_kt`, `manure_ch4_kt` and `manure_n2o_kt` added.

## Examples

``` r
tibble::tibble(
  enteric_ch4_tier1 = 8e7,
  manure_ch4_tier1 = 1e7,
  manure_n2o_total = 5e5
) |>
  livestock_emissions_to_kt(tier = 1)
#> # A tibble: 1 × 6
#>   enteric_ch4_tier1 manure_ch4_tier1 manure_n2o_total enteric_ch4_kt
#>               <dbl>            <dbl>            <dbl>          <dbl>
#> 1          80000000         10000000           500000             80
#> # ℹ 2 more variables: manure_ch4_kt <dbl>, manure_n2o_kt <dbl>
```
