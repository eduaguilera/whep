# IPCC 2006 MCF by temperature.

Table 10.17 (2006): MCF values by MMS type and annual temperature.

## Usage

``` r
ipcc_2006_mcf_temp
```

## Format

A tibble with `system`, `temp_c`, `mcf_percent`.

## Source

Loosely IPCC 2006, Vol 4, Ch 10, Table 10.17, but re-resolved onto a
10/15/20/25 degree Celsius grid the published table does not use, and
the `temp_c == 25` value of every one of the four rows appears in no
column of Table 10.17. Verified against the published table:

- Liquid/slurry is the only row Table 10.17 resolves per degree, as
  17/19/20/22/25/27 percent for 10 to 15 degrees rising to 65 percent at
  25 (without a natural crust cover). Stored 17 is its 10 degree column,
  but 25 is the 14 degree column rather than the 27 of 15 degrees, 35 is
  the 18 degree column rather than the 42 of 20 degrees, and 48 is in no
  column at all (46 at 21 degrees, 50 at 22).

- Pasture/range/paddock, daily spread and solid storage are published
  only by climate class – 1.0/1.5/2.0, 0.1/0.5/1.0 and 2.0/4.0/5.0
  percent for cool/temperate/warm. Stored 10 and 15 degrees take the
  cool and temperate figures, but 20 degrees takes the warm figure
  although 20 degrees falls inside the published temperate class, and 25
  degrees takes 2.5, 1.5 and 6 percent, none of which the table
  publishes. **Assumed, unverified.** No function in `R/` reads this
  object – the Tier 2 manure path uses
  [climate_mcf](https://eduaguilera.github.io/whep/reference/climate_mcf.md)
  and the Tier 1 path
  [ipcc_2019_mcf_manure](https://eduaguilera.github.io/whep/reference/ipcc_2019_mcf_manure.md)
  – so nothing published depends on it. Tracked in whep#601.

## Examples

``` r
ipcc_2006_mcf_temp
#> # A tibble: 16 × 3
#>    system                temp_c mcf_percent
#>    <chr>                  <dbl>       <dbl>
#>  1 Liquid/Slurry             10        17  
#>  2 Liquid/Slurry             15        25  
#>  3 Liquid/Slurry             20        35  
#>  4 Liquid/Slurry             25        48  
#>  5 Solid Storage             10         2  
#>  6 Solid Storage             15         4  
#>  7 Solid Storage             20         5  
#>  8 Solid Storage             25         6  
#>  9 Pasture/Range/Paddock     10         1  
#> 10 Pasture/Range/Paddock     15         1.5
#> 11 Pasture/Range/Paddock     20         2  
#> 12 Pasture/Range/Paddock     25         2.5
#> 13 Daily Spread              10         0.1
#> 14 Daily Spread              15         0.5
#> 15 Daily Spread              20         1  
#> 16 Daily Spread              25         1.5
```
