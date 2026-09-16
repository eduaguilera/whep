# Climate-zone MCF values.

Methane Conversion Factors by MMS type and climate zone
(Cool/Temperate/Warm).

## Usage

``` r
climate_mcf
```

## Format

A tibble with `mms_type`, `climate_zone`, `mcf_percent`.

## Source

Predominantly the 2006 Guidelines, Vol 4, Ch 10, Table 10.17, **not**
the 2019 Refinement, and with the same provenance profile as
[ipcc_2019_mcf_manure](https://eduaguilera.github.io/whep/reference/ipcc_2019_mcf_manure.md),
which holds the same values under the Tier 1 system labels. Since
whep#1022 this object is **no longer the default** the Tier 2 path
reads: `.calc_manure_ch4_tier2()` weights the `edition == "ipcc_2019"`
rows of
[climate_mcf_ipcc](https://eduaguilera.github.io/whep/reference/climate_mcf_ipcc.md)
by the manure-system mix, and this table is reached only with
`mcf_source = "as_shipped"`. Verified against both editions:

- Matching both: daily spread 0.1/0.5/1.0, solid storage 2.0/4.0/5.0,
  poultry manure 1.5 and burned for fuel 10.

- The 2006 edition only: pasture/range/paddock 1.0/1.5/2.0, against a
  single 0.47 percent in the 2019 Refinement.

- Derived from the 2006 per-degree rows, but not at the class bound:
  liquid/slurry 17/35/80 takes 35 from the 18 degree column rather than
  the 42 of 20 degrees, and anaerobic lagoon 66/73/80 takes 73 from the
  14 degree column rather than the 78 of 20 degrees.

- Matching neither edition: dry lot 1.5/2.5/4.0 (both give 1.0/1.5/2.0)
  and the single all-climate values for intensive-windrow composting 0.5
  and passive-windrow composting 1.0, which both editions resolve by
  climate (2006 gives 0.5/1.0/1.5 for each; the 2019 Refinement
  0.5/1.0/1.5 and 1.0/2.0/2.5). `"Anaerobic Digester"` 0 is published by
  neither: 2006 gives the range 0-100 percent and requires a
  calculation, the 2019 Refinement six leakage-and-storage classes
  spanning 1.00 to 13.17 percent. **Assumed, unverified.**

Only six of the eleven systems here are reachable on the live path:
[regional_mms_distribution](https://eduaguilera.github.io/whep/reference/regional_mms_distribution.md)
routes manure to daily spread, solid storage, liquid/slurry, the
anaerobic lagoon, pasture/range/paddock and poultry manure, and
`.calc_weighted_mcf()` aborts on any label outside that set. So the
dry-lot, composting and anaerobic-digester cells enter no published
number today, and the four rows keyed `climate_zone` `"All"` are
unreachable for a second reason: the MCF join is on
`(mms_type, climate_zone)` and never asks for `"All"`. The cells that
both are live and depend on the edition are pasture/range/paddock,
liquid/slurry and the anaerobic lagoon.
[climate_mcf_ipcc](https://eduaguilera.github.io/whep/reference/climate_mcf_ipcc.md)
carries the as-published alternative for each and now supplies the
default.

Tracked in whep#601 and whep#1022.

## Examples

``` r
climate_mcf
#> # A tibble: 25 × 3
#>    mms_type      climate_zone mcf_percent
#>    <chr>         <chr>              <dbl>
#>  1 Daily Spread  Cool                 0.1
#>  2 Daily Spread  Temperate            0.5
#>  3 Daily Spread  Warm                 1  
#>  4 Solid Storage Cool                 2  
#>  5 Solid Storage Temperate            4  
#>  6 Solid Storage Warm                 5  
#>  7 Dry Lot       Cool                 1.5
#>  8 Dry Lot       Temperate            2.5
#>  9 Dry Lot       Warm                 4  
#> 10 Liquid/Slurry Cool                17  
#> # ℹ 15 more rows
```
