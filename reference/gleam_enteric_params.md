# Enteric methane conversion factors (IPCC defaults, not GLEAM).

Ym (% of gross energy converted to CH4) by species and production
system. Despite the object name, the values are IPCC defaults, mixed
across the 2006 and 2019 editions, plus one unsourced row; none is a
GLEAM value. No function in `R/` reads this object; the Tier 2 enteric
chain uses
[ipcc_tier2_ym_values](https://eduaguilera.github.io/whep/reference/ipcc_tier2_ym_values.md)
instead.

## Usage

``` r
gleam_enteric_params
```

## Format

A tibble with `species`, `system`, `ym_percent`, `notes`.

## Source

Traced row by row against the published tables (whep#959):

- Cattle and buffalo 6.5% (grazing, mixed) and feedlot cattle 3.0% are
  the 2006 IPCC Guidelines, Vol 4, Ch 10, Table 10.12, which gives 6.5%
  for every non-feedlot cattle and buffalo class and 3.0% for cattle fed
  diets of 90 percent or more concentrates. They are not the 2019
  Refinement's Table 10.12 (Updated), which resolves Ym by production
  level and digestibility (5.7 / 6.0 / 6.3 / 6.5 for dairy cows and
  buffalo, 7.0 / 6.3 for non-dairy, 4.0 for grain feedlots and 3.0 only
  for steam-flaked-corn feedlots).

- Sheep 6.5% is the 2006 Table 10.13 "Mature Sheep" value (lambs under
  one year are 4.5% there). The 2019 Refinement's Table 10.13 (Updated)
  replaces it with a single 6.7% for all sheep.

- Goats 5.5% is the 2019 Refinement, Vol 4, Ch 10, Table 10.13
  (Updated); the 2006 edition has no goat Ym.

- Pigs 0.0% ("negligible") appears in no IPCC table. **Assumed,
  unverified.** IPCC gives swine no Ym, only a Tier 1 enteric factor of
  1.5 (developed) and 1.0 (developing countries) kg CH4 head-1 yr-1
  (2006 Table 10.10; the same in the 2019 Table 10.10 (Updated)).

GLEAM does not publish fixed Ym values of this shape. FAO. 2018. *Global
Livestock Environmental Assessment Model, Model description, Version
2.0, Revision 5*, Table 4.12, p. 67, computes Ym for non-feedlot cattle,
buffalo and adult small ruminants as 9.75 - 0.05 x ration digestibility
(7.75 - 0.05 x digestibility for young small ruminants), uses 3 for
feedlot cattle, and gives pigs 1.01 (adult reproductive) and 0.39
(replacement and fattening), not 0. The GLEAM 2.0 and 3.0 Supplement S1
workbooks in `data-raw/` contain no Ym table. The Version 3.0 model
description was not checked.

## Examples

``` r
gleam_enteric_params
#> # A tibble: 10 × 4
#>    species system  ym_percent notes                 
#>    <chr>   <chr>        <dbl> <chr>                 
#>  1 Cattle  Grazing        6.5 IPCC default          
#>  2 Cattle  Mixed          6.5 IPCC default          
#>  3 Cattle  Feedlot        3   High concentrate diet 
#>  4 Buffalo Grazing        6.5 IPCC default          
#>  5 Buffalo Mixed          6.5 IPCC default          
#>  6 Sheep   Grazing        6.5 IPCC default          
#>  7 Sheep   Mixed          6.5 IPCC default          
#>  8 Goats   Grazing        5.5 IPCC default          
#>  9 Goats   Mixed          5.5 IPCC default          
#> 10 Pigs    All            0   Negligible enteric CH4
```
