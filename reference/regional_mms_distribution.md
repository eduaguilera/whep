# Regional MMS distribution.

Fraction of manure managed in each MMS type by region and species.

## Usage

``` r
regional_mms_distribution
```

## Format

A tibble with `region`, `species`, `mms_type`, `fraction`.

## Source

Unverified. The values are a hardcoded `tribble()` in
`generate_ipcc_tier2_params()` in `data-raw/livestock_coefficients.R`,
annotated there "GLEAM 3.0 / FAO statistics (simplified)"; they are
round to the nearest 5 percentage points and match no table of the
committed GLEAM 3.0 Supplement S1 workbook, which carries no MMS shares.
Unlike `gleam_mms_shares` this object **is** result-affecting, through
`.resolve_mms_shares()`: it weights the Tier 2 manure CH4 methane
conversion factor (`climate_mcf`) and the Tier **1** manure direct-N2O
emission factor (`ipcc_2019_n2o_ef_direct`). It does not reach Tier 2
direct N2O, whose rows carry no `region` column and so take the pasture
EF3 for every stream (measured, whep#921).

The real shares are published in Supplement S1, Tables 4.2-4.11 of FAO.
2018. *GLEAM Model description, Version 2.0, Revision 5* (workbook md5
`72fd2ea477dfe8b30cd3657b2baa4af1`, re-downloaded from FAO and verified
2026-08-26; see `gleam_mms_shares`), per production system (dairy / beef
/ feedlot cattle, dairy / non-dairy buffalo, small ruminants, backyard /
intermediate / industrial pigs, chickens) over the 10 GLEAM regions.
Adopting them takes four choices that each move numbers: collapsing the
per-system tables onto `species_gen`, collapsing GLEAM's regions onto
the IPCC labels `.add_ipcc_region()` emits, deriving the `Global` row
GLEAM does not publish, and mapping GLEAM's richer MMS vocabulary
(Drylot, Pit storage, Burned for fuel, Uncovered anaerobic lagoon) onto
`mms_type`. Under one illustrative crosswalk (equal weights throughout)
on FAOSTAT 2020 head counts, whep#921 measured Tier 1 manure direct N2O
-11.1% (Buffalo -57%, Poultry +22%), Tier 1 manure CO2e -4.2% and Tier 2
manure CH4 -26.9%. Treat as unverified until a crosswalk is chosen.

## Examples

``` r
regional_mms_distribution
#> # A tibble: 33 × 4
#>    region         species mms_type              fraction
#>    <chr>          <chr>   <chr>                    <dbl>
#>  1 North America  Cattle  Liquid/Slurry             0.4 
#>  2 North America  Cattle  Solid Storage             0.3 
#>  3 North America  Cattle  Pasture/Range/Paddock     0.25
#>  4 North America  Cattle  Daily Spread              0.05
#>  5 Western Europe Cattle  Liquid/Slurry             0.35
#>  6 Western Europe Cattle  Solid Storage             0.45
#>  7 Western Europe Cattle  Pasture/Range/Paddock     0.15
#>  8 Western Europe Cattle  Daily Spread              0.05
#>  9 Latin America  Cattle  Pasture/Range/Paddock     0.7 
#> 10 Latin America  Cattle  Solid Storage             0.15
#> # ℹ 23 more rows
```
