# IPCC direct manure N2O emission factors (EF3), by edition.

EF3 (kg N2O-N per kg N) for the six manure-management labels WHEP's
manure engine carries, in two editions. The `ef3_source` option of the
manure engine (see
[manure_engine_options](https://eduaguilera.github.io/whep/reference/manure_engine_options.md))
and of
[`apply_management_losses()`](https://eduaguilera.github.io/whep/reference/apply_management_losses.md)
selects one; `"ipcc_2019"` is the default.

- `"ipcc_2019"`: the 2019 Refinement, Vol 4, Ch 10, Table 10.21
  (Updated): daily spread 0, solid storage 0.010, uncovered anaerobic
  lagoon 0, poultry manure with litter 0.001. Liquid/slurry is published
  as 0.005 with a natural crust, 0 without and 0.005 covered; the first
  two are carried as `"Liquid/Slurry - With Crust"` and
  `"Liquid/Slurry - No Crust"`, and the engine's single
  `"Liquid/Slurry"` label takes their mix at the crust share the
  `slurry_crust` option selects: `"half"` (default, 0.0025),
  `"none"` (0) or `"all"` (0.005). Half is GLEAM 2.0's assumption:
  Supplement S1 footnotes the "Liquid slurry\*" of its pig and chicken
  tables (Tab. 4.8-4.11) "50% of this manure management system is
  assumed to present natural crust". Its cattle and buffalo tables (Tab.
  4.2-4.6) state no share, so the half is extended to them by
  assumption. Pasture/range/paddock is deferred to Ch 11, whose Table
  11.1 (Updated) gives the aggregated EF3PRP 0.004 for "cattle (dairy,
  non-dairy and buffalo), poultry and pigs"
  (`animal_class == "cattle_poultry_pigs"`) and 0.003 for "sheep and
  'other animals'" (`"sheep_other"`), the chapter listing goats, horses,
  mules, donkeys, camels, reindeer and camelids as other animals. Its
  disaggregated wet/dry pair (0.006 / 0.002) needs a moisture regime per
  row and is not carried.

- `"as_shipped"`: the values the engine read before whep#601, a
  crosswalk onto
  [ipcc_2019_n2o_ef_direct](https://eduaguilera.github.io/whep/reference/ipcc_2019_n2o_ef_direct.md),
  which holds 2006 and unpublished values (see its `@source`). Its
  `"Liquid/Slurry"` 0.002 is not the no-crust value (both editions print
  0 for that) but the EF3 of pit storage below animal confinements.
  `slurry_crust` does not apply to it. Kept so earlier figures stay
  reproducible.

`"Solid Storage"` also carries GLEAM's dry-lot shares (see
[regional_mms_distribution](https://eduaguilera.github.io/whep/reference/regional_mms_distribution.md)),
which Table 10.21 prices at 0.02, not 0.010.

## Usage

``` r
ipcc_manure_ef3
```

## Format

A tibble with one row per `edition`, `mms_type` and `animal_class`:

- `edition`: `"ipcc_2019"` or `"as_shipped"`.

- `mms_type`: manure-management system label, plus the two published
  liquid/slurry crust rows of the 2019 edition.

- `animal_class`: `"all"`, or for 2019 pasture `"cattle_poultry_pigs"`
  or `"sheep_other"`.

- `ef3`: kg N2O-N per kg N.

- `ipcc_system`: the published row (or the shipped table's row) the
  value was read from.

## Source

IPCC. 2019. *2019 Refinement to the 2006 IPCC Guidelines for National
Greenhouse Gas Inventories*, Vol 4, Ch 10, Table 10.21 (Updated), pp.
10.91-10.93 (PDF md5 `c1784e747af9bb307e93f4170c12679a`), and Vol 4, Ch
11, Table 11.1 (Updated), p. 11.13 (PDF md5
`06c11d4f5a2b729564f8b3bd1ba11824`). Both downloaded from
`ipcc-nggip.iges.or.jp` and checksummed on 2026-10-07.

## Measured effect

On FAOSTAT 2020 heads at national grain, Tier 1 direct manure N2O goes
from 1.523 Tg (as shipped) to 1.178 Tg, almost all of it the pasture
factor (0.010 to 0.004 / 0.003): keeping pasture at 0.010 would give
1.830 Tg. The liquid/slurry crust share moves it between 1.120 Tg
(`"none"`) and 1.236 Tg (`"all"`). In
[`apply_management_losses()`](https://eduaguilera.github.io/whep/reference/apply_management_losses.md),
which carries no management N2O on the grazing stream, the 2019 edition
moves the nitrogen applied to land by -1.4 to +0.6 percent per species
under the Global split.

## Examples

``` r
ipcc_manure_ef3
#> # A tibble: 14 × 5
#>    edition    mms_type                   animal_class          ef3 ipcc_system  
#>    <chr>      <chr>                      <chr>               <dbl> <chr>        
#>  1 ipcc_2019  Pasture/Range/Paddock      cattle_poultry_pigs 0.004 EF3PRP,CPP (…
#>  2 ipcc_2019  Pasture/Range/Paddock      sheep_other         0.003 EF3PRP,SO (C…
#>  3 ipcc_2019  Daily Spread               all                 0     Daily spread 
#>  4 ipcc_2019  Solid Storage              all                 0.01  Solid storage
#>  5 ipcc_2019  Liquid/Slurry - With Crust all                 0.005 Liquid/Slurr…
#>  6 ipcc_2019  Liquid/Slurry - No Crust   all                 0     Liquid/Slurr…
#>  7 ipcc_2019  Anaerobic Lagoon           all                 0     Uncovered an…
#>  8 ipcc_2019  Poultry Manure             all                 0.001 Poultry manu…
#>  9 as_shipped Pasture/Range/Paddock      all                 0.01  Pasture/Rang…
#> 10 as_shipped Daily Spread               all                 0.01  Daily Spread 
#> 11 as_shipped Solid Storage              all                 0.005 Solid Storage
#> 12 as_shipped Liquid/Slurry              all                 0.002 Liquid/Slurry
#> 13 as_shipped Anaerobic Lagoon           all                 0.001 Uncovered An…
#> 14 as_shipped Poultry Manure             all                 0.001 Poultry Manu…
```
