# IPCC Tier 1 nitrogen excretion per head, as published.

Annual nitrogen excretion per head (kg N/head/yr) by IPCC region and
animal category, derived exactly as the 2019 Refinement's Tier 1 says:
Equation 10.30 (Updated), \\Nex = Nrate \times TAM / 1000 \times 365\\,
with the default rate of Table 10.19 (Updated) and the default typical
animal mass of Table 10A.5 (New), to which the text under the equation
points. Both inputs are stored beside the product. Tier 1 manure N2O
reads this table by default; the `nex_source` manure-engine option
selects it (`"ipcc_2019"`) or the older
[ipcc_2019_n_excretion](https://eduaguilera.github.io/whep/reference/ipcc_2019_n_excretion.md)
(`"as_shipped"`), see
[manure_engine_options](https://eduaguilera.github.io/whep/reference/manure_engine_options.md).

Only the Tier 1 ("Mean") column is carried for the five regions that
also print high- and low-productivity values: footnote 1 of both tables
says those are for the Tier 1a method, which needs heads split by
productivity system. A value the tables merge across regions is repeated
for each region it spans. Buffalo has no North America or Oceania row
(Table 10.19 prints NA, "these animal categories do not occur in these
regions"). Horses and mules and asses share one rate row and take their
own masses.

Categories: `"Dairy Cattle"`, `"Other Cattle"`, `"Buffalo"`,
`"Swine - Market"` (Table 10.19 "Finishing"), `"Swine - Breeding"`,
`"Poultry - Layers"` ("Hens \>/= 1 yr"), `"Poultry - Broilers"`,
`"Chickens"` (the chicken aggregate, for a chicken that is neither),
`"Turkeys"`, `"Ducks"`, `"Sheep"`, `"Goats"`, `"Horses"`,
`"Mules and Asses"` and `"Camels"`. Geese have no row in either table; a
missing coefficient aborts by default. Only an explicit
`nex_missing = "as_shipped"` requests a legacy value, stamped in
`method_manure_n2o` as `nex_as_shipped_fallback`.

## Usage

``` r
ipcc_n_excretion_tier1
```

## Format

A tibble with one row per `edition`, `region` and `category`:

- `edition`: `"ipcc_2019"`.

- `region`: IPCC Tier 1 region.

- `category`: animal category, as above.

- `nex_kg_n_head_yr`: annual N excretion, kg N per head per year.

- `nrate_kg_n_1000kg_day`: Table 10.19 (Updated) rate, kg N per 1000 kg
  animal mass per day.

- `tam_kg`: Table 10A.5 (New) typical animal mass, kg per head.

- `ipcc_category`: the row the values were read from.

## Source

IPCC. 2019. *2019 Refinement to the 2006 IPCC Guidelines for National
Greenhouse Gas Inventories*, Vol 4, Ch 10, Equation 10.30 (Updated), p.
10.80, Table 10.19 (Updated), pp. 10.83-10.84, and Table 10A.5 (New),
pp. 10.117-10.118 (PDF md5 `c1784e747af9bb307e93f4170c12679a`,
downloaded from `ipcc-nggip.iges.or.jp`). Every rate and mass is a
transcription, read from both the PDF's text layer and its page images.

## Measured effect

On FAOSTAT 2020 heads at national grain, Tier 1 N excretion goes from
144.9 Tg N (as shipped) to 126.3 Tg N: cattle 64.1 to 72.2, buffalo 11.2
to 13.2, poultry 19.5 to 14.4, swine 14.4 to 12.8, sheep 15.4 to 5.1 and
goats 13.1 to 3.3 Tg N. Tier 1 manure N2O goes from 2.151 to 1.962 Tg.
The sheep and goat drop is the edition's: Table 10.19 (Updated) takes
their rates from GLEAM (0.32 to 0.46 kg N per 1000 kg per day), about a
quarter of the 2006 Table 10.19 rates outside North America (sheep 0.85
to 1.17, goats 1.28 to 1.42). FAOSTAT's 2020 Tier 1 figures for the same
heads are 22.2 Tg N for sheep and 25.3 for goats, because FAOSTAT takes
its rates from the 2006 table (FAOSTAT Livestock Manure methodological
note, 2025). This historical comparison uses the explicitly selectable
hybrid policy `nex_missing = "as_shipped"`. Every row resolved an IPCC
region; missing coefficients were geese and 163 buffalo head in Oceania.
The table also lacks a North American buffalo coefficient, but this
frozen population contains no affected North American buffalo row. The
strict default now aborts on those rows, rather than returning a partial
total or silently choosing that hybrid.

## Why the 2019 sheep and goat rates are kept

The default follows the package's 2019 edition precedent. The difference
from FAOSTAT is not a transcription error: FAOSTAT uses the 2006 rates.
External comparisons give class-specific support, not validation of a
global flock-average rate. Lazzari and Schlegel (2025, Agrarforschung
Schweiz 16:26-35 and 16:81-89, Table 3) model a 75 kg suckler ewe at
9.55 kg N/yr and a 60 kg dairy goat at 9.26 kg N/yr. These imply 0.35
and 0.42 kg N per 1000 kg per day. Decandia et al. (2011, Options
Mediterraneennes A 99:201-209, Table 2) report 15.5 g N/day at 50.6 kg
for dry ewes and 16.3 g N/day at 46.2 kg for dry goats (0.31 and 0.35).
The physiological scope matters: the same Lazzari sheep table models a
dairy ewe producing 500 kg milk annually at 16.07 kg N/yr and 75 kg live
weight, implying 0.587, above the Western European IPCC sheep rate 0.36.
The supporting sources do not establish regional flock composition.
Their corroboration therefore remains provisional and scope-limited,
separate from the edition precedent. See decision
`whep-tier1-n-excretion-ipcc-2019-rate-times-mass` (whep#601).

## Examples

``` r
ipcc_n_excretion_tier1
#> # A tibble: 133 × 7
#>    edition   region       category nex_kg_n_head_yr nrate_kg_n_1000kg_day tam_kg
#>    <chr>     <chr>        <chr>               <dbl>                 <dbl>  <dbl>
#>  1 ipcc_2019 North Ameri… Dairy C…            140.                   0.59    650
#>  2 ipcc_2019 Western Eur… Dairy C…            118.                   0.54    600
#>  3 ipcc_2019 Eastern Eur… Dairy C…             84.3                  0.42    550
#>  4 ipcc_2019 Oceania      Dairy C…            128.                   0.72    488
#>  5 ipcc_2019 Latin Ameri… Dairy C…             72.3                  0.39    508
#>  6 ipcc_2019 Africa       Dairy C…             41.8                  0.44    260
#>  7 ipcc_2019 Middle East  Dairy C…             63.7                  0.5     349
#>  8 ipcc_2019 Asia         Dairy C…             62.0                  0.44    386
#>  9 ipcc_2019 Indian Subc… Dairy C…             67.6                  0.65    285
#> 10 ipcc_2019 North Ameri… Other C…             59.4                  0.4     407
#> # ℹ 123 more rows
#> # ℹ 1 more variable: ipcc_category <chr>
```
