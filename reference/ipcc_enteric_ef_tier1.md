# IPCC Tier 1 enteric emission factors, as published.

Tier 1 enteric fermentation emission factors (kg CH4/head/yr) as the
2019 Refinement publishes them, which the `ipcc_2019_enteric_ef_*`
tables did not hold (whep#601). Tier 1 enteric CH4 reads this table by
default; the `enteric_ef_source` manure-engine option selects it
(`"ipcc_2019"`) or the older tables (`"as_shipped"`), see
[manure_engine_options](https://eduaguilera.github.io/whep/reference/manure_engine_options.md).

A category has one of three shapes:

- **regional** (`region` is an IPCC region, `productivity == "All"`):
  dairy cattle, other cattle and buffalo, Table 10.11 (Updated). Only
  the Tier 1 column is carried; the high/low productivity rows the table
  adds for five regions are Tier 1a, which needs the head count split by
  productivity system. North America and Oceania publish no buffalo
  factor ("no buffalo herds").

- **split by productivity** (`region == "All"`, `productivity` `"High"`
  or `"Low"`): sheep, goats and swine, Table 10.10 (Updated). Its
  footnote 1 says that "for all regions other than North America, Europe
  and Oceania the Tier 1 default values are the low productivity EFs",
  so Tier 1 takes `"High"` in North America, Western Europe, Eastern
  Europe and Oceania and `"Low"` in every other IPCC region.

- **flat** (`region == "All"`, `productivity == "All"`): horses, camels,
  mules and asses, and the deer, ostrich and llama/alpaca rows of the
  same table.

Poultry has no row: both editions say "insufficient data for
calculation". A row the edition prices at nothing – poultry, buffalo in
North America or Oceania, or a regional or productivity-split species on
a row whose IPCC region does not resolve – keeps the as-shipped factor,
and `method_enteric` says so (`enteric_ef_as_shipped_fallback`).

## Usage

``` r
ipcc_enteric_ef_tier1
```

## Format

A tibble with one row per `edition`, `region`, `category` and
`productivity`:

- `edition`: `"ipcc_2019"`.

- `region`: IPCC Tier 1 region, or `"All"`.

- `category`: `"Dairy Cattle"`, `"Other Cattle"`, `"Buffalo"` or a
  general species name.

- `productivity`: `"All"`, `"High"` or `"Low"`.

- `ef_kg_head_yr`: emission factor, kg CH4 per head per year.

- `ipcc_table`: the table the value was read from.

## Source

IPCC. 2019. *2019 Refinement to the 2006 IPCC Guidelines for National
Greenhouse Gas Inventories*, Vol 4, Ch 10, Table 10.10 (Updated), p.
10.38, and Table 10.11 (Updated), pp. 10.40-10.43 (PDF md5
`c1784e747af9bb307e93f4170c12679a`, downloaded from
`ipcc-nggip.iges.or.jp` and checksummed on 2026-10-07). Every value is a
transcription.

## Measured effect

On FAOSTAT 2020 heads at national grain, Tier 1 enteric CH4 goes from
110.8 Tg (as shipped) to 125.3 Tg: cattle 78.58 to 90.98 Tg, buffalo
11.21 to 16.55 Tg, sheep 10.30 to 7.31 Tg (low productivity outside the
four high regions), swine 1.41 to 1.08 Tg and goats 5.47 to 5.56 Tg.
Every row resolved an IPCC region; the only fallback rows were poultry
and 163 buffalo head in North America and Oceania.

## Examples

``` r
ipcc_enteric_ef_tier1
#> # A tibble: 37 × 6
#>    edition   region         category     productivity ef_kg_head_yr ipcc_table  
#>    <chr>     <chr>          <chr>        <chr>                <dbl> <chr>       
#>  1 ipcc_2019 North America  Dairy Cattle All                    138 10.11 (Upda…
#>  2 ipcc_2019 North America  Other Cattle All                     64 10.11 (Upda…
#>  3 ipcc_2019 Western Europe Dairy Cattle All                    126 10.11 (Upda…
#>  4 ipcc_2019 Western Europe Other Cattle All                     52 10.11 (Upda…
#>  5 ipcc_2019 Western Europe Buffalo      All                     78 10.11 (Upda…
#>  6 ipcc_2019 Eastern Europe Dairy Cattle All                     93 10.11 (Upda…
#>  7 ipcc_2019 Eastern Europe Other Cattle All                     58 10.11 (Upda…
#>  8 ipcc_2019 Eastern Europe Buffalo      All                     68 10.11 (Upda…
#>  9 ipcc_2019 Oceania        Dairy Cattle All                     93 10.11 (Upda…
#> 10 ipcc_2019 Oceania        Other Cattle All                     63 10.11 (Upda…
#> # ℹ 27 more rows
```
