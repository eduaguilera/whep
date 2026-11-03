# IPCC 2019 nitrogen excretion rates.

Default nitrogen excretion by animal category and region, stored as
annual excretion per head (kg N/head/yr). That is the form the Tier 1
manure N2O path consumes, and the same quantity the Tier 2 path derives
from the energy balance. Since whep#601 the manure engine reads the
sourced
[ipcc_n_excretion_tier1](https://eduaguilera.github.io/whep/reference/ipcc_n_excretion_tier1.md)
by default; this table is read only under `nex_source = "as_shipped"`
(see
[manure_engine_options](https://eduaguilera.github.io/whep/reference/manure_engine_options.md))
and as the fallback only when `nex_missing = "as_shipped"` is explicitly
requested.

## Usage

``` r
ipcc_2019_n_excretion
```

## Format

A tibble with `region`, `category`, `nex_kg_n_head_yr`.

## Source

Unverified. IPCC 2019 Refinement, Vol 4, Ch 10, Table 10.19 (Updated)
publishes the excretion *rate* per 1000 kg animal mass per day, not an
annual per-head amount, and the values stored here do not follow from
it. Equation 10.30 (Updated) converts the rate with the typical animal
mass of Table 10A.5 (New), and
[ipcc_n_excretion_tier1](https://eduaguilera.github.io/whep/reference/ipcc_n_excretion_tier1.md)
holds the result: 140 kg N/head/yr for North American dairy cattle (0.59
x 650 kg x 365 / 1000) against the 105 stored, 118 for Western Europe
(100 stored), 84 for Eastern Europe (80), 128 for Oceania (80), 72 for
Latin America (50), 62 for Asia (50), 42 for Africa (40), 64 for the
Middle East (40) and 68 for the Indian Subcontinent (50). Tracked in
whep#601.

## Examples

``` r
ipcc_2019_n_excretion
#> # A tibble: 30 × 3
#>    region         category     nex_kg_n_head_yr
#>    <chr>          <chr>                   <dbl>
#>  1 North America  Dairy Cattle              105
#>  2 North America  Other Cattle               56
#>  3 Western Europe Dairy Cattle              100
#>  4 Western Europe Other Cattle               50
#>  5 Eastern Europe Dairy Cattle               80
#>  6 Eastern Europe Other Cattle               50
#>  7 Oceania        Dairy Cattle               80
#>  8 Oceania        Other Cattle               40
#>  9 Latin America  Dairy Cattle               50
#> 10 Latin America  Other Cattle               40
#> # ℹ 20 more rows
```
