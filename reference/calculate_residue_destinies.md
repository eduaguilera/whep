# Estimate the destinies of crop residues.

Splits crop residue dry matter into three destinies that sum to the
total residue: fed to livestock, burned / removed for fuel, and left on
the field for soil incorporation.

## Usage

``` r
calculate_residue_destinies(x, method = c("recovery_regional", "shares"))
```

## Arguments

- x:

  A tibble with `item_prod_code` and `residue_dm_t`. The
  `recovery_regional` method also needs `region_krausmann` (for the
  recovery rate) and `region_un_sub` (for the feed-use fraction, the UN
  M49 sub-regions of `regions_full$region_UN_sub`). `region_krausmann`
  can use the recovery-table labels or the matching `regions_full`
  labels. The `shares` method needs `year`.

- method:

  Destiny method: `"recovery_regional"` (default, the regional recovery
  rate times the UN-sub-regional feed-use fraction) or `"shares"` (the
  Spain-specific per-crop-year use/burn shares, flagged
  `to_be_revised`).

## Value

The input tibble with `residue_feed_dm_t`, `residue_burn_dm_t`,
`residue_soil_dm_t` and `method_residue_destiny`.

## Where the recovery rates come from

The `recovery_regional` recovery rates live in
`inst/extdata/coefs/residue_recovery.csv`, whose per-row `source` column
records provenance. They are **Wirsenius (2000)**, *Human Use of Land
and Organic Materials*, PhD thesis, Chalmers University of Technology –
Table 3.17 (recovery rates, p. 94) and Table 3.16 (harvest index, p. 92,
from which `residue_dm_product_dm` is the residue:product ratio rounded
to one decimal). The file was named after Krausmann and its two key
columns still are, but no coefficient in it is Krausmann's. Only the
crop category (`items_prod_full$Cat_Krausmann`) really is his:
`region_krausmann` here holds `regions_full$region_HANPP` labels, whose
eight values are Wirsenius's eight regions (whep#1132).

Nine of the twenty crop categories match Table 3.17 cell for cell. The
rest do not, and the `source` column says which is which:
`Groundnuts in Shell` uses the cereals pattern where Table 3.17 gives
0.90 everywhere, and ten categories – roots, tubers, cassava, sugar
beet, sugar crops nes, dry beans, pulses, oil palm, castor beans,
permanent and fodder crops – appear in neither table, so their recovery
rates are assumed and unverified. Wirsenius states that recovery rates
for the crop flows Table 3.17 omits "were assumed to be close to 100
percent"; this table instead assumes between 0 and 0.9 for them, which
is why fodder crops recover nothing at all here.

## The feed-use fraction is a different, unpaired source

Wirsenius coordinates Table 3.17 directly with the feed assignments of
his Table 3.20 (p. 102) – "assumptions on recovery rates were directly
coordinated with those on assignment for use as feed". WHEP does not use
Table 3.20: the feed half comes from `residue_feed_fraction.csv` (Smil
1999, Lal 2005, Krausmann 2008, Erenstein 2014, McIntire 1992), whose
named values span 0.05 to 0.45 around a 0.20 global default. That is
well under the "some 33 percent of the amount generated" Wirsenius
reports for cereals straw and stover fed to animals (p. 177), and under
the livestock share of Smerald, Rahimi & Scheer (2023), *Scientific
Data* **10**:685,
[doi:10.1038/s41597-023-02587-0](https://doi.org/10.1038/s41597-023-02587-0)
. Re-anchoring it is **not** done here on purpose: the gross residue
base it multiplies is itself too high, so the two errors partly cancel
and fixing one alone would land further from the truth (whep#1132,
whep#1041).

## Examples

``` r
calculate_residue_destinies(
  tibble::tibble(
    item_prod_code = "15", residue_dm_t = 100,
    region_krausmann = "Western Europe", region_un_sub = "Western Europe"
  )
)
#> # A tibble: 1 × 8
#>   item_prod_code residue_dm_t region_krausmann region_un_sub  residue_feed_dm_t
#>   <chr>                 <dbl> <chr>            <chr>                      <dbl>
#> 1 15                      100 West Europe      Western Europe              10.5
#> # ℹ 3 more variables: residue_burn_dm_t <dbl>, residue_soil_dm_t <dbl>,
#> #   method_residue_destiny <chr>
```
