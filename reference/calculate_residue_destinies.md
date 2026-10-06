# Estimate the destinies of crop residues.

Splits crop residue dry matter into four destinies that sum to the total
residue: fed to livestock, used as livestock bedding, burned / removed
for fuel, and left on the field for soil incorporation.

## Usage

``` r
calculate_residue_destinies(
  x,
  method = c("recovery_regional", "shares"),
  bedding_fraction = 0,
  unmatched_recovery = c("report", "abort"),
  recovery = c("wirsenius", "legacy")
)
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

- bedding_fraction:

  Fraction of the recovered **non-feed** residue used as livestock
  bedding, one number in `[0, 1]`. Default `0`, which is unset rather
  than measured; see the Bedding section for why, and what a caller
  setting it must convert from.

- unmatched_recovery:

  What the `recovery_regional` method does with a row that reaches no
  recovery rate at all, because its crop carries no Krausmann category
  or its region label reaches no recovery region: `"report"` (default)
  keeps the historical all-to-soil treatment and warns with the row
  count and tonnage, `"abort"` refuses to continue. Ignored by the
  `"shares"` method.

- recovery:

  Which recovery-rate variant the `recovery_regional` method reads:
  `"wirsenius"` (default since whep#1330, every rate Wirsenius 2000
  states, at the value it states) or `"legacy"` (the table as shipped
  before whep#1163). See the Two recovery variants section. Ignored by
  the `"shares"` method.

## Value

The input tibble with `residue_feed_dm_t`, `residue_bedding_dm_t`,
`residue_burn_dm_t`, `residue_soil_dm_t`, `residue_bedding_fraction` and
`method_residue_destiny`, and `method_residue_recovery` (the `recovery`
variant, `NA` for the `"shares"` method). The `"recovery_regional"`
method also returns `residue_recovery_matched`, `FALSE` where no
recovery rate was found, which is what separates a rate the table gives
as zero from a zero standing in for a failed lookup.

## Bedding

Bedding straw leaves the field with the rest of the recovered residue
and comes back to the soil later, through the yard, as part of the
managed manure. It is therefore carved out of the recovered **non-feed**
residue – the mass the commodity balance books as `other_uses` – and
**never** out of `residue_soil_dm_t`, which is the residue that stays on
the field. That is the split IPCC 2019 Refinement Vol. 4 Ch. 10 p. 10.95
asks for when it tells inventory compilers to cross-check bedding
nitrogen "relative to the amount of agricultural residues that is
removed for other purposes (i.e. bedding) other than the amount of
agricultural residues returned to soils or burnt", so as "to eliminate
the possibility of double counting".

`bedding_fraction` defaults to **0**, and that default is *unset, not
measured*: no global bedding-only fraction of crop residue could be
sourced (whep#1005). FAO GLEAM's `FracRemove` and IPCC 2019 Eq. 11.6's
`FracRemove` both merge bedding with feed and construction into one
term. Three partial anchors exist and none is on this function's
denominator, so each needs converting before it can be used here:

- Wirsenius (2000), PhD thesis, Chalmers University of Technology, Table
  3.21 p. 126 – litter is 14% of *distributed* cereal straw and stover
  and 11% of distributed crop by-products. The author grades these "very
  rough", and the South & Central Asia cattle entry is 0 because the
  data were absent, which must not be inherited as an estimate.

- Statistics Denmark HALM/HALM1/HALM2 – the only official statistic with
  a bedding-only column: 16-21% of straw *production*, about 30% of
  *removed* straw.

- Bentsen, Felby & Thorsen (2014), Prog. Energy Combust. Sci. 40:59-73,
  Table 5 – Denmark, barley 16% and wheat 11% of *production*.

## Where the recovery rates come from

The `recovery_regional` recovery rates live in
`inst/extdata/coefs/residue_recovery.csv`. Its numeric columns are
sourced separately and carry a provenance column each (`source_ratio`,
`source_recovery` and `source_recovery_wirsenius`), because they agree
with the source to different degrees. All are **Wirsenius (2000)**,
*Human Use of Land and Organic Materials*, PhD thesis, Chalmers
University of Technology – Table 3.17 (recovery rates, p. 94) and Table
3.16 (harvest index, p. 92, from which `residue_dm_product_dm` is the
residue:product ratio rounded to one decimal). The file was named after
Krausmann and its two key columns still are, but no coefficient in it is
Krausmann's. Only the crop category (`items_prod_full$Cat_Krausmann`)
really is his: `region_krausmann` here holds `regions_full$region_HANPP`
labels, whose eight values are Wirsenius's eight regions (whep#1132).

Table 3.17 states its rates per crop **category**, not per crop: one
"Cereals straw & stover" row governs every cereal, and one "Sugar crops
tops & leaves" row governs both crops Table 3.16 files under sugar
crops, cane and beet. Read that way it governs twelve of the twenty
categories here, and `source_recovery` splits them (whep#1150):

- nine match it cell for cell;

- three sit **below** it – `Groundnuts in Shell`, `Sugar Beets` and
  `Sugar Crops nes`, each against 0.90 in every region;

- three – roots and tubers, cassava and oil palm – are residues the
  thesis does model (Table 2.6, pp. 48-49: cassava leaves and tops,
  potato tops, oil palm leaves and trunks) but Table 3.17 does not list,
  and for those Wirsenius states that recovery rates "were assumed to be
  close to 100 percent" (p. 94);

- five – dry beans, pulses, castor beans, permanent crops and fodder
  crops – have **no residue flow in the thesis at all**: Table 2.6 gives
  pulses, fruits, tree nuts, vegetables and stimulants "no
  representation of by-products" (p. 47), models forage crops whole, and
  has no castor. The p. 94 default does not reach them, so the source is
  silent on them (whep#1163).

## Two recovery variants

`recovery =` selects the rate column, and `method_residue_recovery`
records which one was used:

- `"wirsenius"` (default) reads `recovery_rates_wirsenius`: every rate
  the thesis states, at the value it states. The three below-source
  categories take 0.90, the three p. 94 categories take 1.00 – "close to
  100 percent" read as 1.00, which is a reading of the text and not a
  number it prints – and the five categories the source is silent on
  keep the legacy assumed rate. `source_recovery_wirsenius` labels each.

- `"legacy"` reads `recovery_rates`, the table as shipped before
  whep#1163, whose departures from the source are all downward.

`"wirsenius"` is the only variant in which every rate is traceable to
the cited source, and it is the default since whep#1330. Its switch
waited on the gross residue base these rates multiply, once thought to
be about 36% too high (step 2 of whep#1132). In dry matter that excess
is not there: the 36% compared the pin's fresh weight with dry-matter
literature, and the pin's cereal residue in dry matter lies inside the
three-method band of Smerald, Rahimi & Scheer (2023),
[doi:10.1038/s41597-023-02587-0](https://doi.org/10.1038/s41597-023-02587-0)
, in every year 1997–2021, its 1997–2021 mean 4.8% below theirs
(`validation/residue_base_dm.R`, whep#1330). That check covers cereals
only, and the rates the switch moves are all non-cereal; no published
global total for the non-cereal residue base was found to check it
against.

Measured on the `crop_residues` pin as read by
[`get_primary_residues()`](https://eduaguilera.github.io/whep/reference/get_primary_residues.md),
on its fresh `value` (the commodity balance's basis, whep#1330),
`"wirsenius"` against `"legacy"` in 2010 raises recovered residue from
6341 to 6463 Mt fresh matter (+1.9%), the feed destiny from 1852 to 1891
Mt (+2.1%) and the burned/other-use destiny from 4489 to 4572 Mt
(+1.8%), and lowers the soil destiny from 1294 to 1172 Mt (-9.4%); over
1961–1965 the same moves are +3.5%, +3.4%, +3.6% and -15.2%. Roots and
tubers, cassava, sugar beet and groundnut are the only categories that
move.

Three categories move no mass at all today, which is why the fodder rate
of 0 is not the live problem it looks like: `Sugar Crops nes` and
`Oil Palm Fruit` are in the table and in no production item, and no
`Fodder crops` item reaches the residue pin.

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
. Re-anchoring it is **not** done here: it was held back because the
gross residue base it multiplies was thought too high (whep#1132,
whep#1041), which in dry matter it is not for cereals (whep#1330), so
that re-anchoring is now a choice of its own.

## Examples

``` r
calculate_residue_destinies(
  tibble::tibble(
    item_prod_code = "15", residue_dm_t = 100,
    region_krausmann = "Western Europe", region_un_sub = "Western Europe"
  )
)
#> # A tibble: 1 × 12
#>   item_prod_code residue_dm_t region_krausmann region_un_sub 
#>   <chr>                 <dbl> <chr>            <chr>         
#> 1 15                      100 West Europe      Western Europe
#> # ℹ 8 more variables: residue_recovery_matched <lgl>, residue_feed_dm_t <dbl>,
#> #   residue_burn_dm_t <dbl>, residue_soil_dm_t <dbl>,
#> #   residue_bedding_dm_t <dbl>, residue_bedding_fraction <dbl>,
#> #   method_residue_destiny <chr>, method_residue_recovery <chr>
```
