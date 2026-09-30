# Crop sources for the irrigated:rainfed regime yield ratio

Gives every primary crop production item the two sources of its
irrigated:rainfed yield ratio: the SPAM2010 crop whose irrigated and
all-rainfed yields set the ratio's level (read with
[`read_spam_yields()`](https://eduaguilera.github.io/whep/reference/read_spam_yields.md)),
and the LPJmL crop functional type whose band yields supply its
year-to-year anomaly (read with
[`read_lpjml_regime_yield()`](https://eduaguilera.github.io/whep/reference/read_lpjml_regime_yield.md)).
Items that neither source classifies get a stand-in, so every item has
both sources; each `*_basis` column says how close the stand-in is, and
`rationale` says why it was chosen.

The items are every row of
[cft_mapping](https://eduaguilera.github.io/whep/reference/cft_mapping.md)
and every `"Primary crops"` item of
[items_prod_full](https://eduaguilera.github.io/whep/reference/items_prod_full.md)
except fallow, which together hold every item that carries harvested
area in
[`build_primary_production()`](https://eduaguilera.github.io/whep/reference/build_primary_production.md)
or in the gridded land use.

## Usage

``` r
regime_yield_crop_mapping
```

## Format

A tibble with one row per production item. Columns:

- `item_prod_code`: Integer FAOSTAT item code, the join key.

- `item_prod_name`: Human-readable item name, for reading only.

- `spam_crop`: SPAM2010 v2.0 crop code (one of its 42, e.g. `"whea"`),
  or several joined by an operator whose meaning `spam_basis` fixes (see
  the section "Reading `spam_crop`").

- `spam_basis`: How the SPAM crop relates to the item: `"direct"` (it is
  the item), `"direct_aggregate"` (SPAM itself lists the item in that
  multi-crop aggregate), `"group_proxy"` (SPAM does not list the item;
  the aggregate of its crop group stands in), `"proxy"` (a different
  crop stands in, e.g. grain maize for forage maize),
  `"composite_weighted"` (an area-weighted mean over several SPAM crops'
  ratios) or `"product_dominance"` (one of two SPAM crops, chosen per
  country).

- `lpjml_cft`: LPJmL crop functional type, spelled as in
  [cft_mapping](https://eduaguilera.github.io/whep/reference/cft_mapping.md)'s
  `cft_lpjml`: one of the twelve crop CFTs or `"others"`. Equal to
  `cft_lpjml` for every item
  [cft_mapping](https://eduaguilera.github.io/whep/reference/cft_mapping.md)
  classifies.

- `lpjml_basis`: As `spam_basis`, for the LPJmL CFT. Items on LPJmL's
  generic `"others"` stand are `"group_proxy"`.

- `rationale`: Why each non-direct source was chosen. The weakest
  stand-ins, the fodder crops, are marked `WEAK`.

## Source

SPAM crops from Table S3 of the supplement to Yu, Q. et al. (2020). A
cultivated planet in 2010 – Part 2: The global gridded
agricultural-production maps. Earth System Science Data 12, 3545-3572.
[doi:10.5194/essd-12-3545-2020](https://doi.org/10.5194/essd-12-3545-2020)
. LPJmL crop functional types from
[cft_mapping](https://eduaguilera.github.io/whep/reference/cft_mapping.md).
Stand-ins for the remaining items chosen by WHEP.

## Reading `spam_crop`

The ratio meant here is the irrigated yield over the all-rainfed yield,
each yield being production over harvested area. A single code is that
crop's ratio. Joined codes mean one of three things:

- `+` with `spam_basis` `"direct"`: `"pmil+smil"` (millet) and
  `"acof+rcof"` (coffee). SPAM splits one FAO item in two by national
  shares, so the codes are **pooled**: harvested area and production are
  summed over both codes per regime, and the ratio is computed from the
  sums.

- `+` with `spam_basis` `"composite_weighted"`: the forage grasses
  (`"whea+barl+ocer"`, items 638, 639, 645, 651, 996) and forage legumes
  (`"bean+chic+cowp+pige+lent+opul+rest"`, items 640, 641, 643). Each
  code's ratio is computed on its own, and the item's ratio is the
  **area-weighted mean of those ratios**, weighted by each crop's
  SPAM2010 harvested area (irrigated plus rainfed) in the country, or by
  its global harvested area where the country grows none of them.

- `|` with `spam_basis` `"product_dominance"`: Linum (772) and Hemp
  (776), `"ooil|ofib"`. WHEP books one harvested area for the seed (SPAM
  `ooil`) and the fibre (SPAM `ofib`), so **one** of the two is used per
  country: the aggregate of whichever product has the larger FAOSTAT
  production there.

## Examples

``` r
head(regime_yield_crop_mapping)
#> # A tibble: 6 × 7
#>   item_prod_code item_prod_name spam_crop spam_basis       lpjml_cft lpjml_basis
#>            <int> <chr>          <chr>     <chr>            <chr>     <chr>      
#> 1             15 Wheat          whea      direct           temperat… direct_agg…
#> 2             27 Rice           rice      direct           rice      direct     
#> 3             44 Barley         barl      direct           temperat… direct_agg…
#> 4             56 Maize (corn)   maiz      direct           maize     direct     
#> 5             71 Rye            ocer      direct_aggregate temperat… direct_agg…
#> 6             75 Oats           ocer      direct_aggregate temperat… direct_agg…
#> # ℹ 1 more variable: rationale <chr>
```
