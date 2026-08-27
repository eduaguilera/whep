# Check how much arable land the fallow split attributes to fodder crops.

Report, per `(year, area_code)`, the share of the reconciled arable land
extension that lands on the FAOSTAT fodder items (commodity-balance
items of `Cat_1 == "Fodder_green"` that are not grass, i.e.
`2000`-`2003`).

With `fallow_weights = NULL` — the default, and the path the
land-balance footprint
([`build_land_balance_footprint()`](https://eduaguilera.github.io/whep/reference/build_land_balance_footprint.md))
takes —
[`build_fao_arable_fallow_extension()`](https://eduaguilera.github.io/whep/reference/build_fao_arable_fallow_extension.md)
rescales every arable crop's cropped physical area proportionally up to
FAO Arable land. Each crop's share of the output is therefore exactly
its share of the input harvested area, so an inflated fodder harvested
area is passed straight through into the published per-crop arable land
footprint, taking land away from the ordinary crops (whep#356). Fodder
harvested area is reconstructed rather than surveyed (dry-matter yield
imputation, EU AgriDB splicing, linear filling), so it is the term most
likely to be wrong, and in some country-years it alone exceeds the
country's whole FAO arable land.

This is a diagnostic, not a correction: nothing is rescaled or dropped.
It flags where the attribution is implausible so the fodder
reconstruction can be inspected, or agro-climatic `fallow_weights` (see
[`gridded_fallow_weights()`](https://eduaguilera.github.io/whep/reference/gridded_fallow_weights.md))
supplied instead.

## Usage

``` r
check_fodder_land_share(
  extension,
  threshold = 0.5,
  items_prod_full = whep::items_prod_full
)
```

## Arguments

- extension:

  Tibble of the arable/permanent land extension with columns `year`,
  `area_code`, `item_cbs_code` and `impact_u`, as returned by
  [`build_fao_arable_fallow_extension()`](https://eduaguilera.github.io/whep/reference/build_fao_arable_fallow_extension.md).

- threshold:

  Share of arable land above which a `(year, area_code)` is flagged
  (default `0.5`). Fodder is real land use, so a moderate share is
  expected; half a country's arable land is not.

- items_prod_full:

  Crosswalk used to classify `item_cbs_code` as perennial via
  `Herb_Woody`. Defaults to
  [items_prod_full](https://eduaguilera.github.io/whep/reference/items_prod_full.md).

## Value

A tibble with one row per `(year, area_code)` that has at least one
arable row in `extension`, ordered by descending `fodder_share`:

- `year`, `area_code`: the country-year.

- `fodder_ha`: arable land attributed to fodder items.

- `arable_ha`: total arable (non-perennial) land attributed.

- `fodder_share`: `fodder_ha / arable_ha` (`NA` when `arable_ha` is
  zero).

- `flagged`: `TRUE` when `fodder_share > threshold`.

## Examples

``` r
extension <- tibble::tribble(
  ~year, ~area_code, ~item_cbs_code, ~impact_u,
  2000L, 10L, 2003L, 700, # fodder mix
  2000L, 10L, 2511L, 300 # wheat
)
check_fodder_land_share(extension)
#> # A tibble: 1 × 6
#>    year area_code fodder_ha arable_ha fodder_share flagged
#>   <int>     <int>     <dbl>     <dbl>        <dbl> <lgl>  
#> 1  2000        10       700      1000          0.7 TRUE   
```
