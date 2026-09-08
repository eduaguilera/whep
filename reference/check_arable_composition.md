# Check where the arable-land extension changes composition mid-panel.

Report, per term and `area_code`, whether a term of the fallow-inclusive
arable land extension is present for part of a country's panel and
absent for the rest. Two terms switch off inside the published panel and
neither switch is a land-use change:

- **`fodder`** — the FAOSTAT fodder items (`Cat_1 == "Fodder_green"` and
  not grass, i.e. CBS `2000`-`2003`). FAOSTAT's fodder tonnage
  (`faostat-production-old`, production only) ends in 2013 and EU
  AgriDB, the only other source, ends in 2019, so a build reaching 2020
  has no fodder at all from that year on and every ordinary arable crop
  silently absorbs fodder's share of the country's arable land
  (whep#938).

- **`temp_grassland_netting`** — the temporary grassland netted out of
  the arable target. Modelled CBS 3002 comes from EU AgriDB alone, so it
  exists for 26 EU polities and stops at 2019; from 2020 the netting
  term is identically zero while FAO's arable land still contains
  temporary meadows, and the reconciliation changes method at the
  boundary (whep#937).

This is a diagnostic, not a correction: it flags the discontinuity so a
series is not read across it. The treatments live behind
[`build_fao_arable_fallow_extension()`](https://eduaguilera.github.io/whep/reference/build_fao_arable_fallow_extension.md)'s
`fodder_gap` and `temp_grassland_basis` arguments, whose defaults
reproduce the published behaviour.

## Usage

``` r
check_arable_composition(extension, items_prod_full = whep::items_prod_full)
```

## Arguments

- extension:

  Tibble of the arable/permanent land extension as returned by
  [`build_fao_arable_fallow_extension()`](https://eduaguilera.github.io/whep/reference/build_fao_arable_fallow_extension.md):
  `year`, `area_code`, `item_cbs_code`, `impact_u`, and optionally
  `temp_grassland_netted_ha`. The `temp_grassland_netting` term is
  reported only when that column is present.

- items_prod_full:

  Crosswalk used to classify `item_cbs_code` as perennial via
  `Herb_Woody`. Defaults to
  [items_prod_full](https://eduaguilera.github.io/whep/reference/items_prod_full.md).

## Value

A tibble with one row per `(term, area_code)`:

- `term`: `"fodder"` or `"temp_grassland_netting"`.

- `area_code`: the country.

- `panel_first_year`, `panel_last_year`: the years that country has
  arable rows for.

- `term_first_year`, `term_last_year`: the years the term is positive
  (`NA` when it never is).

- `n_years_absent`: panel years in which the term is absent.

- `break_year`: the first panel year after `term_last_year` with no term
  (`NA` when the term runs to the end of the panel, or never appears).

- `broken`: `TRUE` when the term is present in some panel year and
  absent in a later one.

- `never_present`: `TRUE` when the term is absent for the whole panel.

## Details

[`check_series_jumps()`](https://eduaguilera.github.io/whep/reference/check_series_jumps.md)
cannot find either break. A term does not fall to a small value at the
boundary, it stops having rows, and its `min_value` guard skips any pair
involving a zero, so a scan over the completed series would not flag it
either. Coverage, not a ratio, is what has to be checked.

## Examples

``` r
extension <- tibble::tribble(
  ~year, ~area_code, ~item_cbs_code, ~impact_u, ~temp_grassland_netted_ha,
  2019L, 10L, 2003L, 100, 50, # fodder mix, netting fires
  2019L, 10L, 2511L, 900, 50,
  2020L, 10L, 2511L, 1000, 0 # fodder gone, netting off
)
check_arable_composition(extension)
#> # A tibble: 2 × 10
#>   term                area_code panel_first_year panel_last_year term_first_year
#>   <chr>                   <int>            <int>           <int>           <int>
#> 1 fodder                     10             2019            2020            2019
#> 2 temp_grassland_net…        10             2019            2020            2019
#> # ℹ 5 more variables: term_last_year <int>, n_years_absent <int>,
#> #   break_year <int>, broken <lgl>, never_present <lgl>
```
