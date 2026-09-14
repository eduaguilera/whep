# Crop residue items

Get type and amount of residue produced for each crop production item.

## Usage

``` r
get_primary_residues(example = FALSE)
```

## Arguments

- example:

  If `TRUE`, return a small example output without downloading remote
  data. Default is `FALSE`.

## Value

A tibble with the crop residue data. It contains the following columns:

- `year`: The year in which the recorded event occurred.

- `area_code`: The code of the country where the data is from. For code
  details see e.g.
  [`add_area_name()`](https://eduaguilera.github.io/whep/reference/add_area_name.md).

- `item_cbs_code_crop`: FAOSTAT internal code for each commodity balance
  sheet item. This is the crop that is generating the residue.

- `item_cbs_code_residue`: FAOSTAT internal code for each commodity
  balance sheet item. This is the obtained residue. In the commodity
  balance sheet, this can be three different items right now:

  - `2105`: `Straw`

  - `2106`: `Other crop residues`

  - `2107`: `Firewood`

  These are actually not FAOSTAT defined items, but custom defined by
  us. When necessary, FAOSTAT codes are extended for our needs.

- `value`: The amount of residue produced, measured in tonnes.

## The two batch pins on the build path

Two further aliases were published in that same 2025-07-14 batch —
`crop_residues` and `bilateral_trade`, alongside the four above, between
12:33:43Z and 12:33:50Z. Unlike the four, these two are read on the
default build path, which is why no warning is attached to them: every
commodity balance build reads them, so a warning at the read is noise
rather than information.

They do **not** share a provenance despite sharing a timestamp. Each was
established separately (#1054), because the timestamp alone establishes
nothing.

### `crop_residues` is predecessor output

Read by `get_primary_residues()`, and from there by
[`build_commodity_balances()`](https://eduaguilera.github.io/whep/reference/build_commodity_balances.md).
All 475,688 of its `Product` rows equal the `primary_prod` pin's
`tonnes` values exactly — no key unmatched on either side, no value
differing at a relative tolerance of 1e-6 — so it is a downstream
artifact of the same predecessor run, carrying that run's production
series into the commodity balance.

Its residue quantities are those production numbers times a
residue-to-product ratio that varies by year and does not exist in this
repository: 100 of its 116 `Name_biomass` items carry between 77 and 252
distinct ratios across 1961–2021 (the 16 that carry one flat ratio are
all fodder items), 33,411 area-item-year keys carry a residue of exactly
0, and `biomass_coefs$kg_residue_kg_product_FM` reproduces only 3,189 of
the 472,790 keys where the comparison can be made. The ratios are
therefore not recoverable here, and the artifact is not reproducible
from this package.

It is not a small input. `get_primary_residues()` supplies 7.63 Gt to
the 2010 commodity balance (Straw 3.60 Gt, Other crop residues 2.49 Gt,
Firewood 1.54 Gt) and 327.7 Gt over 1961–2021, and 3,998 of its 249,095
output rows carry `NA` polity columns because the pin is name-keyed.

Driving the same residue model off a fresh
[`get_primary_production()`](https://eduaguilera.github.io/whep/reference/get_primary_production.md)
would move those numbers. Measured for 2010 at (`area_code`,
`item_prod`): 1.958 Gt of current production sits on 6,425 keys the pin
never sees (809 Mt of it primary crops, including the modelled
temporary-grassland item), 64.8 Mt on 296 keys is pin-only, and of the
8,010 shared keys 538 disagree, putting 1.102 Gt — 11.01% of the pin's
shared-key mass — more than 1% apart. The largest class is rice, 673.9
Mt in the pin against 463.3 Mt fresh, a ratio of 0.6876: current code
puts rice on a milled-equivalent basis while the pin's is paddy, which
is a basis difference rather than an error, since straw scales with the
field crop. The second is fodder, where the current build is close to
twice the pin on every forage and silage item, the pin predating that
work. Replacing the pin therefore means choosing a residue model, which
is a science decision and not a refresh.

### `bilateral_trade` is a curated FAOSTAT input

Read by
[`get_bilateral_trade()`](https://eduaguilera.github.io/whep/reference/get_bilateral_trade.md)
and by the live-animal branch of
[`build_commodity_balances()`](https://eduaguilera.github.io/whep/reference/build_commodity_balances.md).
Its values are the FAOSTAT Detailed Trade Matrix, not model output:
against the `faostat-trade-bilateral` pin over 2010, all 296,642 shared
`tonnes` keys and all 5,102 shared `Head` keys agree exactly, with not
one key differing. Over the full 1986–2021 span it carries
83,147,095,412 tonnes against FAOSTAT's 83,159,972,741 on CBS-mapped
quantity rows, and 11,707,083,640 head against 11,708,244,416.

The only transformations are an aggregation of FAOSTAT trade items onto
CBS item names, a fold of 18 FAOSTAT areas into area code 999 (Bhutan,
Comoros, Cook Islands, Equatorial Guinea, Faroe Islands, Marshall
Islands, Micronesia, Nauru, New Caledonia, North Macedonia, Niue,
Seychelles, Eswatini, Syrian Arab Republic, China Taiwan Province of,
Tonga, Tuvalu and Palestine — 3.18% of its rows, 1.892% of its tonnage,
0.492% of its head counts), and a `Country_share` column the reader
discards. So the shared timestamp implies nothing about it, and there is
nothing to regenerate: what it holds is what FAOSTAT published.

Two things it does not hold. FAOSTAT's `1000 Head` rows are absent
entirely — 89,073 rows and 76,141,882 thousand head over 1986–2021,
against the 11,707,083,640 head the pin does carry, all of it live
broiler chicken, turkey, duck, rabbit and goose trade — and so are its
5,011 `No` rows. Current code drops the same rows when it reads the raw
pin, so
[`build_detailed_trade()`](https://eduaguilera.github.io/whep/reference/build_detailed_trade.md)
would not recover them; the same class of unit was fixed for
`faostat-trade-totals` in \#865 and is still open here.

## Examples

``` r
get_primary_residues(example = TRUE)
#> # A tibble: 10 × 9
#>     year area_code polity_area_code reporting_polity_code reporting_polity_name 
#>    <dbl>     <dbl>            <int> <chr>                 <chr>                 
#>  1  2010       174              174 PRT-1800-2025         Portugal              
#>  2  1975        54               54 DNK-1920-2025         Denmark               
#>  3  1988        53               53 BEN-1960-2025         Benin                 
#>  4  2020       178              178 ERI-1993-2025         Eritrea               
#>  5  1972       131              131 MYS-1965-2025         Malaysia              
#>  6  2011         4                4 DZA-1962-2025         Algeria (1962-2025)   
#>  7  1965       144              144 MOZ-1891-1975         Mozambique (1891-1975)
#>  8  2018       167              167 CZE-1993-2025         Czechia               
#>  9  1994       109              109 JAM-1800-2025         Jamaica               
#> 10  1982       194              194 SAU-1924-2025         Saudi Arabia          
#> # ℹ 4 more variables: reporting_polity_has_geometry <lgl>,
#> #   item_cbs_code_crop <dbl>, item_cbs_code_residue <dbl>, value <dbl>
```
