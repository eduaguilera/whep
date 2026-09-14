# External inputs

The information needed for accessing external datasets used as inputs in
our modeling.

## Usage

``` r
whep_inputs
```

## Format

A tibble where each row corresponds to one external input dataset. It
contains the following columns:

- `alias`: An internal name used to refer to this dataset, which is the
  expected name when trying to get the dataset with
  [`whep_read_file()`](https://eduaguilera.github.io/whep/reference/whep_read_file.md).

- `board_url`: The public static URL where the data is found, following
  the concept of a *board* from the
  [`pins`](https://pins.rstudio.com/index.html) package, which is what
  we use for storing these input datasets.

- `version`: The specific version of the dataset, as defined by the
  `pins` package. The version is a string similar to
  `"20250714T123343Z-114b5"`. This version is the one used by default if
  no `version` is specified when calling
  [`whep_read_file()`](https://eduaguilera.github.io/whep/reference/whep_read_file.md).
  If you want to use a different one, you can find the available
  versions of a file by using
  [`whep_list_file_versions()`](https://eduaguilera.github.io/whep/reference/whep_list_file_versions.md).

## Source

Created by the package authors.

## Frozen predecessor-pipeline references

Four aliases in `whep_inputs` are not outputs of this package:
`primary_prod`, `commodity_balance_sheet`, `processing_coefs` and
`feed_intake`. They are the 2025-07-14 snapshot of the predecessor
R-script pipeline this package replaced, kept frozen so
`inst/scripts/compare_global_whep.R` can benchmark `whep` against it. No
package function reads them, their schema is the old one (CamelCase,
name-keyed, no polity columns) and they stop in 2021, so reading one
warns.

Their numbers are not what current code produces, so they are
references, not substitutes. `primary_prod` (2,443,516 rows, 1961–2021)
against a fresh
[`build_primary_production()`](https://eduaguilera.github.io/whep/reference/build_primary_production.md)
over the same 2015–2021 window:

- The pin has no `slaughtered_heads` row in any of its years; the build
  emits 12,195 over 2015–2021. Three `item_prod` codes are pin-only
  (378, 773, 1163; 1,332 rows) and three appear in no pin year at all
  (1051, and the modelled grassland items 3001 and 3002).

- 317,587 rows against the build's 328,242 for the window, a 3.2%
  shortfall.

- Its 2020–2021 fodder harvested area is carried forward from 2019 —
  85.93 Mha in each year, 468 country-item series over 96 areas, equal
  to 2019 to the last digit — where the build emits no fodder row at all
  after 2019, because `eu-agridb-fodder` stops in 2019,
  `faostat-production-old` in 2013, and `faostat-production` carries
  none of the 16 fodder items.

- Even in a year both cover they disagree: 2019 fodder harvested area is
  85.93 Mha in the pin against 90.42 Mha from the build.

Build the current series with the matching `build_*()` or `get_*()`
function instead.

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

Read by
[`get_primary_residues()`](https://eduaguilera.github.io/whep/reference/get_primary_residues.md),
and from there by
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

It is not a small input.
[`get_primary_residues()`](https://eduaguilera.github.io/whep/reference/get_primary_residues.md)
supplies 7.63 Gt to the 2010 commodity balance (Straw 3.60 Gt, Other
crop residues 2.49 Gt, Firewood 1.54 Gt) and 327.7 Gt over 1961–2021,
and 3,998 of its 249,095 output rows carry `NA` polity columns because
the pin is name-keyed.

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
