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
