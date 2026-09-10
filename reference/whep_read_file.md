# Download, cache and read files

Used to fetch input files that are needed for the package's functions
and that were built in external sources and are too large to include
directly. This is a public function for transparency purposes, so that
users can inspect the original inputs of this package that were not
directly processed here.

If the requested file doesn't exist locally, it is downloaded from a
public link and cached before reading it. This is all implemented using
the [`pins`](https://pins.rstudio.com/index.html) package. It supports
multiple file formats and file versioning.

## Usage

``` r
whep_read_file(file_alias, type = "parquet", version = NULL)
```

## Arguments

- file_alias:

  Internal name of the requested file. You can find the possible values
  in the `alias` column of the
  [`whep_inputs`](https://eduaguilera.github.io/whep/reference/whep_inputs.md)
  dataset.

- type:

  The extension of the file that must be read. Possible values:

  - `parquet`: This is the default value for code efficiency reasons.

  - `csv`: Mainly available for those who want a more human-readable
    option. If the `parquet` version is available, this is useless
    because this function already returns the dataset in an `R` object,
    so the origin is irrelevant, and `parquet` is read faster.

  - `nc` / `nc4`: Returns the path to the downloaded NetCDF instead of
    its contents, because these grids are read lazily by `ncdf4`/`terra`
    and are far too large to materialise as a tibble.

  - `raw`: Returns the file path without any processing.

  Saving each file in both formats is for transparency and accessibility
  purposes, e.g., having to share the data with non-programmers who can
  easily import a CSV into a spreadsheet. You will most likely never
  have to set this option manually unless for some reason a file could
  not be supplied in e.g. `parquet` format but was in another one.

- version:

  The version of the file that must be read. Possible values:

  - `NULL`: This is the default value. A frozen version is chosen to
    make the code reproducible when the file has a registry version.
    Each release will have its own frozen versions. The version is the
    string that can be found in
    [`whep_inputs`](https://eduaguilera.github.io/whep/reference/whep_inputs.md)
    in the `version` column. A blank registry version requests the
    latest board version.

  - `"latest"`: This overrides the frozen version and instead fetches
    the latest one that is available. This might or might not match the
    frozen version.

  - Other: A specific version can also be used. For more details read
    the `version` column information from
    [`whep_inputs`](https://eduaguilera.github.io/whep/reference/whep_inputs.md).

## Value

A tibble with the dataset. Some information about each dataset can be
found in the code where it's used as input for further processing.

## Frozen predecessor-pipeline references

Four aliases in
[`whep_inputs`](https://eduaguilera.github.io/whep/reference/whep_inputs.md)
are not outputs of this package: `primary_prod`,
`commodity_balance_sheet`, `processing_coefs` and `feed_intake`. They
are the 2025-07-14 snapshot of the predecessor R-script pipeline this
package replaced, kept frozen so `inst/scripts/compare_global_whep.R`
can benchmark `whep` against it. No package function reads them, their
schema is the old one (CamelCase, name-keyed, no polity columns) and
they stop in 2021, so reading one warns.

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

## Examples

``` r
whep_read_file("read_example")
#> ℹ Fetching files for read_example...
#> # A tibble: 1 × 2
#>   col_1                col_2         
#>   <chr>                <chr>         
#> 1 I'm a sample dataset second version
whep_read_file("read_example", type = "parquet", version = "latest")
#> ℹ Fetching files for read_example...
#> # A tibble: 1 × 2
#>   col_1                col_2        
#>   <chr>                <chr>        
#> 1 I'm a sample dataset third version
whep_read_file(
  "read_example",
  type = "csv",
  version = "20250721T152646Z-ce61b"
)
#> ℹ Fetching files for read_example...
#> # A tibble: 1 × 2
#>   col_1                col_2        
#>   <chr>                <chr>        
#> 1 I'm a sample dataset first version
```
