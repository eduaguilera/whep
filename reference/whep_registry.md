# Read a registry of pinned input files

Reads and validates a registry CSV, so another package or project can
fetch its own pinned inputs with
[`whep_read_file()`](https://eduaguilera.github.io/whep/reference/whep_read_file.md)
and
[`whep_list_file_versions()`](https://eduaguilera.github.io/whep/reference/whep_list_file_versions.md)
instead of the ones listed in
[`whep_inputs`](https://eduaguilera.github.io/whep/reference/whep_inputs.md).

A registry has one row per input and the same columns as
[`whep_inputs`](https://eduaguilera.github.io/whep/reference/whep_inputs.md):

- `alias`: the name the input is read by. It must be non-empty and
  unique.

- `board_url`: the public link to the `_pins.yaml` manifest of the
  [`pins`](https://pins.rstudio.com/index.html) board holding it. It
  must be an `https://saco.csic.es/public.php/dav/files/` public link
  ending in `/_pins.yaml`, as every board in
  [`whep_inputs`](https://eduaguilera.github.io/whep/reference/whep_inputs.md)
  is.

- `version`: the frozen pins version read by default, such as
  `"20250714T123343Z-114b5"`. A blank cell or `"latest"` reads the
  newest version on the board.

Further columns, such as a description, are kept. Every column is read
as text, so a version string is never reinterpreted.

## Usage

``` r
whep_registry(path)
```

## Arguments

- path:

  Path to the registry CSV.

## Value

A tibble with one row per input, ready to pass as the `registry`
argument of
[`whep_read_file()`](https://eduaguilera.github.io/whep/reference/whep_read_file.md)
and
[`whep_list_file_versions()`](https://eduaguilera.github.io/whep/reference/whep_list_file_versions.md).

## Examples

``` r
# This package's own registry is itself a valid registry.
system.file("extdata", "whep_inputs.csv", package = "whep") |>
  whep_registry()
#> # A tibble: 86 × 3
#>    alias                   board_url                                     version
#>    <chr>                   <chr>                                         <chr>  
#>  1 commodity_balance_sheet https://saco.csic.es/public.php/dav/files/nr… 202507…
#>  2 bilateral_trade         https://saco.csic.es/public.php/dav/files/nr… 202610…
#>  3 processing_coefs        https://saco.csic.es/public.php/dav/files/nr… 202507…
#>  4 feed_intake             https://saco.csic.es/public.php/dav/files/nr… 202507…
#>  5 primary_prod            https://saco.csic.es/public.php/dav/files/nr… 202507…
#>  6 crop_residues           https://saco.csic.es/public.php/dav/files/nr… 202507…
#>  7 read_example            https://saco.csic.es/public.php/dav/files/nr… 202507…
#>  8 faostat-production      https://saco.csic.es/public.php/dav/files/nr… 202603…
#>  9 faostat-production-old  https://saco.csic.es/public.php/dav/files/nr… 202603…
#> 10 faostat-landuse         https://saco.csic.es/public.php/dav/files/nr… 202610…
#> # ℹ 76 more rows
```
