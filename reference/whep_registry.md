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
  ending in `/_pins.yaml` for public rows. Restricted rows instead store
  a relative path such as `restricted:folder/_pins.yaml`, resolved
  against the user's configured restricted board.

- `version`: the frozen pins version read by default, such as
  `"20250714T123343Z-114b5"`. A blank cell or `"latest"` reads the
  newest version on the board.

Optional `access` is `"public"` or `"restricted"`; omitting the column
retains the public registry interface. `public_alternative` may suggest
another alias but never causes an automatic fallback. Restricted reads
and version listings use the supplied registry, including when its alias
also exists in this package. See
[`whep_data_access_status()`](https://eduaguilera.github.io/whep/reference/whep_data_access_status.md)
for setup.

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
#> # A tibble: 90 × 5
#>    alias                   board_url           version access public_alternative
#>    <chr>                   <chr>               <chr>   <chr>  <chr>             
#>  1 commodity_balance_sheet https://saco.csic.… 202507… public NA                
#>  2 bilateral_trade         https://saco.csic.… 202610… public NA                
#>  3 processing_coefs        https://saco.csic.… 202507… public NA                
#>  4 feed_intake             https://saco.csic.… 202507… public NA                
#>  5 primary_prod            https://saco.csic.… 202507… public NA                
#>  6 crop_residues           https://saco.csic.… 202507… public NA                
#>  7 read_example            https://saco.csic.… 202507… public NA                
#>  8 faostat-production      https://saco.csic.… 202603… public NA                
#>  9 faostat-production-old  https://saco.csic.… 202603… public NA                
#> 10 faostat-landuse         https://saco.csic.… 202610… public NA                
#> # ℹ 80 more rows
```
