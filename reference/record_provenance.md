# Record provenance for a reproducible result.

Capture the information needed to regenerate a result: the package
version (the code), the R version, the pinned versions of the input
datasets used, and a timestamp. Attach the record to an output with
[`attach_provenance()`](https://eduaguilera.github.io/whep/reference/attach_provenance.md)
so any number can be traced back to the exact inputs and code that
produced it.

Unknown aliases are an error rather than a silent omission, so a
provenance record never quietly drops an input it could not resolve.

## Usage

``` r
record_provenance(
  aliases = NULL,
  inputs = whep::whep_inputs,
  recorded_at = Sys.time()
)
```

## Arguments

- aliases:

  Optional character vector of input aliases to record. When `NULL`
  (default), every registered input is recorded.

- inputs:

  Tibble of registered inputs with `alias` and `version` columns.
  Defaults to
  [whep_inputs](https://eduaguilera.github.io/whep/reference/whep_inputs.md).

- recorded_at:

  Timestamp for the record. Defaults to the current time; pass a fixed
  value for reproducible output.

## Value

A tibble with one row per recorded input:

- `recorded_at`: When the record was made.

- `whep_version`: Installed package version (the code).

- `r_version`: R version.

- `input_alias`: Input dataset alias.

- `input_version`: Pinned version of that input.

## Examples

``` r
prov <- record_provenance(
  aliases = "bilateral_trade",
  recorded_at = as.POSIXct("2026-01-01", tz = "UTC")
)
prov
#> # A tibble: 1 × 5
#>   recorded_at         whep_version r_version input_alias     input_version      
#>   <dttm>              <chr>        <chr>     <chr>           <chr>              
#> 1 2026-01-01 00:00:00 0.3.0.9000   4.6.1     bilateral_trade 20250714T123347Z-2…
```
