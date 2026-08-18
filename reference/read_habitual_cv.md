# Read FAOSTAT's coefficient of variation of habitual caloric consumption.

Reads item 21058 from the FAOSTAT Suite of Food Security Indicators bulk
download, resolved to WHEP area codes. The file is located in order: the
`dir` argument, then `WHEP_FAOSTAT_FS_DIR`, then a cache under
`rappdirs::user_cache_dir("whep")`, downloading on first use.

As with the UN WPP reader, the size and MD5 recorded here are WHEP's
own: FAOSTAT publishes no checksum for its bulk files, so a mismatch
means the upstream release changed, not necessarily that the download
failed.

## Usage

``` r
read_habitual_cv(years = NULL, data = NULL, dir = NULL)
```

## Arguments

- years:

  Optional integer vector of years to keep.

- data:

  Optional pre-read table, bypassing the file. Used by the tests.

- dir:

  Optional directory holding the bulk zip.

## Value

A tibble with `year`, `area_code`, `cv`.

## Examples

``` r
read_habitual_cv(
  data = tibble::tribble(
    ~`Area Code`, ~`Item Code`, ~Year, ~Value,
    2L,           21058L,       2010L, 0.25
  )
)
#> # A tibble: 1 × 3
#>    year area_code    cv
#>   <int>     <int> <dbl>
#> 1  2010         2  0.25
```
