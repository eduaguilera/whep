# Input file versions

Lists all existing versions of an input file from
[`whep_inputs`](https://eduaguilera.github.io/whep/reference/whep_inputs.md).

## Usage

``` r
whep_list_file_versions(file_alias)
```

## Arguments

- file_alias:

  Internal name of the requested file. You can find the possible values
  in the
  [`whep_inputs`](https://eduaguilera.github.io/whep/reference/whep_inputs.md)
  dataset.

## Value

A tibble where each row is a version. For details about its format, see
[`pins::pin_versions()`](https://pins.rstudio.com/reference/pin_versions.html).

## Examples

``` r
whep_list_file_versions("read_example")
#> # A tibble: 3 × 3
#>   version                created             hash 
#>   <chr>                  <dttm>              <chr>
#> 1 20250721T152646Z-ce61b 2025-07-21 15:26:46 ce61b
#> 2 20250721T152756Z-f00da 2025-07-21 15:27:56 f00da
#> 3 20250721T152854Z-7cc8f 2025-07-21 15:28:54 7cc8f
```
