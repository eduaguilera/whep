# Retrieve a result's provenance record.

Return the provenance record attached by
[`attach_provenance()`](https://eduaguilera.github.io/whep/reference/attach_provenance.md),
or `NULL` when none is present.

## Usage

``` r
get_provenance(x)
```

## Arguments

- x:

  An object that may carry a `whep_provenance` attribute.

## Value

The provenance tibble, or `NULL`.

## Examples

``` r
get_provenance(tibble::tibble(value = 1))
#> NULL
```
