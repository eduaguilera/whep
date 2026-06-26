# Retrieve a result's scope record.

Return the scope record attached by
[`attach_scope()`](https://eduaguilera.github.io/whep/reference/attach_scope.md),
or `NULL` when none is present.

## Usage

``` r
get_scope(x)
```

## Arguments

- x:

  An object that may carry a `whep_scope` attribute.

## Value

The scope tibble, or `NULL`.

## Examples

``` r
get_scope(tibble::tibble(value = 1))
#> NULL
```
