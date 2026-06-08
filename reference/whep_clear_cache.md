# Clear the build pipeline cache

Removes cached results from
[`build_primary_production()`](https://eduaguilera.github.io/whep/reference/build_primary_production.md),
[`build_commodity_balances()`](https://eduaguilera.github.io/whep/reference/build_commodity_balances.md),
and
[`build_processing_coefs()`](https://eduaguilera.github.io/whep/reference/build_processing_coefs.md)
so that the next call rebuilds from scratch.

## Usage

``` r
whep_clear_cache()
```

## Value

Invisible `NULL`.

## Examples

``` r
whep_clear_cache()
#> ✔ Build cache cleared.
```
