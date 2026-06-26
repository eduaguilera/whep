# Attach a scope record to a result.

Store a scope record (from
[`footprint_scope()`](https://eduaguilera.github.io/whep/reference/footprint_scope.md))
on an object as an attribute, so the result carries its goal-and-scope
with it.

## Usage

``` r
attach_scope(x, scope)
```

## Arguments

- x:

  Any R object, typically a footprint tibble.

- scope:

  Scope tibble from
  [`footprint_scope()`](https://eduaguilera.github.io/whep/reference/footprint_scope.md).

## Value

`x`, unchanged except for an added `whep_scope` attribute.

## Examples

``` r
scope <- footprint_scope("cropland", "ha", "FABIO-MRIO")
result <- attach_scope(tibble::tibble(value = 1), scope)
get_scope(result)
#> # A tibble: 1 × 7
#>   stressor units method     boundary            allocation vintage limitations
#>   <chr>    <chr> <chr>      <chr>               <chr>      <chr>   <chr>      
#> 1 cropland ha    FABIO-MRIO cradle-to-farm-gate mass       NA      NA         
```
