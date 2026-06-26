# Describe the scope of a footprint result.

Build a machine-readable scope record for a footprint: what is measured,
in which units, by which method, and under which system boundary,
allocation rule, data vintage and known limitations. This is the ISO
14044 goal-and-scope made into an attachable data object rather than
prose, so a footprint result always travels with the assumptions behind
it.

Attach it to a result with
[`attach_scope()`](https://eduaguilera.github.io/whep/reference/attach_scope.md)
and read it back with
[`get_scope()`](https://eduaguilera.github.io/whep/reference/get_scope.md).

## Usage

``` r
footprint_scope(stressor, units, method, details = list())
```

## Arguments

- stressor:

  What is measured, e.g. `"cropland"`.

- units:

  Units of the footprint value, e.g. `"ha"`.

- method:

  Estimation method used, e.g. `"FABIO-MRIO"`. This mirrors the
  multi-method `method_<quantity>` columns recorded elsewhere in the
  package.

- details:

  Optional named list overriding any of: `boundary` (system boundary),
  `allocation` (allocation rule), `vintage` (data years) and
  `limitations` (free text).

## Value

A one-row tibble with columns `stressor`, `units`, `method`, `boundary`,
`allocation`, `vintage` and `limitations`.

## Examples

``` r
footprint_scope(
  stressor = "cropland",
  units = "ha",
  method = "FABIO-MRIO",
  details = list(vintage = "1850-2023", allocation = "mass")
)
#> # A tibble: 1 × 7
#>   stressor units method     boundary            allocation vintage   limitations
#>   <chr>    <chr> <chr>      <chr>               <chr>      <chr>     <chr>      
#> 1 cropland ha    FABIO-MRIO cradle-to-farm-gate mass       1850-2023 NA         
```
