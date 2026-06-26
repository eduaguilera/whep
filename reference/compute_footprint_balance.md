# Compute land footprints by physical trade balance.

Estimate consumption-based land footprints by propagating direct land
use through the bilateral physical trade network, an approach
independent of the Leontief inverse used by
[`compute_footprint()`](https://eduaguilera.github.io/whep/reference/compute_footprint.md).
Each country's supply pool (production plus imports) carries an
embodied-land intensity `s`; solving the balance \\(D - M) s = L\\ per
item gives `s`, where `D` is diagonal supply throughput, `M` routes
import intensities, and `L` is direct land. The footprint of consumption
(supply minus exports) is then `consumption * s`.

Unlike the multi-regional input-output method, this tracer handles only
the trade dimension, not inter-product processing transformations.
Comparing the two with
[`compare_footprint_methods()`](https://eduaguilera.github.io/whep/reference/compare_footprint_methods.md)
isolates the effect of that assumption – an apples-to-apples stress test
on shared FAOSTAT data.

## Usage

``` r
compute_footprint_balance(production, trade, extension)
```

## Arguments

- production:

  Tibble with `area_code`, `item_cbs_code` and `value` (quantity
  produced).

- trade:

  Tibble with `from_code`, `to_code`, `item_cbs_code` and `value`
  (quantity exported from `from_code` to `to_code`).

- extension:

  Tibble with `area_code`, `item_cbs_code` and `value` (direct land use
  of domestic production).

## Value

A tibble with `area_code` (consuming country), `item_cbs_code`, `value`
(embodied land in consumption) and `method` (`"land_balance"`).

## Examples

``` r
production <- tibble::tibble(
  area_code = c(1L, 2L),
  item_cbs_code = c(10L, 10L),
  value = c(100, 0)
)
trade <- tibble::tibble(
  from_code = 1L, to_code = 2L, item_cbs_code = 10L, value = 40
)
extension <- tibble::tibble(
  area_code = c(1L, 2L),
  item_cbs_code = c(10L, 10L),
  value = c(50, 0)
)
compute_footprint_balance(production, trade, extension)
#> # A tibble: 2 × 4
#>   area_code item_cbs_code value method      
#>       <int>         <int> <dbl> <chr>       
#> 1         1            10    30 land_balance
#> 2         2            10    20 land_balance
```
