# Complete columns from a typed prototype.

Add absent columns to a tibble and safely cast present columns to the
types declared by a zero-row prototype. Missing columns receive typed
missing values unless a scalar default is supplied. Defaults never
replace missing values inside a column that is already present.

Prototype columns are returned first in prototype order. With
`extra = "keep"`, other input columns follow in their original relative
order. With `extra = "drop"`, the output has exactly the prototype
schema.

## Usage

``` r
ensure_columns(data, prototype, defaults = NULL, extra = c("keep", "drop"))
```

## Arguments

- data:

  Input tibble.

- prototype:

  Zero-row tibble defining required column names, types, and order.

- defaults:

  Optional named list of size-one defaults for absent columns. Every
  name must occur in `prototype`, and each value must be safely
  convertible to the corresponding prototype type.

- extra:

  Whether columns absent from `prototype` are `"keep"` or `"drop"`.

## Value

An ungrouped tibble with the same rows as `data`, completed and ordered
from `prototype`. Grouping metadata is not preserved.

## Examples

``` r
prototype <- tibble::tibble(
  year = integer(),
  value = double(),
  source = character()
)
data <- tibble::tibble(value = 2, year = 2020L, note = "observed")

ensure_columns(
  data,
  prototype,
  defaults = list(source = "unknown")
)
#> # A tibble: 1 × 4
#>    year value source  note    
#>   <int> <dbl> <chr>   <chr>   
#> 1  2020     2 unknown observed
```
