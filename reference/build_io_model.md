# Build multi-regional input-output model.

Construct a multi-regional input-output (MRIO) model from supply-use
tables, bilateral trade, and commodity balance sheets. Uses the industry
technology assumption to derive symmetric product-by-product tables.

The resulting matrices follow the FABIO methodology (Bruckner et al.,
2019). Rows and columns of `Z` represent (country, item) pairs. Each
entry `Z[i,j]` gives the intermediate flow from sector `i` to sector
`j`.

## Usage

``` r
build_io_model(
  supply_use = NULL,
  bilateral_trade = NULL,
  cbs = NULL,
  years = NULL,
  endogenize_losses = FALSE,
  method = c("mass", "value"),
  prices = NULL
)
```

## Arguments

- supply_use:

  Tibble from
  [`build_supply_use()`](https://eduaguilera.github.io/whep/reference/build_supply_use.md).
  By default, this function calls
  [`build_supply_use()`](https://eduaguilera.github.io/whep/reference/build_supply_use.md)
  internally. Must have columns: `year`, `area_code`, `proc_group`,
  `proc_cbs_code`, `item_cbs_code`, `type`, `value`.

- bilateral_trade:

  Tibble from
  [`get_bilateral_trade()`](https://eduaguilera.github.io/whep/reference/get_bilateral_trade.md).
  By default, this function calls
  [`get_bilateral_trade()`](https://eduaguilera.github.io/whep/reference/get_bilateral_trade.md)
  internally. Must have columns: `year`, `item_cbs_code`,
  `bilateral_trade` (list-column of matrices).

- cbs:

  Tibble from
  [`get_wide_cbs()`](https://eduaguilera.github.io/whep/reference/get_wide_cbs.md).
  By default, this function calls
  [`get_wide_cbs()`](https://eduaguilera.github.io/whep/reference/get_wide_cbs.md)
  internally. Must have columns: `year`, `area_code`, `item_cbs_code`,
  `production`, `import`, `export`, `stock_withdrawal`,
  `stock_addition`, plus final demand columns (`food`, `other_uses`).

- years:

  Numeric vector of years to compute, or NULL. If NULL, computes all
  years in the intersection of available data across inputs. If
  specified, must be a subset of available years.

- endogenize_losses:

  Logical. If `TRUE` and `cbs` contains a `losses` column, losses are
  moved from final demand to the diagonal of `Z` (self-use), following
  the FABIO convention. The `losses` column is removed from Y and
  `fd_labels`. Defaults to `FALSE`.

- method:

  Co-product allocation method. `"mass"` (default) splits a multi-output
  process's inputs across its products by physical mass; `"value"`
  splits them by economic value (mass times export price), so high-value
  co-products (e.g. oil over cake, meat over hides) carry a larger share
  of upstream pressures. A process whose co-products lack usable prices
  falls back to mass.

- prices:

  Optional tibble of item prices as from
  [`build_cbs_prices()`](https://eduaguilera.github.io/whep/reference/build_cbs_prices.md)
  (`year`, `element`, `item_cbs_code`, `price`). Used only when
  `method = "value"`; built automatically when `NULL`.

## Value

A tibble with one row per year and list-columns:

- `Z`: Inter-industry flow matrix (product-by-product).

- `Y`: Final demand matrix.

- `X`: Total output vector.

- `labels`: Tibble mapping row/column indices to `area_code`,
  `item_cbs_code`, and reporting polity metadata.

- `fd_labels`: Tibble mapping each Y column to its `area_code`
  (consuming polity), `fd_col` (demand category, e.g. `"food"`), and
  reporting polity metadata. Pass to
  [`compute_footprint()`](https://eduaguilera.github.io/whep/reference/compute_footprint.md)
  as `fd_labels` to get a `target_fd` column in the footprint output.

## Examples

``` r
su <- build_supply_use(example = TRUE)
btd <- get_bilateral_trade(example = TRUE)
cbs <- get_wide_cbs(example = TRUE)
build_io_model(su, btd, cbs)
#> Warning: no non-missing arguments to min; returning Inf
#> Warning: no non-missing arguments to max; returning -Inf
#> ℹ Building IO model for 0 years.
#>   Year range: Inf--Inf.
#>   Final demand columns: food, other_uses, and stock_addition.
#> ✔ IO model complete.
#> # A tibble: 0 × 6
#> # ℹ 6 variables: year <dbl>, Z <list>, Y <list>, X <list>, labels <list>,
#> #   fd_labels <list>
```
