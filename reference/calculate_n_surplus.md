# Calculate the gridded soil-surface nitrogen surplus.

Derives the nitrogen surplus from a
[`build_nitrogen_balance()`](https://eduaguilera.github.io/whep/reference/build_nitrogen_balance.md)
output. The default `"harvest_removal"` method is the standard
soil-surface nitrogen balance: net inputs (`n_input_std_t`) minus the
nitrogen exported in harvest (crop product, used or exported residue and
grazed forage), matching the basis of the Schulte-Uebbing et al. (2022)
critical nitrogen surplus. Field-burnt residue nitrogen is not
subtracted (it volatilises in place, a loss inside the surplus, not an
export), nor is recycled residue (returned to the field, internal). The
`"full_balance"` method instead returns the balance's post-loss
`n_balance_t`, a sensitivity alternative. The surplus may be negative (a
nitrogen deficit) and is not clamped here, as clamping is a boundary or
leaching concern. The full grid key (`lon`, `lat`, `area_code`,
`item_cbs_code`, `year`) and every balance column are preserved.

## Usage

``` r
calculate_n_surplus(
  balance,
  method = c("harvest_removal", "full_balance"),
  example = FALSE
)
```

## Arguments

- balance:

  A
  [`build_nitrogen_balance()`](https://eduaguilera.github.io/whep/reference/build_nitrogen_balance.md)
  output tibble. For `"harvest_removal"` it must carry `n_input_std_t`,
  `prod_n_t`, `used_residue_n_t` and `grazed_weeds_n_t`; for
  `"full_balance"` it must carry `n_balance_t`. When an `area_ha` column
  (each crop's harvested hectares in the cell) is present, the
  per-hectare surplus `surplus_kgn_ha` is also emitted.

- method:

  Surplus definition: `"harvest_removal"` (default,
  `n_input_std_t - (prod_n_t + used_residue_n_t + grazed_weeds_n_t)`) or
  `"full_balance"` (the balance's `n_balance_t`, a sensitivity).

- example:

  If `TRUE`, return a small fixture instead of computing from `balance`.
  Defaults to `FALSE`.

## Value

The `balance` tibble with `surplus_n_t` (tonnes N, may be negative),
`method_surplus` and, when `area_ha` is present, `surplus_kgn_ha` (kg N
per hectare). An area-keyed `balance` also gains the polity columns
below; one without an `area_code` is returned without them.

## Polity columns

Every area-keyed output carries the polity its `area_code` resolves to
in that row's year:

- `polity_area_code`: The numeric key rows are AGGREGATED on, for the
  matrix workflows. It is a bucket, not an identity: use
  `reporting_polity_code` to say which territory a row belongs to.

- `reporting_polity_code`: The polity itself, e.g. `ESP-1846-1914`. It
  is year-aware, so the same `area_code` resolves to different polities
  in different years, which is the point of the crosswalk.

- `reporting_polity_name`: Its name. It can differ from the area's own
  name where the area folds into an aggregate.

- `reporting_polity_has_geometry`: Whether the polity has a polygon in
  the WHEP polity database, for callers that need to map or intersect
  it. `FALSE` is a documented gap upstream, not an error.

Rows whose `area_code` resolves to no polity keep the columns with `NA`
rather than being dropped, so a gap is visible instead of silent.

Rows before the back-cast anchor year resolve to the polity live in that
anchor year rather than to the polity live in the row's own year,
because WHEP's pre-anchor series are back-cast onto the anchor-year
territory. See
[`add_polity_code()`](https://eduaguilera.github.io/whep/reference/add_polity_code.md)
for the reasoning.

## Examples

``` r
calculate_n_surplus(example = TRUE)
#> # A tibble: 8 × 20
#>    year area_code polity_area_code reporting_polity_code reporting_polity_name
#>   <int>     <int>            <int> <chr>                 <chr>                
#> 1  2010         1                1 ARM-1991-2025         Armenia              
#> 2  2010         1                1 ARM-1991-2025         Armenia              
#> 3  2010         1                1 ARM-1991-2025         Armenia              
#> 4  2010         1                1 ARM-1991-2025         Armenia              
#> 5  2010         1                1 ARM-1991-2025         Armenia              
#> 6  2011         1                1 ARM-1991-2025         Armenia              
#> 7  2010         1                1 ARM-1991-2025         Armenia              
#> 8  2011         1                1 ARM-1991-2025         Armenia              
#> # ℹ 15 more variables: reporting_polity_has_geometry <lgl>, lon <dbl>,
#> #   lat <dbl>, item_cbs_code <int>, area_ha <dbl>, n_input_std_t <dbl>,
#> #   prod_n_t <dbl>, used_residue_n_t <dbl>, grazed_weeds_n_t <dbl>,
#> #   burnt_residue_n_t <dbl>, n_balance_t <dbl>, surplus_n_t <dbl>,
#> #   method_surplus <chr>, production_n_t <dbl>, surplus_kgn_ha <dbl>
```
