# Build the embodied-nitrogen footprint extension.

Selects one nitrogen category from a
[`build_n_boundary_exceedance()`](https://eduaguilera.github.io/whep/reference/build_n_boundary_exceedance.md)
country-resolution output into the `impact_u` column of a
[`build_footprint()`](https://eduaguilera.github.io/whep/reference/build_footprint.md)
extension: `"exceedance"` (the default) carries `exceedance_n_t`,
`"within_boundary"` carries `within_boundary_n_t`, and `"production"`
carries `production_n_t` (harvested product plus used residue plus
grazed forage). The chosen category is stamped in `method_n_exceedance`.

The three categories are traced as three separate extension passes (one
call per category), not one signed impact: the footprint framework
carries a single non-negative `impact_u`, so the within-boundary and
exceedance parts are run as distinct passes and compared afterwards,
never combined into one signed intensity.

The per-crop (`item_cbs_code`) granularity is preserved so the footprint
can be traced to origin (locked plan decision 14). Rows with a missing
key are dropped defensively; zero-impact crops are kept because they
still consume trade.

## Usage

``` r
build_n_exceedance_extension(
  exceedance,
  category = c("exceedance", "within_boundary", "production")
)
```

## Arguments

- exceedance:

  A
  [`build_n_boundary_exceedance()`](https://eduaguilera.github.io/whep/reference/build_n_boundary_exceedance.md)
  output at `resolution = "country"`, keyed by `year`, `area_code`,
  `item_cbs_code` with the mass terms `exceedance_n_t`,
  `within_boundary_n_t`, `actual_n_t` and, for
  `category = "production"`, `production_n_t`.

- category:

  Which nitrogen mass to carry into `impact_u`: `"exceedance"`
  (default), `"within_boundary"` or `"production"`. Validated with
  [`rlang::arg_match()`](https://rlang.r-lib.org/reference/arg_match.html).

## Value

A tibble with the
[`build_footprint()`](https://eduaguilera.github.io/whep/reference/build_footprint.md)
extension contract columns `year`, `area_code`, `item_cbs_code`,
`impact_u` (tonnes N) and `method_n_exceedance` (the chosen category).

## Examples

``` r
build_n_exceedance_extension(
  tibble::tribble(
    ~year,
    ~area_code,
    ~item_cbs_code,
    ~exceedance_n_t,
    ~within_boundary_n_t,
    ~actual_n_t,
    2010L, 10L, 2511L, 5, 3, 8,
    2010L, 10L, 2513L, 0, 4, 4
  ),
  category = "exceedance"
)
#> # A tibble: 2 × 5
#>    year area_code item_cbs_code impact_u method_n_exceedance
#>   <int>     <int>         <int>    <dbl> <chr>              
#> 1  2010        10          2511        5 exceedance         
#> 2  2010        10          2513        0 exceedance         
```
