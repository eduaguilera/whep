# Build livestock nutrient flows from realised feed intake.

Top-level driver that traces the nitrogen, carbon and volatile solids in
realised feed intake (the
[`redistribute_feed()`](https://eduaguilera.github.io/whep/reference/redistribute_feed.md)
result) through livestock excretion, manure management and application
to soil by land use and crop, plus the management-loss side-streams. It
chains
[`estimate_n_excretion()`](https://eduaguilera.github.io/whep/reference/estimate_n_excretion.md),
[`split_manure_management()`](https://eduaguilera.github.io/whep/reference/split_manure_management.md),
[`apply_management_losses()`](https://eduaguilera.github.io/whep/reference/apply_management_losses.md)
and
[`allocate_manure_to_land()`](https://eduaguilera.github.io/whep/reference/allocate_manure_to_land.md);
at the `"subnational"` resolution it additionally spills each cell's
un-placeable surplus to neighbouring cells with
[`allocate_manure_transport()`](https://eduaguilera.github.io/whep/reference/allocate_manure_transport.md)
before local disposal. Every method choice is recorded in a `method_*`
provenance column and the nitrogen balance (excreted = applied +
management losses) is conserved.

## Usage

``` r
build_livestock_nutrient_flows(
  intake,
  resolution = "national",
  methods = list(),
  gridded = NULL
)
```

## Arguments

- intake:

  A tibble of realised feed intake (the
  [`redistribute_feed()`](https://eduaguilera.github.io/whep/reference/redistribute_feed.md)
  result); see
  [`estimate_n_excretion()`](https://eduaguilera.github.io/whep/reference/estimate_n_excretion.md)
  for the required columns. At `"subnational"` resolution
  `sub_territory` is the `"lon_lat"` cell id.

- resolution:

  One of `"global"`, `"national"` (default) or `"subnational"`.
  Transport between cells runs only at `"subnational"`.

- methods:

  A named list of per-stage option lists, any of `excretion`, `split`,
  `losses`, `allocation` and `transport`, each forwarded to the matching
  pipeline function's `options`.

- gridded:

  The land-surface layer (`crops` and optional `grass` tibbles) passed
  to
  [`allocate_manure_to_land()`](https://eduaguilera.github.io/whep/reference/allocate_manure_to_land.md);
  required for the default `"potential_uptake"` cap. `NULL` is treated
  as an empty list.

## Value

A named list with `applied` (manure applied per
`land_use x crop (x cell)` with all `method_*` provenance columns),
`losses` (management-loss side-streams per polity) and `excretion` (the
per-category excretion totals).

## Examples

``` r
intake <- tibble::tribble(
  ~year, ~territory, ~sub_territory, ~livestock_category,
  ~item_cbs_code, ~feed_quality, ~intake_dm_t,
  2020L, "ESP", NA, "Cattle_milk", 2513L, "high_quality", 200,
  2020L, "ESP", NA, "Cattle_milk", NA, "grass", 600
)
gridded <- list(
  crops = tibble::tribble(
    ~year, ~territory, ~sub_territory, ~crop, ~manure_n_receptivity, ~crop_n_cap,
    2020L, "ESP", NA, "barley", 6, 200,
    2020L, "ESP", NA, "wheat", 4, 200
  )
)
build_livestock_nutrient_flows(intake, gridded = gridded)
#> $applied
#> # A tibble: 3 × 19
#>    year territory sub_territory land_use crop  source_stream applied_n applied_c
#>   <int> <chr>     <lgl>         <chr>    <chr> <chr>             <dbl>     <dbl>
#> 1  2020 ESP       NA            Cropland barl… collected          2.41      41.0
#> 2  2020 ESP       NA            Cropland wheat collected          1.61      27.3
#> 3  2020 ESP       NA            Grassla… NA    grazing            6.29     120. 
#> # ℹ 11 more variables: applied_vs <dbl>, over_cap <lgl>,
#> #   method_allocation <chr>, method_cap <chr>, disposal_method <chr>,
#> #   resolution <chr>, method_n_excretion <chr>, method_vs <chr>,
#> #   method_mms <chr>, method_losses <chr>, method_transport <chr>
#> 
#> $losses
#> # A tibble: 1 × 10
#>    year territory sub_territory n_volatilized n_leached n2o_direct_n   n2_n
#>   <int> <chr>     <lgl>                 <dbl>     <dbl>        <dbl>  <dbl>
#> 1  2020 ESP       NA                     2.08    0.0755       0.0289 0.0868
#> # ℹ 3 more variables: n2o_indirect_n <dbl>, c_lost <dbl>, vs_destroyed <dbl>
#> 
#> $excretion
#> # A tibble: 1 × 10
#>    year territory sub_territory livestock_category n_intake n_excretion
#>   <int> <chr>     <lgl>         <chr>                 <dbl>       <dbl>
#> 1  2020 ESP       NA            Cattle_milk            15.7        12.6
#> # ℹ 4 more variables: c_excretion <dbl>, vs_excretion <dbl>,
#> #   method_n_excretion <chr>, method_vs <chr>
#> 
```
