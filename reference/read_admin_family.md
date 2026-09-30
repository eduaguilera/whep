# Read the per-family subnational admin-statistics pins

Read the tier-2 and tier-3 administrative-statistics families that WHEP
compiled for the subnational spatialization, one pin per family, and
return them as a named list of tibbles.

Every family is read by an explicit alias: there is no discovery step
and no wildcard, so a family that is not shipped cannot be silently
substituted by another. A requested family that is absent – its alias is
not registered in
[`whep_inputs`](https://eduaguilera.github.io/whep/reference/whep_inputs.md),
or its pin holds no rows – is named in the returned `not_shipped`
element instead of aborting the read, because a family whose publication
consent has not cleared is a planned state of this dataset (see the T23
outcome recorded in `inst/extdata/admin_stats_pins_manifest.csv`). Any
other failure of
[`whep_read_file()`](https://eduaguilera.github.io/whep/reference/whep_read_file.md)
is left to propagate: an unreachable board is not an embargo.

## Usage

``` r
read_admin_family(families = NULL, example = FALSE)
```

## Arguments

- families:

  Character vector of family aliases to read. `NULL`, the default, reads
  all five. An alias outside that set aborts.

- example:

  If `TRUE`, return a small fixture instead of reading any pin. Defaults
  to `FALSE`.

## Value

A named list with one element per requested family that is shipped, each
a tibble in the shared admin-statistics reader shape (`source`,
`source_native_unit_id`, `source_native_unit_name`,
`source_native_item_code`, `source_native_item_name`, `indicator_used`,
`quantity`, `year`, `value` or `share`, `value_unit`, `value_flag`,
`grain`, `nuts_version`, `source_version`, `recorded_at`), plus a
`not_shipped` element holding the aliases that were requested but are
not on the board. `not_shipped` is always present, and is `character(0)`
when every requested family loaded.

## Families

The five aliases, their tier and what each ships, as staged on
2026-09-03 from the harmonized subnational panel:

- `"admin-stats-japan"` (tier 2): 46 prefectures, 32,095 observed area
  and production rows, 1961-2022.

- `"admin-stats-spain-provinces"` (tier 2): 50 provinces, 323,469
  observed-lane area and production rows, 1961-2021. The compilation's
  Spanish livestock rows are all flagged `admin_level_overlap` and are
  therefore not shipped, which is why three of its 53 units are absent.

- `"admin-stats-australia"` (tier 2): 8 states and territories, 6,597
  observed area, production and head-count rows, 1961-2022.

- `"admin-stats-france-livestock"` (tier 2): 89 departments, 58,740
  observed head counts, 1961-2020.

- `"admin-stats-latam"` (tier 3): 142 first-level units of the
  Infante-Amate, Urrego-Mesa, Badia-Miro and Aguilera panel, 875,514
  rows, 1961-2023, shipped as **shares only**. Each row carries `share`,
  its unit's share of the country total for the same item, indicator and
  year, and no `value`; this reader aborts if a future version of that
  pin carries one.

`"admin-stats-jrc"` is not one of these. It is the public tier-1 JRC
product with its own reader, and passing it here aborts as an unknown
alias.

## Examples

``` r
read_admin_family(example = TRUE)
#> $`admin-stats-japan`
#> # A tibble: 5 × 15
#>   source     source_native_unit_id source_native_unit_n…¹ source_native_item_c…²
#>   <chr>      <chr>                 <chr>                  <chr>                 
#> 1 admin-sta… JPN-HOKKAIDO          Hokkaido               27                    
#> 2 admin-sta… JPN-NIIGATA           Niigata                27                    
#> 3 admin-sta… JPN-AKITA             Akita                  27                    
#> 4 admin-sta… JPN-MIYAGI            Miyagi                 27                    
#> 5 admin-sta… JPN-FUKUSHIMA         Fukushima              27                    
#> # ℹ abbreviated names: ¹​source_native_unit_name, ²​source_native_item_code
#> # ℹ 11 more variables: source_native_item_name <chr>, indicator_used <chr>,
#> #   quantity <chr>, year <int>, value <dbl>, value_unit <chr>,
#> #   value_flag <chr>, grain <chr>, nuts_version <chr>, source_version <chr>,
#> #   recorded_at <chr>
#> 
#> $`admin-stats-latam`
#> # A tibble: 5 × 15
#>   source     source_native_unit_id source_native_unit_n…¹ source_native_item_c…²
#>   <chr>      <chr>                 <chr>                  <chr>                 
#> 1 admin-sta… BOL-LAPAZ             La Paz                 661                   
#> 2 admin-sta… BOL-COCHABAMBA        Cochabamba             661                   
#> 3 admin-sta… BOL-BEN               Beni                   661                   
#> 4 admin-sta… BOL-PANDO             Pando                  661                   
#> 5 admin-sta… BOL-SANTACRUZ         Santa Cruz             661                   
#> # ℹ abbreviated names: ¹​source_native_unit_name, ²​source_native_item_code
#> # ℹ 11 more variables: source_native_item_name <chr>, indicator_used <chr>,
#> #   quantity <chr>, year <int>, share <dbl>, value_unit <chr>,
#> #   value_flag <chr>, grain <chr>, nuts_version <chr>, source_version <chr>,
#> #   recorded_at <chr>
#> 
#> $not_shipped
#> [1] "admin-stats-spain-provinces"  "admin-stats-australia"       
#> [3] "admin-stats-france-livestock"
#> 
```
