# Split livestock excretion across manure-management systems.

Splits the excreted nitrogen, carbon and volatile solids from
[`estimate_n_excretion()`](https://eduaguilera.github.io/whep/reference/estimate_n_excretion.md)
across manure-management systems (MMS), separating the in-situ grazing
stream (pasture/range/paddock, deposited where it falls) from the
collected/housed streams routed to storage. The split conserves mass:
the per-species MMS shares sum to one.

## Usage

``` r
split_manure_management(excretion, options = list())
```

## Arguments

- excretion:

  A tibble from
  [`estimate_n_excretion()`](https://eduaguilera.github.io/whep/reference/estimate_n_excretion.md)
  with `year`, `territory`, `sub_territory`, `livestock_category`,
  `n_excretion`, `c_excretion` and `vs_excretion`.

- options:

  A named list. `mms_source` selects the MMS-share table
  (`"regional_default"`, the global IPCC/GLEAM default in
  `regional_mms_distribution`).

## Value

A tibble with one row per
`year x territory x sub_territory x livestock_category x mms_type`, plus
`species_gen`, `loss_category`, `stream` (`"grazing"` or `"collected"`),
`n_stream`, `c_stream`, `vs_stream` and `method_mms`.

## Examples

``` r
excretion <- tibble::tribble(
  ~year, ~territory, ~sub_territory, ~livestock_category,
  ~n_excretion, ~c_excretion, ~vs_excretion,
  2020L, "ES", NA, "Cattle_milk", 100, 1900, 60,
  2020L, "ES", NA, "Pigs", 30, 270, 20
)
split_manure_management(excretion)
#> # A tibble: 7 × 13
#>    year territory sub_territory livestock_category species_gen loss_category
#>   <int> <chr>     <lgl>         <chr>              <chr>       <chr>        
#> 1  2020 ES        NA            Cattle_milk        Cattle      Dairy Cattle 
#> 2  2020 ES        NA            Cattle_milk        Cattle      Dairy Cattle 
#> 3  2020 ES        NA            Cattle_milk        Cattle      Dairy Cattle 
#> 4  2020 ES        NA            Cattle_milk        Cattle      Dairy Cattle 
#> 5  2020 ES        NA            Pigs               Swine       Swine        
#> 6  2020 ES        NA            Pigs               Swine       Swine        
#> 7  2020 ES        NA            Pigs               Swine       Swine        
#> # ℹ 7 more variables: cn_species <chr>, mms_type <chr>, stream <chr>,
#> #   n_stream <dbl>, c_stream <dbl>, vs_stream <dbl>, method_mms <chr>
```
