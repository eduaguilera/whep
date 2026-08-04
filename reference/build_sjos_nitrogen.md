# Assemble the end-to-end SJOS-N output tables.

Composes the Safe and Just Operating Space for nitrogen (SJOS-N) modules
into a named list of analysis-output tables from one coherent set of
inputs. The gridded soil-surface nitrogen surplus
([`calculate_n_surplus()`](https://eduaguilera.github.io/whep/reference/calculate_n_surplus.md))
is compared to the Schulte-Uebbing critical nitrogen layer for the
surplus-mode boundary
([`build_n_boundary_exceedance()`](https://eduaguilera.github.io/whep/reference/build_n_boundary_exceedance.md),
at grid and country resolution) and the same balance's process-based
losses are routed to their medium-specific critical loads for the
pathway boundary
([`build_n_pathway_exceedance()`](https://eduaguilera.github.io/whep/reference/build_n_pathway_exceedance.md)).
The nourishment axis
([`build_food_supply()`](https://eduaguilera.github.io/whep/reference/build_food_supply.md)
then
[`normalize_nourishment()`](https://eduaguilera.github.io/whep/reference/normalize_nourishment.md))
is crossed with the country-aggregated exceedance into the 2-way
classification
([`classify_sjos_n()`](https://eduaguilera.github.io/whep/reference/classify_sjos_n.md))
and, via the per-capita anthropogenic reactive nitrogen
([`build_n_percapita()`](https://eduaguilera.github.io/whep/reference/build_n_percapita.md)),
into the boundary-versus-nourishment scatter
([`build_n_boundary_percapita()`](https://eduaguilera.github.io/whep/reference/build_n_boundary_percapita.md)).
The country exceedance finally becomes an embodied-nitrogen trade
footprint
([`build_sjos_n_footprint()`](https://eduaguilera.github.io/whep/reference/build_sjos_n_footprint.md)).

The same nitrogen balance feeds the surplus and the pathway boundaries,
the same nourishment feeds the classification and the scatter, and the
one country exceedance feeds the classification, the footprint extension
and the footprint: consistency is enforced by construction. When
`example = TRUE`, a single coherent fixture set drives the whole chain
without any real data.

## Usage

``` r
build_sjos_nitrogen(
  data = list(),
  surplus_method = "harvest_removal",
  boundary_land_use = "ara",
  nh3_source = "soil",
  footprint_category = "exceedance",
  example = FALSE
)
```

## Arguments

- data:

  Named list of injected module inputs. When `example = FALSE` it must
  carry a `balance`
  ([`build_nitrogen_balance()`](https://eduaguilera.github.io/whep/reference/build_nitrogen_balance.md)
  output), a `critical`
  ([`read_critical_n()`](https://eduaguilera.github.io/whep/reference/read_critical_n.md)
  critical surplus), a `critical_loads` list (the three medium critical
  loads for the pathway boundary), `cbs_food`, `population`, `n_inputs`,
  and optionally `biomass_coefs` / `items_full` for the food supply,
  `manure_mgmt_nh3_n_t` for the pathway boundary when
  `nh3_source = "total_agricultural"`, and either an `io` model or
  `fp_flows` for the footprint. A real call without either source aborts
  rather than fabricating a domestic-only footprint. Defaults to
  [`list()`](https://rdrr.io/r/base/list.html).

- surplus_method:

  Surplus definition passed to
  [`calculate_n_surplus()`](https://eduaguilera.github.io/whep/reference/calculate_n_surplus.md),
  `"harvest_removal"` (default) or `"full_balance"`.

- boundary_land_use:

  Land-use scope stamp passed to
  [`build_n_boundary_exceedance()`](https://eduaguilera.github.io/whep/reference/build_n_boundary_exceedance.md),
  `"ara"` (default, the robust historical comparison) or `"all"` (all
  WHEP grassland, a sensitivity rather than a reconstructed
  intensive-grassland class).

- nh3_source:

  Air-pressure scope passed to
  [`build_n_pathway_exceedance()`](https://eduaguilera.github.io/whep/reference/build_n_pathway_exceedance.md),
  `"soil"` (default) or `"total_agricultural"`.

- footprint_category:

  Which per-crop nitrogen mass the footprint traces, `"exceedance"`
  (default), `"within_boundary"` or `"production"`.

- example:

  If `TRUE`, drive the whole chain from the coherent fixture set instead
  of `data`. Defaults to `FALSE`.

## Value

A named list of SJOS-N output tables: `surplus` (per-crop gridded
surplus), `boundary_surplus` (a list with the `grid` and `country`
surplus-mode exceedance), `boundary_pathway` (the pathway-mode
exceedance with `binding_boundary`), `nourishment` (per-capita food
supply with the normalized adequacy score and class), `scatter` (the
per-capita boundary versus nourishment points), `sjos_class` (the 2-way
classification) and `footprint` (a list with the `fp_all` and `fp_food`
embodied-nitrogen footprints).

## Examples

``` r
build_sjos_nitrogen(example = TRUE)
#> $surplus
#> # A tibble: 7 × 22
#>    year area_code polity_area_code reporting_polity_code reporting_polity_name
#>   <int>     <int>            <int> <chr>                 <chr>                
#> 1  2010         1                1 ARM-1991-2025         Armenia              
#> 2  2010         1                1 ARM-1991-2025         Armenia              
#> 3  2010         1                1 ARM-1991-2025         Armenia              
#> 4  2010         1                1 ARM-1991-2025         Armenia              
#> 5  2010         2                2 AFG-1919-2025         Afghanistan          
#> 6  2010         2                2 AFG-1919-2025         Afghanistan          
#> 7  2010         2                2 AFG-1919-2025         Afghanistan          
#> # ℹ 17 more variables: reporting_polity_has_geometry <lgl>, lon <dbl>,
#> #   lat <dbl>, item_cbs_code <int>, area_ha <dbl>, n_input_std_t <dbl>,
#> #   prod_n_t <dbl>, used_residue_n_t <dbl>, grazed_weeds_n_t <dbl>,
#> #   burnt_residue_n_t <dbl>, n_balance_t <dbl>, nh3_n_t <dbl>, no3_n_t <dbl>,
#> #   surplus_n_t <dbl>, method_surplus <chr>, production_n_t <dbl>,
#> #   surplus_kgn_ha <dbl>
#> 
#> $boundary_surplus
#> $boundary_surplus$grid
#> # A tibble: 7 × 22
#>    year area_code polity_area_code reporting_polity_code reporting_polity_name
#>   <int>     <int>            <int> <chr>                 <chr>                
#> 1  2010         1                1 ARM-1991-2025         Armenia              
#> 2  2010         1                1 ARM-1991-2025         Armenia              
#> 3  2010         1                1 ARM-1991-2025         Armenia              
#> 4  2010         1                1 ARM-1991-2025         Armenia              
#> 5  2010         2                2 AFG-1919-2025         Afghanistan          
#> 6  2010         2                2 AFG-1919-2025         Afghanistan          
#> 7  2010         2                2 AFG-1919-2025         Afghanistan          
#> # ℹ 17 more variables: reporting_polity_has_geometry <lgl>, lon <dbl>,
#> #   lat <dbl>, item_cbs_code <int>, area_ha <dbl>, critical_kgn_ha <dbl>,
#> #   actual_kgn_ha <dbl>, exceed_share <dbl>, exceedance_kgn_ha <dbl>,
#> #   within_boundary_kgn_ha <dbl>, exceedance_n_t <dbl>,
#> #   within_boundary_n_t <dbl>, actual_n_t <dbl>, production_n_t <dbl>,
#> #   metric <chr>, land_use <chr>, method_boundary <chr>
#> 
#> $boundary_surplus$country
#> # A tibble: 6 × 14
#>    year area_code polity_area_code reporting_polity_code reporting_polity_name
#>   <int>     <int>            <int> <chr>                 <chr>                
#> 1  2010         1                1 ARM-1991-2025         Armenia              
#> 2  2010         1                1 ARM-1991-2025         Armenia              
#> 3  2010         1                1 ARM-1991-2025         Armenia              
#> 4  2010         2                2 AFG-1919-2025         Afghanistan          
#> 5  2010         2                2 AFG-1919-2025         Afghanistan          
#> 6  2010         2                2 AFG-1919-2025         Afghanistan          
#> # ℹ 9 more variables: reporting_polity_has_geometry <lgl>, item_cbs_code <int>,
#> #   exceedance_n_t <dbl>, within_boundary_n_t <dbl>, actual_n_t <dbl>,
#> #   production_n_t <dbl>, metric <chr>, land_use <chr>, method_boundary <chr>
#> 
#> 
#> $boundary_pathway
#> # A tibble: 7 × 32
#>    year area_code polity_area_code reporting_polity_code reporting_polity_name
#>   <int>     <int>            <int> <chr>                 <chr>                
#> 1  2010         1                1 ARM-1991-2025         Armenia              
#> 2  2010         1                1 ARM-1991-2025         Armenia              
#> 3  2010         1                1 ARM-1991-2025         Armenia              
#> 4  2010         1                1 ARM-1991-2025         Armenia              
#> 5  2010         2                2 AFG-1919-2025         Afghanistan          
#> 6  2010         2                2 AFG-1919-2025         Afghanistan          
#> 7  2010         2                2 AFG-1919-2025         Afghanistan          
#> # ℹ 27 more variables: reporting_polity_has_geometry <lgl>, lon <dbl>,
#> #   lat <dbl>, item_cbs_code <int>, area_ha <dbl>, critical_air_kgn_ha <dbl>,
#> #   actual_air_kgn_ha <dbl>, exceed_share_air <dbl>,
#> #   exceedance_air_kgn_ha <dbl>, within_air_kgn_ha <dbl>,
#> #   exceedance_air_n_t <dbl>, within_air_n_t <dbl>, actual_air_n_t <dbl>,
#> #   critical_gw_kgn_ha <dbl>, critical_sw_kgn_ha <dbl>,
#> #   critical_water_kgn_ha <dbl>, actual_water_kgn_ha <dbl>, …
#> 
#> $nourishment
#> # A tibble: 2 × 13
#>    year area_code polity_area_code reporting_polity_code reporting_polity_name
#>   <int>     <int>            <int> <chr>                 <chr>                
#> 1  2010         1                1 ARM-1991-2025         Armenia              
#> 2  2010         2                2 AFG-1919-2025         Afghanistan          
#> # ℹ 8 more variables: reporting_polity_has_geometry <lgl>,
#> #   protein_g_cap_day <dbl>, energy_kcal_cap_day <dbl>, population <dbl>,
#> #   method_food_supply <chr>, method_protein_basis <chr>, value_norm <dbl>,
#> #   nourish <chr>
#> 
#> $scatter
#> # A tibble: 2 × 5
#>    year area_code nourish_norm boundary_norm population
#>   <int>     <int>        <dbl>         <dbl>      <dbl>
#> 1  2010         1        0.965          1.71 4000000000
#> 2  2010         2        2.05           1.83 3000000000
#> 
#> $sjos_class
#> # A tibble: 6 × 9
#>    year area_code item_cbs_code exceedance_n_t within_boundary_n_t actual_n_t
#>   <int>     <int>         <int>          <dbl>               <dbl>      <dbl>
#> 1  2010         1          2511             58                  29         87
#> 2  2010         1          2513              0                   1          1
#> 3  2010         1          2555              0                   0          0
#> 4  2010         2          2511             11                   4         15
#> 5  2010         2          2513              0                   1          1
#> 6  2010         2          2555              2                   2          4
#> # ℹ 3 more variables: nourish <chr>, boundary_side <chr>, sjos_class <fct>
#> 
#> $footprint
#> $footprint$fp_all
#> # A tibble: 6 × 13
#>    year origin_area origin_item target_area target_item target_fd origin        
#>   <int>       <int>       <int>       <int>       <int> <chr>     <chr>         
#> 1  2010           1        2511           1        2511 food      Domestic cons…
#> 2  2010           1        2513           1        2513 food      Domestic cons…
#> 3  2010           1        2555           1        2555 food      Domestic cons…
#> 4  2010           2        2511           2        2511 food      Domestic cons…
#> 5  2010           2        2513           2        2513 food      Domestic cons…
#> 6  2010           2        2555           2        2555 food      Domestic cons…
#> # ℹ 6 more variables: impact_u <dbl>, item_cbs_code <int>, category <chr>,
#> #   nourish <chr>, boundary_side <chr>, sjos_class <fct>
#> 
#> $footprint$fp_food
#> # A tibble: 6 × 13
#>    year origin_area origin_item target_area target_item target_fd origin        
#>   <int>       <int>       <int>       <int>       <int> <chr>     <chr>         
#> 1  2010           1        2511           1        2511 food      Domestic cons…
#> 2  2010           1        2513           1        2513 food      Domestic cons…
#> 3  2010           1        2555           1        2555 food      Domestic cons…
#> 4  2010           2        2511           2        2511 food      Domestic cons…
#> 5  2010           2        2513           2        2513 food      Domestic cons…
#> 6  2010           2        2555           2        2555 food      Domestic cons…
#> # ℹ 6 more variables: impact_u <dbl>, item_cbs_code <int>, category <chr>,
#> #   nourish <chr>, boundary_side <chr>, sjos_class <fct>
#> 
#> 
```
