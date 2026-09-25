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
([`build_sjos_n_footprint()`](https://eduaguilera.github.io/whep/reference/build_sjos_n_footprint.md)),
which carries the producer's classes (`origin_classes`, the per-crop
classification) and the consuming country's nourishment class
(`target_nourish`, joined on `target_area` and `year` from the same
nourishment table). The driver joins no consumer boundary class: that is
a country-year class decided after aggregation, which a caller supplies
to
[`build_sjos_n_footprint()`](https://eduaguilera.github.io/whep/reference/build_sjos_n_footprint.md)
directly.

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
  grassland_split = c("image_density", "none"),
  nh3_source = "soil",
  footprint_category = "exceedance",
  nourishment_thresholds = c("composed", "flat"),
  nourishment_band = list(),
  negative_critical = c("keep", "clamp"),
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
  loads for the pathway boundary), `cbs_food` and `n_inputs`, and
  optionally `population` (read with
  [`read_population()`](https://eduaguilera.github.io/whep/reference/read_population.md)
  at its own default composition over the years of `cbs_food` and
  `n_inputs` when absent; inject a table to use any other source),
  `biomass_coefs` / `items_full` for the food supply,
  `manure_mgmt_nh3_n_t` for the pathway boundary when
  `nh3_source = "total_agricultural"`, `critical_binding` (a
  [`build_critical_n_binding()`](https://eduaguilera.github.io/whep/reference/build_critical_n_binding.md)
  table for the `boundary_land_use` scope, whose `binding_threshold` is
  then carried into the grid boundary; absent, the column is `NA`), and
  either an `io` model or `fp_flows` for the footprint. A real call
  without either source aborts rather than fabricating a domestic-only
  footprint. `grassland` is the
  [`build_n_boundary_exceedance()`](https://eduaguilera.github.io/whep/reference/build_n_boundary_exceedance.md)
  grassland-split input, only used and only optional when
  `boundary_land_use = "all"` and `grassland_split = "image_density"`
  (see `grassland_split`); a real call without it and without `critical`
  to build it from aborts rather than guessing a var/threshold to match.
  Defaults to [`list()`](https://rdrr.io/r/base/list.html).

- surplus_method:

  Surplus definition passed to
  [`calculate_n_surplus()`](https://eduaguilera.github.io/whep/reference/calculate_n_surplus.md),
  `"harvest_removal"` (default) or `"full_balance"`.

- boundary_land_use:

  Land-use scope stamp passed to
  [`build_n_boundary_exceedance()`](https://eduaguilera.github.io/whep/reference/build_n_boundary_exceedance.md),
  `"ara"` (default, the robust historical comparison) or `"all"`
  (cropland and intensive grassland compared like for like against the
  critical allowance, extensive grassland against IMAGE's 2010 budget;
  see `grassland_split`) (issue \#1285).

- grassland_split:

  Grassland treatment passed to
  [`build_n_boundary_exceedance()`](https://eduaguilera.github.io/whep/reference/build_n_boundary_exceedance.md),
  used only when `boundary_land_use = "all"`: `"image_density"`
  (default) splits each cell into a managed and an extensive component,
  `"none"` compares one cell pressure with the deposited `"all"`-scope
  allowance. Under `"image_density"`, `data$grassland` is used when
  supplied (its four elements, see
  [`build_n_boundary_exceedance()`](https://eduaguilera.github.io/whep/reference/build_n_boundary_exceedance.md));
  otherwise it is built from
  [`build_grassland_intensity_classes()`](https://eduaguilera.github.io/whep/reference/build_grassland_intensity_classes.md),
  the IMAGE 2010 extensive budget, and the `"ara"`/`"igl"` critical
  layers matched to `data$critical`'s own var and threshold. Ignored (no
  extra reads) when `boundary_land_use` is not `"all"`.

- nh3_source:

  Air-pressure scope passed to
  [`build_n_pathway_exceedance()`](https://eduaguilera.github.io/whep/reference/build_n_pathway_exceedance.md),
  `"soil"` (default) or `"total_agricultural"`.

- footprint_category:

  Which per-crop nitrogen mass the footprint traces, `"exceedance"`
  (default), `"within_boundary"` or `"production"`.

- nourishment_thresholds:

  Which band the "just" axis classifies against: `"composed"` (default)
  builds it per country and year from
  [`build_nourishment_band()`](https://eduaguilera.github.io/whep/reference/build_nourishment_band.md)'s
  four sourced terms, or `"flat"` restores the retired 62.1 / 85.05
  pair. `"flat"` survives for continuity and sensitivity only: of its
  five underlying numbers only the 46 g/cap/day floor was ever sourced,
  and the 1.35 multiplier behind both bounds was a preliminary
  presentation figure (whep#753).

- nourishment_band:

  Named list of options for the composed band, ignored when
  `nourishment_thresholds = "flat"`. `quality_method` and
  `quality_variant` select the protein-quality tier and its bracket
  ([`build_protein_quality()`](https://eduaguilera.github.io/whep/reference/build_protein_quality.md));
  `wedge_method` and `wedge_coverage` select the loss wedge
  ([`build_loss_wedge()`](https://eduaguilera.github.io/whep/reference/build_loss_wedge.md));
  `shortfall`, `ceiling` and `requirement_sd` go to
  [`build_nourishment_band()`](https://eduaguilera.github.io/whep/reference/build_nourishment_band.md)
  itself, and `ceiling` is the sensitivity knob the band's own
  documentation asks callers to sweep. An option this list does not name
  **aborts** rather than being ignored, so a mistyped knob cannot
  silently run the default and be reported as a sensitivity. Defaults to
  [`list()`](https://rdrr.io/r/base/list.html), which leaves every
  builder on its own default.

- negative_critical:

  Treatment of cells whose critical surplus is below zero, passed to
  [`build_n_boundary_exceedance()`](https://eduaguilera.github.io/whep/reference/build_n_boundary_exceedance.md):
  `"keep"` (default, as the source) or `"clamp"` (zero allowance, a
  declared departure from Schulte-Uebbing et al. 2022). It reaches the
  grid and country boundary and through them the classification and the
  footprint, and is stamped as `negative_critical` in both boundary
  tables, in `sjos_class` and in both footprint tables.

- example:

  If `TRUE`, drive the whole chain from the coherent fixture set instead
  of `data`. Defaults to `FALSE`.

## Value

A named list of SJOS-N output tables: `surplus` (per-crop gridded
surplus), `boundary_surplus` (a list with the `grid` and `country`
surplus-mode exceedance), `boundary_pathway` (the pathway-mode
exceedance with `binding_boundary`), `nourishment` (per-capita food
supply with the normalized adequacy score and class), `scatter` (the
per-capita boundary versus nourishment points; it and `nourishment`
carry `method_population`, `"read_population"` or `"supplied"`),
`sjos_class` (the 2-way classification) and `footprint` (a list with the
`fp_all` and `fp_food` embodied-nitrogen footprints, both carrying
`target_nourish`, and `target_class_diag`, the per-year count of flows
whose consumer country-year has no nourishment class). The boundary
tables, `sjos_class` and both footprint tables carry
`negative_critical`.

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
#> # A tibble: 7 × 79
#>    year area_code polity_area_code reporting_polity_code reporting_polity_name
#>   <int>     <int>            <int> <chr>                 <chr>                
#> 1  2010         1                1 ARM-1991-2025         Armenia              
#> 2  2010         1                1 ARM-1991-2025         Armenia              
#> 3  2010         1                1 ARM-1991-2025         Armenia              
#> 4  2010         1                1 ARM-1991-2025         Armenia              
#> 5  2010         2                2 AFG-1919-2025         Afghanistan          
#> 6  2010         2                2 AFG-1919-2025         Afghanistan          
#> 7  2010         2                2 AFG-1919-2025         Afghanistan          
#> # ℹ 74 more variables: reporting_polity_has_geometry <lgl>, cell_id <int>,
#> #   source_row <int>, source_col <int>, lon <dbl>, lat <dbl>,
#> #   item_cbs_code <int>, actual_year <int>, critical_reference_year <int>,
#> #   area_ha <dbl>, source_area_ha <dbl>, image_region <int>,
#> #   critical_threshold <chr>, binding_threshold <chr>,
#> #   binding_matches_mi <lgl>, actual_n_t <dbl>, pressure_share <dbl>,
#> #   pressure_condition_ratio <dbl>, critical_n_t <dbl>, …
#> 
#> $boundary_surplus$country
#> # A tibble: 6 × 37
#>    year area_code polity_area_code reporting_polity_code reporting_polity_name
#>   <int>     <int>            <int> <chr>                 <chr>                
#> 1  2010         1                1 ARM-1991-2025         Armenia              
#> 2  2010         1                1 ARM-1991-2025         Armenia              
#> 3  2010         1                1 ARM-1991-2025         Armenia              
#> 4  2010         2                2 AFG-1919-2025         Afghanistan          
#> 5  2010         2                2 AFG-1919-2025         Afghanistan          
#> 6  2010         2                2 AFG-1919-2025         Afghanistan          
#> # ℹ 32 more variables: reporting_polity_has_geometry <lgl>,
#> #   item_cbs_code <int>, actual_n_t <dbl>, critical_n_t <dbl>,
#> #   signed_margin_n_t <dbl>, crop_critical_n_t <dbl>,
#> #   positive_overshoot_n_t <dbl>, exceedance_n_t <dbl>,
#> #   within_boundary_n_t <dbl>, unallocated_critical_n_t <dbl>,
#> #   unallocated_signed_margin_n_t <dbl>,
#> #   unallocated_positive_overshoot_n_t <dbl>, production_n_t <dbl>, …
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
#> # A tibble: 2 × 14
#>    year area_code polity_area_code reporting_polity_code reporting_polity_name
#>   <int>     <int>            <int> <chr>                 <chr>                
#> 1  2010         1                1 ARM-1991-2025         Armenia              
#> 2  2010         2                2 AFG-1919-2025         Afghanistan          
#> # ℹ 9 more variables: reporting_polity_has_geometry <lgl>,
#> #   protein_g_cap_day <dbl>, energy_kcal_cap_day <dbl>, population <dbl>,
#> #   method_food_supply <chr>, method_protein_basis <chr>, value_norm <dbl>,
#> #   nourish <chr>, method_population <chr>
#> 
#> $scatter
#> # A tibble: 2 × 6
#>    year area_code nourish_norm boundary_norm population method_population
#>   <int>     <int>        <dbl>         <dbl>      <dbl> <chr>            
#> 1  2010         1         1.13          1.71 4000000000 supplied         
#> 2  2010         2         1.38          1.83 3000000000 supplied         
#> 
#> $sjos_class
#> # A tibble: 6 × 10
#>    year area_code item_cbs_code exceedance_n_t within_boundary_n_t actual_n_t
#>   <int>     <int>         <int>          <dbl>               <dbl>      <dbl>
#> 1  2010         1          2511         17.8                69.2           87
#> 2  2010         1          2513          0.712               0.288          1
#> 3  2010         1          2555          0                  -2             -2
#> 4  2010         2          2511          0                  15             15
#> 5  2010         2          2513          0                   1              1
#> 6  2010         2          2555          0                   4              4
#> # ℹ 4 more variables: nourish <chr>, boundary_side <chr>, sjos_class <fct>,
#> #   negative_critical <chr>
#> 
#> $footprint
#> $footprint$fp_all
#> # A tibble: 6 × 15
#>    year origin_area origin_item target_area target_item target_fd origin        
#>   <int>       <int>       <int>       <int>       <int> <chr>     <chr>         
#> 1  2010           1        2511           1        2511 food      Domestic cons…
#> 2  2010           1        2513           1        2513 food      Domestic cons…
#> 3  2010           1        2555           1        2555 food      Domestic cons…
#> 4  2010           2        2511           2        2511 food      Domestic cons…
#> 5  2010           2        2513           2        2513 food      Domestic cons…
#> 6  2010           2        2555           2        2555 food      Domestic cons…
#> # ℹ 8 more variables: impact_u <dbl>, item_cbs_code <int>, category <chr>,
#> #   nourish <chr>, boundary_side <chr>, sjos_class <fct>, target_nourish <chr>,
#> #   negative_critical <chr>
#> 
#> $footprint$fp_food
#> # A tibble: 6 × 15
#>    year origin_area origin_item target_area target_item target_fd origin        
#>   <int>       <int>       <int>       <int>       <int> <chr>     <chr>         
#> 1  2010           1        2511           1        2511 food      Domestic cons…
#> 2  2010           1        2513           1        2513 food      Domestic cons…
#> 3  2010           1        2555           1        2555 food      Domestic cons…
#> 4  2010           2        2511           2        2511 food      Domestic cons…
#> 5  2010           2        2513           2        2513 food      Domestic cons…
#> 6  2010           2        2555           2        2555 food      Domestic cons…
#> # ℹ 8 more variables: impact_u <dbl>, item_cbs_code <int>, category <chr>,
#> #   nourish <chr>, boundary_side <chr>, sjos_class <fct>, target_nourish <chr>,
#> #   negative_critical <chr>
#> 
#> $footprint$target_class_diag
#> # A tibble: 2 × 8
#>   table  year n_flows n_flows_unclassified n_target_areas n_target_areas_uncla…¹
#>   <chr> <int>   <int>                <int>          <int>                  <int>
#> 1 fp_a…  2010       6                    0              2                      0
#> 2 fp_f…  2010       6                    0              2                      0
#> # ℹ abbreviated name: ¹​n_target_areas_unclassified
#> # ℹ 2 more variables: impact_u <dbl>, impact_u_unclassified <dbl>
#> 
#> 
```
