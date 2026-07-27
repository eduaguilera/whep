# Allocate field-available manure to cropland and grassland by crop.

Distributes the manure nitrogen, carbon and volatile solids that survive
management (the output of
[`apply_management_losses()`](https://eduaguilera.github.io/whep/reference/apply_management_losses.md))
across cropland crops and grassland, with a local agronomic cap and an
explicit overflow path. The collected/housed manure fills cropland up to
each crop's cap (weighted by West-2014 receptivity by default), the
surplus spills onto grassland up to the grassland cap, and any residual
beyond all reachable sinks follows the `disposal_method`. The in-situ
grazing stream is deposited on grassland where it falls, uncapped.
Carbon and volatile solids ride along with each stream at its
post-storage bundle ratio, so mass is conserved by construction.

## Usage

``` r
allocate_manure_to_land(applied, gridded = list(), options = list())
```

## Arguments

- applied:

  A tibble from
  [`apply_management_losses()`](https://eduaguilera.github.io/whep/reference/apply_management_losses.md)
  with at least `year`, `territory`, `sub_territory`, `stream`,
  `applied_n`, `applied_c` and `applied_vs`.

- gridded:

  A named list describing the land surface for each polity:

  - `crops`: a tibble keyed by `year`, `territory`, `sub_territory`,
    `crop` with the allocation weight (`manure_n_receptivity` for
    `"area_x_receptivity"`, `crop_n_demand` for `"crop_n_demand"`) and
    the cap basis (`crop_n_cap`, t N, for
    `"potential_uptake"`/`"realised_removal"`; `crop_area_ha` for
    `"fixed_ceiling"`).

  - `grass` (optional): a tibble keyed by `year`, `territory`,
    `sub_territory` with `grass_n_cap` (t N) or `grass_area_ha`. The
    grassland cap is scaled by `f_n_tolerance` on the same footing as
    the uptake-based crop caps (not for `"fixed_ceiling"`). Absent
    grassland means no grassland sink (cap zero).

- options:

  A named list of method options: `method` (`"area_x_receptivity"`
  default, or `"crop_n_demand"`), `cap_method` (`"potential_uptake"`
  default, `"realised_removal"`, or `"fixed_ceiling"`), `f_n_tolerance`
  (default 1.2, applied to the uptake-based caps), `fixed_ceiling_kg_ha`
  (default 170, EU Nitrates) and `disposal_method` (`"over_apply_local"`
  default, `"unmanaged_disposal"`, or `"retain_unallocated"`).

## Value

A tibble with one row per allocation target: `year`, `territory`,
`sub_territory`, `land_use` (`"Cropland"`/`"Grassland"`/`"Disposal"`/
`"Unallocated"`), `crop`, `source_stream` (`"collected"`/`"grazing"`),
`manure_type` (`"Excreta"`/`"Solid"`/`"Liquid"`, carried from `applied`
when present; each allocated row's N/C/VS is split across the
`manure_type`s in proportion to their share of the cell's collected N,
so the capacity-filling math itself stays pooled), `applied_n`,
`applied_c`, `applied_vs`, `over_cap` and the `method_allocation`,
`method_cap` and `disposal_method` provenance columns. The N split by
`manure_type` is exact (mass-conserving per type); `applied_c` and
`applied_vs` are split by the SAME N-proportional share, not by each
`manure_type`'s own C:N/VS:N ratio, so a per-`manure_type` carbon or
volatile-solids figure does not reflect that type's actual composition
(e.g. solid vs. liquid manure have materially different C:N). Callers
needing manure-type-specific carbon/VS (e.g. CH4 potential by system)
should not rely on the `manure_type`-broken-out `applied_c`/
`applied_vs` columns without accounting for this.

## Examples

``` r
applied <- tibble::tribble(
  ~year, ~territory, ~sub_territory, ~stream, ~manure_type,
  ~applied_n, ~applied_c, ~applied_vs,
  2020L, "ESP", NA, "collected", "Solid", 80, 800, 40,
  2020L, "ESP", NA, "grazing", "Excreta", 20, 380, 12
)
crops <- tibble::tribble(
  ~year, ~territory, ~sub_territory, ~crop, ~manure_n_receptivity, ~crop_n_cap,
  2020L, "ESP", NA, "barley", 6, 50,
  2020L, "ESP", NA, "wheat", 4, 40
)
allocate_manure_to_land(applied, list(crops = crops))
#> # A tibble: 3 × 14
#>    year territory sub_territory land_use  crop   source_stream manure_type
#>   <int> <chr>     <lgl>         <chr>     <chr>  <chr>         <chr>      
#> 1  2020 ESP       NA            Cropland  barley collected     Solid      
#> 2  2020 ESP       NA            Cropland  wheat  collected     Solid      
#> 3  2020 ESP       NA            Grassland NA     grazing       Excreta    
#> # ℹ 7 more variables: applied_n <dbl>, applied_c <dbl>, applied_vs <dbl>,
#> #   over_cap <lgl>, method_allocation <chr>, method_cap <chr>,
#> #   disposal_method <chr>
```
