# Report which Rest-of-World members report under their own territory

Promoting a member of the FABIO Rest-of-World bucket has two halves: the
numeric one, which stops its rows being summed into `polity_area_code`
999, and the territorial one, which lets it publish under its own
year-scoped `reporting_polity_code` instead of the bucket's
`ROW-1850-2025`. Only the first is unconditional. The second needs
upstream to name a polity for the area in `faostat_area_polity_map.csv`,
and for 30 of the 61 members it does not (whep#717).

Without this, a member still on `ROW-1850-2025` is indistinguishable
from the genuine residual: both publish an aggregate polity on the
continent `"World"` with no geometry, and nothing says which is which.
This names them, and says for each what is missing.

## Usage

``` r
row_promotion_status(crosswalk = NULL)
```

## Arguments

- crosswalk:

  Crosswalk to inspect. Defaults to the crosswalk this run resolves
  through, so the answer reflects `options(whep.unfold_rest_of_world)`.

## Value

A tibble with one row per Rest-of-World member, ordered by `area_code`:

- `area_code`, `area_name`, `area_iso3c`: The member.

- `cbs`: Whether
  [regions_full](https://eduaguilera.github.io/whep/reference/regions_full.md)
  flags it as a commodity-balance reporter, which is what
  `"cbs_reporters"` promotes.

- `status`: One of the four above.

- `n_periods`: How many polity periods it resolves through.

- `polity_codes`: Those periods, comma-separated.

## The statuses

- `"own_polity"`: promoted, and publishing under its own polity. 31
  members.

- `"polity_unmapped"`: promoted, still on `ROW-1850-2025`, but a live
  non-aggregate polity carrying the area's ISO3 **does** exist upstream.
  All that is missing is a row of the FAOSTAT area map naming it for
  this area, so this is the actionable list to send upstream. 6 members:
  22 Aruba, 71 French Southern and Antarctic Territories, 94 Holy See,
  218 Tokelau, 243 Wallis and Futuna, 271 South Georgia and the South
  Sandwich Islands.

- `"no_polity"`: promoted, still on `ROW-1850-2025`, and upstream has no
  polity for the territory at all. 24 members. Three of them are not
  territories and must never acquire one – 252 `"Unspecified"`, 254
  `"Others (adjustment)"` and the 999 bucket itself, which is excluded
  here because it is the residual rather than a member of it.

- `"folded"`: this run is re-folding the member, so it is summed into
  bucket 999 and carries the bucket's polity. Only under
  `options(whep.unfold_rest_of_world = "none")` or `"cbs_reporters"`.

The ISO3 test behind `"polity_unmapped"` decides **nothing**: it reports
that upstream holds a polity this package cannot reach, which is a gap
in the map, not a mapping. Resolving an area to a polity by matching
ISO3 downstream is the defect whep#711 removed, and \#717 argues
explicitly against re-minting a territorial identity here.

## See also

[`folded_reporting_areas()`](https://eduaguilera.github.io/whep/reference/folded_reporting_areas.md)
for the areas whose rows are summed into another area's code, and
[`polity_mapping_provenance()`](https://eduaguilera.github.io/whep/reference/polity_mapping_provenance.md)
for which authority a resolved row rests on.

## Examples

``` r
status <- row_promotion_status()
table(status$status)
#> 
#>       no_polity      own_polity polity_unmapped 
#>              24              31               6 
status[status$status == "polity_unmapped", c("area_code", "area_name")]
#> # A tibble: 6 × 2
#>   area_code area_name                                   
#>       <int> <chr>                                       
#> 1        22 Aruba                                       
#> 2        71 French Southern and Antarctic Territories   
#> 3        94 Holy See                                    
#> 4       218 Tokelau                                     
#> 5       243 Wallis and Futuna Islands                   
#> 6       271 South Georgia and the South Sandwich Islands
```
