# Estimate direct soil N2O emissions from applied nitrogen.

Four emission-factor regimes for direct nitrous oxide from nitrogen
applied to soil, `n2o_direct_n_t = n_input_t * ef`.

`"ipcc2019"` (the default) is the IPCC 2019 Tier 1 wet/dry
disaggregation, keyed on each row's `moisture_regime` (`"wet"` or
`"dry"`), with every factor read from
[ipcc_2019_soil_n_ef](https://eduaguilera.github.io/whep/reference/ipcc_2019_soil_n_ef.md):

- `EF1` (IPCC 2019 Refinement Vol 4 Ch 11 Table 11.1 (Updated) p.
  11.13): 0.016 for synthetic fertiliser and 0.006 for every other N
  input in wet climates, 0.005 for all N inputs in dry climates. Stamped
  `"ipcc2019_wet_dry"`.

- `EF1FR` (same table): 0.004, the aggregated flooded-rice factor, on
  every rice row in both moisture and both water regimes. No drainage
  layer exists for its continuous-flooding (0.003) versus drained
  (0.005) split, and upland rice, which the table prices with `EF1`, has
  no layer of its own either. Rice rows are those whose `item_cbs_code`
  is the CBS item of a production item classed `Crop_irrig == "Rice"` in
  [items_prod_full](https://eduaguilera.github.io/whep/reference/items_prod_full.md)
  (2807, rice and products). Stamped `"ipcc2019_ef1fr"`.

- `EF3PRP` (same table) on excreta deposited by grazing animals, as the
  N-weighted blend
  `excreta_cpp_share * EF3PRP,CPP + (1 - excreta_cpp_share) * EF3PRP,SO`,
  with `EF3PRP,CPP` 0.006 wet and 0.002 dry for cattle, poultry and pigs
  and `EF3PRP,SO` 0.003 for sheep and other animals. Excreta keep this
  factor on a rice row too: Equation 11.1 prices urine and dung
  deposited by grazing animals apart from the N applied to the crop. A
  row labelled by species reads its share from the label: 1 on
  `"Excreta_cattle_monog"`, 0 on `"Excreta_other"`. Stamped
  `"ipcc2019_ef3prp_blend"`.

- Excreta labelled so only as the fallback of a manure-type split
  (`method_excreta_species == "fallback_label_no_species"`) is collected
  manure applied to the land, not grazing deposition: it takes the
  factor of applied manure on its row (`EF1` for other N inputs, `EF1FR`
  on a rice item), whatever its missing species mix. Stamped
  `"ipcc2019_fallback_applied_manure"`.

`"ipcc2019_atl_med"` is the earlier IPCC 2019 Tier 1 option keyed on the
Atlantic/Mediterranean `climate` instead: the
[n2o_efs_disaggregated](https://eduaguilera.github.io/whep/reference/n2o_efs_disaggregated.md)
table's two climate-level rows (`irrig_type == "Tier_1"` for ATL,
`irrig_type == "Med_average"` for MED, 0.010 and 0.005), whatever the
input. `"aguilera"` is a finer Mediterranean-calibrated disaggregation
(Cayuela et al. 2017), selectable where its `irrig_type`/`fert_type`
granularity is available and its regional emission factors apply:
`ef * mf`, `ef` from
[n2o_efs_disaggregated](https://eduaguilera.github.io/whep/reference/n2o_efs_disaggregated.md)
on `(irrig_type, climate)`, `mf` from
[fertiliser_n2o_modifiers](https://eduaguilera.github.io/whep/reference/fertiliser_n2o_modifiers.md)
on `(fert_type, climate)`. An `"Excreta"` row, whose species are carried
as a share rather than in its label, takes
`excreta_cpp_share * mf(Excreta_cattle_monog) + (1 - excreta_cpp_share) * mf(Excreta_other)`,
stamped `"aguilera_excreta_blend"`; a fallback-labelled `"Excreta"` row,
which is collected manure applied to the land and not grazing
deposition, takes `mf(Solid)` (0.38 on MED, 0.60 on ATL), stamped
`"aguilera_fallback_applied_manure"`. `"ipcc2006"` uses the
[n2o_efs_ipcc2006](https://eduaguilera.github.io/whep/reference/n2o_efs_ipcc2006.md)
table (IPCC 2006 Tier 1 defaults, flat 0.010 except flooded rice 0.003),
keyed like `"aguilera"` on `(irrig_type, climate)` with no `mf`.

## Usage

``` r
calculate_soil_n2o(
  x,
  method = c("ipcc2019", "ipcc2019_atl_med", "aguilera", "ipcc2006"),
  example = FALSE
)
```

## Source

IPCC (2019) 2019 Refinement to the 2006 IPCC Guidelines for National
Greenhouse Gas Inventories, Vol 4, Ch 11, Table 11.1 (Updated), p.
11.13.

## Arguments

- x:

  A tibble with `n_input_t`. `method = "ipcc2019"` requires `fert_type`
  (`"Synthetic"`, `"Solid"`, `"Liquid"`, `"Human"`, `"Recycling"`,
  `"SOM"` or an excreta label: `"Excreta"`, `"Excreta_cattle_monog"`,
  `"Excreta_other"`), `item_cbs_code` and `moisture_regime`, plus
  `excreta_cpp_share` on `"Excreta"` rows. `"ipcc2019_atl_med"` requires
  `climate`. `"aguilera"` and `"ipcc2006"` require `climate` and
  `irrig_type`, and aguilera `fert_type` (and `excreta_cpp_share` on
  `"Excreta"` rows).

- method:

  `"ipcc2019"` (default, IPCC 2019 Tier 1 wet/dry), `"ipcc2019_atl_med"`
  (IPCC 2019 Tier 1 on ATL/MED), `"aguilera"` (Mediterranean-calibrated)
  or `"ipcc2006"` (IPCC 2006 Tier 1).

- example:

  If `TRUE`, return a small fixture instead of computing from `x`.
  Defaults to `FALSE`.

## Value

`x` with `n2o_direct_n_t` and `method_soil_n2o` appended.
`method_soil_n2o` names the factor each row took: the method name, or
for `"ipcc2019"` one of `"ipcc2019_wet_dry"`, `"ipcc2019_ef1fr"`,
`"ipcc2019_ef3prp_blend"`, `"ipcc2019_fallback_applied_manure"`, and for
`"aguilera"` `"aguilera_excreta_blend"` on blended excreta rows and
`"aguilera_fallback_applied_manure"` on fallback-labelled ones.

## Details

`method = "ipcc2019"` aborts when `moisture_regime` is absent
(`whep_missing_moisture_regime`), missing on any row
(`whep_moisture_regime_na`, naming the count) or neither `"wet"` nor
`"dry"` (`whep_moisture_regime_value`): which class a row belongs to is
decided where the drivers are built, not here. An excreta row carrying N
without an `excreta_cpp_share` aborts (`whep_excreta_share_missing`), as
does a share outside 0 to 1 (`whep_excreta_share_invalid`) and a share
on a species-labelled row that contradicts its label
(`whep_excreta_share_label_conflict`); a row with no excreta N emits
nothing whatever its mix. A `fert_type` with no direct factor in
Equation 11.1 (`"BNF"`, `"Deposition"`) aborts
(`whep_soil_n2o_fert_type`).

## Examples

``` r
calculate_soil_n2o(example = TRUE)
#> # A tibble: 4 × 7
#>   n_input_t fert_type item_cbs_code moisture_regime excreta_cpp_share
#>       <dbl> <chr>             <int> <chr>                       <dbl>
#> 1        10 Synthetic          2511 wet                          NA  
#> 2        10 Solid              2511 dry                          NA  
#> 3        10 Synthetic          2807 wet                          NA  
#> 4        10 Excreta            3000 wet                           0.5
#> # ℹ 2 more variables: n2o_direct_n_t <dbl>, method_soil_n2o <chr>
```
