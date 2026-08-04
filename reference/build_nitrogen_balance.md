# Build the full nitrogen balance: inputs, outputs, losses and NUE.

Assembles
[`build_n_inputs()`](https://eduaguilera.github.io/whep/reference/build_n_inputs.md)'s
long-format nitrogen inputs into the aggregate `N_input_*` sums, adds
the output side (crop production, residue use/burning, grazed weeds,
soil organic-matter sequestration), runs the nitrogen-loss cascade
([`calculate_nh3()`](https://eduaguilera.github.io/whep/reference/calculate_nh3.md),
[`calculate_soil_n2o()`](https://eduaguilera.github.io/whep/reference/calculate_soil_n2o.md),
[`calculate_n_leaching()`](https://eduaguilera.github.io/whep/reference/calculate_n_leaching.md),
[`calculate_indirect_n2o_nh3()`](https://eduaguilera.github.io/whep/reference/calculate_indirect_n2o_nh3.md)),
closes the balance (`N_input_full - N_output_full`), applies the
N-limitation cap on SOM sequestration, and derives
nutrient-use-efficiency (NUE) indicators plus a GWP/CO2e indicator for
the nitrous-oxide streams. Ported from Spain_Hist's
`Balance_parameters()`/`N_Figs.R` equations.

## Usage

``` r
build_nitrogen_balance(
  methods = list(nh3 = "manner", n2o = "ipcc2019", leaching = "meisinger_drainage"),
  resolution = c("grid", "polity"),
  data = list(),
  gwp = c("ar6", "ar5", "ar4"),
  example = FALSE
)
```

## Arguments

- methods:

  A named list of method choices: `nh3` (forwarded to
  [`calculate_nh3()`](https://eduaguilera.github.io/whep/reference/calculate_nh3.md),
  default `"manner"`), `n2o` (forwarded to
  [`calculate_soil_n2o()`](https://eduaguilera.github.io/whep/reference/calculate_soil_n2o.md),
  default `"ipcc2019"`, the globally applicable IPCC 2019 Tier 1 method)
  and `leaching` (forwarded to
  [`calculate_n_leaching()`](https://eduaguilera.github.io/whep/reference/calculate_n_leaching.md),
  default `"meisinger_drainage"`).

- resolution:

  `"grid"` (default) or `"polity"`, as in
  [`build_n_inputs()`](https://eduaguilera.github.io/whep/reference/build_n_inputs.md);
  `"polity"` sums every term (and re-derives every indicator) over
  cells.

- data:

  Named list of pre-loaded upstream inputs. `n_inputs`
  ([`build_n_inputs()`](https://eduaguilera.github.io/whep/reference/build_n_inputs.md)'s
  output) is used directly when supplied, else
  [`build_n_inputs()`](https://eduaguilera.github.io/whep/reference/build_n_inputs.md)
  is called with this SAME `data` list (so its nested readers, e.g.
  `bnf_input`/`npp_n_input`/`livestock_intake`, propagate through). Also
  required:

  - `npp_n_input`: shared with
    [`build_n_inputs()`](https://eduaguilera.github.io/whep/reference/build_n_inputs.md)'s
    `"recycling"` term (see
    [`calculate_npp_carbon_nitrogen()`](https://eduaguilera.github.io/whep/reference/calculate_npp_carbon_nitrogen.md));
    used here for `prod_n_t`.

  - `ag_land_support`: the
    [`build_ag_land_support()`](https://eduaguilera.github.io/whep/reference/build_ag_land_support.md)
    table, supplying `area_ha` (the per-hectare boundary denominator) on
    a physical-land basis. Derived natively when absent and derivable,
    and shared with
    [`build_n_inputs()`](https://eduaguilera.github.io/whep/reference/build_n_inputs.md)
    so both see one table; when neither supplied nor derivable,
    `area_ha` falls back to the harvested area the NPP input carries.

  - `residue_destiny_input`:
    [`calculate_residue_destinies()`](https://eduaguilera.github.io/whep/reference/calculate_residue_destinies.md)'s
    required input (`item_prod_code`, `residue_dm_t`, plus whatever the
    chosen `residue_destiny_method` needs), for `used_residue_n_t`/
    `burnt_residue_n_t`. `residue_destiny_method` selects the method
    (default `"krausmann_regional"`).

  - `livestock_intake`: shared with
    [`build_n_inputs()`](https://eduaguilera.github.io/whep/reference/build_n_inputs.md)'s
    manure term; its `"grass"` `feed_quality` rows drive
    `grazed_weeds_n_t`.

  - `carbon_balance`: shared with
    [`build_n_inputs()`](https://eduaguilera.github.io/whep/reference/build_n_inputs.md)'s
    `"som_ mineralization"` term; its `son_change_kgn_ha` drives
    `som_ sequestration_n_t`.

  - `n_balance_drivers`: additional per-`(key, fert_type)` driver
    columns
    [`calculate_nh3()`](https://eduaguilera.github.io/whep/reference/calculate_nh3.md)/[`calculate_soil_n2o()`](https://eduaguilera.github.io/whep/reference/calculate_soil_n2o.md)/
    [`calculate_indirect_n2o_nh3()`](https://eduaguilera.github.io/whep/reference/calculate_indirect_n2o_nh3.md)
    need beyond `n_input_t`/`fert_type` (`climate`, `irrig_type`, plus
    every MANNER driver column when `methods$nh3 == "manner"`), joined
    by the balance key AND `fert_type` (the Title-case vocabulary from
    [`calculate_nh3()`](https://eduaguilera.github.io/whep/reference/calculate_nh3.md)'s
    own documentation, e.g. `"Synthetic"`, `"Solid"`, `"Excreta_other"`;
    see the file-level "fert_type vocabulary bridge" note for the exact
    mapping from
    [`build_n_inputs()`](https://eduaguilera.github.io/whep/reference/build_n_inputs.md)'s
    lowercase values). A second set of balance-key-only driver columns
    (`fert_type`, `climate`, `irrig_cat`, `tillage`, `som_share`,
    `cn_input`, `land_use`) for
    [`calculate_n_leaching()`](https://eduaguilera.github.io/whep/reference/calculate_n_leaching.md)
    is read from `n_balance_leaching_drivers`, which must hold at most
    one row per balance key (a many-to-one join aborts on duplicate keys
    rather than fanning the rows out and misaligning `drainage_mm`).
    Missing required drivers abort inside the called `calculate_*()`
    function, naming the exact column.

  - `drainage_mm`: annual drainage (mm) for
    [`calculate_n_leaching()`](https://eduaguilera.github.io/whep/reference/calculate_n_leaching.md),
    as a numeric vector aligned to the balance-key rows, or already
    present as a `drainage_mm` column via `n_balance_leaching_drivers`.

- gwp:

  100-year global warming potential standard for N2O, `"ar6"` (default),
  `"ar5"` or `"ar4"`, matching
  [`build_crop_soil_n2o_extension()`](https://eduaguilera.github.io/whep/reference/build_crop_soil_n2o_extension.md).

- example:

  If `TRUE`, return a small fixture instead of assembling real data.
  Defaults to `FALSE`.

## Value

A tibble keyed by `year`/`area_code`/`item_cbs_code` (plus `lon`/`lat`
at `resolution = "grid"`) with `area_ha` (each crop's harvested hectares
in the cell, summed over cells at `resolution = "polity"`; used
downstream to convert tonnes N to a per-hectare rate), the input
aggregates (`n_input_full_t`, `n_input_full_nosom_t`, `n_input_std_t`,
`n_input_som_t`, `n_input_for_n2o_t`), the output aggregates
(`n_output_residues_t`, `n_output_som_t`, `n_output_useful_t`,
`n_output_std_t`, `n_output_full_t`), the loss terms (`nh3_n_t`,
`n2o_direct_n_t`, `no3_n_t`, `denitrification_n_t`,
`n2o_indirect_no3_n_t`, `n2o_indirect_nh3_n_t`), the (post-cap)
`som_sequestration_n_t`, `n_balance_t`, `surplus_t`, `surplus_share`,
the five NUE ratios (`nue_std`, `nue_residues`, `nue_som`, `nue_useful`,
`nue_full`), `total_gwp_co2e_kg`, and the `method_nh3`/
`method_soil_n2o`/`method_leaching` provenance columns, plus the polity
columns below.

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
build_nitrogen_balance(example = TRUE)
#> # A tibble: 1 × 41
#>    year area_code polity_area_code reporting_polity_code reporting_polity_name
#>   <int>     <int>            <int> <chr>                 <chr>                
#> 1  2020        10               10 AUS-1901-2025         Australia            
#> # ℹ 36 more variables: reporting_polity_has_geometry <lgl>,
#> #   item_cbs_code <int>, area_ha <dbl>, n_input_full_t <dbl>,
#> #   n_input_full_nosom_t <dbl>, n_input_std_t <dbl>, n_input_som_t <dbl>,
#> #   n_input_for_n2o_t <dbl>, prod_n_t <dbl>, used_residue_n_t <dbl>,
#> #   burnt_residue_n_t <dbl>, grazed_weeds_n_t <dbl>,
#> #   som_sequestration_n_t <dbl>, n_output_residues_t <dbl>,
#> #   n_output_som_t <dbl>, n_output_useful_t <dbl>, n_output_std_t <dbl>, …
```
