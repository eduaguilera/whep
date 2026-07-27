# Assemble gridded nitrogen inputs from every WHEP N-input source.

Combines biological nitrogen fixation
([`calculate_bnf()`](https://eduaguilera.github.io/whep/reference/calculate_bnf.md)),
residue/root N recycling
([`calculate_npp_carbon_nitrogen()`](https://eduaguilera.github.io/whep/reference/calculate_npp_carbon_nitrogen.md)),
livestock manure
([`build_livestock_nutrient_flows()`](https://eduaguilera.github.io/whep/reference/build_livestock_nutrient_flows.md)),
atmospheric deposition
([`build_n_deposition()`](https://eduaguilera.github.io/whep/reference/build_n_deposition.md)),
urban/human-excreta N
([`build_urban_n()`](https://eduaguilera.github.io/whep/reference/build_urban_n.md)),
soil organic-matter mineralization
([`build_carbon_balance()`](https://eduaguilera.github.io/whep/reference/build_carbon_balance.md)'s
`son_change_kgn_ha`) and synthetic fertiliser (a country total
spatialized to crops and cells via
[`spatialize_country_n_to_crops()`](https://eduaguilera.github.io/whep/reference/spatialize_country_n_to_crops.md))
into one long-format tibble of nitrogen inputs to agricultural land.

`fert_type` values: `"bnf"`, `"recycling"`, `"manure_solid"`,
`"manure_liquid"`, `"excreta"`, `"deposition"`, `"urban"`,
`"som_mineralization"`, `"synthetic"` and `"accum_loss"`. The last is a
documented gap (perennial-crop standing-biomass N accumulation from
Spain_Hist's N_balance.R): its source computation was not available for
this task, so it is never emitted, only reserved in the vocabulary.

Terms that are fundamentally per-cell or per-land-use rather than
per-crop (`"deposition"`, `"urban"`, `"som_mineralization"`, and
transported manure whose pooled crop-plus-grass sink does not identify a
single landing crop) carry `item_cbs_code = NA_integer_`, the same "no
specific item" sentinel already used package-wide for non-crop rows.

## Usage

``` r
build_n_inputs(
  years = NULL,
  resolution = c("grid", "polity"),
  data = list(),
  example = FALSE
)
```

## Arguments

- years:

  Optional integer vector of calendar years to keep. `NULL` keeps every
  year the assembled inputs cover.

- resolution:

  `"grid"` (default, per cell/crop/year/fert_type) or `"polity"` (summed
  to `area_code`/`item_cbs_code`/`year`/`fert_type`).

- data:

  Named list of pre-loaded, caller-supplied upstream inputs. Each of the
  following is required for its corresponding `fert_type` to be emitted
  (a missing one silently skips that source rather than erroring, since
  callers may only want a subset):

  - `bnf_input`:
    [`calculate_bnf()`](https://eduaguilera.github.io/whep/reference/calculate_bnf.md)'s
    required input tibble (`lon`, `lat`, `area_code`, `year`,
    `item_prod_code`, `crop_npp_n_t`, `product_n_t`, `weed_npp_n_t`,
    `land_use`, `legumes_seeded`, `seeded_cover_crop_share`, `area_ha`).

  - `npp_n_input`:
    [`calculate_npp_carbon_nitrogen()`](https://eduaguilera.github.io/whep/reference/calculate_npp_carbon_nitrogen.md)'s
    required input tibble (`lon`, `lat`, `area_code`, `year`,
    `item_prod_code`, `item_cbs_code`, `product_dm_t`, `residue_dm_t`,
    `root_dm_t`, optionally `residue_soil_dm_t`).

  - `livestock_intake`:
    [`build_livestock_nutrient_flows()`](https://eduaguilera.github.io/whep/reference/build_livestock_nutrient_flows.md)'s
    `intake` argument (the
    [`redistribute_feed()`](https://eduaguilera.github.io/whep/reference/redistribute_feed.md)
    realised-intake contract), plus `gridded` (its land-surface layer)
    and `resolution`/`methods` (forwarded as-is).

  - `nhx`, `noy`, `cell_polity`:
    [`build_n_deposition()`](https://eduaguilera.github.io/whep/reference/build_n_deposition.md)'s
    inputs.

  - `urban_population`, `cropland_ha`, `cell_polity`:
    [`build_urban_n()`](https://eduaguilera.github.io/whep/reference/build_urban_n.md)'s
    inputs.

  - `carbon_balance`:
    [`build_carbon_balance()`](https://eduaguilera.github.io/whep/reference/build_carbon_balance.md)'s
    `"grid"`-resolution output (`lon`, `lat`, `area_code`, `land_use`,
    `year`, `area_ha`, `son_change_kgn_ha`); this driver requires it
    supplied directly, it is never computed here.

  - `primary_prod`, `fertilizer`, `crop_patterns`, `type_cropland`,
    `cell_polity`: the synthetic-fertiliser assembly (country total from
    `fertilizer`, the `faostat-fertilizer-nutrients` pin, split to crops
    by `primary_prod` harvested-area shares, then to cells by
    `crop_patterns`/`type_cropland`).

  - `gridded`, `resolution` (of the manure engine, default
    `"national"`), `methods`: forwarded to
    [`build_livestock_nutrient_flows()`](https://eduaguilera.github.io/whep/reference/build_livestock_nutrient_flows.md).

- example:

  If `TRUE`, return a small fixture instead of assembling real data.
  Defaults to `FALSE`.

## Value

A tibble. At `resolution = "grid"`: `lon`, `lat`, `area_code`,
`item_cbs_code`, `year`, `fert_type`, `n_input_t`, `method_recycling_n`.
At `resolution = "polity"`: `area_code`, `item_cbs_code`, `year`,
`fert_type`, `method_recycling_n`, `n_input_t` (summed over cells).
`method_recycling_n` records which residue basis the `"recycling"` term
used: `"residue_soil_returned"` when the upstream NPP input supplied
`residue_soil_dm_t` (residue N net of removal for feed/fuel/burning) or
`"total_residue"` when only gross residue N was available; it is `NA`
for every other `fert_type`.

## Examples

``` r
build_n_inputs(example = TRUE)
#> # A tibble: 9 × 8
#>     lon   lat area_code item_cbs_code  year fert_type          n_input_t
#>   <dbl> <dbl>     <int>         <int> <int> <chr>                  <dbl>
#> 1 -0.25 -0.25         1          2511  2020 bnf                      3.2
#> 2 -0.25 -0.25         1          2511  2020 recycling                5.6
#> 3 -0.25 -0.25         1          2511  2020 synthetic               12.4
#> 4 -0.25 -0.25         1            NA  2020 deposition               0.9
#> 5 -0.25 -0.25         1            NA  2020 urban                    4.5
#> 6 -0.25 -0.25         1            NA  2020 som_mineralization       1.1
#> 7 -0.25 -0.25         1          3000  2020 excreta                  2.3
#> 8 -0.25 -0.25         1          2511  2020 manure_solid             1.8
#> 9 -0.25 -0.25         1          2511  2020 manure_liquid            0.7
#> # ℹ 1 more variable: method_recycling_n <chr>
```
