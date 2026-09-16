# Estimate livestock nitrogen, carbon and volatile-solids excretion.

Converts realised feed intake (the output of
[`redistribute_feed()`](https://eduaguilera.github.io/whep/reference/redistribute_feed.md))
into excreted nitrogen, carbon and volatile solids per
`year x territory x sub_territory x livestock_category`. All excretion
methods share one canonical nitrogen intake,
`n_intake = sum(intake_dm_t * feed_n_content)`, so the methods are
directly comparable.

Carbon is the carbon of the excreted organic matter,
`vs_excretion * c_vs_fraction`, so it inherits the
intake-and-digestibility mass balance of the volatile solids. Urine
carbon is not part of the volatile solids under
`method_vs = "intake_digestibility"` and is not counted: at a C:N near
0.9 it is under a tenth of a dairy cow's excreted carbon (Dijkstra et
al. 2018, Table 5) and is respired within days of deposition, so it
never reaches the soil carbon this feeds.

## Usage

``` r
estimate_n_excretion(intake, options = list())
```

## Arguments

- intake:

  A tibble of realised feed intake with at least `year`, `territory`,
  `sub_territory`, `livestock_category`, `item_cbs_code`, `feed_quality`
  and `intake_dm_t` (the
  [`redistribute_feed()`](https://eduaguilera.github.io/whep/reference/redistribute_feed.md)
  result). `territory` is a stringified `area_code`
  (`as.character(area_code)`, what
  [`redistribute_feed()`](https://eduaguilera.github.io/whep/reference/redistribute_feed.md)
  emits and what the whole manure chain carries through to the nitrogen
  inputs); an `iso3c` literal is still resolved there but is deprecated,
  since it can only answer with an aggregation bucket and so loses the
  territory for 62 of the 257 codes it knows.

- options:

  A named list of method options:

  - `method`: `"intake_minus_retention"` (default,
    `n_intake * (1 - n_retention_frac)`) or `"intake_minus_product_n"`
    (`n_intake - product_n`).

  - `method_vs`: `"intake_digestibility"` (default,
    `intake_dm_t * (1 - digestibility) * (1 - ash)`).

  - `method_c`: `"volatile_solids"` (default and only method,
    `vs_excretion * c_vs_fraction`).

  - `c_vs_fraction`: carbon per unit of volatile solids, kg C / kg VS.
    Default 0.47; see Details.

  - `forage_n`: nitrogen content of the grazed forage that intake rows
    with no `item_cbs_code` take. `"assumed_midrange"` (default, 0.02 kg
    N/kg DM, an assumed unverified value), `"gleam_grass_fresh"`
    (0.022), `"gleam_grass_hay"` (0.017) and `"gleam_grass_mean"` from
    GLEAM 3.0 Supplement S1 Tab. S.3.3, or `"biomass_coefs_grass"`
    (0.0174) from the `bio_coefs` `Grass` row.

  - `product_n`: a tibble (`year`, `territory`, `sub_territory`,
    `livestock_category`, `product_n`) required by
    `"intake_minus_product_n"`.

## Value

A tibble with one row per
`year x territory x sub_territory x livestock_category` and columns
`n_intake`, `n_excretion`, `c_excretion`, `vs_excretion`,
`method_n_excretion`, `method_vs`, `method_c_excretion` and
`method_forage_n`.

## Details

The default `c_vs_fraction` of 0.47 kg C per kg of volatile solids is
the carbon content of the components that make up faecal organic matter
in Dijkstra et al. (2018, Front. Sustain. Food Syst. 2:63,
doi:10.3389/fsufs.2018.00063, Table 1): fibre 0.44, microbial organic
matter 0.47, protein 0.52 and lipids 0.75 g C per g dry matter, weighted
towards the fibre and microbial debris that dominate faeces. Measured
manures bracket it: 0.52 for fresh bedded dairy manure (Choi et al.
2022, PeerJ 10:e14134, doi:10.7717/peerj.14134, Table 1: 43.3% C and
83.3% VS of dry matter) and 0.39-0.46 for stored cattle and pig manure
(Baek et al. 2020, Int. J. Environ. Res. Public Health 17:4737,
doi:10.3390/ijerph17134737, Table 1).

## Examples

``` r
intake <- tibble::tribble(
  ~year, ~territory, ~sub_territory, ~livestock_category,
  ~item_cbs_code, ~feed_quality, ~intake_dm_t,
  2020L, "203", NA, "Cattle_milk", 2513L, "high_quality", 100,
  2020L, "203", NA, "Cattle_milk", NA, "grass", 500
)
estimate_n_excretion(intake)
#> # A tibble: 1 × 12
#>    year territory sub_territory livestock_category n_intake n_excretion
#>   <int> <chr>     <lgl>         <chr>                 <dbl>       <dbl>
#> 1  2020 203       NA            Cattle_milk            11.9        9.49
#> # ℹ 6 more variables: c_excretion <dbl>, vs_excretion <dbl>,
#> #   method_n_excretion <chr>, method_vs <chr>, method_c_excretion <chr>,
#> #   method_forage_n <chr>
```
