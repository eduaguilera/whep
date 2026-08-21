# Which territorial identity WHEP's year-less objects carry

A WHEP polity code is year-scoped: `ESP-1846-1914` and `ESP-1978-2025`
are different rows of
[polities](https://eduaguilera.github.io/whep/reference/polities.md).
Several WHEP objects have no year dimension at all – the cell/area
grids, the static crop-pattern weights, coefficient tables keyed by a
country label – so "attach the polity code" has no single answer for
them. This is the register of the answer each one carries, so that the
choice is stated rather than left to whichever join a consumer happens
to write.

It decides nothing at call time: the table is a declaration, and
`tests/testthat/test_territorial_identity.R` checks every row of it
against the object it describes, including that no year-less
territory-keyed dataset is missing from it.

## Usage

``` r
polity_identity_conventions(kind = NULL)
```

## Arguments

- kind:

  Optional character vector restricting the result to one or more of
  `"package_data"`, `"input_pin"` and `"function_output"`. `NULL`
  (default) returns every row.

## Value

A `tibble` with one row per registered object and the columns:

- `object`: The dataset name, pin alias or function it describes.

- `kind`: `"package_data"`, `"input_pin"` or `"function_output"`.

- `territory_key`: The column(s) naming a territory, comma-separated, or
  `NA` when the object names none.

- `identity`: `"present_day_polity"`, `"polity_period"` or
  `"identity_free"`.

- `status`: `"carried"` when the object materialises the identity today,
  `"resolved_by_consumer"` when the consumer attaches it with its own
  year, `"recommended"` when this is the proposed convention and not yet
  the implemented one.

- `carrier`: The column holding the polity code, `NA` when there is
  none.

- `resolver`: The supported call that attaches identity, `NA` when none
  is needed.

- `rationale`: One sentence saying why.

## The three conventions

- `"present_day_polity"`:

  The object is a registry of what reports *today*, so the present-day
  polity is what it means. Its `carrier` column holds a real
  [polities](https://eduaguilera.github.io/whep/reference/polities.md)
  code, the one `add_polity_code(year_column = NULL)` resolves – the
  crosswalk period running to the open end.

- `"polity_period"`:

  One row of the object covers territory that changed hands, so the
  object needs a validity interval and has to become year-aware.
  [`build_polycell_support()`](https://eduaguilera.github.io/whep/reference/build_polycell_support.md)
  on the `edu/polycell-spatial-support` branch is the worked precedent
  for the grid: one row per (cell, polity, validity interval).

- `"identity_free"`:

  The object is a coefficient about a *place*, or a label whose referent
  changes with the year it is applied in, so carrying one polity code
  would be false precision. Identity is attached at the consumer, with
  the consumer's year, through the `resolver` call.

## Why a year-less object cannot simply be handed a code

Measured on the deployed `spatialize-country-grid` pin (58,795 cells,
178 area codes, no year): **52,420 cells, 89.2%, sit under an
`area_code` that
[polity_area_crosswalk](https://eduaguilera.github.io/whep/reference/polity_area_crosswalk.md)
maps to more than one polity over time**, so which polity a cell belongs
to is a function of the year the grid does not have. The same holds for
country labels resolved through
[`resolve_polity_label()`](https://eduaguilera.github.io/whep/reference/resolve_polity_label.md):
33 of
[mueller_synthetic_n](https://eduaguilera.github.io/whep/reference/mueller_synthetic_n.md)'s
156 `iso3c` values, 37 of
[crops_manure_n](https://eduaguilera.github.io/whep/reference/crops_manure_n.md)'s
184 `ISO` values and 38 of `gleam_geographic_hierarchy`'s 204 `iso3`
values name a *different* polity at 1961 than at 2020.

## What counts as a territory here

A supra-national statistical grouping – an IPCC or GLEAM region, a UN
sub-region, `region_krausmann` – is **not** a territory and is out of
scope by construction: it names a class of places, never a state, so it
can never acquire a polity code. Only the column names in
`whep:::.territory_key_names()` put an object in this register. That is
also why a bare `code` is not one of them: `biomass_coefs$Code` is a
crop.

## The trap this register exists to make visible

[regions_full](https://eduaguilera.github.io/whep/reference/regions_full.md)
and
[polities_cats](https://eduaguilera.github.io/whep/reference/polities_cats.md)
carry a column of ISO3-like stems (`"AFG"`, `"ROW"`, `"RAFR"`) kept for
older callers, of which **not one value is a
[polities](https://eduaguilera.github.io/whep/reference/polities.md)
code**. Until whep#687 that column was literally named `polity_code`, so
a join from either dataset to
[polities](https://eduaguilera.github.io/whep/reference/polities.md) or
[polity_area_crosswalk](https://eduaguilera.github.io/whep/reference/polity_area_crosswalk.md)
on the one column whose name promised identity came back empty and
nothing warned. It is now `legacy_polity_prefix`, which claims nothing.
Their real carrier is `reporting_polity_code`, a
[polities](https://eduaguilera.github.io/whep/reference/polities.md)
code on all 259 of `regions_full`'s non-`NA` rows and all 198 of
`polities_cats`'s.

[polity_area_crosswalk](https://eduaguilera.github.io/whep/reference/polity_area_crosswalk.md)
vendors the same stems, and shipped them under `reporting_polity_code` —
the name that means the opposite — until whep#711 renamed them
`legacy_polity_prefix`/`legacy_polity_name` to match. The register does
not reach that table, because it enumerates year-*less* objects and the
crosswalk carries a year; the guard for it lives in
`tests/testthat/test_territorial_identity.R` alongside \#687's.

## See also

[`add_polity_code()`](https://eduaguilera.github.io/whep/reference/add_polity_code.md)
for the numeric-code route,
[`resolve_polity_label()`](https://eduaguilera.github.io/whep/reference/resolve_polity_label.md)
for the label route, and
[whep_polity_columns](https://eduaguilera.github.io/whep/reference/whep_polity_columns.md)
for what a year-*aware* WHEP output carries.

## Examples

``` r
polity_identity_conventions() |>
  dplyr::select(object, kind, identity, status)
#> # A tibble: 11 × 4
#>    object                     kind            identity           status         
#>    <chr>                      <chr>           <chr>              <chr>          
#>  1 regions_full               package_data    present_day_polity carried        
#>  2 polities_cats              package_data    present_day_polity carried        
#>  3 gleam_geographic_hierarchy package_data    present_day_polity carried        
#>  4 mueller_synthetic_n        package_data    identity_free      resolved_by_co…
#>  5 crops_manure_n             package_data    identity_free      resolved_by_co…
#>  6 gleam_dressing_percentages package_data    identity_free      resolved_by_co…
#>  7 gleam_fracremove           package_data    identity_free      resolved_by_co…
#>  8 gleam_mechanization_levels package_data    identity_free      resolved_by_co…
#>  9 spatialize-crop-patterns   input_pin       identity_free      carried        
#> 10 spatialize-country-grid    input_pin       polity_period      recommended    
#> 11 build_cell_polity          function_output polity_period      recommended    

polity_identity_conventions(kind = "package_data") |>
  dplyr::select(object, carrier, resolver)
#> # A tibble: 8 × 3
#>   object                     carrier               resolver                     
#>   <chr>                      <chr>                 <chr>                        
#> 1 regions_full               reporting_polity_code add_polity_code(year_column …
#> 2 polities_cats              reporting_polity_code add_polity_code(year_column …
#> 3 gleam_geographic_hierarchy reporting_polity_code resolve_polity_label(iso3, y…
#> 4 mueller_synthetic_n        NA                    resolve_polity_label(iso3c, …
#> 5 crops_manure_n             NA                    resolve_polity_label(ISO, so…
#> 6 gleam_dressing_percentages NA                    resolve_polity_label(country…
#> 7 gleam_fracremove           NA                    resolve_polity_label(country…
#> 8 gleam_mechanization_levels NA                    resolve_polity_label(country…
```
