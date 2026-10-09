# Resolve source-native admin identifiers to polity codes

Attach `level_polity_code` to administrative-statistics rows by
resolving each row's **source-native identifier** – not its name –
through
[`resolve_polity_label()`](https://eduaguilera.github.io/whep/reference/resolve_polity_label.md),
under a source slug naming the code system the identifier belongs to
and, for NUTS geographies, its nomenclature version. Rows that resolve
to nothing keep `NA` and are counted in the returned diagnostics table
rather than being dropped.

This is the load-time step the admin-shares design puts between the
readers, which emit source-native keys only, and every consumer that
needs a polity. Resolving here rather than at pin-build time is why the
`admin-shares` pin does not need a staleness warning; see below.

## Usage

``` r
resolve_admin_units(x, code_system, year_col = "year", aliases = NULL)
```

## Arguments

- x:

  Administrative-statistics rows, in the shape the readers emit: a data
  frame carrying `source`, `source_native_unit_id`, the year column
  named by `year_col`, and `nuts_version` where `code_system` is a NUTS
  system. `source_native_unit_name` is never read. The admin-shares
  contract
  ([`admin_shares_schema()`](https://eduaguilera.github.io/whep/reference/admin_shares_schema.md))
  calls the same identifier `source_native_id`, so a caller resolving
  that table renames the column first. An existing `level_polity_code`
  is overwritten: this function is the authority on it.

- code_system:

  Code system the identifiers belong to, one of the slugs in the table
  above. Length 1, or one value per row of `x`.

- year_col:

  Name of the year column in `x`. Defaults to `"year"`.

- aliases:

  Optional alias table in the
  [polity_label_aliases](https://eduaguilera.github.io/whep/reference/polity_label_aliases.md)
  schema, used **instead of** the package data. `NULL`, the default and
  what production calls use, resolves against the published
  [polity_label_aliases](https://eduaguilera.github.io/whep/reference/polity_label_aliases.md)
  through the ALIAS ROUTE ONLY – never
  [`resolve_polity_label()`](https://eduaguilera.github.io/whep/reference/resolve_polity_label.md)
  itself, whose name and ISO3 identity routes would let an
  administrative identifier that happens to collide with a polity name
  or ISO3 code resolve to the wrong thing (usually the container). An
  injected table takes the same alias-only route, with the same source
  and year scoping, so the two paths agree on everything but which table
  they read.

## Value

A list of two tibbles:

- `rows`: `x` with `alias_source` (the slug each row was resolved under,
  `NA` where none could be built) and `level_polity_code` (the resolved
  polity, `NA` where nothing matched).

- `diagnostics`: one row per `(source, alias_source)` with `n_rows`,
  `n_unresolved` and `example_ids` – up to five distinct unresolved
  identifiers, `"|"`-joined in the C locale, `NA` when the group
  resolved fully. Ordered by `n_unresolved`, descending. A row with no
  native identifier at all – a derived residual, in
  [`admin_shares_schema()`](https://eduaguilera.github.io/whep/reference/admin_shares_schema.md)'s
  terms – cannot resolve and is counted here like any other unresolved
  row, contributing no example id.

## Alias source slugs

`code_system` names the identifier's code system; the slug passed to
[`resolve_polity_label()`](https://eduaguilera.github.io/whep/reference/resolve_polity_label.md)
is built per row from it:

- `"usda-nass-fips"`: the state FIPS code
  [`read_admin_stats_nass()`](https://eduaguilera.github.io/whep/reference/read_admin_stats_nass.md)
  emits. Slug as given.

- `"eurostat-nuts"`: the NUTS code
  [`read_admin_stats_eurostat()`](https://eduaguilera.github.io/whep/reference/read_admin_stats_eurostat.md)
  emits. Slug `"eurostat-nuts<nuts_version>"`, e.g.
  `"eurostat-nuts2016"`.

- `"jrc-nuts"`: the NUTS code of the JRC subnational release
  (`inst/extdata/jrc_subnational_source_manifest.csv`), coded on
  NUTS 2016. Slug `"jrc-nuts<nuts_version>"`.

- `"ibge-uf"`: the UF code
  [`read_admin_stats_sidra()`](https://eduaguilera.github.io/whep/reference/read_admin_stats_sidra.md)
  emits. Slug as given.

- `"whep-lab-<family>"`: the compilation's own `admin_unit_id`, as
  [`read_admin_family()`](https://eduaguilera.github.io/whep/reference/read_admin_family.md)
  emits it. Slug as given.

The five `"whep-lab-"` slugs are the five tier-2/3 families
[`read_admin_family()`](https://eduaguilera.github.io/whep/reference/read_admin_family.md)
reads, with the `"admin-stats-"` prefix replaced: `"whep-lab-japan"`,
`"whep-lab-spain-provinces"`, `"whep-lab-australia"`,
`"whep-lab-france-livestock"` and `"whep-lab-latam"`. Their identifiers
are the compilation's own (`"JPN-AICHI"`, `"ESP-ES111"`, `"FRA-FR102"`),
which is why each family is its own code system rather than a shared
one.

Identifiers are normalised exactly as
[`resolve_polity_label()`](https://eduaguilera.github.io/whep/reference/resolve_polity_label.md)
normalises a label, on both sides of the comparison, so the injected and
published routes agree on what a key is.

## Alias rows this needs from whep-polities

The alias rows are a whep-polities deliverable and are **not published
yet**: on the 2026-09-03 snapshot,
[polity_label_aliases](https://eduaguilera.github.io/whep/reference/polity_label_aliases.md)
holds 1,007 rows scoped to 15 sources (`crops-manure-n`, `fao`,
`fao1952`, `faostat`, `federico_tena`, `iia`, `iia-cotton`, `iia-tea`,
`juan`, `lassaletta-grassland-share`, `mitchell`, `mueller-synthetic-n`,
`sa_colonial`, `trade-sources`, `whep-split-2026-06-29`) and none of the
code systems above. Until they land, `aliases = NULL` resolves every
administrative unit to `NA` – visibly, and counted – which is the honest
state, and the tests inject the rows they need.

One example row per slug, for the fixture countries, in the
[polity_label_aliases](https://eduaguilera.github.io/whep/reference/polity_label_aliases.md)
schema (`source_label`, `source`, `year_start`, `year_end`,
`polity_code`): `"19"` under `"usda-nass-fips"` for the Iowa state
polity; `"FR21"` under `"eurostat-nuts2013"` and `"FRF2"` under
`"eurostat-nuts2016"`, both for the one Champagne-Ardenne polity the
recoding renames; `"FRF2"` under `"jrc-nuts2016"` for the same one;
`"35"` under `"ibge-uf"` for the Sao Paulo state polity; `"ESP-ES111"`
under `"whep-lab-spain-provinces"` for the NUTS-3 province `ES111`; and
`"JPN-AICHI"` under `"whep-lab-japan"` for `JPN-23-1871-2025`, the one
target polity that already exists in the shipped
[polities](https://eduaguilera.github.io/whep/reference/polities.md).

## Why there is no staleness warning

The `admin-shares` pin is keyed on source-native identifiers and carries
no resolved polity code, so nothing in it can disagree with a newer
[polities](https://eduaguilera.github.io/whep/reference/polities.md)
snapshot: the resolution is redone here on every load. A pin that froze
resolved codes would need a `.warn_stale_admin_shares()` guard to say
which snapshot it was resolved against; this one has nothing to warn
about, which is why the plan chose source-native pinning.

## Examples

``` r
# Two NUTS vintages of one code, resolved under their own slugs. The
# alias rows are injected, and their polity code is illustrative: the
# real rows are the whep-polities deliverable described above.
rows <- tibble::tibble(
  source = "Eurostat_apro_cpnhr_h",
  source_native_unit_id = c("FR21", "FRF2", "FR83"),
  source_native_unit_name = c("Champagne-Ardenne", "idem", "Corse"),
  nuts_version = c("2013", "2016", "2013"),
  year = c(1995L, 1995L, 1995L)
)
aliases <- tibble::tibble(
  source_label = c("FR21", "FRF2"),
  source = c("eurostat-nuts2013", "eurostat-nuts2016"),
  year_start = NA_integer_,
  year_end = NA_integer_,
  polity_code = "FR-CHAMPAGNE-ARDENNE",
  common_name = "Champagne-Ardenne",
  confidence = "high",
  observed_rows = NA_integer_
)
resolved <- resolve_admin_units(rows, "eurostat-nuts", aliases = aliases)
resolved$rows[, c("alias_source", "level_polity_code")]
#> # A tibble: 3 × 2
#>   alias_source      level_polity_code   
#>   <chr>             <chr>               
#> 1 eurostat-nuts2013 FR-CHAMPAGNE-ARDENNE
#> 2 eurostat-nuts2016 FR-CHAMPAGNE-ARDENNE
#> 3 eurostat-nuts2013 NA                  
resolved$diagnostics
#> # A tibble: 2 × 5
#>   source                alias_source      n_rows n_unresolved example_ids
#>   <chr>                 <chr>              <int>        <int> <chr>      
#> 1 Eurostat_apro_cpnhr_h eurostat-nuts2013      2            1 FR83       
#> 2 Eurostat_apro_cpnhr_h eurostat-nuts2016      1            0 NA         
```
