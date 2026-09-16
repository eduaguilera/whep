# Build per-cell livestock greenhouse-gas emissions.

Run the IPCC 2019 livestock emission model on a gridded herd, one
0.5-degree cell at a time, and return enteric CH4, manure CH4 and manure
N2O in kilotonnes per cell, year and species.

Unlike a spatial disaggregation of a national emission total, this
resolves the drivers that actually vary within a country: the
manure-management climate zone and the ambient temperature come from the
cell
([`build_cell_climate_zone()`](https://eduaguilera.github.io/whep/reference/build_cell_climate_zone.md)),
and the diet quality comes from the cell's own feed mix when one is
available. A national total spread over cells cannot show any of that,
because every cell of a country then carries the same implied climate
and diet.

**Both grains are reported.** The gridded sum is the primary output. The
same model is also run once per country on the head-weighted national
mean temperature and the national diet, and each cell additionally
carries that national estimate rescaled onto it (`*_national_kt`) plus
the per-country ratio between the two (`divergence_*`). The global
difference between the two grains is small while the per-country
difference is not, so the per-country ratio is emitted per row rather
than summarised away.

## Usage

``` r
build_gridded_livestock_emissions(
  gridded_livestock = NULL,
  method_diet = c("per_cell_feed", "national_feed", "uniform_medium"),
  tier = 2,
  data = list(),
  example = FALSE
)
```

## Arguments

- gridded_livestock:

  A tibble of gridded head counts (required unless `example = TRUE`),
  for example from
  [`build_gridded_livestock()`](https://eduaguilera.github.io/whep/reference/build_gridded_livestock.md),
  with columns `lon`, `lat`, `year`, `area_code`, `heads`, and either
  `species` (an IPCC species label such as `"Cattle, dairy"`) or
  `species_group` (a spatializer group label). Groups that name more
  than one IPCC species (`"sheep_goats"`, `"equines"`, `"poultry"`,
  `"other"`) abort rather than being split on an assumption. Polity
  columns and cell identifiers are preserved when present.

- method_diet:

  How each row's `diet_quality` is resolved, in decreasing rigour:

  - `"per_cell_feed"` (default): from the cell's own feed mix, falling
    back per row to that country's national mix where a cell has no
    classifiable feed. Needs `data$feed_intake` at cell grain (a
    `sub_territory` column), for example from
    [`build_feed_intake_local()`](https://eduaguilera.github.io/whep/reference/build_feed_intake_local.md).

  - `"national_feed"`: from the country's feed mix. Needs
    `data$feed_intake`, for example from
    [`get_feed_intake()`](https://eduaguilera.github.io/whep/reference/get_feed_intake.md).

  - `"uniform_medium"`: every row gets the IPCC `"Medium"` diet. This is
    an assumption, not a measurement, and is never selected implicitly.

  Whatever is requested, the value actually used is recorded per row in
  `method_diet`. A row that no requested method resolves aborts.

- tier:

  IPCC tier, `2` (default) or `1`. Tier 2 is the default here because
  the per-cell drivers only enter the Tier 2 energy and
  manure-management equations; Tier 1 emission factors carry no climate,
  temperature or diet dimension, so a Tier 1 grid differs from a
  disaggregated national total only by rounding.

- data:

  Optional named list of pre-loaded inputs: `cell_climate` (a
  [`build_cell_climate_zone()`](https://eduaguilera.github.io/whep/reference/build_cell_climate_zone.md)
  output) and `feed_intake` (a feed-intake table). `cell_climate` falls
  back to
  [`build_cell_climate_zone()`](https://eduaguilera.github.io/whep/reference/build_cell_climate_zone.md),
  which reads CRU from `WHEP_CRU_DIR`. `feed_intake` has no fallback:
  the readers that produce it rebuild the whole feed allocation, so it
  is supplied or the diet method is `"uniform_medium"`.

- example:

  If `TRUE`, return a small fixture instead of reading remote data.
  Defaults to `FALSE`.

## Value

A tibble with one row per `year`, `area_code`, `lon`, `lat` and
`species`:

- `heads`: Head count in the cell.

- `enteric_ch4_kt`, `manure_ch4_kt`, `manure_n2o_kt`: Gridded emissions
  (kilotonnes), the primary output.

- `enteric_ch4_national_kt`, `manure_ch4_national_kt`,
  `manure_n2o_national_kt`: The national-grain estimate rescaled onto
  the cell, so summing these over a country reproduces the
  national-grain run.

- `divergence_enteric_ch4`, `divergence_manure_ch4`,
  `divergence_manure_n2o`: Per-country ratio of the gridded total to the
  national-grain total. `1` means the two grains agree.

- `mean_annual_temp_c`, `climate_zone`, `diet_quality`: The resolved
  per-cell drivers.

- `method_climate_zone`, `method_diet`, `method_enteric`,
  `method_manure_ch4`, `method_manure_n2o`: Method tracking.

plus the polity columns below.

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
for the reasoning. Where that polity is not live in the row's own year –
41.5% of the pre-1961 `(area, year)` cells –
[`add_polity_code()`](https://eduaguilera.github.io/whep/reference/add_polity_code.md)
says so as `mapping_status == "backcast_anchor"`, and
[`polity_coverage_gaps()`](https://eduaguilera.github.io/whep/reference/polity_coverage_gaps.md)
reports it as `gap_kind == "backcast_anchor"`. These columns do not say
so either way.

A row whose year no mapped period covers is resolved to the NEAREST
period of the same area instead, so `reporting_polity_code` can name a
polity that did not exist in that row's year – FAOSTAT bucket 206 "Sudan
(former)" keeps reporting after `SUD-1956-2011` ends, and its post-2011
rows carry that code. These columns do not say so:
[`add_polity_code()`](https://eduaguilera.github.io/whep/reference/add_polity_code.md)
reports such a row as `mapping_status == "out_of_span"`, and that column
is dropped here so that adding it does not change the schema of every
area-keyed output at once.
[`polity_coverage_gaps()`](https://eduaguilera.github.io/whep/reference/polity_coverage_gaps.md)
reports the stand-in rows of a built table, and
`options(whep.polity_mapping_status = "flag")` (or `"status"`) carries
the signal on the outputs themselves. Both are opt-in; the default is no
extra column.

## Examples

``` r
build_gridded_livestock_emissions(example = TRUE)
#> # A tibble: 3 × 27
#>    year area_code polity_area_code reporting_polity_code reporting_polity_name
#>   <int>     <int>            <int> <chr>                 <chr>                
#> 1  1961       114              114 KEN-1926-1963         Kenya (1926-1963)    
#> 2  1961       114              114 KEN-1926-1963         Kenya (1926-1963)    
#> 3  1961       114              114 KEN-1926-1963         Kenya (1926-1963)    
#> # ℹ 22 more variables: reporting_polity_has_geometry <lgl>, lon <dbl>,
#> #   lat <dbl>, species <chr>, heads <dbl>, mean_annual_temp_c <dbl>,
#> #   climate_zone <chr>, diet_quality <chr>, enteric_ch4_kt <dbl>,
#> #   manure_ch4_kt <dbl>, manure_n2o_kt <dbl>, divergence_enteric_ch4 <dbl>,
#> #   divergence_manure_ch4 <dbl>, divergence_manure_n2o <dbl>,
#> #   enteric_ch4_national_kt <dbl>, manure_ch4_national_kt <dbl>,
#> #   manure_n2o_national_kt <dbl>, method_climate_zone <chr>, …
```
