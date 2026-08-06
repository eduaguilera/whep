# Build country per-capita anthropogenic reactive nitrogen.

Aggregates a
[`build_n_inputs()`](https://eduaguilera.github.io/whep/reference/build_n_inputs.md)
long-format nitrogen-input tibble to the total anthropogenic reactive
nitrogen entering each country's agricultural land and divides by
population, giving the per-capita reactive nitrogen (kg N/cap/yr) that
[`build_n_boundary_percapita()`](https://eduaguilera.github.io/whep/reference/build_n_boundary_percapita.md)
consumes as its `n_percapita` input. The default `"synthetic_bnf"`
framing (the Campbell / Global framing) sums the synthetic-fertiliser
and biological-nitrogen- fixation input terms using
`synthetic * syn_tot_agri_ratio + BNF`, the locked Campbell / Global
framing; recycled or internal terms (manure, deposition, urban,
soil-organic-matter mineralization) are excluded. Any finer grid key
(`lon`, `lat`, `item_cbs_code`) is aggregated away to the country total,
and country-years without a matching population row are dropped. The
chosen framing is stamped on every row.

## Usage

``` r
build_n_percapita(
  n_inputs,
  population,
  framing = c("synthetic_bnf"),
  params = NULL,
  example = FALSE
)
```

## Arguments

- n_inputs:

  A
  [`build_n_inputs()`](https://eduaguilera.github.io/whep/reference/build_n_inputs.md)
  long-format output with `fert_type`, `n_input_t` and the `year`,
  `area_code` keys (finer grid keys such as `lon`/`lat`/`item_cbs_code`
  are summed away).

- population:

  A tibble keyed by `year`, `area_code` with `population` (absolute
  persons).

- framing:

  How the total anthropogenic reactive nitrogen is defined.
  `"synthetic_bnf"` (default) scales the `"synthetic"` term by
  `syn_tot_agri_ratio` and adds the `"bnf"` term; other framings can be
  added.

- params:

  Boundary parameters, defaulting to
  [n_boundary_params](https://eduaguilera.github.io/whep/reference/n_boundary_params.md),
  used here for `syn_tot_agri_ratio`.

- example:

  If `TRUE`, return a small fixture instead of computing from
  `n_inputs`/`population`. Defaults to `FALSE`.

## Value

A tibble keyed by `year`, `area_code` with `n_percapita_kg`, the country
total anthropogenic reactive nitrogen per capita (kg N/cap/yr), and
`framing`, the anthropogenic definition it was computed under, plus the
polity columns below.

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
build_n_percapita(example = TRUE)
#> # A tibble: 2 × 8
#>    year area_code polity_area_code reporting_polity_code reporting_polity_name
#>   <int>     <int>            <int> <chr>                 <chr>                
#> 1  2000        10               10 AUS-1901-2025         Australia            
#> 2  2000        20               20 BWA-1966-2025         Botswana             
#> # ℹ 3 more variables: reporting_polity_has_geometry <lgl>,
#> #   n_percapita_kg <dbl>, framing <chr>
```
