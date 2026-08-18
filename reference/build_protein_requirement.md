# Build the population protein requirement.

Weights the WHO/FAO/UNU TRS 935 per-class protein requirements by a
population's age and sex structure, giving the mean requirement of an
average member of that population in grams of protein per day.

The default `requirement = "average"` uses the class AVERAGE (median)
requirement, which is the anchor TRS 935 names for population use. The
alternative `"safe"` uses the class safe level, the 97.5th percentile of
the individual requirement distribution; it is offered for continuity
with analyses built on the safe level, but TRS 935 calls that
application to populations incorrect, and it double-counts the
requirement margin whenever a dispersion allowance is also applied
downstream.

Population rows are supplied as age groups. Each group is expanded to
the single years of age it spans, which are assumed uniformly
distributed within the group; on UN WPP 2024 data the difference between
five-year groups and single-year data is at most 0.105 g/cap/day (0.3%).

It also returns the population's **amino acid scoring pattern**,
weighted from TRS 935 Table 50 by the same age structure. Requirement
and protein quality are both age-dependent, so a downstream quality
score must be taken against the pattern this population actually
requires, not against an adult pattern; scoring separably costs roughly
1.5% in the youngest populations and 0.4% in the oldest, always in the
same direction. Note the two outputs are weighted by **different**
quantities: the requirement by headcount, the pattern by headcount times
protein requirement, because a pattern is a composition per gram of
protein rather than an amount.

The age-weighted pattern is WHEP's own construction. It follows from the
anchor — TRS 935's requirement is defined against a PDCAAS of 1.0 on its
own pattern — but no published study scores a national diet against a
demographically weighted pattern, so it should be reported as a WHEP
method, not as standard practice.

## Usage

``` r
build_protein_requirement(data = list(), requirement = c("average", "safe"))
```

## Arguments

- data:

  Named list of injected inputs. `population_age` is required: `year`,
  `area_code`, `age_start`, `age_span`, `sex` (`"m"` / `"f"`) and
  `population`. `protein_requirement` and `protein_scoring_pattern`
  override the packaged coefficient tables.

- requirement:

  Which TRS 935 column to weight: `"average"` (default, the class
  average requirement) or `"safe"` (the class safe level).

## Value

A tibble keyed by `year`, `area_code` with `requirement_g_cap_day`,
`population`, `method_requirement`, the scoring pattern columns
`lysine_mg_g`, `saa_mg_g`, `threonine_mg_g` and `tryptophan_mg_g`, plus
the polity columns below.

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
build_protein_requirement(
  data = list(
    population_age = tibble::tribble(
      ~year, ~area_code, ~age_start, ~age_span, ~sex, ~population,
      2010L, 10L,        0L,         5L,        "m",  1000,
      2010L, 10L,        0L,         5L,        "f",  1000,
      2010L, 10L,        20L,        5L,        "m",  3000,
      2010L, 10L,        20L,        5L,        "f",  3000
    )
  )
)
#> # A tibble: 1 × 13
#>    year area_code polity_area_code reporting_polity_code reporting_polity_name
#>   <int>     <int>            <int> <chr>                 <chr>                
#> 1  2010        10               10 AUS-1901-2025         Australia            
#> # ℹ 8 more variables: reporting_polity_has_geometry <lgl>,
#> #   requirement_g_cap_day <dbl>, lysine_mg_g <dbl>, saa_mg_g <dbl>,
#> #   threonine_mg_g <dbl>, tryptophan_mg_g <dbl>, population <dbl>,
#> #   method_requirement <chr>
```
