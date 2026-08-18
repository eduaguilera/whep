# Build the SJOS-N nourishment band.

Returns the floor and ceiling on **mean per-capita protein supply** that
bound the adequate band, the prevalence of inadequacy and of excess, and
the number of people on each side. It replaces a flat 46 x 1.35 floor
and a flat 63 x 1.35 ceiling, of which only the 46 was ever sourced
(whep#753).

Both bounds are one inversion of the WHO/FAO/UNU TRS 935 Box 1
lognormal:

\$\$S_D = \sqrt{S_I^2 + S_R^2}\$\$ \$\$\mathrm{bound} = \mathrm{anchor}
\cdot \exp(z S_D + S_I^2/2) / (1 - \omega)\$\$

The **floor** anchors on the demographically weighted *average*
requirement with \\z = \Phi^{-1}(1 - \mathrm{shortfall})\\: the supply
below which more than `shortfall` of the population falls short. The
**ceiling** anchors on `multiple` times the demographically weighted
*safe level* with \\z = \Phi^{-1}(\mathrm{share})\\: the supply above
which more than `share` of the population exceeds that limit.

`exp(z S_D)` carries a median anchor to the required median intake, and
`exp(S_I^2/2)` carries a median to a **mean**, which is what makes
either bound comparable with a per-capita supply figure. Omitting the
second is not conservative — it sets the required mean equal to the
required median, about 5% too low at a typical dispersion.

**The two tails do not take the same tolerance, and that is the model's
finding rather than a preference.** `shortfall` defaults to 2.5%, fixed
independently by TRS 935 Figure 7 ("Safe population intake ie.
risk\<2.5%") and by FAO's stated lowest feasible PoU target. Applying
2.5% to the upper tail as well puts the ceiling *below* the floor for
162 of 167 country-years on the 2010 build, because TRS 935 calls
intakes below requirement harmful while calling twice the safe level
"unlikely to be associated with any risk" (section 14.2).

**`share` is WHEP's own criterion, not a sourced value, and it is the
only number in the band that is.** It defaults to 0.5, which reads
"Over" as *the typical member of this population exceeds the limit* — a
definition of over-nourishment, not a measurement of harm. Where
`shortfall` is fixed independently by TRS 935 Figure 7 and by FAO's PoU
target, nothing external fixes `share`: TRS 935 declines to set a
tolerable upper intake at all. It is exposed precisely so it can be
varied, the chosen value is stamped in `method_ceiling`, and any
published use should carry a sensitivity across it. The `people_over`
column exists partly for this reason: reported as a continuous
headcount, the result does not rest on where the class boundary was
drawn.

`multiple` defaults to 2, which TRS 935 section 13.7 names as "twice the
recommended intake, previously identified as a safe upper limit ...
likely to be safe". The report's own alternative is 3–4x, which it says
"approach the tolerable upper limit and cannot be assumed to be
risk-free"; both stay selectable and the choice is stamped in
`method_ceiling`.

**Protein quality divides BOTH bounds**, which is algebraically the
diet-side correction TRS 935 section 14.1.5 prefers: comparing
`supply x q` against a crude bound and comparing crude supply against
`bound / q` are the same inequality, and dividing both keeps the
published supply series untouched while moving the floor and the ceiling
together. Correcting only the floor would leave the ceiling on crude
protein and put the two bounds on different bases. Supply `data$quality`
from
[`build_protein_quality()`](https://eduaguilera.github.io/whep/reference/build_protein_quality.md);
without it the band stays on crude protein and `method_quality` says
`"none"`, which is a known understatement of 11-36%.

Supplying `data$supply` adds the prevalences and, where a population is
available, the headcounts. A country is not uniformly under or over:
with a supply inside the band both tails are populated, and
`people_under` / `people_over` are the quantities that say by how much.

## Usage

``` r
build_nourishment_band(
  data = list(),
  shortfall = 0.025,
  ceiling = list(multiple = 2, share = 0.5),
  requirement_sd = 0.12
)
```

## Arguments

- data:

  Named list of injected inputs. `requirement` (`year`, `area_code`,
  `requirement_g_cap_day`, optionally `population`), `requirement_safe`
  (the same from
  [`build_protein_requirement()`](https://eduaguilera.github.io/whep/reference/build_protein_requirement.md)
  with `requirement = "safe"`), `dispersion` (`year`, `area_code`,
  `sigma`) and `loss_wedge` (`year`, `area_code`, `omega`) are required;
  optional `supply` (`year`, `area_code`, `protein_g_cap_day`,
  optionally `population`) adds the prevalence and headcount columns,
  and optional `quality` (`year`, `area_code`, `quality`) from
  [`build_protein_quality()`](https://eduaguilera.github.io/whep/reference/build_protein_quality.md)
  divides both bounds.

- shortfall:

  Tolerated prevalence of inadequacy, in `(0, 1)`. Defaults to `0.025`.

- ceiling:

  Named list with `multiple` (of the safe level, positive; defaults to
  `2`) and `share` (tolerated prevalence of excess, in `(0, 1)`;
  defaults to `0.5`).

- requirement_sd:

  Log-scale SD of the requirement, `S_R`. Defaults to `0.12`, TRS 935
  p.38 for adults on a per-kilogram basis. The report notes this
  captures only about a fifth of observed between-individual variance,
  so it is exposed for sensitivity rather than fixed.

## Value

A tibble keyed by `year`, `area_code` with `floor_g_cap_day`,
`ceiling_g_cap_day`, `requirement_g_cap_day`, `safe_g_cap_day`,
`sigma_intake`, `sigma_deficit`, `omega`, `quality`, `method_quality`,
`method_shortfall` and `method_ceiling`; plus
`prevalence_protein_deficit`, `prevalence_protein_excess`,
`people_under` and `people_over` when `data$supply` is given; plus the
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
build_nourishment_band(
  data = list(
    requirement = tibble::tribble(
      ~year, ~area_code, ~requirement_g_cap_day,
      2010L, 10L,        32
    ),
    requirement_safe = tibble::tribble(
      ~year, ~area_code, ~requirement_g_cap_day,
      2010L, 10L,        40
    ),
    dispersion = tibble::tribble(
      ~year, ~area_code, ~sigma,
      2010L, 10L,        0.26
    ),
    loss_wedge = tibble::tribble(
      ~year, ~area_code, ~omega,
      2010L, 10L,        0.025
    )
  )
)
#> # A tibble: 1 × 17
#>    year area_code polity_area_code reporting_polity_code reporting_polity_name
#>   <int>     <int>            <int> <chr>                 <chr>                
#> 1  2010        10               10 AUS-1901-2025         Australia            
#> # ℹ 12 more variables: reporting_polity_has_geometry <lgl>,
#> #   requirement_g_cap_day <dbl>, safe_g_cap_day <dbl>, sigma_intake <dbl>,
#> #   sigma_deficit <dbl>, omega <dbl>, floor_g_cap_day <dbl>,
#> #   ceiling_g_cap_day <dbl>, quality <dbl>, method_quality <chr>,
#> #   method_shortfall <dbl>, method_ceiling <chr>
```
