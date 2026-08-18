# Build the protein-quality correction for the nourishment band.

Returns the diet's protein quality per country and year, the factor the
SJOS-N band divides by. WHO/FAO/UNU TRS 935 issues its safe level of
protein intake "for proteins with a protein digestibility-corrected
amino acid score value of 1.0" (section 14.2), and no real diet reaches
that, so an uncorrected band is too low for every country.

`method = "trs935_item"` (default) is **tier 1a**: it uses the measured
true digestibility TRS 935 Table 5 publishes for each commodity, and
falls back to the tier 1b class rate for the items the report does not
measure. Table 5 has 35 rows and prints **no fruit, vegetable, root,
tuber or sugar** entry at all, so the fallback is not a corner case — on
the 2010 world basket the measured share is **84.5%** of food protein
and the rest takes the class rate. `protein_measured_share` reports it
per row.

`"digestibility_share"` is **tier 1b**: the protein-weighted mean of
**0.95 for animal protein and 0.80 for plant protein**, which is how TRS
935 Table 43 footnote b computes it. The animal/plant split follows
FAO's own Food Balance Sheet grouping — Animal Products (item 2941)
against Vegetal Products (2903) — so it reconciles against FAOSTAT's
published aggregates rather than being WHEP's opinion. `"none"` returns
a quality of 1 and leaves the band on crude protein.

**Both are a provable lower bound on the full correction**, because
PDCAAS is `min(1, AAS) x D`, which never exceeds `D`. They are
conservative about the **size of the correction**, not about nourishment
adequacy — they under-correct, and so classify fewer countries as
deficient than the full amino acid score would.
[`build_protein_score()`](https://eduaguilera.github.io/whep/reference/build_protein_score.md)
is that full score, tier 2; it is code-complete and validated but needs
a composition table WHEP does not have, and it arrives as a new method
rather than silently changing this one.

`variant` brackets the one judgement tier 1a makes. Table 5 prints
several forms of the same commodity and CBS cannot say which was eaten:
wheat whole 0.86, cereal 0.77, flour white 0.96; maize 0.85, corn whole
0.87, corn cereal 0.70; rice polished 0.88, cereal 0.75. **The
processing direction is not uniform** — refining raises wheat by
removing bran and lowers maize, rice and oats through extrusion and
Maillard damage — so there is no single axis to sweep and the bracket is
carried per item. `"default"` takes the least-processed form, which is
the consistent partner for WHEP's own whole-commodity agronomic
nitrogen; `"low"` and `"high"` give the span.

## Usage

``` r
build_protein_quality(
  data = list(),
  method = c("trs935_item", "digestibility_share", "none"),
  variant = c("default", "low", "high")
)
```

## Arguments

- data:

  Named list of injected inputs. `protein_supply` (`year`, `area_code`,
  `item_cbs_code`, `protein_t`) is required; `protein_digestibility`,
  `protein_digestibility_items` and `protein_digestibility_trs935`
  override the packaged tables.

- method:

  `"trs935_item"` (default), `"digestibility_share"` or `"none"`.

- variant:

  Which Table 5 row each item takes, for `"trs935_item"`: `"default"`
  (the least-processed form the report names for the commodity), or
  `"low"` / `"high"`, the plausible bracket. Ignored by the other
  methods.

## Value

A tibble keyed by `year`, `area_code` with `quality`,
`animal_protein_share`, `protein_classified_share`,
`protein_measured_share` (the share carrying a measured Table 5 value
rather than the class rate) and `method_quality`, plus the polity
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
build_protein_quality(
  data = list(
    protein_supply = tibble::tribble(
      ~year, ~area_code, ~item_cbs_code, ~protein_t,
      2010L, 10L,        2731L,          40,
      2010L, 10L,        2511L,          60
    )
  )
)
#> # A tibble: 1 × 11
#>    year area_code polity_area_code reporting_polity_code reporting_polity_name
#>   <int>     <int>            <int> <chr>                 <chr>                
#> 1  2010        10               10 AUS-1901-2025         Australia            
#> # ℹ 6 more variables: reporting_polity_has_geometry <lgl>,
#> #   animal_protein_share <dbl>, protein_classified_share <dbl>,
#> #   protein_measured_share <dbl>, quality <dbl>, method_quality <chr>
```
