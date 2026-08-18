# Score a diet's protein against the age-weighted requirement pattern.

Computes the aggregate protein digestibility-corrected amino acid score
(PDCAAS) of a diet, per country and year, following the worked example
in WHO/FAO/UNU TRS 935 Table 6 (printed p.100). This is **tier 2** of
the protein-quality ladder and the standard the ladder targets;
[`build_protein_quality()`](https://eduaguilera.github.io/whep/reference/build_protein_quality.md)
is the tier 1b lower bound that ships when no amino acid composition is
available.

Given per-item protein supply `P_j`, digestibility `d_j` and amino acid
content `aa_ij` in mg per g protein:

\$\$P^d_j = P_j d_j \qquad D = \sum_j P^d_j / \sum_j P_j\$\$ \$\$A_i =
\sum_j P^d_j \\ aa\_{ij} / \sum_j P^d_j \qquad \mathrm{AAS} = \min_i
(A_i / \mathrm{ref}\_i)\$\$ \$\$q = \min(1, \mathrm{AAS}) \cdot D\$\$

The amino acid profile is weighted by **digestible** protein, not crude,
and the score is truncated **before** multiplying by digestibility,
which is the TRS 935 convention rather than FNP 92's. Both choices are
load-bearing and are explained at the top of the source file.

**Averaging per-item scores is not an approximation of this, it is a
different and biased quantity.** FAO forbids it twice in words, and
because [`min()`](https://rdrr.io/r/base/Extremes.html) is concave the
average of item scores is a rigorous lower bound on diet quality and so
a rigorous upper bound on the floor.

Four amino acids are enough for most diets — TRS 935 p.99: "in
calculating scores it is usually only necessary to use a pattern based
on these four amino acids" (lysine, sulfur amino acids, threonine,
tryptophan) — and are what both FAO worked examples use. Supplying more
is supported: every amino acid present in both `amino_acids` and
`pattern` is scored.

## Usage

``` r
build_protein_score(data = list())
```

## Arguments

- data:

  Named list of injected inputs. `protein_supply` (`year`, `area_code`,
  `item_cbs_code`, `protein_t`), `amino_acids` (`item_cbs_code`,
  `amino_acid`, `mg_per_g_protein`) and `digestibility`
  (`item_cbs_code`, `digestibility`) are required; `pattern`
  (`amino_acid`, `mg_per_g_protein`) overrides the packaged adult
  reference pattern.

## Value

A tibble keyed by `year`, `area_code` with `quality`,
`amino_acid_score`, `digestibility`, `limiting_amino_acid`,
`protein_scored_share` and `method_quality`, plus the polity columns
below.

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
# TRS 935 Table 6: wheat, chickpea and milk powder against the adult pattern.
build_protein_score(
  data = list(
    protein_supply = tibble::tribble(
      ~year, ~area_code, ~item_cbs_code, ~protein_t,
      2010L, 10L,        1L,             52.0,
      2010L, 10L,        2L,             22.0,
      2010L, 10L,        3L,             11.9
    ),
    digestibility = tibble::tribble(
      ~item_cbs_code, ~digestibility,
      1L,             0.85,
      2L,             0.80,
      3L,             0.95
    ),
    amino_acids = tibble::tribble(
      ~item_cbs_code, ~amino_acid, ~mg_per_g_protein,
      1L,             "lysine",    25,
      2L,             "lysine",    70,
      3L,             "lysine",    80
    ),
    pattern = tibble::tribble(
      ~amino_acid, ~mg_per_g_protein,
      "lysine",    45
    )
  )
)
#> # A tibble: 1 × 12
#>    year area_code polity_area_code reporting_polity_code reporting_polity_name
#>   <int>     <int>            <int> <chr>                 <chr>                
#> 1  2010        10               10 AUS-1901-2025         Australia            
#> # ℹ 7 more variables: reporting_polity_has_geometry <lgl>,
#> #   amino_acid_score <dbl>, limiting_amino_acid <chr>, digestibility <dbl>,
#> #   protein_scored_share <dbl>, quality <dbl>, method_quality <chr>
```
