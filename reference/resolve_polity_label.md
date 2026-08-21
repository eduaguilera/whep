# Resolve a source's country label to a polity

Maps a country or area **label**, as a source writes it, to a WHEP
polity code. This complements
[`add_polity_code()`](https://eduaguilera.github.io/whep/reference/add_polity_code.md),
which resolves numeric FAOSTAT/FABIO area codes: before this existed
there was no supported path from a label to a polity, so datasets
carrying labels went unresolved.
[mueller_synthetic_n](https://eduaguilera.github.io/whep/reference/mueller_synthetic_n.md)'s
`iso3c` column holds FAO-style legacy codes (`"BZE"` for Belize, `"ROM"`
for Romania, `"ZAR"` for Zaire) and
[lassaletta_grassland_share](https://eduaguilera.github.io/whep/reference/lassaletta_grassland_share.md)'s
`Country` holds name variants (`"Cape Verde"`, `"Swaziland"`), none of
which resolve against
[polities](https://eduaguilera.github.io/whep/reference/polities.md)
directly.

## Usage

``` r
resolve_polity_label(label, source = NULL, year = NULL)
```

## Arguments

- label:

  Character vector of source labels.

- source:

  Optional source slug (e.g. `"lassaletta-grassland-share"`). Length 1,
  or the same length as `label`. On the alias route `NULL` matches
  unscoped aliases only – 180 of 903 – so a `NULL` source narrows that
  route sharply; the identity routes then get their turn, subject to the
  guards above.

- year:

  Optional integer vector of years. Length 1, or the same length as
  `label`. On the alias route `NULL` matches aliases with no year scope
  only, which is the 15 of 903 published aliases carrying NEITHER bound.
  The name and ISO3 routes can still answer without a year, but only for
  an identifier exactly one polity has ever carried, so supplying a year
  remains much the stronger question: it is what lets a label resolve to
  the right *period* rather than to nothing.

## Value

A character vector of polity codes, `NA` where nothing matched.

## Details

The mapping is
[polity_label_aliases](https://eduaguilera.github.io/whep/reference/polity_label_aliases.md),
a copy of the map published by `whep-polities`. It is deliberately NOT
computed here: a label's meaning is a fact about the source, upstream
already decides it, and a second lookup in this package would be a
second authority for the same question.

Resolution is **source- and year-aware**, and both matter:

- An alias may be scoped to one `source`, because the same label can
  mean different things in different sources. A scoped alias never
  applies to another source; an unscoped one applies to any.

- An alias may be scoped to a year range, because a label's referent
  changes. `"Cape Verde"` in 1970 is the Portuguese colony
  `CPV-1886-1975`; in 1990 it is `CPV-1975-2025`.

Where several aliases match, the most specific wins: year-scoped over
unscoped, then source-scoped, then the narrower year range. That
ordering mirrors `matchlib.Matcher.match_alias` upstream, so both sides
agree.

Where no alias applies, a second route tries the polity's own
`polity_name` and then, for a three-letter label, its `iso3_code`. That
mirrors upstream's "alias, then ISO/name family + year containment", and
both halves are needed. Without the name half a caller passing the
database's own name for a polity got `NA`:
`resolve_polity_label("Netherlands")` found nothing while
[polities](https://eduaguilera.github.io/whep/reference/polities.md)
carried a polity named exactly that. Without the ISO3 half the map
answers only for labels a curator had to decide about, which is 380 of
[mueller_synthetic_n](https://eduaguilera.github.io/whep/reference/mueller_synthetic_n.md)'s
5,043 rows – the 11 legacy codes – against all 5,043 with it, asked at
`year = 2000`. Asking without a year resolves only 1,255, because the
guard below then refuses every identifier more than one live polity has
ever carried. Two guards bound both halves.

- An identifier resolves only when **exactly one** polity carries it in
  the year asked about, because otherwise row order would decide and
  `NA` is the honest answer. Sharing an identifier is common in the
  shipped
  [polities](https://eduaguilera.github.io/whep/reference/polities.md)
  snapshot: of its 726 live rows, 110 normalised names and 133 ISO3
  codes are carried by more than one polity. A year separates nearly all
  of them – no two live polities sharing a normalised name cover a
  common year – but not the ISO3 index, where 69 pairs still do, 62 of
  them naming different territories rather than successive periods of
  one. `"PAN"` in 1970 is the case that matters: `PAN-1903-1979` and the
  Canal Zone `CZN-1903-1979` both carry that ISO3 then – a real
  territorial overlap no re-sync removes – so the answer is `NA`, while
  `"PAN"` in 2000 resolves to `PAN-1979-2025`.

- An alias covering that year outranks both, whatever its source, and a
  label naming an area the crosswalk leaves unmapped is refused
  outright.

Returns `NA` when neither route resolves, which is a real answer rather
than a failure. Some labels are aggregates a source keeps reporting
after the territory stopped existing – `"FSU"` runs to 2009 though
nothing has held that territory since 1991 – and those years are
deliberately unmapped rather than routed to a polity that had ended.

Every resolved code is one
[`get_polity_geometries()`](https://eduaguilera.github.io/whep/reference/get_polity_geometries.md)
can return a row for, and that is an invariant rather than a happy
accident:
[polity_label_aliases](https://eduaguilera.github.io/whep/reference/polity_label_aliases.md)
and [polities](https://eduaguilera.github.io/whep/reference/polities.md)
are regenerated together from a single upstream revision, and
`data-raw/table_mappings.R` aborts the build if any alias names a polity
the shipped table does not carry. A dangling resolution therefore cannot
ship.

## See also

[`add_polity_code()`](https://eduaguilera.github.io/whep/reference/add_polity_code.md)
for numeric area codes.

## Examples

``` r
resolve_polity_label("ZAR", source = "mueller-synthetic-n", year = 2000)
#> [1] "COD-1960-2025"
resolve_polity_label(
  c("Cape Verde", "Cape Verde"),
  source = "lassaletta-grassland-share",
  year = c(1970L, 1990L)
)
#> [1] "CPV-1886-1975" "CPV-1975-2025"
```
