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
resolve_polity_label(
  label,
  source = NULL,
  year = NULL,
  item = NULL,
  country = NULL,
  back_cast = TRUE,
  unit = NULL,
  indicator = NULL
)
```

## Arguments

- label:

  Character vector of source labels.

- source:

  Optional source slug (e.g. `"lassaletta-grassland-share"`). Length 1,
  or the same length as `label`. On the alias route `NULL` matches
  unscoped aliases only – 188 of 1,003 – so a `NULL` source narrows that
  route sharply; the identity routes then get their turn, subject to the
  guards above.

- year:

  Optional integer vector of years. Length 1, or the same length as
  `label`. On the alias route `NULL` matches aliases with no year scope
  only, which is the 14 of 1,003 published aliases carrying NEITHER
  bound. The name and ISO3 routes can still answer without a year, but
  only for an identifier exactly one polity has ever carried, so
  supplying a year remains much the stronger question: it is what lets a
  label resolve to the right *period* rather than to nothing.

- item:

  Optional item names as the source writes them. Length 1, or the same
  length as `label`. Only used to apply
  [polity_label_item_corrections](https://eduaguilera.github.io/whep/reference/polity_label_item_corrections.md),
  which needs `source` and `year` too.

- country:

  Optional ISO3 code of the country each label belongs to. Length 1, or
  the same length as `label`. Restricts the name and ISO3 routes to that
  country's polities.

- back_cast:

  Logical. `TRUE` (the default) keeps aliases upstream marks as
  reconstructions (`disposition == "back_cast"`); `FALSE` drops them.

- unit, indicator:

  Optional unit and indicator of each row, as the source writes them
  (e.g. `"tonnes"`, `"ha"`). Length 1, or the same length as `label`.
  Used to match
  [polity_label_item_corrections](https://eduaguilera.github.io/whep/reference/polity_label_item_corrections.md)
  rules scoped on them, and `indicator` also to choose between
  [polity_label_aliases](https://eduaguilera.github.io/whep/reference/polity_label_aliases.md)
  rules scoped on it; required for the rows such a rule would otherwise
  match.

## Value

A character vector of polity codes, `NA` where nothing matched. On the
identity routes a subnational polity contained in another candidate (per
[polity_containment](https://eduaguilera.github.io/whep/reference/polity_containment.md))
never competes with its container, so an ISO3 shared by a state and its
provinces resolves to the state.

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

Three more inputs follow contracts `whep-polities` publishes beside the
alias map:

- **`item`: rows a source files under another territory's label for one
  item.** An alias has no item dimension, so it cannot say that
  Mitchell's pre-1910 `"south africa"` sugar cane is Natal's while every
  other `"south africa"` item of those years is the Cape's.
  [polity_label_item_corrections](https://eduaguilera.github.io/whep/reference/polity_label_item_corrections.md)
  states those cases. When `source`, the label, `item` and a year inside
  the rule's inclusive range all match, the label is replaced by the
  rule's `correct_label` BEFORE any route below runs, so the usual alias
  and year rules then place it. Rules do not chain: each is tested
  against the label the caller passed. A row without a year is never
  corrected. A corrected row ignores `country`, which named the
  territory it was misfiled under, and its new label is not read as an
  ISO3 code, so only the corrected label decides. A rule whose
  `polity_code` is `"UNROUTED"` marks rows that belong to no polity (a
  wrong territory with no right one to land on), and those resolve to
  `NA`. A rule may also be scoped on `unit` or `indicator`, where only
  that separates the rows: Mitchell's 1955-1960 `"viet nam"` rice output
  in tonnes is North plus South Vietnam, its area in hectares South
  only. A scoped rule applies only when the caller's `unit` /
  `indicator` equals it; a row it would otherwise match but whose scoped
  value is missing is an error of class
  `whep_error_unscoped_label_item_correction`, not a silent miss.

- **`country`: the reporting country, as an ISO3 code.** The name route
  compares normalised names, and normalisation drops parenthesised
  qualifiers, so a bare subnational name meets another country's unit:
  `"Santa Cruz (department of Bolivia)"` normalises to `"santa cruz"`,
  and so does Argentina's province. Given `country`, the name and ISO3
  routes only consider polities whose `iso3_code` (its code prefix where
  that is missing) is that country. Without it, a name that live
  polities of two or more countries carry at the same time – counting a
  subnational polity's parenthesised qualifier as a name, so Mexico's
  `"Ciudad de México (Distrito Federal)"` shares `"distrito federal"`
  with Brazil's – is refused with a warning of class
  `whep_warn_ambiguous_polity_name` rather than guessed. The refusal
  holds in every year, not only in years two such polities overlap,
  because a source can report a unit before its own polity begins: the
  subnational panel reports Argentina's Santa Cruz from 1900, and with
  no Argentine polity until 1955 the year alone sent it to Bolivia. The
  alias route is not restricted: an alias names its polity explicitly.

- **`back_cast`: whether to accept reconstructions.** An alias whose
  `disposition` is `"back_cast"` routes years a source reconstructs onto
  a boundary that did not exist yet to the modern polity, which may
  begin after those years by design (`BRA-TO-1988-2025` receives the
  panel's 1900-1987 Tocantins series). `back_cast = FALSE` drops those
  aliases, for a caller that wants observation only.

- **`indicator`: aliases split per indicator.** One panel unit id can
  name two territories: whep-polities \#703 found `CHL-LL`'s crops
  reported for Los Lagos plus Los Ríos in every year, while its landuse
  and livestock are Los Lagos alone. The alias map's optional
  `indicator` column scopes a rule to rows carrying that indicator (`NA`
  means any). Where a label, source and year carry scoped rules, only
  the rule for the row's `indicator` applies; an indicator the split
  leaves out resolves to `NA`, never to the name route, which would put
  the rows back on the post-split polity; and a row that gives no
  `indicator` is an error of class
  `whep_error_unscoped_indicator_alias`. Upstream allows the scope only
  on the subnational panel's slugs (`"juan-subnational"`,
  `"whep-lab-*"`), so callers resolving that panel must pass its
  `indicator` column verbatim.

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
