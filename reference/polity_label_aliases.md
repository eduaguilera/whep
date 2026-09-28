# Source label to polity aliases

Published map from the country/area **labels** a source writes to
periodized WHEP `polity_code` values, consumed by
[`resolve_polity_label()`](https://eduaguilera.github.io/whep/reference/resolve_polity_label.md).
It is the label-keyed counterpart of
[polity_area_crosswalk](https://eduaguilera.github.io/whep/reference/polity_area_crosswalk.md),
which is keyed by numeric reporting area code.

## Usage

``` r
polity_label_aliases
```

## Format

A tibble with one row per alias. Columns:

- `source_label`: The label exactly as the source writes it.

- `source`: Source slug the alias is scoped to, or `NA` when it applies
  to any source.

- `year_start`, `year_end`: Year range the alias is scoped to. A missing
  bound is unbounded on that side; both missing means the alias is not
  year-scoped.

- `polity_code`: The WHEP polity the label resolves to.

- `common_name`: Human-readable name of that polity.

- `confidence`: Curator's confidence in the alias.

- `observed_rows`: Source rows actually observed for the label, `NA`
  when the label is merely mappable.

- `disposition`: `NA` when the source observed the territory,
  `"back_cast"` when the years are a reconstruction onto a boundary that
  did not exist yet. A `"back_cast"` alias may begin before its target
  polity does, by design: the polity's own span still starts when the
  territory did.
  [`resolve_polity_label()`](https://eduaguilera.github.io/whep/reference/resolve_polity_label.md)
  drops these rules when asked `back_cast = FALSE`. All `NA` in a
  snapshot taken before whep-polities introduced the column.

- `indicator`: `NA` when the alias applies to every indicator, otherwise
  the one indicator it routes (`"area"`, `"production"`, `"yield"`,
  `"livestock_stock"` or `"landuse"`, the subnational panel's own
  values). whep-polities \#703 added it so that a panel unit whose id
  names different territories for different indicators can be split; it
  allows the scope only on the panel's slugs.
  [`resolve_polity_label()`](https://eduaguilera.github.io/whep/reference/resolve_polity_label.md)
  applies a scoped rule only to rows passing that `indicator`. Absent
  from a snapshot taken before \#703, which is read as all `NA`.

## Source

`~/whep-polities/data/final/label_alias_map.csv`.

## Details

The map is authored and gated in `whep-polities`; this package embeds a
copy rather than deciding label identity itself, so that a label's
meaning has one authority.
