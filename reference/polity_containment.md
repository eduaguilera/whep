# Polity containment edges

Published member-container containment edges between periodized WHEP
[polities](https://eduaguilera.github.io/whep/reference/polities.md):
which admin/subnational polity sits inside which container polity, and
for what years. This is the piece
[polity_area_crosswalk](https://eduaguilera.github.io/whep/reference/polity_area_crosswalk.md)
and
[polity_label_aliases](https://eduaguilera.github.io/whep/reference/polity_label_aliases.md)
do not carry – both map a *reporting area* or a *label* to a polity,
while this table relates polities to each other. Any WHEP admin unit (a
province, a prefecture, ...) is already a `polity_type == "subnational"`
row of
[polities](https://eduaguilera.github.io/whep/reference/polities.md);
the containment edge is the missing link between it and the polity it
belongs to (whep-polities#649, closing whep#51).

## Usage

``` r
polity_containment
```

## Format

A tibble with one row per containment edge. Columns:

- `member_code`: `polity_code` of the contained polity.

- `container_code`: `polity_code` of the containing polity.

- `start_year`, `end_year`: Validity interval of the edge itself, on the
  same convention as
  [polities](https://eduaguilera.github.io/whep/reference/polities.md):
  `start_year` is inclusive and `end_year` is exclusive at a succession
  (a later edge for the same member takes over from `end_year`) and
  inclusive at the open end (an edge nothing succeeds still covers its
  own terminal year). The interval belongs to the edge, not to either
  endpoint polity: `JPN-23-1871-2025` is one member polity spanning
  1871-2025, but four separate edges route it through `JPN-1800-1895`,
  `JPN-1895-1945`, `JPN-1945-1952` and `JPN-1952-2025` as Japan's own
  periodization changes underneath it.

- `basis`: Free-text description of what the containment relationship
  represents (a historical territory, a reporting convention, ...).

## Source

`~/whep-polities/data/final/polity_containment.csv`.

## Details

The map is authored and gated in `whep-polities`; this package embeds a
copy rather than deciding containment itself, on the same grounds as
[polity_label_aliases](https://eduaguilera.github.io/whep/reference/polity_label_aliases.md)
and
[polity_area_crosswalk](https://eduaguilera.github.io/whep/reference/polity_area_crosswalk.md).
