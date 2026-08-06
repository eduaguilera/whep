# Polities

Periodized WHEP polity database imported from the `whep-polities`
repository.

## Usage

``` r
polities
```

## Format

An sf data frame where each row corresponds to one territorial polity
over a continuous time interval. Key columns include:

- `polity_code`: Stable WHEP polity identifier, usually
  `PREFIX-start_year-end_year`.

- `polity_name`: Human-readable polity name.

- `start_year`, `end_year`: Half-open validity interval for the row:
  `start_year` is inclusive, `end_year` is **exclusive**, so the row
  covers `start_year:(end_year - 1)` and hands over to its successor in
  `end_year` (`F51-1947-1993` Czechoslovakia covers 1947-1992, and 1993
  belongs to `CZE-1993-2025` and `SVK-1993-2025`). Open periods carry
  the vintage's horizon as `end_year`, so they too stop one year short
  of it.

- `iso3_code`, `iso3c`: ISO3 code where one exists. `iso3c` is retained
  as a compatibility alias.

- `wiki_status`: Upstream review state. `"retired"` and `"superseded"`
  mark a DEAD row, kept so a code already held in older output stays
  resolvable, but never a resolution target:
  [polity_area_crosswalk](https://eduaguilera.github.io/whep/reference/polity_area_crosswalk.md)
  excludes them, so resolve through it rather than through this table.

- `polygon_status`: Polygon status in `whep-polities` (`"assigned"`,
  `"proxy"`, `"missing"`, or `"excluded"`).

- `has_geometry`: Logical flag indicating whether the geometry is
  non-empty.

- `geom`: Multipolygon geometry.

## Source

`~/whep-polities/data/final/polities_database.gpkg`.
