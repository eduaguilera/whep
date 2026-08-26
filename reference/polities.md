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

- `start_year`, `end_year`: Validity interval for the row. `start_year`
  is inclusive; `end_year` is **exclusive at a succession** and
  **inclusive at the open end**. Exclusive at a succession, so
  `F51-1947-1993` Czechoslovakia covers 1947-1992 and 1993 belongs to
  `CZE-1993-2025` and `SVK-1993-2025`, and 2014 belongs to
  `"RUS-2014-2025"` rather than to `"RUS-1991-2014"` (filtering
  `year <= end_year` returns both epochs and double-counts every
  boundary year). Inclusive at the open end, so 2025 still belongs to
  `"RUS-2014-2025"`, because no later interval of that polity follows it
  and a uniformly exclusive read would leave the current year with no
  polity at all. **Openness is absence of a successor, not a comparison
  against the last year in the table.** 258 live polities have no
  successor, 246 of them end in 2025, and no live polity ending in 2025
  carries one, so the two readings agree on this vintage; the successor
  test is the one that keeps agreeing when the horizon moves. The
  distinction is not decoration: 256 of the 781 rows end in 2025 but
  only 253 are open, because three are succeeded there (`BLZ-1800-2025`,
  `CAN-1948-2025` and `IRQ-1921-2025`, all `retired` or `superseded`),
  and opening those too would count the terminal year twice.

- `iso3_code`, `iso3c`: ISO3 code where one exists. `iso3c` is retained
  as a compatibility alias.

- `wiki_status`: Upstream review state. `"retired"` and `"superseded"`
  mark a DEAD row, kept so a code already held in older output stays
  resolvable, but never a resolution target:
  [polity_area_crosswalk](https://eduaguilera.github.io/whep/reference/polity_area_crosswalk.md)
  excludes them, so resolve through it rather than through this table.

- `polygon_status`: Polygon status in `whep-polities` (`"assigned"`,
  `"proxy"`, `"missing"`, or `"excluded"`).

- `polygon_feature_year`, `polygon_feature_date`: The vintage of the
  source feature the polygon was taken from. `polygon_feature_date`
  arrived with the \#835 upstream re-sync and is populated on 3 rows,
  where upstream recorded a day rather than only a year.

- `has_geometry`: Logical flag indicating whether the geometry is
  non-empty.

- `geom`: Multipolygon geometry.

## Source

`~/whep-polities/data/final/polities_database.gpkg`.
