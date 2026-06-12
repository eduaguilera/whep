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

- `start_year`, `end_year`: Inclusive validity years for the row.

- `iso3_code`, `iso3c`: ISO3 code where one exists. `iso3c` is retained
  as a compatibility alias.

- `polygon_status`: Polygon status in `whep-polities` (`"assigned"`,
  `"proxy"`, `"missing"`, or `"excluded"`).

- `has_geometry`: Logical flag indicating whether the geometry is
  non-empty.

- `geom`: Multipolygon geometry.

## Source

`~/whep-polities/data/final/polities_database.gpkg`.
