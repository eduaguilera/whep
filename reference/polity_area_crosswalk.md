# FAOSTAT/FABIO area-to-polity crosswalk

Year-aware bridge from numeric reporting `area_code` values used by
FAOSTAT/FABIO-derived WHEP data to periodized WHEP `polity_code` values.

## Usage

``` r
polity_area_crosswalk
```

## Format

A tibble with one row per area-code/polity-period mapping. Key columns:

- `area_code`: Numeric FAOSTAT/FABIO reporting area code.

- `area_name`: Reporting area name.

- `area_iso3c`: Reporting-area ISO3-like code where available.

- `polity_area_code`: Numeric area code retained for WHEP matrix
  workflows.

- `polity_code`, `polity_name`: Matched WHEP polity, or `NA` for
  statistical composites that are not real polities.

- `polity_start_year`, `polity_end_year`: Validity interval for the
  matched polity. `polity_end_year` is exclusive, so the period covers
  `polity_start_year:(polity_end_year - 1)`.
  [`add_polity_code()`](https://eduaguilera.github.io/whep/reference/add_polity_code.md)
  resolves a year on that reading, widened to the inclusive
  `map_year_end` below where the upstream map declares a reported year
  past the territorial span.

- `mapping_source`: How the area-to-polity decision was reached.
  `"upstream_map"` for the published `whep-polities` FAOSTAT area map,
  which is the authority for the years FAOSTAT reports;
  `"prefix_outside_map"` for a period of a mapped area lying outside
  every span the map declares, kept so sources reported under their own
  historical borders still resolve; `"fabio_row_fold"` where FABIO
  collapses the area into its Rest-of-World bucket; `"prefix_fallback"`
  where the map covers the area not at all and the mapping is inferred
  from the polity-code prefix.

- `map_year_start`, `map_year_end`: Inclusive reporting years the
  upstream map assigns to this area-polity pair, `NA` unless
  `mapping_source` is `"upstream_map"`.

- `map_match_route`: Upstream's record of how it decided the row
  (`"iso-equal"`, `"registry"`, `"manual-route"`, `"manual-replace"`,
  `"manual-span"`), `NA` unless `mapping_source` is `"upstream_map"`.

- `mapping_status`: `"matched"`, `"manual"`, `"unmapped"`, or
  `"not_a_reporting_area"`.

- `mapping_note`: Explanation for manual or unmapped rows.

## Source

Derived from
[polities](https://eduaguilera.github.io/whep/reference/polities.md),
`~/whep-polities/data/final/faostat_area_polity_map.csv` and
`inst/extdata/harmonization/regions_full.csv`.
