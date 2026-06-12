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
  matched polity.

- `mapping_status`: `"matched"`, `"manual"`, `"unmapped"`, or
  `"not_a_reporting_area"`.

- `mapping_note`: Explanation for manual or unmapped rows.

## Source

Derived from
[polities](https://eduaguilera.github.io/whep/reference/polities.md) and
`inst/extdata/harmonization/regions_full.csv`.
