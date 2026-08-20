# Run one-at-a-time sensitivity analysis on typology thresholds.

Varies each threshold by +/- `variation` (one at a time) and reports the
share of province-year observations that retain the same typology as the
baseline classification.

## Usage

``` r
run_typology_sensitivity(
  n_prov_destiny = NULL,
  variation = 0.2,
  baseline = NULL
)
```

## Arguments

- n_prov_destiny:

  Nitrogen flows tibble. If `NULL`, loaded automatically.

- variation:

  Relative variation applied to each threshold (default 0.2 = 20%).

- baseline:

  Pre-computed indicator table from `create_typologies_spain()`,
  carrying `year`, `province_name` and the indicator columns the
  thresholds act on. If `NULL`, computed automatically (slow).

## Value

A tibble with columns `threshold`, `direction`, and `agreement_pct`.

## Examples

``` r
# `baseline` carries the indicator columns the thresholds act on plus the
# unperturbed `Typology_base` each perturbation is compared against. Two
# provinces show the output shape; the real analysis runs over 50 provinces
# and 1860-2023.
baseline <- tibble::tribble(
  ~year,
  ~province_name,
  ~production_seminatural,
  ~production_crops,
  ~animal_ingestion,
  ~synthetic_share,
  ~crop_productivity,
  ~Livestock_density,
  ~imported_feed_share,
  ~feed_from_seminatural_share,
  ~local_feed_share,
  ~Manure_share,
  ~Typology_base,
  2000,
  "A",
  1,
  100,
  5,
  0.8,
  40,
  0.1,
  0.1,
  0.1,
  0.1,
  0.1,
  "Specialized cropping systems (intensive)",
  2000,
  "B",
  1,
  10,
  50,
  0.1,
  40,
  0.5,
  0.1,
  0.5,
  0.5,
  0.5,
  "Connected crop-livestock systems (intensive)"
)
sensitivity <- run_typology_sensitivity(baseline = baseline)
```
