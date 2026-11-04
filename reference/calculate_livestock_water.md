# Account for cohort-specific direct livestock water.

Multiply explicitly supplied cohort rates by animal-day exposure,
retaining species, cohort, production system and source identity. FAO
(2019), *Water use of livestock production systems and supply chains*,
sections 4.5.4–4.5.5, distinguishes water supplied, returned and
evaporated: ISBN 978-92-5-131713-6. Drinking intake or water
requirements are not automatically withdrawals or net consumptive blue
water. No demographic or consumption fraction is inferred. Supplied
provenance is an attestation, not verified evidence. Existing generated
output columns and conflicting join columns abort rather than silently
replacing prior quantities or provenance.

## Usage

``` r
calculate_livestock_water(exposure, rates, consumption = NULL)
```

## Arguments

- exposure:

  Tibble with `item_cbs_code`, `cohort`, `production_system`,
  `animal_days`, `rate_id`, `assessment_period`, `exposure_source`, and
  `exposure_basis`. Animal-days must cover the described assessment
  interval; a snapshot head count is insufficient. Optional `year`,
  canonical polity, and other context columns are retained unchanged,
  including unresolved values. No territorial aggregation or identity
  validation is performed. An annual extension adapter must separately
  require and validate year and canonical polity, and reject unresolved
  multi-year study records.

- rates:

  Tibble unique by `rate_id`, with matching `item_cbs_code`, `cohort`,
  `production_system`, rates `drinking_l_day`, `service_l_day`, and
  nonempty `rate_source`, `rate_basis`, `quantity_basis`. `rate_basis`
  is `"measured"`, `"modelled"`, or `"assumed"`; `quantity_basis` is
  `"withdrawal"`, `"requirement"`, or `"free_drinking_intake"`. The
  first two require both component rates on that same stated basis. Free
  intake requires `service_l_day = NA_real_`: service, total direct
  water and consumption remain unknown, never zero. This explicitly
  retains a drinking estimate without mixing it with a differently
  defined service quantity. Use
  [`estimate_dairy_drinking()`](https://eduaguilera.github.io/whep/reference/estimate_dairy_drinking.md)
  to derive an empirical rate; its examples show the composition path.

- consumption:

  Optional tibble unique by `rate_id`, with `drinking_fraction`,
  `service_fraction` between zero and one, and nonempty
  `consumption_source`, `consumption_basis`. Only explicit withdrawal
  rates can be converted. These fractions must match the stated system
  boundary; they are not universally applicable livestock constants.

## Value

Exposure rows with rates, provenance, `drinking_m3`, `service_m3`,
`direct_water_m3`, and `consumption_m3` (unknown when not supplied).
`coverage_direct_water` is `"service_unknown"` for intake-only rows and
`"complete"` for rows with both rates. This describes supplied component
coverage, not completeness of an external herd inventory.

## Examples

``` r
x <- tibble::tibble(
  year = 2000L, reporting_polity_code = "synthetic-example",
  item_cbs_code = 961L, cohort = "adult", production_system = "example",
  animal_days = 10, rate_id = "synthetic",
  assessment_period = "synthetic annual interval",
  exposure_source = "synthetic example", exposure_basis = "assumed"
)
rates <- tibble::tibble(
  rate_id = "synthetic", item_cbs_code = 961L, cohort = "adult",
  production_system = "example", drinking_l_day = 30, service_l_day = 8,
  rate_source = "synthetic example", rate_basis = "assumed",
  quantity_basis = "requirement"
)
calculate_livestock_water(x, rates)
#> # A tibble: 1 × 20
#>    year reporting_polity_code item_cbs_code cohort production_system animal_days
#>   <int> <chr>                         <int> <chr>  <chr>                   <dbl>
#> 1  2000 synthetic-example               961 adult  example                    10
#> # ℹ 14 more variables: rate_id <chr>, assessment_period <chr>,
#> #   exposure_source <chr>, exposure_basis <chr>, drinking_l_day <dbl>,
#> #   service_l_day <dbl>, rate_source <chr>, rate_basis <chr>,
#> #   quantity_basis <chr>, drinking_m3 <dbl>, service_m3 <dbl>,
#> #   direct_water_m3 <dbl>, coverage_direct_water <chr>, consumption_m3 <dbl>
```
