# Estimate free drinking intake of lactating dairy cows.

Apply the empirical lactating-cow model of Cardot, Le Roux and Jurjanz
(2008), doi:10.3168/jds.2007-0204. `"cardot_table5"` uses the body
equation and Table 5 coefficients (DMI 1.54; minimum temperature 0.58).
`"cardot_abstract"` explicitly selects the differing abstract
coefficients (1.53 and 0.57). Other coefficients are identical. The fit
used 1,837 cow-days from 41 mid-lactation Holstein cows during three
winters in a free-stall, corn-silage/concentrate TMR setting (summer
grazing excluded). R-squared was 0.446. Checking marginal predictor
ranges does not establish validity for other breeds, diets, climates or
joint conditions.

`"legacy_adult"` selects the 55 L/day mixed-system dairy drinking demand
in Chapagain and Hoekstra (2003), *Value of Water Research Report 13*,
Table 3.8 (<https://waterfootprint.org/resources/Report13.pdf>): mean of
industrial 70 and grazing 40 L/day. Its adult lactation assumption is a
sensitivity, not an observed population coefficient. Neither method
includes service water, leakage or net consumption. Existing generated
rate, quantity and method columns are rejected rather than overwritten;
supply the original predictor/exposure columns.

## Usage

``` r
estimate_dairy_drinking(
  data,
  method = c("cardot_table5", "cardot_abstract", "legacy_adult"),
  extrapolation = c("abort", "allow")
)
```

## Arguments

- data:

  Tibble with `animal_days`, `item_cbs_code` (960), `cohort`
  (`"lactating"`), and `production_system`. Cardot methods require
  `production_system = "free_stall_tmr"`, `dry_matter_intake_kg_day`,
  `milk_kg_day`, `diet_dry_matter_percent`, `minimum_temperature_c`, and
  `rainfall_mm_day`. Additional identity/source columns are retained.

- method:

  Select `"cardot_table5"` (default), `"cardot_abstract"`, or
  `"legacy_adult"`. There is no fallback between methods.

- extrapolation:

  `"abort"` (default) rejects predictors outside the marginal ranges in
  Table 4. `"allow"` warns and marks such rows. Impossible negative
  predictions always abort, never clamp to zero.

## Value

Input rows with `drinking_l_day`, `drinking_m3`,
`method_drinking_water`, `quantity_basis`, `water_extrapolated`, and
`drinking_rate_source` / `drinking_rate_basis` provenance.

## Examples

``` r
# Published rounded means/exposure, not raw individual cow-day records.
x <- tibble::tibble(
  animal_days = 1837, item_cbs_code = 960L, cohort = "lactating",
  production_system = "free_stall_tmr", dry_matter_intake_kg_day = 20.6,
  milk_kg_day = 26.5, diet_dry_matter_percent = 48.1,
  minimum_temperature_c = 0.1, rainfall_mm_day = 1.3,
  rate_id = "study_means", assessment_period = "2003–2005 winter trials",
  exposure_source = "Cardot 2008 Tables 1 and 3",
  exposure_basis = "observed cow-days"
)
intake <- estimate_dairy_drinking(x)
rates <- intake |>
  dplyr::transmute(
    rate_id, item_cbs_code, cohort, production_system, drinking_l_day,
    service_l_day = NA_real_, quantity_basis,
    rate_source = drinking_rate_source, rate_basis = drinking_rate_basis
  )
calculate_livestock_water(x, rates)
#> # A tibble: 1 × 23
#>   animal_days item_cbs_code cohort    production_system dry_matter_intake_kg_day
#>         <dbl>         <int> <chr>     <chr>                                <dbl>
#> 1        1837           960 lactating free_stall_tmr                        20.6
#> # ℹ 18 more variables: milk_kg_day <dbl>, diet_dry_matter_percent <dbl>,
#> #   minimum_temperature_c <dbl>, rainfall_mm_day <dbl>, rate_id <chr>,
#> #   assessment_period <chr>, exposure_source <chr>, exposure_basis <chr>,
#> #   drinking_l_day <dbl>, service_l_day <dbl>, quantity_basis <chr>,
#> #   rate_source <chr>, rate_basis <chr>, drinking_m3 <dbl>, service_m3 <dbl>,
#> #   direct_water_m3 <dbl>, coverage_direct_water <chr>, consumption_m3 <dbl>
```
