# Correct HaNi's European deposition trend toward EMEP.

Rescales one HaNi species, country by country and year by year, so that
its trajectory inside the EMEP core countries follows EMEP's while its
level over a reference period stays HaNi's own. This is the
year-resolved correction whep#1121 asks for and is **opt-in**: nothing
in the package calls it, and
[`build_n_deposition()`](https://eduaguilera.github.io/whep/reference/build_n_deposition.md)
keeps reading plain HaNi unless a corrected field is injected through
its `data` argument.

## Usage

``` r
correct_n_deposition(
  hani,
  emep,
  cell_polity,
  method = c("emep_trend", "none"),
  reference_years = 2010:2019,
  area_codes = NULL
)
```

## Arguments

- hani:

  One HaNi species as returned by
  [`read_n_deposition()`](https://eduaguilera.github.io/whep/reference/read_n_deposition.md):
  `lon`, `lat`, `year`, `value_g` and `method_deposition`, which must be
  `"hani"` on every row so that a field cannot be corrected twice. It
  must cover `reference_years`.

- emep:

  The same species from
  [`read_emep_deposition()`](https://eduaguilera.github.io/whep/reference/read_emep_deposition.md):
  `lon`, `lat`, `year` and `deposition_kgn_ha`. It must cover
  `reference_years`.

- cell_polity:

  The cell-polity table, as
  [`build_cell_polity()`](https://eduaguilera.github.io/whep/reference/build_cell_polity.md)
  returns it: `lon`, `lat`, `area_code`, `polity_frac` and
  `cell_area_ha`.

- method:

  `"emep_trend"` (default) applies the correction; `"none"` returns
  `hani` unchanged, so a pipeline can switch it off without a second
  code path.

- reference_years:

  Integer years over which the corrected mass keeps HaNi's mean level.
  Defaults to `2010:2019`.

- area_codes:

  Integer `area_code`s of the countries to correct. `NULL` (default)
  takes the 39 EMEP core countries, the European countries the EMEP
  model is built to represent. Its domain also reaches Central Asia and
  the Arabian peninsula, where the two products disagree by factors of
  2-5 in both directions, which is a domain-edge artifact rather than a
  HaNi bias, so those countries are not corrected by default.

## Value

`hani` with `value_g` rescaled on corrected rows, those rows stamped
`method_deposition = "hani_emep_trend"`, and a `deposition_correction`
column holding the factor applied (`1` on every untouched row).

## What is wrong with HaNi over Europe

Over cells at least 98% land in the 39 EMEP core countries, HaNi falls
from 10.6 to 8.6 kg N/ha/yr between 1990 and 2019 (-19%) while EMEP
falls from 14.6 to 8.3 (-43%), so the HaNi/EMEP ratio climbs from 0.73
to 1.04 (`validation/n_deposition_emep.R`). HaNi is too flat over
exactly the period in which European emission controls halved
deposition.

## The correction

For country \\c\\ and year \\y\\ the factor is \$\$f\_{c,y} =
\frac{E\_{c,y} / \bar{E}\_c}{H\_{c,y} / \bar{H}\_c}\$\$ where \\H\\ and
\\E\\ are the HaNi and EMEP masses summed over the country's cells and
the bars are their means over `reference_years`. Every HaNi cell of that
country-year is multiplied by \\f\_{c,y}\\. Three properties follow, and
the tests pin each:

- **Year-resolved.** Every year has its own factor, so the three years
  2011-2013 in which HaNi sits above EMEP are lowered while 1990 is
  raised. A single scale factor anchored on one year cannot do both.

- **Trend, not level.** The mean of the corrected mass over
  `reference_years` equals HaNi's, and HaNi's spatial pattern inside a
  country is kept. EMEP's level is never imposed: HaNi books mass
  deposited to land while EMEP is a density over the whole cell, so
  their levels differ by the land fraction in every coastal cell, and
  that difference cancels in a ratio of ratios but not in a ratio.

- **Bounded in time and space.** Only country-years in the EMEP core
  list that have both products are touched; every other row is returned
  as read and keeps `method_deposition = "hani"`. In particular
  **nothing before 1990 is corrected** (EMEP rv5.6 starts in 1990), so a
  corrected series steps at 1989-1990 by that year's factor – about +40%
  over the core countries for the two species together. How to carry the
  correction back in time is the open part of whep#1121 and is not
  decided here.

`reference_years` defaults to 2010-2019, the last decade the two
products overlap, in which their core-country ratio stays within
0.89-1.03. It is a choice, not a sourced value (assumed, unverified): it
says which period's HaNi level is trusted. A decade rather than one
year, because the ratio is not monotone and a single anchor year would
carry its own noise into every other year.

Each border cell is assigned to the country holding its largest
`polity_frac`, so it is counted once when the factors are formed and
gets one factor.

## Examples

``` r
correct_n_deposition(
  hani = read_n_deposition(example = TRUE),
  emep = read_emep_deposition(example = TRUE),
  cell_polity = tibble::tribble(
    ~lon, ~lat, ~area_code, ~polity_frac, ~cell_area_ha,
    -0.25, -0.25, 79L, 1, 300000
  ),
  reference_years = 2020L
)
#> # A tibble: 1 × 6
#>     lon   lat  year  value_g method_deposition deposition_correction
#>   <dbl> <dbl> <int>    <dbl> <chr>                             <dbl>
#> 1 -0.25 -0.25  2020 30800000 hani_emep_trend                       1
```
