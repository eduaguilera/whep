# IPCC Ym values.

Methane conversion rate (% GE) by species and feed situation.

- Cattle feedlot (\>90% concentrate): 3.0%.

- Sheep: a single 6.7%, irrespective of feed quality (no body-weight
  split).

## Usage

``` r
ipcc_2019_ym
```

## Format

A tibble with `category`, `feed_situation`, `ym_percent`.

## Source

Mixed across editions.

- Sheep 6.7% and goats 5.5% are the 2019 Refinement, Vol 4, Ch 10, Table
  10.13 (Updated).

- Cattle and buffalo 6.5% on pasture/range and mixed rations are the
  2006 Guidelines Table 10.12, which gives 6.5% for every non-feedlot
  cattle and buffalo class. The 2019 Refinement's Table 10.12 (Updated)
  resolves cattle and buffalo Ym by production level and feed
  digestibility instead: 5.7 / 6.0 / 6.3 / 6.5 for dairy cows by yield
  class, 7.0 for \>75 percent forage non-dairy, 6.3 for mixed rations,
  4.0 for grain feedlots and 3.0 for steam-flaked-corn feedlots. The
  stored feedlot 3.0% is therefore the 2006 "\>=90 percent concentrate"
  value, which in the 2019 Refinement applies only to the
  steam-flaked-corn case.

- Camels 5.0% appears in no IPCC table. Both editions instead direct
  compilers to reuse the other-cattle or buffalo Ym for camels, which
  would be 6.5%. **Assumed, unverified.** Tracked in whep#601.

## Examples

``` r
ipcc_2019_ym
#> # A tibble: 8 × 4
#>   category feed_situation       ym_percent ym_uncertainty
#>   <chr>    <chr>                     <dbl>          <dbl>
#> 1 Cattle   Pasture/Range               6.5              1
#> 2 Cattle   Mixed                       6.5              1
#> 3 Cattle   Feedlot (>90% conc.)        3                1
#> 4 Buffalo  Pasture/Range               6.5              1
#> 5 Buffalo  Mixed                       6.5              1
#> 6 Sheep    All                         6.7              1
#> 7 Goats    All                         5.5              1
#> 8 Camels   All                         5                1
```
