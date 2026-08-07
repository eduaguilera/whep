
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Who Has Eaten the Planet <a href="https://eduaguilera.github.io/whep/"><img src="man/figures/logo.png" align="right" height="139" alt="whep website" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/eduaguilera/whep/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/eduaguilera/whep/actions/workflows/R-CMD-check.yaml)
[![CRAN
status](https://www.r-pkg.org/badges/version/whep)](https://CRAN.R-project.org/package=whep)
[![whep status
badge](https://eduaguilera.r-universe.dev/whep/badges/version)](https://eduaguilera.r-universe.dev/whep)
[![Codecov test
coverage](https://codecov.io/gh/eduaguilera/whep/branch/main/graph/badge.svg)](https://app.codecov.io/gh/eduaguilera/whep?branch=main)
[![Project Status:
Active](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
<!-- badges: end -->

`whep` harmonises the scattered public record of world agriculture —
what each country produced, fed, traded, processed and consumed — into
one set of tidy tables reaching back to 1850, and models the
environmental pressures that follow from it: soil carbon and nitrogen
balances, greenhouse gas emissions, land, water, and the footprints
these leave along international trade chains.

## Why this package exists

The environmental history of food is a data problem before it is a
modelling problem. The quantities a researcher needs live in a dozen
archives that do not agree on item lists, country codes, units or years
— and most of them begin in 1961, when FAOSTAT does. Work on the last
sixty years is therefore abundant, while the century of agricultural
change that produced today’s food system is, as the project abstract
below puts it, “very poorly quantified before 1961”.

`whep` is the code that closes that gap for the WHEP project, released
as a package so the same pipeline can be read, re-run and criticised by
anyone else. It does three things, each a chain of functions you can
call individually, inspect and swap:

- **Harmonise.** FAOSTAT, LUH2 land use, HYDE population, CRU climate,
  LPJmL model output and HWSD soils are mapped onto one item/region/unit
  vocabulary, gap-filled where the record is silent, and extended back
  to 1850 — `build_primary_production()`, `build_commodity_balances()`,
  `build_detailed_trade()`, `build_food_supply()`.
- **Model the biophysics.** Gridded soil carbon dynamics (five
  selectable models), the full soil nitrogen balance (inputs, ammonia,
  direct and indirect nitrous oxide, leaching), the soil water budget,
  and livestock feed intake and emissions — `build_carbon_balance()`,
  `build_nitrogen_balance()`, `build_water_balance()`,
  `get_feed_intake()`.
- **Trace pressures through trade.** A physical multi-regional
  input–output core with one extension per stressor, so a pressure
  caused where production happens is attributed to the consumption that
  drove it — `build_io_model()`, `build_footprint()`.

### Who it is for

Researchers in food-system, agro-environmental and environmental-history
work who are comfortable in R and want the accounting itself rather than
a finished figure: the intermediate tables, the coefficients behind
them, and the freedom to change a method and see what moves. It is a
research pipeline, not a general-purpose data-download tool, and it
assumes you want to know how each number was made.

### How it relates to FABIO

The footprint core deliberately follows FABIO, the physical
multi-regional input–output model of global agriculture and forestry
(Bruckner et al. 2019,
[doi:10.1021/acs.est.9b03554](https://doi.org/10.1021/acs.est.9b03554)),
and reuses its item and region code conventions, so results stay
comparable and the method is a published one. What `whep` adds is the
reason it exists:

- **Time span.** The published FABIO tables start in 1986; `whep` builds
  its own supply-use base from 1850.
- **Territories that change.** Borders move over 170 years, so the
  harmonised tables record the polity a row belonged to in that year
  (`ESP-1800-2025`, `BLX-1850-1999`) next to the FAOSTAT-style area
  code, instead of assuming today’s country list held in 1900.
- **Biophysics computed, not attached.** Soil carbon, nitrogen and water
  are modelled here, gridded and dynamic, so an extension can be traced
  back to the mechanism and the coefficients that produced it.
- **Methods stay selectable.** Where more than one defensible method
  exists it is a `method =` (or `tier =`) argument, never a silent
  fallback: the default is the most rigorous available, and the choice
  is recorded in a `method_*` column of the output.

## Installation

The package is under constant development. Initial stable releases are
available from both CRAN and
[R-universe](https://eduaguilera.r-universe.dev/whep).

You can install the stable version on CRAN:

``` r
install.packages('whep')
```

If you want the development version of `whep`, you can:

``` r
# Install from GitHub
pak::pak("eduaguilera/whep")

# Install from R-universe
install.packages(
  "whep",
  repos = c("https://eduaguilera.r-universe.dev", "https://cloud.r-project.org")
)
```

## Usage

``` r
library(whep)
```

Every pipeline builder returns a tidy tibble keyed by year, area and
item. A full build reads pinned inputs or multi-gigabyte rasters and
takes minutes to hours, so each builder also accepts `example = TRUE`
and returns a small fixture of its own output — enough to see the schema
and try a join without downloading anything:

``` r
build_commodity_balances(example = TRUE) |>
  add_item_cbs_name() |>
  dplyr::select(year, reporting_polity_name, item_cbs_name, element, value)
#> # A tibble: 10 × 5
#>     year reporting_polity_name     item_cbs_name           element             value
#>    <dbl> <chr>                     <chr>                   <chr>               <dbl>
#>  1  2010 Laos                      Bovine Meat             import           1.76e+ 3
#>  2  1981 Tunisia                   Poultry Meat            domestic_supply  4.1 e+ 4
#>  3  1906 Spain                     Wine                    processing       6.35e+ 4
#>  4  1899 Guinea-Bissau (1886-1974) Eggs                    food             7.26e+ 1
#>  5  2018 Costa Rica                Palm kernels            domestic_supply  1.20e+ 5
#>  6  1871 Australia                 Wool (Clean Eq.)        stock_variation -7.28e-12
#>  7  1938 Uganda (1926-1962)        Milk - Excluding Butter production       1.51e+ 5
#>  8  1924 Austria                   Sunflower seed          production       1.61e+ 2
#>  9  1928 Hong Kong                 Fruits, Other           domestic_supply  1.85e+ 4
#> 10  1879 Venezuela                 Peas                    seed             3.83e- 8
```

Joins and grouping happen on integer codes throughout, because country
and item names change over the period covered; the `add_*_name()`
helpers attach human-readable labels at the end, as above. Each row also
carries the polity it belonged to in that year (`reporting_polity_code`,
`reporting_polity_name`), which is why the 1899 row above reads
*Guinea-Bissau (1886-1974)* and not today’s Guinea-Bissau.

The estimation functions work on your own numbers, not only on pipeline
output. Direct soil nitrous oxide from applied nitrogen, using the
default IPCC 2019 Tier 1 emission factors:

``` r
n_applied <- tibble::tribble(
  ~area_code, ~climate, ~irrig_type, ~n_input_t,
  203, "MED", "Rainfed", 1000,
  11, "ATL", "Rainfed", 1000
)

calculate_soil_n2o(n_applied)
#> # A tibble: 2 × 6
#>   area_code climate irrig_type n_input_t n2o_direct_n_t method_soil_n2o
#>       <dbl> <chr>   <chr>          <dbl>          <dbl> <chr>          
#> 1       203 MED     Rainfed         1000              5 ipcc2019       
#> 2        11 ATL     Rainfed         1000             10 ipcc2019
```

Alternative methods are arguments rather than hidden defaults, and the
one used is recorded in the output, so a sensitivity analysis is a
one-line change — here the older IPCC 2006 factors, which do not
distinguish the two climates:

``` r
calculate_soil_n2o(n_applied, method = "ipcc2006")
#> # A tibble: 2 × 6
#>   area_code climate irrig_type n_input_t n2o_direct_n_t method_soil_n2o
#>       <dbl> <chr>   <chr>          <dbl>          <dbl> <chr>          
#> 1       203 MED     Rainfed         1000             10 ipcc2006       
#> 2        11 ATL     Rainfed         1000             10 ipcc2006
```

Running a pipeline for real needs its external inputs: WHEP-curated
datasets are fetched with `whep_read_file()` from the board listed in
`whep_inputs`, and the large third-party archives (CRU, LUH2, LPJmL,
HYDE, HWSD) are read from local disk through documented `WHEP_*`
environment variables, with each reader telling you what to set. Tracing
a pressure through trade is covered end-to-end in the [Environmental
footprint
analysis](https://eduaguilera.github.io/whep/articles/footprint-analysis.html)
vignette, and every function is listed on the [reference
page](https://eduaguilera.github.io/whep/reference/index.html).

## The WHEP project

#### **Who Has Eaten the Planet? The paths of food systems beyond the safe and just operating space (1850-2020)**

Food production covers the most basic human need, and simultaneously is
the main driver of anthropogenic environmental impacts. These impacts
have resulted in the transgression, during the brief period since the
industrial revolution, of the planetary boundaries defining the safe
operating space of humanity. A rich research literature quantifies the
last 60 years’ fast, heterogeneous, and often unfair development in food
supply and related environmental impacts, and how these depend on
agro-climatic factors, technology, and trade flows, all of which have
greatly changed but with different trajectories around the world.
However, these developments lack an integrated approach, and are very
poorly quantified before 1961. WHEP will bridge these knowledge gaps,
assessing “who has eaten the planet” by answering the questions:

> What are the environmental impacts of food production since 1850?

> What is the role of trade in food supply and in displacing the
> responsibilities for these impacts?

> How are impacts related to planetary boundaries, food supply and
> inequality?

These highly ambitious goals are addressed by four objectives:

1.  Constructing a consolidated global country-level annual database on
    agricultural production and management, using massive data collation
    in combination with modeling.
2.  Estimating the environmental impacts: greenhouse gas emissions and
    carbon, land, water, nitrogen, and phosphorus through spatially
    explicit, integrated, dynamic modeling.
3.  Calculating product footprints and tracing them along international
    trade chains.
4.  Analyzing the observed trajectories in the safe and just operating
    space, by assessing the drivers, and how impacts at the production
    and consumption levels are related to fair and healthy supply. This
    ground-breaking research will shed new light on the environmental
    history of food, opening up many new research frontiers, and
    providing necessary information to design fair and sustainable
    policies.

You can also visit the [European project
site](https://cordis.europa.eu/project/id/101115126).

## Contributing

We welcome contributions! Please see our [Contributing
Guide](https://github.com/eduaguilera/whep/blob/main/.github/CONTRIBUTING.md)
for how to find something to work on, set up the package, run the CI
checks locally, and open a pull request.

If you’re new to R package development, we have written a **small free
online book** covering both git and R package development. You can
directly **access it
[here](https://lbm364dl.github.io/follow-the-workflow/)**.

## Citation

To cite `whep` in publications, use:

``` r
citation("whep")
```

## Code of Conduct

Please note that this package is released with a [Contributor Code of
Conduct](https://ropensci.org/code-of-conduct/). By contributing to this
project, you agree to abide by its terms.
