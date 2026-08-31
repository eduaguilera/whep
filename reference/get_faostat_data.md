# Scrape activity data from FAOSTAT and post-process it

Important: Dynamically allows for the introduction of subsets as
`"..."`.

Note: overhead by individually scraping FAOSTAT code QCL for crop data;
it's fine.

Data is downloaded straight from FAOSTAT's public bulk download service
(`https://bulks-faostat.fao.org`), with no third-party client library
and no API key: the dataset catalog resolves `activity_data` to its "All
Data Normalized" zip, which is downloaded and read directly (#45).
FAOSTAT's separate query API at `faostatservices.fao.org` now requires
an authorization header WHEP does not have; the bulk download service is
unaffected and needs none.

## Usage

``` r
get_faostat_data(activity_data, ..., example = FALSE)
```

## Arguments

- activity_data:

  activity data required from FAOSTAT; needs to be one of
  `c('livestock','crop_area','crop_yield','crop_production')`.

- ...:

  can be whichever column name from the resulting bulk data,
  particularly `year`, `area` or `ISO3_CODE`.

- example:

  Logical. If `TRUE`, return a small hardcoded example `tibble` instead
  of scraping FAOSTAT. Useful for offline demos and documentation.
  Default `FALSE`.

## Value

`tibble` of FAOSTAT for `activity_data` with columns `area`, `item`,
`element`, `year`, `value`, `unit` and `ISO3_CODE`; default is for all
years and countries. `ISO3_CODE` is resolved from the `area_iso3c`
column of
[polity_area_crosswalk](https://eduaguilera.github.io/whep/reference/polity_area_crosswalk.md)
and is `NA` for FAOSTAT's regional and multi-territory aggregates,
including the `"China"` aggregate (area 351), which by design has no
ISO3 code of its own.

## Examples

``` r
get_faostat_data(example = TRUE)
#> # A tibble: 10 × 7
#>    area     item               element  year   value unit    ISO3_CODE
#>    <chr>    <chr>              <chr>   <int>   <dbl> <chr>   <chr>    
#>  1 Portugal Asses              stocks   2010    1500 An      PRT      
#>  2 Portugal Cattle, dairy      stocks   2010  245000 An      PRT      
#>  3 Portugal Cattle, non-dairy  stocks   2010 1180000 An      PRT      
#>  4 Portugal Chickens, broilers stocks   2010   27000 1000 An PRT      
#>  5 Portugal Goats              stocks   2010  412000 An      PRT      
#>  6 Portugal Horses             stocks   2010   22000 An      PRT      
#>  7 Portugal Mules and hinnies  stocks   2010    3200 An      PRT      
#>  8 Portugal Sheep              stocks   2010 2230000 An      PRT      
#>  9 Portugal Swine, breeding    stocks   2010  340000 An      PRT      
#> 10 Portugal Swine, market      stocks   2010 1980000 An      PRT      
```
