# Scrapes activity_data from FAOSTAT and slightly post-processes it

Important: Dynamically allows for the introduction of subsets as
`"..."`.

Note: overhead by individually scraping FAOSTAT code QCL for crop data;
it's fine.

## Usage

``` r
get_faostat_data(activity_data, ...)
```

## Arguments

- activity_data:

  activity data required from FAOSTAT; needs to be one of
  `c('livestock','crop_area','crop_yield','crop_production')`.

- ...:

  can be whichever column name from `get_faostat_bulk`, particularly
  `year`, `area` or `ISO3_CODE`.

## Value

`data.frame` of FAOSTAT for `activity_data`; default is for all years
and countries.

## Examples

``` r
# \donttest{
get_faostat_data("livestock", year = 2010, area = "Portugal")
#> Loading required package: FAOSTAT
#> Warning: Duplicated ISO3_CODE matched, double check the data
#> Warning: Certain ISO3_CODE were not matched.
#> Warning: Please check the correct China has been specified.
#> # A tibble: 18 × 7
#>    area     item               element  year    value unit  ISO3_CODE
#>    <chr>    <chr>              <chr>   <int>    <dbl> <chr> <chr>    
#>  1 Portugal Asses              stocks   2010    13538 An    PRT      
#>  2 Portugal Cattle, dairy      stocks   2010   243000 An    PRT      
#>  3 Portugal Cattle, non-dairy  stocks   2010  1204000 An    PRT      
#>  4 Portugal Chickens, broilers stocks   2010 25733000 An    PRT      
#>  5 Portugal Chickens, layers   stocks   2010  6500000 An    PRT      
#>  6 Portugal Goats              stocks   2010   419000 An    PRT      
#>  7 Portugal Horses             stocks   2010    97000 An    PRT      
#>  8 Portugal Mules and hinnies  stocks   2010     6273 An    PRT      
#>  9 Portugal Sheep              stocks   2010  2226000 An    PRT      
#> 10 Portugal Swine, breeding    stocks   2010   191700 An    PRT      
#> 11 Portugal Swine, market      stocks   2010  1725300 An    PRT      
#> 12 Portugal Turkeys            stocks   2010  2232000 An    PRT      
#> 13 Portugal Cattle             stocks   2010  1447000 An    PRT      
#> 14 Portugal Chickens           stocks   2010 32233000 An    PRT      
#> 15 Portugal Mules and Asses    stocks   2010    19811 An    PRT      
#> 16 Portugal Poultry Birds      stocks   2010 34465000 An    PRT      
#> 17 Portugal Sheep and Goats    stocks   2010  2645000 An    PRT      
#> 18 Portugal Swine              stocks   2010  1917000 An    PRT      
# }
```
