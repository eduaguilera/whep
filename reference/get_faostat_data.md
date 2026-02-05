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
if (FALSE) { # \dontrun{
get_faostat_data("livestock", year = 2010, area = "Portugal")
} # }
```
