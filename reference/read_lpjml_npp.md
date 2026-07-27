# Read a per-PFT annual LPJmL carbon variable into a tidy tibble.

Reads one annual per-plant-functional-type (PFT) carbon output of a
finished LPJmL run, either net primary production (`"npp"`, from
`pft_npp.nc`) or harvested carbon (`"harvestc"`, from
`pft_harvestc.nc`), and returns it in tidy long form with the PFT name
attached. Values are per-PFT-stand carbon densities in grams of carbon
per square metre per year. Requested years are sliced at read time so
the full cube is never materialised. The two files index their PFT bands
differently, so downstream code should join on `name_pft`, never on the
band position.

## Usage

``` r
read_lpjml_npp(
  var = c("npp", "harvestc"),
  years = NULL,
  run_dir = NULL,
  first_year = 1901L,
  data = NULL,
  example = FALSE
)
```

## Arguments

- var:

  Logical variable name, `"npp"` (per-PFT net primary production) or
  `"harvestc"` (per-PFT harvested carbon).

- years:

  Optional integer vector of calendar years to keep. `NULL` keeps every
  year present in the file.

- run_dir:

  Path to the LPJmL run output directory. Defaults to
  `Sys.getenv("WHEP_LPJML_RUN_DIR")`.

- first_year:

  First calendar year of the run's annual time axis.

- data:

  Optional pre-read tibble (`lon`, `lat`, `year`, `npft`, `name_pft`,
  `value`) used in place of reading NetCDF, for testing.

- example:

  If `TRUE`, return a small fixture instead of reading remote data.
  Defaults to `FALSE`.

## Value

A tibble with columns `lon`, `lat`, `year`, `npft` (band index),
`name_pft` (PFT name) and `value` (gC/m2/yr).

## Examples

``` r
read_lpjml_npp(example = TRUE)
#> # A tibble: 10 × 6
#>      lon   lat  year  npft name_pft                              value
#>    <dbl> <dbl> <int> <int> <chr>                                 <dbl>
#>  1  26.2  35.2  2000     3 temperate needleleaved evergreen tree  48.6
#>  2  26.2  35.2  2000     9 Tropical C4 grass                     325  
#>  3  26.2  35.2  2000    10 Temperate C3 grass                     66.9
#>  4  26.2  35.2  2000    25 rainfed grassland                     496  
#>  5 -64.2 -35.8  2000     3 temperate needleleaved evergreen tree 699  
#>  6 -64.2 -35.8  2000    10 Temperate C3 grass                     96.2
#>  7 -64.2 -35.8  2000    25 rainfed grassland                     910  
#>  8 -74.8 -52.2  2000    10 Temperate C3 grass                    279  
#>  9 -74.8 -52.2  2000    25 rainfed grassland                     325  
#> 10 -74.8 -52.2  2000    41 irrigated grassland                     0  
```
