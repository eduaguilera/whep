# Read a CRU TS 4.09 monthly climate variable into a tidy tibble.

Reads one monthly variable of the CRU TS 4.09 high-resolution gridded
climate dataset (1901-2024, global 0.5 degree) from its NetCDF file and
returns it in tidy long form, one row per land cell-month. Requested
years are sliced at read time so the full grid is never materialised,
and ocean fill cells are dropped. Values are returned in the file's
native units (not converted): `tmp`/`tmn`/`tmx`/`dtr` in degrees
Celsius, `pre` in mm/month, `pet` in mm/day, `vap` in hPa, `cld` in
percent, `wet` and `frs` in days.

## Usage

``` r
read_cru_climate(
  var = c("tmp", "pet", "pre", "tmn", "tmx", "vap", "cld", "wet", "dtr", "frs"),
  years = NULL,
  cru_dir = NULL,
  data = NULL,
  example = FALSE
)
```

## Source

CRU TS 4.09 (Climatic Research Unit, University of East Anglia; Harris,
Osborn & Jones 2020, Scientific Data,
[doi:10.1038/s41597-020-0453-3](https://doi.org/10.1038/s41597-020-0453-3)
).

## Arguments

- var:

  Variable name, one of `"tmp"`, `"pet"`, `"pre"`, `"tmn"`, `"tmx"`,
  `"vap"`, `"cld"`, `"wet"`, `"dtr"` or `"frs"`.

- years:

  Optional integer vector of calendar years to keep. `NULL` keeps every
  year present in the file.

- cru_dir:

  Path to the CRU TS NetCDF directory. Defaults to
  `Sys.getenv("WHEP_CRU_DIR")`; aborts when neither is set.

- data:

  Optional pre-read tibble (`lon`, `lat`, `year`, `month`, `value`) used
  in place of reading NetCDF, for testing.

- example:

  If `TRUE`, return a small fixture instead of reading data. Defaults to
  `FALSE`.

## Value

A tibble with columns `lon`, `lat`, `year`, `month`, `value` (native
units) and `var` (the requested variable name).

## Examples

``` r
read_cru_climate(example = TRUE)
#> # A tibble: 10 × 6
#>       lon    lat  year month value var  
#>     <dbl>  <dbl> <int> <int> <dbl> <chr>
#>  1  22.8  -31.2   2000     7   8.6 tmp  
#>  2  49.2  -14.2   2000     1  20.8 tmp  
#>  3 110.     1.25  2000     1  26.2 tmp  
#>  4  57.2   25.8   2000     1  20.7 tmp  
#>  5  -1.75  27.2   2000     1  12.7 tmp  
#>  6  68.2   27.2   2000     1  16.2 tmp  
#>  7 -92.8   38.8   2000     1  -0.1 tmp  
#>  8  42.2   58.8   2000     7  19   tmp  
#>  9  18.8   66.2   2000     7  13   tmp  
#> 10  80.8   72.8   2000     7   5.6 tmp  
```
