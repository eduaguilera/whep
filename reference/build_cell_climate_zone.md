# Build the per-cell IPCC climate zone from CRU mean annual temperature.

Reduce CRU TS monthly near-surface temperature to a mean annual
temperature (MAT) per 0.5-degree land cell and year, and classify each
cell into the IPCC manure-management climate zone (`"Cool"`,
`"Temperate"`, `"Warm"`) that
[climate_mcf](https://eduaguilera.github.io/whep/reference/climate_mcf.md)
is keyed on. This is the per-cell replacement for the single hardcoded
`"Temperate"` zone the manure CH4 model falls back to when no zone is
supplied.

CRU TS covers 1901 onwards, so years outside its span cannot be
measured. They are not dropped and not silently given a measured year's
value: the 30-year climatology at the nearest end of the record is held
constant across them and stamped in `method_climate_zone` with its own
label (for example `"climatology_1901_1930"`), so a reader can tell a
backcast zone from a measured one without reading this code. Holding an
early-record climatology constant backwards is an assumption, not an
observation.

## Usage

``` r
build_cell_climate_zone(
  years = NULL,
  cru_dir = NULL,
  data = NULL,
  example = FALSE
)
```

## Source

CRU TS (Climatic Research Unit, University of East Anglia; Harris,
Osborn & Jones 2020, Scientific Data,
[doi:10.1038/s41597-020-0453-3](https://doi.org/10.1038/s41597-020-0453-3)
). Zone cuts: IPCC 2006 Guidelines Vol.4 Ch.3, adopted by the 2019
Refinement Vol.4 Ch.10.

## Arguments

- years:

  Integer vector of calendar years to return. `NULL` (default) returns
  every year the CRU record covers. Years before the record are backcast
  and years after it are forward-held, both from the nearest 30-year
  climatology.

- cru_dir:

  Path to the CRU TS NetCDF directory. Defaults to
  `Sys.getenv("WHEP_CRU_DIR")`; aborts when neither is set. Ignored when
  `data` is supplied.

- data:

  Optional pre-read tibble of annual means (`lon`, `lat`, `year`,
  `mean_annual_temp_c`) used in place of reading CRU, for testing. The
  years it carries are treated as the measured record, so the backcast
  and forward-hold logic still applies to `years` outside them.

- example:

  If `TRUE`, return a small fixture instead of reading CRU data.
  Defaults to `FALSE`.

## Value

A tibble with one row per cell and year:

- `lon`, `lat`: Cell centre coordinates (0.5-degree grid).

- `year`: Calendar year.

- `mean_annual_temp_c`: Mean annual near-surface air temperature
  (degrees Celsius), the mean of the twelve monthly CRU values.

- `climate_zone`: IPCC zone, `"Cool"`, `"Temperate"` or `"Warm"`.

- `method_climate_zone`: `"cru_ts_annual"` for a measured year, or
  `"climatology_<first>_<last>"` for a held climatology.

## Examples

``` r
build_cell_climate_zone(example = TRUE)
#> # A tibble: 6 × 6
#>     lon   lat  year mean_annual_temp_c climate_zone method_climate_zone  
#>   <dbl> <dbl> <int>              <dbl> <chr>        <chr>                
#> 1  34.2 -0.25  1880               21.6 Warm         climatology_1901_1930
#> 2  34.8  0.25  1880               20.0 Warm         climatology_1901_1930
#> 3  35.2  0.75  1880               17.2 Temperate    climatology_1901_1930
#> 4  34.2 -0.25  1961               22.5 Warm         cru_ts_annual        
#> 5  34.8  0.25  1961               20.9 Warm         cru_ts_annual        
#> 6  35.2  0.75  1961               17.0 Temperate    cru_ts_annual        
```
