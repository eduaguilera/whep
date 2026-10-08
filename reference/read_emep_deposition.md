# Read EMEP MSC-W atmospheric nitrogen deposition onto WHEP's grid.

Reads the yearly EMEP MSC-W model results (EMEP01 rv5.6, 2025 reporting
round) for one nitrogen species and aggregates the 0.1-degree grid to
WHEP's 0.5-degree grid. NHx is `DDEP_RDN_m2Grid + WDEP_RDN` and NOy is
`DDEP_OXN_m2Grid + WDEP_OXN`, dry plus wet. The source is a density
(mgN/m2 over the whole grid cell), so it is converted to kg N/ha (x
0.01) and aggregated by plain mean: the 0.1-degree grid nests exactly
5x5 inside each 0.5-degree block and the cell area varies by under 0.2
per mille inside a block.

EMEP is the reference
[`correct_n_deposition()`](https://eduaguilera.github.io/whep/reference/correct_n_deposition.md)
corrects HaNi's European trend toward. It is a regional product (domain
lon -30 to 90, lat 30 to 82) and is not a substitute for HaNi anywhere
else.

The files are third-party model output and are read from local disk:
`inst/scripts/download/download_emep.R` fetches them from the Norwegian
Meteorological Institute THREDDS server into `<dest_dir>/EMEP/`, and
`WHEP_EMEP_DIR` points there.

Simpson, D. *et al.* (2012). The EMEP MSC-W chemical transport model –
technical description. *Atmospheric Chemistry and Physics* 12(16),
7825-7865.
[doi:10.5194/acp-12-7825-2012](https://doi.org/10.5194/acp-12-7825-2012)

## Usage

``` r
read_emep_deposition(
  species = c("nhx", "noy"),
  emep_dir = NULL,
  years = NULL,
  example = FALSE
)
```

## Arguments

- species:

  Which species to read, `"nhx"` or `"noy"`.

- emep_dir:

  Path to the directory holding the yearly EMEP files. Defaults to
  `Sys.getenv("WHEP_EMEP_DIR")`.

- years:

  Optional integer vector of calendar years to read. `NULL` reads every
  year with a file in `emep_dir`. A requested year with no file is not
  read; the years actually returned are the ones in the output.

- example:

  If `TRUE`, return a small fixture instead of reading data. Defaults to
  `FALSE`.

## Value

A tibble with `lon`, `lat`, `year` and `deposition_kgn_ha` (the species'
grid-mean deposition rate over the whole 0.5-degree cell).

## Examples

``` r
read_emep_deposition(example = TRUE)
#> # A tibble: 1 × 4
#>     lon   lat  year deposition_kgn_ha
#>   <dbl> <dbl> <int>             <dbl>
#> 1 -0.25 -0.25  2020               1.1
```
