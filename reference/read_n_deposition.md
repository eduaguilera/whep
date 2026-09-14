# Read a HaNi atmospheric nitrogen deposition species onto WHEP's grid.

Reads one HaNi NHx or NOy deposition NetCDF (native 5-arcmin grid, total
grams N deposited per native cell per year) and aggregates it to WHEP's
0.5-degree grid by summing the 6x6 fine cells inside each 0.5-degree
block, since the source quantity is an extensive mass. Returns the
summed mass per 0.5-degree cell; converting to a per-hectare rate needs
the true cell area and is done downstream by
[`build_n_deposition()`](https://eduaguilera.github.io/whep/reference/build_n_deposition.md).

## Usage

``` r
read_n_deposition(
  species = c("nhx", "noy"),
  hani_dir = NULL,
  years = NULL,
  example = FALSE
)
```

## Source

Tian, H., Bian, Z., Shi, H., Qin, X., Pan, N., Lu, C., Pan, S.,
Tubiello, F. N., Chang, J., Conchedda, G., Liu, J., Mueller, N.,
Nishina, K., Xu, R., Yang, J., You, L. and Zhang, B. (2022). History of
anthropogenic Nitrogen inputs (HaNi) to the terrestrial biosphere: a 5
arcmin resolution annual dataset from 1860 to 2019. *Earth System
Science Data* 14(10), 4551-4568.
[doi:10.5194/essd-14-4551-2022](https://doi.org/10.5194/essd-14-4551-2022)

## Arguments

- species:

  Which HaNi species to read, `"nhx"` or `"noy"`.

- hani_dir:

  Path to the directory holding `ndep_nhx.nc` and `ndep_noy.nc`.
  Defaults to `Sys.getenv("WHEP_HANI_DIR")`.

- years:

  Optional integer vector of calendar years to keep. `NULL` reads every
  year present in the file.

- example:

  If `TRUE`, return a small fixture instead of reading data. Defaults to
  `FALSE`.

## Value

A tibble with `lon`, `lat`, `year`, `value_g` (total grams N deposited
in the 0.5-degree cell that year) and `method_deposition`, the constant
`"hani"`. The provenance tag travels with the field so that
[`build_n_deposition()`](https://eduaguilera.github.io/whep/reference/build_n_deposition.md)
can record where its deposition came from instead of asserting it; see
that function's `method_deposition` note.

## Measured European bias, growing backwards in time

HaNi is **too flat over Europe**: it largely misses both the European
deposition peak around 1990 and the fall that emission controls drove
afterwards. Measured against the EMEP MSC-W model (rv5.6, 2025 reporting
round) on WHEP's own 0.5-degree grid, over the 3193 cells assigned to an
EMEP core country, area-weighted kg N/ha/yr:

|      |      |       |             |
|------|------|-------|-------------|
| year | HaNi | EMEP  | HaNi / EMEP |
| 1990 | 9.81 | 14.08 | 0.696       |
| 2000 | 9.65 | 11.28 | 0.856       |
| 2010 | 9.04 | 9.56  | 0.946       |
| 2019 | 8.00 | 8.07  | 0.992       |

Across 1990-2019 HaNi falls 18.4% where EMEP falls 42.7%, and the
shortfall summed over those cells and years is 22.6 Tg N. The worst 1990
ratios are Italy 0.386, Denmark 0.429, Ireland 0.570, Germany 0.585 and
Poland 0.603. The error is therefore a **function of time**, largest
exactly where and when European deposition was largest, and it compounds
backwards into the pre-1990 period where EMEP offers no check at all.
Anything integrating deposition over the historical period inherits a
trajectory that is too flat.

Nothing here corrects for it: which product is right, and what shape a
correction should take, is a scientific decision recorded in whep#1097
and not taken by this reader. What the code does provide is the means to
express one – a corrected field injected through
[`build_n_deposition()`](https://eduaguilera.github.io/whep/reference/build_n_deposition.md)'s
`data` argument is recorded per cell in `method_deposition` rather than
inheriting HaNi's name. `validation/n_deposition_emep.R` reproduces the
table above and writes the full per-country, per-year ratio series.

The comparison product is the EMEP MSC-W chemical transport model, run
in support of the Convention on Long-Range Transboundary Air Pollution:
Simpson, D. *et al.* (2012). The EMEP MSC-W chemical transport model –
technical description. *Atmospheric Chemistry and Physics* 12(16),
7825-7865.
[doi:10.5194/acp-12-7825-2012](https://doi.org/10.5194/acp-12-7825-2012)

## Examples

``` r
read_n_deposition(example = TRUE)
#> # A tibble: 1 × 5
#>     lon   lat  year  value_g method_deposition
#>   <dbl> <dbl> <int>    <dbl> <chr>            
#> 1 -0.25 -0.25  2020 30800000 hani             
```
