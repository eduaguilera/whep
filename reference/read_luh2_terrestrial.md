# Read the LUH2 terrestrial-area validation layer

Reads `staticData_quarterdeg.nc` and returns `(1 - icwtr) * carea`
summed to the 0.5-degree grid: the terrestrial area LUH2 itself implies.
This is the DA-5 validation layer for
[`build_polycell_support()`](https://eduaguilera.github.io/whep/reference/build_polycell_support.md)
and is never a production mask, because `icwtr` includes the ocean as
well as ice and inland water, and because LUH2 misses small islands its
own 0.25-degree mask calls sea.

## Usage

``` r
read_luh2_terrestrial(vintage = c("GCB2022", "v2h"), dir = NULL)
```

## Arguments

- vintage:

  Which LUH2 tree to read. `"GCB2022"` (default) is the
  `UofMD-landState-LUH2-GCB2022` release under
  `Sys.getenv("WHEP_LUH2_DIR")`; `"v2h"` is the base release under
  `Sys.getenv("WHEP_LUH2_V2H_DIR")`.

- dir:

  Directory holding `staticData_quarterdeg.nc`, overriding the vintage's
  environment variable.

## Value

A `tibble` with `lon`, `lat` and `terrestrial_ha` on the 0.5-degree
grid, carrying the vintage in its `"luh2_vintage"` attribute.

## Examples

``` r
# Requires WHEP_LUH2_DIR to be set; not run without it.
if (nzchar(Sys.getenv("WHEP_LUH2_DIR"))) {
  read_luh2_terrestrial(vintage = "GCB2022")
}
```
