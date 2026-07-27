# Assemble WHEP's cell-polity crosswalk with true grid-cell area.

Reads the cached cell-polity fraction parquet (`lon`, `lat`,
`area_code`, `polity_frac`) and adds `cell_area_ha`, computed from
latitude with the same 0.5-degree cell-area formula used across the
package (see
[`build_grass_availability_lpjml()`](https://eduaguilera.github.io/whep/reference/build_grass_availability_lpjml.md)).
This assembles the `data$cell_polity` contract that every Module C
function (e.g.
[`build_n_deposition()`](https://eduaguilera.github.io/whep/reference/build_n_deposition.md),
[`build_urban_n()`](https://eduaguilera.github.io/whep/reference/build_urban_n.md),
[`get_soc_climate_drivers()`](https://eduaguilera.github.io/whep/reference/get_soc_climate_drivers.md))
expects as a required input.

## Usage

``` r
build_cell_polity(polity_fraction_path = NULL)
```

## Arguments

- polity_fraction_path:

  Path to the cell-polity fraction parquet. Defaults to
  `Sys.getenv("WHEP_POLITY_FRACTION_PATH")`.

## Value

A tibble with `lon`, `lat`, `area_code`, `polity_frac` and
`cell_area_ha`.

## Examples

``` r
# Requires WHEP_POLITY_FRACTION_PATH to be set; not run without it.
if (nzchar(Sys.getenv("WHEP_POLITY_FRACTION_PATH"))) {
  build_cell_polity()
}
```
