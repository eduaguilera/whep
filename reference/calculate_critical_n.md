# Calculate critical-nitrogen allowances with the Schulte-Uebbing method.

Computes the critical nitrogen input and surplus of every 0.5-degree
cell with the equations of Schulte-Uebbing et al. (2022, Nature 610,
507-512, doi:10.1038/s41586-022-05158-2, Supplementary Information Eqs.
1-36), instead of reading the fixed 2010 layers of their archive
(doi:10.5281/zenodo.6395016) as
[`read_critical_n()`](https://eduaguilera.github.io/whep/reference/read_critical_n.md)
does by default. The critical input is the fertiliser and manure that
keeps each of three impacts at its threshold, plus the constant fixation
and the deposition it implies:

- `"de"`: deposition at the critical load of the cell's biome (5 to 20
  kg N/ha of cell area, SI Supplementary Table 2);

- `"sw"`: 5 mg N/l in runoff to surface water (SI Eq. 12);

- `"gw"`: 11.6 mg NO3-N/l in water leaching from agricultural soil (SI
  Eq. 29);

- `"mi"`: the lowest of the three, land use by land use (SI Eqs. 31-32).

Fertiliser and manure change in their current proportion (SI Eq. 3);
fixation, the non-agricultural sources and extensive grassland stay at
their current values. Where the non-agricultural sources alone exceed a
threshold, fertiliser and manure are set to zero (`critical_rule`
`"non_agricultural_floor"`). Where the threshold allows more than crops
can use, the input is cut off at the input needed for the regional yield
potential at current nitrogen use efficiency, capped at 0.8 (Methods
Eqs. 1-2; `"yield_potential_cap"`). The critical surplus is input minus
uptake (SI Eqs. 5-6).

With the archive's own 2010 inputs the result reproduces the deposited
layers closely but not exactly, because the source does not print every
rule it applied (see the section below). Supplying `inputs` from another
year or model applies the same method to them.

## Usage

``` r
calculate_critical_n(
  inputs = NULL,
  dir = NULL,
  verify_source = TRUE,
  example = FALSE
)
```

## Arguments

- inputs:

  Optional tibble with one row per cell and the IMAGE-GNM quantities
  listed by `whep:::.critn_input_specs()`: `cell_id`, `lon`, `lat`,
  areas in hectares (`area_total_ha`, `area_arable_ha`,
  `area_intensive_ha`, `area_extensive_ha`, `area_natural_ha`), the
  IMAGE `biome` and `image_region` codes, `runoff_l` (litres per year)
  and the nitrogen flows in kg N per cell per year. Fertiliser and
  manure are net of NH3, as the archive deposits them. When `NULL`
  (default) the 2010 inputs are read from the archive's `Input_files`.

- dir:

  Optional archive directory, resolved as in
  [`read_critical_n()`](https://eduaguilera.github.io/whep/reference/read_critical_n.md).
  Ignored when `inputs` is supplied.

- verify_source:

  If `TRUE` (default), the archive's input rasters are checked against
  the package's content manifest before they are read. Ignored when
  `inputs` is supplied.

- example:

  If `TRUE`, return the result for a small fixture of four cells instead
  of reading data. Defaults to `FALSE`.

## Value

A tibble with one row per cell, threshold and land-use scope: `cell_id`,
`lon`, `lat`, `image_region`, `critical_threshold` (`"de"`, `"sw"`,
`"gw"` or `"mi"`), `critical_land_use` (`"ara"`, `"igl"` or `"all"`),
`area_ha` (the hectares of that scope), the critical
`critical_n_input_kgn_ha` and `critical_n_surplus_kgn_ha`, the current
`current_n_input_kgn_ha` and `current_n_surplus_kgn_ha` (all kg N per
hectare per year), `critical_rule` (`"environmental_threshold"`,
`"non_agricultural_floor"` or `"yield_potential_cap"`; `NA` for `"all"`)
and `method_critical_n = "reproduced"`. A cell whose land use has no
current fertiliser or manure, no crop uptake, or no agricultural
leaching has no critical value there, as in the archive.

## Rules recovered from the deposited layers

The SI prints the equations for cells with one reducible agricultural
land use and states that cells combining arable land with grassland
follow "slightly different" formulas. The rules for those cells, and
several values the text leaves out, were recovered by recomputing the
archive's outputs from its inputs: fertiliser and manure enter gross of
their NH3 emission; the groundwater limit is 11.6 mg NO3-N/l (the
article's Methods print 11.3); ice cells get 5 kg N/ha of critical
deposition (Supplementary Table 2 prints "n.a."); the fertiliser share
is clipped to \[1e-4, 1 - 1e-4\]; the regional yield-gap ratios carry
three decimals (each rounds to the two printed in Supplementary Table
5). In cells with both arable land and intensive grassland, both land
uses are scaled by one factor for the deposition and surface-water
thresholds. For groundwater there, each land use is held to its area
share of the critical leaching and the one that must fall further is
solved first; this rule is a reconstruction, and those cells carry the
remaining differences.

## Examples

``` r
calculate_critical_n(example = TRUE)
#> # A tibble: 36 × 13
#>    cell_id   lon   lat image_region critical_threshold critical_land_use area_ha
#>      <int> <dbl> <dbl>        <int> <chr>              <chr>               <dbl>
#>  1   89786  72.8  27.8           18 de                 ara                45604.
#>  2   61368 -96.2  47.2            2 de                 ara                49086.
#>  3   59642 121.   48.8           20 de                 ara                28095 
#>  4   59642 121.   48.8           20 de                 igl               118959 
#>  5   61591  15.2  47.2           11 de                 igl                59438.
#>  6   89786  72.8  27.8           18 de                 all                45604.
#>  7   61368 -96.2  47.2            2 de                 all                49086.
#>  8   59642 121.   48.8           20 de                 all               147054 
#>  9   61591  15.2  47.2           11 de                 all                59438.
#> 10   89786  72.8  27.8           18 sw                 ara                45604.
#> # ℹ 26 more rows
#> # ℹ 6 more variables: critical_n_input_kgn_ha <dbl>,
#> #   critical_n_surplus_kgn_ha <dbl>, current_n_input_kgn_ha <dbl>,
#> #   current_n_surplus_kgn_ha <dbl>, critical_rule <chr>,
#> #   method_critical_n <chr>
```
