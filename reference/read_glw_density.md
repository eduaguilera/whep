# Read GLW3 gridded livestock counts on WHEP's 0.5-degree grid

Read the Gridded Livestock of the World version 3 rasters (GLW 3,
Gilbert et al. 2018) and return them as the `glw_density` table
[`build_gridded_livestock()`](https://eduaguilera.github.io/whep/reference/build_gridded_livestock.md)
allocates on under `proxy_method = "glw3"`: one row per 0.5-degree cell
and WHEP `species_group`, carrying that cell's GLW3 head count.

GLW3 is published at 5 arc-minutes (1/12 degree) in absolute animals per
pixel, so the 36 pixels tiling a 0.5-degree cell are summed and nothing
is rescaled; see *How five-arcmin pixels become 0.5-degree cells*. The
eight GLW species are then mapped onto WHEP's eleven species groups by
`crosswalk`; see *The GLW3 crosswalk and what it cannot split*.

The rasters come from `glw_dir`, else the `WHEP_GLW3_DIR` environment
variable, which points at the directory
`inst/scripts/download/download_glw3.R` fills from the published Harvard
Dataverse DOIs. An unset variable aborts naming that script; there is no
fallback to another layer.

## Usage

``` r
read_glw_density(
  species = NULL,
  variant = c("DA", "AW"),
  glw_dir = NULL,
  crosswalk = NULL,
  example = FALSE
)
```

## Source

Gilbert, M., Nicolas, G., Cinardi, G., Vanwambeke, S., Van Boeckel, T.
P., Wint, G. R. W. and Robinson, T. P. (2018). Global distribution data
for cattle, buffaloes, horses, sheep, goats, pigs, chickens and ducks in
2010. *Scientific Data*, 5, 180227.
[doi:10.1038/sdata.2018.227](https://doi.org/10.1038/sdata.2018.227) .
Rasters: Harvard Dataverse `glw_3`, version 3.0, CC0 1.0 Universal.

## Arguments

- species:

  Which GLW species to read, as named in `crosswalk`'s `glw_species`
  column (`"cattle"`, `"buffaloes"`, `"sheep"`, `"goats"`, `"pigs"`,
  `"chickens"`, `"ducks"`, `"horses"`). `NULL` (default) reads all of
  them. An unknown name aborts listing the known ones rather than
  returning a table missing a group.

- variant:

  Which GLW3 product to read: `"DA"` (default, the dasymetric product,
  `5_<Sp>_2010_Da.tif`) or `"AW"` (the areal-weighted product,
  `6_<Sp>_2010_Aw.tif`), validated with
  [`rlang::arg_match()`](https://rlang.r-lib.org/reference/arg_match.html).
  The two are alternatives, never fallbacks: the dasymetric product
  redistributes census counts with high-resolution covariates, the
  areal-weighted one spreads them evenly over the reporting unit's
  suitable land.

- glw_dir:

  Directory holding the GLW3 GeoTIFFs, overriding `WHEP_GLW3_DIR`. A
  `GLW3` subdirectory of it is also searched, since that is where
  `download_glw3(dest_dir)` puts them. Defaults to `NULL`.

- crosswalk:

  Optional tibble mapping GLW species onto WHEP species groups,
  replacing the packaged default. Required columns `glw_species`,
  `glw_code`, `species_group`; see *The GLW3 crosswalk and what it
  cannot split*. Defaults to `NULL`.

- example:

  If `TRUE`, return a small fixture instead of reading any raster.
  Defaults to `FALSE`.

## Value

A tibble with one row per cell and species group:

- `lon`, `lat`: 0.5-degree cell centre coordinates.

- `species_group`: WHEP livestock functional type.

- `density`: GLW3 head count in that cell, the quantity
  [`build_gridded_livestock()`](https://eduaguilera.github.io/whep/reference/build_gridded_livestock.md)
  documents as "heads per cell".

- `glw_variant`: the `variant` these rows were read from, `"DA"` or
  `"AW"`. It travels with the values so that
  [`build_gridded_livestock()`](https://eduaguilera.github.io/whep/reference/build_gridded_livestock.md)
  can record the product a run allocated on in `method_livestock_proxy`
  without being told separately, which is the only way the label cannot
  drift from the geography. Cells with no positive count are dropped, so
  the table is sparse.

## How five-arcmin pixels become 0.5-degree cells

GLW3 pixel values are absolute animal counts per pixel, not densities
(Dataverse file description: "absolute number of animals per pixel; 4320
by 2160 pixels of 0.083333 decimal degrees resolution"). The aggregation
is therefore a plain block sum: each 0.5-degree cell takes the sum of
the 6 x 6 = 36 five-arcmin pixels it contains, missing pixels skipped,
and a cell whose pixels are all missing is dropped. No division by area
happens anywhere, because none is needed to reach the engine's per-cell
head count.

The block factor is derived from the raster's own resolution rather than
hardcoded, and a resolution that does not divide 0.5 degrees a whole
number of times aborts: a raster on another grid would otherwise be
silently resampled onto shifted cells.

Resolution alone does not place a raster, so the extent is checked too.
Every edge must fall on a WHEP cell boundary: a raster of the right
resolution offset by, say, 0.1 degrees aggregates onto centres that are
not WHEP's, and an extent that is not a whole number of cells leaves a
partial edge block whose truncated sum would travel as a full cell's
head count. Neither is visible downstream –
[`build_gridded_livestock()`](https://eduaguilera.github.io/whep/reference/build_gridded_livestock.md)'s
join on `lon`/`lat` would simply drop the species – so both abort here,
naming the offending edge and its offset.

## The GLW3 crosswalk and what it cannot split

`inst/extdata/glw3_species_group.csv` maps the eight GLW species onto
WHEP's `species_group` vocabulary
(`inst/extdata/livestock_mapping.csv`). Columns:

- `glw_species`: GLW species name, and the value `species` selects on.

- `glw_code`: the two-letter code in the published file names (`Ct`,
  `Bf`, `Sh`, `Gt`, `Pg`, `Ch`, `Ho`, `Dk`).

- `species_group`: the WHEP group the layer feeds.

- `note`: why that row exists, for the reader of the file.

Two rules govern a many-to-one or one-to-many row set, and both are
applied by this reader:

- **Several GLW species to one group are summed**: `sheep` + `goats`
  into `sheep_goats`.

- **One GLW species to several groups gives each group the same value**:
  `cattle` feeds both `cattle_dairy` and `cattle_non_dairy`, `chickens`
  feeds `chickens_layers` and `chickens_broilers`. GLW3 carries no dairy
  and no layer/broiler split, so the finer groups inherit one geography
  and their national totals still differ. This is the interim "coarse
  layer constrains the sum of the finer groups" rule of whep#1000; task
  T10 may replace it with a split, and the replacement is a new
  crosswalk plus a rule here, not a change of contract.

`poultry` takes the `ducks` layer alone. WHEP's `poultry` group is
ducks + geese/guinea fowls + turkeys
(`inst/extdata/livestock_mapping.csv`, FAOSTAT items 1068, 1072 and
1079); chickens are not in it, they have their own two groups. The
crosswalk used to feed `chickens` into `poultry` as well, and since
chickens outnumber ducks by an order of magnitude almost everywhere, the
summation replaced the duck geography with a chicken-dominated one
(whep#1000, task T15a-iii).

This is the duck-only half of the decided rule, and it is interim. The
decided rule is a within-group split by national head shares,
`density(poultry) = duck * w_duck + chicken * (1 - w_duck)` with
`w_duck` the national ducks share of ducks + geese/guinea fowls +
turkeys per area and year, falling back to the duck layer alone where a
country reports no poultry heads. That split cannot be applied here: it
is per country and per year, while this table is neither, and the
item-grain national head counts it needs are not among the inputs
[`run_spatialize()`](https://eduaguilera.github.io/whep/reference/run_spatialize.md)
loads (`livestock_country_data.parquet` is already summed to
`species_group`, so ducks, geese and turkeys arrive as one number).
Until that input exists, `poultry` carries the geography of the one
group member GLW3 publishes, which is the decided rule's own no-heads
treatment, rather than the geography of a species that is not in the
group at all.

`camels` and `other` have no GLW species and are absent from the result
by construction. That is deliberate:
`build_gridded_livestock(proxy_method = "glw3")` aborts naming any group
its density table does not cover, so those groups have to be run under
`proxy_method = "luh2"` rather than being given a wrong geography here.

## Examples

``` r
read_glw_density(example = TRUE)
#> # A tibble: 14 × 5
#>      lon   lat species_group     density glw_variant
#>    <dbl> <dbl> <chr>               <dbl> <chr>      
#>  1 -3.75  40.2 cattle_dairy        12400 DA         
#>  2 -3.25  40.2 cattle_dairy         8150 DA         
#>  3 -3.75  40.2 cattle_non_dairy    12400 DA         
#>  4 -3.25  40.2 cattle_non_dairy     8150 DA         
#>  5 -3.75  40.2 chickens_broilers  870000 DA         
#>  6 -3.25  40.2 chickens_broilers  642000 DA         
#>  7 -3.75  40.2 chickens_layers    870000 DA         
#>  8 -3.25  40.2 chickens_layers    642000 DA         
#>  9 -3.75  40.2 pigs                61200 DA         
#> 10 -3.25  40.2 pigs                44800 DA         
#> 11 -3.75  40.2 poultry             25300 DA         
#> 12 -3.25  40.2 poultry             18100 DA         
#> 13 -3.75  40.2 sheep_goats        143000 DA         
#> 14 -3.25  40.2 sheep_goats         97600 DA         
```
