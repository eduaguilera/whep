# Physical arable and permanent-crop land base (fallow-inclusive).

Return FAO's physical land-use split of cropland into **arable land**
(annual/temporary crops plus their rotational fallow and temporary
meadows) and **permanent-crop land** (orchards, plantations, vineyards),
keyed by `(area_code, year)`.

whep's other crop-area paths
([`get_crop_land_extension()`](https://eduaguilera.github.io/whep/reference/get_crop_land_extension.md),
[`build_cropgrids_land_extension()`](https://eduaguilera.github.io/whep/reference/build_cropgrids_land_extension.md))
are all derived from crop *production* / harvested area and therefore
cannot recover the physical fallow-inclusive arable land of rain-fed,
fallow-prone economies: in a drought year a country's cereal harvest
collapses while its arable land (which counts the resting fallow) is
unchanged, so a harvested-area method assigns that land to perennials
and over-states the permanent share (e.g. Tunisia 2020 permanent share
0.73 from harvested area vs 0.43 physical). FAO's RL land-use survey
(`Cropland` = `Arable land` + `Permanent crops`) is the physical land
base; this function ingests it.

From 1961 the split is FAO's own (`source == "fao"`). Before 1961
(FAOSTAT's start) it is backcast from LUH2 land use: LUH2's annual vs.
perennial crop functional types give a perennial fraction and a cropland
shape that are spliced onto the FAO 1961 level so the series is
continuous (`source == "luh2"`). See Details.

## Usage

``` r
get_arable_permanent_land(
  years = NULL,
  input_dir = NULL,
  data = NULL,
  luh2_data = NULL,
  example = FALSE
)
```

## Arguments

- years:

  Integer vector of years to return, or `NULL` (default) for all
  available (1700-2025). The pre-1961 LUH2 backcast is computed only
  when `years` is `NULL` or requests a year before 1961.

- input_dir:

  Optional directory holding a local FAOSTAT RL land-use file
  (`faostat_land_use.csv` or a parquet with the FAOSTAT RL columns). If
  `NULL` (default) the pinned `faostat-landuse` dataset is read via
  [`whep_read_file()`](https://eduaguilera.github.io/whep/reference/whep_read_file.md).

- data:

  Optional in-memory FAOSTAT RL table in the raw pin schema (columns
  `Area Code`, `Item Code`, `Element`, `Unit`, `Year`, `Value`), used
  instead of the pin (chiefly for testing).

- luh2_data:

  Optional in-memory LUH2 land-use table (columns `ISO3`, `Year`,
  `Land_Use`, `Area_Mha`) used for the pre-1961 backcast instead of the
  pinned `luh2-areas` dataset (chiefly for testing).

- example:

  If `TRUE`, return a small illustrative table without reading remote
  data. Defaults to `FALSE`.

## Value

A tibble with one row per `(area_code, year)`:

- `area_code`: integer FAOSTAT area code (harmonised via
  [polity_area_crosswalk](https://eduaguilera.github.io/whep/reference/polity_area_crosswalk.md);
  the FAOSTAT "China" aggregate 351 is dropped).

- `year`: integer.

- `arable_ha`, `permanent_ha`, `cropland_ha`: physical land area in
  hectares.

- `source`: provenance, `"fao"` (\>= 1961) or `"luh2"` (pre-1961
  backcast).

## Details

The FAO identity `Cropland = Arable land + Permanent crops` holds in the
source to rounding for essentially all country-years; `permanent_ha` is
taken as `Cropland - Arable land` (clamped at 0) so
`arable_ha + permanent_ha` reconstructs `cropland_ha` exactly wherever
FAO reports `Arable <= Cropland`. Where FAO reports `Arable land` but
not `Permanent crops` (924 country-years, mostly arable-only economies)
this yields the permanent land the survey implies; where it reports
`Permanent crops` but not `Arable land` (a few coconut atolls)
`arable_ha` is filled from `Cropland - Permanent crops`.

Pre-1961 backcast: LUH2 annual cropland is `c3ann + c4ann + c3nfx`,
perennial is `c3per + c4per`. For each country the perennial fraction
and the cropland level are rescaled by their ratio to the LUH2 value at
1961 and multiplied by the FAO 1961 perennial fraction and cropland, so
both match FAO exactly at the 1961 splice point and carry LUH2's earlier
dynamics backwards. Countries without a FAO 1961 anchor receive no
backcast.

## Examples

``` r
get_arable_permanent_land(example = TRUE)
#> # A tibble: 2 × 6
#>   area_code  year arable_ha permanent_ha cropland_ha source
#>       <int> <int>     <dbl>        <dbl>       <dbl> <chr> 
#> 1       222  2020   2831300      2119200     4950500 fao   
#> 2       222  1960   2600000      1500000     4100000 luh2  
```
