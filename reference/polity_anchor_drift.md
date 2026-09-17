# Find back-cast rows whose polity label and growth proxy describe different territories

A pre-1962 WHEP row is a reconstruction with **two territorial
references** and they need not agree (whep#748):

- its **level** is the area's reported value at the back-cast anchor,
  walked backwards by
  [`fill_proxy_growth()`](https://eduaguilera.github.io/whep/reference/fill_proxy_growth.md),
  so it describes the territory that area had in `backcast_anchor`. That
  is the territory `reporting_polity_code` names, because
  [`add_polity_code()`](https://eduaguilera.github.io/whep/reference/add_polity_code.md)
  floors the polity lookup at the same anchor;

- its **year-on-year movement** is a ratio of LUH2 land, and under
  `build_primary_production(land_method = "present_day")` the
  `luh2-areas` pin is keyed on **present-day ISO3**, so the movement
  describes the territory that area has today.

Wherever a territory changed after the anchor those are different
polygons. Nothing in a built table says so:
[`polity_coverage_gaps()`](https://eduaguilera.github.io/whep/reference/polity_coverage_gaps.md)
answers the neighbouring question "was this polity live in the row's
year", which is true of far more rows and says nothing about whether the
label's extent is the extent the value was computed on. This reports the
disagreement itself, for a table that has already been built, so it can
be sized without re-deriving the crosswalk.

No row is an error and none is dropped from any build. `drift_kind` says
what kind of disagreement a pair carries:

- `"entity"`: the anchor polity and the reference polity are different
  entities, e.g. area 181 is labelled `SRH-1953-1964` (Southern
  Rhodesia) while its LUH2 movement is Zimbabwe's. The label can then
  overlap a sibling area's own published rows.

- `"interval"`: the same entity in two vintages, e.g. area 238 is
  labelled `ETH-1952-1993`, which includes Eritrea, while its movement
  is `ETH-1993-2025`, which does not. The entity name matches and the
  polygon does not, which is the harder class to notice.

- `"unmapped_reference"`: the area resolves to no polity at
  `reference_year` at all, so what the movement describes cannot be
  named.

A pair whose anchor polity is itself `NA` is **not** reported: that row
has no label to disagree with, and it is
[`polity_mapping_provenance()`](https://eduaguilera.github.io/whep/reference/polity_mapping_provenance.md)
and
[`polity_coverage_gaps()`](https://eduaguilera.github.io/whep/reference/polity_coverage_gaps.md)
that account for it.

`data_year_polity_code` is reported alongside, resolved unfloored,
because it is the third reference in play:
`build_primary_production(land_method = "historical_polity")` measures
the `ha` half inside that polity's polygon (see
[`build_historical_land_areas()`](https://eduaguilera.github.io/whep/reference/build_historical_land_areas.md)),
so under that method the movement describes `data_year_polity_code`
rather than `reference_polity_code`.

## Usage

``` r
polity_anchor_drift(
  table,
  code_column = "area_code",
  year_column = "year",
  backcast_anchor = 1961L,
  reference_year = 2023L
)
```

## Arguments

- table:

  A data frame carrying an area-code column and a year column.

- code_column:

  Name of the column holding numeric area codes. The column may hold
  either a FAOSTAT `area_code` or the `polity_area_code` bucket that
  published outputs are keyed by; both resolve through the same lookup.

- year_column:

  Name of the column holding years. Required: the whole question is
  year-dependent.

- backcast_anchor:

  First year of reported (non-back-cast) FAOSTAT data; passed to the
  same resolution
  [`add_polity_code()`](https://eduaguilera.github.io/whep/reference/add_polity_code.md)
  documents, and the year the level is anchored at.

- reference_year:

  The year whose territory the growth proxy describes. Defaults to
  `2023L`,
  [`build_primary_production()`](https://eduaguilera.github.io/whep/reference/build_primary_production.md)'s
  default `end_year`, i.e. the last year the published artifact covers,
  which is the vintage the present-day ISO3 keying of the `luh2-areas`
  pin resolves to. Measured on the shipped crosswalk the answer is
  identical for every reference year from 2014 to 2024, so the default
  is not load-bearing.

## Value

A tibble with one row per drifting `(area_code, year)`, ordered by area
code and year, carrying `area_code`, `year`, `anchor_polity_code`,
`anchor_polity_name`, `data_year_polity_code`, `reference_polity_code`,
`reference_polity_name`, `drift_kind` and `n_rows`, the number of rows
of `table` that pair carries. Zero rows means every pre-anchor row's
label describes the same territory its growth proxy does.

## Only the back-cast era is reported

A row at or after `backcast_anchor` carries a value its own year's
territory reported, resolved at its own year, so its label and its level
agree by construction and it is not a disagreement this function has
anything to say about. Such rows are filtered out, which is why a
`table` holding no pre-anchor year returns zero rows. A missing year
column aborts rather than returning zero rows for the same reason: an
empty answer must mean "no drift", never "nothing was supplied to look
at".

## See also

[`polity_coverage_gaps()`](https://eduaguilera.github.io/whep/reference/polity_coverage_gaps.md)
for rows attributed to a polity not live in their year,
[`polity_mapping_provenance()`](https://eduaguilera.github.io/whep/reference/polity_mapping_provenance.md)
for which authority a row's identity rests on, and
[`polity_bucket_coverage()`](https://eduaguilera.github.io/whep/reference/polity_bucket_coverage.md)
for buckets that sum more than one territory.

## Examples

``` r
# Area 238 Ethiopia is the documented case: the 1850 row is labelled
# `ETH-1952-1993`, which includes Eritrea, while its LUH2 growth proxy is
# keyed on present-day `ETH`, which does not.
polity_anchor_drift(
  tibble::tibble(
    area_code = c(238L, 238L, 11L),
    year = c(1850L, 2000L, 1850L),
    value = 1
  )
)
#> # A tibble: 1 × 9
#>   area_code  year anchor_polity_code anchor_polity_name   data_year_polity_code
#>       <int> <int> <chr>              <chr>                <chr>                
#> 1       238  1850 ETH-1952-1993      Ethiopia (1952-1993) ETH-1800-1889        
#> # ℹ 4 more variables: reference_polity_code <chr>, reference_polity_name <chr>,
#> #   drift_kind <chr>, n_rows <int>
```
