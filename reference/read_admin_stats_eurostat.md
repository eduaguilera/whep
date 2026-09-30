# Read Eurostat regional crop and livestock statistics

Read one of Eurostat's four regional agricultural dataflows over the
SDMX-CSV dissemination API and return its rows in the source's own
identifiers: the NUTS code and label exactly as Eurostat served them,
the crop or animal code and label exactly as Eurostat served them, and
the value converted to WHEP units. Nothing is mapped to a WHEP item code
and nothing is resolved to a polity here; those are separate steps, so a
change in either leaves this reader untouched.

Tables, verified live on 2026-09-02:

- `"apro_cpshr"` – crop production by NUTS 2 region, 2000-2024.

- `"apro_cpnhr_h"` – the historical companion, 1975-1999.

- `"apro_mt_ls_r"` – animal populations by NUTS 2 region, 1977-2025. It
  carries dairy cows (`A2300F`) and non-dairy cows (`A2300G`) separately
  but **no poultry**.

- `"ef_lsk_poultry"` – farm-structure-survey poultry by NUTS 2 region,
  census years only (2005, 2007, 2010, 2013, 2016, 2020, 2023).

No registration or key is needed. The dataflow identifier is checked
against Eurostat's own dataflow registry before any data request, so a
renamed table aborts naming the tables this reader knows instead of
returning an empty tibble.

## Usage

``` r
read_admin_stats_eurostat(
  table = c("apro_cpshr", "apro_cpnhr_h", "apro_mt_ls_r", "ef_lsk_poultry"),
  filters = list(),
  nuts_level = 2L,
  file = NULL,
  example = FALSE
)
```

## Arguments

- table:

  Dataflow to read, one of `"apro_cpshr"`, `"apro_cpnhr_h"`,
  `"apro_mt_ls_r"` or `"ef_lsk_poultry"`.

- filters:

  Named list narrowing the request, any of: `geo` (NUTS codes, e.g.
  `c("FR", "FRF2")`), `items` (crop or animal dimension codes, e.g.
  `"C1110"` or `"A2000"`) and `years` (integer years; the request spans
  their range and the result is filtered to them exactly). An absent or
  empty element means "everything Eurostat serves", which for
  `"ef_lsk_poultry"` is large.

- nuts_level:

  Integer NUTS levels to return as units, a subset of `1:3`. Defaults to
  `2L`, the level every one of these tables is published at.

- file:

  Path to an already-downloaded SDMX-CSV response to parse instead of
  calling the API. It must have been requested with `label=both`, since
  the version suffix is read from the labels.

- example:

  If `TRUE`, return a small fixture instead of reading data. Defaults to
  `FALSE`.

## Value

A tibble of source-native rows, one per unit x item x measure x year,
with columns `source` (`"Eurostat_<table>"`), `source_native_unit_id`,
`source_native_unit_name`, `source_native_item_code`,
`source_native_item_name`, `quantity` (`"area"`, `"production"` or
`"heads"`), `indicator_used` (the admin-shares indicator vocabulary for
area and production rows, `NA` for head counts, which that vocabulary
does not cover), `year`, `value` (converted to hectares, tonnes or
head), `value_unit`, `value_flag` (Eurostat's observation flag and
confidentiality status verbatim, joined by `"|"` when both are present,
`NA` when clean), `concept_break`, `grain`, `nuts_level`,
`nuts_version`, `source_version` (the response's `LAST UPDATE` stamp
verbatim) and `recorded_at`. The NUTS 0 rows are attached as the
`"national_check"` attribute, in the same columns with `grain = NA`.

## Units and the values this reader converts

Eurostat serves crop areas in thousand hectares (`AR_THS_HA`,
`MAR_THS_HA`), crop production in thousand tonnes (`HPRD_THS_T`,
`HPRD_HUMD_EU_THS_T`) and animal populations in thousand head (`THS_HD`)
or head (`HD`). Every response carries those units in the measure
dimension's own label, so the conversion factor is not a constant this
package asserts from outside: the reader checks each response's label
against the unit it is about to assume and aborts if Eurostat changes
it.

Humidity (`HUMD_PC`, `HUMD_EU_PC`) and yield (`YLD_HUMD_EU_T_HA`) rows
are dropped and counted. Yield is exactly production divided by area,
both of which this reader returns, and its tonnes-per-hectare unit is
outside this contract's `value_unit` vocabulary.

## What the area figures mean, and the 2025 break

Eurostat's own metadata (ESMS `apro_cp_esms`, section 3.4, metadata last
updated 7 October 2025) states that "under the pre-SAIO data collection,
up to the 2024 reference year, the area concept was area under
cultivation, i.e. the area actually harvested, with non-harvested areas
(for example due to natural disasters) excluded", which is why
`AR_THS_HA` maps to `indicator_used = "area_harvested"`. From the 2025
reference year the SAIO regulation changes that: "for cereals for the
production of grain, dry pulses and protein crops, root crops,
industrial crops, and plants harvested green, the areas refer to the
sown area", while vegetables stay on harvested area and permanent crops
on production area.

Because the new concept differs by crop group, and this reader has no
crop vocabulary, it does **not** relabel 2025+ rows. It flags them:
every `"apro_cpshr"` row with `year >= 2025` gets
`concept_break = TRUE`, and choosing the right `indicator_used` for them
is the resolver's job once a crop-group vocabulary exists. As of
2026-09-02 the dataflow held no year beyond 2024, so the flag is
prospective.

## NUTS versions, and why rows are deduplicated

Eurostat labels every geography whose code has been retired with the
last nomenclature in which it was valid – `FR21` arrives as
`"Champagne-Ardenne (NUTS 2013)"`. `nuts_version` is read from that
suffix, and a code without one is on the current nomenclature, NUTS 2024
(regulation 2023/674, in force 2024-2026; see
<https://ec.europa.eu/eurostat/web/nuts/history>). The suffix is
Eurostat's own retirement marker, so `nuts_version` means "the last
nomenclature this code was valid in", which is exactly what a later
code-system-scoped resolution step needs.

One response therefore mixes vintages: `apro_cpnhr_h` carries `FR21` for
1989-1999 and `FRF2` – a pure recoding of the same polygon – for
1990-1999. Rows for the same territory in the same year are deduplicated
keeping the newest vintage, and the dropped count, the code pairs and
any value disagreement are reported. Two rows count as the same
territory only when they share a country, a NUTS level and a label once
the version suffix is stripped; that comparison finds recodings only,
never resolves a code to a polity, and never merges across levels, so
`FI2`/`FI20` (both labelled "Aland") and `DE3`/`DE30` (both "Berlin")
stay separate. A genuine boundary change between vintages keeps both
rows, visible, for the resolution step to settle.

## Grain, and the levels this returns

NUTS 2 is `grain = "admin1"` and NUTS 3 is `"admin2"`, the mapping the
admin-shares grain rule uses for EU countries. NUTS 1 rows are returned
only when asked for (`nuts_level = 1L`) and carry `grain = NA`: in some
countries NUTS 1 is a genuine first-order division (Germany's
Bundeslaender) and in others it is an aggregate of the units below it,
so which of the two it is cannot be decided per row here. Germany is the
case that forces the question – it reports crop area at NUTS 2 only up
to 2004 and at NUTS 1 from 2005 onward (verified against `apro_cpshr` on
2026-09-02).

NUTS 0 rows are not units at all; they are the national totals the units
should sum to, and they are returned separately as the
`"national_check"` attribute of the result. Supranational aggregates
(`EU`, `EU27_2020`) and composite codes that would double-count their
own members (`EL41_42`, served for 2007-2014 alongside `EL41` and
`EL42`) are dropped and counted.

## Examples

``` r
read_admin_stats_eurostat(example = TRUE)
#> # A tibble: 10 × 17
#>    source    source_native_unit_id source_native_unit_n…¹ source_native_item_c…²
#>    <chr>     <chr>                 <chr>                  <chr>                 
#>  1 Eurostat… FR21                  Champagne-Ardenne (NU… C1110                 
#>  2 Eurostat… FRF2                  Champagne-Ardenne      C1110                 
#>  3 Eurostat… FRF2                  Champagne-Ardenne      C1110                 
#>  4 Eurostat… FRF1                  Alsace                 C1300                 
#>  5 Eurostat… ES51                  Cataluña               C1110                 
#>  6 Eurostat… DE11                  Stuttgart              C1110                 
#>  7 Eurostat… FRF2                  Champagne-Ardenne      A2000                 
#>  8 Eurostat… FRF2                  Champagne-Ardenne      A2300F                
#>  9 Eurostat… FI20                  Åland                  A3100                 
#> 10 Eurostat… FRF2                  Champagne-Ardenne      A5000                 
#> # ℹ abbreviated names: ¹​source_native_unit_name, ²​source_native_item_code
#> # ℹ 13 more variables: source_native_item_name <chr>, quantity <chr>,
#> #   indicator_used <chr>, year <int>, value <dbl>, value_unit <chr>,
#> #   value_flag <chr>, concept_break <lgl>, grain <chr>, nuts_level <int>,
#> #   nuts_version <chr>, source_version <chr>, recorded_at <chr>
```
