# Source-item vocabularies for the subnational admin statistics

Seven tables mapping the crop and livestock classes the subnational
admin-statistics readers return – verbatim, in each publisher's own
identifiers – onto WHEP's own item and species vocabularies. Four cover
crops (`admin_items_*`) and three cover livestock (`admin_species_*`);
the JRC dataset publishes no livestock, so it has no species table.

The tables are vocabulary, not data: they say what a source class means
in WHEP terms and, where it means nothing exactly, that it is dropped
and why. Nothing here converts, sums or reads a value.

## Format

The four `admin_items_*` tables share eight columns:

- `source`: the reader's own source label (`"USDA_NASS"`, `"Eurostat"`,
  `"IBGE_PAM"`, `"JRC_subnational_crops"`).

- `source_table`: the dataflow, dump or release the class belongs to.
  `"apro_cpshr|apro_cpnhr_h"` where the two Eurostat crop dataflows
  share one vocabulary.

- `class_key`: the table's unique key, the source class code where the
  publisher issues one and the class name where it does not.

- `source_class_code`, `source_class_name`: the class exactly as the
  source publishes it. The code is `NA` for JRC and NASS, neither of
  which issues one.

- `item_prod_code`: the WHEP
  [items_prod_full](https://eduaguilera.github.io/whep/reference/items_prod_full.md)
  key, as character. `NA` on a dropped class.

- `mapping_kind`, `mapping_reason`: as described above.

The three `admin_species_*` tables carry the same columns with
`item_prod_code` replaced by two:

- `species_group`: the WHEP livestock functional type of
  `inst/extdata/livestock_mapping.csv` (`"cattle_dairy"`, `"pigs"`,
  `"sheep_goats"`, ...). `NA` where the class maps to no single group.

- `constrains`: for an `"aggregate"` class that maps to no single group,
  the `"+"`-joined set of groups whose **sum** it constrains, for
  example `"cattle_dairy+cattle_non_dairy"`. `NA` on every other kind,
  which the builder enforces.

## Source

Eurostat dissemination API
(<https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1>), dataflows
`apro_cpshr`, `apro_cpnhr_h`, `apro_mt_ls_r` and `ef_lsk_poultry`; IBGE
SIDRA aggregate metadata
(<https://servicodados.ibge.gov.br/api/v3/agregados>), tables 5457, 3939
and 94; Ronchetti, G. et al. (2024). Harmonized European Union
subnational crop statistics reveal climate impacts and crop cultivation
shifts. *Earth System Science Data*, 16, 1623-1649.
[doi:10.5194/essd-16-1623-2024](https://doi.org/10.5194/essd-16-1623-2024)
; USDA National Agricultural Statistics Service, Quick Stats bulk
downloads (<https://www.nass.usda.gov/datasets/>).

## Mapping kinds

`mapping_kind` takes five values, and the distinction between the first
four is what stops a source total being counted twice:

- `"exact"`: one source class, one WHEP target.

- `"aggregate"`: a total the publisher ships alongside its own members.
  The aggregate binds; its members are never summed alongside it.

- `"member"`: one of those members. It is recorded so the class is
  accounted for rather than silently absent, and is not summed.

- `"sum_member"`: one of several classes that **do** sum to the WHEP
  target, the publisher shipping no aggregate for them.

- `"dropped"`: no exact WHEP counterpart. The class is never folded into
  a near neighbour; `mapping_reason` says what it is and why it could
  not be mapped, so a coverage report can count it.

Every row that is not `"exact"` carries a `mapping_reason`, and the
builder aborts on one that does not.

## The rules these tables encode

The tables decide nothing. They are the data form of the vocabulary
rules fixed for the subnational spatialization, and each row's
`mapping_reason` names the rule it applies:

- Crops. Where a publisher ships an aggregate class, that aggregate
  binds and its members are never summed alongside it. Maize is grain
  maize only, silage and forage maize excluded. A source rice area is
  paddy and binds `item_prod_code` 27. A class with no exact WHEP
  counterpart is dropped and counted, never folded into a near
  neighbour.

- Livestock. A published dairy series binds dairy cattle, and non-dairy
  is the total minus the dairy series, the unit-year being refused where
  that difference is negative. A combined sheep-and-goats class
  constrains their sum. Published reference dates are accepted as they
  are, which is why the NASS hog inventory keeps its 1 December date. A
  combined poultry class constrains the sum of the poultry groups.

## Where the class lists come from

Each table enumerates the classes its source actually serves, taken from
the source itself rather than from a reader's defaults:

- Eurostat: the `crops` dimension of `apro_cpshr` and `apro_cpnhr_h`
  (identical 79-class vocabularies), the `animals` dimension of
  `apro_mt_ls_r` (47 classes) and of `ef_lsk_poultry` (9 classes), read
  off the dissemination API on 2026-09-04.

- IBGE SIDRA: classification 782 of table 5457 (72 categories, one of
  them the all-crops total) and classification 79 of table 3939 (10 herd
  types), from the `servicodados.ibge.gov.br` metadata endpoint on
  2026-09-04, plus table 94, whose single series is milked cows.

- JRC: the nine crop classes of release 2025.01 of the harmonised EU
  subnational crop statistics, as
  `inst/scripts/prepare_jrc_subnational.R` measured them on the pinned
  archive.

- USDA NASS: the Quick Stats `SHORT_DESC` series
  [`read_admin_stats_nass()`](https://eduaguilera.github.io/whep/reference/read_admin_stats_nass.md)
  requests, plus the silage-maize series the maize rule excludes. This
  is deliberately **not** an inventory of the NASS vocabulary: Quick
  Stats keys tens of thousands of series on `SHORT_DESC`, the bulk dump
  is the only complete listing of them, and no offline copy of it ships
  with this package.

[crops_eurostat](https://eduaguilera.github.io/whep/reference/crops_eurostat.md)
is the older, label-only Eurostat crop table (13 green fodder and
root-crop codes, no WHEP target). It is left as it is;
`admin_items_eurostat` is the vocabulary that carries the mapping, and
covers those 13 codes among its 79.

## See also

[`read_admin_stats_nass()`](https://eduaguilera.github.io/whep/reference/read_admin_stats_nass.md),
[`read_admin_stats_eurostat()`](https://eduaguilera.github.io/whep/reference/read_admin_stats_eurostat.md),
[`read_admin_stats_sidra()`](https://eduaguilera.github.io/whep/reference/read_admin_stats_sidra.md),
[items_prod_full](https://eduaguilera.github.io/whep/reference/items_prod_full.md),
[crops_eurostat](https://eduaguilera.github.io/whep/reference/crops_eurostat.md).
