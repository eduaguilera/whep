# The admin-shares table contract

Declare, as data, the shared row shape every admin-shares reader emits
and every admin-shares consumer reads: one subnational administrative
unit's reported value and its share of the FAOSTAT container's total,
for one `(item_prod_code, indicator_used, year)`. Readers emit
source-native identifiers and never resolve names to WHEP codes here; a
later resolution step reconciles those once. Unresolved rows keep
`level_polity_code == NA` visibly rather than being dropped.

There are two measurement columns, `value` and `share`, and a row must
carry at least one of them. `value` may be missing wherever `share` is
present, which is a **first-class case** created by a publication
consent and not a gap to be filled; see the section below before writing
anything that assumes an absolute value is there.

`admin_shares_schema()` is the
[`check_table_schema()`](https://eduaguilera.github.io/whep/reference/check_table_schema.md)
contract itself;
[`admin_shares_prototype()`](https://eduaguilera.github.io/whep/reference/admin_shares_prototype.md)
is the zero-row tibble it implies, and the two cannot drift apart
because the prototype is built *from* the schema with
[`empty_table_from_schema()`](https://eduaguilera.github.io/whep/reference/empty_table_from_schema.md).

## Usage

``` r
admin_shares_schema()
```

## Value

A schema list, as documented in
[`check_table_schema()`](https://eduaguilera.github.io/whep/reference/check_table_schema.md):
closed (`extra_columns = "forbid"`), keyed on
`(area_code, level_polity_code, level, item_prod_code, species_group, indicator_used, year)`.

## Admin-shares table

One row per
`(area_code, level_polity_code, level, item_prod_code, species_group, indicator_used, year)`:

- `area_code`: FAOSTAT-style area code of the *container* – the polity
  the subnational units sum into, not the unit itself.

- `level_polity_code`: the polity code resolved at the row's granted
  depth (`level`); `NA` when that depth is not yet resolved to a polity.

- `level`: administrative depth granted for this row, a positive integer
  (`1L` is the container's direct subnational units).

- `item_prod_code`: WHEP production-item code (see
  [`add_item_prod_code()`](https://eduaguilera.github.io/whep/reference/add_item_prod_code.md)
  /
  [`add_item_prod_name()`](https://eduaguilera.github.io/whep/reference/add_item_prod_name.md))
  on a crop row; `NA` on a livestock row.

- `species_group`: the livestock group a livestock row counts, in the
  vocabulary
  [`build_gridded_livestock()`](https://eduaguilera.github.io/whep/reference/build_gridded_livestock.md)
  allocates in (`"cattle_dairy"`, `"pigs"`, `"sheep_goats"`, ...); `NA`
  on a crop row. A row names exactly one of `item_prod_code` and
  `species_group`.

- `indicator_used`: which indicator this row's `value` anchors to, one
  of `"area_harvested"`, `"area_planted_or_sown"`, `"area_main"`,
  `"area_cultivated"`, `"production"`, `"yield"` for a crop row, or
  `"head_count"` (live animals, head) for a livestock row.

- `year`: calendar year of the observation.

- `value`: the unit's own reported value for `indicator_used`, in the
  source's native unit. `NA` on a row whose source is consented to ship
  shares only – see *Shares-only rows* below. Never invented.

- `share`: the unit's own value divided by the admin sum across sibling
  units for the same
  `(area_code, level, item_prod_code, indicator_used, year)`, taken by
  the row's **own producer** over its own units. `NA` where the producer
  ships values and that sum has not been taken. This package does not
  fill it in at load: a share derived here from the values in the same
  table would make the seam gate's value-versus-share identity
  (`R/admin_shares_gate.R`, tier A) true by construction, and an
  identity that cannot fail detects nothing.

- `source`: dataset label of the row's producer, e.g. `"USDA_NASS"`,
  `"Eurostat_apro_cpshr"`, `"Eurostat_apro_cpnhr_h"`,
  `"Eurostat_apro_mt_ls_r"`, `"Eurostat_ef_lsk_poultry"`, `"IBGE_PAM"`,
  `"IBGE_PPM"`, `"JRC_subnational_crops"`, or a tier-2/3
  admin-statistics family label. Documented here, **not** enforced as a
  closed vocabulary: later tiers add sources this list cannot enumerate
  in advance, unlike `indicator_used`, `grain` and `treatment_year`,
  which the contract does close. A caller may therefore put any label
  here, and
  [`resolve_admin_shares()`](https://eduaguilera.github.io/whep/reference/resolve_admin_shares.md)
  will rank it. Where the vocabulary *is* closed is at the pin boundary:
  every source in the assembled `admin-shares` pin must be declared
  before
  [`read_admin_shares()`](https://eduaguilera.github.io/whep/reference/read_admin_shares.md)
  will hand it over, because that artifact carries sources whose values
  are withheld by a publication consent and an undeclared label is
  indistinguishable from a new one.

- `tier`: data tier of the source, `1L`-`3L`.

- `grain`: the reporting geography's fineness, one of `"admin1"`,
  `"admin2"`, `"admin3"`, in that ascending order. Stored as `character`
  rather than an R `ordered` factor:
  [`check_table_schema()`](https://eduaguilera.github.io/whep/reference/check_table_schema.md)'s
  type vocabulary (`R/table_schema.R`) has no factor type, so the
  ordering is carried by the vocabulary's declared order rather than by
  the column's R class. A caller needing genuine ordered comparisons can
  do
  `factor(grain, levels = c("admin1", "admin2", "admin3"), ordered = TRUE)`.

- `concept_break`: whether the item concept changed where this row's
  source or grain took over from another.

- `nuts_version`: the NUTS nomenclature version the row's geography was
  coded under, `NA` outside NUTS geographies.

- `source_native_id`: the row's identifier exactly as the reader found
  it in the source; `NA` for a row with no native identifier, e.g. one
  derived rather than read (such as a residual).

- `source_native_name`: the row's name exactly as the reader found it,
  diagnostic only – never joined or matched on.

- `source_id`: the producer's immutable identifier, in
  [`row_evidence()`](https://eduaguilera.github.io/whep/reference/row_evidence.md)'s
  vocabulary (`R/row_evidence.R`) – the same value space as `source`, so
  an admin-shares table can be handed to the row-evidence family without
  renaming.

- `source_version`: version or vintage of that source, `NA` when the
  producer has none, exactly as
  [`row_evidence()`](https://eduaguilera.github.io/whep/reference/row_evidence.md)
  documents it.

- `recorded_at`: when the row was recorded, as an ISO 8601 UTC string
  (`"2026-01-01T00:00:00Z"`), the same stamp shape
  [`row_evidence()`](https://eduaguilera.github.io/whep/reference/row_evidence.md)
  writes.

- `treatment_year`: how this row's year was obtained, one of
  `"observed"`, `"interpolated"`, `"carried"`. Reserved for the gap
  rule; this contract only names the vocabulary.

- `treatment_value`: how the row's value was obtained, `"observed"`
  (reported by the unit) or `"reconstructed"` (derived by the source,
  e.g. from a national total). `NA` when the source does not say, which
  is every source today; it is never defaulted to `"observed"`.

- `value_flag`: a free-text data-quality flag, `NA` when the row is
  clean.

## Shares-only rows

A row may carry `share` and no `value`. That is not a defect, not a
missing observation and not a gap for a later step to fill: it is what a
publication consent produces, and the contract admits it on purpose.

The Latin American subnational panel of Infante-Amate, Urrego-Mesa,
Badia-Miro and Aguilera ships to WHEP as **derived shares only** –
875,514 rows over 142 first-level units of Argentina, Bolivia, Brazil,
Chile, Colombia and Mexico – under the co-author agreement of 2026-09-02
recorded in `inst/extdata/admin_stats_pins_manifest.csv`. Its source
values are withheld until that panel's own publication, so WHEP may
carry each unit's share of its container's total and nothing else. Five
of those six countries have no other subnational evidence in this
package. Demanding a `value` would therefore not have improved the data:
it would have dropped five countries out of the subnational constraint
while every balance and conservation check still passed.

**A synthetic value is forbidden**, in both the forms that tempt:

- `value = 0` satisfies the contract and then enters the allocation as a
  reported area of zero – a claim the source never made, and one that a
  downstream reader cannot tell from a real zero.

- a value back-computed as `share * national total` satisfies it too,
  and additionally reconstructs the quantity the consent withheld.

Neither is acceptable, and finding either in code is a defect to report
rather than a shortcut to reuse. A shares-only row travels as a
shares-only row:
[`resolve_admin_shares()`](https://eduaguilera.github.io/whep/reference/resolve_admin_shares.md)
ranks candidates on indicator, grain, tier and run length, none of which
reads `value`, and
[`allocate_level_crops()`](https://eduaguilera.github.io/whep/reference/allocate_level_crops.md)
(`R/spatialize_levels.R`) carries a `"share_normalised"` denominator for
exactly this case.

What the contract does still refuse is a row carrying **neither**
measurement:
[`ensure_admin_shares()`](https://eduaguilera.github.io/whep/reference/ensure_admin_shares.md)
and
[`resolve_admin_shares()`](https://eduaguilera.github.io/whep/reference/resolve_admin_shares.md)
abort on one with class `whep_error_admin_no_measure`, because such a
row constrains nothing and would enter an allocation as an invisible
abstention.

It equally refuses a **non-finite** `value` or `share`, with class
`whep_error_admin_nonfinite`. `NaN` is not a missing measurement: it is
what a 0/0 leaves behind, and since `is.na(NaN)` is `TRUE` every
`is.na(value)` branch downstream would read it as the consented
shares-only case above. `Inf` is refused with it, which no schema bound
catches either –
[`check_table_schema()`](https://eduaguilera.github.io/whep/reference/check_table_schema.md)
guards `min` and `max` with `!is.na(values)`, and `value` has no
maximum.

## Examples

``` r
admin_shares_schema()
#> $columns
#> $columns[[1]]
#> $columns[[1]]$name
#> [1] "area_code"
#> 
#> $columns[[1]]$type
#> [1] "integer"
#> 
#> $columns[[1]]$allow_missing
#> [1] FALSE
#> 
#> 
#> $columns[[2]]
#> $columns[[2]]$name
#> [1] "level_polity_code"
#> 
#> $columns[[2]]$type
#> [1] "character"
#> 
#> 
#> $columns[[3]]
#> $columns[[3]]$name
#> [1] "level"
#> 
#> $columns[[3]]$type
#> [1] "integer"
#> 
#> $columns[[3]]$allow_missing
#> [1] FALSE
#> 
#> $columns[[3]]$min
#> [1] 1
#> 
#> 
#> $columns[[4]]
#> $columns[[4]]$name
#> [1] "item_prod_code"
#> 
#> $columns[[4]]$type
#> [1] "integer"
#> 
#> 
#> $columns[[5]]
#> $columns[[5]]$name
#> [1] "species_group"
#> 
#> $columns[[5]]$type
#> [1] "character"
#> 
#> $columns[[5]]$allowed
#>  [1] "buffalo"           "camels"            "cattle_dairy"     
#>  [4] "cattle_non_dairy"  "chickens_broilers" "chickens_layers"  
#>  [7] "equines"           "other"             "pigs"             
#> [10] "poultry"           "sheep_goats"      
#> 
#> 
#> $columns[[6]]
#> $columns[[6]]$name
#> [1] "indicator_used"
#> 
#> $columns[[6]]$type
#> [1] "character"
#> 
#> $columns[[6]]$allow_missing
#> [1] FALSE
#> 
#> $columns[[6]]$allowed
#> [1] "area_harvested"       "area_planted_or_sown" "area_main"           
#> [4] "area_cultivated"      "production"           "yield"               
#> [7] "head_count"          
#> 
#> 
#> $columns[[7]]
#> $columns[[7]]$name
#> [1] "year"
#> 
#> $columns[[7]]$type
#> [1] "integer"
#> 
#> $columns[[7]]$allow_missing
#> [1] FALSE
#> 
#> 
#> $columns[[8]]
#> $columns[[8]]$name
#> [1] "value"
#> 
#> $columns[[8]]$type
#> [1] "double"
#> 
#> $columns[[8]]$min
#> [1] 0
#> 
#> 
#> $columns[[9]]
#> $columns[[9]]$name
#> [1] "share"
#> 
#> $columns[[9]]$type
#> [1] "double"
#> 
#> $columns[[9]]$min
#> [1] 0
#> 
#> $columns[[9]]$max
#> [1] 1
#> 
#> 
#> $columns[[10]]
#> $columns[[10]]$name
#> [1] "source"
#> 
#> $columns[[10]]$type
#> [1] "character"
#> 
#> $columns[[10]]$allow_missing
#> [1] FALSE
#> 
#> 
#> $columns[[11]]
#> $columns[[11]]$name
#> [1] "tier"
#> 
#> $columns[[11]]$type
#> [1] "integer"
#> 
#> $columns[[11]]$allow_missing
#> [1] FALSE
#> 
#> $columns[[11]]$min
#> [1] 1
#> 
#> $columns[[11]]$max
#> [1] 3
#> 
#> 
#> $columns[[12]]
#> $columns[[12]]$name
#> [1] "grain"
#> 
#> $columns[[12]]$type
#> [1] "character"
#> 
#> $columns[[12]]$allow_missing
#> [1] FALSE
#> 
#> $columns[[12]]$allowed
#> [1] "admin1" "admin2" "admin3"
#> 
#> 
#> $columns[[13]]
#> $columns[[13]]$name
#> [1] "concept_break"
#> 
#> $columns[[13]]$type
#> [1] "logical"
#> 
#> $columns[[13]]$allow_missing
#> [1] FALSE
#> 
#> 
#> $columns[[14]]
#> $columns[[14]]$name
#> [1] "nuts_version"
#> 
#> $columns[[14]]$type
#> [1] "character"
#> 
#> 
#> $columns[[15]]
#> $columns[[15]]$name
#> [1] "source_native_id"
#> 
#> $columns[[15]]$type
#> [1] "character"
#> 
#> 
#> $columns[[16]]
#> $columns[[16]]$name
#> [1] "source_native_name"
#> 
#> $columns[[16]]$type
#> [1] "character"
#> 
#> 
#> $columns[[17]]
#> $columns[[17]]$name
#> [1] "source_id"
#> 
#> $columns[[17]]$type
#> [1] "character"
#> 
#> $columns[[17]]$allow_missing
#> [1] FALSE
#> 
#> 
#> $columns[[18]]
#> $columns[[18]]$name
#> [1] "source_version"
#> 
#> $columns[[18]]$type
#> [1] "character"
#> 
#> 
#> $columns[[19]]
#> $columns[[19]]$name
#> [1] "recorded_at"
#> 
#> $columns[[19]]$type
#> [1] "character"
#> 
#> $columns[[19]]$allow_missing
#> [1] FALSE
#> 
#> 
#> $columns[[20]]
#> $columns[[20]]$name
#> [1] "treatment_year"
#> 
#> $columns[[20]]$type
#> [1] "character"
#> 
#> $columns[[20]]$allow_missing
#> [1] FALSE
#> 
#> $columns[[20]]$allowed
#> [1] "observed"     "interpolated" "carried"     
#> 
#> 
#> $columns[[21]]
#> $columns[[21]]$name
#> [1] "treatment_value"
#> 
#> $columns[[21]]$type
#> [1] "character"
#> 
#> $columns[[21]]$allowed
#> [1] "observed"      "reconstructed"
#> 
#> 
#> $columns[[22]]
#> $columns[[22]]$name
#> [1] "value_flag"
#> 
#> $columns[[22]]$type
#> [1] "character"
#> 
#> 
#> 
#> $key
#> [1] "area_code"         "level_polity_code" "level"            
#> [4] "item_prod_code"    "species_group"     "indicator_used"   
#> [7] "year"             
#> 
#> $extra_columns
#> [1] "forbid"
#> 

# A schema-conformant table: two sibling units of one container.
rows <- tibble::tibble(
  area_code = c(840L, 840L),
  level_polity_code = c("USA-IOWA", "USA-ILLINOIS"),
  level = c(1L, 1L),
  item_prod_code = c(44L, 44L),
  indicator_used = c("area_harvested", "area_harvested"),
  year = c(2020L, 2020L),
  value = c(1000000, 800000),
  share = c(0.42, 0.34),
  source = c("USDA_NASS", "USDA_NASS"),
  tier = c(1L, 1L),
  grain = c("admin1", "admin1"),
  concept_break = c(FALSE, FALSE),
  nuts_version = NA_character_,
  source_native_id = c("19", "17"),
  source_native_name = c("Iowa", "Illinois"),
  source_id = c("USDA_NASS", "USDA_NASS"),
  source_version = c("2021-05", "2021-05"),
  recorded_at = "2026-01-01T00:00:00Z",
  treatment_year = c("observed", "observed"),
  value_flag = NA_character_
)
nrow(check_table_schema(rows, admin_shares_schema()))
#> [1] 2

# A shares-only pair, as a consented source ships it: `share` present,
# `value` absent, and the contract satisfied.
consented <- rows |>
  dplyr::mutate(
    value = NA_real_,
    share = c(0.79, 0.21),
    source = "admin-stats-latam",
    source_id = "admin-stats-latam",
    tier = 3L
  )
nrow(check_table_schema(consented, admin_shares_schema()))
#> [1] 2
```
