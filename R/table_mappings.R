#' Commodity balance sheet items
#'
#' Defines name/code correspondences for commodity balance sheet (CBS) items.
#'
#' @format
#' A tibble where each row corresponds to one CBS item.
#' It contains the following columns:
#' - `item_cbs_code`: A numeric code used to refer to the CBS item.
#' - `item_cbs_name`: A natural language name for the item.
#' - `item_type`: An ad-hoc grouping of items. This is a work in progress
#'   evolving depending on our needs, so for now it only has two possible
#'   values:
#'   - `livestock`: The CBS item represents a live animal.
#'   - `other`: Not any of the previous groups.
#' @source Inspired by [FAOSTAT data](https://www.fao.org/faostat/en/#data/FBS).
"items_cbs"

#' Primary production items
#'
#' Defines name/code correspondences for production items.
#'
#' @format
#' A tibble where each row corresponds to one production item.
#' It contains the following columns:
#' - `item_prod_code`: A numeric code used to refer to the item.
#' - `item_prod_name`: A natural language name for the item.
#' - `item_type`: An ad-hoc grouping of items. This is a work in progress
#'   evolving depending on our needs, so for now it only has two possible
#'   values:
#'   - `crop_product`: The CBS item represents a crop product.
#'   - `other`: Not any of the previous groups.
#' @source Inspired by [FAOSTAT data](https://www.fao.org/faostat/en/#data/QCL).
"items_prod"

#' Polities
#'
#' Periodized WHEP polity database imported from the `whep-polities`
#' repository.
#'
#' @format
#' An sf data frame where each row corresponds to one territorial polity over
#' a continuous time interval. Key columns include:
#' - `polity_code`: Stable WHEP polity identifier, usually
#'   `PREFIX-start_year-end_year`.
#' - `polity_name`: Human-readable polity name.
#' - `start_year`, `end_year`: Validity interval for the row. `start_year` is
#'   inclusive; `end_year` is **exclusive at a succession** and **inclusive at
#'   the open end**. Exclusive at a succession, so `F51-1947-1993`
#'   Czechoslovakia covers 1947-1992 and 1993 belongs to `CZE-1993-2025` and
#'   `SVK-1993-2025`, and 2014 belongs to `"RUS-2014-2025"` rather than to
#'   `"RUS-1991-2014"` (filtering `year <= end_year` returns both epochs and
#'   double-counts every boundary year). Inclusive at the open end, so 2025
#'   still belongs to `"RUS-2014-2025"`, because no later interval of that
#'   polity follows it and a uniformly exclusive read would leave the current
#'   year with no polity at all. **Openness is absence of a successor, not a
#'   comparison against the last year in the table.** 257 live polities have no
#'   successor, 244 of them end in 2025, and no live polity ending in 2025
#'   carries one, so the two readings agree on this vintage; the successor test
#'   is the one that keeps agreeing when the horizon moves. The distinction is
#'   not decoration: 254 of the 779 rows end in 2025 but only 251 are open,
#'   because three are succeeded there (`BLZ-1800-2025`, `CAN-1948-2025` and
#'   `IRQ-1921-2025`, all `retired` or `superseded`), and opening those too
#'   would count the terminal year twice.
#' - `iso3_code`, `iso3c`: ISO3 code where one exists. `iso3c` is retained as
#'   a compatibility alias.
#' - `wiki_status`: Upstream review state. `"retired"` and `"superseded"` mark a
#'   DEAD row, kept so a code already held in older output stays resolvable, but
#'   never a resolution target: [polity_area_crosswalk] excludes them, so resolve
#'   through it rather than through this table.
#' - `polygon_status`: Polygon status in `whep-polities` (`"assigned"`,
#'   `"proxy"`, `"missing"`, or `"excluded"`).
#' - `polygon_feature_year`, `polygon_feature_date`: The vintage of the source
#'   feature the polygon was taken from. `polygon_feature_date` arrived with the
#'   #835 upstream re-sync and is populated on 3 rows, where upstream recorded a
#'   day rather than only a year.
#' - `has_geometry`: Logical flag indicating whether the geometry is non-empty.
#' - `geom`: Multipolygon geometry.
#' @source `~/whep-polities/data/final/polities_database.gpkg`.
"polities"

#' FAOSTAT/FABIO area-to-polity crosswalk
#'
#' Year-aware bridge from numeric reporting `area_code` values used by
#' FAOSTAT/FABIO-derived WHEP data to periodized WHEP `polity_code` values.
#'
#' @format
#' A tibble with one row per area-code/polity-period mapping. Key columns:
#' - `area_code`: Numeric FAOSTAT/FABIO reporting area code.
#' - `area_name`: Reporting area name.
#' - `area_iso3c`: Reporting-area ISO3-like code where available.
#' - `legacy_polity_prefix`, `legacy_polity_name`: The ISO3-like stem (`"ARM"`,
#'   `"ROCE"`, `"REUR"`) and legacy label this package vendors from
#'   `regions_full.csv`, kept for older callers and used at build time only as a
#'   *candidate prefix* for polity inference. **Neither is an identity**: not one
#'   stem is a [polities] `polity_code`, so a join to [polities] on it comes back
#'   empty. Read `polity_code` for the polity this row resolves to. Until #711
#'   the pair shipped as `reporting_polity_code`/`reporting_polity_name`, the
#'   package's own names for a real periodized polity (see
#'   [whep_polity_columns]), which is exactly the trap #687 removed from
#'   [regions_full].
#' - `polity_area_code`: Numeric area code retained for WHEP matrix workflows.
#' - `polity_code`, `polity_name`: Matched WHEP polity, or `NA` for
#'   statistical composites that are not real polities.
#' - `polity_start_year`, `polity_end_year`: Validity interval for the matched
#'   polity, on the same convention as [polities]: `polity_end_year` is
#'   exclusive at a succession, so the period covers
#'   `polity_start_year:(polity_end_year - 1)`, and inclusive at the open end,
#'   so an interval nothing succeeds covers its own terminal year.
#'   [add_polity_code()] resolves a year on that reading, widened further to the
#'   inclusive `map_year_end` below where the upstream map declares a reported
#'   year past the territorial span.
#' - `mapping_source`: How the area-to-polity decision was reached.
#'   `"upstream_map"` for the published `whep-polities` FAOSTAT area map, which
#'   is the authority for the years FAOSTAT reports; `"prefix_outside_map"` for a
#'   period of a mapped area lying outside every span the map declares, kept so
#'   sources reported under their own historical borders still resolve;
#'   `"fabio_row_fold"` where FABIO collapses the area into its Rest-of-World
#'   bucket; `"prefix_fallback"` where the map covers the area not at all and the
#'   mapping is inferred from the polity-code prefix.
#' - `map_year_start`, `map_year_end`: Inclusive reporting years the upstream map
#'   assigns to this area-polity pair, `NA` unless `mapping_source` is
#'   `"upstream_map"`.
#' - `map_match_route`: Upstream's record of how it decided the row
#'   (`"iso-equal"`, `"registry"`, `"manual-route"`, `"manual-replace"`,
#'   `"manual-span"`), `NA` unless `mapping_source` is `"upstream_map"`.
#' - `mapping_status`: Whether a polity was found, **not** how much to trust it.
#'   `"matched"` when a live polity resolved; `"manual"` when the decision was
#'   curated by hand, either by upstream (a `manual-*` `map_match_route`) or by
#'   this package's own area-prefix overrides; `"unmapped"` when no real polity
#'   is available, which is FAOSTAT area 351 "China" alone, deliberately left
#'   unmapped so it cannot double-count its own components; and
#'   `"not_a_reporting_area"` for a territory that carries no FAOSTAT/FABIO area
#'   at all, which has `NA` in both `area_code` and `polity_area_code` and so
#'   can never be joined to reported data.
#' - `mapping_note`: Explanation for manual, unmapped and non-reporting rows.
#'
#' @section Keying on `(area_code, year)` is keying on the polity:
#' This is a **contract**, asserted by the test suite over the full reporting
#' era, not an accident of the current snapshot: every `(area_code, year)` that
#' resolves at all resolves to exactly **one** `polity_code`, over 17,184
#' resolving pairs and 306 polities. A join that
#' carries both columns is therefore already polity-correct, whether or not it
#' names a polity, and the territorial identity is recoverable from the numeric
#' code rather than lost by it.
#'
#' The one enumerated exception is FAOSTAT area 7 (Angola) in 1975, where
#' `ANG-1905-1975` records no successor upstream and so is widened by a year
#' into `AGO-1975-2025`'s first year. Resolution still returns `AGO-1975-2025`
#' there, but by row order rather than by the data. See #683.
#'
#' @section `polity_area_code` is a bucket, and does not carry the contract:
#' `polity_area_code` is the key rows are **aggregated on** for the matrix
#' workflows, and several `area_code` values can share one. Where a bucket has
#' one member, or its members agree, `(polity_area_code, year)` recovers the
#' polity too. Over 1961-2025 exactly one bucket does not: **206**, which holds
#' Sudan (former) 206, Sudan 276 and South Sudan 277 and answers with three
#' polities in all 65 reported years. See #414. To say which territory a row
#' belongs to, read this table's own `polity_code`, or resolve through
#' [add_polity_code()]; do not infer it from the bucket. In a WHEP *output* the
#' same answer is `reporting_polity_code`, materialised from this table's
#' `polity_code`. This table carries no column of that name, and
#' `legacy_polity_prefix` is not a substitute for one (#711).
#'
#' @section Confidence is the pair, not `mapping_status` alone:
#' `"matched"` covers outcomes of very different confidence -- a curated hit in
#' upstream's published FAOSTAT map, a prefix-inferred period outside every span
#' that map declares, a prefix guess for an area the map does not cover, and the
#' FABIO Rest-of-World fold. **`mapping_source` is the column that separates
#' them**, and it is non-`NA` on every row, so read the two together rather than
#' filtering on `mapping_status == "matched"` and assuming a curated decision:
#'
#' ```r
#' dplyr::count(polity_area_crosswalk, mapping_status, mapping_source)
#' ```
#'
#' Curated rows are `mapping_source == "upstream_map"`; everything a prefix
#' decided is `"prefix_outside_map"` or `"prefix_fallback"`. Prefix inference
#' never overrides the map where the map speaks: a prefix-derived period whose
#' years overlap any span the map declares for that area is dropped at build
#' time, so the two branches cannot disagree about a reported year.
#'
#' @section The row space is reporting areas, not polities:
#' This table is **not an index of [polities]**, and reading a missing
#' `polity_code` as a coverage gap is the mistake #875 records. The builder
#' starts from the reporting areas of `regions_full.csv` and asks which polity
#' each one names; a polity therefore has a row **only if some reporting area
#' names it**. 176 of the 735 live polities have none, because no FAOSTAT or
#' FABIO area was ever reported under their territory -- 121 national, 33
#' colonial, 8 aggregate and the rest smaller units. The 559 that remain are
#' exactly the distinct non-`NA` `polity_code` values here.
#'
#' Absence is structural rather than accidental, and the test suite asserts it:
#' not one of the absent aggregate polities carries a `legacy_polity_prefix`
#' any reporting area carries, so the row space has no slot to put them in.
#' A polity absent from here is reached by the **label** route instead --
#' [resolve_polity_label()] over [polity_label_aliases], which maps the label a
#' historical source writes straight to a polity and needs no area code. That
#' is how five of the eight absent aggregates resolve --
#' `GCT-1919-1956` Gold Coast and British Togoland, `MASG-1946-1963`,
#' `SYL-1944-1953` (all scoped to the `fao1952` yearbook),
#' `PAPNG-1920-1949` (`mitchell`) and `AOI-1936-1941` (any source) -- all of
#' them pre-FAOSTAT combined reporting units that never had an area code.
#' Two more (`EGYSUD-1934-1956`, `CODRU-1922-1960`) are upstream
#' composed-union identities held for footnote series that fold a colony into
#' its metropole, and upstream has registered no label for them yet, so they
#' are reachable from neither route by design rather than by omission.
#'
#' No absence here can NA an `area_code` on a polycell, either:
#' [build_polycell_support()] excludes `polity_type == "aggregate"` by type
#' before any lookup runs, so an aggregate emits no polycell to carry one.
#'
#' The one absence that is a real gap is `F206-2011-2025`, Sudan and South
#' Sudan combined, and **it is a gap in the bucket vocabulary rather than in
#' `area_code`**. FAOSTAT stops reporting area 206 in 2011 and does not
#' resume, so upstream deliberately adds no map row for it; the post-2011
#' combination is WHEP's own fold of areas 276 and 277 into
#' `polity_area_code` 206. This table answers per reporting area and has no
#' column in which to say "bucket 206 from 2012 answers as `F206-2011-2025`",
#' which is why [polity_bucket_coverage()] still labels that bucket
#' `SUD-1956-2011` / `"out_of_span"` for 2012 onward -- a polity that ended in
#' 2011. Expressing it is #742 and labelling it is #860; neither is decided
#' here.
#' @source Derived from [polities],
#'   `~/whep-polities/data/final/faostat_area_polity_map.csv` and
#'   `inst/extdata/harmonization/regions_full.csv`.
"polity_area_crosswalk"

#' Source label to polity aliases
#'
#' Published map from the country/area **labels** a source writes to periodized
#' WHEP `polity_code` values, consumed by [resolve_polity_label()]. It is the
#' label-keyed counterpart of [polity_area_crosswalk], which is keyed by numeric
#' reporting area code.
#'
#' The map is authored and gated in `whep-polities`; this package embeds a copy
#' rather than deciding label identity itself, so that a label's meaning has one
#' authority.
#'
#' @format
#' A tibble with one row per alias. Columns:
#' - `source_label`: The label exactly as the source writes it.
#' - `source`: Source slug the alias is scoped to, or `NA` when it applies to
#'   any source.
#' - `year_start`, `year_end`: Year range the alias is scoped to. A missing bound
#'   is unbounded on that side; both missing means the alias is not year-scoped.
#' - `polity_code`: The WHEP polity the label resolves to.
#' - `common_name`: Human-readable name of that polity.
#' - `confidence`: Curator's confidence in the alias.
#' - `observed_rows`: Source rows actually observed for the label, `NA` when the
#'   label is merely mappable.
#' @source `~/whep-polities/data/final/label_alias_map.csv`.
"polity_label_aliases"
