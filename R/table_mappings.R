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
#' a continuous time interval. Every column is described below:
#' - `polity_code`: Stable WHEP polity identifier, usually
#'   `PREFIX-start_year-end_year`.
#' - `polity_name`: Human-readable polity name.
#' - `start_year`, `end_year`: Validity years. `start_year` is inclusive and
#'   `end_year` is **exclusive** — area 185 in 2014 resolves to `RUS-2014-2025`,
#'   not `RUS-1991-2014`. Filtering with `year <= end_year` double-counts every
#'   boundary year. This entry said "Inclusive" until 0.3.0.9000, which was the
#'   opposite of the behaviour.
#' - `iso3_code`, `iso3c`: ISO3 code where one exists. `iso3c` is retained as
#'   a compatibility alias. Not always an ISO 3166-1 code: historical entities
#'   with no assignment carry a WHEP-internal key, and some values are withdrawn
#'   ISO codes (`CSK`, `SUN`, `YUG`, `SCG`) that were real when the data was.
#' - `polygon_status`: One of `"assigned"`, `"proxy"`, `"estimate"`,
#'   `"polygon_vintage_drift"` or `"unassigned"`. Only `"unassigned"` asserts
#'   that no polygon exists. The list here read `"missing"` and `"excluded"`
#'   until 0.3.0.9000 — two values the vocabulary no longer has, so a filter on
#'   either returned nothing rather than erring.
#' - `polygon_area_km2`: Area as RECORDED upstream from an independent source,
#'   for cross-checking the attached geometry. Sparse by nature — present for
#'   182 of 740 rows — so compute area from `geom` rather than relying on it.
#' - `has_geometry`: Logical flag indicating whether the geometry is non-empty.
#' - `wiki_status`: **The column a consumer must filter on.** One of `"draft"`,
#'   `"reviewed"`, `"retired"`, `"superseded"`. The last two mean the row MUST
#'   NEVER RECEIVE DATA — upstream publishes them as `dead_status` in its manifest
#'   and lists the codes in `dead_polity_codes`. 27 of 740 rows are dead, and they
#'   are not obviously distinguishable otherwise: retired duplicates carry the same
#'   name, iso3 and often a valid geometry as their live successor, which is how
#'   `ARG-1800-2025` and `BRA-1800-2025` sat alongside `ARG-1902-2025` and
#'   `BRA-1909-2025` spanning identical years.
#' - `polity_type`: One of eight values including `"national"`, `"colonial"`,
#'   `"subnational"` and `"aggregate"`. `"aggregate"` marks rest-of-world and the
#'   continental "Other" buckets, which are not territories: filtering them out is
#'   how you get a list of real polities.
#' - `continent`: Seven values. Coarser than some region taxonomies and finer than
#'   others — `gleam_geographic_hierarchy` says `"Americas"` where this says
#'   `"North America"` or `"South America"`, which is a granularity difference and
#'   not a disagreement.
#' - `cow_code`: Correlates of War state number where one applies, for 523 of 740
#'   rows. Not unique over overlapping years: 29 pairs share a code by design,
#'   because COW numbers a state while this table periodizes territory.
#' - `predecessor`, `successor`: Semicolon-separated polity codes, present for 414
#'   and 489 rows. A dissolution lists several successors in one field
#'   (`AEF-1910-1960` names four), so split on `"; "` rather than treating either
#'   as a single code.
#' - `polygon_source`, `polygon_feature_id`, `polygon_feature_year`: provenance of
#'   the attached geometry — which dataset, which feature in it, and for which
#'   vintage. `polygon_feature_id` is the field to check when a polygon looks
#'   wrong: recording it as prose rather than a resolvable value is what upstream's
#'   `polygon_gap_polity_codes` tracks.
#' - `last_ingest`: Date the wiki page was last reconciled with its sources.
#' - `geom`: Multipolygon geometry.
#'
#' This list is exhaustive as of 0.3.0.9000 — every column of the shipped table
#' appears above. It read "Key columns include" while omitting ten, among them
#' `wiki_status`, which is the one a consumer cannot afford to miss.
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
#' - `area_iso3c`: ISO3-like code of the reporting AREA, where available. Not the
#'   same as `iso3_code`, which this table also carries and which belongs to the
#'   resolved POLITY — they differ on 56 of the 273 rows that have both. An area
#'   folded into a rest-of-world polity keeps its own code while the polity's is
#'   `ROW` (`ASM` against `ROW`), and a colonial-era row carries the era's key
#'   while the area keeps the modern one (`AGO` against `ANG`, `BWA` against
#'   `BEC`). And some rows record a source's own aggregation: the reporting area is
#'   a parent state while `area_iso3c` carries a dependency's code, because the
#'   source reports that dependency under the parent — `JEY`, `GGY` and `IMN`
#'   against `GBR-*`, `ALA` against `FIN-*`, `SXM` against `NLD-*`. Those rows
#'   have no `area_code`, so nothing routes through them; they document how the
#'   source counts, not a territorial claim. Join on whichever column answers your
#'   question, but not on the assumption that they agree.
#' - `reporting_polity_prefix`: ISO3-shaped family key for the reporting area —
#'   a PREFIX, never a code. Do not join it to [polities]; use `polity_code`.
#'   It was called `reporting_polity_code` until 0.3.0.9000, where it held 609
#'   bare prefixes and no periodized codes, so that join returned nothing.
#' - `polity_area_code`: Numeric area code retained for WHEP matrix workflows.
#' - `polity_code`, `polity_name`: Matched WHEP polity, or `NA` for
#'   statistical composites that are not real polities.
#' - `polity_start_year`, `polity_end_year`: Validity interval for the matched
#'   polity.
#' - `mapping_status`: `"matched"`, `"manual"`, `"unmapped"`, or
#'   `"not_a_reporting_area"`. Together with [add_polity_code()] this
#'   distinguishes three cases a bare `is.na(polity_code)` check conflates: an
#'   area that resolved (`"matched"`/`"manual"`), one deliberately left unmapped
#'   because mapping it would double-count (`"unmapped"` — FAOSTAT 351 "China" is
#'   the example, reported alongside its own components), and an area code that
#'   does not exist at all, which yields `NA` here because no crosswalk row was
#'   found. A typo and a documented non-mapping are different problems.
#' - `mapping_note`: Explanation for manual or unmapped rows.
#' - `reporting_polity_name`: Name of the polity the area reports as. Differs from
#'   `area_name` on 72 areas, because a folded area carries its aggregate's label:
#'   Bermuda reads `"Latin America Other"`. Joining downstream on the wrong one of
#'   these silently loses rows — that defect cost 13.3% of the fodder bridge and
#'   6.4% of `gdp-population`.
#' - `cbs`: Logical. `TRUE` where the area has its own commodity balance sheet.
#'   Load-bearing beyond its own reporting: it gates which areas are unfolded from
#'   the FABIO rest-of-world bucket at the POLITY level, which is what keeps 351
#'   China and five deliberately-folded territories from unfolding. It no longer
#'   affects the numeric key — see `fabio_code` below.
#' - `fabio_code`: FABIO's numeric area, `999` for everything FABIO folds into
#'   rest-of-world — 62 areas. **Equal to `polity_area_code` on every row where
#'   both are present** (600 of 601 rows with an `area_code`; the exception is 351
#'   China, which is unmapped by design and has neither). So aggregating on either
#'   reproduces FABIO.
#'
#'   That equality is a deliberate change and reverses what this documentation used
#'   to say. Sixteen areas that FABIO folds report data of their own, and giving
#'   them their own aggregation key — so their data stops being attributed to
#'   `ROW-1850-2023` — inflates global `feed` by 13.7 times, with the whole increase
#'   landing on one area. The consequence to be aware of is that
#'   `reporting_polity_code` and `polity_area_code` disagree for those sixteen: the
#'   crosswalk names the Faroe Islands while the numbers sit in rest-of-world. See
#'   whep#419, which costs both sides of that choice.
#' - `polity_type`, `cow_code`, `continent`, `wiki_status`, `polygon_status`,
#'   `has_geometry`: carried through from [polities] for the matched polity, with
#'   the same meanings. `wiki_status` matters most: `"retired"` and `"superseded"`
#'   rows must never receive data, and a crosswalk row can point at one.
#'
#' This list is exhaustive as of 0.3.0.9000 — every column of the shipped table
#' appears above.
#' @source Derived from [polities] and `inst/extdata/harmonization/regions_full.csv`.
"polity_area_crosswalk"
