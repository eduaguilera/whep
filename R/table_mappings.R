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
#' - `area_iso3c`: ISO3-like code of the reporting AREA, where available. Not the
#'   same as `iso3_code`, which this table also carries and which belongs to the
#'   resolved POLITY — they differ on 56 of the 273 rows that have both. An area
#'   folded into a rest-of-world polity keeps its own code while the polity's is
#'   `ROW` (`ASM` against `ROW`), and a colonial-era row carries the era's key
#'   while the area keeps the modern one (`AGO` against `ANG`, `BWA` against
#'   `BEC`). A dependency with no FAOSTAT area of its own resolves to its parent
#'   state, so `JEY` sits against `GBR` and `ALA` against `FIN`. Join on whichever
#'   answers your question, but not on the assumption that they agree.
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
#'   `"not_a_reporting_area"`.
#' - `mapping_note`: Explanation for manual or unmapped rows.
#' @source Derived from [polities] and `inst/extdata/harmonization/regions_full.csv`.
"polity_area_crosswalk"
