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
#' - `start_year`, `end_year`: Inclusive validity years for the row.
#' - `iso3_code`, `iso3c`: ISO3 code where one exists. `iso3c` is retained as
#'   a compatibility alias.
#' - `polygon_status`: Polygon status in `whep-polities` (`"assigned"`,
#'   `"proxy"`, `"missing"`, or `"excluded"`).
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
#' - `polity_area_code`: Numeric area code retained for WHEP matrix workflows.
#' - `polity_code`, `polity_name`: Matched WHEP polity, or `NA` for
#'   statistical composites that are not real polities.
#' - `polity_start_year`, `polity_end_year`: Validity interval for the matched
#'   polity. `polity_end_year` is exclusive.
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
#' - `mapping_status`: `"matched"`, `"manual"`, `"unmapped"`, or
#'   `"not_a_reporting_area"`.
#' - `mapping_note`: Explanation for manual or unmapped rows.
#' @source Derived from [polities],
#'   `~/whep-polities/data/final/faostat_area_polity_map.csv` and
#'   `inst/extdata/harmonization/regions_full.csv`.
"polity_area_crosswalk"
