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
#' - `start_year`, `end_year`: Half-open validity interval for the row:
#'   `start_year` is inclusive, `end_year` is **exclusive**, so the row covers
#'   `start_year:(end_year - 1)` and hands over to its successor in `end_year`
#'   (`F51-1947-1993` Czechoslovakia covers 1947-1992, and 1993 belongs to
#'   `CZE-1993-2025` and `SVK-1993-2025`). Open periods carry the vintage's
#'   horizon as `end_year`, so they too stop one year short of it.
#' - `iso3_code`, `iso3c`: ISO3 code where one exists. `iso3c` is retained as
#'   a compatibility alias.
#' - `wiki_status`: Upstream review state. `"retired"` and `"superseded"` mark a
#'   DEAD row, kept so a code already held in older output stays resolvable, but
#'   never a resolution target: [polity_area_crosswalk] excludes them, so resolve
#'   through it rather than through this table.
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
#'   polity. `polity_end_year` is exclusive, so the period covers
#'   `polity_start_year:(polity_end_year - 1)`. [add_polity_code()] resolves a
#'   year on that reading, widened to the inclusive `map_year_end` below where
#'   the upstream map declares a reported year past the territorial span.
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
