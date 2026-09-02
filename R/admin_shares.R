#' The admin-shares table contract
#'
#' @description
#' Declare, as data, the shared row shape every admin-shares reader emits
#' and every admin-shares consumer reads: one subnational administrative
#' unit's reported value and its share of the FAOSTAT container's total,
#' for one `(item_prod_code, indicator_used, year)`. Readers emit
#' source-native identifiers and never resolve names to WHEP codes here;
#' a later resolution step reconciles those once. Unresolved rows keep
#' `level_polity_code == NA` visibly rather than being dropped.
#'
#' `admin_shares_schema()` is the [check_table_schema()] contract itself;
#' [admin_shares_prototype()] is the zero-row tibble it implies, and the
#' two cannot drift apart because the prototype is built *from* the schema
#' with [empty_table_from_schema()].
#'
#' @section Admin-shares table:
#' One row per `(area_code, level_polity_code, level, item_prod_code,
#' indicator_used, year)`:
#'
#' - `area_code`: FAOSTAT-style area code of the *container* -- the polity
#'   the subnational units sum into, not the unit itself.
#' - `level_polity_code`: the polity code resolved at the row's granted
#'   depth (`level`); `NA` when that depth is not yet resolved to a
#'   polity.
#' - `level`: administrative depth granted for this row, a positive
#'   integer (`1L` is the container's direct subnational units).
#' - `item_prod_code`: WHEP production-item code (see `add_item_prod_code()`
#'   / `add_item_prod_name()`).
#' - `indicator_used`: which FAOSTAT-style indicator this row's `value`
#'   anchors to, one of `"area_harvested"`, `"area_planted_or_sown"`,
#'   `"area_main"`, `"area_cultivated"`, `"production"`, `"yield"`.
#' - `year`: calendar year of the observation.
#' - `value`: the unit's own reported value for `indicator_used`, in the
#'   source's native unit.
#' - `share`: `value` divided by the admin sum across sibling units for
#'   the same `(area_code, level, item_prod_code, indicator_used, year)`;
#'   `NA` before that sum is available.
#' - `source`: dataset label of the row's producer, e.g. `"USDA_NASS"`,
#'   `"Eurostat_apro_cpshr"`, `"Eurostat_apro_cpnhr_h"`,
#'   `"Eurostat_apro_mt_ls_r"`, `"Eurostat_ef_lsk_poultry"`, `"IBGE_PAM"`,
#'   `"IBGE_PPM"`, `"JRC_1975_2020"`, or a tier-2/3 admin-statistics family
#'   label. Documented here, not enforced as a closed vocabulary: later
#'   tiers add sources this list cannot enumerate in advance, unlike
#'   `indicator_used`, `grain` and `treatment_year`, which the contract
#'   does close.
#' - `tier`: data tier of the source, `1L`-`3L`.
#' - `grain`: the reporting geography's fineness, one of `"admin1"`,
#'   `"admin2"`, `"admin3"`, in that ascending order. Stored as
#'   `character` rather than an R `ordered` factor:
#'   [check_table_schema()]'s type vocabulary (`R/table_schema.R`) has no
#'   factor type, so the ordering is carried by the vocabulary's declared
#'   order rather than by the column's R class. A caller needing genuine
#'   ordered comparisons can do `factor(grain, levels = c("admin1",
#'   "admin2", "admin3"), ordered = TRUE)`.
#' - `concept_break`: whether the item concept changed where this row's
#'   source or grain took over from another.
#' - `nuts_version`: the NUTS nomenclature version the row's geography was
#'   coded under, `NA` outside NUTS geographies.
#' - `source_native_id`: the row's identifier exactly as the reader found
#'   it in the source; `NA` for a row with no native identifier, e.g. one
#'   derived rather than read (such as a residual).
#' - `source_native_name`: the row's name exactly as the reader found it,
#'   diagnostic only -- never joined or matched on.
#' - `source_id`: the producer's immutable identifier, in
#'   [row_evidence()]'s vocabulary (`R/row_evidence.R`) -- the same value
#'   space as `source`, so an admin-shares table can be handed to the
#'   row-evidence family without renaming.
#' - `source_version`: version or vintage of that source, `NA` when the
#'   producer has none, exactly as [row_evidence()] documents it.
#' - `recorded_at`: when the row was recorded, as an ISO 8601 UTC string
#'   (`"2026-01-01T00:00:00Z"`), the same stamp shape [row_evidence()]
#'   writes.
#' - `treatment_year`: how this row's year was obtained, one of
#'   `"observed"`, `"interpolated"`, `"carried"`. Reserved for the gap
#'   rule; this contract only names the vocabulary.
#' - `value_flag`: a free-text data-quality flag, `NA` when the row is
#'   clean.
#'
#' @inheritSection whep_polity_columns Polity columns
#'
#' `area_code` and `level_polity_code` above follow that same
#' two-code-space convention at admin-shares' own granularity: `area_code`
#' is the container's key exactly as described there, and
#' `level_polity_code` plays the role `reporting_polity_code` plays there,
#' naming the polity resolved at the row's granted depth (`level`) rather
#' than at a fixed year-resolved depth.
#'
#' @return A schema list, as documented in [check_table_schema()]: closed
#'   (`extra_columns = "forbid"`), keyed on `(area_code, level_polity_code,
#'   level, item_prod_code, indicator_used, year)`.
#'
#' @export
#'
#' @examples
#' admin_shares_schema()
#'
#' # A schema-conformant table: two sibling units of one container.
#' rows <- tibble::tibble(
#'   area_code = c(840L, 840L),
#'   level_polity_code = c("USA-IOWA", "USA-ILLINOIS"),
#'   level = c(1L, 1L),
#'   item_prod_code = c(44L, 44L),
#'   indicator_used = c("area_harvested", "area_harvested"),
#'   year = c(2020L, 2020L),
#'   value = c(1000000, 800000),
#'   share = c(0.42, 0.34),
#'   source = c("USDA_NASS", "USDA_NASS"),
#'   tier = c(1L, 1L),
#'   grain = c("admin1", "admin1"),
#'   concept_break = c(FALSE, FALSE),
#'   nuts_version = NA_character_,
#'   source_native_id = c("19", "17"),
#'   source_native_name = c("Iowa", "Illinois"),
#'   source_id = c("USDA_NASS", "USDA_NASS"),
#'   source_version = c("2021-05", "2021-05"),
#'   recorded_at = "2026-01-01T00:00:00Z",
#'   treatment_year = c("observed", "observed"),
#'   value_flag = NA_character_
#' )
#' nrow(check_table_schema(rows, admin_shares_schema()))
admin_shares_schema <- function() {
  list(
    columns = list(
      list(name = "area_code", type = "integer", allow_missing = FALSE),
      list(name = "level_polity_code", type = "character"),
      list(name = "level", type = "integer", allow_missing = FALSE, min = 1L),
      list(name = "item_prod_code", type = "integer", allow_missing = FALSE),
      list(
        name = "indicator_used",
        type = "character",
        allow_missing = FALSE,
        allowed = c(
          "area_harvested",
          "area_planted_or_sown",
          "area_main",
          "area_cultivated",
          "production",
          "yield"
        )
      ),
      list(name = "year", type = "integer", allow_missing = FALSE),
      list(name = "value", type = "double", allow_missing = FALSE, min = 0),
      list(name = "share", type = "double", min = 0, max = 1),
      list(name = "source", type = "character", allow_missing = FALSE),
      list(
        name = "tier",
        type = "integer",
        allow_missing = FALSE,
        min = 1L,
        max = 3L
      ),
      list(
        name = "grain",
        type = "character",
        allow_missing = FALSE,
        allowed = c("admin1", "admin2", "admin3")
      ),
      list(name = "concept_break", type = "logical", allow_missing = FALSE),
      list(name = "nuts_version", type = "character"),
      list(name = "source_native_id", type = "character"),
      list(name = "source_native_name", type = "character"),
      list(name = "source_id", type = "character", allow_missing = FALSE),
      list(name = "source_version", type = "character"),
      list(name = "recorded_at", type = "character", allow_missing = FALSE),
      list(
        name = "treatment_year",
        type = "character",
        allow_missing = FALSE,
        allowed = c("observed", "interpolated", "carried")
      ),
      list(name = "value_flag", type = "character")
    ),
    key = c(
      "area_code",
      "level_polity_code",
      "level",
      "item_prod_code",
      "indicator_used",
      "year"
    ),
    extra_columns = "forbid"
  )
}

#' The admin-shares zero-row prototype
#'
#' @description
#' The zero-row tibble [admin_shares_schema()] describes: the declared
#' columns, in the declared order, each with the declared type. Built with
#' [empty_table_from_schema()], so it passes [check_table_schema()] by
#' construction and cannot drift from the schema. See
#' [admin_shares_schema()] for the full column-by-column contract.
#'
#' @return A zero-row tibble with the admin-shares contract's columns, in
#'   contract order.
#'
#' @export
#'
#' @examples
#' admin_shares_prototype()
#' nrow(check_table_schema(admin_shares_prototype(), admin_shares_schema()))
admin_shares_prototype <- function() {
  empty_table_from_schema(admin_shares_schema())
}

#' Complete a table to the admin-shares contract
#'
#' @description
#' Complete `x` onto [admin_shares_prototype()] with [ensure_columns()] --
#' adding absent columns as typed missing values and casting present ones
#' to the contract's types -- then prove the result against
#' [admin_shares_schema()] with [assert_table_schema()], which aborts
#' naming the offending columns and values when it does not conform.
#'
#' @param x Tibble to complete. May already carry extra columns or be
#'   missing contract columns; see [ensure_columns()].
#'
#' @return `x` completed to the admin-shares contract, invisibly.
#'
#' @export
#'
#' @examples
#' # Omits only the columns the contract allows missing: `share`,
#' # `nuts_version`, `source_native_id`, `source_native_name`,
#' # `source_version` and `value_flag`. `level_polity_code` is part of
#' # the key, so it must stay present and distinct per row even though the
#' # contract allows it to be `NA` for a genuinely unresolved row.
#' partial <- tibble::tibble(
#'   area_code = c(840L, 840L),
#'   level_polity_code = c("USA-IOWA", "USA-ILLINOIS"),
#'   level = c(1L, 1L),
#'   item_prod_code = c(44L, 44L),
#'   indicator_used = c("area_harvested", "area_harvested"),
#'   year = c(2020L, 2020L),
#'   value = c(1000000, 800000),
#'   source = c("USDA_NASS", "USDA_NASS"),
#'   tier = c(1L, 1L),
#'   grain = c("admin1", "admin1"),
#'   concept_break = c(FALSE, FALSE),
#'   source_id = c("USDA_NASS", "USDA_NASS"),
#'   recorded_at = "2026-01-01T00:00:00Z",
#'   treatment_year = c("observed", "observed")
#' )
#' completed <- ensure_admin_shares(partial)
#' names(completed)
#' nrow(check_table_schema(completed, admin_shares_schema()))
ensure_admin_shares <- function(x) {
  completed <- ensure_columns(x, admin_shares_prototype())
  assert_table_schema(completed, admin_shares_schema(), arg = "x")
}
