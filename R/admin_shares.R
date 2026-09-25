# NSE globals for admin_shares.R (#1000, T33/T38): none. This file holds
# schema lists, prototypes and base-R checks only; the two `purrr::map()`
# calls take anonymous functions over their own arguments, so no verb here
# evaluates a bare column name.

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
#' There are two measurement columns, `value` and `share`, and a row must
#' carry at least one of them. `value` may be missing wherever `share` is
#' present, which is a **first-class case** created by a publication
#' consent and not a gap to be filled; see the section below before
#' writing anything that assumes an absolute value is there.
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
#'   source's native unit. `NA` on a row whose source is consented to
#'   ship shares only -- see *Shares-only rows* below. Never invented.
#' - `share`: the unit's own value divided by the admin sum across sibling
#'   units for the same `(area_code, level, item_prod_code,
#'   indicator_used, year)`, taken by the row's **own producer** over its
#'   own units. `NA` where the producer ships values and that sum has not
#'   been taken. This package does not fill it in at load: a share derived
#'   here from the values in the same table would make the seam gate's
#'   value-versus-share identity (`R/admin_shares_gate.R`, tier A) true by
#'   construction, and an identity that cannot fail detects nothing.
#' - `source`: dataset label of the row's producer, e.g. `"USDA_NASS"`,
#'   `"Eurostat_apro_cpshr"`, `"Eurostat_apro_cpnhr_h"`,
#'   `"Eurostat_apro_mt_ls_r"`, `"Eurostat_ef_lsk_poultry"`, `"IBGE_PAM"`,
#'   `"IBGE_PPM"`, `"JRC_subnational_crops"`, or a tier-2/3
#'   admin-statistics family label. Documented here, **not** enforced as a
#'   closed vocabulary: later tiers add sources this list cannot enumerate
#'   in advance, unlike `indicator_used`, `grain` and `treatment_year`,
#'   which the contract does close. A caller may therefore put any label
#'   here, and [resolve_admin_shares()] will rank it. Where the vocabulary
#'   *is* closed is at the pin boundary: every source in the assembled
#'   `admin-shares` pin must be declared before [read_admin_shares()] will
#'   hand it over, because that artifact carries sources whose values are
#'   withheld by a publication consent and an undeclared label is
#'   indistinguishable from a new one.
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
#' @section Shares-only rows:
#' A row may carry `share` and no `value`. That is not a defect, not a
#' missing observation and not a gap for a later step to fill: it is what
#' a publication consent produces, and the contract admits it on purpose.
#'
#' The Latin American subnational panel of Infante-Amate, Urrego-Mesa,
#' Badia-Miro and Aguilera ships to WHEP as **derived shares only** --
#' 875,514 rows over 142 first-level units of Argentina, Bolivia, Brazil,
#' Chile, Colombia and Mexico -- under the co-author agreement of
#' 2026-09-02 recorded in `inst/extdata/admin_stats_pins_manifest.csv`.
#' Its source values are withheld until that panel's own publication, so
#' WHEP may carry each unit's share of its container's total and nothing
#' else. Five of those six countries have no other subnational evidence in
#' this package. Demanding a `value` would therefore not have improved the
#' data: it would have dropped five countries out of the subnational
#' constraint while every balance and conservation check still passed.
#'
#' **A synthetic value is forbidden**, in both the forms that tempt:
#'
#' - `value = 0` satisfies the contract and then enters the allocation as
#'   a reported area of zero -- a claim the source never made, and one
#'   that a downstream reader cannot tell from a real zero.
#' - a value back-computed as `share * national total` satisfies it too,
#'   and additionally reconstructs the quantity the consent withheld.
#'
#' Neither is acceptable, and finding either in code is a defect to report
#' rather than a shortcut to reuse. A shares-only row travels as a
#' shares-only row: [resolve_admin_shares()] ranks candidates on
#' indicator, grain, tier and run length, none of which reads `value`, and
#' `allocate_level_crops()` (`R/spatialize_levels.R`) carries a
#' `"share_normalised"` denominator for exactly this case.
#'
#' What the contract does still refuse is a row carrying **neither**
#' measurement: [ensure_admin_shares()] and [resolve_admin_shares()] abort
#' on one with class `whep_error_admin_no_measure`, because such a row
#' constrains nothing and would enter an allocation as an invisible
#' abstention.
#'
#' It equally refuses a **non-finite** `value` or `share`, with class
#' `whep_error_admin_nonfinite`. `NaN` is not a missing measurement: it is
#' what a 0/0 leaves behind, and since `is.na(NaN)` is `TRUE` every
#' `is.na(value)` branch downstream would read it as the consented
#' shares-only case above. `Inf` is refused with it, which no schema bound
#' catches either -- [check_table_schema()] guards `min` and `max` with
#' `!is.na(values)`, and `value` has no maximum.
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
#'
#' # A shares-only pair, as a consented source ships it: `share` present,
#' # `value` absent, and the contract satisfied.
#' consented <- rows |>
#'   dplyr::mutate(
#'     value = NA_real_,
#'     share = c(0.79, 0.21),
#'     source = "admin-stats-latam",
#'     source_id = "admin-stats-latam",
#'     tier = 3L
#'   )
#' nrow(check_table_schema(consented, admin_shares_schema()))
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
      # `value` is allow-missing on purpose: a source consented to ship
      # derived shares only has none (see the *Shares-only rows* section).
      # The cross-column rule that keeps this honest -- a row must carry
      # `value`, `share`, or both -- is not expressible in a
      # `check_table_schema()` column specification, so it lives in
      # `.abort_measureless_admin_rows()` below and is enforced by
      # `ensure_admin_shares()` and `resolve_admin_shares()`.
      list(name = "value", type = "double", min = 0),
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
#' Two rules are proved here rather than in the schema, because a
#' [check_table_schema()] specification speaks of one column at a time and
#' skips a missing value on every bound:
#'
#' - Every row must carry `value`, `share`, or both. A row with neither
#'   aborts with class `whep_error_admin_no_measure`. A row with `share`
#'   alone is accepted -- see the *Shares-only rows* section of
#'   [admin_shares_schema()], and never repair one by inventing a value.
#' - Neither measurement may be non-finite. `NaN` or `Inf` aborts with
#'   class `whep_error_admin_nonfinite`, because `is.na(NaN)` is `TRUE`
#'   and a `NaN` would otherwise travel as a shares-only row.
#'
#' @param x Tibble to complete. May already carry extra columns or be
#'   missing contract columns; see [ensure_columns()].
#'
#' @return `x` completed to the admin-shares contract, invisibly.
#'
#' @export
#'
#' @examples
#' # Omits columns the contract allows missing: `share`, `nuts_version`,
#' # `source_native_id`, `source_native_name`, `source_version` and
#' # `value_flag`. (`value` is allow-missing too, but a row must carry it
#' # or `share`, so this valued fixture keeps it.) `level_polity_code` is
#' # part of the key, so it must stay present and distinct per row even
#' # though the contract allows it to be `NA` for a genuinely unresolved
#' # row.
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
  .abort_nonfinite_admin_rows(completed, arg = "x")
  .abort_measureless_admin_rows(completed, arg = "x")
  assert_table_schema(completed, admin_shares_schema(), arg = "x")
}

# The cross-column half of the measurement rule. `value` is allow-missing
# so that a source consented to ship derived shares only (T23, 2026-09-02)
# can enter the contract at all; what is refused is a row carrying NEITHER
# measurement, which constrains nothing and would reach an allocation as an
# invisible abstention. The abort names the count and one offender rather
# than every row, so a large table's message stays readable.
#
# Every column is reached through `.admin_row_field()` because this rule
# also runs on tables that have not been proved against the schema yet: a
# bare `x$value` on a table without that column emits tibble's "Unknown or
# uninitialised column" warning ahead of the classed error the caller is
# waiting for.
.abort_measureless_admin_rows <- function(x, arg = "x") {
  if (!all(rlang::has_name(x, c("value", "share")))) {
    return(invisible(NULL))
  }
  measureless <- is.na(x$value) & is.na(x$share)
  if (!any(measureless)) {
    return(invisible(NULL))
  }
  first <- which(measureless)[[1L]]
  # Computed before the call, not inside it: a `{}` expression starting
  # with a dot is a cli style, not an R expression, since cli 3.4.0.
  unit <- .admin_row_field(x, "source_native_id", first)
  item <- .admin_row_field(x, "item_prod_code", first)
  when <- .admin_row_field(x, "year", first)
  producer <- .admin_row_field(x, "source", first)
  cli::cli_abort(
    c(
      "{sum(measureless)} {.arg {arg}} row{?s} carr{?ies/y} neither
       {.field value} nor {.field share}.",
      "x" = "First at row {first}: unit {.val {unit}}, item
             {.val {item}}, year {.val {when}}, source
             {.val {producer}}.",
      "i" = "A row may ship {.field share} alone -- that is the consented
             shares-only case. It may not ship nothing, and a value must
             never be invented to fill the gap."
    ),
    class = "whep_error_admin_no_measure"
  )
}

# The other half of "a measurement, or nothing at all": `NaN` and `Inf`
# are neither, and nothing else in this contract catches them.
# `is.na(NaN)` is `TRUE`, so with `value` allow-missing every downstream
# `is.na(value)` branch reads a 0/0 artefact as the consented shares-only
# case -- computed garbage laundered into a publication consent.
# `check_table_schema()` cannot help: it guards its bounds with
# `!is.na(values)`, and `value` has no maximum, so `Inf` clears `min = 0`
# as well.
.abort_nonfinite_admin_rows <- function(x, arg = "x") {
  offenders <- c("value", "share") |>
    rlang::set_names() |>
    purrr::map(\(column) .nonfinite_admin_rows(x, column)) |>
    purrr::keep(\(rows) length(rows) > 0L)
  if (length(offenders) == 0L) {
    return(invisible(NULL))
  }
  column <- names(offenders)[[1L]]
  first <- offenders[[column]][[1L]]
  found <- x[[column]][[first]]
  unit <- .admin_row_field(x, "source_native_id", first)
  producer <- .admin_row_field(x, "source", first)
  cli::cli_abort(
    c(
      "{sum(lengths(offenders))} {.arg {arg}} row{?s} carr{?ies/y} a
       non-finite measurement.",
      "x" = "First at row {first}: {.field {column}} is {.val {found}},
             unit {.val {unit}}, source {.val {producer}}.",
      "i" = "{.code NaN} is not a missing measurement: every
             {.code is.na(value)} branch downstream would read it as the
             consented shares-only case."
    ),
    class = "whep_error_admin_nonfinite"
  )
}

.nonfinite_admin_rows <- function(x, column) {
  if (!rlang::has_name(x, column)) {
    return(integer())
  }
  which(is.nan(x[[column]]) | is.infinite(x[[column]]))
}

# One row's field for an abort message, from a table that may not carry
# the column: these rules run before the schema is proved.
.admin_row_field <- function(x, column, i) {
  if (!rlang::has_name(x, column)) {
    return(NA)
  }
  x[[column]][[i]]
}
