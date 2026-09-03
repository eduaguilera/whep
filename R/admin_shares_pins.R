# Loader for the per-family tier-2/3 admin-statistics pins (#1000, T24).
#
# The five families ship as one pin each, not as one combined pin, because
# their publication consent was decided family by family (T23,
# 2026-09-02): the Japanese prefectures, the Spanish provinces, the
# Australian states and the French departments' livestock ship as raw
# observed rows with attribution, while the Latin American research panel
# ships DERIVED SHARES ONLY -- no source values -- until its own
# publication. A family whose consent has not cleared is therefore simply
# absent from the board, which is a normal state this loader has to report
# rather than an error it may swallow.
#
# Two rules follow from that, and both are load-bearing:
#
# - An absent family is REPORTED, never try-caught into silence. The
#   loader asks `whep_inputs` whether the alias is registered before it
#   reads anything, the way `.read_fishstat_trade()` does, and a
#   registered alias whose pin yields nothing is treated the same way;
#   every other error from `whep_read_file()` propagates untouched, so a
#   board outage cannot masquerade as an embargo.
# - The Latin American pin must never carry a `value` column.
#   `.admin_family_check()` aborts if one ever appears, so a regenerated
#   pin that quietly reinstated source values fails here instead of
#   travelling downstream.
#
# The pins themselves are built and staged by
# `inst/scripts/prepare_admin_stats_pins.R`, which also writes
# `inst/extdata/admin_stats_pins_manifest.csv`. Registering the aliases in
# `inst/extdata/whep_inputs.csv` (and the board upload behind it) is a
# separate, deliberate step that this wave does not take: until it
# happens, every family reports as `not_shipped`, which is exactly what
# T25's coverage report expects to see.

#' Read the per-family subnational admin-statistics pins
#'
#' @description
#' Read the tier-2 and tier-3 administrative-statistics families that WHEP
#' compiled for the subnational spatialization, one pin per family, and
#' return them as a named list of tibbles.
#'
#' Every family is read by an explicit alias: there is no discovery step
#' and no wildcard, so a family that is not shipped cannot be silently
#' substituted by another. A requested family that is absent -- its alias
#' is not registered in [`whep_inputs`], or its pin holds no rows -- is
#' named in the returned `not_shipped` element instead of aborting the
#' read, because a family whose publication consent has not cleared is a
#' planned state of this dataset (see the T23 outcome recorded in
#' `inst/extdata/admin_stats_pins_manifest.csv`). Any other failure of
#' [whep_read_file()] is left to propagate: an unreachable board is not an
#' embargo.
#'
#' @section Families:
#' The five aliases, their tier and what each ships, as staged on
#' 2026-09-03 from the harmonized subnational panel:
#'
#' - `"admin-stats-japan"` (tier 2): 46 prefectures, 32,095 observed area
#'   and production rows, 1961-2022.
#' - `"admin-stats-spain-provinces"` (tier 2): 50 provinces, 323,469
#'   observed-lane area and production rows, 1961-2021. The compilation's
#'   Spanish livestock rows are all flagged `admin_level_overlap` and are
#'   therefore not shipped, which is why three of its 53 units are absent.
#' - `"admin-stats-australia"` (tier 2): 8 states and territories, 6,597
#'   observed area, production and head-count rows, 1961-2022.
#' - `"admin-stats-france-livestock"` (tier 2): 89 departments, 58,740
#'   observed head counts, 1961-2020.
#' - `"admin-stats-latam"` (tier 3): 142 first-level units of the
#'   Infante-Amate, Urrego-Mesa, Badia-Miro and Aguilera panel, 875,514
#'   rows, 1961-2023, shipped as **shares only**. Each row carries
#'   `share`, its unit's share of the country total for the same item,
#'   indicator and year, and no `value`; this reader aborts if a future
#'   version of that pin carries one.
#'
#' `"admin-stats-jrc"` is not one of these. It is the public tier-1 JRC
#' product with its own reader, and passing it here aborts as an unknown
#' alias.
#'
#' @param families Character vector of family aliases to read. `NULL`, the
#'   default, reads all five. An alias outside that set aborts.
#' @param example If `TRUE`, return a small fixture instead of reading any
#'   pin. Defaults to `FALSE`.
#'
#' @return A named list with one element per requested family that is
#'   shipped, each a tibble in the shared admin-statistics reader shape
#'   (`source`, `source_native_unit_id`, `source_native_unit_name`,
#'   `source_native_item_code`, `source_native_item_name`,
#'   `indicator_used`, `quantity`, `year`, `value` or `share`,
#'   `value_unit`, `value_flag`, `grain`, `nuts_version`, `source_version`,
#'   `recorded_at`), plus a `not_shipped` element holding the aliases that
#'   were requested but are not on the board. `not_shipped` is always
#'   present, and is `character(0)` when every requested family loaded.
#'
#' @export
#'
#' @examples
#' read_admin_family(example = TRUE)
read_admin_family <- function(families = NULL, example = FALSE) {
  if (example) {
    return(.example_admin_family())
  }
  aliases <- .admin_family_check_aliases(families %||% .admin_family_aliases())
  loaded <- aliases |>
    rlang::set_names() |>
    purrr::map(.admin_family_read_one) |>
    purrr::compact()
  c(loaded, list(not_shipped = setdiff(aliases, names(loaded))))
}

# The families T23 decided on, in tier and then source order. This vector
# is the whole vocabulary: `read_admin_family()` refuses anything else
# rather than passing an unrecognised alias to the board, where it would
# surface as a `whep_inputs` lookup failure with no hint that the name is
# not a family at all.
.admin_family_aliases <- function() {
  c(
    "admin-stats-japan",
    "admin-stats-spain-provinces",
    "admin-stats-australia",
    "admin-stats-france-livestock",
    "admin-stats-latam"
  )
}

# The one family shipping derived shares instead of source values, per the
# T23 consent outcome of 2026-09-02.
.admin_family_shares_only <- function() {
  "admin-stats-latam"
}

.admin_family_check_aliases <- function(families) {
  if (!is.character(families) || length(families) == 0) {
    cli::cli_abort(
      "{.arg families} must be a character vector of family aliases."
    )
  }
  known <- .admin_family_aliases()
  unknown <- setdiff(families, known)
  if (length(unknown) > 0) {
    cli::cli_abort(c(
      "{length(unknown)} unknown admin-statistics famil{?y/ies}:
       {.val {unknown}}.",
      i = "Known famil{?y/ies}: {.val {known}}."
    ))
  }
  unique(families)
}

# Whether the board knows the alias at all. Asking `whep_inputs` first is
# what keeps an unregistered family a reportable absence instead of an
# abort from `.fetch_file_info()`, and it is one of the two seams the
# tests stub.
.admin_family_registered <- function(alias) {
  alias %in% whep::whep_inputs$alias
}

# A registered alias whose pin yields nothing is an absence too, and
# `NULL` is how the reader says so. Everything else -- a board outage, a
# corrupt file -- comes back as an error from `whep_read_file()` and is
# deliberately not caught here.
.admin_family_read_one <- function(alias) {
  if (!.admin_family_registered(alias)) {
    return(NULL)
  }
  rows <- whep_read_file(alias)
  if (is.null(rows) || nrow(rows) == 0) {
    return(NULL)
  }
  .admin_family_check(tibble::as_tibble(rows), alias)
}

# The columns every family pin carries, in the shared reader shape that
# `R/admin_stats_nass.R` and the other admin-statistics readers emit.
.admin_family_columns <- function() {
  c(
    "source",
    "source_native_unit_id",
    "source_native_unit_name",
    "source_native_item_code",
    "source_native_item_name",
    "indicator_used",
    "quantity",
    "year",
    "value_unit",
    "value_flag",
    "grain",
    "nuts_version",
    "source_version",
    "recorded_at"
  )
}

# The measurement column each family ships: a raw value for the consented
# tier-2 families, a derived share for the tier-3 panel.
.admin_family_measure <- function(alias) {
  if (alias %in% .admin_family_shares_only()) {
    return("share")
  }
  "value"
}

# The family's full column set in reader-contract order, the measurement
# column sitting where `value` sits in `read_admin_stats_nass()`'s output.
.admin_family_shape <- function(alias) {
  columns <- .admin_family_columns()
  append(columns, .admin_family_measure(alias), after = match("year", columns))
}

.admin_family_check <- function(rows, alias) {
  measure <- .admin_family_measure(alias)
  columns <- .admin_family_columns()
  missing <- setdiff(c(columns, measure), names(rows))
  if (length(missing) > 0) {
    cli::cli_abort(c(
      "The {.val {alias}} pin is missing column{?s} {.field {missing}}.",
      i = "Every family pin carries {.field {columns}} plus
           {.field {measure}}."
    ))
  }
  .admin_family_check_consent(rows, alias)
}

# The T23 boundary, enforced where the data enters the package rather than
# trusted to the producer: the Latin American panel is consented as
# derived shares, so a `value` column in that pin would be a
# redistribution of source values its co-authors have not agreed to.
.admin_family_check_consent <- function(rows, alias) {
  if (alias %in% .admin_family_shares_only() && "value" %in% names(rows)) {
    cli::cli_abort(c(
      "The {.val {alias}} pin carries a {.field value} column.",
      x = "That family is consented as derived shares only (T23,
           2026-09-02); source values must not be redistributed.",
      i = "Rebuild the pin with
           {.file inst/scripts/prepare_admin_stats_pins.R}."
    ))
  }
  rows
}
