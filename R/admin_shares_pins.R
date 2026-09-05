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
# Three rules follow from that, and all three are load-bearing:
#
# - An absent family is REPORTED, never try-caught into silence. The
#   loader asks `whep_inputs` whether the alias is registered before it
#   reads anything, the way `.read_fishstat_trade()` does, and a
#   registered alias whose pin yields nothing is treated the same way;
#   every other error from `whep_read_file()` propagates untouched, so a
#   board outage cannot masquerade as an embargo.
# - The Latin American pin must never carry a `value` column, NOR any
#   other column outside its declared shape. `.admin_family_check()`
#   aborts on either, so a regenerated pin that quietly reinstated source
#   values fails here instead of travelling downstream -- under that name
#   or under an alias for it, which is why the declared set is closed and
#   not merely required.
# - WHICH family a pin's rows belong to is decided by the rows' own
#   `source` column, resolved through `.admin_source_registry()`, and
#   never by the alias the caller asked for. Reading the alias alone made
#   the consent rule ask about Japan while the rows were the Latin
#   American panel's, so that panel's withheld source values were
#   delivered intact from inside any value family's pin.
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

# --- The one authority: every source this package recognises ------------
#
# A CLOSED WORLD. Every source that may appear in an admin-shares artifact
# is declared here, and a label resolving to no row of this table is
# REFUSED -- whatever tier it claims, however it is spelled. Three earlier
# rounds tried the opposite, rejecting bad spellings one at a time, and
# each round produced the next one: a trailing space, then a leading
# non-breaking space, a zero-width space, a Cyrillic `a`, a fullwidth `a`,
# an underscore variant. That contest cannot be won, and the decisive case
# is not a spelling at all: a label in no recognised namespace at tier 1
# carrying the withheld values is INDISTINGUISHABLE from a genuine new
# public product, because `admin_shares_schema()` deliberately leaves
# `source` open and `tier` is a precedence rank, never a permission.
# Gating on what the package recognises rather than on what it rejects
# retires the whole class: an odd spelling simply is not a declared
# source.
#
# Each row declares three things about one source, and this table is the
# only place any of them is stated:
#
# - `is_family`: whether it is one of the in-house families
#   `read_admin_family()` reads. A family's IDENTITY is its own source
#   label -- there is no second name for it -- so the declaration is the
#   flag and `.admin_family_of()` hands back the label itself. Repeating
#   the alias in a column of its own would be one more literal to typo,
#   and a typo there would invent a second identity.
# - `measure`: the measurement it is PERMITTED to ship, `"value"` or
#   `"share"`. Both the gate and the two preparers in `inst/scripts` read
#   it here; the preparers used to infer it from whichever column a pin
#   happened to carry, so the builder's notion of what a family ships
#   could differ from the gate's.
# - `consent`: `"manifest"` where the source's publication permission must
#   be recorded in `inst/extdata/admin_stats_pins_manifest.csv` (the T23
#   families), `"public_licence"` where it is a public product shipping
#   under its own licence and carrying no row there.
#
# It is CODE, not a data file, on purpose. The manifest is a build product
# rewritten by both preparers, so a permission recorded there can be
# rewritten by the same run that ships the data; a declaration a reviewer
# has to approve in a diff cannot. `admin-shares`, the assembled pin's own
# alias, is deliberately absent: it names no producer, so it grants
# nothing to a row calling itself that.
#
# The public products are the labels WHEP's own tier-1 readers emit --
# `R/admin_stats_nass.R`, `R/admin_stats_sidra.R`,
# `R/admin_stats_eurostat.R` and `inst/scripts/prepare_jrc_subnational.R`.
# Three of them (`IBGE_PPM`, `Eurostat_apro_mt_ls_r`,
# `Eurostat_ef_lsk_poultry`) serve head counts, which
# `admin_shares_schema()` does not carry, so no row of theirs reaches the
# assembled pin today; they are declared because they are recognised
# producers, not because they are expected.
.admin_source_registry <- function() {
  tibble::tribble(
    ~source,                        ~is_family, ~measure, ~consent,
    "admin-stats-japan",            TRUE,       "value",  "manifest",
    "admin-stats-spain-provinces",  TRUE,       "value",  "manifest",
    "admin-stats-australia",        TRUE,       "value",  "manifest",
    "admin-stats-france-livestock", TRUE,       "value",  "manifest",
    "admin-stats-latam",            TRUE,       "share",  "manifest",
    "USDA_NASS",                    FALSE,      "value",  "public_licence",
    "IBGE_PAM",                     FALSE,      "value",  "public_licence",
    "IBGE_PPM",                     FALSE,      "value",  "public_licence",
    "Eurostat_apro_cpshr",          FALSE,      "value",  "public_licence",
    "Eurostat_apro_cpnhr_h",        FALSE,      "value",  "public_licence",
    "Eurostat_apro_mt_ls_r",        FALSE,      "value",  "public_licence",
    "Eurostat_ef_lsk_poultry",      FALSE,      "value",  "public_licence",
    "JRC_subnational_crops",        FALSE,      "value",  "public_licence"
  )
}

# Folding a label for lookup. Kept for USABILITY -- a trailing space or a
# capitalised vintage should find its declaration rather than fail a
# reviewer's eye -- and it is NOT the security boundary any more: under
# the closed world above, a spelling this does not fold simply resolves to
# no declaration and is refused. Nothing below may be relaxed on the
# grounds that this folds it.
#
# Folded over the DISTINCT labels and mapped back, not over every row: the
# assembled pin is 1.2M rows carrying four labels, and `trimws()` is a
# regex, so the honest one-liner cost the read seconds each time a rule
# asked which source a row named.
.admin_label_canonical <- function(labels) {
  labels <- as.character(labels)
  distinct <- unique(labels)
  tolower(trimws(distinct))[match(labels, distinct)]
}

# The registry row each label resolves to, `NA` where it resolves to none.
# Every rule in this file asks its question through this one match, so
# there is a single answer to "which declared source is this?".
.admin_registry_match <- function(labels) {
  declared <- .admin_source_registry()$source
  match(.admin_label_canonical(labels), .admin_label_canonical(declared))
}

# The declared source a label names, canonically, `NA` where it names
# none.
.admin_source_of <- function(labels) {
  .admin_source_registry()$source[.admin_registry_match(labels)]
}

# The declared family a label names -- its own source label -- `NA` where
# it names none, either because the label is a public product, which
# belongs to no family, or because it is not declared at all.
.admin_family_of <- function(labels) {
  named <- .admin_source_of(labels)
  named[!.admin_registry_is_family(labels)] <- NA_character_
  named
}

# Whether each label names a declared family. `FALSE`, not `NA`, for an
# undeclared label: this answers "is this one of the five", and the
# undeclared case is refused by `.admin_shares_check_declared()` on its
# own terms rather than by an `NA` leaking into a subscript.
.admin_registry_is_family <- function(labels) {
  is_family <- .admin_source_registry()$is_family[
    .admin_registry_match(labels)
  ]
  !is.na(is_family) & is_family
}

# The measurement a label's source is PERMITTED to ship, `NA` where the
# label is not declared.
.admin_measure_of <- function(labels) {
  .admin_source_registry()$measure[.admin_registry_match(labels)]
}

# The families T23 decided on, in tier and then source order, read off the
# registry rather than restated. This vector is the whole family
# vocabulary: `read_admin_family()` refuses anything else rather than
# passing an unrecognised alias to the board, where it would surface as a
# `whep_inputs` lookup failure with no hint that the name is not a family
# at all.
.admin_family_aliases <- function() {
  registry <- .admin_source_registry()
  registry$source[registry$is_family]
}

# The families shipping derived shares instead of source values, per the
# T23 consent outcome of 2026-09-02. One today, and derived rather than
# restated so it cannot disagree with the measurement each family is
# declared for.
.admin_family_shares_only <- function() {
  registry <- .admin_source_registry()
  registry$source[registry$is_family & registry$measure == "share"]
}

# The sources whose publication permission must be recorded in the consent
# manifest, declared as such rather than guessed from a label's namespace.
.admin_consented_sources <- function() {
  registry <- .admin_source_registry()
  registry$source[registry$consent == "manifest"]
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

# The measurement column each source is PERMITTED to ship, from the one
# authority. An undeclared label is refused rather than defaulted to
# `"value"`: a default is a permission granted by omission, which is the
# hole the closed world closes. `inst/scripts/prepare_admin_stats_pins.R`
# and `inst/scripts/prepare_admin_shares_pin.R` both read the measurement
# through here, so no builder can ship a family a measurement this gate
# would then refuse.
.admin_family_measure <- function(alias) {
  measure <- .admin_measure_of(alias)
  if (!anyNA(measure)) {
    return(measure)
  }
  undeclared <- alias[is.na(measure)]
  declared <- .admin_source_registry()$source
  cli::cli_abort(
    c(
      "{length(undeclared)} undeclared source{?s} with no permitted
       measurement: {.val {undeclared}}.",
      "i" = "Declared source{?s}: {.val {declared}}.",
      "i" = "A new source needs a declaration in
             {.fun .admin_source_registry}."
    ),
    class = "whep_error_admin_unknown_source"
  )
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
  .admin_family_check_source(rows, alias)
  .admin_family_check_consent(rows, alias)
  .admin_family_check_columns(rows, alias)
  .admin_family_check_bounds(rows, alias)
}

# WHICH family these rows belong to, read off the rows' OWN `source`
# column and resolved through `.admin_source_registry()` -- never taken
# from the alias the caller asked for.
#
# Every rule below this one was keyed on that alias, so a pin served under
# a value family's name was never asked what it actually contained: the
# Latin American panel's withheld source values were delivered intact from
# inside `admin-stats-japan`, `admin-stats-australia`,
# `admin-stats-spain-provinces` and `admin-stats-france-livestock`,
# because `.admin_family_check_consent()` was asked about Japan while the
# rows were the panel's. A label naming another declared source, or naming
# none at all, is refused here rather than delivered under the alias
# asked for.
#
# `inst/scripts/prepare_admin_shares_pin.R` runs this same rule on every
# route rows take into the assembled pin -- a staged family folder and an
# injected tier-1 source included -- because both of those rewrite
# `source` to the name they were served under, and a rewrite that never
# asked the rows what they were is where this gate was being bypassed.
# The wording is therefore about a SOURCE, not only a family.
#
# Rows with no `source` column at all are refused on the same terms: they
# name no producer, so nothing can be resolved, and letting the alias
# decide for them is the alias-only gate this rule replaced. Every reader
# and every family pin emits the column, so nothing legitimate lacks it.
.admin_family_check_source <- function(rows, alias) {
  if (!rlang::has_name(rows, "source")) {
    cli::cli_abort(
      c(
        "The rows served as {.val {alias}} carry no {.code source}
         column.",
        "x" = "Rows naming no producer cannot be resolved against the
               declared sources, and the name they were served under does
               not decide what they are.",
        "i" = "Every admin-statistics reader and family pin emits
               {.field source}; inject rows in that reader shape."
      ),
      class = "whep_error_admin_family_source"
    )
  }
  labels <- unique(rows$source)
  resolved <- .admin_source_of(labels)
  foreign <- is.na(resolved) | resolved != alias
  if (!any(foreign)) {
    return(invisible(NULL))
  }
  offending <- labels[foreign]
  declared <- .admin_source_registry()$source
  cli::cli_abort(
    c(
      "The rows served as {.val {alias}} carry {length(offending)} foreign
       {.field source} label{?s}: {.val {offending}}.",
      "x" = "A pin ships the rows of the source it is served as and nothing
             else. A label naming another declared source, or naming none,
             is refused rather than relabelled to the name asked for.",
      "i" = "Declared source{?s}: {.val {declared}}."
    ),
    class = "whep_error_admin_family_source"
  )
}

# The declared column set is CLOSED, the way the assembled contract
# closes its own (`admin_shares_schema()`, `extra_columns = "forbid"`).
# Requiring the declared columns without closing the set left the consent
# rule below testing one column NAME: a shares-only family's withheld
# values travelled untouched in any column not literally called `value`
# -- `raw_value_ha`, say -- and the caller received them. It runs after
# the consent rule so that a `value` column still reports as the T23
# breach it is rather than as an ordinary undeclared column.
.admin_family_check_columns <- function(rows, alias) {
  shape <- .admin_family_shape(alias)
  extra <- setdiff(names(rows), shape)
  if (length(extra) == 0L) {
    return(rows)
  }
  cli::cli_abort(
    c(
      "The {.val {alias}} pin carries {length(extra)} undeclared
       column{?s}: {.field {extra}}.",
      x = "A family pin ships its declared shape and nothing else; an
           undeclared column is a measurement no rule on this path can
           see.",
      i = "Declared: {.field {shape}}."
    ),
    class = "whep_error_admin_pin_columns"
  )
}

# The declared measure column must hold a MEASUREMENT of the kind it is
# declared for. Closing the column set (above) stops a value travelling in
# a column the family does not declare; it does not stop one travelling in
# the column it does declare, and a shares-only pin whose `share` column
# held the withheld source values passed every rule on this path because
# nothing here asked whether a share was a share. The bound is the one the
# assembled contract already holds (`admin_shares_schema()`: `share` in
# [0, 1], `value` at least 0), plus finiteness, which no bound catches on
# its own. `NA` passes: it is a missing measurement, not a smuggled one,
# and the assembled contract refuses a row left with none.
.admin_family_check_bounds <- function(rows, alias) {
  measure <- .admin_family_measure(alias)
  x <- rows[[measure]]
  upper <- if (measure == "share") 1 else Inf
  # `is.nan(x)` is tested on its own because `is.na(NaN)` is TRUE, so a
  # `!is.na(x)` guard alone lets a 0/0 artefact through as though it were a
  # missing measurement -- the same laundering this file refuses at the
  # assembled boundary.
  offending <- if (!is.numeric(x)) {
    rep(TRUE, length(x))
  } else {
    is.nan(x) | (!is.na(x) & (!is.finite(x) | x < 0 | x > upper))
  }
  if (!any(offending)) {
    return(rows)
  }
  first <- which(offending)[[1L]]
  found <- x[[first]]
  cli::cli_abort(
    c(
      "The {.val {alias}} pin's {.field {measure}} column carries
       {sum(offending)} value{?s} that {?is/are} not a {measure}.",
      "x" = "First at row {first}: {.val {found}} (class
             {.cls {class(x)}}).",
      "i" = "A {.field share} is a finite number in [0, 1] and a
             {.field value} a finite non-negative number; anything else
             in that column is a quantity this family is not declared to
             ship."
    ),
    class = "whep_error_admin_pin_measure_bounds"
  )
}

# The T23 boundary, enforced where the data enters the package rather than
# trusted to the producer: the Latin American panel is consented as
# derived shares, so a `value` column in that pin would be a
# redistribution of source values its co-authors have not agreed to.
#
# Keyed on `alias` and not on the rows, which is only sound because
# `.admin_family_check_source()` has just proved that every row names this
# family. Before it did, this rule was asked about whichever family the
# caller named while the rows were another's.
.admin_family_check_consent <- function(rows, alias) {
  if (alias %in% .admin_family_shares_only() && "value" %in% names(rows)) {
    cli::cli_abort(
      c(
        "The {.val {alias}} pin carries a {.field value} column.",
        x = "That family is consented as derived shares only (T23,
             2026-09-02); source values must not be redistributed.",
        i = "Rebuild the pin with
             {.file inst/scripts/prepare_admin_stats_pins.R}."
      ),
      # The same condition the assembled pin raises when the withheld
      # values reach it row-wise, so one class covers both paths.
      class = "whep_error_admin_pin_consent_value"
    )
  }
  rows
}

# --- The assembled admin-shares pin (#1000, T38) ------------------------
#
# NSE globals this block needs: NONE. Everything below is base-R
# subsetting and `tibble::tibble()`, deliberately, so that adding the
# assembled pin's reader costs `R/utils.R` no new entries.
#
# `read_admin_family()` above reads the five per-family pins in their
# source-native READER shape. `read_admin_shares()` below reads the single
# assembled pin in the ADMIN-SHARES CONTRACT shape
# (`admin_shares_schema()`), which is what `resolve_admin_shares()` and
# the allocation consume. The assembly itself -- the vocabulary joins, the
# consent gate, the container resolution and the counted drops -- lives in
# `inst/scripts/prepare_admin_shares_pin.R`, outside the package build, so
# that the pin can be rebuilt without shipping its inputs.
#
# Six properties are proved on every read rather than trusted to the
# builder, because each of them fails silently if it is wrong.
#
# The last three are about IDENTITY, and they are worth stating as one
# rule before they are stated separately: a source may ship only what it
# is DECLARED to ship, and which source a row belongs to is decided by
# resolving its labels against `.admin_source_registry()`, never by how
# they are spelled and never by the tier they claim.
#
# - THE PIN CARRIES NO RESOLVED POLITY CODE. Source-native pinning is the
#   whole reason this artifact needs no staleness warning
#   (`R/admin_shares_polities.R`, "Why there is no staleness warning"). A
#   frozen `level_polity_code` inside it would reintroduce the whep#905
#   trap invisibly, so one is refused.
# - EVERY ROW CARRIES A MEASUREMENT. `value` is allow-missing since T38 so
#   that the consented shares-only family can enter the contract at all;
#   a row with neither `value` nor `share` is still refused, and so is a
#   non-finite one -- `is.na(NaN)` is `TRUE`, so a 0/0 artefact would
#   otherwise be read as the consented case.
# - A ROW NAMES ONE PRODUCER. `source` and `source_id` are the same value
#   space (`admin_shares_schema()`) and every builder writes them equal.
#   Nothing read them together, so a pin naming one producer in `source`
#   and another in `source_id` travelled: the gates read `source`, and the
#   row's real family sat in the column nobody looked at.
# - EVERY SOURCE IS DECLARED, AND A CONSENTED ONE HAS ITS CONSENT ROW.
#   This is a CLOSED WORLD: a label resolving to no row of
#   `.admin_source_registry()` is refused, whatever tier it claims and
#   however it is spelled. A deny-list on labels was tried twice and lost
#   twice, because the case it cannot decide is not a spelling: a label in
#   no recognised namespace at tier 1 carrying the withheld values is
#   indistinguishable from a genuine new public product, `source` being
#   open by design and `tier` a precedence rank. A declared source whose
#   `consent` is `"manifest"` and whose row is missing from
#   `inst/extdata/admin_stats_pins_manifest.csv` is data shipped without a
#   recorded permission, which is the one failure this whole design exists
#   to prevent. Only a declared source's row grants that permission: the
#   manifest also carries this pin's own bookkeeping row, and
#   `"admin-shares"` is not a declared source, so it grants nothing.
# - EVERY SOURCE SHIPS THE MEASUREMENT IT IS DECLARED FOR. The family
#   loader can refuse a `value` column outright, one pin per family; this
#   pin has one `value` column for every source, so the rule is per source
#   and per row here. A shares-only family carrying source values is the
#   T23 breach itself, and it would pass every other check on this list.
#   The measurement comes from the row's declaration, resolved from both
#   label columns, and never from `tier`: `tier` is a precedence rank for
#   `resolve_admin_shares()`, not a permission, so a shares-only family is
#   shares-only at tier 1 exactly as it is at tier 3.

#' Read the assembled subnational admin-shares pin
#'
#' @description
#' Read the single `admin-shares` pin -- the tier-1, tier-2 and tier-3
#' administrative statistics assembled onto the one contract
#' [admin_shares_schema()] declares -- together with the report of which
#' families are **not** in it and why.
#'
#' Polity resolution is deliberately not done here. The pin is keyed on
#' source-native identifiers and carries `level_polity_code == NA` on
#' every row; a caller that needs polities passes the rows through
#' [resolve_admin_units()], which redoes the resolution against the
#' current [polities] snapshot on every load. That is what lets this
#' artifact age without going stale.
#'
#' @section What the excluded report can and cannot say:
#' `excluded` names every tier-2/3 family of [read_admin_family()] that
#' contributed no row to this pin, with a reason from a closed vocabulary
#' and a `detail` saying why the family is absent -- not what permission
#' it ships under, which is the manifest's own job:
#'
#' - `"no_consent_manifest_row"`: the family is absent from
#'   `inst/extdata/admin_stats_pins_manifest.csv`, so its publication
#'   consent is not recorded and the assembly refused to ship it.
#' - `"no_rows_in_pin"`: its consent is recorded, but none of its rows
#'   survived onto the contract.
#'
#' The second reason is expected for `"admin-stats-france-livestock"` and
#' will stay so: that family is head counts, and head counts are outside
#' [admin_shares_schema()] by design -- `indicator_used` closes over area,
#' production and yield (`R/admin_stats_sidra.R`). The livestock
#' constraint travels the reader and family path, not this pin, and that
#' is what its `detail` says.
#'
#' A read cannot see which rows were dropped or why, so for any other
#' family `detail` points at the assembly's per-source counts. Those
#' counts are a **second, build-time report**, written by
#' `inst/scripts/prepare_admin_shares_pin.R` and not carried into the pin.
#' It shares this vocabulary where the two can mean the same thing
#' (`"no_consent_manifest_row"`, `"no_rows_in_pin"`) and adds one reason a
#' read cannot observe: `"not_available"`, a consented family that was
#' neither registered on the board nor staged locally when the pin was
#' assembled. At read time that is indistinguishable from
#' `"no_rows_in_pin"`, which is why the vocabulary here stays closed at
#' two.
#'
#' @param example If `TRUE`, return a small fixture instead of reading the
#'   pin. Defaults to `FALSE`.
#'
#' @return A named list of three elements:
#'
#' - `shares`: the pin's rows, in [admin_shares_prototype()]'s shape, or
#'   the zero-row prototype when the pin is not shipped.
#' - `excluded`: a tibble of `source`, `reason` and `detail`, as above.
#' - `not_shipped`: `"admin-shares"` when the pin's alias is not
#'   registered in [`whep_inputs`] or holds no rows, `character(0)`
#'   otherwise.
#'
#' @export
#'
#' @examples
#' read_admin_shares(example = TRUE)
read_admin_shares <- function(example = FALSE) {
  if (example) {
    return(.example_admin_shares())
  }
  rows <- .admin_shares_read_pin()
  list(
    shares = rows,
    excluded = .admin_shares_excluded(rows),
    not_shipped = if (nrow(rows) == 0L) .admin_shares_alias() else character()
  )
}

.admin_shares_alias <- function() {
  "admin-shares"
}

# The contract, rekeyed source-natively. The pin carries no resolved
# polity code, so the contract's own key -- which includes
# `level_polity_code` -- collapses to one value on every row and can prove
# nothing. `(source, source_native_id, level, item_prod_code,
# indicator_used, year)` is the key that actually identifies a pin row,
# and adding `source` to the contract key follows the precedent
# `.admin_resolve_schema()` (`R/admin_shares_resolve.R`) already sets.
.admin_shares_pin_schema <- function() {
  schema <- admin_shares_schema()
  schema$key <- c(
    "source",
    "source_native_id",
    "level",
    "item_prod_code",
    "indicator_used",
    "year"
  )
  schema
}

# The same registration seam `.admin_family_registered()` gives the family
# loader: asking `whep_inputs` first is what keeps an unregistered pin a
# reportable absence rather than an abort from `.fetch_file_info()`.
.admin_shares_registered <- function() {
  .admin_shares_alias() %in% whep::whep_inputs$alias
}

.admin_shares_read_pin <- function() {
  if (!.admin_shares_registered()) {
    return(admin_shares_prototype())
  }
  rows <- whep_read_file(.admin_shares_alias())
  if (is.null(rows) || nrow(rows) == 0L) {
    return(admin_shares_prototype())
  }
  .admin_shares_check_pin(tibble::as_tibble(rows))
}

# The schema is proved first, so that a pin missing a column fails on the
# column it is missing. The cross-column and per-source rules below all
# read columns the schema has just established.
.admin_shares_check_pin <- function(
  rows,
  manifest = .admin_shares_manifest()
) {
  assert_table_schema(rows, .admin_shares_pin_schema(), arg = "admin-shares")
  .admin_shares_check_unresolved(rows)
  .abort_nonfinite_admin_rows(rows, arg = "admin-shares")
  .abort_measureless_admin_rows(rows, arg = "admin-shares")
  .admin_shares_check_identity(rows)
  .admin_shares_check_declared(rows)
  .admin_shares_check_consent(rows, manifest)
  .admin_shares_check_measure(rows)
  rows
}

# --- Which source a row belongs to ------------------------------------
#
# Resolution against `.admin_source_registry()`, not spelling, and never
# the tier. The resolution itself lives at the top of this file, beside
# the registry; what follows uses it.

# One column of `rows`, or an all-missing stand-in where the frame has
# none. These rules also run from `prepare_admin_shares_pin.R` and from
# callers holding a table the schema has not proved yet, where a bare `$`
# emits tibble's "Unknown or uninitialised column" warning ahead of the
# classed error.
.admin_column <- function(rows, column, missing = NA) {
  if (!rlang::has_name(rows, column)) {
    return(rep(missing, nrow(rows)))
  }
  rows[[column]]
}

# Every declared family the pin names.
#
# `source` alone, deliberately. It used to read `source_id` as well, and
# that half was unreachable: `.admin_shares_check_identity()` runs before
# every rule here and aborts when the two identifiers do not name one
# producer, so by this point `source_id` can only repeat `source`. It read
# as protection it could not give. `.admin_shares_check_identity()` is
# what carries that job now, and `.admin_shares_check_declared()` reads
# both columns before it, so an undeclared label in either one is refused
# whether or not the two agree.
.admin_families_named <- function(rows) {
  named <- .admin_family_of(.admin_column(rows, "source", NA_character_))
  intersect(.admin_family_aliases(), named)
}

# The two identifiers must name one producer. Compared canonically, so
# this rule is about the producer named and not about the spelling, which
# the family rules judge on their own.
.admin_shares_check_identity <- function(rows) {
  split <- .admin_label_canonical(rows$source) !=
    .admin_label_canonical(rows$source_id)
  split <- !is.na(split) & split
  if (!any(split)) {
    return(invisible(NULL))
  }
  alias <- .admin_shares_alias()
  first <- which(split)[[1L]]
  named <- rows$source[[first]]
  identified <- rows$source_id[[first]]
  cli::cli_abort(
    c(
      "{sum(split)} row{?s} of the {.val {alias}} pin name one producer in
       {.field source} and another in {.field source_id}.",
      "x" = "First at row {first}: {.val {named}} against
             {.val {identified}}.",
      "i" = "{.field source_id} is the producer's immutable identifier in
             the same value space as {.field source}; a row whose two
             identifiers disagree hides its own family from any gate that
             reads one of them."
    ),
    class = "whep_error_admin_pin_identity"
  )
}

.admin_shares_check_unresolved <- function(rows) {
  resolved <- !is.na(rows$level_polity_code)
  if (!any(resolved)) {
    return(invisible(NULL))
  }
  alias <- .admin_shares_alias()
  cli::cli_abort(
    c(
      "The {.val {alias}} pin carries {sum(resolved)} resolved
       {.field level_polity_code} value{?s}.",
      "x" = "For example {.val {rows$level_polity_code[resolved][[1L]]}}.",
      "i" = "The pin is keyed source-natively so that resolution is redone
             on every load; a frozen polity code would age against
             {.val polities} with nothing to warn about it.",
      "i" = "Resolve with {.fun resolve_admin_units} after reading."
    ),
    class = "whep_error_admin_pin_resolved"
  )
}

# THE CLOSED WORLD. Every label in either identifier column must resolve
# to a row of `.admin_source_registry()`; one that does not is REFUSED.
#
# This gates on what the package RECOGNISES, not on what it rejects, and
# that is the whole difference. Two earlier rounds gated on rejection --
# an `admin-` namespace test plus `tier > 1` -- and each round a new
# spelling walked past it. The case that ends the argument is not a
# spelling: a label in no recognised namespace at tier 1, carrying the
# withheld values, is indistinguishable from a genuine new public product,
# because `admin_shares_schema()` leaves `source` open on purpose and
# `tier` is a precedence rank. Under this rule that label is simply not
# declared, so the entire class of perturbation dies with no string
# cleverness at all -- and the honest new source it resembles gets a
# declaration, in a diff, which is the point.
#
# `unique()` first: the pin is 1.2M rows carrying four labels.
.admin_shares_check_declared <- function(rows) {
  labels <- unique(c(
    .admin_column(rows, "source", NA_character_),
    .admin_column(rows, "source_id", NA_character_)
  ))
  undeclared <- labels[is.na(.admin_registry_match(labels))]
  if (length(undeclared) == 0L) {
    return(invisible(NULL))
  }
  alias <- .admin_shares_alias()
  declared <- .admin_source_registry()$source
  cli::cli_abort(
    c(
      "{length(undeclared)} source label{?s} in the {.val {alias}} pin
       {?is/are} not declared: {.val {undeclared}}.",
      "x" = "An undeclared source has no recorded permission and no
             permitted measurement, so nothing here can say what it may
             ship. Tier does not help: it is a precedence rank, not a
             permission.",
      "i" = "Declared source{?s}: {.val {declared}}.",
      "i" = "A genuinely new source needs a declaration in
             {.fun .admin_source_registry}
             ({.file R/admin_shares_pins.R}), not a widened gate."
    ),
    class = "whep_error_admin_unknown_source"
  )
}

# The consent gate, checked again where the data enters the package. A
# source declared `"public_licence"` is a public product shipping under
# its own licence and has no row here; a source declared `"manifest"` must
# have one, and data shipped without it is the failure this design exists
# to prevent.
.admin_shares_check_consent <- function(
  rows,
  manifest = .admin_shares_manifest()
) {
  .admin_shares_check_declared(rows)
  named <- .admin_source_of(.admin_column(rows, "source", NA_character_))
  ungranted <- setdiff(
    intersect(unique(named), .admin_consented_sources()),
    .admin_shares_consented(manifest)
  )
  if (length(ungranted) == 0L) {
    return(invisible(NULL))
  }
  alias <- .admin_shares_alias()
  cli::cli_abort(
    c(
      "{length(ungranted)} source{?s} in the {.val {alias}} pin
       ha{?s/ve} no consent row: {.val {ungranted}}.",
      "i" = "A source declared {.val manifest} ships under a permission
             recorded in
             {.file inst/extdata/admin_stats_pins_manifest.csv}.",
      "i" = "Rebuild with
             {.file inst/scripts/prepare_admin_shares_pin.R}."
    ),
    class = "whep_error_admin_pin_consent"
  )
}

# The manifest rows that record a DECLARED source's publication consent.
# The manifest is also where the assembled pin's own bookkeeping row lives
# -- its version, md5 and composed attribution -- and that row records a
# permission for nothing. Counting it would let a source calling itself
# `admin-shares` be granted, by its own row, the consent this gate exists
# to demand, and would do the same for every future non-source row added
# to the same file. `"admin-shares"` is not in the registry at all, so it
# is refused as undeclared before it ever reaches this.
.admin_shares_consented <- function(manifest) {
  intersect(manifest$alias, .admin_consented_sources())
}

# The T23 boundary at the seam the assembled pin creates. The family read
# path refuses a `value` COLUMN in the shares-only family's pin
# (`.admin_family_check_consent()`), which is all it needs to: one pin,
# one family. This pin carries one `value` column for every source, so the
# same rule has to be per source and per row -- and it runs in both
# directions, because the relaxation that lets the consented family in is
# source-agnostic and a value family going quietly shares-only would widen
# that consent to a source it was never granted for.
#
# What a row may ship is its DECLARATION's answer, not `rows$source ==
# alias`: an exact match on one column let `"admin-stats-latam "` at tier
# 1, and a `source_id` naming the family while `source` named a consented
# one, carry the withheld values through with nothing raised. The
# declared world is closed first, so a row reaching the two rules below
# always has a permitted measurement to be judged against.
.admin_shares_check_measure <- function(rows) {
  .admin_shares_check_declared(rows)
  measure <- .admin_row_measure(rows)
  .abort_shares_only_values(rows, measure %in% "share")
  .abort_valueless_family(rows, measure %in% "value")
}

# The measurement each row's source is PERMITTED to ship, resolved from
# both identifier columns. `source_id` is the producer's immutable
# identifier in the same value space as `source`
# (`admin_shares_schema()`), so a row naming a shares-only source in
# EITHER column is judged shares-only: fail closed where the two disagree.
# `.admin_shares_check_identity()` refuses a disagreeing pair outright, so
# that only bites when a rule below is called on its own.
.admin_row_measure <- function(rows) {
  named <- .admin_measure_of(.admin_column(rows, "source", NA_character_))
  identified <- .admin_measure_of(
    .admin_column(rows, "source_id", NA_character_)
  )
  measure <- named
  measure[is.na(measure)] <- identified[is.na(measure)]
  measure[identified %in% "share"] <- "share"
  measure
}

# Every label the offending rows travel under, in both columns and as
# written -- a perturbed spelling has to be visible in the message rather
# than folded into the canonical form the rule matched it by.
.admin_offending_labels <- function(rows, offending) {
  sort(unique(c(
    .admin_column(rows, "source", NA_character_)[offending],
    .admin_column(rows, "source_id", NA_character_)[offending]
  )))
}

.abort_shares_only_values <- function(rows, in_family) {
  offending <- in_family & !is.na(rows$value)
  if (!any(offending)) {
    return(invisible(NULL))
  }
  alias <- .admin_shares_alias()
  sources <- .admin_offending_labels(rows, offending)
  found <- sum(offending)
  cli::cli_abort(
    c(
      "{found} row{?s} of {.val {sources}} carry a
       {.field value} in the {.val {alias}} pin.",
      "x" = "Each family named there is consented as derived shares only
             (T23, 2026-09-02); its source values must not be
             redistributed.",
      "i" = "The permission is recorded in
             {.file inst/extdata/admin_stats_pins_manifest.csv}.",
      "i" = "Rebuild with
             {.file inst/scripts/prepare_admin_shares_pin.R}."
    ),
    class = "whep_error_admin_pin_consent_value"
  )
}

.abort_valueless_family <- function(rows, in_family) {
  offending <- in_family & is.na(rows$value)
  if (!any(offending)) {
    return(invisible(NULL))
  }
  alias <- .admin_shares_alias()
  sources <- .admin_offending_labels(rows, offending)
  found <- sum(offending)
  cli::cli_abort(
    c(
      "{found} row{?s} of {.val {sources}} ship no
       {.field value} in the {.val {alias}} pin.",
      "x" = "Each source named there is declared to ship
             {.field value} in {.fun .admin_source_registry}.",
      "i" = "{.field value} is allow-missing for a source declared
             shares-only alone; a value source going quietly shares-only
             widens a consent granted to another source."
    ),
    class = "whep_error_admin_pin_measure"
  )
}

# `utils::read.csv()`, not `data.table::fread()`: the manifest's
# `attribution` column is prose with embedded commas and quotes, and
# `fread()` does not collapse an escaped quote inside a quoted field.
.admin_shares_manifest <- function() {
  path <- system.file(
    "extdata",
    "admin_stats_pins_manifest.csv",
    package = "whep"
  )
  if (!nzchar(path)) {
    return(tibble::tibble(alias = character(), attribution = character()))
  }
  tibble::as_tibble(utils::read.csv(path, stringsAsFactors = FALSE))
}

# `as.character()` around each `ifelse()`: with every family present,
# `absent` is `character(0)` and `ifelse()` returns `logical(0)`, which
# would give the zero-row report two logical columns and a shape its
# callers cannot bind to.
.admin_shares_excluded <- function(rows, manifest = .admin_shares_manifest()) {
  # `.admin_families_named()`, not `unique(rows$source)`: the report has to
  # agree with the gate about which families are in the pin, or a family
  # present under a perturbed label is reported as absent.
  absent <- setdiff(.admin_family_aliases(), .admin_families_named(rows))
  granted <- absent %in% .admin_shares_consented(manifest)
  tibble::tibble(
    source = absent,
    reason = as.character(
      ifelse(granted, "no_rows_in_pin", "no_consent_manifest_row")
    ),
    detail = as.character(
      ifelse(
        granted,
        .admin_shares_absence(absent),
        "absent from inst/extdata/admin_stats_pins_manifest.csv"
      )
    )
  )
}

# Why a consented family contributed no row. The family's attribution used
# to be reported here, which says what permission the family ships under
# and nothing at all about its absence. Where the reason is structural --
# the family's rows cannot enter this contract at any vintage -- it is
# named; otherwise the report points at the only thing that can say which
# filter took the rows, which is the assembly's own per-source counts.
.admin_shares_absence <- function(alias) {
  known <- .admin_family_no_rows_reason()
  ifelse(
    alias %in% names(known),
    known[alias],
    paste0(
      "consent is recorded and no row of this family reached the ",
      "contract; the per-source drop counts are in the assembly report ",
      "of inst/scripts/prepare_admin_shares_pin.R"
    )
  )
}

# The one family whose absence is permanent and explainable from the
# contract itself: it ships head counts, and `indicator_used` closes over
# area, production and yield (`admin_shares_schema()`), so no row of it
# can enter. No count is hardcoded here -- a count would go stale against
# the family pin, and the reason would not.
.admin_family_no_rows_reason <- function() {
  c(
    "admin-stats-france-livestock" = paste0(
      "the family ships head counts only, and head counts are outside ",
      "admin_shares_schema()'s indicator_used vocabulary by design, so ",
      "no row of it can enter this contract; the livestock constraint ",
      "travels the read_admin_family() path instead"
    )
  )
}
