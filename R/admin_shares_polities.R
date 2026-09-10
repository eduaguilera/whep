# NSE globals for admin_shares_polities.R (#1000): the two symbols this
# file adds are alias_source and n_unresolved. ("source",
# "source_native_unit_id" and "level_polity_code" are already declared in
# R/utils.R.)
#
# Resolution at load (#1000, T34).
#
# The admin-statistics readers emit SOURCE-NATIVE identifiers -- a state
# FIPS code, a NUTS code plus its nomenclature version, an IBGE UF code,
# the in-house compilation's own unit id -- and never a WHEP polity code.
# This file is the single place where those identifiers become polity
# codes, and it does so at load time rather than at pin-build time. That
# is what keeps the `admin-shares` pin from ageing against a polities
# snapshot (the plan's whep#905 trap): the pin stores what the source
# said, so a whep-polities re-sync changes the answer on the next read
# instead of leaving a frozen code inside the artifact.
#
# TWO RULES, BOTH LOAD-BEARING:
#
# - Resolution goes through `resolve_polity_label()` on the identifier,
#   under a source slug naming the CODE SYSTEM the identifier belongs to.
#   Never on `source_native_unit_name`, which stays diagnostic: names are
#   what the alias map exists to decide about, and matching them here
#   would be a second authority for the same question
#   (`R/polities.R:1820-1823`). `.add_reporter_polity()`
#   (`R/sources.R:44`) is the precedent -- it asks upstream for
#   source-scoped alias rows rather than name-matching locally.
# - A code system's identifiers mean nothing outside it. "21" is a NUTS
#   code, an IBGE UF and a NASS FIPS code at once, so the slug carries the
#   code system, and for the NUTS systems the nomenclature version too:
#   FR21 is Champagne-Ardenne under NUTS 2013 and, in the tables that
#   recode it, nothing at all under NUTS 2016. A row whose version is
#   missing gets no slug and stays unresolved, rather than borrowing
#   another vintage's meaning.
#
# `aliases` exists because the alias rows these slugs need have not been
# published yet: the shipped `polity_label_aliases` carries 1,007 rows
# over 15 source slugs (checked 2026-09-03), none of them a code system
# named here. Injecting a table lets the fixture tests exercise the exact
# routing the published rows will take. The injected route mirrors
# `resolve_polity_label()`'s ALIAS route only -- no name route, no ISO3
# route -- so a fixture cannot resolve through a path the production call
# would not use.

#' Resolve source-native admin identifiers to polity codes
#'
#' @description
#' Attach `level_polity_code` to administrative-statistics rows by
#' resolving each row's **source-native identifier** -- not its name --
#' through [resolve_polity_label()], under a source slug naming the code
#' system the identifier belongs to and, for NUTS geographies, its
#' nomenclature version. Rows that resolve to nothing keep `NA` and are
#' counted in the returned diagnostics table rather than being dropped.
#'
#' This is the load-time step the admin-shares design puts between the
#' readers, which emit source-native keys only, and every consumer that
#' needs a polity. Resolving here rather than at pin-build time is why the
#' `admin-shares` pin does not need a staleness warning; see below.
#'
#' @section Alias source slugs:
#' `code_system` names the identifier's code system; the slug passed to
#' [resolve_polity_label()] is built per row from it:
#'
#' - `"usda-nass-fips"`: the state FIPS code
#'   [read_admin_stats_nass()] emits. Slug as given.
#' - `"eurostat-nuts"`: the NUTS code [read_admin_stats_eurostat()]
#'   emits. Slug `"eurostat-nuts<nuts_version>"`, e.g.
#'   `"eurostat-nuts2016"`.
#' - `"jrc-nuts"`: the NUTS code of the JRC subnational release
#'   (`inst/extdata/jrc_subnational_source_manifest.csv`), coded on NUTS
#'   2016. Slug `"jrc-nuts<nuts_version>"`.
#' - `"ibge-uf"`: the UF code [read_admin_stats_sidra()] emits. Slug as
#'   given.
#' - `"whep-lab-<family>"`: the compilation's own `admin_unit_id`, as
#'   [read_admin_family()] emits it. Slug as given.
#'
#' The five `"whep-lab-"` slugs are the five tier-2/3 families
#' [read_admin_family()] reads, with the `"admin-stats-"` prefix replaced:
#' `"whep-lab-japan"`, `"whep-lab-spain-provinces"`,
#' `"whep-lab-australia"`, `"whep-lab-france-livestock"` and
#' `"whep-lab-latam"`. Their identifiers are the compilation's own
#' (`"JPN-AICHI"`, `"ESP-ES111"`, `"FRA-FR102"`), which is why each family
#' is its own code system rather than a shared one.
#'
#' Identifiers are normalised exactly as [resolve_polity_label()]
#' normalises a label, on both sides of the comparison, so the injected
#' and published routes agree on what a key is.
#'
#' @section Alias rows this needs from whep-polities:
#' The alias rows are a whep-polities deliverable and are **not published
#' yet**: on the 2026-09-03 snapshot, [polity_label_aliases] holds 1,007
#' rows scoped to 15 sources (`crops-manure-n`, `fao`, `fao1952`,
#' `faostat`, `federico_tena`, `iia`, `iia-cotton`, `iia-tea`, `juan`,
#' `lassaletta-grassland-share`, `mitchell`, `mueller-synthetic-n`,
#' `sa_colonial`, `trade-sources`, `whep-split-2026-06-29`) and none of
#' the code systems above. Until they land, `aliases = NULL` resolves
#' every administrative unit to `NA` -- visibly, and counted -- which is
#' the honest state, and the tests inject the rows they need.
#'
#' One example row per slug, for the fixture countries, in the
#' [polity_label_aliases] schema (`source_label`, `source`, `year_start`,
#' `year_end`, `polity_code`): `"19"` under `"usda-nass-fips"` for the
#' Iowa state polity; `"FR21"` under `"eurostat-nuts2013"` and `"FRF2"`
#' under `"eurostat-nuts2016"`, both for the one Champagne-Ardenne polity
#' the recoding renames; `"FRF2"` under `"jrc-nuts2016"` for the same one;
#' `"35"` under `"ibge-uf"` for the Sao Paulo state polity; `"ESP-ES111"`
#' under `"whep-lab-spain-provinces"` for the NUTS-3 province `ES111`; and
#' `"JPN-AICHI"` under `"whep-lab-japan"` for `JPN-AICHI-1871-2025`, the
#' one target polity that already exists in the shipped [polities].
#'
#' @section Why there is no staleness warning:
#' The `admin-shares` pin is keyed on source-native identifiers and
#' carries no resolved polity code, so nothing in it can disagree with a
#' newer [polities] snapshot: the resolution is redone here on every load.
#' A pin that froze resolved codes would need a
#' `.warn_stale_admin_shares()` guard to say which snapshot it was
#' resolved against; this one has nothing to warn about, which is why the
#' plan chose source-native pinning.
#'
#' @param x Administrative-statistics rows, in the shape the readers emit:
#'   a data frame carrying `source`, `source_native_unit_id`, the year
#'   column named by `year_col`, and `nuts_version` where `code_system`
#'   is a NUTS system. `source_native_unit_name` is never read. The
#'   admin-shares contract ([admin_shares_schema()]) calls the same
#'   identifier `source_native_id`, so a caller resolving that table
#'   renames the column first. An existing `level_polity_code` is
#'   overwritten: this function is the authority on it.
#' @param code_system Code system the identifiers belong to, one of the
#'   slugs in the table above. Length 1, or one value per row of `x`.
#' @param year_col Name of the year column in `x`. Defaults to `"year"`.
#' @param aliases Optional alias table in the [polity_label_aliases]
#'   schema, used **instead of** the package data. `NULL`, the default and
#'   what production calls use, resolves against the published
#'   [polity_label_aliases] through the ALIAS ROUTE ONLY -- never
#'   [resolve_polity_label()] itself, whose name and ISO3 identity routes
#'   would let an administrative identifier that happens to collide with a
#'   polity name or ISO3 code resolve to the wrong thing (usually the
#'   container). An injected table takes the same alias-only route, with
#'   the same source and year scoping, so the two paths agree on
#'   everything but which table they read.
#'
#' @return A list of two tibbles:
#'
#' - `rows`: `x` with `alias_source` (the slug each row was resolved
#'   under, `NA` where none could be built) and `level_polity_code` (the
#'   resolved polity, `NA` where nothing matched).
#' - `diagnostics`: one row per `(source, alias_source)` with `n_rows`,
#'   `n_unresolved` and `example_ids` -- up to five distinct unresolved
#'   identifiers, `"|"`-joined in the C locale, `NA` when the group
#'   resolved fully. Ordered by `n_unresolved`, descending. A row with no
#'   native identifier at all -- a derived residual, in
#'   [admin_shares_schema()]'s terms -- cannot resolve and is counted
#'   here like any other unresolved row, contributing no example id.
#'
#' @export
#'
#' @examples
#' # Two NUTS vintages of one code, resolved under their own slugs. The
#' # alias rows are injected, and their polity code is illustrative: the
#' # real rows are the whep-polities deliverable described above.
#' rows <- tibble::tibble(
#'   source = "Eurostat_apro_cpnhr_h",
#'   source_native_unit_id = c("FR21", "FRF2", "FR83"),
#'   source_native_unit_name = c("Champagne-Ardenne", "idem", "Corse"),
#'   nuts_version = c("2013", "2016", "2013"),
#'   year = c(1995L, 1995L, 1995L)
#' )
#' aliases <- tibble::tibble(
#'   source_label = c("FR21", "FRF2"),
#'   source = c("eurostat-nuts2013", "eurostat-nuts2016"),
#'   year_start = NA_integer_,
#'   year_end = NA_integer_,
#'   polity_code = "FR-CHAMPAGNE-ARDENNE",
#'   common_name = "Champagne-Ardenne",
#'   confidence = "high",
#'   observed_rows = NA_integer_
#' )
#' resolved <- resolve_admin_units(rows, "eurostat-nuts", aliases = aliases)
#' resolved$rows[, c("alias_source", "level_polity_code")]
#' resolved$diagnostics
resolve_admin_units <- function(
  x,
  code_system,
  year_col = "year",
  aliases = NULL
) {
  x <- .admin_check_unit_input(x, year_col)
  code_system <- .admin_code_system_arg(code_system, nrow(x))
  slug <- .admin_alias_slug(code_system, x)
  rows <- dplyr::mutate(
    x,
    alias_source = slug,
    level_polity_code = .resolve_with_aliases(
      x$source_native_unit_id,
      slug,
      x[[year_col]],
      aliases
    )
  )
  list(rows = rows, diagnostics = .admin_unresolved_report(rows))
}

# Code systems and slugs -------------------------------------------------

# The closed vocabulary of code systems. The in-house families are read
# off `read_admin_family()`'s own alias list rather than restated, so a
# family added there cannot be missing here.
.admin_code_systems <- function() {
  families <- sub("^admin-stats-", "", .admin_family_aliases())
  c(
    "usda-nass-fips",
    "eurostat-nuts",
    "jrc-nuts",
    "ibge-uf",
    paste0("whep-lab-", families)
  )
}

# The code systems whose identifiers only mean something together with a
# nomenclature version. Eurostat serves several NUTS vintages inside one
# response and JRC's release is coded on NUTS 2016, so the version is part
# of the key rather than a property of the row.
.admin_versioned_systems <- function() {
  c("eurostat-nuts", "jrc-nuts")
}

.admin_code_system_arg <- function(code_system, n) {
  if (!is.character(code_system) || length(code_system) == 0) {
    cli::cli_abort(
      "{.arg code_system} must be a character vector of code-system slugs."
    )
  }
  if (length(code_system) != 1L && length(code_system) != n) {
    cli::cli_abort(
      "{.arg code_system} must be length 1 or {n}, not
       {length(code_system)}."
    )
  }
  code_system <- rlang::arg_match(
    code_system,
    .admin_code_systems(),
    multiple = TRUE
  )
  if (length(code_system) == 1L) rep(code_system, n) else code_system
}

# The per-row slug. A versioned system with no version on the row yields
# `NA`, never the bare system: resolving FR21 under "eurostat-nuts" would
# silently pick whichever vintage upstream happened to scope its alias to.
.admin_alias_slug <- function(code_system, x) {
  versioned <- code_system %in% .admin_versioned_systems()
  if (!any(versioned)) {
    return(code_system)
  }
  if (!rlang::has_name(x, "nuts_version")) {
    cli::cli_abort(c(
      "{.arg x} must carry {.field nuts_version} for code system{?s}
       {.val {unique(code_system[versioned])}}.",
      i = "A NUTS identifier means nothing without its nomenclature
           version."
    ))
  }
  version <- as.character(x$nuts_version)
  dplyr::case_when(
    !versioned ~ code_system,
    !is.na(version) ~ paste0(code_system, version),
    .default = NA_character_
  )
}

.admin_check_unit_input <- function(x, year_col) {
  if (!is.data.frame(x)) {
    cli::cli_abort("{.arg x} must be a data frame.")
  }
  if (!rlang::is_string(year_col)) {
    cli::cli_abort("{.arg year_col} must be a single column name.")
  }
  required <- c("source", "source_native_unit_id", year_col)
  missing <- setdiff(required, names(x))
  if (length(missing) > 0) {
    cli::cli_abort(
      "{.arg x} is missing column{?s} {.field {missing}}."
    )
  }
  if (!is.numeric(x[[year_col]])) {
    cli::cli_abort(
      "{.arg x}'s {.field {year_col}} must be numeric, not
       {.cls {class(x[[year_col]])}}."
    )
  }
  tibble::as_tibble(x)
}

# Resolution -------------------------------------------------------------

# Resolve identifiers under their slugs, either through the published
# alias map (`aliases = NULL`, the production path) or through an injected
# table. Distinct `(label, source, year)` triples are resolved once and
# spread back, the way `.add_reporter_polity()` resolves its reporters
# once per year: a NASS dump repeats 50 identifiers over millions of rows.
.resolve_with_aliases <- function(label, source, year, aliases) {
  keys <- tibble::tibble(
    label = as.character(label),
    source = as.character(source),
    year = as.integer(year)
  )
  group <- vctrs::vec_group_id(keys)
  first <- !duplicated(group)
  codes <- .resolve_unique_keys(keys[first, , drop = FALSE], aliases)
  codes[match(group, group[first])]
}

.resolve_unique_keys <- function(keys, aliases) {
  # `aliases = NULL` (the production path) resolves against the SAME
  # published table `resolve_polity_label()` reads, `polity_label_aliases`,
  # but through the alias route ONLY -- never `resolve_polity_label()`
  # itself, whose name and ISO3 identity routes would let an
  # administrative identifier that happens to collide with a polity name
  # or ISO3 code resolve to the wrong thing (usually the container). See
  # the file banner and finding #1000/T34-3.
  aliases <- .assert_alias_table(aliases %||% polity_label_aliases)
  alias_key <- .norm_polity_label(aliases$source_label)
  label_key <- .norm_polity_label(keys$label)
  vapply(
    seq_along(label_key),
    function(i) {
      .alias_route_one(
        label_key[i],
        keys$source[i],
        keys$year[i],
        aliases,
        alias_key
      )
    },
    character(1)
  )
}

.assert_alias_table <- function(aliases) {
  if (!is.data.frame(aliases)) {
    cli::cli_abort(
      "{.arg aliases} must be a data frame in the
       {.code polity_label_aliases} schema, or {.code NULL}."
    )
  }
  required <- c(
    "source_label",
    "source",
    "year_start",
    "year_end",
    "polity_code"
  )
  missing <- setdiff(required, names(aliases))
  if (length(missing) > 0) {
    cli::cli_abort(c(
      "{.arg aliases} is missing column{?s} {.field {missing}}.",
      i = "See {.code polity_label_aliases} for the schema."
    ))
  }
  aliases
}

# THE ALIAS ROUTE, AND ONLY THE ALIAS ROUTE. `resolve_polity_label()`
# falls back to the polity's own name and ISO3 code when no alias
# applies; an injected table must not, because those routes read the
# package's `polities` table and would let a fixture resolve through a
# path the injected rows do not describe. No match is `NA`, which is the
# same answer the published map gives today.
.alias_route_one <- function(key, source, year, aliases, alias_key) {
  if (is.na(key)) {
    return(NA_character_)
  }
  cand <- aliases[which(alias_key == key), , drop = FALSE]
  cand <- .alias_scope_filter(cand, source, year)
  if (nrow(cand) == 0L) {
    return(NA_character_)
  }
  as.character(cand$polity_code[.alias_specificity_order(cand)[1]])
}

# A source-scoped alias applies only to that source; an unscoped one
# applies to any, exactly as `resolve_polity_label()` reads them. A
# year-scoped alias applies only inside its range, and A MISSING BOUND IS
# UNBOUNDED ON THAT SIDE rather than unscoped on both.
.alias_scope_filter <- function(cand, source, year) {
  scoped_source <- .alias_is_source_scoped(cand)
  keep <- !scoped_source | (!is.na(source) & cand$source == source)
  cand <- cand[keep, , drop = FALSE]
  scoped_year <- .alias_is_year_scoped(cand)
  in_range <- !scoped_year |
    (!is.na(year) &
      year >= .alias_bound(cand$year_start, -Inf) &
      year <= .alias_bound(cand$year_end, Inf))
  cand[in_range, , drop = FALSE]
}

# Most specific first: year-scoped, then source-scoped, then the narrower
# span. The same ordering `resolve_polity_label()` applies, so both sides
# pick the same rule when several match.
.alias_specificity_order <- function(cand) {
  scoped_year <- .alias_is_year_scoped(cand)
  span <- .alias_bound(cand$year_end, Inf) -
    .alias_bound(cand$year_start, -Inf)
  order(
    -(2L * as.integer(scoped_year) + as.integer(.alias_is_source_scoped(cand))),
    span
  )
}

.alias_is_source_scoped <- function(cand) {
  !is.na(cand$source) & cand$source != ""
}

.alias_is_year_scoped <- function(cand) {
  !is.na(cand$year_start) | !is.na(cand$year_end)
}

.alias_bound <- function(bound, unbounded) {
  ifelse(is.na(bound), unbounded, as.numeric(bound))
}

# Diagnostics ------------------------------------------------------------

.admin_unresolved_report <- function(rows) {
  rows |>
    dplyr::summarise(
      n_rows = dplyr::n(),
      n_unresolved = sum(is.na(level_polity_code)),
      example_ids = .admin_example_ids(
        source_native_unit_id[is.na(level_polity_code)]
      ),
      .by = c(source, alias_source)
    ) |>
    dplyr::arrange(dplyr::desc(n_unresolved), source, alias_source)
}

# Up to `max_ids` distinct identifiers, "|"-joined in the C locale, the
# convention `.admin_join_unique()` uses in `R/admin_shares_resolve.R` so
# the table writes to parquet and CSV unchanged.
.admin_example_ids <- function(ids, max_ids = 5L) {
  present <- sort(unique(as.character(ids[!is.na(ids)])), method = "radix")
  if (length(present) == 0L) {
    return(NA_character_)
  }
  paste(present[seq_len(min(length(present), max_ids))], collapse = "|")
}
