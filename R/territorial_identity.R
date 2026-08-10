#' Which territorial identity WHEP's year-less objects carry
#'
#' @description
#' A WHEP polity code is year-scoped: `ESP-1846-1914` and `ESP-1978-2025` are
#' different rows of [polities]. Several WHEP objects have no year dimension at
#' all -- the cell/area grids, the static crop-pattern weights, coefficient
#' tables keyed by a country label -- so "attach the polity code" has no single
#' answer for them. This is the register of the answer each one carries, so
#' that the choice is stated rather than left to whichever join a consumer
#' happens to write.
#'
#' It decides nothing at call time: the table is a declaration, and
#' `tests/testthat/test_territorial_identity.R` checks every row of it against
#' the object it describes, including that no year-less territory-keyed dataset
#' is missing from it.
#'
#' @details
#' # The three conventions
#'
#' \describe{
#'   \item{`"present_day_polity"`}{The object is a registry of what reports
#'     *today*, so the present-day polity is what it means. Its `carrier`
#'     column holds a real [polities] code, the one
#'     `add_polity_code(year_column = NULL)` resolves -- the crosswalk period
#'     running to the open end.}
#'   \item{`"polity_period"`}{One row of the object covers territory that
#'     changed hands, so the object needs a validity interval and has to become
#'     year-aware. `build_polycell_support()` on the
#'     `edu/polycell-spatial-support` branch is the worked precedent for the
#'     grid: one row per (cell, polity, validity interval).}
#'   \item{`"identity_free"`}{The object is a coefficient about a *place*, or a
#'     label whose referent changes with the year it is applied in, so carrying
#'     one polity code would be false precision. Identity is attached at the
#'     consumer, with the consumer's year, through the `resolver` call.}
#' }
#'
#' # Why a year-less object cannot simply be handed a code
#'
#' Measured on the deployed `spatialize-country-grid` pin (58,795 cells, 178
#' area codes, no year): **52,420 cells, 89.2%, sit under an `area_code` that
#' [polity_area_crosswalk] maps to more than one polity over time**, so which
#' polity a cell belongs to is a function of the year the grid does not have.
#' The same holds for country labels resolved through [resolve_polity_label()]:
#' 33 of [mueller_synthetic_n]'s 156 `iso3c` values, 37 of [crops_manure_n]'s
#' 184 `ISO` values and 38 of `gleam_geographic_hierarchy`'s 204 `iso3` values
#' name a *different* polity at 1961 than at 2020.
#'
#' # What counts as a territory here
#'
#' A supra-national statistical grouping -- an IPCC or GLEAM region, a UN
#' sub-region, `region_krausmann` -- is **not** a territory and is out of scope
#' by construction: it names a class of places, never a state, so it can never
#' acquire a polity code. Only the column names in
#' `whep:::.territory_key_names()` put an object in this register. That is also
#' why a bare `code` is not one of them: `biomass_coefs$Code` is a crop.
#'
#' # The trap this register exists to make visible
#'
#' [regions_full] and [polities_cats] carry a column of ISO3-like stems
#' (`"AFG"`, `"ROW"`, `"RAFR"`) kept for older callers, of which **not one value
#' is a [polities] code**. Until whep#687 that column was literally named
#' `polity_code`, so a join from either dataset to [polities] or
#' [polity_area_crosswalk] on the one column whose name promised identity came
#' back empty and nothing warned. It is now `legacy_polity_prefix`, which claims
#' nothing. Their real carrier is `reporting_polity_code`, a [polities] code on
#' all 259 of `regions_full`'s non-`NA` rows and all 198 of `polities_cats`'s.
#'
#' @param kind Optional character vector restricting the result to one or more
#'   of `"package_data"`, `"input_pin"` and `"function_output"`. `NULL`
#'   (default) returns every row.
#'
#' @returns A `tibble` with one row per registered object and the columns:
#' - `object`: The dataset name, pin alias or function it describes.
#' - `kind`: `"package_data"`, `"input_pin"` or `"function_output"`.
#' - `territory_key`: The column(s) naming a territory, comma-separated, or
#'   `NA` when the object names none.
#' - `identity`: `"present_day_polity"`, `"polity_period"` or
#'   `"identity_free"`.
#' - `status`: `"carried"` when the object materialises the identity today,
#'   `"resolved_by_consumer"` when the consumer attaches it with its own year,
#'   `"recommended"` when this is the proposed convention and not yet the
#'   implemented one.
#' - `carrier`: The column holding the polity code, `NA` when there is none.
#' - `resolver`: The supported call that attaches identity, `NA` when none is
#'   needed.
#' - `rationale`: One sentence saying why.
#'
#' @seealso [add_polity_code()] for the numeric-code route,
#'   [resolve_polity_label()] for the label route, and [whep_polity_columns]
#'   for what a year-*aware* WHEP output carries.
#' @export
#'
#' @examples
#' polity_identity_conventions() |>
#'   dplyr::select(object, kind, identity, status)
#'
#' polity_identity_conventions(kind = "package_data") |>
#'   dplyr::select(object, carrier, resolver)
polity_identity_conventions <- function(kind = NULL) {
  registry <- .yearless_identity_registry()
  if (is.null(kind)) {
    return(registry)
  }
  known <- unique(registry$kind)
  unknown <- setdiff(kind, known)
  if (length(unknown) > 0) {
    cli::cli_abort(c(
      "{.arg kind} must name a registered kind.",
      "x" = "Unknown: {.val {unknown}}.",
      "i" = "Known kinds: {.val {known}}."
    ))
  }
  wanted <- kind
  registry |>
    dplyr::filter(.data$kind %in% wanted)
}

# --- The register itself ------------------------------------------------------

# One row per year-less WHEP object that names, or could name, a territory.
# Kept as data rather than prose so the guards in
# tests/testthat/test_territorial_identity.R can check every claim against the
# object it is made about, and so a new year-less territory-keyed dataset
# cannot arrive without an answer.
.yearless_identity_registry <- function() {
  dplyr::bind_rows(
    .yearless_identity_data(),
    .yearless_identity_inputs()
  ) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), .squish_registry_text))
}

# Registries and coefficient tables shipped as package data.
.yearless_identity_data <- function() {
  tibble::tribble(
    ~object,
    ~kind,
    ~territory_key,
    ~identity,
    ~status,
    ~carrier,
    ~resolver,
    ~rationale,
    "regions_full",
    "package_data",
    "legacy_polity_prefix, polity_name, iso3c, uISO3c, polity_area_code,
     reporting_polity_code",
    "present_day_polity",
    "carried",
    "reporting_polity_code",
    "add_polity_code(year_column = NULL)",
    "A registry of the areas that report today, so the present-day polity is
     what it means; legacy_polity_prefix is an ISO3-like stem and is not one.",
    "polities_cats",
    "package_data",
    "legacy_polity_prefix, polity_name, iso3c, uISO3c, polity_area_code,
     reporting_polity_code",
    "present_day_polity",
    "carried",
    "reporting_polity_code",
    "add_polity_code(year_column = NULL)",
    "The sovereign-country subset of regions_full, derived from it, so it
     carries the same present-day identity.",
    "gleam_geographic_hierarchy",
    "package_data",
    "iso3, country, reporting_polity_code",
    "present_day_polity",
    "carried",
    "reporting_polity_code",
    "resolve_polity_label(iso3, year = )",
    "GLEAM's own present-day country registry, the same shape as regions_full
     and the same answer; it carries the polity the present-day year resolves
     its iso3 to, NA for the three territories upstream has no polity for.",
    "mueller_synthetic_n",
    "package_data",
    "iso3c",
    "identity_free",
    "resolved_by_consumer",
    NA_character_,
    "resolve_polity_label(iso3c, source = 'mueller-synthetic-n', year = )",
    "A crop N application rate per country label; 33 of its 156 labels name a
     different polity at 1961 than at 2020, so the year it is applied in
     decides the polity and the table cannot.",
    "crops_manure_n",
    "package_data",
    "ISO",
    "identity_free",
    "resolved_by_consumer",
    NA_character_,
    "resolve_polity_label(ISO, source = 'crops-manure-n', year = )",
    "A manure N total per crop and country label; 37 of its 184 labels name a
     different polity at 1961 than at 2020, so identity belongs at the
     consumer's year.",
    "gleam_dressing_percentages",
    "package_data",
    "country",
    "identity_free",
    "resolved_by_consumer",
    NA_character_,
    "resolve_polity_label(country, year = )",
    "A GLEAM carcass yield per country, applied across WHEP's whole historical
     span, so the polity is the one live in the year it is applied in.",
    "gleam_fracremove",
    "package_data",
    "country",
    "identity_free",
    "resolved_by_consumer",
    NA_character_,
    "resolve_polity_label(country, year = )",
    "A GLEAM crop-residue removal fraction per country, applied across WHEP's
     whole historical span, so the polity is the one live in the year it is
     applied in.",
    "gleam_mechanization_levels",
    "package_data",
    "country",
    "identity_free",
    "resolved_by_consumer",
    NA_character_,
    "resolve_polity_label(country, year = )",
    "A GLEAM mechanisation share per country and feed material, applied across
     WHEP's whole historical span, so the polity is the one live in the year it
     is applied in."
  )
}

# Deployed inputs and the functions that read them.
.yearless_identity_inputs <- function() {
  tibble::tribble(
    ~object,
    ~kind,
    ~territory_key,
    ~identity,
    ~status,
    ~carrier,
    ~resolver,
    ~rationale,
    "spatialize-crop-patterns",
    "input_pin",
    NA_character_,
    "identity_free",
    "carried",
    NA_character_,
    NA_character_,
    "Keyed on (lon, lat, item_prod_code) only: a cell's crop composition is a
     property of the place, not of a state, and giving it a territory would be
     false precision as well as a key it does not need.",
    "spatialize-country-grid",
    "input_pin",
    "area_code",
    "polity_period",
    "recommended",
    NA_character_,
    NA_character_,
    "58,795 cells against 178 area codes with no year, 89.2% of them under an
     area that held more than one polity, so the unit is (cell, polity,
     validity interval) rather than a code added to the cell.",
    "build_cell_polity",
    "function_output",
    "area_code",
    "polity_period",
    "recommended",
    NA_character_,
    NA_character_,
    "The same grid from the other pin; its area_key argument already chooses
     which reporting vocabulary it speaks, and the polity needs the validity
     interval a polycell support table gives it."
  )
}

# The tribbles above are wrapped to 80 columns, which puts newlines and runs of
# spaces inside the long strings. Squish once here rather than making every
# consumer do it.
.squish_registry_text <- function(x) {
  ifelse(is.na(x), NA_character_, stringr::str_squish(x))
}

# The year "the present day" means when a LABEL is resolved to its present-day
# polity, i.e. the year `resolve_polity_label()` has to be asked about for the
# open period to be the one it answers with.
#
# The numeric route needs no such year: `add_polity_code(year_column = NULL)`
# goes through `.current_area_lookup()`, which picks the period reaching the
# crosswalk's open-period sentinel, `max(polity_end_year)`. The label route has
# no equivalent, and its year filter is `start_year <= y < end_year` with no
# open-end exception -- whep#577's "inclusive at an open end" rule lives in
# `.polity_join_end_year()`, on the crosswalk route only, which is whep#712.
# MEASURED on the
# shipped snapshot: of `gleam_geographic_hierarchy`'s 204 iso3 values,
# `resolve_polity_label()` answers for 201 at the sentinel minus one and for
# ONE at the sentinel itself, because 227 live polities end there.
#
# So the present day is the last year the open period covers, derived from the
# data rather than written down: the sentinel moves when the snapshot does, and
# `sentinel - 1` keeps resolving to the same open period whether or not the
# label route ever gains the inclusive-open-end rule.
.present_day_polity_year <- function() {
  as.integer(max(polities$end_year, na.rm = TRUE)) - 1L
}

# The column names that make a table say which *territory* a row is about.
# Deliberately an explicit list rather than a pattern. A bare "code" matches
# item codes -- biomass_coefs$Code is a crop -- and a supra-national grouping
# such as `region`, `gleam_region` or `continent` names a class of places
# rather than a state, so it can never carry a polity code and is out of scope.
.territory_key_names <- function() {
  c(
    "area",
    "area_code",
    "area_name",
    "iso",
    "iso3",
    "iso3c",
    "uiso3c",
    "country",
    "country_code",
    "polity_code",
    # Names a territory without claiming to be an identity (whep#687). Listed
    # so the register keeps covering the column the rename moved.
    "legacy_polity_prefix",
    "polity_name",
    "polity_area_code",
    "reporting_polity_code"
  )
}

.is_territory_key <- function(columns) {
  tolower(columns) %in% .territory_key_names()
}

# Every dataset this package exposes that names a territory and has no year to
# scope it with. This is what the register has to cover: a dataset of this
# shape arriving without a declared identity is exactly the silent choice #458
# forbids.
.yearless_territorial_datasets <- function() {
  names <- .exposed_dataset_names()
  keys <- purrr::map(names, .territorial_keys_if_yearless)
  keep <- purrr::map_lgl(keys, ~ length(.x) > 0)
  stats::setNames(keys[keep], names[keep])
}

.territorial_keys_if_yearless <- function(name) {
  value <- .exposed_dataset(name)
  if (!is.data.frame(value)) {
    return(character())
  }
  columns <- names(value)
  if (any(grepl("year", columns, ignore.case = TRUE))) {
    return(character())
  }
  columns[.is_territory_key(columns)]
}

# `utils::data()` indexes the .rda FILES, so it cannot see the 45 coefficient
# tables that share data/livestock_coefs.rda; the lazy-data environment holds
# every object a caller can actually reach. Union both so neither route's blind
# spot can hide a dataset from the register.
.exposed_dataset_names <- function() {
  listed <- utils::data(package = "whep")$results
  from_index <- if (is.null(listed)) character() else listed[, "Item"]
  lazy <- .getNamespaceInfo(asNamespace("whep"), "lazydata")
  from_lazy <- if (is.null(lazy)) character() else ls(lazy)
  sort(unique(sub("\\s.*$", "", c(from_index, from_lazy))))
}

.exposed_dataset <- function(name) {
  value <- try(get(name, envir = asNamespace("whep")), silent = TRUE)
  if (inherits(value, "try-error")) NULL else value
}
