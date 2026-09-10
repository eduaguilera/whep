# IBGE SIDRA admin-statistics reader (Brazil, state grain). Every fact below
# was verified live against the unauthenticated API on 2026-09-02; each probe
# URL is quoted so it can be re-run without a key.
#
# - Endpoint: https://apisidra.ibge.gov.br/values/t/{table}/n3/all/
#   v/{variables}/p/{years}[/c{class}/all]?formato=json. Territorial level N3
#   is the "Unidade da Federacao" (state), so the served grain is admin1.
# - Response shape: a JSON array whose FIRST element is a header object
#   mapping the short keys to Portuguese labels; every later element is one
#   value row carrying those keys -- D1C/D1N unit code/name, D2C/D2N variable,
#   D3C/D3N year, D4C/D4N classification category (absent when the table has
#   no classification, e.g. table 94), MC/MN unit of measure, V the value.
# - Tables, from https://servicodados.ibge.gov.br/api/v3/agregados/{table}/
#   metadados: 5457 (PAM, annual 1974-2024) with variables 8331 "area
#   plantada ou destinada a colheita" (Hectares), 216 "area colhida"
#   (Hectares) and 214 "quantidade produzida" (Toneladas), classification 782
#   with 72 categories of which id 0 is "Total", not a crop; 3939 (PPM herds,
#   annual 1974-2024) with variable 105 (Cabecas) and classification 79
#   holding ten herd types; 94 (vacas ordenhadas, annual 1974-2024) with
#   variable 107 (Cabecas) and no classification.
# - Units are served in the units WHEP uses, so the conversion factor is 1 in
#   all three cases and no conversion is applied: MC 1006 Hectares -> "ha",
#   MC 1017 Toneladas -> "tonnes", MC 24 Cabecas -> "heads". The reader still
#   checks the served unit code against the variable and aborts if it changes.
# - Value cap: the API refuses an oversized query with HTTP 400 and the body
#   "Quantidade de valores solicitados: 198288 excedeu o limite: 50000"
#   (probe: t/5457/n3/all/v/216,214/p/1974-2024/c782/all). whep#1000 quotes a
#   100,000 cap; 50,000 is what the server enforces today, so the year-block
#   sizes in `.sidra_table_spec()` are computed against 50,000.
# - Special value codes, kept verbatim in `value_flag`: "-" is an absolute
#   zero (parsed to 0 with no flag, it being the only way SIDRA writes a
#   zero), "..." not available, ".." not applicable, "X" withheld to protect
#   the informant. "-", "..." and ".." all occur in the shipped fixture
#   (tests/testthat/fixtures/sidra_5457_pam_states.json); "X" does not, and is
#   exercised from a row constructed in the test file.
# - Planted area (8331) is published only from 1988: in 1985 every state
#   returns "..." while 216 returns values (probe: t/5457/n3/all/
#   v/8331,216/p/1985,1988,1990/c782/40118).
# - SIDRA publishes no vintage, edition or last-update stamp on either the
#   values or the metadata endpoint, so `source_version` is NA and
#   `recorded_at` carries the fetch time of the block the row came from.
# - Unit ids are the 2-digit IBGE "Divisao Territorial Brasileira" UF codes
#   served in D1C (11 Rondonia, 17 Tocantins, 35 Sao Paulo, 41 Parana,
#   43 Rio Grande do Sul, ... 53 Distrito Federal).
# - Years the table has not published are dropped silently by the API rather
#   than refused (probe: p/2024,2025,2026 returned only 2024), so a requested
#   year absent from the result means "not published", not "zero".
# - Open for the item and species vocabulary decision (T10, whep#1000): PPM
#   classification 79 reports Caprino (2681) and Ovino (2677) separately
#   where WHEP's `species_group` has one `sheep_goats` class, so the two are
#   summed there, not here. It also carries two total/subset pairs --
#   "Galinaceos - total" (32796) with "Galinaceos - galinhas" (32793), and
#   "Suino - total" (32794) with "Suino - matrizes de suinos" (32795) --
#   where reading both members as one species doubles the herd. Whether
#   galinhas is WHEP's `layers` (leaving total minus galinhas as
#   `broilers`) is a mapping claim about the source's definition and is not
#   asserted here. All of it is left source-native for T10 to decide.

#' Read Brazilian state agricultural statistics from IBGE SIDRA
#'
#' @description
#' Read one IBGE SIDRA table of Brazilian state-level (`admin1`) agricultural
#' statistics through the unauthenticated SIDRA values API and return it in
#' the shared admin-statistics reader shape: one row per source-native unit,
#' item, indicator and year, in WHEP units.
#'
#' Three tables are served, all annual from 1974:
#'
#' - `"5457"` -- Producao Agricola Municipal (PAM) crop areas and production.
#'   Harvested area (`indicator_used == "area_harvested"`) is the indicator
#'   the allocation binds on; planted-or-sown area
#'   (`"area_planted_or_sown"`) is a flagged proxy SIDRA publishes only from
#'   1988, and production (`"production"`) is the last-resort indicator.
#' - `"3939"` -- Pesquisa da Pecuaria Municipal (PPM) herd inventories, in
#'   head. Livestock rows carry `quantity == "heads"` and
#'   `indicator_used == NA`, head counts not being one of the
#'   area/production/yield indicators [admin_shares_schema()] closes over.
#' - `"94"` -- milked cows, in head, the dairy split PPM table 3939 does not
#'   report.
#'
#' Rows are **source-native**: unit and item identifiers are returned exactly
#' as SIDRA served them and are never mapped to `item_prod_code` or resolved
#' to a polity here. Values are returned in WHEP units with a conversion
#' factor of 1 throughout, SIDRA serving hectares, tonnes and head directly.
#'
#' Requests are paged by year block so no single query exceeds the API's
#' 50,000-value cap; see `years_per_request`.
#'
#' @param table SIDRA table to read, one of `"5457"` (PAM crops), `"3939"`
#'   (PPM herds) or `"94"` (milked cows).
#' @param years Integer vector of calendar years. `NULL` (the default)
#'   requests 1974 -- the first year all three tables publish -- through the
#'   current year; years the table has not published are absent from the
#'   result rather than an error.
#' @param items Optional character vector of classification category codes to
#'   restrict the request to (`c782` codes for table `"5457"`, `c79` codes for
#'   `"3939"`). `NULL` requests every category and drops the `"0"` ("Total")
#'   category, which is the all-crops total rather than an item. Must be
#'   `NULL` for table `"94"`, which has no classification.
#' @param years_per_request Number of years per API request. `NULL` uses the
#'   per-table default that keeps a request under the 50,000-value cap.
#' @param example If `TRUE`, return a small fixture instead of reading from
#'   the API. Defaults to `FALSE`.
#'
#' @return A tibble with one row per unit, item, indicator and year:
#'
#' - `source`: `"IBGE_PAM"` for table 5457, `"IBGE_PPM"` for 3939 and 94.
#' - `source_native_unit_id`, `source_native_unit_name`: the IBGE UF code and
#'   name exactly as served.
#' - `source_native_item_code`, `source_native_item_name`: the classification
#'   category exactly as served; the code is `NA` for table 94, whose item is
#'   the table itself and whose name is the served variable name.
#' - `indicator_used`: `"area_harvested"`, `"area_planted_or_sown"` or
#'   `"production"` for crop rows, `NA` for head counts.
#' - `quantity`: `"area"`, `"production"` or `"heads"`.
#' - `year`: calendar year.
#' - `value`: the served value in WHEP units, `NA` where the source gave a
#'   missing-value code.
#' - `value_unit`: `"ha"`, `"tonnes"` or `"heads"`.
#' - `value_flag`: the source's missing-value code verbatim (`"..."`, `".."`
#'   or `"X"`), `NA` when the row carries a value. An absolute zero (`"-"`)
#'   is a value, not a flag.
#' - `grain`: always `"admin1"`; SIDRA level N3 is the state.
#' - `nuts_version`: always `NA`, Brazil having no NUTS geography.
#' - `source_version`: always `NA`; SIDRA exposes no vintage stamp.
#' - `recorded_at`: ISO 8601 UTC fetch time of the request the row came from.
#'
#' @source IBGE SIDRA API (<https://apisidra.ibge.gov.br>), tables 5457
#'   (Producao Agricola Municipal), 3939 and 94 (Pesquisa da Pecuaria
#'   Municipal). Unauthenticated; verified 2026-09-02.
#'
#' @export
#'
#' @examples
#' read_admin_stats_sidra(example = TRUE)
read_admin_stats_sidra <- function(
  table = c("5457", "3939", "94"),
  years = NULL,
  items = NULL,
  years_per_request = NULL,
  example = FALSE
) {
  if (example) {
    return(.example_admin_stats_sidra())
  }
  table <- rlang::arg_match(table)
  spec <- .sidra_table_spec(table)
  requested <- .sidra_resolve_years(years)
  .sidra_check_items(items, spec)
  .sidra_warn_planted(table, requested)

  .sidra_year_blocks(requested, years_per_request %||% spec$block) |>
    purrr::map(\(block) .sidra_read_block(table, block, items)) |>
    purrr::list_rbind() |>
    .sidra_drop_total_category(items) |>
    dplyr::arrange(
      year,
      source_native_unit_id,
      source_native_item_code,
      variable_code
    ) |>
    dplyr::select(-"variable_code")
}

# --- Request construction ------------------------------------------------

.sidra_base_url <- function() {
  "https://apisidra.ibge.gov.br/values"
}

# The API's own refusal names the cap: "Quantidade de valores solicitados:
# 198288 excedeu o limite: 50000" (HTTP 400, observed 2026-09-02).
.sidra_value_cap <- function() {
  50000L
}

# First year all three tables publish, from each table's metadata
# `periodicidade$inicio` (verified 2026-09-02).
.sidra_first_year <- function() {
  1974L
}

# Planted-or-sown area (variable 8331) is published only from this year;
# earlier years return "..." for every state while harvested area does not.
.sidra_planted_first_year <- function() {
  1988L
}

# Per-table request specification. `block` is the default number of years per
# request, sized against `.sidra_value_cap()`: a request costs
# units x categories x variables x years values, and SIDRA serves 27 states.
# - 5457: 27 x 72 x 3 = 5,832 per year, so 5 years = 29,160 values, leaving
#   room for the product list to grow by ~70% before the cap binds.
# - 3939: 27 x 10 x 1 = 270 per year, so 25 years = 6,750 values.
# - 94: 27 x 1 x 1 = 27 per year, so 25 years = 675 values.
.sidra_table_spec <- function(table) {
  specs <- list(
    "5457" = list(
      source = "IBGE_PAM",
      class_id = "782",
      variables = c("8331", "216", "214"),
      block = 5L
    ),
    "3939" = list(
      source = "IBGE_PPM",
      class_id = "79",
      variables = "105",
      block = 25L
    ),
    "94" = list(
      source = "IBGE_PPM",
      class_id = NA_character_,
      variables = "107",
      block = 25L
    )
  )
  specs[[table]]
}

# Served variable -> WHEP indicator, quantity, unit and the SIDRA unit code
# that variable must arrive with (MC 1006 Hectares, 1017 Toneladas,
# 24 Cabecas). Head counts carry no `indicator_used`: the closed vocabulary
# in `admin_shares_schema()` covers areas, production and yield only.
.sidra_variable_spec <- function() {
  tibble::tribble(
    ~variable_code, ~indicator_used, ~quantity, ~value_unit, ~sidra_unit_code,
    "216", "area_harvested", "area", "ha", "1006",
    "8331", "area_planted_or_sown", "area", "ha", "1006",
    "214", "production", "production", "tonnes", "1017",
    "105", NA_character_, "heads", "heads", "24",
    "107", NA_character_, "heads", "heads", "24"
  )
}

.sidra_missing_codes <- function() {
  c("...", "..", "X")
}

.sidra_resolve_years <- function(years) {
  resolved <- years %||%
    seq.int(.sidra_first_year(), as.integer(format(Sys.Date(), "%Y")))
  if (!is.numeric(resolved) || length(resolved) == 0 || anyNA(resolved)) {
    cli::cli_abort(
      "{.arg years} must be a non-missing numeric vector, or {.code NULL}."
    )
  }
  sort(unique(as.integer(resolved)))
}

.sidra_check_items <- function(items, spec) {
  if (is.null(items)) {
    return(invisible(NULL))
  }
  if (is.na(spec$class_id)) {
    cli::cli_abort(c(
      "This SIDRA table has no classification, so {.arg items} cannot be set.",
      i = "Pass {.code items = NULL} for table {.val 94} (milked cows)."
    ))
  }
  if (!is.character(items) || length(items) == 0) {
    cli::cli_abort("{.arg items} must be a character vector, or {.code NULL}.")
  }
  invisible(NULL)
}

.sidra_warn_planted <- function(table, years) {
  first_year <- .sidra_planted_first_year()
  early <- years[years < first_year]
  if (table != "5457" || length(early) == 0) {
    return(invisible(NULL))
  }
  cli::cli_warn(c(
    "SIDRA publishes planted-or-sown area only from {.val {first_year}}, so
     {length(early)} requested year{?s} cannot carry it.",
    i = "Those rows come back with {.field value} {.code NA} and
      {.field value_flag} {.val ...}; harvested area covers them."
  ))
}

.sidra_year_blocks <- function(years, block_size) {
  if (!is.numeric(block_size) || length(block_size) != 1 || block_size < 1) {
    cli::cli_abort("{.arg years_per_request} must be a positive number.")
  }
  years <- sort(unique(as.integer(years)))
  unname(split(years, ceiling(seq_along(years) / as.integer(block_size))))
}

.sidra_values_url <- function(table, years, items = NULL) {
  spec <- .sidra_table_spec(table)
  path <- paste(
    .sidra_base_url(),
    "t",
    table,
    "n3",
    "all",
    "v",
    paste(spec$variables, collapse = ","),
    "p",
    paste(years, collapse = ","),
    sep = "/"
  )
  if (!is.na(spec$class_id)) {
    categories <- if (is.null(items)) "all" else paste(items, collapse = ",")
    path <- paste0(path, "/c", spec$class_id, "/", categories)
  }
  paste0(path, "?formato=json")
}

# The only network call in this file; tests stub it.
.fetch_sidra <- function(url) {
  response <- httr::GET(url)
  if (httr::http_error(response)) {
    body <- httr::content(response, as = "text", encoding = "UTF-8")
    cli::cli_abort(c(
      "SIDRA request failed ({httr::status_code(response)}).",
      i = "{.url {url}}",
      x = "{.val {body}}",
      i = "A cap refusal names the limit; lower {.arg years_per_request}."
    ))
  }
  httr::content(response, as = "parsed", type = "application/json")
}

# --- Parsing -------------------------------------------------------------

.sidra_read_block <- function(table, years, items) {
  spec <- .sidra_table_spec(table)
  payload <- .fetch_sidra(.sidra_values_url(table, years, items))
  recorded_at <- .format_evidence_stamp(Sys.time())

  .sidra_parse_values(payload) |>
    .sidra_shape_rows(spec, recorded_at)
}

# The first element of a SIDRA values response is a header object naming the
# dimensions; the rest are value rows keyed on the same short names.
.sidra_parse_values <- function(payload) {
  if (!is.list(payload) || length(payload) == 0) {
    cli::cli_abort("SIDRA returned no payload where a header row was due.")
  }
  header <- payload[[1]]
  required <- c("V", "D1C", "D1N", "D2C", "D2N", "D3C", "MC", "MN")
  if (!all(required %in% names(header))) {
    cli::cli_abort(c(
      "Unexpected SIDRA payload: the header row is missing
       {.field {setdiff(required, names(header))}}.",
      i = "The values endpoint's first element must map the dimension keys."
    ))
  }

  rows <- payload[-1]
  if (length(rows) == 0) {
    return(.sidra_empty_raw())
  }
  tibble::tibble(
    unit_id = .sidra_field(rows, "D1C"),
    unit_name = .sidra_field(rows, "D1N"),
    variable_code = .sidra_field(rows, "D2C"),
    variable_name = .sidra_field(rows, "D2N"),
    year_raw = .sidra_field(rows, "D3C"),
    item_code = .sidra_field(rows, "D4C"),
    item_name = .sidra_field(rows, "D4N"),
    unit_code = .sidra_field(rows, "MC"),
    unit_label = .sidra_field(rows, "MN"),
    value_raw = .sidra_field(rows, "V")
  )
}

# D4C/D4N are absent from a table with no classification (table 94), so a
# missing key is NA rather than an error.
.sidra_field <- function(rows, key) {
  purrr::map_chr(rows, \(row) {
    value <- row[[key]]
    if (is.null(value)) NA_character_ else as.character(value)
  })
}

.sidra_empty_raw <- function() {
  tibble::tibble(
    unit_id = character(),
    unit_name = character(),
    variable_code = character(),
    variable_name = character(),
    year_raw = character(),
    item_code = character(),
    item_name = character(),
    unit_code = character(),
    unit_label = character(),
    value_raw = character()
  )
}

.sidra_shape_rows <- function(raw, spec, recorded_at) {
  joined <- .sidra_add_variable_spec(raw, spec)
  .sidra_check_units(joined)
  parsed <- .sidra_parse_value_codes(joined$value_raw)

  tibble::tibble(
    source = rep(spec$source, nrow(joined)),
    source_native_unit_id = joined$unit_id,
    source_native_unit_name = joined$unit_name,
    source_native_item_code = joined$item_code,
    source_native_item_name = dplyr::coalesce(
      joined$item_name,
      joined$variable_name
    ),
    indicator_used = joined$indicator_used,
    quantity = joined$quantity,
    year = as.integer(joined$year_raw),
    value = parsed$value,
    value_unit = joined$value_unit,
    value_flag = parsed$flag,
    grain = rep("admin1", nrow(joined)),
    nuts_version = rep(NA_character_, nrow(joined)),
    source_version = rep(NA_character_, nrow(joined)),
    recorded_at = rep(recorded_at, nrow(joined)),
    variable_code = joined$variable_code
  )
}

.sidra_add_variable_spec <- function(raw, spec) {
  known <- .sidra_variable_spec()
  unknown <- setdiff(unique(raw$variable_code), known$variable_code)
  if (length(unknown) > 0) {
    cli::cli_abort(c(
      "SIDRA served variable{?s} {.val {unknown}}, which this reader does
       not map to a WHEP indicator and unit.",
      i = "Expected {.val {spec$variables}} for this table."
    ))
  }
  dplyr::left_join(raw, known, by = "variable_code")
}

# A missing-value row carries an empty unit of measure, so only rows that
# state one are checked -- but a stated unit disagreeing with the variable's
# documented one is a silent unit change and aborts.
.sidra_check_units <- function(joined) {
  stated <- !is.na(joined$unit_code) & joined$unit_code != ""
  wrong <- stated & joined$unit_code != joined$sidra_unit_code
  if (!any(wrong)) {
    return(invisible(NULL))
  }
  offending <- joined[wrong, ] |>
    dplyr::distinct(variable_code, unit_code, unit_label, sidra_unit_code)
  cli::cli_abort(c(
    "SIDRA served an unexpected unit of measure.",
    x = "Variable {.val {offending$variable_code}} arrived as
       {.val {offending$unit_label}} (code {.val {offending$unit_code}}),
       not code {.val {offending$sidra_unit_code}}.",
    i = "Units are consumed unconverted, so this must be resolved rather
      than coerced."
  ))
}

# "-" is SIDRA's absolute zero and the only way it writes one, so it parses
# to 0 and leaves `value_flag` clean; the three missing-value codes parse to
# NA and keep the code verbatim. Anything else is a format change and aborts
# rather than becoming a silent NA.
.sidra_parse_value_codes <- function(x) {
  numeric_like <- stringr::str_detect(x, "^-?[0-9]+(\\.[0-9]+)?$")
  known <- numeric_like | x %in% c("-", .sidra_missing_codes())
  if (!all(known)) {
    cli::cli_abort(c(
      "SIDRA served unrecognised value{?s} {.val {unique(x[!known])}}.",
      i = "Known codes are {.val -}, {.val ...}, {.val ..} and {.val X}."
    ))
  }
  value <- rep(NA_real_, length(x))
  value[numeric_like] <- as.numeric(x[numeric_like])
  value[x == "-"] <- 0
  list(
    value = value,
    flag = dplyr::if_else(x %in% .sidra_missing_codes(), x, NA_character_)
  )
}

# Category "0" of classification 782 is the all-crops total for the state,
# not an item, so an unrestricted request drops it rather than handing a
# double count downstream. Requesting it explicitly keeps it.
.sidra_drop_total_category <- function(x, items) {
  if (!is.null(items)) {
    return(x)
  }
  dplyr::filter(
    x,
    is.na(source_native_item_code) | source_native_item_code != "0"
  )
}
