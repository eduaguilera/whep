#' Check a table against a declarative schema.
#'
#' @description
#' Validate a tibble against a serializable schema and return one row of
#' diagnostics per violation. The input is never modified, so this is the
#' read-only counterpart of [ensure_columns()], which *coerces* a table to a
#' typed prototype. Use [ensure_columns()] to reach a schema and
#' `check_table_schema()` to prove a table is already there.
#'
#' The schema is plain data — nested lists of scalars and atomic vectors — so
#' it round-trips through `yaml` or `jsonlite` unchanged and can live next to
#' the artifact it describes. Project vocabularies and scientific bounds stay
#' in the caller's schema; this function hard-codes none of them.
#'
#' @section Schema representation:
#' A list with these fields, all optional except `columns`:
#'
#' - `columns`: An *ordered* list of column specifications (see below). A
#'   list, not a named list, so the declared order survives any
#'   serialization. May be empty.
#' - `key`: Character vector of column names that must jointly be unique.
#'   Every name must be declared in `columns`.
#' - `extra_columns`: `"allow"` (default) or `"forbid"` for columns the
#'   schema does not declare.
#' - `column_order`: `"ignore"` (default) or `"strict"`. Under `"strict"`
#'   the declared columns that are present must appear in the declared
#'   relative order.
#' - `allow_empty`: Whether a zero-row table is acceptable. `TRUE` by
#'   default; set `FALSE` to flag an empty result.
#'
#' Each column specification is a list with:
#'
#' - `name`: Column name. Required.
#' - `type`: One of `"logical"`, `"integer"`, `"double"`, `"character"`,
#'   `"Date"`, `"list"` or `"any"`. Required. Types are compared exactly
#'   (`vctrs::vec_is()`): an `integer` column does not satisfy `"double"`.
#'   `"any"` skips the type check.
#' - `required`: Whether the column must be present. `TRUE` by default.
#' - `allow_missing`: Whether `NA` is acceptable. `TRUE` by default.
#' - `min`, `max`: Inclusive bounds, for `"integer"`, `"double"` and
#'   `"Date"` only.
#' - `allowed`: Permitted values, i.e. a caller-owned vocabulary.
#' - `unique`: Whether values must be unique within the column. `FALSE` by
#'   default.
#' - `severity`: `"error"` (default) or `"warning"` for every diagnostic
#'   attributed to this column.
#'
#' `min`, `max`, `allowed`, `unique` and `allow_missing = FALSE` do not apply
#' to `"list"` or `"any"` columns. Unknown fields, at either level, abort
#' rather than being ignored, so a mistyped `minimum` cannot silently disable
#' a bound.
#'
#' @section Diagnostics:
#' One row per violation, ordered deterministically: table-scope rules
#' first, then each declared column in schema order (by row, then rule),
#' then undeclared columns in input order, then key duplicates. Columns:
#'
#' - `row`: Row index in `data`, `NA` for table- and column-scope rules.
#' - `column`: Column name, `NA` for table-scope rules.
#' - `rule`: One of `"empty_table"`, `"column_order"`, `"missing_column"`,
#'   `"unexpected_column"`, `"type_mismatch"`, `"missing_value"`,
#'   `"below_min"`, `"above_max"`, `"not_allowed"`, `"duplicate_value"`,
#'   `"duplicate_key"`.
#' - `value`: The offending value, formatted, `NA` where no single value is
#'   at fault.
#' - `severity`: `"error"` or `"warning"`, from the column specification.
#'   Table-scope rules are always `"error"`.
#' - `detail`: Human-readable context.
#'
#' A column whose type does not match reports `type_mismatch` and its value
#' rules are skipped, so a wrongly typed column yields one diagnostic rather
#' than one per row.
#'
#' @param data Table to validate. Not modified.
#' @param schema Declarative schema, as described above.
#'
#' @return A tibble of diagnostics, empty when `data` conforms.
#'
#' @export
#'
#' @examples
#' # A keyed long table with a vocabulary and a scientific bound.
#' schema <- list(
#'   columns = list(
#'     list(name = "year", type = "integer", min = 1961, max = 2023),
#'     list(name = "area_code", type = "integer", allow_missing = FALSE),
#'     list(
#'       name = "source",
#'       type = "character",
#'       allowed = c("FAOSTAT_prod", "LUH2")
#'     ),
#'     list(name = "value", type = "double", min = 0)
#'   ),
#'   key = c("year", "area_code")
#' )
#' data <- tibble::tibble(
#'   year = c(2000L, 2000L, 1900L),
#'   area_code = c(4L, 4L, 8L),
#'   source = c("FAOSTAT_prod", "guess", "LUH2"),
#'   value = c(1, -2, 3)
#' )
#' check_table_schema(data, schema)
#'
#' # The schema is data: it survives a YAML round trip unchanged.
#' identical(
#'   check_table_schema(data, yaml::yaml.load(yaml::as.yaml(schema))),
#'   check_table_schema(data, schema)
#' )
#'
#' # A structurally different schema: closed column set, strict order,
#' # no key, a non-empty requirement and a list column.
#' manifest_schema <- list(
#'   columns = list(
#'     list(name = "built_at", type = "Date"),
#'     list(name = "inputs", type = "list")
#'   ),
#'   extra_columns = "forbid",
#'   column_order = "strict",
#'   allow_empty = FALSE
#' )
#' check_table_schema(tibble::tibble(inputs = list()), manifest_schema)
check_table_schema <- function(data, schema) {
  .validate_schema_data(data)
  spec <- .parse_table_schema(schema)
  column_diagnostics <- spec$columns |>
    purrr::map(.schema_column_diagnostics, data = data) |>
    purrr::list_rbind()

  dplyr::bind_rows(
    .schema_table_diagnostics(data, spec),
    column_diagnostics,
    .schema_extra_diagnostics(data, spec),
    .schema_key_diagnostics(data, spec)
  ) |>
    vctrs::vec_cast(.schema_diagnostics_prototype())
}

#' Assert that a table conforms to a declarative schema.
#'
#' @description
#' Build-time gate over a tabular artifact. Aborts when
#' [check_table_schema()] reports any `"error"` diagnostic and warns when it
#' reports only `"warning"` ones, so a table that has silently lost a
#' column, changed type or gained duplicate keys fails at the moment it is
#' produced instead of downstream. Returns its input, so it can sit inside a
#' pipeline without changing the value that flows through.
#'
#' @inheritParams check_table_schema
#' @param arg Name of the validated object, used in messages.
#'
#' @return Invisibly, `data`, unchanged. Called for its side effect of
#'   aborting on violation.
#'
#' @export
#'
#' @examples
#' schema <- list(
#'   columns = list(
#'     list(name = "year", type = "integer"),
#'     list(name = "value", type = "double", min = 0)
#'   ),
#'   key = "year"
#' )
#' data <- tibble::tibble(year = c(2000L, 2001L), value = c(1, 2))
#' assert_table_schema(data, schema)
assert_table_schema <- function(data, schema, arg = "data") {
  diagnostics <- check_table_schema(data, schema)
  .warn_schema_diagnostics(
    dplyr::filter(diagnostics, severity == "warning"),
    arg
  )
  .abort_schema_diagnostics(
    dplyr::filter(diagnostics, severity == "error"),
    arg
  )
  invisible(data)
}

# --- Schema primitives ---
#
# `.parse_table_schema()` and `.schema_type_prototype()` are the shared
# schema seam of the declarative-schema family (whep#372-#375): the typed
# empty-table constructor (#374) and the standard table writer (#375) should
# resolve a schema through them rather than re-reading the raw list, so all
# three agree on the supported types, the field vocabulary and the column
# order.

.schema_types <- c(
  "logical",
  "integer",
  "double",
  "character",
  "Date",
  "list",
  "any"
)

.schema_column_fields <- c(
  "name",
  "type",
  "required",
  "allow_missing",
  "min",
  "max",
  "allowed",
  "unique",
  "severity"
)

.schema_value_types <- c(
  "logical",
  "integer",
  "double",
  "character",
  "Date"
)

.schema_ordered_types <- c("integer", "double", "Date")

.schema_diagnostics_prototype <- function() {
  tibble::tibble(
    row = integer(),
    column = character(),
    rule = character(),
    value = character(),
    severity = character(),
    detail = character()
  )
}

.schema_type_prototype <- function(type) {
  switch(
    type,
    logical = logical(),
    integer = integer(),
    double = double(),
    character = character(),
    Date = as.Date(character()),
    list = list(),
    any = NULL
  )
}

.schema_abort <- function(message, class, env = rlang::caller_env()) {
  cli::cli_abort(
    message,
    class = c(class, "whep_error_table_schema"),
    call = env,
    .envir = env
  )
}

.validate_schema_data <- function(data) {
  if (!is.data.frame(data)) {
    .schema_abort(
      "{.arg data} must be a data frame or tibble.",
      "whep_error_schema_input"
    )
  }
  names_seen <- names(data)
  duplicates <- unique(names_seen[duplicated(names_seen)])
  if (length(duplicates) > 0) {
    .schema_abort(
      "{.arg data} has duplicate column{?s}: {.field {duplicates}}.",
      "whep_error_schema_input"
    )
  }
}

.parse_table_schema <- function(schema) {
  if (!is.list(schema) || is.data.frame(schema)) {
    .schema_abort(
      "{.arg schema} must be a list.",
      "whep_error_schema_spec"
    )
  }
  .check_schema_fields(
    schema,
    c(
      "columns",
      "key",
      "extra_columns",
      "column_order",
      "allow_empty"
    )
  )
  columns <- .parse_schema_columns(schema$columns)

  list(
    columns = columns,
    key = .parse_schema_key(schema$key, columns),
    extra_columns = .parse_schema_choice(
      schema$extra_columns,
      c("allow", "forbid"),
      "schema",
      "extra_columns"
    ),
    column_order = .parse_schema_choice(
      schema$column_order,
      c("ignore", "strict"),
      "schema",
      "column_order"
    ),
    allow_empty = .parse_schema_flag(
      schema$allow_empty,
      TRUE,
      "schema",
      "allow_empty"
    )
  )
}

.check_schema_fields <- function(x, allowed, where = "schema") {
  field_names <- names(x)
  if (length(x) > 0 && (is.null(field_names) || any(field_names == ""))) {
    .schema_abort(
      "Every field of {.field {where}} must be named.",
      "whep_error_schema_spec"
    )
  }
  unknown <- setdiff(field_names, allowed)
  if (length(unknown) > 0) {
    .schema_abort(
      c(
        "Unknown {cli::qty(length(unknown))}field{?s} in \\
         {.field {where}}: {.field {unknown}}.",
        "i" = "Supported: {.field {allowed}}."
      ),
      "whep_error_schema_spec"
    )
  }
}

.parse_schema_columns <- function(columns) {
  if (is.null(columns) || !is.list(columns) || !is.null(names(columns))) {
    .schema_abort(
      c(
        "{.field schema$columns} must be an unnamed list of columns.",
        "i" = "An unnamed list keeps the declared column order serializable."
      ),
      "whep_error_schema_spec"
    )
  }
  parsed <- purrr::imap(columns, .parse_schema_column)
  declared <- purrr::map_chr(parsed, "name")
  duplicates <- unique(declared[duplicated(declared)])
  if (length(duplicates) > 0) {
    .schema_abort(
      "{.field schema} declares {.field {duplicates}} more than once.",
      "whep_error_schema_spec"
    )
  }
  parsed
}

.parse_schema_column <- function(column, index) {
  where <- stringr::str_c("schema$columns[[", index, "]]")
  if (!is.list(column)) {
    .schema_abort(
      "{.field {where}} must be a list.",
      "whep_error_schema_spec"
    )
  }
  .check_schema_fields(column, .schema_column_fields, where)
  name <- .parse_schema_name(column$name, where)
  type <- .parse_schema_type(column$type, where)
  .check_schema_type_fields(column, type, where)

  list(
    name = name,
    type = type,
    prototype = .schema_type_prototype(type),
    required = .parse_schema_flag(column$required, TRUE, where, "required"),
    allow_missing = .parse_schema_flag(
      column$allow_missing,
      TRUE,
      where,
      "allow_missing"
    ),
    unique = .parse_schema_flag(column$unique, FALSE, where, "unique"),
    severity = .parse_schema_choice(
      column$severity,
      c("error", "warning"),
      where,
      "severity"
    ),
    min = .cast_schema_bound(column$min, type, where, "min"),
    max = .cast_schema_bound(column$max, type, where, "max"),
    allowed = .cast_schema_allowed(column$allowed, type, where)
  )
}

.parse_schema_name <- function(name, where) {
  ok <- rlang::is_string(name) && !is.na(name) && name != ""
  if (!ok) {
    .schema_abort(
      "{.field {where}$name} must be a non-empty string.",
      "whep_error_schema_spec"
    )
  }
  name
}

.parse_schema_type <- function(type, where) {
  if (is.null(type)) {
    types <- .schema_types
    .schema_abort(
      c(
        "{.field {where}} does not declare a {.field type}.",
        "i" = "Must be one of {.val {types}}."
      ),
      "whep_error_schema_spec"
    )
  }
  .parse_schema_choice(type, .schema_types, where, "type")
}

.parse_schema_choice <- function(value, values, where, field) {
  if (is.null(value)) {
    return(values[[1]])
  }
  if (!rlang::is_string(value) || !value %in% values) {
    .schema_abort(
      c(
        "{.field {where}} has an invalid {.field {field}}.",
        "i" = "Must be one of {.val {values}}."
      ),
      "whep_error_schema_spec"
    )
  }
  value
}

.parse_schema_flag <- function(value, default, where, field) {
  if (is.null(value)) {
    return(default)
  }
  if (!rlang::is_bool(value)) {
    .schema_abort(
      "{.field {where}${field}} must be {.val {TRUE}} or {.val {FALSE}}.",
      "whep_error_schema_spec"
    )
  }
  value
}

.parse_schema_key <- function(key, columns) {
  if (is.null(key)) {
    return(character())
  }
  if (!is.character(key) || anyNA(key)) {
    .schema_abort(
      "{.field schema$key} must be a character vector of column names.",
      "whep_error_schema_spec"
    )
  }
  unknown <- setdiff(key, purrr::map_chr(columns, "name"))
  if (length(unknown) > 0) {
    .schema_abort(
      "{.field schema$key} names {cli::qty(length(unknown))}undeclared \\
       column{?s}: {.field {unknown}}.",
      "whep_error_schema_spec"
    )
  }
  key
}

.check_schema_type_fields <- function(column, type, where) {
  value_only <- c("allowed", "unique")
  present <- names(column)
  bad <- character()
  if (!type %in% .schema_ordered_types) {
    bad <- c(bad, intersect(present, c("min", "max")))
  }
  if (!type %in% .schema_value_types) {
    bad <- c(bad, intersect(present, value_only))
    if (isFALSE(column$allow_missing)) {
      bad <- c(bad, "allow_missing")
    }
  }
  if (length(bad) > 0) {
    fields <- unique(bad)
    ordered <- .schema_ordered_types
    comparable <- .schema_value_types
    .schema_abort(
      c(
        "{.field {where}} sets {.field {fields}} on a {.val {type}}.",
        "i" = "Bounds need {.val {ordered}}; value rules \\
               need {.val {comparable}}."
      ),
      "whep_error_schema_spec"
    )
  }
}

.cast_schema_bound <- function(value, type, where, field) {
  if (is.null(value)) {
    return(NULL)
  }
  if (vctrs::vec_size(value) != 1L) {
    .schema_abort(
      "{.field {where}${field}} must be a single value.",
      "whep_error_schema_spec"
    )
  }
  .cast_schema_value(value, type, where, field)
}

.cast_schema_allowed <- function(value, type, where) {
  if (is.null(value)) {
    return(NULL)
  }
  if (vctrs::vec_size(value) == 0L) {
    .schema_abort(
      "{.field {where}$allowed} must list at least one value.",
      "whep_error_schema_spec"
    )
  }
  .cast_schema_value(value, type, where, "allowed")
}

.cast_schema_value <- function(value, type, where, field) {
  tryCatch(
    vctrs::vec_cast(value, .schema_type_prototype(type)),
    error = function(cnd) {
      .schema_abort(
        c(
          "{.field {where}${field}} is not a {.val {type}}.",
          "i" = "Declare it in the column's own type."
        ),
        "whep_error_schema_spec"
      )
    }
  )
}

.schema_diagnostic <- function(rule, detail, column, severity, row, value) {
  tibble::tibble(
    row = vctrs::vec_cast(row, integer()),
    column = column,
    rule = rule,
    value = value,
    severity = severity,
    detail = detail
  )
}

.schema_table_diagnostics <- function(data, spec) {
  dplyr::bind_rows(
    .schema_empty_diagnostic(data, spec),
    .schema_order_diagnostic(data, spec)
  )
}

.schema_empty_diagnostic <- function(data, spec) {
  if (spec$allow_empty || nrow(data) > 0) {
    return(.schema_diagnostics_prototype())
  }
  .schema_diagnostic(
    rule = "empty_table",
    detail = "the schema requires at least one row",
    column = NA_character_,
    severity = "error",
    row = NA_integer_,
    value = NA_character_
  )
}

.schema_order_diagnostic <- function(data, spec) {
  declared <- purrr::map_chr(spec$columns, "name")
  present <- intersect(declared, names(data))
  actual <- intersect(names(data), declared)
  if (spec$column_order == "ignore" || identical(present, actual)) {
    return(.schema_diagnostics_prototype())
  }
  .schema_diagnostic(
    rule = "column_order",
    detail = stringr::str_c(
      "declared order ",
      stringr::str_c(present, collapse = ", "),
      "; found ",
      stringr::str_c(actual, collapse = ", ")
    ),
    column = NA_character_,
    severity = "error",
    row = NA_integer_,
    value = NA_character_
  )
}

.schema_extra_diagnostics <- function(data, spec) {
  declared <- purrr::map_chr(spec$columns, "name")
  extra <- setdiff(names(data), declared)
  if (spec$extra_columns == "allow" || length(extra) == 0) {
    return(.schema_diagnostics_prototype())
  }
  .schema_diagnostic(
    rule = "unexpected_column",
    detail = "the schema forbids undeclared columns",
    column = extra,
    severity = "error",
    row = NA_integer_,
    value = NA_character_
  )
}

.schema_column_diagnostics <- function(column, data) {
  if (!rlang::has_name(data, column$name)) {
    return(.schema_missing_diagnostic(column))
  }
  values <- data[[column$name]]
  mismatch <- .schema_type_diagnostic(values, column)
  if (nrow(mismatch) > 0) {
    return(mismatch)
  }
  dplyr::bind_rows(
    .schema_missing_value_rows(values, column),
    .schema_bound_rows(values, column, "min"),
    .schema_bound_rows(values, column, "max"),
    .schema_allowed_rows(values, column),
    .schema_unique_rows(values, column)
  ) |>
    dplyr::arrange(row, rule)
}

.schema_missing_diagnostic <- function(column) {
  if (!column$required) {
    return(.schema_diagnostics_prototype())
  }
  .schema_diagnostic(
    rule = "missing_column",
    detail = stringr::str_c("the schema requires a ", column$type, " column"),
    column = column$name,
    severity = column$severity,
    row = NA_integer_,
    value = NA_character_
  )
}

.schema_type_diagnostic <- function(values, column) {
  if (column$type == "any" || vctrs::vec_is(values, column$prototype)) {
    return(.schema_diagnostics_prototype())
  }
  .schema_diagnostic(
    rule = "type_mismatch",
    detail = stringr::str_c("declared ", column$type),
    column = column$name,
    severity = column$severity,
    row = NA_integer_,
    value = vctrs::vec_ptype_full(values)
  )
}

.schema_missing_value_rows <- function(values, column) {
  if (column$allow_missing) {
    return(.schema_diagnostics_prototype())
  }
  .schema_flagged_rows(
    is.na(values),
    values,
    column,
    "missing_value",
    "the column forbids missing values"
  )
}

.schema_bound_rows <- function(values, column, field) {
  bound <- column[[field]]
  if (is.null(bound)) {
    return(.schema_diagnostics_prototype())
  }
  flagged <- if (field == "min") {
    !is.na(values) & values < bound
  } else {
    !is.na(values) & values > bound
  }
  rule <- if (field == "min") "below_min" else "above_max"
  .schema_flagged_rows(
    flagged,
    values,
    column,
    rule,
    stringr::str_c(field, " is ", .schema_format(bound))
  )
}

.schema_allowed_rows <- function(values, column) {
  if (is.null(column$allowed)) {
    return(.schema_diagnostics_prototype())
  }
  .schema_flagged_rows(
    !is.na(values) & !vctrs::vec_in(values, column$allowed),
    values,
    column,
    "not_allowed",
    stringr::str_c(
      "allowed: ",
      stringr::str_c(.schema_format(column$allowed), collapse = ", ")
    )
  )
}

.schema_unique_rows <- function(values, column) {
  if (!column$unique) {
    return(.schema_diagnostics_prototype())
  }
  .schema_flagged_rows(
    vctrs::vec_duplicate_detect(values),
    values,
    column,
    "duplicate_value",
    "the column must be unique"
  )
}

.schema_flagged_rows <- function(flagged, values, column, rule, detail) {
  rows <- which(flagged)
  .schema_diagnostic(
    rule = rule,
    detail = detail,
    column = column$name,
    severity = column$severity,
    row = rows,
    value = .schema_format(vctrs::vec_slice(values, rows))
  )
}

.schema_key_diagnostics <- function(data, spec) {
  key <- spec$key
  if (length(key) == 0 || !all(key %in% names(data)) || nrow(data) == 0) {
    return(.schema_diagnostics_prototype())
  }
  keys <- data[, key, drop = FALSE]
  rows <- which(vctrs::vec_duplicate_detect(keys))
  .schema_diagnostic(
    rule = "duplicate_key",
    detail = stringr::str_c(
      "key (",
      stringr::str_c(key, collapse = ", "),
      ") must be unique"
    ),
    column = NA_character_,
    severity = "error",
    row = rows,
    value = .schema_key_labels(keys, rows)
  )
}

.schema_key_labels <- function(keys, rows) {
  keys |>
    vctrs::vec_slice(rows) |>
    purrr::map(.schema_format) |>
    purrr::pmap_chr(function(...) stringr::str_c(..., sep = " | "))
}

.schema_format <- function(values) {
  values |>
    format(trim = TRUE, justify = "none") |>
    as.character() |>
    stringr::str_trunc(40)
}

.schema_rule_tally <- function(diagnostics) {
  counts <- table(diagnostics$rule)
  stringr::str_c(names(counts), " x", as.integer(counts)) |>
    stringr::str_c(collapse = ", ")
}

.schema_first_details <- function(diagnostics) {
  shown <- vctrs::vec_slice(diagnostics, seq_len(min(3L, nrow(diagnostics))))
  labels <- stringr::str_c(
    dplyr::coalesce(shown$column, "<table>"),
    ": ",
    shown$rule,
    dplyr::if_else(
      is.na(shown$row),
      "",
      stringr::str_c(" at row ", shown$row)
    ),
    " (",
    shown$detail,
    ")"
  )
  stringr::str_c("First: ", stringr::str_c(labels, collapse = "; "), ".")
}

.warn_schema_diagnostics <- function(diagnostics, arg) {
  if (nrow(diagnostics) == 0) {
    return(invisible(NULL))
  }
  n_warn <- nrow(diagnostics)
  tally <- .schema_rule_tally(diagnostics)
  detail <- .schema_first_details(diagnostics)
  cli::cli_warn(c(
    "{.arg {arg}} has {cli::qty(n_warn)}{n_warn} schema warning{?s}: {tally}.",
    "i" = "{detail}"
  ))
}

.abort_schema_diagnostics <- function(diagnostics, arg) {
  if (nrow(diagnostics) == 0) {
    return(invisible(NULL))
  }
  n_bad <- nrow(diagnostics)
  tally <- .schema_rule_tally(diagnostics)
  detail <- .schema_first_details(diagnostics)
  cli::cli_abort(
    c(
      "{.arg {arg}} does not conform to its schema.",
      "x" = "{cli::qty(n_bad)}{n_bad} violation{?s}: {tally}.",
      "i" = "{detail}"
    ),
    class = c("whep_error_schema_violation", "whep_error_table_schema"),
    call = rlang::caller_env()
  )
}
