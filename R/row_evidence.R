#' Produce row-level evidence for a table.
#'
#' @description
#' Record, as data, what each row of a result rests on: which producer
#' claimed it, at which version of that producer's source, and the
#' documented evidence fields the producer carries per row (a FAOSTAT
#' flag, a `method_*` label, a gap-filling status). This is the
#' row-level counterpart of [record_provenance()], which records the
#' code and input versions behind a whole dataset (whep#372).
#'
#' The record is a **separate table**, not an attribute. Attributes are
#' silently dropped by `dplyr` joins, filters and `summarise()`, so
#' attribute-borne evidence cannot survive the composition it is meant
#' to document. A keyed sidecar can: [evidence_for()] re-aligns it onto
#' a table after a join, [combine_row_evidence()] merges the output of
#' several producers without letting one overwrite another, and
#' [evidence_conflicts()] reports where two producers disagree.
#'
#' @section Evidence table:
#' Format `"whep-row-evidence/1"`. One row per (table row × evidence
#' field), every column `character` so the table round-trips through
#' Parquet, CSV and YAML unchanged:
#'
#' - `row_key`: The row's identity, i.e. its key values joined by
#'   `U+001F` (unit separator). Opaque: build and read it with this
#'   family's functions rather than by hand.
#' - `key_columns`: The key column names, comma-separated, so a
#'   consumer can re-derive `row_key` from the data alone.
#' - `source_id`: The producer's immutable identifier. Callers should
#'   use the package's existing dataset labels (`"FAOSTAT_prod"`,
#'   `"FAOSTAT_FBS_New"`, `"LUH2"`), because nothing downstream can
#'   recover an identity that was renamed between builds.
#' - `source_version`: Version or vintage of that source, `NA` when the
#'   producer has none.
#' - `recorded_at`: When the record was made, as ISO 8601 UTC.
#' - `field`, `value`: The evidence field's name and its formatted
#'   value for that row. `value` may be `NA`; the field name may not.
#'
#' Rows are ordered by `row_key`, `field`, `source_id`, `recorded_at`
#' and `value`, in the C locale, so two runs over the same input give
#' byte-identical output apart from `recorded_at`.
#'
#' The schema is available as data from [row_evidence_schema()] and is
#' asserted with [assert_table_schema()]. It declares no key: combining
#' two runs of the same producer legitimately repeats a claim, so
#' duplication is not an error. Disagreement is, and that is what
#' [evidence_conflicts()] finds.
#'
#' @param data Table the evidence describes. Not modified. May have
#'   zero rows, which yields a zero-row evidence table.
#' @param source_id Immutable identifier of the producer, one non-empty
#'   string.
#' @param key Character vector of column names of `data` that jointly
#'   identify a row. Must be unique-valued and free of `NA`, otherwise
#'   a piece of evidence would address more than one row or none.
#' @param fields The row-level evidence to record, either a character
#'   vector of column names of `data`, or a named list whose elements
#'   are vectors of length 1 or `nrow(data)`. A list column cannot be
#'   recorded.
#' @param source_version Version or vintage of the source, one string
#'   or `NA`.
#' @param recorded_at Timestamp of the record, a length-one `POSIXct`.
#'   Defaults to the current time; pass a fixed value for reproducible
#'   output.
#'
#' @return A tibble of row evidence, as described above.
#'
#' @seealso [combine_row_evidence()] to merge producers,
#'   [evidence_for()] to carry evidence through a join,
#'   [evidence_conflicts()] to find disagreement, and
#'   [record_provenance()] for the dataset-level record.
#'
#' @export
#'
#' @examples
#' cbs <- tibble::tibble(
#'   area_code = c(724L, 724L, 76L),
#'   item_cbs_code = c(2511L, 2513L, 2511L),
#'   year = 2020L,
#'   value = c(1.5, 2.5, 3.5),
#'   fao_flag = c("A", "E", "A")
#' )
#'
#' # Evidence the table already carries per row, plus one derived field.
#' evidence <- row_evidence(
#'   cbs,
#'   source_id = "FAOSTAT_FBS_New",
#'   key = c("area_code", "item_cbs_code", "year"),
#'   fields = list(
#'     fao_flag = cbs$fao_flag,
#'     imputed = cbs$fao_flag == "E"
#'   ),
#'   source_version = "2024-03-14",
#'   recorded_at = as.POSIXct("2026-01-01", tz = "UTC")
#' )
#' evidence
#'
#' # It is a documented schema, so it can be proved rather than trusted.
#' assert_table_schema(evidence, row_evidence_schema())
#'
#' # And it round-trips: every column is character.
#' path <- tempfile(fileext = ".parquet")
#' write_table_checked(evidence, path)
#' identical(tibble::as_tibble(nanoparquet::read_parquet(path)), evidence)
#' unlink(path)
row_evidence <- function(
  data,
  source_id,
  key,
  fields,
  source_version = NA_character_,
  recorded_at = Sys.time()
) {
  .check_evidence_table(data, "data")
  key <- .check_evidence_key(data, key)
  meta <- list(
    source_id = .check_evidence_string(source_id, "source_id"),
    source_version = .check_evidence_version(source_version),
    recorded_at = .format_evidence_time(recorded_at)
  )
  values <- .resolve_evidence_fields(data, fields)
  row_key <- .encode_evidence_key(data, key)
  .check_evidence_unique(row_key, key)
  .evidence_rows(row_key, key, meta, values)
}

#' The declarative schema of a row-evidence table.
#'
#' @description
#' Return the `"whep-row-evidence/1"` contract as a declarative schema,
#' ready for [check_table_schema()] or [assert_table_schema()]. It is
#' plain data, so it can be serialized next to an evidence artifact and
#' used by a consumer that does not load this package.
#'
#' The schema is closed (`extra_columns = "forbid"`) and ordered
#' (`column_order = "strict"`): an evidence table with an extra column
#' or a permuted column order is not this format, because a consumer
#' rebuilding `row_key` from `key_columns` relies on both.
#'
#' @return A schema list, as documented in [check_table_schema()].
#'
#' @export
#'
#' @examples
#' row_evidence_schema()
#'
#' # The contract is data, so it survives serialization unchanged.
#' identical(
#'   yaml::yaml.load(yaml::as.yaml(row_evidence_schema())),
#'   row_evidence_schema()
#' )
row_evidence_schema <- function() {
  list(
    columns = list(
      list(name = "row_key", type = "character", allow_missing = FALSE),
      list(name = "key_columns", type = "character", allow_missing = FALSE),
      list(name = "source_id", type = "character", allow_missing = FALSE),
      list(name = "source_version", type = "character"),
      list(name = "recorded_at", type = "character", allow_missing = FALSE),
      list(name = "field", type = "character", allow_missing = FALSE),
      list(name = "value", type = "character")
    ),
    extra_columns = "forbid",
    column_order = "strict"
  )
}

#' Combine row evidence from several producers.
#'
#' @description
#' Merge evidence tables deterministically. Nothing is overwritten: the
#' result is the union of the claims, with exact duplicates collapsed
#' and the surviving rows in the format's canonical order, so the same
#' inputs always give the same output whatever order they arrive in.
#'
#' Two producers claiming different values for the same row and field is
#' kept as what it is — two claims — instead of one silently winning by
#' argument position. Find those with [evidence_conflicts()] and decide
#' explicitly.
#'
#' All inputs must agree on `key_columns`: evidence keyed on
#' `(area_code, year)` and evidence keyed on `(area_code)` address
#' different things, and merging them would change what a `row_key`
#' means. That aborts rather than being reconciled.
#'
#' @param ... Evidence tables from [row_evidence()], or lists of them.
#'   Each is validated against [row_evidence_schema()]. Zero-row tables
#'   are allowed and contribute no rows or key constraint.
#'
#' @return A tibble of row evidence, in canonical order.
#'
#' @export
#'
#' @examples
#' production <- tibble::tibble(area_code = c(724L, 76L), year = 2020L)
#' key <- c("area_code", "year")
#' recorded_at <- as.POSIXct("2026-01-01", tz = "UTC")
#'
#' faostat <- row_evidence(
#'   production,
#'   source_id = "FAOSTAT_prod",
#'   key = key,
#'   fields = list(method_land = "reported"),
#'   recorded_at = recorded_at
#' )
#' luh2 <- row_evidence(
#'   production,
#'   source_id = "LUH2",
#'   key = key,
#'   fields = list(method_land = "back-cast"),
#'   recorded_at = recorded_at
#' )
#'
#' combined <- combine_row_evidence(faostat, luh2)
#' combined
#'
#' # Both claims survive, and the disagreement is visible.
#' evidence_conflicts(combined)
combine_row_evidence <- function(...) {
  parts <- .collect_evidence_parts(rlang::list2(...))
  .check_evidence_key_columns(parts)
  parts |>
    purrr::list_rbind() |>
    dplyr::distinct() |>
    .arrange_evidence()
}

#' Report where row evidence disagrees.
#'
#' @description
#' Find the `(row_key, field)` pairs carrying more than one distinct
#' value, i.e. the places where two producers, or two runs of one
#' producer, made different claims about the same row. Reports rather
#' than aborts, so a caller can decide which disagreements matter;
#' `nrow() == 0` is the clean result.
#'
#' A repeated *identical* claim is not a conflict, so re-recording the
#' same evidence is harmless.
#'
#' @param evidence Evidence table, validated against
#'   [row_evidence_schema()].
#'
#' @return A tibble with one row per conflicting `(row_key, field)`,
#'   ordered by both:
#'   - `row_key`, `field`: The row and evidence field in dispute.
#'   - `n_values`: Number of distinct values claimed. `NA` counts as a
#'     value of its own, since "unknown" and "0.5" are a disagreement.
#'   - `values`: The claimed values, `" | "`-separated, with `NA`
#'     rendered as `"NA"`.
#'   - `source_ids`: The producers involved, comma-separated.
#'
#' @export
#'
#' @examples
#' rows <- tibble::tibble(area_code = 724L, year = 2020L)
#' key <- c("area_code", "year")
#' recorded_at <- as.POSIXct("2026-01-01", tz = "UTC")
#' agreeing <- combine_row_evidence(
#'   row_evidence(rows, "A", key, list(flag = "E"), recorded_at = recorded_at),
#'   row_evidence(rows, "B", key, list(flag = "E"), recorded_at = recorded_at)
#' )
#' evidence_conflicts(agreeing)
#'
#' disagreeing <- combine_row_evidence(
#'   agreeing,
#'   row_evidence(rows, "C", key, list(flag = "A"), recorded_at = recorded_at)
#' )
#' evidence_conflicts(disagreeing)
evidence_conflicts <- function(evidence) {
  .assert_evidence(evidence, "evidence")
  evidence |>
    dplyr::summarise(
      n_values = dplyr::n_distinct(value),
      values = .join_evidence_values(value),
      source_ids = .join_evidence_values(source_id, ", "),
      .by = c(row_key, field)
    ) |>
    dplyr::filter(n_values > 1) |>
    dplyr::arrange(row_key, field)
}

#' Carry row evidence through a join or a filter.
#'
#' @description
#' Re-align an evidence table onto a table that has since been joined,
#' filtered, reordered or reduced: the evidence rows whose `row_key`
#' still occurs in `data` are returned, in canonical order, with their
#' `source_id` and `source_version` untouched. Evidence is therefore
#' narrowed by composition but never re-attributed by it.
#'
#' The key is taken from the evidence's own `key_columns`, not from the
#' caller, so a table that no longer carries those columns — an
#' aggregation that dropped `year`, say — aborts instead of matching on
#' whatever is left. Rows of `data` that carry no evidence are a
#' warning, not a silent gap: after a left join they are exactly the
#' rows whose provenance was lost.
#'
#' @param data Table the evidence is being carried onto. Must carry the
#'   evidence's key columns. Duplicated keys are allowed: a fan-out
#'   join does not change which claims apply.
#' @inheritParams evidence_conflicts
#'
#' @return A tibble of row evidence, in canonical order, containing the
#'   rows of `evidence` that apply to `data`.
#'
#' @export
#'
#' @examples
#' rows <- tibble::tibble(area_code = c(724L, 76L), year = 2020L)
#' evidence <- row_evidence(
#'   rows,
#'   source_id = "FAOSTAT_prod",
#'   key = c("area_code", "year"),
#'   fields = list(fao_flag = c("A", "E")),
#'   recorded_at = as.POSIXct("2026-01-01", tz = "UTC")
#' )
#'
#' # A filter narrows the evidence to the rows that survive.
#' evidence_for(dplyr::filter(rows, area_code == 724L), evidence)
#'
#' # A row with no evidence is reported rather than passed over.
#' extended <- dplyr::bind_rows(rows, tibble::tibble(
#'   area_code = 231L,
#'   year = 2020L
#' ))
#' suppressWarnings(evidence_for(extended, evidence))
evidence_for <- function(data, evidence) {
  .assert_evidence(evidence, "evidence")
  .check_evidence_table(data, "data")
  if (nrow(evidence) == 0) {
    return(evidence)
  }
  key <- .evidence_key_names(evidence)
  .check_evidence_columns(data, key)
  keys <- unique(.encode_evidence_key(data, key))
  .warn_evidence_gaps(setdiff(keys, evidence$row_key), length(keys))
  evidence |>
    dplyr::filter(row_key %in% keys) |>
    .arrange_evidence()
}

# --- Format primitives ---
#
# `row_key` joins key values with the unit separator: it is the one
# character no formatted value is allowed to contain, so the encoding is
# injective and a key value carrying it aborts rather than colliding.

.evidence_separator <- "\u001f"

.evidence_prototype <- function() {
  tibble::tibble(
    row_key = character(),
    key_columns = character(),
    source_id = character(),
    source_version = character(),
    recorded_at = character(),
    field = character(),
    value = character()
  )
}

.evidence_abort <- function(message, class, env = rlang::caller_env()) {
  cli::cli_abort(
    message,
    class = c(class, "whep_error_row_evidence"),
    call = env,
    .envir = env
  )
}

.evidence_rows <- function(row_key, key, meta, values) {
  key_columns <- stringr::str_c(key, collapse = ",")
  values |>
    purrr::imap(\(value, field) {
      tibble::tibble(
        row_key = row_key,
        key_columns = key_columns,
        source_id = meta$source_id,
        source_version = meta$source_version,
        recorded_at = meta$recorded_at,
        field = field,
        value = value
      )
    }) |>
    purrr::list_rbind() |>
    .arrange_evidence()
}

# The C locale is what makes the order reproducible across machines;
# `dplyr::arrange()` uses it for character columns by default.
.arrange_evidence <- function(evidence) {
  evidence |>
    vctrs::vec_cast(.evidence_prototype()) |>
    dplyr::arrange(row_key, field, source_id, recorded_at, value)
}

.encode_evidence_key <- function(data, key) {
  parts <- purrr::map(key, \(name) .format_evidence_column(data, name))
  purrr::pmap_chr(parts, \(...) {
    stringr::str_c(..., sep = .evidence_separator)
  })
}

.format_evidence_column <- function(data, name) {
  formatted <- .format_evidence_values(data[[name]], name)
  if (anyNA(formatted)) {
    .evidence_abort(
      c(
        "Key column {.field {name}} has missing values.",
        "i" = "Evidence must address an identified row."
      ),
      "whep_error_evidence_key"
    )
  }
  if (any(stringr::str_detect(formatted, .evidence_separator))) {
    .evidence_abort(
      c(
        "Key column {.field {name}} contains the unit separator.",
        "i" = "That character joins key values, so it cannot occur \\
               inside one."
      ),
      "whep_error_evidence_key"
    )
  }
  formatted
}

.format_evidence_values <- function(values, name) {
  if (is.list(values)) {
    .evidence_abort(
      c(
        "{.field {name}} is a list column.",
        "i" = "Row evidence records formatted scalars, not nested data."
      ),
      "whep_error_evidence_field"
    )
  }
  if (inherits(values, "POSIXt")) {
    return(.format_evidence_stamp(values))
  }
  as.character(values)
}

.format_evidence_time <- function(recorded_at) {
  if (!inherits(recorded_at, "POSIXt") || anyNA(recorded_at)) {
    .evidence_abort(
      "{.arg recorded_at} must be a non-missing {.cls POSIXct}.",
      "whep_error_evidence_input"
    )
  }
  .format_evidence_stamp(recorded_at)
}

# ISO 8601 UTC, the same stamp shape `write_table_checked()` writes into
# its provenance sidecar.
.format_evidence_stamp <- function(times) {
  formatted <- format(as.POSIXlt(times, tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ")
  dplyr::if_else(is.na(times), NA_character_, formatted)
}

.join_evidence_values <- function(values, sep = " | ") {
  values |>
    dplyr::coalesce("NA") |>
    unique() |>
    sort(method = "radix") |>
    stringr::str_c(collapse = sep)
}

# --- Input validation ---

.check_evidence_table <- function(data, arg) {
  if (!is.data.frame(data)) {
    .evidence_abort(
      "{.arg {arg}} must be a data frame, not \\
       {.obj_type_friendly {data}}.",
      "whep_error_evidence_input"
    )
  }
  invisible(data)
}

.check_evidence_string <- function(value, arg) {
  ok <- rlang::is_string(value) && !is.na(value) && value != ""
  if (!ok) {
    .evidence_abort(
      "{.arg {arg}} must be one non-empty string.",
      "whep_error_evidence_input"
    )
  }
  value
}

.check_evidence_version <- function(source_version) {
  if (length(source_version) == 1 && all(is.na(source_version))) {
    return(NA_character_)
  }
  .check_evidence_string(source_version, "source_version")
}

.check_evidence_key <- function(data, key) {
  ok <- is.character(key) && length(key) > 0 && !anyNA(key)
  if (!ok) {
    .evidence_abort(
      "{.arg key} must be a character vector of column names.",
      "whep_error_evidence_input"
    )
  }
  duplicates <- unique(key[duplicated(key)])
  if (length(duplicates) > 0) {
    .evidence_abort(
      "{.arg key} names {.field {duplicates}} more than once.",
      "whep_error_evidence_input"
    )
  }
  if (any(stringr::str_detect(key, ","))) {
    .evidence_abort(
      c(
        "{.arg key} column names cannot contain a comma.",
        "i" = "{.field key_columns} is a comma-separated list of them."
      ),
      "whep_error_evidence_input"
    )
  }
  .check_evidence_columns(data, key)
  key
}

.check_evidence_columns <- function(data, key) {
  absent <- setdiff(key, names(data))
  if (length(absent) > 0) {
    .evidence_abort(
      "{.arg data} {cli::qty(length(absent))}lack{?s/} the key \\
       {cli::qty(length(absent))}column{?s} {.field {absent}}.",
      "whep_error_evidence_key"
    )
  }
  invisible(key)
}

.check_evidence_unique <- function(row_key, key) {
  duplicates <- unique(row_key[duplicated(row_key)])
  if (length(duplicates) == 0) {
    return(invisible(row_key))
  }
  n_dup <- length(duplicates)
  columns <- key
  .evidence_abort(
    c(
      "{.arg key} does not identify a row: {cli::qty(n_dup)}{n_dup} \\
       duplicated {cli::qty(n_dup)}key{?s}.",
      "i" = "Evidence keyed on {.field {columns}} would address more \\
             than one row."
    ),
    "whep_error_evidence_key"
  )
}

.resolve_evidence_fields <- function(data, fields) {
  values <- if (is.character(fields)) {
    .evidence_fields_from_data(data, fields)
  } else if (is.list(fields) && !is.data.frame(fields)) {
    .evidence_fields_from_list(data, fields)
  } else {
    .evidence_abort(
      c(
        "{.arg fields} must be a character vector or a named list.",
        "i" = "A character vector names columns of {.arg data}; a list \\
               holds the values themselves."
      ),
      "whep_error_evidence_field"
    )
  }
  .check_evidence_names(names(values))
  values
}

.evidence_fields_from_data <- function(data, fields) {
  if (length(fields) == 0 || anyNA(fields)) {
    .evidence_abort(
      "{.arg fields} must name at least one column, and none may be \\
       missing.",
      "whep_error_evidence_field"
    )
  }
  absent <- setdiff(fields, names(data))
  if (length(absent) > 0) {
    .evidence_abort(
      "{.arg data} has no {cli::qty(length(absent))}column{?s} \\
       {.field {absent}}.",
      "whep_error_evidence_field"
    )
  }
  fields |>
    rlang::set_names() |>
    purrr::map(\(name) .format_evidence_values(data[[name]], name))
}

.evidence_fields_from_list <- function(data, fields) {
  if (length(fields) == 0) {
    .evidence_abort(
      "{.arg fields} must record at least one evidence field.",
      "whep_error_evidence_field"
    )
  }
  purrr::imap(fields, \(value, name) {
    .recycle_evidence_field(value, name, nrow(data))
  })
}

.recycle_evidence_field <- function(value, name, size) {
  formatted <- .format_evidence_values(value, name)
  if (!length(formatted) %in% unique(c(1L, size))) {
    .evidence_abort(
      "Evidence field {.field {name}} has {length(formatted)} value{?s}, \\
       not 1 or {size}.",
      "whep_error_evidence_field"
    )
  }
  vctrs::vec_recycle(formatted, size)
}

.check_evidence_names <- function(field_names) {
  ok <- !is.null(field_names) &&
    !anyNA(field_names) &&
    all(field_names != "")
  if (!ok) {
    .evidence_abort(
      "Every evidence field must be named.",
      "whep_error_evidence_field"
    )
  }
  duplicates <- unique(field_names[duplicated(field_names)])
  if (length(duplicates) > 0) {
    .evidence_abort(
      "Evidence field {.field {duplicates}} is recorded more than once.",
      "whep_error_evidence_field"
    )
  }
  invisible(field_names)
}

# --- Evidence-table validation ---

.assert_evidence <- function(evidence, arg) {
  assert_table_schema(evidence, row_evidence_schema(), arg = arg)
}

.collect_evidence_parts <- function(parts) {
  parts <- purrr::list_c(purrr::map(parts, .as_evidence_part_list))
  if (length(parts) == 0) {
    .evidence_abort(
      "{.fun combine_row_evidence} needs at least one evidence table.",
      "whep_error_evidence_input"
    )
  }
  purrr::imap(parts, \(part, index) {
    .assert_evidence(part, stringr::str_c("..", index))
  })
}

# A data frame is itself a list, so `purrr::list_flatten()` would tear
# one into its columns; the two cases are separated by hand instead.
.as_evidence_part_list <- function(part) {
  if (is.data.frame(part)) {
    return(list(part))
  }
  if (!is.list(part)) {
    .evidence_abort(
      "Every argument must be an evidence table or a list of them, not \\
       {.obj_type_friendly {part}}.",
      "whep_error_evidence_input"
    )
  }
  unname(as.list(part))
}

.evidence_key_names <- function(evidence) {
  declared <- unique(evidence$key_columns)
  if (length(declared) > 1) {
    .evidence_abort(
      c(
        "The evidence mixes {length(declared)} key spaces.",
        "x" = "Found {.field {declared}}."
      ),
      "whep_error_evidence_key"
    )
  }
  stringr::str_split_1(declared, ",")
}

.check_evidence_key_columns <- function(parts) {
  declared <- parts |>
    purrr::map(\(part) unique(part$key_columns)) |>
    purrr::list_c() |>
    unique()
  if (length(declared) > 1) {
    .evidence_abort(
      c(
        "Evidence tables keyed differently cannot be combined.",
        "x" = "Found {.field {declared}}.",
        "i" = "A {.field row_key} means something different in each, \\
               so merging them would change what a row is."
      ),
      "whep_error_evidence_key"
    )
  }
  invisible(declared)
}

.warn_evidence_gaps <- function(missing, n_keys) {
  if (length(missing) == 0) {
    return(invisible(NULL))
  }
  n_missing <- length(missing)
  cli::cli_warn(
    c(
      "{cli::qty(n_missing)}{n_missing} of {n_keys} \\
       {cli::qty(n_keys)}row{?s} of {.arg data} carr{?ies/y} no evidence.",
      "i" = "They are the rows whose provenance a join did not carry."
    ),
    class = "whep_warn_evidence_gap"
  )
}
