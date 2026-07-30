#' Complete columns from a typed prototype.
#'
#' @description
#' Add absent columns to a tibble and safely cast present columns to the types
#' declared by a zero-row prototype. Missing columns receive typed missing
#' values unless a scalar default is supplied. Defaults never replace missing
#' values inside a column that is already present.
#'
#' Prototype columns are returned first in prototype order. With
#' `extra = "keep"`, other input columns follow in their original relative
#' order. With `extra = "drop"`, the output has exactly the prototype schema.
#'
#' @param data Input tibble.
#' @param prototype Zero-row tibble defining required column names, types, and
#'   order.
#' @param defaults Optional named list of size-one defaults for absent columns.
#'   Every name must occur in `prototype`, and each value must be safely
#'   convertible to the corresponding prototype type.
#' @param extra Whether columns absent from `prototype` are `"keep"` or
#'   `"drop"`.
#'
#' @return An ungrouped tibble with the same rows as `data`, completed and
#'   ordered from `prototype`. Grouping metadata is not preserved.
#'
#' @export
#'
#' @examples
#' prototype <- tibble::tibble(
#'   year = integer(),
#'   value = double(),
#'   source = character()
#' )
#' data <- tibble::tibble(value = 2, year = 2020L, note = "observed")
#'
#' ensure_columns(
#'   data,
#'   prototype,
#'   defaults = list(source = "unknown")
#' )
ensure_columns <- function(
  data,
  prototype,
  defaults = NULL,
  extra = c("keep", "drop")
) {
  extra <- rlang::arg_match(extra)
  .validate_column_inputs(data, prototype)
  defaults <- .prepare_column_defaults(defaults, prototype)

  output <- purrr::reduce(
    names(prototype),
    .ensure_one_column,
    .init = tibble::as_tibble(data),
    prototype = prototype,
    defaults = defaults
  )

  .order_ensured_columns(output, names(prototype), extra)
}

.validate_column_inputs <- function(data, prototype) {
  if (!tibble::is_tibble(data)) {
    cli::cli_abort(
      "{.arg data} must be a tibble.",
      class = c("whep_error_columns_input", "whep_error_ensure_columns")
    )
  }
  if (!tibble::is_tibble(prototype)) {
    cli::cli_abort(
      "{.arg prototype} must be a tibble.",
      class = c("whep_error_columns_input", "whep_error_ensure_columns")
    )
  }
  if (nrow(prototype) != 0L) {
    cli::cli_abort(
      "{.arg prototype} must have zero rows.",
      class = c("whep_error_columns_input", "whep_error_ensure_columns")
    )
  }
  .check_column_names(data, "data")
  .check_column_names(prototype, "prototype")
}

.check_column_names <- function(x, arg) {
  column_names <- names(x)
  empty <- is.na(column_names) | column_names == ""
  if (any(empty)) {
    cli::cli_abort(
      "{.arg {arg}} must have non-empty column names.",
      class = c("whep_error_columns_input", "whep_error_ensure_columns")
    )
  }
  duplicates <- unique(column_names[duplicated(column_names)])
  if (length(duplicates) > 0L) {
    cli::cli_abort(
      "{.arg {arg}} has duplicate column{?s}: {.field {duplicates}}.",
      class = c("whep_error_columns_input", "whep_error_ensure_columns")
    )
  }
}

.prepare_column_defaults <- function(defaults, prototype) {
  if (is.null(defaults)) {
    return(list())
  }
  if (!is.list(defaults)) {
    cli::cli_abort(
      "{.arg defaults} must be a named list.",
      class = c("whep_error_columns_default", "whep_error_ensure_columns")
    )
  }
  if (length(defaults) == 0L) {
    return(defaults)
  }
  .check_default_names(defaults, prototype)
  .check_default_sizes(defaults)

  purrr::imap(
    defaults,
    .cast_column_default,
    prototype = prototype
  )
}

.cast_column_default <- function(value, name, prototype) {
  .cast_ensured_column(value, prototype[[name]], name, "defaults")
}

.check_default_names <- function(defaults, prototype) {
  default_names <- names(defaults)
  empty <- is.null(default_names) ||
    any(is.na(default_names) | default_names == "")
  if (empty) {
    cli::cli_abort(
      "{.arg defaults} must have one non-empty name per value.",
      class = c("whep_error_columns_default", "whep_error_ensure_columns")
    )
  }
  duplicates <- unique(default_names[duplicated(default_names)])
  if (length(duplicates) > 0L) {
    cli::cli_abort(
      "{.arg defaults} has duplicate name{?s}: {.field {duplicates}}.",
      class = c("whep_error_columns_default", "whep_error_ensure_columns")
    )
  }
  unknown <- setdiff(default_names, names(prototype))
  if (length(unknown) > 0L) {
    cli::cli_abort(
      "{.arg defaults} contains unknown column{?s}: {.field {unknown}}.",
      class = c("whep_error_columns_default", "whep_error_ensure_columns")
    )
  }
}

.check_default_sizes <- function(defaults) {
  is_vector <- purrr::map_lgl(defaults, vctrs::obj_is_vector)
  if (any(!is_vector)) {
    invalid <- names(defaults)[!is_vector]
    cli::cli_abort(
      "Default{?s} for {.field {invalid}} must be vector{?s}.",
      class = c("whep_error_columns_default", "whep_error_ensure_columns")
    )
  }
  sizes <- purrr::map_int(defaults, vctrs::vec_size)
  if (any(sizes != 1L)) {
    invalid <- names(defaults)[sizes != 1L]
    cli::cli_abort(
      "Default{?s} for {.field {invalid}} must have size one.",
      class = c("whep_error_columns_default", "whep_error_ensure_columns")
    )
  }
}

.ensure_one_column <- function(data, name, prototype, defaults) {
  if (rlang::has_name(data, name)) {
    data[[name]] <- .cast_ensured_column(
      data[[name]],
      prototype[[name]],
      name,
      "data"
    )
    return(data)
  }

  data[[name]] <- if (rlang::has_name(defaults, name)) {
    vctrs::vec_recycle(defaults[[name]], nrow(data))
  } else {
    vctrs::vec_init(prototype[[name]], nrow(data))
  }
  data
}

.cast_ensured_column <- function(value, prototype, name, source) {
  tryCatch(
    vctrs::vec_cast(
      value,
      prototype,
      x_arg = stringr::str_c(source, "$", name),
      to_arg = stringr::str_c("prototype$", name)
    ),
    vctrs_error_incompatible_type = .abort_ensured_column_cast
  )
}

.abort_ensured_column_cast <- function(cnd) {
  name <- stringr::str_remove(cnd$to_arg, stringr::fixed("prototype$"))
  error_class <- if (
    stringr::str_starts(cnd$x_arg, stringr::fixed("defaults$"))
  ) {
    "whep_error_columns_default"
  } else {
    "whep_error_columns_cast"
  }
  cli::cli_abort(
    "Column {.field {name}} is incompatible with its prototype.",
    parent = cnd,
    class = c(error_class, "whep_error_ensure_columns")
  )
}

.order_ensured_columns <- function(data, prototype_names, extra) {
  extra_names <- setdiff(names(data), prototype_names)
  output_names <- if (extra == "keep") {
    c(prototype_names, extra_names)
  } else {
    prototype_names
  }
  dplyr::select(data, dplyr::all_of(output_names))
}
