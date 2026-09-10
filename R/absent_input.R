#' Refuse an input that was never supplied
#'
#' @description
#' Assert that each named input **arrived and is not vacuous**, rather than
#' that the totals it feeds reconcile. An input is *supplied* when its column
#' exists and holds at least one value that is neither missing nor zero; it is
#' *absent* when the column is missing, or present and identically zero or
#' missing in every row.
#'
#' @section Why reconciliation cannot do this:
#' Zero satisfies a sum. A pin published without its inland water and ice
#' layers booked 1,643 Mha of lakes and 618 Mha of glaciers as land, and
#' `polity_area_ha == land_area_ha + inland_water_ha + ice_area_ha` still held
#' to `max |residual| = 0 ha`, because both absent layers were zero (whep#885,
#' whep#1010). Three FAOSTAT emission labels lost to a pin-vocabulary change
#' summed to a literal zero through `sum(na.rm = TRUE)`, and a test named
#' *"enteric_ch4_kt conservation is exact"* went on passing, because zero
#' distributes to zero (whep#1016). A consistency identity, a conservation
#' check and a row count are all satisfied by an absence. Only an assertion
#' that the input was supplied is not.
#'
#' @section Supplied is a label first, a value test second:
#' When `stamp` names a column the producer wrote, that column decides: it is a
#' comma-separated list of the keys that were actually consumed, written by
#' [stamp_inputs_supplied()], and no arithmetic downstream can forge it. The
#' value test is the fallback for a table published before its producer
#' carried a stamp. The fallback is deliberately **scale-free** -- identically
#' zero, not a magnitude floor -- so that it holds on a single-country
#' development build as well as on a global one. A floor belongs at publication
#' time, where the expected scale is known.
#'
#' @section What it cannot see:
#' Three absences pass this check, and a caller must not read a pass as more
#' than it is.
#'
#' * A **partial** absence. One non-zero value anywhere in the column is
#'   enough; a layer supplied for three countries out of two hundred is
#'   supplied as far as this is concerned.
#' * A **zero-row** input. Zero rows is a filter that matched nothing, not a
#'   zero-filled column, and the caller that wrote the filter is the one to
#'   answer for it. Columns are judged only when `data` has rows.
#' * A value the **code failed to reach**. If the lookup was never called, the
#'   quantity is absent from the code path and not from the data, and that is a
#'   defect rather than a fill (whep#1034). Run the lookup and look at what it
#'   returns.
#'
#' @param data A tibble carrying the inputs to check.
#' @param required Column names to check, optionally named. A name is the key
#'   used in messages and matched against `stamp`; an unnamed entry uses the
#'   column name as its own key.
#' @param stamp Optional name of a provenance column written by
#'   [stamp_inputs_supplied()]. Used when `data` carries it, ignored otherwise.
#' @param action `"abort"` (default) to raise, `"warn"` to carry on. Abort
#'   where a wrong answer is worse than no answer -- a density whose
#'   denominator is inflated, a stressor that would ship as zero. Warn where a
#'   caller reading only the columns that did arrive is unaffected.
#' @param details Extra `cli` bullets appended to the message, for the caller
#'   to name its source and its remedy.
#'
#' @return `data`, invisibly. Raises or warns with class `whep_absent_input`
#'   when an input is absent; the condition carries the absent keys in its
#'   `absent` field.
#'
#' @export
#'
#' @examples
#' supplied <- tibble::tibble(land_ha = c(1, 2), water_ha = c(0.5, 0))
#' check_inputs_supplied(supplied, c("land_ha", "water_ha"))
#'
#' absent <- tibble::tibble(land_ha = c(1, 2), water_ha = c(0, 0))
#' try(check_inputs_supplied(absent, c(land = "land_ha", water = "water_ha")))
check_inputs_supplied <- function(
  data,
  required,
  stamp = NULL,
  action = c("abort", "warn"),
  details = NULL
) {
  action <- rlang::arg_match(action)
  keys <- .supplied_keys(required)
  absent <- .absent_inputs(data, required, keys, stamp)
  if (length(absent) == 0L) {
    return(invisible(data))
  }
  .signal_absent_inputs(absent, action, details)
  invisible(data)
}

#' Refuse a value vocabulary that no longer contains the labels a filter needs
#'
#' @description
#' Assert that each label a downstream `filter()` selects on actually occurs in
#' the column it selects from. A label that has been renamed upstream does not
#' raise: the filter matches no rows, the sum of no rows is zero, and the zero
#' is indistinguishable from a measurement.
#'
#' This is the sibling of [check_inputs_supplied()] for the case where the
#' column is present and populated but its **vocabulary** has moved. It is the
#' shape of whep#1016, where three FAOSTAT emission `Element` labels were lost
#' to a pin revision and roughly 108 Tg CH4/yr shipped as literal zero, and it
#' is invisible to any check written around `coalesce()`, `replace_na()` or
#' `na.rm` -- none of those appear anywhere near it.
#'
#' The message names the labels that are **present**, up to six of them, as
#' well as the ones that are missing. A rename is only obvious once the new
#' spelling is in front of the reader.
#'
#' A table with **no rows** carries no vocabulary and is passed, for the same
#' reason [check_inputs_supplied()] does not judge one: an empty table is an
#' absent table rather than a moved label, and the caller that produced it is
#' the one to answer for it. The case this is for -- whep#1016 -- is a pin of
#' 2.5 million rows whose labels moved, not an empty one.
#'
#' @param data A tibble carrying the vocabulary to check.
#' @param column Name of the column the labels are drawn from. A column that is
#'   not there at all is reported as the whole vocabulary being absent, even on
#'   a table with no rows.
#' @param labels Labels that must each occur at least once. Compared as
#'   character, so a numeric code may be given as a number.
#' @param action `"abort"` (default) to raise, `"warn"` to carry on.
#' @param details Extra `cli` bullets appended to the message.
#'
#' @return `data`, invisibly. Raises or warns with class `whep_absent_label`
#'   (and `whep_absent_input`) when a label is missing; the condition carries
#'   the missing labels in its `absent` field and what was seen instead in its
#'   `observed` field.
#'
#' @export
#'
#' @examples
#' landuse <- tibble::tibble(Element = c("Area", "Area"), Value = c(1, 2))
#' check_labels_supplied(landuse, "Element", "Area")
#'
#' renamed <- tibble::tibble(Element = c("Area under cultivation"), Value = 1)
#' try(check_labels_supplied(renamed, "Element", "Area"))
check_labels_supplied <- function(
  data,
  column,
  labels,
  action = c("abort", "warn"),
  details = NULL
) {
  action <- rlang::arg_match(action)
  wanted <- unique(as.character(labels))
  if (length(wanted) == 0L || anyNA(wanted)) {
    cli::cli_abort("{.arg labels} must be a non-missing, non-empty vector.")
  }
  if (!rlang::has_name(data, column)) {
    .signal_absent_labels(wanted, column, character(), action, details)
    return(invisible(data))
  }
  if (nrow(data) == 0L) {
    return(invisible(data))
  }
  observed <- unique(as.character(data[[column]]))
  absent <- wanted[!wanted %in% observed]
  if (length(absent) == 0L) {
    return(invisible(data))
  }
  .signal_absent_labels(absent, column, observed, action, details)
  invisible(data)
}

#' Record which optional inputs a build consumed
#'
#' @description
#' Write the provenance stamp [check_inputs_supplied()] reads: a character
#' column naming the optional inputs this build actually had, comma-separated
#' and sorted, or `"none"` when it had none. The point of the stamp is that it
#' is a **label**: unlike a total, it cannot be satisfied by arithmetic, so a
#' consumer can tell an absent layer from a measured zero years later and
#' without the producer's arguments in hand.
#'
#' @param data A tibble to stamp.
#' @param supplied Keys of the inputs that were supplied. May be empty.
#' @param column Name of the stamp column.
#'
#' @return `data` with `column` added or overwritten.
#'
#' @export
#'
#' @examples
#' stamp_inputs_supplied(tibble::tibble(x = 1:2), c("ice", "water"))
#' stamp_inputs_supplied(tibble::tibble(x = 1:2), character())
stamp_inputs_supplied <- function(
  data,
  supplied,
  column = "inputs_supplied"
) {
  if (!rlang::is_character(supplied)) {
    cli::cli_abort("{.arg supplied} must be a character vector.")
  }
  supplied <- sort(unique(supplied[!is.na(supplied)]))
  label <- if (length(supplied) == 0L) {
    "none"
  } else {
    paste(supplied, collapse = ",")
  }
  data[[column]] <- label
  data
}

# The keys are what the message names and what the stamp is matched on. An
# unnamed entry is its own key, which keeps the common case a bare character
# vector.
.supplied_keys <- function(required) {
  if (!rlang::is_character(required) || length(required) == 0L) {
    cli::cli_abort("{.arg required} must be a non-empty character vector.")
  }
  keys <- rlang::names2(required)
  ifelse(nzchar(keys), keys, unname(required))
}

# Stamp first, columns second. A table whose producer wrote the stamp is judged
# on the stamp alone: it is the only evidence that survives a value being
# legitimately zero everywhere, and the only one no downstream sum can forge.
.absent_inputs <- function(data, required, keys, stamp) {
  named <- .stamped_keys(data, stamp)
  if (!is.null(named)) {
    return(setdiff(keys, named))
  }
  if (nrow(data) == 0L) {
    return(keys[!purrr::map_lgl(required, ~ rlang::has_name(data, .x))])
  }
  keys[purrr::map_lgl(required, ~ .input_is_absent(data, .x))]
}

.stamped_keys <- function(data, stamp) {
  if (is.null(stamp) || !rlang::has_name(data, stamp)) {
    return(NULL)
  }
  labels <- unique(stats::na.omit(as.character(data[[stamp]])))
  stringr::str_trim(unlist(stringr::str_split(labels, ",")))
}

.input_is_absent <- function(data, column) {
  if (!rlang::has_name(data, column)) {
    return(TRUE)
  }
  .input_is_vacuous(data[[column]])
}

# Vacuous means "carries no information a measurement would have carried". All
# missing is vacuous for any type; all zero is vacuous for a number, because a
# quantity that is zero everywhere on Earth is not a measurement of anything;
# all empty is vacuous for a label. Anything else is left alone -- an
# all-`FALSE` flag is a perfectly ordinary observation.
.input_is_vacuous <- function(values) {
  present <- values[!is.na(values)]
  if (length(present) == 0L) {
    return(TRUE)
  }
  if (is.numeric(present)) {
    return(all(present == 0))
  }
  if (is.character(present)) {
    return(all(!nzchar(present)))
  }
  FALSE
}

.signal_absent_inputs <- function(absent, action, details) {
  message <- c(
    "{cli::qty(length(absent))}Input{?s} {.field {absent}}
     {cli::qty(length(absent))}{?was/were} not supplied.",
    x = "An absent input and a measured zero are the same number downstream,
         so every total that reconciles will go on reconciling while this is
         missing.",
    details
  )
  if (identical(action, "abort")) {
    cli::cli_abort(
      message,
      class = "whep_absent_input",
      absent = absent,
      call = rlang::caller_env(2)
    )
  }
  cli::cli_warn(
    message,
    class = "whep_absent_input",
    absent = absent
  )
}

# Naming what IS there is the whole diagnostic value. "no rows matched" sends
# the reader looking for a data gap; "the column now says `Area under
# cultivation`" sends them to the rename that caused it.
.signal_absent_labels <- function(absent, column, observed, action, details) {
  shown <- utils::head(sort(observed), 6L)
  extra <- length(observed) - length(shown)
  seen <- if (length(observed) == 0L) {
    c(x = "{.field {column}} is not a column of this table at all.")
  } else if (extra > 0L) {
    c(
      i = "{.field {column}} holds {.val {shown}} and {extra}
           {cli::qty(extra)}other{?s}."
    )
  } else {
    c(i = "{.field {column}} holds {.val {shown}}.")
  }
  message <- c(
    "{cli::qty(length(absent))}Label{?s} {.val {absent}}
     {cli::qty(length(absent))}{?is/are} not in the {.field {column}}
     vocabulary.",
    x = "A filter on a label nothing carries matches no rows, and the sum of
         no rows is zero -- which no total, conservation check or row count
         downstream can tell from a measurement.",
    seen,
    details
  )
  if (identical(action, "abort")) {
    cli::cli_abort(
      message,
      class = c("whep_absent_label", "whep_absent_input"),
      absent = absent,
      observed = observed,
      call = rlang::caller_env(2)
    )
  }
  cli::cli_warn(
    message,
    class = c("whep_absent_label", "whep_absent_input"),
    absent = absent,
    observed = observed
  )
}
