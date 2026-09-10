# Completeness against an expected key lattice: the absent-ROW half of the
# absent-input family (whep#1073, sibling of whep#1034).
#
# The guards for whep#1034 assert what is PRESENT IN A FRAME -- a column
# exists, a value vocabulary still carries the labels a filter selects on.
# Neither can see an input that is absent because its ROW never arrived.
# Nothing is null, so no coalesce / replace_na / na.rm policy is involved; no
# column is missing, so no column-existence check fires; and the aggregate over
# the rows that did arrive is a perfectly ordinary number.
#
# The assertion therefore has to be made against an EXPECTED KEY SET rather
# than against the contents of the rows that arrived, which is what this file
# provides.

#' Report the expected keys that no row supplies
#'
#' @description
#' Enumerate the keys an expected lattice requires and the data does not
#' supply, returning one row per missing key. A zero-row result means every
#' expected key is present in every group.
#'
#' This is the detector [check_keys_complete()] signals on, exposed separately
#' because a caller that wants to *act* on the gaps -- report them, drop the
#' groups that carry them -- needs the keys themselves rather than a condition.
#'
#' @section Why a total, an identity or a row count cannot do this:
#' An absent month drops its row, so a year sums **eleven** months, and the
#' eleven-month total is a plausible number that no `na.rm` choice can question
#' -- there is no `NA` anywhere. A conservation check over the rows that
#' arrived still balances, because both sides are built from the same eleven.
#' A row count still looks reasonable, because it is reasonable: it is the
#' count of the rows that came. Only an assertion against the key set that
#' *should* have arrived distinguishes the two.
#'
#' @section What the expected lattice may be:
#' `expected` is one of three forms, and the form decides the cost.
#'
#' * A **named list** of value vectors, e.g. `list(month = 1:12)`. The expected
#'   key set is the cross product of the elements, required in every group.
#' * A **data frame** of expected key rows, e.g. every `(year, area_code)` pair
#'   a polity vocabulary admits. Use this for a lattice that varies along one
#'   of its own keys: put the varying key in the frame and do not group on it,
#'   and the whole per-year vocabulary is one key set.
#' * A **function** of `data` returning either of the above, for a rule that
#'   has to read the data first -- the years it actually spans, the model's
#'   cell list.
#'
#' The contract is containment, not equality: every expected key must occur,
#' and a key the data carries beyond the expected set is not a failure.
#'
#' @section Cost, which is the reason the design is shaped this way:
#' A full cross-join assertion is free on twelve months and ruinous on tens of
#' millions of cell-months, so completeness is established in stages and each
#' stage runs only when the cheaper one could not settle it.
#'
#' 1. **Scalar counts.** When the observed vocabulary of each key column lies
#'    inside the expected one, the lattice is complete if and only if the
#'    number of distinct key tuples equals the number of groups times the size
#'    of the expected key set. That is two [dplyr::n_distinct()] passes over
#'    column vectors: no frame is copied, no lattice is built and no grouped
#'    result is materialised.
#' 2. **A distinct-key anti-join**, reached only when the counts disagree (or
#'    when the observed vocabulary is not inside the expected one, which makes
#'    the count identity unsound). This works from the *distinct* key tuples,
#'    not from every row.
#' 3. **Enumeration of the incomplete groups only.** The lattice is expanded
#'    for the groups that already failed their count, never for the whole
#'    table. The enumeration is the diagnostic, not the detector.
#'
#' One rule follows from stage 1 and is worth knowing before wiring a check on
#' a large table: **narrow the data to the keys you are asserting about**. An
#' expected set deliberately smaller than a column's observed vocabulary --
#' "months 1 and 12 must be here", on a frame that also holds 2 to 11 --
#' defeats the count identity and sends every call down stage 2, even when
#' nothing is missing. Filtering to the two months first keeps the assertion
#' identical and the cost scalar; `.wb_check_swc_boundary()` does exactly this.
#'
#' Measured on the 86,781,420-row `lpjml-soc-hydrology` cell-month lattice
#' (7,231,785 cell-year groups, `lon`/`lat`/`year`/`month`): stage 1 settles it
#' in **13.9 s with no measurable allocation above the frame itself**. For
#' comparison, a grouped distinct count of the same lattice takes 66.7 s and
#' +166 MB, and materialising the full lattice and anti-joining it takes 36.6 s
#' and +2.1 GB. Memory is the binding constraint on this chain (whep#624),
#' which is why the cheapest stage is also the one that allocates nothing;
#' stage 2 costs a further 6.4 s and +2.0 GB there, and is paid only by a
#' lattice that is already broken.
#'
#' @section What it cannot see:
#' Three absences pass this check, and a caller must not read a pass as more
#' than it is.
#'
#' * A **group that is absent entirely**. No rows at all means no group, and
#'   `.by` can only name groups the data already carries. Assert the expected
#'   *group* set the same way: put the grouping keys in the data-frame form of
#'   `expected` and pass no `.by`.
#' * A key that is present but **wrong**. This is a completeness check, not a
#'   validity one; month 13 is not a failure here, only an absent month 12 is.
#' * A lattice that is complete while its **values** are absent as zeros. That
#'   is the other half of the family: assert the input was supplied, not that
#'   its keys are all there (whep#1034).
#'
#' @param data A tibble to check.
#' @param expected The expected key set: a named list of value vectors, a data
#'   frame of expected key rows, or a function of `data` returning either.
#' @param .by Character vector of grouping columns. Every group must carry the
#'   whole expected key set. `NULL` (default) treats the table as one group.
#' @param max_gaps Refuse rather than enumerate when more keys than this are
#'   missing. A result that large is not a partially incomplete lattice, it is
#'   an almost entirely absent one, and enumerating it helps nobody.
#'
#' @return A tibble with the `.by` columns and the expected key columns, one
#'   row per missing key, sorted by group. Zero rows when the lattice is
#'   complete.
#'
#' @export
#'
#' @examples
#' eleven <- tibble::tibble(year = 2000L, month = 1:11)
#' key_lattice_gaps(eleven, list(month = 1:12), .by = "year")
#'
#' # Complete: zero rows.
#' key_lattice_gaps(
#'   tibble::tibble(year = 2000L, month = 1:12),
#'   list(month = 1:12),
#'   .by = "year"
#' )
key_lattice_gaps <- function(data, expected, .by = NULL, max_gaps = 1e6) {
  by_cols <- .lattice_by_cols(.by)
  wanted <- .lattice_wanted(expected, data)
  .lattice_check_args(data, wanted, by_cols, max_gaps)
  if (.lattice_complete_by_count(data, wanted, by_cols)) {
    return(.lattice_no_gaps(data, wanted, by_cols))
  }
  observed <- .lattice_distinct_keys(data, names(wanted), by_cols)
  .lattice_enumerate_gaps(observed, wanted, by_cols, max_gaps)
}

#' Refuse an aggregate built over an incomplete key lattice
#'
#' @description
#' Assert that every key an expected lattice requires is supplied by some row,
#' and abort naming the keys that are **missing** rather than the rows that are
#' present. This is the absent-row sibling of the absent-column and
#' absent-label guards: an aggregate over eleven of twelve months is a number
#' with nothing wrong with it except that it is wrong, and no `NA` policy,
#' identity or row count can tell it from a twelve-month one.
#'
#' The gaps themselves come from [key_lattice_gaps()]; see its documentation
#' for the forms `expected` may take and for the staged cost that makes the
#' check affordable on a lattice of tens of millions of keys.
#'
#' @section Abort versus warn is the caller's decision, not this function's:
#' Both are wrong as a blanket policy, so neither is imposed.
#'
#' * **Abort** is right where the aggregate is the deliverable and a
#'   short-summed one would ship: an eleven-month annual water balance drives
#'   nitrogen leaching downstream, and no consumer of the annual total can
#'   recover the twelfth month. It is wrong where a legitimately partial period
#'   exists -- a model run still writing its current year -- because it blocks
#'   a build over the complete years for the sake of an incomplete one the
#'   caller never asked for.
#' * **Warn** keeps that build alive, and is what a caller who has decided the
#'   gap is acceptable should select explicitly. It must not be the default:
#'   two warnings were already being printed when whep#1010 shipped 533 Mha of
#'   lakes and glaciers as land, so a warning inside a build that runs for
#'   hours gates nothing.
#'
#' The default is therefore `"abort"`, the most rigorous option, and a caller
#' who wants the build to continue says so and is recorded as having said so.
#' A third policy -- drop the incomplete groups, so the aggregate is absent
#' rather than wrong -- is deliberately not an `action` here, because this
#' function returns `data` unchanged and a silent drop is the very thing being
#' fixed. Do it with [key_lattice_gaps()] and an anti-join at the site, where
#' the reader can see which groups went and why.
#'
#' @param data A tibble to check.
#' @param expected The expected key set; see [key_lattice_gaps()].
#' @param .by Character vector of grouping columns, or `NULL` for one group.
#' @param action `"abort"` (default) to raise, `"warn"` to carry on.
#' @param details Extra `cli` bullets appended to the message, for the caller
#'   to name its source and its remedy. Interpolated in the caller's own
#'   environment, so a bullet may name the caller's variables.
#'
#' @return `data`, invisibly. Raises or warns with class
#'   `whep_incomplete_lattice` (and `whep_absent_input`) when a key is
#'   missing; the condition carries the gap tibble in its `missing` field.
#'
#' @export
#'
#' @examples
#' complete <- tibble::tibble(year = 2000L, month = 1:12, value = 1)
#' check_keys_complete(complete, list(month = 1:12), .by = "year")
#'
#' eleven <- tibble::tibble(year = 2000L, month = c(1:6, 8:12), value = 1)
#' try(check_keys_complete(eleven, list(month = 1:12), .by = "year"))
check_keys_complete <- function(
  data,
  expected,
  .by = NULL,
  action = c("abort", "warn"),
  details = NULL
) {
  action <- rlang::arg_match(action)
  gaps <- key_lattice_gaps(data, expected, .by = .by)
  if (nrow(gaps) == 0L) {
    return(invisible(data))
  }
  .signal_lattice_gaps(gaps, action, details)
  invisible(data)
}

# ---- Private helpers --------------------------------------------------

.lattice_by_cols <- function(.by) {
  if (is.null(.by)) {
    return(character(0))
  }
  if (!rlang::is_character(.by)) {
    cli::cli_abort("{.arg .by} must be a character vector or {.code NULL}.")
  }
  .by
}

# Resolve `expected` to a tibble of expected key rows. A named list becomes the
# cross product of its elements; a data frame is taken as given; a function is
# called on the data first, so a rule can read the span it has to cover.
.lattice_wanted <- function(expected, data) {
  if (is.function(expected)) {
    expected <- expected(data)
  }
  if (is.data.frame(expected)) {
    return(dplyr::distinct(tibble::as_tibble(expected)))
  }
  if (!rlang::is_list(expected) || !.lattice_all_named(expected)) {
    cli::cli_abort(c(
      "{.arg expected} must be a named list, a data frame, or a function
       returning one of those.",
      i = "Got {.cls {class(expected)}}."
    ))
  }
  dplyr::distinct(rlang::exec(tidyr::expand_grid, !!!expected))
}

.lattice_all_named <- function(expected) {
  names <- rlang::names2(expected)
  length(expected) > 0L && all(nzchar(names)) && !anyNA(names)
}

.lattice_check_args <- function(data, wanted, by_cols, max_gaps) {
  if (!is.numeric(max_gaps) || length(max_gaps) != 1L || max_gaps < 0) {
    cli::cli_abort("{.arg max_gaps} must be a single non-negative number.")
  }
  needed <- c(by_cols, names(wanted))
  absent <- needed[!purrr::map_lgl(needed, \(col) rlang::has_name(data, col))]
  if (length(absent) > 0L) {
    cli::cli_abort(c(
      "{cli::qty(length(absent))}Column{?s} {.field {absent}}
       {cli::qty(length(absent))}{?is/are} not in {.arg data}.",
      i = "A key lattice can only be checked against columns that exist; an
           absent column is a different question from an absent row."
    ))
  }
  if (nrow(wanted) == 0L) {
    cli::cli_abort("{.arg expected} resolved to an empty key set.")
  }
}

# Stage 1: the scalar-count fast path. Sound only when every observed key value
# lies inside the expected vocabulary for its column -- otherwise a surplus key
# can make the count come out right while an expected one is missing -- and
# only for a cross-product expected set, whose size per group is known.
# Returns FALSE ("could not settle it") rather than guessing, which costs the
# caller stage 2 and never costs it a wrong answer.
#
# n_distinct() is given the column vectors rather than the frame so that no
# copy of `data` is made: on the 86.8M-row cell-month lattice this is the
# difference between no measurable allocation and +2 GB.
.lattice_complete_by_count <- function(data, wanted, by_cols) {
  key_cols <- names(wanted)
  if (!.lattice_is_cross_product(wanted)) {
    return(FALSE)
  }
  inside <- purrr::map_lgl(key_cols, function(col) {
    length(setdiff(unique(data[[col]]), wanted[[col]])) == 0L
  })
  if (!all(inside)) {
    return(FALSE)
  }
  n_groups <- .lattice_n_distinct(data, by_cols)
  n_tuples <- .lattice_n_distinct(data, c(by_cols, key_cols))
  n_tuples == n_groups * nrow(wanted)
}

# Distinct tuple count over `cols`, without copying the frame. No columns means
# one group: the whole table.
.lattice_n_distinct <- function(data, cols) {
  if (length(cols) == 0L) {
    return(1L)
  }
  rlang::exec(dplyr::n_distinct, !!!unname(as.list(data[cols])))
}

# Whether the expected frame is the full cross product of its own columns, so
# that its size per group is nrow(wanted) and the count identity applies. True
# by construction for the named-list form; tested rather than assumed, so a
# caller passing a rectangular data frame gets the fast path too.
.lattice_is_cross_product <- function(wanted) {
  sizes <- purrr::map_int(wanted, \(col) length(unique(col)))
  prod(sizes) == nrow(wanted)
}

# The zero-row gap tibble, carrying the same columns and types a real gap
# would, so a caller can bind or join against it without a special case.
.lattice_no_gaps <- function(data, wanted, by_cols) {
  dplyr::bind_cols(
    data[0L, by_cols, drop = FALSE],
    wanted[0L, , drop = FALSE]
  )
}

# Stage 2: the distinct (group, key) tuples, which the enumeration works from
# rather than from the rows.
.lattice_distinct_keys <- function(data, key_cols, by_cols) {
  dplyr::distinct(data[c(by_cols, key_cols)])
}

# Stage 3: enumerate the gaps, expanding the lattice for the incomplete groups
# only.
.lattice_enumerate_gaps <- function(observed, wanted, by_cols, max_gaps) {
  key_cols <- names(wanted)
  if (length(by_cols) == 0L) {
    return(.lattice_anti_join(wanted, observed, key_cols, max_gaps))
  }
  groups <- .lattice_incomplete_groups(observed, wanted, by_cols, key_cols)
  full <- dplyr::cross_join(groups, wanted)
  .lattice_anti_join(full, observed, c(by_cols, key_cols), max_gaps)
}

# The groups that cannot be complete, by count alone: a group carrying fewer
# distinct in-lattice keys than the lattice requires is missing at least one.
# Counting first is what keeps the expansion proportional to the defect rather
# than to the table.
.lattice_incomplete_groups <- function(observed, wanted, by_cols, key_cols) {
  observed |>
    dplyr::semi_join(wanted, by = key_cols) |>
    dplyr::count(dplyr::pick(dplyr::all_of(by_cols)), name = "n_keys") |>
    dplyr::filter(.data$n_keys < nrow(wanted)) |>
    dplyr::select(dplyr::all_of(by_cols))
}

.lattice_anti_join <- function(full, observed, join_cols, max_gaps) {
  gaps <- dplyr::anti_join(full, observed, by = join_cols)
  if (nrow(gaps) > max_gaps) {
    cli::cli_abort(c(
      "{nrow(gaps)} expected keys are missing, more than {.arg max_gaps}
       ({max_gaps}).",
      i = "An input this incomplete is an absent input, not a lattice with
           gaps in it; enumerating every missing key helps nobody.",
      i = "Raise {.arg max_gaps} to see them anyway."
    ))
  }
  dplyr::arrange(gaps, dplyr::pick(dplyr::all_of(join_cols)))
}

# Name the MISSING keys, not the rows that arrived. The keys are what sends a
# reader to the cause: "year=2023, month=12 is absent" is a truncated run,
# while "58,795 rows arrived" is no information at all.
.signal_lattice_gaps <- function(gaps, action, details) {
  caller <- rlang::caller_env(2)
  message <- c(
    "{nrow(gaps)} expected key{?s} {?is/are} missing from the lattice.",
    x = "An aggregate over the keys that did arrive is a plausible number that
         no total, identity, {.code na.rm} choice or row count can tell from a
         complete one -- nothing is missing as an {.val NA}, the rows are
         simply not there.",
    i = "Missing: {.val {shown$labels}}{shown$more}",
    details
  )
  # `details` is the caller's prose and may name the caller's own variables, so
  # cli has to interpolate in an environment that carries both: the locals this
  # message needs, parented on the caller's frame. Without this a `{var}` in a
  # caller's bullet silently resolves to `stats::var` and cli dies trying to
  # paste a closure.
  envir <- rlang::env(caller, gaps = gaps, shown = .lattice_gap_labels(gaps))
  if (identical(action, "abort")) {
    cli::cli_abort(
      message,
      class = c("whep_incomplete_lattice", "whep_absent_input"),
      missing = gaps,
      call = caller,
      .envir = envir
    )
  }
  cli::cli_warn(
    message,
    class = c("whep_incomplete_lattice", "whep_absent_input"),
    missing = gaps,
    .envir = envir
  )
}

# Up to six missing keys as "year=2023, month=12" strings, plus a count of the
# rest. Six is enough to see the pattern -- one month, one year, one cell --
# and short enough to read in a build log.
.lattice_gap_labels <- function(gaps) {
  head_rows <- utils::head(gaps, 6L)
  labels <- purrr::map_chr(seq_len(nrow(head_rows)), function(i) {
    row <- head_rows[i, , drop = FALSE]
    paste(
      paste0(names(row), "=", purrr::map_chr(row, \(col) format(col))),
      collapse = ", "
    )
  })
  extra <- nrow(gaps) - nrow(head_rows)
  list(
    labels = labels,
    more = if (extra > 0L) paste0(" and ", extra, " more") else ""
  )
}
