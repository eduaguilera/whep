# Condition-capturing machinery for inst/scripts/run_nitrogen_balance.R's
# nbd_stage(). Split out from the script (rather than left at script scope)
# because the script executes its pipeline top to bottom the moment it is
# sourced -- so the only way to unit-test this without a live pin/raster
# environment is to keep the generic, reusable pieces here (whep#1288).
#
# nbd_stage() used to wrap every stage in
# suppressMessages(suppressWarnings(...)), which discarded each stage's
# reports entirely: the console never showed them and neither did the
# WHEP_NBD_OUT result, including the nitrogen-balance's own accounts of what
# it moved or dropped (.ni_report_reallocated(), .ni_warn_stranded_dropped(),
# .ni_report_unattributed() in R/n_balance_inputs.R; .warn_unclassified_feed()
# in R/feed_intake_redistribute.R). These two helpers let nbd_stage() capture
# each condition instead of suppressing it, so it can be recorded, printed and
# saved rather than lost.

# Run `expr`, muffling every warning and message it raises instead of losing
# them, and handing back what was said instead of discarding it.
#
# @param expr An expression to evaluate.
# @return A list with `value` (the result, or -- mirroring
#   `tryCatch(expr, error = function(e) e)` -- a condition of class `"error"`
#   if `expr` aborted; check with `inherits(value, "error")`) and
#   `conditions`, a tibble with one row per captured warning or message, in
#   the order raised: `class` (`"warning"` or `"message"`) and `message` (its
#   text, trimmed of the trailing newline `message()` conditions carry).
.nbd_capture_conditions <- function(expr) {
  log <- new.env(parent = emptyenv())
  log$conditions <- list()
  value <- withCallingHandlers(
    tryCatch(force(expr), error = function(e) e),
    warning = function(cnd) .nbd_log_condition(log, cnd, "warning"),
    message = function(cnd) .nbd_log_condition(log, cnd, "message")
  )
  list(value = value, conditions = .nbd_bind_conditions(log$conditions))
}

# Append one condition to `log$conditions` and muffle it, so it is recorded
# rather than printed. `log` is an environment, so the append is visible to
# .nbd_capture_conditions() without a superassignment.
.nbd_log_condition <- function(log, cnd, class) {
  log$conditions[[length(log$conditions) + 1L]] <- tibble::tibble(
    class = class,
    message = trimws(conditionMessage(cnd))
  )
  restart <- if (class == "warning") "muffleWarning" else "muffleMessage"
  invokeRestart(restart)
}

# dplyr::bind_rows(list()) already returns a zero-row tibble, but with no
# columns -- awkward for a caller filtering on `class`/`message` downstream,
# so the empty case gets the same two columns as the populated one.
.nbd_bind_conditions <- function(conditions) {
  if (length(conditions) == 0L) {
    return(tibble::tibble(class = character(), message = character()))
  }
  dplyr::bind_rows(conditions)
}

# Build one coverage-table row for nbd_stage(), including the conditions
# .nbd_capture_conditions() captured as a nested tibble -- so a caller reading
# the WHEP_NBD_OUT result (or `dplyr::bind_rows()` of many stages) can recover
# what each stage warned or reported, not only its status.
#
# @param label Stage label.
# @param status `"ok"`, `"FAIL"` or `"skip"`.
# @param seconds Elapsed seconds.
# @param rows Row or list length of the stage's value, or `NA_integer_`.
# @param detail Error message or skip reason, or `NA_character_`.
# @param conditions A tibble from `.nbd_capture_conditions()` (`class`,
#   `message`), or `NULL` for a stage that never ran (a skip).
# @return A one-row tibble: `input`, `status`, `seconds`, `rows`, `detail`,
#   and `conditions` (a list-column holding that tibble).
.nbd_stage_row <- function(
  label,
  status,
  seconds,
  rows,
  detail,
  conditions = NULL
) {
  if (is.null(conditions)) {
    conditions <- tibble::tibble(class = character(), message = character())
  }
  tibble::tibble(
    input = label,
    status = status,
    seconds = seconds,
    rows = rows,
    detail = if (is.na(detail)) {
      NA_character_
    } else {
      substr(gsub("\\s+", " ", detail), 1, 1200)
    },
    conditions = list(conditions)
  )
}
