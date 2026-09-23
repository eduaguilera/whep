# Named measurement conversions and non-mutating numeric-bound diagnostics
# (#378).
#
# Each conversion factor is an exact identity -- an SI prefix or a published
# reporting unit -- written once here so a call site reads
# `n_t * .kg_per_tonne()` instead of a bare `* 1000` whose unit direction the
# reader has to reconstruct. The functions return the same double literal the
# call sites used to inline, so replacing one changes no computed value.
#
# The bound diagnostic reports; it never clips, drops or renormalises. A value
# outside a physical range is a defect upstream, and rewriting it here would
# hide it.

# 1 tonne = 1000 kilograms. Exact, by definition.
.kg_per_tonne <- function() {
  1000
}

# 1 teragram = 1e9 kilograms. Exact, by definition of the SI prefixes.
.kg_per_teragram <- function() {
  1e9
}

# FAOSTAT ("1000 persons", "1000 No") and UN WPP ("PopMale", "PopFemale")
# publish population in thousands of persons.
.persons_per_thousand <- function() {
  1000
}

# Count the values of `x` that fall outside `[lower, upper]`, without touching
# `x`. `NA` (including `NaN`) is counted as missing, not as a violation, so a
# gap stays a gap; `Inf`/`-Inf` are counted as non-finite. Returns a one-row
# tibble so callers and tests can read each count by name.
.bound_violations <- function(x, lower = -Inf, upper = Inf) {
  .check_bound_rule(x, lower, upper)
  missing <- is.na(x)
  finite <- is.finite(x)
  tibble::tibble(
    n_values = length(x),
    n_missing = sum(missing),
    n_non_finite = sum(!missing & !finite),
    n_below = sum(finite & x < lower),
    n_above = sum(finite & x > upper)
  )
}

# Warn when any finite value of `x` falls outside `[lower, upper]` or any value
# is infinite, naming the quantity and the counts. Returns the diagnostic from
# `.bound_violations()` invisibly; `x` itself is never modified.
.warn_out_of_bounds <- function(x, what, lower = -Inf, upper = Inf) {
  diagnostic <- .bound_violations(x, lower, upper)
  bad <- diagnostic$n_non_finite + diagnostic$n_below + diagnostic$n_above
  if (bad > 0L) {
    cli::cli_warn(
      c(
        "{.field {what}} has {bad} value{?s} outside its physical range.",
        "i" = "Expected range: [{lower}, {upper}].",
        "i" = "Below: {diagnostic$n_below}; above: {diagnostic$n_above};
          infinite: {diagnostic$n_non_finite}.",
        "i" = "Values are reported unchanged, not clipped."
      ),
      class = "whep_out_of_bounds_warning"
    )
  }
  invisible(diagnostic)
}

# Reject a malformed rule rather than guess what it meant.
.check_bound_rule <- function(x, lower, upper) {
  if (!is.numeric(x)) {
    cli::cli_abort(
      "{.arg x} must be numeric, not {.obj_type_friendly {x}}.",
      class = "whep_bound_rule_error"
    )
  }
  if (!.is_single_number(lower) || !.is_single_number(upper) || lower > upper) {
    cli::cli_abort(
      "{.arg lower} and {.arg upper} must be single non-missing numbers with
       {.arg lower} <= {.arg upper}.",
      class = "whep_bound_rule_error"
    )
  }
  invisible(TRUE)
}

.is_single_number <- function(b) {
  is.numeric(b) && length(b) == 1L && !is.na(b)
}
