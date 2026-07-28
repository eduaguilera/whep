# Run the gates the way CI runs them.
#
# Why this exists, concretely: the polity tests read contracts published by the
# sibling whep-polities repository, located through WHEP_POLITIES_MANIFEST and
# WHEP_POLITIES_FAOSTAT_MAP. A developer with that checkout has those contracts
# available; a CI runner does not. So the same suite is MORE lenient locally, and
# a test can pass on a laptop and fail on a runner — which is what happened while
# this integration was being written, repeatedly, and each time the local run had
# been reported as green.
#
# Running the suite is not enough. It has to run under the conditions CI has.
#
# This script neutralises the upstream contract paths, then runs the three gates
# in the order CI does. `air` is invoked if it is on PATH; it is a separate binary
# rather than an R package, so its absence is reported rather than fatal.
#
# For lint it reuses the exact configuration in check_lint.R, which already
# mirrors .github/workflows/lint.yaml. Do not re-specify the linter set here: the
# workflow disables object_usage_linter, line_length_linter, indentation_linter
# and commas_linter, so a default lintr run reports dozens of lints CI ignores and
# buries the ones it does not.
#
# Usage, from the package root:
#   Rscript inst/scripts/check_like_ci.R

# --- 1. Make the upstream contracts unreadable, as on a runner ---------------
# Pointed at a path that cannot exist rather than unset, so the tests take their
# documented skip branch instead of falling back to a default location that might
# happen to resolve on this machine.
Sys.setenv(
  WHEP_POLITIES_MANIFEST = "/nonexistent/polities_manifest.json",
  WHEP_POLITIES_FAOSTAT_MAP = "/nonexistent/faostat_area_polity_map.csv",
  WHEP_POLITIES_FAOSTAT_ALIASES = "/nonexistent/faostat_aliases.csv"
)

results <- list()
record <- function(name, ok, detail = "") {
  results[[length(results) + 1L]] <<- list(
    name = name,
    ok = ok,
    detail = detail
  )
  cat(sprintf("%-24s %s %s\n", name, if (ok) "PASS" else "FAIL", detail))
}

cat("Running the CI gates with the upstream contracts unreachable.\n\n")

# --- 2. Formatting -----------------------------------------------------------
air <- Sys.which("air")
if (nzchar(air)) {
  out <- suppressWarnings(
    system2(air, c("format", "--check", "."), stdout = TRUE, stderr = TRUE)
  )
  status <- attr(out, "status")
  ok <- is.null(status) || identical(status, 0L)
  record(
    "air format --check",
    ok,
    if (ok) "" else paste(utils::head(out, 3), collapse = " | ")
  )
} else {
  cat(sprintf(
    "%-24s SKIP air not on PATH (CI resolves it via setup-air@v1)\n",
    "air format --check"
  ))
}

# --- 3. Lint -----------------------------------------------------------------
lints <- eval(parse(file = file.path("inst", "scripts", "check_lint.R")))
n_lints <- length(lints)
record("lintr (CI config)", n_lints == 0L, sprintf("%d lint(s)", n_lints))
if (n_lints > 0L) {
  for (x in utils::head(lints, 15)) {
    cat(sprintf(
      "    %s:%s [%s] %s\n",
      x$filename,
      x$line_number,
      x$linter,
      x$message
    ))
  }
}

# --- 4. Tests ----------------------------------------------------------------
# load_all rather than an installed build: this is a pre-push check, and the point
# is to exercise the working tree.
suppressMessages(devtools::load_all(quiet = TRUE))
res <- as.data.frame(
  testthat::test_dir(
    "tests/testthat",
    reporter = "silent",
    stop_on_failure = FALSE
  )
)
n_fail <- sum(res$failed)
record(
  "testthat suite",
  n_fail == 0L,
  sprintf(
    "%d pass, %d fail, %d warn, %d skip",
    sum(res$passed),
    n_fail,
    sum(res$warning),
    sum(res$skipped)
  )
)
if (n_fail > 0L) {
  bad <- res[res$failed > 0L, c("file", "test", "failed")]
  print(bad, row.names = FALSE)
}

# --- 5. Verdict --------------------------------------------------------------
cat("\n")
failed <- Filter(function(r) !r$ok, results)
if (length(failed) == 0L) {
  cat("All reproducible gates pass.\n")
  cat(
    "NOT covered here: R CMD check across the platform matrix, and pkgdown.\n"
  )
} else {
  cat(sprintf(
    "%d gate(s) failed: %s\n",
    length(failed),
    paste(vapply(failed, function(r) r$name, ""), collapse = ", ")
  ))
  quit(status = 1L)
}
