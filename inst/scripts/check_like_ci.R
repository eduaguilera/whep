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
raw <- testthat::test_dir(
  "tests/testthat",
  reporter = "silent",
  stop_on_failure = FALSE
)
res <- as.data.frame(raw)
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

# A warning is indistinguishable from a pass in that summary line too, and one
# warning class in particular means an assertion tested NOTHING: reading a column
# that does not exist returns NULL, and NULL is silently benign inside the usual
# checks — `is.na(NULL)` is `logical(0)`, so `all()` of it is TRUE and `any()` of it
# is FALSE. The assertion passes vacuously.
#
# This branch has been bitten twice. Renaming a `polity_code` column to
# `polity_prefix` left a test reading the old name and it kept passing; renaming the
# crosswalk's `reporting_polity_code` to `reporting_polity_prefix` did the same to a
# different test. Both times the only signal was this script's warning count going
# from 2 to 3 while failures stayed at 0 — something a summary line does not
# highlight and a green badge actively hides.
#
# So promote that class from a counted warning to a failed gate, naming the test.
#
# The first attempt at this was a globalCallingHandlers() backstop in
# tests/testthat/setup.R, which would also have covered CI. It cannot work: testthat
# sources setup.R with handlers already on the stack, and globalCallingHandlers()
# refuses to install there. Hence the check lives in this pre-push harness instead.
#
# Scope, stated honestly: only tibbles warn on absent-column access. `data.frame` and
# `data.table` return NULL in complete silence, so a test that calls as.data.frame()
# first is not covered by anything here. The durable habit is to assert a column
# exists before asserting anything about its contents; this gate is the backstop.
vacuous <- do.call(
  rbind,
  lapply(raw, function(tst) {
    warns <- Filter(
      function(e) inherits(e, "expectation_warning"),
      tst$results
    )
    msgs <- vapply(warns, conditionMessage, character(1))
    hit <- grepl("Unknown or uninitialised column", msgs, fixed = TRUE)
    if (!any(hit)) {
      return(NULL)
    }
    data.frame(
      file = tst$file,
      test = tst$test,
      msg = trimws(msgs[hit]),
      stringsAsFactors = FALSE
    )
  })
)
record(
  "no vacuous column access",
  is.null(vacuous),
  if (is.null(vacuous)) {
    "no test reads a column that does not exist"
  } else {
    sprintf("%d assertion(s) read an absent column", nrow(vacuous))
  }
)
if (!is.null(vacuous)) {
  cat("\n  These assertions pass without testing anything:\n")
  for (i in seq_len(nrow(vacuous))) {
    cat(sprintf(
      "    %s: %s\n      %s\n",
      vacuous$file[i],
      vacuous$test[i],
      vacuous$msg[i]
    ))
  }
}

# A skip is indistinguishable from a pass in that summary line, which is how the
# assertion guarding this integration's ORIGINAL defect — area codes resolving to
# retired polities — went unrun on CI for the whole life of the branch while the branch
# read as green. So name what was silenced.
#
# The upstream-contract skips are unavoidable rather than an oversight: whep-polities is
# a PRIVATE repository, so CI cannot clone it without a cross-repo token, and these
# assertions compare this package against files only that repo publishes. Closing the
# gap needs a secret, which is a maintainer decision. Everything checkable WITHOUT
# upstream has been moved into tests that run unconditionally
# (test_crosswalk_polity_agreement.R, and the alias/polity agreement checks).
skipped <- res[res$skipped > 0, c("file", "test")]
if (nrow(skipped) > 0L) {
  upstream <- grepl("upstream|contract", skipped$file, ignore.case = TRUE)
  cat(sprintf(
    "\n  %d test(s) SKIPPED — %d needing the upstream contracts, %d needing external data:\n",
    nrow(skipped),
    sum(upstream),
    sum(!upstream)
  ))
  for (i in which(upstream)) {
    cat(sprintf("    [upstream] %s: %s\n", skipped$file[i], skipped$test[i]))
  }
  if (any(!upstream)) {
    cat(sprintf(
      "    [external] %s\n",
      paste(unique(skipped$file[!upstream]), collapse = ", ")
    ))
  }
}
if (n_fail > 0L) {
  bad <- res[res$failed > 0L, c("file", "test", "failed")]
  print(bad, row.names = FALSE)
}

# --- 5. pkgdown reference index ----------------------------------------------
# Not the whole site build, which is slow and needs network — just the check that
# actually fails: pkgdown errors if an exported topic is missing from
# _pkgdown.yml's index. That is how the pkgdown job broke after
# `resolve_polity_label()` and `polity_label_aliases` were added, and it is cheap
# to catch here.
topics <- sub("\\.Rd$", "", basename(list.files("man", pattern = "\\.Rd$")))
if (length(topics) > 0L && file.exists("_pkgdown.yml")) {
  cfg <- yaml::read_yaml("_pkgdown.yml")
  indexed <- trimws(unlist(strsplit(
    unlist(lapply(cfg$reference, function(x) x$contents)),
    "\n"
  )))
  # Selector helpers match many topics at once; they cannot be compared by name.
  indexed <- indexed[
    nzchar(indexed) &
      !grepl("^(starts_with|matches|has_keyword|ends_with)", indexed)
  ]
  missing_topics <- setdiff(topics, indexed)
  missing_topics <- missing_topics[
    !grepl("^(whep-package|reexports|pipe)$", missing_topics)
  ]
  record(
    "pkgdown index",
    length(missing_topics) == 0L,
    if (length(missing_topics) == 0L) {
      sprintf("%d topics all indexed", length(topics))
    } else {
      paste("missing:", paste(utils::head(missing_topics, 6), collapse = ", "))
    }
  )
}

# --- 6. Verdict --------------------------------------------------------------
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
