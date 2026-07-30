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
pre_env <- c(
  WHEP_POLITIES_MANIFEST = Sys.getenv("WHEP_POLITIES_MANIFEST", ""),
  WHEP_POLITIES_FAOSTAT_MAP = Sys.getenv("WHEP_POLITIES_FAOSTAT_MAP", ""),
  WHEP_POLITIES_CSV = Sys.getenv("WHEP_POLITIES_CSV", "")
)
Sys.setenv(
  WHEP_POLITIES_MANIFEST = "/nonexistent/polities_manifest.json",
  WHEP_POLITIES_FAOSTAT_MAP = "/nonexistent/faostat_area_polity_map.csv",
  WHEP_POLITIES_FAOSTAT_ALIASES = "/nonexistent/faostat_aliases.csv",
  WHEP_POLITIES_CSV = "/nonexistent/polities_database.csv"
)

results <- list()
# Each gate's wall-clock is printed alongside its verdict. Not decoration: the reason this
# script stopped being run before pushes is that nobody knew which gate was eating the time,
# so the whole thing got abandoned rather than the slow part addressed.
.gate_clock <- Sys.time()

record <- function(name, ok, detail = "") {
  now <- Sys.time()
  secs <- as.numeric(now - .gate_clock, units = "secs")
  .gate_clock <<- now
  results[[length(results) + 1L]] <<- list(
    name = name,
    ok = ok,
    detail = detail,
    secs = secs
  )
  cat(sprintf(
    "%-24s %s %6.1fs  %s\n",
    name,
    if (ok) "PASS" else "FAIL",
    secs,
    detail
  ))
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
#
# WHEP_CHECK_FAST=1 mirrors CI exactly by setting CI=true for the suite, which makes the four
# `skip_on_ci()` real-pin tests skip as they do there.
#
# Why that option exists, with every figure MEASURED on an otherwise idle machine — the two
# earlier versions of this comment were guesses and both were wrong:
#
#   air              0.8s
#   lintr           39.2s
#   suite mirrored  85.7s   4630 pass, 0 fail, 2 warn, 21 skip
#   ----------------------
#   whole script    ~2 min
#
#   suite NOT mirrored   ~25 min. The four skip_on_ci() real-pin tests are the entire
#                        difference; with them skipped the slowest single file is 5.4s.
#
# Two things made this hard to establish, both worth recording because they cost several
# rounds of wrong conclusions.
#
# First, every measurement I took while a 27-minute production build was running was
# meaningless. I read a starved run as evidence about cost, concluded "lintr dominates, several
# minutes", and wrote that here. Uncontended, lintr is 39 seconds.
#
# Second, `WHEP_CHECK_FAST=1` silently did nothing for one whole run: as.logical("1") is NA in
# R, not TRUE, so the flag read FALSE and the full suite ran while I waited for a two-minute one
# and drew conclusions from the delay. The flag now accepts 1/true/t/yes/y.
#
# The reason any of this matters: a pre-push gate that takes tens of minutes does not get
# waited for. Mine did not. It was started, starved, killed and restarted several times, and in
# the meantime I pushed a change to the canonical area key that broke 8 tests across three
# files. CI caught it; the gate that should have was still running.
#
# The default remains the full run, because those four tests are the only local coverage of
# the pin-backed paths and hiding them would trade one blind spot for another. But `fast` is
# what you want before a push.
# `as.logical("1")` is NA in R, not TRUE, so the obvious `WHEP_CHECK_FAST=1` silently did
# nothing — the flag read FALSE and the full 25-minute suite ran while I sat waiting for a
# two-minute one and drew conclusions from the delay. Accept the spellings people actually
# type.
fast <- tolower(trimws(Sys.getenv("WHEP_CHECK_FAST", ""))) %in%
  c("1", "true", "t", "yes", "y")
if (fast) {
  cat(
    "\n  WHEP_CHECK_FAST=1: mirroring CI, so the four skip_on_ci() real-pin tests\n",
    "  (soil carbon, carbon balance, grass natural carbon inputs, LUH2 landuse) will\n",
    "  skip. Run without it to exercise those locally.\n\n",
    sep = ""
  )
  old_ci <- Sys.getenv("CI", unset = NA)
  Sys.setenv(CI = "true")
  on.exit(
    if (is.na(old_ci)) Sys.unsetenv("CI") else Sys.setenv(CI = old_ci),
    add = TRUE
  )
}
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
  # Topics marked `@keywords internal` are omitted from a pkgdown site by design, so
  # requiring them in the index is wrong. Detected from the .Rd rather than added to
  # the name list above: this gate already carries three hardcoded exemptions, and a
  # fourth would have made the list the thing to maintain instead of the rule.
  #
  # The case that prompted it: whep_polity_columns, a doc-only topic holding the shared
  # description of the polity columns eight builders emit. It has no user-facing page
  # because it has no user-facing function.
  if (length(missing_topics) > 0L) {
    is_internal <- vapply(
      missing_topics,
      function(topic) {
        rd <- file.path("man", paste0(topic, ".Rd"))
        if (!file.exists(rd)) {
          return(FALSE)
        }
        any(grepl("\\keyword\\{internal\\}", readLines(rd, warn = FALSE)))
      },
      logical(1)
    )
    missing_topics <- missing_topics[!is_internal]
  }
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

# --- 5b. The upstream contract tests, run WITH the contracts present ---------
# Section 1 blinds this run to the published contracts because CI is blind to
# them. That is faithful, and it has a cost nobody accounted for: those
# assertions then execute NOWHERE. CI cannot run them (the files live on
# whep-polities#39 and are absent from a runner), and this script refuses to.
#
# It cost something real. `folded_into_aggregate` in
# test_upstream_faostat_agreement.R carries a bidirectional baseline whose whole
# purpose is to fail when a listed gap closes, so a fixed case cannot stay
# baselined. Twelve of its twenty-seven entries closed while the file skipped on
# every run, local and CI. The test was correct and simply never executed.
#
# So: a SECOND pass, over those files only, with the contracts located rather
# than neutralised. Not a relaxation of the first pass — both conditions now get
# exercised, which is the point, since each catches what the other cannot. When
# no contracts are found this reports SKIP with the paths it tried, rather than
# passing quietly.
# All THREE contract files, from ONE directory. The first version of this pass
# required only the manifest and the FAOSTAT map, and left WHEP_POLITIES_CSV to
# its own default -- which resolves to the sibling repo's MAIN branch. So it
# compared a branch manifest against a main CSV and reported three identity
# fields as disagreeing with upstream when upstream agreed with itself
# perfectly; the wiki, the branch CSV and this package's data all said AGO,
# while the main CSV still said ANG. Mixing sources produces exactly the class
# of finding this whole comparison exists to detect, which makes it worse than
# no check.
contract_files <- c(
  "polities_manifest.json",
  "faostat_area_polity_map.csv",
  "polities_database.csv"
)
# `pre_env` is captured at the top of this script, before section 1 overwrites
# these variables, so a developer who has already pointed them somewhere keeps
# that choice.
candidates <- unique(c(
  dirname(pre_env[["WHEP_POLITIES_MANIFEST"]]),
  dirname(pre_env[["WHEP_POLITIES_CSV"]]),
  path.expand("~/whep-polities/data/final"),
  # git worktrees of the sibling repo, where the branch under review lives
  Sys.glob("/tmp/*/data/final"),
  Sys.glob(path.expand("~/whep-polities*/data/final"))
))
candidates <- Filter(
  function(d) {
    !is.na(d) && nzchar(d) && all(file.exists(file.path(d, contract_files)))
  },
  candidates
)

.gate_clock <- Sys.time()
if (length(candidates) == 0L) {
  record(
    "upstream contract tests",
    TRUE,
    sprintf(
      "SKIP - no directory with %s found; tried %d location(s)",
      paste(contract_files, collapse = " + "),
      length(candidates)
    )
  )
} else {
  contract_dir <- candidates[[1]]
  Sys.setenv(
    WHEP_POLITIES_MANIFEST = file.path(contract_dir, "polities_manifest.json"),
    WHEP_POLITIES_FAOSTAT_MAP = file.path(
      contract_dir,
      "faostat_area_polity_map.csv"
    ),
    WHEP_POLITIES_CSV = file.path(contract_dir, "polities_database.csv")
  )
  # One test_local call with a regex filter, not four. Each call reloads the
  # package, and four reloads made this pass cost more than the suite it
  # supplements.
  contract_tests <- c(
    "upstream_faostat_agreement",
    "polities_upstream_contract",
    "faostat_unmapped_contract",
    "polity_output_coverage"
  )
  res <- tryCatch(
    testthat::test_local(
      filter = paste(contract_tests, collapse = "|"),
      reporter = "silent",
      stop_on_failure = FALSE
    ),
    error = function(e) NULL
  )
  totals <- c(pass = 0L, fail = 1L, skip = 0L)
  if (!is.null(res)) {
    df <- as.data.frame(res)
    totals <- c(
      pass = sum(df$passed),
      fail = sum(df$failed),
      skip = sum(df$skipped)
    )
  }
  record(
    "upstream contract tests",
    totals[["fail"]] == 0L,
    sprintf(
      "%d pass, %d fail, %d skip (contracts from %s)",
      totals[["pass"]],
      totals[["fail"]],
      totals[["skip"]],
      contract_dir
    )
  )
}

# --- 5c. system.file() paths that .Rbuildignore excludes ---------------------
# This script's whole premise is "run the gates the way CI runs them", and it has
# a structural blind spot it cannot fix by running more tests: it uses
# testthat::test_local(), which loads the SOURCE tree, where pkgload maps
# system.file() onto inst/. CI checks an INSTALLED package, where a
# build-ignored file is simply absent and system.file() returns "".
#
# That difference cost a five-platform CI failure. test_mueller_country_codes.R
# read inst/extdata/mueller_synthetic_n.csv, which .Rbuildignore excludes because
# the same data ships as the exported dataset whep::mueller_synthetic_n. Locally
# the path resolved and the test passed; on every CI platform read_csv("") errored.
# Two representations of one fact, and the test read the one that does not ship.
#
# No amount of test-running catches this from a source tree, so it is checked
# statically instead: any file a test reaches for through system.file() must not
# be excluded from the build.
ignore_path <- ".Rbuildignore"
test_files <- Sys.glob("tests/testthat/*.R")
if (file.exists(ignore_path) && length(test_files) > 0L) {
  patterns <- Filter(nzchar, trimws(readLines(ignore_path, warn = FALSE)))
  excluded <- character(0)
  for (tf in test_files) {
    src <- paste(readLines(tf, warn = FALSE), collapse = "\n")
    # system.file("dir", "name", ...) -> the inst-relative path it resolves to
    hits <- regmatches(
      src,
      gregexpr('system\\.file\\(\\s*"[^"]+"\\s*,\\s*"[^"]+"', src)
    )[[1]]
    for (h in hits) {
      parts <- regmatches(h, gregexpr('"[^"]+"', h))[[1]]
      parts <- gsub('"', "", parts)
      if (length(parts) < 2L || parts[[1]] == "..") {
        next
      }
      rel <- file.path("inst", parts[[1]], parts[[2]])
      hit <- vapply(
        patterns,
        function(p) {
          isTRUE(tryCatch(grepl(p, rel), error = function(e) FALSE))
        },
        logical(1)
      )
      if (any(hit)) {
        excluded <- c(excluded, sprintf("%s -> %s", basename(tf), rel))
      }
    }
  }
  record(
    "no test reads a build-ignored file",
    length(excluded) == 0L,
    if (length(excluded) == 0L) {
      sprintf("%d test file(s) scanned", length(test_files))
    } else {
      paste(
        "returns \"\" in an installed package:",
        paste(unique(excluded), collapse = "; ")
      )
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
