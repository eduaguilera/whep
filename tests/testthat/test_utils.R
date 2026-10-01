# Package-wide guard, not a per-script test file (#1129).
#
# `R/utils.R` is one hand-maintained `utils::globalVariables()` call. Nothing
# generates it, so a change that introduces an NSE symbol and forgets to declare
# it ships a gap. `R CMD check` does report each one -- "no visible binding for
# global variable" -- but r-lib/actions' check-r-package fails on warnings and
# above, never on notes, so the gap merges green. That is exactly how #1114
# landed with `predecessor` and `reporting_polity_code` undeclared, and why
# #1135 had to be opened after it.
#
# The first test runs the same scan `R CMD check` runs, with the same settings,
# and fails instead of noting. It reads the namespace rather than `R/`, so it
# holds under `R CMD check` (installed package, no `R/` directory) exactly as
# under `devtools::test()`.
#
# The last two assert the preconditions that make the `R/utils.R merge=union`
# line in `.gitattributes` safe. Those need the sources, and skip where `R/` is
# absent.

# Scanner ----------------------------------------------------------------

# Names `R CMD check` suppresses on top of whatever R/utils.R declares.
# `tools:::.check_code_usage_in_package()` adds exactly these three.
.suppressed_global_names <- function() {
  c(
    ".Generic",
    ".Method",
    ".Class",
    utils::globalVariables(package = "whep")
  )
}

# The settings `R CMD check` scans with. `skipWith = TRUE` is load-bearing:
# without it the scan also reports every symbol read inside a `with()` block,
# which on current main is 15 more names -- `.century_rhs()`'s state and
# parameter list -- none of which is a defect or is meant to be declared.
.package_usage_messages <- function() {
  .local_first_src_info_only()
  found <- character()
  codetools::checkUsageEnv(
    asNamespace("whep"),
    report = function(msg) found <<- c(found, msg),
    skipWith = TRUE,
    suppressPartialMatchArgs = FALSE,
    suppressLocalUnused = TRUE,
    suppressUndefined = .suppressed_global_names()
  )
  unique(found)
}

# The same scan over a single function, for the guard-the-guard probe.
.probe_usage_messages <- function(probe) {
  .local_first_src_info_only()
  found <- character()
  codetools::checkUsage(
    probe,
    name = "probe",
    report = function(msg) found <<- c(found, msg),
    skipWith = TRUE,
    suppressPartialMatchArgs = FALSE,
    suppressLocalUnused = TRUE,
    suppressUndefined = .suppressed_global_names()
  )
  unique(found)
}

# Speed, not coverage (#1349). For every use of every local variable, codetools
# appends one row to a per-variable `srcinfo` data frame with `rbind()`, and the
# only reader, `checkUsageFinishLocals()`, ever looks at row 1 -- the location
# printed after a local-variable message. Over the whole namespace that rbind
# was ~40% of an 18 s scan. Recording the first row and skipping the rest gives
# the same messages, locations included: measured on main, all 468 messages of
# the scan with local-variable checks switched ON are identical with and
# without it. The test below re-proves that on a probe. Should codetools drop
# or rename the internal, the mock aborts rather than letting the scan pass.
.local_first_src_info_only <- function(env = rlang::caller_env()) {
  testthat::local_mocked_bindings(
    incLocalSrcInfo = .record_first_src_info,
    .package = "codetools",
    .env = env
  )
}

# What `codetools:::incLocalSrcInfo()` builds for the first use. The object is
# `identical()` to its `as.data.frame(stringsAsFactors = FALSE)`, built without
# that call, which is itself about a fifth of the remaining scan.
.record_first_src_info <- function(vn, w) {
  usage_entry <- utils::getFromNamespace("getLocalUsageEntry", "codetools")
  entry <- usage_entry(vn, w)
  if (!is.null(get("srcinfo", entry, inherits = FALSE))) {
    return(invisible())
  }
  first <- structure(
    list(
      srcfile = if (is.null(w$srcfile)) NA_character_ else w$srcfile,
      frow = if (is.null(w$frow)) NA_integer_ else w$frow,
      lrow = if (is.null(w$lrow)) NA_integer_ else w$lrow
    ),
    class = "data.frame",
    row.names = c(NA_integer_, -1L)
  )
  assign("srcinfo", first, entry)
}

# Local-variable messages switched on, so the stored location is printed.
.located_local_messages <- function(probe) {
  found <- character()
  codetools::checkUsage(
    probe,
    name = "probe",
    report = function(msg) found <<- c(found, msg),
    suppressLocalUnused = FALSE
  )
  found
}

# Sources ----------------------------------------------------------------

# `R/` is absent from a built tarball's installed package, which is where the
# tests run under `R CMD check`. The `offline-tests` job runs `devtools::test()`
# on the checkout, so these two run there on every push and pull request.
.skip_without_utils_source <- function() {
  testthat::skip_if_not_installed("here")
  root <- tryCatch(here::here(), error = function(cnd) NA_character_)
  path <- if (is.na(root)) NA_character_ else file.path(root, "R", "utils.R")
  if (is.na(path) || !file.exists(path)) {
    testthat::skip("R/ is not in the built package")
  }
  path
}

.parsed_globals_call <- function(path) {
  exprs <- as.list(parse(path))
  testthat::expect_length(exprs, 1)
  exprs[[1]]
}

# Tests ------------------------------------------------------------------

testthat::test_that("every NSE symbol whep uses is declared as a global", {
  testthat::skip_if_not_installed("codetools")

  # The failure message lists the offenders with their file and line, so it
  # says what to append to R/utils.R and where the symbol is read.
  testthat::expect_equal(.package_usage_messages(), character())
})

testthat::test_that("the scan reports a symbol that is not declared", {
  # Guards the guard: were the settings above to stop reporting undeclared
  # globals, the check would pass vacuously on a real gap.
  testthat::skip_if_not_installed("codetools")
  probe <- function(x) x[zzz_undeclared_probe_symbol > 0, ]

  testthat::expect_match(
    .probe_usage_messages(probe),
    "no visible binding for global variable",
    all = FALSE
  )
})

testthat::test_that("the first-use shortcut reports what codetools reports", {
  # Guards the speed-up: the mocked source-info recorder must leave every
  # message, and the location printed with it, exactly as codetools has them.
  # `y` is assigned three times and never read, so codetools keeps three
  # source rows and reports the first; the probe carries real srcrefs.
  testthat::skip_if_not_installed("codetools")
  lines <- c(
    "function(x) {",
    "  y <- 1",
    "  y <- x",
    "  y <- 2",
    "  x",
    "}"
  )
  probe <- eval(parse(
    text = lines,
    keep.source = TRUE,
    srcfile = srcfilecopy("probe.R", lines)
  ))

  plain <- .located_local_messages(probe)
  shortcut <- local({
    .local_first_src_info_only()
    .located_local_messages(probe)
  })

  testthat::expect_match(plain, "probe.R:2", fixed = TRUE)
  testthat::expect_identical(shortcut, plain)
})

testthat::test_that("R/utils.R holds nothing but the globalVariables() call", {
  # Precondition for `R/utils.R merge=union`: union merge keeps both sides of
  # every conflicting hunk, which is right for an allowlist of strings and
  # wrong for code. A helper added here would be merged by taking both
  # definitions.
  globals_call <- .parsed_globals_call(.skip_without_utils_source())

  testthat::expect_true(
    rlang::is_call(globals_call, "globalVariables", ns = "utils")
  )
})

testthat::test_that("the declaration list ends in the NULL append sentinel", {
  # Precondition for `R/utils.R merge=union`: with the sentinel every entry
  # line is comma-terminated, so two branches appending a block each merge into
  # valid R. Without it they merge into `"last of A"` `"first of B"`, which is
  # a syntax error union merge introduces without saying so.
  globals_call <- .parsed_globals_call(.skip_without_utils_source())
  entries <- as.list(rlang::call_args(globals_call)[[1]])

  testthat::expect_null(entries[[length(entries)]])
})
