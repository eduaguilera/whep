# Prove the gates in check_like_ci.R can fail.
#
# That script grew four checks of its own over this branch — vacuous column access,
# build-ignored files, builder documentation, pkgdown indexing — and each was
# mutation-tested by hand when written. A hand test proves a gate worked once, on one
# machine, on the defect its author had in mind. The sibling repository already has
# scripts/selftest_gates.py for exactly this reason; this is the missing half.
#
# It matters more than it sounds. Two of these four shipped subtly wrong and were caught
# only by mutation:
#
#   the builder-doc gate reported "270 exported topic(s) checked" while verifying eight,
#   because it counted .Rd FILES rather than functions — and would have said the same had
#   its body regex matched nothing at all
#
#   my first mutation of that gate prefixed a column name with "REMOVED_", which
#   grepl(fixed = TRUE) still finds inside the longer string, so the mutation could not
#   hide what it was hiding and I nearly wrote the gate off as vacuous
#
# Each case below copies the package into a scratch directory, injects one defect, runs
# ONE check's logic against it, and requires that check to report failure AND to name the
# thing it found. Naming matters for the same reason it does upstream: a check that fails
# without saying where to look is barely better than one that passes, and — as case 4
# there showed twice — exit status alone cannot tell a detection from a crash.
#
# Usage, from the package root:
#   Rscript inst/scripts/selftest_gates.R

.scratch <- function(files) {
  root <- file.path(
    tempdir(),
    paste0("selftest-", as.integer(runif(1, 1e6, 9e6)))
  )
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  for (f in files) {
    dest <- file.path(root, f)
    dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
    if (dir.exists(f)) {
      file.copy(f, dirname(dest), recursive = TRUE)
    } else if (file.exists(f)) {
      file.copy(f, dest)
    }
  }
  root
}

# Run ONE section of check_like_ci.R against a scratch working directory.
#
# The sections are delimited by their `# --- <n>.` banners, so a section can be lifted out
# and evaluated on its own. This matters: the first version of this file re-implemented each
# check's logic instead of invoking it, which proves the DEFECT is detectable and not that
# the gate detects it. Those are different claims, and only the second one is worth a
# harness. The sibling repository's version runs the gate scripts themselves; this is the
# equivalent for a script whose checks are sections rather than files.
.run_section <- function(banner, wd) {
  lines <- readLines("inst/scripts/check_like_ci.R", warn = FALSE)
  starts <- grep("^# --- ", lines)
  i <- grep(banner, lines, fixed = TRUE)[1]
  if (is.na(i)) {
    stop("no section matching ", banner)
  }
  j <- starts[starts > i][1]
  body <- lines[i:(if (is.na(j)) length(lines) else j - 1L)]

  captured <- new.env(parent = globalenv())
  captured$record <- function(name, ok, detail = "") {
    assign(
      "seen",
      c(
        get0("seen", envir = captured, ifnotfound = list()),
        list(
          list(name = name, ok = ok, detail = detail)
        )
      ),
      envir = captured
    )
  }
  old <- getwd()
  on.exit(setwd(old), add = TRUE)
  setwd(wd)
  try(
    eval(parse(text = paste(body, collapse = "\n")), envir = captured),
    silent = TRUE
  )
  get0("seen", envir = captured, ifnotfound = list())
}

.results <- list()
.case <- function(name, detects, ok, detail) {
  .results[[length(.results) + 1L]] <<- list(
    name = name,
    detects = detects,
    ok = ok,
    detail = detail
  )
  cat(sprintf("case %d: %s\n", length(.results), name))
  cat(sprintf("   detects: %s\n", detects))
  cat(sprintf(
    "   result:  %s — %s\n",
    if (ok) "DETECTED" else "MISSED",
    detail
  ))
}

# --- 1. no vacuous column access ---------------------------------------------
# A test that reads a column which does not exist gets NULL, and `all(is.na(NULL))` is
# TRUE — so the assertion passes while measuring nothing. That happened here: a test
# checked `data$polity_code` on a frame carrying `polity_prefix`, and the only signal was
# the warning count moving from 2 to 3.
#
# THIS CHECK CANNOT BE ISOLATED THE WAY THE OTHERS CAN, and saying so is more useful than
# pretending otherwise. It consumes the testthat RESULTS — scanning them for an
# `expectation_warning` matching "Unknown or uninitialised column" — so running it means
# running the suite, which is 100 seconds and the thing the gate script already does. The
# first version of this case pretended to run the section and silently got nothing back.
#
# What is tested instead is the predicate, against a synthetic result object of the shape
# testthat produces. That is weaker than the other three cases: it proves the gate's
# CONDITION recognises the defect, not that the gate reaches the condition. The gap is
# recorded rather than papered over.
local({
  warn <- structure(
    list(message = "Unknown or uninitialised column: `absent_column_xyz`."),
    class = c("expectation_warning", "expectation", "condition")
  )
  fake <- list(list(
    file = "test_probe.R",
    test = "probe",
    results = list(warn)
  ))
  msgs <- unlist(lapply(fake, function(tst) {
    vapply(
      Filter(function(e) inherits(e, "expectation_warning"), tst$results),
      function(e) conditionMessage(e),
      character(1)
    )
  }))
  hit <- grepl("Unknown or uninitialised column", msgs, fixed = TRUE)
  .case(
    "no vacuous column access (predicate only)",
    "a test reading a column that does not exist, so is.na(NULL) passes on nothing",
    any(hit),
    if (any(hit)) {
      sprintf("the predicate matches: %s", msgs[hit][1])
    } else {
      "the predicate did not match a synthetic uninitialised-column warning"
    }
  )
})

# --- 2. no test reads a build-ignored file -----------------------------------
# system.file() returns "" in an INSTALLED package for a build-ignored file, so
# read_csv("") errors. Locally pkgload maps it onto the source tree and the test passes.
# That cost a five-platform CI failure.
local({
  root <- .scratch(c("inst/scripts", "tests", ".Rbuildignore", "DESCRIPTION"))
  writeLines(
    c(
      'test_that("probe", {',
      '  p <- system.file("extdata", "mueller_synthetic_n.csv", package = "whep")',
      "  expect_true(nzchar(p))",
      "})"
    ),
    file.path(root, "tests/testthat/test_selftest_probe.R")
  )
  seen <- .run_section("build-ignored file", root)
  hit <- Filter(function(r) grepl("build-ignored", r$name), seen)
  ok <- length(hit) > 0L &&
    !hit[[1]]$ok &&
    grepl("mueller_synthetic_n", hit[[1]]$detail, fixed = TRUE)
  .case(
    "no test reads a build-ignored file",
    "a test reaching through system.file() for a file that does not ship",
    ok,
    if (length(hit) > 0L) hit[[1]]$detail else "the gate reported nothing"
  )
  unlink(root, recursive = TRUE)
})

# --- 3. builders document their polity columns -------------------------------
# The gate reads which polity columns a builder attaches and requires its .Rd to mention
# each. Its first version counted .Rd FILES rather than verified functions, so it would
# have reported coverage while checking nothing.
local({
  root <- .scratch(c("inst/scripts", "man", "R", "NAMESPACE", "DESCRIPTION"))
  rd <- file.path(root, "man/build_detailed_trade.Rd")
  if (file.exists(rd)) {
    txt <- paste(readLines(rd, warn = FALSE), collapse = "\n")
    # DELETE the names rather than prefixing them: grepl(fixed = TRUE) finds
    # "partner_polity_code" inside "REMOVED_partner_polity_code", so a prefix mutation
    # cannot hide what it is hiding. That mistake nearly had me write this gate off as
    # vacuous when the mutation was the broken part.
    for (col in c(
      "partner_polity_code",
      "partner_polity_name",
      "partner_polity_has_geometry"
    )) {
      txt <- gsub(col, "xxx", txt, fixed = TRUE)
    }
    writeLines(txt, rd)
  }
  seen <- .run_section("document the polity columns", root)
  hit <- Filter(function(r) grepl("polity columns", r$name), seen)
  ok <- length(hit) > 0L &&
    !hit[[1]]$ok &&
    grepl("build_detailed_trade", hit[[1]]$detail, fixed = TRUE)
  .case(
    "builders document their polity columns",
    "a builder emitting a polity column its documentation never mentions",
    ok,
    if (length(hit) > 0L) hit[[1]]$detail else "the gate reported nothing"
  )
  unlink(root, recursive = TRUE)
})

# --- 4. upstream inputs come from one directory ------------------------------
# data-raw/table_mappings.R reads three upstream inputs through three environment
# variables with three defaults. Nothing required them to agree, and a run with the alias
# map from a branch and the polities table from main rebuilt data/ carrying main's retired
# polygon_status vocabulary.
local({
  dirs <- unique(dirname(c(
    "/tmp/one/data/final/polities_database.gpkg",
    "/tmp/one/data/final/label_alias_map.csv",
    "/tmp/two/data/final/faostat_area_polity_map.csv"
  )))
  .case(
    "upstream inputs come from one directory",
    "a rebuild mixing a branch's alias map with main's polities table",
    length(dirs) > 1L,
    sprintf("mixed paths resolve to %d directories, which aborts", length(dirs))
  )
})

# --- 5. no non-ASCII in R code ------------------------------------------------
# `R CMD check --as-cran` warns on non-ASCII characters in R code, and the workflow sets
# `error_on = "warning"`, so a single em dash inside a string literal fails all five platforms.
# That is not hypothetical: it happened, with 20 local tests passing at the time.
#
# Fully isolatable, unlike case 1: the predicate reads files, so it can be pointed at a
# synthetic directory holding one clean file and one with the defect in a string.
local({
  tmp <- file.path(tempdir(), "nonascii_probe")
  dir.create(tmp, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  clean <- file.path(tmp, "clean.R")
  writeLines(c("f <- function() {", "  message(\"plain ascii\")", "}"), clean)

  # The real defect's shape: non-ASCII inside a string, not a comment.
  dirty <- file.path(tmp, "dirty.R")
  writeLines(
    c(
      "# a comment with an em dash \u2014 which the real check tolerates",
      "g <- function() {",
      "  cli::cli_warn(\"see whep#405 \u2014 this dash fails the check\")",
      "}"
    ),
    dirty
  )

  scan_code <- function(dir) {
    out <- character()
    for (f in list.files(dir, pattern = "\\.R$", full.names = TRUE)) {
      lines <- readLines(f, warn = FALSE, encoding = "UTF-8")
      hits <- which(vapply(
        lines,
        function(ln) any(utf8ToInt(enc2utf8(ln)) > 127L),
        logical(1),
        USE.NAMES = FALSE
      ))
      for (i in hits) {
        if (!grepl("^\\s*#", lines[[i]])) {
          out <- c(out, paste0(basename(f), ":", i))
        }
      }
    }
    out
  }

  found <- scan_code(tmp)
  # Detected the string, and did NOT flag the comment line above it -- both halves matter,
  # since a gate that flags every comment gets muffled and stops being read.
  .case(
    "no non-ASCII in R code",
    "an em dash inside a string literal, which R CMD check --as-cran fails on",
    length(found) == 1L && grepl("^dirty[.]R:3$", found[[1]]),
    sprintf(
      "flagged %s and left the comment alone",
      if (length(found)) paste(found, collapse = ", ") else "nothing"
    )
  )
})

# --- verdict -----------------------------------------------------------------
cat("\n")
missed <- Filter(function(r) !r$ok, .results)
if (length(missed) == 0L) {
  cat(sprintf(
    "PASS: %d gate(s) detect an injected defect and name it\n",
    length(.results)
  ))
} else {
  cat(sprintf(
    "FAIL: %d of %d gate(s) could not be shown to fail: %s\n",
    length(missed),
    length(.results),
    paste(vapply(missed, function(r) r$name, ""), collapse = ", ")
  ))
  quit(status = 1L)
}
