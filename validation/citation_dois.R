# Every DOI cited anywhere in the repo is registered.
#
# WHY THIS EXISTS, AND WHY IT IS NOT IN tests/
#
# `R CMD check --as-cran` validates DOIs in `man/*.Rd`. That is how #893/#607
# found `10.1088/1748-9326/aad4d8`, the fabricated GLEAM citation #883 removes.
# But 34 of the repo's 71 DOIs appear ONLY outside `man/` (#900) -- 13 of them in
# `inst/extdata/coefs/bnf_provenance.csv` alone, plus `validation/SOURCES.md`,
# the source registries, and the `inst/scripts/download/*` headers that identify
# which dataset was actually downloaded. Nothing checks those. The GLEAM DOI was
# caught by luck of placement: it happened to be cited in roxygen that became an
# `.Rd`. An identical fabrication in a provenance CSV would be flagged by
# nothing.
#
# It needs the network, so it cannot live in `tests/`: the `offline-tests` job
# runs with `https_proxy` pointed at a dead port on purpose, and `R CMD check`
# should not fail because doi.org is briefly unreachable.
#
# WHAT IS AND IS NOT CHECKED. That a DOI is REGISTERED, nothing more. It cannot
# tell whether a registered DOI names the right paper -- and it is worth knowing
# that nothing can, by inspection: the house citation style is author + year +
# DOI without naming the journal, so a DOI pointing at the wrong paper reads
# exactly like one pointing at the right paper. (Measured: flagging "the
# Crossref journal for this DOI is not mentioned nearby" fires on 49 of 199
# occurrences, i.e. on the convention rather than on defects.) Resolution is the
# cheap half of the problem; it is still worth having.
#
# Usage:
#   Rscript validation/citation_dois.R

suppressPackageStartupMessages({
  library(jsonlite)
})

# Where citations live. `man/` is included even though `--as-cran` covers it, so
# this script's count is the whole repo rather than the remainder.
SEARCH_DIRS <- c("R", "inst", "validation", "data-raw", "man") # nolint: object_name_linter.

# DOIs known not to be registered, with the issue that tracks each.
#
# An entry here is a FILED DEFECT, not an accepted one. The check reports a
# declared entry as KNOWN and fails if one disappears, because a disappearance
# means the citation was fixed and this list should shrink in the same commit.
# nolint start: object_name_linter.
KNOWN_UNREGISTERED <- c(
  "10.1088/1748-9326/aad4d8" = "#893/#607, fabricated GLEAM citation; removed by #883"
)
# nolint end

# THE EXTRACTOR IS THE FIDDLY PART (#900). A DOI may contain `;`, `[`, `]`, `(`
# and `)`: `10.1890/1051-0761(1997)007[1226:ITICBM]2.0.CO;2` is real and is
# cited in this package. A pattern that stops at `;` truncates it, and the
# truncation 404s -- which reports a fabricated DOI that does not exist. So the
# character class below deliberately admits those, and excludes only what cannot
# appear in a DOI or would swallow the surrounding markup: whitespace, quotes,
# backslash, braces, backtick, `>` and `,`.
#
# Trailing punctuation IS stripped afterwards, because prose puts a full stop
# after a citation -- but `;2` at the end of the ICBM DOI must survive, so the
# strip is anchored to characters that cannot end a DOI.
DOI_PATTERN <- "10\\.[0-9]{4,9}/[^[:space:]{}\\\\\"'`>,]+" # nolint: object_name_linter.

main <- function() {
  found <- collect_dois()
  if (nrow(found) == 0L) {
    cli::cli_abort(
      "No DOIs found; the extractor or the search paths are wrong."
    )
  }
  cli::cli_h1("Citation DOI registration")
  cli::cli_alert_info(
    "{nrow(found)} distinct DOI{?s} across {length(unique(unlist(found$files)))} file{?s}."
  )
  found$registered <- vapply(found$doi, doi_is_registered, logical(1))
  report(found)
}

# ---- Collecting -------------------------------------------------------------

# THIS FILE MUST EXCLUDE ITSELF, and the reason is not tidiness. It lives under
# `validation/`, which it scans, and it names DOIs in `KNOWN_UNREGISTERED` --
# so without this it reads its own exception list as citations. Measured: the
# count went 69 -> 70 and a declared-but-uncited entry was reported as KNOWN,
# because declaring a DOI made it appear. Two consequences, the second serious:
# the "declared entry no longer appears" arm becomes unreachable, and once #883
# removes the GLEAM DOI from the package this script would still cite it, so the
# check could never notice its own exception had been fixed. Self-inclusion
# defeats the whole bidirectional design.
#
# Excluded by path with an assertion, not by a pattern: if the file is renamed
# the assertion fails loudly instead of the exclusion silently missing.
SELF <- "validation/citation_dois.R" # nolint: object_name_linter.

collect_dois <- function() {
  if (!file.exists(SELF)) {
    cli::cli_abort(c(
      "{.path {SELF}} not found, so this script cannot exclude itself.",
      i = "If it was renamed, update {.code SELF}: without the exclusion the
           script reads its own {.code KNOWN_UNREGISTERED} list as citations."
    ))
  }
  files <- unlist(lapply(
    SEARCH_DIRS[dir.exists(SEARCH_DIRS)],
    list.files,
    recursive = TRUE,
    full.names = TRUE,
    all.files = FALSE
  ))
  files <- files[
    normalizePath(files, mustWork = FALSE) !=
      normalizePath(SELF, mustWork = FALSE)
  ]
  # Binary blobs cannot carry a citation and reading them warns.
  files <- files[
    !grepl(
      "[.](rda|rds|gpkg|parquet|nc|xlsx|png|zip|so|o)$",
      files,
      ignore.case = TRUE
    )
  ]
  hits <- list()
  for (f in files) {
    txt <- tryCatch(
      readLines(f, warn = FALSE, encoding = "UTF-8"),
      error = function(e) character()
    )
    if (length(txt) == 0L) {
      next
    }
    m <- regmatches(txt, gregexpr(DOI_PATTERN, txt))
    for (d in unique(unlist(m))) {
      hits[[strip_trailing(d)]] <- unique(c(hits[[strip_trailing(d)]], f))
    }
  }
  if (length(hits) == 0L) {
    return(data.frame(doi = character(), stringsAsFactors = FALSE))
  }
  out <- data.frame(doi = names(hits), stringsAsFactors = FALSE)
  out$files <- unname(hits[out$doi])
  out[order(out$doi), , drop = FALSE]
}

# Trailing prose punctuation, stripped one character at a time rather than by a
# bracket class. In R's default (TRE) engine a backslash inside `[...]` is
# LITERAL, so `"[.,;)\\]]+$"` does not mean what it looks like and silently
# fails to strip `)` -- which left `10.5281/zenodo.14946695)` in the sweep and
# reported it as unregistered. The loop has no escaping to get wrong.
#
# `;` is in the set and that is safe: the ICBM DOI ends `...2.0.CO;2`, so its
# last character is `2` and the loop stops at once. Verified as a control below.
strip_trailing <- function(x) {
  drop <- c(".", ",", ";", ")", "]")
  vapply(
    x,
    function(s) {
      while (nchar(s) > 0L && substring(s, nchar(s)) %in% drop) {
        s <- substring(s, 1L, nchar(s) - 1L)
      }
      s
    },
    character(1),
    USE.NAMES = FALSE
  )
}

# ---- Checking ---------------------------------------------------------------

# Registration via the DOI HANDLE API, deliberately not by fetching the DOI and
# not via Crossref. Both alternatives were measured on this repo and both are
# wrong (#900):
#
#   * `https://doi.org/<doi>` returns 403 for 20 of 69 -- Wiley, ACS, AGU, PNAS,
#     Royal Society, Science, ESA, SSSA and Zenodo all bot-block. A 403 means the
#     DOI resolved and the publisher refused the caller.
#   * `api.crossref.org` 404s on 13 of 69, because Crossref is one registrar and
#     not the registry: Zenodo, figshare, DANS and PIK DOIs are DataCite.
#
# The handle API answers from the registry itself: responseCode 1 = registered,
# 100 = no such handle.
doi_is_registered <- function(doi) {
  url <- paste0(
    "https://doi.org/api/handles/",
    utils::URLencode(doi, reserved = FALSE)
  )
  # `suppressWarnings`, not laziness: an unregistered DOI is a 404, and
  # `fromJSON` warns about it before erroring. The 404 IS the answer here, so
  # the warning is noise that would bury the finding it accompanies.
  res <- suppressWarnings(tryCatch(
    jsonlite::fromJSON(url),
    error = function(e) NULL
  ))
  isTRUE(res$responseCode == 1L)
}

# ---- Reporting -------------------------------------------------------------

report <- function(found) {
  unreg <- found[!found$registered, , drop = FALSE]
  declared <- names(KNOWN_UNREGISTERED)

  new_bad <- setdiff(unreg$doi, declared)
  fixed <- setdiff(declared, unreg$doi)

  for (d in intersect(declared, unreg$doi)) {
    cli::cli_alert_info(
      "KNOWN unregistered: {.val {d}} -- {KNOWN_UNREGISTERED[[d]]}"
    )
  }

  if (length(new_bad) == 0L && length(fixed) == 0L) {
    cli::cli_alert_success(
      "All {nrow(found)} DOIs are registered, bar {length(declared)} declared."
    )
    return(invisible(NULL))
  }
  for (d in new_bad) {
    cli::cli_alert_danger(
      "NOT REGISTERED and not declared: {.val {d}}"
    )
    cli::cli_alert_info(
      "  cited in: {.file {found$files[found$doi == d][[1]]}}"
    )
  }
  for (d in fixed) {
    cli::cli_alert_warning(
      "{.val {d}} is declared unregistered but no longer appears, or now resolves.
       Remove it from {.code KNOWN_UNREGISTERED} in this file."
    )
  }
  quit(status = 1L)
}

if (sys.nframe() == 0L) {
  main()
}
