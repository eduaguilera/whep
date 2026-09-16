# Guards the packaged coefficient tables that are transcribed from a workbook
# living in ANOTHER repository, so an upstream edit cannot sit unnoticed.
#
# WHY THIS EXISTS
#
# `whep::biomass_coefs` is a transcription of `Biomass_coefs.xlsx` in
# `afsetools`. Upstream edited five below-ground columns two days after WHEP's
# copy was built, and 92 changed `BG_Biomass_kgDM_ha` cells sat there for four
# months with nothing anywhere reporting it. See #524.
#
# `test_data_raw_freshness.R` (#384) cannot catch this and says so: it proves
# each `data/*.rda` matches what its builder emits from the inputs CURRENTLY IN
# THE REPO. Here the CSV and the `.rda` agree with each other perfectly and are
# both stale against a source outside the repository. A gate that only diffs
# `.rda`-vs-`data-raw` is blind to it by construction.
#
# WHAT THIS IS, AND IS NOT
#
# It is not a claim that upstream is right and WHEP is wrong, or the reverse.
# Adopting an upstream edit is a scientific decision, and this script makes no
# decision -- it makes the drift VISIBLE, which is the thing that was missing.
# A mismatch is a prompt to look, not a fault.
#
# It lives in `validation/` rather than `tests/` because it reads a file in
# another repository. The test suite may not do that (#490).
#
# Usage:
#   Rscript validation/upstream_coefs.R            # check against the baseline
#   Rscript validation/upstream_coefs.R --record   # rewrite the baseline
#
# The upstream checkout is found via `WHEP_AFSETOOLS_DIR`, else a sibling
# `../afsetools` next to this repository. Absent either, every source is
# reported as unavailable rather than passing quietly.

if (sys.nframe() == 0L) {
  suppressPackageStartupMessages({
    devtools::load_all(".", quiet = TRUE)
    library(dplyr)
  })
}

BASELINE_PATH <- "validation/gt_upstream_coefs.json"

# Relative tolerance on a numeric cell before it counts as changed.
#
# Loose enough to absorb the xlsx -> R and CSV -> R double round trips, which
# disagree in their last bits, and tight enough that a real coefficient edit
# cannot hide. #524's own caveat is the reason this is not a string comparison:
# `as.character()` reports ~190 phantom differences here, drowning the real
# ones. `"0.56000000000000005"` and `"0.56"` are the same number.
CELL_TOL <- 1e-6

# One entry per packaged table transcribed from an external workbook.
#
# `rows` is asserted rather than discovered: these are row-aligned
# transcriptions, so a changed row count means the comparison below is
# meaningless and must not be reported as a column diff.
SOURCES <- list(
  list(
    name = "biomass_coefs",
    workbook = "inst/extdata/Biomass_coefs.xlsx",
    sheet = "Coefs",
    skip = 1,
    packaged = function() whep::biomass_coefs,
    rows = 421L
  )
)

main <- function() {
  record <- "--record" %in% commandArgs(trailingOnly = TRUE)
  observed <- lapply(SOURCES, measure_source)
  names(observed) <- vapply(SOURCES, function(s) s$name, character(1))

  if (record) {
    write_baseline(observed)
    return(invisible(NULL))
  }
  report(observed, read_baseline())
}

# ---- Locating the upstream checkout ----------------------------------------

# Never a hardcoded absolute path: the env var first, then a sibling checkout,
# then nothing. Returns NA when the workbook cannot be found, which the caller
# reports rather than treating as agreement.
upstream_path <- function(relative) {
  roots <- c(
    Sys.getenv("WHEP_AFSETOOLS_DIR", unset = NA_character_),
    file.path("..", "afsetools")
  )
  roots <- roots[!is.na(roots) & nzchar(roots)]
  for (root in roots) {
    candidate <- file.path(root, relative)
    if (file.exists(candidate)) {
      return(normalizePath(candidate, winslash = "/"))
    }
  }
  NA_character_
}

# ---- Measuring -------------------------------------------------------------

measure_source <- function(spec) {
  path <- upstream_path(spec$workbook)
  if (is.na(path)) {
    return(list(
      name = spec$name,
      unavailable = sprintf("no %s found", spec$workbook),
      md5 = NA_character_
    ))
  }
  if (!requireNamespace("readxl", quietly = TRUE)) {
    return(list(name = spec$name, unavailable = "readxl not installed"))
  }

  upstream <- readxl::read_excel(path, sheet = spec$sheet, skip = spec$skip)
  packaged <- spec$packaged()

  list(
    name = spec$name,
    unavailable = NULL,
    path = path,
    md5 = unname(tools::md5sum(path)),
    upstream_rows = nrow(upstream),
    packaged_rows = nrow(packaged),
    expected_rows = spec$rows,
    only_upstream = setdiff(names(upstream), names(packaged)),
    only_packaged = setdiff(names(packaged), names(upstream)),
    changed = changed_columns(upstream, packaged)
  )
}

# Per-column count of cells that differ, over the columns the two share.
#
# Numeric columns compare numerically with a tolerance; anything else compares
# as text. A column that is character on one side and numeric on the other is
# coerced to numeric first when both coerce cleanly, which is what keeps a
# character `Equiv` holding "0.56000000000000005" from reading as a difference
# against 0.56.
changed_columns <- function(upstream, packaged) {
  shared <- intersect(names(upstream), names(packaged))
  n <- min(nrow(upstream), nrow(packaged))
  counts <- vapply(
    shared,
    function(column) {
      cell_differences(
        upstream[[column]][seq_len(n)],
        packaged[[column]][
          seq_len(n)
        ]
      )
    },
    integer(1)
  )
  counts[counts > 0L]
}

cell_differences <- function(a, b) {
  a_chr <- as.character(a)
  b_chr <- as.character(b)
  a_num <- suppressWarnings(as.numeric(a_chr))
  b_num <- suppressWarnings(as.numeric(b_chr))

  # Per CELL, not per column. `Equiv` holds a crop name in most rows and a bare
  # number in two of them, so a column-level "is this numeric" test falls
  # through to text for the whole column and then reports
  # "0.56000000000000005" against "0.56" as a difference. Those are exactly
  # the phantom differences #524's own caveat warns about, and left alone they
  # would sit in the baseline forever pretending to be real drift.
  numeric_pair <- !is.na(a_num) & !is.na(b_num)
  same_number <- numeric_pair &
    abs(a_num - b_num) <= CELL_TOL * pmax(1, abs(a_num), abs(b_num))
  same_text <- !numeric_pair & !is.na(a_chr) & !is.na(b_chr) & a_chr == b_chr

  sum(!((is.na(a_chr) & is.na(b_chr)) | same_number | same_text))
}

# ---- Baseline --------------------------------------------------------------

read_baseline <- function() {
  if (!file.exists(BASELINE_PATH)) {
    cli::cli_abort(c(
      "No baseline at {.path {BASELINE_PATH}}.",
      i = "Record one with
           {.code Rscript validation/upstream_coefs.R --record}."
    ))
  }
  jsonlite::fromJSON(BASELINE_PATH, simplifyVector = FALSE)
}

write_baseline <- function(observed) {
  payload <- list(
    note = paste(
      "Recorded state of the packaged coefficient tables against the external",
      "workbooks they are transcribed from. `changed` is the accepted",
      "difference at the time of recording, not a target of zero: WHEP may",
      "deliberately differ from upstream. Re-record only when the difference",
      "has been looked at, and say in the commit what moved and why."
    ),
    sources = lapply(observed, function(o) {
      list(
        md5 = o$md5,
        upstream_rows = o$upstream_rows,
        packaged_rows = o$packaged_rows,
        only_upstream = as.list(o$only_upstream),
        only_packaged = as.list(o$only_packaged),
        changed = as.list(o$changed)
      )
    })
  )
  jsonlite::write_json(payload, BASELINE_PATH, auto_unbox = TRUE, pretty = TRUE)
  cli::cli_alert_success(
    "Recorded {length(observed)} source{?s} to {.path {BASELINE_PATH}}."
  )
}

# ---- Reporting -------------------------------------------------------------

report <- function(observed, baseline) {
  cli::cli_h1("Packaged coefficients vs their upstream workbooks")
  rows <- lapply(
    observed,
    function(o) check_one(o, baseline$sources[[o$name]])
  )
  table <- dplyr::bind_rows(rows)
  print(as.data.frame(table), row.names = FALSE)
  emit_metric(table)

  failed <- dplyr::filter(table, .data$verdict != "ok")
  cat("\n")
  if (nrow(failed) == 0L) {
    cli::cli_alert_success(
      "All {nrow(table)} source{?s} {?matches/match} the baseline."
    )
    return(invisible(NULL))
  }
  for (i in seq_len(nrow(failed))) {
    cli::cli_alert_warning("{failed$source[[i]]}: {failed$detail[[i]]}")
  }
  cli::cli_alert_info(
    "If the change is understood, re-record with
     {.code Rscript validation/upstream_coefs.R --record}."
  )
  invisible(NULL)
}

# Same METRIC shape `stability.R`, `nourishment_axis.R` and `lpjml_pins.R`
# emit, so `validate_all.R` folds this in without parsing the table above.
emit_metric <- function(table) {
  cat(sprintf(
    "METRIC sources_checked=%d sources_ok=%d drifted=%d unavailable=%d\n",
    nrow(table),
    sum(table$verdict == "ok"),
    sum(table$verdict == "DRIFTED"),
    sum(table$verdict == "UNAVAILABLE")
  ))
}

check_one <- function(observed, expected) {
  base <- tibble::tibble(source = observed$name)

  if (!is.null(observed$unavailable)) {
    return(dplyr::mutate(
      base,
      verdict = "UNAVAILABLE",
      detail = observed$unavailable
    ))
  }
  if (is.null(expected)) {
    return(dplyr::mutate(
      base,
      verdict = "NEW",
      detail = "not in the baseline; record it"
    ))
  }

  problems <- c(
    row_problem(observed),
    schema_problem(observed, expected),
    md5_problem(observed, expected),
    column_problem(observed, expected)
  )
  dplyr::mutate(
    base,
    verdict = if (length(problems) == 0L) "ok" else "DRIFTED",
    detail = if (length(problems) == 0L) {
      sprintf(
        "%d column%s differ from upstream, as recorded",
        length(observed$changed),
        if (length(observed$changed) == 1L) "" else "s"
      )
    } else {
      paste(problems, collapse = "; ")
    }
  )
}

# A changed row count invalidates the row-aligned comparison entirely, so it is
# reported on its own rather than as a pile of column diffs.
row_problem <- function(observed) {
  if (
    identical(as.integer(observed$upstream_rows), observed$expected_rows) &&
      identical(as.integer(observed$packaged_rows), observed$expected_rows)
  ) {
    return(character())
  }
  sprintf(
    "ROW COUNT: upstream %d, packaged %d, expected %d -- the row-aligned
     comparison below is meaningless until this is resolved",
    observed$upstream_rows,
    observed$packaged_rows,
    observed$expected_rows
  )
}

schema_problem <- function(observed, expected) {
  gained <- setdiff(observed$only_upstream, unlist(expected$only_upstream))
  lost <- setdiff(observed$only_packaged, unlist(expected$only_packaged))
  out <- character()
  if (length(gained) > 0L) {
    out <- c(
      out,
      sprintf("upstream-only columns: %s", paste(gained, collapse = ", "))
    )
  }
  if (length(lost) > 0L) {
    out <- c(
      out,
      sprintf("packaged-only columns: %s", paste(lost, collapse = ", "))
    )
  }
  out
}

# The workbook's own checksum. It moves on any edit, including one that
# changes no shared column, so it is the earliest signal available.
md5_problem <- function(observed, expected) {
  if (identical(observed$md5, expected$md5)) {
    return(character())
  }
  sprintf("workbook md5 %s vs recorded %s", observed$md5, expected$md5)
}

column_problem <- function(observed, expected) {
  recorded <- unlist(expected$changed)
  seen <- observed$changed
  moved <- names(seen)[
    vapply(
      names(seen),
      function(nm) {
        !identical(as.integer(recorded[[nm]]), as.integer(seen[[nm]]))
      },
      logical(1)
    )
  ]
  gone <- setdiff(names(recorded), names(seen))
  out <- character()
  if (length(moved) > 0L) {
    out <- c(
      out,
      sprintf(
        "columns now differing: %s",
        paste(sprintf("%s (%d cells)", moved, seen[moved]), collapse = ", ")
      )
    )
  }
  if (length(gone) > 0L) {
    out <- c(
      out,
      sprintf(
        "columns that agree again: %s",
        paste(gone, collapse = ", ")
      )
    )
  }
  out
}

if (sys.nframe() == 0L) {
  main()
}
