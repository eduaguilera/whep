# Which `data/*.rda` files actually CHANGED, as opposed to merely being rewritten?
#
# Re-running a `data-raw/` script re-serialises every dataset it touches, and `git diff` reports
# each one as `Bin 1030 -> 1048 bytes` with nothing to say about content. On this branch that
# meant 33 changed files, of which **27 were byte churn and identical in content** -- tables
# about soil carbon, manure and drainage that no polity work should touch. A real change hides
# perfectly in a list like that, and one did: `urban_n_reference$area_code` had gone from the
# ISO3 string "ESP" to the integer 203 (#401), which is intended but is a breaking type change
# and was undocumented until this comparison surfaced it.
#
# So: never read a binary diff as a change set, and never wave one through either. Run this.
#
# Usage, from the package root:
#   Rscript inst/scripts/compare_datasets_to_ref.R                # against origin/main
#   Rscript inst/scripts/compare_datasets_to_ref.R origin/HEAD~5  # against any git ref
#
# Covers `data/*.rda` AND `inst/extdata/*.csv`, because the CSVs have the same failure mode and
# one of them hid a real defect behind it (whep#404, see `load_one()` below).
#
# Compares content only: attributes are ignored, `sf` geometry columns are dropped (a
# re-serialised geometry compares unequal for reasons that are never the point), row ORDER is
# normalised, CSV columns are read as character so 45 and 45.0 compare equal, and a file absent
# from the ref is reported as new rather than as a difference.

suppressMessages({
  library(cli)
})

args <- commandArgs(trailingOnly = TRUE)
ref <- if (length(args) > 0L) args[[1]] else "origin/main"

changed <- system2(
  "git",
  c("diff", "--name-only", ref, "--", "data/", "inst/extdata/"),
  stdout = TRUE
)
changed <- changed[grepl("\\.(rda|csv)$", changed)]
if (length(changed) == 0L) {
  cli::cli_alert_success(
    "No dataset or extdata file differs from {.val {ref}}."
  )
  quit(status = 0)
}

tmp <- tempfile("dsref")
dir.create(tmp)
on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

load_one <- function(path) {
  # CSVs are compared as data too, and for the same reason the `.rda` comparison exists.
  # `inst/extdata/cow_to_lpjml.csv` once showed 191 insertions against 190 deletions -- every
  # line including the header -- which reads as pure quoting churn. It was not: FAOSTAT area 45
  # was labelled "Mayotte" when 45 is Comoros, so every Comoros grid cell carried Mayotte's
  # LPJmL index (whep#404). Two real rows hiding behind a whole-file rewrite.
  #
  # Read with every column as character, so a rewrite that turns 45 into 45.0 or drops a
  # thousands separator is not reported as a content change when the value is the same.
  if (grepl("\\.csv$", path, ignore.case = TRUE)) {
    return(utils::read.csv(
      path,
      colClasses = "character",
      check.names = FALSE,
      stringsAsFactors = FALSE
    ))
  }
  e <- new.env()
  load(path, envir = e)
  get(ls(e)[1], envir = e)
}

flatten <- function(x) {
  if (inherits(x, "sf")) {
    x <- sf::st_drop_geometry(x)
  }
  x <- as.data.frame(x)
  # Row order is not content. Regenerating a table often re-sorts it -- cow_to_lpjml.csv moved
  # its first row from ARM to AFG -- and reporting that as a difference buries the rows that
  # actually changed. Ordered by every column so the comparison is order-independent.
  if (nrow(x) > 1L && ncol(x) > 0L) {
    x <- x[do.call(order, lapply(x, as.character)), , drop = FALSE]
    rownames(x) <- NULL
  }
  x
}

same <- differing <- new <- broken <- character()

for (f in changed) {
  base <- basename(f)
  ref_path <- file.path(tmp, base)
  ok <- suppressWarnings(system2(
    "git",
    c("cat-file", "blob", paste0(ref, ":", f)),
    stdout = ref_path,
    stderr = FALSE
  ))
  if (!identical(ok, 0L) || file.size(ref_path) == 0L) {
    new <- c(new, base)
    next
  }
  a <- try(load_one(f), silent = TRUE)
  b <- try(load_one(ref_path), silent = TRUE)
  if (inherits(a, "try-error") || inherits(b, "try-error")) {
    broken <- c(broken, base)
    next
  }
  eq <- isTRUE(all.equal(
    flatten(b),
    flatten(a),
    check.attributes = FALSE
  ))
  if (eq) same <- c(same, base) else differing <- c(differing, base)
}

cli::cli_h1("{length(changed)} data file{?s} differ{?s/} from {.val {ref}}")

if (length(same) > 0L) {
  cli::cli_alert_info(
    "{length(same)} {?is/are} byte churn only -- content identical:"
  )
  cli::cli_ul(same)
}
if (length(new) > 0L) {
  cli::cli_alert_info("{length(new)} {?is/are} new in this tree:")
  cli::cli_ul(new)
}
if (length(broken) > 0L) {
  cli::cli_alert_warning("{length(broken)} could not be loaded on one side:")
  cli::cli_ul(broken)
}
if (length(differing) > 0L) {
  cli::cli_alert_warning(
    "{length(differing)} {?has/have} genuinely different CONTENT -- each should be
     intended and in NEWS.md:"
  )
  cli::cli_ul(differing)
} else {
  cli::cli_alert_success("No dataset changed content.")
}
