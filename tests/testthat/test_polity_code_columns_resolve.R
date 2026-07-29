# Any column named `*_polity_code` promises a periodized WHEP code. This sweeps
# EVERY exported dataset for that column class and holds it to the promise, rather
# than naming the datasets to check — the point is to catch the next table to
# acquire such a column, not the ones already known.
#
# What it was written for: polity_area_crosswalk shipped a `reporting_polity_code`
# column holding 609 bare family prefixes and zero periodized codes, 206 of whose
# distinct values named no polity at all. It is the table the documentation points
# consumers at for joining, so the obvious join returned nothing. It survived because
# every OTHER carrier of that column name — regions_full, polities_cats, and the
# production/CBS/trade/supply-use outputs — was correct, and no check looked across
# them all at once.
#
# That column is now `reporting_polity_prefix`, which is what it always held.

testthat::test_that("every *_polity_code column in an exported dataset resolves", {
  # A prefix may itself contain hyphens — AZE-SSR-1920-1991 and IDN-JVM-1949-1951
  # are real upstream codes — so anchor on the trailing year pair, not on a
  # single-token prefix. My first attempt used [A-Za-z0-9]+ and reported five
  # perfectly valid codes as malformed.
  code_re <- "^.+-[0-9]{4}-[0-9]{4}$"
  valid <- whep::polities$polity_code

  exported <- utils::data(package = "whep")$results[, "Item"]
  checked <- 0L
  problems <- character()

  for (nm in exported) {
    d <- tryCatch(
      get(nm, envir = asNamespace("whep")),
      error = function(e) NULL
    )
    if (is.null(d) || !is.data.frame(d)) {
      next
    }
    cols <- grep("polity_code$", names(d), value = TRUE)
    # `polity_area_code` is a numeric FABIO/FAOSTAT area code, not a polity code,
    # despite ending in "_code" — excluded by name, not by index.
    cols <- setdiff(cols, "polity_area_code")
    for (col in cols) {
      v <- stats::na.omit(d[[col]])
      if (length(v) == 0L) {
        next
      }
      checked <- checked + 1L
      bare <- unique(v[!grepl(code_re, v)])
      if (length(bare) > 0L) {
        problems <- c(
          problems,
          paste0(
            nm,
            "$",
            col,
            " holds ",
            length(bare),
            " non-periodized value(s), e.g. ",
            paste(utils::head(sort(bare), 4), collapse = ", ")
          )
        )
        next
      }
      unknown <- setdiff(unique(v), valid)
      if (length(unknown) > 0L) {
        problems <- c(
          problems,
          paste0(
            nm,
            "$",
            col,
            " has ",
            length(unknown),
            " code(s) absent from whep::polities, e.g. ",
            paste(utils::head(sort(unknown), 4), collapse = ", ")
          )
        )
      }
    }
  }

  # Guard the sweep itself: if a refactor stops exposing these columns the test
  # must not pass by checking nothing.
  testthat::expect_gt(checked, 2L)
  testthat::expect_equal(
    length(problems),
    0L,
    info = paste0(
      "columns named *_polity_code that do not hold resolvable periodized codes ",
      "(rename them *_polity_prefix if they hold family keys): ",
      paste(problems, collapse = " | ")
    )
  )
})

testthat::test_that("the crosswalk's family key is named as a prefix", {
  # Bidirectional: the rename must stay, AND the column must keep holding prefixes
  # rather than quietly becoming periodized under a prefix name.
  cw <- whep::polity_area_crosswalk
  testthat::expect_false("reporting_polity_code" %in% names(cw))
  testthat::expect_true("reporting_polity_prefix" %in% names(cw))
  v <- stats::na.omit(cw$reporting_polity_prefix)
  testthat::expect_gt(length(v), 0L)
  testthat::expect_false(any(grepl("^.+-[0-9]{4}-[0-9]{4}$", v)))
})
