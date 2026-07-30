# The `polities` roxygen listed a `polygon_status` vocabulary that no longer existed and described
# `end_year` as inclusive when it is exclusive. Both are the kind of error that produces silence
# rather than a complaint: a filter on `polygon_status == "missing"` returns zero rows, and one on
# `year <= end_year` double-counts every boundary year.
#
# Documentation cannot be tested directly, but the FACTS it asserts can be. If the vocabulary grows
# a value or the interval convention changes, these fail and the roxygen gets revisited — which is
# what did not happen when the vocabulary was migrated on this branch.

testthat::test_that("the documented polygon_status vocabulary is the actual one", {
  documented <- c(
    "assigned",
    "proxy",
    "estimate",
    "polygon_vintage_drift",
    "unassigned"
  )
  actual <- sort(unique(stats::na.omit(
    as.data.frame(whep::polities)$polygon_status
  )))
  testthat::expect_setequal(actual, documented)
})

testthat::test_that("end_year is exclusive, as the documentation now says", {
  # The behaviour the roxygen described backwards. Adjacent periods settle it.
  r <- as.data.frame(add_polity_code(
    data.frame(area_code = 185L, year = c(2013L, 2014L))
  ))
  testthat::expect_equal(r$polity_code, c("RUS-1991-2014", "RUS-2014-2025"))
})

testthat::test_that("polygon_area_km2 is sparse, as documented", {
  # Asserted so the "compute it from geom instead" advice cannot silently become wrong advice: if
  # the field ever becomes dense, the documentation should stop warning readers off it.
  p <- as.data.frame(whep::polities)
  recorded <- sum(!is.na(p$polygon_area_km2))
  testthat::expect_lt(recorded / nrow(p), 0.5)
  # And it must not be empty either, or the cross-check it exists for has nothing to work with.
  testthat::expect_gt(recorded, 50L)
})

testthat::test_that("the alias map's exhaustive column list really is exhaustive", {
  # polity_label_aliases documents itself as "A tibble with one row per alias:" followed by every
  # column — an exhaustive listing, unlike polities and polity_area_crosswalk, which say "Key
  # columns" and are selective by design. That distinction decides whether an omission is a defect,
  # and here it is: `observed_rows` was added to the published contract on this branch, consumed by
  # data-raw/table_mappings.R to decide which areas may be folded, and left out of the list.
  #
  # Asserted against the .Rd rather than the source, so it reflects what a reader actually gets.
  # Read the INSTALLED documentation, not man/*.Rd. R CMD check runs tests from an installed package
  # in a temporary library where man/ does not exist, so the source-tree path skipped on CI with
  # "polity_label_aliases.Rd not available" — confirmed from CI's own skip list, not assumed. The
  # rendered help is reachable in both contexts; the source file is used too when present.
  rendered <- tryCatch(
    paste(
      utils::capture.output(print(utils::help(
        "polity_label_aliases",
        package = "whep"
      ))),
      collapse = " "
    ),
    error = function(e) ""
  )
  # testthat sets the working directory to tests/testthat, so a bare "man/..." does not resolve —
  # which made every column look undocumented on the first attempt. Try the candidates instead of
  # assuming one.
  candidates <- c(
    file.path("man", "polity_label_aliases.Rd"),
    file.path("..", "..", "man", "polity_label_aliases.Rd"),
    system.file("..", "man", "polity_label_aliases.Rd", package = "whep")
  )
  source_rd <- candidates[file.exists(candidates)]
  from_source <- if (length(source_rd)) {
    paste(readLines(source_rd[1], warn = FALSE), collapse = " ")
  } else {
    ""
  }
  text <- paste(rendered, from_source)
  testthat::skip_if(
    !nzchar(trimws(text)),
    "no rendered or source documentation reachable for polity_label_aliases"
  )
  columns <- names(as.data.frame(whep::polity_label_aliases))
  testthat::expect_gte(length(columns), 8L)

  undocumented <- columns[
    !vapply(
      columns,
      function(col) grepl(col, text, fixed = TRUE),
      logical(1)
    )
  ]
  testthat::expect_equal(
    length(undocumented),
    0L,
    info = paste0(
      "polity_label_aliases documents its columns exhaustively, so these are missing from ",
      "@format: ",
      paste(undocumented, collapse = ", ")
    )
  )
})

# Both polity tables now claim their column list is EXHAUSTIVE. That claim rots the
# moment a column is added, and it rots silently: nothing about an undocumented column
# fails, which is how `polities` came to describe ten of its twenty columns under the
# heading "Key columns include" — among them `wiki_status`, the one a consumer must
# filter on to avoid routing data to a retired polity.
#
# Found by comparing each dataset's column names against its .Rd text, the same check
# that found six undocumented fields in the upstream manifest. Applied here it also
# confirms polity_label_aliases, regions_full and polities_cats were already complete, so
# the gap was specific rather than general.
#
# The .Rd files are absent from an installed package, so this skips there rather than
# passing vacuously.
testthat::test_that("every column of the polity tables is documented", {
  for (ds in c("polities", "polity_area_crosswalk", "polity_label_aliases")) {
    rd <- system.file("..", "man", paste0(ds, ".Rd"), package = "whep")
    if (!file.exists(rd)) {
      rd <- file.path("man", paste0(ds, ".Rd"))
    }
    testthat::skip_if_not(
      file.exists(rd),
      paste0("man/", ds, ".Rd not available in an installed package")
    )
    doc <- paste(readLines(rd, warn = FALSE), collapse = " ")
    cols <- names(as.data.frame(get(ds, envir = asNamespace("whep"))))
    # Non-vacuous: an empty column list would make the setdiff below trivially empty.
    testthat::expect_gt(length(cols), 5L)

    undocumented <- cols[
      !vapply(
        cols,
        function(col) grepl(col, doc, fixed = TRUE),
        logical(1)
      )
    ]
    testthat::expect_equal(
      length(undocumented),
      0L,
      info = paste0(
        ds,
        " has columns absent from its documentation, which claims to be ",
        "exhaustive: ",
        paste(undocumented, collapse = ", ")
      )
    )
  }
})

# resolve_polity_label()'s @param text quotes three counts, and they are the numbers a
# caller uses to decide whether to pass `source` and `year` at all: how many aliases are
# unscoped by source, how many are unscoped by year, and the total. All three were written
# when there were 672 aliases and went stale as the registry grew to 869 — the docs still
# said "166 of 672" and "19 of the 672" while the answers were 167 of 869 and 17 of 869.
#
# Pinned because prose counts rot silently and these particular ones steer behaviour: a
# reader told that 19 of 672 aliases are year-unscoped concludes something different from
# one told 17 of 869. The other structural figures quoted across this branch were
# re-measured at the same time and were all still correct — 740 polities, 713 live, 27
# dead, 182 with a recorded polygon area, 17 areas keeping their own aggregation key,
# 272 and 198 rows in the two reference tables. Only the alias counts moved, because the
# aliases are what this branch changed.
testthat::test_that("the alias counts in resolve_polity_label's docs are current", {
  # Resolve the path the way the test above does. My first version used
  # file.path("man", ...), which testthat resolves against tests/testthat/ — so it
  # skipped with the message "absent in an installed package" while the file sat in the
  # repository root. A skip is invisible in a summary line, and one carrying the wrong
  # reason is worse than none.
  rd <- system.file("..", "man", "resolve_polity_label.Rd", package = "whep")
  if (!file.exists(rd)) {
    rd <- testthat::test_path("..", "..", "man", "resolve_polity_label.Rd")
  }
  testthat::skip_if_not(
    file.exists(rd),
    "man/resolve_polity_label.Rd not reachable; absent from an installed package"
  )
  doc <- paste(readLines(rd, warn = FALSE), collapse = " ")
  al <- as.data.frame(whep::polity_label_aliases)

  total <- nrow(al)
  unscoped_source <- sum(is.na(al$source))
  unscoped_year <- sum(is.na(al$year_start) & is.na(al$year_end))
  # Non-vacuous: an empty table would make every grepl below search for "0 of 0".
  testthat::expect_gt(total, 100L)

  testthat::expect_true(
    grepl(paste0(unscoped_source, " of ", total), doc, fixed = TRUE),
    info = paste0(
      "the source-unscoped count is stale; should read ",
      unscoped_source,
      " of ",
      total
    )
  )
  testthat::expect_true(
    grepl(paste0(unscoped_year, " of the ", total), doc, fixed = TRUE),
    info = paste0(
      "the year-unscoped count is stale; should read ",
      unscoped_year,
      " of the ",
      total
    )
  )
})
