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
