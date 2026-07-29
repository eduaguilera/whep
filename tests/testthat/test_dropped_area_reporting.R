# Three sites drop rows whose area resolves to no polity: build_trade (twice), read_raw_inputs, and the
# grassland extension. Dropping is correct in all of them. Being silent about it was not: the
# read_raw_inputs filter alone removes 1,109,466 rows of real FAOSTAT production — 26% — and said
# nothing, so a genuinely unknown area code would have vanished as quietly as an expected one.
#
# The classification took three attempts, which is why it is asserted rather than trusted:
#
#   v1  one message for everything: "area codes not mapped to a polity, dropping"
#   v2  read mapping_status to split deliberate from unknown. Wrong: every call site passes
#       include_unmapped = FALSE, which strips unmapped rows from the lookup, so mapping_status is NA
#       for BOTH and the message called FAOSTAT 351 "China" a code this project does not know
#   v3  classify from the crosswalk. Still wrong in effect: all 34 codes absent from the crosswalk in
#       real production are FAOSTAT regional groups (>= 5000), so v3 labelled 34 expected codes as
#       input errors on every read
#   v4  three buckets, informational for the two expected ones and a warning only for the third
#
# The third bucket is empty on real production today, which is the point: it fires only when something
# is actually wrong.

testthat::test_that("each drop reason gets its own message, at the right severity", {
  dt <- data.table::data.table(
    area_code = c(2L, 351L, 5100L, 5101L, 4444L),
    polity_code = c("AFG-1919-2025", NA, NA, NA, NA)
  )

  seen <- list(info = character(), warn = character())
  withCallingHandlers(
    whep:::.warn_unmapped_codes(dt, "polity_code", "area_code", "input"),
    message = function(m) {
      seen$info <<- c(seen$info, conditionMessage(m))
      invokeRestart("muffleMessage")
    },
    warning = function(w) {
      seen$warn <<- c(seen$warn, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  # Deliberate and FAOSTAT-group drops are expected, so they inform rather than warn.
  testthat::expect_true(any(grepl("deliberately unmapped", seen$info)))
  testthat::expect_true(any(grepl("351", seen$info)))
  testthat::expect_true(any(grepl("FAOSTAT regional group", seen$info)))

  # Only the unknown code warns, and it names the code.
  testthat::expect_length(seen$warn, 1L)
  testthat::expect_match(seen$warn[[1]], "NOT FOUND")
  testthat::expect_match(seen$warn[[1]], "4444")

  # And an unknown code must never be reported as a FAOSTAT group or vice versa.
  testthat::expect_false(any(grepl("4444", seen$info)))
  testthat::expect_false(any(grepl("5100", seen$warn)))
})

testthat::test_that("nothing is said when nothing is dropped", {
  clean <- data.table::data.table(
    area_code = c(2L, 3L),
    polity_code = c("AFG-1919-2025", "ALB-1913-2025")
  )
  testthat::expect_silent(
    whep:::.warn_unmapped_codes(clean, "polity_code", "area_code", "input")
  )
})

testthat::test_that("the function accepts a tibble as well as a data.table", {
  # The grassland path is a dplyr pipeline; the trade path is data.table. A classifier that only
  # worked on one would have quietly done nothing at the other site.
  tbl <- tibble::tibble(
    area_code = c(2L, 4444L),
    polity_code = c("AFG-1919-2025", NA)
  )
  testthat::expect_warning(
    whep:::.warn_unmapped_codes(
      tbl,
      "polity_code",
      "area_code",
      "grassland occupation"
    ),
    "NOT FOUND"
  )
})

testthat::test_that("the FAOSTAT group threshold matches how areas are actually numbered", {
  # The classifier treats area_code >= 5000 as one of FAOSTAT's own regional groups rather than a
  # territory. That is an assumption about the SOURCE's numbering, embedded in this package, so it needs
  # pinning from both directions or it drifts silently into mislabelling.
  #
  #   nothing this project models may sit at or above the threshold, or a real territory would be
  #     dismissed as an aggregate
  #   the codes real data carries that this project does NOT model should be at or above it, or a
  #     source aggregate would be reported as the caller's mistake
  #
  # Measured when the threshold was chosen: the crosswalk's highest area code is 999, and of the 244
  # distinct codes in real FAOSTAT production, all 34 absent from the crosswalk are >= 5000 and none
  # below it is absent.
  threshold <- 5000L

  modelled <- unique(stats::na.omit(
    as.data.frame(whep::polity_area_crosswalk)$area_code
  ))
  testthat::expect_gt(length(modelled), 200L)
  testthat::expect_false(
    any(modelled >= threshold),
    info = paste0(
      "an area this project models now sits at or above the FAOSTAT group threshold, so the ",
      "classifier would dismiss it as an aggregate: ",
      paste(
        utils::head(sort(modelled[modelled >= threshold]), 6),
        collapse = ", "
      )
    )
  )

  # regions_full is the other numbering source and must agree.
  regions <- unique(stats::na.omit(as.data.frame(whep::regions_full)$code))
  testthat::expect_false(any(regions >= threshold))
})

testthat::test_that("no pinned input carries an area code we cannot account for", {
  # The invariant that makes the warning worth having, across every area-keyed input rather than one.
  # On real data the two informational branches fire and the warning one does not, because every code
  # these sources ship is either a territory this project maps, a deliberate aggregate, or one of
  # FAOSTAT's own regional groups.
  #
  # Verified end to end on a real production slice: 244 distinct codes in, 205 out, with "15 and 351"
  # reported as deliberately unmapped and 34 reported as regional groups — and no warning. Measured
  # across the others too: international-yields 179 codes, fishstat-trade 189,
  # faostat-production-old 221, none unexplained.
  #
  # If any of these sources introduces a reporting area this project has never seen, this is what turns
  # a silent drop into a message naming the code.
  #
  # The area column is spelled three ways across these pins — area_code, "Area Code", AreaCode — so the
  # candidates are tried in turn and a pin whose column is not found is REPORTED rather than quietly
  # passing. I checked one input with an incomplete candidate list and it looked like the pin had no
  # area column at all.
  modelled <- unique(stats::na.omit(
    as.data.frame(whep::polity_area_crosswalk)$area_code
  ))
  group_min <- whep:::faostat_group_code_min
  deliberate <- whep:::faostat_deliberate_area_codes

  inputs <- c(
    "faostat-production",
    "faostat-production-old",
    "international-yields",
    "fishstat-trade"
  )
  checked <- 0L
  problems <- character()
  for (pin in inputs) {
    raw <- tryCatch(
      data.table::as.data.table(whep:::.read_input(pin)),
      error = function(e) NULL
    )
    if (is.null(raw)) {
      next
    }
    col <- intersect(c("area_code", "Area Code", "AreaCode"), names(raw))
    if (length(col) == 0L) {
      problems <- c(problems, paste0(pin, ": no recognisable area column"))
      next
    }
    codes <- suppressWarnings(as.integer(raw[[col[1]]]))
    codes <- sort(unique(codes[!is.na(codes)]))
    if (length(codes) == 0L) {
      next
    }
    checked <- checked + 1L
    unexplained <- setdiff(
      codes,
      c(modelled, deliberate, codes[codes >= group_min])
    )
    if (length(unexplained) > 0L) {
      problems <- c(
        problems,
        paste0(
          pin,
          ": ",
          length(unexplained),
          " unaccounted code(s) — ",
          paste(utils::head(unexplained, 6), collapse = ", ")
        )
      )
    }
  }

  testthat::skip_if(checked == 0L, "no pinned inputs reachable")
  testthat::expect_equal(
    length(problems),
    0L,
    info = paste0(
      "these inputs carry area codes that are neither mapped, nor deliberately unmapped, nor ",
      "regional groups, so their rows are dropped and someone should decide what they are: ",
      paste(problems, collapse = "; ")
    )
  )
})
