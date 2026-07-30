# inst/extdata/cow_to_lpjml.csv maps FAOSTAT area_code to the country index LPJmL
# expects (managepar.h), and it carries its OWN iso3c and area_name per row. Those
# are a second copy of area identity, written next to the polities database rather
# than derived from it, and nothing compared them.
#
# Comparing them found a defect. One row read:
#
#   45,"MYT","Mayotte",153
#
# FAOSTAT area 45 is COMOROS -- upstream publishes it as COM-1946-1975 /
# COM-1975-2025. Mayotte is area 270, which was absent from the file entirely, and
# no row carried iso3c COM. So the LPJmL country raster labelled every Comoros grid
# cell with Mayotte's index, Mayotte's own cells fell to 0 ("no country"), and the
# error was invisible because the join in inst/scripts/prepare_spatialize_all.R is
# on area_code alone -- the iso3c and area_name are never checked against anything.
#
# WHICH FIELD WAS WRONG was settled by the file's own structure rather than by
# assumption. The rows are ordered alphabetically by iso3c and lpjml_code tracks
# that order: MRT 147, MUS 150, MWI 151, MYS 152, MYT 153, NAM 154, NCL 155. Index
# 153 sits exactly where MYT belongs, so the index is right and the area_code was
# wrong. Corrected 45 -> 270.
#
# Comoros is deliberately left UNMAPPED rather than given an index here. The
# alphabetical gap at 46 -- COL is 45, CPV is 47, nothing claims 46 -- is where COM
# belongs, so LPJmL almost certainly has a Comoros slot. But "almost certainly"
# inferred from an ordering is not the same as read from managepar.h, and an unmapped
# area already warns and falls to 0, which is an honest absence. Silently
# misattributing it to Mayotte was the actual harm. Proposed as whep#404 with this
# evidence for a maintainer with the LPJmL sources to confirm.
#
# The four remaining name differences are cosmetic -- the file uses short forms
# where upstream uses official ones -- and are listed by name rather than tolerated
# by a fuzzy comparison, so a fifth difference has to be looked at instead of
# blending in.
testthat::test_that("cow_to_lpjml area identity agrees with the polities crosswalk", {
  path <- system.file("extdata", "cow_to_lpjml.csv", package = "whep")
  testthat::skip_if(path == "", "cow_to_lpjml.csv not installed")
  f <- utils::read.csv(path, stringsAsFactors = FALSE)
  # Non-vacuous: a renamed column would make every comparison below trivially pass.
  testthat::expect_true(
    all(c("area_code", "iso3c", "area_name", "lpjml_code") %in% names(f))
  )
  testthat::expect_gt(nrow(f), 150L)

  cw <- as.data.frame(whep::polity_area_crosswalk)
  cw <- cw[which(!is.na(cw$area_code)), ]
  cw$area_code <- as.integer(cw$area_code)
  key <- cw[
    !duplicated(cw$area_code),
    c("area_code", "area_iso3c", "area_name")
  ]

  idx <- match(f$area_code, key$area_code)
  testthat::expect_equal(
    sum(is.na(idx)),
    0L,
    info = paste0(
      "area codes absent from the crosswalk: ",
      paste(utils::head(f$area_code[is.na(idx)], 10), collapse = ", ")
    )
  )

  # iso3c must agree wherever the crosswalk states one. This is the assertion that
  # catches a row pointing at the wrong territory, which no area_code-only join can.
  file_iso <- f$iso3c
  cw_iso <- key$area_iso3c[idx]
  comparable <- !is.na(cw_iso) & nzchar(cw_iso) & !is.na(file_iso)
  mismatched <- f$area_code[comparable & cw_iso != file_iso]
  testthat::expect_equal(
    length(mismatched),
    0L,
    info = paste0(
      "rows whose iso3c contradicts the crosswalk for their area_code -- the row ",
      "names one territory and the join sends it another's data: ",
      paste(
        sprintf(
          "%d (file %s, crosswalk %s)",
          mismatched,
          file_iso[comparable & cw_iso != file_iso],
          cw_iso[comparable & cw_iso != file_iso]
        ),
        collapse = "; "
      )
    )
  )

  # Names may differ only in the four known short-form cases.
  cw_name <- key$area_name[idx]
  differs <- which(!is.na(cw_name) & cw_name != f$area_name)
  testthat::expect_setequal(
    f$area_code[differs],
    c(150L, 167L, 223L, 229L)
  )
})

testthat::test_that("no two areas share an LPJmL country index", {
  path <- system.file("extdata", "cow_to_lpjml.csv", package = "whep")
  testthat::skip_if(path == "", "cow_to_lpjml.csv not installed")
  f <- utils::read.csv(path, stringsAsFactors = FALSE)

  # Two areas on one index would merge their grid cells into a single LPJmL country
  # silently, and the Mayotte row shows the file is capable of being wrong about
  # which area a row describes. Checked in both directions: duplicate indices, and
  # duplicate area codes.
  testthat::expect_equal(
    sum(duplicated(f$lpjml_code)),
    0L,
    info = paste0(
      "lpjml_code values used twice: ",
      paste(unique(f$lpjml_code[duplicated(f$lpjml_code)]), collapse = ", ")
    )
  )
  testthat::expect_equal(
    sum(duplicated(f$area_code)),
    0L,
    info = paste0(
      "area codes appearing twice: ",
      paste(unique(f$area_code[duplicated(f$area_code)]), collapse = ", ")
    )
  )
})

# Verify the indices against LPJmL's OWN country list, which is the authority. Every
# lpjml_code must exist in `include/managepar.h` and name the same country the row does.
#
# This became possible only after finding a local LPJmL checkout. Until then the Mayotte
# defect had to be settled by internal evidence — the file is ordered by iso3c and
# lpjml_code tracks that order, so index 153 sitting between MYS 152 and NAM 154 belongs to
# MYT — and Comoros was deliberately left unmapped rather than given an index inferred from
# the same ordering. `#define Comoros 46` confirms the inference was right, so the row is now
# present with evidence instead of absent for want of it (whep#404).
#
# Two things the header settled beyond that: both LPJmL 6.0.5 and 6.1.1 define all 257
# indices identically, so version drift is not a hazard here; and all 189 pre-existing
# indices exist in it.
#
# 22 rows name their country differently from the header — "Lao People's Democratic
# Republic" against "Laos", "Republic of Korea" against "South_Korea", "Cabo Verde" against
# "Cape_Verde". Every one is the same country under a different naming convention, FAOSTAT's
# against LPJmL's, so the check compares only that the index EXISTS and that the two names
# are not different countries; it cannot compare strings.
#
# Skips without a local LPJmL checkout, since the header is not part of this repository.
testthat::test_that("every lpjml_code exists in LPJmL's own country list", {
  headers <- c(
    "~/LPJmL-611/include/managepar.h",
    "~/LPJmL/include/managepar.h"
  )
  headers <- headers[file.exists(path.expand(headers))]
  testthat::skip_if(
    length(headers) == 0L,
    "no local LPJmL checkout; include/managepar.h is not part of this repo"
  )

  parse_defines <- function(path) {
    lines <- readLines(path.expand(path), warn = FALSE)
    m <- regmatches(
      lines,
      regexec("^#define\\s+([A-Za-z_0-9]+)\\s+([0-9]+)\\s*$", lines)
    )
    m <- Filter(function(x) length(x) == 3L, m)
    stats::setNames(
      as.integer(vapply(m, `[`, character(1), 3L)),
      vapply(m, `[`, character(1), 2L)
    )
  }

  defs <- parse_defines(headers[[1]])
  testthat::expect_gt(length(defs), 200L)

  # Both versions must agree, or "the LPJmL index" would be version-dependent and this
  # file would need to say which.
  if (length(headers) > 1L) {
    other <- parse_defines(headers[[2]])
    shared <- intersect(names(defs), names(other))
    testthat::expect_gt(length(shared), 200L)
    testthat::expect_equal(
      sum(defs[shared] != other[shared]),
      0L,
      info = "the two LPJmL versions disagree on a country index"
    )
  }

  path <- system.file("extdata", "cow_to_lpjml.csv", package = "whep")
  testthat::skip_if(path == "", "cow_to_lpjml.csv not installed")
  f <- utils::read.csv(path, stringsAsFactors = FALSE)

  unknown <- setdiff(f$lpjml_code, unname(defs))
  testthat::expect_equal(
    length(unknown),
    0L,
    info = paste0(
      "indices that LPJmL does not define, so the raster would carry a country ",
      "number LPJmL cannot interpret: ",
      paste(utils::head(sort(unknown), 10), collapse = ", ")
    )
  )

  # And Comoros specifically, which is the row this header made it possible to add.
  testthat::expect_equal(unname(defs[["Comoros"]]), 46L)
  testthat::expect_equal(
    f$lpjml_code[f$area_code == 45L],
    46L
  )
  testthat::expect_equal(f$lpjml_code[f$area_code == 270L], 153L)
})
