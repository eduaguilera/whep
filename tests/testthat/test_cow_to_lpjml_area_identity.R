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
