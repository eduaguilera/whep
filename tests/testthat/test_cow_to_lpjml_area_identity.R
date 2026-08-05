# inst/extdata/cow_to_lpjml.csv maps a FAOSTAT area_code to the country index
# LPJmL expects (include/managepar.h), and it carries its OWN iso3c and area_name
# per row. Those are a second copy of area identity, written next to the polities
# database rather than derived from it, and nothing compared the two.
#
# Comparing them found a defect. One row read:
#
#   45,"MYT","Mayotte",153
#
# FAOSTAT area 45 is COMOROS -- polity_area_crosswalk publishes it as
# COM-1946-1975 / COM-1975-2025. Mayotte is area 270, which was absent from the
# file entirely, and no row carried iso3c COM. So in
# inst/scripts/prepare_spatialize_all.R every Comoros grid cell was written into
# the LPJmL country raster with Mayotte's index, Mayotte's own cells fell to 0
# ("no country"), and the error was invisible because that join is on area_code
# alone -- iso3c and area_name are never checked against anything.
#
# WHICH FIELD WAS WRONG is settled by LPJmL's own header, not by assumption:
# `#define Comoros 46` and `#define Mayotte 153`. So index 153 was right and the
# area_code was wrong (45 -> 270), and Comoros gets the row it never had,
# 45,"COM","Comoros",46. Index 46 was previously unclaimed by any row, and both
# local LPJmL checkouts (5.9.7 and 6.1.1) define all 257 indices identically, so
# the number is not version-dependent.
#
# The four remaining name differences are cosmetic -- the file uses short forms
# where upstream uses official ones -- and are listed by area code rather than
# tolerated by a fuzzy comparison, so a fifth difference has to be looked at
# instead of blending in.
test_that("cow_to_lpjml area identity agrees with the polities crosswalk", {
  path <- system.file("extdata", "cow_to_lpjml.csv", package = "whep")
  testthat::skip_if(path == "", "cow_to_lpjml.csv not installed")
  f <- utils::read.csv(path, stringsAsFactors = FALSE)
  # Non-vacuous: a renamed column would make every comparison below pass on an
  # empty vector.
  testthat::expect_true(
    all(c("area_code", "iso3c", "area_name", "lpjml_code") %in% names(f))
  )
  testthat::expect_gt(nrow(f), 150L)

  cw <- as.data.frame(whep::polity_area_crosswalk)
  cw <- cw[!is.na(cw$area_code), c("area_code", "area_iso3c", "area_name")]
  key <- cw[!duplicated(cw$area_code), ]
  idx <- match(f$area_code, key$area_code)

  testthat::expect_equal(
    sum(is.na(idx)),
    0L,
    info = paste0(
      "area codes absent from the crosswalk: ",
      paste(utils::head(f$area_code[is.na(idx)], 10), collapse = ", ")
    )
  )

  # The assertion that catches a row pointing at the wrong territory, which no
  # area_code-only join can.
  cw_iso <- key$area_iso3c[idx]
  wrong <- which(!is.na(cw_iso) & nzchar(cw_iso) & cw_iso != f$iso3c)
  testthat::expect_equal(
    length(wrong),
    0L,
    info = paste0(
      "rows whose iso3c contradicts the crosswalk for their area_code -- the ",
      "row names one territory and the join sends it another's cells: ",
      paste(
        sprintf(
          "%d (file %s, crosswalk %s)",
          f$area_code[wrong],
          f$iso3c[wrong],
          cw_iso[wrong]
        ),
        collapse = "; "
      )
    )
  )

  # Names may differ only in the four known short-form cases: 150 Netherlands,
  # 167 Czechia, 223 Turkiye, 229 United Kingdom.
  cw_name <- key$area_name[idx]
  differs <- which(!is.na(cw_name) & cw_name != f$area_name)
  testthat::expect_setequal(f$area_code[differs], c(150L, 167L, 223L, 229L))
})

# Two areas on one index would merge their grid cells into a single LPJmL country
# silently, and the Mayotte row shows the file is capable of being wrong about
# which area a row describes. Checked in both directions.
test_that("no two areas share an LPJmL country index", {
  path <- system.file("extdata", "cow_to_lpjml.csv", package = "whep")
  testthat::skip_if(path == "", "cow_to_lpjml.csv not installed")
  f <- utils::read.csv(path, stringsAsFactors = FALSE)

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

# Verify the indices against LPJmL's own country list, which is the authority for
# what a number in this column means. Every lpjml_code must be defined in
# include/managepar.h, and the two rows this issue is about must carry the
# indices the header gives them.
#
# The header is not part of this repository, so this skips without a local LPJmL
# checkout: silent on CI, load-bearing for anyone with the sources. Point
# WHEP_LPJML_DIR at a checkout to run it from elsewhere.
test_that("every lpjml_code exists in LPJmL's own country list", {
  roots <- c(Sys.getenv("WHEP_LPJML_DIR"), "~/LPJmL-611", "~/LPJmL")
  headers <- file.path(roots[nzchar(roots)], "include", "managepar.h")
  headers <- headers[file.exists(path.expand(headers))]
  testthat::skip_if(
    length(headers) == 0L,
    "no local LPJmL checkout; include/managepar.h is not part of this repo"
  )

  parse_defines <- function(p) {
    lines <- readLines(path.expand(p), warn = FALSE)
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

  # Two versions disagreeing would make "the LPJmL index" version-dependent, and
  # this file would then have to say which version it targets.
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
      "indices LPJmL does not define, so the raster would carry a country ",
      "number LPJmL cannot interpret: ",
      paste(utils::head(sort(unknown), 10), collapse = ", ")
    )
  )

  testthat::expect_equal(unname(defs[["Comoros"]]), 46L)
  testthat::expect_equal(unname(defs[["Mayotte"]]), 153L)
  testthat::expect_equal(f$lpjml_code[f$area_code == 45L], 46L)
  testthat::expect_equal(f$lpjml_code[f$area_code == 270L], 153L)
})
