# Two columns in the crosswalk look like the same thing and are not. `area_iso3c` is the reporting
# AREA's code; `iso3_code` is the resolved POLITY's. They differ on 56 of the 273 rows carrying both,
# and every difference is meaningful:
#
#   an area folded into a rest-of-world polity keeps its own code while the polity's is ROW —
#     ASM against ROW, AND against ROW, ABW against ROW
#   a colonial-era row carries the era's key while the area keeps the modern one —
#     AGO against ANG, BWA against BEC, BEL against BLX
#
# A consumer assuming they agree gets the wrong answer on a fifth of the table, silently. This is the
# same "two names, two meanings" hazard as the four misnamed columns this branch found, except here
# both names are defensible and the fix is documentation rather than a rename.
#
# The test asserts they REMAIN distinct. If a refactor ever made them identical, the documentation
# above would become misleading in the opposite direction, and someone would have collapsed a real
# distinction rather than a redundant one.

testthat::test_that("polities' iso3_code and iso3c really are aliases", {
  p <- as.data.frame(whep::polities)
  a <- p$iso3_code
  b <- p$iso3c
  agree <- (is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & a == b)
  testthat::expect_gt(length(agree), 500L)
  testthat::expect_true(
    all(agree),
    info = paste0(
      "iso3c is documented as a compatibility alias for iso3_code but differs for: ",
      paste(utils::head(p$polity_code[!agree], 8), collapse = ", ")
    )
  )
})

testthat::test_that("the crosswalk's area_iso3c and iso3_code stay distinct", {
  cw <- unique(as.data.frame(whep::polity_area_crosswalk)[,
    c("area_code", "area_iso3c", "iso3_code")
  ])
  both <- cw[which(!is.na(cw$area_iso3c) & !is.na(cw$iso3_code)), ]
  testthat::expect_gt(nrow(both), 200L)

  differing <- both[both$area_iso3c != both$iso3_code, ]
  # They must differ somewhere, or the documented distinction is fiction.
  testthat::expect_gt(nrow(differing), 20L)

  # Three explained causes: the ROW fold, a known multi-era override, and a parent-state row
  # carrying a dependency's code. Anything else means a fourth nobody has looked at.
  overrides <- c(
    7L,
    15L,
    20L,
    51L,
    72L,
    181L,
    206L,
    228L,
    237L,
    240L,
    248L,
    249L,
    251L
  )
  unexplained <- differing[
    differing$iso3_code != "ROW" &
      !is.na(differing$area_code) &
      !differing$area_code %in% overrides,
  ]
  testthat::expect_equal(
    nrow(unexplained),
    0L,
    info = paste0(
      "these areas' iso3_code differs from area_iso3c for neither the ROW fold nor a known ",
      "multi-era override: ",
      paste(
        utils::head(
          paste0(
            unexplained$area_code,
            " ",
            unexplained$area_iso3c,
            "/",
            unexplained$iso3_code
          ),
          8
        ),
        collapse = ", "
      )
    )
  )
})
