# regions_full$ADB_Region is the join key between the EU AgriDB fodder input and the
# polities. Two things about it are worth stating plainly, because neither is
# apparent from the name.
#
# FIRST, IT IS NOT A REGION. Its 28 values are ISO 3166-1 alpha-2 country codes --
# AT, BE, BG, CZ, DE -- plus two composites for historical unions, BE_LU for
# Belgium-Luxembourg (area 15) and CZ_SK for Czechoslovakia (area 51). One value per
# country, so a column named for a regional grouping is in fact a country
# identifier. That is a naming problem rather than a data problem: the composites
# are exactly why the column cannot simply be an iso2c, since AgriDB reports the
# unions as single reporters and the polities database models them as their own
# polities (BLX, CSK).
#
# SECOND, THIS JOIN HAS BROKEN TWICE. Once loudly, when a rename from polity_code to
# polity_prefix missed this call site and the build aborted. Once silently, when the
# Czechoslovakia key was corrected from CSK to F51 -- right in itself, since no CSK
# polity exists -- and the join simply stopped matching, dropping 13% of fodder rows
# with no error at all. The current form joins numeric code to numeric code, which
# removes the class, but nothing checked that the two vocabularies still cover each
# other.
#
# They do, exactly: 28 values on each side, neither direction leaking. Measured
# against the real pin, not a fixture. Asserted in both directions because the two
# failures are different -- a fodder code with no regions_full row drops data, while
# a regions_full code with no fodder row is merely unused, and only the first is a
# defect.
testthat::test_that("every ADB_Region row can be reached by the code-to-code join", {
  r <- as.data.frame(whep::regions_full)
  adb <- r[which(!is.na(r$ADB_Region) & nzchar(r$ADB_Region)), ]

  # 28 is load-bearing: it is the evidence for the comment in build_production.R
  # claiming the prefix hop was never needed. That comment said 26 until Austria and
  # the United Kingdom had their codes filled in, so the count is pinned here rather
  # than left in prose that drifts.
  testthat::expect_equal(nrow(adb), 28L)
  testthat::expect_equal(sum(is.na(adb$code)), 0L)

  # The composites are the reason this cannot be replaced by a plain ISO2 lookup.
  testthat::expect_true(all(c("BE_LU", "CZ_SK") %in% adb$ADB_Region))
  unions <- adb[adb$ADB_Region %in% c("BE_LU", "CZ_SK"), ]
  testthat::expect_setequal(unions$code, c(15L, 51L))

  # Every other value is a two-letter code, and each names exactly one area. A
  # duplicate would silently merge two countries' fodder.
  plain <- adb$ADB_Region[!adb$ADB_Region %in% c("BE_LU", "CZ_SK")]
  testthat::expect_true(all(nchar(plain) == 2L))
  testthat::expect_equal(sum(duplicated(adb$ADB_Region)), 0L)
})

testthat::test_that("the fodder input and ADB_Region cover each other exactly", {
  fodder <- tryCatch(
    whep:::whep_read_file("eu-agridb-fodder"),
    error = function(e) NULL
  )
  testthat::skip_if(is.null(fodder), "eu-agridb-fodder pin unavailable")
  testthat::skip_on_ci()

  fodder <- as.data.frame(fodder)
  testthat::expect_true("Region" %in% names(fodder))
  source_codes <- sort(unique(stats::na.omit(as.character(fodder$Region))))
  # Non-vacuous: a renamed column would make both setdiffs empty and this would
  # pass while comparing nothing to nothing.
  testthat::expect_gt(length(source_codes), 20L)

  r <- as.data.frame(whep::regions_full)
  mapped <- sort(unique(stats::na.omit(
    r$ADB_Region[which(nzchar(r$ADB_Region))]
  )))

  # The direction that loses data: a reporter the join cannot place.
  unplaced <- setdiff(source_codes, mapped)
  at_risk <- sum(fodder$Region %in% unplaced, na.rm = TRUE)
  testthat::expect_equal(
    length(unplaced),
    0L,
    info = paste0(
      "fodder reporters with no ADB_Region row, whose rows drop silently: ",
      paste(unplaced, collapse = ", "),
      " (",
      at_risk,
      " of ",
      nrow(fodder),
      " rows)"
    )
  )

  # The other direction is not a defect, only waste, so it is reported rather than
  # asserted as zero -- except that today it IS zero, and saying so is what makes a
  # future gap visible.
  testthat::expect_equal(
    length(setdiff(mapped, source_codes)),
    0L,
    info = paste0(
      "ADB codes no fodder row uses: ",
      paste(setdiff(mapped, source_codes), collapse = ", ")
    )
  )
})
