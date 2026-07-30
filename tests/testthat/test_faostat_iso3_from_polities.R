# `.correct_iso3_from_polities()` replaced seven hardcoded ISO3 patches with a lookup against
# the crosswalk. Two things have to hold for that to be an improvement rather than a rewrite:
# the seven cases it replaced must still come out the same, and the lookup must not be
# ambiguous.
#
# The seven are asserted by NAME rather than by count, because the point of the change is that
# the class is covered, not that seven particular strings are. They were introduced as
# "manually fix some crazy countries/ISO3_CODE" and each is a FAOSTAT rename or long form:
# "China, mainland", "Türkiye", "Netherlands (Kingdom of the)", "Sudan", "South Sudan",
# "Czechia", "Lao People's Democratic Republic".

testthat::test_that("the seven formerly-hardcoded names still get their own ISO3", {
  expected <- c(
    "China, mainland" = "CHN",
    "Türkiye" = "TUR",
    "Netherlands (Kingdom of the)" = "NLD",
    "Sudan" = "SDN",
    "South Sudan" = "SSD",
    "Czechia" = "CZE",
    "Lao People's Democratic Republic" = "LAO"
  )

  # `fillCountryCode()` is what produced the wrong answers these patches existed to fix, so
  # the input here is the worst case: no code at all for any of them.
  df <- data.frame(
    area = names(expected),
    ISO3_CODE = NA_character_,
    stringsAsFactors = FALSE
  )
  out <- suppressMessages(whep:::.correct_iso3_from_polities(df))

  testthat::expect_equal(out$ISO3_CODE, unname(expected))
})

testthat::test_that("the crosswalk lookup is unambiguous for reporting areas", {
  # This is the assumption the function rests on, and it is NOT true of the whole crosswalk.
  # Unrestricted, "France" maps to FRA and BLM, "United Kingdom" to GGY/JEY/IMN, "Finland" to
  # FIN and ALA -- dependencies that have a polity and an ISO3 but no FAOSTAT area of their own
  # (whep#407). A lookup built without the `area_code` filter would resolve "France" by row
  # order and could label French rows BLM.
  cw <- as.data.frame(whep::polity_area_crosswalk)

  keep <- which(
    !is.na(cw$area_name) & !is.na(cw$area_iso3c) & !is.na(cw$area_code)
  )
  lookup <- unique(cw[keep, c("area_name", "area_iso3c")])
  # Non-vacuous: an empty or tiny lookup would make the uniqueness claim hollow.
  testthat::expect_gt(nrow(lookup), 200L)
  testthat::expect_equal(sum(duplicated(lookup$area_name)), 0L)

  # And the filter is load-bearing: without it the ambiguity is real, so this documents WHY
  # the filter is there rather than leaving it to be tidied away as redundant.
  unfiltered <- unique(cw[
    which(!is.na(cw$area_name) & !is.na(cw$area_iso3c)),
    c("area_name", "area_iso3c")
  ])
  testthat::expect_gt(sum(duplicated(unfiltered$area_name)), 0L)
})

testthat::test_that("a code the crosswalk cannot vouch for is left alone", {
  # The function corrects what it can prove and does not guess. A name absent from the
  # crosswalk keeps whatever `fillCountryCode()` returned -- including NA, because inventing a
  # code for an unknown area is how a wrong join starts.
  df <- data.frame(
    area = c("Sudan", "Neverland", "Atlantis"),
    ISO3_CODE = c("XXX", "NVL", NA_character_),
    stringsAsFactors = FALSE
  )
  out <- suppressMessages(whep:::.correct_iso3_from_polities(df))

  # Corrected, because the crosswalk knows Sudan and disagreed.
  testthat::expect_equal(out$ISO3_CODE[1], "SDN")
  # Untouched, because it does not.
  testthat::expect_equal(out$ISO3_CODE[2], "NVL")
  testthat::expect_true(is.na(out$ISO3_CODE[3]))
})

testthat::test_that("the correction announces what it changed", {
  # Silent correction of a published identifier is how the seven patches went unnoticed for as
  # long as they did. A caller should be able to see that a code was overridden and to what.
  df <- data.frame(
    area = c("Czechia", "Sudan"),
    ISO3_CODE = c(NA_character_, "WRONG"),
    stringsAsFactors = FALSE
  )
  msg <- testthat::capture_messages(whep:::.correct_iso3_from_polities(df))
  joined <- paste(msg, collapse = " ")
  testthat::expect_match(joined, "Czechia", fixed = TRUE)
  testthat::expect_match(joined, "CZE", fixed = TRUE)
  testthat::expect_match(joined, "SDN", fixed = TRUE)
})

testthat::test_that("input without the expected columns passes through untouched", {
  df <- data.frame(x = 1:3)
  testthat::expect_equal(whep:::.correct_iso3_from_polities(df), df)
})
