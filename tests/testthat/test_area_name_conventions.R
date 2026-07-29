# `regions_full` carries THREE columns that look like an area name, and they are not
# interchangeable. Code that joins on an area name — and a lot of it does, `by = c("year",
# "area", ...)` appears throughout build_production.R and build_cbs.R — must use the canonical
# one, or rows silently fail to match. An unmatched join yields NA, not an error.
#
# Measured across the 260 areas the crosswalk and regions_full share:
#
#   FAOSTAT_name   differs from the crosswalk's area_name on 0. This is the canonical form.
#   name           differs on 15 — short forms: Turkey, Czech Republic, Ivory Coast, Lao,
#                  North Korea, Tanzania, United Kingdom, Bolivia, Iran, Venezuela, Moldova,
#                  Netherlands, South Korea, Reunion, Cote d'Ivoire's long form.
#   polity_name    differs on 72 — it carries the AGGREGATE label for folded areas
#                  ("Latin America Other" for Bermuda) as well as short forms.
#
# Both non-canonical columns have already cost real data on this branch. gdp-population's pin
# uses short forms and 1,892 of its rows (6.4%) joined to nothing. The fodder bridge labelled
# its rows from polity_name, so Netherlands and the United Kingdom arrived under names the
# downstream join would not match — and the UK was the area whose ADB code had only just been
# filled in, which would have made that fix look complete while half of it silently failed.

testthat::test_that("FAOSTAT_name is the canonical area name and agrees with the crosswalk", {
  cw <- unique(as.data.frame(whep::polity_area_crosswalk)[, c(
    "area_code",
    "area_name"
  )])
  cw <- cw[!is.na(cw$area_code) & !is.na(cw$area_name), ]
  rf <- as.data.frame(whep::regions_full)[, c(
    "code",
    "name",
    "FAOSTAT_name",
    "polity_name"
  )]
  both <- merge(cw, rf, by.x = "area_code", by.y = "code")
  testthat::expect_gt(nrow(both), 200L)

  # The invariant worth locking: the crosswalk's area_name IS regions_full's FAOSTAT_name.
  # Anything joining on an area name can rely on that and on nothing else.
  differing <- both[
    !is.na(both$FAOSTAT_name) & both$area_name != both$FAOSTAT_name,
  ]
  testthat::expect_equal(
    nrow(differing),
    0L,
    info = paste0(
      "the crosswalk's area_name has diverged from regions_full$FAOSTAT_name, so every join ",
      "keyed on an area name is now unreliable: ",
      paste(utils::head(differing$area_code, 8), collapse = ", ")
    )
  )

  # And the two traps must remain visibly different, so nobody concludes from a green suite
  # that the columns are interchangeable. If a future cleanup genuinely unifies them, these
  # fail and the comment above needs rewriting.
  testthat::expect_gt(sum(both$area_name != both$name, na.rm = TRUE), 0L)
  testthat::expect_gt(sum(both$area_name != both$polity_name, na.rm = TRUE), 0L)
})

testthat::test_that("the fodder bridge labels rows with the canonical area name", {
  fodder <- tryCatch(whep:::.read_fodder_euadb(), error = function(e) NULL)
  testthat::skip_if(is.null(fodder), "eu-agridb-fodder pin unavailable")

  cw <- unique(as.data.frame(whep::polity_area_crosswalk)[, c(
    "area_code",
    "area_name"
  )])
  pairs <- unique(as.data.frame(fodder)[, c("area_code", "area")])
  m <- merge(pairs, cw, by = "area_code", all.x = TRUE)
  testthat::expect_gt(nrow(m), 20L)

  mismatch <- m[!is.na(m$area_name) & m$area != m$area_name, ]
  testthat::expect_equal(
    nrow(mismatch),
    0L,
    info = paste0(
      "fodder rows carry a non-canonical area label, so they will not match downstream ",
      "joins keyed on area: ",
      paste(
        utils::head(paste0(mismatch$area, " != ", mismatch$area_name), 5),
        collapse = "; "
      )
    )
  )
})
