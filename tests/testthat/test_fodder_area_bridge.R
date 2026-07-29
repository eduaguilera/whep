# The EuropeAgriDB fodder input is keyed by an ADB region code, not by a FAOSTAT area, so it
# reaches a polity only through regions_full's `ADB_Region` column. Every row must arrive
# somewhere: a row with no area is a row of production silently dropped from the dataset.
#
# It was 13.3% when first measured, from two independent causes:
#
#   Czechoslovakia, 1,062 rows. The bridge joined regions_full's family key against the
#     lookup's `area_iso3c`, and those are separate fields. Correcting area 51's key from CSK
#     to F51 — right in itself, since no CSK polity exists — silently stopped the join
#     matching, because `area_iso3c` is still CSK. The bridge now joins numeric code to
#     numeric code, which removes the whole class.
#   Austria and the United Kingdom, 2,030 rows. Both have a blank ADB_Region in the vendored
#     table while every other EU country carries its two-letter code, and the pin reports them
#     under AT and GB. Filled as a documented override in data-raw.
#
# Neither aborted anything. The first only surfaced because a real-data run was made and the
# NA rate measured; the second had been dropping 8.8% of fodder rows for as long as the bridge
# has existed.

testthat::test_that("every fodder row reaches an area and a polity", {
  fodder <- tryCatch(whep:::.read_fodder_euadb(), error = function(e) NULL)
  testthat::skip_if(is.null(fodder), "eu-agridb-fodder pin unavailable")

  # Non-vacuous: an empty read would satisfy every assertion below.
  testthat::expect_gt(nrow(fodder), 20000L)
  testthat::expect_true(all(c("area", "area_code") %in% names(fodder)))

  for (col in c("area", "area_code")) {
    missing <- sum(is.na(fodder[[col]]))
    testthat::expect_equal(
      missing,
      0L,
      info = paste0(
        missing,
        " of ",
        nrow(fodder),
        " fodder rows have no ",
        col,
        " — that is production dropped from the dataset, not merely unlabelled. ",
        "Check regions_full$ADB_Region against the pin's Region column."
      )
    )
  }
})

testthat::test_that("regions_full carries an ADB region for every EU reporter it needs", {
  # Guards the override directly, so removing it fails here rather than showing up as a
  # silently lower row count in a dataset nobody is looking at.
  rf <- as.data.frame(whep::regions_full)
  adb <- stats::setNames(rf$ADB_Region, rf$code)
  testthat::expect_equal(unname(adb[as.character(11L)]), "AT")
  testthat::expect_equal(unname(adb[as.character(229L)]), "GB")
  # And the codes stay unique, so two areas cannot claim one ADB region.
  present <- stats::na.omit(rf$ADB_Region)
  testthat::expect_equal(anyDuplicated(present), 0L)
})
