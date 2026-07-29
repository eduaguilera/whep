# No area that carries data of its own may be folded into a rest-of-world polity.
#
# The fold exists for a good reason: FABIO aggregates many small territories into a
# single rest-of-world bucket, and routing them individually would diverge from FABIO
# for no benefit. But the test for "carries data of its own" was originally the `cbs`
# flag, which covers commodity balances only. Eleven folded areas carry no commodity
# balances and substantial production and trade — Bermuda 67,310 observed rows, Faroe
# Islands 45,036, Cook Islands 42,137, Palestine 32,534, Equatorial Guinea 23,719, Niue
# 22,055, Reunion 13,083, Guadeloupe 11,766, Martinique 9,541, Palau 9,051, French
# Guiana 8,934. All eleven were routed to ROW-1850-2023 while each has its own live
# polity that the upstream alias map already targeted for the same label: two published
# contracts disagreeing about where one territory's data belongs.
#
# The condition now consults the alias map's `observed_rows`, published upstream for
# exactly this purpose. This test states the invariant directly so the next narrowing of
# the fold condition cannot quietly re-bury an area with data.
#
# Derived, not enumerated: it asks the alias map which labels have data rather than
# listing the eleven, so an area that acquires data later is covered without an edit.

testthat::test_that("no ROW-folded area has observed data of its own", {
  cw <- as.data.frame(whep::polity_area_crosswalk)
  al <- as.data.frame(whep::polity_label_aliases)
  testthat::expect_true("observed_rows" %in% names(al))

  with_data <- unique(al$source_label[
    !is.na(al$observed_rows) &
      al$observed_rows > 0 &
      !startsWith(al$polity_code, "ROW-")
  ])
  # Guard the sweep: if observed_rows stops arriving this must not pass vacuously.
  testthat::expect_gt(length(with_data), 100L)

  folded_areas <- unique(cw$area_code[
    !is.na(cw$polity_code) & startsWith(cw$polity_code, "ROW-")
  ])
  # The fold must still be doing its job — 46 areas genuinely have no data.
  testthat::expect_gt(length(folded_areas), 20L)

  offending <- unique(cw[
    cw$area_code %in% folded_areas & cw$area_name %in% with_data,
    c("area_code", "area_name")
  ])
  testthat::expect_equal(
    nrow(offending),
    0L,
    info = paste0(
      "areas folded into a rest-of-world polity despite having observed data: ",
      paste(
        utils::head(paste0(offending$area_code, " ", offending$area_name), 8),
        collapse = ", "
      )
    )
  )
})

testthat::test_that("the eleven reattributed areas reach their own polities", {
  # Pinned by area code, checking only that the target is NOT a ROW polity and that a
  # polity resolves at all. The exact code is deliberately not asserted: several of
  # these split into two periods (Equatorial Guinea, French Guiana, Reunion, Bermuda),
  # and which period a year lands in is the year-aware matcher's business, tested
  # elsewhere.
  cw <- as.data.frame(whep::polity_area_crosswalk)
  reattributed <- c(17L, 47L, 61L, 64L, 69L, 87L, 135L, 160L, 180L, 182L, 299L)
  for (ac in reattributed) {
    # `which()`, not a bare logical: the crosswalk carries rows with NA area_code
    # (statistical non-reporting areas), and `[` with an NA index returns NA-filled
    # rows. Those made `any(startsWith(NA, "ROW-"))` evaluate to NA, which fails
    # expect_false — so this reported all eleven areas as still folded when in fact
    # every one had moved.
    rows <- cw[which(cw$area_code == ac & !is.na(cw$polity_code)), ]
    testthat::expect_gt(nrow(rows), 0L)
    testthat::expect_false(
      any(startsWith(rows$polity_code, "ROW-")),
      info = paste0("area ", ac, " still folds to a rest-of-world polity")
    )
  }
})
