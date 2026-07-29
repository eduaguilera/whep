# data-raw/table_mappings.R carries `manual_area_prefixes`, eleven hand-written area-to-prefix
# overrides. Its own comment concedes that "upstream's own FAOSTAT matching pipeline already
# assigns exactly these polities", and test_upstream_faostat_agreement.R compares the two — but
# that test needs the private upstream repository, so it is one of the eight [upstream] skips and
# never runs on CI.
#
# Checked against the published map: all eleven are already there, with year bounds the manual
# list does not have.
#
#   51 -> F51    228 -> F228   248 -> F248   72 -> FRS      206 -> SUD
#   7 -> ANG+AGO    20 -> BEC+BWA    181 -> SRH+ZWE    237 -> F237+VNM
#   249 -> F249+YEM    251 -> NRH+ZMB
#
# So the override list is a second authority for a question upstream answers — the very thing
# resolve_polity_label()'s documentation says not to do for aliases: "a label's meaning is a fact
# about the source, upstream already decides it, and a second lookup in this package would be a
# second authority for the same question."
#
# Replacing the list with a read of the published map is the right repair and is a larger change
# than a test. What this does is make the duplication VERIFIED rather than merely conceded, using
# only the embedded crosswalk so it runs on CI: each override must have actually produced the
# prefixes it claims. If upstream's mapping moves and the hand-written list does not, this fails
# here instead of silently disagreeing.

testthat::test_that("each manual area override produced the prefixes it claims", {
  expected <- list(
    "51" = "F51",
    "228" = "F228",
    "248" = "F248",
    "72" = "FRS",
    # Both, deliberately. The override splits area 206 at the 2011 secession — SUD-* for the
    # pre-secession state "which INCLUDED present-day South Sudan", SDN-2011-2025 for after —
    # and the four periods are contiguous and non-overlapping, so the year-aware pick is
    # unambiguous.
    #
    # This is the ONE place the override and the published map disagree: upstream maps area 206
    # to SUD only, declining to give the former-Sudan area any post-2011 mapping. Both readings
    # are defensible and the difference is inert in practice, because FAOSTAT reports post-2011
    # years under areas 276 and 277 rather than 206. Recorded rather than silently reconciled —
    # I first wrote "SUD" here by copying upstream, and this assertion is what surfaced the
    # divergence.
    "206" = c("SDN", "SUD"),
    "7" = c("AGO", "ANG"),
    "20" = c("BEC", "BWA"),
    "181" = c("SRH", "ZWE"),
    "237" = c("F237", "VNM"),
    "249" = c("F249", "YEM"),
    "251" = c("NRH", "ZMB")
  )

  cw <- as.data.frame(whep::polity_area_crosswalk)
  for (area in names(expected)) {
    rows <- cw[
      which(cw$area_code == as.integer(area) & !is.na(cw$polity_code)),
    ]
    # expect_true rather than expect_gt: only the former carries `info`, and naming the area
    # is the whole point of the message.
    testthat::expect_true(
      nrow(rows) > 0L,
      info = paste0("area ", area, " resolves to no polity at all")
    )
    got <- sort(unique(sub("-.*", "", rows$polity_code)))
    testthat::expect_setequal(got, sort(expected[[area]]))
  }
})

testthat::test_that("the two-prefix areas really do span eras without overlapping", {
  # The override comment justifies listing two prefixes per area by asserting the chains are
  # "contiguous and overlap-free", so add_polity_code()'s year logic can pick the era. That is
  # the load-bearing claim — if two polities of an area overlapped, the year-aware pick would be
  # ambiguous and the override would be routing data by tie-break order.
  cw <- as.data.frame(whep::polity_area_crosswalk)
  for (area in c(7L, 20L, 181L, 237L, 249L, 251L)) {
    rows <- unique(cw[
      which(
        cw$area_code == area &
          !is.na(cw$polity_code) &
          !is.na(cw$polity_start_year) &
          !is.na(cw$polity_end_year)
      ),
      c("polity_code", "polity_start_year", "polity_end_year")
    ])
    testthat::expect_gt(nrow(rows), 1L)
    rows <- rows[order(rows$polity_start_year), ]
    overlaps <- rows$polity_start_year[-1] < rows$polity_end_year[-nrow(rows)]
    testthat::expect_false(
      any(overlaps),
      info = paste0(
        "area ",
        area,
        " has overlapping polity periods, so the two-prefix override routes ",
        "by tie-break rather than by era: ",
        paste(rows$polity_code[c(FALSE, overlaps)], collapse = ", ")
      )
    )
  }
})
