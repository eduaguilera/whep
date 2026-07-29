# Which reattributed areas can have their pre-1962 production back-cast, and why the rest
# cannot.
#
# The historical extension needs a LUH2 land series for an area, keyed by ISO3. Pulling eleven
# areas out of the rest-of-world fold moved them from "back-cast as part of the RoW aggregate"
# to "back-cast on their own land series, if one exists" — and for four of them none does, so
# the count of areas with no LUH2 match went 5 -> 9.
#
# Those four are exactly the French overseas departments: French Guiana, Guadeloupe,
# Martinique and Reunion. LUH2 carries 237 ISO3 codes and none of them, because a departement
# is legally part of France and LUH2 folds them into FRA. That is not a gap we can close by
# mapping harder; the land data does not exist separately.
#
# So their pre-1962 production is no longer back-cast. That is the honest outcome — the
# alternative is attributing a share of aggregate land to a territory LUH2 never resolved —
# but it IS a change in what those four carry before 1962, and this test is where it is
# written down rather than left in a warning nobody reads.
#
# Bidirectional: if LUH2 ever gains these codes, or if one of the seven loses its coverage,
# this fails and the explanation gets revisited.

testthat::test_that("LUH2 land lost to unmatched codes stays negligible", {
  # .read_land_areas() warns that it drops LUH2 ISO3 codes with no crosswalk match. Measuring
  # it properly took two attempts and the difference matters.
  #
  # First I measured only the six named in the warning — JEY, GGY, IMN, BLM, ALA, SXM, which
  # are Jersey, Guernsey, Isle of Man, Saint-Barthelemy, Aland and Sint Maarten — and got
  # 0.1085 Mha of 13,714 in 2022, 0.0008%. True, and not the whole story.
  #
  # Against the FULL crosswalk the only unmatched code is `-99`, and it carries 42.93 Mha,
  # 0.31%. That is not a territory and not our gap: it is LUH2's own sentinel for land it does
  # not attribute to any country. The six real territories are dropped for a different reason —
  # they exist in the crosswalk but have no FAOSTAT area, so .current_area_lookup(include_
  # unmapped = FALSE) excludes them.
  #
  # So: nothing here is worth creating polities for, but the two causes are distinct and the
  # bigger number is upstream of us entirely. Asserted separately, with headroom, so a real
  # regression — a substantial territory falling out of the crosswalk — fails while these stay
  # quiet.
  land <- tryCatch(whep:::.read_input("luh2-areas"), error = function(e) NULL)
  testthat::skip_if(is.null(land), "luh2-areas pin unavailable")
  dt <- data.table::as.data.table(land)

  cw <- as.data.frame(whep::polity_area_crosswalk)
  known <- unique(stats::na.omit(cw$area_iso3c))
  latest <- max(dt$Year)
  in_year <- dt[dt$Year == latest, ]
  total <- sum(in_year$Area_Mha, na.rm = TRUE)

  # LUH2's own unattributed bucket, kept visible rather than folded into the check below.
  sentinel <- in_year[in_year$ISO3 == "-99", ]
  testthat::expect_lt(sum(sentinel$Area_Mha, na.rm = TRUE) / total, 0.01)

  # Real territories with no crosswalk match at all.
  dropped <- in_year[!in_year$ISO3 %in% c(known, "-99"), ]
  testthat::expect_lt(sum(dropped$Area_Mha, na.rm = TRUE) / total, 0.001)
})

testthat::test_that("LUH2 covers the reattributed areas except the French departments", {
  # Deliberately NOT skip_if_offline(): that implies skip_on_cran(), so the test skipped
  # even here where the pin is cached and it could have run. A skip must be for a real
  # reason, and the tryCatch below already is one — it skips exactly when the pin cannot
  # be read, and runs otherwise.
  land <- tryCatch(
    whep:::.read_input("luh2-areas"),
    error = function(e) NULL
  )
  testthat::skip_if(is.null(land), "luh2-areas pin unavailable")

  iso <- unique(land$ISO3)
  testthat::expect_gt(length(iso), 200L)

  cw <- as.data.frame(whep::polity_area_crosswalk)
  reattributed <- c(17L, 47L, 61L, 64L, 69L, 87L, 135L, 160L, 180L, 182L, 299L)
  rows <- unique(cw[
    which(cw$area_code %in% reattributed),
    c("area_code", "area_iso3c")
  ])

  covered <- rows$area_iso3c[rows$area_iso3c %in% iso]
  uncovered <- sort(rows$area_iso3c[!rows$area_iso3c %in% iso])

  # The French overseas departments, and only those.
  testthat::expect_setequal(uncovered, c("GLP", "GUF", "MTQ", "REU"))
  testthat::expect_setequal(
    sort(covered),
    c("BMU", "COK", "FRO", "GNQ", "NIU", "PLW", "PSE")
  )
})
