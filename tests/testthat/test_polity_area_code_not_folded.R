# `polity_area_code` is the numeric area key the builds actually key on —
# get_primary_production() emits it AS its `area_code`, and build_trade.R assigns
# `area_code := polity_area_code` outright. So an area folded at THIS level is folded in the
# output regardless of what `polity_code` says.
#
# That distinction cost a full smoke run to find. The eleven areas pulled out of the FABIO
# rest-of-world fold had their `polity_code` corrected to their own polities, and I reported
# that as done — but `polity_area_code` still took `fabio_code`, which is 999 for every
# folded area. The result: the Faroe Islands' 2,458 raw production rows and Palestine's 9,606
# were still summed into area 999 and attributed to ROW-1850-2023, while the crosswalk
# claimed FRO-1800-2025 and PSE-1948-2025. Two representations of one decision, and only one
# had been fixed.
#
# Nothing caught it, and "0 rows unresolved" actively concealed it: every row DID have a
# reporting polity, because 999 resolves to ROW perfectly well.

testthat::test_that("areas with data of their own keep their own numeric area code", {
  cw <- as.data.frame(whep::polity_area_crosswalk)
  reattributed <- c(17L, 47L, 61L, 64L, 69L, 87L, 135L, 160L, 180L, 182L, 299L)

  for (ac in reattributed) {
    rows <- cw[which(cw$area_code == ac), ]
    testthat::expect_gt(nrow(rows), 0L)
    testthat::expect_true(
      all(rows$polity_area_code == ac),
      info = paste0(
        "area ",
        ac,
        " carries polity_area_code ",
        paste(unique(rows$polity_area_code), collapse = "/"),
        " — folding it at the numeric level puts its data back into the aggregate ",
        "no matter what polity_code says"
      )
    )
  }

  # And the areas that genuinely have no data of their own must STILL fold, so this does not
  # quietly widen into "nothing ever folds".
  still_folded <- c(30L, 152L, 252L, 254L)
  for (ac in still_folded) {
    rows <- cw[which(cw$area_code == ac), ]
    if (nrow(rows) == 0L) {
      next
    }
    testthat::expect_true(
      all(rows$polity_area_code == 999L),
      info = paste0(
        "area ",
        ac,
        " has no data of its own and should still fold to 999"
      )
    )
  }
})

# whep#403 asked whether unfolding thirteen territories from FABIO's rest-of-world bucket
# breaks a comparison against FABIO. When I filed it I wrote "if any published comparison
# against FABIO exists that I have not found, it will shift by these 73k rows". One does:
# inst/scripts/compare_fabio_footprints.R reproduces FABIO land footprints for benchmark
# years and compares them with WHEP's.
#
# It is unaffected, and the reason is structural rather than lucky. The script targets
# CHN, USA and EU28, selecting WHEP areas with
# `regions_full |> filter(iso3c %in% target_iso)`. None of the thirteen carries one of
# those codes — the French overseas departments are GUF, GLP, MTQ and REU rather than FRA,
# so they are not part of the EU28 selection even though France is:
#
#   17 BMU   47 COK   61 GNQ   64 FRO   69 GUF   87 GLP   88 GUM
#  135 MTQ  160 NIU  180 PLW  182 REU  209 SWZ  299 PSE
#
# So the divergence is confined to rest-of-world, which the script never compares. That is
# worth asserting rather than noting: a future unfold of a territory that DOES carry a
# target iso3c would silently change a published comparison, and this is where that would
# surface.
testthat::test_that("no unfolded territory falls inside the FABIO comparison's targets", {
  r <- as.data.frame(whep::regions_full)
  # Areas that report their own polity while FABIO folds them into rest-of-world: their
  # fabio_code is 999 and their polity_area_code is their own.
  unfolded <- unique(r$code[which(
    r$fabio_code == 999L & r$polity_area_code == r$code & !is.na(r$code)
  )])
  # Non-vacuous: if nothing is unfolded the assertion below is empty of content.
  testthat::expect_gt(length(unfolded), 10L)

  eu28 <- c(
    "AUT",
    "BEL",
    "BGR",
    "HRV",
    "CYP",
    "CZE",
    "DNK",
    "EST",
    "FIN",
    "FRA",
    "DEU",
    "GRC",
    "HUN",
    "IRL",
    "ITA",
    "LVA",
    "LTU",
    "LUX",
    "MLT",
    "NLD",
    "POL",
    "PRT",
    "ROU",
    "SVK",
    "SVN",
    "ESP",
    "SWE",
    "GBR"
  )
  targets <- c("CHN", "USA", eu28)

  iso <- r$iso3c[match(unfolded, r$code)]
  clash <- unfolded[which(iso %in% targets)]
  testthat::expect_equal(
    length(clash),
    0L,
    info = paste0(
      "unfolded areas whose iso3c is a target of ",
      "inst/scripts/compare_fabio_footprints.R, so unfolding them changes a ",
      "published FABIO comparison: ",
      paste(clash, collapse = ", ")
    )
  )
})
