# The Bouwman feed taxonomy is the one region system in this package that is NOT
# derivable from polities -- test_dataset_area_identifiers.R pins that, and it is
# right to. But the taxonomy still has to MEET the polities somewhere, and that
# join is unguarded.
#
# Where it happens: `.feed_region_lookup()` reads `region` from
# whep::polity_area_crosswalk and renames it `region_bouwman`. The feed mix then
# joins the conversion table on that label. So one vocabulary is written in the
# crosswalk (upstream's area table) and the other in conv_bouwman (a source file),
# with nothing requiring them to agree.
#
# A single-character disagreement -- "South East Asia" against "Southeast Asia" --
# would silently drop every area in that region from the feed mix. There IS a
# warning for the case, and the warning names areas rather than regions, so a
# whole region vanishing reads as a list of unrelated countries. Measured today:
# the two vocabularies match exactly in both directions, 17 values each.
#
# The second assertion is which areas legitimately have NO Bouwman region. Nine do,
# in two classes, and the split between them is the interesting part:
#
#   30, 252, 254, 999   accounting residuals, not territories: Antarctica (no
#                       livestock), Unspecified, Others/adjustment, and RoW itself.
#   901-905             the continental "Other" buckets for Africa, Asia, Europe,
#                       Latin America and North America.
#
# The second class looks like an oversight next to area 906, Oceania Other, which
# DOES carry a region ("Oceania"). One of six continental buckets assigned and five
# not reads as exactly the kind of inconsistency this integration exists to remove.
# It is not one. Bouwman's taxonomy has a single Oceania region, so 906 maps
# unambiguously; every other continent is split across several of the seventeen --
# Africa into Eastern/Northern/Southern/Western, Asia into East/South/South
# East/Middle East/Japan, Europe into OECD/Eastern, Latin America into
# Central/South, North America into Canada/USA. There is no single correct value for
# 901-905, so NA is the honest one, and inventing one would misattribute feed
# conversion across a continent.
#
# Both halves are asserted, including 906 by name, so the asymmetry is deliberate by
# test rather than accidental by omission.
#
# Of the 47 areas whose data folds into polity_area_code 999, 43 carry their own
# region and so lose nothing -- the fold is an aggregation key, not the join key,
# and that distinction is what decides whether this is a live defect or a correct
# exemption. It is a correct exemption.
testthat::test_that("the crosswalk and conv_bouwman agree on the Bouwman vocabulary", {
  cw <- as.data.frame(whep::polity_area_crosswalk)
  cb <- as.data.frame(whep::conv_bouwman)
  testthat::expect_true("region" %in% names(cw))
  testthat::expect_true("region_bouwman" %in% names(cb))

  from_crosswalk <- sort(unique(stats::na.omit(cw$region)))
  from_source <- sort(unique(stats::na.omit(cb$region_bouwman)))
  # Non-vacuous: an empty or renamed column would make setequal pass on nothing.
  testthat::expect_equal(length(from_source), 17L)
  testthat::expect_gt(length(from_crosswalk), 10L)

  testthat::expect_equal(
    setdiff(from_crosswalk, from_source),
    character(0),
    info = paste0(
      "region labels in the crosswalk that conv_bouwman does not define, so ",
      "every area carrying them drops out of the feed mix: ",
      paste(setdiff(from_crosswalk, from_source), collapse = ", ")
    )
  )
  testthat::expect_equal(
    setdiff(from_source, from_crosswalk),
    character(0),
    info = paste0(
      "Bouwman regions with no areas assigned to them: ",
      paste(setdiff(from_source, from_crosswalk), collapse = ", ")
    )
  )
})

testthat::test_that("only accounting residuals lack a Bouwman region", {
  cw <- as.data.frame(whep::polity_area_crosswalk)
  coded <- cw[which(!is.na(cw$area_code)), ]
  by_area <- split(coded$region, coded$area_code)
  regionless <- as.integer(names(
    by_area[vapply(by_area, function(v) all(is.na(v)), logical(1))]
  ))

  # Pinned by identity, not by count. A count would let a real country replace
  # Antarctica without complaint, and the whole claim here is that every
  # region-less area is a residual rather than a territory.
  testthat::expect_equal(
    sort(regionless),
    c(30L, 252L, 254L, 901L, 902L, 903L, 904L, 905L, 999L),
    info = paste0(
      "areas with no Bouwman region: ",
      paste(sort(regionless), collapse = ", ")
    )
  )

  # Oceania Other is assigned while its five siblings are not, because Bouwman has
  # exactly one Oceania region and several for every other continent. Asserted so
  # that a reader who notices the asymmetry finds it deliberate, and so that
  # "fixing" it by blanking 906 fails.
  testthat::expect_equal(
    unique(stats::na.omit(coded$region[coded$area_code == 906L])),
    "Oceania"
  )

  # And the fold key is not the join key: areas whose data aggregates into RoW
  # keep their own region, so folding does not cost them their feed mix.
  r <- as.data.frame(whep::regions_full)
  folded <- unique(r$code[which(r$polity_area_code == 999L)])
  folded <- folded[!is.na(folded)]
  testthat::expect_gt(length(folded), 40L)
  keeps_region <- vapply(
    folded,
    function(a) any(!is.na(coded$region[coded$area_code == a])),
    logical(1)
  )
  # The four residuals are themselves in the folded set, and they are the only
  # members that legitimately lack a region.
  testthat::expect_equal(
    sort(folded[!keeps_region]),
    c(30L, 252L, 254L, 999L),
    info = paste0(
      "areas folded into RoW that also lose their region: ",
      paste(sort(folded[!keeps_region]), collapse = ", ")
    )
  )
})

# The vocabulary tests above establish that nine areas carry no Bouwman region and that
# eight of them legitimately cannot. What they do not establish is whether it MATTERS —
# and the feed mix has a warning saying it does:
#
#   No Bouwman region for {n} area(s): {dropped} t of feed demand is dropped from the mix.
#
# That warning has never fired in any smoke run, and this is why: no livestock production
# lands on a region-less area at all. Measured on a real 1990-1991 build — 24,351
# livestock rows, ZERO of them on any of the nine, 0.0000% of livestock value.
#
# So the gap is LATENT rather than live, which is the distinction that decides whether a
# missing region is a defect or a correct exemption. Antarctica has no Bouwman region and
# no livestock; the continental "Other" buckets have neither. If that ever stops being
# true the feed mix silently drops the demand, with a warning nobody reads, so it is
# asserted here instead.
#
# Real pins, so it skips on CI, and the skip is listed in the local gate's inventory
# rather than being silent.
testthat::test_that("no livestock production lands on an area without a Bouwman region", {
  testthat::skip_on_ci()
  prod <- tryCatch(
    suppressWarnings(suppressMessages(
      build_primary_production(start_year = 1990, end_year = 1991)
    )),
    error = function(e) NULL
  )
  testthat::skip_if(is.null(prod), "production pins unavailable")
  prod <- as.data.frame(prod)

  cw <- as.data.frame(whep::polity_area_crosswalk)
  coded <- cw[which(!is.na(cw$area_code)), ]
  by_area <- split(coded$region, coded$area_code)
  regionless <- as.integer(names(
    by_area[vapply(by_area, function(v) all(is.na(v)), logical(1))]
  ))
  testthat::expect_gt(length(regionless), 5L)

  livestock <- prod[which(!is.na(prod$live_anim_code)), ]
  # Non-vacuous: no livestock rows at all would make the assertion below meaningless.
  testthat::expect_gt(nrow(livestock), 1000L)

  stranded <- livestock[which(livestock$polity_area_code %in% regionless), ]
  testthat::expect_equal(
    nrow(stranded),
    0L,
    info = paste0(
      "livestock production on areas with no Bouwman region, whose feed demand the ",
      "mix drops: ",
      paste(
        utils::head(sort(unique(stranded$polity_area_code)), 10),
        collapse = ", "
      )
    )
  )
})
