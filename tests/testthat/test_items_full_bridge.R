# Locks on the PACKAGED item bridge: whep::items_full is the only thing that
# decides which CBS item reaches a biomass coefficient, and a name that
# resolves to nothing drops the item's mass with no arithmetic error anywhere
# (whep#970). These read the shipped tables, not a fixture, so a harmonization
# edit has to come past them. None of them is a clean bill of health: they
# record what the shipped table violates today, so the violation stays visible
# and the list can only shrink.

testthat::test_that("only the three placeholder rows fail the biomass bridge", {
  # The bridge is exact apart from one value. Anything else appearing here is
  # a new unreachable item, which is why the assertion is on the whole set of
  # unresolved names rather than on the three codes alone.
  unresolved <- whep::items_full |>
    dplyr::filter(!is.na(.data$Name_biomass)) |>
    dplyr::filter(!.data$Name_biomass %in% whep::biomass_coefs$Name_biomass) |>
    dplyr::distinct(.data$Name_biomass) |>
    dplyr::pull(.data$Name_biomass)
  testthat::expect_equal(unresolved, "0")

  # Which items the placeholder costs, pinned by code and name. 2775 is the
  # only one that ever carries CBS feed mass; 4000 is denominated in work
  # hours (see .animal_draught_item_code()) and 2899 is a residual aggregate
  # tagged "non_feed", so those two have no biomass counterpart to find.
  placeholder <- whep::items_full |>
    dplyr::filter(.data$Name_biomass %in% "0") |>
    dplyr::arrange(.data$item_cbs_code)
  testthat::expect_equal(
    as.integer(placeholder$item_cbs_code),
    c(2775L, 2899L, 4000L)
  )
  testthat::expect_equal(
    placeholder$item_cbs,
    c("Aquatic Plants", "Miscellaneous", "Animal draught")
  )
})

testthat::test_that("the placeholder lock is not vacuous", {
  # whep::items_full is lazy-loaded package data, so it cannot be mocked: the
  # only way to show the expectation above is doing work is to watch a
  # deliberately wrong version of it fail against the same read.
  placeholder <- whep::items_full |>
    dplyr::filter(.data$Name_biomass %in% "0")
  testthat::expect_failure(
    testthat::expect_equal(
      sort(as.integer(placeholder$item_cbs_code)),
      c(2775L, 2899L)
    )
  )
  testthat::expect_failure(
    testthat::expect_equal(
      whep::items_full |>
        dplyr::filter(!is.na(.data$Name_biomass)) |>
        dplyr::filter(
          !.data$Name_biomass %in% whep::biomass_coefs$Name_biomass
        ) |>
        nrow(),
      0L
    )
  )
})

testthat::test_that("a missing Name_biomass cannot match a coefficient row", {
  # The 21 live-animal rows carry NA, and dplyr joins match NA to NA by
  # default. That is harmless only while biomass_coefs holds no NA and no
  # empty name; if one ever arrived, every live animal would silently inherit
  # its coefficients. Same for the "0" placeholder.
  names_bio <- whep::biomass_coefs$Name_biomass
  testthat::expect_false(any(is.na(names_bio)))
  testthat::expect_false(any(names_bio == ""))
  testthat::expect_false(any(names_bio == "0"))

  # The NA rows are exactly the live animals, so NA means "no biomass
  # counterpart" and not "not filled in".
  na_rows <- whep::items_full |>
    dplyr::filter(is.na(.data$Name_biomass))
  testthat::expect_equal(nrow(na_rows), 21L)
  testthat::expect_equal(unique(na_rows$group), "Livestock")
})

testthat::test_that("feed_taxonomy and items_full cover the same items", {
  # .build_feed_avail_national() joins both by item_cbs_code, so an item in
  # one and not the other loses either its density or its feed quality. They
  # agree exactly today; this pins that.
  codes_items <- as.integer(whep::items_full$item_cbs_code)
  codes_tax <- as.integer(whep::feed_taxonomy$item_cbs_code)
  testthat::expect_equal(setdiff(codes_tax, codes_items), integer(0))
  testthat::expect_equal(setdiff(codes_items, codes_tax), integer(0))

  # One row of each carries no code at all: Ethanol. Both joins are keyed on
  # item_cbs_code and dplyr matches NA to NA, so a CBS row that arrived
  # without a code would silently take Ethanol's classification. Nothing
  # produces such a row today; this pins that there is exactly one uncoded
  # item to collide with, not a growing set.
  uncoded <- whep::items_full |>
    dplyr::filter(is.na(.data$item_cbs_code))
  testthat::expect_equal(uncoded$item_cbs, "Ethanol")
})
