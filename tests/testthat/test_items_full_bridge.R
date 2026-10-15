# Locks on the PACKAGED item bridge: whep::items_full is the only thing that
# decides which CBS item reaches a biomass coefficient, and a name that
# resolves to nothing drops the item's mass with no arithmetic error anywhere
# (whep#970). These read the shipped tables, not a fixture, so a harmonization
# edit has to come past them.

testthat::test_that("every non-missing Name_biomass resolves", {
  # The bridge is exact. Anything appearing here is an unreachable item whose
  # mass drops with no arithmetic error anywhere, which is why the assertion
  # is on the whole set of unresolved names rather than on chosen codes.
  unresolved <- whep::items_full |>
    dplyr::filter(!is.na(.data$Name_biomass)) |>
    dplyr::filter(!.data$Name_biomass %in% whep::biomass_coefs$Name_biomass) |>
    dplyr::distinct(.data$Name_biomass) |>
    dplyr::pull(.data$Name_biomass)
  testthat::expect_equal(unresolved, character(0))

  # The upstream "0" placeholder is gone (whep#970): 2775 Aquatic Plants
  # reaches its own coefficient row, and 4000 Animal draught and 2899
  # Miscellaneous carry NA, the table's spelling of "no biomass counterpart".
  testthat::expect_false(any(whep::items_full$Name_biomass %in% "0"))
  aquatic <- whep::items_full |>
    dplyr::filter(.data$item_cbs_code == 2775)
  testthat::expect_equal(aquatic$Name_biomass, "Aquatic Plants")
})

testthat::test_that("the bridge lock is not vacuous", {
  # whep::items_full is lazy-loaded package data, so it cannot be mocked: the
  # only way to show the expectation above is doing work is to watch it fail
  # on a deliberately broken copy of the same table.
  broken <- whep::items_full |>
    dplyr::mutate(
      Name_biomass = dplyr::if_else(
        .data$item_cbs_code %in% 2775,
        "0",
        .data$Name_biomass
      )
    )
  testthat::expect_failure(
    testthat::expect_equal(
      broken |>
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
  # The 22 live-animal rows carry NA, and dplyr joins match NA to NA by
  # default. That is harmless only while biomass_coefs holds no NA and no
  # empty name; if one ever arrived, every live animal would silently inherit
  # its coefficients.
  names_bio <- whep::biomass_coefs$Name_biomass
  testthat::expect_false(any(is.na(names_bio)))
  testthat::expect_false(any(names_bio == ""))
  testthat::expect_false(any(names_bio == "0"))

  # The NA rows are the live animals plus two items with no biomass at all:
  # 4000 Animal draught is denominated in work hours (see
  # .animal_draught_item_code()) and 2899 Miscellaneous is FAO's residual
  # aggregate, tagged "non_feed" (whep#970). So NA means "no biomass
  # counterpart" and not "not filled in".
  na_rows <- whep::items_full |>
    dplyr::filter(is.na(.data$Name_biomass))
  testthat::expect_equal(nrow(na_rows), 24L)
  testthat::expect_equal(
    sort(unique(na_rows$group)),
    c("Draught", "Livestock", "Primary crops")
  )
  testthat::expect_setequal(
    as.integer(na_rows$item_cbs_code[na_rows$group != "Livestock"]),
    c(2899L, 4000L)
  )
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
