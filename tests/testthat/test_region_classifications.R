# test_region_classifications.R -- the regional grouping columns of
# regions_full: which ones WHEP consumes, and what each one's gaps are.
#
# Issue #386 counted 17 grouping columns and found 14 of them read by nothing
# outside the roxygen block that documents them. Re-measured on this commit over
# `R/`, `data-raw/`, `tests/`, `vignettes/` and `inst/`, the census has moved:
# six columns now have an in-tree consumer, not three.
#
#   region              data-raw/table_mappings.R -> polity_area_crosswalk
#   region_code         none of its own -- a 1:1 relabelling of `region`
#   region_krausmann    residue_destiny, soil_carbon_inputs, prepare_spatialize
#   region_HANPP        residue_destiny, soil_carbon_inputs
#   region_UN_sub       soil_carbon_inputs -> residue feed-use fraction (#405)
#   ADB_Region          build_production
#   EU27                eu_aggregate
#
# The remaining eleven -- region_krausmann2, region_UN, region_ILO1/2/3,
# region_IEA, region_IPCC, region_labour, region_labour_agg,
# region_labour_mech, Lassaletta -- are published taxonomies shipped for
# downstream use and read by nothing here. This file pins what a consumer would
# inherit if one were added, so that giving one of them a caller is an explicit
# decision rather than a silent one. `region_test` was dropped in #386: it was
# not a taxonomy, it was a two-valued scratch column.
#
# Package data only: no network, no WHEP_* path, no pipeline build.

test_that("regions_full ships no experimental region_test column", {
  # Zero in-tree consumers and only two values ("Europe", "Other"), so it was
  # a working column left in a published table, not a classification. Asserted
  # on both tables because polities_cats is derived from regions_full and would
  # have inherited it.
  testthat::expect_false("region_test" %in% names(whep::regions_full))
  testthat::expect_false("region_test" %in% names(whep::polities_cats))
})

test_that("the grouping columns are all still present", {
  # A schema pin, so removing one of the unconsumed taxonomies is a visible
  # edit to this list rather than a silent narrowing of a published dataset.
  groupings <- c(
    "EU27",
    "ADB_Region",
    "region",
    "region_code",
    "Lassaletta",
    "region_krausmann",
    "region_HANPP",
    "region_krausmann2",
    "region_UN_sub",
    "region_UN",
    "region_ILO1",
    "region_ILO2",
    "region_ILO3",
    "region_IEA",
    "region_IPCC",
    "region_labour",
    "region_labour_agg",
    "region_labour_mech"
  )
  testthat::expect_true(all(groupings %in% names(whep::regions_full)))
  testthat::expect_true(all(groupings %in% names(whep::polities_cats)))

  # None of them is degenerate: a column carrying one value or none is not a
  # grouping, and would be dead weight of a different kind than #386 measured.
  distinct_values <- vapply(
    whep::regions_full[groupings],
    function(column) dplyr::n_distinct(column[!is.na(column)]),
    integer(1)
  )
  testthat::expect_true(all(distinct_values >= 2L))
})

test_that("region_code is a 1:1 relabelling of region", {
  # This is why region_code needs no consumer of its own: it carries no
  # information `region` does not. Both directions, so neither a code serving
  # two names nor a name served by two codes can appear.
  pairs <- whep::regions_full |>
    dplyr::filter(!is.na(.data$region_code), !is.na(.data$region)) |>
    dplyr::distinct(.data$region_code, .data$region)

  testthat::expect_equal(
    dplyr::n_distinct(pairs$region_code),
    nrow(pairs)
  )
  testthat::expect_equal(
    dplyr::n_distinct(pairs$region),
    nrow(pairs)
  )
  # Non-vacuous: 17 named regions, not a handful.
  testthat::expect_gt(nrow(pairs), 15L)
})

test_that("the consumed groupings cover every CBS reporter", {
  # The two groupings a coefficient table is keyed by, plus `region`. A CBS
  # reporter missing one of these does not drop out, it takes a fallback -- so
  # completeness here is what keeps the fallback from being reached silently.
  reporters <- whep::regions_full |>
    dplyr::filter(.data$cbs %in% TRUE)
  testthat::expect_gt(nrow(reporters), 150L)

  for (column in c("region_krausmann", "region_HANPP")) {
    testthat::expect_equal(
      sum(is.na(reporters[[column]])),
      0L,
      info = column
    )
  }
  # `region` is complete apart from Rest of World, which spans continents by
  # construction and is not a region of the classification.
  testthat::expect_equal(
    reporters$code[is.na(reporters$region)],
    999
  )
})

test_that("region_UN_sub is missing exactly the dissolved federations", {
  # M49 assigns sub-regions to states that exist, so the four federations WHEP
  # still books commodity balances for have none. They are CBS reporters, and
  # region_UN_sub gained a consumer in #405 (the residue feed-use fraction), so
  # this gap is reached on every historical build and resolves to the "Global"
  # default. Pinned by identity rather than by count.
  reporters <- whep::regions_full |>
    dplyr::filter(.data$cbs %in% TRUE)

  testthat::expect_setequal(
    reporters$code[is.na(reporters$region_UN_sub)],
    # Czechoslovakia, Serbia and Montenegro, USSR, Yugoslav SFR.
    c(51, 186, 228, 248)
  )
})

test_that("the unconsumed present-day taxonomies share that same gap", {
  # The reason #386's "these columns are incomplete" reading is the wrong one:
  # the gap is not scattered, it is the dissolved states, identically in every
  # present-day taxonomy. So it is one decision for a future consumer, not a
  # per-column data-quality problem. region_UN is the odd one out -- it labels
  # three of the four -- and RoW carries a value rather than NA everywhere.
  reporters <- whep::regions_full |>
    dplyr::filter(.data$cbs %in% TRUE)
  federations <- c(51, 186, 228, 248)

  for (column in c(
    "region_ILO1",
    "region_ILO2",
    "region_ILO3",
    "region_IEA",
    "region_IPCC"
  )) {
    testthat::expect_setequal(
      reporters$code[is.na(reporters[[column]])],
      federations
    )
  }
  testthat::expect_equal(reporters$code[is.na(reporters$region_UN)], 51)

  row_labels <- reporters |>
    dplyr::filter(.data$code == 999) |>
    dplyr::select("region_UN", "region_ILO1", "region_IEA", "region_IPCC")
  testthat::expect_true(all(unlist(row_labels) == "RoW"))
})

test_that("the region_UN_sub gap costs nothing at today's coefficients", {
  # Why the gap above is a latent hazard rather than a live error: the feed-use
  # fraction of every European sub-region a successor of those federations sits
  # in equals the Global fallback the missing key resolves to, so filling the
  # gap with a European sub-region would move no published value. Should the
  # table ever price Eastern or Southern Europe away from Global, this fails and
  # the four federations need a decision instead of a default.
  feed <- whep::whep_coef_table("residue_feed_fraction")
  fractions <- feed$feed_use_fraction[
    match(
      c("Global", "Eastern Europe", "Southern Europe"),
      feed$region_un_sub
    )
  ]
  testthat::expect_false(anyNA(fractions))
  testthat::expect_equal(fractions[2], fractions[1])
  testthat::expect_equal(fractions[3], fractions[1])
})

test_that("the region_labour columns hold only their own vocabulary", {
  # Two rows used to carry a region name in a class column: Angola (7)
  # region_labour_mech "Middle Africa", and Northern Mariana Islands (163)
  # "Micronesia" in both region_labour_agg and region_labour_mech. The source
  # spreadsheet (General/input/Codes_coefs.xlsx, sheet regions_full) has the
  # same typed-in values, so the cells were repaired by hand (whep#855).
  # Asserted on polities_cats too, which inherits every value from
  # regions_full.
  mech_vocabulary <- c("mech", "no_mech", "RoW")
  agg_vocabulary <- c(
    "SAA",
    "LACA",
    "Europe",
    "AUS",
    "SE-Asia",
    "MENA",
    "FSU",
    "NAME",
    "RoW"
  )
  labels <- dplyr::bind_rows(whep::regions_full, whep::polities_cats)
  mech <- labels$region_labour_mech
  agg <- labels$region_labour_agg

  testthat::expect_true(all(mech[!is.na(mech)] %in% mech_vocabulary))
  testthat::expect_true(all(agg[!is.na(agg)] %in% agg_vocabulary))
})

test_that("the repaired region_labour cells follow their region", {
  # Each damaged cell held the UN M49 region of its row, as if the lookup key
  # had been pasted where its result belonged. They were filled with the class
  # every other member of that region carries: the other eight Middle Africa
  # countries are all no_mech, and the other Micronesia rows are all mech and,
  # except the US Minor Outlying Islands (NAME), all AUS (whep#855).
  repaired <- whep::regions_full |>
    dplyr::filter(.data$code %in% c(7, 163)) |>
    dplyr::arrange(.data$code)

  testthat::expect_equal(repaired$region_labour_mech, c("no_mech", "mech"))
  testthat::expect_equal(repaired$region_labour_agg, c("SAA", "AUS"))

  middle_africa <- whep::regions_full |>
    dplyr::filter(.data$region_labour == "Middle Africa") |>
    dplyr::pull(.data$region_labour_mech)
  testthat::expect_equal(unique(middle_africa), "no_mech")

  micronesia <- whep::regions_full |>
    dplyr::filter(.data$region_UN_sub == "Micronesia") |>
    dplyr::pull(.data$region_labour_mech)
  testthat::expect_equal(unique(micronesia), "mech")

  angola <- whep::polities_cats |>
    dplyr::filter(.data$code == 7) |>
    dplyr::pull(.data$region_labour_mech)
  testthat::expect_equal(angola, "no_mech")
})
