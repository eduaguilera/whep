# .calculate_feed_domestic_share -----------------------------------------------

test_that(".calculate_feed_domestic_share returns one row per year-province", {
  # Two items (Wheat, Barley) both destined as Feed for the same year. Before
  # aggregating feed_summary over Item, pivot_wider() leaves one row per
  # Year x Item, and joining that against province-level LU shares by Year
  # alone fans every province out once per item.
  feed_df <- tibble::tribble(
    ~Year, ~Item, ~Element, ~Destiny, ~Value_destiny,
    2000, "Wheat", "Production", "Feed", 100,
    2000, "Wheat", "Export", "Feed", 20,
    2000, "Wheat", "Import", "Feed", 10,
    2000, "Barley", "Production", "Feed", 50,
    2000, "Barley", "Export", "Feed", 0,
    2000, "Barley", "Import", "Feed", 5
  )

  lu_df <- tibble::tribble(
    ~Year, ~Province_name, ~LU_total,
    2000, "A", 60,
    2000, "B", 40
  )

  codes_coefs_item <- tibble::tribble(
    ~item, ~Name_biomass,
    "Wheat", "Cereal",
    "Barley", "Cereal"
  )

  biomass_coefs <- tibble::tribble(
    ~Name_biomass, ~Product_kgDM_kgFM, ~Product_kgN_kgDM,
    "Cereal", 1, 1
  )

  out <- .calculate_feed_domestic_share(
    feed_df,
    lu_df,
    codes_coefs_item,
    biomass_coefs
  )

  # No fan-out: exactly one row per Year x Province_name.
  expect_equal(nrow(out), 2)
  expect_equal(nrow(dplyr::distinct(out, Year, Province_name)), 2)

  # National totals are summed across items (Production 150, Export 20,
  # Import 15) before being split by province LU share.
  expected_share <- (150 - 20) / ((150 - 20) + 15)
  expect_equal(
    out$local_feed_share,
    rep(expected_share, 2),
    tolerance = 1e-12
  )
})
