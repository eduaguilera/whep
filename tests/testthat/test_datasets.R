# test_datasets.R — tests for package data consistency

# -- default_destiny values ----------------------------------------------------

test_that("items_full has correct default_destiny for non-food items", {
  items <- whep::items_full

  fodder <- items |>
    dplyr::filter(item_cbs_code %in% c(2000, 2001, 2002, 2003))
  expect_true(all(fodder$default_destiny == "Feed", na.rm = TRUE))

  ethanol <- items |> dplyr::filter(item_cbs_code == 2659)
  expect_true(all(ethanol$default_destiny == "Other_uses", na.rm = TRUE))

  tobacco <- items |> dplyr::filter(item_cbs_code == 2671)
  expect_true(all(tobacco$default_destiny == "Other_uses", na.rm = TRUE))

  cotton <- items |> dplyr::filter(item_cbs_code == 2661)
  expect_true(all(cotton$default_destiny == "Other_uses", na.rm = TRUE))

  rubber <- items |> dplyr::filter(item_cbs_code == 2672)
  expect_true(all(rubber$default_destiny == "Other_uses", na.rm = TRUE))

  wool <- items |> dplyr::filter(item_cbs_code == 2746)
  expect_true(all(wool$default_destiny == "Other_uses", na.rm = TRUE))
})


# -- polity coverage -----------------------------------------------------------

test_that("polities includes promoted FAOSTAT-reporting countries", {
  pol <- whep::polities
  promoted <- c("BTN", "COM", "MHL", "FSM", "NRU", "SYC", "TON", "TUV")
  for (code in promoted) {
    expect_true(
      code %in% pol$iso3c,
      info = paste(code, "should be a standalone polity")
    )
  }
  expect_gte(nrow(pol), 200L)
})

test_that("regions_full maps promoted countries to own polity_code", {
  reg <- whep::regions_full

  btn <- reg |> dplyr::filter(iso3c == "BTN")
  expect_true(nrow(btn) > 0)
  expect_equal(btn$polity_code[1], "BTN")

  com <- reg |> dplyr::filter(iso3c == "COM")
  expect_true(nrow(com) > 0)
  expect_equal(com$polity_code[1], "COM")
})


# -- source_flags.csv consistency ----------------------------------------------

test_that("source_flags.csv covers all source labels used in code", {
  flags <- readr::read_csv(
    system.file("extdata", "source_flags.csv", package = "whep"),
    show_col_types = FALSE
  )

  required <- c(
    "FAOSTAT_prod",
    "EuropeAgriDB",
    "fill_linear",
    "imputed_yield",
    "imputed_cbs_ratio",
    "LUH2_cropland",
    "LUH2_agriland",
    "LUH2_grassland",
    "FAOSTAT_FBS_New",
    "FAOSTAT_FBS_Old",
    "FAOSTAT_FBS_Old_scaled",
    "FAOSTAT_CBS",
    "FAOSTAT_trade",
    "fishstat_trade"
  )

  for (src in required) {
    expect_true(
      src %in% flags$source,
      info = paste(src, "must be documented in source_flags.csv")
    )
  }
})
