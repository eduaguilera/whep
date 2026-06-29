# A minimal land extension (year, area_code, item_cbs_code, impact_u in ha) for
# countries with known ISO3 and Chaudhary CFs: Brazil (21) and France (68),
# with a crop sector (wheat 2511) and a grass/pasture sector (3000).
.biodiv_land_fixture <- function() {
  tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~impact_u,
    2010L, 21L, 2511L, 1e6,
    2010L, 21L, 3000L, 2e6,
    2010L, 68L, 2511L, 1e6
  )
}

testthat::test_that("example has the expected structure", {
  result <- whep::build_biodiversity_extension(example = TRUE)

  pointblank::expect_col_exists(
    result,
    c("year", "area_code", "item_cbs_code", "impact_u", "method_biodiversity")
  )
  pointblank::expect_col_vals_gte(result, "impact_u", 0)
  testthat::expect_true(
    all(grepl("^chaudhary_brooks_2018", result$method_biodiversity))
  )
})

testthat::test_that("layers CFs on the land extension keyed by sector", {
  result <- whep::build_biodiversity_extension(
    data = list(land = .biodiv_land_fixture())
  )

  pointblank::expect_col_exists(
    result,
    c("year", "area_code", "item_cbs_code", "impact_u", "method_biodiversity")
  )
  testthat::expect_false(any(is.na(result$impact_u)))
  testthat::expect_true(all(result$impact_u >= 0))
  testthat::expect_true(
    all(
      result$method_biodiversity == "chaudhary_brooks_2018_occupation_intense"
    )
  )
  testthat::expect_equal(nrow(result), 3L)
})

testthat::test_that("impact_u is land area (m2) times the country CF", {
  result <- whep::build_biodiversity_extension(
    data = list(land = .biodiv_land_fixture())
  )
  cf_bra_crop <- whep::biodiversity_cf |>
    dplyr::filter(
      .data$iso3c == "BRA",
      .data$land_use_type == "crop",
      .data$intensity == "intense"
    ) |>
    dplyr::pull("cf_mean")

  got <- result$impact_u[
    result$area_code == 21L & result$item_cbs_code == 2511L
  ]
  testthat::expect_equal(got, 1e6 * 1e4 * cf_bra_crop)
})

testthat::test_that("grass sectors use the pasture CF, crops the crop CF", {
  result <- whep::build_biodiversity_extension(
    data = list(land = .biodiv_land_fixture())
  )
  cf_bra <- whep::biodiversity_cf |>
    dplyr::filter(.data$iso3c == "BRA", .data$intensity == "intense")
  crop_cf <- cf_bra$cf_mean[cf_bra$land_use_type == "crop"]
  past_cf <- cf_bra$cf_mean[cf_bra$land_use_type == "pasture"]

  crop_row <- result$impact_u[
    result$area_code == 21L & result$item_cbs_code == 2511L
  ]
  past_row <- result$impact_u[
    result$area_code == 21L & result$item_cbs_code == 3000L
  ]
  testthat::expect_equal(crop_row, 1e6 * 1e4 * crop_cf)
  testthat::expect_equal(past_row, 2e6 * 1e4 * past_cf)
})

testthat::test_that("intensity selects the matching CF (intense >= minimal)", {
  land <- .biodiv_land_fixture()
  hi <- whep::build_biodiversity_extension(
    intensity = "intense",
    data = list(land = land)
  )
  lo <- whep::build_biodiversity_extension(
    intensity = "minimal",
    data = list(land = land)
  )

  testthat::expect_true(all(grepl("intense$", hi$method_biodiversity)))
  testthat::expect_true(all(grepl("minimal$", lo$method_biodiversity)))
  joined <- dplyr::inner_join(
    hi,
    lo,
    by = c("year", "area_code", "item_cbs_code"),
    suffix = c("_hi", "_lo")
  )
  testthat::expect_true(all(joined$impact_u_hi >= joined$impact_u_lo))
})

testthat::test_that("impact scales linearly with land area", {
  land <- .biodiv_land_fixture()
  base <- whep::build_biodiversity_extension(data = list(land = land))
  doubled <- whep::build_biodiversity_extension(
    data = list(land = dplyr::mutate(land, impact_u = .data$impact_u * 2))
  )

  joined <- dplyr::inner_join(
    base,
    doubled,
    by = c("year", "area_code", "item_cbs_code"),
    suffix = c("_b", "_d")
  )
  testthat::expect_equal(joined$impact_u_d, joined$impact_u_b * 2)
})

testthat::test_that("areas without a CF are dropped (treated as zero)", {
  land <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~impact_u,
    2010L, 21L, 2511L, 1e6,
    2010L, 999999L, 2511L, 1e6
  )
  result <- whep::build_biodiversity_extension(data = list(land = land))

  testthat::expect_false(any(result$area_code == 999999L))
  testthat::expect_true(any(result$area_code == 21L))
})
