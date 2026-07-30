# USA (area_code 231) production: carcass tonnes per meat group plus the
# slaughtered head counts used to attribute each group to its live-animal
# sectors. iso3 USA resolves to GLEAM energy factors for every meat species.
.energy_prod_fixture <- function() {
  tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~unit, ~value,
    2000L, 231L, 2731L, "tonnes", 1e7,
    2000L, 231L, 961L, "slaughtered_heads", 3e7,
    2000L, 231L, 946L, "slaughtered_heads", 1e6,
    2000L, 231L, 2732L, "tonnes", 2e5,
    2000L, 231L, 976L, "slaughtered_heads", 5e6,
    2000L, 231L, 1016L, "slaughtered_heads", 1e6,
    2000L, 231L, 2733L, "tonnes", 7e6,
    2000L, 231L, 1049L, "slaughtered_heads", 9e7,
    2000L, 231L, 1051L, "slaughtered_heads", 1e7,
    2000L, 231L, 2734L, "tonnes", 1.5e7,
    2000L, 231L, 1053L, "slaughtered_heads", 8e9
  )
}

testthat::test_that("example has the expected structure", {
  result <- whep::build_energy_co2_extension(example = TRUE)

  pointblank::expect_col_exists(
    result,
    c("year", "area_code", "item_cbs_code", "impact_u", "method_energy")
  )
  pointblank::expect_col_vals_gt(result, "impact_u", 0)
  testthat::expect_true(all(result$method_energy == "GLEAM_3.0_energy_meat"))
})

testthat::test_that("output is keyed by the meat live-animal sectors", {
  result <- whep::build_energy_co2_extension(
    data = list(primary_prod = .energy_prod_fixture())
  )

  pointblank::expect_col_exists(
    result,
    c("year", "area_code", "item_cbs_code", "impact_u", "method_energy")
  )
  testthat::expect_setequal(
    result$item_cbs_code,
    c(961L, 946L, 976L, 1016L, 1049L, 1051L, 1053L)
  )
  testthat::expect_true(all(result$impact_u > 0))
  testthat::expect_false(any(is.na(result$impact_u)))
  testthat::expect_true(all(result$method_energy == "GLEAM_3.0_energy_meat"))
})

testthat::test_that("milk and egg sectors get no energy CO2 (meat only)", {
  prod <- .energy_prod_fixture() |>
    dplyr::bind_rows(
      tibble::tribble(
        ~year, ~area_code, ~item_cbs_code, ~unit, ~value,
        2000L, 231L, 960L, "slaughtered_heads", 5e6,
        2000L, 231L, 1052L, "slaughtered_heads", 1e8
      )
    )
  result <- whep::build_energy_co2_extension(
    data = list(primary_prod = prod)
  )

  testthat::expect_false(any(result$item_cbs_code %in% c(960L, 1052L)))
})

testthat::test_that("emissions scale linearly with carcass production", {
  base <- whep::build_energy_co2_extension(
    data = list(primary_prod = .energy_prod_fixture())
  )
  doubled <- .energy_prod_fixture() |>
    dplyr::mutate(
      value = dplyr::if_else(unit == "tonnes", value * 2, value)
    )
  result <- whep::build_energy_co2_extension(
    data = list(primary_prod = doubled)
  )

  joined <- dplyr::inner_join(
    base,
    result,
    by = c("year", "area_code", "item_cbs_code"),
    suffix = c("_base", "_double")
  )
  testthat::expect_equal(joined$impact_u_double, joined$impact_u_base * 2)
})

testthat::test_that("a group is split across its sectors by slaughtered heads", {
  result <- whep::build_energy_co2_extension(
    data = list(primary_prod = .energy_prod_fixture())
  )

  cattle <- result$impact_u[result$item_cbs_code == 961L]
  buffalo <- result$impact_u[result$item_cbs_code == 946L]
  # Same group factor and dressing, so the ratio is the head-count ratio (30:1).
  testthat::expect_equal(cattle / buffalo, 30)
})

testthat::test_that("only the gleam method is available", {
  testthat::expect_error(
    whep::build_energy_co2_extension(method = "fao"),
    "should be"
  )
})
