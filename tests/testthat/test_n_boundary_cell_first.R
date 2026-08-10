.cell_first_critical <- function(value = 50, area = 100, var = "critical_n_surplus") {
  tibble::tibble(
    lon = 0.25,
    lat = 0.25,
    value = value,
    source_area_ha = area,
    image_region = 9L,
    critical_var = var,
    critical_threshold = "mi",
    critical_land_use = "ara",
    critical_year = 2010L,
    critical_source = "Schulte-Uebbing et al. (2022)"
  )
}

.cell_first_surplus <- function(values = c(4, 4), year = 2015L) {
  tibble::tibble(
    lon = 0.25,
    lat = 0.25,
    area_code = c(1L, 1L),
    item_cbs_code = c(2511L, 2513L),
    year = year,
    area_ha = c(100, 100),
    n_input_std_t = values,
    surplus_n_t = values,
    surplus_kgn_ha = values * 10
  )
}

testthat::test_that("one source-cell allowance is consumed after crop aggregation", {
  out <- whep::build_n_boundary_exceedance(
    .cell_first_surplus(),
    .cell_first_critical(),
    resolution = "grid",
    actual_year = 2015L,
    critical_reference_year = 2010L
  )

  # Two 4 t crops face one 5 t source-cell allowance: the old per-crop
  # comparison incorrectly returned zero for both crops.
  testthat::expect_equal(unique(out$cell_actual_n_t), 8)
  testthat::expect_equal(unique(out$cell_critical_n_t), 5)
  testthat::expect_equal(unique(out$cell_signed_margin_n_t), 3)
  testthat::expect_equal(unique(out$cell_positive_overshoot_n_t), 3)
  testthat::expect_equal(sum(out$critical_n_t), 5)
  testthat::expect_equal(sum(out$signed_margin_n_t), 3)
  testthat::expect_equal(sum(out$positive_overshoot_n_t), 3)
  testthat::expect_equal(out$pressure_share, c(0.5, 0.5))
})

testthat::test_that("signed surplus shares conserve mixed-sign attribution", {
  out <- whep::build_n_boundary_exceedance(
    .cell_first_surplus(c(8, -2)),
    .cell_first_critical(),
    resolution = "grid",
    actual_year = 2015L,
    critical_reference_year = 2010L
  )

  testthat::expect_equal(out$pressure_share, c(4 / 3, -1 / 3))
  testthat::expect_equal(out$critical_n_t, c(20 / 3, -5 / 3))
  testthat::expect_equal(out$signed_margin_n_t, c(4 / 3, -1 / 3))
  testthat::expect_equal(out$positive_overshoot_n_t, c(4 / 3, -1 / 3))
  testthat::expect_equal(sum(out$positive_overshoot_n_t), 1)
  testthat::expect_equal(unique(out$cell_positive_overshoot_n_t), 1)
})

testthat::test_that("total input uses crop input rather than surplus shares", {
  actual <- .cell_first_surplus(c(8, -2)) |>
    dplyr::mutate(n_input_std_t = c(9, 3))
  out <- whep::build_n_boundary_exceedance(
    actual,
    .cell_first_critical(100, var = "critical_n_input"),
    metric = "input",
    resolution = "grid",
    actual_year = 2015L,
    critical_reference_year = 2010L
  )
  testthat::expect_equal(out$actual_n_t, c(9, 3))
  testthat::expect_equal(out$pressure_share, c(0.75, 0.25))
  testthat::expect_equal(sum(out$critical_n_t), 10)
  testthat::expect_equal(sum(out$signed_margin_n_t), 2)
})

testthat::test_that("cell result retains negative critical values", {
  out <- whep::build_n_boundary_exceedance(
    .cell_first_surplus(c(0, 0)),
    .cell_first_critical(-20),
    resolution = "cell",
    actual_year = 2015L,
    critical_reference_year = 2010L
  )
  testthat::expect_equal(out$cell_critical_n_t, -2)
  testthat::expect_equal(out$cell_signed_margin_n_t, 2)
  testthat::expect_equal(out$cell_positive_overshoot_n_t, 2)
  testthat::expect_equal(out$coverage_state, "valid")
})

testthat::test_that("undefined zero denominators become explicit residuals", {
  out <- whep::build_n_boundary_exceedance(
    .cell_first_surplus(c(2, -2)),
    .cell_first_critical(-20),
    resolution = "grid",
    actual_year = 2015L,
    critical_reference_year = 2010L
  )
  crops <- dplyr::filter(out, .data$attribution_record_type == "crop_allocation")
  residual <- dplyr::filter(out, .data$attribution_record_type == "cell_residual")
  testthat::expect_true(all(is.na(crops$pressure_share)))
  testthat::expect_equal(sum(crops$positive_overshoot_n_t), 0)
  testthat::expect_equal(residual$unallocated_positive_overshoot_n_t, 2)
  testthat::expect_equal(
    sum(crops$positive_overshoot_n_t) +
      residual$unallocated_positive_overshoot_n_t,
    unique(out$cell_positive_overshoot_n_t)
  )
})

testthat::test_that("near-cancelling surplus is residualized conservatively", {
  out <- whep::build_n_boundary_exceedance(
    .cell_first_surplus(c(1, -1 + 1e-12)),
    .cell_first_critical(0),
    resolution = "grid",
    actual_year = 2015L,
    critical_reference_year = 2010L
  )
  crops <- dplyr::filter(out, .data$attribution_record_type == "crop_allocation")
  residual <- dplyr::filter(out, .data$attribution_record_type == "cell_residual")
  testthat::expect_true(all(is.na(crops$pressure_share)))
  testthat::expect_lt(unique(out$pressure_condition_ratio), 1e-11)
  testthat::expect_equal(
    sum(crops$signed_margin_n_t) + residual$unallocated_signed_margin_n_t,
    unique(out$cell_signed_margin_n_t),
    tolerance = 1e-12
  )
})

testthat::test_that("coverage states distinguish missing actual and domain", {
  critical <- dplyr::bind_rows(
    .cell_first_critical(),
    dplyr::mutate(.cell_first_critical(), lon = 0.75)
  )
  actual <- dplyr::bind_rows(
    .cell_first_surplus(),
    dplyr::mutate(.cell_first_surplus(1), lon = 1.25, area_code = 2L)
  )
  out <- whep::build_n_boundary_exceedance(
    actual,
    critical,
    resolution = "cell",
    actual_year = 2015L,
    critical_reference_year = 2010L
  )
  testthat::expect_setequal(
    out$coverage_state,
    c("valid", "missing_actual", "out_of_domain")
  )
  testthat::expect_true(all(is.na(out$cell_positive_overshoot_n_t[
    out$coverage_state != "valid"
  ])))
})

testthat::test_that("IMAGE context and fractional polities join by cell key", {
  actual <- .cell_first_surplus(c(3, 2)) |>
    dplyr::mutate(area_code = c(1L, 2L))
  grid <- whep::build_n_boundary_exceedance(
    actual,
    .cell_first_critical(),
    resolution = "grid",
    actual_year = 2015L,
    critical_reference_year = 2010L
  )
  testthat::expect_true(all(grid$image_region == 9L))
  testthat::expect_equal(sum(grid$pressure_share), 1)
  polity <- whep::build_n_boundary_exceedance(
    actual,
    .cell_first_critical(),
    resolution = "country",
    actual_year = 2015L,
    critical_reference_year = 2010L
  )
  testthat::expect_equal(sum(polity$critical_n_t), 5)
})

testthat::test_that("unsupported source modes hard-error", {
  for (mode in c("no_increase", "new_fixation")) {
    testthat::expect_error(
      whep::build_n_boundary_exceedance(
        .cell_first_surplus(),
        .cell_first_critical(),
        allocation_scenario = mode,
        actual_year = 2015L,
        critical_reference_year = 2010L
      ),
      "unsupported|upstream"
    )
  }
})

testthat::test_that("fixed-reference provenance is explicit", {
  out <- whep::build_n_boundary_exceedance(
    .cell_first_surplus(),
    .cell_first_critical(),
    resolution = "cell",
    actual_year = 2015L,
    critical_reference_year = 2010L
  )
  testthat::expect_true(all(out$actual_year == 2015L))
  testthat::expect_true(all(out$critical_reference_year == 2010L))
  testthat::expect_true(all(out$allocation_scenario == "yield_gap"))
  testthat::expect_match(unique(out$provisional_reason), "urban|manure|grass")
})
