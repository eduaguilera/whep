.nbx_actual <- function(values, indicator = "surplus", year = 2015L) {
  out <- tibble::tibble(
    lon = 0.25,
    lat = 0.25,
    area_code = seq_along(values),
    item_cbs_code = 2500L + seq_along(values),
    year = year,
    area_ha = 50,
    surplus_n_t = values,
    n_input_std_t = values,
    production_n_t = abs(values)
  )
  if (indicator == "total_input") {
    out$surplus_n_t <- values - 1
  }
  out
}

.nbx_boundary <- function(
  critical_kgn_ha = 50,
  source_land_area_ha = 100,
  image_region = 11L,
  critical_value_present = TRUE
) {
  tibble::tibble(
    cell_id = 129961L,
    source_row = 180L,
    source_col = 361L,
    lon = 0.25,
    lat = 0.25,
    source_land_area_ha = source_land_area_ha,
    critical_kgn_ha = if (critical_value_present) critical_kgn_ha else NA_real_,
    image_region = image_region,
    indicator = "surplus",
    impact_scope = "mi",
    land_class = "ara",
    allocation_scenario = "yield_gap",
    critical_reference_year = 2010L,
    source_record = "6395016",
    source_version = "1.0",
    source_doi = "10.5281/zenodo.6395016",
    source_archive_md5 = "d6b4bf88e9b140bd25a147396e371733"
  )
}

.nbx_run <- function(
  values,
  critical_kgn_ha = 50,
  indicator = "surplus",
  resolution = "grid",
  boundary = NULL
) {
  if (is.null(boundary)) {
    boundary <- .nbx_boundary(critical_kgn_ha)
  }
  boundary$indicator <- indicator
  whep::build_n_boundary_exceedance(
    actual = .nbx_actual(values, indicator),
    boundary = boundary,
    indicator = indicator,
    land_class = "ara",
    impact_scope = "mi",
    allocation_scenario = "yield_gap",
    resolution = resolution,
    actual_year = 2015L,
    critical_reference_year = 2010L
  )
}

testthat::test_that("one cell allowance is consumed after crop aggregation", {
  out <- .nbx_run(c(4, 4), indicator = "total_input")
  testthat::expect_equal(unique(out$cell_actual_n_t), 8)
  testthat::expect_equal(unique(out$cell_critical_n_t), 5)
  testthat::expect_equal(unique(out$cell_positive_overshoot_n_t), 3)
  testthat::expect_equal(out$pressure_share, c(0.5, 0.5))
  testthat::expect_equal(out$crop_critical_n_t, c(2.5, 2.5))
  testthat::expect_equal(out$exceedance_n_t, c(1.5, 1.5))
  testthat::expect_equal(sum(out$crop_critical_n_t), 5)
  testthat::expect_equal(sum(out$exceedance_n_t), 3)
})

testthat::test_that("the old per-crop-full-allowance result is rejected", {
  out <- .nbx_run(c(4, 4), indicator = "total_input")
  old_per_crop_overshoot <- sum(pmax(c(4, 4) - 5, 0))
  testthat::expect_equal(old_per_crop_overshoot, 0)
  testthat::expect_equal(sum(out$exceedance_n_t), 3)
  testthat::expect_false(isTRUE(all.equal(
    sum(out$exceedance_n_t),
    old_per_crop_overshoot
  )))
})

testthat::test_that("signed surplus shares retain negative contributions", {
  out <- .nbx_run(c(8, -2))
  testthat::expect_equal(out$pressure_share, c(8 / 6, -2 / 6))
  testthat::expect_equal(out$crop_critical_n_t, c(20 / 3, -5 / 3))
  testthat::expect_equal(out$signed_margin_n_t, c(4 / 3, -1 / 3))
  testthat::expect_equal(out$exceedance_n_t, c(4 / 3, -1 / 3))
  testthat::expect_equal(sum(out$signed_margin_n_t), 1)
  testthat::expect_equal(sum(out$exceedance_n_t), 1)
})

testthat::test_that("near-zero signed surplus is preserved as a residual", {
  out <- .nbx_run(c(1, -0.999999999), critical_kgn_ha = -10)
  crops <- dplyr::filter(
    out,
    .data$attribution_record_type == "crop_allocation"
  )
  residual <- dplyr::filter(
    out,
    .data$attribution_record_type == "cell_residual"
  )
  testthat::expect_true(all(is.na(crops$pressure_share)))
  testthat::expect_true(all(
    crops$attribution_status == "undefined_near_zero_denominator"
  ))
  testthat::expect_equal(
    sum(crops$crop_critical_n_t) + residual$unallocated_critical_n_t,
    unique(out$cell_critical_n_t),
    tolerance = 1e-7
  )
  testthat::expect_equal(
    sum(crops$exceedance_n_t) + residual$unallocated_positive_overshoot_n_t,
    unique(out$cell_positive_overshoot_n_t),
    tolerance = 1e-7
  )
})

testthat::test_that("zero pressure with a zero allowance has a zero allocation", {
  out <- .nbx_run(c(1, -1), critical_kgn_ha = 0)
  crops <- dplyr::filter(
    out,
    .data$attribution_record_type == "crop_allocation"
  )
  residual <- dplyr::filter(
    out,
    .data$attribution_record_type == "cell_residual"
  )
  testthat::expect_true(all(is.na(crops$pressure_share)))
  testthat::expect_equal(crops$crop_critical_n_t, c(0, 0))
  testthat::expect_equal(crops$signed_margin_n_t, c(0, 0))
  testthat::expect_equal(residual$unallocated_signed_margin_n_t, 0)
  testthat::expect_true(all(
    out$attribution_state == "undefined_zero_denominator"
  ))
})

testthat::test_that("consequential zero denominators expose exact residuals", {
  surplus <- .nbx_run(c(1, -1), critical_kgn_ha = -20)
  total_input <- .nbx_run(
    c(0, 0),
    critical_kgn_ha = 50,
    indicator = "total_input"
  )
  surplus_residual <- dplyr::filter(
    surplus,
    .data$attribution_record_type == "cell_residual"
  )
  input_residual <- dplyr::filter(
    total_input,
    .data$attribution_record_type == "cell_residual"
  )
  testthat::expect_equal(surplus_residual$unallocated_critical_n_t, -2)
  testthat::expect_equal(surplus_residual$unallocated_signed_margin_n_t, 2)
  testthat::expect_equal(
    surplus_residual$unallocated_positive_overshoot_n_t,
    2
  )
  testthat::expect_equal(input_residual$unallocated_critical_n_t, 5)
  testthat::expect_equal(input_residual$unallocated_signed_margin_n_t, -5)
  testthat::expect_equal(input_residual$unallocated_positive_overshoot_n_t, 0)
})

testthat::test_that("negative critical values retain restoration overshoot", {
  out <- .nbx_run(c(3, 1), critical_kgn_ha = -20)
  testthat::expect_equal(unique(out$cell_critical_n_t), -2)
  testthat::expect_equal(unique(out$cell_signed_margin_n_t), 6)
  testthat::expect_equal(unique(out$cell_positive_overshoot_n_t), 6)
  testthat::expect_equal(sum(out$exceedance_n_t), 6)
})

testthat::test_that("missing actual and boundary-domain states remain distinct", {
  missing_actual <- .nbx_run(c(NA_real_, 1))
  testthat::expect_true(all(missing_actual$coverage_state == "missing_actual"))
  testthat::expect_true(all(is.na(missing_actual$exceedance_n_t)))

  out_of_domain <- .nbx_boundary(critical_value_present = FALSE)
  out_of_domain$critical_state <- "out_of_domain"
  missing_critical <- .nbx_boundary(critical_value_present = FALSE)
  missing_critical$critical_state <- "missing_critical"
  testthat::expect_true(all(
    .nbx_run(c(1, 1), boundary = out_of_domain)$coverage_state ==
      "out_of_domain"
  ))
  testthat::expect_true(all(
    .nbx_run(c(1, 1), boundary = missing_critical)$coverage_state ==
      "missing_critical"
  ))

  zero_land <- .nbx_boundary(source_land_area_ha = 0)
  testthat::expect_true(all(
    .nbx_run(c(1, 1), boundary = zero_land)$coverage_state == "zero_land"
  ))
})

testthat::test_that("IMAGE aggregation is keyed by source cell", {
  actual <- dplyr::bind_rows(
    .nbx_actual(c(4, 1)),
    dplyr::mutate(.nbx_actual(c(2, 3)), lon = 0.75)
  ) |>
    dplyr::mutate(area_code = 1L)
  boundary <- dplyr::bind_rows(
    .nbx_boundary(image_region = 11L),
    dplyr::mutate(
      .nbx_boundary(image_region = 20L),
      cell_id = 129962L,
      source_col = 362L,
      lon = 0.75
    )
  )
  out <- whep::build_n_boundary_exceedance(
    actual,
    boundary,
    resolution = "image_region",
    actual_year = 2015L,
    critical_reference_year = 2010L
  )
  testthat::expect_setequal(out$image_region, c(11L, 20L))
  testthat::expect_equal(sum(out$actual_n_t), 10)
})

testthat::test_that("fractional polity rows conserve the source-cell result", {
  actual <- .nbx_actual(c(3, 2)) |>
    dplyr::mutate(area_code = c(1L, 2L), item_cbs_code = 2511L)
  grid <- whep::build_n_boundary_exceedance(
    actual,
    .nbx_boundary(),
    actual_year = 2015L,
    critical_reference_year = 2010L
  )
  country <- whep::build_n_boundary_exceedance(
    actual,
    .nbx_boundary(),
    resolution = "country",
    actual_year = 2015L,
    critical_reference_year = 2010L
  )
  testthat::expect_equal(sum(grid$actual_n_t), 5)
  testthat::expect_equal(sum(country$actual_n_t), 5)
  testthat::expect_equal(sum(country$exceedance_n_t), 0)
})

testthat::test_that("fixed-reference provenance is explicit", {
  out <- .nbx_run(c(4, 4), indicator = "total_input")
  testthat::expect_true(all(out$actual_year == 2015L))
  testthat::expect_true(all(out$critical_reference_year == 2010L))
  testthat::expect_true(all(out$allocation_scenario == "yield_gap"))
  testthat::expect_true(all(out$indicator == "total_input"))
  testthat::expect_true(all(out$urban_treatment == "included_provisional"))
  testthat::expect_true(all(out$land_scope_status == "provisional"))
})

testthat::test_that("comparison years must be supplied explicitly", {
  testthat::expect_error(
    whep::build_n_boundary_exceedance(
      .nbx_actual(c(1, 1)),
      .nbx_boundary(),
      critical_reference_year = 2010L
    ),
    "actual_year.*explicit"
  )
  testthat::expect_error(
    whep::build_n_boundary_exceedance(
      .nbx_actual(c(1, 1)),
      .nbx_boundary(),
      actual_year = 2015L
    ),
    "critical_reference_year.*explicit"
  )
})

testthat::test_that("unsupported grid modes hard-error before calculation", {
  testthat::expect_error(
    whep::build_n_boundary_exceedance(
      actual = tibble::tibble(),
      boundary = tibble::tibble(),
      allocation_scenario = "no_increase"
    ),
    "no-increase|no_increase|upstream"
  )
  testthat::expect_error(
    whep::build_n_boundary_exceedance(
      actual = tibble::tibble(),
      boundary = tibble::tibble(),
      indicator = "new_fixation"
    ),
    "new.fixation|new_fixation|surface"
  )
})

testthat::test_that("off-grid coordinates and duplicate boundary keys abort", {
  off_grid <- dplyr::mutate(.nbx_actual(c(1, 1)), lon = 0.3)
  testthat::expect_error(
    whep::build_n_boundary_exceedance(
      off_grid,
      .nbx_boundary(),
      actual_year = 2015L,
      critical_reference_year = 2010L
    ),
    "0.5-degree|cell centre|aligned"
  )
  duplicated <- dplyr::bind_rows(.nbx_boundary(), .nbx_boundary())
  testthat::expect_error(
    whep::build_n_boundary_exceedance(
      .nbx_actual(c(1, 1)),
      duplicated,
      actual_year = 2015L,
      critical_reference_year = 2010L
    ),
    "unique|duplicate|cell"
  )
})

testthat::test_that("build_n_boundary_exceedance example uses the real path", {
  out <- whep::build_n_boundary_exceedance(example = TRUE)
  testthat::expect_s3_class(out, "tbl_df")
  pointblank::expect_col_exists(
    out,
    c(
      "cell_id",
      "item_cbs_code",
      "crop_critical_n_t",
      "signed_margin_n_t",
      "exceedance_n_t",
      "critical_reference_year"
    )
  )
})
