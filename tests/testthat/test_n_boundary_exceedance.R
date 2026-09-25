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

# The land class decides which crop rows enter the cell aggregate at all, so it
# decides every number downstream of it. `"igl"` is new here and the old
# per-crop file's grass-scope guard did not survive the rewrite.
.nbx_run_scope <- function(land_class) {
  actual <- .nbx_actual(c(4, 4), indicator = "total_input")
  actual$item_cbs_code <- c(2501L, 3000L)
  boundary <- .nbx_boundary()
  boundary$indicator <- "total_input"
  boundary$land_class <- land_class
  whep::build_n_boundary_exceedance(
    actual = actual,
    boundary = boundary,
    indicator = "total_input",
    land_class = land_class,
    impact_scope = "mi",
    allocation_scenario = "yield_gap",
    resolution = "grid",
    actual_year = 2015L,
    critical_reference_year = 2010L,
    # This test is about which rows enter one cell aggregate; the grassland
    # split, the default under "all", is tested below on its own fixtures.
    grassland_split = "none"
  )
}

testthat::test_that("the land class selects which crop rows are compared", {
  arable <- .nbx_run_scope("ara")
  testthat::expect_equal(arable$item_cbs_code, 2501L)
  testthat::expect_equal(unique(arable$cell_actual_n_t), 4)

  grass <- .nbx_run_scope("igl")
  testthat::expect_equal(grass$item_cbs_code, 3000L)
  testthat::expect_equal(unique(grass$cell_actual_n_t), 4)

  # "all" is the sensitivity scope: it keeps both, so the one allowance is
  # spread over twice the pressure rather than being consumed twice.
  both <- .nbx_run_scope("all")
  testthat::expect_equal(sort(both$item_cbs_code), c(2501L, 3000L))
  testthat::expect_equal(unique(both$cell_actual_n_t), 8)
  testthat::expect_equal(sum(both$crop_critical_n_t), 5)
})

.nbx_run_no_crop <- function(values, indicator = "total_input") {
  actual <- .nbx_actual(values, indicator = indicator)
  actual$item_cbs_code <- c(2501L, NA_integer_)
  boundary <- .nbx_boundary()
  boundary$indicator <- indicator
  whep::build_n_boundary_exceedance(
    actual = actual,
    boundary = boundary,
    indicator = indicator,
    land_class = "ara",
    impact_scope = "mi",
    allocation_scenario = "yield_gap",
    resolution = "grid",
    actual_year = 2015L,
    critical_reference_year = 2010L
  )
}

testthat::test_that("a row naming no crop cannot enter the cell aggregate", {
  out <- suppressWarnings(.nbx_run_no_crop(c(4, 4)))
  testthat::expect_equal(out$item_cbs_code, 2501L)
  testthat::expect_equal(unique(out$cell_actual_n_t), 4)
})

testthat::test_that("crop-less pressure leaving is reported, not silent", {
  # The exclusion is defensible -- a row that names no crop cannot meet a
  # critical allowance. Letting an is.na() decide it without naming the mass
  # is not (#1173): 4 t of the cell's 8 t of pressure leaves here, and every
  # denominator below is computed without it.
  testthat::expect_warning(
    .nbx_run_no_crop(c(4, 4)),
    class = "whep_nbx_no_crop_mass"
  )
  testthat::expect_warning(.nbx_run_no_crop(c(4, 4)), "4 t N")
})

testthat::test_that("a zero-mass crop-less row is reported as no loss", {
  # The shape a gridded build_nitrogen_balance() actually produces: the row
  # exists (its output-term full join manufactured it) and carries nothing.
  # It is still named, so "nothing was dropped" is a statement rather than an
  # absence of one.
  testthat::expect_message(
    .nbx_run_no_crop(c(4, 0)),
    class = "whep_nbx_no_crop_dropped"
  )
  testthat::expect_no_warning(suppressMessages(.nbx_run_no_crop(c(4, 0))))
  out <- suppressMessages(.nbx_run_no_crop(c(4, 0)))
  testthat::expect_equal(out$item_cbs_code, 2501L)
})

testthat::test_that("an all-crop actual pressure reports nothing", {
  testthat::expect_no_message(.nbx_run(c(4, 4)))
  testthat::expect_no_warning(.nbx_run(c(4, 4)))
})

# Backs the NEWS claim about which way the numbers moved. For a non-negative
# pressure the cell form can only report at least as much overshoot as the old
# per-crop form, because the old one subtracted a fresh copy of the allowance
# for every crop. Asserted as an invariant over a sweep rather than on one
# hand-picked cell.
testthat::test_that("cell-first input overshoot never falls below per-crop", {
  cases <- list(
    list(values = c(4, 4), critical = 50),
    list(values = c(9, 1), critical = 50),
    list(values = c(3, 3, 3), critical = 50),
    list(values = c(0, 0), critical = 50),
    list(values = c(20, 1), critical = 10),
    list(values = c(1, 1), critical = 200)
  )
  margins <- purrr::map_dbl(cases, function(case) {
    out <- .nbx_run(
      case$values,
      critical_kgn_ha = case$critical,
      indicator = "total_input"
    )
    allowance <- unique(out$cell_critical_n_t)
    old <- sum(pmax(case$values - allowance, 0))
    sum(out$exceedance_n_t) - old
  })
  testthat::expect_true(all(margins >= -1e-9))
  # And it is a real difference somewhere, not a vacuous inequality.
  testthat::expect_true(any(margins > 1e-9))
})

# ---- Intensive/extensive grassland split (issue #1285) ---------------------
#
# Fixtures: helper_nbx_grassland_split.R (cells A-G, described there).
# Goldens: helper_nbx_grassland_golden.R, captured from the code before the
# split existed. Expected split values below are derived by hand from the
# fixture numbers, in t N unless stated.

.gs_split <- function(resolution = "cell", metric = "surplus", ...) {
  suppressMessages(.gs_run(
    resolution,
    metric = metric,
    grassland = .gs_grassland(metric = metric),
    ...
  ))
}

.gs_cell <- function(out, cell) {
  dplyr::filter(out, .data$cell_id == .gs_cell_id(cell))
}

# `out` must equal the golden on every golden column, and carry only the
# split's columns besides, empty and stamped "none".
.gs_expect_golden <- function(out, golden, grain = c("cell", "grid", "agg")) {
  grain <- match.arg(grain)
  testthat::expect_identical(
    tibble::as_tibble(out[names(golden)]),
    golden
  )
  stamps <- c("method_grassland_split", "grassland_split")
  expected_extra <- switch(
    grain,
    cell = c(whep:::.nbx_split_output_cols(), "grassland_split"),
    grid = c(
      whep:::.nbx_split_output_cols(),
      "grassland_split",
      "boundary_component"
    ),
    agg = stamps
  )
  extra <- setdiff(names(out), names(golden))
  testthat::expect_setequal(extra, expected_extra)
  testthat::expect_true(all(out$method_grassland_split == "none"))
  testthat::expect_true(all(out$grassland_split == "none"))
  empty <- setdiff(extra, stamps)
  testthat::expect_true(all(is.na(as.matrix(out[empty]))))
}

.gs_none <- function(resolution, ...) {
  .gs_run(resolution, grassland_split = "none", ...)
}

testthat::test_that("grassland_split = 'none' reproduces the unsplit output", {
  .gs_expect_golden(.gs_none("cell"), .gs_golden_cell_all_surplus(), "cell")
  .gs_expect_golden(.gs_none("grid"), .gs_golden_grid_all_surplus(), "grid")
  .gs_expect_golden(
    .gs_none("country"),
    .gs_golden_country_all_surplus(),
    "agg"
  )
  .gs_expect_golden(
    .gs_none("grid", metric = "input"),
    .gs_golden_grid_all_input(),
    "grid"
  )
  # The inputs of the split are ignored when it is switched off.
  .gs_expect_golden(
    .gs_none("grid", grassland = .gs_grassland()),
    .gs_golden_grid_all_surplus(),
    "grid"
  )
})

testthat::test_that("the split leaves 'ara' and 'igl' exactly unchanged", {
  # Default grassland_split, no grassland inputs: the split is not applied.
  .gs_expect_golden(
    .gs_run("grid", land_use = "ara"),
    .gs_golden_grid_ara_surplus(),
    "grid"
  )
  .gs_expect_golden(
    .gs_run("cell", land_use = "igl", grassland = .gs_grassland()),
    .gs_golden_cell_igl_surplus(),
    "cell"
  )
})

testthat::test_that("the split conserves each cell's actual pressure", {
  split <- .gs_split("cell")
  none <- .gs_none("cell")
  both <- dplyr::inner_join(
    dplyr::select(
      split,
      "cell_id",
      "managed_actual_n_t",
      "extensive_actual_n_t"
    ),
    dplyr::select(none, "cell_id", "cell_actual_n_t"),
    by = "cell_id"
  )
  testthat::expect_equal(nrow(both), nrow(none))
  testthat::expect_equal(
    both$managed_actual_n_t + both$extensive_actual_n_t,
    both$cell_actual_n_t
  )
  testthat::expect_equal(sum(both$cell_actual_n_t), 60)
})

testthat::test_that("extensive headroom never nets against managed excess", {
  b <- .gs_cell(.gs_split("cell"), "B")
  # Managed: 15 t against 100 ha x 50 kg/ha = 5 t, an excess of 10.
  # Extensive: 2 t against 200 ha x 30 kg/ha = 6 t, a headroom of 4.
  testthat::expect_equal(b$managed_positive_overshoot_n_t, 10)
  testthat::expect_equal(b$extensive_critical_n_t - b$extensive_actual_n_t, 4)
  testthat::expect_equal(b$extensive_positive_overshoot_n_t, 0)
  testthat::expect_equal(b$cell_positive_overshoot_n_t, 10)
  # Netting would have reported 17 - 11 = 6.
  testthat::expect_equal(b$cell_signed_margin_n_t, 6)
})

testthat::test_that("a demoted cell moves grassland to the extensive side", {
  d <- .gs_cell(.gs_split("cell"), "D")
  none <- .gs_cell(.gs_none("cell"), "D")
  testthat::expect_equal(d$grassland_class, "extensive")
  testthat::expect_equal(d$managed_actual_n_t, 3)
  testthat::expect_equal(d$extensive_actual_n_t, 5)
  # The managed area shrinks by the cell's IMAGE grassland (100 ha).
  testthat::expect_equal(d$managed_area_ha, none$source_area_ha - 100)
  # What remains managed is cropland, at the cell's own ara rate.
  testthat::expect_equal(d$managed_critical_kgn_ha, 70)
  testthat::expect_equal(d$method_allowance_managed, "archive")
  testthat::expect_equal(d$managed_critical_n_t, 70 * 50 / 1000)
  testthat::expect_equal(none$cell_critical_n_t, 40 * 150 / 1000)
  # No archive extensive rate: C's 12 kg/ha, the nearest in country 1.
  testthat::expect_equal(d$extensive_critical_kgn_ha, 12)
  testthat::expect_equal(d$method_allowance_extensive, "nearest_country")
  testthat::expect_equal(d$extensive_critical_n_t, 1.2)
  testthat::expect_equal(d$managed_positive_overshoot_n_t, 0)
  testthat::expect_equal(d$cell_positive_overshoot_n_t, 3.8)
})

testthat::test_that("a promoted extensive-only cell borrows a managed rate", {
  c_cell <- .gs_cell(.gs_split("cell"), "C")
  none <- .gs_cell(.gs_none("cell"), "C")
  testthat::expect_equal(none$coverage_state, "out_of_domain")
  testthat::expect_equal(c_cell$coverage_state, "valid")
  testthat::expect_equal(c_cell$grassland_class, "intensive")
  testthat::expect_equal(c_cell$method_allowance_managed, "nearest_country")
  # A's igl rate: see the fixture header for why B cannot lend and A beats D.
  testthat::expect_equal(c_cell$managed_critical_kgn_ha, 80)
  testthat::expect_equal(c_cell$managed_area_ha, 80)
  testthat::expect_equal(c_cell$managed_critical_n_t, 80 * 80 / 1000)
  testthat::expect_equal(c_cell$cell_positive_overshoot_n_t, 0)
  testthat::expect_equal(c_cell$method_allowance_extensive, "no_area")
})

testthat::test_that("managed rates are only lent by IMAGE-intensive cells", {
  # Take D out of country 1. B, IMAGE-extensive with cropland, is 0.5 degrees
  # from C and has a finite managed (all and ara) rate of 50, but that rate
  # describes cropland; C's promoted grassland needs an igl rate, and the
  # nearest cell with one is A, 1.0 degree away at 80.
  classes <- .gs_classes() |>
    dplyr::mutate(
      country_2010 = dplyr::if_else(
        .data$cell_id == .gs_cell_id("D"),
        3L,
        .data$country_2010
      )
    )
  out <- suppressMessages(
    .gs_run("cell", grassland = .gs_grassland(classes = classes))
  )
  lon <- .gs_lon()
  testthat::expect_lt(
    abs(lon[["B"]] - lon[["C"]]),
    abs(lon[["A"]] - lon[["C"]])
  )
  c_cell <- .gs_cell(out, "C")
  testthat::expect_equal(c_cell$managed_critical_kgn_ha, 80)
  testthat::expect_equal(c_cell$method_allowance_managed, "nearest_country")
  # Every cell with its own archive rate keeps it for its own managed area,
  # whatever its class: B (extensive, with cropland) and D (demoted).
  b <- .gs_cell(out, "B")
  d <- .gs_cell(out, "D")
  testthat::expect_equal(b$managed_critical_kgn_ha, 50)
  testthat::expect_equal(b$method_allowance_managed, "archive")
  testthat::expect_equal(d$managed_critical_kgn_ha, 70)
  testthat::expect_equal(d$method_allowance_managed, "archive")
})

testthat::test_that("WHEP grassland without IMAGE grassland uses WHEP's area", {
  e <- .gs_cell(.gs_split("cell"), "E")
  testthat::expect_equal(e$method_grassland_split, "no_image_grassland")
  testthat::expect_equal(e$extensive_area_ha, 20)
  testthat::expect_equal(
    e$method_allowance_extensive,
    "nearest_country_whep_area"
  )
  testthat::expect_equal(e$extensive_critical_n_t, 12 * 20 / 1000)
  testthat::expect_equal(e$extensive_positive_overshoot_n_t, 1 - 0.24)
  testthat::expect_equal(e$managed_area_ha, 100)
})

testthat::test_that("a cell outside the class table is compared as cropland", {
  g <- .gs_cell(.gs_split("cell"), "G")
  testthat::expect_equal(g$method_grassland_split, "no_grassland")
  testthat::expect_true(is.na(g$grassland_class))
  testthat::expect_equal(g$managed_area_ha, 100)
  testthat::expect_equal(g$extensive_coverage_state, "empty")
  testthat::expect_equal(g$cell_positive_overshoot_n_t, 2)
})

testthat::test_that("pressure on a component with no area is excluded", {
  # F: WHEP books 2 t of crop pressure where IMAGE has no cropland and the
  # cell is extensive. It cannot meet an allowance, so it is left out of the
  # managed comparison and reported -- not charged as 2 t of overshoot.
  testthat::expect_message(
    .gs_run("cell", grassland = .gs_grassland()),
    class = "whep_nbx_zero_land_component"
  )
  testthat::expect_message(
    .gs_run("cell", grassland = .gs_grassland()),
    "managed 2 t N in 1 cell"
  )
  f <- .gs_cell(.gs_split("cell"), "F")
  testthat::expect_equal(f$managed_coverage_state, "zero_land")
  testthat::expect_equal(f$excluded_actual_n_t, 2)
  testthat::expect_true(is.na(f$managed_positive_overshoot_n_t))
  testthat::expect_equal(f$coverage_state, "valid")
  testthat::expect_equal(f$cell_actual_n_t, 1)
  testthat::expect_equal(f$cell_positive_overshoot_n_t, 1 - 0.6)
  grid <- .gs_split("grid")
  f_rows <- dplyr::filter(grid, .data$cell_id == .gs_cell_id("F"))
  testthat::expect_equal(f_rows$item_cbs_code, 3000L)
  # A 3, B 10, C 0, D 3.8, E 0.76, F 0.4, G 2; F's 2 t is not in it.
  testthat::expect_equal(sum(grid$positive_overshoot_n_t), 19.96)
})

testthat::test_that("a component with no rate after transfer is reported", {
  # F loses its own extensive rate and has no country or region donor.
  f_id <- .gs_cell_id("F")
  classes <- .gs_classes() |>
    dplyr::mutate(
      image_region = dplyr::if_else(
        .data$cell_id == f_id,
        NA_integer_,
        .data$image_region
      )
    )
  budget <- .gs_budget() |>
    dplyr::mutate(
      ext_surplus_kgn_ha = dplyr::if_else(
        .data$cell_id == f_id,
        NA_real_,
        .data$ext_surplus_kgn_ha
      )
    )
  grassland <- .gs_grassland(classes, budget)
  # F's crop pressure on zero managed area is reported too, as its own class.
  testthat::expect_message(
    testthat::expect_message(
      .gs_run("cell", grassland = grassland),
      class = "whep_nbx_zero_land_component"
    ),
    class = "whep_nbx_missing_critical_component"
  )
  f <- .gs_cell(suppressMessages(.gs_run("cell", grassland = grassland)), "F")
  testthat::expect_equal(f$extensive_coverage_state, "missing_critical")
  testthat::expect_equal(f$method_allowance_extensive, "none")
  testthat::expect_equal(f$coverage_state, "missing_critical")
  testthat::expect_true(is.na(f$cell_positive_overshoot_n_t))
})

testthat::test_that("crop attribution reconciles per component and cell", {
  cells <- .gs_split("cell") |>
    dplyr::filter(.data$coverage_state == "valid")
  grid <- .gs_split("grid")
  by_cell <- dplyr::summarise(
    grid,
    actual = sum(.data$actual_n_t),
    critical = sum(.data$critical_n_t + .data$unallocated_critical_n_t),
    overshoot = sum(
      .data$positive_overshoot_n_t + .data$unallocated_positive_overshoot_n_t
    ),
    .by = "cell_id"
  ) |>
    dplyr::inner_join(cells, by = "cell_id")
  testthat::expect_equal(nrow(by_cell), nrow(cells))
  testthat::expect_equal(by_cell$actual, by_cell$cell_actual_n_t)
  testthat::expect_equal(by_cell$critical, by_cell$cell_critical_n_t)
  testthat::expect_equal(
    by_cell$overshoot,
    by_cell$cell_positive_overshoot_n_t
  )

  by_component <- grid |>
    dplyr::summarise(
      overshoot = sum(.data$positive_overshoot_n_t),
      target = dplyr::if_else(
        dplyr::first(.data$boundary_component) == "managed",
        dplyr::first(.data$managed_positive_overshoot_n_t),
        dplyr::first(.data$extensive_positive_overshoot_n_t)
      ),
      .by = c("cell_id", "boundary_component")
    )
  testthat::expect_equal(by_component$overshoot, by_component$target)
  a <- dplyr::filter(grid, .data$cell_id == .gs_cell_id("A"))
  testthat::expect_equal(a$pressure_share, c(8, 1, 3) / 12)

  country <- .gs_split("country")
  testthat::expect_equal(sum(country$actual_n_t), sum(cells$cell_actual_n_t))
  testthat::expect_equal(
    sum(country$positive_overshoot_n_t) +
      sum(country$unallocated_positive_overshoot_n_t),
    sum(cells$cell_positive_overshoot_n_t)
  )
  area_2 <- dplyr::filter(country, .data$area_code == 2L)
  testthat::expect_equal(sort(area_2$item_cbs_code), c(2513L, 3000L))
  testthat::expect_equal(area_2$actual_n_t, c(1, 1))
})

testthat::test_that("the component check catches what the cell check cannot", {
  # One cell, one managed and one extensive row. 0.5 t of allowance has moved
  # from the extensive row to the managed row: the cell still sums to 2 t, so
  # only the per-component identity can see it.
  moved <- tibble::tibble(
    cell_id = 1L,
    year = 2015L,
    boundary_component = c("managed", "extensive"),
    actual_n_t = c(1, 1),
    critical_n_t = c(1.5, 0.5),
    signed_margin_n_t = c(-0.5, 0.5),
    positive_overshoot_n_t = c(0, 0),
    unallocated_critical_n_t = 0,
    unallocated_signed_margin_n_t = 0,
    unallocated_positive_overshoot_n_t = 0,
    cell_actual_n_t = 2,
    cell_critical_n_t = 2,
    cell_signed_margin_n_t = 0,
    cell_positive_overshoot_n_t = 0,
    unit_actual_n_t = 1,
    unit_critical_n_t = 1,
    unit_signed_margin_n_t = 0,
    unit_positive_overshoot_n_t = 0
  )
  testthat::expect_no_error(
    whep:::.nbx_assert_reconciled_by(
      moved,
      c("cell_id", "year"),
      "cell",
      1e-10
    )
  )
  testthat::expect_error(
    whep:::.nbx_assert_reconciliation(moved),
    class = "whep_nbx_reconciliation"
  )
})

testthat::test_that("metric = 'input' uses the extensive input budget", {
  out <- .gs_split("cell", metric = "input")
  b <- .gs_cell(out, "B")
  d <- .gs_cell(out, "D")
  testthat::expect_equal(b$extensive_critical_kgn_ha, 45)
  testthat::expect_equal(b$extensive_critical_n_t, 45 * 200 / 1000)
  # D borrows C's input rate, 20 kg/ha.
  testthat::expect_equal(d$extensive_critical_n_t, 20 * 100 / 1000)
})

testthat::test_that("the class of the actual year is used", {
  out <- suppressMessages(whep::build_n_boundary_exceedance(
    surplus = .gs_surplus(2010L),
    critical = .gs_critical(),
    land_use = "all",
    resolution = "cell",
    actual_year = 2010L,
    critical_reference_year = 2010L,
    grassland = .gs_grassland()
  ))
  d <- .gs_cell(out, "D")
  testthat::expect_equal(d$grassland_class, "intensive")
  testthat::expect_equal(d$extensive_actual_n_t, 0)
  # 2010 is the IMAGE map: D's managed area is the deposited source area,
  # and its ara + igl allowance is the all-scope one, (70*50 + 25*100)/1000.
  testthat::expect_equal(d$managed_area_ha, 150)
  testthat::expect_equal(d$managed_critical_n_t, 6)
  none <- whep::build_n_boundary_exceedance(
    surplus = .gs_surplus(2010L),
    critical = .gs_critical(),
    land_use = "all",
    resolution = "cell",
    actual_year = 2010L,
    critical_reference_year = 2010L,
    grassland_split = "none"
  )
  both <- dplyr::inner_join(
    dplyr::select(out, "cell_id", "managed_critical_n_t"),
    dplyr::select(none, "cell_id", "cell_critical_n_t"),
    by = "cell_id"
  ) |>
    dplyr::filter(!is.na(.data$cell_critical_n_t))
  testthat::expect_equal(both$managed_critical_n_t, both$cell_critical_n_t)
})

testthat::test_that("ara plus igl reproduces the all-scope allowance", {
  # 10 ha of cropland at ara 50 plus 5 ha of intensive grassland at igl 80 is
  # 0.5 + 0.4 = 0.9 t; the all-scope rate, 60 over 15 ha, is also 0.9 t.
  a_id <- .gs_cell_id("A")
  surplus <- dplyr::filter(.gs_surplus(2010L), .data$lon == 0.25)
  critical <- .gs_critical() |>
    dplyr::filter(.data$lon == 0.25) |>
    dplyr::mutate(source_area_ha = 15)
  classes <- .gs_classes() |>
    dplyr::filter(.data$cell_id == a_id) |>
    dplyr::mutate(a_crop_ha = 10, grass_ha_image = 5, whep_grass_ha = 5)
  grassland <- .gs_grassland(classes = classes)
  grassland$critical_ara <- dplyr::filter(
    grassland$critical_ara,
    .data$lon == 0.25
  )
  grassland$critical_igl <- dplyr::filter(
    grassland$critical_igl,
    .data$lon == 0.25
  )
  run <- \(split) {
    whep::build_n_boundary_exceedance(
      surplus = surplus,
      critical = critical,
      land_use = "all",
      resolution = "cell",
      actual_year = 2010L,
      critical_reference_year = 2010L,
      grassland_split = split,
      grassland = grassland
    )
  }
  split <- run("image_density")
  testthat::expect_equal(split$managed_critical_n_t, 0.9)
  testthat::expect_equal(split$managed_critical_kgn_ha, 60)
  testthat::expect_equal(run("none")$cell_critical_n_t, 0.9)
})

testthat::test_that("the split never falls back when its inputs are absent", {
  testthat::expect_error(
    .gs_run("cell"),
    class = "whep_nbx_grassland_missing"
  )
  testthat::expect_error(
    .gs_run("cell", grassland = list(classes = .gs_classes())),
    class = "whep_nbx_grassland_missing"
  )
  testthat::expect_error(
    .gs_run("cell", grassland = list(extensive_budget = .gs_budget())),
    class = "whep_nbx_grassland_missing"
  )
  no_igl <- .gs_grassland()
  no_igl$critical_igl <- NULL
  testthat::expect_error(
    .gs_run("cell", grassland = no_igl),
    class = "whep_nbx_grassland_missing"
  )
  testthat::expect_error(
    .gs_run("cell", metric = "new_fixation", grassland = .gs_grassland()),
    class = "whep_nbx_grassland_metric"
  )
})

testthat::test_that("the ara and igl layers must match the compared surface", {
  swapped <- .gs_grassland()
  swapped$critical_ara <- .gs_layer("igl")
  testthat::expect_error(
    .gs_run("cell", grassland = swapped),
    class = "whep_nbx_grassland_bad_layer"
  )
  other_threshold <- .gs_grassland()
  other_threshold$critical_igl$critical_threshold <- "de"
  testthat::expect_error(
    .gs_run("cell", grassland = other_threshold),
    class = "whep_nbx_grassland_bad_layer"
  )
  # Surplus layers against an input comparison.
  testthat::expect_error(
    .gs_run("cell", metric = "input", grassland = .gs_grassland()),
    class = "whep_nbx_grassland_bad_layer"
  )
})

testthat::test_that("the split refuses inputs it cannot interpret", {
  # Grassland pressure in a cell the class table does not carry.
  no_e <- dplyr::filter(.gs_classes(), .data$cell_id != .gs_cell_id("E"))
  testthat::expect_error(
    .gs_run("cell", grassland = .gs_grassland(classes = no_e)),
    class = "whep_nbx_grassland_uncovered"
  )
  # A class table built from another map than the critical surface.
  shifted <- dplyr::mutate(.gs_classes(), a_crop_ha = .data$a_crop_ha + 1)
  testthat::expect_error(
    .gs_run("cell", grassland = .gs_grassland(classes = shifted)),
    class = "whep_nbx_grassland_area_mismatch"
  )
  bad_class <- dplyr::mutate(.gs_classes(), grassland_class = "pastoral")
  testthat::expect_error(
    .gs_run("cell", grassland = .gs_grassland(classes = bad_class)),
    class = "whep_nbx_grassland_bad_classes"
  )
  testthat::expect_error(
    .gs_run("cell", grassland = .gs_grassland(classes = .gs_classes(2020L))),
    class = "whep_nbx_grassland_uncovered"
  )
})

testthat::test_that("an all-missing extensive budget is an absent input", {
  vacuous <- dplyr::mutate(.gs_budget(), ext_surplus_kgn_ha = NA_real_)
  expect_supplied_guard(
    identity = all(is.na(vacuous$ext_surplus_kgn_ha)) &&
      nrow(vacuous) == nrow(.gs_budget()),
    guard = .gs_run("cell", grassland = .gs_grassland(budget = vacuous))
  )
})

# A compared component with allowance area and no pressure row still owns its
# allowance: an IMAGE-extensive cell with no WHEP grassland that year, or IMAGE
# cropland where WHEP books only grassland. Every crop-level grain must carry
# that allowance, so the totals there equal the cell totals.
.gs_expect_rowless_carried <- function(surplus, component) {
  run <- \(resolution) {
    suppressMessages(whep::build_n_boundary_exceedance(
      surplus = surplus,
      critical = .gs_critical(),
      land_use = "all",
      resolution = resolution,
      actual_year = 2015L,
      critical_reference_year = 2010L,
      grassland = .gs_grassland()
    ))
  }
  cells <- dplyr::filter(run("cell"), .data$coverage_state == "valid")
  b <- .gs_cell(cells, "B")
  testthat::expect_equal(b[[paste0(component, "_actual_n_t")]], 0)
  testthat::expect_gt(b[[paste0(component, "_critical_n_t")]], 0)
  purrr::walk(c("grid", "country", "image_region"), \(resolution) {
    out <- run(resolution)
    testthat::expect_equal(
      sum(out$actual_n_t),
      sum(cells$cell_actual_n_t),
      label = paste(resolution, "actual")
    )
    testthat::expect_equal(
      sum(out$critical_n_t) + sum(out$unallocated_critical_n_t),
      sum(cells$cell_critical_n_t),
      label = paste(resolution, "critical")
    )
    testthat::expect_equal(
      sum(out$signed_margin_n_t) + sum(out$unallocated_signed_margin_n_t),
      sum(cells$cell_signed_margin_n_t),
      label = paste(resolution, "margin")
    )
    testthat::expect_equal(
      sum(out$positive_overshoot_n_t) +
        sum(out$unallocated_positive_overshoot_n_t),
      sum(cells$cell_positive_overshoot_n_t),
      label = paste(resolution, "overshoot")
    )
  })
  grid <- run("grid")
  residual <- dplyr::filter(
    grid,
    .data$cell_id == .gs_cell_id("B"),
    .data$attribution_record_type == "cell_residual"
  )
  testthat::expect_equal(residual$boundary_component, component)
  testthat::expect_equal(
    residual$unallocated_critical_n_t,
    b[[paste0(component, "_critical_n_t")]]
  )
  testthat::expect_equal(
    residual$unallocated_signed_margin_n_t,
    -b[[paste0(component, "_critical_n_t")]]
  )
  testthat::expect_equal(residual$unallocated_positive_overshoot_n_t, 0)
}

testthat::test_that("a row-less extensive component keeps its allowance", {
  no_grass <- dplyr::filter(
    .gs_surplus(),
    !(.data$lon == 0.75 & .data$item_cbs_code == 3000L)
  )
  .gs_expect_rowless_carried(no_grass, "extensive")
})

testthat::test_that("a row-less managed component keeps its allowance", {
  no_crop <- dplyr::filter(
    .gs_surplus(),
    !(.data$lon == 0.75 & .data$item_cbs_code == 2511L)
  )
  .gs_expect_rowless_carried(no_crop, "managed")
})

# IMAGE-intensive grassland with no published igl value (435 cells in the
# archive) is not lent a rate: its grassland part stays uncompared, as in the
# published surface, and its cropland is still compared at its own ara rate
# (maintainer decision 2026-09-24). Only grassland promoted from extensive
# borrows an igl rate.
.gs_without_igl <- function(cells) {
  grassland <- .gs_grassland()
  grassland$critical_igl <- dplyr::filter(
    grassland$critical_igl,
    !.data$lon %in% .gs_lon()[cells]
  )
  grassland
}

testthat::test_that("IMAGE-intensive grassland without igl is not compared", {
  grassland <- .gs_without_igl("A")
  run <- \(resolution) .gs_run(resolution, grassland = grassland)
  testthat::expect_message(
    testthat::expect_message(
      run("cell"),
      class = "whep_nbx_zero_land_component"
    ),
    class = "whep_nbx_missing_critical_component"
  )
  testthat::expect_message(
    suppressMessages(run("cell"), classes = "whep_nbx_zero_land_component"),
    "managed 3 t N in 1 cell"
  )
  cells <- suppressMessages(run("cell"))
  a <- .gs_cell(cells, "A")
  # A's 3 t of intensive grassland pressure leaves the comparison; its
  # cropland (100 ha at ara 50 = 5 t) meets its 9 t of crop pressure.
  testthat::expect_equal(a$method_allowance_managed, "none")
  testthat::expect_equal(a$coverage_state, "valid")
  testthat::expect_equal(a$managed_area_ha, 100)
  testthat::expect_equal(a$managed_critical_n_t, 5)
  testthat::expect_equal(a$managed_actual_n_t, 9)
  testthat::expect_equal(a$managed_positive_overshoot_n_t, 4)
  testthat::expect_equal(a$excluded_igl_actual_n_t, 3)
  testthat::expect_equal(a$excluded_actual_n_t, 3)
  testthat::expect_equal(
    a$managed_actual_n_t + a$extensive_actual_n_t + a$excluded_igl_actual_n_t,
    12
  )
  # A no longer lends; C, promoted, borrows D's igl rate instead.
  c_cell <- .gs_cell(cells, "C")
  testthat::expect_equal(c_cell$managed_critical_kgn_ha, 25)
  testthat::expect_equal(c_cell$method_allowance_managed, "nearest_country")
  grid <- suppressMessages(run("grid"))
  a_rows <- dplyr::filter(grid, .data$cell_id == .gs_cell_id("A"))
  testthat::expect_setequal(a_rows$item_cbs_code, c(2511L, 2513L))
  testthat::expect_equal(sum(a_rows$critical_n_t), 5)
})

testthat::test_that("an intensive-only cell without igl stays uncompared", {
  # The published surface has no value for A: no igl, and no cropland.
  a_lon <- .gs_lon()[["A"]]
  grassland <- .gs_without_igl("A")
  grassland$classes <- dplyr::mutate(
    grassland$classes,
    a_crop_ha = dplyr::if_else(.data$lon == a_lon, 0, .data$a_crop_ha)
  )
  grassland$critical_ara <- dplyr::filter(
    grassland$critical_ara,
    .data$lon != a_lon
  )
  surplus <- dplyr::filter(
    .gs_surplus(),
    .data$lon != a_lon | .data$item_cbs_code == 3000L
  )
  out <- suppressMessages(whep::build_n_boundary_exceedance(
    surplus = surplus,
    critical = dplyr::filter(.gs_critical(), .data$lon != a_lon),
    land_use = "all",
    resolution = "cell",
    actual_year = 2015L,
    critical_reference_year = 2010L,
    grassland = grassland
  ))
  a <- .gs_cell(out, "A")
  testthat::expect_equal(a$coverage_state, "missing_critical")
  testthat::expect_equal(a$method_allowance_managed, "none")
  testthat::expect_equal(a$excluded_igl_actual_n_t, 3)
  testthat::expect_true(is.na(a$cell_positive_overshoot_n_t))
})

testthat::test_that("a pressure-only cell outside the domain has no method", {
  stray <- tibble::tibble(
    lon = 7.25,
    lat = 0.25,
    area_code = 1L,
    item_cbs_code = 2511L,
    area_ha = 10,
    surplus_n_t = 1,
    year = 2015L,
    n_input_std_t = 1
  )
  out <- suppressMessages(whep::build_n_boundary_exceedance(
    surplus = dplyr::bind_rows(.gs_surplus(), stray),
    critical = .gs_critical(),
    land_use = "all",
    resolution = "cell",
    actual_year = 2015L,
    critical_reference_year = 2010L,
    grassland = .gs_grassland()
  ))
  cell <- dplyr::filter(out, .data$lon == 7.25)
  testthat::expect_equal(cell$coverage_state, "out_of_domain")
  testthat::expect_true(is.na(cell$method_allowance_managed))
  testthat::expect_true(is.na(cell$method_allowance_extensive))
  testthat::expect_true(is.na(cell$managed_coverage_state))
})

testthat::test_that("an all-missing ara layer is an absent input", {
  grassland <- .gs_grassland()
  grassland$critical_ara$value <- NA_real_
  expect_supplied_guard(
    identity = nrow(grassland$critical_ara) == 5L &&
      all(grassland$critical_ara$critical_land_use == "ara"),
    guard = .gs_run("cell", grassland = grassland)
  )
})

testthat::test_that("an all-zero igl layer is an absent input", {
  grassland <- .gs_grassland()
  grassland$critical_igl$value <- 0
  expect_supplied_guard(
    identity = nrow(grassland$critical_igl) == 2L &&
      all(grassland$critical_igl$critical_land_use == "igl"),
    guard = .gs_run("cell", grassland = grassland)
  )
})

testthat::test_that("ara and igl must be the 2010 reference layers", {
  grassland <- .gs_grassland()
  grassland$critical_igl$critical_year <- 2011L
  testthat::expect_error(
    .gs_run("cell", grassland = grassland),
    class = "whep_nbx_grassland_bad_layer"
  )
})

testthat::test_that("ara and igl must combine into the all surface", {
  # A: all is 60; with igl 90 instead of 80, (50*100 + 90*50)/150 = 63.3,
  # 5.6 % off. D and the single-use cells still agree.
  grassland <- .gs_grassland()
  grassland$critical_igl <- dplyr::mutate(
    grassland$critical_igl,
    value = dplyr::if_else(.data$lon == .gs_lon()[["A"]], 90, .data$value)
  )
  testthat::expect_error(
    .gs_run("cell", grassland = grassland),
    class = "whep_nbx_grassland_bad_layer"
  )
  testthat::expect_error(
    .gs_run("cell", grassland = grassland),
    "1 cell.*cell 129241"
  )
  # Within 1 % passes: igl 81 gives 60.33, 0.56 % off.
  grassland$critical_igl$value[[1L]] <- 81
  testthat::expect_no_error(
    suppressMessages(.gs_run("cell", grassland = grassland))
  )
})
