.cell_first_critical <- function(
  value = 50,
  area = 100,
  var = "critical_n_surplus"
) {
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
  crops <- dplyr::filter(
    out,
    .data$attribution_record_type == "crop_allocation"
  )
  residual <- dplyr::filter(
    out,
    .data$attribution_record_type == "cell_residual"
  )
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
  crops <- dplyr::filter(
    out,
    .data$attribution_record_type == "crop_allocation"
  )
  residual <- dplyr::filter(
    out,
    .data$attribution_record_type == "cell_residual"
  )
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

# ---- negative_critical: keep versus clamp ------------------------------------

# Two cells: 8 t of surplus against a -2 t allowance (-20 kg/ha on 100 ha) in
# the first, 8 t against +5 t in the second. Only the first can move.
.negative_critical_case <- function(negative_critical, resolution = "grid") {
  critical <- dplyr::bind_rows(
    .cell_first_critical(-20),
    dplyr::mutate(.cell_first_critical(50), lon = 0.75)
  )
  actual <- dplyr::bind_rows(
    .cell_first_surplus(),
    dplyr::mutate(.cell_first_surplus(), lon = 0.75)
  )
  whep::build_n_boundary_exceedance(
    actual,
    critical,
    resolution = resolution,
    actual_year = 2015L,
    critical_reference_year = 2010L,
    negative_critical = negative_critical
  )
}

testthat::test_that("keep compares against the negative critical surplus", {
  cell <- .negative_critical_case("keep", "cell") |> dplyr::arrange(lon)
  testthat::expect_equal(cell$source_critical_kgn_ha, c(-20, 50))
  testthat::expect_equal(cell$critical_kgn_ha, c(-20, 50))
  testthat::expect_equal(cell$cell_critical_n_t, c(-2, 5))
  testthat::expect_equal(cell$cell_positive_overshoot_n_t, c(10, 3))
  grid <- .negative_critical_case("keep")
  within <- dplyr::summarise(
    grid,
    within = sum(within_boundary_n_t),
    .by = "lon"
  ) |>
    dplyr::arrange(lon)
  # The overshoot exceeds the 8 t actual surplus, so within is negative.
  testthat::expect_equal(within$within, c(-2, 5))
})

testthat::test_that("clamp gives a negative critical surplus zero allowance", {
  cell <- .negative_critical_case("clamp", "cell") |> dplyr::arrange(lon)
  testthat::expect_equal(cell$source_critical_kgn_ha, c(-20, 50))
  testthat::expect_equal(cell$critical_kgn_ha, c(0, 50))
  testthat::expect_equal(cell$cell_critical_n_t, c(0, 5))
  testthat::expect_equal(cell$cell_signed_margin_n_t, c(8, 3))
  testthat::expect_equal(cell$cell_positive_overshoot_n_t, c(8, 3))
  grid <- .negative_critical_case("clamp")
  within <- dplyr::summarise(
    grid,
    within = sum(within_boundary_n_t),
    .by = "lon"
  ) |>
    dplyr::arrange(lon)
  testthat::expect_equal(within$within, c(0, 5))
  testthat::expect_true(all(grid$within_boundary_n_t >= 0))
})

testthat::test_that("clamp leaves non-negative critical cells untouched", {
  keep <- .negative_critical_case("keep") |> dplyr::filter(lon == 0.75)
  clamp <- .negative_critical_case("clamp") |> dplyr::filter(lon == 0.75)
  masses <- c(
    "critical_n_t",
    "signed_margin_n_t",
    "exceedance_n_t",
    "within_boundary_n_t"
  )
  testthat::expect_equal(clamp[masses], keep[masses])
})

testthat::test_that("within plus exceedance conserves the actual pressure", {
  for (mode in c("keep", "clamp")) {
    grid <- .negative_critical_case(mode)
    testthat::expect_equal(
      grid$within_boundary_n_t + grid$exceedance_n_t,
      grid$actual_n_t
    )
    per_cell <- dplyr::summarise(
      grid,
      total = sum(within_boundary_n_t + exceedance_n_t),
      cell_actual = dplyr::first(cell_actual_n_t),
      .by = "cell_id"
    )
    testthat::expect_equal(per_cell$total, per_cell$cell_actual)
    country <- .negative_critical_case(mode, "country")
    testthat::expect_equal(
      sum(country$within_boundary_n_t + country$exceedance_n_t),
      sum(grid$actual_n_t)
    )
  }
})

testthat::test_that("the negative_critical choice is stamped in every row", {
  for (mode in c("keep", "clamp")) {
    for (res in c("cell", "grid", "country", "image_region")) {
      out <- .negative_critical_case(mode, res)
      testthat::expect_true(rlang::has_name(out, "negative_critical"))
      testthat::expect_true(all(out$negative_critical == mode))
    }
  }
})

.negative_critical_case_one <- function(mode) {
  whep::build_n_boundary_exceedance(
    .cell_first_surplus(),
    .cell_first_critical(-20),
    resolution = "grid",
    actual_year = 2015L,
    critical_reference_year = 2010L,
    negative_critical = mode
  )
}

testthat::test_that("keep is the default negative_critical treatment", {
  default <- whep::build_n_boundary_exceedance(
    .cell_first_surplus(),
    .cell_first_critical(-20),
    resolution = "grid",
    actual_year = 2015L,
    critical_reference_year = 2010L
  )
  testthat::expect_identical(default, .negative_critical_case_one("keep"))
  testthat::expect_true(all(default$negative_critical == "keep"))
  testthat::expect_true(all(is.na(default$binding_threshold)))
  testthat::expect_true(all(is.na(default$binding_matches_mi)))
})

testthat::test_that("an unknown negative_critical value aborts", {
  testthat::expect_error(
    .negative_critical_case_one("floor"),
    class = "rlang_error"
  )
})

# ---- binding threshold carried from build_critical_n_binding() -------------

.binding_table <- function(
  mi = -20,
  land_use = "ara",
  label = "surface_water"
) {
  tibble::tibble(
    cell_id = whep:::.nbx_add_cell_key(
      tibble::tibble(lon = 0.25, lat = 0.25),
      "test"
    )$cell_id,
    lon = 0.25,
    lat = 0.25,
    critical_land_use = land_use,
    critical_de_kgn_ha = 30,
    critical_gw_kgn_ha = 10,
    critical_sw_kgn_ha = -20,
    binding_critical_kgn_ha = -20,
    binding_threshold = label,
    critical_mi_kgn_ha = mi,
    binding_matches_mi = mi == -20
  )
}

testthat::test_that("a binding table reaches the cell and grid results", {
  args <- list(
    .cell_first_surplus(),
    .cell_first_critical(-20),
    actual_year = 2015L,
    critical_reference_year = 2010L,
    binding = .binding_table()
  )
  grid <- rlang::exec(
    whep::build_n_boundary_exceedance,
    !!!args,
    resolution = "grid"
  )
  cell <- rlang::exec(
    whep::build_n_boundary_exceedance,
    !!!args,
    resolution = "cell"
  )
  testthat::expect_true(all(grid$binding_threshold == "surface_water"))
  testthat::expect_true(all(grid$binding_matches_mi))
  testthat::expect_equal(cell$binding_threshold, "surface_water")
  # The binding label is a property of the source surface, not of the clamp.
  clamp <- rlang::exec(
    whep::build_n_boundary_exceedance,
    !!!args,
    resolution = "cell",
    negative_critical = "clamp"
  )
  testthat::expect_equal(clamp$binding_threshold, "surface_water")
})

testthat::test_that("a binding table from another scope or layer aborts", {
  call <- function(binding) {
    whep::build_n_boundary_exceedance(
      .cell_first_surplus(),
      .cell_first_critical(-20),
      resolution = "cell",
      actual_year = 2015L,
      critical_reference_year = 2010L,
      binding = binding
    )
  }
  testthat::expect_error(call(.binding_table(land_use = "all")), "land_use")
  testthat::expect_error(call(.binding_table(mi = 12)), "not built from")
  testthat::expect_error(
    call(dplyr::bind_rows(.binding_table(), .binding_table())),
    "duplicate"
  )
  testthat::expect_error(
    call(dplyr::select(.binding_table(), -"binding_threshold")),
    "binding_threshold"
  )
})

# ---- real archive: the published 2010 decomposition ------------------------

.real_decomposition <- function(dir, negative_critical) {
  critical <- whep::read_critical_n(
    "critical_n_surplus",
    "mi",
    "all",
    dir = dir
  )
  current <- whep::read_critical_n("exceedance", "mi", "all", dir = dir) |>
    dplyr::select("cell_id", exceedance = "value") |>
    dplyr::inner_join(critical, by = "cell_id") |>
    dplyr::transmute(
      lon = .data$lon,
      lat = .data$lat,
      area_code = 1L,
      item_cbs_code = 2511L,
      year = 2010L,
      area_ha = .data$source_area_ha,
      surplus_n_t = (.data$value + .data$exceedance) *
        .data$source_area_ha /
        1000
    )
  whep::build_n_boundary_exceedance(
    current,
    critical,
    land_use = "all",
    resolution = "cell",
    actual_year = 2010L,
    critical_reference_year = 2010L,
    negative_critical = negative_critical
  ) |>
    dplyr::summarise(
      current = sum(cell_actual_n_t) / 1e6,
      exceedance = sum(cell_positive_overshoot_n_t) / 1e6,
      allowable = sum(cell_actual_n_t - cell_positive_overshoot_n_t) / 1e6
    )
}

testthat::test_that("keep reproduces the published 2010 decomposition", {
  dir <- .real_critn_dir()
  keep <- .real_decomposition(dir, "keep")
  # Schulte-Uebbing et al. (2022): 43 Mt N allowable + 76 Mt N exceedance =
  # 119 Mt N current agricultural surplus.
  testthat::expect_equal(
    round(c(keep$allowable, keep$exceedance, keep$current)),
    c(43, 76, 119)
  )
  clamp <- .real_decomposition(dir, "clamp")
  testthat::expect_equal(clamp$current, keep$current)
  testthat::expect_lt(clamp$exceedance, keep$exceedance)
  testthat::expect_false(round(clamp$exceedance) == 76)
  # The clamp moves 0.43 Mt N from exceedance to allowable.
  testthat::expect_equal(
    round(c(clamp$allowable, clamp$exceedance), 2),
    c(43.50, 75.25)
  )
})
