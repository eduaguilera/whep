# calculate_critical_n(): the Schulte-Uebbing et al. (2022) critical-nitrogen
# allowances computed from their inputs (#1291). The fixture is four real
# cells of the archive's 2010 Input_files (R/toy_examples.R); the real-data
# test at the end compares every deposited layer when the archive is cached.

.critn_fixture_prep <- function(inputs = whep:::.example_critical_n_inputs()) {
  whep:::.critn_prepare(inputs)
}

.critn_forward_at <- function(prep, x) {
  whep:::.critn_forward(prep, pmax(x$x_ara, 0), pmax(x$x_igl, 0))
}

testthat::test_that("calculate_critical_n returns one row per cell and scope", {
  testthat::skip_if_not_installed("pointblank")
  out <- whep::calculate_critical_n(example = TRUE)
  pointblank::expect_col_exists(
    out,
    c(
      "cell_id",
      "lon",
      "lat",
      "image_region",
      "critical_threshold",
      "critical_land_use",
      "area_ha",
      "critical_n_input_kgn_ha",
      "critical_n_surplus_kgn_ha",
      "current_n_input_kgn_ha",
      "current_n_surplus_kgn_ha",
      "critical_rule",
      "method_critical_n"
    )
  )
  pointblank::expect_col_vals_in_set(
    out,
    "critical_threshold",
    c("de", "sw", "gw", "mi")
  )
  pointblank::expect_col_vals_equal(out, "method_critical_n", "reproduced")
  pointblank::expect_col_vals_not_null(out, "critical_n_input_kgn_ha")
  # Arable land in three cells, intensive grassland in two, so three + two
  # + four "all" rows for each of the four thresholds.
  counts <- dplyr::count(out, .data$critical_land_use)
  testthat::expect_equal(counts$n[counts$critical_land_use == "ara"], 12L)
  testthat::expect_equal(counts$n[counts$critical_land_use == "igl"], 8L)
  testthat::expect_equal(counts$n[counts$critical_land_use == "all"], 16L)
  testthat::expect_setequal(
    unique(stats::na.omit(out$critical_rule)),
    c(
      "environmental_threshold",
      "non_agricultural_floor",
      "yield_potential_cap"
    )
  )
  testthat::expect_true(all(is.na(
    out$critical_rule[out$critical_land_use == "all"]
  )))
})

testthat::test_that("current inputs are fertiliser and manure gross of NH3", {
  prep <- .critn_fixture_prep()
  inputs <- whep:::.example_critical_n_inputs()
  # Arable-only cell: all storage NH3 belongs to arable land.
  i <- 1L
  gross <- inputs$fertilizer_net_arable_kg[i] +
    inputs$manure_net_arable_kg[i] +
    inputs$nh3_fertilizer_arable_kg[i] +
    inputs$nh3_spreading_arable_kg[i] +
    inputs$nh3_storage_kg[i]
  testthat::expect_equal(prep$x_ara[i], gross)
  testthat::expect_equal(
    prep$input_ara[i],
    gross +
      inputs$fixation_arable_kg[i] +
      prep$f_ara[i] *
        max(
          inputs$deposition_kg[i],
          inputs$nh3_fertilizer_arable_kg[i] +
            inputs$nh3_spreading_arable_kg[i] +
            inputs$nh3_storage_kg[i]
        )
  )
})

testthat::test_that("each threshold's limit is met where it binds", {
  prep <- .critn_fixture_prep()
  de <- whep:::.critn_env_deposition(prep)
  sw <- whep:::.critn_env_surface_water(prep)
  gw <- whep:::.critn_env_groundwater(prep)
  # SI Eq. 7: total emission equals the biome's critical deposition times
  # the cell area wherever the solution is positive.
  ok <- de$x_ara > 0 | de$x_igl > 0
  testthat::expect_equal(
    .critn_forward_at(prep, de)$emission[ok],
    prep$critical_deposition_kgn_ha[ok] * prep$area_total_ha[ok]
  )
  # SI Eq. 12: load to surface water equals 5 mg N/l of runoff.
  ok <- sw$x_ara > 0 | sw$x_igl > 0
  testthat::expect_true(any(ok))
  testthat::expect_equal(
    .critn_forward_at(prep, sw)$load[ok],
    5e-6 * prep$runoff_l[ok]
  )
  # SI Eq. 29 in the arable-with-extensive cell: leaching from arable land
  # equals 11.6 mg NO3-N/l of the water leaving it.
  i <- 2L
  testthat::expect_gt(gw$x_ara[i], 0)
  emission <- prep$emission_fixed[i] + prep$c_ara[i] * gw$x_ara[i]
  input <- gw$x_ara[i] + prep$fixation_arable_kg[i] + prep$f_ara[i] * emission
  leaching <- prep$fle_ag[i] *
    (1 - prep$fsro_ag[i]) *
    (1 - prep$fnup_ara[i]) *
    input
  testthat::expect_equal(
    leaching,
    11.6e-6 * (1 - prep$fsro_ag[i]) * prep$runoff_l[i] * prep$f_ara[i]
  )
})

testthat::test_that("all impacts take the lowest threshold per land use", {
  out <- whep::calculate_critical_n(example = TRUE) |>
    dplyr::filter(.data$critical_land_use != "all")
  wide <- out |>
    dplyr::select(
      "cell_id",
      "critical_land_use",
      "critical_threshold",
      "critical_n_input_kgn_ha"
    ) |>
    tidyr::pivot_wider(
      names_from = "critical_threshold",
      values_from = "critical_n_input_kgn_ha"
    )
  # Single-use cells: the minimum of the three is exactly "mi".
  single <- dplyr::filter(wide, .data$cell_id != 59642L)
  testthat::expect_equal(
    single$mi,
    pmin(single$de, single$sw, single$gw)
  )
})

testthat::test_that("the non-agricultural floor leaves fixation and deposition", {
  prep <- .critn_fixture_prep()
  out <- whep::calculate_critical_n(example = TRUE)
  # India cell, groundwater: the non-agricultural sources already exceed it.
  row <- dplyr::filter(
    out,
    .data$cell_id == 89786L,
    .data$critical_threshold == "gw",
    .data$critical_land_use == "ara"
  )
  testthat::expect_equal(row$critical_rule, "non_agricultural_floor")
  i <- 1L
  testthat::expect_equal(
    row$critical_n_input_kgn_ha,
    (prep$fixation_arable_kg[i] + prep$f_ara[i] * prep$emission_fixed[i]) /
      prep$area_arable_ha[i]
  )
})

testthat::test_that("the cut-off keeps uptake at the regional yield potential", {
  prep <- .critn_fixture_prep()
  out <- whep::calculate_critical_n(example = TRUE)
  row <- dplyr::filter(
    out,
    .data$cell_id == 61368L,
    .data$critical_threshold == "de",
    .data$critical_land_use == "ara"
  )
  testthat::expect_equal(row$critical_rule, "yield_potential_cap")
  i <- 2L
  ratios <- whep:::.critn_region_ratios()
  ratio <- ratios$ratio_arable[ratios$image_region == 2L]
  uptake_max <- prep$uptake_arable_kg[i] * ratio
  testthat::expect_equal(
    (row$critical_n_input_kgn_ha - row$critical_n_surplus_kgn_ha) *
      prep$area_arable_ha[i],
    uptake_max
  )
  # Methods Eq. 1: the input at the cut-off is uptake at yield potential over
  # current NUE (here below the 0.8 ceiling).
  testthat::expect_lt(prep$nue_ara[i], 0.8)
  testthat::expect_equal(
    row$critical_n_input_kgn_ha * prep$area_arable_ha[i],
    uptake_max / prep$nue_ara[i]
  )
})

testthat::test_that("NUE above 0.8 is capped in the cut-off", {
  inputs <- whep:::.example_critical_n_inputs()[2, ]
  # Push NUE well above 0.8 and runoff up so no threshold binds.
  inputs$uptake_arable_kg <- 0.95 *
    whep:::.critn_prepare(inputs)$input_ara
  inputs$runoff_l <- inputs$runoff_l * 100
  out <- whep::calculate_critical_n(inputs = inputs) |>
    dplyr::filter(
      .data$critical_threshold == "sw",
      .data$critical_land_use == "ara"
    )
  testthat::expect_equal(out$critical_rule, "yield_potential_cap")
  # Surplus at the cut-off is input times (1 - 0.8), not (1 - 0.95).
  testthat::expect_equal(
    out$critical_n_surplus_kgn_ha,
    0.2 * out$critical_n_input_kgn_ha
  )
})

testthat::test_that("the cut-off binds once uptake reaches yield potential", {
  inputs <- whep:::.example_critical_n_inputs()[2, ]
  inputs$uptake_arable_kg <- 0.95 * whep:::.critn_prepare(inputs)$input_ara
  prep <- whep:::.critn_prepare(inputs)
  # A critical input whose uptake (NUE 0.95) passes uptake at yield
  # potential while the input stays below the cut-off input, which divides
  # by NUE capped at 0.8. The archive cuts off here, raising the input.
  input <- prep$uptake_max_ara / 0.9
  x <- (input - prep$fixation_arable_kg - prep$f_ara * prep$emission_fixed) /
    (1 + prep$f_ara * prep$c_ara)
  testthat::expect_lt(x, prep$x_max_ara)
  out <- whep:::.critn_finish(prep, list(x_ara = x, x_igl = 0), "sw") |>
    dplyr::filter(.data$critical_land_use == "ara")
  testthat::expect_equal(out$critical_rule, "yield_potential_cap")
  testthat::expect_equal(
    out$critical_n_input_kgn_ha * prep$area_arable_ha,
    prep$uptake_max_ara / 0.8
  )
})

testthat::test_that("the cut-off counts the other land use's deposition", {
  prep <- .critn_fixture_prep()[3, ]
  testthat::expect_lt(prep$nue_igl, 0.8)
  x_ara <- prep$x_ara
  # Intensive grassland's own fertiliser plus manure stays below its
  # cut-off value, but with the NH3 arable land deposits on it the input
  # passes the cut-off input.
  push <- prep$f_igl * prep$c_ara * x_ara / (1 + prep$f_igl * prep$c_igl)
  x_igl <- prep$x_max_igl - push / 2
  out <- whep:::.critn_finish(
    prep,
    list(x_ara = x_ara, x_igl = x_igl),
    "de"
  ) |>
    dplyr::filter(.data$critical_land_use == "igl")
  testthat::expect_equal(out$critical_rule, "yield_potential_cap")
  testthat::expect_equal(
    (out$critical_n_input_kgn_ha - out$critical_n_surplus_kgn_ha) *
      prep$area_intensive_ha,
    prep$uptake_max_igl
  )
})

testthat::test_that("critical NH3 is shared by current NH3 in mixed cells", {
  inputs <- whep:::.example_critical_n_inputs()[3, ]
  # Grassland with manure only: its fertiliser share is clipped to 1e-4, so
  # its NH3 per kg is not its current NH3 over its current input.
  testthat::expect_equal(inputs$fertilizer_net_grass_kg, 0)
  prep <- whep:::.critn_prepare(inputs)
  de <- whep:::.critn_env_deposition(prep)
  nh3_ara <- prep$nh3_fer_ara + prep$nh3_man_ara
  nh3_igl <- prep$nh3_fer_igl + prep$nh3_man_igl
  allowance <- prep$limit_de - prep$emission_fixed
  testthat::expect_gt(allowance, 0)
  testthat::expect_equal(
    prep$c_igl * de$x_igl,
    allowance * nh3_igl / (nh3_ara + nh3_igl)
  )
  testthat::expect_equal(
    prep$c_ara * de$x_ara,
    allowance * nh3_ara / (nh3_ara + nh3_igl)
  )
})

testthat::test_that("a land use emitting no NH3 has no allowance", {
  inputs <- whep:::.example_critical_n_inputs()
  # Arable land of the US cell gets fertiliser that emits no NH3, and no
  # manure: the archive leaves such cells empty in every layer.
  inputs$manure_net_arable_kg[2] <- 0
  inputs$nh3_fertilizer_arable_kg[2] <- 0
  inputs$nh3_spreading_arable_kg[2] <- 0
  out <- whep::calculate_critical_n(inputs = inputs)
  arable <- dplyr::filter(out, .data$critical_land_use == "ara")
  testthat::expect_false(61368L %in% arable$cell_id)
  testthat::expect_true(all(is.finite(out$critical_n_input_kgn_ha)))
  testthat::expect_true(89786L %in% arable$cell_id)
})

testthat::test_that("the fertiliser share is clipped to [1e-4, 1 - 1e-4]", {
  inputs <- whep:::.example_critical_n_inputs()[1, ]
  inputs$manure_net_arable_kg <- 0
  inputs$nh3_spreading_arable_kg <- 0
  inputs$nh3_storage_kg <- 0
  prep <- whep:::.critn_prepare(inputs)
  fertilizer <- inputs$fertilizer_net_arable_kg +
    inputs$nh3_fertilizer_arable_kg
  testthat::expect_equal(
    prep$c_ara,
    (1 - 1e-4) * inputs$nh3_fertilizer_arable_kg / fertilizer
  )
})

testthat::test_that("a cell without agricultural leaching has no allowance", {
  inputs <- whep:::.example_critical_n_inputs()
  inputs$leaching_ag_kg[2] <- 0
  out <- whep::calculate_critical_n(inputs = inputs)
  testthat::expect_false(61368L %in% out$cell_id)
  testthat::expect_true(all(c(89786L, 59642L, 61591L) %in% out$cell_id))
})

testthat::test_that("yield-gap ratios round to Supplementary Table 5", {
  ratios <- whep:::.critn_region_ratios()
  testthat::expect_equal(ratios$image_region, 1:26)
  testthat::expect_equal(round(ratios$ratio_arable, 2), ratios$ratio_arable_si)
  # Table 5 note 6: grassland uptake never falls below current uptake.
  testthat::expect_equal(
    round(ratios$ratio_grass, 2),
    pmax(1, ratios$ratio_grass_si),
    tolerance = 0.006
  )
  testthat::expect_true(all(ratios$ratio_grass >= 1))
})

testthat::test_that("critical deposition follows Supplementary Table 2", {
  rates <- whep:::.critn_biome_rates()
  testthat::expect_equal(rates$biome, 7:20)
  testthat::expect_equal(
    rates$critical_deposition_kgn_ha,
    c(5, 10, 10, 7.5, 7.5, 12.5, 12.5, 10, 17.5, 5, 7.5, 15, 20, 20)
  )
})

testthat::test_that("absent runoff is refused although its limit reconciles", {
  inputs <- whep:::.example_critical_n_inputs()
  inputs$runoff_l <- 0
  prep <- whep:::.critn_prepare(inputs)
  expect_supplied_guard(
    identity = isTRUE(all.equal(prep$limit_sw, 5e-6 * prep$runoff_l)),
    guard = whep::calculate_critical_n(inputs = inputs)
  )
})

testthat::test_that("a missing flow on agricultural land aborts", {
  inputs <- whep:::.example_critical_n_inputs()
  inputs$nh3_storage_kg[1] <- NA_real_
  testthat::expect_error(
    whep::calculate_critical_n(inputs = inputs),
    class = "whep_critn_missing_input"
  )
  testthat::expect_error(
    whep::calculate_critical_n(inputs = dplyr::select(inputs, -"runoff_l")),
    "runoff_l"
  )
})

testthat::test_that("read_critical_n stamps a reproduced layer", {
  tmp <- withr::local_tempdir()
  .critical_n_write_asc(tmp)
  reproduced <- tibble::tibble(
    lon = c(0.25, 0.75),
    lat = 0.75,
    critical_threshold = "mi",
    critical_land_use = "all",
    critical_n_input_kgn_ha = c(90, 80),
    critical_n_surplus_kgn_ha = c(30, 20),
    current_n_surplus_kgn_ha = c(50, 10)
  )
  testthat::local_mocked_bindings(
    calculate_critical_n = function(...) reproduced
  )
  surplus <- whep::read_critical_n(
    "critical_n_surplus",
    dir = tmp,
    method = "reproduced"
  )
  testthat::expect_equal(surplus$value, c(30, 20))
  testthat::expect_equal(unique(surplus$method_critical_n), "reproduced")
  testthat::expect_match(unique(surplus$critical_source), "recomputed by WHEP")
  testthat::expect_equal(surplus$source_area_ha, c(110, 220))
  exceedance <- whep::read_critical_n(
    "exceedance",
    dir = tmp,
    method = "reproduced"
  )
  testthat::expect_equal(exceedance$value, c(20, -10))
  input <- whep::read_critical_n(
    "critical_n_input",
    dir = tmp,
    method = "reproduced"
  )
  testthat::expect_equal(input$value, c(90, 80))
})

testthat::test_that("read_critical_n refuses a layer it cannot reproduce", {
  testthat::expect_error(
    whep::read_critical_n(
      "threshold_exceedance",
      method = "reproduced",
      example = TRUE
    ),
    class = "whep_critn_method_unsupported"
  )
  testthat::expect_error(
    whep::read_critical_n(method = "recomputed", example = TRUE),
    "method"
  )
})

testthat::test_that("calculate_critical_n reproduces the deposited layers", {
  dir <- .real_critn_dir()
  out <- whep::calculate_critical_n(dir = dir)
  root <- whep:::.critn_root_path(dir)
  single <- whep:::.critn_read_inputs(root) |>
    dplyr::filter(
      (.data$area_arable_ha > 0) + (.data$area_intensive_ha > 0) == 1L,
      .data$area_extensive_ha == 0
    ) |>
    dplyr::pull("cell_id")
  layers <- tidyr::expand_grid(
    threshold = c("de", "sw", "gw", "mi"),
    land_use = c("ara", "igl", "all"),
    var = c("critical_n_input", "critical_n_surplus")
  )
  gate <- purrr::pmap(layers, \(threshold, land_use, var) {
    archive <- whep::read_critical_n(var, threshold, land_use, dir = dir)
    ours <- dplyr::filter(
      out,
      .data$critical_threshold == .env$threshold,
      .data$critical_land_use == .env$land_use
    )
    value <- if (var == "critical_n_input") {
      ours$critical_n_input_kgn_ha
    } else {
      ours$critical_n_surplus_kgn_ha
    }
    both <- dplyr::inner_join(
      dplyr::select(archive, "cell_id", "source_area_ha", archive = "value"),
      tibble::tibble(cell_id = ours$cell_id, reproduced = value),
      by = "cell_id"
    )
    diff <- abs(both$reproduced - both$archive)
    tibble::tibble(
      threshold = threshold,
      layer = paste(var, threshold, land_use),
      missed = sum(!archive$cell_id %in% ours$cell_id),
      single_exact = mean(diff[both$cell_id %in% single] < 0.005),
      within_1 = mean(diff <= 1),
      total_ratio = sum(both$reproduced * both$source_area_ha) /
        sum(both$archive * both$source_area_ha)
    )
  }) |>
    purrr::list_rbind()
  # No deposited cell is left without a reproduced value.
  testthat::expect_true(all(gate$missed == 0L))
  # Cells with one reducible land use and no extensive grassland follow the
  # printed equations: exact to the rasters' 0.001 kg N/ha, both rounded.
  # The all-impacts layer departs from the lowest of the three thresholds in
  # 2 of the 128 arable-only cells (cells 89068 and 89787: "mi" equals the
  # surface-water cut-off while deposition and groundwater are at the floor).
  per_threshold <- dplyr::filter(gate, .data$threshold != "mi")
  testthat::expect_true(all(per_threshold$single_exact == 1))
  testthat::expect_true(all(gate$single_exact >= 0.98))
  # Measured 2026-10-06: at least 91% of cells within 1 kg N/ha in every
  # layer, global totals within 1.7%.
  testthat::expect_true(all(gate$within_1 >= 0.9))
  testthat::expect_true(all(abs(gate$total_ratio - 1) < 0.02))
})
