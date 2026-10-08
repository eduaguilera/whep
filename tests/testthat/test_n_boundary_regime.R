# Rainfed/irrigated comparison units of the critical-N boundary (issue
# #1345). Every expected value is derived by hand in the test that uses it.

.nbr_critical <- function(land_use = "ara", value = 50, area = 100) {
  tibble::tibble(
    lon = 0.25,
    lat = 0.25,
    value = value,
    source_area_ha = area,
    image_region = 11L,
    critical_var = "critical_n_surplus",
    critical_land_use = land_use,
    critical_threshold = "mi",
    critical_year = 2010L
  )
}

# The worked cell of the issue: an allowance of 50 kg/ha on 100 ha = 5 t,
# shared 70/30 by area, so 3.5 t rainfed and 1.5 t irrigated. The rainfed
# part is 3 t under its share and the irrigated part 5 t over.
.nbr_surplus <- function() {
  tibble::tribble(
    ~area_code, ~item_cbs_code, ~water_regime, ~area_ha, ~surplus_n_t,
    1L,         2511L,          "rainfed",     50,       0.25,
    1L,         2511L,          "irrigated",   30,       6.5,
    2L,         2513L,          "rainfed",     20,       0.25,
    2L,         2513L,          "irrigated",   0,        0
  ) |>
    dplyr::mutate(
      lon = 0.25,
      lat = 0.25,
      year = 2010L,
      n_input_std_t = abs(.data$surplus_n_t) + 1
    )
}

.nbr_run <- function(
  regime_comparison,
  resolution = "cell",
  surplus = .nbr_surplus(),
  ...
) {
  whep::build_n_boundary_exceedance(
    surplus = surplus,
    critical = .nbr_critical(),
    land_use = "ara",
    resolution = resolution,
    actual_year = 2010L,
    critical_reference_year = 2010L,
    regime_comparison = regime_comparison,
    ...
  )
}

testthat::test_that("separate parts do not net irrigated excess away", {
  netted <- .nbr_run("netted")
  separate <- .nbr_run("separate")
  testthat::expect_equal(netted$cell_positive_overshoot_n_t, 2)
  testthat::expect_equal(separate$cell_positive_overshoot_n_t, 5)
  # Actual, allowance and signed margin are those of the netted cell.
  testthat::expect_equal(separate$cell_actual_n_t, 7)
  testthat::expect_equal(separate$cell_critical_n_t, 5)
  testthat::expect_equal(separate$cell_signed_margin_n_t, 2)
  testthat::expect_equal(separate$rainfed_actual_n_t, 0.5)
  testthat::expect_equal(separate$rainfed_critical_n_t, 3.5)
  testthat::expect_equal(separate$rainfed_positive_overshoot_n_t, 0)
  testthat::expect_equal(separate$irrigated_actual_n_t, 6.5)
  testthat::expect_equal(separate$irrigated_critical_n_t, 1.5)
  testthat::expect_equal(separate$irrigated_positive_overshoot_n_t, 5)
  testthat::expect_identical(separate$regime_comparison, "separate")
  testthat::expect_identical(netted$regime_comparison, "netted")
  testthat::expect_true(is.na(netted$irrigated_positive_overshoot_n_t))
})

testthat::test_that("crop rows share their own part's allowance", {
  grid <- .nbr_run("separate", "grid")
  crop <- dplyr::filter(
    grid,
    .data$attribution_record_type == "crop_allocation"
  )
  rainfed <- dplyr::filter(crop, .data$water_regime == "rainfed")
  # The rainfed part (0.5 t, allowance 3.5 t) is split equally between the
  # two crops; the irrigated part is crop 2511 alone (crop 2513 has no
  # irrigated pressure), so it takes the whole irrigated overshoot.
  testthat::expect_equal(rainfed$pressure_share, c(0.5, 0.5))
  testthat::expect_equal(rainfed$critical_n_t, c(1.75, 1.75))
  testthat::expect_equal(rainfed$exceedance_n_t, c(0, 0))
  irrigated <- dplyr::filter(
    crop,
    .data$water_regime == "irrigated",
    .data$item_cbs_code == 2511L
  )
  testthat::expect_equal(irrigated$critical_n_t, 1.5)
  testthat::expect_equal(irrigated$exceedance_n_t, 5)
  testthat::expect_equal(irrigated$regime_positive_overshoot_n_t, 5)
  # The zero-pressure irrigated row of 2513 takes a zero share of its part.
  testthat::expect_equal(sum(grid$exceedance_n_t), 5)
  testthat::expect_equal(
    sum(grid$critical_n_t) + sum(grid$unallocated_critical_n_t),
    5
  )
  country <- .nbr_run("separate", "country")
  testthat::expect_equal(
    country$exceedance_n_t[country$item_cbs_code == 2511L],
    5
  )
  testthat::expect_identical(unique(country$regime_comparison), "separate")
})

testthat::test_that("separate equals netted when no part nets against another", {
  # Pressure proportional to area: both parts sit at the same intensity, so
  # neither has headroom the other could have used.
  proportional <- .nbr_surplus() |>
    dplyr::mutate(surplus_n_t = .data$area_ha * 0.08)
  netted <- .nbr_run("netted", surplus = proportional)
  separate <- .nbr_run("separate", surplus = proportional)
  testthat::expect_equal(
    separate$cell_positive_overshoot_n_t,
    netted$cell_positive_overshoot_n_t
  )
  testthat::expect_equal(netted$cell_positive_overshoot_n_t, 3)
  # A balance built without the split carries every row as rainfed.
  rainfed_only <- .nbr_surplus() |>
    dplyr::mutate(water_regime = "rainfed") |>
    dplyr::summarise(
      area_ha = sum(.data$area_ha),
      surplus_n_t = sum(.data$surplus_n_t),
      n_input_std_t = sum(.data$n_input_std_t),
      .by = c(
        "lon",
        "lat",
        "area_code",
        "item_cbs_code",
        "year",
        "water_regime"
      )
    )
  testthat::expect_equal(
    .nbr_run("separate", surplus = rainfed_only)$cell_positive_overshoot_n_t,
    .nbr_run("netted")$cell_positive_overshoot_n_t
  )
})

testthat::test_that("netted output is unchanged by carrying the regime rows", {
  collapsed <- .nbr_surplus() |>
    dplyr::summarise(
      area_ha = sum(.data$area_ha),
      surplus_n_t = sum(.data$surplus_n_t),
      n_input_std_t = sum(.data$n_input_std_t),
      .by = c("lon", "lat", "area_code", "item_cbs_code", "year")
    )
  testthat::expect_equal(
    .nbr_run("netted", "grid"),
    .nbr_run("netted", "grid", surplus = collapsed)
  )
  testthat::expect_equal(
    .nbr_run("netted"),
    .nbr_run("netted", surplus = collapsed)
  )
})

testthat::test_that("the grassland-split components separate their parts", {
  # Cell B of the split fixture, 2015: managed = crop 2511 (100 ha, 15 t)
  # against 50 kg/ha on 100 ha = 5 t; extensive = grass 3000 (200 ha, 2 t)
  # against 30 kg/ha on 200 ha = 6 t. Split the crop 60/40 by area with the
  # irrigated part carrying 14 t: rainfed 1 t against 3 t, irrigated 14 t
  # against 2 t. Netted managed overshoot 10 t; separate 12 t.
  surplus <- .gs_surplus() |>
    dplyr::mutate(water_regime = "rainfed")
  b_crop <- surplus$lon == 0.75 & surplus$item_cbs_code == 2511L
  irrigated <- surplus[b_crop, ] |>
    dplyr::mutate(
      water_regime = "irrigated",
      area_ha = 40,
      surplus_n_t = 14,
      n_input_std_t = 14
    )
  surplus$area_ha[b_crop] <- 60
  surplus$surplus_n_t[b_crop] <- 1
  surplus$n_input_std_t[b_crop] <- 1
  surplus <- dplyr::bind_rows(surplus, irrigated)
  run <- \(mode, resolution) {
    suppressMessages(whep::build_n_boundary_exceedance(
      surplus = surplus,
      critical = .gs_critical(),
      land_use = "all",
      resolution = resolution,
      actual_year = 2015L,
      critical_reference_year = 2010L,
      grassland = .gs_grassland(),
      regime_comparison = mode
    ))
  }
  netted <- run("netted", "cell")
  separate <- run("separate", "cell")
  b <- .gs_cell_id("B")
  testthat::expect_equal(
    netted$managed_positive_overshoot_n_t[netted$cell_id == b],
    10
  )
  testthat::expect_equal(
    separate$managed_positive_overshoot_n_t[separate$cell_id == b],
    12
  )
  # Extensive headroom still never nets against managed excess, and the
  # managed allowance is unchanged.
  testthat::expect_equal(
    separate$cell_positive_overshoot_n_t[separate$cell_id == b],
    12
  )
  testthat::expect_equal(
    separate$managed_critical_n_t[separate$cell_id == b],
    5
  )
  # Every other cell has its pressure in one part only: nothing moves.
  others <- netted$cell_id != b
  testthat::expect_equal(
    separate$cell_positive_overshoot_n_t[others],
    netted$cell_positive_overshoot_n_t[others]
  )
  # The crop rows reconcile to their part, component and cell (asserted
  # inside), and the attributed exceedance is the cell exceedance.
  grid <- run("separate", "grid")
  testthat::expect_equal(
    sum(grid$exceedance_n_t) + sum(grid$unallocated_positive_overshoot_n_t),
    sum(separate$cell_positive_overshoot_n_t, na.rm = TRUE)
  )
})

testthat::test_that("separate refuses rows that carry no regime", {
  unsplit <- dplyr::select(.nbr_surplus(), -"water_regime")
  testthat::expect_error(
    .nbr_run("separate", surplus = unsplit),
    class = "whep_nbx_regime_missing"
  )
  # The netted comparison has nothing to refuse.
  testthat::expect_no_error(.nbr_run("netted", surplus = unsplit))
  odd <- .nbr_surplus()
  odd$water_regime[[2L]] <- NA_character_
  testthat::expect_error(
    .nbr_run("separate", surplus = odd),
    class = "whep_nbx_regime_bad"
  )
})

testthat::test_that("a part area that is missing aborts", {
  surplus <- .nbr_surplus()
  surplus$area_ha[[1L]] <- NA_real_
  testthat::expect_error(
    .nbr_run("separate", surplus = surplus),
    class = "whep_nbx_regime_area"
  )
})

testthat::test_that("a unit without area gives its allowance to its one part", {
  no_area <- .nbr_surplus() |>
    dplyr::mutate(
      area_ha = 0,
      surplus_n_t = dplyr::if_else(
        .data$water_regime == "rainfed",
        .data$surplus_n_t,
        0
      )
    )
  # Only the rainfed part carries pressure (0.5 t): it takes the whole 5 t
  # allowance, which is the netted comparison.
  separate <- .nbr_run("separate", surplus = no_area)
  testthat::expect_equal(separate$rainfed_critical_n_t, 5)
  testthat::expect_equal(separate$irrigated_critical_n_t, 0)
  testthat::expect_equal(
    separate$cell_positive_overshoot_n_t,
    .nbr_run("netted", surplus = no_area)$cell_positive_overshoot_n_t
  )
  both <- dplyr::mutate(.nbr_surplus(), area_ha = 0)
  testthat::expect_error(
    .nbr_run("separate", surplus = both),
    class = "whep_nbx_regime_no_area"
  )
})

testthat::test_that("a negative allowance is shared by area as it stands", {
  # keep: -20 kg/ha on 100 ha = -2 t, shared -1.4 t rainfed and -0.6 t
  # irrigated; overshoots 0.5 + 1.4 = 1.9 and 6.5 + 0.6 = 7.1.
  run <- \(negative_critical) {
    whep::build_n_boundary_exceedance(
      surplus = .nbr_surplus(),
      critical = .nbr_critical(value = -20),
      land_use = "ara",
      resolution = "cell",
      actual_year = 2010L,
      critical_reference_year = 2010L,
      negative_critical = negative_critical,
      regime_comparison = "separate"
    )
  }
  keep <- run("keep")
  testthat::expect_equal(keep$rainfed_positive_overshoot_n_t, 1.9)
  testthat::expect_equal(keep$irrigated_positive_overshoot_n_t, 7.1)
  testthat::expect_equal(keep$cell_positive_overshoot_n_t, 9)
  # clamp: a zero allowance, so each part overshoots by its own surplus.
  testthat::expect_equal(run("clamp")$cell_positive_overshoot_n_t, 7)
})

testthat::test_that("the country table compares the parts it was given", {
  surplus <- .nbr_surplus()
  ag_land <- tibble::tibble(
    area_code = c(1L, 2L),
    year = 2010L,
    area_ha = c(80, 20)
  )
  separate <- whep::build_n_boundary_country(
    .nbr_run("separate", "grid"),
    surplus,
    ag_land
  )
  netted <- whep::build_n_boundary_country(
    .nbr_run("netted", "grid"),
    surplus,
    ag_land
  )
  testthat::expect_equal(sum(separate$country$exceedance_n_t), 5)
  testthat::expect_equal(sum(netted$country$exceedance_n_t), 2)
  # Inputs are joined regime by regime, so none is counted twice.
  testthat::expect_equal(
    sum(separate$country$input_std_n_t),
    sum(surplus$n_input_std_t)
  )
  testthat::expect_equal(
    separate$country$input_std_n_t,
    netted$country$input_std_n_t
  )
  # Country 1's irrigated row lies in an exceeding part, its rainfed row in
  # a part within its allowance; netted, both lie in one exceeding cell.
  c1 <- dplyr::filter(separate$country, .data$area_code == 1L)
  testthat::expect_equal(c1$positive_surplus_n_t, 6.75)
  testthat::expect_equal(c1$exceeding_surplus_n_t, 6.5)
  c1_netted <- dplyr::filter(netted$country, .data$area_code == 1L)
  testthat::expect_equal(c1_netted$exceeding_surplus_n_t, 6.75)
  testthat::expect_equal(separate$diagnostics$exceedance_gap_n_t, 0)
})
