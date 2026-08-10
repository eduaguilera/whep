testthat::test_that("build_water_balance closes the water budget exactly", {
  wb <- whep::build_water_balance(example = TRUE)

  resid <- wb$water_input_mm -
    (wb$aet_mm + wb$runoff_mm + wb$drainage_mm + wb$soil_water_change_mm)
  testthat::expect_true(all(abs(resid) < 1e-6))

  pointblank::expect_col_exists(
    wb,
    c(
      "lon",
      "lat",
      "area_code",
      "year",
      "drainage_mm",
      "aet_mm",
      "water_input_mm",
      "method_water"
    )
  )
})

testthat::test_that("method_water records the aet, drainage and bg methods", {
  wb <- whep::build_water_balance(example = TRUE)
  testthat::expect_true(all(
    wb$method_water == "aet:components|drain:seepage|bg:cft_native"
  ))
})

testthat::test_that("residual drainage still closes the 4-term budget", {
  wb <- whep::build_water_balance(
    method = list(drainage = "residual"),
    example = TRUE
  )
  resid <- wb$water_input_mm -
    (wb$aet_mm + wb$runoff_mm + wb$drainage_mm + wb$soil_water_change_mm)
  testthat::expect_true(all(abs(resid) < 1e-6))
  testthat::expect_true(all(
    wb$method_water == "aet:components|drain:residual|bg:cft_native"
  ))
})

testthat::test_that("example exposes footprint columns and prec+irrig split", {
  wb <- whep::build_water_balance(example = TRUE)

  pointblank::expect_col_exists(
    wb,
    c(
      "prec_mm",
      "irrig_mm",
      "blue_consump_mm",
      "green_consump_mm",
      "cft_nir_mm"
    )
  )
  # water input is precipitation plus irrigation, exactly.
  split_resid <- wb$water_input_mm - (wb$prec_mm + wb$irrig_mm)
  testthat::expect_true(all(abs(split_resid) < 1e-6))
})

testthat::test_that("get_soc_climate_drivers returns monthly climate drivers", {
  drv <- whep::get_soc_climate_drivers(example = TRUE)
  pointblank::expect_col_exists(
    drv,
    c(
      "lon",
      "lat",
      "area_code",
      "year",
      "month",
      "temp_c",
      "swc_topsoil",
      "clay_pct"
    )
  )
  pointblank::expect_col_vals_between(drv, "month", 1, 12)
})

# ---- get_soc_climate_drivers real-path wiring (CRU temp + LPJmL prec/irrig -
# CRU pet). Inject synthetic CRU + LPJmL monthly inputs so no NetCDF is read.

.socd_synthetic <- function() {
  cells <- tibble::tribble(
    ~lon, ~lat,
    9.25, 47.75,
    -3.25, 40.25
  )
  months <- tidyr::expand_grid(cells, year = 2000L, month = 1:12)
  # temp in degrees Celsius; pet in mm per day; prec and irrig in mm per month.
  temp <- dplyr::mutate(months, value = 5 + 10 * (month / 12))
  pet <- dplyr::mutate(months, value = 1 + 2 * (month / 12))
  prec <- dplyr::mutate(months, value = 60)
  irrig <- dplyr::mutate(months, value = 5)
  swc <- tidyr::expand_grid(cells, year = 2000L, month = 1:12, layer = 1:2) |>
    dplyr::mutate(value = dplyr::if_else(layer == 1L, 0.45, 0.40))
  clay <- dplyr::mutate(cells, clay_pct = 22)
  cell_polity <- dplyr::mutate(cells, area_code = c(11L, 203L))
  # Injected loam-class soil hydraulics (t_field 0.29, t_wilt 0.14, porosity
  # 0.43) so the ICBM driver path never reaches the real HWSD reader.
  soil_hydraulic <- dplyr::mutate(
    cells,
    t_field = 0.29,
    t_wilt = 0.14,
    porosity = 0.43
  )
  list(
    temp = temp,
    pet = pet,
    prec = prec,
    irrig = irrig,
    swc = swc,
    clay = clay,
    cell_polity = cell_polity,
    soil_hydraulic = soil_hydraulic
  )
}

testthat::test_that("get_soc_climate_drivers wires CRU temp + prec+irrig-PET", {
  drv <- whep::get_soc_climate_drivers(data = .socd_synthetic())
  pointblank::expect_col_exists(
    drv,
    c(
      "lon",
      "lat",
      "area_code",
      "year",
      "month",
      "temp_c",
      "swc_topsoil",
      "water_minus_pet_mm",
      "clay_pct",
      "method_water_input"
    )
  )
  # temp_c comes straight from the CRU tmp input.
  jan <- dplyr::filter(drv, month == 1L, area_code == 11L)
  testthat::expect_equal(jan$temp_c, 5 + 10 / 12, tolerance = 1e-8)
  # The water surplus is precipitation plus irrigation minus PET as a monthly
  # total (PET mm per day times the days in the month). January 2000 has 31
  # days and PET per day of one plus two-twelfths.
  expected_wmp <- (60 + 5) - (1 + 2 / 12) * 31
  testthat::expect_equal(jan$water_minus_pet_mm, expected_wmp, tolerance = 1e-6)
  # area_code arrives from the cell-polity crosswalk; swc from LPJmL topsoil.
  testthat::expect_true(all(drv$swc_topsoil == 0.45))
  testthat::expect_setequal(unique(drv$area_code), c(11L, 203L))
})

testthat::test_that("SOC LPJmL readers receive the requested run directory", {
  data <- .socd_synthetic()
  lpjml <- data[c("prec", "irrig")]
  data$prec <- NULL
  data$irrig <- NULL
  calls <- list()
  testthat::local_mocked_bindings(
    read_lpjml_hydrology = function(
      var,
      run_dir = NULL,
      years = NULL,
      monthly = FALSE,
      ...
    ) {
      calls[[length(calls) + 1L]] <<- list(
        var = var,
        run_dir = run_dir,
        years = years,
        monthly = monthly
      )
      lpjml[[var]]
    },
    .package = "whep"
  )

  whep::get_soc_climate_drivers(
    run_dir = "/tmp/intended-lpjml-run",
    years = 2000L,
    data = data
  )

  testthat::expect_setequal(
    vapply(calls, `[[`, character(1), "var"),
    c("prec", "irrig")
  )
  testthat::expect_true(all(vapply(
    calls,
    \(x) identical(x$run_dir, "/tmp/intended-lpjml-run"),
    logical(1)
  )))
  testthat::expect_true(all(vapply(
    calls,
    \(x) identical(x$years, 2000L) && isTRUE(x$monthly),
    logical(1)
  )))
})

testthat::test_that("SOC years filter also applies to injected inputs", {
  data <- .socd_synthetic()
  time_inputs <- c("temp", "pet", "prec", "irrig", "swc")
  for (name in time_inputs) {
    data[[name]] <- dplyr::bind_rows(
      data[[name]],
      dplyr::mutate(data[[name]], year = 2001L)
    )
  }

  out <- whep::get_soc_climate_drivers(years = 2000L, data = data)

  testthat::expect_setequal(out$year, 2000L)
  testthat::expect_equal(nrow(out), 24L)
})

testthat::test_that("SOC drivers feed a plausible HSOC modifier", {
  drv <- whep::get_soc_climate_drivers(data = .socd_synthetic())
  one <- dplyr::filter(drv, area_code == 11L) |> dplyr::arrange(month)
  cm <- whep::soc_rate_modifier_rothc(
    temp_c = one$temp_c,
    water_minus_pet_mm = one$water_minus_pet_mm,
    clay_pct = one$clay_pct[1],
    soil_cover = 0
  )
  testthat::expect_true(is.finite(cm))
  testthat::expect_gt(cm, 0)
  testthat::expect_lt(cm, 2)
})

testthat::test_that("SOC drivers emit the Century and AMG climate columns", {
  drv <- whep::get_soc_climate_drivers(data = .socd_synthetic())
  pointblank::expect_col_exists(
    drv,
    c("precip_mm", "pet_mm", "water_balance_mm")
  )
  one <- dplyr::filter(drv, area_code == 11L) |> dplyr::arrange(month)
  # precip_mm is precipitation only (irrigation excluded), from the LPJmL prec
  # input of 60 mm every month; pet_mm is the CRU mm/day times days in month.
  testthat::expect_true(all(one$precip_mm == 60))
  testthat::expect_equal(one$pet_mm[1], (1 + 2 / 12) * 31, tolerance = 1e-8)
  # water_balance_mm is the annual sum of the monthly water surplus, identical
  # on every month of the cell-year (the AMG per-cell-year scalar).
  testthat::expect_equal(
    unique(one$water_balance_mm),
    sum(one$water_minus_pet_mm),
    tolerance = 1e-8
  )
  testthat::expect_length(unique(one$water_balance_mm), 1)
})

testthat::test_that("SOC drivers drive non-neutral Century and AMG modifiers", {
  drv <- whep::get_soc_climate_drivers(data = .socd_synthetic())
  one <- dplyr::filter(drv, area_code == 11L) |> dplyr::arrange(month)
  century <- whep::soc_rate_modifier_century(
    temp_c = one$temp_c,
    precip_mm = one$precip_mm,
    pet_mm = one$pet_mm
  )
  amg <- whep::soc_rate_modifier_amg(
    temp_c = one$temp_c,
    water_balance_mm = one$water_balance_mm
  )
  # A finite modifier that is NOT exactly 1 confirms the drivers reach the
  # modifier rather than the neutral fallback the missing columns used to force.
  testthat::expect_true(is.finite(century) && century > 0 && century != 1)
  testthat::expect_true(is.finite(amg) && amg > 0 && amg != 1)
  testthat::expect_equal(century, 0.19223662444227, tolerance = 1e-6)
  testthat::expect_equal(amg, 0.603259664579879, tolerance = 1e-6)
})

testthat::test_that("SOC drivers emit the ICBM moisture columns", {
  drv <- whep::get_soc_climate_drivers(data = .socd_synthetic())
  pointblank::expect_col_exists(
    drv,
    c("theta", "t_field", "t_wilt", "porosity")
  )
  # The injected loam-class references arrive unchanged, and theta is the
  # topsoil fractional saturation times the cell porosity, NOT the 0.4
  # whole-profile constant used elsewhere in the water balance.
  testthat::expect_true(all(drv$t_field == 0.29))
  testthat::expect_true(all(drv$t_wilt == 0.14))
  testthat::expect_true(all(drv$porosity == 0.43))
  testthat::expect_equal(drv$theta, drv$swc_topsoil * drv$porosity)
  # swc_topsoil is 0.45 in the fixture, so theta is 0.45 * 0.43, well above the
  # 0.4 * 0.45 the old whole-profile porosity constant would have given.
  testthat::expect_true(all(abs(drv$theta - 0.45 * 0.43) < 1e-9))
  testthat::expect_false(isTRUE(all.equal(unique(drv$theta), 0.45 * 0.4)))
})

testthat::test_that("the texture-class hydraulic table is physically ordered", {
  hyd <- whep::soil_hydraulic_by_texture
  # Every USDA class: wilting point < field capacity < porosity, all in (0, 1).
  testthat::expect_true(all(hyd$wilting_point > 0 & hyd$wilting_point < 1))
  testthat::expect_true(all(hyd$field_capacity > hyd$wilting_point))
  testthat::expect_true(all(hyd$porosity > hyd$field_capacity))
  testthat::expect_true(all(hyd$porosity < 1))
  # The HWSD code crosswalk resolves every code 1..13 to a class in the table.
  joined <- dplyr::inner_join(
    whep::hwsd_texture_usda,
    hyd,
    by = "usda_texture_class"
  )
  testthat::expect_equal(nrow(joined), 13L)
  # Spot-check the two clay codes (heavy = 1, light = 3) both map to clay
  # (porosity 0.47), and code 13 maps to sand (porosity 0.43).
  clay_codes <- joined[joined$t_usda_tex %in% c(1L, 3L), ]
  testthat::expect_true(all(clay_codes$usda_texture_class == "clay"))
  testthat::expect_true(all(clay_codes$porosity == 0.47))
  sand <- joined[joined$t_usda_tex == 13L, ]
  testthat::expect_equal(sand$field_capacity, 0.08)
})

testthat::test_that("SOC drivers drive a non-neutral ICBM modifier end-to-end", {
  drv <- whep::get_soc_climate_drivers(data = .socd_synthetic())
  one <- dplyr::filter(drv, area_code == 11L) |> dplyr::arrange(month)
  # The all-present check in .soc_climate_drivers("icbm") now succeeds, so the
  # ICBM moisture response actually runs instead of the neutral-1 fallback.
  icbm <- whep::soc_rate_modifier_icbm(
    temp_c = one$temp_c,
    theta = one$theta,
    t_field = one$t_field,
    t_wilt = one$t_wilt,
    porosity = one$porosity
  )
  testthat::expect_true(is.finite(icbm) && icbm > 0 && icbm != 1)
  # Feeding the four ICBM drivers into calculate_soc_dynamics(model = "icbm")
  # yields a trajectory that DIFFERS from the neutral-modifier one.
  driven <- whep::calculate_soc_dynamics(
    model = "icbm",
    data = list(
      initial_soc_mgc_ha = 50,
      c_input_mgc_ha_yr = 2,
      years = 5,
      temp_c = one$temp_c,
      theta = one$theta,
      t_field = one$t_field[1],
      t_wilt = one$t_wilt[1],
      porosity = one$porosity[1]
    )
  )
  neutral <- whep::calculate_soc_dynamics(
    model = "icbm",
    data = list(
      initial_soc_mgc_ha = 50,
      c_input_mgc_ha_yr = 2,
      years = 5,
      climate_modifier = 1
    )
  )
  final_driven <- dplyr::last(driven$soc_total)
  final_neutral <- dplyr::last(neutral$soc_total)
  testthat::expect_true(is.finite(final_driven))
  testthat::expect_gt(abs(final_driven - final_neutral), 1e-6)
})

# ---- Real-data smoke test (skip if CRU dir absent): read a few 2000 cells.
testthat::test_that("get_soc_climate_drivers reads real CRU for a few cells", {
  cru_dir <- Sys.getenv("WHEP_CRU_DIR", "")
  testthat::skip_if_not(
    file.exists(file.path(cru_dir, "cru_ts4.09.1901.2024.tmp.dat.nc")),
    "CRU TS 4.09 tmp file not found."
  )
  tmp <- whep::read_cru_climate("tmp", years = 2000L)
  pet <- whep::read_cru_climate("pet", years = 2000L)
  # A handful of European land cells.
  sel <- tmp |>
    dplyr::filter(lon > -5, lon < 5, lat > 40, lat < 50) |>
    dplyr::distinct(lon, lat) |>
    head(4)
  temp_sel <- dplyr::semi_join(tmp, sel, by = c("lon", "lat"))
  pet_sel <- dplyr::semi_join(pet, sel, by = c("lon", "lat"))
  testthat::expect_true(all(is.finite(temp_sel$value)))
  testthat::expect_true(all(temp_sel$value > -40 & temp_sel$value < 40))
  # PET in mm/day is small and positive.
  testthat::expect_true(all(pet_sel$value >= 0 & pet_sel$value < 20))
})

testthat::test_that("polity resolution aggregates by year and area_code", {
  grid <- whep::build_water_balance(resolution = "grid", example = TRUE)
  pol <- whep::build_water_balance(resolution = "polity", example = TRUE)

  pointblank::expect_col_exists(pol, c("year", "area_code"))
  testthat::expect_false(rlang::has_name(pol, "lon"))
  testthat::expect_equal(
    nrow(pol),
    nrow(dplyr::distinct(grid, year, area_code))
  )
})

testthat::test_that("an invalid drainage method is rejected", {
  testthat::expect_error(
    whep::build_water_balance(method = list(drainage = "nonsense")),
    "drainage"
  )
})

# ---- Real-path closure test (exercises read -> compute -> blue_green ->
# finalise on synthetic per-cell MONTHLY inputs; NOT the fixture). The synthetic
# inputs are constructed so the 4-term budget closes exactly, i.e. precipitation
# plus irrigation equals AET plus runoff plus seepage plus the storage change.

# Build synthetic monthly LPJmL-style inputs for `n_cells` cells x 12 months x
# one year. Soil-water saturation drops linearly Jan -> Dec so dStorage != 0 and
# the storage term participates. prec is solved so the 4-term budget closes.
.wb_synthetic_monthly <- function() {
  cells <- tibble::tribble(
    ~lon, ~lat, ~area_code,
    9.25, 47.75, 11L,
    -55.25, -12.25, 21L
  )
  porosity <- 0.4
  thickness_mm <- c(200, 300, 500, 1000, 1000, 10000)
  swc_jan <- c(0.50, 0.45, 0.40, 0.35, 0.30, 0.25)
  swc_dec <- c(0.40, 0.38, 0.34, 0.31, 0.28, 0.24)
  d_storage_mm <- sum((swc_dec - swc_jan) * thickness_mm * porosity)

  months <- 1:12
  flux <- tidyr::expand_grid(cells, month = months) |>
    dplyr::mutate(
      year = 2000L,
      transp = 40 + lon * 0,
      evap = 15,
      interc = 5,
      irrig = 8,
      runoff = 12,
      seepage = 10
    )
  aet_annual <- (40 + 15 + 5) * 12
  irrig_annual <- 8 * 12
  runoff_annual <- 12 * 12
  seepage_annual <- 10 * 12
  water_input_annual <- aet_annual +
    runoff_annual +
    seepage_annual +
    d_storage_mm
  prec_monthly <- (water_input_annual - irrig_annual) / 12
  flux <- dplyr::mutate(flux, prec = prec_monthly)

  swc <- tidyr::expand_grid(
    cells,
    month = months,
    layer = seq_along(swc_jan)
  ) |>
    dplyr::mutate(
      year = 2000L,
      value = swc_jan[layer] +
        (swc_dec[layer] - swc_jan[layer]) * (month - 1) / 11
    )

  to_long <- function(var) {
    dplyr::select(flux, lon, lat, year, month, value = dplyr::all_of(var))
  }
  cell_polity <- dplyr::mutate(cells, polity_frac = 1, cell_area_ha = 30000)
  list(
    inputs = list(
      transp = to_long("transp"),
      evap = to_long("evap"),
      interc = to_long("interc"),
      prec = to_long("prec"),
      irrig = to_long("irrig"),
      runoff = to_long("runoff"),
      seepage = to_long("seepage"),
      swc = swc,
      cell_polity = cell_polity
    ),
    water_input_annual = water_input_annual
  )
}

testthat::test_that("real-path 4-term budget closes within 1% (runoff included)", {
  syn <- .wb_synthetic_monthly()
  wb <- suppressWarnings(
    whep::build_water_balance(data = syn$inputs, example = FALSE)
  )

  resid <- wb$water_input_mm -
    (wb$aet_mm + wb$runoff_mm + wb$drainage_mm + wb$soil_water_change_mm)
  testthat::expect_true(all(abs(resid) < 0.01 * wb$water_input_mm))
  testthat::expect_true(all(wb$runoff_mm > 0))
  pointblank::expect_col_exists(wb, c("runoff_mm", "drainage_mm"))
})

testthat::test_that("residual drainage does not read an absent seepage file", {
  inputs <- .wb_synthetic_monthly()$inputs
  inputs$seepage <- NULL
  testthat::local_mocked_bindings(
    read_lpjml_hydrology = function(...) {
      stop("no hydrology reader should be called for fully injected inputs")
    },
    .package = "whep"
  )

  wb <- suppressWarnings(whep::build_water_balance(
    method = list(drainage = "residual"),
    data = inputs
  ))

  testthat::expect_true(all(is.finite(wb$drainage_mm)))
  testthat::expect_true(all(stringr::str_detect(
    wb$method_water,
    "drain:residual"
  )))
})

testthat::test_that("real-path method_water records the blue_green choice", {
  syn <- .wb_synthetic_monthly()
  wb <- suppressWarnings(
    whep::build_water_balance(data = syn$inputs, example = FALSE)
  )
  testthat::expect_true(all(stringr::str_detect(wb$method_water, "bg:")))
})

testthat::test_that("cft_native without per-CFT data warns and falls back", {
  syn <- .wb_synthetic_monthly()
  testthat::expect_warning(
    wb <- whep::build_water_balance(data = syn$inputs, example = FALSE),
    "per-CFT consumptive water"
  )
  testthat::expect_true(all(
    stringr::str_detect(wb$method_water, "bg:irrig_share_fallback")
  ))
})

testthat::test_that("cft_native uses per-CFT consumptive water when supplied", {
  syn <- .wb_synthetic_monthly()
  cells <- dplyr::distinct(syn$inputs$prec, lon, lat, year)
  syn$inputs$cft_consump_water_b <- dplyr::mutate(cells, value = 120)
  syn$inputs$cft_consump_water_g <- dplyr::mutate(cells, value = 280)

  wb <- whep::build_water_balance(data = syn$inputs, example = FALSE)
  testthat::expect_true(all(stringr::str_detect(
    wb$method_water,
    "bg:cft_native"
  )))
  # blue share = 120 / (120 + 280) = 0.3 of total AET.
  testthat::expect_equal(wb$aet_blue_mm, wb$aet_mm * 0.3, tolerance = 1e-8)
})

testthat::test_that("real-path rejects an invalid drainage method", {
  syn <- .wb_synthetic_monthly()
  testthat::expect_error(
    whep::build_water_balance(
      method = list(drainage = "nonsense"),
      data = syn$inputs
    ),
    "drainage"
  )
})

testthat::test_that("real-path exposes prec/irrig split and footprint cols", {
  syn <- .wb_synthetic_monthly()
  wb <- suppressWarnings(
    whep::build_water_balance(data = syn$inputs, example = FALSE)
  )

  pointblank::expect_col_exists(
    wb,
    c(
      "prec_mm",
      "irrig_mm",
      "blue_consump_mm",
      "green_consump_mm",
      "cft_nir_mm"
    )
  )
  split_resid <- wb$water_input_mm - (wb$prec_mm + wb$irrig_mm)
  testthat::expect_true(all(abs(split_resid) < 1e-6))
  # 4-term closure STILL holds alongside the additive prec/irrig split.
  resid <- wb$water_input_mm -
    (wb$aet_mm + wb$runoff_mm + wb$drainage_mm + wb$soil_water_change_mm)
  testthat::expect_true(all(abs(resid) < 0.01 * wb$water_input_mm))
})

testthat::test_that("blue/green consumptive equal the per-CFT mm summed", {
  syn <- .wb_synthetic_monthly()
  cells <- dplyr::distinct(syn$inputs$prec, lon, lat, year)
  # Two crop bands per cell so the per-cell sum is exercised.
  syn$inputs$cft_consump_water_b <- dplyr::bind_rows(
    dplyr::mutate(cells, value = 50),
    dplyr::mutate(cells, value = 70)
  )
  syn$inputs$cft_consump_water_g <- dplyr::bind_rows(
    dplyr::mutate(cells, value = 100),
    dplyr::mutate(cells, value = 180)
  )

  wb <- whep::build_water_balance(data = syn$inputs, example = FALSE)
  # cft_consump_water_b summed over bands gives blue_consump_mm (50 + 70).
  testthat::expect_equal(
    wb$blue_consump_mm,
    rep(120, nrow(wb)),
    tolerance = 1e-8
  )
  testthat::expect_equal(
    wb$green_consump_mm,
    rep(280, nrow(wb)),
    tolerance = 1e-8
  )
})

testthat::test_that("bands selects which CFT bands are summed", {
  syn <- .wb_synthetic_monthly()
  cells <- dplyr::distinct(syn$inputs$prec, lon, lat, year)
  syn$inputs$cft_consump_water_g <- dplyr::bind_rows(
    dplyr::mutate(cells, band = 3L, band_name = "rainfed maize", value = 180),
    dplyr::mutate(
      cells,
      band = 14L,
      band_name = "rainfed grassland",
      value = 100
    )
  )
  syn$inputs$cft_consump_water_b <- dplyr::mutate(
    cells,
    band = 14L,
    band_name = "rainfed grassland",
    value = 20
  )

  grass <- whep::build_water_balance(
    data = syn$inputs,
    bands = "rainfed grassland"
  )
  # Grassland alone (100), not the whole-cell total (100 + 180).
  testthat::expect_equal(
    grass$green_consump_mm,
    rep(100, nrow(grass)),
    tolerance = 1e-8
  )

  # The default still totals every band, so existing callers are unaffected.
  all_bands <- whep::build_water_balance(data = syn$inputs)
  testthat::expect_equal(
    all_bands$green_consump_mm,
    rep(280, nrow(all_bands)),
    tolerance = 1e-8
  )
})

testthat::test_that("an unknown or unnameable band aborts", {
  syn <- .wb_synthetic_monthly()
  cells <- dplyr::distinct(syn$inputs$prec, lon, lat, year)
  named <- dplyr::mutate(
    cells,
    band = 14L,
    band_name = "rainfed grassland",
    value = 100
  )
  syn$inputs$cft_consump_water_b <- named
  syn$inputs$cft_consump_water_g <- named

  testthat::expect_error(
    whep::build_water_balance(data = syn$inputs, bands = "rainfed sorghum"),
    "not in this input"
  )

  # A band-name-less input cannot be filtered: aborting beats silently
  # returning the whole-cell total under a grassland-only request.
  syn$inputs$cft_consump_water_g <- dplyr::select(named, -band_name)
  testthat::expect_error(
    whep::build_water_balance(data = syn$inputs, bands = "rainfed grassland"),
    "band_name"
  )
})

testthat::test_that("polity resolution carries the new footprint columns", {
  pol <- whep::build_water_balance(resolution = "polity", example = TRUE)
  pointblank::expect_col_exists(
    pol,
    c(
      "prec_mm",
      "irrig_mm",
      "blue_consump_mm",
      "green_consump_mm",
      "cft_nir_mm"
    )
  )
  # cft_nir_mm is all-NA in the fixture; the all-NA guard keeps it NA, not NaN.
  testthat::expect_true(all(is.na(pol$cft_nir_mm)))
})

testthat::test_that("a single NA-weight cell does not poison the polity mean", {
  # Put both synthetic cells in one polity, then give the second cell an NA
  # cell_area_ha (a border/coastal crosswalk row). The polity aggregate must be
  # the first cell's value, not NA for the whole polity.
  syn <- .wb_synthetic_monthly()
  cells <- dplyr::distinct(syn$inputs$prec, lon, lat)
  syn$inputs$cell_polity <- tibble::tibble(
    lon = cells$lon,
    lat = cells$lat,
    area_code = 11L,
    polity_frac = 1,
    cell_area_ha = c(30000, NA_real_)
  )
  grid <- suppressWarnings(
    whep::build_water_balance(data = syn$inputs, resolution = "grid")
  )
  pol <- suppressWarnings(
    whep::build_water_balance(data = syn$inputs, resolution = "polity")
  )
  keep_cell <- dplyr::filter(grid, lon == cells$lon[1], lat == cells$lat[1])
  # Exactly one polity-year row, and every depth column finite (not NA) and
  # equal to the surviving (non-NA-weight) cell's value.
  testthat::expect_equal(nrow(pol), 1L)
  testthat::expect_false(is.na(pol$water_input_mm))
  testthat::expect_equal(
    pol$water_input_mm,
    keep_cell$water_input_mm,
    tolerance = 1e-6
  )
  testthat::expect_equal(pol$aet_mm, keep_cell$aet_mm, tolerance = 1e-6)
  testthat::expect_equal(
    pol$drainage_mm,
    keep_cell$drainage_mm,
    tolerance = 1e-6
  )
})

testthat::test_that("zero-area polity weights return NA rather than NaN", {
  all_zero <- whep:::.wb_weighted_mean(c(1, 2), c(0, 0))
  one_valid <- whep:::.wb_weighted_mean(c(1, 3), c(0, 2))

  testthat::expect_true(is.na(all_zero))
  testthat::expect_false(is.nan(all_zero))
  testthat::expect_equal(one_valid, 3)
})

# Two cells in one polity with DIFFERENT per-cell flux totals (so
# water_input_mm/aet_mm genuinely differ) and DIFFERENT weights
# (polity_frac * cell_area_ha). Each cell's own 4-term budget closes exactly
# (no runoff/drainage, single-layer swc held constant so soil_water_change is
# 0), which keeps the fixture simple while still varying the two depth values
# the weighted mean must respect.
.wb_two_cell_diff_depths <- function() {
  cells <- tibble::tribble(
    ~lon, ~lat, ~area_code, ~prec, ~irrig, ~aet_total,
    9.25, 47.75, 11L, 500, 0, 300,
    -3.25, 40.25, 11L, 900, 0, 300
  )
  months <- tidyr::expand_grid(
    dplyr::select(cells, lon, lat, area_code, prec, irrig, aet_total),
    month = 1:12
  ) |>
    dplyr::mutate(
      year = 2000L,
      transp = aet_total / 12,
      evap = 0,
      interc = 0,
      runoff = 0,
      seepage = 0,
      prec_m = prec / 12,
      irrig_m = irrig / 12
    )
  to_long <- function(var) {
    dplyr::select(months, lon, lat, year, month, value = dplyr::all_of(var))
  }
  swc <- tidyr::expand_grid(
    dplyr::select(cells, lon, lat),
    year = 2000L,
    month = 1:12,
    layer = 1L
  ) |>
    dplyr::mutate(value = 0.3)
  cell_polity <- tibble::tibble(
    lon = cells$lon,
    lat = cells$lat,
    area_code = cells$area_code,
    polity_frac = 1,
    cell_area_ha = c(10000, 90000)
  )
  list(
    transp = to_long("transp"),
    evap = to_long("evap"),
    interc = to_long("interc"),
    prec = to_long("prec_m"),
    irrig = to_long("irrig_m"),
    runoff = to_long("runoff"),
    seepage = to_long("seepage"),
    swc = swc,
    cell_polity = cell_polity
  )
}

testthat::test_that("polity mean is genuinely area-weighted, not a plain mean", {
  # water_input_mm (prec) is 500 for the light-weight cell (10000 ha) and 900
  # for the heavy-weight cell (90000 ha). A plain unweighted mean would give
  # 700; the area-weighted mean is much closer to 900, so this fails if
  # .wb_aggregate_polity() silently used mean() instead of
  # stats::weighted.mean().
  inputs <- .wb_two_cell_diff_depths()
  grid <- suppressWarnings(
    whep::build_water_balance(data = inputs, resolution = "grid")
  )
  pol <- suppressWarnings(
    whep::build_water_balance(data = inputs, resolution = "polity")
  )
  grid <- dplyr::inner_join(grid, inputs$cell_polity, by = c("lon", "lat"))
  expected_water_input <- stats::weighted.mean(
    grid$water_input_mm,
    grid$cell_area_ha
  )
  unweighted_water_input <- mean(grid$water_input_mm)

  testthat::expect_equal(nrow(pol), 1L)
  testthat::expect_equal(
    pol$water_input_mm,
    expected_water_input,
    tolerance = 1e-8
  )
  testthat::expect_equal(pol$water_input_mm, 860, tolerance = 1e-8)
  # Sanity: the two cells' depths and weights genuinely differ, so the
  # weighted and unweighted means are not coincidentally equal.
  testthat::expect_true(
    abs(expected_water_input - unweighted_water_input) > 1e-3
  )
})

testthat::test_that("a genuinely mixed NA-weight polity keeps only the valid cells' weighted mean", {
  # T10a fix regression (R/water_balance.R .wb_weighted_mean): three cells in
  # one polity, two with DIFFERENT valid depths/weights (the 500/10000 and
  # 900/90000 fixture cells) and a third with an NA cell_area_ha. The polity
  # aggregate must equal the weighted mean of the two VALID cells only, not NA
  # and not a mean that folds in the NA-weight cell.
  inputs <- .wb_two_cell_diff_depths()
  third_cell <- tibble::tribble(
    ~lon, ~lat, ~area_code,
    50.25, 10.25, 11L
  )
  months <- tidyr::expand_grid(third_cell, month = 1:12) |>
    dplyr::mutate(
      year = 2000L,
      transp = 60,
      evap = 0,
      interc = 0,
      irrig = 0,
      runoff = 0,
      seepage = 0,
      prec = 100
    )
  swc_extra <- tidyr::expand_grid(third_cell, month = 1:12, layer = 1L) |>
    dplyr::mutate(year = 2000L, value = 0.3)
  to_long <- function(data, var) {
    dplyr::select(data, lon, lat, year, month, value = dplyr::all_of(var))
  }
  inputs$transp <- dplyr::bind_rows(inputs$transp, to_long(months, "transp"))
  inputs$evap <- dplyr::bind_rows(inputs$evap, to_long(months, "evap"))
  inputs$interc <- dplyr::bind_rows(inputs$interc, to_long(months, "interc"))
  inputs$prec <- dplyr::bind_rows(inputs$prec, to_long(months, "prec"))
  inputs$irrig <- dplyr::bind_rows(inputs$irrig, to_long(months, "irrig"))
  inputs$runoff <- dplyr::bind_rows(inputs$runoff, to_long(months, "runoff"))
  inputs$seepage <- dplyr::bind_rows(inputs$seepage, to_long(months, "seepage"))
  inputs$swc <- dplyr::bind_rows(inputs$swc, swc_extra)

  cells <- dplyr::distinct(inputs$prec, lon, lat)
  inputs$cell_polity <- tibble::tibble(
    lon = cells$lon,
    lat = cells$lat,
    area_code = 11L,
    polity_frac = 1,
    cell_area_ha = c(10000, 90000, NA_real_)
  )

  grid <- suppressWarnings(
    whep::build_water_balance(data = inputs, resolution = "grid")
  )
  pol <- suppressWarnings(
    whep::build_water_balance(data = inputs, resolution = "polity")
  )
  valid_grid <- dplyr::filter(grid, lon != third_cell$lon) |>
    dplyr::inner_join(inputs$cell_polity, by = c("lon", "lat"))
  expected_water_input <- stats::weighted.mean(
    valid_grid$water_input_mm,
    valid_grid$cell_area_ha
  )

  testthat::expect_equal(nrow(pol), 1L)
  testthat::expect_false(is.na(pol$water_input_mm))
  testthat::expect_equal(
    pol$water_input_mm,
    expected_water_input,
    tolerance = 1e-8
  )
  testthat::expect_equal(pol$water_input_mm, 860, tolerance = 1e-8)
})

testthat::test_that(".wb_attach_polity drops unsimulated cells and warns (#381)", {
  # A, B: simulated (finite drainage) and in the crosswalk -> kept.
  # C: in the crosswalk but no model data (non-finite drainage, a crosswalk
  #    cell outside the simulated grid) -> must be dropped, not passed as NaN.
  # E: simulated but absent from the crosswalk -> warned (would be dropped by
  #    the join and silently lost from the polity aggregation).
  terms <- tibble::tribble(
    ~lon, ~lat, ~drainage_mm, ~aet_mm,
    0.25, 0.25, 100, 1,
    0.75, 0.25, 120, 1,
    0.25, 0.75, NaN, 1,
    0.75, 0.75, 90, 1
  )
  crosswalk <- tibble::tribble(
    ~lon, ~lat, ~area_code, ~polity_frac, ~cell_area_ha,
    0.25, 0.25, 11L, 1, 30000,
    0.75, 0.25, 203L, 1, 30000,
    0.25, 0.75, 250L, 1, 30000,
    0.15, 0.15, 76L, 1, 30000
  )
  warnings <- character(0)
  out <- withCallingHandlers(
    whep:::.wb_attach_polity(terms, list(cell_polity = crosswalk)),
    warning = function(cond) {
      warnings <<- c(warnings, conditionMessage(cond))
      invokeRestart("muffleWarning")
    }
  )
  testthat::expect_true(any(grepl("no polity", warnings)))
  testthat::expect_true(any(grepl("no LPJmL model data", warnings)))
  testthat::expect_setequal(
    paste(out$lon, out$lat),
    c("0.25 0.25", "0.75 0.25")
  )
  testthat::expect_true(all(is.finite(out$drainage_mm)))
})

testthat::test_that("regions.csv carries the iso3c->area_code crosswalk (#381 guard)", {
  # inst/scripts/prepare_spatialize_all.R maps iso3c -> area_code via
  # inst/extdata/regions.csv for both the country grid and the cell x polity
  # crosswalk. Guard the columns it needs so a future schema drift (like the
  # polities restructure that removed area_code, which silently broke both
  # rasterisation sections) fails loudly here instead.
  path <- system.file("extdata", "regions.csv", package = "whep")
  testthat::expect_true(nzchar(path))
  regions <- utils::read.csv(path, stringsAsFactors = FALSE)
  testthat::expect_true(all(c("iso3c", "area_code") %in% names(regions)))
  testthat::expect_true(any(!is.na(regions$area_code)))
})

# ---- The LPJmL hydrology pin seam ------------------------------------------
# get_soc_climate_drivers() must work without an LPJmL run. Only the three
# LPJmL monthly drivers are pinned; CRU temperature and the HWSD texture
# products stay local because a user can download both.

.socd_pin_fixture <- function() {
  tibble::tribble(
    ~lon, ~lat, ~year, ~month, ~swc_topsoil, ~prec_mm, ~irrig_mm,
    0.25, 40.25, 2000L, 1L, 0.4, 50, 5,
    0.25, 40.25, 2000L, 2L, 0.5, 60, 0
  )
}

testthat::test_that("no pin is fetched when a run directory is available", {
  withr::local_envvar(WHEP_LPJML_RUN_DIR = "/some/run")
  testthat::expect_null(whep:::.socd_pin_hydrology(list(), NULL, NULL))
  testthat::expect_null(
    whep:::.socd_pin_hydrology(list(), "/explicit/run", NULL)
  )
})

testthat::test_that("no pin is fetched when every LPJmL var is supplied", {
  withr::local_envvar(WHEP_LPJML_RUN_DIR = "")
  supplied <- list(swc = "a", prec = "b", irrig = "c")
  testthat::expect_null(whep:::.socd_pin_hydrology(supplied, NULL, NULL))
})

testthat::test_that("a partial injection still needs the pin", {
  # Only prec supplied: swc and irrig must still be sourced, so the pin is
  # needed. Guards against a partial override silently zeroing the others.
  # Asserts the decision, not a failed fetch, so it never touches the network.
  withr::local_envvar(WHEP_LPJML_RUN_DIR = "")
  testthat::expect_true(
    whep:::.socd_needs_pin(list(prec = .socd_pin_fixture()), NULL)
  )
  testthat::expect_false(
    whep:::.socd_needs_pin(list(prec = 1, swc = 1, irrig = 1), NULL)
  )
})

testthat::test_that("pin columns are reshaped to the reader contract", {
  out <- whep:::.socd_pin_var(.socd_pin_fixture(), "prec_mm")
  testthat::expect_equal(
    names(out),
    c("lon", "lat", "year", "month", "value")
  )
  testthat::expect_equal(out$value, c(50, 60))
  testthat::expect_null(whep:::.socd_pin_var(NULL, "prec_mm"))
})

testthat::test_that("the pin error names both ways out", {
  # A deliberately unregistered alias so this fails in the local lookup and
  # never reaches the board -- the point is the message, not the transport.
  testthat::expect_error(
    whep:::.read_lpjml_pin("lpjml-not-a-registered-alias"),
    "WHEP_LPJML_RUN_DIR"
  )
})

# ---- Year-scoped polity validity (whep#462) ---------------------------------
#
# `data$cell_polity` is a present-day rasterization with no year dimension,
# while polity validity IS year-scoped, so a cell labelled `area_code` 52
# (Azerbaijan) carries that label in 1901 as readily as in 2009 and the polity
# resolution falls back to `AZE-1991-2025`, a state that did not exist then.
# Two cells, one whose area code is valid across the whole span (11, Austria)
# and one that is not, over two years straddling AZE-1991-2025's start, so the
# same cell is a stand-in in one year and a real period hit in the other.
.wb_validity_inputs <- function(years = c(1990L, 2000L)) {
  cells <- tibble::tribble(
    ~lon, ~lat, ~area_code,
    9.25, 47.75, 11L,
    47.75, 40.25, 52L
  )
  flux <- tidyr::expand_grid(cells, year = years, month = 1:12) |>
    dplyr::mutate(
      transp = 40,
      evap = 15,
      interc = 5,
      irrig = 8,
      runoff = 12,
      seepage = 10,
      prec = 100
    )
  swc <- tidyr::expand_grid(cells, year = years, month = 1:12, layer = 1:6) |>
    dplyr::mutate(value = 0.4)
  to_long <- function(var) {
    dplyr::select(flux, lon, lat, year, month, value = dplyr::all_of(var))
  }
  list(
    transp = to_long("transp"),
    evap = to_long("evap"),
    interc = to_long("interc"),
    prec = to_long("prec"),
    irrig = to_long("irrig"),
    runoff = to_long("runoff"),
    seepage = to_long("seepage"),
    swc = swc,
    cell_polity = dplyr::mutate(cells, polity_frac = 1, cell_area_ha = 30000)
  )
}

# blue_green is pinned to irrig_share so the per-CFT fallback warning does not
# compete with the validity warning these tests are about.
.wb_validity_call <- function(...) {
  whep::build_water_balance(
    data = .wb_validity_inputs(),
    method = list(blue_green = "irrig_share"),
    ...
  )
}

testthat::test_that("a pre-independence cell-year is named, not silent", {
  testthat::expect_warning(
    wb <- .wb_validity_call(resolution = "polity"),
    "did not exist in that row's year"
  )

  az <- dplyr::filter(wb, area_code == 52L)
  testthat::expect_setequal(az$year, c(1990L, 2000L))
  # Both years land on the same post-1991 polity: that is the defect, and the
  # warning above is the only thing on the default path that says so.
  testthat::expect_setequal(az$reporting_polity_code, "AZE-1991-2025")
})

testthat::test_that("the validity warning names the count and the codes", {
  testthat::expect_warning(
    .wb_validity_call(resolution = "polity"),
    "1 row over 1 area code"
  )
  testthat::expect_warning(
    .wb_validity_call(resolution = "polity"),
    "1990-1990"
  )
  testthat::expect_warning(.wb_validity_call(resolution = "polity"), "52")
})

testthat::test_that("flag marks exactly the out-of-span rows", {
  testthat::expect_warning(
    wb <- .wb_validity_call(resolution = "polity", polity_validity = "flag"),
    "kept and flagged"
  )

  pointblank::expect_col_exists(wb, "reporting_polity_out_of_span")
  flagged <- dplyr::filter(wb, reporting_polity_out_of_span)
  testthat::expect_equal(nrow(flagged), 1L)
  testthat::expect_equal(flagged$area_code, 52L)
  testthat::expect_equal(flagged$year, 1990L)
})

testthat::test_that("drop removes only the out-of-span rows", {
  testthat::expect_warning(
    .wb_validity_call(resolution = "polity"),
    "kept as-is"
  )
  testthat::expect_warning(
    .wb_validity_call(resolution = "polity", polity_validity = "drop"),
    "dropped"
  )
  kept <- suppressWarnings(.wb_validity_call(resolution = "polity"))
  dropped <- suppressWarnings(
    .wb_validity_call(resolution = "polity", polity_validity = "drop")
  )

  testthat::expect_equal(nrow(kept), 4L)
  testthat::expect_equal(nrow(dropped), 3L)
  gone <- dplyr::anti_join(kept, dropped, by = c("area_code", "year"))
  testthat::expect_equal(gone$area_code, 52L)
  testthat::expect_equal(gone$year, 1990L)
  # The surviving rows are untouched: dropping must not re-weight anything.
  testthat::expect_equal(
    dplyr::semi_join(kept, dropped, by = c("area_code", "year")),
    dropped,
    ignore_attr = TRUE
  )
})

testthat::test_that("the grid resolution reports validity too", {
  testthat::expect_warning(
    grid <- .wb_validity_call(resolution = "grid", polity_validity = "drop"),
    "did not exist in that row's year"
  )

  testthat::expect_equal(nrow(grid), 3L)
  testthat::expect_false(any(grid$area_code == 52L & grid$year == 1990L))
})

testthat::test_that("keep is the default and leaves the numbers alone", {
  testthat::expect_warning(
    .wb_validity_call(resolution = "polity", polity_validity = "keep"),
    "kept as-is"
  )
  explicit <- suppressWarnings(
    .wb_validity_call(resolution = "polity", polity_validity = "keep")
  )
  default <- suppressWarnings(.wb_validity_call(resolution = "polity"))

  testthat::expect_equal(default, explicit)
  testthat::expect_false(
    rlang::has_name(default, "reporting_polity_out_of_span")
  )
})

testthat::test_that("an in-span build warns about nothing", {
  testthat::expect_no_warning(
    whep::build_water_balance(resolution = "polity", example = TRUE)
  )
  testthat::expect_no_warning(whep::get_soc_climate_drivers(example = TRUE))
})

testthat::test_that("an invalid polity_validity is rejected", {
  testthat::expect_error(
    whep::build_water_balance(polity_validity = "nonsense"),
    "polity_validity"
  )
  testthat::expect_error(
    whep::get_soc_climate_drivers(polity_validity = "nonsense"),
    "polity_validity"
  )
})

testthat::test_that("SOC drivers honour polity_validity as well", {
  data <- .socd_synthetic()
  monthly <- c("temp", "pet", "prec", "irrig", "swc")
  data[monthly] <- purrr::map(
    data[monthly],
    \(x) dplyr::mutate(x, year = 1950L)
  )
  data$cell_polity <- dplyr::mutate(data$cell_polity, area_code = 52L)

  testthat::expect_warning(
    drv <- whep::get_soc_climate_drivers(data = data, polity_validity = "flag"),
    "did not exist in that row's year"
  )
  testthat::expect_true(all(drv$reporting_polity_out_of_span))
  testthat::expect_warning(
    empty <- whep::get_soc_climate_drivers(
      data = data,
      polity_validity = "drop"
    ),
    "dropped"
  )
  testthat::expect_equal(nrow(empty), 0L)
})
