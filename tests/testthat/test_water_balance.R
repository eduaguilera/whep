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
  list(
    temp = temp,
    pet = pet,
    prec = prec,
    irrig = irrig,
    swc = swc,
    clay = clay,
    cell_polity = cell_polity
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

# -- Spain_Hist port-fidelity validation (skip_on_ci, Task A4) ---------------
#
# Compare whep::build_water_balance() on REAL LPJmL 0.5-degree grid cells
# covering Spain against Spain_Hist's own water balance reference,
# Wat_ygpit.csv, at NATIONAL area-weighted-mean totals for a single reference
# year (2005, chosen because Wat_ygpit.csv covers 1860-2023 but the local
# LPJmL run only covers 1901-2009).
#
# THIS TEST IS METHODOLOGICALLY DIFFERENT from Module B/C's fidelity tests
# (test_carbon_balance.R, test_n_balance.R): those compare "does WHEP's PORTED
# TRANSFORM reproduce Spain_Hist given the SAME real inputs" ("province as
# cell", Spain's own per-crop carbon inputs / N inputs injected directly into
# WHEP's assembler). This test compares TWO INDEPENDENT HYDROLOGICAL MODELS
# of the same physical process on Spain's real geography: WHEP's
# build_water_balance() is driven ENTIRELY by real LPJmL grid-cell hydrology
# output (a global process-based dynamic vegetation + hydrology model run at
# 0.5-degree resolution), while Spain_Hist's Wat_ygpit.csv comes from an
# independent bucket-model + crop-Kc + GlobWat water balance computed at
# per-crop x per-province grain. No WHEP input is taken from Spain_Hist here
# (unlike Module B/C's fidelity tests, which inject Spain's own driver
# values); this is a genuine cross-model comparison, not a port-fidelity
# check of a transcribed equation, so a real, non-trivial divergence is
# expected and is NOT evidence of a bug.
#
# GRAIN: compared at NATIONAL level only (area-weighted mean depth, mm),
# never per-province, because LPJmL's 0.5-degree grid cells do not align with
# Spain's 50 province boundaries -- following Task C8's precedent in
# test_n_balance.R for a similarly grain-mismatched comparison (there, N
# masses are summed nationally instead of compared per-province because the
# WHEP crop-name join is incomplete at province grain; here, mm DEPTHS are
# area-weighted-mean nationally, mirroring test_carbon_balance.R's/
# test_n_balance.R's own province-area-weighting pattern for turning
# per-region values into one national figure, since summing mm depths across
# regions of very different area would not be physically meaningful).
#
# Spain_Hist's own reference is itself area-weighted across ALL SIX of its
# land-use classes (Cropland, Dehesa, Forest_high, Forest_low,
# Pasture_Shrubland, Other), not cropland alone: WHEP's LPJmL cells are not
# restricted to agricultural land either (LPJmL simulates the whole grid
# cell under its natural-potential-vegetation-plus-cropland/irrigation mix),
# so comparing both sides' whole-territory area-weighted mean is the
# apples-to-apples national scope (a cropland-only diagnostic split is
# reported in the comment below to help interpret the gap, but is not what
# is asserted).

# Spain_Hist's reference NATIONAL area-weighted-mean totals (2005, every
# LandUse class) for the 3 terms this test compares. Area-weighted (not
# summed) because WaterInput_mm/AET_mm/S_mm are per-hectare depths (mm), not
# volumes; weighting by Area_ygpit_ha mirrors test_carbon_balance.R's /
# test_n_balance.R's own province-area-weighting pattern for building one
# national figure out of per-region depth/rate values. NULL when the file is
# absent so the test skips rather than failing on a missing dependency.
.wb_spain_reference_national <- function(ref_year = 2005L) {
  f <- file.path(.spain_hist_l_dir(), "Wat_ygpit.csv")
  if (!file.exists(f)) {
    return(NULL)
  }
  cols <- c(
    "YearH",
    "Province_name",
    "LandUse",
    "Area_ygpit_ha",
    "WaterInput_mm",
    "AET_mm",
    "S_mm"
  )
  ref_year_rows <- data.table::fread(f, select = cols) |>
    tibble::as_tibble() |>
    dplyr::filter(.data$YearH == ref_year)
  dplyr::summarise(
    ref_year_rows,
    water_input_ref = stats::weighted.mean(
      .data$WaterInput_mm,
      .data$Area_ygpit_ha,
      na.rm = TRUE
    ),
    aet_ref = stats::weighted.mean(
      .data$AET_mm,
      .data$Area_ygpit_ha,
      na.rm = TRUE
    ),
    drainage_ref = stats::weighted.mean(
      .data$S_mm,
      .data$Area_ygpit_ha,
      na.rm = TRUE
    )
  )
}

# Real LPJmL hydrology inputs for build_water_balance(), pre-filtered to the
# grid cells covering Spain via the real cell-polity crosswalk
# (whep::build_cell_polity(), area_code == 203L). Reads each annual flux
# read_lpjml_hydrology() needs (.wb_read_inputs()'s flux_vars: transp, evap,
# interc, prec, irrig, runoff, seepage) plus monthly swc ONCE for the single
# reference year (years = 2005L), never the years = NULL default (which would
# read all 109 years globally). NULL when the cell-polity parquet or the
# LPJmL run directory are absent, so the test skips.
.wb_spain_lpjml_inputs <- function(ref_year = 2005L) {
  cell_polity <- tryCatch(whep::build_cell_polity(), error = function(e) NULL)
  if (is.null(cell_polity)) {
    return(NULL)
  }
  spain_cells <- dplyr::filter(cell_polity, .data$area_code == 203L)
  if (nrow(spain_cells) == 0) {
    return(NULL)
  }
  fluxes <- tryCatch(
    .wb_spain_fluxes(spain_cells, ref_year),
    error = function(e) NULL
  )
  if (is.null(fluxes)) {
    return(NULL)
  }
  c(fluxes, list(cell_polity = spain_cells))
}

# Read the 7 annual hydrology fluxes plus monthly swc for ref_year, each
# inner-joined down to the Spain cell list before returning. Errors (e.g. the
# LPJmL run directory is absent) propagate to the caller's tryCatch.
.wb_spain_fluxes <- function(spain_cells, ref_year) {
  cell_key <- dplyr::select(spain_cells, "lon", "lat")
  flux_vars <- c(
    transp = "transp",
    evap = "evap",
    interc = "interc",
    prec = "prec",
    irrig = "irrig",
    runoff = "runoff",
    seepage = "drainage"
  )
  fluxes <- purrr::map(flux_vars, function(reader_var) {
    raw <- whep::read_lpjml_hydrology(
      reader_var,
      years = ref_year,
      monthly = FALSE
    )
    dplyr::inner_join(raw, cell_key, by = c("lon", "lat"))
  })
  swc_raw <- whep::read_lpjml_hydrology(
    "swc",
    years = ref_year,
    monthly = TRUE
  )
  fluxes$swc <- dplyr::inner_join(swc_raw, cell_key, by = c("lon", "lat"))
  fluxes
}

testthat::test_that("WHEP water balance matches Spain_Hist national 2005 totals", {
  testthat::skip_on_ci()
  ref_nat <- .wb_spain_reference_national()
  testthat::skip_if(
    is.null(ref_nat),
    "Spain_Hist Wat_ygpit.csv not found."
  )
  spain_inputs <- .wb_spain_lpjml_inputs()
  testthat::skip_if(
    is.null(spain_inputs),
    "Real LPJmL hydrology / cell-polity crosswalk not found for Spain."
  )

  whep_out <- whep::build_water_balance(
    method = list(blue_green = "irrig_share"),
    resolution = "polity",
    data = spain_inputs
  )

  terms <- c("water_input", "aet", "drainage")
  cmp <- tibble::tibble(
    term = terms,
    whep = c(
      whep_out$water_input_mm,
      whep_out$aet_mm,
      whep_out$drainage_mm
    ),
    ref = c(
      ref_nat$water_input_ref,
      ref_nat$aet_ref,
      ref_nat$drainage_ref
    )
  ) |>
    dplyr::mutate(abs_pct_diff = 100 * abs(.data$whep - .data$ref) / .data$ref)

  # Measured divergence (task A4, real 2005 run, national area-weighted-mean
  # totals across whole-territory Spain, n = 276 LPJmL 0.5-degree cells vs 50
  # Spain_Hist provinces x 6 land-use classes collapsed to one national
  # area-weighted mean per term):
  #   water_input_mm  447 mm (WHEP) vs 459 mm (Spain_Hist)  ->  2.62%
  #   aet_mm          352 mm (WHEP) vs 276 mm (Spain_Hist)  -> 27.69%
  #   drainage_mm      60 mm (WHEP) vs  67 mm (Spain_Hist)  -> 10.25%
  #   (median = 10.25%, max = 27.69%)
  # Decomposition (not tuned to force a pass, per this branch's Module B/C
  # precedent):
  #   (i) water_input_mm (precipitation + irrigation) is the closest term:
  #       both sides ultimately derive precipitation from gridded climate
  #       forcing at a similar spatial/temporal resolution, so this term
  #       mostly validates that the two models see a similar amount of water
  #       arriving at the land surface.
  #   (ii) aet_mm is the largest divergence and is NOT primarily a bug: a
  #       diagnostic split of Spain_Hist's own reference by land-use class
  #       shows AET = 325 mm for CROPLAND alone (area 18.1M ha) vs 248 mm for
  #       every NON-CROPLAND class combined (area 32.5M ha, i.e. forest,
  #       dehesa, pasture/shrubland, other) -- Spain_Hist's non-cropland AET
  #       is a coarser proxy (it is not the project's focus, unlike its
  #       detailed per-crop Kc-based cropland AET) and is area-weighted 64%
  #       into the whole-territory reference used here. LPJmL, by contrast,
  #       simulates a full process-based dynamic-vegetation water balance
  #       (transpiration + evaporation + interception) uniformly over every
  #       cell regardless of land use. Restricting to Spain_Hist's own
  #       cropland-only AET (325 mm) against WHEP's whole-territory 352 mm
  #       would narrow the gap to ~8.28%, i.e. most of the 27.69% divergence
  #       traces to the non-cropland two-thirds of Spain's territory, where
  #       the two models' AET methods differ most, not to a WHEP defect on
  #       the land use Spain_Hist itself models most carefully.
  #   (iii) drainage_mm (LPJmL deep seepage vs Spain_Hist's GlobWat-corrected
  #       bucket-model drainage, S_mm) sits just above the 10% bar. Because
  #       drainage is the water-budget residual after AET and runoff are
  #       removed, it inherits some of (ii)'s AET gap indirectly (WHEP's
  #       higher AET, all else equal, would be expected to leave LESS residual
  #       water for drainage than Spain_Hist's lower-AET reference -- yet WHEP's
  #       drainage is ALSO lower than Spain_Hist's here, 60 vs 67 mm, so the
  #       gap is not a simple AET-residual pass-through; LPJmL's runoff_mm,
  #       which Spain_Hist's own water.r model does not report or partition
  #       out the same way, is the more likely absorber of the difference).
  # None of this is a bug in build_water_balance()'s own arithmetic (the
  # 4-term budget-closure tests earlier in this file exercise that arithmetic
  # directly on synthetic inputs and pass exactly); it is the documented cost
  # of comparing two independent hydrological models -- LPJmL's process-based
  # dynamic-vegetation water balance vs Spain_Hist's crop-Kc bucket model plus
  # GlobWat correction -- on Spain's real geography. The 10% tolerance
  # (test_carbon_balance.R's / test_n_balance.R's precedent for a similarly
  # in-scope-limited fidelity comparison) is NOT loosened to force a pass:
  # the measured divergence is reported here for diagnosis.
  worst <- max(cmp$abs_pct_diff)
  med <- stats::median(cmp$abs_pct_diff)
  testthat::expect_lt(
    worst,
    10,
    label = sprintf(
      paste0(
        "WHEP water balance vs Spain_Hist 2005 national area-weighted ",
        "totals: median divergence = %.2f%%, max = %.2f%% (terms: %s)"
      ),
      med,
      worst,
      paste(sprintf("%s=%.1f%%", cmp$term, cmp$abs_pct_diff), collapse = ", ")
    )
  )
})
