# build_crop_water_consumption() keeps the per-CFT water per band (#916). Its contract
# is that summing it over bands gives back build_water_balance()'s per-CFT
# columns exactly, so most tests here compare the two on the same inputs.

# Synthetic inputs with realistic (non-unit) stand fractions and three bands
# carrying all three per-CFT cubes. Unit fractions would make weighted and
# unweighted sums agree and hide a weighting error.
.cwc_inputs <- function() {
  syn <- .wb_synthetic_monthly()
  cells <- dplyr::distinct(syn$inputs$prec, lon, lat, year)
  bands <- tibble::tribble(
    ~band, ~band_name,             ~blue, ~green, ~nir, ~frac,
    1L,    "rainfed maize",        0,     410,    0,    0.30,
    3L,    "irrigated maize",      220,   180,    190,  0.07,
    14L,   "rainfed grassland",    0,     520,    0,    0.45
  )
  grid <- tidyr::expand_grid(cells, bands) |>
    dplyr::mutate(scale = 1 + (lon > 0) * 0.3)
  syn$inputs$cft_consump_water_b <- .cwc_cube(grid, "blue")
  syn$inputs$cft_consump_water_g <- .cwc_cube(grid, "green")
  syn$inputs$cft_nir <- .cwc_cube(grid, "nir")
  syn$inputs$stand_frac <- .cwc_cube(dplyr::mutate(grid, scale = 1), "frac")
  syn$inputs
}

# One per-CFT cube from the band grid: column `col`, scaled per cell.
.cwc_cube <- function(grid, col) {
  dplyr::transmute(
    grid,
    lon,
    lat,
    year,
    band,
    band_name,
    value = .data[[col]] * scale
  )
}

.cwc_band_sums <- function(per_band) {
  per_band |>
    dplyr::summarise(
      dplyr::across(
        c(blue_consump_mm, green_consump_mm, cft_nir_mm),
        sum
      ),
      .by = c(lon, lat, year)
    ) |>
    dplyr::arrange(lon, lat, year)
}

testthat::test_that("summed over bands it equals build_water_balance()", {
  inputs <- .cwc_inputs()
  wb <- whep::build_water_balance(data = inputs) |>
    dplyr::arrange(lon, lat, year)
  per_band <- whep::build_crop_water_consumption(data = inputs)

  testthat::expect_equal(nrow(per_band), 6L)
  sums <- .cwc_band_sums(per_band)
  testthat::expect_identical(sums$blue_consump_mm, wb$blue_consump_mm)
  testthat::expect_identical(sums$green_consump_mm, wb$green_consump_mm)
  testthat::expect_identical(sums$cft_nir_mm, wb$cft_nir_mm)
  # The identity is not vacuous: every term carries water.
  testthat::expect_true(all(wb$blue_consump_mm > 0))
  testthat::expect_true(all(wb$cft_nir_mm > 0))
})

testthat::test_that("each band is its stand density times its fraction", {
  inputs <- .cwc_inputs()
  per_band <- whep::build_crop_water_consumption(data = inputs)
  maize <- dplyr::filter(
    per_band,
    band_name == "irrigated maize",
    lon < 0
  )
  testthat::expect_equal(maize$stand_frac, 0.07)
  testthat::expect_equal(maize$blue_consump_mm, 220 * 0.07)
  testthat::expect_equal(maize$cft_nir_mm, 190 * 0.07)
})

testthat::test_that("bands matches build_water_balance(bands = )", {
  inputs <- .cwc_inputs()
  pick <- c("rainfed grassland", "irrigated maize")
  wb <- whep::build_water_balance(data = inputs, bands = pick) |>
    dplyr::arrange(lon, lat, year)
  per_band <- whep::build_crop_water_consumption(data = inputs, bands = pick)

  testthat::expect_setequal(unique(per_band$band_name), pick)
  sums <- .cwc_band_sums(per_band)
  testthat::expect_identical(sums$blue_consump_mm, wb$blue_consump_mm)
  testthat::expect_identical(sums$green_consump_mm, wb$green_consump_mm)
})

testthat::test_that("an unknown band aborts rather than returning nothing", {
  testthat::expect_error(
    whep::build_crop_water_consumption(
      data = .cwc_inputs(),
      bands = "rainfed kale"
    ),
    "not in this input"
  )
})

testthat::test_that("polity volumes reconcile with the polity mean depths", {
  inputs <- .cwc_inputs()
  wb <- whep::build_water_balance(data = inputs, resolution = "polity") |>
    dplyr::arrange(area_code)
  vol <- whep::build_crop_water_consumption(
    data = inputs,
    resolution = "polity"
  )

  weight <- inputs$cell_polity |>
    dplyr::summarise(
      weight_ha = sum(polity_frac * cell_area_ha),
      .by = area_code
    )
  depth <- vol |>
    dplyr::summarise(
      dplyr::across(dplyr::ends_with("_m3"), sum),
      .by = area_code
    ) |>
    dplyr::left_join(weight, by = "area_code") |>
    dplyr::arrange(area_code)
  # 1 mm over 1 ha is 10 m3.
  testthat::expect_equal(
    depth$blue_consump_m3 / (10 * depth$weight_ha),
    wb$blue_consump_mm
  )
  testthat::expect_equal(
    depth$green_consump_m3 / (10 * depth$weight_ha),
    wb$green_consump_mm
  )
  testthat::expect_equal(
    depth$cft_nir_m3 / (10 * depth$weight_ha),
    wb$cft_nir_mm
  )
  testthat::expect_true(
    all(c("reporting_polity_code", "stand_area_ha") %in% names(vol))
  )
})

testthat::test_that("polity resolution needs the crosswalk", {
  inputs <- .cwc_inputs()
  inputs$cell_polity <- NULL
  testthat::expect_error(
    whep::build_crop_water_consumption(data = inputs, resolution = "polity"),
    "cell_polity"
  )
})

testthat::test_that("an absent cube stays NA, never zero", {
  inputs <- .cwc_inputs()
  testthat::local_mocked_bindings(
    read_lpjml_hydrology = function(var, ...) NULL,
    .package = "whep"
  )
  inputs$cft_nir <- NULL
  grid <- whep::build_crop_water_consumption(data = inputs)
  testthat::expect_true(all(is.na(grid$cft_nir_mm)))
  testthat::expect_false(anyNA(grid$green_consump_mm))

  poly <- whep::build_crop_water_consumption(
    data = inputs,
    resolution = "polity"
  )
  testthat::expect_true(all(is.na(poly$cft_nir_m3)))
})

testthat::test_that("unsimulated cells are dropped, partial NA is kept", {
  inputs <- .cwc_inputs()
  ocean <- inputs$cft_consump_water_b |>
    dplyr::filter(lon == min(lon)) |>
    dplyr::mutate(lon = 170.25, value = NA_real_)
  cubes <- c("cft_consump_water_b", "cft_consump_water_g", "cft_nir")
  inputs[c(cubes, "stand_frac")] <- purrr::map(
    inputs[c(cubes, "stand_frac")],
    \(x) dplyr::bind_rows(x, ocean)
  )
  # One band NA in a single cube only: that row must survive, showing NA.
  inputs$cft_nir <- dplyr::mutate(
    inputs$cft_nir,
    value = dplyr::if_else(band == 1L & lon < 0, NA_real_, value)
  )
  grid <- testthat::expect_no_warning(
    whep::build_crop_water_consumption(data = inputs)
  )
  testthat::expect_false(170.25 %in% grid$lon)
  testthat::expect_equal(nrow(grid), 6L)
  testthat::expect_equal(sum(is.na(grid$cft_nir_mm)), 1L)
})

testthat::test_that("absent cubes are read for the requested years only", {
  inputs <- .cwc_inputs()
  seen <- list()
  testthat::local_mocked_bindings(
    read_lpjml_hydrology = function(var, years = NULL, ...) {
      seen[[var]] <<- years
      inputs[[var]]
    },
    .package = "whep"
  )
  supplied <- inputs[c("stand_frac", "cell_polity")]
  out <- whep::build_crop_water_consumption(years = 2000L, data = supplied)
  testthat::expect_setequal(
    names(seen),
    c("cft_consump_water_b", "cft_consump_water_g", "cft_nir")
  )
  testthat::expect_true(all(purrr::map_lgl(seen, \(y) identical(y, 2000L))))
  testthat::expect_equal(nrow(out), 6L)
})

testthat::test_that("no cube at all aborts", {
  testthat::local_mocked_bindings(
    read_lpjml_hydrology = function(var, ...) NULL,
    .package = "whep"
  )
  testthat::expect_error(
    whep::build_crop_water_consumption(data = list()),
    "No per-CFT water cube"
  )
})

testthat::test_that("missing stand fractions abort, never an unweighted sum", {
  inputs <- .cwc_inputs()
  testthat::local_mocked_bindings(
    read_lpjml_hydrology = function(var, ...) NULL,
    .package = "whep"
  )
  inputs$stand_frac <- NULL
  testthat::expect_error(
    whep::build_crop_water_consumption(data = inputs),
    "per-STAND"
  )
})

testthat::test_that("blue water on a rainfed band warns (LPJmL 6.x defect)", {
  inputs <- .cwc_inputs()
  inputs$cft_consump_water_b <- dplyr::mutate(
    inputs$cft_consump_water_b,
    value = dplyr::if_else(band_name == "rainfed grassland", 400, value)
  )
  testthat::expect_warning(
    whep::build_crop_water_consumption(data = inputs),
    class = "whep_rainfed_blue_water"
  )
})

testthat::test_that("bands carry the build_crop_water_use() crop group", {
  grid <- whep::build_crop_water_consumption(data = .cwc_inputs())
  groups <- dplyr::distinct(grid, band_name, crop_group) |>
    dplyr::arrange(band_name)
  testthat::expect_identical(
    groups$crop_group,
    c(
      "cropland_irrigated_herbaceous",
      NA,
      "cropland_rainfed_herbaceous"
    )
  )
})

testthat::test_that("the example runs the real pipeline", {
  grid <- whep::build_crop_water_consumption(example = TRUE)
  poly <- whep::build_crop_water_consumption(
    resolution = "polity",
    example = TRUE
  )
  testthat::expect_equal(nrow(grid), 6L)
  testthat::expect_equal(nrow(poly), 6L)
  testthat::expect_false(anyNA(grid$blue_consump_mm))
  # 180 mm/yr per stand on a 5% stand is 9 mm/yr over the cell.
  testthat::expect_equal(
    grid$blue_consump_mm[grid$band == 17L & grid$lon > 0],
    9
  )
})
