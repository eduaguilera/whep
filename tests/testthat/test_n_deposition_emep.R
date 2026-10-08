# ---- read_emep_deposition() --------------------------------------------

# Write a tiny EMEP-style yearly NetCDF on the 0.1-degree grid covering the
# 0.5-degree blocks centred at (0.25, 0.25) and (0.75, 0.25): 10 x 5 fine
# cells. Each component is filled with its own constant, so a wrong component
# choice or a missing one changes the result.
.emep_fixture_file <- function(dir, year, emis_year = year, fills = NULL) {
  fills <- fills %||%
    c(
      DDEP_RDN_m2Grid = 100,
      WDEP_RDN = 200,
      DDEP_OXN_m2Grid = 30,
      WDEP_OXN = 70,
      DDEP_RDN_m2Seminat = 9999
    )
  lon <- seq(0.05, 0.95, by = 0.1)
  lat <- seq(0.05, 0.45, by = 0.1)
  dim_lon <- ncdf4::ncdim_def("lon", "degrees_east", lon)
  dim_lat <- ncdf4::ncdim_def("lat", "degrees_north", lat)
  vars <- lapply(names(fills), \(v) {
    ncdf4::ncvar_def(v, "mgN/m2", list(dim_lon, dim_lat), missval = -1e20)
  })
  path <- file.path(
    dir,
    sprintf("EMEP01_rv5.6_year.%dmet_%demis_rep2025.nc", year, emis_year)
  )
  nc <- ncdf4::nc_create(path, vars)
  purrr::walk2(vars, fills, \(v, f) {
    vals <- matrix(f, nrow = length(lon), ncol = length(lat))
    # One fine cell of the first block differs, so a sum and a mean of the 25
    # disagree.
    vals[1, 1] <- 2 * f
    ncdf4::ncvar_put(nc, v, vals)
  })
  ncdf4::nc_close(nc)
  path
}

testthat::test_that("read_emep_deposition averages 25 fine cells per block", {
  testthat::skip_if_not_installed("ncdf4")
  dir <- withr::local_tempdir()
  .emep_fixture_file(dir, 2000L)

  out <- whep::read_emep_deposition("nhx", emep_dir = dir) |>
    dplyr::arrange(.data$lon)

  testthat::expect_equal(out$lon, c(0.25, 0.75))
  testthat::expect_equal(out$lat, c(0.25, 0.25))
  testthat::expect_equal(out$year, c(2000L, 2000L))
  # NHx is dry + wet reduced N, the grid-average dry term only: 100 + 200 =
  # 300 mgN/m2 = 3 kg N/ha; the first block holds one doubled fine cell, so
  # its mean is (24 * 300 + 600) / 25 = 312 mgN/m2.
  testthat::expect_equal(out$deposition_kgn_ha, c(3.12, 3))
})

testthat::test_that("read_emep_deposition reads oxidised N for noy", {
  testthat::skip_if_not_installed("ncdf4")
  dir <- withr::local_tempdir()
  .emep_fixture_file(dir, 2000L)

  out <- whep::read_emep_deposition("noy", emep_dir = dir) |>
    dplyr::arrange(.data$lon)

  # 30 + 70 = 100 mgN/m2 = 1 kg N/ha; first block (24 * 100 + 200) / 25.
  testthat::expect_equal(out$deposition_kgn_ha, c(1.04, 1))
})

testthat::test_that("read_emep_deposition takes the year from the file", {
  testthat::skip_if_not_installed("ncdf4")
  dir <- withr::local_tempdir()
  .emep_fixture_file(dir, 1990L)
  .emep_fixture_file(dir, 1991L)
  # A met year paired with another emission year is not a trend-run file.
  .emep_fixture_file(dir, 1992L, emis_year = 2000L)
  file.create(file.path(
    dir,
    "EMEP01_rv5.6_year.1993met_1993emis_rep2025.nc.part"
  ))

  all_years <- whep::read_emep_deposition("nhx", emep_dir = dir)
  one_year <- whep::read_emep_deposition("nhx", emep_dir = dir, years = 1991L)
  none <- whep::read_emep_deposition("nhx", emep_dir = dir, years = 1850L)

  testthat::expect_setequal(unique(all_years$year), c(1990L, 1991L))
  testthat::expect_equal(unique(one_year$year), 1991L)
  testthat::expect_equal(nrow(none), 0L)
  testthat::expect_named(
    none,
    c("lon", "lat", "year", "deposition_kgn_ha"),
    ignore.order = TRUE
  )
})

testthat::test_that("read_emep_deposition aborts without a directory", {
  withr::local_envvar(WHEP_EMEP_DIR = "")
  testthat::expect_error(
    whep::read_emep_deposition("nhx"),
    "WHEP_EMEP_DIR"
  )
})

testthat::test_that("read_emep_deposition example fixture is schema-complete", {
  out <- whep::read_emep_deposition(example = TRUE)
  pointblank::expect_col_exists(
    out,
    c("lon", "lat", "year", "deposition_kgn_ha")
  )
})

# ---- correct_n_deposition() --------------------------------------------

# Two countries, one cell each, both 100,000 ha. Country 79 is corrected,
# country 351 is outside `area_codes`. HaNi is flat at 10 t; EMEP falls from
# 20 to 10 t across 1990-1992, the shape whep#1121 measured. 1980 has HaNi
# only, so it lies outside the overlap.
.ndc_cells <- function() {
  tibble::tribble(
    ~lon, ~lat, ~area_code, ~polity_frac, ~cell_area_ha,
    0.25, 0.25, 79L, 1, 100000,
    10.25, 0.25, 351L, 1, 100000
  )
}

.ndc_hani <- function() {
  tidyr::expand_grid(
    tibble::tibble(lon = c(0.25, 10.25), lat = 0.25),
    year = c(1980L, 1990L, 1991L, 1992L)
  ) |>
    dplyr::mutate(value_g = 1e7, method_deposition = "hani")
}

# kg N/ha giving 20, 15 and 10 t over 100,000 ha.
.ndc_emep <- function() {
  tidyr::expand_grid(
    tibble::tibble(lon = c(0.25, 10.25), lat = 0.25),
    tibble::tibble(
      year = c(1990L, 1991L, 1992L),
      deposition_kgn_ha = c(0.2, 0.15, 0.1)
    )
  )
}

.ndc_run <- function(...) {
  whep::correct_n_deposition(
    hani = .ndc_hani(),
    emep = .ndc_emep(),
    cell_polity = .ndc_cells(),
    area_codes = 79L,
    ...
  )
}

.ndc_mass <- function(out, lon, years) {
  out |>
    dplyr::filter(.data$lon == !!lon, .data$year %in% years) |>
    dplyr::arrange(.data$year) |>
    dplyr::pull("value_g")
}

testthat::test_that("correct_n_deposition imposes EMEP's trajectory", {
  out <- .ndc_run(reference_years = 1992L)

  # Anchored on 1992, EMEP is 2x and 1.5x its 1992 level in 1990 and 1991
  # while HaNi is flat, so HaNi is scaled by exactly those ratios.
  testthat::expect_equal(
    .ndc_mass(out, 0.25, 1990:1992),
    c(2e7, 1.5e7, 1e7)
  )
  pointblank::expect_col_vals_in_set(
    dplyr::filter(out, .data$lon == 0.25, .data$year >= 1990L),
    "method_deposition",
    "hani_emep_trend"
  )
})

testthat::test_that("correct_n_deposition keeps HaNi's reference-period level", {
  reference <- 1990:1992
  out <- .ndc_run(reference_years = reference)

  corrected <- mean(.ndc_mass(out, 0.25, reference))
  original <- mean(.ndc_mass(.ndc_hani(), 0.25, reference))
  testthat::expect_equal(corrected, original)
  # ... and EMEP's level (15 t mean) is NOT imposed: only its shape is.
  testthat::expect_equal(.ndc_mass(out, 0.25, 1990L), 1e7 * 20 / 15)
})

testthat::test_that("correct_n_deposition does not read a level gap as a trend", {
  # EMEP a constant 3x HaNi, as a coastal cell reads when HaNi books land only
  # and EMEP the whole cell: no trend difference, so nothing moves.
  emep <- .ndc_emep() |> dplyr::mutate(deposition_kgn_ha = 0.3)
  out <- whep::correct_n_deposition(
    hani = .ndc_hani(),
    emep = emep,
    cell_polity = .ndc_cells(),
    reference_years = 1990:1992,
    area_codes = 79L
  )

  testthat::expect_equal(out$value_g, .ndc_hani()$value_g)
  pointblank::expect_col_vals_equal(
    dplyr::filter(out, .data$lon == 0.25, .data$year >= 1990L),
    "deposition_correction",
    1
  )
})

testthat::test_that("correct_n_deposition can lower a year as well as raise one", {
  # HaNi above EMEP's trajectory in one year, as in 2011-2013: a year-resolved
  # factor lowers that year while raising another.
  hani <- .ndc_hani() |>
    dplyr::mutate(
      value_g = dplyr::if_else(.data$year == 1991L, 2e7, .data$value_g)
    )
  out <- whep::correct_n_deposition(
    hani = hani,
    emep = .ndc_emep(),
    cell_polity = .ndc_cells(),
    reference_years = 1992L,
    area_codes = 79L
  )

  factors <- out |>
    dplyr::filter(.data$lon == 0.25, .data$year %in% c(1990L, 1991L)) |>
    dplyr::arrange(.data$year) |>
    dplyr::pull("deposition_correction")
  testthat::expect_gt(factors[[1]], 1)
  testthat::expect_lt(factors[[2]], 1)
})

testthat::test_that("correct_n_deposition leaves rows without a reference alone", {
  out <- .ndc_run(reference_years = 1992L)

  untouched <- dplyr::filter(
    out,
    .data$lon == 10.25 | .data$year == 1980L
  )
  # Outside the corrected countries, and before EMEP starts: returned as
  # read, still stamped "hani", with a declared factor of one.
  testthat::expect_equal(nrow(untouched), 5L)
  pointblank::expect_col_vals_equal(untouched, "value_g", 1e7)
  pointblank::expect_col_vals_in_set(untouched, "method_deposition", "hani")
  pointblank::expect_col_vals_equal(untouched, "deposition_correction", 1)
  testthat::expect_equal(nrow(out), nrow(.ndc_hani()))
})

testthat::test_that("correct_n_deposition counts a border cell once", {
  # The cell is 60% country 79 and 40% country 80: it joins 79's totals once,
  # not both countries' at its full mass.
  cells <- tibble::tribble(
    ~lon, ~lat, ~area_code, ~polity_frac, ~cell_area_ha,
    0.25, 0.25, 79L, 0.6, 100000,
    0.25, 0.25, 80L, 0.4, 100000
  )
  hani <- dplyr::filter(.ndc_hani(), .data$lon == 0.25)
  out <- whep::correct_n_deposition(
    hani = hani,
    emep = .ndc_emep(),
    cell_polity = cells,
    reference_years = 1992L,
    area_codes = c(79L, 80L)
  )

  testthat::expect_equal(nrow(out), nrow(hani))
  testthat::expect_equal(.ndc_mass(out, 0.25, 1990L), 2e7)
})

testthat::test_that("correct_n_deposition defaults to the EMEP core countries", {
  cells <- .ndc_cells()
  out <- whep::correct_n_deposition(
    hani = .ndc_hani(),
    emep = .ndc_emep(),
    cell_polity = cells,
    reference_years = 1992L
  )

  # Germany (79) is a core country, China (351) is at the domain edge.
  stamps <- out |>
    dplyr::filter(.data$year == 1990L) |>
    dplyr::arrange(.data$lon) |>
    dplyr::pull("method_deposition")
  testthat::expect_equal(stamps, c("hani_emep_trend", "hani"))
})

testthat::test_that("correct_n_deposition method none returns HaNi as read", {
  out <- .ndc_run(method = "none")

  testthat::expect_equal(out$value_g, .ndc_hani()$value_g)
  pointblank::expect_col_vals_in_set(out, "method_deposition", "hani")
})

testthat::test_that("correct_n_deposition refuses a field that is not HaNi", {
  hani <- .ndc_hani() |> dplyr::mutate(method_deposition = "hani_emep_trend")
  testthat::expect_error(
    whep::correct_n_deposition(
      hani = hani,
      emep = .ndc_emep(),
      cell_polity = .ndc_cells(),
      reference_years = 1992L
    ),
    class = "whep_deposition_not_hani"
  )
  testthat::expect_error(
    whep::correct_n_deposition(
      hani = dplyr::select(.ndc_hani(), -"method_deposition"),
      emep = .ndc_emep(),
      cell_polity = .ndc_cells(),
      reference_years = 1992L
    ),
    class = "whep_deposition_not_hani"
  )
})

testthat::test_that("correct_n_deposition refuses an incomplete reference", {
  # 2019 is outside both fixtures: anchoring on it would be anchoring on
  # nothing.
  testthat::expect_error(
    .ndc_run(reference_years = 1992:2019),
    class = "whep_deposition_reference"
  )
  # An EMEP field on a grid HaNi does not share matches no cell at all.
  shifted <- .ndc_emep() |> dplyr::mutate(lon = .data$lon + 180)
  testthat::expect_error(
    whep::correct_n_deposition(
      hani = .ndc_hani(),
      emep = shifted,
      cell_polity = .ndc_cells(),
      reference_years = 1992L,
      area_codes = 79L
    ),
    class = "whep_deposition_reference"
  )
})

testthat::test_that("correct_n_deposition refuses an EMEP field that is empty", {
  vacuous <- .ndc_emep() |> dplyr::mutate(deposition_kgn_ha = 0)
  hani <- .ndc_hani()

  expect_supplied_guard(
    # The method moves nothing when EMEP is proportional to HaNi in every
    # year (a level gap, not a trend), and an all-zero field is proportional
    # to anything -- so without the guard an absent EMEP would read as "HaNi's
    # trend is already right".
    identity = vacuous |>
      dplyr::inner_join(hani, by = c("lon", "lat", "year")) |>
      dplyr::mutate(ratio = .data$deposition_kgn_ha / .data$value_g) |>
      dplyr::pull("ratio") |>
      dplyr::n_distinct() ==
      1L,
    guard = whep::correct_n_deposition(
      hani = hani,
      emep = vacuous,
      cell_polity = .ndc_cells(),
      reference_years = 1992L,
      area_codes = 79L
    )
  )
})

testthat::test_that("a corrected field keeps its stamp through build_n_deposition", {
  nhx <- .ndc_run(reference_years = 1992L)
  noy <- .ndc_run(reference_years = 1992L)

  out <- whep::build_n_deposition(
    data = list(
      nhx = dplyr::select(nhx, -"deposition_correction"),
      noy = dplyr::select(noy, -"deposition_correction"),
      cell_polity = .ndc_cells()
    ),
    years = 1990L
  )

  stamps <- out |>
    dplyr::distinct(.data$area_code, .data$method_deposition) |>
    dplyr::arrange(.data$area_code)
  testthat::expect_equal(stamps$method_deposition, c("hani_emep_trend", "hani"))
})

testthat::test_that("correct_n_deposition example runs", {
  out <- whep::correct_n_deposition(
    hani = whep::read_n_deposition(example = TRUE),
    emep = whep::read_emep_deposition(example = TRUE),
    cell_polity = tibble::tribble(
      ~lon, ~lat, ~area_code, ~polity_frac, ~cell_area_ha,
      -0.25, -0.25, 79L, 1, 300000
    ),
    reference_years = 2020L
  )
  pointblank::expect_col_vals_equal(out, "deposition_correction", 1)
  pointblank::expect_col_vals_in_set(
    out,
    "method_deposition",
    "hani_emep_trend"
  )
})
