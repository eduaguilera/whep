# Raw-states fixture: two 0.5-degree cells, two years (1750, 2000), the 12
# LUH2 v2h state names. Per cell-year the 12 state fractions sum to 1 exactly,
# so the four carbon-balance classes must also sum to 1.
.luh2_raw_fixture <- function() {
  states <- c(
    "c3ann",
    "c4ann",
    "c3per",
    "c4per",
    "c3nfx",
    "pastr",
    "range",
    "primf",
    "secdf",
    "primn",
    "secdn",
    "urban"
  )
  cell_a <- c(
    0.10,
    0.05,
    0.02,
    0.01,
    0.02,
    0.15,
    0.10,
    0.20,
    0.15,
    0.05,
    0.10,
    0.05
  )
  cell_b <- c(
    0.04,
    0.02,
    0.01,
    0.01,
    0.02,
    0.05,
    0.05,
    0.40,
    0.20,
    0.05,
    0.10,
    0.05
  )
  grid <- tidyr::expand_grid(
    tibble::tibble(lon = c(-3.25, 35.25), lat = c(40.25, -1.25)),
    year = c(1750L, 2000L)
  )
  purrr::pmap_dfr(grid, function(lon, lat, year) {
    frac <- if (lon < 0) cell_a else cell_b
    tibble::tibble(
      lon = lon,
      lat = lat,
      year = year,
      land_use = states,
      fraction = frac
    )
  })
}

# Minimal polycell support: each fixture cell wholly inside one polity, holding
# less land than the cell has area (the coast/lake case). The gap between
# `land_area_ha` and `cell_area_ha` is what makes the two `area_basis` choices
# distinguishable, and what a `cell_area_frac = land_area_ha / cell_area_ha`
# definition would remove a second time.
.luh2_country_grid_fixture <- function() {
  tibble::tribble(
    ~lon, ~lat, ~area_code, ~cell_area_frac, ~cell_area_ha, ~land_area_ha,
    -3.25, 40.25, 203L, 1, whep:::.luh2_cell_area_ha(40.25), 200000,
    35.25, -1.25, 79L, 1, whep:::.luh2_cell_area_ha(-1.25), 250000
  )
}

test_that("example returns the documented schema", {
  ex <- whep::read_luh2_landuse(example = TRUE)
  pointblank::expect_col_exists(
    ex,
    c(
      "lon",
      "lat",
      "area_code",
      "year",
      "land_use",
      "fraction",
      "area_ha",
      "method_land_area"
    )
  )
  testthat::expect_true(all(
    ex$land_use %in% c("cropland", "grassland", "natural", "urban")
  ))
  testthat::expect_true(all(ex$fraction >= 0 & ex$fraction <= 1))
  # the fixture carries a real border cell, so the schema example itself shows
  # one `fraction` shared by two polycells with different `area_ha`
  border <- ex |> dplyr::filter(lon == 9.25, lat == 47.75)
  testthat::expect_setequal(border$area_code, c(79L, 211L))
  testthat::expect_equal(
    dplyr::n_distinct(round(border$fraction[border$land_use == "cropland"], 7)),
    1L
  )
})

test_that("class mapping sums member states and fractions tile to ~1", {
  raw <- .luh2_raw_fixture()
  out <- whep::read_luh2_landuse(
    resolution = "grid",
    data = list(states = raw, country_grid = .luh2_country_grid_fixture())
  )

  testthat::expect_setequal(
    unique(out$land_use),
    c("cropland", "grassland", "natural", "urban")
  )

  # cropland class aggregates the five crop states for cell A (lon -3.25)
  crop_a <- out |>
    dplyr::filter(lon == -3.25, year == 1750L, land_use == "cropland") |>
    dplyr::pull(fraction)
  testthat::expect_equal(crop_a, 0.10 + 0.05 + 0.02 + 0.01 + 0.02)

  # grassland class aggregates the pastr and range states
  grass_a <- out |>
    dplyr::filter(lon == -3.25, year == 1750L, land_use == "grassland") |>
    dplyr::pull(fraction)
  testthat::expect_equal(grass_a, 0.15 + 0.10)

  # per cell-year the 4 classes tile to ~1
  totals <- out |>
    dplyr::summarise(tot = sum(fraction), .by = c(lon, lat, year)) |>
    dplyr::pull(tot)
  testthat::expect_true(all(abs(totals - 1) < 1e-8))
})

test_that(".luh2_cell_area_ha equator ~309100 ha and shrinks with latitude", {
  eq <- whep:::.luh2_cell_area_ha(0)
  testthat::expect_equal(eq, 309100, tolerance = 0.01)
  mid <- whep:::.luh2_cell_area_ha(45)
  high <- whep:::.luh2_cell_area_ha(60)
  testthat::expect_true(eq > mid && mid > high)
  # cos-law: 45-deg cell ~ cos(45) of equatorial
  testthat::expect_equal(mid / eq, cos(45 * pi / 180), tolerance = 0.01)
})

# RE-PINNED for DA-26 (C7). Before the migration `area_ha` was
# `fraction * cell_area_ha`, and that identity held only because the centroid
# grid gave every cell wholly to one polity: on a border cell it was already
# false, because `area_ha` was scaled by the polity's share. DA-26 replaces the
# whole-cell area with the polycell's measured land, so the identity that
# survives is the one on the LAND: each class keeps its share of the cell's LUH2
# land and that share is spread over `land_area_ha`. Pinning the old identity
# would pin the defect, so it is restated rather than preserved.
test_that("area_ha is the class share of LUH2 land times the polycell's land", {
  raw <- .luh2_raw_fixture()
  cg <- .luh2_country_grid_fixture()
  out <- whep::read_luh2_landuse(
    resolution = "grid",
    data = list(states = raw, country_grid = cg)
  )
  chk <- out |>
    dplyr::mutate(
      luh2_area = fraction * whep:::.luh2_cell_area_ha(lat),
      class_share = luh2_area / sum(luh2_area),
      .by = c(lon, lat, year)
    ) |>
    dplyr::left_join(
      dplyr::select(cg, lon, lat, area_code, land_area_ha),
      by = c("lon", "lat", "area_code")
    ) |>
    dplyr::mutate(expected = class_share * land_area_ha)
  testthat::expect_equal(chk$area_ha, chk$expected, tolerance = 1e-6)
  # and the old identity is now false, by the land/cell-area gap
  testthat::expect_false(isTRUE(all.equal(
    chk$area_ha,
    chk$luh2_area,
    tolerance = 1e-6
  )))
})

test_that("the four classes tile the polycell's own measured land", {
  cg <- .luh2_country_grid_fixture()
  out <- whep::read_luh2_landuse(
    resolution = "grid",
    data = list(states = .luh2_raw_fixture(), country_grid = cg)
  )
  tiled <- out |>
    dplyr::summarise(
      total_ha = sum(area_ha),
      .by = c(lon, lat, area_code, year)
    ) |>
    dplyr::left_join(
      dplyr::select(cg, lon, lat, area_code, land_area_ha),
      by = c("lon", "lat", "area_code")
    )
  testthat::expect_equal(tiled$total_ha, tiled$land_area_ha, tolerance = 1e-9)
})

test_that("the class composition is unchanged by the rescale", {
  raw <- .luh2_raw_fixture()
  cg <- .luh2_country_grid_fixture()
  shares <- function(basis) {
    whep::read_luh2_landuse(
      resolution = "grid",
      area_basis = basis,
      data = list(states = raw, country_grid = cg)
    ) |>
      dplyr::mutate(share = area_ha / sum(area_ha), .by = c(lon, lat, year)) |>
      dplyr::arrange(lon, lat, year, land_use) |>
      dplyr::pull(share)
  }
  testthat::expect_equal(shares("polycell_land"), shares("luh2_fraction"))
})

test_that("area_basis = 'luh2_fraction' reproduces the pre-DA-26 areas", {
  # The transitional basis is the exact arithmetic C7 replaced, kept selectable
  # so the crosswalk change and the area change can be measured apart.
  raw <- .luh2_raw_fixture()
  out <- whep::read_luh2_landuse(
    resolution = "grid",
    area_basis = "luh2_fraction",
    data = list(states = raw, country_grid = .luh2_country_grid_fixture())
  )
  testthat::expect_equal(
    out$area_ha,
    out$fraction * whep:::.luh2_cell_area_ha(out$lat),
    tolerance = 1e-6
  )
  testthat::expect_true(all(out$method_land_area == "luh2_fraction"))
})

test_that("the chosen area basis is recorded on the output", {
  raw <- .luh2_raw_fixture()
  cg <- .luh2_country_grid_fixture()
  for (basis in c("polycell_land", "luh2_fraction")) {
    for (res in c("grid", "polity")) {
      out <- whep::read_luh2_landuse(
        resolution = res,
        area_basis = basis,
        data = list(states = raw, country_grid = cg)
      )
      pointblank::expect_col_exists(out, "method_land_area")
      testthat::expect_true(all(out$method_land_area == basis))
    }
  }
})

test_that("polycell_land refuses a support with no measured land", {
  # A legacy country grid carries no land_area_ha. Falling back to LUH2's own
  # area would be a silent downgrade of the caller's explicit default.
  legacy <- tibble::tribble(
    ~lon, ~lat, ~area_code, ~cell_area_frac,
    -3.25, 40.25, 203L, 1
  )
  testthat::expect_error(
    whep::read_luh2_landuse(
      resolution = "grid",
      data = list(states = .luh2_raw_fixture(), country_grid = legacy)
    ),
    "land_area_ha"
  )
})

test_that("a border cell's land is partitioned, never duplicated", {
  states <- tibble::tibble(
    lon = 0.25,
    lat = 40.25,
    year = 2000L,
    land_use = c("c3ann", "pastr"),
    fraction = c(0.4, 0.2)
  )
  support <- tibble::tribble(
    ~lon, ~lat, ~area_code, ~cell_area_ha, ~land_area_ha, ~cell_area_frac,
    0.25, 40.25, 1L, 100000, 20000, 0.25,
    0.25, 40.25, 2L, 100000, 60000, 0.75
  )
  out <- whep::read_luh2_landuse(
    resolution = "grid",
    data = list(states = states, country_grid = support)
  ) |>
    dplyr::arrange(area_code, land_use)
  # each polycell's classes tile ITS land, and the two together tile the cell's
  testthat::expect_equal(
    out |>
      dplyr::summarise(t = sum(area_ha), .by = area_code) |>
      dplyr::pull(t),
    c(20000, 60000)
  )
  testthat::expect_equal(sum(out$area_ha), 80000)
  # composition identical in both polities (S-A3: no quantity crosses over)
  testthat::expect_equal(
    out$area_ha[out$area_code == 1L] / 20000,
    out$area_ha[out$area_code == 2L] / 60000
  )
})

test_that("land LUH2 gives no composition for is reported, not invented", {
  states <- tibble::tibble(
    lon = 0.25,
    lat = 40.25,
    year = 2000L,
    land_use = "c3ann",
    fraction = 0
  )
  support <- tibble::tribble(
    ~lon, ~lat, ~area_code, ~cell_area_ha, ~land_area_ha, ~cell_area_frac,
    0.25, 40.25, 1L, 100000, 30000, 1
  )
  testthat::expect_warning(
    out <- whep::read_luh2_landuse(
      resolution = "grid",
      data = list(states = states, country_grid = support)
    ),
    "no land-use composition"
  )
  testthat::expect_equal(sum(out$area_ha), 0)
})

test_that("read_luh2_landuse refuses a support that folds two polities", {
  support <- tibble::tribble(
    ~lon, ~lat, ~area_code, ~cell_area_ha, ~land_area_ha, ~cell_area_frac,
    0.25, 40.25, 206L, 100000, 20000, 0.25,
    0.25, 40.25, 206L, 100000, 60000, 0.75
  )
  testthat::expect_error(
    whep::read_luh2_landuse(
      resolution = "grid",
      data = list(states = .luh2_raw_fixture(), country_grid = support)
    ),
    "one row per cell"
  )
})

test_that("build_cell_polity-shaped fractions split LUH2 border cells", {
  states <- tibble::tibble(
    lon = 0.25,
    lat = 40.25,
    year = 2000L,
    land_use = "c3ann",
    fraction = 1
  )
  # build_cell_polity() calls this column polity_frac; it has the same
  # physical-cell compartment meaning as cell_area_frac. Under the transitional
  # basis LUH2's own land total is preserved and only the split changes, which
  # is what makes it the control for the crosswalk half of the migration.
  cell_polity <- tibble::tribble(
    ~lon, ~lat, ~area_code, ~polity_frac, ~cell_area_ha,
    0.25, 40.25, 1L, 0.25, 100,
    0.25, 40.25, 2L, 0.75, 100
  )

  out <- whep::read_luh2_landuse(
    resolution = "grid",
    area_basis = "luh2_fraction",
    data = list(states = states, country_grid = cell_polity)
  ) |>
    dplyr::arrange(.data$area_code)
  physical_area <- whep:::.luh2_cell_area_ha(40.25)

  testthat::expect_equal(
    out$area_ha,
    physical_area * c(0.25, 0.75),
    tolerance = 1e-6
  )
  testthat::expect_equal(sum(out$area_ha), physical_area, tolerance = 1e-6)
})

test_that("1750 is retrievable when the source covers it", {
  raw <- .luh2_raw_fixture()
  out <- whep::read_luh2_landuse(
    years = 1750L,
    data = list(states = raw, country_grid = .luh2_country_grid_fixture())
  )
  testthat::expect_true(all(out$year == 1750L))
  testthat::expect_true(nrow(out) > 0L)
})

test_that("polity resolution sums area_ha to (area_code, year, land_use)", {
  raw <- .luh2_raw_fixture()
  out <- whep::read_luh2_landuse(
    resolution = "polity",
    data = list(states = raw, country_grid = .luh2_country_grid_fixture())
  )
  pointblank::expect_col_exists(
    out,
    c("area_code", "year", "land_use", "area_ha")
  )
  testthat::expect_false(any(c("lon", "lat") %in% names(out)))
  testthat::expect_setequal(unique(out$area_code), c(203L, 79L))
})

test_that(".luh2_nc_years defaults to calendar years 850..end, not indices", {
  # time_len = 1166 -> a complete v2h file spanning 850..2015. The default must
  # be calendar years (base 850), never bare 1-based time indices (1..1166),
  # which would be mis-read as pre-850 calendar years and abort.
  testthat::local_mocked_bindings(.luh2_time_len_nc = function(nc_path) 1166L)
  yrs <- whep:::.luh2_nc_years("ignored.nc")
  testthat::expect_equal(yrs[1], 850L)
  testthat::expect_equal(yrs[length(yrs)], 2015L)
  testthat::expect_equal(length(yrs), 1166L)
  testthat::expect_false(any(yrs < 850L))
})

# Record which nc path each source dispatch resolved to, without reading it.
.luh2_mock_nc_reader <- function() {
  testthat::local_mocked_bindings(
    .luh2_read_states_nc = function(nc_path, years = NULL, origin = "local") {
      tibble::tibble(nc_path = nc_path, origin = origin, year = years)
    },
    .package = "whep",
    .env = parent.frame()
  )
}

test_that("a local tree is preferred over a 6.7 GB download", {
  # Issue #457: the Zenodo payload is byte-identical to what a local tree holds,
  # so there is nothing to gain from re-fetching 6.7 GB when the file is present.
  states_dir <- withr::local_tempdir()
  file.create(file.path(states_dir, "states.nc"))
  .luh2_mock_nc_reader()
  testthat::local_mocked_bindings(
    .luh2_states_dir = function() states_dir,
    .luh2_zenodo_states = function(...) {
      stop("Zenodo must not be reached while a local tree exists")
    },
    .package = "whep"
  )

  out <- whep:::.luh2_read_states_source(years = 1750L)
  testthat::expect_equal(out$origin, "local")
  testthat::expect_equal(out$nc_path, file.path(states_dir, "states.nc"))
})

test_that("states_source = 'zenodo' ignores a local tree", {
  # The reproducibility lever: insist on the checksum-verified reference vintage
  # even when WHEP_LUH2_DIR holds something.
  states_dir <- withr::local_tempdir()
  file.create(file.path(states_dir, "states.nc"))
  .luh2_mock_nc_reader()
  testthat::local_mocked_bindings(
    .luh2_states_dir = function() states_dir,
    .luh2_zenodo_states = function(...) "/cache/states.nc",
    .package = "whep"
  )

  out <- whep:::.luh2_read_states_source(
    years = 1750L,
    states_source = "zenodo"
  )
  testthat::expect_equal(out$origin, "zenodo")
  testthat::expect_equal(out$nc_path, "/cache/states.nc")
})

test_that("auto downloads from Zenodo when there is no local tree", {
  .luh2_mock_nc_reader()
  testthat::local_mocked_bindings(
    .luh2_states_dir = function() "",
    .luh2_zenodo_states = function(...) "/cache/states.nc",
    .package = "whep"
  )

  out <- whep:::.luh2_read_states_source(years = 1750L)
  testthat::expect_equal(out$origin, "zenodo")
})

test_that("states_source = 'local' aborts rather than downloading", {
  .luh2_mock_nc_reader()
  testthat::local_mocked_bindings(
    .luh2_states_dir = function() "",
    .luh2_zenodo_states = function(...) {
      stop("Zenodo must not be reached when the local tree was asked for")
    },
    .package = "whep"
  )

  testthat::expect_error(
    whep:::.luh2_read_states_source(years = 1750L, states_source = "local"),
    "No local LUH2 states tree"
  )
})

test_that("an unset LUH2 directory cannot select a current-directory file", {
  temp_dir <- withr::local_tempdir()
  withr::local_dir(temp_dir)
  file.create("states.nc")
  testthat::local_mocked_bindings(
    .luh2_states_dir = function() "",
    .package = "whep"
  )

  testthat::expect_null(whep:::.luh2_local_states_nc())
})

# ---- Zenodo cache ----------------------------------------------------------

test_that("a cached states.nc of the published size is not re-downloaded", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "states.nc")
  writeBin(raw(1L), path)
  # stand in for the 6.7 GB payload: claim its size rather than allocating it
  testthat::local_mocked_bindings(
    .luh2_states_bytes = function() 1,
    .package = "whep"
  )

  out <- whep:::.luh2_zenodo_states(
    dir = dir,
    download = function(...) stop("a full cache must not re-download")
  )
  testthat::expect_equal(out, path)
})

test_that("a truncated cached file is re-downloaded, not read", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "states.nc")
  writeBin(raw(2L), path)
  testthat::local_mocked_bindings(
    .luh2_states_bytes = function() 100,
    .package = "whep"
  )

  called <- FALSE
  out <- whep:::.luh2_zenodo_states(
    dir = dir,
    download = function(p) {
      called <<- TRUE
      p
    }
  )
  testthat::expect_true(called)
  testthat::expect_equal(out, path)
})

test_that("a download that fails its MD5 is deleted, not kept", {
  # A kept mismatching file of the right size would pass the cheap size check
  # forever after, so verification must remove it.
  dir <- withr::local_tempdir()
  path <- file.path(dir, "states.nc")
  writeBin(as.raw(1:4), path)

  testthat::expect_error(
    whep:::.luh2_verify_download(path, 0L),
    "does not match the published MD5"
  )
  testthat::expect_false(file.exists(path))
})

test_that("a download error is reported with the manual instruction", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "states.nc")

  testthat::expect_error(
    whep:::.luh2_verify_download(path, simpleError("host unreachable")),
    "host unreachable"
  )
})

test_that("the download lifts R's timeout entirely, then restores it", {
  # R's default download timeout is 60 s: a 6.7 GB fetch cannot finish inside
  # that, and any finite replacement is a guess about someone else's bandwidth.
  # 0 disables the timeout in libcurl (verified: a 75 s drip completes under 0
  # and is cut off at exactly 60.0 s under 60), so it must be exactly 0 here --
  # max(old, 0) would silently leave the 60 s default in place.
  dir <- withr::local_tempdir()
  path <- file.path(dir, "states.nc")
  withr::local_options(timeout = 60)
  seen <- NULL

  testthat::expect_error(
    whep:::.luh2_download_states(
      path,
      fetch = function(p) {
        seen <<- getOption("timeout")
        writeBin(as.raw(1:4), p)
        0L
      }
    ),
    "does not match the published MD5"
  )
  testthat::expect_identical(seen, 0L)
  testthat::expect_equal(getOption("timeout"), 60)
})

test_that("a matching download passes verification", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "states.nc")
  writeBin(as.raw(1:4), path)
  digest <- unname(tools::md5sum(path))
  testthat::local_mocked_bindings(
    .luh2_states_md5 = function() digest,
    .package = "whep"
  )

  testthat::expect_equal(whep:::.luh2_verify_download(path, 0L), path)
  testthat::expect_true(file.exists(path))
})

test_that("the published Zenodo identifiers are the ones that were verified", {
  # These three constants are the whole provenance guarantee; a silent edit to
  # any of them would let a different product through as the reference vintage.
  testthat::expect_equal(
    whep:::.luh2_states_md5(),
    "411ef3d657c3108942954c895f658a17"
  )
  testthat::expect_equal(whep:::.luh2_states_bytes(), 6657587367)
  testthat::expect_equal(whep:::.luh2_states_doi(), "10.5281/zenodo.15556812")
  testthat::expect_match(whep:::.luh2_states_url(), "records/15556812")
})

test_that("the retired luh2_v2h_states pin is no longer registered", {
  # Issue #457: the pin was a byte-identical, unversioned mirror of the Zenodo
  # asset. Leaving the row behind would keep a second acquisition path alive.
  testthat::expect_false("luh2_v2h_states" %in% whep::whep_inputs$alias)
})

# Build a tiny synthetic LUH2 v2h states.nc (native grid, one time slice = year
# 850) carrying the 12 state fractions, so the real NetCDF parser and class
# mapper can be smoke-tested on CI without the 6.7 GB real states.nc.
# `global_attrs` writes the CF global attributes the vintage is read from.
.luh2_fixture_states_nc <- function(
  global_attrs = list(source_id = "UofMD-landState-LUH2-GCB2022")
) {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  lon <- c(-179.875, -179.625, -179.375, -179.125)
  lat <- c(89.875, 89.625)
  frac <- c(
    c3ann = 0.10,
    c4ann = 0.02,
    c3per = 0.01,
    c4per = 0.00,
    c3nfx = 0.02,
    pastr = 0.20,
    range = 0.05,
    primf = 0.40,
    secdf = 0.05,
    primn = 0.02,
    secdn = 0.01,
    urban = 0.05
  )
  fill <- 1e20
  dim_lon <- ncdf4::ncdim_def("lon", "degrees_east", lon)
  dim_lat <- ncdf4::ncdim_def("lat", "degrees_north", lat)
  dim_time <- ncdf4::ncdim_def("time", "years since 850-1-1", 0)
  vars <- lapply(names(frac), \(s) {
    ncdf4::ncvar_def(s, "1", list(dim_lon, dim_lat, dim_time), missval = fill)
  })
  path <- file.path(dir, "states.nc")
  nc <- ncdf4::nc_create(path, vars)
  for (s in names(frac)) {
    arr <- array(frac[[s]], dim = c(length(lon), length(lat), 1L))
    arr[1L, 1L, 1L] <- fill # one ocean sub-cell -> NA, to exercise the drop
    ncdf4::ncvar_put(nc, s, arr)
  }
  for (nm in names(global_attrs)) {
    ncdf4::ncatt_put(nc, 0, nm, global_attrs[[nm]])
  }
  ncdf4::nc_close(nc)
  list(
    path = path,
    year = 850L,
    cropland = sum(frac[c("c3ann", "c4ann", "c3per", "c4per", "c3nfx")])
  )
}

test_that("states.nc parser reads a synthetic file and maps 12 states to 4 classes", {
  fx <- .luh2_fixture_states_nc()
  states <- whep:::.luh2_read_states_nc(fx$path, years = fx$year)

  pointblank::expect_col_exists(
    states,
    c("lon", "lat", "year", "land_use", "fraction")
  )
  testthat::expect_true(all(states$year == 850L))
  testthat::expect_true(all(
    states$fraction >= -1e-9 & states$fraction <= 1.001
  ))
  # all 12 native states survive the slice/aggregate
  testthat::expect_setequal(
    unique(states$land_use),
    c(
      "c3ann",
      "c4ann",
      "c3per",
      "c4per",
      "c3nfx",
      "pastr",
      "range",
      "primf",
      "secdf",
      "primn",
      "secdn",
      "urban"
    )
  )

  mapped <- whep:::.luh2_map_classes(states)
  testthat::expect_setequal(
    unique(mapped$land_use),
    c("cropland", "grassland", "natural", "urban")
  )
  # 12 -> 4 conserves fraction: cropland = sum of the five crop states. The
  # fully-present 0.5-degree cell reaches the exact sum (ocean-adjacent cells
  # fall short by their dropped ocean area, so use the max cell).
  crop <- mapped$fraction[mapped$land_use == "cropland"]
  testthat::expect_true(all(crop > 0 & crop <= fx$cropland + 1e-6))
  testthat::expect_equal(max(crop), fx$cropland, tolerance = 1e-6)
})

test_that("the LUH2 vintage read is recorded on the result", {
  # Issue #457: the base v2h release (850-2015) and the Global Carbon Budget
  # variants (850-2022) are different products, so a result must say which one
  # produced it instead of citing the base release by assumption.
  fx <- .luh2_fixture_states_nc()
  states <- whep:::.luh2_read_states_nc(
    fx$path,
    years = fx$year,
    origin = "zenodo"
  )
  prov <- whep::get_provenance(states)

  pointblank::expect_col_exists(
    prov,
    c("input_alias", "input_version", "input_origin", "input_source_id")
  )
  testthat::expect_equal(prov$input_alias, "luh2_states")
  testthat::expect_equal(prov$input_source_id, "UofMD-landState-LUH2-GCB2022")
  testthat::expect_equal(prov$input_origin, "zenodo")
  testthat::expect_equal(prov$input_first_year, 850L)
  testthat::expect_equal(prov$input_last_year, 850L)
  # the Zenodo record is only claimed when the verified download was the source
  testthat::expect_equal(prov$input_version, "10.5281/zenodo.15556812")

  local_read <- whep:::.luh2_read_states_nc(fx$path, years = fx$year)
  local_prov <- whep::get_provenance(local_read)
  testthat::expect_equal(local_prov$input_origin, "local")
  testthat::expect_true(is.na(local_prov$input_version))
})

test_that("a base-v2h file without source_id falls back to its other attributes", {
  fx <- .luh2_fixture_states_nc(
    global_attrs = list(dataset_version_number = "LUH2 v2h")
  )
  testthat::expect_equal(
    whep:::.luh2_nc_source_id(fx$path),
    "LUH2 v2h"
  )

  bare <- .luh2_fixture_states_nc(global_attrs = list())
  testthat::expect_true(is.na(whep:::.luh2_nc_source_id(bare$path)))
})

test_that("read_luh2_landuse carries the vintage through to its output", {
  fx <- .luh2_fixture_states_nc()
  testthat::local_mocked_bindings(
    .luh2_read_states_source = function(years = NULL, ...) {
      whep:::.luh2_read_states_nc(fx$path, years = years, origin = "zenodo")
    },
    .luh2_read_country_grid = function() {
      tibble::tibble(
        lon = -179.75,
        lat = 89.75,
        area_code = 1L,
        cell_area_frac = 1,
        cell_area_ha = whep:::.luh2_cell_area_ha(89.75),
        land_area_ha = 1000
      )
    },
    .package = "whep"
  )

  out <- whep::read_luh2_landuse(resolution = "grid", years = fx$year)
  testthat::expect_equal(
    whep::get_provenance(out)$input_source_id,
    "UofMD-landState-LUH2-GCB2022"
  )

  # injected states carry no vintage, so no record is invented for them
  plain <- whep::read_luh2_landuse(
    data = list(
      states = .luh2_raw_fixture(),
      country_grid = .luh2_country_grid_fixture()
    )
  )
  testthat::expect_null(whep::get_provenance(plain))
})

test_that("real Zenodo cache smoke test (skipped when not populated)", {
  # 6.7 GB: never download during a test run. Only assert against a cache that
  # some earlier real run already filled.
  testthat::skip_on_ci()
  cached <- file.path(whep:::.luh2_cache_dir(), "states.nc")
  testthat::skip_if(
    !file.exists(cached),
    "LUH2 Zenodo cache not populated"
  )
  testthat::expect_true(whep:::.luh2_cached_size_ok(cached))
  testthat::expect_equal(
    whep:::.luh2_nc_source_id(cached),
    whep:::.luh2_reference_source_id()
  )
  # the cache is only ever written after an MD5 match, so it must still match
  testthat::expect_equal(
    unname(tools::md5sum(cached)),
    whep:::.luh2_states_md5()
  )
})

# ---- Real states.nc smoke test ---------------------------------------------
# The real grid from wherever this machine has it: a WHEP_LUH2_DIR tree if one is
# set, else the populated Zenodo cache. Resolving both matters because the
# canonical home is now the cache, so keying these plausibility checks on the env
# var alone would silently stop exercising them.
.luh2_test_states_path <- function() {
  local_nc <- file.path(Sys.getenv("WHEP_LUH2_DIR", ""), "states.nc")
  if (nzchar(Sys.getenv("WHEP_LUH2_DIR")) && file.exists(local_nc)) {
    return(local_nc)
  }
  file.path(whep:::.luh2_cache_dir(), "states.nc")
}

# The arguments a real-data read needs. C7 moved the default reader onto the
# polycell support; when that artifact is not published the same states.nc is
# still read, on the transitional basis over the centroid grid, so the reader
# itself stays covered instead of the whole block skipping. Which basis ran is
# printed, never left implicit.
.luh2_test_read_args <- function() {
  support <- try(whep:::.carbon_cell_support(), silent = TRUE)
  if (!inherits(support, "try-error")) {
    cat("[real-states read on the polycell support]\n")
    return(list(basis = "polycell_land", grid = support))
  }
  cat(
    "[no polycell support: real-states read on the centroid grid,",
    "area_basis = 'luh2_fraction', cell_area_frac DECLARED = 1]\n"
  )
  # C8/S-A5: `.normalize_country_grid()` no longer defaults a missing share to
  # 1, so the centroid pin (`lon`, `lat`, `area_code` only) is refused. The
  # whole-cell convention it implies is DECLARED here instead of inferred
  # inside the normaliser -- same values, but the fallback can no longer be
  # mistaken for a polity-resolved split. It is a smoke check on the reader,
  # not a production path; the production path has no fallback (AM-38).
  list(
    basis = "luh2_fraction",
    grid = dplyr::mutate(
      whep::whep_read_file("spatialize-country-grid"),
      cell_area_frac = 1
    )
  )
}

test_that("the real states.nc reads at 0.5 deg with plausible year-2000 land use", {
  testthat::skip_if(
    !file.exists(.luh2_test_states_path()),
    "no real LUH2 states.nc present"
  )
  args <- .luh2_test_read_args()
  out <- whep::read_luh2_landuse(
    resolution = "grid",
    years = 2000L,
    area_basis = args$basis,
    data = list(country_grid = args$grid)
  )
  local_states <- whep:::.luh2_read_states_nc(
    .luh2_test_states_path(),
    years = 2000L,
    origin = "local"
  )

  # 0.5-degree grid on the standard 0.5 centres
  testthat::expect_true(all(out$year == 2000L))
  lon_off <- (out$lon + 179.75) %% 0.5
  lat_off <- (out$lat - 83.75) %% 0.5
  testthat::expect_true(all(abs(pmin(lon_off, 0.5 - lon_off)) < 1e-6))
  testthat::expect_true(all(abs(pmin(lat_off, 0.5 - lat_off)) < 1e-6))

  # fractions in 0..1
  testthat::expect_true(all(out$fraction >= -1e-9 & out$fraction <= 1 + 1e-6))

  # per-cell 4-class totals: inland cells tile to ~1; coastal cells fall short
  # by their ocean fraction, so no cell exceeds ~1 and inland cells hit 1.
  totals <- out |>
    dplyr::summarise(tot = sum(fraction), .by = c(lon, lat))
  testthat::expect_true(all(totals$tot < 1.02))
  # a fully-inland central-Europe cell tiles to ~1
  ce_tot <- totals |>
    dplyr::filter(abs(lon - 9.25) < 0.01, abs(lat - 48.25) < 0.01) |>
    dplyr::pull(tot)
  testthat::expect_true(length(ce_tot) == 1L && abs(ce_tot - 1) < 0.02)

  # that central-Europe cell has meaningful cropland
  crop_ce <- out |>
    dplyr::filter(
      abs(lon - 9.25) < 0.01,
      abs(lat - 48.25) < 0.01,
      land_use == "cropland"
    ) |>
    dplyr::pull(fraction)
  testthat::expect_true(length(crop_ce) == 1L && crop_ce > 0.1)

  # Global cropland area: the country-grid join (centroid rasterization) only
  # retains cells assignable to a polity, so it undercounts the physical
  # global total. Verify the reader's own aggregation against the ~1.5 Gha
  # literature value on the pre-join gridded classes.
  phys <- local_states |>
    whep:::.luh2_map_classes()
  phys$area_ha <- phys$fraction * whep:::.luh2_cell_area_ha(phys$lat)
  gcrop <- phys |>
    dplyr::filter(land_use == "cropland") |>
    dplyr::summarise(gha = sum(area_ha) / 1e9) |>
    dplyr::pull(gha)
  gland <- sum(phys$area_ha) / 1e9
  cat(
    "\nYear-2000 global cropland (Gha):",
    round(gcrop, 3),
    "| total land (Gha):",
    round(gland, 3),
    "\n"
  )
  print(utils::head(as.data.frame(out)))
  testthat::expect_true(gcrop > 1.3 && gcrop < 1.7)
  testthat::expect_true(gland > 12 && gland < 14)
})

test_that("the real states.nc is readable for 1750", {
  testthat::skip_if(
    !file.exists(.luh2_test_states_path()),
    "no real LUH2 states.nc present"
  )
  args <- .luh2_test_read_args()
  out <- whep::read_luh2_landuse(
    resolution = "grid",
    years = 1750L,
    area_basis = args$basis,
    data = list(country_grid = args$grid)
  )
  testthat::expect_true(all(out$year == 1750L))
  testthat::expect_true(nrow(out) > 0L)
  testthat::expect_true(
    whep::get_provenance(out)$input_origin %in% c("local", "zenodo")
  )
})

test_that("the real reference states.nc matches its published MD5", {
  # Issue #457: the retired pin was byte-identical to the Zenodo asset. That is
  # what makes the published MD5 usable as the vintage check rather than a
  # recorded claim -- assert it against the real file when one is present.
  testthat::skip_on_ci()
  real_nc <- .luh2_test_states_path()
  testthat::skip_if(!file.exists(real_nc), "no real LUH2 states.nc present")
  testthat::skip_if(
    whep:::.luh2_nc_source_id(real_nc) != whep:::.luh2_reference_source_id(),
    "the real states.nc is a different LUH2 vintage than the reference"
  )

  testthat::expect_equal(file.size(real_nc), whep:::.luh2_states_bytes())
  testthat::expect_equal(
    unname(tools::md5sum(real_nc)),
    whep:::.luh2_states_md5()
  )
})

test_that("an off-vintage local tree warns instead of passing silently", {
  fx <- .luh2_fixture_states_nc(
    global_attrs = list(source_id = "LUH2 v2h")
  )
  testthat::expect_warning(
    whep:::.luh2_read_states_nc(fx$path, years = fx$year, origin = "local"),
    "not the reference vintage"
  )
  # the reference vintage, and any non-local origin, stay quiet
  ref <- .luh2_fixture_states_nc()
  testthat::expect_no_warning(
    whep:::.luh2_read_states_nc(ref$path, years = ref$year, origin = "local")
  )
})
