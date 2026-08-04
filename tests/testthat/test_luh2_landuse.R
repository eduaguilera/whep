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

# Minimal country grid: each fixture cell wholly inside one polity.
.luh2_country_grid_fixture <- function() {
  tibble::tribble(
    ~lon, ~lat, ~area_code, ~cell_area_frac,
    -3.25, 40.25, 203L, 1,
    35.25, -1.25, 79L, 1
  )
}

test_that("example returns the documented schema", {
  ex <- whep::read_luh2_landuse(example = TRUE)
  pointblank::expect_col_exists(
    ex,
    c("lon", "lat", "area_code", "year", "land_use", "fraction", "area_ha")
  )
  testthat::expect_true(all(
    ex$land_use %in% c("cropland", "grassland", "natural", "urban")
  ))
  testthat::expect_true(all(ex$fraction >= 0 & ex$fraction <= 1))
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

test_that("area_ha equals fraction times cell_area_ha", {
  raw <- .luh2_raw_fixture()
  out <- whep::read_luh2_landuse(
    resolution = "grid",
    data = list(states = raw, country_grid = .luh2_country_grid_fixture())
  )
  chk <- out |>
    dplyr::mutate(
      expected = fraction * whep:::.luh2_cell_area_ha(lat)
    )
  testthat::expect_equal(chk$area_ha, chk$expected, tolerance = 1e-6)
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
  # physical-cell compartment meaning as cell_area_frac.
  cell_polity <- tibble::tribble(
    ~lon, ~lat, ~area_code, ~polity_frac, ~cell_area_ha,
    0.25, 40.25, 1L, 0.25, 100,
    0.25, 40.25, 2L, 0.75, 100
  )

  out <- whep::read_luh2_landuse(
    resolution = "grid",
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

test_that("the pin is the primary states source, not the local directory", {
  # Issue #457: WHEP_LUH2_DIR may point at any of the v2h trees in circulation,
  # so the pin (which names its vintage) must win whenever it is readable.
  testthat::local_mocked_bindings(
    .luh2_states_dir = function() "/local/tree",
    .luh2_read_states_local = function(years = NULL) {
      stop("the local tree must not be read while the pin is readable")
    },
    .luh2_read_states = function(years = NULL) {
      tibble::tibble(origin = "pin", year = years)
    },
    .package = "whep"
  )

  out <- whep:::.luh2_read_states_source(years = 1750L)
  testthat::expect_equal(out$origin, "pin")
})

test_that("states_source = 'local' skips the pin outright", {
  testthat::local_mocked_bindings(
    .luh2_read_states = function(years = NULL) {
      stop("the pin must not be fetched when the local tree was asked for")
    },
    .luh2_read_states_local = function(years = NULL) {
      tibble::tibble(origin = "local", year = years)
    },
    .package = "whep"
  )

  out <- whep:::.luh2_read_states_source(
    years = 1750L,
    states_source = "local"
  )
  testthat::expect_equal(out$origin, "local")
})

test_that("an unreadable pin falls back to the local directory with a warning", {
  testthat::local_mocked_bindings(
    .luh2_read_states = function(years = NULL) stop("board unreachable"),
    .luh2_read_states_local = function(years = NULL) {
      tibble::tibble(origin = "local", year = years)
    },
    .package = "whep"
  )

  testthat::expect_warning(
    out <- whep:::.luh2_read_states_source(years = 1750L),
    "Falling back"
  )
  testthat::expect_equal(out$origin, "local")
})

test_that("an unset LUH2 directory cannot select a current-directory file", {
  temp_dir <- withr::local_tempdir()
  withr::local_dir(temp_dir)
  file.create("states.nc")
  testthat::local_mocked_bindings(
    .luh2_states_dir = function() "",
    .luh2_read_states_nc = function(...) {
      stop("current-directory states.nc must not be read")
    },
    .package = "whep"
  )

  testthat::expect_error(
    whep:::.luh2_read_states_local(years = 1750L),
    "No LUH2 v2h states source"
  )
})

# Build a tiny synthetic LUH2 v2h states.nc (native grid, one time slice = year
# 850) carrying the 12 state fractions, so the real NetCDF parser and class
# mapper can be smoke-tested on CI without the ~6.6 GB luh2_v2h_states pin.
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

test_that("the pin is fetched as a NetCDF path, not as a table", {
  # Issue #457: the payload is one .nc member, so the pin reader asked for the
  # default type = "parquet" and aborted with "this input has no parquet file".
  fx <- .luh2_fixture_states_nc()
  asked <- NULL
  testthat::local_mocked_bindings(
    whep_read_file = function(file_alias, type = "parquet", ...) {
      asked <<- c(asked, type)
      if (type != "nc") {
        cli::cli_abort("This input has no {.val {type}} file.")
      }
      fx$path
    },
    .package = "whep"
  )

  states <- whep:::.luh2_read_states(years = fx$year)
  testthat::expect_equal(asked, "nc")
  pointblank::expect_col_exists(
    states,
    c("lon", "lat", "year", "land_use", "fraction")
  )
  testthat::expect_true(all(states$year == fx$year))
})

test_that("a tidy tabular payload is still decoded", {
  # A future re-upload as a tidy parquet must need no reader change, so the
  # tabular decoder stays reachable when the pin has no NetCDF member.
  raw <- .luh2_raw_fixture()
  testthat::local_mocked_bindings(
    whep_read_file = function(file_alias, type = "parquet", ...) {
      if (type == "nc") {
        cli::cli_abort("This input has no {.val nc} file.")
      }
      raw
    },
    .package = "whep"
  )

  states <- whep:::.luh2_read_states(years = 1750L)
  testthat::expect_true(all(states$year == 1750L))
  testthat::expect_setequal(unique(states$land_use), unique(raw$land_use))
})

test_that("the LUH2 vintage read is recorded on the result", {
  # Issue #457: the base v2h release (850-2015) and the Global Carbon Budget
  # variants (850-2022) are different products, so a result must say which one
  # produced it instead of citing the base release by assumption.
  fx <- .luh2_fixture_states_nc()
  states <- whep:::.luh2_read_states_nc(
    fx$path,
    years = fx$year,
    origin = "pin"
  )
  prov <- whep::get_provenance(states)

  pointblank::expect_col_exists(
    prov,
    c("input_alias", "input_version", "input_origin", "input_source_id")
  )
  testthat::expect_equal(prov$input_alias, "luh2_v2h_states")
  testthat::expect_equal(prov$input_source_id, "UofMD-landState-LUH2-GCB2022")
  testthat::expect_equal(prov$input_origin, "pin")
  testthat::expect_equal(prov$input_first_year, 850L)
  testthat::expect_equal(prov$input_last_year, 850L)
  # the pinned version is only claimed when the pin was what was read
  registered <- whep::whep_inputs |>
    dplyr::filter(alias == "luh2_v2h_states") |>
    dplyr::pull(version)
  testthat::expect_equal(prov$input_version, registered)

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
      whep:::.luh2_read_states_nc(fx$path, years = years, origin = "pin")
    },
    .luh2_read_country_grid = function() {
      tibble::tibble(
        lon = -179.75,
        lat = 89.75,
        area_code = 1L,
        cell_area_frac = 1
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

test_that("real pin smoke test (skipped when unreadable)", {
  # The luh2_v2h_states pin is the ~6.6 GB LUH2 states product. On CI there is
  # no warm cache, so reading it downloads the whole file and hangs the check
  # (the tryCatch below only guards against an *error*, not a slow download).
  testthat::skip_on_ci()
  states <- tryCatch(
    whep:::.luh2_read_states(years = 1750L),
    error = function(e) NULL
  )
  testthat::skip_if(is.null(states), "luh2_v2h_states pin not readable")
  # the pinned payload is the Global Carbon Budget vintage, 850-2022
  prov <- whep::get_provenance(states)
  testthat::expect_equal(prov$input_origin, "pin")
  testthat::expect_true(!is.na(prov$input_source_id))
  cat("\nLUH2 pin vintage:", prov$input_source_id, "\n")
  cat(
    "\nLUH2 states found:",
    paste(sort(unique(states$land_use)), collapse = ", "),
    "\n"
  )
  print(utils::head(as.data.frame(states)))
  pointblank::expect_col_exists(
    states,
    c("lon", "lat", "year", "land_use", "fraction")
  )
})

# ---- Real local states.nc smoke test ---------------------------------------
.luh2_local_states_nc <- function() {
  file.path(
    Sys.getenv("WHEP_LUH2_DIR", ""),
    "states.nc"
  )
}

test_that("local states.nc reads at 0.5 deg with plausible year-2000 land use", {
  testthat::skip_if(
    !file.exists(.luh2_local_states_nc()),
    "local LUH2 states.nc not present"
  )
  # ask for the LOCAL states outright: the pin is now the primary source, so
  # calling read_luh2_landuse() bare would exercise the pin, not this file.
  out <- whep::read_luh2_landuse(
    resolution = "grid",
    years = 2000L,
    states_source = "local"
  )
  local_states <- whep:::.luh2_read_states_local(years = 2000L)

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

test_that("local states.nc is readable for 1750", {
  testthat::skip_if(
    !file.exists(.luh2_local_states_nc()),
    "local LUH2 states.nc not present"
  )
  out <- whep::read_luh2_landuse(
    resolution = "grid",
    years = 1750L,
    states_source = "local"
  )
  testthat::expect_true(all(out$year == 1750L))
  testthat::expect_true(nrow(out) > 0L)
  testthat::expect_equal(whep::get_provenance(out)$input_origin, "local")
})

test_that("the pin and a same-vintage local tree agree exactly", {
  # Issue #457: the pin can only replace the local workaround if it decodes to
  # the same grid. Only meaningful when WHEP_LUH2_DIR holds the pinned vintage.
  testthat::skip_on_ci()
  testthat::skip_if(
    !file.exists(.luh2_local_states_nc()),
    "local LUH2 states.nc not present"
  )
  pinned <- tryCatch(
    whep:::.luh2_read_states(years = 2000L),
    error = function(e) NULL
  )
  testthat::skip_if(is.null(pinned), "luh2_v2h_states pin not readable")
  local_states <- whep:::.luh2_read_states_local(years = 2000L)
  testthat::skip_if(
    whep::get_provenance(pinned)$input_source_id !=
      whep::get_provenance(local_states)$input_source_id,
    "WHEP_LUH2_DIR holds a different LUH2 vintage than the pin"
  )

  testthat::expect_equal(
    pinned |> whep::attach_provenance(NULL),
    local_states |> whep::attach_provenance(NULL)
  )
})
