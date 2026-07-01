# Hand-built fixtures keep the NPP-minus-harvest arithmetic checkable by
# inspection. Values are per-PFT-stand gC/m2/yr as read_lpjml_npp() returns.

.gn_npp_fixture <- function() {
  # Two cells, one year. Natural PFTs (tree + grass) coexist in the natural
  # stand; the two managed grasslands are separate stands. Band indices match
  # pft_npp.nc's 43-band order.
  tibble::tribble(
    ~lon, ~lat, ~year, ~npft, ~name_pft, ~value,
    0.25, 0.25, 2000L, 3L, "temperate needleleaved evergreen tree", 500,
    0.25, 0.25, 2000L, 10L, "Temperate C3 grass", 300,
    0.25, 0.25, 2000L, 25L, "rainfed grassland", 400,
    0.25, 0.25, 2000L, 41L, "irrigated grassland", 600,
    0.75, 0.25, 2000L, 1L, "tropical broadleaved evergreen tree", 900,
    0.75, 0.25, 2000L, 25L, "rainfed grassland", 500
  )
}

.gn_harvest_fixture <- function() {
  # harvestc.nc band order (32 bands). Only grasslands carry harvest here.
  tibble::tribble(
    ~lon, ~lat, ~year, ~npft, ~name_pft, ~value,
    0.25, 0.25, 2000L, 14L, "rainfed grassland", 100,
    0.25, 0.25, 2000L, 30L, "irrigated grassland", 700,
    0.75, 0.25, 2000L, 14L, "rainfed grassland", 50
  )
}

.gn_stand_frac_fixture <- function() {
  # Per-cell stand fractions for the two managed grasslands (cftfrac). Cell A
  # has both rainfed (0.3) and irrigated (0.1); cell B only rainfed (0.4).
  tibble::tribble(
    ~lon, ~lat, ~year, ~name_pft, ~stand_frac,
    0.25, 0.25, 2000L, "rainfed grassland", 0.3,
    0.25, 0.25, 2000L, "irrigated grassland", 0.1,
    0.75, 0.25, 2000L, "rainfed grassland", 0.4
  )
}

.gn_country_grid_fixture <- function() {
  tibble::tribble(
    ~lon, ~lat, ~area_code, ~cell_area_frac,
    0.25, 0.25, 1L, 1,
    0.75, 0.25, 1L, 1
  )
}

.gn_land_use_fixture <- function() {
  # Grassland area per cell (ha), used to spread polity excreta to a per-ha
  # density. Cell A 100 ha grassland, cell B 300 ha grassland.
  tibble::tribble(
    ~lon, ~lat, ~area_code, ~year, ~land_use, ~area_ha,
    0.25, 0.25, 1L, 2000L, "grassland", 100,
    0.75, 0.25, 1L, 2000L, "grassland", 300
  )
}

.gn_excreta_fixture <- function() {
  # build_livestock_nutrient_flows()$applied shape. applied_c tonnes C on
  # grassland. Polity 1 total grazing excreta = 80 tonnes C.
  tibble::tribble(
    ~year, ~territory, ~sub_territory, ~land_use, ~crop, ~applied_c,
    2000L, "1", NA, "Grassland", NA, 80,
    2000L, "1", NA, "Cropland", "wheat", 999
  )
}

.gn_fixture_data <- function(excreta = TRUE) {
  d <- list(
    npp = .gn_npp_fixture(),
    harvestc = .gn_harvest_fixture(),
    stand_frac = .gn_stand_frac_fixture(),
    country_grid = .gn_country_grid_fixture(),
    land_use = .gn_land_use_fixture(),
    residue_humification = whep::residue_humification
  )
  if (excreta) {
    d$excreta <- .gn_excreta_fixture()
  }
  d
}

testthat::test_that("grid output has the documented schema and classes", {
  out <- whep::build_grass_natural_carbon_inputs(
    resolution = "grid",
    data = .gn_fixture_data()
  )
  expected <- c(
    "lon",
    "lat",
    "area_code",
    "year",
    "land_use",
    "c_input_mgc_ha_yr",
    "humified_fraction",
    "method_c_input"
  )
  testthat::expect_true(all(expected %in% names(out)))
  testthat::expect_setequal(out$land_use, c("grassland", "natural"))
  testthat::expect_true(
    all(out$method_c_input == "lpjml_npp_minus_harvest")
  )
})

testthat::test_that("natural C input sums the natural PFT bands (1-11)", {
  out <- whep::build_grass_natural_carbon_inputs(
    resolution = "grid",
    data = .gn_fixture_data(excreta = FALSE)
  )
  # Cell A natural: tree 500 + Temp C3 grass 300 = 800 gC/m2 = 8.0 MgC/ha.
  cell_a <- out[
    out$lon == 0.25 & out$land_use == "natural",
  ]
  testthat::expect_equal(cell_a$c_input_mgc_ha_yr, 8.0)
  # Cell B natural: tropical tree 900 only = 9.0 MgC/ha.
  cell_b <- out[
    out$lon == 0.75 & out$land_use == "natural",
  ]
  testthat::expect_equal(cell_b$c_input_mgc_ha_yr, 9.0)
})

testthat::test_that("natural humified fraction is the woody value", {
  out <- whep::build_grass_natural_carbon_inputs(
    resolution = "grid",
    data = .gn_fixture_data(excreta = FALSE)
  )
  woody <- whep::residue_humification$humified_fraction[
    whep::residue_humification$input_type == "woody_residue"
  ]
  nat <- out[out$land_use == "natural", ]
  testthat::expect_true(all(nat$humified_fraction == woody))
})

testthat::test_that("grassland humified fraction is the weed value", {
  out <- whep::build_grass_natural_carbon_inputs(
    resolution = "grid",
    data = .gn_fixture_data(excreta = FALSE)
  )
  weed <- whep::residue_humification$humified_fraction[
    whep::residue_humification$input_type == "weed"
  ]
  gr <- out[out$land_use == "grassland", ]
  testthat::expect_true(all(gr$humified_fraction == weed))
})

testthat::test_that("grassland net C is floored at zero", {
  out <- whep::build_grass_natural_carbon_inputs(
    resolution = "grid",
    data = .gn_fixture_data(excreta = FALSE)
  )
  # Cell A irrigated grassland: npp 600 - harvest 700 = -100 -> floored 0.
  # rainfed: 400 - 100 = 300 gC/m2. Area-weighted over stands (rf 0.3, irr 0.1):
  # (0.3*300 + 0.1*0) / 0.4 = 225 gC/m2 = 2.25 MgC/ha.
  cell_a <- out[out$lon == 0.25 & out$land_use == "grassland", ]
  testthat::expect_equal(cell_a$c_input_mgc_ha_yr, 2.25)
  testthat::expect_gte(cell_a$c_input_mgc_ha_yr, 0)
})

testthat::test_that("unit conversion is 100 gC/m2 = 1 MgC/ha", {
  data <- .gn_fixture_data(excreta = FALSE)
  # Cell B natural = 900 gC/m2 -> 9.0 MgC/ha.
  out <- whep::build_grass_natural_carbon_inputs(
    resolution = "grid",
    data = data
  )
  cell_b <- out[out$lon == 0.75 & out$land_use == "natural", ]
  testthat::expect_equal(cell_b$c_input_mgc_ha_yr, 900 * 0.01)
})

testthat::test_that("grazing excreta adds a per-ha density to grassland", {
  with_ex <- whep::build_grass_natural_carbon_inputs(
    resolution = "grid",
    data = .gn_fixture_data(excreta = TRUE)
  )
  without_ex <- whep::build_grass_natural_carbon_inputs(
    resolution = "grid",
    data = .gn_fixture_data(excreta = FALSE)
  )
  # Polity 1 total grassland area = 100 + 300 = 400 ha; excreta 80 tonnes C
  # -> uniform 0.2 MgC/ha added to every grassland cell in polity 1.
  gr_with <- with_ex[with_ex$land_use == "grassland", ]
  gr_without <- without_ex[without_ex$land_use == "grassland", ]
  gr_with <- gr_with[order(gr_with$lon), ]
  gr_without <- gr_without[order(gr_without$lon), ]
  testthat::expect_equal(
    gr_with$c_input_mgc_ha_yr - gr_without$c_input_mgc_ha_yr,
    rep(0.2, nrow(gr_with))
  )
  # Natural rows are unchanged by excreta.
  nat_with <- with_ex[with_ex$land_use == "natural", ]
  nat_without <- without_ex[without_ex$land_use == "natural", ]
  testthat::expect_equal(
    sort(nat_with$c_input_mgc_ha_yr),
    sort(nat_without$c_input_mgc_ha_yr)
  )
})

testthat::test_that("polity output aggregates area-weighted per class", {
  out <- whep::build_grass_natural_carbon_inputs(
    resolution = "polity",
    data = .gn_fixture_data(excreta = FALSE)
  )
  testthat::expect_false(rlang::has_name(out, "lon"))
  testthat::expect_true(
    all(c("area_code", "year", "land_use", "c_input_mgc_ha_yr") %in% names(out))
  )
  testthat::expect_setequal(out$land_use, c("grassland", "natural"))
})

testthat::test_that("example = TRUE returns the documented schema", {
  out <- whep::build_grass_natural_carbon_inputs(example = TRUE)
  expected <- c(
    "lon",
    "lat",
    "area_code",
    "year",
    "land_use",
    "c_input_mgc_ha_yr",
    "humified_fraction",
    "method_c_input"
  )
  testthat::expect_true(all(expected %in% names(out)))
  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_gt(nrow(out), 0L)
})

# Real-data smoke test: read the actual LPJmL run and assert plausible
# grassland and natural carbon-input magnitudes. Skipped when the files are
# absent (CI, machines without the run). Verified 2026-07-01 (year 2000):
# grassland productive-cell median 4.53 MgC/ha/yr (IQR 3.49-7.94), natural
# vegetated-cell median 5.14 (IQR 3.05-7.67, p95 11.5).
.gn_real_run_dir <- function() {
  Sys.getenv("WHEP_LPJML_RUN_DIR")
}

.gn_read_cftfrac_grassland <- function(run_dir, year) {
  path <- file.path(run_dir, "cftfrac.nc")
  testthat::skip_if_not(file.exists(path), "cftfrac.nc absent")
  time_index <- year - 1901L + 1L
  nc <- ncdf4::nc_open(path)
  on.exit(ncdf4::nc_close(nc))
  lon <- ncdf4::ncvar_get(nc, "lon")
  lat <- ncdf4::ncvar_get(nc, "lat")
  names_pft <- as.character(ncdf4::ncvar_get(nc, "NamePFT"))
  bands <- which(names_pft %in% c("rainfed grassland", "irrigated grassland"))
  parts <- purrr::map(bands, function(k) {
    slab <- ncdf4::ncvar_get(
      nc,
      "CFTfrac",
      start = c(1L, 1L, k, time_index),
      count = c(-1L, -1L, 1L, 1L)
    )
    dt <- data.table::data.table(
      lon = rep(lon, times = length(lat)),
      lat = rep(lat, each = length(lon)),
      year = year,
      name_pft = names_pft[k],
      stand_frac = as.vector(slab)
    )
    dt[is.finite(stand_frac) & stand_frac > 0]
  })
  tibble::as_tibble(data.table::rbindlist(parts))
}

testthat::test_that("real LPJmL run gives plausible C-input magnitudes", {
  run_dir <- .gn_real_run_dir()
  testthat::skip_if(
    !nzchar(run_dir) || !dir.exists(run_dir),
    "WHEP_LPJML_RUN_DIR not set or absent"
  )
  testthat::skip_if_not(
    file.exists(file.path(run_dir, "pft_npp.nc")),
    "pft_npp.nc absent"
  )
  year <- 2000L
  npp <- whep::read_lpjml_npp("npp", years = year, run_dir = run_dir)
  harvestc <- whep::read_lpjml_npp("harvestc", years = year, run_dir = run_dir)
  stand_frac <- .gn_read_cftfrac_grassland(run_dir, year)
  cells <- unique(rbind(
    npp[, c("lon", "lat")],
    stand_frac[, c("lon", "lat")]
  ))
  country_grid <- tibble::tibble(
    lon = cells$lon,
    lat = cells$lat,
    area_code = 1L,
    cell_area_frac = 1
  )
  land_use <- whep::read_luh2_landuse(
    "grid",
    years = year,
    data = list(country_grid = country_grid)
  )
  land_use <- land_use[land_use$land_use == "grassland", ]
  out <- whep::build_grass_natural_carbon_inputs(
    resolution = "grid",
    data = list(
      npp = npp,
      harvestc = harvestc,
      stand_frac = stand_frac,
      country_grid = country_grid,
      land_use = land_use,
      excreta = NULL
    )
  )
  # Grassland on productive cells (rainfed grassland NPP > 200 gC/m2): the
  # excreta-free residue input should sit in ~2-8 MgC/ha/yr.
  productive <- npp[
    npp$name_pft == "rainfed grassland" & npp$value > 200,
    c("lon", "lat")
  ]
  grass <- merge(
    out[out$land_use == "grassland", ],
    productive,
    by = c("lon", "lat")
  )
  grass_median <- stats::median(grass$c_input_mgc_ha_yr)
  testthat::expect_gt(grass_median, 2)
  testthat::expect_lt(grass_median, 8)
  # Natural on vegetated cells (input > 1 MgC/ha/yr): ~5-15 MgC/ha/yr.
  nat <- out[out$land_use == "natural" & out$c_input_mgc_ha_yr > 1, ]
  nat_median <- stats::median(nat$c_input_mgc_ha_yr)
  testthat::expect_gt(nat_median, 4)
  testthat::expect_lt(nat_median, 15)
})
