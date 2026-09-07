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

.gn_excreta_fixture <- function(applied_c = 80) {
  # build_livestock_nutrient_flows()$applied shape. applied_c tonnes C on
  # grassland. Polity 1 total grazing excreta = 80 tonnes C by default.
  tibble::tribble(
    ~year, ~territory, ~sub_territory, ~land_use, ~crop, ~applied_c,
    2000L, "1", NA, "Grassland", NA, applied_c,
    2000L, "1", NA, "Cropland", "wheat", 999
  )
}

.gn_intake_fixture <- function(grass_dm_t = 100) {
  # redistribute_feed() shape. Only the "grass" rows are grazed off the
  # sward; the substitute the feed cascade adds when grass runs short comes
  # out of the non-grass supply and must not be charged to grassland.
  tibble::tribble(
    ~year, ~territory, ~livestock_category, ~feed_quality, ~intake_dm_t,
    2000L, "1", "Cattle_meat", "grass", grass_dm_t,
    2000L, "1", "Cattle_meat", "substitute", 900,
    2000L, "1", "Cattle_meat", "high_quality", 900
  )
}

.gn_fixture_data <- function(excreta = TRUE, intake = FALSE) {
  d <- list(
    npp = .gn_npp_fixture(),
    harvestc = .gn_harvest_fixture(),
    stand_frac = .gn_stand_frac_fixture(),
    country_grid = .gn_country_grid_fixture(),
    land_use = .gn_land_use_fixture(),
    residue_humification = whep::residue_humification
  )
  if (!isFALSE(excreta)) {
    d$excreta <- if (isTRUE(excreta)) {
      .gn_excreta_fixture()
    } else {
      .gn_excreta_fixture(applied_c = excreta)
    }
  }
  if (!isFALSE(intake)) {
    d$livestock_intake <- if (isTRUE(intake)) {
      .gn_intake_fixture()
    } else {
      intake
    }
  }
  d
}

# Production as the natural input, and LPJmL's own grazing, named on every
# call: these fixtures carry per-PFT NPP and no litterfall, and the
# arithmetic they check is that of NPP minus the run's own harvest. Both
# package defaults differ (litterfall, and WHEP's grazing), and are exercised
# in their own sections.
.gn_build_npp <- function(..., method_grazing = "lpjml") {
  whep::build_grass_natural_carbon_inputs(
    ...,
    method_natural_c = "npp",
    method_grazing = method_grazing
  )
}

testthat::test_that("grid output has the documented schema and classes", {
  out <- .gn_build_npp(
    resolution = "grid",
    data = .gn_fixture_data(excreta = FALSE)
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
  # The two classes are fed differently and say so: grassland is net of its
  # harvest, natural is not harvested at all, so subtracting nothing is named
  # as subtracting nothing.
  testthat::expect_equal(
    unique(out$method_c_input[out$land_use == "grassland"]),
    "lpjml_npp_minus_harvest"
  )
  testthat::expect_equal(
    unique(out$method_c_input[out$land_use == "natural"]),
    "lpjml_npp"
  )
})

testthat::test_that("natural C input sums the natural PFT bands", {
  out <- .gn_build_npp(
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

testthat::test_that("method 'woody' gives natural land the woody value", {
  # This was the unconditional behaviour before the humification fraction
  # was carbon-weighted across the natural PFTs. It is now what
  # method_natural_hf = "woody" selects, and it must still be reachable
  # exactly, so a user can reproduce a pre-change build.
  out <- .gn_build_npp(
    resolution = "grid",
    method_natural_hf = "woody",
    data = .gn_fixture_data(excreta = FALSE)
  )
  woody <- whep::residue_humification$humified_fraction[
    whep::residue_humification$input_type == "woody_residue"
  ]
  nat <- out[out$land_use == "natural", ]
  testthat::expect_true(all(nat$humified_fraction == woody))
})

testthat::test_that("the default carbon-weights natural humification", {
  d <- .gn_fixture_data(excreta = FALSE)
  seam <- whep:::.gn_net_c_from_lpjml(d, years = NULL)
  d$net_c <- seam
  out <- .gn_build_npp(resolution = "grid", data = d)
  nat <- out[out$land_use == "natural", ]
  coef <- \(x) {
    whep::residue_humification$humified_fraction[
      whep::residue_humification$input_type == x
    ]
  }

  # Bounded by the two tabulated coefficients...
  testthat::expect_true(all(nat$humified_fraction <= coef("woody_residue")))
  testthat::expect_true(all(nat$humified_fraction >= coef("weed")))
  # ...and STRICTLY inside them somewhere, which is the half that proves the
  # default actually carbon-weights. Both bounds above are satisfied by the
  # woody constant alone, so a wiring mistake that ignored
  # `method_natural_hf` and applied woody everywhere passed this test.
  testthat::expect_true(any(nat$humified_fraction < coef("woody_residue")))
  testthat::expect_true(any(nat$humified_fraction > coef("weed")))
})

testthat::test_that("grassland humified fraction carbon-weights npp and excreta", {
  weed <- whep::residue_humification$humified_fraction[
    whep::residue_humification$input_type == "weed"
  ]
  excreta_hf <- whep::residue_humification$humified_fraction[
    whep::residue_humification$input_type == "excreta"
  ]
  # With no excreta the blend reduces to the weed (grass-litter) value.
  gr0 <- .gn_build_npp(
    resolution = "grid",
    data = .gn_fixture_data(excreta = 0, intake = TRUE),
    method_grazing = "whep"
  )
  gr0 <- gr0[gr0$land_use == "grassland", ]
  testthat::expect_equal(gr0$humified_fraction, rep(weed, nrow(gr0)))
  # With grazing excreta added, each grassland cell's fraction is the
  # carbon-weighted blend of weed (litter) and the higher excreta coefficient,
  # so it sits strictly between the two and above the weed-only value.
  gr1 <- .gn_build_npp(
    resolution = "grid",
    data = .gn_fixture_data(excreta = TRUE, intake = TRUE),
    method_grazing = "whep"
  )
  gr1 <- gr1[gr1$land_use == "grassland" & gr1$c_input_mgc_ha_yr > 0, ]
  testthat::expect_true(all(gr1$humified_fraction >= weed - 1e-9))
  testthat::expect_true(all(gr1$humified_fraction <= excreta_hf + 1e-9))
  testthat::expect_true(any(gr1$humified_fraction > weed + 1e-9))
})

testthat::test_that("grassland net C is floored at zero", {
  out <- .gn_build_npp(
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
  out <- .gn_build_npp(
    resolution = "grid",
    data = data
  )
  cell_b <- out[out$lon == 0.75 & out$land_use == "natural", ]
  testthat::expect_equal(cell_b$c_input_mgc_ha_yr, 900 * 0.01)
})

testthat::test_that("grazing excreta adds a per-ha density to grassland", {
  with_ex <- .gn_build_npp(
    resolution = "grid",
    data = .gn_fixture_data(excreta = TRUE, intake = TRUE),
    method_grazing = "whep"
  )
  without_ex <- .gn_build_npp(
    resolution = "grid",
    data = .gn_fixture_data(excreta = 0, intake = TRUE),
    method_grazing = "whep"
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

testthat::test_that("grassland area is not polity-scaled twice", {
  country_grid <- tibble::tribble(
    ~lon, ~lat, ~area_code, ~cell_area_frac,
    0.25, 0.25, 1L, 0.25,
    0.25, 0.25, 2L, 0.75
  )
  # The grid-level LUH2 result has already split a physical 400-ha grassland
  # cell into these two polity-compartment areas.
  land_use <- tibble::tribble(
    ~lon, ~lat, ~area_code, ~year, ~land_use, ~area_ha,
    0.25, 0.25, 1L, 2000L, "grassland", 100,
    0.25, 0.25, 2L, 2000L, "grassland", 300
  )

  area <- whep:::.gn_grass_area(land_use, country_grid) |>
    dplyr::arrange(.data$area_code)

  testthat::expect_equal(area$grass_area_ha, c(100, 300))
  testthat::expect_equal(sum(area$grass_area_ha), 400)
})

testthat::test_that("ISO3 excreta territory resolves to area_code, not NA", {
  # An `applied` stream keyed by ISO3 (not a stringified area_code) must
  # resolve through the canonical territory helper; a bare as.integer() cast
  # would NA the ISO3 and silently drop all grazing-excreta carbon.
  esp_code <- whep::regions_full$code[
    whep::regions_full$iso3c == "ESP" & !is.na(whep::regions_full$iso3c)
  ][1]
  d <- .gn_fixture_data(excreta = FALSE, intake = TRUE)
  d$country_grid <- tibble::tribble(
    ~lon, ~lat, ~area_code, ~cell_area_frac,
    0.25, 0.25, esp_code, 1,
    0.75, 0.25, esp_code, 1
  )
  d$land_use <- tibble::tribble(
    ~lon, ~lat, ~area_code, ~year, ~land_use, ~area_ha,
    0.25, 0.25, esp_code, 2000L, "grassland", 100,
    0.75, 0.25, esp_code, 2000L, "grassland", 300
  )
  # The intake is keyed the same way, so the grazing removal lands on the
  # same polity and cancels in the difference below.
  d$livestock_intake$territory <- "ESP"
  d$excreta <- tibble::tribble(
    ~year, ~territory, ~sub_territory, ~land_use, ~crop, ~applied_c,
    2000L, "ESP", NA, "Grassland", NA, 80
  )
  # The ISO3 form is a deprecated bridge (#463), so resolving it warns; this
  # test is about it still resolving rather than dropping the excreta carbon.
  warnings <- testthat::capture_warnings(
    with_ex <- .gn_build_npp(
      resolution = "grid",
      data = d,
      method_grazing = "whep"
    )
  )
  testthat::expect_true(any(grepl("deprecated", warnings)))
  d0 <- d
  d0$excreta$applied_c <- 0
  without_ex <- suppressWarnings(.gn_build_npp(
    resolution = "grid",
    data = d0,
    method_grazing = "whep"
  ))
  gr_with <- with_ex[with_ex$land_use == "grassland", ]
  gr_with <- gr_with[order(gr_with$lon), ]
  gr_without <- without_ex[without_ex$land_use == "grassland", ]
  gr_without <- gr_without[order(gr_without$lon), ]
  # Total grassland area 400 ha; 80 tonnes C -> uniform 0.2 MgC/ha added.
  testthat::expect_equal(
    gr_with$c_input_mgc_ha_yr - gr_without$c_input_mgc_ha_yr,
    rep(0.2, nrow(gr_with))
  )
})

testthat::test_that("polity output aggregates area-weighted per class", {
  out <- .gn_build_npp(
    resolution = "polity",
    data = .gn_fixture_data(excreta = FALSE)
  )
  testthat::expect_false(rlang::has_name(out, "lon"))
  testthat::expect_true(
    all(c("area_code", "year", "land_use", "c_input_mgc_ha_yr") %in% names(out))
  )
  testthat::expect_setequal(out$land_use, c("grassland", "natural"))
  grass <- dplyr::filter(out, .data$land_use == "grassland")
  # Cell densities are 2.25 and 4.5 MgC/ha over 100 and 300 ha. The polity
  # density must conserve their carbon mass, not take the plain cell mean.
  expected <- (2.25 * 100 + 4.5 * 300) / 400
  testthat::expect_equal(grass$c_input_mgc_ha_yr, expected)
  testthat::expect_false(isTRUE(all.equal(
    grass$c_input_mgc_ha_yr,
    mean(c(2.25, 4.5))
  )))
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
    cell_area_frac = 1,
    cell_area_ha = whep:::.luh2_cell_area_ha(cells$lat),
    land_area_ha = whep:::.luh2_cell_area_ha(cells$lat)
  )
  land_use <- whep::read_luh2_landuse(
    "grid",
    years = year,
    data = list(country_grid = country_grid)
  )
  land_use <- land_use[land_use$land_use == "grassland", ]
  out <- .gn_build_npp(
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

# Default stand-fraction reader against the real cftfrac.nc. Skipped on CI and
# whenever the LPJmL run is absent (never fetches a remote pin/raster).
testthat::test_that(".gn_read_stand_frac reads cftfrac.nc grassland stands", {
  testthat::skip_on_ci()
  run_dir <- Sys.getenv("WHEP_LPJML_RUN_DIR")
  testthat::skip_if(
    !nzchar(run_dir) || !file.exists(file.path(run_dir, "cftfrac.nc")),
    "cftfrac.nc not available"
  )
  out <- whep:::.gn_read_stand_frac(run_dir = run_dir)
  testthat::expect_setequal(
    names(out),
    c("lon", "lat", "year", "name_pft", "stand_frac")
  )
  # Rainfed grassland must be present; irrigated grassland may legitimately be
  # ABSENT. LUH2 carries irrigation for the five crop types only (its
  # management.nc has irrig_c3ann/c4ann/c3per/c4per/c3nfx and no pasture
  # equivalent), so a LUH2-driven land-use input has an all-zero irrigated
  # grassland band and the reader, which keeps only positive stand fractions,
  # emits no such rows. Asserting both bands exist would fail on every
  # LUH2-driven run -- verified on the 1901-2023 run, where the input file's
  # band 30 is zero in every cell while band 14 covers 46,370.
  testthat::expect_true("rainfed grassland" %in% out$name_pft)
  testthat::expect_true(all(
    unique(out$name_pft) %in% c("rainfed grassland", "irrigated grassland")
  ))
  testthat::expect_true(all(out$stand_frac > 0 & out$stand_frac <= 1))
})

# ---- The pin seam ----------------------------------------------------------
# The pinned artifact exists so a user who has never run LPJmL still gets the
# real layer. What these guard is that the seam carries ONLY LPJmL-derived
# quantities: if the excreta or a humification fraction ever leaked into it,
# the pinned and run-derived paths would silently disagree.

testthat::test_that("net_c seam reproduces the per-PFT path exactly", {
  from_pfts <- .gn_build_npp(
    data = .gn_fixture_data(excreta = FALSE)
  )
  seam <- whep:::.gn_net_c_from_lpjml(.gn_fixture_data(), years = NULL)
  d <- .gn_fixture_data(excreta = FALSE)
  d$npp <- NULL
  d$harvestc <- NULL
  d$stand_frac <- NULL
  d$net_c <- seam
  from_seam <- .gn_build_npp(data = d)
  testthat::expect_equal(from_seam, from_pfts)
})

testthat::test_that("the net_c seam holds only LPJmL quantities", {
  seam <- whep:::.gn_net_c_from_lpjml(.gn_fixture_data(), years = NULL)
  # woody_share belongs here: it is the woody fraction of the run's own
  # per-PFT production, so it is LPJmL-derived exactly as the density is.
  # The humification COEFFICIENTS it is later blended with are not, and
  # they stay outside the seam - which is the property this guards.
  testthat::expect_setequal(
    names(seam),
    c(
      "lon",
      "lat",
      "year",
      "land_use",
      "npp_c_mgc_ha_yr",
      "net_c_mgc_ha_yr",
      "woody_share"
    )
  )
  # Both grazing methods must be servable from the seam: the whole grassland
  # production and what LPJmL's own grazing left of it are separate columns,
  # so neither method is baked in. Cell A: gross (0.3*400 + 0.1*600)/0.4 =
  # 450 gC/m2, net (0.3*300 + 0.1*0)/0.4 = 225.
  grass <- seam[seam$land_use == "grassland" & seam$lon == 0.25, ]
  testthat::expect_equal(grass$npp_c_mgc_ha_yr, 4.5)
  testthat::expect_equal(grass$net_c_mgc_ha_yr, 2.25)
  # Natural land is never harvested, so it carries no net column value.
  testthat::expect_true(all(is.na(
    seam$net_c_mgc_ha_yr[seam$land_use == "natural"]
  )))
  testthat::expect_setequal(unique(seam$land_use), c("grassland", "natural"))
  # Only natural land has a woody split; grassland is herbaceous by
  # definition and must not acquire one.
  nat <- seam[seam$land_use == "natural", ]
  testthat::expect_true(all(is.finite(nat$woody_share)))
  testthat::expect_true(all(nat$woody_share >= 0 & nat$woody_share <= 1))
})

testthat::test_that("excreta still changes the result through the seam", {
  # The excreta term must stay live on the pinned path. Same net_c, excreta on
  # vs off: grassland carbon and its humified fraction must both move.
  seam <- whep:::.gn_net_c_from_lpjml(.gn_fixture_data(), years = NULL)
  with_ex <- .gn_fixture_data(intake = TRUE)
  with_ex$net_c <- seam
  without <- .gn_fixture_data(excreta = 0, intake = TRUE)
  without$net_c <- seam
  a <- .gn_build_npp(data = with_ex, method_grazing = "whep") |>
    dplyr::filter(land_use == "grassland")
  b <- .gn_build_npp(data = without, method_grazing = "whep") |>
    dplyr::filter(land_use == "grassland")
  testthat::expect_gt(sum(a$c_input_mgc_ha_yr), sum(b$c_input_mgc_ha_yr))
  testthat::expect_false(isTRUE(all.equal(
    a$humified_fraction,
    b$humified_fraction
  )))
})

testthat::test_that("data$net_c takes precedence over run and pin", {
  # A supplied net_c must be used verbatim even with a run directory present,
  # so no network or NetCDF read happens on the injected path.
  withr::local_envvar(WHEP_LPJML_RUN_DIR = "/nonexistent/run")
  d <- .gn_fixture_data(excreta = FALSE)
  d$npp <- NULL
  d$harvestc <- NULL
  d$stand_frac <- NULL
  d$net_c <- tibble::tribble(
    ~lon, ~lat, ~year, ~land_use, ~npp_c_mgc_ha_yr, ~net_c_mgc_ha_yr,
    0.25, 0.25, 2000L, "grassland", 2, 2,
    0.25, 0.25, 2000L, "natural", 5, NA
  )
  out <- .gn_build_npp(data = d)
  testthat::expect_equal(
    dplyr::filter(out, land_use == "natural")$c_input_mgc_ha_yr,
    5
  )
  testthat::expect_equal(
    dplyr::filter(out, land_use == "grassland")$c_input_mgc_ha_yr,
    2
  )
})

testthat::test_that("a malformed net_c input is rejected by name", {
  d <- .gn_fixture_data()
  d$net_c <- tibble::tibble(lon = 0.25, lat = 0.25, year = 2000L)
  testthat::expect_error(
    .gn_build_npp(data = d),
    "land_use"
  )
})

# -- Natural PFT band guard (whep#400) ----------------------------------------

# The three bands LPJmL 6.1.1 added over 5.x, now summed. Real band names, read
# from the run's pft_npp.nc NamePFT variable.
.gn_npp_fixture_611 <- function() {
  dplyr::bind_rows(
    .gn_npp_fixture(),
    tibble::tribble(
      ~lon, ~lat, ~year, ~npft, ~name_pft, ~value,
      0.25, 0.25, 2000L, 2L,
      "tropical broadleaved evergreen tree floodtolerant", 700,
      0.25, 0.25, 2000L, 13L, "C3 graminoid flood tolerant", 200,
      0.25, 0.25, 2000L, 14L, "Sphagnum moss", 100
    )
  )
}

# A band no LPJmL version writes, standing in for the next version's addition.
.gn_npp_fixture_future <- function() {
  dplyr::bind_rows(
    .gn_npp_fixture(),
    tibble::tribble(
      ~lon, ~lat, ~year, ~npft, ~name_pft, ~value,
      0.25, 0.25, 2000L, 12L, "temperate liana", 250
    )
  )
}

testthat::test_that("the guard is silent on the 6.x natural PFT set", {
  testthat::expect_silent(
    whep:::.gn_check_natural_pfts(.gn_npp_fixture_611())
  )
})

testthat::test_that("the guard is silent on a subset of the list", {
  testthat::expect_silent(
    whep:::.gn_check_natural_pfts(.gn_npp_fixture())
  )
})

testthat::test_that("the guard warns on a band no version of the list has", {
  testthat::expect_warning(
    whep:::.gn_check_natural_pfts(.gn_npp_fixture_future()),
    "temperate liana"
  )
})

testthat::test_that("the guard counts and names every unlisted band", {
  w <- testthat::capture_warnings(
    whep:::.gn_check_natural_pfts(.gn_npp_fixture_future())
  )
  testthat::expect_match(w[1], "1 natural PFT band")
  testthat::expect_match(w[1], "temperate liana", fixed = TRUE)
})

testthat::test_that("natural bands stop at the first managed band", {
  testthat::expect_equal(
    whep:::.gn_natural_bands_present(.gn_npp_fixture()),
    c(
      "tropical broadleaved evergreen tree",
      "temperate needleleaved evergreen tree",
      "Temperate C3 grass"
    )
  )
})

testthat::test_that("an all-managed file reports no natural bands", {
  # pft_harvestc.nc carries only managed bands. The guard must stay silent
  # rather than treating the whole file as unlisted natural vegetation.
  managed_only <- .gn_npp_fixture() |>
    dplyr::filter(stringr::str_detect(.data$name_pft, "grassland"))
  testthat::expect_length(
    whep:::.gn_natural_bands_present(managed_only),
    0
  )
  testthat::expect_silent(whep:::.gn_check_natural_pfts(managed_only))
})

testthat::test_that("the three 6.x bands are summed into natural C input", {
  fixture <- .gn_fixture_data(excreta = FALSE)
  fixture$npp <- .gn_npp_fixture_611()
  out <- .gn_build_npp(
    resolution = "grid",
    data = fixture
  )
  # Cell A natural: tree 500 + Temp C3 grass 300 + floodtolerant tree 700 +
  # C3 graminoid 200 + Sphagnum 100 = 1800 gC/m2 = 18.0 MgC/ha. Before the
  # three were listed this was 8.0.
  cell_a <- out[out$lon == 0.25 & out$land_use == "natural", ]
  testthat::expect_equal(cell_a$c_input_mgc_ha_yr, 18.0)
})

testthat::test_that("all fourteen 6.x natural PFTs are listed", {
  testthat::expect_length(whep:::.gn_natural_pfts(), 14)
  purrr::walk(
    c(
      "tropical broadleaved evergreen tree floodtolerant",
      "C3 graminoid flood tolerant",
      "Sphagnum moss"
    ),
    \(nm) testthat::expect_true(nm %in% whep:::.gn_natural_pfts())
  )
})

# ---- natural humification is carbon-weighted, not a woody constant -----

testthat::test_that("the woody PFT list is a strict subset of the natural one", {
  woody <- whep:::.gn_woody_natural_pfts()
  natural <- whep:::.gn_natural_pfts()

  testthat::expect_length(woody, 9L)
  testthat::expect_length(natural, 14L)
  testthat::expect_true(all(woody %in% natural))
  # The five that are not woody. Naming them here means a PFT quietly
  # changing sides shows up as a test failure rather than as a shifted
  # humification fraction.
  testthat::expect_setequal(
    setdiff(natural, woody),
    c(
      "Tropical C4 grass",
      "Temperate C3 grass",
      "Polar C3 grass",
      "C3 graminoid flood tolerant",
      "Sphagnum moss"
    )
  )
})

testthat::test_that(".gn_woody_share weights by carbon, not by count", {
  # Three units of woody production against one of grass is 0.75 by carbon.
  # By PFT COUNT the same cell would be 0.5, which is the error this guards.
  testthat::expect_equal(
    whep:::.gn_woody_share(c(3, 1), c(TRUE, FALSE)),
    0.75
  )
  testthat::expect_equal(
    whep:::.gn_woody_share(c(1, 1, 1, 9), c(TRUE, TRUE, TRUE, FALSE)),
    0.25
  )
})

testthat::test_that("a stand producing nothing keeps the woody constant", {
  # Dividing by zero production would give NaN, and an NaN humification
  # fraction propagates into a non-finite equilibrium that
  # .cb_check_equilibrium() would then abort the whole build over.
  testthat::expect_equal(
    whep:::.gn_woody_share(c(0, 0), c(TRUE, FALSE)),
    1
  )
  testthat::expect_equal(
    whep:::.gn_natural_hf(
      tibble::tibble(
        woody_share = whep:::.gn_woody_share(c(0, 0), c(TRUE, FALSE))
      ),
      "woody_share",
      0.325,
      0.1153
    ),
    0.325
  )
})

testthat::test_that(".gn_natural_hf interpolates between the two coefficients", {
  rows <- tibble::tibble(woody_share = c(1, 0.5, 0))
  hf <- whep:::.gn_natural_hf(rows, "woody_share", 0.325, 0.1153)

  testthat::expect_equal(hf, c(0.325, 0.22015, 0.1153))
  # Never outside the two tabulated coefficients, whatever the share.
  wide <- tibble::tibble(woody_share = seq(0, 1, by = 0.05))
  all_hf <- whep:::.gn_natural_hf(wide, "woody_share", 0.325, 0.1153)
  testthat::expect_true(all(all_hf >= 0.1153 & all_hf <= 0.325))
})

testthat::test_that("method 'woody' reproduces the previous behaviour exactly", {
  rows <- tibble::tibble(woody_share = c(1, 0.5, 0))
  testthat::expect_equal(
    whep:::.gn_natural_hf(rows, "woody", 0.325, 0.1153),
    0.325
  )
})

testthat::test_that("a layer without woody_share falls back loudly", {
  # A pin built before woody_share existed is an ordinary state, not an
  # error - but it must not silently look like a carbon-weighted run.
  rows <- tibble::tibble(npp_c_mgc_ha_yr = 5)
  testthat::expect_warning(
    hf <- whep:::.gn_natural_hf(rows, "woody_share", 0.325, 0.1153),
    "woody_share"
  )
  testthat::expect_equal(hf, 0.325)
})

testthat::test_that("build_grass_natural_carbon_inputs validates the method", {
  testthat::expect_error(
    .gn_build_npp(method_natural_hf = "guess"),
    class = "rlang_error"
  )
})

testthat::test_that("the natural output drops woody_share after using it", {
  # woody_share is an input to the humification fraction, not part of the
  # carbon-input contract build_carbon_inputs() consumes.
  out <- whep::build_grass_natural_carbon_inputs(example = TRUE)
  testthat::expect_false("woody_share" %in% names(out))
})

# -- method_natural_c: production or litterfall -------------------------------

.gn_net_c_both <- function() {
  # Both quantities side by side, as a pin regenerated from a 2026-08-27 run
  # carries them. Natural litterfall sits below natural production because the
  # stand also grows, burns and is converted.
  tibble::tribble(
    ~lon, ~lat, ~year, ~land_use, ~npp_c_mgc_ha_yr, ~net_c_mgc_ha_yr,
    ~litterfall_c_mgc_ha_yr, ~woody_share,
    0.25, 0.25, 2000L, "grassland", 2, 2, NA, NA,
    0.25, 0.25, 2000L, "natural", 5, NA, 4, 0.72
  )
}

testthat::test_that("the default puts natural land on litterfall", {
  d <- .gn_fixture_data(excreta = FALSE)
  d$npp <- NULL
  d$harvestc <- NULL
  d$stand_frac <- NULL
  d$net_c <- .gn_net_c_both()
  out <- whep::build_grass_natural_carbon_inputs(
    data = d,
    method_grazing = "lpjml"
  ) |>
    dplyr::filter(land_use == "natural")
  testthat::expect_equal(out$c_input_mgc_ha_yr, 4)
  testthat::expect_identical(unique(out$method_c_input), "lpjml_litterfall")
  # The unused quantity must not survive into the output as a stray column.
  testthat::expect_false("litterfall_c_mgc_ha_yr" %in% names(out))
  testthat::expect_false("npp_c_mgc_ha_yr" %in% names(out))
  # Production stays selectable, and says so.
  npp <- whep::build_grass_natural_carbon_inputs(
    data = d,
    method_natural_c = "npp",
    method_grazing = "lpjml"
  ) |>
    dplyr::filter(land_use == "natural")
  testthat::expect_equal(npp$c_input_mgc_ha_yr, 5)
  testthat::expect_identical(unique(npp$method_c_input), "lpjml_npp")
})

testthat::test_that("method_natural_c = litterfall switches the input", {
  d <- .gn_fixture_data(excreta = FALSE)
  d$npp <- NULL
  d$harvestc <- NULL
  d$stand_frac <- NULL
  d$net_c <- .gn_net_c_both()
  out <- whep::build_grass_natural_carbon_inputs(
    data = d,
    method_natural_c = "litterfall",
    method_grazing = "lpjml"
  ) |>
    dplyr::filter(land_use == "natural")
  testthat::expect_equal(out$c_input_mgc_ha_yr, 4)
  testthat::expect_identical(unique(out$method_c_input), "lpjml_litterfall")

  # Grassland is untouched: litfallc_nv covers the natural stand only.
  grass <- whep::build_grass_natural_carbon_inputs(
    data = d,
    method_natural_c = "litterfall",
    method_grazing = "lpjml"
  ) |>
    dplyr::filter(land_use == "grassland")
  base <- whep::build_grass_natural_carbon_inputs(
    data = d,
    method_grazing = "lpjml"
  ) |>
    dplyr::filter(land_use == "grassland")
  testthat::expect_equal(grass$c_input_mgc_ha_yr, base$c_input_mgc_ha_yr)
})

testthat::test_that("litterfall on a layer without it aborts, never falls back", {
  # A silent fall back to production would substitute one method for another
  # and move natural equilibrium carbon by ~0.92x with nothing recording it.
  d <- .gn_fixture_data()
  d$npp <- NULL
  d$harvestc <- NULL
  d$stand_frac <- NULL
  d$net_c <- tibble::tribble(
    ~lon, ~lat, ~year, ~land_use, ~npp_c_mgc_ha_yr,
    0.25, 0.25, 2000L, "natural", 5
  )
  testthat::expect_error(
    whep::build_grass_natural_carbon_inputs(
      data = d,
      method_natural_c = "litterfall"
    ),
    "litterfall_c_mgc_ha_yr"
  )
})

testthat::test_that("an unknown method is refused", {
  testthat::expect_error(
    whep::build_grass_natural_carbon_inputs(method_natural_c = "residues"),
    class = "rlang_error"
  )
})

testthat::test_that("litterfall is converted from per-cell to per-stand", {
  # litfallc_nv is a whole-cell density; pft_npp is per-stand. Half-natural
  # cell shedding 2 MgC/ha of cell area experiences 4 on its own stand.
  natural <- tibble::tribble(
    ~lon, ~lat, ~year, ~land_use, ~npp_c_mgc_ha_yr,
    0.25, 0.25, 2000L, "natural", 9
  )
  out <- whep:::.gn_attach_litterfall(
    natural,
    list(
      litterfall_nv = tibble::tribble(
        ~lon, ~lat, ~year, ~litterfall_c_mgc_ha_yr,
        0.25, 0.25, 2000L, 2
      ),
      natural_cover = tibble::tribble(
        ~lon, ~lat, ~year, ~natural_stand_frac, ~natural_cover,
        0.25, 0.25, 2000L, 0.5, 0.9
      )
    ),
    years = 2000L,
    run_dir = NULL
  )
  testthat::expect_equal(out$litterfall_c_mgc_ha_yr, 4)
  # The production column and the row itself survive the join.
  testthat::expect_equal(out$npp_c_mgc_ha_yr, 9)
  testthat::expect_identical(nrow(out), 1L)
})

testthat::test_that("a cell with no natural stand gets no per-stand value", {
  # Dividing by zero would manufacture an infinite carbon input.
  natural <- tibble::tribble(
    ~lon, ~lat, ~year, ~land_use, ~npp_c_mgc_ha_yr,
    0.25, 0.25, 2000L, "natural", 9
  )
  out <- whep:::.gn_attach_litterfall(
    natural,
    list(
      litterfall_nv = tibble::tribble(
        ~lon, ~lat, ~year, ~litterfall_c_mgc_ha_yr,
        0.25, 0.25, 2000L, 0
      ),
      natural_cover = tibble::tribble(
        ~lon, ~lat, ~year, ~natural_stand_frac, ~natural_cover,
        0.25, 0.25, 2000L, 0, 0
      )
    ),
    years = 2000L,
    run_dir = NULL
  )
  testthat::expect_identical(nrow(out), 1L)
  testthat::expect_true(is.na(out$litterfall_c_mgc_ha_yr))
})

testthat::test_that("litterfall NA on a cell with no production is zero litter", {
  # The pin masks litter where no natural PFT grows, so NA sits exactly on
  # the zero-NPP cells (612 of 58,795 natural rows at 2000). Those become 0;
  # an NA on a producing cell stays NA rather than being absorbed.
  d <- .gn_fixture_data()
  d$npp <- NULL
  d$harvestc <- NULL
  d$stand_frac <- NULL
  d$net_c <- tibble::tribble(
    ~lon, ~lat, ~year, ~land_use, ~npp_c_mgc_ha_yr, ~litterfall_c_mgc_ha_yr,
    ~woody_share,
    0.25, 0.25, 2000L, "natural", 0, NA, 0.5,
    0.75, 0.25, 2000L, "natural", 5, NA, 0.5,
    0.25, 0.75, 2000L, "natural", 5, 4, 0.5
  )
  d$country_grid <- tibble::tibble(
    lon = c(0.25, 0.75, 0.25),
    lat = c(0.25, 0.25, 0.75),
    area_code = 1L,
    cell_area_frac = 1
  )
  out <- whep::build_grass_natural_carbon_inputs(data = d) |>
    dplyr::filter(land_use == "natural") |>
    dplyr::arrange(lat, lon)
  testthat::expect_equal(out$c_input_mgc_ha_yr, c(0, NA, 4))
  # Production is untouched by the rule.
  npp <- whep::build_grass_natural_carbon_inputs(
    data = d,
    method_natural_c = "npp"
  ) |>
    dplyr::filter(land_use == "natural") |>
    dplyr::arrange(lat, lon)
  testthat::expect_equal(npp$c_input_mgc_ha_yr, c(0, 5, 5))
})

# ---- method_grazing: whose herd grazes the sward ----------------------------
# LPJmL 6.1 runs its own livestock module on managed grassland, so the layer
# arrives already grazed: pft_harvestc is the uptake NET of the feces and
# urine returned to the stand. Adding WHEP's excreta to that, which the
# excreta argument used to do unconditionally, books the same return twice.
# The default now charges the class WHEP's own removal and return instead,
# starting from the whole production so nothing of LPJmL's grazing survives.
#
# Fixture arithmetic, polity 1 over 400 ha of grassland:
#   grazed  100 t DM * 0.45 = 45 MgC   -> 0.1125 MgC/ha
#   excreta                   80 MgC   -> 0.2 MgC/ha
#   cell A whole production (0.3*400 + 0.1*600)/0.4 = 450 gC/m2 = 4.5 MgC/ha
#   cell A net of LPJmL's grazing (0.3*300 + 0.1*0)/0.4 = 225      = 2.25
#   cell B whole 500 gC/m2 = 5.0, net (500-50) = 4.5

.gn_grass_rows <- function(...) {
  .gn_build_npp(resolution = "grid", ...) |>
    dplyr::filter(land_use == "grassland") |>
    dplyr::arrange(lon)
}

testthat::test_that("the whep method charges WHEP's removal and return", {
  out <- .gn_grass_rows(
    data = .gn_fixture_data(intake = TRUE),
    method_grazing = "whep"
  )
  testthat::expect_equal(
    out$c_input_mgc_ha_yr,
    c(4.5 - 0.1125 + 0.2, 5 - 0.1125 + 0.2)
  )
  testthat::expect_identical(
    unique(out$method_c_input),
    "lpjml_npp_minus_whep_grazing"
  )
})

testthat::test_that("the lpjml method keeps the model's own grazing", {
  out <- .gn_grass_rows(data = .gn_fixture_data(excreta = FALSE))
  testthat::expect_equal(out$c_input_mgc_ha_yr, c(2.25, 4.5))
  testthat::expect_identical(
    unique(out$method_c_input),
    "lpjml_npp_minus_harvest"
  )
})

testthat::test_that("the methods differ by exactly the two herds", {
  lpjml <- .gn_grass_rows(data = .gn_fixture_data(excreta = FALSE))
  whep_m <- .gn_grass_rows(
    data = .gn_fixture_data(intake = TRUE),
    method_grazing = "whep"
  )
  # LPJmL's grazing (harvest) is added back and WHEP's is taken off, so the
  # difference is the model's removal minus WHEP's, plus WHEP's return.
  lpjml_removed <- c(4.5 - 2.25, 5 - 4.5)
  testthat::expect_equal(
    whep_m$c_input_mgc_ha_yr - lpjml$c_input_mgc_ha_yr,
    lpjml_removed - 0.1125 + 0.2
  )
})

testthat::test_that("only the grass rows are grazed off the sward", {
  # The feed cascade's grass-deficit substitute comes out of the non-grass
  # supply; charging it to grassland would remove carbon nothing ate there.
  base <- .gn_grass_rows(
    data = .gn_fixture_data(intake = TRUE),
    method_grazing = "whep"
  )
  more_substitute <- .gn_intake_fixture()
  more_substitute$intake_dm_t[more_substitute$feed_quality != "grass"] <- 1e6
  moved <- .gn_grass_rows(
    data = .gn_fixture_data(intake = more_substitute),
    method_grazing = "whep"
  )
  testthat::expect_equal(moved$c_input_mgc_ha_yr, base$c_input_mgc_ha_yr)
})

testthat::test_that("the lpjml method refuses WHEP's excreta, loudly", {
  # The latent double count: this used to add the excreta silently on top of
  # a layer that already carried LPJmL's own return.
  testthat::expect_warning(
    with_ex <- .gn_grass_rows(data = .gn_fixture_data(excreta = TRUE)),
    "Ignoring"
  )
  without <- .gn_grass_rows(data = .gn_fixture_data(excreta = FALSE))
  testthat::expect_equal(with_ex$c_input_mgc_ha_yr, without$c_input_mgc_ha_yr)
})

testthat::test_that("the whep method aborts without its own inputs", {
  # Reading absent inputs as zero would silently turn grazed grassland into
  # ungrazed grassland, an input even LPJmL's harvest was subtracted from.
  testthat::expect_error(
    .gn_grass_rows(
      data = .gn_fixture_data(excreta = TRUE),
      method_grazing = "whep"
    ),
    "livestock_intake"
  )
  testthat::expect_error(
    .gn_grass_rows(
      data = .gn_fixture_data(excreta = FALSE, intake = TRUE),
      method_grazing = "whep"
    ),
    "excreta"
  )
})

testthat::test_that("a year the intake does not cover aborts", {
  # A gap in WHEP's feed chain and a year nothing grazed are the same number
  # once the missing rows become a zero removal.
  short <- .gn_intake_fixture()
  short$year <- 1999L
  testthat::expect_error(
    .gn_grass_rows(
      data = .gn_fixture_data(intake = short),
      method_grazing = "whep"
    ),
    "2000"
  )
})

testthat::test_that("grazing beyond production is floored and reported", {
  # The removal is a polity total spread uniformly, so a cell below the
  # polity mean can be asked for more carbon than it grew.
  hungry <- .gn_intake_fixture(grass_dm_t = 1e5)
  testthat::expect_warning(
    out <- .gn_grass_rows(
      data = .gn_fixture_data(intake = hungry),
      method_grazing = "whep"
    ),
    "exceeds grassland production"
  )
  # Nothing but the excreta is left on either cell, and neither goes negative.
  testthat::expect_equal(out$c_input_mgc_ha_yr, c(0.2, 0.2))
})

testthat::test_that("a grassland layer without the net column is refused", {
  # Before the split, `npp_c_mgc_ha_yr` on a grassland row meant production
  # already net of LPJmL's grazing. Read as the whole production it would
  # double-subtract the grazing; the two cannot be told apart, so the layer
  # is refused under either method.
  d <- .gn_fixture_data(excreta = FALSE)
  d$npp <- NULL
  d$harvestc <- NULL
  d$stand_frac <- NULL
  d$net_c <- tibble::tribble(
    ~lon, ~lat, ~year, ~land_use, ~npp_c_mgc_ha_yr,
    0.25, 0.25, 2000L, "grassland", 2
  )
  testthat::expect_error(.gn_build_npp(data = d), "net_c_mgc_ha_yr")
  testthat::expect_error(
    .gn_build_npp(data = d, method_grazing = "whep"),
    "net_c_mgc_ha_yr"
  )
})

testthat::test_that("floored grazing carbon is reported as mass, weighted by area", {
  # The warning used to report an area-unweighted ratio of per-hectare
  # densities, so a 1 ha cell short by 4 MgC/ha and a 10,000 ha cell short by
  # 0.01 MgC/ha contributed equally. The quantity a reader needs is the mass
  # that did not get removed.
  rows <- tibble::tibble(
    lon = c(0.25, 0.75),
    lat = 0.25,
    year = 2020L,
    npp_c_mgc_ha_yr = c(1, 5),
    grazed_c = c(5, 5)
  )
  land_use <- tibble::tibble(
    lon = c(0.25, 0.75),
    lat = 0.25,
    year = 2020L,
    land_use = "grassland",
    area_ha = c(1, 10000)
  )
  # Only the first cell is short, by 4 MgC/ha over 1 ha = 4 Mg C.
  testthat::expect_warning(
    whep:::.gn_warn_grazing_over_production(rows, land_use),
    "Tg C unremoved"
  )
  testthat::expect_warning(
    whep:::.gn_warn_grazing_over_production(rows, land_use),
    "Mha"
  )
  # Without a land-use layer it must SAY it is not a mass rather than pretend.
  testthat::expect_warning(
    whep:::.gn_warn_grazing_over_production(rows),
    "not a mass"
  )
})

testthat::test_that("a tiny cell cannot dominate the floored-carbon report", {
  # The regression for the weighting itself: make the small cell hugely short
  # and the large cell barely so. Unweighted the small one dominates; weighted
  # it is negligible, which is the truth.
  rows <- tibble::tibble(
    lon = c(0.25, 0.75),
    lat = 0.25,
    year = 2020L,
    npp_c_mgc_ha_yr = c(0.001, 4.99),
    grazed_c = c(100, 5)
  )
  land_use <- tibble::tibble(
    lon = c(0.25, 0.75),
    lat = 0.25,
    year = 2020L,
    land_use = "grassland",
    area_ha = c(1, 1e6)
  )
  w <- testthat::capture_warnings(
    whep:::.gn_warn_grazing_over_production(rows, land_use)
  )
  # Unremoved mass = 100 Mg (tiny cell) + 0.01 * 1e6 = 10,100 Mg, against
  # asked = 100 + 5e6. So the share is ~0.2%, not the ~95% an unweighted
  # density ratio would report.
  testthat::expect_true(any(grepl("0.2%", w, fixed = TRUE)))
})
