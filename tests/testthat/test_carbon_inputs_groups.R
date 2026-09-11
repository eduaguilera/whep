# Cropland resolved into crop groups (Spain_Hist convention) inside
# build_carbon_inputs(). Offline: every input is injected.

.cig_cropland <- function() {
  # Two crops in one cell-year: wheat (herbaceous, 15) and olives (woody,
  # 260), with distinct densities so weighting is visible.
  tibble::tribble(
    ~lon, ~lat, ~area_code, ~item_prod_code, ~year,
    ~total_c_input_mgc_ha_yr, ~humified_fraction, ~method_c_input,
    ~crop_area_ha,
    0.25, 0.25, 1L, "15", 2000L, 2.0, 0.20, "x", 100,
    0.25, 0.25, 1L, "260", 2000L, 4.0, 0.30, "x", 50
  )
}

.cig_area <- function() {
  tibble::tribble(
    ~lon, ~lat, ~area_code, ~item_prod_code, ~year, ~crop_area_ha,
    0.25, 0.25, 1L, "15", 2000L, 100,
    0.25, 0.25, 1L, "260", 2000L, 50
  )
}

.cig_share <- function(wheat = 0.5, olive = 0) {
  tibble::tribble(
    ~lon, ~lat, ~area_code, ~item_prod_code, ~year, ~irrigated_share,
    0.25, 0.25, 1L, 15L, 2000L, wheat,
    0.25, 0.25, 1L, 260L, 2000L, olive
  )
}

.cig_grass <- function() {
  tibble::tribble(
    ~lon, ~lat, ~area_code, ~year, ~land_use,
    ~c_input_mgc_ha_yr, ~humified_fraction, ~method_c_input,
    0.25, 0.25, 1L, 2000L, "grassland", 1.0, 0.1, "g",
    0.25, 0.25, 1L, 2000L, "natural", 1.5, 0.3, "n"
  )
}

.cig_data <- function(share = NULL) {
  d <- list(
    cropland = .cig_cropland(),
    crop_area = .cig_area(),
    grass_natural = .cig_grass()
  )
  if (!is.null(share)) {
    d$crop_regime_share <- share
  }
  d
}

testthat::test_that("the config validates element-wise and defaults to groups", {
  cfg <- whep:::.ci_group_config()
  testthat::expect_identical(cfg$method, "spain_hist")
  testthat::expect_identical(cfg$irrigation, "spatialized")
  # "none" stays reachable and is what the pre-group behaviour is called.
  testthat::expect_identical(
    whep:::.ci_group_config(list(method = "none"))$method,
    "none"
  )
  testthat::expect_error(
    whep:::.ci_group_config(list(method = "spain")),
    "crop_groups"
  )
  testthat::expect_error(
    whep:::.ci_group_config(list(mehtod = "none")),
    "Unknown"
  )
})

testthat::test_that("method none is the single cropland class, unchanged", {
  before <- whep::build_carbon_inputs(
    data = .cig_data(),
    crop_groups = list(method = "none")
  )
  after <- whep::build_carbon_inputs(
    data = .cig_data(.cig_share()),
    crop_groups = list(method = "none")
  )
  testthat::expect_identical(before, after)
  crop <- before[before$land_use == "cropland", ]
  testthat::expect_identical(nrow(crop), 1L)
  # 100 ha at 2.0 plus 50 ha at 4.0 -> 2.667 MgC/ha/yr
  testthat::expect_equal(crop$c_input_mgc_ha_yr, (100 * 2 + 50 * 4) / 150)
  testthat::expect_identical(crop$method_c_input, "humified_weighted")
})

testthat::test_that("spain_hist splits cropland into labelled groups", {
  out <- whep::build_carbon_inputs(
    data = .cig_data(.cig_share(wheat = 0.5, olive = 0)),
    crop_groups = list(method = "spain_hist")
  )
  crops <- out[whep:::.soc_is_cropland(out$land_use), ]
  testthat::expect_setequal(
    crops$land_use,
    c(
      "cropland_rainfed_herbaceous",
      "cropland_irrigated_herbaceous",
      "cropland_rainfed_olive"
    )
  )
  # Wheat's 100 ha split 50/50 by its irrigated share; olives wholly rainfed.
  # Each group keeps its own crop's density.
  rf <- crops[crops$land_use == "cropland_rainfed_herbaceous", ]
  ir <- crops[crops$land_use == "cropland_irrigated_herbaceous", ]
  ol <- crops[crops$land_use == "cropland_rainfed_olive", ]
  testthat::expect_equal(rf$c_input_mgc_ha_yr, 2.0)
  testthat::expect_equal(ir$c_input_mgc_ha_yr, 2.0)
  testthat::expect_equal(ol$c_input_mgc_ha_yr, 4.0)
  testthat::expect_true(all(
    crops$method_c_input == "humified_weighted_spain_hist"
  ))
  # Grassland and natural pass through untouched.
  testthat::expect_setequal(
    out$land_use[!whep:::.soc_is_cropland(out$land_use)],
    c("grassland", "natural")
  )
})

testthat::test_that("group areas partition the cropland area", {
  data <- .cig_data(.cig_share(wheat = 0.3, olive = 0.2))
  out <- whep::build_carbon_inputs(
    resolution = "polity",
    data = data,
    crop_groups = list(method = "spain_hist")
  )
  # Polity aggregation area-weights by class_area_ha, so a group's density
  # survives aggregation only if its area was carried. Check by conservation:
  # total cropland carbon mass equals the single-class total.
  single <- whep::build_carbon_inputs(
    resolution = "polity",
    data = data,
    crop_groups = list(method = "none")
  )
  grouped <- whep:::.ci_cropland_class(
    data$cropland,
    data$crop_area,
    data$crop_regime_share
  )
  mass_grouped <- sum(grouped$c_input_mgc_ha_yr * grouped$class_area_ha)
  testthat::expect_equal(mass_grouped, 100 * 2 + 50 * 4)
  testthat::expect_equal(sum(grouped$class_area_ha), 150)
  testthat::expect_identical(nrow(grouped), 4L)
  testthat::expect_true(all(
    whep:::.soc_is_cropland(out$land_use) |
      out$land_use %in% c("grassland", "natural")
  ))
  testthat::expect_identical(nrow(single[single$land_use == "cropland", ]), 1L)
})

testthat::test_that("a crop with no share row is booked rainfed and reported", {
  share <- .cig_share()[1, ] # olives have no row
  testthat::expect_message(
    out <- whep::build_carbon_inputs(
      data = .cig_data(share),
      crop_groups = list(method = "spain_hist")
    ),
    "no irrigated share"
  )
  testthat::expect_true("cropland_rainfed_olive" %in% out$land_use)
  testthat::expect_false("cropland_irrigated_olive" %in% out$land_use)
})

testthat::test_that("irrigation = none puts every crop in its rainfed group", {
  out <- whep::build_carbon_inputs(
    data = .cig_data(),
    crop_groups = list(method = "spain_hist", irrigation = "none")
  )
  crops <- out$land_use[whep:::.soc_is_cropland(out$land_use)]
  testthat::expect_setequal(
    crops,
    c("cropland_rainfed_herbaceous", "cropland_rainfed_olive")
  )
})

testthat::test_that("a malformed share layer is named", {
  testthat::expect_error(
    whep::build_carbon_inputs(
      data = .cig_data(tibble::tibble(lon = 0.25)),
      crop_groups = list(method = "spain_hist")
    ),
    "crop_regime_share"
  )
})

testthat::test_that("the spatialized split rides the carbon path's polycell support", {
  # The spatialize chain's country_grid.parquet is the centroid crosswalk
  # with no polity share; build_gridded_landuse() refuses it (S-A5), and a
  # second crosswalk would split the regimes on different polycells than the
  # carbon they split. The support handed down must be the carbon path's own,
  # with its cell_area_frac.
  seen <- NULL
  testthat::local_mocked_bindings(
    .read_spatial_input = function(...) tibble::tibble(),
    .sci_read_country_grid = function() {
      tibble::tibble(
        lon = c(0.25, 0.25),
        lat = 5.25,
        area_code = c(1L, 2L),
        cell_area_frac = c(0.6, 0.4),
        land_area_ha = 100
      )
    },
    build_gridded_landuse = function(
      country_areas,
      crop_patterns,
      gridded_cropland,
      country_grid,
      config = list()
    ) {
      seen <<- country_grid
      tibble::tibble(
        lon = 0.25,
        lat = 5.25,
        area_code = c(1L, 1L, 2L),
        item_prod_code = c("15", "260", "15"),
        year = 2010L,
        rainfed_ha = c(30, 0, 5),
        irrigated_ha = c(10, 0, 0)
      )
    },
    .package = "whep"
  )
  cfg <- whep:::.ci_group_config(list(method = "spain_hist"))
  share <- whep:::.ci_regime_shares(list(), 2010L, cfg)
  testthat::expect_named(
    seen,
    c("lon", "lat", "area_code", "cell_area_frac")
  )
  testthat::expect_equal(seen$cell_area_frac, c(0.6, 0.4))
  testthat::expect_equal(share$irrigated_share, c(0.25, 0, 0))
  testthat::expect_identical(share$item_prod_code, c("15", "260", "15"))
  # An injected support is used as given, not re-read.
  injected <- whep:::.ci_regime_shares(
    list(
      country_grid = tibble::tibble(
        lon = 0.75,
        lat = 5.25,
        area_code = 3L,
        cell_area_frac = 1
      )
    ),
    2010L,
    cfg
  )
  testthat::expect_equal(seen$lon, 0.75)
  testthat::expect_identical(nrow(injected), 3L)
})

testthat::test_that("shares are built for the years the crop layer carries", {
  # The production chain reads `years` as a range while the gridded land-use
  # builder takes exact years; a real run asked for c(2000, 2010), gridded
  # eleven years of carbon and had shares for two. The shares must follow the
  # layer being collapsed, year for year.
  asked <- list()
  testthat::local_mocked_bindings(
    .read_spatial_input = function(...) tibble::tibble(),
    .sci_read_country_grid = function() {
      tibble::tibble(lon = 0.25, lat = 5.25, area_code = 1L, cell_area_frac = 1)
    },
    build_gridded_landuse = function(
      country_areas,
      crop_patterns,
      gridded_cropland,
      country_grid,
      config = list()
    ) {
      asked[[length(asked) + 1L]] <<- config$years
      tidyr::expand_grid(
        lon = 0.25,
        lat = 5.25,
        area_code = 1L,
        item_prod_code = "15",
        year = config$years
      ) |>
        dplyr::mutate(rainfed_ha = 30, irrigated_ha = 10)
    },
    .package = "whep"
  )
  cropland <- tidyr::expand_grid(
    lon = 0.25,
    lat = 5.25,
    area_code = 1L,
    item_prod_code = "15",
    year = c(2000L, 2001L, 2002L)
  ) |>
    dplyr::mutate(
      total_c_input_mgc_ha_yr = 2,
      humified_fraction = 0.3,
      method_c_input = "humified_weighted",
      crop_area_ha = 100
    )
  crop_area <- tibble::tibble(
    lon = 0.25,
    lat = 5.25,
    area_code = 1L,
    item_prod_code = "15",
    crop_area_ha = 100
  )
  out <- whep:::.ci_cropland_input(
    list(cropland = cropland, crop_area = crop_area),
    years = c(2000L, 2002L),
    crop_area,
    whep:::.ci_group_config(list(method = "spain_hist"))
  )
  # Asked for exactly the layer's years, not the range argument.
  testthat::expect_identical(asked[[1]], c(2000L, 2001L, 2002L))
  # And every year got its split: no year is wholly rainfed.
  irrigated <- out[out$land_use == "cropland_irrigated_herbaceous", ]
  testthat::expect_setequal(irrigated$year, c(2000L, 2001L, 2002L))
  testthat::expect_equal(irrigated$class_area_ha, rep(25, 3))
})

testthat::test_that("an injected share layer is trimmed to the years asked", {
  cfg <- whep:::.ci_group_config(list(method = "spain_hist"))
  layer <- tibble::tibble(
    lon = 0.25,
    lat = 5.25,
    area_code = 1L,
    item_prod_code = "15",
    year = c(2000L, 2001L),
    irrigated_share = 0.5
  )
  testthat::expect_identical(
    whep:::.ci_regime_shares(list(crop_regime_share = layer), 2001L, cfg)$year,
    2001L
  )
  testthat::expect_identical(
    nrow(whep:::.ci_regime_shares(list(crop_regime_share = layer), NULL, cfg)),
    2L
  )
})
