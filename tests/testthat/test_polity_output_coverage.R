# The set of polities whose status asserts a polygon the upstream GeoPackage
# cannot carry. Returns NULL when the manifest is unavailable, so the caller can
# skip rather than guess.
#
# This used to fall back to a hardcoded list of six codes. That is a copy of
# upstream data living in a test, and it drifted exactly as such copies do: when
# whep#382 brought the NRH chain into the crosswalk, NRH-1911-1953 — a gap
# upstream has published all along — was absent from the copy, so the suite
# passed locally (manifest readable) and failed on CI (manifest absent). A test
# that is STRICTER on CI than on a developer's machine is worse than no test,
# because the failure teaches nothing about the code under change.
#
# Returning NULL is not weakening the check: without the baseline the assertion
# cannot tell a published gap from a new one, so it has nothing to assert.
upstream_manifest <- function() {
  path <- Sys.getenv(
    "WHEP_POLITIES_MANIFEST",
    unset = path.expand("~/whep-polities/data/final/polities_manifest.json")
  )
  if (!file.exists(path)) {
    return(NULL)
  }
  jsonlite::fromJSON(path, simplifyVector = TRUE)
}

upstream_polygon_gaps <- function() {
  mf <- upstream_manifest()
  if (is.null(mf)) {
    return(NULL)
  }
  mf$polygon_gap_polity_codes
}

# Which polygon_status values ASSERT a polygon exists. Read from the manifest rather
# than written as "anything except unassigned": that hardcoding holds only because the
# vocabulary happens to have exactly one non-claiming value today. While the four
# legacy statuses were still in use, three of them (`missing`, `excluded`,
# `unassigned`) asserted no polygon, and a `!= "unassigned"` test would have counted
# two of those as claiming one.
upstream_claims_polygon <- function() {
  mf <- upstream_manifest()
  if (is.null(mf)) {
    return(NULL)
  }
  mf$claims_polygon_status
}

# WHAT EVIDENCE EACH BUILDER HAS, because "the tests pass" and "it was run on real data"
# are different claims and this file only makes the first one. Recorded so a reader does not
# assume the stronger one.
#
#   smoked on real pins        build_primary_production (170,972 rows, 0 polity NA)
#                             build_commodity_balances, chained (265,124 rows, 0 NA)
#                             build_detailed_trade (9,966,763 rows, 0 NA across all seven
#                               columns including partner)
#   example only, plus a       build_supply_use, get_feed_intake, build_processing_coefs,
#     transitive argument      get_primary_residues, get_wide_cbs, get_processing_coefs
#   synthetic geometry         build_gridded_landuse, build_gridded_livestock, the IO and
#                               footprint outputs, build_constant_territory_series
#
# The transitive argument for the middle group: each joins on `area_code` from
# get_wide_cbs() or get_primary_production(), both of which were smoked at zero polity NA,
# and none of them resolves a polity itself. So a polity NA there would have to come from
# the join losing rows, which the assertions below would catch on the example data.
#
# It is weaker than a real run and it is why they are listed separately. Three attempts at
# real runs exceeded a nine-minute budget -- get_feed_intake, build_constant_territory_series
# and build_supply_use -- because `.cache_get()` is cold in a fresh session, so each rebuilds
# production and CBS from scratch before doing its own work. Warming the cache costs the same
# time. The honest position is that these three are unverified on full-scale data, not that
# they are verified because a test passed.

expect_polity_match <- function(data, code_col, polity_col) {
  testthat::expect_true(code_col %in% names(data))
  testthat::expect_true(polity_col %in% names(data))
  has_code <- !is.na(data[[code_col]])
  testthat::expect_false(any(is.na(data[[polity_col]][has_code])))
}

testthat::test_that("public area-code example outputs carry reporting polities", {
  outputs <- list(
    get_primary_production = get_primary_production(example = TRUE),
    build_primary_production = build_primary_production(example = TRUE),
    get_wide_cbs = get_wide_cbs(example = TRUE),
    build_commodity_balances = build_commodity_balances(example = TRUE),
    build_processing_coefs = build_processing_coefs(example = TRUE),
    get_processing_coefs = get_processing_coefs(example = TRUE),
    get_primary_residues = get_primary_residues(example = TRUE),
    get_feed_intake = get_feed_intake(example = TRUE),
    build_supply_use = build_supply_use(example = TRUE)
  )

  purrr::walk(
    outputs,
    \(output) {
      expect_polity_match(
        output,
        "area_code",
        "reporting_polity_code"
      )
    }
  )
})

testthat::test_that("public trade example output carries reporter and partner polities", {
  output <- build_detailed_trade(example = TRUE)

  expect_polity_match(output, "area_code", "reporting_polity_code")
  expect_polity_match(output, "area_code_partner", "partner_polity_code")
})

testthat::test_that("IO and footprint role outputs carry role polities", {
  supply_use <- tibble::tribble(
    ~year, ~area_code, ~proc_group, ~proc_cbs_code, ~item_cbs_code, ~type, ~value,
    2000L, 1L, "crop_production", 10L, 10L, "supply", 100,
    2000L, 1L, "crop_production", 10L, 10L, "use", 5,
    2000L, 2L, "crop_production", 10L, 10L, "supply", 80,
    2000L, 2L, "crop_production", 10L, 10L, "use", 3
  )
  bilateral_trade <- tibble::tibble(
    year = 2000L,
    item_cbs_code = 10L,
    bilateral_trade = list(matrix(c(0, 5, 3, 0), nrow = 2))
  )
  cbs <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~production, ~import, ~export, ~food, ~other_uses, ~stock_withdrawal, ~stock_addition,
    2000L, 1L, 10L, 100, 3, 5, 50, 20, 0, 0,
    2000L, 2L, 10L, 80, 5, 3, 40, 15, 0, 0
  )

  io <- build_io_model(supply_use, bilateral_trade, cbs)
  labels <- io$labels[[1]]
  fd_labels <- io$fd_labels[[1]]
  extensions <- rep(1, length(io$X[[1]]))

  expect_polity_match(labels, "area_code", "reporting_polity_code")
  expect_polity_match(fd_labels, "area_code", "reporting_polity_code")

  footprint <- compute_footprint(
    x_vec = io$X[[1]],
    y_mat = io$Y[[1]],
    extensions = extensions,
    labels = labels,
    z_mat = io$Z[[1]],
    fd_labels = fd_labels
  )
  paths <- compute_footprint_paths(
    io$Z[[1]],
    io$X[[1]],
    io$Y[[1]],
    extensions,
    labels,
    fd_labels,
    conserve_extensions = FALSE
  )
  product_paths <- compute_fp_product_paths(
    io$Z[[1]],
    io$X[[1]],
    io$Y[[1]],
    extensions,
    labels,
    fd_labels,
    conserve_extensions = FALSE
  )

  expect_polity_match(footprint, "origin_area", "origin_polity_code")
  expect_polity_match(footprint, "target_area", "target_polity_code")
  expect_polity_match(paths, "origin_area", "origin_polity_code")
  expect_polity_match(paths, "use_area", "use_polity_code")
  expect_polity_match(paths, "target_area", "target_polity_code")
  expect_polity_match(product_paths, "origin_area", "origin_polity_code")
  expect_polity_match(product_paths, "product_area", "product_polity_code")
  expect_polity_match(product_paths, "target_area", "target_polity_code")
})

testthat::test_that("spatialized public outputs carry reporting polities", {
  country_areas <- tibble::tibble(
    year = 2000L,
    area_code = 1L,
    item_prod_code = 15L,
    harvested_area_ha = 1000,
    irrigated_area_ha = 0
  )
  crop_patterns <- tibble::tibble(
    lon = c(0.25, 0.75),
    lat = c(50.25, 50.25),
    item_prod_code = 15L,
    harvest_fraction = c(0.6, 0.4)
  )
  gridded_cropland <- tibble::tibble(
    lon = c(0.25, 0.75),
    lat = c(50.25, 50.25),
    year = 2000L,
    cropland_ha = c(800, 500),
    irrigated_ha = 0
  )
  country_grid <- tibble::tibble(
    lon = c(0.25, 0.75),
    lat = c(50.25, 50.25),
    area_code = 1L
  )

  landuse <- build_gridded_landuse(
    country_areas,
    crop_patterns,
    gridded_cropland,
    country_grid,
    config = list(years = 2000L)
  )
  livestock <- build_gridded_livestock(
    tibble::tibble(
      year = 2000L,
      area_code = 1L,
      species_group = "cattle",
      heads = 5000
    ),
    tibble::tibble(
      lon = c(0.25, 0.75),
      lat = c(50.25, 50.25),
      year = 2000L,
      pasture_ha = c(600, 400),
      rangeland_ha = c(200, 100)
    ),
    gridded_cropland,
    country_grid
  )

  expect_polity_match(landuse, "area_code", "reporting_polity_code")
  expect_polity_match(livestock, "area_code", "reporting_polity_code")
})

testthat::test_that("legacy area reference tables are backed by polities", {
  # FAOSTAT area 351 "China" is a statistical aggregate of its components
  # (mainland 41, Hong Kong 96, Macao 128, Taiwan 214), reported alongside them
  # for every year. It is intentionally left unmapped so it cannot double-count
  # China; every OTHER reporting area must still map to a polity. The dedup
  # lives in the table_mappings data-raw script.
  aggregate_codes <- 351L

  cw <- whep::polity_area_crosswalk
  testthat::expect_true(all(is.na(cw$polity_code[
    cw$area_code %in% aggregate_codes
  ])))
  cw <- cw[!cw$area_code %in% aggregate_codes, ]
  testthat::expect_false(any(is.na(cw$polity_code)))
  # Every crosswalk polity must have a polygon UNLESS it is explicitly
  # polygon_status == "unassigned": some historical periods (e.g. pre-1883
  # Chile, before the War of the Pacific) have no faithful-vintage polygon,
  # and we record an honest gap rather than back-project a later/modern border.
  #
  # Exception: upstream tracks a backlog of rows whose status DOES assert a
  # polygon the GeoPackage cannot carry, because their feature id was recorded
  # as prose rather than a resolvable value (whep-polities issue #3, listed in
  # its scripts/validate_polygons_baseline.txt and published as
  # polygon_gap_polity_codes in its manifest). Asserting the strict invariant
  # here makes this test red until that backlog clears, which is how a test stops
  # being read; tolerating exactly the published set keeps it sharp for anything
  # NEW. test_polities_upstream_contract.R makes the same assertion against the
  # manifest directly.
  #
  # Skipped, not guessed, when the manifest is absent — see upstream_polygon_gaps().
  known_gaps <- upstream_polygon_gaps()
  if (is.null(known_gaps)) {
    testthat::succeed(
      "upstream manifest unavailable, so a published polygon gap cannot be told
       apart from a new one; assertion skipped"
    )
  } else {
    no_geometry <- cw[!cw$has_geometry, ]
    # Which statuses CLAIM a polygon is read from the manifest, not asserted here as
    # "anything except unassigned". That hardcoding held only because the vocabulary
    # happens to have exactly one non-claiming value today; when four legacy statuses
    # were still in use (`derived`, `missing`, `approximate`, `excluded`), three of them
    # asserted no polygon and this comparison would have counted them as claiming one.
    claims <- upstream_claims_polygon()
    unexpected <- no_geometry[
      no_geometry$polygon_status %in%
        claims &
        !no_geometry$polity_code %in% known_gaps,
    ]
    testthat::expect_equal(
      nrow(unexpected),
      0L,
      info = paste0(
        "polities lack geometry while claiming one, outside the upstream ",
        "backlog: ",
        paste(utils::head(unique(unexpected$polity_code), 10), collapse = ", ")
      )
    )
  }

  for (data in list(whep::regions_full, whep::polities_cats)) {
    data <- data[!data$code %in% aggregate_codes, ]
    expect_polity_match(data, "code", "reporting_polity_code")
    # `polity_prefix`, not `polity_code` — these tables carry the ISO3-shaped
    # family key, and `reporting_polity_code` above carries the real code. On
    # the old name this read an absent column, so `is.na(NULL)` was
    # `logical(0)`, `any()` of it was FALSE, and the assertion passed
    # vacuously while only emitting a warning.
    testthat::expect_false(any(is.na(data$polity_prefix)))
    coded_rows <- !is.na(data$code)
    testthat::expect_true(all(data$reporting_polity_has_geometry[coded_rows]))
  }
})
