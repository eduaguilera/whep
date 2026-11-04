.reporting_test_rect <- function(xmin, xmax) {
  sf::st_polygon(list(cbind(
    c(xmin, xmax, xmax, xmin, xmin),
    c(45.1, 45.1, 45.2, 45.2, 45.1)
  )))
}

.reporting_test_geoms <- function(overlap = FALSE) {
  sf::st_sf(
    polity_code = c("AAA-2000-2020", "BBB-2000-2020"),
    start_year = 2000L,
    end_year = 2020L,
    polity_type = c("national", "subnational"),
    area_code = c(1L, NA_integer_),
    geometry = sf::st_sfc(
      .reporting_test_rect(10.1, 10.2),
      .reporting_test_rect(if (overlap) 10.1 else 10.2, 10.3),
      crs = 4326
    )
  )
}

.reporting_test_inventory <- function() {
  tibble::tribble(
    ~source, ~year, ~reporting_polity_code,
    "fixture", 2010L, "AAA-2000-2020"
  )
}

.reporting_test_domain <- function(x) {
  sf::st_sf(
    domain_source = "fixture-complete-domain",
    year = 2010L,
    geometry = sf::st_union(x)
  )
}

testthat::test_that("reporting subset preserves the complete border domain", {
  testthat::skip_if_not_installed("sf")
  x <- .reporting_test_geoms()
  b <- suppressWarnings(whep::build_reporting_support(
    .reporting_test_inventory(),
    .reporting_test_domain(x),
    x,
    layers = list(
      water = tibble::tibble(lon = 10.25, lat = 45.25, water_frac = 0),
      ice = sf::st_sf(
        geometry = sf::st_sfc(
          .reporting_test_rect(10.8, 10.9),
          crs = 4326
        )
      )
    )
  ))
  s <- whep::read_reporting_support(b)
  testthat::expect_equal(nrow(b$canonical_geometries), 2L)
  testthat::expect_gt(s$domain_land_area_ha, s$land_area_ha)
  grid <- whep::read_level_country_grid(support = b, geographic_key = "polity")
  testthat::expect_equal(
    grid$cell_area_frac,
    s$land_area_ha / s$domain_land_area_ha
  )
  testthat::expect_lt(grid$cell_area_frac, 1)
  testthat::expect_gt(b$audit$unrepresented_land_ha, 0)
})

testthat::test_that("native support ignores source-code annotations", {
  testthat::skip_if_not_installed("sf")
  x <- .reporting_test_geoms()
  domain <- .reporting_test_domain(x)
  a <- suppressWarnings(whep::build_reporting_support(
    .reporting_test_inventory(),
    domain,
    x
  ))
  x$area_code <- c(NA_integer_, 999L)
  b <- suppressWarnings(whep::build_reporting_support(
    .reporting_test_inventory(),
    domain,
    x
  ))
  cols <- c("polity_code", "polity_area_ha", "domain_land_area_ha")
  testthat::expect_equal(a$support[cols], b$support[cols])
})

testthat::test_that("small coastal overlap cannot masquerade as partition", {
  testthat::skip_if_not_installed("sf")
  x <- .reporting_test_geoms(TRUE)
  inventory <- dplyr::bind_rows(
    .reporting_test_inventory(),
    dplyr::mutate(
      .reporting_test_inventory(),
      reporting_polity_code = "BBB-2000-2020"
    )
  )
  testthat::expect_error(
    whep::build_reporting_support(inventory, .reporting_test_domain(x), x),
    class = "whep_reporting_overlap"
  )
})

testthat::test_that("declared alternatives remain recoverable", {
  testthat::skip_if_not_installed("sf")
  x <- .reporting_test_geoms(TRUE)
  inventory <- tibble::tribble(
    ~source, ~year, ~reporting_polity_code, ~use, ~basis,
    "fixture", 2010L, "AAA-2000-2020", "reporting", "source territory",
    "fixture", 2010L, "BBB-2000-2020", "alternative", "separate total"
  )
  b <- suppressWarnings(whep::build_reporting_support(
    inventory,
    .reporting_test_domain(x),
    x
  ))
  testthat::expect_equal(nrow(b$inventory), 2L)
  testthat::expect_equal(nrow(b$canonical_geometries), 2L)
  testthat::expect_identical(unique(b$support$polity_code), "AAA-2000-2020")
})

testthat::test_that("inventory failures are explicit before geographic filtering", {
  testthat::skip_if_not_installed("sf")
  x <- .reporting_test_geoms()
  domain <- .reporting_test_domain(x)
  inventory <- .reporting_test_inventory()
  purrr::walk(list(2010.5, NA_real_, Inf, "2010"), function(year) {
    bad <- inventory
    bad$year <- year
    testthat::expect_error(
      whep::build_reporting_support(bad, domain, x),
      class = "whep_reporting_inventory"
    )
  })
  bad <- inventory
  bad$reporting_polity_code <- "UNKNOWN"
  testthat::expect_error(
    whep::build_reporting_support(bad, domain, x),
    class = "whep_reporting_geometry"
  )
  bad <- inventory
  bad$year <- 1999L
  testthat::expect_error(
    whep::build_reporting_support(bad, domain, x),
    class = "whep_reporting_geometry"
  )
  bad <- inventory
  bad$use <- "alternative"
  testthat::expect_error(
    whep::build_reporting_support(bad, domain, x),
    class = "whep_reporting_policy"
  )
  testthat::expect_error(
    whep::read_reporting_support(tibble::tibble()),
    class = "whep_reporting_bundle"
  )
})

testthat::test_that("missing domain and geometries never become zero coverage", {
  testthat::skip_if_not_installed("sf")
  x <- .reporting_test_geoms()
  inventory <- .reporting_test_inventory()
  domain <- .reporting_test_domain(x)
  bad_domain <- domain
  bad_domain$domain_source <- NA_character_
  testthat::expect_error(
    whep::build_reporting_support(inventory, bad_domain, x),
    class = "whep_reporting_domain"
  )
  narrow <- .reporting_test_domain(x[2, ])
  testthat::expect_error(
    whep::build_reporting_support(inventory, narrow, x),
    class = "whep_reporting_outside"
  )
  sf::st_geometry(x)[1] <- sf::st_sfc(sf::st_polygon(), crs = 4326)
  testthat::expect_error(
    whep::build_reporting_support(inventory, domain, x),
    class = "whep_reporting_geometry"
  )
})

testthat::test_that("source alternatives preserve historical subnational units", {
  testthat::skip_if_not_installed("sf")
  x <- .reporting_test_geoms()
  x$area_code <- NA_integer_
  inventory <- .reporting_test_inventory()
  inventory$reporting_polity_code <- "BBB-2000-2020"
  b <- suppressWarnings(whep::build_reporting_support(
    inventory,
    .reporting_test_domain(x),
    x
  ))
  testthat::expect_equal(unique(b$support$polity_code), "BBB-2000-2020")
  testthat::expect_equal(unique(b$support$layers_supplied), "none")
  testthat::expect_equal(
    sf::st_as_binary(sf::st_geometry(x)),
    sf::st_as_binary(sf::st_geometry(b$canonical_geometries))
  )
})

testthat::test_that("physical denominator cannot be forged below supplied land", {
  testthat::skip_if_not_installed("sf")
  x <- .reporting_test_geoms()
  b <- suppressWarnings(whep::build_reporting_support(
    .reporting_test_inventory(),
    .reporting_test_domain(x),
    x
  ))
  b$support$domain_land_area_ha <- b$support$land_area_ha / 2
  testthat::expect_error(
    whep:::.polity_grid_year_aware(b$support),
    class = "whep_reporting_denominator"
  )
})

testthat::test_that("default native reader does not certify an old unlabeled pin", {
  testthat::local_mocked_bindings(
    read_polycell_support = function(...) {
      tibble::tibble(
        lon = 10.25,
        lat = 45.25,
        polity_code = "AAA-2000-2020",
        start_year = 2000L,
        end_year = 2020L,
        cell_area_ha = 100,
        land_area_ha = 50
      )
    }
  )
  testthat::expect_error(
    whep::read_level_country_grid(geographic_key = "polity"),
    class = "whep_reporting_uncertified"
  )
})

testthat::test_that("overlap diagnostics identify identities and affected area", {
  testthat::skip_if_not_installed("sf")
  x <- .reporting_test_geoms(TRUE)
  inventory <- tibble::tibble(
    source = "fixture",
    year = 2010L,
    reporting_polity_code = x$polity_code
  )
  err <- tryCatch(
    whep::build_reporting_support(inventory, .reporting_test_domain(x), x),
    whep_reporting_overlap = identity
  )
  testthat::expect_equal(err$overlap$left, x$polity_code[1])
  testthat::expect_equal(err$overlap$right, x$polity_code[2])
  testthat::expect_gt(err$overlap$overlap_ha, 0)
})


testthat::test_that("physical domain is bound to its declared source year", {
  testthat::skip_if_not_installed("sf")
  x <- .reporting_test_geoms()
  domain <- .reporting_test_domain(x)
  domain$year <- 2009L
  testthat::expect_error(
    whep::build_reporting_support(.reporting_test_inventory(), domain, x),
    class = "whep_reporting_domain"
  )
})

testthat::test_that("serialized bundles preserve canonical footprints", {
  testthat::skip_if_not_installed("sf")
  x <- .reporting_test_geoms()
  b <- suppressWarnings(whep::build_reporting_support(
    .reporting_test_inventory(),
    .reporting_test_domain(x),
    x
  ))
  path <- withr::local_tempfile(fileext = ".rds")
  saveRDS(b, path)
  testthat::expect_equal(
    whep::read_reporting_support(path, require_layers = FALSE),
    b$support
  )
  broken <- b
  broken$domain <- NULL
  testthat::expect_error(
    whep::read_reporting_support(broken),
    class = "whep_reporting_bundle"
  )
})

testthat::test_that("supplied water and ice use the independent domain", {
  testthat::skip_if_not_installed("sf")
  x <- .reporting_test_geoms()
  layers <- list(
    water = tibble::tibble(lon = 10.25, lat = 45.25, water_frac = 0.01),
    ice = sf::st_sf(
      geometry = sf::st_sfc(
        .reporting_test_rect(10.11, 10.12),
        crs = 4326
      )
    )
  )
  b <- whep::build_reporting_support(
    .reporting_test_inventory(),
    .reporting_test_domain(x),
    x,
    layers
  )
  s <- b$support
  testthat::expect_gt(s$inland_water_ha, 0)
  testthat::expect_gt(s$ice_area_ha, 0)
  testthat::expect_equal(
    s$land_area_ha + s$inland_water_ha + s$ice_area_ha,
    s$polity_area_ha
  )
  testthat::expect_lt(s$land_area_ha, s$domain_land_area_ha)
  testthat::expect_silent(whep:::.check_polycell_layers(s, TRUE, "fixture"))
  testthat::expect_equal(
    b$audit$unrepresented_land_ha,
    s$domain_land_area_ha - s$land_area_ha
  )
})

testthat::test_that("recoverable footprint layer cannot become a partition", {
  testthat::skip_if_not_installed("sf")
  x <- .reporting_test_geoms(TRUE)
  inventory <- tibble::tribble(
    ~source, ~year, ~reporting_polity_code, ~use, ~basis,
    "fixture", 2010L, "AAA-2000-2020", "reporting", "source territory",
    "fixture", 2010L, "BBB-2000-2020", "alternative", "separate total"
  )
  b <- suppressWarnings(whep::build_reporting_support(
    inventory,
    .reporting_test_domain(x),
    x
  ))
  footprints <- whep::read_reporting_support(
    b,
    role = "footprints",
    require_layers = FALSE
  )
  testthat::expect_setequal(footprints$polity_code, x$polity_code)
  testthat::expect_true(all(footprints$support_role == "overlap"))
  testthat::expect_error(
    whep::read_level_country_grid(
      support = footprints,
      geographic_key = "polity"
    ),
    class = "whep_polity_grid_role"
  )
})

testthat::test_that("native geometry intervals validate before integer coercion", {
  testthat::skip_if_not_installed("sf")
  x <- .reporting_test_geoms()
  domain <- .reporting_test_domain(x)
  x$start_year[1] <- 2000.5
  testthat::expect_error(
    whep::build_reporting_support(
      .reporting_test_inventory(),
      domain,
      x
    ),
    class = "whep_reporting_geometry"
  )
  x$start_year <- c(2000, 2000.5)
  testthat::expect_s3_class(
    suppressWarnings(whep::build_reporting_support(
      .reporting_test_inventory(),
      domain,
      x
    )),
    "whep_reporting_support"
  )
})

testthat::test_that("claim diagnostics are invariant to swapped source codes", {
  testthat::skip_if_not_installed("sf")
  x <- .reporting_test_geoms(TRUE)
  inventory <- tibble::tibble(
    source = "fixture",
    year = 2010L,
    reporting_polity_code = x$polity_code
  )
  domain <- .reporting_test_domain(x)
  before <- tryCatch(
    whep::build_reporting_support(inventory, domain, x),
    whep_reporting_overlap = identity
  )
  x$area_code <- c(NA_integer_, 1L)
  after <- tryCatch(
    whep::build_reporting_support(inventory, domain, x),
    whep_reporting_overlap = identity
  )
  testthat::expect_equal(before$overlap, after$overlap)
  inventory$use <- c("reporting", "alternative")
  inventory$basis <- "explicit source alternative"
  b <- suppressWarnings(whep::build_reporting_support(inventory, domain, x))
  testthat::expect_lt(
    sum(b$footprints$polity_area_ha),
    unique(b$footprints$cell_area_ha)
  )
})

testthat::test_that("concentrated ice cannot strand supplied domain water", {
  testthat::skip_if_not_installed("sf")
  x <- .reporting_test_geoms()
  inventory <- tibble::tibble(
    source = "fixture",
    year = 2010L,
    reporting_polity_code = x$polity_code
  )
  domain <- .reporting_test_domain(x)
  dry <- suppressWarnings(whep::build_reporting_support(inventory, domain, x))
  area <- unique(dry$support$domain_area_ha)
  cell <- unique(dry$support$cell_area_ha)
  layers <- list(
    water = tibble::tibble(
      lon = 10.25,
      lat = 45.25,
      water_frac = 0.5 * area / cell
    ),
    ice = sf::st_sf(
      geometry = sf::st_sfc(
        .reporting_test_rect(10.1, 10.18),
        crs = 4326
      )
    )
  )
  b <- whep::build_reporting_support(inventory, domain, x, layers)
  testthat::expect_equal(sum(b$support$inland_water_ha), 0.5 * area)
  testthat::expect_equal(
    sum(b$support$land_area_ha),
    unique(b$support$domain_land_area_ha)
  )
  testthat::expect_silent(whep::read_level_country_grid(
    support = b,
    geographic_key = "polity"
  ))
})

testthat::test_that("persisted footprint membership and years are validated", {
  testthat::skip_if_not_installed("sf")
  x <- .reporting_test_geoms()
  b <- suppressWarnings(whep::build_reporting_support(
    .reporting_test_inventory(),
    .reporting_test_domain(x),
    x
  ))
  broken <- b
  broken$footprints <- NULL
  testthat::expect_error(
    whep::read_reporting_support(broken, "footprints"),
    class = "whep_reporting_bundle"
  )
  broken <- b
  broken$support$polity_code <- "UNKNOWN"
  testthat::expect_error(
    whep::read_reporting_support(broken),
    class = "whep_reporting_bundle"
  )
  broken <- b
  broken$footprints$start_year <- 2010.5
  testthat::expect_error(
    whep::read_reporting_support(broken, "footprints"),
    class = "whep_reporting_bundle"
  )
  broken <- b
  broken$inventory$year <- 2009L
  testthat::expect_error(
    whep::read_reporting_support(broken),
    class = "whep_reporting_bundle"
  )
  broken <- b
  broken$support$source <- "other-source"
  testthat::expect_error(
    whep::read_reporting_support(broken),
    class = "whep_reporting_bundle"
  )
})

testthat::test_that("canonical whole Germany survives derived domain union", {
  testthat::skip_if_not_installed("sf")
  testthat::skip_if_not_installed("terra")
  x <- whep::get_polity_geometries(c(
    "DEU-1949-1990",
    "F77-1949-1990",
    "F78-1949-1990"
  ))
  domain <- x
  domain$domain_source <- "packaged whole and divided Germany"
  domain$year <- 1960L
  inventory <- tibble::tibble(
    source = "fixture",
    year = 1960L,
    reporting_polity_code = "DEU-1949-1990"
  )
  b <- suppressWarnings(whep::build_reporting_support(inventory, domain, x))
  testthat::expect_identical(unique(b$support$polity_code), "DEU-1949-1990")
  testthat::expect_equal(
    sf::st_as_binary(sf::st_geometry(b$canonical_geometries)),
    sf::st_as_binary(sf::st_geometry(x))
  )
})

testthat::test_that("supplied water demand retains excess outside physical capacity", {
  testthat::skip_if_not_installed("sf")
  x <- .reporting_test_geoms()
  inventory <- tibble::tibble(
    source = "fixture",
    year = 2010L,
    reporting_polity_code = x$polity_code
  )
  b <- suppressWarnings(whep::build_reporting_support(
    inventory,
    .reporting_test_domain(x),
    x,
    list(water = tibble::tibble(lon = 10.25, lat = 45.25, water_frac = 1))
  ))
  physical <- b$domain_support
  testthat::expect_equal(
    physical$inland_water_ha + physical$water_excess_ha,
    physical$cell_area_ha
  )
  testthat::expect_gt(physical$water_excess_ha, 0)
  testthat::expect_equal(
    b$audit$domain_water_excess_ha,
    physical$water_excess_ha
  )
  testthat::expect_equal(
    sum(b$support$inland_water_ha),
    physical$inland_water_ha
  )
  testthat::expect_true(all(
    b$support$water_spatial_method == "uniform_ice_free_area"
  ))
})

testthat::test_that("geometry-only bundles require an explicit layer opt-out", {
  testthat::skip_if_not_installed("sf")
  x <- .reporting_test_geoms()
  b <- suppressWarnings(whep::build_reporting_support(
    .reporting_test_inventory(),
    .reporting_test_domain(x),
    x
  ))
  testthat::expect_error(
    whep::read_reporting_support(b),
    class = "whep_polycell_absent_layers"
  )
  testthat::expect_error(
    whep::read_level_country_grid(
      support = b,
      geographic_key = "polity"
    ),
    class = "whep_polycell_absent_layers"
  )
  testthat::expect_equal(
    whep::read_reporting_support(b, require_layers = FALSE),
    b$support
  )
})

testthat::test_that("domain provenance and physical component totals survive reads", {
  testthat::skip_if_not_installed("sf")
  x <- .reporting_test_geoms()
  b <- suppressWarnings(whep::build_reporting_support(
    .reporting_test_inventory(),
    .reporting_test_domain(x),
    x
  ))
  broken <- b
  broken$domain$domain_source <- NULL
  testthat::expect_error(
    whep::read_reporting_support(broken, require_layers = FALSE),
    class = "whep_reporting_bundle"
  )
  broken <- b
  broken$domain_support$start_year <- 2010.5
  testthat::expect_error(
    whep::read_reporting_support(broken, require_layers = FALSE),
    class = "whep_reporting_bundle"
  )
  broken <- b
  broken$domain_support$inland_water_ha <- 42
  testthat::expect_error(
    whep::read_reporting_support(broken, require_layers = FALSE),
    class = "whep_reporting_bundle"
  )
})

testthat::test_that("numerical closure retains domain and genuinely unrepresented land", {
  x <- tibble::tribble(
    ~cell_id, ~polity_code, ~land_area_ha, ~domain_land_area_ha,
    1L, "AAA", 30, 100,
    1L, "BBB", 70 + 5e-9, 100,
    2L, "AAA", 30, 100,
    2L, "BBB", 50, 100
  )
  out <- whep:::.rs_close_support(x)
  testthat::expect_identical(out$domain_land_area_ha, x$domain_land_area_ha)
  testthat::expect_identical(out$measured_land_area_ha, x$land_area_ha)
  testthat::expect_lte(sum(out$land_area_ha[out$cell_id == 1]), 100)
  testthat::expect_equal(out$land_area_ha[out$cell_id == 2], c(30, 50))
  testthat::expect_equal(sum(out$area_adjustment_ha[out$cell_id == 2]), 0)
  testthat::expect_lte(sum(out$area_adjustment_ha), whep:::.pcs_area_floor_ha())
  testthat::expect_gt(sum(out$area_adjustment_ha), 0)
  testthat::expect_setequal(out$polity_code, x$polity_code)
})

testthat::test_that("closure refuses discrepancies above existing support resolution", {
  x <- tibble::tibble(
    cell_id = 1L,
    polity_code = "AAA",
    land_area_ha = 100 + whep:::.pcs_area_floor_ha() + 1e-9,
    domain_land_area_ha = 100
  )
  testthat::expect_error(
    whep:::.rs_close_support(x),
    class = "whep_reporting_capacity"
  )
})

testthat::test_that("native geometry support requires no FAOSTAT lookup", {
  testthat::skip_if_not_installed("sf")
  testthat::local_mocked_bindings(
    .polity_reporting_area_code = function(...) {
      cli::cli_abort("FAOSTAT lookup forbidden")
    }
  )
  x <- .reporting_test_geoms()
  x$area_code <- NULL
  testthat::expect_s3_class(
    suppressWarnings(whep::build_reporting_support(
      .reporting_test_inventory(),
      .reporting_test_domain(x),
      x
    )),
    "whep_reporting_support"
  )
})

testthat::test_that("persisted row denominators and layer provenance match domain cells", {
  testthat::skip_if_not_installed("sf")
  x <- .reporting_test_geoms()
  b <- suppressWarnings(whep::build_reporting_support(
    .reporting_test_inventory(),
    .reporting_test_domain(x),
    x
  ))
  broken <- b
  broken$domain_support$polity_area_ha <- 2 * b$domain_support$polity_area_ha
  broken$domain_support$land_area_ha <- 2 * b$domain_support$land_area_ha
  broken$audit <- whep:::.rs_audit(broken$support, broken$domain_support)
  testthat::expect_error(
    whep::read_reporting_support(broken, require_layers = FALSE),
    class = "whep_reporting_bundle"
  )
  broken <- b
  broken$domain_support$cell_id <- broken$domain_support$cell_id + 1L
  broken$audit <- whep:::.rs_audit(broken$support, broken$domain_support)
  testthat::expect_error(
    whep::read_reporting_support(broken, require_layers = FALSE),
    class = "whep_reporting_bundle"
  )
  broken <- b
  broken$domain_support$layers_supplied <- "water+ice"
  testthat::expect_error(
    whep::read_reporting_support(broken, require_layers = FALSE),
    class = "whep_reporting_bundle"
  )
  broken <- b
  broken$domain_support$water_excess_ha <- 1
  broken$audit <- whep:::.rs_audit(broken$support, broken$domain_support)
  testthat::expect_error(
    whep::read_reporting_support(broken, require_layers = FALSE),
    class = "whep_reporting_bundle"
  )
  broken <- b
  broken$footprints$inland_water_ha <- 1
  broken$support <- whep:::.rs_close_support(broken$footprints)
  broken$support$support_role <- "partition"
  broken$audit <- whep:::.rs_audit(broken$support, broken$domain_support)
  testthat::expect_error(
    whep::read_reporting_support(broken, require_layers = FALSE),
    class = "whep_reporting_bundle"
  )
  testthat::expect_identical(
    whep::read_reporting_support(b, require_layers = FALSE),
    b$support
  )
})
