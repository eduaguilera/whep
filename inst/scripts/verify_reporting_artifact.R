# Offline driver regression checks. Run from the source checkout after
# pkgload::load_all(); this developer script is not shipped in the tarball.
source("inst/scripts/build_reporting_support.R")

testthat::test_that("artifact water coverage is explicit, not inferred", {
  domain <- tibble::tibble(lon = c(10.25, 10.75), lat = c(45.25, 45.25))
  partial <- tibble::tibble(lon = 10.25, lat = 45.25, water_frac = 0.1)
  testthat::expect_error(
    .rsa_check_water(domain, partial),
    class = "whep_artifact_water"
  )
  full <- dplyr::bind_rows(
    partial,
    tibble::tibble(lon = 10.75, lat = 45.25, water_frac = 0)
  )
  testthat::expect_silent(.rsa_check_water(domain, full))
  testthat::expect_error(
    .rsa_check_water(domain, full[c(1, 1, 2), ]),
    class = "whep_artifact_water"
  )
  full$water_frac[2] <- NA_real_
  testthat::expect_error(
    .rsa_check_water(domain, full),
    class = "whep_artifact_water"
  )
})

testthat::test_that("artifact writer preserves a complete native bundle", {
  testthat::skip_if_not_installed("sf")
  root <- withr::local_tempdir()
  x <- whep::polycell_example_geometries()
  domain <- sf::st_sf(
    domain_source = "offline fixture",
    year = 2010L,
    geometry = sf::st_union(x)
  )
  inv <- tibble::tibble(
    source = "fixture",
    year = 2010L,
    reporting_polity_code = x$polity_code
  )
  # Both physical layers are supplied; the toy ice polygon lies outside the
  # domain, representing a checked source with no local glacier.
  water <- tidyr::expand_grid(
    lon = c(10.25, 10.75, 11.25),
    lat = c(44.75, 45.25)
  ) |>
    dplyr::mutate(water_frac = 0.01)
  ice <- x
  sf::st_geometry(ice) <- sf::st_geometry(x) + c(20, 0)
  sf::st_crs(ice) <- 4326
  readr::write_csv(inv, file.path(root, "inventory.csv"))
  saveRDS(domain, file.path(root, "domain.rds"))
  saveRDS(water, file.path(root, "water.rds"))
  saveRDS(ice, file.path(root, "ice.rds"))
  config <- list(
    inventory = file.path(root, "inventory.csv"),
    domain = file.path(root, "domain.rds"),
    water = file.path(root, "water.rds"),
    ice = file.path(root, "ice.rds"),
    provenance = list(
      cohort_id = "offline fixture",
      domain_basis = "declared complete toy rectangle",
      inventory_basis = "one toy observation",
      source_files = list(geometry = file.path(root, "domain.rds"))
    )
  )
  testthat::local_mocked_bindings(
    get_polity_geometries = function() x,
    .package = "whep"
  )
  result <- .rsa_build(config, file.path(root, "result"))
  testthat::expect_true(result$readback_identical)
  testthat::expect_false(result$global_pin_restored)
  testthat::expect_false(result$published)
  testthat::expect_setequal(
    result$footprint_identities,
    inv$reporting_polity_code
  )
  testthat::expect_error(
    .rsa_build(config, file.path(root, "result")),
    "already exists"
  )
  invalid <- inv
  invalid$reporting_polity_code <- "ABSENT-2000-2020"
  readr::write_csv(invalid, config$inventory)
  testthat::expect_error(
    .rsa_build(config, file.path(root, "unknown")),
    class = "whep_reporting_geometry"
  )
  testthat::expect_false(dir.exists(file.path(root, "unknown")))
  readr::write_csv(inv, config$inventory)
  saveRDS(water[1, ], config$water)
  testthat::expect_error(
    suppressWarnings(.rsa_build(config, file.path(root, "incomplete"))),
    class = "whep_artifact_water"
  )
  testthat::expect_false(dir.exists(file.path(root, "incomplete")))
})

testthat::test_that("artifact config rejects ambiguous attestations and paths", {
  root <- withr::local_tempdir()
  path <- file.path(root, "input")
  writeLines("fixture", path)
  config <- list(
    inventory = path,
    domain = path,
    water = path,
    ice = path,
    provenance = list(
      cohort_id = "fixture",
      inventory_basis = "fixture",
      domain_basis = "fixture",
      source_files = list(source = path)
    )
  )
  testthat::expect_equal(nrow(.rsa_sources(config)), 5L)
  bad <- config
  bad$provenance$cohort_id <- NA_character_
  testthat::expect_error(.rsa_sources(bad), class = "whep_artifact_config")
  bad$provenance$cohort_id <- " "
  testthat::expect_error(.rsa_sources(bad), class = "whep_artifact_config")
  bad <- config
  bad$water <- c(path, path)
  testthat::expect_error(.rsa_sources(bad), class = "whep_artifact_config")
  bad <- config
  names(bad$provenance$source_files) <- "water"
  testthat::expect_error(.rsa_sources(bad), class = "whep_artifact_config")
  bad <- config
  bad$domain <- root
  testthat::expect_error(.rsa_sources(bad), class = "whep_artifact_config")
  testthat::expect_error(
    .rsa_build(config, c(root, root)),
    class = "whep_artifact_config"
  )
  testthat::expect_error(
    .rsa_build(config, path),
    class = "whep_artifact_output"
  )
  testthat::expect_identical(readLines(path), "fixture")
})
