# Tests for the Natural Earth -> area_code resolution shared by
# `prepare_country_grid()` and `build_cell_polity_fraction()` in
# inst/scripts/prepare_spatialize_all.R (whep#1297). Both live at script scope,
# so the script is sourced first, as the other prepare_spatialize tests do.
#
# Natural Earth gives some territories a code that is not an ISO3 code and
# that `regions.csv` therefore cannot match -- Somaliland is `SOL`. An
# unmatched feature contributes no cell to either grid, so its land vanished
# from every gridded build: 55 cells and 10.5 Mha of IMAGE 2010 agricultural
# land in northern Somalia alone.

.source_prepare_spatialize()

# A toy Natural Earth layer in the shapefile's own attribute layout: Somalia,
# Somaliland (no ISO3, `SOL`) and Kosovo (no ISO3, `KOS`, which has no
# reporting area at all, whep#933).
.cg_write_toy_natural_earth <- function(dir) {
  wkt <- c(
    "POLYGON ((44 7, 46 7, 46 9, 44 9, 44 7))",
    "POLYGON ((44 9, 46 9, 46 10, 44 10, 44 9))",
    "POLYGON ((20 42, 21 42, 21 43, 20 43, 20 42))"
  )
  countries <- terra::vect(wkt, crs = "EPSG:4326")
  countries$ADMIN <- c("Somalia", "Somaliland", "Kosovo")
  countries$ISO_A3 <- c("SOM", "-99", "-99")
  countries$ISO_A3_EH <- c("SOM", "-99", "-99")
  countries$ADM0_A3 <- c("SOM", "SOL", "KOS")
  shp_dir <- file.path(dir, "NaturalEarth", "Countries_shape")
  dir.create(shp_dir, recursive = TRUE)
  terra::writeVector(
    countries,
    file.path(shp_dir, "ne_10m_admin_0_countries.shp"),
    filetype = "ESRI Shapefile"
  )
  invisible(dir)
}

# terra writes its integer NoData as -2147483648, which the producers coerce to
# NA on purpose (#381), so every toy build warns about that coercion. Muffle
# only that warning; anything else still surfaces.
.cg_quietly <- function(expr) {
  withCallingHandlers(
    suppressMessages(expr),
    warning = function(w) {
      if (grepl("coercion to integer range", conditionMessage(w))) {
        invokeRestart("muffleWarning")
      }
    }
  )
}

.cg_somaliland_cells <- function(grid) {
  dplyr::filter(grid, .data$lat > 9, .data$lon > 44, .data$lon < 46)
}


test_that("Somaliland cells are on the country grid as Somalia", {
  .need_spatialize_helper("prepare_country_grid")
  testthat::skip_if_not_installed("terra")
  dir <- .cg_write_toy_natural_earth(withr::local_tempdir())

  grid <- .cg_quietly(prepare_country_grid(dir, 0.5))

  # 4 x 2 half-degree cells north of 9 N.
  somaliland <- .cg_somaliland_cells(grid)
  expect_equal(nrow(somaliland), 8L)
  expect_true(all(somaliland$area_code == 201L))
  expect_false(any(grid$lon > 20 & grid$lon < 21))
})


test_that("the cell x polity crosswalk keeps Somaliland and names Kosovo", {
  .need_spatialize_helper("build_cell_polity_fraction")
  testthat::skip_if_not_installed("terra")
  dir <- .cg_write_toy_natural_earth(withr::local_tempdir())
  grid <- .cg_quietly(prepare_country_grid(dir, 0.5))

  warnings <- testthat::capture_warnings(
    fraction <- .cg_quietly(
      build_cell_polity_fraction(dir, grid, 0.5, subcells = 2L)
    )
  )
  # Kosovo has no reporting area (whep#933) and must still be named; Somaliland
  # must no longer be.
  expect_true(any(grepl("KOS", warnings)))
  expect_false(any(grepl("SOL", warnings)))

  somaliland <- .cg_somaliland_cells(fraction)
  expect_equal(nrow(somaliland), 8L)
  expect_true(all(somaliland$area_code == 201L))
  expect_true(all(somaliland$polity_frac == 1))
  # Somalia's 16 cells plus Somaliland's 8, each summing to 1.
  sums <- dplyr::summarise(fraction, s = sum(polity_frac), .by = c(lon, lat))
  expect_equal(nrow(sums), 24L)
  expect_true(all(abs(sums$s - 1) < 1e-12))
})


test_that("the iso3c cascade prefers ISO_A3, then ISO_A3_EH, then ADM0_A3", {
  .need_spatialize_helper(".natural_earth_iso3c")
  countries <- data.frame(
    ISO_A3 = c("SOM", "-99", "-99", "-99"),
    ISO_A3_EH = c("SOM", "NOR", "-99", "-99"),
    ADM0_A3 = c("SOM", "NOR", "SOL", "KOS")
  )
  expect_identical(
    .natural_earth_iso3c(countries),
    c("SOM", "NOR", "SOM", "KOS")
  )
})


test_that("every Natural Earth alias lands on a regions.csv reporting area", {
  .need_spatialize_helper(".natural_earth_iso3c_aliases")
  aliases <- .natural_earth_iso3c_aliases()
  lookup <- .spatialize_area_lookup()

  # The alias is only needed where regions.csv cannot match the raw code, and
  # it is only usable where it can match the target.
  expect_false(any(aliases$ne_iso3c %in% lookup$iso3c))
  expect_true(all(aliases$iso3c %in% lookup$iso3c))
  expect_false(anyDuplicated(aliases$ne_iso3c) > 0)
})


# The table is copied out of the polity registry, not chosen: each target is
# the present-day national polity whose `whep::polities` geometry holds the
# feature. Re-asserting it here means a registry change that moves one of them
# fails a test instead of leaving two crosswalks that disagree.
test_that("each alias agrees with the polity registry's geometry", {
  .need_spatialize_helper(".natural_earth_iso3c_aliases")
  testthat::skip_if_not_installed("sf")
  # One interior point per aliased Natural Earth feature.
  points <- tibble::tribble(
    ~ne_iso3c,    ~lon,    ~lat,
        "SOL",  44.060,   9.560,
        "CYN",  33.600,  35.250,
        "CNM",  33.963,  35.059,
        "ESB",  33.760,  35.026,
        "ALA",  20.100,  60.200,
        "KAS",  77.271,  35.418,
        "BRT",  33.701,  21.852,
        "SPI", -73.315, -49.519
  )
  expect_setequal(points$ne_iso3c, .natural_earth_iso3c_aliases()$ne_iso3c)

  old <- sf::sf_use_s2(FALSE)
  withr::defer(suppressMessages(sf::sf_use_s2(old)))
  registry <- whep::polities
  registry <- registry[
    registry$start_year <= 2020 &
      registry$end_year >= 2020 &
      registry$polity_type == "national",
    "iso3_code"
  ]
  located <- sf::st_as_sf(points, coords = c("lon", "lat"), crs = 4326) |>
    sf::st_join(registry) |>
    suppressMessages() |>
    sf::st_drop_geometry() |>
    dplyr::distinct(.data$ne_iso3c, .data$iso3_code)

  expected <- dplyr::select(.natural_earth_iso3c_aliases(), "ne_iso3c", "iso3c")
  expect_equal(
    dplyr::arrange(located, .data$ne_iso3c),
    dplyr::arrange(
      dplyr::rename(expected, iso3_code = "iso3c"),
      .data$ne_iso3c
    ),
    ignore_attr = TRUE
  )
})
