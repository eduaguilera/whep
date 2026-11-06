obs <- tibble::tibble(
  year = c(1950L, 1952L),
  reporting_polity_code = "PAK-WP-1949-1971",
  area_code = 165L,
  ha = c(10, 14),
  tonnes = c(100, 210),
  source = "historical_fixture",
  method_cane = "compiled",
  source_period = c("1949/50", "1951/52"),
  source_footprint = "declared test footprint",
  mass_basis = "fresh_cane"
)
land <- tibble::tibble(
  year = 1949:1953,
  reporting_polity_code = "PAK-WP-1949-1971",
  area_code = 165L,
  land_area_ha = seq(90, 130, 10),
  land_source = "supplied fixture"
)
test_that("source anchors stay exact and every target key has both quantities", {
  x <- whep:::.prepare_cane_history(obs, land, "source_anchored_land")
  expect_equal(nrow(x), 10L)
  expect_false(anyNA(x$value))
  expect_equal(x$value[x$unit == "tonnes"], c(90, 100, 155, 210, 227.5))
  expect_equal(x$value[x$unit == "ha"], c(9, 10, 12, 14, 14 * 130 / 120))
  expect_equal(unique(x$method_cane[x$year == 1951]), "linear_area_production")
  expect_equal(unique(x$source[x$year %in% obs$year]), "historical_fixture")
  expect_equal(unique(x$cane_extrapolation_years[x$year == 1949]), 1L)
  expect_equal(unique(x$cane_anchor_start[x$year == 1951]), 1950L)
  expect_equal(unique(x$cane_anchor_end[x$year == 1951]), 1952L)
})
test_that("alternatives are explicit and never silent fallbacks", {
  x <- whep:::.prepare_cane_history(obs, land, "source_anchored_constant")
  expect_equal(x$value[x$unit == "tonnes"], c(100, 100, 155, 210, 210))
  expect_error(
    whep:::.prepare_cane_history(obs, land, "source_only"),
    class = "whep_error_cane_coverage"
  )
  expect_error(whep:::.prepare_cane_history(obs, land, "guess"))
  expect_equal(
    nrow(whep:::.prepare_cane_history(
      obs,
      dplyr::filter(land, year %in% obs$year),
      "source_only"
    )),
    4L
  )
})
test_that("missing inputs and identities do not become zero or another territory", {
  expect_error(
    whep:::.prepare_cane_history(
      obs,
      dplyr::filter(land, year != 1950),
      "source_anchored_land"
    ),
    class = "whep_error_cane_land"
  )
  expect_error(
    whep:::.prepare_cane_history(
      obs,
      dplyr::mutate(land, land_source = ""),
      "source_anchored_land"
    ),
    class = "whep_error_cane_input"
  )
  expect_error(
    whep:::.prepare_cane_history(obs[0, ], land, "source_anchored_land"),
    class = "whep_error_cane_input"
  )
  expect_error(
    whep:::.prepare_cane_history(
      dplyr::bind_rows(obs, obs),
      land,
      "source_anchored_land"
    ),
    class = "whep_error_cane_keys"
  )
  other <- dplyr::mutate(
    land,
    reporting_polity_code = "IND-1949-2025",
    area_code = 100L
  )
  expect_error(
    whep:::.prepare_cane_history(
      obs,
      dplyr::bind_rows(land, other),
      "source_anchored_land"
    ),
    class = "whep_error_cane_coverage"
  )
  zero <- dplyr::mutate(
    land,
    land_area_ha = dplyr::if_else(year == 1950, 0, land_area_ha)
  )
  expect_error(
    whep:::.prepare_cane_history(obs, zero, "source_anchored_land"),
    class = "whep_error_cane_land"
  )
})

test_that("sugar mass cannot enter as fresh cane", {
  wrong <- dplyr::mutate(obs, mass_basis = "raw_sugar")
  expect_error(
    whep:::.prepare_cane_history(wrong, land),
    class = "whep_error_cane_basis"
  )
  wrong <- dplyr::mutate(obs, reporting_polity_code = "PAK-1949-1971")
  expect_error(
    whep:::.prepare_cane_history(wrong, land),
    class = "whep_error_historical_identity"
  )
})

test_that("nonfinite and fractional area keys are rejected before coercion", {
  for (bad in c(Inf, -Inf, 165.5)) {
    wrong_obs <- dplyr::mutate(obs, area_code = bad)
    wrong_land <- dplyr::mutate(land, area_code = bad)
    expect_error(
      whep:::.prepare_cane_history(wrong_obs, wrong_land),
      class = "whep_error_cane_keys"
    )
  }
})

test_that("ingestion checks values, units and identity, not just row count", {
  artifact <- whep:::.prepare_cane_history(obs, land)
  prepared <- whep:::.prepare_historical_production(artifact, land$year)
  expect_null(whep:::.check_cane_ingestion(artifact, prepared))
  wrong <- prepared
  wrong$value[1L] <- wrong$value[1L] + 1
  expect_error(
    whep:::.check_cane_ingestion(artifact, wrong),
    class = "whep_error_cane_ingestion"
  )
  wrong <- prepared
  wrong$unit[1L] <- "t"
  expect_error(
    whep:::.check_cane_ingestion(artifact, wrong),
    class = "whep_error_cane_ingestion"
  )
  wrong <- dplyr::mutate(prepared, area_code = 16L)
  expect_error(
    whep:::.check_cane_ingestion(artifact, wrong),
    class = "whep_error_cane_ingestion"
  )
})

test_that("malformed source quantities and provenance are rejected", {
  expect_error(
    whep:::.prepare_cane_history(dplyr::select(obs, -mass_basis), land),
    class = "whep_error_cane_schema"
  )
  expect_error(
    whep:::.prepare_cane_history(dplyr::mutate(obs, tonnes = "100"), land),
    class = "whep_error_cane_input"
  )
  expect_error(
    whep:::.prepare_cane_history(
      dplyr::mutate(obs, source = NA_character_),
      land
    ),
    class = "whep_error_cane_input"
  )
  expect_error(
    whep:::.prepare_cane_history(dplyr::mutate(obs, tonnes = Inf), land),
    class = "whep_error_cane_input"
  )
  expect_error(
    whep:::.prepare_cane_history(obs, dplyr::mutate(land, land_area_ha = -1)),
    class = "whep_error_cane_input"
  )
  expect_error(
    whep:::.prepare_cane_history(dplyr::mutate(obs, year = NA_real_), land),
    class = "whep_error_cane_keys"
  )
  expect_error(
    whep:::.prepare_cane_history(
      dplyr::mutate(obs, reporting_polity_code = NA_character_),
      land
    ),
    class = "whep_error_cane_keys"
  )
})
