.source_target_fixture <- function() {
  list(
    production = tibble::tribble(
      ~source, ~source_area_code, ~year, ~item_prod_code,
      ~harvested_area_ha, ~territorial_basis,
      "archive", "region-a", 1900L, 15L, 30, "source_reported",
      "archive", "region-b", 1900L, 15L, 70, "source_reported"
    ),
    mapping = tibble::tribble(
      ~source, ~source_area_code, ~start_year, ~end_year, ~polity_code,
      "archive", "region-a", 1800L, 1950L, "AAA-1800-1950",
      "archive", "region-b", 1800L, 1950L, "BBB-1800-1950"
    )
  )
}

.resolve_source_fixture <- function(fix) {
  whep:::.polity_source_targets(fix$production, fix$mapping)
}

testthat::test_that("source targets reach crop allocation on exact polities", {
  data <- .source_target_fixture()
  data$support <- tibble::tribble(
    ~lon, ~lat, ~polity_code, ~start_year, ~end_year,
    ~cell_area_ha, ~land_area_ha,
    0.25, 0.25, "AAA-1800-1950", 1800L, 1950L, 100, 100,
    0.75, 0.25, "BBB-1800-1950", 1800L, 1950L, 100, 100
  )
  data$luh2 <- tibble::tribble(
    ~year, ~lon, ~lat, ~luh2_type, ~crop_ha, ~irrig_ha,
    1900L, 0.25, 0.25, "c3ann", 100, 10,
    1900L, 0.75, 0.25, "c3ann", 100, 20
  )
  data$type_mapping <- tibble::tribble(
    ~item_prod_code, ~luh2_type,
    15L, "c3ann"
  )
  prepared <- whep:::.prepare_polity_luh2(data)
  testthat::expect_equal(prepared$harvested_area_ha, c(30, 70))
  testthat::expect_equal(prepared$irrigated_area_ha, c(10, 20))
  testthat::expect_equal(prepared$source_area_code, c("region-a", "region-b"))
  out <- whep::build_gridded_landuse(
    prepared,
    tibble::tribble(
      ~lon, ~lat, ~item_prod_code, ~harvest_fraction,
      0.25, 0.25, 15L, 1,
      0.75, 0.25, 15L, 1
    ),
    dplyr::transmute(
      data$luh2,
      lon,
      lat,
      year,
      cropland_ha = crop_ha,
      irrigated_ha = irrig_ha
    ),
    whep:::.polity_grid_year_aware(data$support)
  )
  testthat::expect_equal(out$polity_code, prepared$polity_code)
  testthat::expect_equal(out$rainfed_ha + out$irrigated_ha, c(30, 70))
})

testthat::test_that("source territories resolve before any numeric fold", {
  fix <- .source_target_fixture()
  fix$production$area_code <- 999L
  out <- .resolve_source_fixture(fix)
  testthat::expect_equal(out$polity_code, fix$mapping$polity_code)
  testthat::expect_equal(out[names(fix$production)], fix$production)
  testthat::expect_equal(out$harvested_area_ha, c(30, 70))
})

testthat::test_that("source mappings respect source and exclusive end year", {
  fix <- .source_target_fixture()
  fix$production$year[1] <- 1950L
  testthat::expect_error(
    .resolve_source_fixture(fix),
    class = "whep_source_territory"
  )
  fix <- .source_target_fixture()
  fix$mapping$source[1] <- "another_archive"
  testthat::expect_error(
    .resolve_source_fixture(fix),
    class = "whep_source_territory"
  )
})

testthat::test_that("ambiguous maps and competing observations remain visible", {
  fix <- .source_target_fixture()
  fix$mapping <- dplyr::bind_rows(fix$mapping, fix$mapping[1, ])
  testthat::expect_error(
    .resolve_source_fixture(fix),
    class = "whep_source_territory"
  )
  fix <- .source_target_fixture()
  fix$mapping$polity_code[2] <- fix$mapping$polity_code[1]
  testthat::expect_error(
    .resolve_source_fixture(fix),
    class = "whep_source_territory"
  )
})

testthat::test_that("anchored or invalid observations cannot be relabelled", {
  fix <- .source_target_fixture()
  fix$production$territorial_basis[1] <- "backcast_anchor"
  testthat::expect_error(
    .resolve_source_fixture(fix),
    class = "whep_source_territory"
  )
  fix <- .source_target_fixture()
  fix$production$year[1] <- NA_integer_
  testthat::expect_error(
    .resolve_source_fixture(fix),
    class = "whep_source_territory"
  )
  fix <- .source_target_fixture()
  fix$production$harvested_area_ha[1] <- NA_real_
  testthat::expect_error(
    .resolve_source_fixture(fix),
    class = "whep_source_territory"
  )
})
