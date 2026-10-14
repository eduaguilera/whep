# The one crop placement the nitrogen and soil carbon chains share
# (R/crop_allocation.R, whep#1002). Offline: helper_crop_allocation.R.

.ca_grid <- function() {
  tibble::tribble(
    ~lon, ~lat,  ~area_code, ~cell_area_frac,
    0.25, 50.25, 10L,        1,
    0.75, 50.25, 10L,        1
  )
}

testthat::test_that("the allocation is the soil carbon inputs' own weights", {
  inputs <- .ca_inputs()
  alloc <- whep:::.crop_allocation(2010L, .ca_grid(), inputs)
  # The carbon chain's per-year weights, built by its own helpers.
  engine <- whep:::.sci_engine_crops(2010L, .ca_grid(), inputs$engine_inputs)
  carbon <- whep:::.sci_support_weights(
    whep:::.sci_spatialized_weights(engine),
    "fodder_pattern",
    list(country_grid = .ca_grid(), fodder_patterns = inputs$fodder_patterns)
  )
  key <- c("lon", "lat", "area_code", "item_prod_code")
  ours <- alloc$weights |>
    dplyr::transmute(
      lon,
      lat,
      area_code,
      item_prod_code = as.character(item_prod_code),
      area_weight
    ) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(key)))
  theirs <- carbon$weights |>
    dplyr::select(dplyr::all_of(key), "area_weight") |>
    dplyr::arrange(dplyr::across(dplyr::all_of(key)))
  testthat::expect_equal(ours, theirs, ignore_attr = TRUE)
  testthat::expect_equal(
    dplyr::arrange(alloc$fallback, lon)$cropland_weight,
    dplyr::arrange(carbon$fallback, lon)$cropland_weight
  )
  # The fodder layer places alfalfa, which the engine leaves out.
  testthat::expect_true(641L %in% alloc$weights$item_prod_code)
})

testthat::test_that("a supplied allocation is checked, not trusted", {
  bad <- .ca_allocation()
  bad$weights$area_weight[1] <- 0.5
  testthat::expect_error(
    whep:::.crop_allocation(2010L, NULL, list(crop_allocation = bad)),
    class = "whep_crop_allocation"
  )
  testthat::expect_error(
    whep:::.crop_allocation(
      2011L,
      NULL,
      list(crop_allocation = .ca_allocation())
    ),
    class = "whep_crop_allocation"
  )
})

testthat::test_that("synthetic N is placed by the shared allocation", {
  totals <- tibble::tribble(
    ~year, ~area_code, ~n_t,
    2010L, 10L,        100
  )
  shares <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~area_share,
    2010L, 10L,        2511L,          0.7,
    2010L, 10L,        2807L,          0.3
  )
  cell_polity <- tibble::tribble(
    ~lon, ~lat,  ~area_code, ~polity_frac, ~cell_area_ha,
    0.25, 50.25, 10L,        1,            3000,
    0.75, 50.25, 10L,        1,            3000
  )
  out <- whep::spatialize_country_n_to_crops(
    totals,
    shares,
    cell_polity,
    resolution = "grid",
    data = list(crop_allocation = .ca_allocation())
  )
  wheat <- dplyr::arrange(out[out$item_cbs_code == 2511L, ], lon)
  testthat::expect_equal(wheat$n_t, 70 * c(0.75, 0.25))
  rice <- out[out$item_cbs_code == 2807L, ]
  testthat::expect_equal(rice$lon, 0.75)
  testthat::expect_equal(rice$n_t, 30)
  testthat::expect_equal(sum(out$n_t), 100)
})

testthat::test_that("an item the allocation does not place takes the fallback", {
  totals <- tibble::tribble(
    ~year, ~area_code, ~n_t,
    2010L, 10L,        50
  )
  # Barley (2513, production item 44) is not in the allocation.
  shares <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~area_share,
    2010L, 10L,        2513L,          1
  )
  cell_polity <- tibble::tribble(
    ~lon, ~lat,  ~area_code, ~polity_frac, ~cell_area_ha,
    0.25, 50.25, 10L,        1,            3000,
    0.75, 50.25, 10L,        1,            3000
  )
  testthat::expect_warning(
    out <- whep::spatialize_country_n_to_crops(
      totals,
      shares,
      cell_polity,
      resolution = "grid",
      data = list(crop_allocation = .ca_allocation())
    ),
    "crop allocation"
  )
  testthat::expect_equal(dplyr::arrange(out, lon)$n_t, 50 * c(0.6, 0.4))
})

testthat::test_that("the crop NPP is spread by the allocation, totals kept", {
  npp <- tibble::tribble(
    ~year, ~area_code, ~item_prod_code, ~crop_npp_n_t, ~area_ha,
    2010L, 10L,        15L,             40,            600,
    2010L, 10L,        44L,             10,            100,
    2010L, 30L,        15L,             5,             50
  )
  out <- whep:::.n_grid_npp(npp, .ca_allocation())
  wheat <- dplyr::arrange(out[out$item_prod_code == 15L, ], lon)
  testthat::expect_equal(wheat$crop_npp_n_t, 40 * c(0.75, 0.25))
  testthat::expect_equal(wheat$area_ha, 600 * c(0.75, 0.25))
  # Barley has no cells of its own: the polity's cropland, 0.6 / 0.4.
  barley <- dplyr::arrange(out[out$item_prod_code == 44L, ], lon)
  testthat::expect_equal(barley$crop_npp_n_t, 10 * c(0.6, 0.4))
  testthat::expect_identical(unique(barley$item_cbs_code), 2513L)
  # Polity 30 has no cell at all: left out, for the caller's report.
  testthat::expect_false(30L %in% out$area_code)
  testthat::expect_identical(
    class(out$item_prod_code),
    class(npp$item_prod_code)
  )
})

testthat::test_that("the manure crop layer is spread by the allocation", {
  production <- tibble::tribble(
    ~year, ~area_code, ~item_prod_code, ~item_cbs_code, ~unit, ~value,
    ~live_anim_code,
    2010L, 10L,        15L,             2511L,          "ha",  600,
    NA
  )
  national <- whep:::.sci_manure_crop_layer(production)
  out <- whep:::.n_manure_crop_layer(
    "grid",
    production,
    .ca_grid(),
    .ca_allocation()
  )
  testthat::expect_equal(sum(out$crop_area_ha), sum(national$crop_area_ha))
  testthat::expect_equal(
    sort(out$crop_area_ha),
    sort(sum(national$crop_area_ha) * c(0.75, 0.25))
  )
})

testthat::test_that("the land support splits cropland by the allocation", {
  cell_polity <- tibble::tribble(
    ~lon, ~lat,  ~area_code, ~polity_frac,
    0.25, 50.25, 10L,        1,
    0.75, 50.25, 10L,        1
  )
  type_cropland <- tibble::tribble(
    ~lon, ~lat,  ~year, ~luh2_type, ~type_ha,
    0.25, 50.25, 2010L, "c3ann",    1000,
    0.75, 50.25, 2010L, "c3ann",    800
  )
  harvested <- tibble::tribble(
    ~area_code, ~item_prod_code, ~year, ~faostat_area_ha,
    10L,        15L,             2010L, 600,
    10L,        27L,             2010L, 300,
    10L,        44L,             2010L, 100
  )
  out <- whep::build_ag_land_support(
    years = 2010L,
    grassland = "none",
    data = list(
      cell_polity = cell_polity,
      type_cropland = type_cropland,
      crop_allocation = .ca_allocation(),
      harvested_area = harvested
    )
  )
  # Cell 0.25: wheat 450 ha, barley 60 ha (fallback 0.6 x 100); cell 0.75:
  # wheat 150, rice 300, barley 40. Each cell's physical cropland is split in
  # those proportions and never inflated.
  a <- out[out$lon == 0.25, ]
  testthat::expect_equal(sum(a$area_ha), 1000)
  testthat::expect_equal(
    a$area_ha[a$item_cbs_code == 2511L],
    1000 * 450 / 510
  )
  testthat::expect_equal(
    a$area_ha[a$item_cbs_code == 2513L],
    1000 * 60 / 510
  )
  b <- out[out$lon == 0.75, ]
  testthat::expect_equal(sum(b$area_ha), 800)
  testthat::expect_equal(b$area_ha[b$item_cbs_code == 2807L], 800 * 300 / 490)
})

testthat::test_that("national areas fold to the bucket only off the support", {
  areas <- tibble::tribble(
    ~year, ~area_code, ~item_prod_code, ~harvested_area_ha,
    2015L, 276L,       15L,             100,
    2015L, 277L,       15L,             40
  )
  bucket <- whep:::.cell_polity_bucket_lookup()
  folded_code <- bucket$polity_area_code[bucket$area_code == 276L]
  # The carbon path's support is keyed on the bucket: both fold onto it.
  folded <- whep:::.sci_areas_to_bucket(areas, folded_code)
  testthat::expect_identical(folded$area_code, folded_code)
  testthat::expect_equal(folded$harvested_area_ha, 140)
  # A support keyed on the polities' own codes meets their own areas.
  kept <- whep:::.sci_areas_to_bucket(areas, c(276L, 277L))
  testthat::expect_setequal(kept$area_code, c(276L, 277L))
  testthat::expect_equal(sum(kept$harvested_area_ha), 140)
})
