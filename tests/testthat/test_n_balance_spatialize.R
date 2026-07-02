# Helper fixtures --------------------------------------------------------------

.nbs_country_totals <- function() {
  tibble::tribble(
    ~year, ~area_code, ~n_t,
    2010L, 10L, 100
  )
}

.nbs_crop_shares <- function() {
  tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~area_share,
    2010L, 10L, 2511L, 0.7, # wheat, item_prod_code 15
    2010L, 10L, 2807L, 0.3 # rice, item_prod_code 27
  )
}

.nbs_cell_polity <- function() {
  tibble::tribble(
    ~lon, ~lat, ~area_code, ~polity_frac, ~cell_area_ha,
    0.25, 50.25, 10L, 1, 3000,
    0.75, 50.25, 10L, 1, 3000
  )
}

.nbs_crop_patterns <- function() {
  tibble::tribble(
    ~lon, ~lat, ~item_prod_code, ~harvest_fraction,
    0.25, 50.25, 15L, 0.6, # wheat
    0.75, 50.25, 15L, 0.2,
    0.25, 50.25, 27L, 0.1, # rice
    0.75, 50.25, 27L, 0.3
  )
}

.nbs_type_cropland <- function() {
  tibble::tribble(
    ~lon, ~lat, ~year, ~luh2_type, ~type_ha, ~type_irrig_ha,
    0.25, 50.25, 2010L, "c3ann", 1000, 100,
    0.75, 50.25, 2010L, "c3ann", 1000, 50,
    0.25, 50.25, 2010L, "c4ann", 200, 0
  )
}

.nbs_grid_data <- function() {
  list(
    crop_patterns = .nbs_crop_patterns(),
    type_cropland = .nbs_type_cropland()
  )
}

# build_cell_polity --------------------------------------------------------

testthat::test_that("build_cell_polity aborts with no path or env var", {
  withr::local_envvar(WHEP_POLITY_FRACTION_PATH = "")
  testthat::expect_error(
    whep::build_cell_polity(),
    "WHEP_POLITY_FRACTION_PATH"
  )
})

testthat::test_that("build_cell_polity adds cell_area_ha from latitude", {
  path <- withr::local_tempfile(fileext = ".parquet")
  raw <- tibble::tribble(
    ~lon, ~lat, ~area_code, ~polity_frac,
    0.25, 50.25, 10L, 1,
    0.75, 50.25, 10L, 0.5
  )
  nanoparquet::write_parquet(raw, path)

  result <- whep::build_cell_polity(polity_fraction_path = path)

  pointblank::expect_col_exists(
    result,
    c("lon", "lat", "area_code", "polity_frac", "cell_area_ha")
  )
  testthat::expect_equal(nrow(result), 2L)
  testthat::expect_true(all(result$cell_area_ha > 0))
})

# spatialize_country_n_to_crops --------------------------------------------

testthat::test_that("polity_crop resolution splits totals by area share", {
  result <- whep::spatialize_country_n_to_crops(
    country_totals = .nbs_country_totals(),
    crop_shares = .nbs_crop_shares(),
    cell_polity = NULL,
    resolution = "polity_crop"
  )

  pointblank::expect_col_exists(
    result,
    c("year", "area_code", "item_cbs_code", "n_t")
  )
  testthat::expect_equal(sum(result$n_t), 100)
  wheat <- result$n_t[result$item_cbs_code == 2511L]
  rice <- result$n_t[result$item_cbs_code == 2807L]
  testthat::expect_equal(wheat, 70)
  testthat::expect_equal(rice, 30)
})

testthat::test_that("grid resolution conserves mass to the country total", {
  result <- whep::spatialize_country_n_to_crops(
    country_totals = .nbs_country_totals(),
    crop_shares = .nbs_crop_shares(),
    cell_polity = .nbs_cell_polity(),
    resolution = "grid",
    data = .nbs_grid_data()
  )

  pointblank::expect_col_exists(
    result,
    c("lon", "lat", "area_code", "year", "item_cbs_code", "n_t")
  )
  testthat::expect_equal(sum(result$n_t), sum(.nbs_country_totals()$n_t))
})

testthat::test_that("grid cells split within a crop by crop-pattern hectares", {
  result <- whep::spatialize_country_n_to_crops(
    country_totals = .nbs_country_totals(),
    crop_shares = .nbs_crop_shares(),
    cell_polity = .nbs_cell_polity(),
    resolution = "grid",
    data = .nbs_grid_data()
  )

  wheat <- result[result$item_cbs_code == 2511L, ]
  # wheat crop_pattern_ha: cell1 = 1200*0.6=720, cell2 = 1000*0.2=200
  # cell1 share = 720/920, cell2 share = 200/920, of the 70 t wheat total
  wheat_cell1 <- wheat$n_t[wheat$lon == 0.25]
  wheat_cell2 <- wheat$n_t[wheat$lon == 0.75]
  testthat::expect_equal(wheat_cell1, 70 * 720 / 920)
  testthat::expect_equal(wheat_cell2, 70 * 200 / 920)
  testthat::expect_equal(sum(wheat$n_t), 70)
})

testthat::test_that("grid resolution requires cell_polity", {
  testthat::expect_error(
    whep::spatialize_country_n_to_crops(
      country_totals = .nbs_country_totals(),
      crop_shares = .nbs_crop_shares(),
      cell_polity = NULL,
      resolution = "grid",
      data = .nbs_grid_data()
    ),
    "cell_polity"
  )
})

testthat::test_that("missing required columns abort with a clear message", {
  testthat::expect_error(
    whep::spatialize_country_n_to_crops(
      country_totals = dplyr::select(.nbs_country_totals(), -n_t),
      crop_shares = .nbs_crop_shares(),
      cell_polity = NULL,
      resolution = "polity_crop"
    ),
    "country_totals"
  )
})
