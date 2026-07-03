# Periodized cell -> polity grassland aggregation --------------------------
# Exercises .aggregate_grassland_cells on a tiny synthetic grid so the logic
# (per-polity independent sums, same-entity period dedup, cross-entity
# overlap) is tested without touching the real gridded pasture pin.

agg_grassland <- getFromNamespace(".aggregate_grassland_cells", "whep")

# Two cells. Cell A sits inside two overlapping France periods (same entity)
# plus a distinct federation. Cell B sits inside only the federation.
grid_fixture <- function() {
  tibble::tribble(
    ~lon, ~lat, ~year, ~pasture_ha, ~rangeland_ha,
    10, 45, 1900, 100, 10,
    20, 15, 1900, 200, 20
  )
}

cells_fixture <- function() {
  tibble::tribble(
    ~lon, ~lat, ~polity_code, ~start_year, ~end_year,
    10, 45, "FRA-1800-1919", 1800, 1919, # same-entity, earlier start
    10, 45, "FRA-1871-1919", 1871, 1919, # same-entity, later start (winner)
    10, 45, "XAF-1895-1960", 1895, 1960, # distinct federation, overlaps FRA
    20, 15, "XAF-1895-1960", 1895, 1960
  )
}

testthat::test_that("same-entity overlapping periods are not double-counted", {
  result <- agg_grassland(grid_fixture(), cells_fixture())

  fra <- result[startsWith(result$polity_code, "FRA-"), ]
  # Only the later-starting France period keeps cell A.
  testthat::expect_setequal(unique(fra$polity_code), "FRA-1871-1919")
  pasture <- fra$value[fra$item_prod_code == 3001]
  testthat::expect_equal(pasture, 100)
})

testthat::test_that("distinct overlapping entities each keep their own land", {
  result <- agg_grassland(grid_fixture(), cells_fixture())

  # The federation spans both cells; France only cell A. Overlap on cell A is
  # kept for both entities (summed per polity independently).
  xaf_pasture <- result$value[
    result$polity_code == "XAF-1895-1960" & result$item_prod_code == 3001
  ]
  testthat::expect_equal(xaf_pasture, 300) # 100 (cell A) + 200 (cell B)
})

testthat::test_that("output schema is polity-keyed grassland", {
  result <- agg_grassland(grid_fixture(), cells_fixture())

  pointblank::expect_col_exists(
    result,
    columns = c(
      "year",
      "polity_code",
      "item_prod_code",
      "item_cbs_code",
      "unit",
      "value",
      "source"
    )
  )
  testthat::expect_true(all(result$item_cbs_code == 3000))
  testthat::expect_setequal(result$item_prod_code, c(3001, 3002))
  testthat::expect_true(all(result$unit == "ha"))
  testthat::expect_true(all(result$source == "LUH2_grassland"))
  testthat::expect_false(any(result$polity_code %in% c(NA, "")))
})

testthat::test_that("polity_grid_cells dataset is well-formed", {
  cells <- whep::polity_grid_cells

  testthat::expect_s3_class(cells, "tbl_df")
  pointblank::expect_col_exists(
    cells,
    columns = c("lon", "lat", "polity_code", "start_year", "end_year")
  )
  testthat::expect_false(any(is.na(cells$polity_code)))
  testthat::expect_true(all(cells$start_year <= cells$end_year))
})
