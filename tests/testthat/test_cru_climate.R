test_that("read_cru_climate example returns the tidy schema", {
  out <- whep::read_cru_climate(example = TRUE)
  pointblank::expect_col_exists(
    out,
    columns = c("lon", "lat", "year", "month", "value", "var")
  )
  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_gt(nrow(out), 0)
  testthat::expect_true(all(out$month %in% 1:12))
})

test_that("read_cru_climate uses injected data and returns tidy schema", {
  injected <- tibble::tribble(
    ~lon, ~lat, ~year, ~month, ~value,
    -0.25, 51.75, 2000L, 1L, 4.2,
    -0.25, 51.75, 2000L, 7L, 17.8
  )
  out <- whep::read_cru_climate(var = "tmp", data = injected)
  pointblank::expect_col_exists(
    out,
    columns = c("lon", "lat", "year", "month", "value", "var")
  )
  testthat::expect_equal(nrow(out), 2)
  testthat::expect_true(all(out$var == "tmp"))
  testthat::expect_equal(sort(out$value), c(4.2, 17.8))
})

test_that("read_cru_climate filters injected data by year", {
  injected <- tibble::tribble(
    ~lon, ~lat, ~year, ~month, ~value,
    -0.25, 51.75, 1999L, 1L, 3.1,
    -0.25, 51.75, 2000L, 1L, 4.2
  )
  out <- whep::read_cru_climate(var = "pre", years = 2000, data = injected)
  testthat::expect_equal(nrow(out), 1)
  testthat::expect_equal(out$year, 2000L)
})

test_that("read_cru_climate reads a real CRU file (smoke)", {
  cru_dir <- Sys.getenv("WHEP_CRU_DIR", "C:/XL_files/CRU/CRU_TS_4")
  testthat::skip_if(!dir.exists(cru_dir))
  out <- whep::read_cru_climate(var = "tmp", years = 2000, cru_dir = cru_dir)
  pointblank::expect_col_exists(
    out,
    columns = c("lon", "lat", "year", "month", "value", "var")
  )
  testthat::expect_true(all(out$lon >= -180 & out$lon <= 180))
  testthat::expect_true(all(out$lat >= -90 & out$lat <= 90))
  testthat::expect_true(all(out$month %in% 1:12))
  testthat::expect_true(all(out$year == 2000L))
  testthat::expect_true(all(is.finite(out$value)))
  mean_temp <- mean(out$value)
  testthat::expect_gt(mean_temp, -40)
  testthat::expect_lt(mean_temp, 40)
})
