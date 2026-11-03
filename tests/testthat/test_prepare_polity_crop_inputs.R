testthat::test_that("native preparation exposes irrigation method explicitly", {
  data <- .polity_preparation_data()
  luh2 <- whep::prepare_polity_crop_inputs(data, method = "luh2")
  mirca <- whep::prepare_polity_crop_inputs(data, method = "mirca")
  testthat::expect_equal(luh2$country_areas$irrigated_area_ha, c(6, 14))
  testthat::expect_equal(
    mirca$country_areas$irrigated_area_ha,
    c(15, 7) * 20 / 22
  )
  testthat::expect_equal(mirca$country_areas$harvested_area_ha, c(30, 70))
  testthat::expect_equal(mirca$gridded_cropland$irrigated_ha, 20)
  testthat::expect_equal(mirca$manifest$input_revisions, data$input_revisions)
  testthat::expect_equal(mirca$manifest$geographic_key, "polity")
})

testthat::test_that("native preparation writes a distinct checked artifact", {
  testthat::skip_if_not_installed("nanoparquet")
  out_dir <- withr::local_tempdir()
  out <- whep::prepare_polity_crop_inputs(
    .polity_preparation_data(),
    method = "luh2",
    output_dir = out_dir
  )
  path <- file.path(out_dir, "polity_country_areas.parquet")
  testthat::expect_true(file.exists(path))
  testthat::expect_equal(
    tibble::as_tibble(nanoparquet::read_parquet(path)),
    out$country_areas
  )
  manifest <- jsonlite::read_json(
    file.path(out_dir, "polity_input_manifest.json"),
    simplifyVector = TRUE
  )
  testthat::expect_equal(
    unname(tools::md5sum(path)),
    unname(manifest$files[["polity_country_areas.parquet"]])
  )
})

testthat::test_that("legacy numeric metadata cannot become native keys", {
  data <- .polity_preparation_data()
  data$production$area_code <- 999L
  data$production$polity_area_code <- 999L
  out <- whep::prepare_polity_crop_inputs(data, method = "luh2")
  testthat::expect_false("area_code" %in% names(out$country_areas))
  testthat::expect_false("polity_area_code" %in% names(out$country_areas))
  testthat::expect_equal(out$country_areas$input_area_code, c(999L, 999L))
  testthat::expect_equal(
    out$country_areas$input_polity_area_code,
    c(999L, 999L)
  )
})

testthat::test_that("native preparation cannot silently change methods", {
  data <- .polity_preparation_data()
  data$mirca <- NULL
  testthat::expect_error(
    whep::prepare_polity_crop_inputs(data),
    class = "whep_polity_preparation"
  )
  data <- .polity_preparation_data()
  data$input_revisions$support <- NULL
  testthat::expect_error(
    whep::prepare_polity_crop_inputs(data),
    class = "whep_polity_preparation"
  )
  data <- .polity_preparation_data()
  data$mirca <- data$mirca[1, ]
  testthat::expect_error(
    whep::prepare_polity_crop_inputs(data),
    class = "whep_polity_preparation"
  )
})

testthat::test_that("raw FAOSTAT is mapped before bucket aggregation", {
  data <- .polity_preparation_data()
  data$production <- tibble::tribble(
    ~`Area Code`, ~Year, ~`Item Code`, ~Element, ~Unit, ~Value, ~Flag,
    999L, 1900L, 15L, "Area harvested", "ha", 30, "A",
    999L, 1900L, 56L, "Area harvested", "ha", 70, "E",
    999L, 1900L, 15L, "Production", "t", 200, "A"
  )
  data$mapping <- tibble::tribble(
    ~area_code, ~year_start, ~year_end, ~polity_code,
    999L, 1900L, 1900L, "AAA-1800-1950"
  )
  out <- whep::prepare_polity_crop_inputs(
    data,
    method = "luh2",
    source_format = "faostat"
  )
  testthat::expect_equal(out$country_areas$harvested_area_ha, c(30, 70))
  testthat::expect_equal(out$country_areas$fao_flag, c("A", "E"))
  testthat::expect_equal(out$country_areas$source_area_code, c("999", "999"))
  testthat::expect_equal(out$country_areas$end_year, c(1901, 1901))
  testthat::expect_equal(out$manifest$source_format, "faostat")
  data$mapping$year_end <- 1899L
  testthat::expect_error(whep::prepare_polity_crop_inputs(
    data,
    method = "luh2",
    source_format = "faostat"
  ))
})

testthat::test_that("native preparation rejects a target with only foreign patterns", {
  data <- .polity_preparation_data()
  data$crop_patterns$lon[data$crop_patterns$item_prod_code == 56L] <- 10.25
  testthat::expect_error(
    whep::prepare_polity_crop_inputs(data, method = "luh2"),
    class = "whep_polity_preparation"
  )
})

testthat::test_that("native pattern coverage respects the reporting year", {
  data <- .polity_preparation_data()
  data$support <- dplyr::bind_rows(
    dplyr::mutate(data$support, end_year = 1900L),
    dplyr::mutate(data$support, start_year = 1900L, lon = 10.25)
  )
  data$luh2$lon <- 10.25
  testthat::expect_error(
    whep::prepare_polity_crop_inputs(data, method = "luh2"),
    class = "whep_polity_preparation"
  )
  data$crop_patterns$lon <- 10.25
  testthat::expect_no_error(
    whep::prepare_polity_crop_inputs(data, method = "luh2")
  )
})

testthat::test_that("zero targets do not require fabricated pattern rows", {
  data <- .polity_preparation_data()
  data$production$harvested_area_ha[2] <- 0
  data$crop_patterns <- data$crop_patterns[1, ]
  out <- whep::prepare_polity_crop_inputs(data, method = "luh2")
  testthat::expect_equal(out$country_areas$harvested_area_ha, c(30, 0))
})

testthat::test_that("prepared input validation catches foreign-only patterns", {
  inputs <- whep::prepare_polity_crop_inputs(
    .polity_preparation_data(),
    method = "luh2"
  )
  inputs$crop_patterns$lon[2] <- 10.25
  testthat::expect_error(
    whep:::.check_native_prepared_tables(inputs, inputs$manifest),
    class = "whep_polity_preparation"
  )
})

testthat::test_that("missing MIRCA basis is not a rainfed zero", {
  x <- .polity_preparation_data()
  x$mirca$irrig_ha[1] <- 0
  x$mirca$rainfed_ha[1] <- 0
  testthat::expect_error(
    whep::prepare_polity_crop_inputs(x),
    class = "whep_polity_preparation"
  )
})
testthat::test_that("zero MIRCA weights distinguish supplied zero irrigation", {
  x <- .polity_preparation_data()
  x$mirca$irrig_ha[] <- 0
  testthat::expect_error(
    whep::prepare_polity_crop_inputs(x),
    class = "whep_polity_preparation"
  )
  x$luh2$irrig_ha[] <- 0
  out <- whep::prepare_polity_crop_inputs(x)
  testthat::expect_equal(out$country_areas$irrigated_area_ha, c(0, 0))
  testthat::expect_true(all(is.finite(out$country_areas$irrigated_area_ha)))
})
testthat::test_that("harvested-area cap exposes unallocated physical water", {
  x <- .polity_preparation_data()
  x$luh2$irrig_ha[] <- 100
  out <- whep::prepare_polity_crop_inputs(x)
  testthat::expect_equal(
    out$country_areas$irrigated_area_ha,
    c(30, 7 * 100 / 22)
  )
  testthat::expect_lt(sum(out$country_areas$irrigated_area_ha), 100)
})
testthat::test_that("absent physical year cannot borrow another year", {
  x <- .polity_preparation_data()
  x$luh2$year[] <- 1901L
  testthat::expect_error(
    whep::prepare_polity_crop_inputs(x),
    class = "whep_polity_preparation"
  )
})
