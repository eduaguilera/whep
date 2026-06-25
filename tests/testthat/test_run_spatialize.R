testthat::test_that("lpjml preset disables type-aware allocation", {
  config <- getFromNamespace(".resolve_spatialize_config", "whep")(
    "lpjml",
    list()
  )
  testthat::expect_false(config$use_type_constraint)
  testthat::expect_true(config$aggregate_to_cft)
})

testthat::test_that("whep preset enables type-aware allocation", {
  config <- getFromNamespace(".resolve_spatialize_config", "whep")(
    "whep",
    list()
  )
  testthat::expect_true(config$use_type_constraint)
})

testthat::test_that("overrides take precedence over preset defaults", {
  config <- getFromNamespace(".resolve_spatialize_config", "whep")(
    "lpjml",
    list(use_type_constraint = TRUE, max_iterations = 50L)
  )
  testthat::expect_true(config$use_type_constraint)
  testthat::expect_equal(config$max_iterations, 50L)
})

testthat::test_that("unknown override keys are rejected", {
  testthat::expect_error(
    getFromNamespace(".validate_overrides", "whep")(
      list(not_a_real_flag = TRUE)
    ),
    "not_a_real_flag"
  )
})

testthat::test_that("unnamed overrides are rejected", {
  testthat::expect_error(
    getFromNamespace(".validate_overrides", "whep")(list(TRUE)),
    "named"
  )
})

testthat::test_that("lpjml default years intersect benchmark years with availability", {
  country_areas <- tibble::tribble(
      ~year, ~area_code, ~item_prod_code, ~harvested_area_ha,
      1995L,         1L,             15L,               500,
      2000L,         1L,             15L,              1000,
      2005L,         1L,             15L,              1100,
      2010L,         1L,             15L,              1200
    )
  picked <- getFromNamespace(".resolve_years", "whep")(
    years = NULL,
    preset = "lpjml",
    country_areas = country_areas
  )
  testthat::expect_setequal(picked, c(2000L, 2010L))
})

testthat::test_that("lpjml falls back to all available years if none of the samples match", {
  country_areas <- tibble::tribble(
      ~year, ~area_code, ~item_prod_code, ~harvested_area_ha,
      2021L,         1L,             15L,               500,
      2022L,         1L,             15L,               510
    )
  picked <- getFromNamespace(".resolve_years", "whep")(
    years = NULL,
    preset = "lpjml",
    country_areas = country_areas
  )
  testthat::expect_setequal(picked, c(2021L, 2022L))
})

testthat::test_that("whep default years use all available years", {
  country_areas <- tibble::tribble(
    ~year, ~area_code, ~item_prod_code, ~harvested_area_ha,
    1995L,         1L,             15L,               500,
    2000L,         1L,             15L,              1000,
    2005L,         1L,             15L,              1100
  )
  picked <- getFromNamespace(".resolve_years", "whep")(
    years = NULL,
    preset = "whep",
    country_areas = country_areas
  )
  testthat::expect_setequal(picked, c(1995L, 2000L, 2005L))
})

testthat::test_that("custom overrides produce a distinct default output directory", {
  fn <- getFromNamespace(".default_spatialize_out_dir", "whep")
  base <- fn("/tmp/l", "lpjml", list())
  custom <- fn("/tmp/l", "lpjml", list(use_type_constraint = TRUE))
  testthat::expect_false(base == custom)
  testthat::expect_match(custom, "_custom$")
})

testthat::test_that(".resolve_paths can run from pinned inputs without l_files_dir", {
  paths <- getFromNamespace(".resolve_paths", "whep")(
    list(),
    "whep",
    list()
  )
  testthat::expect_null(paths$l_files_dir)
  testthat::expect_null(paths$input_dir)
  testthat::expect_match(paths$out_dir, "whep_spatialize")
})

testthat::test_that("lpjml default years are the 10y benchmark sequence", {
  benchmarks <- getFromNamespace(".benchmark_years", "whep")()
  testthat::expect_setequal(
    benchmarks,
    seq(1850L, 2020L, by = 10L)
  )

  country_areas <- tibble::tibble(year = 1850L:2020L)
  picked <- getFromNamespace(".resolve_years", "whep")(
    years = NULL,
    preset = "lpjml",
    country_areas = country_areas
  )
  testthat::expect_setequal(picked, benchmarks)
})

testthat::test_that("unknown components are rejected", {
  testthat::expect_error(
    getFromNamespace(".validate_components", "whep")("soil"),
    "soil"
  )
})

testthat::test_that("empty components vector is rejected", {
  testthat::expect_error(
    getFromNamespace(".validate_components", "whep")(character()),
    "empty"
  )
})

testthat::test_that("components de-duplicate preserving known names", {
  fn <- getFromNamespace(".validate_components", "whep")
  testthat::expect_setequal(
    fn(c("landuse", "landuse", "livestock")),
    c("landuse", "livestock")
  )
})

testthat::test_that(".resolve_cft_target follows preset defaults", {
  fn <- getFromNamespace(".resolve_cft_target", "whep")
  testthat::expect_equal(fn(NULL, "whep"), "whep")
  testthat::expect_equal(fn(NULL, "lpjml"), "lpjml")
  testthat::expect_equal(fn("whep", "lpjml"), "whep")
  testthat::expect_equal(fn("lpjml", "whep"), "lpjml")
  testthat::expect_error(fn("bogus", "whep"))
})

testthat::test_that(".write_landuse_outputs aggregates by cft_target column", {
  result_crops <- tibble::tribble(
    ~lon,  ~lat,  ~year, ~item_prod_code, ~rainfed_ha, ~irrigated_ha,
     0.25, 50.25, 2000L,              15L,         100,             0,
     0.25, 50.25, 2000L,              56L,          50,             0,
     0.25, 50.25, 2000L,             267L,          30,             0
  )
  cft_mapping <- tibble::tribble(
    ~item_prod_code, ~cft_name,             ~cft_lpjml,
                15L, "temperate_cereals",   "temperate_cereals",
                56L, "maize",               "maize",
               267L, "oil_crops_sunflower", "oil_crops_sunflower"
  )
  # Add a coffee-like row: granular 'coffee' rolls up to LPJmL 'others'
  result_crops2 <- dplyr::bind_rows(
    result_crops,
    tibble::tibble(
      lon = 0.25,
      lat = 50.25,
      year = 2000L,
      item_prod_code = 656L,
      rainfed_ha = 10,
      irrigated_ha = 0
    )
  )
  cft_mapping2 <- dplyr::bind_rows(
    cft_mapping,
    tibble::tibble(
      item_prod_code = 656L,
      cft_name = "coffee",
      cft_lpjml = "others"
    )
  )

  fn <- getFromNamespace(".write_landuse_outputs", "whep")
  config <- list(aggregate_to_cft = TRUE)

  tmp_whep <- withr::local_tempdir()
  fn(result_crops2, cft_mapping2, tmp_whep, config, cft_target = "whep")
  whep_out <- nanoparquet::read_parquet(
    file.path(tmp_whep, "gridded_landuse.parquet")
  )
  testthat::expect_true("coffee" %in% whep_out$cft_name)

  tmp_lpjml <- withr::local_tempdir()
  fn(result_crops2, cft_mapping2, tmp_lpjml, config, cft_target = "lpjml")
  lpjml_out <- nanoparquet::read_parquet(
    file.path(tmp_lpjml, "gridded_landuse.parquet")
  )
  testthat::expect_false("coffee" %in% lpjml_out$cft_name)
  testthat::expect_true("others" %in% lpjml_out$cft_name)
})

testthat::test_that(".write_run_metadata writes a round-trippable YAML", {
  tmp <- withr::local_tempdir()
  fn <- getFromNamespace(".write_run_metadata", "whep")
  fn(
    out_dir = tmp,
    preset = "lpjml",
    years = c(1990L, 2000L, 2010L),
    components = c("landuse", "livestock"),
    cft_target = "lpjml",
    config = list(use_type_constraint = FALSE, aggregate_to_cft = TRUE),
    overrides = list(),
    input_dir = "/irrelevant"
  )
  meta <- yaml::read_yaml(file.path(tmp, "run_metadata.yaml"))
  testthat::expect_equal(meta$preset, "lpjml")
  testthat::expect_equal(meta$cft_target, "lpjml")
  testthat::expect_setequal(meta$components, c("landuse", "livestock"))
  testthat::expect_equal(meta$years, c(1990L, 2000L, 2010L))
  testthat::expect_false(meta$config$use_type_constraint)
})

testthat::test_that(".load_landuse_inputs reads pinned inputs when input_dir is NULL", {
  pins <- list(
    "spatialize-country-areas" = tibble::tibble(
      year = 2000L,
      area_code = 1L,
      item_prod_code = 15L,
      harvested_area_ha = 100
    ),
    "spatialize-crop-patterns" = tibble::tibble(
      lon = 0.25,
      lat = 50.25,
      item_prod_code = 15L,
      harvest_fraction = 1
    ),
    "spatialize-gridded-cropland" = tibble::tibble(
      lon = 0.25,
      lat = 50.25,
      year = 2000L,
      cropland_ha = 100
    ),
    "spatialize-country-grid" = tibble::tibble(
      lon = 0.25,
      lat = 50.25,
      area_code = 1L
    ),
    "spatialize-type-cropland" = tibble::tibble(
      lon = 0.25,
      lat = 50.25,
      year = 2000L,
      cropland_type = "cropland",
      cropland_ha = 100
    ),
    "spatialize-multicropping" = tibble::tibble(
      lon = 0.25,
      lat = 50.25,
      mc_rainfed = 1,
      mc_irrigated = 1
    )
  )
  testthat::local_mocked_bindings(
    whep_read_file = function(file_alias, ...) pins[[file_alias]],
    .package = "whep"
  )
  inputs <- getFromNamespace(".load_landuse_inputs", "whep")(
    NULL,
    list(use_type_constraint = TRUE)
  )
  testthat::expect_equal(inputs$input_dir, NULL)
  testthat::expect_equal(inputs$country_grid$area_code, 1L)
  testthat::expect_equal(inputs$multicropping$mc_rainfed, 1)
  testthat::expect_s3_class(inputs$type_cropland, "tbl_df")
})

# --- Livestock-only end-to-end path ------------------------------------
.write_livestock_fixture <- function(dir) {
  livestock_data <- tibble::tribble(
    ~year, ~area_code, ~species_group, ~heads, ~enteric_ch4_kt,
    2000L, 1L, "cattle", 10000, 1.0,
    2000L, 1L, "pigs",    5000, 0.0
  )
  gridded_pasture <- tibble::tribble(
    ~lon,  ~lat,  ~year, ~pasture_ha, ~rangeland_ha,
     0.25, 50.25, 2000L,         500,           100,
     0.75, 50.25, 2000L,         400,            80
  )
  gridded_cropland <- tibble::tribble(
    ~lon,  ~lat,  ~year, ~cropland_ha,
     0.25, 50.25, 2000L,          300,
     0.75, 50.25, 2000L,          200
  )
  country_grid <- tibble::tribble(
    ~lon,  ~lat, ~area_code,
     0.25, 50.25,         1L,
     0.75, 50.25,         1L
  )
  nanoparquet::write_parquet(
    livestock_data,
    file.path(dir, "livestock_country_data.parquet")
  )
  nanoparquet::write_parquet(
    gridded_pasture,
    file.path(dir, "gridded_pasture.parquet")
  )
  nanoparquet::write_parquet(
    gridded_cropland,
    file.path(dir, "gridded_cropland.parquet")
  )
  nanoparquet::write_parquet(
    country_grid,
    file.path(dir, "country_grid.parquet")
  )
}

testthat::test_that(".warn_if_out_dir_occupied fires when parquet files exist", {
  tmp <- withr::local_tempdir()
  file.create(file.path(tmp, "gridded_landuse.parquet"))
  fn <- getFromNamespace(".warn_if_out_dir_occupied", "whep")
  testthat::expect_warning(fn(tmp), "already contains")
  tmp2 <- withr::local_tempdir()
  testthat::expect_silent(fn(tmp2))
})

testthat::test_that("run_spatialize(components = 'livestock') writes only livestock outputs", {
  tmp_in <- withr::local_tempdir()
  .write_livestock_fixture(tmp_in)
  tmp_out <- withr::local_tempdir()

  result <- whep::run_spatialize(
    preset = "whep",
    years = 2000L,
    components = "livestock",
    paths = list(
      input_dir = tmp_in,
      out_dir = tmp_out,
      l_files_dir = tmp_in
    )
  )

  testthat::expect_equal(result$components, "livestock")
  testthat::expect_true(
    file.exists(file.path(tmp_out, "gridded_livestock_emissions.parquet"))
  )
  testthat::expect_false(
    file.exists(file.path(tmp_out, "gridded_landuse.parquet"))
  )
  testthat::expect_false(
    file.exists(file.path(tmp_out, "gridded_landuse_crops.parquet"))
  )

  meta <- yaml::read_yaml(file.path(tmp_out, "run_metadata.yaml"))
  testthat::expect_equal(meta$components, "livestock")
  testthat::expect_equal(meta$years, 2000L)
})
