.polity_run_fixture <- function() {
  input <- tempfile("polity-inputs-")
  out <- tempfile("polity-output-")
  whep::prepare_polity_crop_inputs(
    .polity_preparation_data(),
    method = "luh2",
    output_dir = input
  )
  list(input_dir = input, out_dir = out)
}

testthat::test_that("native runner consumes prepared artifacts without external reads", {
  testthat::skip_if_not_installed("jsonlite")
  paths <- .polity_run_fixture()
  withr::defer(unlink(unlist(paths), recursive = TRUE))
  testthat::local_mocked_bindings(
    .load_country_grid = function(...) rlang::abort("Legacy grid loader used"),
    .load_polity_support = function(...) rlang::abort("Legacy support used"),
    .read_spatial_input = function(...) rlang::abort("Legacy input read used"),
    .package = "whep"
  )
  run <- whep::run_spatialize(
    "whep",
    years = 1900L,
    components = "landuse",
    overrides = list(geographic_key = "polity"),
    paths = paths
  )
  crops <- nanoparquet::read_parquet(run$output_paths$landuse_crops)
  testthat::expect_identical(unique(crops$polity_code), "AAA-1800-1950")
  testthat::expect_false("area_code" %in% names(crops))
  testthat::expect_equal(sum(crops$rainfed_ha + crops$irrigated_ha), 100)
  testthat::expect_equal(sum(crops$irrigated_ha), 20)
  testthat::expect_identical(unique(crops$source_area_code), "region")
  testthat::expect_identical(unique(crops$source), "fixture")
  testthat::expect_identical(unique(crops$territorial_basis), "source_reported")
  testthat::expect_identical(unique(crops$geographic_key), "polity")
  cft <- nanoparquet::read_parquet(run$output_paths$landuse_cft)
  testthat::expect_identical(unique(cft$polity_code), "AAA-1800-1950")
  testthat::expect_equal(sum(cft$rainfed_ha + cft$irrigated_ha), 100)
  testthat::expect_identical(
    unique(crops$method_irrigation),
    "luh2_proportional"
  )
  testthat::expect_identical(
    unique(crops$method_weight_rainfed),
    "crop_pattern"
  )
  testthat::expect_identical(
    unique(crops$method_weight_irrigated),
    "crop_pattern"
  )
  testthat::expect_identical(unique(cft$method_irrigation), "luh2_proportional")
  testthat::expect_identical(unique(cft$method_weight_rainfed), "crop_pattern")
  testthat::expect_identical(
    unique(cft$method_weight_irrigated),
    "crop_pattern"
  )
  metadata <- yaml::read_yaml(file.path(paths$out_dir, "run_metadata.yaml"))
  testthat::expect_identical(metadata$polity_inputs$geographic_key, "polity")
  testthat::expect_identical(
    metadata$polity_inputs$input_revisions$mapping,
    "fixture-mapping"
  )
  testthat::expect_true(file.exists(run$output_paths$polity_input_manifest))
})

testthat::test_that("native runner requires a manifest and checked artifacts", {
  testthat::skip_if_not_installed("jsonlite")
  paths <- .polity_run_fixture()
  withr::defer(unlink(unlist(paths), recursive = TRUE))
  config <- whep:::.resolve_spatialize_config(
    "whep",
    list(geographic_key = "polity")
  )
  manifest_path <- file.path(paths$input_dir, "polity_input_manifest.json")
  manifest <- jsonlite::read_json(manifest_path, simplifyVector = FALSE)
  manifest$input_revisions$support <- ""
  jsonlite::write_json(manifest, manifest_path, auto_unbox = TRUE)
  testthat::expect_error(
    whep:::.load_landuse_inputs(paths$input_dir, config),
    class = "whep_polity_input_manifest"
  )
  manifest$input_revisions$support <- "fixture-support"
  jsonlite::write_json(manifest, manifest_path, auto_unbox = TRUE)
  writeLines("not parquet", file.path(paths$input_dir, "crop_patterns.parquet"))
  testthat::expect_error(
    whep:::.load_landuse_inputs(paths$input_dir, config),
    class = "whep_polity_input_checksum"
  )
  unlink(manifest_path)
  testthat::expect_error(
    whep:::.load_landuse_inputs(paths$input_dir, config),
    class = "whep_polity_input_manifest"
  )
})

testthat::test_that("native runner rejects unsupported modes before loading", {
  testthat::expect_error(
    whep::run_spatialize(
      "whep",
      components = "livestock",
      overrides = list(geographic_key = "polity")
    ),
    class = "whep_polity_run_mode"
  )
  testthat::expect_error(
    whep::run_spatialize(
      "whep",
      components = "landuse",
      overrides = list(geographic_key = "polity")
    ),
    class = "whep_polity_run_mode"
  )
  for (override in list(
    list(level = 1L),
    list(output_level = 1L),
    list(area_key = "polity"),
    list(country_grid = "centroid"),
    list(grid_vintage = "snapshot_2015")
  )) {
    testthat::expect_error(
      whep::run_spatialize(
        "whep",
        components = "landuse",
        overrides = c(list(geographic_key = "polity"), override)
      ),
      class = "whep_polity_run_mode"
    )
  }
})

.rewrite_polity_run_file <- function(paths, name, data) {
  path <- file.path(paths$input_dir, name)
  nanoparquet::write_parquet(data, path)
  manifest_path <- file.path(paths$input_dir, "polity_input_manifest.json")
  manifest <- jsonlite::read_json(manifest_path, simplifyVector = FALSE)
  manifest$files[[name]] <- unname(tools::md5sum(path))
  jsonlite::write_json(manifest, manifest_path, auto_unbox = TRUE)
}

testthat::test_that("native loader requires supplied irrigation for positive targets", {
  testthat::skip_if_not_installed("jsonlite")
  paths <- .polity_run_fixture()
  withr::defer(unlink(unlist(paths), recursive = TRUE))
  config <- whep:::.resolve_spatialize_config(
    "whep",
    list(geographic_key = "polity")
  )
  water <- nanoparquet::read_parquet(file.path(
    paths$input_dir,
    "gridded_cropland.parquet"
  ))
  .rewrite_polity_run_file(
    paths,
    "gridded_cropland.parquet",
    dplyr::select(water, -"irrigated_ha")
  )
  testthat::expect_error(whep:::.load_landuse_inputs(paths$input_dir, config))
  water$irrigated_ha <- 0
  .rewrite_polity_run_file(paths, "gridded_cropland.parquet", water)
  testthat::expect_error(
    whep:::.load_landuse_inputs(paths$input_dir, config),
    class = "whep_polity_input_irrigation"
  )
})

testthat::test_that("native loader checks grid fractions even with valid checksums", {
  testthat::skip_if_not_installed("jsonlite")
  paths <- .polity_run_fixture()
  withr::defer(unlink(unlist(paths), recursive = TRUE))
  config <- whep:::.resolve_spatialize_config(
    "whep",
    list(geographic_key = "polity")
  )
  grid <- nanoparquet::read_parquet(file.path(
    paths$input_dir,
    "polity_country_grid.parquet"
  ))
  grid$cell_area_frac <- 0.5
  .rewrite_polity_run_file(paths, "polity_country_grid.parquet", grid)
  testthat::expect_error(
    whep:::.load_landuse_inputs(paths$input_dir, config),
    class = "whep_polity_input_schema"
  )
})

testthat::test_that("checksummed native targets require valid hectare measures", {
  testthat::skip_if_not_installed("jsonlite")
  paths <- .polity_run_fixture()
  withr::defer(unlink(unlist(paths), recursive = TRUE))
  config <- whep:::.resolve_spatialize_config(
    "whep",
    list(geographic_key = "polity")
  )
  targets <- nanoparquet::read_parquet(file.path(
    paths$input_dir,
    "polity_country_areas.parquet"
  ))
  invalid <- list(
    dplyr::mutate(targets, harvested_area_ha = NA_real_),
    dplyr::mutate(targets, harvested_area_ha = Inf),
    dplyr::mutate(targets, harvested_area_ha = -1),
    dplyr::mutate(targets, irrigated_area_ha = NA_real_),
    dplyr::mutate(targets, irrigated_area_ha = Inf),
    dplyr::mutate(targets, irrigated_area_ha = -1),
    dplyr::mutate(targets, irrigated_area_ha = harvested_area_ha + 1)
  )
  purrr::walk(invalid, function(rows) {
    .rewrite_polity_run_file(paths, "polity_country_areas.parquet", rows)
    testthat::expect_error(
      whep:::.load_landuse_inputs(paths$input_dir, config),
      class = "whep_polity_input_schema"
    )
  })
})

testthat::test_that("native target methods and intervals agree with provenance", {
  testthat::skip_if_not_installed("jsonlite")
  paths <- .polity_run_fixture()
  withr::defer(unlink(unlist(paths), recursive = TRUE))
  config <- whep:::.resolve_spatialize_config(
    "whep",
    list(geographic_key = "polity")
  )
  targets <- nanoparquet::read_parquet(file.path(
    paths$input_dir,
    "polity_country_areas.parquet"
  ))
  invalid <- list(
    dplyr::mutate(targets, method_irrigation = "mirca_scaled_luh2"),
    dplyr::mutate(targets, method_irrigation = NA_character_),
    dplyr::select(targets, -"start_year"),
    dplyr::select(targets, -"end_year"),
    dplyr::mutate(targets, start_year = NA_real_),
    dplyr::mutate(targets, end_year = Inf),
    dplyr::mutate(targets, start_year = 1950L),
    dplyr::mutate(targets, start_year = year + 1L),
    dplyr::mutate(targets, end_year = year)
  )
  purrr::walk(invalid, function(rows) {
    .rewrite_polity_run_file(paths, "polity_country_areas.parquet", rows)
    testthat::expect_error(
      whep:::.load_landuse_inputs(paths$input_dir, config),
      class = "whep_polity_input_schema"
    )
  })
})

testthat::test_that("native loader validates MIRCA method and inclusive starts", {
  testthat::skip_if_not_installed("jsonlite")
  paths <- .polity_run_fixture()
  withr::defer(unlink(unlist(paths), recursive = TRUE))
  whep::prepare_polity_crop_inputs(
    .polity_preparation_data(),
    method = "mirca",
    output_dir = paths$input_dir
  )
  config <- whep:::.resolve_spatialize_config(
    "whep",
    list(geographic_key = "polity")
  )
  targets <- nanoparquet::read_parquet(file.path(
    paths$input_dir,
    "polity_country_areas.parquet"
  ))
  targets$start_year <- targets$year
  .rewrite_polity_run_file(paths, "polity_country_areas.parquet", targets)
  loaded <- whep:::.load_landuse_inputs(paths$input_dir, config)
  testthat::expect_identical(
    unique(loaded$country_areas$method_irrigation),
    "mirca_scaled_luh2"
  )
  targets$method_irrigation <- "luh2_proportional"
  .rewrite_polity_run_file(paths, "polity_country_areas.parquet", targets)
  testthat::expect_error(
    whep:::.load_landuse_inputs(paths$input_dir, config),
    class = "whep_polity_input_schema"
  )
})
