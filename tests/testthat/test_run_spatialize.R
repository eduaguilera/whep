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

testthat::test_that(".write_landuse_outputs aborts on a repeated cft_mapping code", {
  result_crops <- tibble::tribble(
      ~lon,  ~lat,  ~year, ~item_prod_code, ~rainfed_ha, ~irrigated_ha,
      0.25, 50.25, 2000L,              15L,         100,             0
    )
  dup_mapping <- tibble::tribble(
      ~item_prod_code, ~cft_name,             ~cft_lpjml,
                  15L, "temperate_cereals",   "temperate_cereals",
                  15L, "other_dup",           "other_dup"
    )
  fn <- getFromNamespace(".write_landuse_outputs", "whep")

  testthat::expect_error(
    fn(
      result_crops,
      dup_mapping,
      withr::local_tempdir(),
      list(aggregate_to_cft = TRUE),
      cft_target = "whep"
    ),
    class = "rlang_error"
  )
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
  # `country_grid` is named explicitly: the default is the polycell support,
  # which is a pin, and what this test exercises is the OTHER pins' wiring.
  inputs <- getFromNamespace(".load_landuse_inputs", "whep")(
    NULL,
    list(use_type_constraint = TRUE, country_grid = "centroid")
  )
  testthat::expect_equal(inputs$input_dir, NULL)
  testthat::expect_equal(inputs$country_grid$area_code, 1L)
  testthat::expect_equal(inputs$multicropping$mc_rainfed, 1)
  testthat::expect_s3_class(inputs$type_cropland, "tbl_df")
})

# --- Livestock-only end-to-end path ------------------------------------
.write_livestock_fixture <- function(dir) {
  livestock_data <- tibble::tribble(
    # `species_group` speaks the vocabulary of
    # `inst/extdata/livestock_mapping.csv`, which is what
    # `.read_livestock_mapping()` hands the engine: `cattle_dairy` and
    # `cattle_non_dairy`, never a bare `cattle`. Until whep#1000 T15a-i an
    # unmapped group was silently given the pasture proxy, so this fixture
    # passed while naming a group the shipped mapping does not have.
    ~year, ~area_code, ~species_group, ~heads, ~enteric_ch4_kt,
    2000L,         1L, "cattle_dairy",  10000,             1.0,
    2000L,         1L,         "pigs",   5000,             0.0
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
    ~lon,  ~lat, ~area_code, ~cell_area_frac,
     0.25, 50.25,         1L,               1,
     0.75, 50.25,         1L,               1
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
    # The fixture is a centroid grid, and the default crosswalk is now the
    # polycell support pin. What this test asserts is which OUTPUTS a
    # components selection writes, so it pins the crosswalk rather than
    # depending on one being published.
    overrides = list(country_grid = "centroid"),
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

# --- The legacy inst/scripts runner -------------------------------------
#
# `inst/scripts/run_spatialize.R` is superseded by `run_spatialize()` but is
# still documented (docs/SPATIALIZATION.md names its outputs) and is neither
# linted (`.lintr` excludes `inst/scripts`) nor executed by any test, so the
# two hazards C8 removed from it can be reintroduced with nothing to notice.
# AM-5 risk 16 (re-attaching `country_grid` on (lon, lat), which under a
# polycell grid is many-to-many in front of a join that already declares
# "many-to-many") and risk 17 (a hand-copied compartment key drifting from
# `.compartment_id_cols()`) are therefore pinned statically. Each check carries
# a positive control, so a passing test means the pattern is absent and not
# that the pattern never matched anything.
.legacy_runner_source <- function() {
  path <- system.file("scripts", "run_spatialize.R", package = "whep")
  if (!nzchar(path)) {
    path <- testthat::test_path(
      "..",
      "..",
      "inst",
      "scripts",
      "run_spatialize.R"
    )
  }
  if (!file.exists(path)) {
    return(NULL)
  }
  paste(readLines(path, warn = FALSE), collapse = "\n")
}

testthat::test_that("the legacy runner keys on the compartment helper", {
  src <- .legacy_runner_source()
  testthat::skip_if(is.null(src), "inst/scripts/run_spatialize.R absent")

  drifted <- 'intersect(c("polycell_id", "cell_id"), names(x))'
  pattern <- 'intersect\\([[:space:]]*c\\("polycell_id"'
  testthat::expect_match(drifted, pattern)
  testthat::expect_no_match(src, pattern)
  testthat::expect_match(src, "whep:::[.]compartment_id_cols\\(")
})

testthat::test_that("the legacy runner never re-attaches the grid on lon/lat", {
  src <- .legacy_runner_source()
  testthat::skip_if(is.null(src), "inst/scripts/run_spatialize.R absent")

  reattach <- 'dplyr::inner_join(result_crops, country_grid, by = "lon")'
  pattern <- "inner_join\\(result_crops, country_grid"
  testthat::expect_match(reattach, pattern)
  testthat::expect_no_match(src, pattern)
  # The engine's own key is used directly, and the script asserts it rather
  # than silently rebuilding it from the grid.
  testthat::expect_match(
    src,
    'stopifnot\\("area_code" %in% names\\(result_crops\\)\\)'
  )
})

testthat::test_that("the legacy runner calls the engine with its real signature", {
  src <- .legacy_runner_source()
  testthat::skip_if(is.null(src), "inst/scripts/run_spatialize.R absent")

  # Until C8 the script passed `cft_mapping`/`type_cropland`/`type_mapping` as
  # bare arguments, which `build_gridded_landuse()` has not accepted since the
  # `config` list was introduced: an "unused arguments" error on the first call
  # of the run. Nothing caught it, because the script is untested.
  engine_args <- names(formals(whep::build_gridded_landuse))
  testthat::expect_true("config" %in% engine_args)
  testthat::expect_false("cft_mapping" %in% engine_args)
  stale <- "country_grid = country_grid,\n  cft_mapping = NULL"
  pattern <- "country_grid = country_grid,[[:space:]]*cft_mapping"
  testthat::expect_match(stale, pattern)
  testthat::expect_no_match(src, pattern)
  testthat::expect_match(src, "config = list\\(")
})

# --- Selecting the cell-to-polity crosswalk (whep#461) ------------------

.write_fraction_grid <- function(dir) {
  nanoparquet::write_parquet(
    tibble::tribble(
      ~lon,  ~lat, ~area_code, ~polity_frac,
      0.25, 50.25,         1L,          0.6,
      0.25, 50.25,         2L,          0.4,
      0.75, 50.25,         1L,          1.0
    ),
    file.path(dir, "cell_polity_fraction.parquet")
  )
}

testthat::test_that("the default crosswalk is the polycell support", {
  # The default moved off the centroid grid with the polycell epic: a grid
  # carrying no polity share is refused outright now, so defaulting to one
  # would make an unparameterised run abort. Asserted through the resolver
  # rather than by reading the support, which is a pin this test must not need.
  tmp <- withr::local_tempdir()
  .write_livestock_fixture(tmp)
  .write_fraction_grid(tmp)
  fn <- getFromNamespace(".load_country_grid", "whep")

  testthat::expect_equal(
    formals(fn)$source,
    NULL
  )
  # `NULL` must resolve to "polycell", which reads the SUPPORT rather than the
  # centroid parquet sitting in `tmp`. Asserted by standing a marker in front
  # of the support reader: an assertion that merely expected an error when the
  # pin was unpublished stopped testing anything the moment it was published.
  #
  # whep#1000 T39 added a second vintage, `"year_aware"`, which reads the
  # support through `read_polycell_support()` rather than through
  # `.carbon_cell_support()`'s 2015 fold, so BOTH are stood in front of. With
  # only the second mocked this test read the live pin -- passing here and
  # breaking the offline-tests job.
  testthat::local_mocked_bindings(
    read_polycell_support = function(...) {
      tibble::tibble(
        lon = 0.25,
        lat = 50.25,
        area_code = 999L,
        start_year = 1800L,
        end_year = 2100L,
        cell_area_ha = 1,
        land_area_ha = 1
      )
    },
    .carbon_cell_support = function(...) {
      tibble::tibble(
        lon = 0.25,
        lat = 50.25,
        area_code = 888L,
        cell_area_ha = 1,
        land_area_ha = 1,
        cell_area_frac = 1
      )
    },
    .package = "whep"
  )

  grid <- fn(tmp, NULL)

  testthat::expect_setequal(grid$area_code, 888L)
  testthat::expect_true(rlang::has_name(grid, "cell_area_frac"))

  # The other vintage reads the support without the 2015 fold. Both markers
  # stand so this test says which branch ran, not merely that one did.
  aware <- fn(tmp, NULL, 0L, "year_aware")
  testthat::expect_setequal(aware$area_code, 999L)
})

testthat::test_that("country_grid = 'centroid' still loads the centroid grid", {
  tmp <- withr::local_tempdir()
  .write_livestock_fixture(tmp)
  .write_fraction_grid(tmp)
  fn <- getFromNamespace(".load_country_grid", "whep")

  grid <- fn(tmp, "centroid")

  testthat::expect_false(rlang::has_name(grid, "polity_frac"))
  testthat::expect_setequal(grid$area_code, 1L)
})

testthat::test_that("country_grid = 'fraction' loads the fractional crosswalk", {
  tmp <- withr::local_tempdir()
  .write_livestock_fixture(tmp)
  .write_fraction_grid(tmp)
  fn <- getFromNamespace(".load_country_grid", "whep")

  grid <- fn(tmp, "fraction")

  testthat::expect_true(rlang::has_name(grid, "polity_frac"))
  testthat::expect_setequal(grid$area_code, c(1L, 2L))
  testthat::expect_equal(sum(grid$polity_frac), 2)
})

testthat::test_that("an unknown country_grid source is rejected", {
  fn <- getFromNamespace(".load_country_grid", "whep")

  # `"polycell"` used to be the unknown value here; it is now one of the three
  # accepted sources and the default, so the check needs a name that is still
  # genuinely unknown or it passes vacuously.
  testthat::expect_error(fn(NULL, "not_a_crosswalk"), class = "rlang_error")
})

testthat::test_that("a fractional run does not silently read another dir", {
  # `input_dir` was asked for; falling back to WHEP_POLITY_FRACTION_PATH here
  # would mix one directory's inputs with another's.
  tmp <- withr::local_tempdir()
  .write_livestock_fixture(tmp)
  fn <- getFromNamespace(".load_country_grid", "whep")

  testthat::expect_error(
    fn(tmp, "fraction"),
    "cell_polity_fraction"
  )
})

testthat::test_that("country_grid is a recognised override and is recorded", {
  tmp_in <- withr::local_tempdir()
  .write_livestock_fixture(tmp_in)
  .write_fraction_grid(tmp_in)
  # Area 2 exists in the fractional crosswalk only, as a share of the cell
  # the centroid grid gives whole to area 1.
  nanoparquet::write_parquet(
    tibble::tribble(
      ~year, ~area_code, ~species_group, ~heads, ~enteric_ch4_kt,
      2000L,         1L, "cattle_dairy",  10000,             1.0,
      2000L,         2L, "cattle_dairy",   2000,             0.2
    ),
    file.path(tmp_in, "livestock_country_data.parquet")
  )
  tmp_out <- withr::local_tempdir()

  result <- whep::run_spatialize(
    preset = "whep",
    years = 2000L,
    components = "livestock",
    overrides = list(country_grid = "fraction"),
    paths = list(input_dir = tmp_in, out_dir = tmp_out)
  )

  testthat::expect_equal(result$config$country_grid, "fraction")
  meta <- yaml::read_yaml(file.path(tmp_out, "run_metadata.yaml"))
  testthat::expect_equal(meta$config$country_grid, "fraction")
  out <- nanoparquet::read_parquet(
    file.path(tmp_out, "gridded_livestock_emissions.parquet")
  )
  testthat::expect_true(2L %in% out$area_code)
  testthat::expect_equal(sum(out$heads), 12000)
})

# --- grid_vintage (whep#1000 T39) -------------------------------------------

# Landuse counterpart of `.write_livestock_fixture()`: the four parquets the
# `"lpjml"` preset reads with `use_type_constraint = FALSE`. Item 15 is
# `cft_mapping`'s wheat, so the CFT aggregation has something to group on.
.write_landuse_fixture <- function(dir) {
  nanoparquet::write_parquet(
    tibble::tribble(
      ~year, ~area_code, ~item_prod_code, ~harvested_area_ha,
      2000L,         1L,             15L,                400
    ),
    file.path(dir, "country_areas.parquet")
  )
  nanoparquet::write_parquet(
    tibble::tribble(
      ~lon,  ~lat, ~item_prod_code, ~harvest_fraction,
      0.25, 50.25,             15L,               0.6,
      0.75, 50.25,             15L,               0.4
    ),
    file.path(dir, "crop_patterns.parquet")
  )
  nanoparquet::write_parquet(
    tibble::tribble(
      ~lon,  ~lat,  ~year, ~cropland_ha,
      0.25, 50.25, 2000L,           600,
      0.75, 50.25, 2000L,           500
    ),
    file.path(dir, "gridded_cropland.parquet")
  )
  nanoparquet::write_parquet(
    tibble::tribble(
      ~lon,  ~lat, ~area_code, ~cell_area_frac,
      0.25, 50.25,         1L,               1,
      0.75, 50.25,         1L,               1
    ),
    file.path(dir, "country_grid.parquet")
  )
}

testthat::test_that("the loader forwards grid_vintage to the reader", {
  # What the loader passes on is the difference between two geographies, so it
  # is captured rather than inferred. The default it resolves is the 2015
  # snapshot, held there by `.grid_vintages()`; T39's own measurement is what
  # keeps it there, and this is the assertion that would catch a silent flip.
  seen <- NULL
  testthat::local_mocked_bindings(
    read_level_country_grid = function(level = 0L, grid_vintage = NULL, ...) {
      seen <<- list(level = level, grid_vintage = grid_vintage)
      tibble::tibble(
        lon = 0.25,
        lat = 50.25,
        area_code = 1L,
        cell_area_frac = 1
      )
    },
    .package = "whep"
  )
  fn <- getFromNamespace(".load_country_grid", "whep")

  fn(NULL, "polycell")
  testthat::expect_identical(seen$grid_vintage, "snapshot_2015")

  fn(NULL, "polycell", 0L, "year_aware")
  testthat::expect_identical(seen$grid_vintage, "year_aware")
})

testthat::test_that("the loader refuses an unknown grid_vintage", {
  fn <- getFromNamespace(".load_country_grid", "whep")
  testthat::expect_error(
    fn(NULL, "polycell", 0L, "2015"),
    class = "rlang_error"
  )
})

testthat::test_that("a static crosswalk says the vintage is not read", {
  tmp <- withr::local_tempdir()
  .write_livestock_fixture(tmp)
  .write_fraction_grid(tmp)
  fn <- getFromNamespace(".load_country_grid", "whep")

  testthat::expect_message(
    grid <- fn(tmp, "centroid", 0L, "year_aware"),
    "carries no validity interval"
  )
  testthat::expect_setequal(grid$area_code, 1L)
  testthat::expect_identical(
    whep:::.grid_vintage_method("centroid", "year_aware"),
    "static_crosswalk"
  )
  testthat::expect_identical(
    whep:::.grid_vintage_method("polycell", "snapshot_2015"),
    "snapshot_2015"
  )
  # A granted depth never reads the key, so what is RECORDED is what the grid
  # actually is, not what the config asked for.
  testthat::expect_identical(
    whep:::.grid_vintage_method("polycell", "snapshot_2015", 1L),
    "year_aware"
  )
})

testthat::test_that("grid_vintage is recorded in metadata and in the rows", {
  tmp_in <- withr::local_tempdir()
  .write_livestock_fixture(tmp_in)
  .write_fraction_grid(tmp_in)
  tmp_out <- withr::local_tempdir()

  result <- whep::run_spatialize(
    preset = "whep",
    years = 2000L,
    components = "livestock",
    overrides = list(country_grid = "fraction", grid_vintage = "year_aware"),
    paths = list(input_dir = tmp_in, out_dir = tmp_out)
  )

  testthat::expect_identical(result$config$grid_vintage, "year_aware")
  meta <- yaml::read_yaml(file.path(tmp_out, "run_metadata.yaml"))
  testthat::expect_identical(meta$config$grid_vintage, "year_aware")
  # The crosswalk carries no vintage, so the RESOLVED method is neither of
  # the two support vintages and the metadata says so beside the request.
  testthat::expect_identical(meta$method_grid_vintage, "static_crosswalk")
  out <- nanoparquet::read_parquet(
    file.path(tmp_out, "gridded_livestock_emissions.parquet")
  )
  testthat::expect_identical(
    unique(out$method_grid_vintage),
    "static_crosswalk"
  )
})

testthat::test_that("run_spatialize refuses an unknown grid_vintage", {
  tmp_in <- withr::local_tempdir()
  .write_livestock_fixture(tmp_in)
  testthat::expect_error(
    whep::run_spatialize(
      preset = "whep",
      years = 2000L,
      components = "livestock",
      overrides = list(country_grid = "centroid", grid_vintage = "2015"),
      paths = list(input_dir = tmp_in, out_dir = withr::local_tempdir())
    ),
    class = "rlang_error"
  )
})

testthat::test_that("both crop outputs carry method_grid_vintage", {
  # The CFT aggregation groups on a fixed column set, so a provenance column
  # added before it is dropped unless it is stamped on both writes.
  tmp_in <- withr::local_tempdir()
  .write_landuse_fixture(tmp_in)
  tmp_out <- withr::local_tempdir()

  whep::run_spatialize(
    preset = "lpjml",
    years = 2000L,
    components = "landuse",
    overrides = list(country_grid = "centroid"),
    paths = list(input_dir = tmp_in, out_dir = tmp_out)
  )

  crops <- nanoparquet::read_parquet(
    file.path(tmp_out, "gridded_landuse_crops.parquet")
  )
  cft <- nanoparquet::read_parquet(
    file.path(tmp_out, "gridded_landuse.parquet")
  )
  testthat::expect_identical(
    unique(crops$method_grid_vintage),
    "static_crosswalk"
  )
  testthat::expect_identical(
    unique(cft$method_grid_vintage),
    "static_crosswalk"
  )
})

testthat::test_that(".read_packaged_cft_mapping reuses the whep::cft_mapping package data", {
  read_packaged_cft_mapping <- getFromNamespace(
    ".read_packaged_cft_mapping",
    "whep"
  )
  testthat::expect_identical(read_packaged_cft_mapping(), whep::cft_mapping)
})

# The hold-out this gate accepts is the hold-out `resolve_admin_shares()`
# reads. The gate once took any non-empty name, so `list(USA = 1961:1989)`
# -- the form the roxygen and this abort's own example used to show -- was
# recorded in `run_metadata.yaml` and then refused by the only consumer
# there is, with class `whep_error_admin_exclude`.

testthat::test_that("an ISO3-keyed constraint_exclude is refused", {
  check <- getFromNamespace(".check_constraint_exclude", "whep")

  err <- testthat::expect_error(
    check(list(USA = 1961:1989)),
    class = "whep_error_admin_exclude"
  )
  testthat::expect_match(conditionMessage(err), "area_code")
  testthat::expect_match(conditionMessage(err), "USA")
})

testthat::test_that("an area_code-keyed hold-out is what both ends take", {
  check <- getFromNamespace(".check_constraint_exclude", "whep")
  exclude <- check(list("840" = c(1989, 1961:1989)))

  testthat::expect_identical(exclude, list("840" = 1961:1989))
  # The same value, through the consumer the gate defers to.
  parse_exclude <- getFromNamespace(".parse_exclude_years", "whep")
  rows <- parse_exclude(exclude)
  testthat::expect_identical(unique(rows$area_code), 840L)
  testthat::expect_identical(sort(rows$year), 1961:1989)
})

# --- The granted-depth wiring (whep#1000 T40) -------------------------------
#
# Every test below fails on the code as it stood before T40, where
# `overrides$level` was validated, written into `run_metadata.yaml`, and then
# read by nothing: `build_allocation_layer()`, `resolve_admin_shares()`,
# `allocate_level_crops()`, `reconcile_admin_allocation()` and `seam_gate()`
# had no call site anywhere in `R/`, `inst/` or `validation/` outside their own
# tests, so a `level = 1L` run allocated FAOSTAT national totals on the level-0
# pattern and ignored every subnational row.

# Two Japanese prefectures, one cell each, in the shape
# `read_polycell_support()` returns. `area_code` is carried explicitly because
# `polity_area_crosswalk` has no row for a prefecture: without it the level-0
# half of the allocation layer would lose the container entirely.
.rs_depth_support <- function() {
  tibble::tribble(
    ~polycell_id, ~cell_id, ~lon,   ~lat,  ~polity_code,
    "AICHI@1",          1L, 137.25, 35.25, "JPN-AICHI-1871-2025",
    "GIFU@2",           2L, 137.75, 35.25, "JPN-GIFU-1871-2025"
  ) |>
    dplyr::mutate(
      area_code = 110L,
      start_year = 1952L,
      end_year = 2025L,
      cell_area_ha = c(3000, 4000),
      land_area_ha = c(3000, 4000)
    )
}

# One item, one year, two prefectures reporting 300 and 700 ha. The pin ships
# `value` and no `share`, which is what every value-shipping family does.
.rs_depth_shares <- function() {
  tibble::tibble(
    area_code = 110L,
    level_polity_code = NA_character_,
    level = 1L,
    item_prod_code = 15L,
    indicator_used = "area_harvested",
    year = 2000L,
    value = c(300, 700),
    share = NA_real_,
    source = "admin-stats-japan",
    tier = 2L,
    grain = "admin1",
    concept_break = FALSE,
    nuts_version = NA_character_,
    source_native_id = c("JPN-AICHI", "JPN-GIFU"),
    source_native_name = c("Aichi", "Gifu"),
    source_id = "admin-stats-japan",
    source_version = NA_character_,
    recorded_at = "2026-01-01T00:00:00Z",
    treatment_year = "observed",
    value_flag = NA_character_
  )
}

# The identity alias route the investigation found for the staged Japanese
# pin: `polity_code == paste0(source_native_unit_id, "-1871-2025")`, bijective
# over the 46 prefectures. Stubbed rather than read, because the alias rows
# are a whep-polities deliverable that `polity_label_aliases` does not carry
# yet, and the suite must not depend on them landing.
.rs_stub_resolve_units <- function(x, code_system, year_col = "year", ...) {
  rows <- dplyr::mutate(
    x,
    alias_source = as.character(code_system),
    level_polity_code = paste0(x$source_native_unit_id, "-1871-2025")
  )
  list(
    rows = rows,
    diagnostics = tibble::tibble(
      source = unique(rows$source),
      alias_source = unique(rows$alias_source),
      n_rows = nrow(rows),
      n_unresolved = 0L,
      example_ids = NA_character_
    )
  )
}

.rs_write_depth_inputs <- function(dir) {
  nanoparquet::write_parquet(
    tibble::tibble(
      year = 2000L,
      area_code = 110L,
      item_prod_code = 15L,
      harvested_area_ha = 1000
    ),
    file.path(dir, "country_areas.parquet")
  )
  nanoparquet::write_parquet(
    tibble::tibble(
      lon = c(137.25, 137.75),
      lat = 35.25,
      item_prod_code = 15L,
      harvest_fraction = c(0.5, 0.5)
    ),
    file.path(dir, "crop_patterns.parquet")
  )
  nanoparquet::write_parquet(
    tibble::tibble(
      lon = c(137.25, 137.75),
      lat = 35.25,
      year = 2000L,
      cropland_ha = c(5000, 5000)
    ),
    file.path(dir, "gridded_cropland.parquet")
  )
}

.rs_local_depth_mocks <- function(env = parent.frame()) {
  testthat::local_mocked_bindings(
    read_polycell_support = function(...) .rs_depth_support(),
    read_admin_shares = function(...) {
      list(
        shares = .rs_depth_shares(),
        excluded = tibble::tibble(
          source = character(),
          reason = character(),
          detail = character()
        ),
        not_shipped = character()
      )
    },
    resolve_admin_units = .rs_stub_resolve_units,
    .package = "whep",
    .env = env
  )
}

testthat::test_that("a depth run allocates on admin shares, not the pattern", {
  # THE TEST THE MISSING CALL SITE FAILED. Two prefectures reporting 300 and
  # 700 ha of a 1,000 ha national total, over two cells whose gridded pattern
  # is an even 50/50 split. A run that ignored the admin rows would put 500 ha
  # in each cell and still conserve the national total, which is why the
  # conservation check alone cannot see the defect.
  .rs_local_depth_mocks()
  tmp_in <- withr::local_tempdir()
  tmp_out <- withr::local_tempdir()
  .rs_write_depth_inputs(tmp_in)

  res <- suppressWarnings(suppressMessages(whep::run_spatialize(
    preset = "lpjml",
    years = 2000L,
    components = "landuse",
    overrides = list(
      level = 1L,
      output_level = 1L,
      granted_containers = 110L
    ),
    paths = list(input_dir = tmp_in, out_dir = tmp_out)
  )))

  crops <- tibble::as_tibble(nanoparquet::read_parquet(
    file.path(tmp_out, "gridded_landuse_crops.parquet")
  ))
  testthat::expect_setequal(
    crops$level_polity_code,
    c("JPN-AICHI-1871-2025", "JPN-GIFU-1871-2025")
  )
  placed <- crops |>
    dplyr::summarise(
      ha = sum(rainfed_ha + irrigated_ha),
      .by = "level_polity_code"
    ) |>
    dplyr::arrange(level_polity_code)
  testthat::expect_equal(placed$ha, c(300, 700))
  testthat::expect_equal(sum(placed$ha), 1000)
  testthat::expect_identical(unique(crops$method_grid_vintage), "year_aware")

  targets <- utils::read.csv(file.path(tmp_out, "admin_targets.csv"))
  testthat::expect_identical(
    unique(targets$method_crop_alloc),
    "admin_area_shares"
  )
  testthat::expect_equal(sort(targets$share), c(0.3, 0.7))
})

testthat::test_that("a depth run records what actually constrained it", {
  .rs_local_depth_mocks()
  tmp_in <- withr::local_tempdir()
  tmp_out <- withr::local_tempdir()
  .rs_write_depth_inputs(tmp_in)

  suppressWarnings(suppressMessages(whep::run_spatialize(
    preset = "lpjml",
    years = 2000L,
    components = "landuse",
    overrides = list(level = 1L, granted_containers = 110L),
    paths = list(input_dir = tmp_in, out_dir = tmp_out)
  )))

  meta <- yaml::read_yaml(file.path(tmp_out, "run_metadata.yaml"))
  testthat::expect_identical(meta$config$granted_containers, 110L)
  testthat::expect_identical(meta$config$double_claim, "co_presence")
  testthat::expect_identical(meta$admin_constraint$n_shares_resolved, 2L)
  testthat::expect_identical(meta$admin_constraint$n_units_constrained, 2L)
  # The family ships values and no share, so the gate's series was normalised
  # from them -- which makes tier A's share-against-value identity vacuous,
  # and the run says so rather than reporting a pass it did not earn.
  testthat::expect_identical(
    meta$admin_constraint$method_admin_share$value_normalised,
    2L
  )
  testthat::expect_identical(
    meta$admin_constraint$method_crop_alloc$admin_area_shares,
    2L
  )

  # Every diagnostic the run promised is on disk, with its header at least.
  for (nm in unlist(whep:::.admin_run_files())) {
    testthat::expect_true(file.exists(file.path(tmp_out, nm)), info = nm)
  }
  coverage <- utils::read.csv(file.path(tmp_out, "admin_coverage.csv"))
  testthat::expect_identical(
    names(coverage),
    names(whep::admin_coverage_prototype())
  )
  testthat::expect_equal(nrow(coverage), 1L)
})

testthat::test_that("a level-0 run says it had no admin constraint", {
  tmp_in <- withr::local_tempdir()
  tmp_out <- withr::local_tempdir()
  .rs_write_depth_inputs(tmp_in)
  nanoparquet::write_parquet(
    tibble::tibble(
      lon = c(137.25, 137.75),
      lat = 35.25,
      area_code = 110L,
      cell_area_frac = 1
    ),
    file.path(tmp_in, "country_grid.parquet")
  )
  suppressWarnings(suppressMessages(whep::run_spatialize(
    preset = "lpjml",
    years = 2000L,
    components = "landuse",
    overrides = list(country_grid = "centroid"),
    paths = list(input_dir = tmp_in, out_dir = tmp_out)
  )))
  meta <- yaml::read_yaml(file.path(tmp_out, "run_metadata.yaml"))
  testthat::expect_identical(meta$admin_constraint, "none")
  testthat::expect_false(
    file.exists(file.path(tmp_out, "admin_targets.csv"))
  )
})

testthat::test_that("a depth needs containers, and level 0 refuses them", {
  testthat::expect_error(
    whep::run_spatialize(preset = "lpjml", overrides = list(level = 1L)),
    "grants a depth to no container"
  )
  testthat::expect_error(
    whep::run_spatialize(
      preset = "lpjml",
      overrides = list(granted_containers = 110L)
    ),
    "level = 0"
  )
})

testthat::test_that("an unregistered admin-shares pin aborts a depth run", {
  testthat::local_mocked_bindings(
    read_polycell_support = function(...) .rs_depth_support(),
    read_admin_shares = function(...) {
      list(
        shares = whep::admin_shares_prototype(),
        excluded = tibble::tibble(),
        not_shipped = "admin-shares"
      )
    },
    .package = "whep"
  )
  tmp_in <- withr::local_tempdir()
  tmp_out <- withr::local_tempdir()
  .rs_write_depth_inputs(tmp_in)
  testthat::expect_error(
    suppressWarnings(suppressMessages(whep::run_spatialize(
      preset = "lpjml",
      years = 2000L,
      components = "landuse",
      overrides = list(level = 1L, granted_containers = 110L),
      paths = list(input_dir = tmp_in, out_dir = tmp_out)
    ))),
    class = "whep_run_no_admin_shares"
  )
})

testthat::test_that("a granted container with no admin row aborts", {
  fn <- getFromNamespace(".admin_scope_containers", "whep")
  testthat::expect_error(
    fn(.rs_depth_shares(), c(110L, 724L)),
    class = "whep_run_admin_container_absent"
  )
  kept <- suppressMessages(fn(.rs_depth_shares(), 110L))
  testthat::expect_equal(nrow(kept), 2L)
})

testthat::test_that("shares that resolve to no polity abort the constraint", {
  testthat::local_mocked_bindings(
    resolve_admin_units = function(x, code_system, ...) {
      list(
        rows = dplyr::mutate(
          x,
          alias_source = "whep-lab-japan",
          level_polity_code = NA_character_
        ),
        diagnostics = tibble::tibble()
      )
    },
    .package = "whep"
  )
  fn <- getFromNamespace(".admin_resolve_units", "whep")
  testthat::expect_error(
    fn(.rs_depth_shares()),
    class = "whep_run_admin_unresolved"
  )
})

testthat::test_that("the unit resolver is handed the readers' column name", {
  # The contract calls the identifier `source_native_id`;
  # `resolve_admin_units()` reads `source_native_unit_id`. Handing the rows
  # over unrenamed aborts on
  # the missing column, so the rename is pinned rather than assumed.
  seen <- NULL
  testthat::local_mocked_bindings(
    resolve_admin_units = function(x, code_system, ...) {
      seen <<- list(cols = names(x), system = code_system)
      .rs_stub_resolve_units(x, code_system, ...)
    },
    .package = "whep"
  )
  fn <- getFromNamespace(".admin_resolve_units", "whep")
  out <- fn(.rs_depth_shares())
  testthat::expect_true("source_native_unit_id" %in% seen$cols)
  testthat::expect_identical(unique(seen$system), "whep-lab-japan")
  testthat::expect_true("source_native_id" %in% names(out$shares))
  testthat::expect_false("alias_source" %in% names(out$shares))
})

testthat::test_that("every admin source names one code system", {
  fn <- getFromNamespace(".admin_code_systems_for", "whep")
  testthat::expect_identical(
    fn(c(
      "admin-stats-japan",
      "USDA_NASS",
      "IBGE_PAM",
      "JRC_subnational_crops"
    )),
    c("whep-lab-japan", "usda-nass-fips", "ibge-uf", "jrc-nuts")
  )
  # Read off the registry, so a family added there cannot go missing here.
  families <- getFromNamespace(".admin_family_aliases", "whep")()
  testthat::expect_identical(
    fn(families),
    paste0("whep-lab-", sub("^admin-stats-", "", families))
  )
  testthat::expect_error(
    fn("Eurostat_apro_cpshr_2099"),
    class = "whep_run_admin_code_system"
  )
})

testthat::test_that("a constraint meeting no layer unit aborts", {
  fn <- getFromNamespace(".admin_check_layer_units", "whep")
  shares <- tibble::tibble(level_polity_code = c("A-1", "A-2"))
  testthat::expect_error(
    fn(shares, tibble::tibble(level_polity_code = c("B-1", "B-2"))),
    class = "whep_run_admin_layer_mismatch"
  )
  testthat::expect_silent(suppressMessages(
    fn(shares, tibble::tibble(level_polity_code = "A-2"))
  ))
})

testthat::test_that("a value-shipping family's share is the group's own", {
  fn <- getFromNamespace(".admin_gate_shares", "whep")
  out <- fn(dplyr::mutate(.rs_depth_shares(), treatment = "observed"))
  testthat::expect_identical(unique(out$share_basis), "value_normalised")
  testthat::expect_equal(out$share, c(0.3, 0.7))
  testthat::expect_equal(sum(out$share), 1)

  # A family that ships its own share keeps it untouched.
  reported <- .rs_depth_shares() |>
    dplyr::mutate(share = c(0.25, 0.75), treatment = "observed")
  kept <- fn(reported)
  testthat::expect_identical(unique(kept$share_basis), "reported")
  testthat::expect_equal(kept$share, c(0.25, 0.75))

  # Neither a share nor a usable value is not silently a zero share.
  none <- .rs_depth_shares() |>
    dplyr::mutate(value = NA_real_, treatment = "observed")
  testthat::expect_identical(unique(fn(none)$share_basis), "unavailable")
})

testthat::test_that("a vacuous tier-A identity is said out loud", {
  fn <- getFromNamespace(".warn_gate_identity_vacuous", "whep")
  gate <- getFromNamespace(".admin_gate_shares", "whep")
  testthat::expect_warning(
    fn(gate(dplyr::mutate(.rs_depth_shares(), treatment = "observed"))),
    "holds by construction"
  )
  reported <- .rs_depth_shares() |>
    dplyr::mutate(share = c(0.25, 0.75), treatment = "observed")
  testthat::expect_silent(fn(gate(reported)))
})
