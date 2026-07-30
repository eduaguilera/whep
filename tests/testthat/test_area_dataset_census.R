# Every dataset that names a territory must be tied to the polities database by something.
#
# This is the census, made durable. Sweeping it by hand found `gleam_geographic_hierarchy`,
# a 204-country table that defines the country universe for the whole energy extension and
# had no link to the polities at all — and the reason an earlier sweep missed it is worth
# keeping: it enumerated EXPORTED objects, and package data is not an export. Data objects
# live in the namespace's lazydata environment and are reachable through `::` because of
# LazyData, so `getNamespaceExports()` never lists them. A sweep built on that returns a
# confident zero.
#
# So the universe here is `lazydata` — 95 objects, of which 23 carry a column naming a
# territory. What this asserts is deliberately modest: each of those 23 is REFERENCED by at
# least one test. That is weaker than "its vocabulary agrees with the polities database",
# which is what the specific tests do one dataset at a time (`test_gleam_hierarchy_*`,
# `test_bouwman_region_membership`, `test_coef_region_vocabulary_matches_join`,
# `test_dataset_area_identifiers`, and the coverage tests for lassaletta, Mueller and
# crops-manure-n). It is the guard those tests cannot provide: a NEW area-carrying dataset
# arriving with no test at all fails here, instead of being integrated by nobody and noticed
# by no one.

.area_carrying_datasets <- function() {
  ns <- asNamespace("whep")
  lazy <- ns$.__NAMESPACE__.$lazydata
  # A column whose NAME claims a territory. Deliberately broad: the cost of a false
  # positive is one test reference, the cost of a false negative is an unlinked dataset.
  pattern <- "^(area|iso3|iso|country|region|continent|polity|reporting)"
  out <- character(0)
  for (nm in sort(ls(lazy))) {
    obj <- tryCatch(get(nm, envir = lazy), error = function(e) NULL)
    if (is.null(obj)) {
      next
    }
    if (inherits(obj, "sf")) {
      obj <- sf::st_drop_geometry(obj)
    }
    if (!is.data.frame(obj)) {
      next
    }
    if (length(grep(pattern, names(obj), ignore.case = TRUE)) > 0L) {
      out <- c(out, nm)
    }
  }
  out
}

test_that("the area-carrying dataset census is what it was measured to be", {
  ds <- .area_carrying_datasets()
  # Non-vacuous twice over: the lazydata environment must be populated, and the pattern
  # must actually match something. A sweep that finds nothing passes every downstream
  # assertion, which is how the exported-objects version of this went wrong.
  expect_gt(length(ls(asNamespace("whep")$.__NAMESPACE__.$lazydata)), 50L)
  expect_gt(length(ds), 15L)

  # Pinned by identity, not count, so a swap of one dataset for another is visible.
  expect_setequal(
    ds,
    c(
      "conv_bouwman",
      "crops_manure_n",
      "gleam_animal_weights",
      "gleam_dressing_percentages",
      "gleam_fracremove",
      "gleam_geographic_hierarchy",
      "gleam_mechanization_levels",
      "gleam_milk_production",
      "gleam_mms_shares",
      "ipcc_2006_enteric_ef",
      "ipcc_2006_manure_ef",
      "ipcc_2019_enteric_ef_cattle",
      "ipcc_2019_manure_ch4_ef_cattle",
      "ipcc_2019_n_excretion",
      "lassaletta_grassland_share",
      "mueller_synthetic_n",
      "polities",
      "polities_cats",
      "polity_area_crosswalk",
      "polity_label_aliases",
      "regional_mms_distribution",
      "regions_full",
      "urban_n_reference"
    )
  )
})

test_that("every area-carrying dataset is referenced by some test", {
  ds <- .area_carrying_datasets()
  files <- list.files(
    testthat::test_path(),
    pattern = "\\.R$",
    full.names = TRUE
  )
  testthat::skip_if(length(files) == 0L, "test files not reachable")
  bodies <- vapply(
    files,
    function(f) paste(readLines(f, warn = FALSE), collapse = " "),
    character(1)
  )
  referenced <- vapply(
    ds,
    function(nm) any(grepl(nm, bodies, fixed = TRUE)),
    logical(1)
  )
  expect_equal(ds[!referenced], character(0))
})

test_that("every documented dataset carrying a territory column is documented as such", {
  # A dataset with no .Rd is a dataset whose territory vocabulary nobody had to explain.
  # All 23 have one today; this keeps that true.
  ds <- .area_carrying_datasets()
  rd <- vapply(
    ds,
    function(nm) {
      p <- system.file("..", "man", paste0(nm, ".Rd"), package = "whep")
      if (!file.exists(p)) {
        p <- testthat::test_path("..", "..", "man", paste0(nm, ".Rd"))
      }
      file.exists(p)
    },
    logical(1)
  )
  testthat::skip_if(!any(rd), "man/ not reachable from an installed package")
  expect_equal(ds[!rd], character(0))
})
