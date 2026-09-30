# The harmoniser lives in inst/scripts/prepare_jrc_subnational.R, which is
# `.Rbuildignore`d (^inst/scripts$), so it is absent from the built
# tarball and its functions are not package objects: they are sourced
# here rather than reached through `whep:::`. That makes this file skip
# under `R CMD check` on the tarball and run under `devtools::test()` /
# `pkgload::load_all()` in a source checkout, which is where the script
# is edited and run. The guard is on the file, the pattern the LPJmL and
# HWSD smoke tests use.
#
# Nothing here touches the network: every test drives the harmoniser from
# the 10-row fixture, which holds verbatim rows of the published
# 2025.01 release (EU27_CROP_STATS_2025.zip, sha256
# d9bf854ccbaf46165d500b5168ba5f8b5bcb91c7f1651bcc3c8e7b47fb3801e6).

.jrc_script_path <- function() {
  candidates <- c(
    testthat::test_path(
      "..",
      "..",
      "inst",
      "scripts",
      "prepare_jrc_subnational.R"
    ),
    system.file("scripts", "prepare_jrc_subnational.R", package = "whep")
  )
  found <- candidates[nzchar(candidates) & file.exists(candidates)]
  if (length(found) == 0L) {
    return(NA_character_)
  }
  found[[1L]]
}

.jrc_load_script <- function() {
  path <- .jrc_script_path()
  testthat::skip_if(
    is.na(path),
    "inst/scripts/prepare_jrc_subnational.R is not in this build"
  )
  env <- new.env(parent = globalenv())
  source(path, local = env)
  env
}

.jrc_fixture_raw <- function(env) {
  env$read_jrc_subnational(
    testthat::test_path("fixtures", "jrc_subnational_sample.csv")
  )
}

.jrc_fixture_harmonized <- function(env) {
  env$harmonize_jrc_subnational(
    .jrc_fixture_raw(env),
    source_version = "2025.01",
    recorded_at = "2026-09-02T00:00:00Z"
  )
}

test_that("the raw reader returns the publisher's 13 columns verbatim", {
  env <- .jrc_load_script()
  raw <- .jrc_fixture_raw(env)

  expect_equal(names(raw), env$.jrc_raw_columns())
  expect_equal(nrow(raw), 10L)
  expect_true(all(vapply(raw, is.character, logical(1))))
})

test_that("the raw reader aborts when a published column is missing", {
  env <- .jrc_load_script()
  path <- withr::local_tempfile(fileext = ".csv")
  .jrc_fixture_raw(env) |>
    dplyr::select(-"COHERENCE_CROP") |>
    readr::write_csv(path)

  expect_error(env$read_jrc_subnational(path), "COHERENCE_CROP")
})

test_that("the harmoniser holds its column contract", {
  env <- .jrc_load_script()
  harmonized <- suppressWarnings(.jrc_fixture_harmonized(env))

  expect_equal(
    names(harmonized),
    c(
      "source",
      "source_native_unit_id",
      "source_native_unit_name",
      "source_native_item_code",
      "source_native_item_name",
      "quantity",
      "indicator_used",
      "year",
      "value",
      "value_unit",
      "value_flag",
      "concept_break",
      "grain",
      "nuts_level",
      "nuts_version",
      "source_version",
      "recorded_at",
      "country_code",
      "statistic_origin",
      "calculated_region",
      "calculated_crop",
      "calculated_value",
      "zero_as_null",
      "coherence_apy",
      "coherence_crop"
    )
  )
  expect_equal(nrow(harmonized), 10L)
  expect_type(harmonized$year, "integer")
  expect_type(harmonized$nuts_level, "integer")
  expect_type(harmonized$value, "double")
  expect_type(harmonized$concept_break, "logical")
  expect_type(harmonized$calculated_value, "logical")
  expect_true(all(harmonized$source == "JRC_subnational_crops"))
  expect_true(all(harmonized$nuts_version == "2016"))
  expect_true(all(harmonized$source_version == "2025.01"))
  expect_false(any(harmonized$concept_break))
})

test_that("the harmoniser keeps the closed vocabularies", {
  env <- .jrc_load_script()
  harmonized <- suppressWarnings(.jrc_fixture_harmonized(env))

  expect_setequal(unique(harmonized$quantity), c("area", "production", "yield"))
  expect_true(all(
    harmonized$indicator_used %in%
      c("area_harvested", "production", "yield")
  ))
  expect_true(all(harmonized$value_unit %in% c("ha", "t", "t/ha")))
  expect_true(all(
    harmonized$grain %in% c("admin1", "admin2") | is.na(harmonized$grain)
  ))
  expect_true(all(
    harmonized$coherence_apy %in%
      c("yes", "no") |
      is.na(harmonized$coherence_apy)
  ))
})

test_that("NUTS level and grain follow the code, and NUTS 1 gets no grain", {
  env <- .jrc_load_script()
  harmonized <- suppressWarnings(.jrc_fixture_harmonized(env))
  by_code <- function(code) {
    dplyr::filter(harmonized, .data$source_native_unit_id == code)[1L, ]
  }

  expect_equal(by_code("CY")$nuts_level, 0L)
  expect_equal(by_code("DE1")$nuts_level, 1L)
  expect_equal(by_code("AT11")$nuts_level, 2L)
  expect_equal(by_code("CZ010")$nuts_level, 3L)

  expect_true(is.na(by_code("CY")$grain))
  expect_true(is.na(by_code("DE1")$grain))
  expect_equal(by_code("AT11")$grain, "admin1")
  expect_equal(by_code("CZ010")$grain, "admin2")

  expect_equal(by_code("CZ010")$country_code, "CZ")
})

test_that("published values and units come through unchanged", {
  env <- .jrc_load_script()
  harmonized <- suppressWarnings(.jrc_fixture_harmonized(env))
  row <- function(code, quantity) {
    dplyr::filter(
      harmonized,
      .data$source_native_unit_id == code,
      .data$quantity == .env$quantity
    )
  }

  expect_equal(row("CZ010", "area")$value, 5502)
  expect_equal(row("CZ010", "area")$value_unit, "ha")
  expect_equal(row("CZ010", "production")$value, 22615)
  expect_equal(row("CZ010", "production")$value_unit, "t")
  expect_equal(row("EL531", "yield")$value, 2.879)
  expect_equal(row("EL531", "yield")$value_unit, "t/ha")
  expect_equal(row("EL531", "yield")$indicator_used, "yield")
})

test_that("the six JRC flags survive as typed columns and in value_flag", {
  env <- .jrc_load_script()
  harmonized <- suppressWarnings(.jrc_fixture_harmonized(env))
  flags <- function(code) {
    dplyr::filter(harmonized, .data$source_native_unit_id == code)[1L, ]
  }

  # EL531 2009 Soft wheat yield: CALCULATED_R and CALCULATED_V both "Yes".
  expect_true(flags("EL531")$calculated_region)
  expect_true(flags("EL531")$calculated_value)
  expect_false(flags("EL531")$calculated_crop)
  expect_equal(
    flags("EL531")$value_flag,
    "calculated_value|calculated_region"
  )

  # BG31 2015: COHERENCE_APY "No"; BG41 2003: COHERENCE_CROP "No".
  expect_equal(flags("BG31")$coherence_apy, "no")
  expect_equal(flags("BG31")$value_flag, "coherence_apy_failed")
  expect_equal(flags("BG41")$coherence_crop, "no")
  expect_equal(flags("BG41")$value_flag, "coherence_crop_failed")

  # AT11 1975: CALCULATED_C "Yes", everything else clean.
  expect_true(flags("AT11")$calculated_crop)
  expect_equal(flags("AT11")$value_flag, "calculated_crop")

  # CZ010 1998 is clean on every flag.
  expect_true(is.na(flags("CZ010")$value_flag))
  expect_equal(flags("CZ010")$coherence_apy, "yes")
})

test_that("a withheld value is kept, flagged and left NA", {
  env <- .jrc_load_script()
  harmonized <- suppressWarnings(.jrc_fixture_harmonized(env))
  withheld <- dplyr::filter(harmonized, .data$source_native_unit_id == "BE10")

  expect_equal(nrow(withheld), 1L)
  expect_true(is.na(withheld$value))
  expect_true(withheld$zero_as_null)
  expect_equal(withheld$value_flag, "zero_as_null|missing_value")
})

test_that("a non-NUTS region code warns and is kept, never repaired", {
  env <- .jrc_load_script()

  expect_warning(
    harmonized <- .jrc_fixture_harmonized(env),
    "not NUTS-shaped"
  )
  bad <- dplyr::filter(harmonized, .data$source_native_unit_id == "5-Dec")

  expect_equal(nrow(bad), 1L)
  expect_equal(bad$value, 5.6)
  expect_true(is.na(bad$nuts_level))
  expect_true(is.na(bad$grain))
  expect_true(is.na(bad$country_code))
  expect_equal(bad$value_flag, "invalid_region_code")
})

test_that("crop classes pass through unchanged, aggregates included", {
  env <- .jrc_load_script()
  raw <- .jrc_fixture_raw(env)
  harmonized <- suppressWarnings(.jrc_fixture_harmonized(env))

  # The reader filters no crop: which of the release's nine classes an
  # item vocabulary keeps is T10/T11's inclusion decision.
  expect_setequal(
    unique(harmonized$source_native_item_code),
    unique(raw$CROP_NAME)
  )
  expect_true("Total wheat" %in% harmonized$source_native_item_code)
})

test_that("the aggregate classes are named, never silently summed", {
  env <- .jrc_load_script()
  known <- env$.jrc_aggregate_classes()

  expect_setequal(known$aggregate, c("Total wheat", "Total barley"))
  expect_equal(
    known$member_1[known$aggregate == "Total wheat"],
    "Soft wheat"
  )
  expect_equal(
    known$member_2[known$aggregate == "Total barley"],
    "Spring barley"
  )

  harmonized <- suppressWarnings(.jrc_fixture_harmonized(env))
  both <- dplyr::bind_rows(
    harmonized,
    dplyr::mutate(
      harmonized[1L, ],
      source_native_item_code = "Total barley"
    )
  )
  expect_message(
    env$.jrc_warn_aggregate_classes(both),
    "Total barley"
  )
  expect_message(
    env$.jrc_warn_aggregate_classes(both),
    "aggregate of"
  )
})

test_that("a repeated region-crop-year-variable key aborts", {
  env <- .jrc_load_script()
  raw <- .jrc_fixture_raw(env)
  doubled <- dplyr::bind_rows(raw, raw[1L, ])

  expect_error(
    env$harmonize_jrc_subnational(doubled, "2025.01"),
    "uniquely identified"
  )
})

test_that("an unknown variable or a changed unit aborts", {
  env <- .jrc_load_script()
  raw <- .jrc_fixture_raw(env)

  expect_error(
    env$harmonize_jrc_subnational(
      dplyr::mutate(raw[1L, ], VARIABLE = "Humidity"),
      "2025.01"
    ),
    "unknown variable"
  )
  expect_error(
    env$harmonize_jrc_subnational(
      dplyr::mutate(raw, UoM = "1000 ha"),
      "2025.01"
    ),
    "unit this reader does not expect"
  )
})

test_that("a non-numeric published value aborts rather than becoming NA", {
  env <- .jrc_load_script()
  raw <- .jrc_fixture_raw(env)

  expect_error(
    env$harmonize_jrc_subnational(
      dplyr::mutate(raw[1L, ], VALUE = "32 797"),
      "2025.01"
    ),
    "not numeric"
  )
})

test_that("source_version must be the publisher's release string", {
  env <- .jrc_load_script()
  raw <- .jrc_fixture_raw(env)

  expect_error(
    env$harmonize_jrc_subnational(raw, source_version = NULL),
    "source_version"
  )
})

test_that("the shipped source manifest carries attribution and checksums", {
  path <- system.file(
    "extdata",
    "jrc_subnational_source_manifest.csv",
    package = "whep"
  )
  if (!nzchar(path)) {
    path <- testthat::test_path(
      "..",
      "..",
      "inst",
      "extdata",
      "jrc_subnational_source_manifest.csv"
    )
  }
  testthat::skip_if_not(file.exists(path), "manifest not in this build")
  manifest <- utils::read.csv(path, stringsAsFactors = FALSE)

  expect_true(all(
    c(
      "relative_path",
      "source_url",
      "bytes",
      "md5",
      "sha256",
      "retrieved_at",
      "role",
      "dataset_release",
      "nuts_version",
      "attribution",
      "doi",
      "licence"
    ) %in%
      names(manifest)
  ))
  expect_true(any(manifest$role == "data"))
  expect_true(all(nchar(manifest$sha256) == 64L))
  expect_true(all(nchar(manifest$md5) == 32L))
  expect_true(all(
    manifest$doi == "10.2905/685949ff-56de-4646-a8df-844b5bb5f835"
  ))
  expect_equal(
    manifest$sha256[manifest$relative_path == "EU27_CROP_STATS_2025.zip"],
    "d9bf854ccbaf46165d500b5168ba5f8b5bcb91c7f1651bcc3c8e7b47fb3801e6"
  )
})
