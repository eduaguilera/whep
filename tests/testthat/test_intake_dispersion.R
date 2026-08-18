# All fixtures are injected, so the suite never reaches FAOSTAT.

.id_cv <- function() {
  tibble::tribble(
    ~year, ~area_code, ~cv,
    2001L, 10L,        0.25,
    2002L, 10L,        0.24,
    2001L, 20L,        0.17
  )
}

testthat::test_that("the CV becomes a log-SD, not a raw standard deviation", {
  # sigma = sqrt(log(1 + cv^2)), the conversion FAO performs itself. Treating a
  # raw CV as a log-SD errs about 1.5% at a CV of 0.25, which is the median
  # country -- small, systematic, and free to get right.
  out <- whep::build_intake_dispersion(
    data = list(habitual_cv = .id_cv()),
    nutrient = "energy"
  )
  cv25 <- dplyr::filter(out, .data$area_code == 10L, .data$year == 2001L)
  testthat::expect_equal(cv25$sigma, sqrt(log(1 + 0.25^2)))
  testthat::expect_lt(cv25$sigma, 0.25)
})

testthat::test_that("protein scales the log-SD, not the CV", {
  # The 1.062 ratio was measured on log-SDs, so it must be applied there. Doing
  # it to the CV first and converting after would give a different number.
  energy <- whep::build_intake_dispersion(
    data = list(habitual_cv = .id_cv()),
    nutrient = "energy"
  )
  protein <- whep::build_intake_dispersion(
    data = list(habitual_cv = .id_cv()),
    nutrient = "protein"
  )
  testthat::expect_equal(protein$sigma, energy$sigma * 1.062)
  testthat::expect_false(
    isTRUE(all.equal(
      protein$sigma,
      sqrt(log(1 + (energy$cv * 1.062)^2))
    ))
  )
})

testthat::test_that("calibrated raises the dispersion and is not the default", {
  base <- whep::build_intake_dispersion(data = list(habitual_cv = .id_cv()))
  cal <- whep::build_intake_dispersion(
    data = list(habitual_cv = .id_cv()),
    estimand = "calibrated"
  )
  testthat::expect_equal(cal$sigma, base$sigma * 1.477)
  testthat::expect_equal(unique(base$method_dispersion), "protein_faostat")
  testthat::expect_equal(unique(cal$method_dispersion), "protein_calibrated")
})

testthat::test_that("years before the series are carried back and stamped", {
  out <- whep::build_intake_dispersion(
    data = list(habitual_cv = .id_cv()),
    years = 1999:2002
  )
  a10 <- dplyr::filter(out, .data$area_code == 10L) |>
    dplyr::arrange(.data$year)
  testthat::expect_equal(a10$year, 1999:2002)
  # 1999 and 2000 take 2001's value, and say so.
  testthat::expect_equal(a10$cv, c(0.25, 0.25, 0.25, 0.24))
  testthat::expect_equal(
    a10$method_cv_year,
    c("hold_constant", "hold_constant", "faostat_observed", "faostat_observed")
  )
})

testthat::test_that("observed_only refuses to fill instead of filling", {
  out <- whep::build_intake_dispersion(
    data = list(habitual_cv = .id_cv()),
    years = 1999:2002,
    temporal = "observed_only"
  )
  testthat::expect_false(any(out$year < 2001L))
  testthat::expect_equal(unique(out$method_cv_year), "faostat_observed")
})

testthat::test_that("each area carries back its OWN earliest value", {
  # Area 20 starts at 0.17 and area 10 at 0.25; a global earliest value would
  # give both the same number.
  out <- whep::build_intake_dispersion(
    data = list(habitual_cv = .id_cv()),
    years = 2000L
  )
  testthat::expect_equal(
    dplyr::filter(out, .data$area_code == 10L)$cv,
    0.25
  )
  testthat::expect_equal(
    dplyr::filter(out, .data$area_code == 20L)$cv,
    0.17
  )
})

testthat::test_that("a missing input column aborts", {
  testthat::expect_error(
    whep::build_intake_dispersion(
      data = list(habitual_cv = dplyr::select(.id_cv(), -"cv"))
    ),
    "cv"
  )
})

testthat::test_that("unknown method values are rejected", {
  testthat::expect_error(
    whep::build_intake_dispersion(
      data = list(habitual_cv = .id_cv()),
      nutrient = "fat"
    ),
    "arg_match|must be one of|fat"
  )
  testthat::expect_error(
    whep::build_intake_dispersion(
      data = list(habitual_cv = .id_cv()),
      estimand = "nope"
    ),
    "arg_match|must be one of|nope"
  )
})

testthat::test_that("read_habitual_cv keeps only item 21058", {
  # The FAOSTAT food-security file carries dozens of indicators in one long
  # table. An unfiltered read would mix undernourishment prevalence into a
  # dispersion column without any error.
  raw <- tibble::tribble(
    ~`Area Code`, ~`Item Code`, ~Year, ~Value,
    2L,           21058L,       2010L, 0.25,
    2L,           21001L,       2010L, 18.6,
    4L,           21058L,       2010L, 0.31
  )
  out <- whep::read_habitual_cv(data = raw)
  testthat::expect_equal(nrow(out), 2L)
  testthat::expect_setequal(out$cv, c(0.25, 0.31))
  testthat::expect_false(any(out$cv > 1))
})

testthat::test_that("a manifest mismatch aborts and discards the file", {
  dir <- withr::local_tempdir()
  fetch <- function(url, path) writeLines("not the real zip", path)
  testthat::expect_error(
    whep:::.fs_download(dir, fetch = fetch),
    "manifest"
  )
  testthat::expect_false(file.exists(file.path(dir, whep:::.fs_file_name())))
})
