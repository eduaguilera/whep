# Offline tests for inst/scripts/prepare_faostat_bulk.R, the producer of the
# FAOSTAT bulk pins. The script is sourced into a local environment; it is not
# in the built package (`^inst/scripts$` is in .Rbuildignore), so every test
# here skips on a tarball install and runs from a source checkout.
#
# What they pin down is whep#1025 and whep#1178: readr's type guesser turned
# FAOSTAT's tonnes label "t" into TRUE (faostat-cbs-new) and an all-empty
# `Note` column into a logical (faostat-fbs-new, faostat-landuse). The spec
# returned by faostat_bulk_col_types() is what stops the next regeneration
# repeating either.

.faostat_bulk_env <- function() {
  path <- testthat::test_path(
    "..",
    "..",
    "inst",
    "scripts",
    "prepare_faostat_bulk.R"
  )
  if (!file.exists(path)) {
    testthat::skip(paste(
      "inst/scripts/prepare_faostat_bulk.R is not in the built package",
      "(`^inst/scripts$` is in .Rbuildignore)."
    ))
  }
  env <- new.env(parent = globalenv())
  sys.source(path, envir = env)
  env
}

# The two shapes that broke: a single-unit domain whose every Unit is "t", and
# a Note column FAO ships empty. `Item Code (CPC)` carries FAO's leading
# apostrophe.
.faostat_bulk_csv <- function() {
  path <- withr::local_tempfile(fileext = ".csv", .local_envir = parent.frame())
  writeLines(
    c(
      "Area Code,Area,Item Code,Item Code (CPC),Item,Element Code,Element,Year Code,Year,Unit,Value,Flag,Note",
      "4,Afghanistan,6620,'01,Cropland,5110,Area,2010,2010,t,100,A,",
      "4,Afghanistan,6620,'01,Cropland,5110,Area,2011,2011,t,200,A,"
    ),
    path
  )
  path
}

test_that("readr's guess makes Note logical, the spec keeps it character", {
  env <- .faostat_bulk_env()
  path <- .faostat_bulk_csv()

  guessed <- readr::read_csv(path, show_col_types = FALSE)
  expect_type(guessed$Note, "logical")
  expect_type(guessed$Unit, "logical")

  typed <- readr::read_csv(
    path,
    col_types = env$faostat_bulk_col_types(),
    show_col_types = FALSE
  )
  expect_type(typed$Note, "character")
  expect_type(typed$Unit, "character")
  expect_equal(unique(typed$Unit), "t")
  expect_type(typed$Flag, "character")
  expect_type(typed$`Item Code (CPC)`, "character")
  expect_type(typed$Value, "double")
})

test_that("no column of a bulk CSV is read as logical under the spec", {
  env <- .faostat_bulk_env()
  typed <- readr::read_csv(
    .faostat_bulk_csv(),
    col_types = env$faostat_bulk_col_types(),
    show_col_types = FALSE
  )
  logical_cols <- names(typed)[purrr::map_lgl(typed, is.logical)]
  expect_equal(logical_cols, character())
})

test_that("both pins with a logical Note have a producer (whep#1178)", {
  # faostat-landuse had none, so nothing could apply the spec to it.
  env <- .faostat_bulk_env()
  expect_contains(
    env$FAOSTAT_BULK_DOMAINS$alias,
    c("faostat-fbs-new", "faostat-landuse")
  )
})

test_that("every producer alias is a registered pin", {
  env <- .faostat_bulk_env()
  expect_contains(whep::whep_inputs$alias, env$FAOSTAT_BULK_DOMAINS$alias)
})

test_that("the RL domain resolves to FAO's land-use bulk archive", {
  env <- .faostat_bulk_env()
  rl <- dplyr::filter(
    env$FAOSTAT_BULK_DOMAINS,
    .data$alias == "faostat-landuse"
  )
  expect_equal(rl$domain, "RL")
  expect_equal(rl$archive, "Inputs_LandUse_E_All_Data_(Normalized).zip")
})
