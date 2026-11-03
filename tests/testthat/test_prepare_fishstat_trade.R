testthat::test_that("raw preparer requires verified inputs and a new directory", {
  script <- testthat::test_path(
    "..",
    "..",
    "inst",
    "scripts",
    "prepare_fishstat_trade.R"
  )
  testthat::skip_if_not(file.exists(script), "Developer script not in tarball")
  env <- new.env(parent = baseenv())
  sys.source(script, envir = env)
  root <- withr::local_tempdir()
  testthat::expect_error(
    env$prepare_fishstat_candidate(root, root),
    "Refusing to overwrite"
  )
  testthat::expect_error(
    env$.fishstat_verified_inputs(root),
    "Missing FishStat source"
  )
  files <- c(
    "TRADE_QUANTITY.csv",
    "CL_FI_COMMODITY_ISSCFC.csv",
    "CL_FI_COUNTRY_GROUPS.csv",
    "FSJ_UNIT.csv",
    "ReadMe.txt",
    "ISSCAAP.pdf",
    "ISSCFC.pdf",
    "Trade_countries_notes.pdf",
    "Trade_E.pdf"
  )
  purrr::walk(file.path(root, files), writeLines, text = "offline fixture")
  testthat::expect_error(
    env$.fishstat_verified_inputs(root),
    "differ from the verified"
  )
})
