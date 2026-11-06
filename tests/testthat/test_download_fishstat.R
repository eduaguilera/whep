# Developer scripts are deliberately excluded from built packages. Their
# offline archive fixture runs from the source checkout, like the preparers.
testthat::test_that("FishStat acquisition verifies bytes and refuses overwrite", {
  script <- testthat::test_path(
    "..",
    "..",
    "inst",
    "scripts",
    "download",
    "download_fishstat.R"
  )
  testthat::skip_if_not(file.exists(script), "Developer script not in tarball")
  testthat::skip_if(Sys.which("zip") == "", "zip utility unavailable")
  env <- new.env(parent = baseenv())
  sys.source(script, envir = env)
  root <- withr::local_tempdir()
  withr::local_dir(root)
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
  purrr::walk(files, writeLines, text = "offline fixture")
  utils::zip("fixture.zip", files, flags = "-q")
  checksum <- unname(tools::md5sum("fixture.zip"))
  testthat::expect_error(
    env$.extract_fishstat_archive("fixture.zip", "bad-hash", "wrong"),
    "checksum differs"
  )
  testthat::expect_false(dir.exists("bad-hash"))
  env$.extract_fishstat_archive("fixture.zip", "valid", checksum)
  testthat::expect_setequal(list.files("valid"), files)
  testthat::expect_equal(
    readLines("valid/TRADE_QUANTITY.csv"),
    "offline fixture"
  )
  testthat::expect_error(
    env$.extract_fishstat_archive("fixture.zip", "valid", checksum),
    "Refusing to overwrite"
  )
  utils::zip("incomplete.zip", files[-1], flags = "-q")
  testthat::expect_error(
    env$.extract_fishstat_archive(
      "incomplete.zip",
      "incomplete",
      unname(tools::md5sum("incomplete.zip"))
    ),
    "missing required data or provenance"
  )
  dir.create("FI_Trade_2025.1.0")
  testthat::expect_error(env$download_fishstat("."), "Refusing to overwrite")
})
