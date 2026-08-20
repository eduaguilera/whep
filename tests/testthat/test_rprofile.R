# The repo .Rprofile attaches the package on session start. CI installs
# dependencies by running Rscript with the repo root as the working directory,
# so an unguarded devtools::load_all() aborts the session before the missing
# Import can be installed, failing every workflow (#616). These tests drive a
# real child R session against a throwaway package whose only Import does not
# exist, which is exactly the state CI is in when a new Import lands.

.write_fake_pkg <- function(dir) {
  writeLines(
    c(
      "Package: whepfakepkg",
      "Title: Throwaway Package",
      "Version: 0.0.0.9000",
      "Description: Throwaway package used to test the repo .Rprofile.",
      "Imports:",
      "    whepnonexistentimportxyz",
      "Encoding: UTF-8"
    ),
    file.path(dir, "DESCRIPTION")
  )
  writeLines("exportPattern(\"^[[:alpha:]]+\")", file.path(dir, "NAMESPACE"))
  dir.create(file.path(dir, "R"))
  writeLines("whep_fake_fun <- function() TRUE", file.path(dir, "R", "fake.R"))
  invisible(dir)
}

# The child session is pointed at the profile explicitly with
# R_PROFILE_USER rather than relying on the working directory, so the test
# cannot be perturbed by whatever profile settings the parent session
# inherited. Its working directory is still `dir`, because that is what
# load_all() reads.
.run_rscript_sentinel <- function(profile) {
  suppressWarnings(system2(
    file.path(R.home("bin"), "Rscript"),
    c("-e", shQuote("cat('SENTINEL-REACHED\\n')")),
    stdout = TRUE,
    stderr = TRUE,
    env = paste0("R_PROFILE_USER=", profile)
  ))
}

test_that("an unguarded load_all() kills the session on a missing Import", {
  skip_if_not_installed("devtools")
  dir <- withr::local_tempdir()
  .write_fake_pkg(dir)
  writeLines(
    "if (requireNamespace(\"devtools\", quietly = TRUE)) devtools::load_all()",
    file.path(dir, ".Rprofile")
  )

  withr::local_dir(dir)
  out <- .run_rscript_sentinel(file.path(dir, ".Rprofile"))

  expect_false(any(grepl("SENTINEL-REACHED", out, fixed = TRUE)))
})

test_that("the repo .Rprofile survives a missing Import and says so", {
  skip_if_not_installed("devtools")
  repo_rprofile <- testthat::test_path("..", "..", ".Rprofile")
  skip_if_not(
    file.exists(repo_rprofile),
    "repo .Rprofile is not part of an installed package"
  )

  dir <- withr::local_tempdir()
  .write_fake_pkg(dir)
  file.copy(repo_rprofile, file.path(dir, ".Rprofile"))

  withr::local_dir(dir)
  out <- .run_rscript_sentinel(file.path(dir, ".Rprofile"))

  expect_true(any(grepl("SENTINEL-REACHED", out, fixed = TRUE)))
  expect_true(any(grepl("package NOT loaded", out, fixed = TRUE)))
})

test_that("the repo .Rprofile still sets the R CMD check clock variable", {
  repo_rprofile <- testthat::test_path("..", "..", ".Rprofile")
  skip_if_not(
    file.exists(repo_rprofile),
    "repo .Rprofile is not part of an installed package"
  )

  dir <- withr::local_tempdir()
  .write_fake_pkg(dir)
  file.copy(repo_rprofile, file.path(dir, ".Rprofile"))

  withr::local_dir(dir)
  out <- suppressWarnings(system2(
    file.path(R.home("bin"), "Rscript"),
    c("-e", shQuote("cat(Sys.getenv('_R_CHECK_SYSTEM_CLOCK_'), '\\n')")),
    stdout = TRUE,
    stderr = TRUE,
    env = paste0("R_PROFILE_USER=", file.path(dir, ".Rprofile"))
  ))

  expect_true(any(grepl("^0", trimws(out))))
})
