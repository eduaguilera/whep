# The repo .Rprofile attaches the package on session start. CI installs
# dependencies by running Rscript with the repo root as the working directory,
# so an unguarded load in that file aborts the session before the missing
# Import can be installed, failing every workflow (#616). These tests drive a
# real child R session against a throwaway package whose only Import does not
# exist, which is exactly the state CI is in when a new Import lands.
#
# Every assertion here depends on a premise the test does not control: that a
# spawned session can load devtools, and that the load really does abort on a
# missing Import. Neither holds everywhere. Under `R CMD check` the tests run
# with the library farm `tools:::setRlibs()` builds, which carries only the
# package's declared dependency closure plus `R_LIBS_USER='NULL'`,
# `R_LIBS_SITE='NULL'` and `R_ENVIRON_USER=''`; whep declares neither devtools
# nor pkgload, so the developer's library is simply not on the child's path.
# Inferring "the load aborted" from the absence of the sentinel silently turned
# that into the opposite claim (#1009), so each premise is now measured and
# reported as a skip instead of being assumed.

.fake_missing_import <- function() {
  "whepnonexistentimportxyz"
}

.write_fake_pkg <- function(dir) {
  writeLines(
    c(
      "Package: whepfakepkg",
      "Title: Throwaway Package",
      "Version: 0.0.0.9000",
      "Description: Throwaway package used to test the repo .Rprofile.",
      "Imports:",
      paste0("    ", .fake_missing_import()),
      "Encoding: UTF-8"
    ),
    file.path(dir, "DESCRIPTION")
  )
  writeLines("exportPattern(\"^[[:alpha:]]+\")", file.path(dir, "NAMESPACE"))
  dir.create(file.path(dir, "R"))
  writeLines("whep_fake_fun <- function() TRUE", file.path(dir, "R", "fake.R"))
  invisible(dir)
}

# Printed by the child from inside the profile, before anything can abort, so
# a session that never read the profile is distinguishable from one that read
# it and died.
.profile_read_marker <- function() {
  "WHEP-PROFILE-READ"
}

.devtools_gone_marker <- function() {
  "WHEP-DEVTOOLS-UNAVAILABLE"
}

# The repo .Rprofile without its tryCatch: the state #616 was reported in.
.unguarded_profile_lines <- function() {
  c(
    paste0("cat(\"", .profile_read_marker(), "\\n\")"),
    "if (requireNamespace(\"devtools\", quietly = TRUE)) {",
    "  devtools::load_all()",
    "} else {",
    paste0("  cat(\"", .devtools_gone_marker(), "\\n\")"),
    "}"
  )
}

# The child session is pointed at the profile explicitly with
# R_PROFILE_USER rather than relying on the working directory, so the test
# cannot be perturbed by whatever profile settings the parent session
# inherited. Its working directory is still `dir`, because that is what the
# load reads. The whole of stdout and stderr comes back, so a failure can say
# what the child did instead of only that it did something.
# `system2(env=)` is documented as unsupported on Windows (see `?system2`) and
# is silently a no-op there: the child never received `R_PROFILE_USER`, never
# loaded the profile under test, and every assertion below failed on Windows
# while passing on the Linux runners. Setting the variable in this process and
# restoring it afterwards works on every platform.
.set_r_profile_user <- function(profile) {
  previous <- Sys.getenv("R_PROFILE_USER", unset = NA)
  Sys.setenv(R_PROFILE_USER = profile)
  previous
}

.restore_r_profile_user <- function(previous) {
  if (is.na(previous)) {
    Sys.unsetenv("R_PROFILE_USER")
  } else {
    Sys.setenv(R_PROFILE_USER = previous)
  }
  invisible(NULL)
}

.run_rscript_sentinel <- function(profile) {
  previous <- .set_r_profile_user(profile)
  on.exit(.restore_r_profile_user(previous), add = TRUE)
  suppressWarnings(system2(
    file.path(R.home("bin"), "Rscript"),
    c("-e", shQuote("cat('SENTINEL-REACHED\\n')")),
    stdout = TRUE,
    stderr = TRUE
  ))
}

# Five outcomes the sentinel on its own cannot tell apart. The first two are
# broken premises, not results: the profile was never read, or devtools was
# unreachable and the load never happened.
.classify_rprofile_child <- function(out) {
  if (!any(grepl(.profile_read_marker(), out, fixed = TRUE))) {
    return("profile-not-read")
  }
  if (any(grepl(.devtools_gone_marker(), out, fixed = TRUE))) {
    return("devtools-unavailable")
  }
  if (any(grepl("SENTINEL-REACHED", out, fixed = TRUE))) {
    return("survived")
  }
  if (any(grepl(.fake_missing_import(), out, fixed = TRUE))) {
    return("aborted-on-missing-import")
  }
  "aborted-for-another-reason"
}

# A spawned session, not this one: the parent may hold a devtools this child
# cannot reach, which is what makes the surfaces disagree. R_PROFILE_USER is
# blanked so the repo profile cannot answer the question for the child.
.child_can_load_devtools <- function() {
  out <- suppressWarnings(system2(
    file.path(R.home("bin"), "Rscript"),
    c("-e", shQuote("cat(requireNamespace('devtools', quietly = TRUE))")),
    stdout = TRUE,
    stderr = TRUE,
    env = "R_PROFILE_USER="
  ))
  any(grepl("TRUE", out, fixed = TRUE))
}

# The premise both guards below rest on, measured on this surface.
.unguarded_load_outcome <- function() {
  dir <- tempfile("whep-rprofile-")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  .write_fake_pkg(dir)
  profile <- file.path(dir, ".Rprofile")
  writeLines(.unguarded_profile_lines(), profile)
  out <- withr::with_dir(dir, .run_rscript_sentinel(profile))
  list(outcome = .classify_rprofile_child(out), out = out)
}

.repo_rprofile_path <- function() {
  testthat::test_path("..", "..", ".Rprofile")
}

test_that("the child outcome is named, not inferred from one string", {
  read <- .profile_read_marker()
  gone <- .devtools_gone_marker()

  expect_equal(.classify_rprofile_child(character()), "profile-not-read")
  expect_equal(
    .classify_rprofile_child("SENTINEL-REACHED"),
    "profile-not-read"
  )
  expect_equal(
    .classify_rprofile_child(c(read, gone, "SENTINEL-REACHED")),
    "devtools-unavailable"
  )
  expect_equal(
    .classify_rprofile_child(c(read, "SENTINEL-REACHED")),
    "survived"
  )
  expect_equal(
    .classify_rprofile_child(c(read, "Error: whepnonexistentimportxyz")),
    "aborted-on-missing-import"
  )
  expect_equal(
    .classify_rprofile_child(c(read, "Error: something else entirely")),
    "aborted-for-another-reason"
  )
})

test_that("an unguarded load_all() kills the session on a missing Import", {
  child <- .unguarded_load_outcome()
  skip_if(
    child$outcome %in%
      c("profile-not-read", "devtools-unavailable", "survived"),
    paste0(
      "an unguarded load does not abort here (",
      child$outcome,
      "): ",
      paste(child$out, collapse = " | ")
    )
  )

  expect_equal(
    child$outcome,
    "aborted-on-missing-import",
    info = paste(child$out, collapse = "\n")
  )
})

test_that("the repo .Rprofile survives a missing Import and says so", {
  repo_rprofile <- .repo_rprofile_path()
  skip_if_not(
    file.exists(repo_rprofile),
    "repo .Rprofile is not part of an installed package"
  )
  skip_if_not(
    .child_can_load_devtools(),
    "{devtools} cannot be loaded in a spawned session"
  )
  premise <- .unguarded_load_outcome()
  skip_if_not(
    premise$outcome == "aborted-on-missing-import",
    paste0(
      "nothing to survive here (",
      premise$outcome,
      "): ",
      paste(premise$out, collapse = " | ")
    )
  )

  dir <- withr::local_tempdir()
  .write_fake_pkg(dir)
  file.copy(repo_rprofile, file.path(dir, ".Rprofile"))

  withr::local_dir(dir)
  out <- .run_rscript_sentinel(file.path(dir, ".Rprofile"))

  expect_true(
    any(grepl("SENTINEL-REACHED", out, fixed = TRUE)),
    info = paste(out, collapse = "\n")
  )
  expect_true(
    any(grepl("package NOT loaded", out, fixed = TRUE)),
    info = paste(out, collapse = "\n")
  )
})

test_that("the repo .Rprofile still sets the R CMD check clock variable", {
  repo_rprofile <- .repo_rprofile_path()
  skip_if_not(
    file.exists(repo_rprofile),
    "repo .Rprofile is not part of an installed package"
  )

  dir <- withr::local_tempdir()
  .write_fake_pkg(dir)
  file.copy(repo_rprofile, file.path(dir, ".Rprofile"))

  withr::local_dir(dir)
  previous <- .set_r_profile_user(file.path(dir, ".Rprofile"))
  on.exit(.restore_r_profile_user(previous), add = TRUE)
  out <- suppressWarnings(system2(
    file.path(R.home("bin"), "Rscript"),
    c("-e", shQuote("cat(Sys.getenv('_R_CHECK_SYSTEM_CLOCK_'), '\\n')")),
    stdout = TRUE,
    stderr = TRUE
  ))

  expect_true(any(grepl("^0", trimws(out))), info = paste(out, collapse = "\n"))
})
