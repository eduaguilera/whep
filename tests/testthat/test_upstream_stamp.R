# Does the embedded data correspond to the upstream state it was built from?
#
# The contract tests in test_polities_upstream_contract.R compare the embedded copies against
# the real upstream files, field by field, which is the strongest check available -- and they
# SKIP on every CI run, because whep-polities is private. So on a runner nothing linked
# `data/*.rda` to any known upstream state, and a rebuild against a different upstream than
# `data-raw/constants.R` read would pass the whole suite.
#
# That failure is not hypothetical. This branch twice shipped two representations of one
# decision with only one of them rebuilt: `regions_full` kept a withdrawn `polity_area_code`
# after `polity_area_crosswalk` had been refolded, and the embedded copy of the database had
# historically drifted to 603 rows against upstream's 740.
#
# `inst/extdata/upstream_stamp.json` is written by `data-raw/constants.R` -- the one data-raw
# script that already requires the manifest, so it cannot record a version it did not read.
# These checks run everywhere, with no upstream needed, and the last one tightens to a direct
# comparison when the manifest IS reachable.

.read_stamp <- function() {
  path <- system.file("extdata", "upstream_stamp.json", package = "whep")
  if (path == "") {
    return(NULL)
  }
  jsonlite::fromJSON(path, simplifyVector = TRUE)
}

testthat::test_that("the upstream stamp is present and well formed", {
  stamp <- .read_stamp()
  testthat::skip_if(
    is.null(stamp),
    "upstream_stamp.json not installed; regenerate with data-raw/constants.R"
  )

  # A 64-hex digest, not merely a non-empty string: a truncated or placeholder value would
  # otherwise satisfy every comparison below while identifying nothing.
  for (field in c(
    "identity_sha256",
    "alias_map_sha256",
    "faostat_area_map_sha256"
  )) {
    testthat::expect_match(
      stamp[[field]],
      "^[0-9a-f]{64}$",
      info = paste(field, "is not a sha256 digest")
    )
  }
  testthat::expect_true(all(
    c("total", "live", "dead") %in% names(stamp$counts)
  ))
  testthat::expect_gt(stamp$counts$total, 500L)
  testthat::expect_equal(
    stamp$counts$total,
    stamp$counts$live + stamp$counts$dead
  )
})

testthat::test_that("the embedded polities match the stamped upstream counts", {
  stamp <- .read_stamp()
  testthat::skip_if(is.null(stamp), "upstream_stamp.json not installed")

  pol <- as.data.frame(whep::polities)
  testthat::expect_equal(nrow(pol), stamp$counts$total)

  # And the live/dead split, because a row count alone cannot see a status flip -- which is
  # exactly how a changed `wiki_status` slipped past the other contract checks once: the row
  # count, the code set and the dead-routing test were all still satisfied.
  dead_status <- c("retired", "superseded")
  if ("wiki_status" %in% names(pol)) {
    dead <- sum(pol$wiki_status %in% dead_status)
    testthat::expect_equal(dead, stamp$counts$dead)
    testthat::expect_equal(nrow(pol) - dead, stamp$counts$live)
  }
})

testthat::test_that("the stamp matches the manifest when the manifest is reachable", {
  # Locally this is the check that matters: it catches a STALE stamp, which the checks above
  # cannot. A stamp recorded from an older upstream stays internally consistent forever.
  stamp <- .read_stamp()
  testthat::skip_if(is.null(stamp), "upstream_stamp.json not installed")

  path <- Sys.getenv("WHEP_POLITIES_MANIFEST", unset = "")
  testthat::skip_if(
    path == "" || !file.exists(path),
    "upstream manifest not reachable; set WHEP_POLITIES_MANIFEST"
  )
  mf <- jsonlite::fromJSON(path, simplifyVector = TRUE)

  testthat::expect_equal(
    stamp$identity_sha256,
    mf$identity_sha256,
    info = paste(
      "the stamp was recorded from a different upstream state than the manifest now",
      "describes -- re-run data-raw/constants.R and rebuild data/"
    )
  )
  testthat::expect_equal(stamp$counts$total, mf$counts$total)
  testthat::expect_equal(
    stamp$alias_map_sha256,
    mf$label_alias_map$sha256
  )
})
