# The two facts about which FAOSTAT areas resolve to nothing on purpose are UPSTREAM's, published in the
# manifest as `faostat_unmapped_areas`, and embedded here by data-raw/constants.R because the manifest is
# not a package resource.
#
# They were hardcoded in this package until upstream published them: the 5000 group threshold measured
# against real production — 34 of 34 unmapped codes are >= 5000, a number upstream already knew — and
# "deliberate" inferred from crosswalk membership. Inference cannot distinguish a decision from an
# absence, which is how a warning here came to report FAOSTAT 351 "China" as an area code nobody knows.
#
# Embedding reintroduces the drift risk that copying always does, so this compares the copy against the
# manifest whenever one is reachable, and asserts the embedded values are usable when it is not.

testthat::test_that("the embedded FAOSTAT constants are present and sane", {
  # Runs everywhere, including CI, because it needs nothing outside the package.
  testthat::expect_true(is.numeric(whep:::faostat_group_code_min))
  testthat::expect_length(whep:::faostat_group_code_min, 1L)
  testthat::expect_gt(whep:::faostat_group_code_min, 1000L)

  testthat::expect_true(is.numeric(whep:::faostat_deliberate_area_codes))
  testthat::expect_gte(length(whep:::faostat_deliberate_area_codes), 1L)
  # 351 China is the case the whole mechanism exists for; losing it would be silent.
  testthat::expect_true(351L %in% whep:::faostat_deliberate_area_codes)

  # No modelled area may sit at or above the threshold, or a real territory would be dismissed as an
  # aggregate. Asserted here too, since this is now where the threshold enters the package.
  modelled <- unique(stats::na.omit(
    as.data.frame(whep::polity_area_crosswalk)$area_code
  ))
  testthat::expect_false(any(modelled >= whep:::faostat_group_code_min))
})

testthat::test_that("the embedded constants match the upstream manifest", {
  path <- Sys.getenv(
    "WHEP_POLITIES_MANIFEST",
    unset = path.expand("~/whep-polities/data/final/polities_manifest.json")
  )
  testthat::skip_if_not(
    file.exists(path),
    paste0("upstream manifest not found at ", path)
  )
  manifest <- jsonlite::fromJSON(path, simplifyVector = TRUE)
  published <- manifest$faostat_unmapped_areas
  testthat::skip_if(
    is.null(published$group_code_min),
    "manifest predates faostat_unmapped_areas"
  )

  testthat::expect_equal(
    as.integer(whep:::faostat_group_code_min),
    as.integer(published$group_code_min),
    info = "R/sysdata.rda is stale — rerun data-raw/constants.R"
  )
  testthat::expect_setequal(
    as.integer(whep:::faostat_deliberate_area_codes),
    as.integer(published$deliberate_area_codes)
  )
})
