# The LPJmL per-CFT band vocabulary, transcribed from the 2026-09-01 v2 run's
# NamePFT variables into inst/extdata/lpjml_cft_bands.csv so that band ->
# regime/group mapping and its tests need no run directory. Nothing here
# reaches a WHEP_* path.

.cft_bands <- function() {
  path <- system.file("extdata", "lpjml_cft_bands.csv", package = "whep")
  if (!nzchar(path) || !file.exists(path)) {
    path <- testthat::test_path(
      "..",
      "..",
      "inst",
      "extdata",
      "lpjml_cft_bands.csv"
    )
  }
  testthat::expect_true(file.exists(path))
  utils::read.csv(path, stringsAsFactors = FALSE)
}

testthat::test_that("cftfrac carries 16 rainfed and 16 irrigated bands, by name", {
  b <- .cft_bands()
  cf <- b[b$output == "cftfrac", ]
  testthat::expect_identical(nrow(cf), 32L)
  testthat::expect_identical(as.vector(table(cf$regime)), c(16L, 16L))
  testthat::expect_identical(cf$band, 1:32)
  # Every band name is unique: the readers select by name, never by index.
  testthat::expect_identical(anyDuplicated(cf$band_name), 0L)
  # The regime is always the first word, so it can be parsed rather than
  # looked up.
  testthat::expect_true(all(grepl("^(rainfed|irrigated) ", cf$band_name)))
})

testthat::test_that("the crop calendar covers 12 crops x 2 regimes, all in cftfrac", {
  b <- .cft_bands()
  sd <- b[b$output == "sdate", ]
  cf <- b[b$output == "cftfrac", ]
  testthat::expect_identical(nrow(sd), 24L)
  testthat::expect_identical(length(unique(sd$crop)), 12L)
  testthat::expect_true(all(sd$band_name %in% cf$band_name))
  # And the two "others" bands have no calendar, which is why 1.2% of
  # cropped area stays on the curve.
  testthat::expect_false(any(grepl("others", sd$band_name)))
  # Index alignment holds for only 12 of 24: this is the trap the readers
  # avoid by matching on name.
  aligned <- sum(sd$band_name == cf$band_name[sd$band])
  testthat::expect_identical(aligned, 12L)
})

testthat::test_that("the natural-PFT vocabulary of fpc and pft_npp agree", {
  b <- .cft_bands()
  fpc <- b[b$output == "fpc", ]
  npp <- b[b$output == "pft_npp", ]
  testthat::expect_identical(nrow(fpc), 15L)
  testthat::expect_identical(nrow(npp), 46L)
  # fpc band 1 is the stand fraction; bands 2-15 are the 14 natural PFTs,
  # every one of which pft_npp also carries.
  testthat::expect_identical(fpc$band_name[1], "natural stand fraction")
  testthat::expect_true(all(fpc$band_name[-1] %in% npp$band_name))
})
