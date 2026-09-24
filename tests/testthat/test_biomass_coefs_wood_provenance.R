# Provenance of the wood Residue_kgN_kgDM coefficients (whep#932).
#
# The upstream workbook's own citations for these cells cannot be opened: the
# European beech anchor's source entry begins "ChatGPT.", the conifer anchor
# cites two papers that do not resolve as written, and the holm-oak anchor's
# source cell is blank. So the shipped numbers are assumed, unverified in
# their own right, and the only way to say anything about them is to place
# them against a source that CAN be opened.
#
# That source is transcribed below rather than fetched, following the idiom of
# test_residue_recovery_provenance.R: the published medians are constants in
# this file, the shipped coefficients are read live from whep::biomass_coefs,
# and the comparison between them is what the assertions pin. Either side
# drifting is a failure, which is the point -- nothing here can move quietly.
#
# Thurner, M., Yu, K., Manzoni, S., Prokushkin, A., Thurner, M. A., Wang, Z.,
# and Hickler, T. (2025). Nitrogen concentrations in boreal and temperate tree
# tissues vary with tree age/size, growth rate, and climate. Biogeosciences
# 22(5), 1475-1493. doi:10.5194/bg-22-1475-2025 (open access, CC-BY-4.0).
# Section 3, p. 1480, and Sect. 3.2, p. 1481. Underlying measurements archived
# at doi:10.5281/zenodo.14742947.

# Table 1 of this file: the medians Thurner et al. (2025) report, in kg N per
# kg dry matter, with the number of measurements behind each. "Stem" is stem
# sapwood -- where a study separates sapwood from heartwood the compilation
# keeps sapwood only (p. 1479), and heartwood runs lower, so these stem
# figures are if anything an upper bound on whole-stem concentration.
.bcw_tissue_medians <- function() {
  tibble::tribble(
    ~tissue,  ~median_kg_n_kg_dm, ~measurements,
    "leaf",   0.0167,             5944L,
    "root",   0.0060,             267L,
    "branch", 0.0035,             599L,
    "stem",   0.0010,             1048L
  )
}

# Table 2: the same medians split by leaf type, p. 1481. BD is broadleaf
# deciduous, NE needleleaf evergreen. These are what place the beech and
# conifer anchors specifically, rather than against a pooled median.
.bcw_medians_by_leaf_type <- function() {
  tibble::tribble(
    ~tissue,  ~broadleaf_decid, ~needleleaf_evergr,
    "leaf",   0.0222,           0.0124,
    "branch", 0.0042,           0.0030,
    "stem",   0.0017,           0.0008,
    "root",   0.0064,           0.0038
  )
}

.bcw_wood_rows <- function() {
  c(
    "European beech",
    "Spruce",
    "Poplar",
    "Willow, Sallow",
    "Eucalyptus",
    "Conifers",
    "Bark (conifers)",
    "Bark (broad-leaved tree)",
    "Broad-leaved tree",
    "Temperate wood",
    "Tropical wood",
    "Average wood",
    "Holm oak forest",
    "Mediterranean shrubland",
    "Shrubland",
    "Savanna",
    "Paper",
    "Charcoal"
  )
}

.bcw_shipped <- function(name) {
  whep::biomass_coefs |>
    dplyr::filter(.data$Name_biomass == name) |>
    dplyr::pull("Residue_kgN_kgDM")
}

.bcw_median <- function(tissue) {
  .bcw_tissue_medians() |>
    dplyr::filter(.data$tissue == !!tissue) |>
    dplyr::pull("median_kg_n_kg_dm")
}

testthat::test_that("the transcribed compilation is internally consistent", {
  # Non-vacuity, in the only form available for a transcription: the totals
  # the paper states about its own database have to add up, and the tissue
  # ordering it reports (leaf > root > branch > stem) has to hold. A typo in
  # one of the constants below shows up here rather than silently loosening
  # every comparison that follows.
  medians <- .bcw_tissue_medians()
  testthat::expect_equal(sum(medians$measurements), 7858L)
  testthat::expect_equal(
    medians$tissue[order(medians$median_kg_n_kg_dm, decreasing = TRUE)],
    c("leaf", "root", "branch", "stem")
  )
  # Every pooled median sits between its own broadleaf and needleleaf values,
  # which it must if it is a median over a mixture of the two.
  by_type <- .bcw_medians_by_leaf_type() |>
    dplyr::inner_join(medians, by = "tissue")
  testthat::expect_equal(nrow(by_type), 4L)
  testthat::expect_true(all(
    by_type$median_kg_n_kg_dm >= by_type$needleleaf_evergr &
      by_type$median_kg_n_kg_dm <= by_type$broadleaf_decid
  ))
  # Branch nitrogen is 3.5x stem nitrogen. That ratio, not any single value,
  # is what whep#932's "3.2x between sources" actually is.
  testthat::expect_equal(
    .bcw_median("branch") / .bcw_median("stem"),
    3.5,
    tolerance = 1e-9
  )
})

testthat::test_that("the shipped wood coefficient is a branch value", {
  # The finding this file exists for. `Average wood` is what both wood items
  # read, and it lands on the branch median, not the stem one -- so it prices
  # forest residue about right and harvested roundwood about 3x too high.
  average_wood <- .bcw_shipped("Average wood")
  testthat::expect_equal(average_wood, 0.0030)
  testthat::expect_equal(
    average_wood / .bcw_median("branch"),
    0.857,
    tolerance = 1e-3
  )
  testthat::expect_equal(
    average_wood / .bcw_median("stem"),
    3.0,
    tolerance = 1e-9
  )
  # And the value the retired biomass_coefs pin carried for the same 18 rows
  # is the stem median, within 5%. The two candidates whep#932 sets against
  # each other are a stem number and a branch number, not two readings of one
  # quantity. 0.00095 is recorded here as history, not as a shipped value.
  retired_pin_value <- 0.00095
  testthat::expect_equal(
    retired_pin_value / .bcw_median("stem"),
    0.95,
    tolerance = 1e-9
  )
  testthat::expect_equal(
    average_wood / retired_pin_value,
    3.158,
    tolerance = 1e-3
  )
})

testthat::test_that("each anchor is classified against its own leaf type", {
  # The three anchors `Average wood` averages do not agree about which tissue
  # they describe, which is why their mean is not a coefficient for anything
  # in particular. Beech is a stem value, the conifer anchor is a branch
  # value, and the holm-oak anchor is above even the broadleaf branch median.
  by_type <- .bcw_medians_by_leaf_type()
  bd_stem <- by_type$broadleaf_decid[by_type$tissue == "stem"]
  bd_branch <- by_type$broadleaf_decid[by_type$tissue == "branch"]
  ne_stem <- by_type$needleleaf_evergr[by_type$tissue == "stem"]
  ne_branch <- by_type$needleleaf_evergr[by_type$tissue == "branch"]

  beech <- .bcw_shipped("European beech")
  testthat::expect_equal(beech, 0.0010)
  testthat::expect_lt(abs(beech - bd_stem), abs(beech - bd_branch))

  conifers <- .bcw_shipped("Conifers")
  testthat::expect_equal(conifers, 0.0035)
  testthat::expect_lt(abs(conifers - ne_branch), abs(conifers - ne_stem))
  testthat::expect_equal(conifers / ne_stem, 4.375, tolerance = 1e-9)

  holm_oak <- .bcw_shipped("Holm oak forest")
  testthat::expect_equal(holm_oak, 0.0045)
  testthat::expect_gt(holm_oak, bd_branch)
  testthat::expect_equal(holm_oak / bd_stem, 2.647, tolerance = 1e-3)
})

testthat::test_that("the comparison is reading real shipped coefficients", {
  # Guard against the whole file passing on constants. Every assertion above
  # compares a transcribed median with a number read live from
  # whep::biomass_coefs, and that read has to reach all 18 rows, all
  # populated, and give the three anchors three different values. The package
  # exports environment resolves before the namespace, so this cannot be
  # stubbed; a wrong expectation is the only way to prove the read is live.
  wood <- whep::biomass_coefs |>
    dplyr::filter(.data$Name_biomass %in% .bcw_wood_rows())
  testthat::expect_equal(nrow(wood), length(.bcw_wood_rows()))
  testthat::expect_false(any(is.na(wood$Residue_kgN_kgDM)))
  anchors <- c(
    .bcw_shipped("European beech"),
    .bcw_shipped("Conifers"),
    .bcw_shipped("Holm oak forest")
  )
  testthat::expect_equal(length(unique(anchors)), 3L)
  testthat::expect_failure(
    testthat::expect_equal(.bcw_shipped("Average wood"), .bcw_median("stem"))
  )
})

testthat::test_that("the unsourced wood anchors are listed, not forgotten", {
  # The closed vocabulary. These three rows are the whole of what carries
  # nitrogen into `Average wood`, and none of them has a citation that
  # resolves. Removing a name from this list is a claim that its provenance
  # was found, and has to be made deliberately.
  unsourced <- tibble::tribble(
    ~Name_biomass,     ~upstream_provenance,
    "European beech",  "source cell begins 'ChatGPT.'",
    "Conifers",        "two citations do not resolve as written",
    "Holm oak forest", "source cell is blank"
  )
  testthat::expect_equal(nrow(unsourced), 3L)
  testthat::expect_true(all(
    unsourced$Name_biomass %in% whep::biomass_coefs$Name_biomass
  ))
  # `Average wood` is the mean of exactly these three and nothing else, so
  # every wood row in the table inherits all three unsourced anchors.
  testthat::expect_equal(
    mean(purrr::map_dbl(unsourced$Name_biomass, .bcw_shipped)),
    .bcw_shipped("Average wood"),
    tolerance = 1e-12
  )
  # The 15 rows that are not anchors all read that mean back, so a change to
  # any one anchor moves all 18.
  followers <- setdiff(.bcw_wood_rows(), unsourced$Name_biomass)
  testthat::expect_equal(length(followers), 15L)
  testthat::expect_setequal(
    unique(purrr::map_dbl(followers, .bcw_shipped)),
    .bcw_shipped("Average wood")
  )
})
