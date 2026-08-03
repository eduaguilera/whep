# Source-value assertions for the BNF coefficient registry
# (`inst/extdata/coefs/bnf.csv`).
#
# `test_coef_integrity.R` checks structure and physical range, which by design
# survives any legitimate coefficient update. That is not enough to catch a
# value mis-read from its source: a sample size transcribed into an index
# column is structurally valid and physically in range, and a test that
# compares the table against a copy of itself passes on any transcription
# error. These tests close that gap by asserting stored values against the
# publications the registry cites, transcribed here together with the table
# that carries them.
#
# Every non-missing numeric cell of `bnf.csv` sits in exactly one of three
# buckets, and the coverage test below fails if a cell escapes all three:
#   source      asserted against an external cited publication
#   derivation  asserted against the registry's own declared derivation rule
#   unasserted  declared here, with the reason it cannot be checked
#
# The ledgers are `tribble`s rather than parallel vectors so that each
# assertion is one readable row. A value and its citation split across two
# position-aligned vectors can slip against each other silently, since the
# citation is only ever read back in a failure message -- the same class of
# misalignment this file exists to catch.

# ---- Anglade et al. (2015), Ecosphere 6(3):37 -------------------------------
# Anglade, J., G. Billen, and J. Garnier. 2015. Relationships for estimating
# N2 fixation in legumes: incidence for N balance of legume-based cropping
# systems in Europe. Ecosphere 6(3):37. doi:10.1890/ES14-00353.1
#
# Table 1 is six side-by-side (n, Median) blocks, each median printed above
# its interquartile range. It is transcribed in full here rather than value
# by value, so that the column alignment this file depends on is proven from
# the table's own arithmetic before any registry value is read out of it: the
# sample sizes must sum to the "All" row, and every median must fall inside
# its own interquartile range. Together those pin which element of each pair
# is the count and which is the central value, which is precisely the
# distinction a sample size transcribed into an index column destroys.
# Alfalfa and clover are forage legumes and carry no NHI.
.anglade_2015_table1 <- function() {
  tibble::tibble(
    species = c("Alfalfa", "Clover", "Faba bean", "Lentil", "Pea", "All"),
    shoot_n_pct_n = c(101, 327, 40, 48, 55, 571),
    shoot_n_pct_median = c(2.9, 3.8, 2.5, 2.2, 2.7, 3.2),
    shoot_n_pct_q1 = c(2.6, 3.1, 2.3, 1.9, 2.5, 2.6),
    shoot_n_pct_q3 = c(3.5, 4.5, 3.2, 2.4, 3, 4),
    ndfa_pct_n = c(120, 409, 91, 92, 239, 951),
    ndfa_pct_median = c(70, 86, 75, 66, 71, 78),
    ndfa_pct_q1 = c(60, 74, 68, 61, 59, 65),
    ndfa_pct_q3 = c(83, 93, 85, 74, 82, 89),
    shoot_n_kg_ha_n = c(101, 327, 82, 74, 188, 772),
    shoot_n_kg_ha_median = c(119, 112, 174, 96, 132, 120),
    shoot_n_kg_ha_q1 = c(77, 44, 113, 79, 87, 67),
    shoot_n_kg_ha_q3 = c(188, 216, 230, 130, 190, 200),
    shoot_dm_t_ha_n = c(123, 428, 42, 47, 86, 726),
    shoot_dm_t_ha_median = c(3.3, 3, 7.3, 4.5, 2.35, 3.4),
    shoot_dm_t_ha_q1 = c(2, 1.5, 4.8, 4.1, 1, 1.6),
    shoot_dm_t_ha_q3 = c(4.6, 5.6, 9, 6.6, 5.8, 6),
    shoot_n_fixed_n = c(142, 495, 82, 74, 216, 1009),
    shoot_n_fixed_median = c(70, 77, 139, 71.5, 82, 80),
    shoot_n_fixed_q1 = c(41, 38, 88, 37, 42, 42),
    shoot_n_fixed_q3 = c(121, 158, 167, 85, 121, 139),
    nhi_pct_n = c(NA, NA, 34, 55, 100, 189),
    nhi_pct_median = c(NA, NA, 74, 75, 75, 75),
    nhi_pct_q1 = c(NA, NA, 63, 65, 68, 65),
    nhi_pct_q3 = c(NA, NA, 77, 78, 82, 80)
  )
}

# Values Anglade et al. (2015) state in the body text rather than in Table 1.
# BGN: "we used mean values of BGP-N reported at maturity for grain and forage
# legumes as a multiplicative factors of N fixed in shoot, 1.3 and 1.7,
# respectively" (Discussion, belowground contributions; restated for the
# fixation equation under "Amounts of N2 fixation and N net inputs").
# NHI: "high aboveground NHI values (0.75 for grain crops and 0.9 for alfalfa)
# were assumed" (Discussion, closing the organic N balance argument).
.anglade_2015_text <- list(
  bgn_grain_legume = 1.3,
  bgn_forage_legume = 1.7,
  nhi_alfalfa = 0.9
)

.anglade_frac <- function(species, column) {
  t1 <- .anglade_2015_table1()
  t1[[column]][t1$species == species] / 100
}

.anglade_ndfa <- function(species) {
  .anglade_frac(species, "ndfa_pct_median")
}

.anglade_nhi <- function(species) {
  .anglade_frac(species, "nhi_pct_median")
}

# ---- Herridge et al. (2008), Plant and Soil 311:1-18 ------------------------
# Herridge, D. F., M. B. Peoples, and R. M. Boddey. 2008. Global inputs of
# biological nitrogen fixation in agricultural systems. Plant and Soil
# 311:1-18.
#
# Table 2, "%Ndfa average" for experiments -- not the adjacent farmers' fields
# column, which reports 36 and 58 for these same two rows. Table 6, "Rate of
# N2 fixation (kg N/ha/year)" for the non-legume systems.
.herridge_2008 <- list(
  ndfa_pct_common_bean = 40,
  ndfa_pct_soybean_groundnut = 68,
  nonsymbiotic_kg_ha_rice = 33,
  nonsymbiotic_kg_ha_sugarcane = 25
)

# ---- Ledgers ----------------------------------------------------------------
.bnf_citations <- function() {
  ang <- "Anglade et al. (2015) Ecosphere 6(3):37"
  her <- "Herridge et al. (2008) Plant Soil 311:1-18"
  list(
    t1_ndfa = paste(ang, "Table 1, Ndfa(%) median"),
    t1_nhi = paste(ang, "Table 1, NHI(%) median"),
    d_nhi_alfalfa = paste(ang, "Discussion, assumed NHI for alfalfa"),
    d_bgn_forage = paste(ang, "Discussion, BGN for forage legumes"),
    d_bgn_grain = paste(ang, "Discussion, BGN for grain legumes"),
    t2_bean = paste(her, "Table 2, common bean %Ndfa average"),
    t2_soy = paste(her, "Table 2, soybean/groundnut %Ndfa average"),
    t6_rice = paste(her, "Table 6, rice"),
    t6_cane = paste(her, "Table 6, sugarcane")
  )
}

.bnf_source_ledger <- function() {
  cite <- .bnf_citations()
  bgn_forage <- .anglade_2015_text$bgn_forage_legume
  bgn_grain <- .anglade_2015_text$bgn_grain_legume
  nhi_alfalfa <- .anglade_2015_text$nhi_alfalfa
  bean_ndfa <- .herridge_2008$ndfa_pct_common_bean / 100
  soy_ndfa <- .herridge_2008$ndfa_pct_soybean_groundnut / 100
  rice_nonsym <- .herridge_2008$nonsymbiotic_kg_ha_rice
  cane_nonsym <- .herridge_2008$nonsymbiotic_kg_ha_sugarcane

  tibble::tribble(
    ~name_bnf, ~column, ~expected, ~citation,
    "Alfalfa", "ndfa", .anglade_ndfa("Alfalfa"), cite$t1_ndfa,
    "Alfalfa", "n_harvest_index", nhi_alfalfa, cite$d_nhi_alfalfa,
    "Alfalfa", "below_ground_n_ratio", bgn_forage, cite$d_bgn_forage,
    "Clover", "ndfa", .anglade_ndfa("Clover"), cite$t1_ndfa,
    "Clover", "below_ground_n_ratio", bgn_forage, cite$d_bgn_forage,
    "Fava bean", "ndfa", .anglade_ndfa("Faba bean"), cite$t1_ndfa,
    "Fava bean", "n_harvest_index", .anglade_nhi("Faba bean"), cite$t1_nhi,
    "Fava bean", "below_ground_n_ratio", bgn_grain, cite$d_bgn_grain,
    "Lentils", "ndfa", .anglade_ndfa("Lentil"), cite$t1_ndfa,
    "Lentils", "n_harvest_index", .anglade_nhi("Lentil"), cite$t1_nhi,
    "Lentils", "below_ground_n_ratio", bgn_grain, cite$d_bgn_grain,
    "Pea", "ndfa", .anglade_ndfa("Pea"), cite$t1_ndfa,
    "Pea", "n_harvest_index", .anglade_nhi("Pea"), cite$t1_nhi,
    "Pea", "below_ground_n_ratio", bgn_grain, cite$d_bgn_grain,
    "Beans", "ndfa", bean_ndfa, cite$t2_bean,
    "Groundnuts, with shell", "ndfa", soy_ndfa, cite$t2_soy,
    "Rice", "nonsymbiotic_base_kg_ha", rice_nonsym, cite$t6_rice,
    "Sugarcane", "nonsymbiotic_base_kg_ha", cane_nonsym, cite$t6_cane
  )
}

# Rows whose `source` field declares them derived from another registry row
# rather than from a publication. Asserting the rule keeps the two rows from
# drifting apart when only one of them is updated.
.bnf_derivation_ledger <- function() {
  tibble::tribble(
    ~name_bnf, ~column, ~derived_from,
    "Fodder, other", "ndfa", "Green leguminous",
    "Fodder, other", "n_harvest_index", "Green leguminous",
    "Fodder, other", "below_ground_n_ratio", "Green leguminous"
  )
}

.bnf_unasserted_reasons <- function() {
  list(
    clover_nhi = paste(
      "Anglade et al. (2015) Table 1 reports no NHI for clover and the",
      "assumed NHI in the text names alfalfa only; 0.9 extends the alfalfa",
      "forage assumption to clover"
    ),
    herridge_mismatch = paste(
      "Herridge et al. (2008) reports a dry-matter harvest index (0.35, or",
      "0.4 for groundnut and soybean) and a below-ground multiplier of 1.4.",
      "A dry-matter index is not the N harvest index stored here, so the",
      "cited source supports neither the stored index nor 1.3"
    ),
    no_primary = paste(
      "cited through Lassaletta et al. (2014); no primary reported value",
      "located for this row"
    ),
    soyabeans = paste(
      "cited through Lassaletta et al. (2014) to Salvagiotti et al. (2008),",
      "which reports %Ndfa of 52 over all data and 58 for zero-N",
      "treatments; 0.57 matches neither and the intermediate value was not",
      "located"
    ),
    composite = paste(
      "composite mixed-sward value attributed to three publications at once;",
      "no single reported number corresponds to it and the sources are not",
      "available to this test"
    ),
    fodder_share = paste(
      "modelling convention for the legume share of a mixed fodder stand,",
      "not a source-reported measurement"
    ),
    definitional = paste(
      "stand composition by definition (1 for a pure legume crop, 0 for a",
      "non-legume system), not a source-reported measurement; asserted",
      "structurally instead"
    )
  )
}

# Cells that cannot be asserted against their cited source here. Each carries
# the reason. This is a declaration of missing coverage, not coverage.
.bnf_unasserted_ledger <- function() {
  r <- .bnf_unasserted_reasons()

  tibble::tribble(
    ~name_bnf, ~column, ~reason,
    "Clover", "n_harvest_index", r$clover_nhi,
    "Beans", "n_harvest_index", r$herridge_mismatch,
    "Beans", "below_ground_n_ratio", r$herridge_mismatch,
    "Groundnuts, with shell", "n_harvest_index", r$herridge_mismatch,
    "Groundnuts, with shell", "below_ground_n_ratio", r$herridge_mismatch,
    "Green leguminous", "ndfa", r$no_primary,
    "Green leguminous", "n_harvest_index", r$no_primary,
    "Green leguminous", "below_ground_n_ratio", r$no_primary,
    "Other pulses", "ndfa", r$no_primary,
    "Other pulses", "n_harvest_index", r$no_primary,
    "Other pulses", "below_ground_n_ratio", r$no_primary,
    "Soyabeans", "ndfa", r$soyabeans,
    "Soyabeans", "n_harvest_index", r$soyabeans,
    "Soyabeans", "below_ground_n_ratio", r$soyabeans,
    "Mixed swards", "ndfa", r$composite,
    "Mixed swards", "n_harvest_index", r$composite,
    "Mixed swards", "below_ground_n_ratio", r$composite,
    "Mixed swards", "leguminous_share", r$composite,
    "Meadows", "ndfa", r$composite,
    "Meadows", "n_harvest_index", r$composite,
    "Meadows", "below_ground_n_ratio", r$composite,
    "Meadows", "leguminous_share", r$composite,
    "Fallow", "ndfa", r$composite,
    "Fallow", "leguminous_share", r$composite,
    "Weeds", "ndfa", r$composite,
    "Weeds", "leguminous_share", r$composite,
    "Fodder, other", "leguminous_share", r$fodder_share,
    "Alfalfa", "leguminous_share", r$definitional,
    "Beans", "leguminous_share", r$definitional,
    "Clover", "leguminous_share", r$definitional,
    "Fava bean", "leguminous_share", r$definitional,
    "Green leguminous", "leguminous_share", r$definitional,
    "Groundnuts, with shell", "leguminous_share", r$definitional,
    "Lentils", "leguminous_share", r$definitional,
    "Other pulses", "leguminous_share", r$definitional,
    "Pea", "leguminous_share", r$definitional,
    "Soyabeans", "leguminous_share", r$definitional,
    "Rice", "leguminous_share", r$definitional,
    "Sugarcane", "leguminous_share", r$definitional
  )
}

.bnf_numeric_columns <- function() {
  c(
    "ndfa",
    "n_harvest_index",
    "below_ground_n_ratio",
    "nonsymbiotic_base_kg_ha",
    "leguminous_share"
  )
}

.bnf_stored <- function(b, name, column) {
  v <- b[[column]][b$name_bnf == name]
  testthat::expect_length(v, 1)
  v
}

.bnf_cell_key <- function(ledger) {
  paste(ledger$name_bnf, ledger$column, sep = " / ")
}

.bnf_stored_cells <- function(b) {
  .bnf_numeric_columns() |>
    purrr::map(function(col) {
      tibble::tibble(name_bnf = b$name_bnf[!is.na(b[[col]])], column = col)
    }) |>
    dplyr::bind_rows()
}

# ---- Tests ------------------------------------------------------------------

test_that("the transcribed Anglade Table 1 reproduces the table's own totals", {
  # Each sample-size column of Table 1 must sum over the five species to the
  # "All" row. This proves the (n, Median) blocks are read in the right
  # order, which is the alignment every Anglade assertion below relies on.
  t1 <- .anglade_2015_table1()
  species <- t1[t1$species != "All", ]
  all_row <- t1[t1$species == "All", ]
  n_cols <- grep("_n$", names(t1), value = TRUE)
  testthat::expect_length(n_cols, 6)
  for (col in n_cols) {
    testthat::expect_equal(
      sum(species[[col]], na.rm = TRUE),
      all_row[[col]],
      info = col
    )
  }
})

test_that("every transcribed Anglade median lies inside its own IQR", {
  # The complement of the totals check above. The sums pin which column of
  # each block is the count; this pins the other one as a central value, so
  # a count read into a median position is refused by construction. Anglade
  # reports 34 as the faba-bean NHI sample size against an NHI IQR of 63-77.
  t1 <- .anglade_2015_table1()
  med_cols <- grep("_median$", names(t1), value = TRUE)
  testthat::expect_length(med_cols, 6)
  for (col in med_cols) {
    stem <- sub("_median$", "", col)
    m <- t1[[col]]
    q1 <- t1[[paste0(stem, "_q1")]]
    q3 <- t1[[paste0(stem, "_q3")]]
    keep <- !is.na(m)
    testthat::expect_true(
      all(q1[keep] <= m[keep] & m[keep] <= q3[keep]),
      info = paste(col, paste(t1$species[keep], m[keep], collapse = "; "))
    )
    testthat::expect_true(all(q1[keep] < q3[keep]), info = col)
  }
})

test_that("every stored coefficient matches the value in its cited source", {
  b <- whep::whep_coef_table("bnf")
  ledger <- .bnf_source_ledger()
  for (i in seq_len(nrow(ledger))) {
    testthat::expect_equal(
      .bnf_stored(b, ledger$name_bnf[i], ledger$column[i]),
      ledger$expected[i],
      info = paste0(
        ledger$name_bnf[i],
        " / ",
        ledger$column[i],
        " -- ",
        ledger$citation[i]
      )
    )
  }
})

test_that("no Anglade coefficient equals a sample size from its own row", {
  # The failure mode this guards is a sample size transcribed into a value
  # column. Every Anglade-cited fraction is a percentage in Table 1, so it
  # must not coincide with any n reported for the same species.
  b <- whep::whep_coef_table("bnf")
  t1 <- .anglade_2015_table1()
  rows <- list(
    Alfalfa = "Alfalfa",
    Clover = "Clover",
    `Fava bean` = "Faba bean",
    Lentils = "Lentil",
    Pea = "Pea"
  )
  n_cols <- grep("_n$", names(t1), value = TRUE)
  for (name in names(rows)) {
    ns <- unlist(t1[t1$species == rows[[name]], n_cols], use.names = FALSE)
    ns <- ns[!is.na(ns)]
    for (col in c("ndfa", "n_harvest_index")) {
      stored_pct <- .bnf_stored(b, name, col) * 100
      testthat::expect_false(
        any(abs(ns - stored_pct) < 1e-8),
        info = paste0(
          name,
          " / ",
          col,
          " = ",
          stored_pct,
          "% coincides with a sample size in Anglade Table 1"
        )
      )
    }
  }
})

test_that("declared derivations reproduce the row they are derived from", {
  b <- whep::whep_coef_table("bnf")
  ledger <- .bnf_derivation_ledger()
  for (i in seq_len(nrow(ledger))) {
    testthat::expect_equal(
      .bnf_stored(b, ledger$name_bnf[i], ledger$column[i]),
      .bnf_stored(b, ledger$derived_from[i], ledger$column[i]),
      info = paste0(ledger$name_bnf[i], " / ", ledger$column[i])
    )
  }
  testthat::expect_match(
    b$source[b$name_bnf == "Fodder, other"],
    "green leguminous"
  )
})

test_that("grain-legume NHI agrees with the nutrient-composition route", {
  # An independent route to the same quantity: `bio_coefs` carries grain N and
  # residue N per kg of product fresh matter from crop-composition literature,
  # so grain N / (grain N + residue N) implies an N harvest index without
  # using `bnf.csv` at all. The two routes are different literatures and are
  # not expected to agree exactly, so this is a wide band, but the ratios
  # observed span 0.81 to 1.19 while the faba-bean sample-size transcription
  # that motivated this file sat at 2.31.
  #
  # This is the one test here that couples two coefficient tables: an edit to
  # `bio_coefs`, or a new FAO item mapped in `names_bnf`, can break it with
  # nothing wrong in `bnf.csv`. If that happens, re-derive the band and the
  # two counts below rather than widening them reflexively.
  #
  # The seven `name_bnf` groups resolve to 14 `item_prod_code` rows, because
  # `Other pulses` alone spans eight FAO items. Both counts are pinned below:
  # a merge that collapsed or silently grew would otherwise leave the band
  # check ranging over the wrong set, which is the fail-open shape this whole
  # file exists to remove.
  b <- whep::whep_coef_table("bnf")
  bio <- whep::whep_coef_table("bio_coefs")
  nb <- whep::whep_coef_table("names_bnf")
  grain_rows <- c(
    "Beans",
    "Fava bean",
    "Lentils",
    "Pea",
    "Other pulses",
    "Groundnuts, with shell",
    "Soyabeans"
  )
  m <- merge(
    nb[, c("item_prod_code", "name_bnf")],
    bio[, c(
      "item_prod_code",
      "product_dm_kgfm",
      "residue_dm_kgfm",
      "residue_kg_product_fm_kg",
      "product_n_kgdm",
      "residue_n_kgdm"
    )],
    by = "item_prod_code"
  )
  m <- m[m$name_bnf %in% grain_rows, ]
  testthat::expect_identical(nrow(m), 14L)
  testthat::expect_identical(length(unique(m$name_bnf)), 7L)
  grain_n <- m$product_n_kgdm * m$product_dm_kgfm
  residue_n <- m$residue_n_kgdm *
    m$residue_dm_kgfm *
    m$residue_kg_product_fm_kg
  implied <- grain_n / (grain_n + residue_n)
  stored <- vapply(
    m$name_bnf,
    function(nm) .bnf_stored(b, nm, "n_harvest_index"),
    numeric(1)
  )
  ratio <- implied / stored
  testthat::expect_true(
    all(!is.na(ratio) & ratio > 0.7 & ratio < 1.4),
    info = paste(m$item_prod_code, m$name_bnf, round(ratio, 3), collapse = "; ")
  )
})

test_that("leguminous share follows the stand definition", {
  b <- whep::whep_coef_table("bnf")
  pure <- c(
    "Alfalfa",
    "Beans",
    "Clover",
    "Fava bean",
    "Green leguminous",
    "Groundnuts, with shell",
    "Lentils",
    "Other pulses",
    "Pea",
    "Soyabeans"
  )
  non_legume <- c("Rice", "Sugarcane")
  mixed <- c("Fodder, other", "Mixed swards", "Meadows", "Fallow", "Weeds")
  testthat::expect_equal(
    b$leguminous_share[match(pure, b$name_bnf)],
    rep(1, length(pure))
  )
  testthat::expect_equal(
    b$leguminous_share[match(non_legume, b$name_bnf)],
    rep(0, length(non_legume))
  )
  v <- b$leguminous_share[match(mixed, b$name_bnf)]
  testthat::expect_true(all(v > 0 & v < 1))
})

test_that("the source ledgers account for every stored coefficient", {
  # Coverage cannot rot silently: a new row, or a new coefficient on an
  # existing row, fails here until it is either asserted against a source or
  # declared unassertable with a reason.
  b <- whep::whep_coef_table("bnf")
  cells <- .bnf_stored_cells(b)
  asserted <- .bnf_cell_key(.bnf_source_ledger())
  derived <- .bnf_cell_key(.bnf_derivation_ledger())
  declared <- .bnf_cell_key(.bnf_unasserted_ledger())
  covered <- c(asserted, derived, declared)

  testthat::expect_equal(anyDuplicated(covered), 0)
  testthat::expect_setequal(.bnf_cell_key(cells), covered)
  # Honest accounting of how much of the registry is source-backed. The
  # partition above is what enforces coverage; `nrow(cells)` pins the
  # registry's size so a new coefficient cannot arrive unnoticed. Update
  # these deliberately, together with the ledgers.
  testthat::expect_equal(nrow(cells), 60)
  testthat::expect_equal(length(asserted), 18)
  testthat::expect_equal(length(derived), 3)
  testthat::expect_equal(length(declared), 39)
})
