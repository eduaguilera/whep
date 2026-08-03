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
  data.frame(
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
    nhi_pct_q3 = c(NA, NA, 77, 78, 82, 80),
    stringsAsFactors = FALSE
  )
}

# Values Anglade et al. (2015) state in the body text rather than in Table 1.
# BGN: "we used mean values of BGP-N reported at maturity for grain and forage
# legumes as a multiplicative factors of N fixed in shoot, 1.3 and 1.7,
# respectively" (Discussion, "Amounts of N2 fixation and N net inputs").
# NHI: "high aboveground NHI values (0.75 for grain crops and 0.9 for alfalfa)
# were assumed" (Discussion, "N use efficiency and N surplus").
.anglade_2015_text <- list(
  bgn_grain_legume = 1.3,
  bgn_forage_legume = 1.7,
  nhi_alfalfa = 0.9
)

# ---- Herridge et al. (2008), Plant and Soil 311:1-18 ------------------------
# Herridge, D. F., M. B. Peoples, and R. M. Boddey. 2008. Global inputs of
# biological nitrogen fixation in agricultural systems. Plant and Soil
# 311:1-18.
#
# Table 2, "%Ndfa average" for experiments; Table 6, "Rate of N2 fixation
# (kg N/ha/year)" for the non-legume systems.
.herridge_2008 <- list(
  ndfa_pct_common_bean = 40,
  ndfa_pct_soybean_groundnut = 68,
  nonsymbiotic_kg_ha_rice = 33,
  nonsymbiotic_kg_ha_sugarcane = 25
)

# ---- Ledgers ----------------------------------------------------------------
.bnf_source_ledger <- function() {
  t1 <- .anglade_2015_table1()
  med <- function(sp, col) t1[[col]][t1$species == sp]
  ang <- "Anglade et al. (2015) Ecosphere 6(3):37"
  her <- "Herridge et al. (2008) Plant Soil 311:1-18"

  data.frame(
    name_bnf = c(
      "Alfalfa", "Alfalfa", "Alfalfa",
      "Clover", "Clover",
      "Fava bean", "Fava bean", "Fava bean",
      "Lentils", "Lentils", "Lentils",
      "Pea", "Pea", "Pea",
      "Beans", "Groundnuts, with shell",
      "Rice", "Sugarcane"
    ),
    column = c(
      "ndfa", "n_harvest_index", "below_ground_n_ratio",
      "ndfa", "below_ground_n_ratio",
      "ndfa", "n_harvest_index", "below_ground_n_ratio",
      "ndfa", "n_harvest_index", "below_ground_n_ratio",
      "ndfa", "n_harvest_index", "below_ground_n_ratio",
      "ndfa", "ndfa",
      "nonsymbiotic_base_kg_ha", "nonsymbiotic_base_kg_ha"
    ),
    expected = c(
      med("Alfalfa", "ndfa_pct_median") / 100,
      .anglade_2015_text$nhi_alfalfa,
      .anglade_2015_text$bgn_forage_legume,
      med("Clover", "ndfa_pct_median") / 100,
      .anglade_2015_text$bgn_forage_legume,
      med("Faba bean", "ndfa_pct_median") / 100,
      med("Faba bean", "nhi_pct_median") / 100,
      .anglade_2015_text$bgn_grain_legume,
      med("Lentil", "ndfa_pct_median") / 100,
      med("Lentil", "nhi_pct_median") / 100,
      .anglade_2015_text$bgn_grain_legume,
      med("Pea", "ndfa_pct_median") / 100,
      med("Pea", "nhi_pct_median") / 100,
      .anglade_2015_text$bgn_grain_legume,
      .herridge_2008$ndfa_pct_common_bean / 100,
      .herridge_2008$ndfa_pct_soybean_groundnut / 100,
      .herridge_2008$nonsymbiotic_kg_ha_rice,
      .herridge_2008$nonsymbiotic_kg_ha_sugarcane
    ),
    citation = c(
      paste(ang, "Table 1, Ndfa(%) median"),
      paste(ang, "Discussion, assumed NHI for alfalfa"),
      paste(ang, "Discussion, BGN for forage legumes"),
      paste(ang, "Table 1, Ndfa(%) median"),
      paste(ang, "Discussion, BGN for forage legumes"),
      paste(ang, "Table 1, Ndfa(%) median"),
      paste(ang, "Table 1, NHI(%) median"),
      paste(ang, "Discussion, BGN for grain legumes"),
      paste(ang, "Table 1, Ndfa(%) median"),
      paste(ang, "Table 1, NHI(%) median"),
      paste(ang, "Discussion, BGN for grain legumes"),
      paste(ang, "Table 1, Ndfa(%) median"),
      paste(ang, "Table 1, NHI(%) median"),
      paste(ang, "Discussion, BGN for grain legumes"),
      paste(her, "Table 2, common bean %Ndfa average"),
      paste(her, "Table 2, soybean/groundnut %Ndfa average"),
      paste(her, "Table 6, rice"),
      paste(her, "Table 6, sugarcane")
    ),
    stringsAsFactors = FALSE
  )
}

# Rows whose `source` field declares them derived from another registry row
# rather than from a publication. Asserting the rule keeps the two rows from
# drifting apart when only one of them is updated.
.bnf_derivation_ledger <- function() {
  data.frame(
    name_bnf = rep("Fodder, other", 3),
    column = c("ndfa", "n_harvest_index", "below_ground_n_ratio"),
    derived_from = rep("Green leguminous", 3),
    stringsAsFactors = FALSE
  )
}

# Cells that cannot be asserted against their cited source here. Each carries
# the reason. This is a declaration of missing coverage, not coverage.
.bnf_unasserted_ledger <- function() {
  frac <- c("ndfa", "n_harvest_index", "below_ground_n_ratio")
  no_primary <- paste(
    "cited through Lassaletta et al. (2014); no primary reported value",
    "located for this row"
  )
  herridge_mismatch <- paste(
    "Herridge et al. (2008) reports a dry-matter harvest index (0.35, or 0.4",
    "for groundnut and soybean) and a below-ground multiplier of 1.4, so the",
    "cited source does not support the stored N harvest index or 1.3"
  )
  composite <- paste(
    "composite mixed-sward value attributed to three publications at once;",
    "no single reported number corresponds to it and the sources are not",
    "available to this test"
  )
  definitional <- paste(
    "stand composition by definition (1 for a pure legume crop, 0 for a",
    "non-legume system), not a source-reported measurement; asserted",
    "structurally instead"
  )

  rbind(
    data.frame(
      name_bnf = "Clover", column = "n_harvest_index",
      reason = paste(
        "Anglade et al. (2015) Table 1 reports no NHI for clover and the",
        "assumed NHI in the text names alfalfa only; 0.9 extends the alfalfa",
        "forage assumption to clover"
      ),
      stringsAsFactors = FALSE
    ),
    data.frame(
      name_bnf = rep(c("Beans", "Groundnuts, with shell"), each = 2),
      column = rep(c("n_harvest_index", "below_ground_n_ratio"), 2),
      reason = herridge_mismatch, stringsAsFactors = FALSE
    ),
    data.frame(
      name_bnf = rep(c("Green leguminous", "Other pulses"), each = 3),
      column = rep(frac, 2), reason = no_primary, stringsAsFactors = FALSE
    ),
    data.frame(
      name_bnf = rep("Soyabeans", 3), column = frac,
      reason = paste(
        "cited through Lassaletta et al. (2014) to Salvagiotti et al. (2008),",
        "which reports %Ndfa of 52 over all data and 58 for zero-N",
        "treatments; 0.57 matches neither and the intermediate value was not",
        "located"
      ),
      stringsAsFactors = FALSE
    ),
    data.frame(
      name_bnf = rep(c("Mixed swards", "Meadows"), each = 4),
      column = rep(c(frac, "leguminous_share"), 2),
      reason = composite, stringsAsFactors = FALSE
    ),
    data.frame(
      name_bnf = rep(c("Fallow", "Weeds"), each = 2),
      column = rep(c("ndfa", "leguminous_share"), 2),
      reason = composite, stringsAsFactors = FALSE
    ),
    data.frame(
      name_bnf = "Fodder, other", column = "leguminous_share",
      reason = paste(
        "modelling convention for the legume share of a mixed fodder stand,",
        "not a source-reported measurement"
      ),
      stringsAsFactors = FALSE
    ),
    data.frame(
      name_bnf = c(
        "Alfalfa", "Beans", "Clover", "Fava bean", "Green leguminous",
        "Groundnuts, with shell", "Lentils", "Other pulses", "Pea",
        "Soyabeans", "Rice", "Sugarcane"
      ),
      column = "leguminous_share", reason = definitional,
      stringsAsFactors = FALSE
    )
  )
}

.bnf_numeric_columns <- function() {
  c(
    "ndfa", "n_harvest_index", "below_ground_n_ratio",
    "nonsymbiotic_base_kg_ha", "leguminous_share"
  )
}

.bnf_stored <- function(b, name, column) {
  v <- b[[column]][b$name_bnf == name]
  testthat::expect_length(v, 1)
  v
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
      sum(species[[col]], na.rm = TRUE), all_row[[col]],
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
        ledger$name_bnf[i], " / ", ledger$column[i], " -- ",
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
    Alfalfa = "Alfalfa", Clover = "Clover", `Fava bean` = "Faba bean",
    Lentils = "Lentil", Pea = "Pea"
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
          name, " / ", col, " = ", stored_pct,
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
    b$source[b$name_bnf == "Fodder, other"], "green leguminous"
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
  # The seven `name_bnf` groups resolve to 14 `item_prod_code` rows, because
  # `Other pulses` alone spans eight FAO items. Both counts are pinned below:
  # a merge that collapsed or silently grew would otherwise leave the band
  # check ranging over the wrong set, which is the fail-open shape this whole
  # file exists to remove.
  b <- whep::whep_coef_table("bnf")
  bio <- whep::whep_coef_table("bio_coefs")
  nb <- whep::whep_coef_table("names_bnf")
  grain_rows <- c(
    "Beans", "Fava bean", "Lentils", "Pea", "Other pulses",
    "Groundnuts, with shell", "Soyabeans"
  )
  m <- merge(
    nb[, c("item_prod_code", "name_bnf")],
    bio[, c(
      "item_prod_code", "product_dm_kgfm", "residue_dm_kgfm",
      "residue_kg_product_fm_kg", "product_n_kgdm", "residue_n_kgdm"
    )],
    by = "item_prod_code"
  )
  m <- m[m$name_bnf %in% grain_rows, ]
  testthat::expect_identical(nrow(m), 14L)
  testthat::expect_identical(length(unique(m$name_bnf)), 7L)
  grain_n <- m$product_n_kgdm * m$product_dm_kgfm
  residue_n <- m$residue_n_kgdm * m$residue_dm_kgfm *
    m$residue_kg_product_fm_kg
  implied <- grain_n / (grain_n + residue_n)
  stored <- vapply(
    m$name_bnf, function(nm) .bnf_stored(b, nm, "n_harvest_index"),
    numeric(1)
  )
  ratio <- implied / stored
  testthat::expect_true(
    all(!is.na(ratio) & ratio > 0.7 & ratio < 1.4),
    info = paste(
      m$item_prod_code, m$name_bnf, round(ratio, 3),
      collapse = "; "
    )
  )
})

test_that("leguminous share follows the stand definition", {
  b <- whep::whep_coef_table("bnf")
  pure <- c(
    "Alfalfa", "Beans", "Clover", "Fava bean", "Green leguminous",
    "Groundnuts, with shell", "Lentils", "Other pulses", "Pea", "Soyabeans"
  )
  non_legume <- c("Rice", "Sugarcane")
  mixed <- c("Fodder, other", "Mixed swards", "Meadows", "Fallow", "Weeds")
  testthat::expect_equal(
    b$leguminous_share[match(pure, b$name_bnf)], rep(1, length(pure))
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
  cells <- do.call(rbind, lapply(.bnf_numeric_columns(), function(col) {
    keep <- !is.na(b[[col]])
    data.frame(
      name_bnf = b$name_bnf[keep], column = col, stringsAsFactors = FALSE
    )
  }))
  key <- function(d) paste(d$name_bnf, d$column, sep = " / ")
  asserted <- key(.bnf_source_ledger())
  derived <- key(.bnf_derivation_ledger())
  declared <- key(.bnf_unasserted_ledger())
  covered <- c(asserted, derived, declared)

  testthat::expect_equal(anyDuplicated(covered), 0)
  testthat::expect_setequal(key(cells), covered)
  # Honest accounting of how much of the registry is source-backed. Update
  # these counts deliberately, together with the ledgers.
  testthat::expect_equal(nrow(cells), 60)
  testthat::expect_equal(length(asserted), 18)
  testthat::expect_equal(length(derived), 3)
  testthat::expect_equal(length(declared), 39)
})
