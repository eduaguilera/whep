# Hygiene invariants on the PACKAGED nutrition coefficients (#500 J2.4).
#
# These read whep::biomass_coefs, not a fixture, so a coefficient edit has to
# come past them. None of them is a clean bill of health: each records what the
# shipped table actually violates today, so the violations are visible and the
# lists can only shrink. A test that passed by asserting nothing would be worse
# than no test.

.bch_proximate <- function() {
  whep::biomass_coefs |>
    dplyr::mutate(
      protein_g_kgfm = .data$N_kgN_kgFM * 6.25 * 1000,
      proximate_g_kgfm = rowSums(
        cbind(
          .data$protein_g_kgfm,
          .data$Lipids_g_kgFM,
          .data$Carbohydrates_g_kgFM,
          .data$Fiber_g_kgFM
        ),
        na.rm = TRUE
      ),
      dry_matter_g_kgfm = .data$Product_kgDM_kgFM * 1000
    ) |>
    dplyr::filter(
      .data$proximate_g_kgfm > 0,
      !is.na(.data$dry_matter_g_kgfm),
      .data$dry_matter_g_kgfm > 0
    )
}

testthat::test_that("every Edible_portion is a fraction", {
  # An edible portion above 1 means more edible matter than matter, so (0, 1]
  # is the whole valid range. This used to carry one exception, the section
  # header ANIMAL PRODUCTS at 4.0; that row is now dropped at ingestion
  # (#752), so the invariant holds outright.
  bad <- whep::biomass_coefs |>
    dplyr::filter(!is.na(.data$Edible_portion)) |>
    dplyr::filter(.data$Edible_portion <= 0 | .data$Edible_portion > 1)
  testthat::expect_equal(bad$Name_biomass, character(0))
  # The column is populated, so the guard is not vacuous.
  ok <- whep::biomass_coefs |>
    dplyr::filter(!is.na(.data$Edible_portion))
  testthat::expect_gt(nrow(ok), 200L)
})

testthat::test_that("the spreadsheet section headers are not shipped", {
  # Three all-caps rows of the upstream workbook are sheet furniture, not
  # commodities. Two are entirely empty; ANIMAL PRODUCTS holds the VLOOKUP
  # column-index vector the Coefs sheet addresses absolutely, which read as
  # data is an Edible_portion of 4 and 3 kg N per kg of fresh matter. They
  # cannot be removed upstream without breaking the workbook, so
  # data-raw/harmonization_tables.R drops them (#752).
  headers <- c(
    "TRANSFORMED PRODUCTS",
    "AGRO-INDUSTRY BYPRODUCTS",
    "ANIMAL PRODUCTS"
  )
  testthat::expect_false(any(headers %in% whep::biomass_coefs$Name_biomass))
  # They are still in the source CSV, so the filter is doing the work and the
  # test is not passing because the rows never existed.
  source_coefs <- readr::read_csv(
    system.file(
      "extdata",
      "harmonization",
      "biomass_coefs.csv",
      package = "whep"
    ),
    show_col_types = FALSE
  )
  testthat::expect_true(all(headers %in% source_coefs$Name_biomass))
})

testthat::test_that("ANIMAL PRODUCTS is unreachable from any item code", {
  # The header row never moved a published number even before it was dropped:
  # no item_cbs_code bridges to it. Asserted, not assumed, because items_full
  # is the only thing that could make such a row live again.
  reachable <- whep::items_full |>
    dplyr::filter(.data$Name_biomass == "ANIMAL PRODUCTS")
  testthat::expect_equal(nrow(reachable), 0L)
})

testthat::test_that("the nutrition lookup drops the label row even if reached", {
  # Belt and braces for the assertion above: if a future items_full DID bridge
  # to it, the food path must still not multiply food tonnes by 3 kg N/kg.
  items <- tibble::tribble(
    ~item_cbs_code, ~Name_biomass,
    2511L,          "ANIMAL PRODUCTS"
  )
  coefs <- tibble::tribble(
    ~Name_biomass,     ~N_kgN_kgFM, ~Product_kgN_kgDM, ~Product_kgDM_kgFM,
    "ANIMAL PRODUCTS", 3,           3,                 1
  ) |>
    dplyr::mutate(
      Edible_portion = 4,
      GE_product_edible_portion_MJ_kgFM = 15,
      GE_product_MJ_kgFM = 15
    )
  lookup <- whep:::.food_nutrition_lookup(items, coefs, "edible_portion")
  testthat::expect_true(all(is.na(lookup$protein_frac_kgfm)))
})

testthat::test_that("no food carries more than a kilogram per kilogram", {
  # The physically impossible bound: protein + lipid + carbohydrate + fibre
  # cannot exceed 1000 g in 1 kg of fresh matter. Thirteen rows do. Urea and
  # Ammonium chloride are feed additives where nitrogen times 6.25 is not
  # protein at all; the rest are real coefficient defects. White sugar left
  # the list in #752.
  impossible <- .bch_proximate() |>
    dplyr::filter(.data$proximate_g_kgfm > 1000) |>
    dplyr::pull(.data$Name_biomass)
  testthat::expect_setequal(
    impossible,
    c(
      "Urea",
      "Ammonium chloride",
      "Carob",
      "Hemp seed",
      "Barley",
      "Barley old",
      "Winter cereals, other",
      "Canary grass",
      "Poppy seeds",
      "Triticale",
      "Linseed",
      "Cereals nes",
      "Grain, mixed"
    )
  )
})

testthat::test_that("the proximate sum against dry matter can only improve", {
  # The tighter chemical bound: the constituents must fit inside the dry
  # matter. 67 rows fail it, but only 24 fail once fibre is left out, so about
  # forty of them are fibre being counted inside carbohydrate as well as beside
  # it rather than a coefficient error. Both counts are ratchets: lower them
  # when a row is fixed, never raise them to admit a new one. Honey left both
  # in #1096: its composition row sums to exactly its 785 g/kg dry matter.
  # Brown sugar, White sugar, Figs, Tigernuts and Butter, Ghee left both in
  # #752.
  prox <- .bch_proximate()
  with_fibre <- sum(prox$proximate_g_kgfm > prox$dry_matter_g_kgfm)
  without_fibre <- sum(
    prox$proximate_g_kgfm - dplyr::coalesce(prox$Fiber_g_kgFM, 0) >
      prox$dry_matter_g_kgfm
  )
  testthat::expect_lte(with_fibre, 67L)
  testthat::expect_lte(without_fibre, 24L)
})

testthat::test_that("no single constituent outweighs its own dry matter", {
  # The strictest reading of the bound, and the one no definitional argument
  # rescues: whatever basis the composition block is on, a single constituent
  # cannot outweigh the dry matter it is part of. Seven rows used to (#752);
  # six are repaired and one is pinned by name. What happened to each:
  #
  # - Honey left in #1096, when its placeholder 1000 g/kg became its `Miel`
  #   row's 780.
  # - Brown sugar and White sugar: the workbook ran its Equiv rescale the
  #   wrong way round. Its `Total azucar` composition (995 g carbohydrate/kg)
  #   was booked on Brown sugar (DM 0.965) and copied per kg of dry matter to
  #   White sugar (DM 0.995), which made 1025.9 g of carbohydrate per kg.
  # - Figs and Tigernuts carried the composition of the DRIED food (`Higo`
  #   2810 kcal/kg, `Chufa` 4430 kcal/kg) on a fresh-basis row. Their blocks
  #   are now diluted by the row's own edible energy over the dried food's.
  # - Butter, Ghee carried 830 g fat/kg in 821.4 g/kg of dry matter; now
  #   FAO's 81.1 g/100 g for butter of cow milk.
  #
  # Vegetables, other is the one left: its dry matter is Tomato's (the row's
  # Equiv) under an average-vegetable composition, and repairing it means
  # moving the dry matter, which production reads.
  offenders <- whep::biomass_coefs |>
    dplyr::filter(
      !is.na(.data$Product_kgDM_kgFM),
      pmax(
        dplyr::coalesce(.data$Carbohydrates_g_kgFM, 0),
        dplyr::coalesce(.data$Lipids_g_kgFM, 0),
        dplyr::coalesce(.data$Fiber_g_kgFM, 0),
        dplyr::coalesce(.data$N_kgN_kgFM, 0) * 6250
      ) >
        .data$Product_kgDM_kgFM * 1000
    ) |>
    dplyr::pull(.data$Name_biomass)
  testthat::expect_setequal(
    offenders,
    c("Vegetables, other", "Urea", "Ammonium chloride")
  )
})

testthat::test_that("the repaired composition rows close against dry matter", {
  # Protein + lipid + carbohydrate + fibre fits inside the dry matter for
  # every row #752 repaired, not only the single largest constituent.
  repaired <- c("Brown sugar", "White sugar", "Figs", "Tigernuts")
  prox <- .bch_proximate() |>
    dplyr::filter(.data$Name_biomass %in% repaired)
  testthat::expect_setequal(prox$Name_biomass, repaired)
  testthat::expect_true(all(
    prox$proximate_g_kgfm <= prox$dry_matter_g_kgfm + 1e-6
  ))
  # Butter's protein is 6 g/kg, so 811 g of fat leaves room for it too.
  butter <- .bch_proximate() |>
    dplyr::filter(.data$Name_biomass == "Butter, Ghee")
  testthat::expect_equal(butter$Lipids_g_kgFM, 811)
  testthat::expect_lte(butter$proximate_g_kgfm, butter$dry_matter_g_kgfm)
})

testthat::test_that("White sugar is Brown sugar's composition per kg DM", {
  # The workbook's Equiv identity still holds after the repair: a child row
  # is its parent's composition rescaled per kg of dry matter.
  sugar <- whep::biomass_coefs |>
    dplyr::filter(.data$Name_biomass %in% c("White sugar", "Brown sugar"))
  white <- sugar[sugar$Name_biomass == "White sugar", ]
  brown <- sugar[sugar$Name_biomass == "Brown sugar", ]
  dm_ratio <- white$Product_kgDM_kgFM / brown$Product_kgDM_kgFM
  testthat::expect_equal(
    white$Carbohydrates_g_kgFM,
    brown$Carbohydrates_g_kgFM * dm_ratio
  )
  testthat::expect_equal(
    white$Calcium_mg_kgFM,
    brown$Calcium_mg_kgFM * dm_ratio
  )
  testthat::expect_equal(white$Carbohydrates_g_kgFM, 995)
})

# Formula-derived nitrogen ceilings for the synthetic feed additives (#931).
# Standard atomic weights, IUPAC 2021 conventional values.
.bch_atomic_weights <- function() {
  c(
    carbon = 12.011,
    hydrogen = 1.008,
    nitrogen = 14.007,
    oxygen = 15.999,
    sulfur = 32.06,
    chlorine = 35.45
  )
}

# One row per additive whose molecular formula is known. The nitrogen mass
# fraction of a pure substance is an upper bound on any commercial grade of
# it, because impurity and moisture can only dilute the molecule.
.bch_additive_formulas <- function() {
  weights <- .bch_atomic_weights()
  tibble::tribble(
    ~Name_biomass,       ~carbon, ~hydrogen, ~nitrogen, ~oxygen, ~sulfur, ~chlorine,
    "Methionine",        5,       11,        1,         2,       1,       0,
    # Lysine is booked as L-lysine monohydrochloride, C6H15ClN2O2 (#1074).
    "Lysine",            6,       15,        2,         2,       0,       1,
    "Threonine",         4,       9,         1,         3,       0,       0,
    "Tryptophan",        11,      12,        2,         2,       0,       0,
    "Valine",            5,       11,        1,         2,       0,       0,
    "Urea",              1,       4,         2,         1,       0,       0,
    "Choline chloride",  5,       14,        1,         1,       0,       1,
    "Ammonium chloride", 0,       4,         1,         0,       0,       1
  ) |>
    dplyr::mutate(
      molar_mass = .data$carbon *
        weights[["carbon"]] +
        .data$hydrogen * weights[["hydrogen"]] +
        .data$nitrogen * weights[["nitrogen"]] +
        .data$oxygen * weights[["oxygen"]] +
        .data$sulfur * weights[["sulfur"]] +
        .data$chlorine * weights[["chlorine"]],
      n_ceiling = weights[["nitrogen"]] * .data$nitrogen / .data$molar_mass
    ) |>
    dplyr::inner_join(
      whep::biomass_coefs |>
        dplyr::select("Name_biomass", "Product_kgN_kgDM"),
      by = "Name_biomass"
    )
}

testthat::test_that("the additive formula table reaches every shipped row", {
  # Guards the two invariants below against passing on an empty join. Both
  # read whep::biomass_coefs, which local_mocked_bindings() cannot stub,
  # because the package exports environment resolves before the namespace.
  additives <- .bch_additive_formulas()
  testthat::expect_equal(nrow(additives), 8L)
  testthat::expect_false(any(is.na(additives$Product_kgN_kgDM)))
  # The molar masses are the published ones, so a typo in an atom count shows
  # up here rather than silently loosening a ceiling.
  testthat::expect_equal(
    round(additives$molar_mass[additives$Name_biomass == "Methionine"], 3),
    149.208
  )
  testthat::expect_equal(
    round(additives$molar_mass[additives$Name_biomass == "Urea"], 3),
    60.056
  )
})

testthat::test_that("no additive holds more nitrogen than its own molecule", {
  # The bound no measurement can cross: a kilogram of dry Methionine cannot
  # hold more nitrogen than a kilogram of pure methionine does. Methionine
  # shipped 0.1143 against a ceiling of 0.0939 until #931, and Lysine 0.2015,
  # above even the free base's 0.1916, until #1074; both now carry the FEDNA
  # entry for the commercial product.
  over <- .bch_additive_formulas() |>
    dplyr::filter(.data$Product_kgN_kgDM > .data$n_ceiling) |>
    dplyr::pull("Name_biomass")
  testthat::expect_length(over, 0L)
  # Not vacuous: the same comparison against a deliberately halved ceiling
  # has to fail, which it can only do if real coefficients are being read.
  testthat::expect_gt(
    .bch_additive_formulas() |>
      dplyr::filter(.data$Product_kgN_kgDM > .data$n_ceiling / 2) |>
      nrow(),
    0L
  )
})

testthat::test_that("every additive whose molecule holds nitrogen ships some", {
  # The opposite failure to the ceiling: Ammonium chloride and Choline
  # chloride shipped 0 kg N although NH4Cl is 26% nitrogen and choline
  # chloride 10%, so their nitrogen silently left the balance (#1074). A
  # commercial grade can dilute the molecule but cannot remove its nitrogen.
  empty <- .bch_additive_formulas() |>
    dplyr::filter(!(.data$Product_kgN_kgDM > 0)) |>
    dplyr::pull("Name_biomass")
  testthat::expect_length(empty, 0L)
})

testthat::test_that("Methionine carries the FEDNA DL-Metionina nitrogen", {
  # FEDNA's industrial amino acids page gives DL-Metionina 58.5% crude
  # protein, and the workbook's own convention for this block is crude
  # protein over 6.25. That is 0.0936, which sits just inside the 0.0939
  # ceiling above, as a grade of better than 99% purity should (#931).
  # The standing alternative is the hydroxy analogue, which holds no
  # nitrogen; pinning the number here makes that switch a visible diff.
  methionine <- whep::biomass_coefs |>
    dplyr::filter(.data$Name_biomass == "Methionine")
  testthat::expect_equal(nrow(methionine), 1L)
  testthat::expect_equal(methionine$Product_kgN_kgDM, 0.0936)
  # The fresh-matter column is that number times the row's dry matter, the
  # identity the source workbook enforces on the additive rows.
  testthat::expect_equal(
    methionine$N_kgN_kgFM,
    methionine$Product_kgN_kgDM * methionine$Product_kgDM_kgFM
  )
})

testthat::test_that("the wood residue nitrogen coefficients are pinned", {
  # Issue whep#932 is open on these. Two different quantities are priced
  # through Average wood: the harvested Wood item, which is stemwood, and
  # the residue production of forest and shrubland land, relabelled
  # Firewood, which is branches, bark and foliage. The shipped 0.0030 is
  # the mean of a beech, a conifer and a holm-oak anchor; the retired pin
  # carried 0.00095 for the same rows. Pinned so that whichever way #932
  # settles, the change is deliberate and lands in a diff of this list.
  wood_rows <- c(
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
  wood <- whep::biomass_coefs |>
    dplyr::filter(.data$Name_biomass %in% wood_rows)
  testthat::expect_equal(nrow(wood), length(wood_rows))
  testthat::expect_equal(
    sort(unique(round(wood$Residue_kgN_kgDM, 6))),
    c(0.001, 0.003, 0.0035, 0.0045)
  )
  anchors <- wood |>
    dplyr::filter(
      .data$Name_biomass %in%
        c("European beech", "Conifers", "Holm oak forest", "Average wood")
    ) |>
    dplyr::arrange(.data$Name_biomass)
  testthat::expect_equal(anchors$Name_biomass, sort(anchors$Name_biomass))
  testthat::expect_equal(
    anchors$Residue_kgN_kgDM,
    c(0.003, 0.0035, 0.001, 0.0045)
  )
  # Both items that reach these rows through items_full point at Average
  # wood, so that single cell is what carries the nitrogen.
  testthat::expect_equal(
    wood$Residue_kgN_kgDM[wood$Name_biomass == "Average wood"],
    0.003
  )
})

# The provenance of the 28 feed-additive rows (#1074). The upstream workbook's
# Sources sheet stops before the additive block, so the record lives here.
.bch_additive_sources <- function() {
  system.file(
    "extdata",
    "harmonization",
    "biomass_coefs_additive_sources.csv",
    package = "whep"
  ) |>
    readr::read_csv(show_col_types = FALSE) |>
    dplyr::left_join(
      whep::biomass_coefs |>
        dplyr::select(
          "Name_biomass",
          "Product_kgDM_kgFM",
          "N_kgN_kgFM",
          shipped_kgN_kgDM = "Product_kgN_kgDM"
        ),
      by = "Name_biomass"
    )
}

testthat::test_that("every feed-additive row has a provenance entry", {
  sources <- .bch_additive_sources()
  additives <- whep::items_full |>
    dplyr::filter(.data$group == "Additives") |>
    dplyr::pull("Name_biomass")
  testthat::expect_length(additives, 28L)
  testthat::expect_setequal(sources$Name_biomass, additives)
  testthat::expect_false(anyDuplicated(sources$Name_biomass) > 0)
  testthat::expect_true(all(
    sources$status %in%
      c(
        "fedna_crude_protein",
        "fedna_choline",
        "nitrogen_free",
        "assumed_unverified"
      )
  ))
  # An assumed value is a declared one: it has to say why.
  assumed <- sources |>
    dplyr::filter(.data$status == "assumed_unverified")
  testthat::expect_false(any(is.na(assumed$note)))
})

testthat::test_that("the additive rows ship the nitrogen their source gives", {
  sources <- .bch_additive_sources()
  testthat::expect_equal(sources$shipped_kgN_kgDM, sources$Product_kgN_kgDM)
  # FEDNA reports crude protein as Kjeldahl nitrogen times 6.25, and the
  # block books that as-fed figure as its Product_kgN_kgDM, the convention
  # Threonine, Tryptophan, Valine, Urea and Yeast came with upstream.
  protein <- sources |>
    dplyr::filter(.data$status == "fedna_crude_protein")
  testthat::expect_gt(nrow(protein), 0L)
  testthat::expect_equal(
    protein$shipped_kgN_kgDM,
    protein$crude_protein_pct / 100 / 6.25
  )
  # Choline nitrogen is not protein, so FEDNA books choline chloride's crude
  # protein as zero and gives its choline content instead. The nitrogen is
  # the choline cation's, C5H14NO+: 14.007 over 104.173.
  choline <- sources |>
    dplyr::filter(.data$status == "fedna_choline")
  testthat::expect_equal(choline$Name_biomass, "Choline chloride")
  testthat::expect_equal(
    choline$shipped_kgN_kgDM,
    round(choline$choline_pct / 100 * 14.007 / 104.173, 4)
  )
  free <- sources |>
    dplyr::filter(.data$status == "nitrogen_free")
  testthat::expect_true(all(free$shipped_kgN_kgDM == 0))
})

testthat::test_that("additive fresh-matter nitrogen is its dry-matter nitrogen", {
  # The identity the workbook enforces on this block. Phytase and Enzimes
  # broke it with a fresh-matter 0.16, exactly 1/6.25, against the 0.1154
  # their own two cells multiply to (#1074).
  sources <- .bch_additive_sources()
  testthat::expect_equal(
    sources$N_kgN_kgFM,
    sources$shipped_kgN_kgDM * sources$Product_kgDM_kgFM
  )
})

testthat::test_that("oil palm residue carries fresh-frond dry matter", {
  # whep#1516. The oil palm residue is fronds and trunks (46 and 14 Mt DM in
  # Malaysia, 2010), and its Residue_kgDM_kgFM was the fruit's 0.65. Fresh
  # fronds are 39.6% dry matter as fed (Feedipedia, "Oil palm (Elaeis
  # guineensis), fronds, fresh"; Heuze, Tran, Sauvant & Lebas 2015,
  # https://www.feedipedia.org/node/6916), and trunks, at 75-80% sap by
  # weight, are wetter still. The anchored dry matter is booked to Firewood
  # as fresh mass through this coefficient.
  palm <- whep::biomass_coefs |>
    dplyr::filter(.data$Name_biomass == "Oil palm")
  testthat::expect_equal(nrow(palm), 1L)
  testthat::expect_equal(palm$Residue_kgDM_kgFM, 0.396)
  testthat::expect_lt(palm$Residue_kgDM_kgFM, palm$Product_kgDM_kgFM)
})
