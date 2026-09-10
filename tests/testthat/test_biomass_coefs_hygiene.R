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
  # cannot exceed 1000 g in 1 kg of fresh matter. Fourteen rows do. Urea and
  # Lysine are feed additives where nitrogen times 6.25 is not protein at all;
  # the rest are real coefficient defects, White sugar among them.
  impossible <- .bch_proximate() |>
    dplyr::filter(.data$proximate_g_kgfm > 1000) |>
    dplyr::pull(.data$Name_biomass)
  testthat::expect_setequal(
    impossible,
    c(
      "Urea",
      "Carob",
      "Lysine",
      "Hemp seed",
      "Barley",
      "Barley old",
      "Winter cereals, other",
      "Canary grass",
      "Poppy seeds",
      "White sugar",
      "Triticale",
      "Linseed",
      "Cereals nes",
      "Grain, mixed"
    )
  )
})

testthat::test_that("the proximate sum against dry matter can only improve", {
  # The tighter chemical bound: the constituents must fit inside the dry
  # matter. 75 rows fail it, but only 32 fail once fibre is left out, so about
  # forty of them are fibre being counted inside carbohydrate as well as beside
  # it rather than a coefficient error. Both counts are ratchets: lower them
  # when a row is fixed, never raise them to admit a new one.
  prox <- .bch_proximate()
  with_fibre <- sum(prox$proximate_g_kgfm > prox$dry_matter_g_kgfm)
  without_fibre <- sum(
    prox$proximate_g_kgfm - dplyr::coalesce(prox$Fiber_g_kgFM, 0) >
      prox$dry_matter_g_kgfm
  )
  testthat::expect_lte(with_fibre, 75L)
  testthat::expect_lte(without_fibre, 32L)
})

testthat::test_that("no single constituent outweighs its own dry matter", {
  # The strictest reading of the bound, and the one no definitional argument
  # rescues: whatever basis the composition block is on, a single constituent
  # cannot outweigh the dry matter it is part of. Six rows carry a
  # carbohydrate value above their own Product_kgDM_kgFM (#752), so at least
  # one of those two cells is wrong in each. Pinned by name: repairing one
  # means editing this list, and a new offender fails loudly.
  #
  # White sugar is arithmetic rather than a typo -- Equiv copies a parent's
  # composition rescaled per kg of dry matter (exact to 1e-13), so Brown
  # sugar's 995 g/kg at DM 0.965 becomes 1025.9 g/kg at DM 0.995, which is
  # more than a kilogram of carbohydrate in a kilogram of fresh matter.
  offenders <- whep::biomass_coefs |>
    dplyr::filter(
      !is.na(.data$Carbohydrates_g_kgFM),
      !is.na(.data$Product_kgDM_kgFM),
      .data$Carbohydrates_g_kgFM > .data$Product_kgDM_kgFM * 1000
    ) |>
    dplyr::pull(.data$Name_biomass)
  testthat::expect_setequal(
    offenders,
    c(
      "Honey",
      "White sugar",
      "Brown sugar",
      "Figs",
      "Tigernuts",
      "Vegetables, other"
    )
  )
  # Fibre never breaks the bound and lipid breaks it exactly once: Butter,
  # Ghee carries 830 g of fat per kg of fresh matter against 821.4 g/kg of dry
  # matter. Same class of defect, different column, so it is pinned here too
  # rather than left to the aggregate ratchet above.
  fatty <- whep::biomass_coefs |>
    dplyr::filter(
      !is.na(.data$Product_kgDM_kgFM),
      dplyr::coalesce(.data$Lipids_g_kgFM, 0) > .data$Product_kgDM_kgFM * 1000
    ) |>
    dplyr::pull(.data$Name_biomass)
  testthat::expect_setequal(fatty, "Butter, Ghee")
  fibrous <- whep::biomass_coefs |>
    dplyr::filter(
      !is.na(.data$Product_kgDM_kgFM),
      dplyr::coalesce(.data$Fiber_g_kgFM, 0) > .data$Product_kgDM_kgFM * 1000
    )
  testthat::expect_equal(nrow(fibrous), 0L)
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
    "Lysine",            6,       14,        2,         2,       0,       0,
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
  # shipped 0.1143 against a ceiling of 0.0939 until #931; it now carries
  # FEDNA's DL-Metionina figure. Lysine is the one remaining breach, 5.2%
  # over the free base, and is pinned rather than fixed because choosing
  # between the free base and the hydrochloride moves a published number.
  over <- .bch_additive_formulas() |>
    dplyr::filter(.data$Product_kgN_kgDM > .data$n_ceiling) |>
    dplyr::pull("Name_biomass")
  testthat::expect_setequal(over, "Lysine")
  # Not vacuous: the same comparison against a deliberately halved ceiling
  # has to fail, which it can only do if real coefficients are being read.
  testthat::expect_failure(
    testthat::expect_setequal(
      .bch_additive_formulas() |>
        dplyr::filter(.data$Product_kgN_kgDM > .data$n_ceiling / 2) |>
        dplyr::pull("Name_biomass"),
      "Lysine"
    )
  )
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
