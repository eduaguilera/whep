# Injected fixtures plus the PACKAGED classification, so the suite is offline.

testthat::test_that("digestibility is the protein-weighted mean of 0.95/0.80", {
  # TRS 935 Table 43 footnote b: "the weighted mean of 95% and 80% for animal
  # and plant protein sources respectively". A 40/60 animal/plant split must
  # give 0.95*0.4 + 0.80*0.6, not the midpoint of the two rates.
  supply <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~protein_t,
    2010L, 10L,        2731L,          40,
    2010L, 10L,        2511L,          60
  )
  out <- whep::build_protein_quality(
    data = list(protein_supply = supply),
    method = "digestibility_share"
  )
  testthat::expect_equal(out$quality, 0.95 * 0.4 + 0.80 * 0.6)
  testthat::expect_equal(out$animal_protein_share, 0.4)
  testthat::expect_false(isTRUE(all.equal(out$quality, (0.95 + 0.80) / 2)))
})

testthat::test_that("the bounds of the rate are the pure diets", {
  all_animal <- whep::build_protein_quality(
    data = list(
      protein_supply = tibble::tribble(
      ~year, ~area_code, ~item_cbs_code, ~protein_t,
      2010L, 10L,        2731L,          10
    )
    ),
    method = "digestibility_share"
  )
  all_plant <- whep::build_protein_quality(
    data = list(
      protein_supply = tibble::tribble(
      ~year, ~area_code, ~item_cbs_code, ~protein_t,
      2010L, 10L,        2511L,          10
    )
    ),
    method = "digestibility_share"
  )
  testthat::expect_equal(all_animal$quality, 0.95)
  testthat::expect_equal(all_plant$quality, 0.80)
})

testthat::test_that("quality is a provable upper bound on the full PDCAAS", {
  # PDCAAS = min(1, AAS) x D <= D, so tier 1b can never exceed the digestibility
  # and the correction it implies can never exceed the full one. That is what
  # makes it conservative about the SIZE of the correction, and therefore
  # anti-conservative about adequacy: it classifies fewer countries deficient.
  supply <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~protein_t,
    2010L, 10L,        2731L,          30,
    2010L, 10L,        2511L,          70
  )
  out <- whep::build_protein_quality(data = list(protein_supply = supply))
  testthat::expect_lte(out$quality, 0.95)
  testthat::expect_gte(out$quality, 0.80)
})

testthat::test_that("none is a real 1, and stamped", {
  supply <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~protein_t,
    2010L, 10L,        2731L,          40
  )
  out <- whep::build_protein_quality(
    data = list(protein_supply = supply),
    method = "none"
  )
  testthat::expect_equal(out$quality, 1)
  testthat::expect_equal(out$method_quality, "none")
})

testthat::test_that("unclassified items leave the weighting and are reported", {
  # An item outside FAO's animal/vegetal grouping must not silently default to
  # either rate; its share is reported instead.
  supply <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~protein_t,
    2010L, 10L,        2731L,          40,
    2010L, 10L,        99999L,         60
  )
  out <- whep::build_protein_quality(
    data = list(protein_supply = supply),
    method = "digestibility_share"
  )
  testthat::expect_equal(out$quality, 0.95)
  testthat::expect_equal(out$protein_classified_share, 0.4)
})

testthat::test_that("the PACKAGED classification follows FAO's own grouping", {
  # It must reconcile against FAOSTAT's published Animal Products (2941) and
  # Vegetal Products (2903) aggregates, not be WHEP's opinion of what is animal.
  # Measured on the 2010 world basket: 108.165 Mt animal against FAO's 108.239,
  # 160.327 Mt plant against 160.398 -- both within 0.07%.
  cls <- whep::whep_coef_table("protein_digestibility")
  testthat::expect_setequal(unique(cls$protein_class), c("animal", "plant"))
  testthat::expect_equal(anyDuplicated(cls$item_cbs_code), 0L)
  animal <- cls$item_cbs_code[cls$protein_class == "animal"]
  # Meat, offals, animal fats, butter, eggs, honey, milk and the aquatic items.
  testthat::expect_true(all(c(2731L, 2744L, 2848L, 2761L) %in% animal))
  # Aquatic plants sit in FAO's Animal Products grouping despite the name; that
  # is FAO's split, and following it is what makes the totals reconcile.
  testthat::expect_true(2775L %in% animal)
  testthat::expect_false(2511L %in% animal)
})

testthat::test_that("a missing input aborts", {
  testthat::expect_error(
    whep::build_protein_quality(data = list()),
    "protein_supply"
  )
  testthat::expect_error(
    whep::build_protein_quality(
      data = list(protein_supply = tibble::tibble(year = 1L))
    ),
    "area_code|item_cbs_code|protein_t"
  )
})

testthat::test_that("an unknown method is rejected", {
  testthat::expect_error(
    whep::build_protein_quality(
      data = list(
        protein_supply = tibble::tribble(
        ~year, ~area_code, ~item_cbs_code, ~protein_t,
        2010L, 10L,        2731L,          40
      )
      ),
      method = "pdcaas"
    ),
    "arg_match|must be one of|pdcaas"
  )
})

# ---- tier 1a: measured per-item digestibility ------------------------------

testthat::test_that("a measured Table 5 value beats the class rate", {
  # Beans are plant, so the class rate is 0.80, but TRS 935 measures them at
  # 0.78. The measured value must win, or tier 1a is doing nothing.
  supply <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~protein_t,
    2010L, 10L,        2546L,          100
  )
  out <- whep::build_protein_quality(data = list(protein_supply = supply))
  testthat::expect_equal(out$quality, 0.78)
  testthat::expect_equal(out$protein_measured_share, 1)
  testthat::expect_equal(out$method_quality, "trs935_item_default")
  # Tier 1b on the same basket returns the class rate instead.
  tier1b <- whep::build_protein_quality(
    data = list(protein_supply = supply),
    method = "digestibility_share"
  )
  testthat::expect_equal(tier1b$quality, 0.80)
})

testthat::test_that("an unmeasured item falls back to the class rate", {
  # Table 5 prints no fruit, vegetable, root, tuber or sugar row at all, so the
  # fallback carries a real share of every diet rather than being a corner case.
  supply <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~protein_t,
    2010L, 10L,        2546L,          50,
    2010L, 10L,        2605L,          50
  )
  out <- whep::build_protein_quality(data = list(protein_supply = supply))
  testthat::expect_equal(out$quality, (0.78 + 0.80) / 2)
  testthat::expect_equal(out$protein_measured_share, 0.5)
})

testthat::test_that("the variant brackets the processing judgement", {
  # Wheat is the case where refining RAISES digestibility (bran removed):
  # whole 0.86, cereal 0.77, flour white 0.96.
  supply <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~protein_t,
    2010L, 10L,        2511L,          100
  )
  q <- function(v) {
    whep::build_protein_quality(
      data = list(protein_supply = supply),
      variant = v
    )$quality
  }
  testthat::expect_equal(q("default"), 0.86)
  testthat::expect_equal(q("low"), 0.77)
  testthat::expect_equal(q("high"), 0.96)
})

testthat::test_that("the processing direction is not uniform across cereals", {
  # Refining raises wheat and lowers maize and rice. A single "processed" sweep
  # would be wrong in one direction for one of them, which is why the bracket is
  # carried per item rather than derived.
  q <- function(code, v) {
    whep::build_protein_quality(
      data = list(
        protein_supply = tibble::tribble(
          ~year, ~area_code, ~item_cbs_code, ~protein_t,
          2010L, 10L,        code,           100
        )
      ),
      variant = v
    )$quality
  }
  testthat::expect_gt(q(2511L, "high"), q(2511L, "default"))
  testthat::expect_lt(q(2514L, "low"), q(2514L, "default"))
  testthat::expect_lt(q(2807L, "low"), q(2807L, "default"))
})

testthat::test_that("the variant is stamped so a sensitivity is self-labelling", {
  supply <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~protein_t,
    2010L, 10L,        2511L,          100
  )
  out <- whep::build_protein_quality(
    data = list(protein_supply = supply),
    variant = "low"
  )
  testthat::expect_equal(out$method_quality, "trs935_item_low")
})

testthat::test_that("the PACKAGED item mapping resolves into Table 5", {
  items <- whep::whep_coef_table("protein_digestibility_items")
  measured <- whep::whep_coef_table("protein_digestibility_trs935")
  testthat::expect_equal(anyDuplicated(items$item_cbs_code), 0L)
  for (col in c("source_default", "source_low", "source_high")) {
    testthat::expect_true(all(items[[col]] %in% measured$source_name))
  }
  # Every mapped row must be a single food, never one of the nine mixed diets:
  # a mixture's digestibility already aggregates a basket and would double-count
  # the aggregation this function performs.
  singles <- measured$source_name[measured$entry_type == "single_food"]
  testthat::expect_true(all(items$source_default %in% singles))
})
