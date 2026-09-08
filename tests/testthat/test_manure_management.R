.toy_excretion <- function() {
  tibble::tribble(
    ~year,
    ~territory,
    ~sub_territory,
    ~livestock_category,
    ~n_excretion,
    ~c_excretion,
    ~vs_excretion,
    2020L,
    "ES",
    NA,
    "Cattle_milk",
    100,
    1900,
    60,
    2020L,
    "ES",
    NA,
    "Pigs",
    30,
    270,
    20
  )
}

test_that("split_manure_management conserves N, C and VS across MMS", {
  res <- whep::split_manure_management(.toy_excretion())
  agg <- res |>
    dplyr::summarise(
      n = sum(n_stream),
      c = sum(c_stream),
      vs = sum(vs_stream),
      .by = livestock_category
    )
  cm <- agg[agg$livestock_category == "Cattle_milk", ]
  expect_equal(cm$n, 100)
  expect_equal(cm$c, 1900)
  expect_equal(cm$vs, 60)
  expect_equal(agg$n[agg$livestock_category == "Pigs"], 30)
})

test_that("grazing (PRP) stream present for cattle, absent for pigs", {
  res <- whep::split_manure_management(.toy_excretion())
  cattle <- res[res$livestock_category == "Cattle_milk", ]
  pigs <- res[res$livestock_category == "Pigs", ]
  expect_true("grazing" %in% cattle$stream)
  expect_true("Pasture/Range/Paddock" %in% cattle$mms_type)
  expect_false("grazing" %in% pigs$stream)
  expect_true(all(pigs$stream == "collected"))
  expect_true(all(res$method_mms == "regional_default"))
})

test_that("split_manure_management guards bad input", {
  expect_error(
    whep::split_manure_management(
      .toy_excretion(),
      options = list(mms_source = "x")
    ),
    "mms_source"
  )
  bad <- .toy_excretion()
  bad$n_excretion <- NULL
  expect_error(whep::split_manure_management(bad), "missing")
})

# Regional MMS shares (#466) -------------------------------------------------

# One cattle row per territory, so the MMS fractions read straight off n_stream.
.cattle_in <- function(territory) {
  tibble::tibble(
    year = 2020L,
    territory = territory,
    sub_territory = NA,
    livestock_category = "Cattle_milk",
    n_excretion = 100,
    c_excretion = 1900,
    vs_excretion = 60
  )
}

.mms_fracs <- function(res) {
  stats::setNames(res$n_stream / 100, res$mms_type)[
    sort(res$mms_type)
  ]
}

test_that("region_specific reaches the North America and Latin America mixes", {
  # Regression for #466: .mms_shares() filtered regional_mms_distribution to
  # region == "Global" unconditionally, so the 15 region-specific rows were
  # unreachable and every territory got the Global split. area_code 231 is the
  # USA (GLEAM "North America"), 21 Brazil (GLEAM "Central & South America",
  # IPCC "Latin America").
  opt <- list(mms_source = "region_specific")
  usa <- whep::split_manure_management(.cattle_in("231"), options = opt)
  bra <- whep::split_manure_management(.cattle_in("21"), options = opt)

  expect_equal(
    .mms_fracs(usa),
    c(
      "Daily Spread" = 0.05,
      "Liquid/Slurry" = 0.40,
      "Pasture/Range/Paddock" = 0.25,
      "Solid Storage" = 0.30
    )
  )
  expect_equal(
    .mms_fracs(bra),
    c(
      "Daily Spread" = 0.10,
      "Liquid/Slurry" = 0.05,
      "Pasture/Range/Paddock" = 0.70,
      "Solid Storage" = 0.15
    )
  )
  # The Global cattle split is 0.50 grazing; neither region may collapse to it.
  expect_false(isTRUE(all.equal(
    unname(.mms_fracs(usa)[["Pasture/Range/Paddock"]]),
    0.50
  )))
  expect_false(isTRUE(all.equal(
    unname(.mms_fracs(bra)[["Pasture/Range/Paddock"]]),
    0.50
  )))
})

test_that("region_specific leaves the default split untouched", {
  # The default is the status quo: every territory keeps the Global rows even
  # where a region-specific row exists.
  usa <- whep::split_manure_management(.cattle_in("231"))
  expect_equal(
    .mms_fracs(usa),
    c(
      "Daily Spread" = 0.05,
      "Liquid/Slurry" = 0.15,
      "Pasture/Range/Paddock" = 0.50,
      "Solid Storage" = 0.30
    )
  )
  expect_true(all(usa$method_mms == "regional_default"))
})

test_that("region_specific falls back to Global and conserves mass", {
  opt <- list(mms_source = "region_specific")
  # 231 = USA: cattle and swine have North American rows, sheep do not.
  # 114 = Kenya (IPCC "Africa") has no regional rows at all.
  # "ES" is neither an area code nor an ISO3: it must resolve to no region
  # instead of aborting, and take the Global rows.
  excretion <- dplyr::bind_rows(
    .cattle_in("114"),
    dplyr::mutate(.cattle_in("231"), livestock_category = "Sheep"),
    dplyr::mutate(.cattle_in("ES"), livestock_category = "Cattle_milk")
  )
  res <- whep::split_manure_management(excretion, options = opt)

  global <- whep::split_manure_management(excretion)
  expect_equal(
    dplyr::arrange(res, territory, mms_type)$n_stream,
    dplyr::arrange(global, territory, mms_type)$n_stream
  )
  expect_true(all(res$method_mms == "region_specific"))
  # Mass is conserved per input row under either source.
  totals <- res |>
    dplyr::summarise(
      n = sum(n_stream),
      c = sum(c_stream),
      vs = sum(vs_stream),
      .by = c(territory, livestock_category)
    )
  expect_true(all(abs(totals$n - 100) < 1e-9))
  expect_true(all(abs(totals$c - 1900) < 1e-9))
  expect_true(all(abs(totals$vs - 60) < 1e-9))
})

test_that("region_specific adds and drops no rows", {
  opt <- list(mms_source = "region_specific")
  excretion <- dplyr::bind_rows(
    .cattle_in("231"),
    .cattle_in("21"),
    .cattle_in("114"),
    dplyr::mutate(.cattle_in("231"), livestock_category = "Pigs")
  )
  a <- whep::split_manure_management(excretion)
  b <- whep::split_manure_management(excretion, options = opt)
  key <- function(d) {
    d |>
      dplyr::distinct(year, territory, sub_territory, livestock_category) |>
      nrow()
  }
  expect_equal(key(a), key(b))
  expect_equal(sum(a$n_stream), sum(b$n_stream))
  # North American swine swap Daily Spread for an Anaerobic Lagoon, so the MMS
  # vocabulary the split emits is allowed to differ; the territories are not.
  expect_setequal(unique(a$territory), unique(b$territory))
})

test_that("the loss stage handles the MMS only region_specific can emit", {
  # "Anaerobic Lagoon" appears in regional_mms_distribution only for North
  # American swine, so the Global-only split could never emit it and the
  # downstream loss tables were never exercised on it. apply_management_losses()
  # aborts on a missing EF3, loss fraction or post-storage C:N, so reaching it
  # at all is the assertion.
  swine <- dplyr::mutate(.cattle_in("231"), livestock_category = "Pigs")
  split <- whep::split_manure_management(
    swine,
    options = list(mms_source = "region_specific")
  )
  expect_true("Anaerobic Lagoon" %in% split$mms_type)
  expect_false(
    "Anaerobic Lagoon" %in% whep::split_manure_management(swine)$mms_type
  )

  res <- whep::apply_management_losses(split)
  lagoon <- res[res$mms_type == "Anaerobic Lagoon", ]
  expect_equal(nrow(lagoon), 1L)
  expect_true(all(!is.na(lagoon$applied_n) & lagoon$applied_n > 0))
  expect_equal(
    sum(
      res$applied_n +
        res$n_volatilized +
        res$n_leached +
        res$n2o_direct_n +
        res$n2_n
    ),
    100,
    tolerance = 1e-8
  )
})

test_that(".mms_region_of resolves an area-code territory to an IPCC region", {
  # Before #678, the shared GLEAM-region resolver given `area_code` alone
  # resolved only the areas its dissolved-federation override table lists, 8 of
  # 266, so this helper had to attach the ISO3 itself. Since #678 it derives
  # the ISO3 from `area_code`, and the expected regions below are unchanged --
  # MEASURED as bit-identical over every reporting area plus an ISO3 literal.
  expect_equal(
    whep:::.mms_region_of(c("231", "21", "79", "114", "ES", NA)),
    c(
      "North America",
      "Latin America",
      "Western Europe",
      "Africa",
      NA,
      NA
    )
  )
  # An ISO3 literal is the deprecated-but-accepted territory form.
  expect_equal(whep:::.mms_region_of("ESP"), "Western Europe")
})

test_that("apply_management_losses conserves N (applied + losses = excreted)", {
  res <- whep::apply_management_losses(
    whep::split_manure_management(.toy_excretion())
  )
  bal <- res |>
    dplyr::mutate(
      total_out = applied_n + n_volatilized + n_leached + n2o_direct_n + n2_n
    ) |>
    dplyr::summarise(out = sum(total_out), .by = livestock_category)
  expect_equal(
    bal$out[bal$livestock_category == "Cattle_milk"],
    100,
    tolerance = 1e-8
  )
  expect_equal(bal$out[bal$livestock_category == "Pigs"], 30, tolerance = 1e-8)
})

test_that("grazing keeps full N, collected loses N, N2 = 3x N2O-N", {
  res <- whep::apply_management_losses(
    whep::split_manure_management(.toy_excretion())
  )
  graz <- res[res$stream == "grazing", ]
  coll <- res[res$stream == "collected", ]
  expect_true(all(
    graz$n_volatilized == 0 &
      graz$n_leached == 0 &
      graz$n2o_direct_n == 0 &
      graz$n2_n == 0
  ))
  expect_true(all(graz$applied_n > 0))
  expect_true(all(coll$n_volatilized > 0))
  expect_true(all(coll$applied_n > 0))
  expect_equal(res$n2_n, 3 * res$n2o_direct_n)
  expect_equal(
    res$n2o_indirect_n,
    res$n_volatilized * 0.010 + res$n_leached * 0.0075
  )
  expect_true(all(res$method_losses == "ipcc_2019_tier2"))
})

test_that("apply_management_losses conserves C and VS, applied C:N post-storage", {
  res <- whep::apply_management_losses(
    whep::split_manure_management(.toy_excretion())
  )
  bal <- res |>
    dplyr::summarise(
      c_tot = sum(applied_c + c_lost),
      vs_tot = sum(applied_vs + vs_destroyed),
      .by = livestock_category
    )
  expect_equal(
    bal$c_tot[bal$livestock_category == "Cattle_milk"],
    1900,
    tolerance = 1e-8
  )
  expect_equal(
    bal$vs_tot[bal$livestock_category == "Cattle_milk"],
    60,
    tolerance = 1e-8
  )
  # Grazing deposition loses no carbon in storage (fresh excreta).
  graz <- res[res$stream == "grazing", ]
  expect_true(all(abs(graz$c_lost) < 1e-6))
  # Collected streams shift to the post-storage C:N: solid storage retains C
  # (N volatilizes faster, C:N rises, c_lost = 0), liquid/slurry loses C.
  coll <- res[res$stream == "collected" & res$applied_n > 0, ]
  expect_true(all(coll$c_lost >= -1e-9))
  expect_true(any(coll$c_lost > 0))
  expect_true(all(res$applied_vs >= 0 & res$vs_destroyed >= -1e-9))
  # Applied C:N differs from fresh-excreta C:N for collected streams.
  excreta_cn <- res$applied_c[res$stream == "grazing"] /
    res$applied_n[res$stream == "grazing"]
  coll_cn <- coll$applied_c / coll$applied_n
  expect_false(isTRUE(all.equal(coll_cn, rep(excreta_cn[1], length(coll_cn)))))
})

test_that("grazing retains full C and VS even above storage C:N cap", {
  # Sheep Excreta C:N is 12.4; feed a high-C:N excretion (C:N = 25) so the
  # storage cap would bite the grazing stream if it were applied. It must not:
  # the in-situ stream undergoes no storage and keeps its full C and VS.
  high_cn <- tibble::tribble(
    ~year,
    ~territory,
    ~sub_territory,
    ~livestock_category,
    ~n_excretion,
    ~c_excretion,
    ~vs_excretion,
    2020L,
    "ES",
    NA,
    "Sheep",
    100,
    2500,
    200
  )
  res <- whep::apply_management_losses(
    whep::split_manure_management(high_cn)
  )
  graz <- res[res$stream == "grazing", ]
  frac <- graz$applied_n / 100
  expect_true(all(abs(graz$applied_c - 2500 * frac) < 1e-8))
  expect_true(all(abs(graz$applied_vs - 200 * frac) < 1e-8))
  expect_true(all(abs(graz$c_lost) < 1e-8))
  expect_true(all(abs(graz$vs_destroyed) < 1e-8))
})

test_that("apply_management_losses guards bad input", {
  ok <- whep::split_manure_management(.toy_excretion())
  expect_error(
    whep::apply_management_losses(ok, options = list(method = "x")),
    "method"
  )
  bad <- ok
  bad$mms_type <- NULL
  expect_error(whep::apply_management_losses(bad), "missing")
})

# The shared MMS-share resolver (#679) ---------------------------------------

test_that(".resolve_mms_shares serves both engines' region columns", {
  # #679: the Tier-2 methane engine and the manure split used to carry two
  # implementations of one rule. The resolver is now shared, and the only
  # thing that differs between the two call sites is the name of the column
  # holding the region -- "region" for the methane engine, "mms_region" for the
  # split. Both must resolve to the same region-specific mix.
  methane_shape <- tibble::tibble(
    species_gen = "Cattle",
    region = "Latin America"
  ) |>
    whep:::.resolve_mms_shares("region")
  split_shape <- tibble::tibble(
    species_gen = "Cattle",
    mms_region = "Latin America"
  ) |>
    whep:::.resolve_mms_shares("mms_region")

  expect_equal(
    dplyr::arrange(methane_shape, mms_type)$fraction,
    dplyr::arrange(split_shape, mms_type)$fraction
  )
  expect_equal(sum(methane_shape$fraction), 1)
  expect_setequal(
    methane_shape$mms_type,
    c(
      "Pasture/Range/Paddock",
      "Solid Storage",
      "Daily Spread",
      "Liquid/Slurry"
    )
  )
})

test_that(".resolve_mms_shares falls back to the Global mix", {
  # The fallback branch: an unmatched region, an NA region, and no region
  # column at all must all give the Global split for that species -- never a
  # dropped row and never a missing fraction (#201).
  global <- tibble::tibble(species_gen = "Cattle") |>
    whep:::.resolve_mms_shares() |>
    dplyr::arrange(mms_type)

  purrr::walk(list("Africa", "not a region", NA_character_), function(case) {
    got <- tibble::tibble(species_gen = "Cattle", region = case) |>
      whep:::.resolve_mms_shares("region") |>
      dplyr::arrange(mms_type)
    expect_equal(got$mms_type, global$mms_type)
    expect_equal(got$fraction, global$fraction)
  })

  # A named region column the rows do not carry also takes the Global branch.
  absent <- tibble::tibble(species_gen = "Cattle") |>
    whep:::.resolve_mms_shares("region") |>
    dplyr::arrange(mms_type)
  expect_equal(absent$fraction, global$fraction)
  expect_equal(sum(global$fraction), 1)
})

test_that(".resolve_mms_shares keeps every input row, matched or not", {
  rows <- tidyr::expand_grid(
    species_gen = unique(whep::regional_mms_distribution$species),
    region = c(unique(whep::regional_mms_distribution$region), "Africa", NA)
  ) |>
    dplyr::mutate(row_id = dplyr::row_number())

  res <- whep:::.resolve_mms_shares(rows, "region")

  expect_setequal(res$row_id, rows$row_id)
  expect_false(anyNA(res$fraction))
  expect_false(anyNA(res$mms_type))
  # Mass conservation: each input row's shares sum to one.
  sums <- res |>
    dplyr::summarise(total = sum(fraction), .by = row_id)
  expect_true(all(abs(sums$total - 1) < 1e-12))
})

test_that("both engines see the same MMS mix for one territory", {
  # Cross-engine agreement, the point of the dedup: Brazil (area_code 21,
  # IPCC "Latin America") must get the same MMS fractions through the methane
  # engine's region column as through split_manure_management().
  split <- whep::split_manure_management(
    .cattle_in("21"),
    options = list(mms_source = "region_specific")
  )
  engine <- tibble::tibble(
    species_gen = "Cattle",
    region = whep:::.mms_region_of("21")
  ) |>
    whep:::.resolve_mms_shares("region")

  expect_equal(
    stats::setNames(split$n_stream / 100, split$mms_type)[
      sort(split$mms_type)
    ],
    stats::setNames(engine$fraction, engine$mms_type)[sort(engine$mms_type)]
  )
})

# ---- Bedding (whep#1005) ---------------------------------------------------

.toy_bedding <- function(dm = 100) {
  # Wheat straw composition from bio_coefs: 0.458 kg C, 0.00592 kg N per kg DM.
  tibble::tibble(
    year = 2020L,
    territory = "ES",
    sub_territory = NA_character_,
    bedding_dm_t = dm,
    bedding_c_t = dm * 0.458,
    bedding_n_t = dm * 0.00592
  )
}

test_that("no bedding is the default and leaves every value where it was", {
  split <- whep::split_manure_management(.toy_excretion())
  expect_true(all(split$dm_bedding == 0))
  expect_true(all(split$n_bedding == 0))
  expect_true(all(split$c_bedding == 0))
  expect_true(all(split$method_bedding == "none"))

  losses <- whep::apply_management_losses(split)
  expect_true(all(losses$method_bedding_carbon == "cap_at_stored_cn"))
  # The same rows without the bedding columns at all -- a `split` assembled by
  # a caller that predates them -- must give identical numbers.
  bare <- dplyr::select(split, -"dm_bedding", -"n_bedding", -"c_bedding")
  expect_equal(
    whep::apply_management_losses(bare)$applied_n,
    losses$applied_n
  )
  expect_equal(
    whep::apply_management_losses(bare)$applied_c,
    losses$applied_c
  )
})

test_that("bedding reaches only the litter-using collected streams", {
  split <- whep::split_manure_management(
    .toy_excretion(),
    options = list(bedding = .toy_bedding())
  )
  expect_true(all(split$method_bedding == "litter_mms_n_share"))
  litter <- split$mms_type %in%
    c("Solid Storage", "Daily Spread", "Poultry Manure")
  expect_true(all(split$dm_bedding[!litter] == 0))
  expect_true(all(split$dm_bedding[litter] > 0))
  expect_true(all(split$dm_bedding[split$stream == "grazing"] == 0))
  # Mass is conserved: every tonne supplied is placed on some stream.
  expect_equal(sum(split$dm_bedding), 100)
  expect_equal(sum(split$n_bedding), 100 * 0.00592)
  expect_equal(sum(split$c_bedding), 100 * 0.458)
})

test_that("bedding is split over litter streams by their excreted N", {
  split <- whep::split_manure_management(
    .toy_excretion(),
    options = list(bedding = .toy_bedding())
  )
  litter <- split[
    split$mms_type %in%
      c("Solid Storage", "Daily Spread", "Poultry Manure") &
      split$stream == "collected",
  ]
  expect_equal(
    litter$dm_bedding / sum(litter$dm_bedding),
    litter$n_stream / sum(litter$n_stream)
  )
})

test_that("bedding nitrogen raises applied N and C, and grazing is untouched", {
  base <- whep::apply_management_losses(
    whep::split_manure_management(.toy_excretion())
  )
  bed <- whep::apply_management_losses(
    whep::split_manure_management(
      .toy_excretion(),
      options = list(bedding = .toy_bedding())
    )
  )
  graze <- base$stream == "grazing"
  expect_equal(bed$applied_n[graze], base$applied_n[graze])
  expect_equal(bed$applied_c[graze], base$applied_c[graze])
  expect_gt(sum(bed$applied_n), sum(base$applied_n))
  expect_gt(sum(bed$applied_c), sum(base$applied_c))
  # Under the default cap the applied C:N of the cattle solid streams does not
  # move at all: `pmin(c, applied_n * cn_post)` already binds there (cattle
  # excreta C:N 19.07 against stored solid 20.16), so WHEP already reports the
  # bedded farmyard-manure C:N with or without any straw in the model, and
  # bedding raises the applied carbon only through the nitrogen it brings.
  # That is the finding whep#1005 turns on.
  cattle_solid <- base$manure_type == "Solid" &
    base$stream == "collected" &
    base$species_gen == "Cattle"
  expect_true(any(cattle_solid))
  expect_equal(
    bed$applied_c[cattle_solid] / bed$applied_n[cattle_solid],
    base$applied_c[cattle_solid] / base$applied_n[cattle_solid]
  )
  # Pig daily spread is the counter-case: its nitrogen losses are small enough
  # (7%) that `applied_n * 13.22` stays above the excreta carbon, the cap does
  # not bind, and the straw carbon does reach the field and lift the applied
  # C:N. Whether the cap binds is a property of the (species, MMS) pair, not of
  # bedding, which is exactly what makes it a decision rather than a detail.
  pig_spread <- base$mms_type == "Daily Spread" & base$species_gen == "Swine"
  expect_true(any(pig_spread))
  expect_true(all(
    bed$applied_c[pig_spread] / bed$applied_n[pig_spread] >
      base$applied_c[pig_spread] / base$applied_n[pig_spread]
  ))
})

test_that("the additive rule lifts the applied C:N above the stored value", {
  split <- whep::split_manure_management(
    .toy_excretion(),
    options = list(bedding = .toy_bedding())
  )
  capped <- whep::apply_management_losses(split)
  additive <- whep::apply_management_losses(
    split,
    options = list(bedding_carbon = "additive")
  )
  expect_true(all(additive$method_bedding_carbon == "additive"))
  cattle_solid <- capped$manure_type == "Solid" &
    capped$stream == "collected" &
    capped$species_gen == "Cattle"
  expect_gt(sum(additive$applied_c), sum(capped$applied_c))
  # The rule only ever adds carbon, and where the cap was binding it lifts the
  # applied C:N above the stored value.
  expect_true(all(additive$applied_c >= capped$applied_c))
  expect_true(all(
    additive$applied_c[cattle_solid] / additive$applied_n[cattle_solid] >
      capped$applied_c[cattle_solid] / capped$applied_n[cattle_solid]
  ))
  # Nitrogen is untouched by a carbon rule.
  expect_equal(additive$applied_n, capped$applied_n)
})

test_that("bedding closes the excreted + bedding = applied + lost balance", {
  split <- whep::split_manure_management(
    .toy_excretion(),
    options = list(bedding = .toy_bedding())
  )
  losses <- whep::apply_management_losses(split)
  supplied <- sum(.toy_excretion()$n_excretion) + sum(split$n_bedding)
  accounted <- sum(losses$applied_n) +
    sum(
      losses$n_volatilized +
        losses$n_leached +
        losses$n2o_direct_n +
        losses$n2_n
    )
  expect_equal(accounted, supplied)
})

test_that("bedding with nowhere to go is reported, not zeroed in silence", {
  # Sheep are 100% pasture/range/paddock in the Global MMS distribution, so a
  # sheep-only herd has no litter-using collected stream to bed.
  sheep <- tibble::tibble(
    year = 2020L,
    territory = "ES",
    sub_territory = NA,
    livestock_category = "Sheep",
    n_excretion = 100,
    c_excretion = 1240,
    vs_excretion = 60
  )
  expect_warning(
    whep::split_manure_management(
      sheep,
      options = list(bedding = .toy_bedding())
    ),
    "no litter-using collected manure stream"
  )
})

test_that("bedding guards its own contract", {
  expect_error(
    whep::split_manure_management(
      .toy_excretion(),
      options = list(bedding = tibble::tibble(year = 2020L))
    ),
    class = "whep_error_bedding_cols"
  )
  expect_error(
    whep::apply_management_losses(
      whep::split_manure_management(.toy_excretion()),
      options = list(bedding_carbon = "whatever")
    ),
    class = "rlang_error"
  )
  # Two bedding rows for one group would place the same litter twice; the
  # join is declared `many-to-one` so it aborts instead.
  expect_error(
    whep::split_manure_management(
      .toy_excretion(),
      options = list(
        bedding = dplyr::bind_rows(.toy_bedding(60), .toy_bedding(40))
      )
    ),
    class = "dplyr_error_join_relationship_many_to_one"
  )
})
