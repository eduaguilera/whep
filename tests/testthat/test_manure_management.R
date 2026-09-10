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

test_that("the grazing (PRP) stream is large for cattle, small for pigs", {
  # Before whep#958 the placeholder gave swine no pasture row at all. GLEAM
  # 2.0 Tab. 4.8 gives backyard pigs 5 percent pasture in every one of its ten
  # regions, so pigs now carry a small grazing stream; cattle keep a large
  # one. Both halves of the table are checked, because the loss stage treats
  # grazing and collected manure differently and the two must stay apart.
  res <- whep::split_manure_management(.toy_excretion())
  cattle <- res[res$livestock_category == "Cattle_milk", ]
  pigs <- res[res$livestock_category == "Pigs", ]
  expect_true("grazing" %in% cattle$stream)
  expect_true("Pasture/Range/Paddock" %in% cattle$mms_type)
  expect_gt(sum(cattle$n_stream[cattle$stream == "grazing"]) / 100, 0.4)
  expect_lt(sum(pigs$n_stream[pigs$stream == "grazing"]) / 30, 0.05)
  expect_true(all(res$method_mms == "gleam_2_0/regional_default"))

  old <- whep::split_manure_management(
    .toy_excretion(),
    options = list(mms_shares = "placeholder")
  )
  old_pigs <- old[old$livestock_category == "Pigs", ]
  expect_false("grazing" %in% old_pigs$stream)
  expect_true(all(old$method_mms == "placeholder/regional_default"))
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
  # region == "Global" unconditionally, so the region-specific rows were
  # unreachable and every territory got the Global split. area_code 231 is the
  # USA (GLEAM "North America"), 21 Brazil (GLEAM "Central & South America",
  # IPCC "Latin America"). The values are the GLEAM 2.0 ingest (whep#958):
  # the mean of Tab. 4.2 (dairy) and Tab. 4.3 (beef) for each region, with
  # drylot folded into solid storage.
  opt <- list(mms_source = "region_specific")
  usa <- whep::split_manure_management(.cattle_in("231"), options = opt)
  bra <- whep::split_manure_management(.cattle_in("21"), options = opt)

  expect_equal(
    .mms_fracs(usa),
    c(
      "Anaerobic Lagoon" = 0.135,
      "Daily Spread" = 0.05,
      "Liquid/Slurry" = 0.135,
      "Pasture/Range/Paddock" = 0.275,
      "Solid Storage" = 0.405
    )
  )
  expect_equal(
    .mms_fracs(bra),
    c(
      "Pasture/Range/Paddock" = 0.73,
      "Solid Storage" = 0.27
    )
  )
  # The Global cattle split is 0.4435 grazing; neither region may collapse to
  # it, which is what #466 broke.
  global_prp <- 0.443522579379146
  expect_false(isTRUE(all.equal(
    unname(.mms_fracs(usa)[["Pasture/Range/Paddock"]]),
    global_prp
  )))
  expect_false(isTRUE(all.equal(
    unname(.mms_fracs(bra)[["Pasture/Range/Paddock"]]),
    global_prp
  )))
})

test_that("mms_shares selects the table half, and rejects anything else", {
  # whep#958 replaced the unsourced placeholder with the GLEAM 2.0 ingest and
  # kept the placeholder selectable. The default must be the sourced half, the
  # placeholder must still reproduce the exact pre-whep#958 split, and an
  # unknown name must abort rather than silently pick one.
  gleam <- whep::split_manure_management(.cattle_in("231"))
  old <- whep::split_manure_management(
    .cattle_in("231"),
    options = list(mms_shares = "placeholder")
  )
  expect_equal(
    .mms_fracs(old),
    c(
      "Daily Spread" = 0.05,
      "Liquid/Slurry" = 0.15,
      "Pasture/Range/Paddock" = 0.50,
      "Solid Storage" = 0.30
    )
  )
  expect_equal(
    .mms_fracs(gleam),
    c(
      "Anaerobic Lagoon" = 0.016,
      "Daily Spread" = 0.009,
      "Liquid/Slurry" = 0.084530612244898,
      "Pasture/Range/Paddock" = 0.443522579379146,
      "Solid Storage" = 0.446946808375956
    )
  )
  expect_false(isTRUE(all.equal(.mms_fracs(gleam), .mms_fracs(old))))
  # Mass is conserved under either half.
  expect_equal(sum(gleam$n_stream), 100)
  expect_equal(sum(old$n_stream), 100)
  expect_error(
    whep::split_manure_management(
      .cattle_in("231"),
      options = list(mms_shares = "gleam_3_0")
    ),
    "mms_shares"
  )
})

test_that("region_specific leaves the default split untouched", {
  # The default is the status quo: every territory keeps the Global rows even
  # where a region-specific row exists.
  usa <- whep::split_manure_management(.cattle_in("231"))
  expect_equal(
    .mms_fracs(usa),
    c(
      "Anaerobic Lagoon" = 0.016,
      "Daily Spread" = 0.009,
      "Liquid/Slurry" = 0.084530612244898,
      "Pasture/Range/Paddock" = 0.443522579379146,
      "Solid Storage" = 0.446946808375956
    )
  )
  expect_true(all(usa$method_mms == "gleam_2_0/regional_default"))
})

test_that("region_specific falls back to Global and conserves mass", {
  opt <- list(mms_source = "region_specific")
  # 231 = USA and 114 = Kenya (IPCC "Africa") both resolve to a region the
  # GLEAM 2.0 ingest has rows for. "ES" is neither an area code nor an ISO3:
  # it must resolve to no region instead of aborting, and fall back to the
  # Global rows, which is the branch under test.
  excretion <- dplyr::bind_rows(
    .cattle_in("114"),
    dplyr::mutate(.cattle_in("231"), livestock_category = "Sheep"),
    dplyr::mutate(.cattle_in("ES"), livestock_category = "Cattle_milk")
  )
  res <- whep::split_manure_management(excretion, options = opt)

  global <- whep::split_manure_management(excretion)
  unresolved <- function(d) {
    dplyr::arrange(d[d$territory == "ES", ], mms_type)$n_stream
  }
  expect_equal(unresolved(res), unresolved(global))
  # The two territories that DO resolve must not have fallen back with it.
  expect_false(isTRUE(all.equal(
    dplyr::arrange(res, territory, mms_type)$n_stream,
    dplyr::arrange(global, territory, mms_type)$n_stream
  )))
  expect_true(all(res$method_mms == "gleam_2_0/region_specific"))
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

test_that("the loss stage handles the lagoon MMS under either half", {
  # Under the placeholder "Anaerobic Lagoon" appeared only for North American
  # swine, so the Global split could never emit it and the downstream loss
  # tables were never exercised on it. The GLEAM 2.0 ingest puts it in the
  # Global swine split too, so both paths now reach it.
  # apply_management_losses() aborts on a missing EF3, loss fraction or
  # post-storage C:N, so reaching it at all is the assertion.
  swine <- dplyr::mutate(.cattle_in("231"), livestock_category = "Pigs")
  split <- whep::split_manure_management(
    swine,
    options = list(mms_source = "region_specific")
  )
  expect_true("Anaerobic Lagoon" %in% split$mms_type)
  expect_true(
    "Anaerobic Lagoon" %in% whep::split_manure_management(swine)$mms_type
  )
  expect_false(
    "Anaerobic Lagoon" %in%
      whep::split_manure_management(
        swine,
        options = list(mms_shares = "placeholder")
      )$mms_type
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
    c("Pasture/Range/Paddock", "Solid Storage")
  )
})

test_that(".resolve_mms_shares falls back to the Global mix", {
  # The fallback branch: an unmatched region, an NA region, and no region
  # column at all must all give the Global split for that species -- never a
  # dropped row and never a missing fraction (#201).
  global <- tibble::tibble(species_gen = "Cattle") |>
    whep:::.resolve_mms_shares() |>
    dplyr::arrange(mms_type)

  purrr::walk(list("not a region", NA_character_), function(case) {
    got <- tibble::tibble(species_gen = "Cattle", region = case) |>
      whep:::.resolve_mms_shares("region") |>
      dplyr::arrange(mms_type)
    expect_equal(got$mms_type, global$mms_type)
    expect_equal(got$fraction, global$fraction)
  })

  # A real region label the table has no rows for takes the same branch. The
  # GLEAM 2.0 ingest covers all nine labels for cattle, so the case is only
  # reachable through a species the source leaves a gap for: Tab. 4.5 and 4.6
  # publish no Oceania column for either buffalo herd.
  buffalo_global <- tibble::tibble(species_gen = "Buffalo") |>
    whep:::.resolve_mms_shares() |>
    dplyr::arrange(mms_type)
  buffalo_oce <- tibble::tibble(
    species_gen = "Buffalo",
    region = "Oceania"
  ) |>
    whep:::.resolve_mms_shares("region") |>
    dplyr::arrange(mms_type)
  expect_equal(buffalo_oce$mms_type, buffalo_global$mms_type)
  expect_equal(buffalo_oce$fraction, buffalo_global$fraction)

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
