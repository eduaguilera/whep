test_that("get_feed_intake local points to build_feed_intake_local", {
  # The local grain is run via the chunked-by-year batch function (the full
  # run is too large for one in-memory result); get_feed_intake redirects there.
  expect_error(
    whep::get_feed_intake(grain = "local"),
    "build_feed_intake_local"
  )
  expect_error(
    whep::get_feed_intake(grain = "local", demand_tier = "ipcc"),
    "build_feed_intake_local"
  )
})

test_that("build_feed_intake_local(example = TRUE) returns the contract", {
  out <- whep::build_feed_intake_local(example = TRUE)
  expect_s3_class(out, "tbl_df")
  expect_setequal(
    names(out),
    c(
      "year",
      "area_code",
      "sub_territory",
      "live_anim_code",
      "item_cbs_code",
      "feed_type",
      "supply",
      "intake",
      "intake_dry_matter",
      "loss",
      "loss_share"
    )
  )
  expect_true(all(out$supply == out$intake))
})

test_that(".resolve_local_years defaults to all years, else the subset", {
  prod <- tibble::tibble(
    year = c(1999, 2000, 2001),
    area_code = 1,
    item_prod_code = "1",
    unit = "heads",
    value = 1
  )
  expect_equal(whep:::.resolve_local_years(NULL, prod), 1999:2001)
  expect_equal(whep:::.resolve_local_years(c(2000L, 2050L), prod), 2000L)
})

test_that("get_feed_intake rejects unknown grain / demand_tier", {
  expect_error(whep::get_feed_intake(grain = "global"))
  expect_error(whep::get_feed_intake(demand_tier = "bouwman"))
})

test_that("get_feed_intake(example = TRUE) is unchanged by the new arguments", {
  expect_identical(
    whep::get_feed_intake(example = TRUE),
    whep::get_feed_intake(
      example = TRUE,
      grain = "national",
      demand_tier = "fcr"
    )
  )
})

test_that(".livestock_crosswalk is the verified 22-row 3-method mapping", {
  cw <- whep:::.livestock_crosswalk()
  expect_equal(nrow(cw), 22L)
  # IPCC Tier-2 energy covers only the 4 ruminant species; pigs/poultry use
  # Bouwman FCR (no Tier-2 energy model); draft/other use Krausmann per-head.
  expect_setequal(unique(cw$demand_source), c("ipcc", "fcr", "krausmann"))
  ipcc <- cw[cw$demand_source == "ipcc", ]
  expect_setequal(
    unique(ipcc$energy_species),
    c("Cattle", "Buffalo", "Sheep", "Goats")
  )
  # Only the IPCC ruminant rows carry an energy_species.
  expect_true(all(is.na(cw$energy_species[cw$demand_source != "ipcc"])))
  # FCR rows (pigs/poultry) must carry a Bouwman class for demand + mix.
  fcr <- cw[cw$demand_source == "fcr", ]
  expect_setequal(unique(fcr$item_bouwman), c("Pigs", "Poultry"))
  expect_true(all(!is.na(cw$live_anim_code)))
  expect_true(all(nzchar(cw$livestock_category)))
})

# Engine 1: DEMAND -------------------------------------------------------------

test_that(".demand_method_label maps the three methods by tier", {
  src <- c("ipcc", "fcr", "krausmann", "ipcc")
  expect_equal(
    whep:::.demand_method_label(src, "ipcc"),
    c(
      "ipcc_tier2_energy",
      "bouwman_fcr",
      "krausmann_per_head",
      "ipcc_tier2_energy"
    )
  )
  # Under the fcr tier even ruminants take the Bouwman FCR magnitude.
  expect_equal(
    whep:::.demand_method_label(src, "fcr"),
    c("bouwman_fcr", "bouwman_fcr", "krausmann_per_head", "bouwman_fcr")
  )
})

test_that(".combine_demand_methods records single or mixed provenance", {
  expect_equal(
    whep:::.combine_demand_methods(c("bouwman_fcr", "bouwman_fcr")),
    "bouwman_fcr"
  )
  # A mixed category (e.g. one ruminant from energy, another fell back to FCR)
  # keeps both methods so provenance is never hidden.
  expect_equal(
    whep:::.combine_demand_methods(c("ipcc_tier2_energy", "bouwman_fcr")),
    "bouwman_fcr+ipcc_tier2_energy"
  )
})

test_that(".energy_to_dm applies the GE/18.45 x 365 /1000 conversion", {
  energy <- tibble::tribble(
    ~year,
    ~area_code,
    ~item_cbs_code,
    ~cohort_heads,
    ~gross_energy,
    ~diet_quality,
    2000L,
    79L,
    976L,
    100,
    20,
    "Medium"
  )
  out <- whep:::.energy_to_dm(energy)
  expected <- 100 * (20 / 18.45) * 365 / 1000
  expect_equal(out$demand_dm_t, expected, tolerance = 1e-6)
  expect_equal(out$live_anim_code, 976L)
})

test_that(".build_demand_energy handles double-typed production codes", {
  # Real get_primary_production carries item_cbs_code / live_anim_code as doubles;
  # the energy bridge keys live_anim_code as a character, so the path must not
  # break on the type (regression for the double-vs-character join).
  production <- tibble::tribble(
    ~year,
    ~area_code,
    ~item_cbs_code,
    ~live_anim_code,
    ~item_prod_code,
    ~unit,
    ~value,
    2000,
    79,
    976,
    976,
    "976",
    "heads",
    1e6
  )
  out <- whep:::.build_demand_energy(production, c(976L))
  expect_true(976L %in% out$live_anim_code)
  expect_gt(out$demand_dm_t[out$live_anim_code == 976L], 0)
})

test_that(".build_demand_energy adds lactation energy from a milk-yield row", {
  # A t_head milk-yield row must reach the energy model (its live_anim_code join
  # keys on a character). Dropping live_anim_code would zero lactation energy and
  # understate dairy demand ~1.85x; this guards that regression. Codes are
  # doubles (real-data shape).
  base <- tibble::tribble(
    ~year,
    ~area_code,
    ~item_cbs_code,
    ~live_anim_code,
    ~item_prod_code,
    ~unit,
    ~value,
    2000,
    79,
    960,
    960,
    "960",
    "heads",
    1e6
  )
  with_milk <- dplyr::bind_rows(
    base,
    tibble::tribble(
      ~year,
      ~area_code,
      ~item_cbs_code,
      ~live_anim_code,
      ~item_prod_code,
      ~unit,
      ~value,
      2000,
      79,
      960,
      960,
      "960",
      "t_head",
      5
    )
  )
  ph_base <- whep:::.build_demand_energy(base, c(960L))
  ph_milk <- whep:::.build_demand_energy(with_milk, c(960L))
  d_base <- ph_base$demand_dm_t[ph_base$live_anim_code == 960L]
  d_milk <- ph_milk$demand_dm_t[ph_milk$live_anim_code == 960L]
  expect_gt(d_milk, d_base * 1.2)
})

test_that(".aggregate_demand_to_category sums codes and keeps method", {
  totals <- tibble::tribble(
    ~year,
    ~area_code,
    ~live_anim_code,
    ~demand_dm_t,
    ~method_demand,
    2000L,
    79L,
    1049L,
    100, # Pigs
    "bouwman_fcr",
    2000L,
    79L,
    1051L,
    50, # Hogs to Pigs
    "bouwman_fcr",
    2000L,
    79L,
    1096L,
    30, # Horses
    "krausmann_per_head"
  )
  out <- whep:::.aggregate_demand_to_category(
    totals,
    whep:::.livestock_crosswalk()
  )
  pigs <- out[out$livestock_category == "Pigs", ]
  expect_equal(pigs$demand_dm_t, 150)
  expect_equal(pigs$method_demand, "bouwman_fcr")
  horses <- out[out$livestock_category == "Horses", ]
  expect_equal(horses$method_demand, "krausmann_per_head")
})

test_that(".build_demand_energy yields biologically plausible ruminant DM", {
  production <- tibble::tribble(
    ~year,
    ~area_code,
    ~item_cbs_code,
    ~live_anim_code,
    ~item_prod_code,
    ~unit,
    ~value,
    2000L,
    79L,
    976L,
    NA_character_,
    "976",
    "heads",
    1e6
  )
  cw <- whep:::.livestock_crosswalk()
  ruminants <- cw$live_anim_code[cw$demand_source == "ipcc"]
  out <- whep:::.build_demand_energy(production, ruminants)
  expect_true(976L %in% out$live_anim_code)
  per_head <- out$demand_dm_t[out$live_anim_code == 976L] / 1e6
  # A ~45 kg ewe eats roughly 0.3-0.6 t DM/yr.
  expect_gt(per_head, 0.2)
  expect_lt(per_head, 0.7)
})

test_that(".build_feed_demand_total returns the empty schema for no input", {
  empty <- tibble::tibble(
    year = integer(),
    area_code = integer(),
    item_prod_code = character(),
    item_cbs_code = integer(),
    unit = character(),
    value = numeric()
  )
  out <- whep:::.build_feed_demand_total(empty, "ipcc")
  expect_equal(nrow(out), 0L)
  expect_setequal(
    names(out),
    c("year", "area_code", "livestock_category", "demand_dm_t", "method_demand")
  )
})

test_that(".overlay_energy_demand keeps uncovered ruminants without crashing", {
  # A ruminant present in legacy totals (product-only, no heads row) cannot be
  # covered by the energy overlay; it must keep its FCR magnitude + label and
  # warn, never error (the malformed-cli crash regression).
  totals <- tibble::tribble(
    ~year,
    ~area_code,
    ~live_anim_code,
    ~demand_dm_t,
    ~method_demand,
    2000L,
    79L,
    946L,
    500, # buffalo, legacy FCR (no heads)
    "bouwman_fcr",
    2000L,
    79L,
    976L,
    999, # sheep, legacy (will be overlaid by energy)
    "bouwman_fcr"
  )
  production <- tibble::tribble(
    ~year,
    ~area_code,
    ~item_cbs_code,
    ~live_anim_code,
    ~item_prod_code,
    ~unit,
    ~value,
    2000L,
    79L,
    976L,
    NA_character_,
    "976",
    "heads",
    1e6 # only sheep heads
  )
  cw <- whep:::.livestock_crosswalk()
  expect_warning(
    out <- whep:::.overlay_energy_demand(totals, production, cw),
    "could not be computed"
  )
  buffalo <- out[out$live_anim_code == 946L, ]
  expect_equal(buffalo$demand_dm_t, 500)
  expect_equal(buffalo$method_demand, "bouwman_fcr")
  sheep <- out[out$live_anim_code == 976L, ]
  expect_equal(sheep$method_demand, "ipcc_tier2_energy")
  expect_true(sheep$demand_dm_t != 999)
})

test_that("every ruminant cohort has a Global GLEAM weight", {
  # Guards against a future cohort being added to gleam_livestock_categories
  # without a matching weight, which would silently undercount energy demand.
  ruminants <- c("Cattle", "Buffalo", "Sheep", "Goats")
  cats <- whep::gleam_livestock_categories |>
    dplyr::filter(species %in% ruminants) |>
    dplyr::distinct(species, cohort)
  global_w <- whep::gleam_animal_weights |>
    dplyr::filter(region == "Global", species %in% ruminants) |>
    dplyr::distinct(species, cohort)
  missing <- dplyr::anti_join(cats, global_w, by = c("species", "cohort"))
  expect_equal(nrow(missing), 0L)
})

# Engine 2: MIX ----------------------------------------------------------------

test_that(".bouwman_feedtype_shares sum to 1 per product-region-year", {
  shares <- whep:::.bouwman_feedtype_shares(whep::conv_bouwman, 1970L)
  totals <- shares |>
    dplyr::summarise(
      tot = sum(dm_share, na.rm = TRUE),
      .by = c(item_bouwman, region_bouwman, year)
    )
  expect_true(all(abs(totals$tot - 1) < 1e-6))
})

test_that(".feedtype_feed_quality maps the 5 feed types, only grass fixed", {
  q <- whep:::.feedtype_feed_quality()
  expect_setequal(
    q$feed_type,
    c("grass", "crops", "residues", "animals", "scavenging")
  )
  expect_true(q$fixed_demand[q$feed_type == "grass"])
  expect_false(any(q$fixed_demand[q$feed_type != "grass"]))
})

test_that(".build_feed_mix conserves the total and emits a valid schema", {
  region <- whep:::.feed_region_lookup(whep::polities_cats)
  bouwman_regions <- unique(whep::conv_bouwman$region_bouwman)
  area <- region$area_code[region$region_bouwman %in% bouwman_regions][1]
  demand_total <- tibble::tibble(
    year = 1970L,
    area_code = area,
    livestock_category = "Cattle_milk",
    demand_dm_t = 1000,
    method_demand = "ipcc_tier2_energy"
  )
  out <- whep:::.build_feed_mix(demand_total)
  expect_setequal(
    names(out),
    c(
      "year",
      "territory",
      "sub_territory",
      "livestock_category",
      "item_cbs_code",
      "feed_group",
      "feed_quality",
      "demand_dm_t",
      "fixed_demand"
    )
  )
  # Bouwman shares sum to 1, so the feed-type split conserves the total.
  expect_equal(sum(out$demand_dm_t), 1000, tolerance = 1e-6)
  expect_true(all(out$fixed_demand[out$feed_quality == "grass"]))
  # The emitted table is accepted by redistribute_feed.
  avail <- tibble::tibble(
    year = 1970L,
    territory = unique(out$territory),
    sub_territory = NA_character_,
    item_cbs_code = NA_integer_,
    feed_group = NA_character_,
    feed_quality = "high_quality",
    avail_dm_t = 1e6,
    feed_scale = "national"
  )
  expect_no_error(
    whep::redistribute_feed(
      out |> dplyr::mutate(fixed_demand = FALSE),
      avail
    )
  )
})

test_that(".build_feed_mix borrows grazer shares for draft species", {
  region <- whep:::.feed_region_lookup(whep::polities_cats)
  bouwman_regions <- unique(whep::conv_bouwman$region_bouwman)
  area <- region$area_code[region$region_bouwman %in% bouwman_regions][1]
  demand_total <- tibble::tibble(
    year = 1970L,
    area_code = area,
    livestock_category = "Horses", # no Bouwman class -> grazer-average shares
    demand_dm_t = 500,
    method_demand = "krausmann_per_head"
  )
  out <- whep:::.build_feed_mix(demand_total)
  expect_gt(nrow(out), 0L)
  expect_equal(sum(out$demand_dm_t), 500, tolerance = 1e-6)
  expect_true("grass" %in% out$feed_quality)
})

test_that(".build_feed_mix conserves the Other category (no graniv fan-out)", {
  # Other maps to several graniv_grazers values (Grazers/Bees/Game); the bridge
  # must not fan its demand into duplicates (a 3x conservation bug).
  region <- whep:::.feed_region_lookup(whep::polities_cats)
  bouwman_regions <- unique(whep::conv_bouwman$region_bouwman)
  area <- region$area_code[region$region_bouwman %in% bouwman_regions][1]
  demand_total <- tibble::tibble(
    year = 1970L,
    area_code = area,
    livestock_category = "Other",
    demand_dm_t = 1000,
    method_demand = "krausmann_per_head"
  )
  out <- whep:::.build_feed_mix(demand_total)
  expect_equal(sum(out$demand_dm_t), 1000, tolerance = 1e-6)
})

test_that(".build_feed_mix warns and drops demand for an area with no region", {
  demand_total <- tibble::tibble(
    year = 1970L,
    area_code = 999999L,
    livestock_category = "Cattle_meat",
    demand_dm_t = 1000,
    method_demand = "ipcc_tier2_energy"
  )
  expect_warning(
    out <- whep:::.build_feed_mix(demand_total),
    "dropped from the mix"
  )
  expect_equal(nrow(out), 0L)
})

# Engine 3: ALLOCATION (national grain) ----------------------------------------

test_that(".build_feed_avail_national tags CBS feed with quality + scale", {
  cbs <- tibble::tribble(
    ~year,
    ~area_code,
    ~item_cbs_code,
    ~feed,
    1970L,
    1L,
    2591L,
    1000, # groundnut cake -> high_quality
    1970L,
    1L,
    2105L,
    500 # straw to residues
  )
  out <- whep:::.build_feed_avail_national(cbs)
  expect_setequal(
    names(out),
    c(
      "year",
      "territory",
      "sub_territory",
      "item_cbs_code",
      "feed_group",
      "feed_quality",
      "avail_dm_t",
      "feed_scale"
    )
  )
  expect_true(all(out$feed_scale == "national"))
  expect_true(all(is.na(out$sub_territory)))
  expect_true(all(c("high_quality", "residues") %in% out$feed_quality))
  expect_true(all(out$avail_dm_t > 0))
  # avail = feed * 0.9 * product_kgdm_kgfm, so DM is below the fresh-matter feed.
  cake <- out[out$item_cbs_code == 2591L, ]
  expect_lt(cake$avail_dm_t, 1000)
})

test_that(".run_redistribute_national meets grass, caps concentrates", {
  region <- whep:::.feed_region_lookup(whep::polities_cats)
  bouwman_regions <- unique(whep::conv_bouwman$region_bouwman)
  area <- region$area_code[region$region_bouwman %in% bouwman_regions][1]
  prod <- tibble::tribble(
    ~year,
    ~area_code,
    ~item_cbs_code,
    ~live_anim_code,
    ~item_prod_code,
    ~unit,
    ~value,
    1970L,
    area,
    960L,
    NA_character_,
    "960",
    "heads",
    1e6 # dairy cows: grass + concentrate + residue demand
  )
  cbs <- tibble::tribble(
    ~year,
    ~area_code,
    ~item_cbs_code,
    ~feed,
    1970L,
    area,
    2591L,
    1e5 # limited high-quality supply -> underfeeding
  )
  out <- whep:::.run_redistribute_national(prod, cbs, "ipcc")
  grass <- out[out$feed_quality == "grass", ]
  # Grass demand is met in full via the unlimited grassland sink.
  expect_true(all(abs(grass$intake_dm_t - grass$demand_dm_t) < 1e-6))
  expect_true(all(grass$hierarchy_level == "6_grassland_unlimited"))
  # High-quality intake never exceeds the CBS availability offered.
  avail <- whep:::.build_feed_avail_national(cbs)
  hq_avail <- sum(avail$avail_dm_t[avail$feed_quality == "high_quality"])
  hq_intake <- sum(out$intake_dm_t[out$feed_quality == "high_quality"])
  expect_lte(hq_intake, hq_avail + 1e-6)
  expect_gt(hq_intake, 0)
})

test_that(".add_scavenging_avail injects avail equal to scavenging demand", {
  feed_demand <- tibble::tribble(
    ~year,
    ~territory,
    ~sub_territory,
    ~livestock_category,
    ~item_cbs_code,
    ~feed_group,
    ~feed_quality,
    ~demand_dm_t,
    ~fixed_demand,
    2000L,
    "79",
    NA_character_,
    "Pigs",
    NA_integer_,
    NA_character_,
    "scavenging",
    30,
    FALSE,
    2000L,
    "79",
    NA_character_,
    "Poultry",
    NA_integer_,
    NA_character_,
    "scavenging",
    20,
    FALSE,
    2000L,
    "79",
    NA_character_,
    "Pigs",
    NA_integer_,
    NA_character_,
    "high_quality",
    50,
    FALSE
  )
  avail0 <- tibble::tibble(
    year = integer(),
    territory = character(),
    sub_territory = character(),
    item_cbs_code = integer(),
    feed_group = character(),
    feed_quality = character(),
    avail_dm_t = numeric(),
    feed_scale = character()
  )
  out <- whep:::.add_scavenging_avail(avail0, feed_demand)
  scav <- out[out$feed_quality == "scavenging", ]
  expect_equal(nrow(scav), 1L)
  expect_equal(scav$item_cbs_code, 3500L)
  expect_equal(scav$avail_dm_t, 50) # 30 + 20, summed across categories
  expect_equal(scav$feed_scale, "national")
})

test_that("scavenging demand is met from its own avail, not spilled elsewhere", {
  feed_demand <- tibble::tribble(
    ~year,
    ~territory,
    ~sub_territory,
    ~livestock_category,
    ~item_cbs_code,
    ~feed_group,
    ~feed_quality,
    ~demand_dm_t,
    ~fixed_demand,
    2000L,
    "79",
    NA_character_,
    "Pigs",
    NA_integer_,
    NA_character_,
    "scavenging",
    30,
    FALSE,
    2000L,
    "79",
    NA_character_,
    "Pigs",
    NA_integer_,
    NA_character_,
    "high_quality",
    100,
    FALSE
  )
  avail0 <- tibble::tibble(
    year = integer(),
    territory = character(),
    sub_territory = character(),
    item_cbs_code = integer(),
    feed_group = character(),
    feed_quality = character(),
    avail_dm_t = numeric(),
    feed_scale = character()
  )
  feed_avail <- whep:::.add_scavenging_avail(avail0, feed_demand)
  res <- whep::redistribute_feed(feed_demand, feed_avail)
  # Scavenging is fully met from its own bounded availability.
  scav_intake <- sum(res$intake_dm_t[res$feed_quality == "scavenging"])
  expect_equal(scav_intake, 30, tolerance = 1e-6)
  # The scavenging item never feeds the high_quality demand (no spillover).
  hq <- res[res$feed_quality == "high_quality", ]
  expect_false(any(hq$item_cbs_code %in% 3500L, na.rm = TRUE))
})

# Phase 6: reshape to the get_feed_intake contract -----------------------------

test_that(".item_feedtype_lookup labels grass and folds additives into crops", {
  ft <- whep:::.item_feedtype_lookup()
  expect_setequal(
    unique(ft$feed_type),
    c("animals", "crops", "grass", "residues", "scavenging")
  )
  # The canonical Grassland item is grass (the grass-sink fallback code).
  expect_equal(ft$feed_type[ft$item_cbs_code == 3000L], "grass")
})

test_that(".item_kgdm_lookup gives the grass density used by the sink", {
  kg <- whep:::.item_kgdm_lookup()
  expect_true(all(kg$product_kgdm_kgfm > 0))
  expect_equal(kg$product_kgdm_kgfm[kg$item_cbs_code == 3000L], 0.2)
})

test_that(".demand_code_shares split sums to 1 within each category", {
  codes <- tibble::tribble(
    ~year,
    ~area_code,
    ~live_anim_code,
    ~demand_dm_t,
    ~method_demand,
    2000L,
    79L,
    1049L,
    60, # Pigs
    "bouwman_fcr",
    2000L,
    79L,
    1051L,
    40, # Hogs to Pigs
    "bouwman_fcr"
  )
  out <- whep:::.demand_code_shares(codes, whep:::.livestock_crosswalk())
  pigs <- out[out$livestock_category == "Pigs", ]
  expect_equal(sum(pigs$code_share), 1, tolerance = 1e-9)
  expect_equal(pigs$code_share[pigs$live_anim_code == 1049L], 0.6)
})

test_that(".safe_share falls back to an equal split when total is zero", {
  expect_equal(whep:::.safe_share(c(0, 0, 0)), rep(1 / 3, 3))
  expect_equal(whep:::.safe_share(c(3, 1)), c(0.75, 0.25))
})

test_that(".safe_share treats NA and negative entries as zero (no DM leak)", {
  # An NA or negative share (a missing/invalid gridded-heads cell) must not
  # propagate NA into the demand nor inflate the other cells beyond the total.
  expect_equal(whep:::.safe_share(c(0.6, NA, 0.4)), c(0.6, 0, 0.4))
  expect_equal(whep:::.safe_share(c(3, -1, 1)), c(0.75, 0, 0.25))
})

test_that(".reshape_redistribute_intake returns the empty contract schema", {
  out <- whep:::.reshape_redistribute_intake(
    whep::redistribute_feed(
      tibble::tibble(
        year = integer(),
        territory = character(),
        sub_territory = character(),
        livestock_category = character(),
        item_cbs_code = integer(),
        feed_group = character(),
        feed_quality = character(),
        demand_dm_t = numeric(),
        fixed_demand = logical()
      ),
      tibble::tibble(
        year = integer(),
        sub_territory = character(),
        item_cbs_code = integer(),
        feed_group = character(),
        feed_quality = character(),
        avail_dm_t = numeric(),
        feed_scale = character()
      )
    ),
    tibble::tibble(
      year = integer(),
      area_code = integer(),
      livestock_category = character(),
      live_anim_code = integer(),
      code_share = numeric()
    )
  )
  expect_equal(nrow(out), 0L)
  expect_setequal(
    names(out),
    c(
      "year",
      "area_code",
      "live_anim_code",
      "item_cbs_code",
      "feed_type",
      "supply",
      "intake",
      "intake_dry_matter",
      "loss",
      "loss_share"
    )
  )
})

test_that(".reshape_redistribute_intake splits, labels, and converts to fresh", {
  ft <- whep:::.item_feedtype_lookup()
  expected_cake <- ft$feed_type[ft$item_cbs_code == 2591L]
  # A redistribute-style result: one Pigs concentrate row + one Cattle_milk
  # grass-sink row (item NA, feed_group grass).
  result <- tibble::tribble(
    ~year,
    ~territory,
    ~sub_territory,
    ~livestock_category,
    ~item_cbs_code,
    ~feed_group,
    ~feed_quality,
    ~intake_dm_t,
    2000L,
    "79",
    NA_character_,
    "Pigs",
    2591L,
    "oilcakes",
    "high_quality",
    100,
    2000L,
    "79",
    NA_character_,
    "Cattle_milk",
    NA_integer_,
    "grass",
    "grass",
    50
  )
  code_shares <- tibble::tribble(
    ~year,
    ~area_code,
    ~livestock_category,
    ~live_anim_code,
    ~code_share,
    2000L,
    79L,
    "Pigs",
    1049L,
    0.6,
    2000L,
    79L,
    "Pigs",
    1051L,
    0.4,
    2000L,
    79L,
    "Cattle_milk",
    960L,
    1
  )
  out <- whep:::.reshape_redistribute_intake(result, code_shares)
  # Demand-pull semantics.
  expect_true(all(out$supply == out$intake))
  expect_true(all(out$loss == 0))
  expect_true(all(out$loss_share == 0))
  # Conservation: total dry matter unchanged by the reshape.
  expect_equal(sum(out$intake_dry_matter), 150, tolerance = 1e-6)
  # Reverse-split: Pigs concentrate (100 DM) fans into 1049/1051 at 60/40.
  pigs <- out[out$live_anim_code %in% c(1049L, 1051L), ]
  expect_equal(sum(pigs$intake_dry_matter), 100, tolerance = 1e-6)
  expect_equal(
    pigs$intake_dry_matter[pigs$live_anim_code == 1049L],
    60,
    tolerance = 1e-6
  )
  expect_equal(unique(pigs$feed_type), expected_cake)
  # Grass sink: relabeled to item 3000, feed_type grass, fresh = DM / 0.2.
  grass <- out[out$feed_type == "grass", ]
  expect_equal(grass$item_cbs_code, 3000L)
  expect_equal(grass$intake_dry_matter, 50, tolerance = 1e-6)
  expect_equal(grass$intake, 50 / 0.2, tolerance = 1e-6)
  expect_equal(grass$live_anim_code, 960L)
})

test_that("national redistribute + reshape yields the contract and conserves DM", {
  region <- whep:::.feed_region_lookup(whep::polities_cats)
  bouwman_regions <- unique(whep::conv_bouwman$region_bouwman)
  area <- region$area_code[region$region_bouwman %in% bouwman_regions][1]
  prod <- tibble::tribble(
    ~year,
    ~area_code,
    ~item_cbs_code,
    ~live_anim_code,
    ~item_prod_code,
    ~unit,
    ~value,
    1970L,
    area,
    960L,
    NA_character_,
    "960",
    "heads",
    1e6
  )
  cbs <- tibble::tribble(
    ~year,
    ~area_code,
    ~item_cbs_code,
    ~feed,
    1970L,
    area,
    2591L,
    1e5
  )
  engine <- whep:::.national_redistribute(prod, cbs, "ipcc")
  out <- whep:::.reshape_redistribute_intake(
    engine$result,
    engine$code_shares
  )
  expect_setequal(
    names(out),
    c(
      "year",
      "area_code",
      "live_anim_code",
      "item_cbs_code",
      "feed_type",
      "supply",
      "intake",
      "intake_dry_matter",
      "loss",
      "loss_share"
    )
  )
  expect_type(out$area_code, "integer")
  # The dairy cohort's live_anim_code survives the reverse-split.
  expect_true(960L %in% out$live_anim_code)
  expect_true("grass" %in% out$feed_type)
  # Reshape conserves the redistribute dry-matter total.
  expect_equal(
    sum(out$intake_dry_matter),
    sum(engine$result$intake_dm_t),
    tolerance = 1e-6
  )
})

test_that("reshape warns when intake has no reverse-split weight", {
  # A category present in the result but absent from code_shares (a desync, or
  # an area_code that did not parse from territory) would be silently dropped by
  # the inner join; the guard must surface it instead.
  result <- tibble::tribble(
    ~year,
    ~territory,
    ~sub_territory,
    ~livestock_category,
    ~item_cbs_code,
    ~feed_group,
    ~feed_quality,
    ~intake_dm_t,
    2000L,
    "79",
    NA_character_,
    "Ghosts",
    2591L,
    "oilcakes",
    "high_quality",
    100
  )
  code_shares <- tibble::tibble(
    year = 2000L,
    area_code = 79L,
    livestock_category = "Pigs",
    live_anim_code = 1049L,
    code_share = 1
  )
  expect_warning(
    out <- whep:::.reshape_redistribute_intake(result, code_shares),
    "intake is dropped"
  )
  expect_equal(nrow(out), 0L)
})

# Provincial grain (sub_territory = cell) --------------------------------------

test_that(".distribute_demand_to_cells splits demand to cells and conserves", {
  feed_demand <- tibble::tribble(
    ~year,
    ~territory,
    ~sub_territory,
    ~livestock_category,
    ~item_cbs_code,
    ~feed_group,
    ~feed_quality,
    ~demand_dm_t,
    ~fixed_demand,
    1970L,
    "79",
    NA_character_,
    "Cattle_milk",
    NA_integer_,
    NA_character_,
    "grass",
    1000,
    TRUE,
    1970L,
    "79",
    NA_character_,
    "Cattle_milk",
    NA_integer_,
    NA_character_,
    "high_quality",
    500,
    FALSE
  )
  cell_shares <- tibble::tribble(
    ~year,
    ~territory,
    ~livestock_category,
    ~sub_territory,
    ~cell_share,
    1970L,
    "79",
    "Cattle_milk",
    "cellA",
    0.7,
    1970L,
    "79",
    "Cattle_milk",
    "cellB",
    0.3
  )
  out <- whep:::.distribute_demand_to_cells(feed_demand, cell_shares)
  expect_setequal(unique(out$sub_territory), c("cellA", "cellB"))
  # Cell shares sum to 1, so the total is conserved.
  expect_equal(sum(out$demand_dm_t), 1500, tolerance = 1e-9)
  grass_a <- out$demand_dm_t[
    out$sub_territory == "cellA" & out$feed_quality == "grass"
  ]
  expect_equal(grass_a, 700, tolerance = 1e-9)
  expect_true(all(
    c("territory", "sub_territory", "fixed_demand") %in% names(out)
  ))
})

test_that(".distribute_demand_to_cells renormalises shares not summing to 1", {
  # Real gridded heads need not sum to exactly 1 (rounding, border cells); the
  # renormalisation must conserve the total rather than drop or inflate it.
  feed_demand <- tibble::tribble(
    ~year,
    ~territory,
    ~sub_territory,
    ~livestock_category,
    ~item_cbs_code,
    ~feed_group,
    ~feed_quality,
    ~demand_dm_t,
    ~fixed_demand,
    1970L,
    "79",
    NA_character_,
    "Cattle_milk",
    NA_integer_,
    NA_character_,
    "grass",
    1000,
    TRUE
  )
  cell_shares <- tibble::tribble(
    ~year,
    ~territory,
    ~livestock_category,
    ~sub_territory,
    ~cell_share,
    1970L,
    "79",
    "Cattle_milk",
    "cellA",
    0.3,
    1970L,
    "79",
    "Cattle_milk",
    "cellB",
    0.2
  )
  out <- whep:::.distribute_demand_to_cells(feed_demand, cell_shares)
  expect_equal(sum(out$demand_dm_t), 1000, tolerance = 1e-9)
  # 0.3 / (0.3 + 0.2) = 0.6 of the total.
  expect_equal(
    out$demand_dm_t[out$sub_territory == "cellA"],
    600,
    tolerance = 1e-9
  )
})

test_that("reshape reports grass-deficit substitute as residues, not grass", {
  # Substitute is leftover non-grass roughage filling a bounded-grass deficit;
  # it must be labelled residues at a roughage density, not folded into grass
  # (which would over-state fresh matter ~5x).
  result <- tibble::tribble(
    ~year,
    ~territory,
    ~sub_territory,
    ~livestock_category,
    ~item_cbs_code,
    ~feed_group,
    ~feed_quality,
    ~intake_dm_t,
    2000L,
    "79",
    NA_character_,
    "Cattle_milk",
    NA_integer_,
    "substitute",
    "substitute",
    90
  )
  code_shares <- tibble::tibble(
    year = 2000L,
    area_code = 79L,
    livestock_category = "Cattle_milk",
    live_anim_code = 960L,
    code_share = 1
  )
  out <- whep:::.reshape_redistribute_intake(result, code_shares)
  expect_equal(nrow(out), 1L)
  expect_equal(out$feed_type, "residues")
  expect_equal(out$intake_dry_matter, 90, tolerance = 1e-6)
  # Fresh matter at the dry-roughage density 0.9, not the grass density 0.2.
  expect_equal(out$intake, 90 / 0.9, tolerance = 1e-6)
})

test_that(".grass_to_cells maps grass to per-cell local grass_availability", {
  grass <- tibble::tribble(
    ~lon,
    ~lat,
    ~year,
    ~grass_avail_dm_t,
    10.25,
    50.25,
    2000L,
    1000,
    10.75,
    50.25,
    2000L,
    500
  )
  # cell (10.25, 50.25) is a border cell split 0.6 polity 1 / 0.4 polity 2; the
  # other cell is wholly polity 1.
  cell_polity <- tibble::tribble(
    ~lon,
    ~lat,
    ~area_code,
    ~polity_frac,
    10.25,
    50.25,
    1L,
    0.6,
    10.25,
    50.25,
    2L,
    0.4,
    10.75,
    50.25,
    1L,
    1.0
  )
  out <- whep:::.grass_to_cells(grass, cell_polity)
  expect_setequal(
    names(out),
    c("year", "territory", "sub_territory", "grass_avail_dm_t")
  )
  cell_b <- whep:::.cell_id(10.25, 50.25)
  es_border <- out$grass_avail_dm_t[
    out$territory == "1" & out$sub_territory == cell_b
  ]
  expect_equal(es_border, 600, tolerance = 1e-9)
  expect_equal(
    out$grass_avail_dm_t[out$territory == "2"],
    400,
    tolerance = 1e-9
  )
  # The split conserves the cell's total grass.
  expect_equal(sum(out$grass_avail_dm_t), 1500, tolerance = 1e-9)
})

test_that(".heads_to_cell_shares maps species groups and shares per category", {
  gridded_heads <- tibble::tribble(
    ~year,
    ~species_group,
    ~area_code,
    ~lon,
    ~lat,
    ~heads,
    2000L,
    "sheep_goats",
    1L,
    10.25,
    50.25,
    70,
    2000L,
    "sheep_goats",
    1L,
    10.75,
    50.25,
    30,
    2000L,
    "cattle_dairy",
    1L,
    10.25,
    50.25,
    100
  )
  out <- whep:::.heads_to_cell_shares(gridded_heads)
  expect_setequal(
    names(out),
    c("year", "territory", "livestock_category", "sub_territory", "cell_share")
  )
  # sheep_goats feeds BOTH Sheep and Goats with the SAME 70/30 cell pattern.
  expect_setequal(
    unique(out$livestock_category),
    c("Sheep", "Goats", "Cattle_milk")
  )
  cell_a <- whep:::.cell_id(10.25, 50.25)
  sheep <- out[out$livestock_category == "Sheep", ]
  goats <- out[out$livestock_category == "Goats", ]
  expect_equal(sum(sheep$cell_share), 1, tolerance = 1e-9)
  expect_equal(
    sheep$cell_share[sheep$sub_territory == cell_a],
    0.7,
    tolerance = 1e-9
  )
  expect_equal(
    goats$cell_share[goats$sub_territory == cell_a],
    0.7,
    tolerance = 1e-9
  )
  # Cattle_milk occupies only cell_a, so its share is 1.
  expect_equal(out$cell_share[out$livestock_category == "Cattle_milk"], 1)
})

test_that(".species_group_to_category covers every crosswalk category", {
  # Guards against a feed category with no gridded proxy (its demand would be
  # dropped from the local allocation).
  m <- whep:::.species_group_to_category()
  cw_cats <- unique(whep:::.livestock_crosswalk()$livestock_category)
  expect_true(all(cw_cats %in% m$livestock_category))
})

test_that(".distribute_demand_to_cells warns on demand with no cell share", {
  feed_demand <- tibble::tribble(
    ~year,
    ~territory,
    ~sub_territory,
    ~livestock_category,
    ~item_cbs_code,
    ~feed_group,
    ~feed_quality,
    ~demand_dm_t,
    ~fixed_demand,
    1970L,
    "79",
    NA_character_,
    "Ghosts",
    NA_integer_,
    NA_character_,
    "grass",
    1000,
    TRUE
  )
  cell_shares <- tibble::tribble(
    ~year,
    ~territory,
    ~livestock_category,
    ~sub_territory,
    ~cell_share,
    1970L,
    "79",
    "Cattle_milk",
    "cellA",
    1
  )
  expect_warning(
    out <- whep:::.distribute_demand_to_cells(feed_demand, cell_shares),
    "dropped from the local"
  )
  expect_equal(nrow(out), 0L)
})

test_that("local run yields a per-cell contract, grass capped per cell", {
  region <- whep:::.feed_region_lookup(whep::polities_cats)
  bouwman_regions <- unique(whep::conv_bouwman$region_bouwman)
  area <- region$area_code[region$region_bouwman %in% bouwman_regions][1]
  prod <- tibble::tribble(
    ~year,
    ~area_code,
    ~item_cbs_code,
    ~live_anim_code,
    ~item_prod_code,
    ~unit,
    ~value,
    1970L,
    area,
    960L,
    NA_character_,
    "960",
    "heads",
    1e6
  )
  cbs <- tibble::tribble(
    ~year,
    ~area_code,
    ~item_cbs_code,
    ~feed,
    1970L,
    area,
    2591L,
    1e5
  )
  cell_shares <- tibble::tribble(
    ~year,
    ~territory,
    ~livestock_category,
    ~sub_territory,
    ~cell_share,
    1970L,
    as.character(area),
    "Cattle_milk",
    "cellA",
    0.7,
    1970L,
    as.character(area),
    "Cattle_milk",
    "cellB",
    0.3
  )
  # cellA grass ceiling is tight (forces a per-cell cap); cellB is effectively
  # unlimited. Grass availability carries territory so border cells are not
  # conflated across polities.
  grass_avail <- tibble::tribble(
    ~year,
    ~territory,
    ~sub_territory,
    ~grass_avail_dm_t,
    1970L,
    as.character(area),
    "cellA",
    1e5,
    1970L,
    as.character(area),
    "cellB",
    1e12
  )
  eng <- whep:::.run_redistribute_local(
    prod,
    cbs,
    "ipcc",
    list(cell_shares = cell_shares, grass_avail = grass_avail)
  )
  out <- whep:::.reshape_redistribute_intake(
    eng$result,
    eng$code_shares,
    local = TRUE
  )
  expect_true("sub_territory" %in% names(out))
  expect_setequal(unique(out$sub_territory), c("cellA", "cellB"))
  expect_true(960L %in% out$live_anim_code)
  # The reshape conserves the redistribute dry-matter total (substitute folded).
  expect_equal(
    sum(out$intake_dry_matter),
    sum(eng$result$intake_dm_t),
    tolerance = 1e-6
  )
  grass_a <- sum(out$intake_dry_matter[
    out$feed_type == "grass" & out$sub_territory == "cellA"
  ])
  grass_b <- sum(out$intake_dry_matter[
    out$feed_type == "grass" & out$sub_territory == "cellB"
  ])
  # cellA holds 70% of the dairy herd, so without a cap it would eat MORE grass
  # than cellB. The per-cell cap reverses that: cellA grass < cellB grass.
  expect_lt(grass_a, grass_b)
})

test_that(".apply_grass_border_grazing pulls neighbour surplus to a deficit cell", {
  # cellA (deficit: demand 100, capped to its ceiling 30) borders cellB (surplus:
  # demand 20 of a 100 ceiling). With a 10% border allowance, cellA recovers
  # 0.1 x cellB-surplus(80) = 8 from across the edge.
  result <- tibble::tribble(
    ~year,
    ~territory,
    ~sub_territory,
    ~livestock_category,
    ~item_cbs_code,
    ~feed_group,
    ~feed_quality,
    ~demand_dm_t,
    ~intake_dm_t,
    ~scaling_factor,
    ~hierarchy_level,
    ~requested_item,
    ~source_compartment,
    ~fixed_demand,
    2000L,
    "1",
    "10.25_50.25",
    "Cattle_meat",
    NA_integer_,
    "grass",
    "grass",
    100,
    30,
    0.3,
    "6_grassland_unlimited",
    NA_integer_,
    "10.25_50.25",
    TRUE,
    2000L,
    "1",
    "10.75_50.25",
    "Cattle_meat",
    NA_integer_,
    "grass",
    "grass",
    20,
    20,
    1,
    "6_grassland_unlimited",
    NA_integer_,
    "10.75_50.25",
    TRUE
  )
  grass_avail <- tibble::tribble(
    ~year,
    ~territory,
    ~sub_territory,
    ~grass_avail_dm_t,
    2000L,
    "1",
    "10.25_50.25",
    30,
    2000L,
    "1",
    "10.75_50.25",
    100
  )
  out <- whep:::.apply_grass_border_grazing(result, grass_avail, 0.1)
  border <- out[out$hierarchy_level == "8_grass_border_grazing", ]
  expect_equal(nrow(border), 1L)
  expect_equal(border$sub_territory, "10.25_50.25")
  expect_equal(border$intake_dm_t, 8, tolerance = 1e-6)
  expect_equal(border$feed_quality, "grass")
  # Conservation: total grass intake rises by exactly the recovered 8 (drawn from
  # the neighbour's unused ceiling, so the global grass is still respected).
  expect_equal(
    sum(out$intake_dm_t) - sum(result$intake_dm_t),
    8,
    tolerance = 1e-6
  )
})

test_that(".apply_grass_border_grazing tops up only the deficit left after substitute", {
  # cellA: grass demand 100, capped to 30, with a 50 non-grass substitute fill, so
  # only 20 of demand is still unmet. Border grazing must fill at most that 20
  # (not the full 70 grass gap), so total intake never exceeds demand.
  result <- tibble::tribble(
    ~year,
    ~territory,
    ~sub_territory,
    ~livestock_category,
    ~item_cbs_code,
    ~feed_group,
    ~feed_quality,
    ~demand_dm_t,
    ~intake_dm_t,
    ~scaling_factor,
    ~hierarchy_level,
    ~requested_item,
    ~source_compartment,
    ~fixed_demand,
    2000L,
    "1",
    "10.25_50.25",
    "Cattle_meat",
    NA_integer_,
    "grass",
    "grass",
    100,
    30,
    0.3,
    "6_grassland_unlimited",
    NA_integer_,
    "10.25_50.25",
    TRUE,
    2000L,
    "1",
    "10.25_50.25",
    "Cattle_meat",
    NA_integer_,
    "substitute",
    "substitute",
    0,
    50,
    NA_real_,
    "7_grass_deficit_substitute",
    NA_integer_,
    "10.25_50.25",
    TRUE,
    2000L,
    "1",
    "10.75_50.25",
    "Cattle_meat",
    NA_integer_,
    "grass",
    "grass",
    20,
    20,
    1,
    "6_grassland_unlimited",
    NA_integer_,
    "10.75_50.25",
    TRUE
  )
  grass_avail <- tibble::tribble(
    ~year,
    ~territory,
    ~sub_territory,
    ~grass_avail_dm_t,
    2000L,
    "1",
    "10.25_50.25",
    30,
    2000L,
    "1",
    "10.75_50.25",
    100
  )
  # allowance 0.5 -> neighbour offers 0.5 x 80 = 40, but only 20 of demand is unmet.
  out <- whep:::.apply_grass_border_grazing(result, grass_avail, 0.5)
  border <- out[out$hierarchy_level == "8_grass_border_grazing", ]
  expect_equal(sum(border$intake_dm_t), 20, tolerance = 1e-6)
  # cellA total intake (grass 30 + substitute 50 + border 20) equals its demand.
  cell_a <- out[out$sub_territory == "10.25_50.25", ]
  expect_equal(sum(cell_a$intake_dm_t), 100, tolerance = 1e-6)
})

test_that(".apply_grass_border_grazing does nothing without a surplus neighbour", {
  # Two deficit cells that are NOT king-move neighbours: no border grazing.
  result <- tibble::tribble(
    ~year,
    ~territory,
    ~sub_territory,
    ~livestock_category,
    ~item_cbs_code,
    ~feed_group,
    ~feed_quality,
    ~demand_dm_t,
    ~intake_dm_t,
    ~scaling_factor,
    ~hierarchy_level,
    ~requested_item,
    ~source_compartment,
    ~fixed_demand,
    2000L,
    "1",
    "10.25_50.25",
    "Sheep",
    NA_integer_,
    "grass",
    "grass",
    100,
    30,
    0.3,
    "6_grassland_unlimited",
    NA_integer_,
    "10.25_50.25",
    TRUE
  )
  grass_avail <- tibble::tibble(
    year = 2000L,
    territory = "1",
    sub_territory = "10.25_50.25",
    grass_avail_dm_t = 30
  )
  out <- whep:::.apply_grass_border_grazing(result, grass_avail, 0.1)
  expect_equal(nrow(out), nrow(result))
  expect_false("8_grass_border_grazing" %in% out$hierarchy_level)
})

test_that("grass border grazing only flows within a country", {
  # A surplus cell in country 1 sits between a same-country deficit neighbour and
  # a different-country deficit neighbour (a 0.5-degree cell can be shared by, or
  # border, another country). The allowance must reach only the same-country one.
  cells <- tibble::tribble(
    ~year, ~territory, ~sub_territory, ~lon, ~lat, ~surplus, ~deficit,
    2000L, "1", "0_0", 0, 0, 100, 0,
    2000L, "1", "0.5_0", 0.5, 0, 0, 50,
    2000L, "2", "0.5_0", 0.5, 0, 0, 50
  )
  flows <- whep:::.grass_border_flows(cells, 0.1)
  # Only territory 1 (same country as the surplus) receives grass.
  expect_setequal(flows$territory, "1")
  expect_true(all(flows$received > 0))
})
