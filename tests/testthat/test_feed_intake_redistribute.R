test_that("get_feed_intake routes the unimplemented redistribute path to an error", {
  expect_error(
    whep::get_feed_intake(grain = "provincial"),
    "not yet implemented"
  )
  expect_error(
    whep::get_feed_intake(demand_tier = "ipcc"),
    "not yet implemented"
  )
  expect_error(
    whep::get_feed_intake(grain = "provincial", demand_tier = "ipcc"),
    "not yet implemented"
  )
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
    50, # Hogs -> Pigs
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
    500 # straw -> residues
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
