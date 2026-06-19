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
