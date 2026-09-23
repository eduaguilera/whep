.toy_intake <- function() {
  tibble::tribble(
    ~year,
    ~territory,
    ~sub_territory,
    ~livestock_category,
    ~item_cbs_code,
    ~feed_quality,
    ~intake_dm_t,
    2020L,
    "ES",
    NA,
    "Cattle_milk",
    2513L,
    "high_quality",
    100,
    2020L,
    "ES",
    NA,
    "Cattle_milk",
    NA,
    "grass",
    500,
    2020L,
    "ES",
    NA,
    "Pigs",
    2514L,
    "high_quality",
    50
  )
}

test_that("estimate_n_excretion returns one row per livestock category", {
  res <- whep::estimate_n_excretion(.toy_intake())
  expect_setequal(res$livestock_category, c("Cattle_milk", "Pigs"))
  expect_true(all(
    c(
      "n_intake",
      "n_excretion",
      "c_excretion",
      "vs_excretion",
      "method_n_excretion",
      "method_vs",
      "method_c_excretion"
    ) %in%
      names(res)
  ))
  expect_true(all(res$n_intake > 0))
  expect_true(all(res$n_excretion > 0 & res$n_excretion < res$n_intake))
  expect_true(all(res$vs_excretion > 0))
  expect_true(all(res$c_excretion > 0))
  expect_true(all(res$method_n_excretion == "intake_minus_retention"))
  expect_true(all(res$method_vs == "intake_digestibility"))
})

test_that("scavenging feed quality (poultry) resolves a digestibility", {
  intake <- tibble::tribble(
    ~year, ~territory, ~sub_territory, ~livestock_category,
    ~item_cbs_code, ~feed_quality, ~intake_dm_t,
    2020L, "ES", NA, "Poultry", NA, "scavenging", 80
  )
  res <- whep::estimate_n_excretion(intake)
  expect_equal(nrow(res), 1L)
  expect_true(res$vs_excretion > 0)
  expect_true(res$n_excretion > 0)
})

test_that("n_intake is the canonical sum of feed N (incl. forage for grass)", {
  fn <- whep:::.feed_n_content_lookup()
  barley_n <- fn$feed_n_kgn_kgdm[fn$item_cbs_code == 2513L]
  forage <- whep:::.forage_n_kgn_kgdm()
  exp_ni <- 100 * barley_n + 500 * forage

  res <- whep::estimate_n_excretion(.toy_intake())
  cm <- res[res$livestock_category == "Cattle_milk", ]
  expect_equal(cm$n_intake, exp_ni)
  # Dairy Cattle N retention = 0.20 -> excretion = 0.80 * intake.
  expect_equal(cm$n_excretion, exp_ni * 0.80)
  # Carbon is the carbon of the volatile solids, never a dung C:N applied to
  # whole-excreta N (which gave 19.07 here, 0.73 kg C per kg VS).
  expect_equal(cm$c_excretion, cm$vs_excretion * 0.47)
  expect_lt(cm$c_excretion / cm$n_excretion, 19)
})

test_that("excreted carbon is bounded by the organic matter it sits in", {
  res <- whep::estimate_n_excretion(.toy_intake())
  ratio <- res$c_excretion / res$vs_excretion
  expect_true(all(ratio > 0.35 & ratio < 0.6))
  expect_true(all(res$method_c_excretion == "volatile_solids"))
})

test_that("c_vs_fraction is an option and is validated", {
  res <- whep::estimate_n_excretion(
    .toy_intake(),
    options = list(c_vs_fraction = 0.5)
  )
  expect_equal(res$c_excretion, res$vs_excretion * 0.5)
  expect_error(
    whep::estimate_n_excretion(
      .toy_intake(),
      options = list(c_vs_fraction = 1.5)
    ),
    "c_vs_fraction"
  )
  expect_error(
    whep::estimate_n_excretion(
      .toy_intake(),
      options = list(method_c = "cn_ratio")
    ),
    "method_c"
  )
})

test_that("intake_minus_product_n subtracts product N", {
  prod <- tibble::tribble(
    ~year,
    ~territory,
    ~sub_territory,
    ~livestock_category,
    ~product_n,
    2020L,
    "ES",
    NA,
    "Cattle_milk",
    5
  )
  res <- whep::estimate_n_excretion(
    .toy_intake(),
    options = list(method = "intake_minus_product_n", product_n = prod)
  )
  cm <- res[res$livestock_category == "Cattle_milk", ]
  expect_equal(cm$n_excretion, cm$n_intake - 5)
  expect_true(all(res$method_n_excretion == "intake_minus_product_n"))
})

test_that("estimate_n_excretion guards against bad input", {
  expect_error(
    whep::estimate_n_excretion(.toy_intake(), options = list(method = "bogus")),
    "Unknown"
  )
  bad <- .toy_intake()
  bad$livestock_category[1] <- "Zebra"
  expect_error(whep::estimate_n_excretion(bad), "bridge")
  expect_error(
    whep::estimate_n_excretion(
      .toy_intake(),
      options = list(method = "intake_minus_product_n")
    ),
    "product_n"
  )
})

test_that("forage_n selects the grazed-forage N and records the choice", {
  # Every one of the grazed sink's 500 t of grass carries the selected N, and
  # nothing else in the toy intake does, so n_intake moves exactly with it.
  fn <- whep:::.feed_n_content_lookup()
  barley_n <- fn$feed_n_kgn_kgdm[fn$item_cbs_code == 2513L]
  for (m in whep:::.forage_n_methods()) {
    res <- whep::estimate_n_excretion(
      .toy_intake(),
      options = list(forage_n = m)
    )
    cm <- res[res$livestock_category == "Cattle_milk", ]
    expect_equal(
      cm$n_intake,
      100 * barley_n + 500 * whep:::.forage_n_kgn_kgdm(m)
    )
    expect_true(all(res$method_forage_n == m))
  }
})

test_that("the default forage_n leaves excreted nitrogen unchanged", {
  # Regression lock: the shipped default is still the 0.02 kg N/kg DM the
  # package has always used, so no published nitrogen moves (whep#1050).
  expect_equal(whep:::.forage_n_kgn_kgdm(), 0.02)
  expect_equal(
    whep::estimate_n_excretion(.toy_intake())$n_excretion,
    whep::estimate_n_excretion(
      .toy_intake(),
      options = list(forage_n = "assumed_midrange")
    )$n_excretion
  )
})

test_that("every forage_n option sits in the GLEAM roughage grass band", {
  # An invariant, not a table of expectations: GLEAM 3.0 Supplement S1
  # Tab. S.3.3 brackets grazed forage at 17-31 g N per kg DM, from GRASSH to
  # GRASSLEGF. A coefficient fitted to a target instead of read from a source
  # lands outside that band.
  vals <- purrr::map_dbl(whep:::.forage_n_methods(), whep:::.forage_n_kgn_kgdm)
  expect_true(all(vals >= 0.017 & vals <= 0.031))
  expect_equal(whep:::.forage_n_kgn_kgdm("gleam_grass_fresh"), 0.022)
  expect_equal(whep:::.forage_n_kgn_kgdm("gleam_grass_hay"), 0.017)
  expect_equal(whep:::.forage_n_kgn_kgdm("gleam_grass_mean"), 0.0195)
  expect_equal(whep:::.forage_n_kgn_kgdm("biomass_coefs_grass"), 0.0174)
})

test_that("an unknown forage_n aborts instead of falling back", {
  expect_error(
    whep::estimate_n_excretion(
      .toy_intake(),
      options = list(forage_n = "calibrated_to_faostat")
    ),
    class = "rlang_error"
  )
  expect_error(whep:::.forage_n_kgn_kgdm("bogus"), class = "rlang_error")
})

test_that("the single-method carbon guard names what it accepts", {
  # whep#1100: `method_c` records a choice nobody can make, because the route
  # it replaced applied a fresh-dung C:N to whole-excreta nitrogen and cannot
  # be right (whep#1006). That is a fine reason to have one method; it is not
  # a reason for the guard to be vaguer than every other selector. The error
  # must say what IS accepted, so the day a second defensible method exists
  # this test is what fails.
  expect_error(
    whep::estimate_n_excretion(
      .toy_intake(),
      options = list(method_c = "excreta_cn")
    ),
    "volatile_solids"
  )
  expect_error(
    whep::estimate_n_excretion(
      .toy_intake(),
      options = list(method_vs = "ipcc_default")
    ),
    "intake_digestibility"
  )
  # A non-string option is refused rather than slipping through a zero-length
  # `%in%` test, which aborted on the `if` instead of on the argument.
  expect_error(
    whep::estimate_n_excretion(
      .toy_intake(),
      options = list(method_c = character(0))
    ),
    class = "rlang_error"
  )
})

# whep#1007: digestibility depends on the species as well as the feed. ------

.one_row_intake <- function(category, quality, dm = 100) {
  tibble::tibble(
    year = 2020L,
    territory = "203",
    sub_territory = NA_character_,
    livestock_category = category,
    item_cbs_code = 2514L,
    feed_quality = quality,
    intake_dm_t = dm
  )
}

.vs_of <- function(category, quality, method = NULL) {
  opts <- if (is.null(method)) list() else list(method_digestibility = method)
  whep::estimate_n_excretion(
    .one_row_intake(category, quality),
    options = opts
  ) |>
    dplyr::pull("vs_excretion")
}

test_that("monogastric concentrates use their own digestibility", {
  # VS = DM * (1 - DE) * (1 - ash). IPCC 2019 Table 10.2 (Updated): growing
  # swine in confinement 80-90 (0.85), broilers in confinement 85-93 (0.89).
  # Ash from ipcc_tier2_manure_ash: swine 4%, poultry 25%.
  expect_equal(.vs_of("Pigs", "high_quality"), 100 * 0.15 * 0.96)
  expect_equal(.vs_of("Poultry", "high_quality"), 100 * 0.11 * 0.75)
  # Free-range swine 50-70 (0.60) for the scavenged feed.
  expect_equal(.vs_of("Pigs", "scavenging"), 100 * 0.40 * 0.96)
})

test_that("the species-blind method stays selectable and reproduces 0.72", {
  expect_equal(
    .vs_of("Pigs", "high_quality", "feed_quality"),
    100 * 0.28 * 0.96
  )
  expect_equal(
    .vs_of("Poultry", "high_quality", "feed_quality"),
    100 * 0.28 * 0.75
  )
})

test_that("ruminant and residue rows do not move between the methods", {
  cases <- tidyr::expand_grid(
    category = c("Cattle_milk", "Cattle_meat", "Sheep", "Goats", "Horses"),
    quality = c("high_quality", "grass", "residues", "scavenging")
  ) |>
    dplyr::bind_rows(
      tibble::tibble(
        category = c("Pigs", "Poultry", "Rabbits"),
        quality = c("residues", "residues", "high_quality")
      )
    )
  new <- purrr::map2_dbl(cases$category, cases$quality, .vs_of)
  old <- purrr::map2_dbl(
    cases$category,
    cases$quality,
    \(c, q) .vs_of(c, q, "feed_quality")
  )
  expect_equal(new, old)
})

test_that("the chosen digestibility method is recorded on every row", {
  res <- whep::estimate_n_excretion(.toy_intake())
  expect_true(rlang::has_name(res, "method_digestibility"))
  expect_true(all(res$method_digestibility == "species_feed_quality"))
  old <- whep::estimate_n_excretion(
    .toy_intake(),
    options = list(method_digestibility = "feed_quality")
  )
  expect_true(all(old$method_digestibility == "feed_quality"))
})

test_that("an unknown digestibility method aborts instead of falling back", {
  expect_error(
    whep::estimate_n_excretion(
      .toy_intake(),
      options = list(method_digestibility = "calibrated")
    ),
    "species_feed_quality"
  )
})

test_that("the digestibility table covers every species x feed quality once", {
  # An override that silently missed its key would leave the species-blind
  # value in place and every total would still reconcile, so assert the table
  # is complete AND that each override actually landed.
  categories <- whep:::.species_taxonomy_bridge()$livestock_category
  qualities <- whep:::.feed_quality_digestibility()$feed_quality
  purrr::walk(c("species_feed_quality", "feed_quality"), \(m) {
    tbl <- whep:::.digestibility_table(m)
    expect_equal(nrow(tbl), length(categories) * length(qualities))
    expect_equal(
      nrow(dplyr::distinct(tbl, livestock_category, feed_quality)),
      nrow(tbl)
    )
    expect_false(anyNA(tbl$digestibility))
    expect_true(all(tbl$digestibility > 0 & tbl$digestibility < 1))
  })
  over <- whep:::.species_digestibility()
  landed <- whep:::.digestibility_table("species_feed_quality") |>
    dplyr::inner_join(over, by = c("livestock_category", "feed_quality"))
  expect_equal(nrow(landed), nrow(over))
  expect_equal(landed$digestibility.x, landed$digestibility.y)
})
