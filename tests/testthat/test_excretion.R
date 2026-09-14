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
      "method_vs"
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
  # Cattle Excreta C:N (bio_coefs) ~ 19.07.
  expect_equal(cm$c_excretion / cm$n_excretion, 19.065383, tolerance = 1e-4)
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
