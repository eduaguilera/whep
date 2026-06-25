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
