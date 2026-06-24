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
