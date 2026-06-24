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
