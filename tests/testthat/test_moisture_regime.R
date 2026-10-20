# test_moisture_regime.R -------------------------------------------------------

# IPCC 2019 Refinement Vol. 4: Table 11.1 footnote 4 (p. 11.13) for the wet/dry
# rule, Figure 3A.5.2 (p. 3.48) for the tropical test. Every threshold below is
# written as a literal on purpose, so a changed constant in the classifier
# fails here instead of moving both sides at once.

# One cell, one climate per year. `prec` may be a scalar or one value per year.
.mr_panel <- function(
  years = 1901:1935,
  prec = 600,
  pet = 1000,
  mat = 12,
  frost = 40,
  lon = 0.25,
  lat = 45.25
) {
  tibble::tibble(
    lon = lon,
    lat = lat,
    year = as.integer(years),
    mat_c = mat,
    frost_days = frost,
    prec_mm = prec,
    pet_mm = pet
  )
}

# The classifier, quietly: tests that expect unclassified rows say so with
# `expect_message()` instead.
.mr_classify <- function(data, ...) {
  whep:::.classify_moisture_regime(data, ...)
}

# The precipitation-plus-irrigation water term, no longer the default (#1531).
.mr_classify_irrig <- function(data, ...) {
  .mr_classify(data, ..., water = "precip_irrig")
}

.mr_value <- function(result, column, at_year) {
  result |>
    dplyr::filter(year == at_year) |>
    dplyr::pull({{ column }})
}

# Output contract -------------------------------------------------------------

testthat::test_that("the output carries the documented columns", {
  testthat::skip_if_not_installed("pointblank")
  result <- .mr_classify(.mr_panel(), years = 1930:1935, water = "precip")

  result |>
    pointblank::expect_col_exists(
      c(
        "lon",
        "lat",
        "year",
        "mat_c",
        "frost_days",
        "prec_mm",
        "pet_mm",
        "water_mm",
        "moisture_ratio",
        "tropical",
        "moisture_regime",
        "method_moisture_regime"
      )
    )
  result |>
    pointblank::expect_col_vals_in_set("moisture_regime", c("wet", "dry"))
  result |>
    pointblank::expect_col_vals_not_null("method_moisture_regime")
  testthat::expect_equal(nrow(result), 6L)
})

testthat::test_that("the defaults are trailing_30 and precip", {
  # Issue #1531: precipitation alone is IPCC's definition, so the default
  # needs no irrigation column, and a row holding one does not use it.
  result <- .mr_classify(.mr_panel(), years = 1935L)
  testthat::expect_equal(
    result$method_moisture_regime,
    "ipcc2019_wet_dry;trailing_30;precip"
  )
  with_irrig <- .mr_classify(
    dplyr::mutate(.mr_panel(), irrig_mm = 5000),
    years = 1935L
  )
  testthat::expect_equal(with_irrig$water_mm, result$water_mm)
  testthat::expect_equal(with_irrig$moisture_regime, result$moisture_regime)
})

testthat::test_that("each row's ratio is its own water over its own PET", {
  fixture <- .mr_panel(prec = 400 + (1901:1935 - 1901) * 20)

  result <- .mr_classify(fixture, window = "trailing_30", water = "precip")

  testthat::expect_equal(result$moisture_ratio, result$water_mm / result$pet_mm)
  testthat::expect_equal(result$water_mm, result$prec_mm)
})

# Tropical test (Figure 3A.5.2) -----------------------------------------------

testthat::test_that("every branch of the tropical test is taken", {
  # Same water and PET on every row (1200 mm over 1500 mm, ratio 0.8): wet
  # under the tropical MAP rule, dry under the ratio rule. So the regime flips
  # exactly where the tropical test does.
  edges <- tibble::tribble(
    ~lon,  ~lat, ~year, ~mat_c, ~frost_days, ~prec_mm, ~pet_mm,
    0.25,  0.25, 2000L,   18.1,           0,     1200,    1500,
    0.75,  0.25, 2000L,   18.0,           0,     1200,    1500,
    1.25,  0.25, 2000L,   17.9,           0,     1200,    1500,
    1.75,  0.25, 2000L,   25.0,           7,     1200,    1500,
    2.25,  0.25, 2000L,   25.0,           8,     1200,    1500
  )

  result <- .mr_classify(edges, window = "annual", water = "precip") |>
    dplyr::arrange(lon)

  testthat::expect_equal(result$tropical, c(TRUE, FALSE, FALSE, TRUE, FALSE))
  testthat::expect_equal(
    result$moisture_regime,
    c("wet", "dry", "dry", "wet", "dry")
  )
})

testthat::test_that("a warm cell with frost is not tropical (no Warm reuse)", {
  frosty <- tibble::tribble(
    ~lon,  ~lat, ~year, ~mat_c, ~frost_days, ~prec_mm, ~pet_mm,
    0.25,  0.25, 2000L,   22.0,          30,     1200,    1500
  )

  result <- .mr_classify(frosty, window = "annual", water = "precip")

  # The manure zone helper calls this cell Warm; Figure 3A.5.2 does not call
  # it tropical.
  testthat::expect_equal(whep:::.climate_zone_from_mat(22), "Warm")
  testthat::expect_false(result$tropical)
  testthat::expect_equal(result$moisture_regime, "dry")
})

# Wet/dry rules (Table 11.1 footnote 4) ---------------------------------------

testthat::test_that("tropical rows are wet only above 1000 mm, 1000 is dry", {
  tropical <- tibble::tribble(
    ~lon,  ~lat, ~year, ~mat_c, ~frost_days, ~prec_mm, ~pet_mm,
    0.25,  0.25, 2000L,     25,           0,    999.9,    2000,
    0.75,  0.25, 2000L,     25,           0,   1000.0,    2000,
    1.25,  0.25, 2000L,     25,           0,   1000.1,    2000,
    1.75,  0.25, 2000L,     25,           0,   1200.0,    2000
  )

  result <- .mr_classify(tropical, window = "annual", water = "precip") |>
    dplyr::arrange(lon)

  testthat::expect_true(all(result$tropical))
  # The last row's ratio is 0.6: the tropical rule is MAP, not MAP:PET.
  testthat::expect_equal(result$moisture_regime, c("dry", "dry", "wet", "wet"))
})

testthat::test_that("other rows are wet only above a ratio of 1, 1 is dry", {
  temperate <- tibble::tribble(
    ~lon,  ~lat, ~year, ~mat_c, ~frost_days, ~prec_mm, ~pet_mm,
    0.25, 45.25, 2000L,     12,          60,      499,     500,
    0.75, 45.25, 2000L,     12,          60,      500,     500,
    1.25, 45.25, 2000L,     12,          60,      501,     500,
    1.75, 45.25, 2000L,     12,          60,     1500,    2000
  )

  result <- .mr_classify(temperate, window = "annual", water = "precip") |>
    dplyr::arrange(lon)

  testthat::expect_false(any(result$tropical))
  testthat::expect_equal(result$moisture_ratio, c(0.998, 1, 1.002, 0.75))
  # The last row has 1500 mm but a ratio of 0.75: dry outside the tropics.
  testthat::expect_equal(result$moisture_regime, c("dry", "dry", "wet", "dry"))
})

testthat::test_that("the edges hold on precipitation plus irrigation too", {
  edges <- tibble::tribble(
    ~lon,  ~lat, ~year, ~mat_c, ~frost_days, ~prec_mm, ~pet_mm, ~irrig_mm,
    0.25, 45.25, 2000L,     12,          60,      300,     500,       200,
    0.75, 45.25, 2000L,     12,          60,      300,     500,       201,
    1.25,  0.25, 2000L,     25,           0,      600,    2000,       400,
    1.75,  0.25, 2000L,     25,           0,      600,    2000,       401
  )

  result <- .mr_classify_irrig(edges, window = "annual") |>
    dplyr::arrange(lon)

  testthat::expect_equal(result$water_mm, c(500, 501, 1000, 1001))
  testthat::expect_equal(result$moisture_regime, c("dry", "wet", "dry", "wet"))
})

# Water basis -----------------------------------------------------------------

.mr_regimes <- function() {
  # One cell-year (MAT 15 degC, 30 frost days, 400 mm over 1000 mm PET) under
  # both regimes of one crop.
  tibble::tribble(
    ~water_regime, ~irrig_mm,
    "rainfed",             0,
    "irrigated",         700
  ) |>
    dplyr::mutate(
      lon = 0.25,
      lat = 40.25,
      year = 2000L,
      item_cbs_code = 2511L,
      mat_c = 15,
      frost_days = 30,
      prec_mm = 400,
      pet_mm = 1000
    )
}

testthat::test_that("irrigation turns a dry irrigated row wet", {
  with_irrig <- .mr_classify_irrig(.mr_regimes(), window = "annual") |>
    dplyr::arrange(water_regime)
  precip_only <- .mr_classify(
    .mr_regimes(),
    window = "annual",
    water = "precip"
  ) |>
    dplyr::arrange(water_regime)

  testthat::expect_equal(with_irrig$water_regime, c("irrigated", "rainfed"))
  testthat::expect_equal(with_irrig$water_mm, c(1100, 400))
  testthat::expect_equal(with_irrig$moisture_regime, c("wet", "dry"))
  testthat::expect_equal(precip_only$water_mm, c(400, 400))
  testthat::expect_equal(precip_only$moisture_regime, c("dry", "dry"))
  testthat::expect_true(
    all(stringr::str_detect(with_irrig$method_moisture_regime, ";precip_irrig"))
  )
  testthat::expect_true(
    all(stringr::str_detect(precip_only$method_moisture_regime, ";precip$"))
  )
  testthat::expect_false(rlang::has_name(precip_only, "irrig_mm"))
})

testthat::test_that("rainfed irrigation is a structural zero, never a value", {
  na_rainfed <- .mr_regimes() |>
    dplyr::mutate(
      irrig_mm = dplyr::if_else(water_regime == "rainfed", NA_real_, irrig_mm)
    )
  wet_rainfed <- .mr_regimes() |>
    dplyr::mutate(
      irrig_mm = dplyr::if_else(water_regime == "rainfed", 50, irrig_mm)
    )

  result <- .mr_classify_irrig(na_rainfed, window = "annual")

  testthat::expect_equal(
    .mr_value(dplyr::filter(result, water_regime == "rainfed"), irrig_mm, 2000),
    0
  )
  testthat::expect_false(anyNA(result$moisture_regime))
  testthat::expect_error(
    .mr_classify_irrig(wet_rainfed, window = "annual"),
    class = "whep_moisture_regime_domain"
  )
})

# Windows ---------------------------------------------------------------------

testthat::test_that("trailing_30 averages first, so one wet year stays dry", {
  # 29 years at 400 mm then one at 2000 mm, PET 1000 mm. The year alone is wet
  # (ratio 2); its 30-year mean is 453.3 mm (ratio 0.45), dry.
  fixture <- .mr_panel(years = 1971:2000, prec = c(rep(400, 29), 2000))

  annual <- .mr_classify(
    fixture,
    years = 2000L,
    window = "annual",
    water = "precip"
  )
  trailing <- .mr_classify(
    fixture,
    years = 2000L,
    window = "trailing_30",
    water = "precip"
  )

  testthat::expect_equal(annual$moisture_regime, "wet")
  testthat::expect_equal(trailing$moisture_regime, "dry")
  testthat::expect_equal(trailing$prec_mm, (29 * 400 + 2000) / 30)
})

testthat::test_that("trailing_30 classifies the mean, not the majority year", {
  # 16 dry years (ratio 0.9) and 14 wet ones (ratio 1.5). A majority vote over
  # annual classes says dry; the 30-year mean (1180 mm, ratio 1.18) is wet.
  fixture <- .mr_panel(
    years = 1971:2000,
    prec = c(rep(900, 16), rep(1500, 14))
  )

  annual <- .mr_classify(fixture, window = "annual", water = "precip")
  trailing <- .mr_classify(
    fixture,
    years = 2000L,
    window = "trailing_30",
    water = "precip"
  )

  testthat::expect_equal(sum(annual$moisture_regime == "dry"), 16L)
  testthat::expect_equal(trailing$prec_mm, 1180)
  testthat::expect_equal(trailing$moisture_regime, "wet")
})

testthat::test_that("the trailing window is exactly the 30 years y-29..y", {
  # A huge value in 1970 sits one year outside 2000's window and inside
  # 1999's. PET 1500 mm: 1000 mm alone is dry.
  prec <- dplyr::if_else(1960:2000 == 1970L, 1e5, 1000)
  fixture <- .mr_panel(years = 1960:2000, prec = prec, pet = 1500)

  result <- .mr_classify(
    fixture,
    years = c(1999L, 2000L),
    window = "trailing_30",
    water = "precip"
  )

  testthat::expect_equal(.mr_value(result, prec_mm, 2000L), 1000)
  testthat::expect_equal(.mr_value(result, moisture_regime, 2000L), "dry")
  testthat::expect_equal(
    .mr_value(result, prec_mm, 1999L),
    (29 * 1000 + 1e5) / 30
  )
  testthat::expect_equal(.mr_value(result, moisture_regime, 1999L), "wet")
})

testthat::test_that("every window input is averaged, temperature included", {
  # MAT crosses 18 degC only in the final year: the annual window calls 2000
  # tropical, the trailing mean (about 12.2 degC) does not.
  fixture <- .mr_panel(
    years = 1971:2000,
    mat = c(rep(12, 29), 19),
    frost = 0,
    prec = 1200,
    pet = 1500
  )

  annual <- .mr_classify(
    fixture,
    years = 2000L,
    window = "annual",
    water = "precip"
  )
  trailing <- .mr_classify(fixture, years = 2000L, water = "precip")

  testthat::expect_true(annual$tropical)
  testthat::expect_equal(annual$moisture_regime, "wet")
  testthat::expect_equal(trailing$mat_c, (29 * 12 + 19) / 30)
  testthat::expect_false(trailing$tropical)
  testthat::expect_equal(trailing$moisture_regime, "dry")
})

testthat::test_that("the normal window holds one fixed period for every year", {
  fixture <- .mr_panel(prec = 500 + (1901:1935 - 1901) * 10)

  result <- .mr_classify(
    fixture,
    years = c(1880L, 1920L, 1935L),
    window = "normal",
    water = "precip",
    normal_period = 1906:1915
  )

  testthat::expect_equal(result$prec_mm, rep(500 + 10 * 9.5, 3))
  testthat::expect_true(
    all(
      result$method_moisture_regime ==
        "ipcc2019_wet_dry;normal_1906_1915;precip"
    )
  )
})

testthat::test_that("the default normal is 1991-2020", {
  fixture <- .mr_panel(years = 1901:2020, prec = 300 + (1901:2020 - 1901))

  result <- .mr_classify(
    fixture,
    years = 2020L,
    window = "normal",
    water = "precip"
  )

  testthat::expect_equal(result$prec_mm, mean(300 + (1991:2020 - 1901)))
  testthat::expect_equal(
    result$method_moisture_regime,
    "ipcc2019_wet_dry;normal_1991_2020;precip"
  )
})

testthat::test_that("an invalid normal period aborts", {
  testthat::expect_error(
    .mr_classify(
      .mr_panel(),
      window = "normal",
      water = "precip",
      normal_period = c(1906L, 1910L)
    ),
    class = "whep_moisture_regime_period"
  )
  testthat::expect_error(
    .mr_classify(
      .mr_panel(),
      window = "normal",
      water = "precip",
      normal_period = integer()
    ),
    class = "whep_moisture_regime_period"
  )
})

testthat::test_that("an unknown window or water basis aborts", {
  testthat::expect_error(
    .mr_classify(.mr_panel(), window = "decadal", water = "precip"),
    class = "rlang_error"
  )
  testthat::expect_error(
    .mr_classify(.mr_panel(), water = "irrigation"),
    class = "rlang_error"
  )
})

# Years outside the record ----------------------------------------------------

.mr_ramp <- function() {
  # 500 mm in 1901 rising 10 mm a year, so every held climatology is a
  # distinct, checkable number: 1901-1930 averages 645 mm, 1906-1935 695 mm.
  .mr_panel(prec = 500 + (1901:1935 - 1901) * 10)
}

testthat::test_that("a year before the record takes the 1901-30 climatology", {
  result <- .mr_classify(
    .mr_ramp(),
    years = 1880L,
    window = "annual",
    water = "precip"
  )

  testthat::expect_equal(result$prec_mm, 645)
  testthat::expect_equal(
    result$method_moisture_regime,
    "ipcc2019_wet_dry;annual;precip;climatology_1901_1930_1of1"
  )
  testthat::expect_false(is.na(result$moisture_regime))
})

testthat::test_that("a trailing window reaching before 1901 is filled", {
  result <- .mr_classify(
    .mr_ramp(),
    years = c(1880L, 1910L, 1930L),
    water = "precip"
  )

  # 1910: 1881-1900 take the climatology (20 years at 645 mm); 1901-1910 are
  # measured (500..590 mm, mean 545 mm).
  testthat::expect_equal(
    .mr_value(result, prec_mm, 1910L),
    (20 * 645 + 10 * 545) / 30
  )
  testthat::expect_equal(
    .mr_value(result, method_moisture_regime, 1910L),
    "ipcc2019_wet_dry;trailing_30;precip;climatology_1901_1930_20of30"
  )
  testthat::expect_equal(.mr_value(result, prec_mm, 1880L), 645)
  testthat::expect_equal(
    .mr_value(result, method_moisture_regime, 1880L),
    "ipcc2019_wet_dry;trailing_30;precip;climatology_1901_1930_30of30"
  )
  # 1930's window is 1901-1930: fully measured, so no climatology label.
  testthat::expect_equal(.mr_value(result, prec_mm, 1930L), 645)
  testthat::expect_equal(
    .mr_value(result, method_moisture_regime, 1930L),
    "ipcc2019_wet_dry;trailing_30;precip"
  )
})

testthat::test_that("a year after the record holds the closing climatology", {
  result <- .mr_classify(
    .mr_ramp(),
    years = 1940L,
    window = "annual",
    water = "precip"
  )

  testthat::expect_equal(result$prec_mm, 695)
  testthat::expect_equal(
    result$method_moisture_regime,
    "ipcc2019_wet_dry;annual;precip;climatology_1906_1935_1of1"
  )
})

testthat::test_that("a hole in the climatology window is not averaged over", {
  # 1905 sits inside the 1901-1930 climatology. Averaging the other 29 years
  # would quietly hand 1880 a climatology one year short.
  holed <- .mr_ramp() |>
    dplyr::mutate(prec_mm = dplyr::if_else(year == 1905L, NA_real_, prec_mm))

  testthat::expect_message(
    result <- .mr_classify(
      holed,
      years = 1880L,
      window = "annual",
      water = "precip"
    ),
    class = "whep_moisture_regime_unclassified"
  )

  testthat::expect_true(is.na(result$prec_mm))
  testthat::expect_true(is.na(result$moisture_regime))
  testthat::expect_equal(
    result$method_moisture_regime,
    paste0(
      "ipcc2019_wet_dry;annual;precip;climatology_1901_1930_1of1;",
      "missing=prec_mm"
    )
  )
})

testthat::test_that("an interior hole in the record aborts", {
  gapped <- .mr_panel(years = c(1901:1910, 1912:1935))

  testthat::expect_error(
    .mr_classify(gapped, years = 1935L, water = "precip"),
    "no data for"
  )
})

testthat::test_that("years = NULL classifies every year of the record", {
  result <- .mr_classify(.mr_panel(), window = "annual", water = "precip")

  testthat::expect_equal(result$year, 1901:1935)
})

# Missing values: stamped, never zero-filled ----------------------------------

testthat::test_that("a missing climate value leaves the row unclassified", {
  fixture <- dplyr::bind_rows(
    .mr_panel(years = 1971:2000, lon = 0.25),
    .mr_panel(years = 1971:2000, lon = 0.75)
  ) |>
    dplyr::mutate(
      prec_mm = dplyr::if_else(lon == 0.25 & year == 1990L, NA_real_, prec_mm)
    )

  testthat::expect_message(
    annual <- .mr_classify(
      fixture,
      years = 1989:1991,
      window = "annual",
      water = "precip"
    ),
    class = "whep_moisture_regime_unclassified"
  )
  hole <- dplyr::filter(annual, lon == 0.25, year == 1990L)

  testthat::expect_true(is.na(hole$moisture_regime))
  testthat::expect_true(is.na(hole$water_mm))
  testthat::expect_equal(
    hole$method_moisture_regime,
    "ipcc2019_wet_dry;annual;precip;missing=prec_mm"
  )
  testthat::expect_equal(sum(is.na(annual$moisture_regime)), 1L)

  # The hole poisons every trailing window that contains 1990, and no other.
  testthat::expect_message(
    trailing <- .mr_classify(fixture, years = 2000L, water = "precip"),
    class = "whep_moisture_regime_unclassified"
  )
  testthat::expect_true(
    is.na(.mr_value(
      dplyr::filter(trailing, lon == 0.25),
      moisture_regime,
      2000
    ))
  )
  testthat::expect_false(
    is.na(.mr_value(
      dplyr::filter(trailing, lon == 0.75),
      moisture_regime,
      2000
    ))
  )
})

testthat::test_that("a cell-year with no row is missing, not zero", {
  fixture <- dplyr::bind_rows(
    .mr_panel(years = 1971:2000, lon = 0.25),
    .mr_panel(years = c(1971:1984, 1986:2000), lon = 0.75)
  )

  testthat::expect_message(
    result <- .mr_classify(fixture, years = 2000L, water = "precip"),
    class = "whep_moisture_regime_unclassified"
  )
  gap <- dplyr::filter(result, lon == 0.75)

  testthat::expect_true(is.na(gap$moisture_regime))
  testthat::expect_equal(
    gap$method_moisture_regime,
    paste0(
      "ipcc2019_wet_dry;trailing_30;precip;",
      "missing=mat_c+frost_days+prec_mm+pet_mm"
    )
  )
})

testthat::test_that("an irrigated series needs irrigation every window year", {
  # Rainfed wheat every year; irrigated wheat only in 2000. The cell climate is
  # complete, so the irrigated row classifies on precipitation alone but not on
  # precipitation plus a 30-year irrigation mean it does not have.
  rainfed <- .mr_panel(years = 1971:2000, prec = 400) |>
    dplyr::mutate(item_cbs_code = 2511L, water_regime = "rainfed", irrig_mm = 0)
  irrigated <- .mr_panel(years = 2000L, prec = 400) |>
    dplyr::mutate(
      item_cbs_code = 2511L,
      water_regime = "irrigated",
      irrig_mm = 700
    )
  fixture <- dplyr::bind_rows(rainfed, irrigated)

  testthat::expect_message(
    with_irrig <- .mr_classify_irrig(fixture, years = 2000L),
    class = "whep_moisture_regime_unclassified"
  )
  precip_only <- .mr_classify(fixture, years = 2000L, water = "precip")

  testthat::expect_equal(
    with_irrig |>
      dplyr::filter(water_regime == "irrigated") |>
      dplyr::pull(method_moisture_regime),
    "ipcc2019_wet_dry;trailing_30;precip_irrig;missing=irrig_mm"
  )
  testthat::expect_equal(
    with_irrig |>
      dplyr::filter(water_regime == "rainfed") |>
      dplyr::pull(moisture_regime),
    "dry"
  )
  testthat::expect_false(anyNA(precip_only$moisture_regime))
})

testthat::test_that("irrigation is averaged per crop and regime", {
  years <- 1971:2000
  rice <- .mr_panel(years = years, prec = 400) |>
    dplyr::mutate(item_cbs_code = 2807L, irrig_mm = 900)
  wheat <- .mr_panel(years = years, prec = 400) |>
    dplyr::mutate(item_cbs_code = 2511L, irrig_mm = c(rep(0, 29), 3000))
  fixture <- dplyr::bind_rows(rice, wheat) |>
    dplyr::mutate(water_regime = "irrigated")

  result <- .mr_classify_irrig(fixture, years = 2000L) |>
    dplyr::arrange(item_cbs_code)

  testthat::expect_equal(result$irrig_mm, c(100, 900))
  testthat::expect_equal(result$moisture_regime, c("dry", "wet"))
})

testthat::test_that("a separate record averages only the irrigated years", {
  # The record holds irrigated wheat in 12 of the 30 window years, 1989-2000,
  # at 10 mm a year more each year; precipitation rises by 20 mm a year.
  climate <- .mr_panel(years = 1971:2000, prec = 300 + 20 * (0:29)) |>
    dplyr::mutate(item_cbs_code = 2511L, water_regime = "irrigated")
  record <- tibble::tibble(
    lon = 0.25,
    lat = 45.25,
    item_cbs_code = 2511L,
    water_regime = "irrigated",
    year = 1989:2000,
    irrig_mm = 100 + 10 * (0:11)
  )

  result <- .mr_classify(
    climate,
    years = 2000L,
    water = "precip_irrig",
    irrigation = record
  )

  testthat::expect_equal(result$irrig_mm, mean(100 + 10 * (0:11)))
  # Climate stays the 30-year mean, not the mean of the irrigated years.
  testthat::expect_equal(result$prec_mm, mean(300 + 20 * (0:29)))
  testthat::expect_equal(result$moisture_ratio, (590 + 155) / 1000)
  testthat::expect_identical(result$moisture_regime, "dry")
  testthat::expect_identical(
    result$method_moisture_regime,
    "ipcc2019_wet_dry;trailing_30;precip_irrig;irrig_years_12of30"
  )

  # A full record carries no count, and an annual window counts 1 of 1.
  full <- dplyr::mutate(
    tidyr::expand_grid(record[1, -5], year = 1971:2000),
    irrig_mm = 100
  )
  testthat::expect_identical(
    .mr_classify_irrig(
      climate,
      years = 2000L,
      irrigation = full
    )$method_moisture_regime,
    "ipcc2019_wet_dry;trailing_30;precip_irrig"
  )
})

testthat::test_that("a held record year with no depth still fails closed", {
  climate <- .mr_panel(years = 1971:2000) |>
    dplyr::mutate(item_cbs_code = 2511L, water_regime = "irrigated")
  record <- tibble::tibble(
    lon = 0.25,
    lat = 45.25,
    item_cbs_code = 2511L,
    water_regime = "irrigated",
    year = 1989:2000,
    irrig_mm = c(NA, rep(700, 11))
  )

  testthat::expect_message(
    result <- .mr_classify_irrig(climate, years = 2000L, irrigation = record),
    class = "whep_moisture_regime_unclassified"
  )
  testthat::expect_true(is.na(result$moisture_regime))
  testthat::expect_true(is.na(result$irrig_mm))
  testthat::expect_match(result$method_moisture_regime, "missing=irrig_mm$")

  # A missing climate year is not excused by the irrigation rule.
  holed <- dplyr::mutate(
    climate,
    prec_mm = dplyr::if_else(year == 1975L, NA_real_, prec_mm)
  )
  testthat::expect_message(
    holed_result <- .mr_classify_irrig(
      holed,
      years = 2000L,
      irrigation = dplyr::mutate(record, irrig_mm = 700)
    ),
    class = "whep_moisture_regime_unclassified"
  )
  testthat::expect_true(is.na(holed_result$moisture_regime))
  testthat::expect_match(
    holed_result$method_moisture_regime,
    "irrig_years_12of30;missing=prec_mm$"
  )
})

testthat::test_that("a zero PET leaves the row unclassified, never Inf", {
  fixture <- tibble::tribble(
    ~lon,  ~lat, ~year, ~mat_c, ~frost_days, ~prec_mm, ~pet_mm,
    0.25, 45.25, 2000L,     12,          60,      500,       0,
    0.75, 45.25, 2000L,     12,          60,      500,     400
  )

  testthat::expect_message(
    result <- .mr_classify(fixture, window = "annual", water = "precip"),
    class = "whep_moisture_regime_unclassified"
  )
  result <- dplyr::arrange(result, lon)

  testthat::expect_true(is.na(result$moisture_ratio[1]))
  testthat::expect_true(is.na(result$moisture_regime[1]))
  testthat::expect_equal(
    result$method_moisture_regime[1],
    "ipcc2019_wet_dry;annual;precip;pet_not_positive"
  )
  testthat::expect_equal(result$moisture_regime[2], "wet")
})

# Contract aborts -------------------------------------------------------------

testthat::test_that("a missing required column aborts by class", {
  testthat::expect_error(
    .mr_classify(dplyr::select(.mr_panel(), -pet_mm), water = "precip"),
    class = "whep_moisture_regime_columns"
  )
  testthat::expect_error(
    .mr_classify(dplyr::select(.mr_panel(), -frost_days), water = "precip"),
    class = "whep_moisture_regime_columns"
  )
  # precip_irrig needs the irrigation column; precip does not.
  testthat::expect_error(
    .mr_classify(.mr_panel(), water = "precip_irrig"),
    class = "whep_moisture_regime_columns"
  )
  testthat::expect_no_error(.mr_classify(.mr_panel(), water = "precip"))
})

testthat::test_that("an empty table aborts", {
  testthat::expect_error(
    .mr_classify(.mr_panel()[0, ], water = "precip"),
    class = "whep_moisture_regime_empty"
  )
})

testthat::test_that("a duplicated key aborts", {
  doubled <- dplyr::bind_rows(.mr_panel(), .mr_panel())

  testthat::expect_error(
    .mr_classify(doubled, water = "precip"),
    class = "whep_moisture_regime_key"
  )
})

testthat::test_that("two climates for one cell-year abort", {
  clash <- .mr_regimes() |>
    dplyr::mutate(prec_mm = c(400, 410))

  testthat::expect_error(
    .mr_classify(clash, window = "annual"),
    class = "whep_moisture_regime_climate"
  )
})

testthat::test_that("out-of-domain inputs abort", {
  negative <- dplyr::mutate(.mr_panel(), prec_mm = -1)
  frost <- dplyr::mutate(.mr_panel(), frost_days = 400)
  label <- dplyr::mutate(.mr_regimes(), water_regime = c("rainfed", "flooded"))

  testthat::expect_error(
    .mr_classify(negative, water = "precip"),
    class = "whep_moisture_regime_domain"
  )
  testthat::expect_error(
    .mr_classify(frost, water = "precip"),
    class = "whep_moisture_regime_domain"
  )
  testthat::expect_error(
    .mr_classify(label, window = "annual"),
    class = "whep_moisture_regime_domain"
  )
})

testthat::test_that("a zero-filled PET column is refused, not classified wet", {
  vacuous <- dplyr::mutate(.mr_panel(), pet_mm = 0)

  expect_supplied_guard(
    identity = all(!is.na(vacuous$pet_mm)),
    guard = .mr_classify(vacuous, water = "precip")
  )
})

testthat::test_that("an all-zero irrigation layer is refused", {
  vacuous <- .mr_regimes() |>
    dplyr::mutate(irrig_mm = 0)

  expect_supplied_guard(
    identity = all(!is.na(vacuous$irrig_mm)),
    guard = .mr_classify_irrig(vacuous, window = "annual")
  )
})
