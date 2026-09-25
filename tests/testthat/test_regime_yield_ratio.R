# build_regime_yield_ratio() and split_regime_yield(): the irrigated:rainfed
# yield ratio of plan decisions D15-D21 and the conservation split. Fully
# offline: every input is a hand-built fixture passed through `data` or
# `production`. Nothing here reaches a pin, a WHEP_* path or the network.
#
# Area codes are WHEP polity buckets: Spain 203, France 68, Italy 106, USA 231.

# SPAM2010 country totals, as read_spam_yields() rows (one row per country,
# crop and technology is enough: the engine sums over pixels anyway).
.ryr_spam_fixture <- function() {
  tibble::tribble(
    ~iso3, ~spam_crop, ~technology, ~harvested_area_ha, ~production_t,
    # Wheat: Spain 5 vs 3 t/ha; France rainfed only.
    "ESP", "whea", "I", 100, 500,
    "ESP", "whea", "R", 400, 1200,
    "FRA", "whea", "I", 0, 0,
    "FRA", "whea", "R", 500, 3500,
    # Barley: Spain 2 vs 1 t/ha.
    "ESP", "barl", "I", 50, 100,
    "ESP", "barl", "R", 150, 150,
    # Other cereals: Spain rainfed only, so undefined there (D21).
    "ESP", "ocer", "R", 20, 20,
    # A pixel whose ISO3 maps to no WHEP polity still counts globally.
    "ZZZ", "ocer", "I", 10, 40,
    "ZZZ", "ocer", "R", 10, 20,
    # Millet, pooled over pearl and small millet.
    "ESP", "pmil", "I", 10, 20,
    "ESP", "pmil", "R", 10, 10,
    "ESP", "smil", "I", 30, 120,
    "ESP", "smil", "R", 90, 90,
    # Rice below 1 in Spain: the anchor floor.
    "ESP", "rice", "I", 10, 10,
    "ESP", "rice", "R", 10, 20,
    # Maize 6 vs 1: large enough for the level cap.
    "ESP", "maiz", "I", 10, 60,
    "ESP", "maiz", "R", 10, 10,
    # Oil crops and fibres for Linum and Hemp (D17).
    "ESP", "ooil", "I", 10, 30,
    "ESP", "ooil", "R", 10, 10,
    "FRA", "ofib", "I", 20, 40,
    "FRA", "ofib", "R", 20, 20
  )
}

# The faostat-fertilizer-nutrients pin shape (agricultural use of N).
.ryr_fert_fixture <- function() {
  tibble::tribble(
    ~`Area Code`, ~Year, ~Value,
    203L, 1961L, 156,
    203L, 1962L, 156,
    203L, 1963L, 156,
    203L, 1964L, 156,
    203L, 1965L, 156,
    203L, 2000L, 2000,
    203L, 2010L, 1000,
    68L, 2000L, 500,
    68L, 2010L, 0,
    231L, 2010L, 5000,
    # The USSR (228) reports 1961-1965; its successors Russia (185) and
    # Ukraine (230) report in 2010 (D23).
    228L, 1961L, 1000,
    228L, 1962L, 1000,
    228L, 1963L, 1000,
    228L, 1964L, 1000,
    228L, 1965L, 1000,
    185L, 2010L, 300,
    230L, 2010L, 100
  ) |>
    dplyr::mutate(
      Element = "Agricultural Use",
      Item = "Nutrient nitrogen N (total)"
    )
}

.ryr_cropland_fixture <- function() {
  tibble::tribble(
    ~area_code, ~year, ~cropland_ha,
    203L, 1950L, 1000,
    203L, 1990L, 1000,
    203L, 2000L, 1000,
    203L, 2010L, 1000,
    68L, 2000L, 1000,
    68L, 2010L, 1000,
    106L, 2010L, 1000,
    231L, 2010L, 1000,
    228L, 1950L, 1000,
    228L, 1961L, 1000,
    185L, 2010L, 1000,
    230L, 2010L, 1000
  )
}

.ryr_production_fixture <- function() {
  tibble::tribble(
    ~year, ~area_code, ~item_prod_code, ~unit, ~value,
    # Linum: Spain seed-dominant, France fibre-dominant, Italy neither.
    2010L, 203L, 333L, "tonnes", 100,
    2010L, 203L, 773L, "tonnes", 10,
    2010L, 68L, 333L, "tonnes", 5,
    2010L, 68L, 773L, "tonnes", 50,
    # Hemp: Spain fibre only, and Spain has no SPAM `ofib`.
    2010L, 203L, 777L, "tonnes", 3,
    # Outside 1961-2023: ignored.
    1950L, 106L, 773L, "tonnes", 1e6
  )
}

.ryr_lpjml_row <- function(lon, lat, year, crop, y_r, y_i, f_r, f_i) {
  tibble::tibble(
    lon = lon,
    lat = lat,
    year = as.integer(year),
    lpjml_crop = crop,
    yield_rainfed = y_r,
    yield_irrigated = y_i,
    stand_frac_rainfed = f_r,
    stand_frac_irrigated = f_i,
    method_regime_yield = dplyr::if_else(
      year < 1901L,
      "lpjml_band_harvest_recycled_climate",
      "lpjml_band_harvest"
    )
  )
}

# Spain's cells are (-3.25, 40.25) and (-3.75, 40.25); France (2.25, 46.25);
# Italy (12.25, 42.25).
.ryr_lpjml_fixture <- function() {
  dplyr::bind_rows(
    .ryr_lpjml_row(-3.25, 40.25, 2010, "temperate cereals", 100, 300, .1, .1),
    .ryr_lpjml_row(-3.25, 40.25, 2000, "temperate cereals", 100, 50, .1, .1),
    .ryr_lpjml_row(-3.25, 40.25, 1950, "temperate cereals", 100, 200, .1, .1),
    .ryr_lpjml_row(-3.25, 40.25, 1900, "temperate cereals", 100, 200, .1, .1),
    .ryr_lpjml_row(-3.25, 40.25, 2000, "maize", 100, 200, .1, .1),
    # A bad maize year: 16 against the cell's own 4 (temporal 4).
    .ryr_lpjml_row(-3.25, 40.25, 2010, "maize", 100, 1600, .1, .1),
    # Pulses have a cell-year ratio but no 1994-2023 rows at all.
    .ryr_lpjml_row(-3.25, 40.25, 2010, "pulses", 100, 130, .1, .1),
    .ryr_lpjml_row(-3.25, 40.25, 2010, "others", 100, 150, .1, .1),
    .ryr_lpjml_row(2.25, 46.25, 2010, "temperate cereals", 100, 100, .1, .1)
  )
}

# The normaliser years: Spain's temperate cereals 2 over the window, others
# 1.5. Maize is 4 in the first Spanish cell and 1 in the second, so Spain's
# maize is (400 + 100) / 2 / 100 = 2.5 and the first cell's spatial part is
# 4 / 2.5 = 1.6 (both cells lie on one latitude, so their areas are equal).
.ryr_window_fixture <- function() {
  dplyr::bind_rows(
    .ryr_lpjml_row(-3.25, 40.25, 2000, "temperate cereals", 100, 200, .1, .1),
    .ryr_lpjml_row(-3.25, 40.25, 2001, "others", 100, 150, .1, .1),
    .ryr_lpjml_row(-3.25, 40.25, 2000, "maize", 100, 400, .1, .1),
    .ryr_lpjml_row(-3.75, 40.25, 2001, "maize", 100, 100, .1, .1),
    .ryr_lpjml_row(2.25, 46.25, 2000, "temperate cereals", 100, 100, .1, .1),
    # Outside 1994-2023: never enters the normaliser.
    .ryr_lpjml_row(-3.25, 40.25, 1980, "temperate cereals", 100, 900, .1, .1)
  )
}

.ryr_data <- function() {
  list(
    spam = .ryr_spam_fixture(),
    fertilizer = .ryr_fert_fixture(),
    cropland = .ryr_cropland_fixture(),
    production = .ryr_production_fixture(),
    lpjml = .ryr_lpjml_fixture(),
    lpjml_window = .ryr_window_fixture()
  )
}

.ryr_cell <- function(area_code, item, year) {
  coords <- list(
    `203` = c(-3.25, 40.25),
    `68` = c(2.25, 46.25),
    `106` = c(12.25, 42.25),
    `185` = c(37.25, 55.75),
    `228` = c(37.25, 55.75)
  )[[as.character(area_code)]]
  tibble::tibble(
    lon = coords[[1]],
    lat = coords[[2]],
    area_code = as.integer(area_code),
    item_prod_code = as.integer(item),
    year = as.integer(year)
  )
}

.ryr_build <- function(cells) {
  whep::build_regime_yield_ratio(cells, data = .ryr_data())
}

.ryr_one <- function(area_code, item, year) {
  .ryr_build(.ryr_cell(area_code, item, year))
}

# -- Anchor -------------------------------------------------------------------

testthat::test_that("the anchor is the ratio of the country's summed yields", {
  out <- .ryr_one(203, 15, 2010)
  testthat::expect_equal(out$ratio_spam, (500 / 100) / (1200 / 400))
  testthat::expect_identical(out$method_ratio_anchor, "spam_country")
  testthat::expect_identical(out$spam_crop_used, "whea")
})

testthat::test_that("a country without the crop's ratio takes the global one", {
  out <- .ryr_one(68, 15, 2010)
  global <- (500 / 100) / ((1200 + 3500) / (400 + 500))
  testthat::expect_equal(out$ratio_spam, global)
  testthat::expect_identical(out$method_ratio_anchor, "spam_global")
})

testthat::test_that("`+` on a direct row pools the crops before the ratio", {
  out <- .ryr_one(203, 79, 2010)
  pooled <- ((20 + 120) / (10 + 30)) / ((10 + 90) / (10 + 90))
  testthat::expect_equal(out$ratio_spam, pooled)
  # Not the mean of the two crops' ratios (2 and 4).
  testthat::expect_false(isTRUE(all.equal(out$ratio_spam, 3)))
})

testthat::test_that("a composite drops undefined members and renormalises", {
  # D21: Spain's other cereals are rainfed only, so only wheat (area 500)
  # and barley (area 200) weigh.
  out <- .ryr_one(203, 638, 2010)
  expected <- (500 * (5 / 3) + 200 * 2) / 700
  testthat::expect_equal(out$ratio_spam, expected)
  testthat::expect_identical(out$method_ratio_anchor, "spam_composite_country")
})

testthat::test_that("a composite with no defined member is the global one", {
  # France has no irrigated wheat, barley or other cereals. Global: wheat
  # 5 / (4700 / 900), weight 1000; barley 2, weight 200; other cereals
  # 4 / (40 / 30), weight 40.
  out <- .ryr_one(68, 638, 2010)
  r_w <- 5 / (4700 / 900)
  r_o <- 4 / (40 / 30)
  expected <- (1000 * r_w + 200 * 2 + 40 * r_o) / 1240
  testthat::expect_equal(out$ratio_spam, expected)
  testthat::expect_identical(out$method_ratio_anchor, "spam_composite_global")
})

testthat::test_that("Linum takes the dominant product's SPAM aggregate", {
  cells <- dplyr::bind_rows(
    .ryr_cell(203, 772, 2010),
    .ryr_cell(68, 772, 2010),
    .ryr_cell(106, 772, 2010)
  )
  out <- .ryr_build(cells) |> dplyr::arrange(.data$area_code)
  # France 68: flax fibre dominates -> ofib, France's own ratio 2.
  # Italy 106: produced neither -> the world's dominant product, linseed
  # (105 t vs 60 t), with its global ratio 3.
  # Spain 203: linseed dominates -> ooil, Spain's own ratio 3.
  testthat::expect_identical(out$spam_crop_used, c("ofib", "ooil", "ooil"))
  testthat::expect_equal(out$ratio_spam, c(2, 3, 3))
  testthat::expect_identical(
    out$method_ratio_anchor,
    c(
      "spam_dominance_country",
      "spam_dominance_world",
      "spam_dominance_country"
    )
  )
})

testthat::test_that("a dominant product SPAM lacks in the country is global", {
  # Spain's hemp is fibre only, and Spain has no SPAM `ofib`.
  out <- .ryr_one(203, 776, 2010)
  testthat::expect_identical(out$spam_crop_used, "ofib")
  testthat::expect_equal(out$ratio_spam, 2)
  testthat::expect_identical(out$method_ratio_anchor, "spam_dominance_global")
})

testthat::test_that("a crop SPAM has nowhere gets no ratio", {
  out <- .ryr_one(203, 249, 2010)
  testthat::expect_identical(out$method_ratio_anchor, "spam_none")
  testthat::expect_true(is.na(out$ratio))
})

testthat::test_that("an anchor below 1 is floored and stamped", {
  out <- .ryr_one(203, 27, 2010)
  testthat::expect_equal(out$ratio_spam, 0.5)
  testthat::expect_identical(out$ratio_anchor, 1)
  testthat::expect_match(out$method_regime_yield, "anchor_floor")
})

# -- Level --------------------------------------------------------------------

testthat::test_that("the level scales the excess gap with N per hectare", {
  # Spain: 2 kg/ha in 2000 against 1 in 2010.
  out <- .ryr_one(203, 15, 2000)
  testthat::expect_equal(out$ratio_level, 1 + (5 / 3 - 1) * 2)
  testthat::expect_identical(out$method_ratio_trend, "faostat")
  at_2010 <- .ryr_one(203, 15, 2010)
  testthat::expect_equal(at_2010$ratio_level, at_2010$ratio_anchor)
})

# Both Spanish cells, so Spain's maize normal pools them (D22).
.ryr_spain_maize <- function(year) {
  dplyr::bind_rows(
    .ryr_cell(203, 56, year),
    tibble::tibble(
      lon = -3.75,
      lat = 40.25,
      area_code = 203L,
      item_prod_code = 56L,
      year = as.integer(year)
    )
  ) |>
    .ryr_build() |>
    dplyr::arrange(dplyr::desc(.data$lon))
}

testthat::test_that("the cap binds the long-term component, not the level", {
  # Maize in 2000: anchor 6, scale 2 -> level 11 (uncapped); spatial 1.6 ->
  # 17.6, capped to 10; a good year (temporal 0.5) leaves R = 5.
  out <- .ryr_spain_maize(2000)[1, ]
  testthat::expect_equal(out$ratio_level, 11)
  testthat::expect_equal(out$ratio_spatial, 1.6)
  testthat::expect_identical(out$ratio_long_term, 10)
  testthat::expect_equal(out$ratio_temporal, 0.5)
  testthat::expect_equal(out$ratio, 5)
  testthat::expect_match(out$method_regime_yield, "level_cap")
  # The second cell: spatial 0.4 brings 11 under the cap (4.4), no stamp.
  dry <- .ryr_spain_maize(2000)[2, ]
  testthat::expect_equal(dry$ratio_spatial, 0.4)
  testthat::expect_equal(dry$ratio_long_term, 4.4)
  testthat::expect_identical(dry$method_ratio_temporal, "no_cell_ratio")
  testthat::expect_identical(dry$method_regime_yield, "none")
})

testthat::test_that("only the temporal part takes R past 10", {
  # Maize in 2010: level 6 x spatial 1.6 = 9.6 (under the cap); the bad year
  # (temporal 16 / 4 = 4) lifts R to 38.4.
  out <- .ryr_spain_maize(2010)[1, ]
  testthat::expect_equal(out$ratio_long_term, 9.6)
  testthat::expect_equal(out$ratio_temporal, 4)
  testthat::expect_equal(out$ratio, 38.4)
  testthat::expect_gt(out$ratio, 10)
  testthat::expect_identical(out$method_regime_yield, "none")
})

testthat::test_that("no synthetic N in 2010 keeps the level at 1", {
  out <- .ryr_one(68, 15, 2000)
  testthat::expect_identical(out$method_ratio_trend, "n_2010_zero")
  testthat::expect_identical(out$ratio_level, 1)
})

testthat::test_that("before 1913 there is no synthetic N and no gap", {
  out <- .ryr_one(203, 15, 1900)
  testthat::expect_identical(out$method_ratio_trend, "pre_synthetic_n")
  testthat::expect_identical(out$ratio_level, 1)
})

testthat::test_that("1913-1960 uses the Smil back-cast", {
  # Spain's 1961-1965 mean (156 t) over Smil's interpolated 1961-1965 mean
  # (15,600 kt) is a share of 1e-5; Smil 1950 is 3,700 kt, so 37 t on
  # 1,000 ha against 1 t/ha in 2010.
  out <- .ryr_one(203, 15, 1950)
  testthat::expect_identical(out$method_ratio_trend, "smil_backcast")
  testthat::expect_equal(out$ratio_level, 1 + (5 / 3 - 1) * 0.037)
})

testthat::test_that("a missing N value gives no ratio rather than a guess", {
  no_t <- .ryr_one(203, 15, 1990)
  testthat::expect_identical(no_t$method_ratio_trend, "no_n_t")
  testthat::expect_true(is.na(no_t$ratio))
  no_2010 <- .ryr_one(106, 15, 2010)
  testthat::expect_identical(no_2010$method_ratio_trend, "no_n_2010")
  testthat::expect_true(is.na(no_2010$ratio))
})

# -- Anomaly ------------------------------------------------------------------

testthat::test_that("spatial x temporal is the cell-year over country ratio", {
  # Spain's cereal cell: 3 in 2010, its window ratio 2 and Spain's 2 (the 1980
  # row is outside the window and does not count): spatial 1, temporal 1.5.
  out <- .ryr_one(203, 15, 2010)
  testthat::expect_equal(out$ratio_spatial, 1)
  testthat::expect_equal(out$ratio_temporal, 1.5)
  testthat::expect_equal(out$ratio_anomaly, 1.5)
  testthat::expect_identical(out$method_ratio_spatial, "lpjml")
  testthat::expect_identical(out$method_ratio_temporal, "lpjml")
  testthat::expect_equal(out$ratio, out$ratio_long_term * 1.5)
  # Maize: 16 in the cell in 2010 over Spain's 2.5 = 6.4 = 1.6 x 4.
  maize <- .ryr_spain_maize(2010)[1, ]
  testthat::expect_equal(maize$ratio_anomaly, 16 / 2.5)
  testthat::expect_equal(
    maize$ratio_spatial * maize$ratio_temporal,
    maize$ratio_anomaly
  )
})

testthat::test_that("items on the others stand use its anomaly", {
  out <- .ryr_one(203, 772, 2010)
  testthat::expect_equal(out$ratio_anomaly, 1)
  testthat::expect_identical(out$method_ratio_temporal, "lpjml")
})

testthat::test_that("an undefined LPJmL ratio leaves its part at 1", {
  no_cell <- .ryr_one(203, 27, 2010)
  testthat::expect_identical(no_cell$method_ratio_temporal, "no_cell_ratio")
  testthat::expect_identical(no_cell$method_ratio_spatial, "no_cell_normal")
  testthat::expect_identical(no_cell$ratio_anomaly, 1)
  # Beans (pulses): a 2010 ratio but no 1994-2023 one, so both parts are 1.
  no_normal <- .ryr_one(203, 176, 2010)
  testthat::expect_identical(no_normal$method_ratio_temporal, "no_cell_normal")
  testthat::expect_identical(no_normal$method_ratio_spatial, "no_cell_normal")
  testthat::expect_identical(no_normal$ratio_temporal, 1)
  testthat::expect_identical(no_normal$ratio_spatial, 1)
})

testthat::test_that("years before 1901 are stamped as recycled climate", {
  out <- .ryr_one(203, 15, 1900)
  testthat::expect_identical(
    out$method_ratio_temporal,
    "lpjml_recycled_climate"
  )
  testthat::expect_equal(out$ratio_anomaly, 1)
})

testthat::test_that("a product below 1 is floored to 1 and stamped", {
  # Spain 2000: level 1 + (2/3) * 2, temporal 0.5 / 2.
  out <- .ryr_one(203, 15, 2000)
  testthat::expect_lt(out$ratio_long_term * out$ratio_temporal, 1)
  testthat::expect_identical(out$ratio, 1)
  testthat::expect_match(out$method_regime_yield, "ratio_floor")
})

# -- D23: synthetic N through the polity lineage -------------------------------

testthat::test_that("a successor takes its predecessor's N before it existed", {
  # Russia (185) reports no N in 1961; the USSR's 1,000 t on 1,000 ha is
  # 1 t/ha, against Russia's own 0.3 t/ha in 2010.
  # Maize has no SPAM ratio in Russia; the global one is Spain's 6.
  out <- .ryr_one(185, 56, 1961)
  testthat::expect_identical(out$method_ratio_trend, "faostat_predecessor")
  testthat::expect_identical(out$method_ratio_n_2010, "own")
  testthat::expect_equal(out$ratio_level, 1 + (6 - 1) * (1 / 0.3))
  smil <- .ryr_one(185, 56, 1950)
  testthat::expect_identical(
    smil$method_ratio_trend,
    "smil_backcast_predecessor"
  )
})

testthat::test_that("a historical polity takes its successors' 2010 N", {
  # The USSR has no 2010 value; Russia and Ukraine together have 400 t on
  # 2,000 ha, 0.2 t/ha, against the USSR's own 1 t/ha in 1961.
  out <- .ryr_one(228, 56, 1961)
  testthat::expect_identical(out$method_ratio_trend, "faostat")
  testthat::expect_identical(out$method_ratio_n_2010, "successors")
  testthat::expect_equal(out$ratio_level, 1 + (6 - 1) * 5)
})

testthat::test_that("the normaliser is a ratio of area-weighted sums", {
  window <- dplyr::bind_rows(
    .ryr_lpjml_row(10.25, 45.25, 2000, "maize", 100, 200, .1, .1),
    .ryr_lpjml_row(10.75, 45.25, 2000, "maize", 100, 600, .1, .3)
  )
  cell_map <- tibble::tibble(
    lon = c(10.25, 10.75),
    lat = 45.25,
    area_code = 106L
  )
  out <- whep:::.ryr_lpjml_normal(cell_map, whep:::.ryr_window_sums(window))
  # Irrigated (0.1 * 200 + 0.3 * 600) / 0.4 = 500 over rainfed 100: 5, not
  # the mean of the cell ratios (2 and 6).
  testthat::expect_equal(out$lpjml_normal, 5)
})

# -- Invariants over a full fixture run ---------------------------------------

.ryr_all_cells <- function() {
  tidyr::expand_grid(
    area_code = c(203L, 68L, 106L),
    item_prod_code = c(15L, 27L, 56L, 79L, 176L, 249L, 638L, 772L, 776L),
    year = c(1900L, 1950L, 1990L, 2000L, 2010L)
  ) |>
    dplyr::mutate(
      lon = c(`203` = -3.25, `68` = 2.25, `106` = 12.25)[
        as.character(.data$area_code)
      ],
      lat = c(`203` = 40.25, `68` = 46.25, `106` = 42.25)[
        as.character(.data$area_code)
      ]
    )
}

testthat::test_that("R >= 1 and the long-term part <= 10 wherever R exists", {
  out <- .ryr_build(.ryr_all_cells())
  built <- out[!is.na(out$ratio), ]
  testthat::expect_gt(nrow(built), 0L)
  testthat::expect_true(all(built$ratio >= 1))
  testthat::expect_true(all(built$ratio_anchor >= 1))
  testthat::expect_true(all(built$ratio_level >= 1))
  testthat::expect_true(all(built$ratio_long_term <= 10))
  # Past 10 only through a bad year.
  testthat::expect_true(all(built$ratio_temporal[built$ratio > 10] > 1))
  testthat::expect_equal(
    built$ratio_anomaly,
    built$ratio_spatial * built$ratio_temporal
  )
  testthat::expect_identical(nrow(out), nrow(.ryr_all_cells()))
})

testthat::test_that("every stamp of build_regime_yield_ratio() is reachable", {
  out <- .ryr_build(.ryr_all_cells())
  testthat::expect_setequal(
    unique(out$method_ratio_anchor),
    c(
      "spam_country",
      "spam_global",
      "spam_composite_country",
      "spam_composite_global",
      "spam_dominance_country",
      "spam_dominance_global",
      "spam_dominance_world",
      "spam_none"
    )
  )
  testthat::expect_setequal(
    unique(out$method_ratio_trend),
    c(
      "faostat",
      "smil_backcast",
      "pre_synthetic_n",
      "n_2010_zero",
      "no_n_t",
      "no_n_2010"
    )
  )
  testthat::expect_setequal(
    unique(out$method_ratio_n_2010),
    c("own", "none", "not_needed")
  )
  testthat::expect_setequal(
    unique(out$method_ratio_spatial),
    c("lpjml", "no_cell_normal")
  )
  testthat::expect_setequal(
    unique(out$method_ratio_temporal),
    c("lpjml", "lpjml_recycled_climate", "no_cell_ratio", "no_cell_normal")
  )
  tokens <- unique(unlist(strsplit(out$method_regime_yield, ";")))
  testthat::expect_setequal(
    tokens,
    c("none", "anchor_floor", "level_cap", "ratio_floor")
  )
})

# -- Input checks -------------------------------------------------------------

testthat::test_that("bad inputs abort with a condition class", {
  cell <- .ryr_cell(203, 15, 2010)
  testthat::expect_error(
    whep::build_regime_yield_ratio(cell, data = list(spm = 1)),
    class = "whep_regime_yield_data"
  )
  testthat::expect_error(
    whep::build_regime_yield_ratio(dplyr::select(cell, -"lat"), data = list()),
    class = "whep_regime_yield_columns"
  )
  testthat::expect_error(
    .ryr_build(dplyr::mutate(cell, item_prod_code = 99999L)),
    class = "whep_regime_yield_items"
  )
  # An empty LPJmL layer would make every anomaly 1; it is refused.
  no_lpjml <- .ryr_data()
  no_lpjml$lpjml <- no_lpjml$lpjml[0, ]
  testthat::expect_error(
    whep::build_regime_yield_ratio(cell, data = no_lpjml),
    class = "whep_regime_yield_lpjml"
  )
  no_window <- .ryr_data()
  no_window$lpjml_window <- dplyr::filter(
    no_window$lpjml_window,
    .data$year < 1994L
  )
  testthat::expect_error(
    whep::build_regime_yield_ratio(cell, data = no_window),
    class = "whep_regime_yield_lpjml"
  )
  bad_spam <- .ryr_data()
  bad_spam$spam$production_t[1] <- NA
  testthat::expect_error(
    whep::build_regime_yield_ratio(cell, data = bad_spam),
    class = "whep_regime_yield_spam"
  )
  no_spam <- .ryr_data()
  no_spam$spam <- no_spam$spam[0, ]
  testthat::expect_error(
    whep::build_regime_yield_ratio(cell, data = no_spam),
    class = "whep_regime_yield_spam"
  )
  no_land <- .ryr_data()
  no_land$cropland <- no_land$cropland[0, ]
  testthat::expect_error(
    whep::build_regime_yield_ratio(cell, data = no_land),
    class = "whep_regime_yield_columns"
  )
})

# -- Split --------------------------------------------------------------------

# National wheat yields for the bound: 1..60 t/ha in Spain (1961-2020, OECD
# Europe) and 1..10 in the USA (1961-1970, region USA); a 1950 yield of 1000
# lies outside 1961-2023 and must not count. Maize has no yields at all.
.ryr_bound_production <- function() {
  wheat <- tibble::tibble(
    year = c(1961L:2020L, 1961L:1970L, 1950L),
    area_code = c(rep(203L, 60L), rep(231L, 10L), 203L),
    item_prod_code = 15L,
    tonnes = c(1:60, 1:10, 1000),
    ha = 1
  )
  tidyr::pivot_longer(
    wheat,
    c("tonnes", "ha"),
    names_to = "unit",
    values_to = "value"
  )
}

.ryr_global_max <- function() {
  stats::quantile(c(1:60, 1:10), 0.99, names = FALSE)
}

.ryr_split_cells <- function() {
  tibble::tribble(
    ~area_code, ~item_prod_code, ~production_t, ~rainfed_ha, ~irrigated_ha,
    ~ratio,
    203L, 15L, 500, 100, 50, 1.8,
    203L, 15L, 0, 100, 50, 2,
    203L, 15L, 900, 0, 10, 3,
    203L, 15L, 700, 70, 0, 3,
    203L, 15L, 100, 0, 0, 2,
    203L, 15L, 100, 10, 10, NA,
    203L, 56L, 100, 10, 10, 2,
    # Mean yield 30 is under the bound, the irrigated yield 107 is over it.
    203L, 15L, 1500, 40, 10, 10,
    # Mean yield 100 is over the bound whatever R is.
    203L, 15L, 3000, 10, 20, 40
  )
}

testthat::test_that("the split keeps production to 1e-9", {
  prod <- .ryr_bound_production()
  out <- whep::split_regime_yield(.ryr_split_cells(), production = prod)
  kept <- out[!is.na(out$yield_rainfed), ]
  back <- kept$rainfed_ha *
    kept$yield_rainfed +
    kept$irrigated_ha * kept$yield_irrigated
  rel <- abs(back - kept$production_t) / pmax(kept$production_t, 1e-12)
  testthat::expect_true(all(rel < 1e-9))
  testthat::expect_true(all(kept$ratio_split >= 1))
  testthat::expect_equal(
    kept$yield_irrigated,
    kept$ratio_split * kept$yield_rainfed
  )
})

testthat::test_that("the bound lowers R to meet Y_max, never below 1", {
  prod <- .ryr_bound_production()
  ymax <- .ryr_global_max()
  out <- whep::split_regime_yield(.ryr_split_cells(), production = prod)
  testthat::expect_equal(
    unique(out$yield_max[out$item_prod_code == 15L]),
    ymax
  )
  clipped <- out[out$method_regime_bound == "clipped", ]
  testthat::expect_identical(nrow(clipped), 1L)
  testthat::expect_equal(clipped$yield_irrigated, ymax, tolerance = 1e-9)
  testthat::expect_gte(clipped$ratio_split, 1)
  testthat::expect_lt(clipped$ratio_split, clipped$ratio)
  at_one <- out[out$method_regime_bound == "clipped_at_one", ]
  testthat::expect_identical(at_one$ratio_split, rep(1, nrow(at_one)))
  testthat::expect_true(all(at_one$yield_irrigated > ymax))
  testthat::expect_true(all(out$ratio_split >= 1, na.rm = TRUE))
})

testthat::test_that("bound = 'region' pools the cell's WHEP region only", {
  prod <- .ryr_bound_production()
  cells <- tibble::tribble(
    ~area_code, ~item_prod_code, ~production_t, ~rainfed_ha, ~irrigated_ha,
    ~ratio,
    231L, 15L, 400, 30, 20, 3
  )
  global <- whep::split_regime_yield(cells, production = prod)
  region <- whep::split_regime_yield(cells, "region", production = prod)
  usa_max <- stats::quantile(1:10, 0.99, names = FALSE)
  testthat::expect_identical(global$method_regime_bound, "not_binding")
  testthat::expect_identical(region$method_regime_bound, "clipped")
  testthat::expect_equal(region$yield_max, usa_max)
  testthat::expect_equal(region$yield_irrigated, usa_max, tolerance = 1e-9)
  testthat::expect_identical(region$method_yield_bound, "region")
  testthat::expect_false("region" %in% names(region))
})

testthat::test_that("every stamp of split_regime_yield() is reachable", {
  prod <- .ryr_bound_production()
  out <- whep::split_regime_yield(.ryr_split_cells(), production = prod)
  testthat::expect_setequal(
    unique(out$method_regime_split),
    c("yield_ratio", "rainfed_only", "irrigated_only", "no_area", "no_ratio")
  )
  testthat::expect_setequal(
    unique(out$method_regime_bound),
    c(
      "not_binding",
      "clipped",
      "clipped_at_one",
      "no_bound",
      "not_applicable"
    )
  )
  none <- out[out$method_regime_split %in% c("no_area", "no_ratio"), ]
  testthat::expect_true(all(is.na(none$yield_rainfed)))
})

testthat::test_that("split inputs are checked", {
  prod <- .ryr_bound_production()
  cells <- .ryr_split_cells()
  testthat::expect_error(
    whep::split_regime_yield(dplyr::select(cells, -"ratio"), production = prod),
    class = "whep_regime_yield_columns"
  )
  negative <- dplyr::mutate(cells, rainfed_ha = -1)
  testthat::expect_error(
    whep::split_regime_yield(negative, production = prod),
    class = "whep_regime_yield_columns"
  )
  testthat::expect_error(
    whep::split_regime_yield(
      cells,
      production = dplyr::bind_rows(prod, prod)
    ),
    class = "whep_regime_yield_production"
  )
})

# -- Constants and example ----------------------------------------------------

testthat::test_that("the Linum and Hemp products match primary_double.csv", {
  path <- system.file(
    "extdata",
    "harmonization",
    "primary_double.csv",
    package = "whep"
  )
  double <- utils::read.csv(path, stringsAsFactors = FALSE)
  products <- whep:::.ryr_dominance_products()
  linum <- double$item_prod_code[double$Item_area == "Linum"]
  hemp <- double$item_prod_code[double$Item_area == "Hemp"]
  testthat::expect_setequal(
    linum,
    unlist(products[1, c("seed_code", "fibre_code")])
  )
  testthat::expect_setequal(
    hemp,
    unlist(products[2, c("seed_code", "fibre_code")])
  )
  mapping <- whep::regime_yield_crop_mapping
  testthat::expect_identical(
    mapping$spam_crop[match(c(333L, 773L, 336L, 777L), mapping$item_prod_code)],
    c("ooil", "ofib", "ooil", "ofib")
  )
})

testthat::test_that("the example returns the documented schema", {
  testthat::skip_if_not_installed("pointblank")
  out <- whep::build_regime_yield_ratio(example = TRUE)
  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_identical(
    names(out),
    names(.ryr_build(.ryr_cell(203, 15, 2010)))
  )
  pointblank::expect_col_vals_gte(out, "ratio", 1, na_pass = TRUE)
  pointblank::expect_col_vals_lte(out, "ratio_long_term", 10, na_pass = TRUE)
  testthat::expect_type(out$item_prod_code, "integer")
  testthat::expect_type(out$area_code, "integer")
})
