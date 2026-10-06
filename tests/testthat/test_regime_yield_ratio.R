# build_regime_yield_ratio() and split_regime_yield(): the irrigated:rainfed
# yield ratio of issue #1233 and the conservation split. Fully
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
    # Other cereals: Spain rainfed only, so undefined there.
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
    # Oil crops and fibres for Linum and Hemp.
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
    # Ukraine (230) report in 2010.
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
    # The USSR has FAOSTAT cropland from 1961 only; before, it takes its
    # successors' LUH2 back-cast (see .ryr_luh2_fixture()).
    228L, 1961L, 1000,
    185L, 2010L, 1000,
    230L, 2010L, 1000
  )
}

# The raw `faostat-production` pin shape, read for the Linum and Hemp
# dominance: flax fibre is 771 "Flax, raw or retted" there.
.ryr_production_fixture <- function() {
  tibble::tribble(
    ~`Area Code`, ~`Item Code`, ~Element, ~Year, ~Value,
    # Linum: Spain seed-dominant, France fibre-dominant, Italy neither.
    203L, 333L, "Production", 2010L, 100,
    203L, 771L, "Production", 2010L, 10,
    68L, 333L, "Production", 2010L, 5,
    68L, 771L, "Production", 2010L, 50,
    # Another element never counts.
    68L, 333L, "Area harvested", 2010L, 1e6,
    # Hemp: Spain fibre only, and Spain has no SPAM `ofib`.
    203L, 777L, "Production", 2010L, 3,
    # Outside 1961-2023: ignored.
    106L, 771L, "Production", 1950L, 1e6
  )
}

# LUH2 national cropland (Mha) of the USSR's successors: 15 in 1950 and 25 in
# 1961, so the USSR's 1950 cropland is 1,000 ha x 15 / 25 = 600 ha.
.ryr_luh2_fixture <- function() {
  tibble::tribble(
    ~ISO3, ~Year, ~Land_Use, ~Area_Mha,
    "RUS", 1950L, "c3ann", 10,
    "RUS", 1961L, "c3ann", 20,
    "UKR", 1950L, "c3ann", 5,
    "UKR", 1961L, "c3ann", 5
  )
}

# The cells the LPJmL run covers: Italy's cell is off its land mask.
.ryr_grid_fixture <- function() {
  tibble::tibble(
    lon = c(-3.25, -3.75, 2.25, 37.25),
    lat = c(40.25, 40.25, 46.25, 55.75)
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
    luh2 = .ryr_luh2_fixture(),
    faostat_production = .ryr_production_fixture(),
    lpjml = .ryr_lpjml_fixture(),
    lpjml_window = .ryr_window_fixture(),
    lpjml_grid = .ryr_grid_fixture()
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

# Many tests build the same cells against the same fixture (#1349). A build is
# a pure function of `cells` here, since `data` is always `.ryr_data()` and
# nothing in this file mocks a binding, so each distinct `cells` is built once
# per file and the result shared. The data itself is shared too. The last test
# asserts nothing changed either.
.ryr_build <- function(cells) {
  memo_fixture(
    paste0("ryr_build_", rlang::hash(cells)),
    \() whep::build_regime_yield_ratio(cells, data = .ryr_shared_data())
  )
}

.ryr_shared_data <- function() {
  memo_fixture("ryr_data", .ryr_data)
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
  # Spain's other cereals are rainfed only, so only wheat (area 500)
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
  testthat::expect_identical(
    out$method_dominance,
    rep("dominance_raw_faostat", 3)
  )
  wheat <- .ryr_one(203, 15, 2010)
  testthat::expect_identical(wheat$method_dominance, "not_applicable")
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
  testthat::expect_true(is.na(out$ratio_unbounded))
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

# Both Spanish cells, so Spain's maize normal pools them.
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

testthat::test_that("the anomalies scale the excess, capped at 9 long-term", {
  # Maize in 2000: anchor 6, scale 2 -> level 11 (uncapped); its excess 10 x
  # spatial 1.6 = 16, capped to 9 -> long-term 10; a good year (temporal 0.5)
  # halves the excess: R = 1 + 9 x 0.5 = 5.5.
  out <- .ryr_spain_maize(2000)[1, ]
  testthat::expect_equal(out$ratio_level, 11)
  testthat::expect_equal(out$ratio_spatial, 1.6)
  testthat::expect_identical(out$ratio_long_term, 10)
  testthat::expect_equal(out$ratio_temporal, 0.5)
  testthat::expect_equal(out$ratio_unbounded, 5.5)
  testthat::expect_match(out$method_regime_yield, "level_cap")
  # The second cell: 10 x spatial 0.4 = 4, under the cap: long-term 5.
  dry <- .ryr_spain_maize(2000)[2, ]
  testthat::expect_equal(dry$ratio_spatial, 0.4)
  testthat::expect_equal(dry$ratio_long_term, 5)
  testthat::expect_identical(dry$method_ratio_temporal, "no_cell_ratio")
  testthat::expect_identical(dry$method_regime_yield, "none")
})

testthat::test_that("only the temporal part takes R past 10", {
  # Maize in 2010: level 6, excess 5 x spatial 1.6 = 8 (under the cap), so
  # long-term 9; the bad year (temporal 16 / 4 = 4) lifts R to 1 + 8 x 4.
  out <- .ryr_spain_maize(2010)[1, ]
  testthat::expect_equal(out$ratio_long_term, 9)
  testthat::expect_equal(out$ratio_temporal, 4)
  testthat::expect_equal(out$ratio_unbounded, 33)
  testthat::expect_gt(out$ratio_unbounded, 10)
  testthat::expect_identical(out$method_regime_yield, "none")
})

testthat::test_that("R is 1 wherever the level is 1, whatever the anomaly", {
  x <- tibble::tibble(
    lon = 0,
    lat = 0,
    area_code = 1L,
    item_prod_code = 1L,
    year = 2000L,
    ratio_spam = c(3, 3, 0.5),
    n_scale = c(0, 0, 1),
    ratio_spatial = c(2, 0.3, 5),
    ratio_temporal = c(5, 0.1, 7),
    spam_crop_used = "whea",
    method_ratio_anchor = "spam_country",
    method_ratio_trend = "faostat",
    method_ratio_n_2010 = "own",
    method_ratio_cropland = "own",
    method_dominance = "not_applicable",
    method_ratio_spatial = "lpjml",
    method_ratio_temporal = "lpjml"
  )
  out <- whep:::.ryr_combine(x)
  testthat::expect_identical(out$ratio_level, c(1, 1, 1))
  testthat::expect_identical(out$ratio_unbounded, c(1, 1, 1))
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
  testthat::expect_true(is.na(no_t$ratio_unbounded))
  no_2010 <- .ryr_one(106, 15, 2010)
  testthat::expect_identical(no_2010$method_ratio_trend, "no_n_2010")
  testthat::expect_true(is.na(no_2010$ratio_unbounded))
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
  testthat::expect_equal(
    out$ratio_unbounded,
    1 + (out$ratio_long_term - 1) * 1.5
  )
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

testthat::test_that("a good year shrinks the excess towards 1, never below", {
  # Spain 2000: level 1 + (2/3) * 2, temporal 0.5 / 2 = 0.25.
  out <- .ryr_one(203, 15, 2000)
  testthat::expect_equal(out$ratio_temporal, 0.25)
  testthat::expect_equal(
    out$ratio_unbounded,
    1 + (out$ratio_long_term - 1) * 0.25
  )
  testthat::expect_gt(out$ratio_unbounded, 1)
})

testthat::test_that("a cell the LPJmL run does not cover keeps anomaly 1", {
  out <- .ryr_one(106, 15, 2010)
  testthat::expect_identical(out$method_ratio_spatial, "no_lpjml_cell")
  testthat::expect_identical(out$method_ratio_temporal, "no_lpjml_cell")
  testthat::expect_identical(out$ratio_anomaly, 1)
})

# -- Synthetic N through the polity lineage -----------------------------------

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
  testthat::expect_identical(
    smil$method_ratio_cropland,
    "successors_luh2_backcast"
  )
})

testthat::test_that("before 1961 the USSR divides by its successors' land", {
  # The USSR's 1950 N: Smil 1950 (3,700 kt) x its 1961-1965 share (1,000 t
  # over Smil's 15,600 kt); its cropland: 1,000 ha x 15 / 25 = 600 ha; its
  # 2010 N: its successors' 0.2 t/ha.
  out <- .ryr_one(228, 56, 1950)
  n_1950 <- 3700 * 1000 / 15600
  testthat::expect_identical(out$method_ratio_trend, "smil_backcast")
  testthat::expect_identical(
    out$method_ratio_cropland,
    "successors_luh2_backcast"
  )
  testthat::expect_identical(out$method_ratio_n_2010, "successors")
  testthat::expect_equal(out$ratio_level, 1 + (6 - 1) * (n_1950 / 600) / 0.2)
})

testthat::test_that("before 1961 a territory with no N reported has level 1", {
  # France reported no N in 1961-1965 and nothing reported for it.
  out <- .ryr_one(68, 15, 1950)
  testthat::expect_identical(out$method_ratio_trend, "no_n_reported_pre1961")
  testthat::expect_identical(out$ratio_level, 1)
  testthat::expect_identical(out$ratio_unbounded, 1)
  # From 1961 a missing value is a gap, not a zero.
  testthat::expect_true(is.na(.ryr_one(203, 15, 1990)$ratio_unbounded))
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
  built <- out[!is.na(out$ratio_unbounded), ]
  testthat::expect_gt(nrow(built), 0L)
  testthat::expect_true(all(built$ratio_unbounded >= 1))
  testthat::expect_true(all(built$ratio_anchor >= 1))
  testthat::expect_true(all(built$ratio_level >= 1))
  testthat::expect_true(all(built$ratio_long_term <= 10))
  # Past 10 only through a bad year.
  testthat::expect_true(all(
    built$ratio_temporal[built$ratio_unbounded > 10] > 1
  ))
  # The anomalies scale the excess: no excess, no gap.
  testthat::expect_true(all(built$ratio_unbounded[built$ratio_level == 1] == 1))
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
      "no_n_reported_pre1961",
      "n_2010_zero",
      "no_n_t",
      "no_n_2010"
    )
  )
  testthat::expect_setequal(
    unique(out$method_ratio_cropland),
    c("own", "none", "not_needed")
  )
  testthat::expect_setequal(
    unique(out$method_ratio_n_2010),
    c("own", "none", "not_needed")
  )
  testthat::expect_setequal(
    unique(out$method_ratio_spatial),
    c("lpjml", "no_cell_normal", "no_lpjml_cell")
  )
  testthat::expect_setequal(
    unique(out$method_ratio_temporal),
    c(
      "lpjml",
      "lpjml_recycled_climate",
      "no_cell_ratio",
      "no_cell_normal",
      "no_lpjml_cell"
    )
  )
  testthat::expect_setequal(
    unique(out$method_dominance),
    c("dominance_raw_faostat", "not_applicable")
  )
  tokens <- unique(unlist(strsplit(out$method_regime_yield, ";")))
  testthat::expect_setequal(tokens, c("none", "anchor_floor", "level_cap"))
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
    ~ratio_unbounded,
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
    203L, 15L, 3000, 10, 20, 40,
    # Rainfed yield 0.8 is under the floor (1); R = 2 brings it to 1.
    203L, 15L, 200, 100, 50, 3,
    # Mean yield 0.67 is under the floor whatever R is.
    203L, 15L, 100, 100, 50, 2,
    # A forage grass has no FAOSTAT yields: it takes wheat's, the item that
    # directly carries one of its SPAM crops. Its absurd R of 1000 puts Y_r
    # at 0.01, under the floor; R = (500 - 100) / 50 = 8 brings it to 1.
    203L, 638L, 500, 100, 50, 1000
  )
}

.ryr_global_min <- function() {
  stats::quantile(c(1:60, 1:10), 0.01, names = FALSE)
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
  testthat::expect_true(all(kept$ratio >= 1))
  testthat::expect_equal(
    kept$yield_irrigated,
    kept$ratio * kept$yield_rainfed
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
  testthat::expect_gte(clipped$ratio, 1)
  testthat::expect_lt(clipped$ratio, clipped$ratio_unbounded)
  at_one <- out[out$method_regime_bound == "clipped_at_one", ]
  testthat::expect_identical(at_one$ratio, rep(1, nrow(at_one)))
  testthat::expect_true(all(at_one$yield_irrigated > ymax))
  testthat::expect_true(all(out$ratio >= 1, na.rm = TRUE))
})

testthat::test_that("the floor lowers R to meet Y_min, never below 1", {
  prod <- .ryr_bound_production()
  ymin <- .ryr_global_min()
  out <- whep::split_regime_yield(.ryr_split_cells(), production = prod)
  testthat::expect_equal(
    unique(out$yield_min[out$item_prod_code == 15L]),
    ymin
  )
  floored <- out[
    out$method_rainfed_floor == "rainfed_floor" & out$item_prod_code == 15L,
  ]
  testthat::expect_identical(nrow(floored), 1L)
  testthat::expect_equal(floored$yield_rainfed, ymin, tolerance = 1e-9)
  testthat::expect_equal(floored$ratio, 2)
  at_one <- out[out$method_rainfed_floor == "rainfed_floor_at_one", ]
  testthat::expect_true(nrow(at_one) > 0L)
  testthat::expect_true(all(at_one$ratio == 1))
  testthat::expect_true(all(at_one$yield_rainfed < ymin))
  # Wherever the floor applies and does not end at 1, Y_r meets it.
  held <- out[out$method_rainfed_floor %in% c("not_binding", "rainfed_floor"), ]
  testthat::expect_true(all(held$yield_rainfed >= ymin - 1e-9))
})

testthat::test_that("a trivial split has ratio 1 whatever R, and no bounds", {
  prod <- .ryr_bound_production()
  out <- whep::split_regime_yield(.ryr_split_cells(), production = prod)
  irr <- out[out$method_regime_split == "trivial_irrigated_only", ]
  rain <- out[out$method_regime_split == "trivial_rainfed_only", ]
  testthat::expect_identical(irr$ratio, 1)
  testthat::expect_identical(rain$ratio, 1)
  testthat::expect_equal(irr$yield_irrigated, 900 / 10)
  testthat::expect_equal(rain$yield_rainfed, 700 / 70)
  testthat::expect_identical(
    c(irr$method_regime_bound, rain$method_rainfed_floor),
    rep("not_applicable_trivial", 2)
  )
})

testthat::test_that("an item without yields is bounded via its SPAM crop", {
  prod <- .ryr_bound_production()
  out <- whep::split_regime_yield(.ryr_split_cells(), production = prod)
  forage <- out[out$item_prod_code == 638L, ]
  testthat::expect_identical(forage$method_bound_source, "bound_via_spam_crop")
  testthat::expect_equal(forage$yield_min, .ryr_global_min())
  testthat::expect_equal(forage$yield_max, .ryr_global_max())
  testthat::expect_equal(forage$ratio, 8)
  testthat::expect_equal(forage$yield_rainfed, 1)
  testthat::expect_identical(forage$method_rainfed_floor, "rainfed_floor")
  # Maize has neither its own yields nor an item directly carrying `maiz`.
  maize <- out[out$item_prod_code == 56L, ]
  testthat::expect_identical(maize$method_bound_source, "none")
  testthat::expect_identical(maize$method_regime_bound, "no_bound")
  # Wheat's own yields.
  testthat::expect_true(all(
    out$method_bound_source[out$item_prod_code == 15L] == "own_yields"
  ))
})

testthat::test_that("the regional pool falls back to the world's, stamped", {
  prod <- .ryr_bound_production()
  # The fixture has wheat yields in OECD Europe (Spain) and the USA only; a
  # Mexican cell (Central America) finds none in its region.
  cells <- tibble::tribble(
    ~area_code, ~item_prod_code, ~production_t, ~rainfed_ha, ~irrigated_ha,
    ~ratio_unbounded,
    138L, 15L, 400, 30, 20, 3
  )
  out <- whep::split_regime_yield(cells, "region", production = prod)
  testthat::expect_identical(out$method_bound_source, "own_yields_global")
  testthat::expect_equal(out$yield_max, .ryr_global_max())
})

testthat::test_that("bound = 'region' pools the cell's WHEP region only", {
  prod <- .ryr_bound_production()
  cells <- tibble::tribble(
    ~area_code, ~item_prod_code, ~production_t, ~rainfed_ha, ~irrigated_ha,
    ~ratio_unbounded,
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
    c(
      "yield_ratio",
      "trivial_rainfed_only",
      "trivial_irrigated_only",
      "no_area",
      "no_ratio"
    )
  )
  testthat::expect_setequal(
    unique(out$method_regime_bound),
    c(
      "not_binding",
      "clipped",
      "clipped_at_one",
      "no_bound",
      "not_applicable_trivial",
      "not_applicable"
    )
  )
  testthat::expect_setequal(
    unique(out$method_rainfed_floor),
    c(
      "not_binding",
      "rainfed_floor",
      "rainfed_floor_at_one",
      "no_bound",
      "not_applicable_trivial",
      "not_applicable"
    )
  )
  testthat::expect_setequal(
    unique(out$method_bound_source),
    c("own_yields", "bound_via_spam_crop", "none")
  )
  none <- out[out$method_regime_split %in% c("no_area", "no_ratio"), ]
  testthat::expect_true(all(is.na(none$yield_rainfed)))
})

testthat::test_that("split inputs are checked", {
  prod <- .ryr_bound_production()
  cells <- .ryr_split_cells()
  testthat::expect_error(
    whep::split_regime_yield(
      dplyr::select(cells, -"ratio_unbounded"),
      production = prod
    ),
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
  # Flax fibre is 771 both in primary_double.csv and in the raw FAOSTAT pin
  # the dominance reads; primary_double.csv named 773 until whep#1302.
  testthat::expect_setequal(
    linum,
    unlist(products[1, c("seed_code", "fibre_code")])
  )
  testthat::expect_identical(products$fibre_code[1], 771L)
  testthat::expect_setequal(
    hemp,
    unlist(products[2, c("seed_code", "fibre_code")])
  )
  mapping <- whep::regime_yield_crop_mapping
  testthat::expect_identical(
    mapping$spam_crop[
      match(c(333L, 771L, 773L, 336L, 777L), mapping$item_prod_code)
    ],
    c("ooil", "ofib", "ofib", "ooil", "ofib")
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
  pointblank::expect_col_vals_gte(out, "ratio_unbounded", 1, na_pass = TRUE)
  pointblank::expect_col_vals_lte(out, "ratio_long_term", 10, na_pass = TRUE)
  testthat::expect_type(out$item_prod_code, "integer")
  testthat::expect_type(out$area_code, "integer")
})

testthat::test_that("the shared builds and their data were never mutated", {
  expect_memo_fixtures_untouched("ryr_")
  testthat::expect_identical(.ryr_shared_data(), .ryr_data())
})
