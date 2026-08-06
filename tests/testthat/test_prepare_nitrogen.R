# Smoke tests for the country-level N-totals helpers added in
# inst/scripts/prepare_spatialize_all.R. Helpers live at script scope
# (not package R/) so we source the script once and exercise the
# offline-safe ones with fixture data. The pin-backed helpers
# (`.faostat_synth_country_total`, `.faostat_manure_country_long`) need
# whep_read_file() over the network and are not covered here.

.source_prepare_spatialize()


test_that(".smil_global_yearly interpolates linearly between anchors", {
  skip_if_not(exists(".smil_global_yearly", mode = "function"))
  out <- .smil_global_yearly(first_year = 1950L, last_year = 1965L)
  expect_true(all(out$year >= 1950L & out$year <= 1965L))
  expect_equal(nrow(out), 16L)
  # 1955 + 1960 + 1965 are Smil anchors; reproduce them exactly
  smil <- whep::smil_2001_synthetic_n_global
  for (anchor in c(1955L, 1960L, 1965L)) {
    expect_equal(
      out$global_mg_n[out$year == anchor],
      smil$global_kt_n[smil$year == anchor] * 1000
    )
  }
  # Midpoint between 1955 (6300 kt) and 1960 (10500 kt) is at 1957.5;
  # linear interpolation gives 8400 kt at 1957 (3/5 of the way).
  v_1957 <- out$global_mg_n[out$year == 1957L]
  expect_equal(v_1957, (6300 + (10500 - 6300) * 2 / 5) * 1000, tolerance = 1)
})


test_that(".smil_synth_pre_1961 backcasts country synth using shares", {
  skip_if_not(exists(".smil_synth_pre_1961", mode = "function"))
  synth_faostat <- tibble::tribble(
    ~area_code, ~area_name, ~year, ~mg_n,
    1L,         "A",        1961L, 60000,
    1L,         "A",        1962L, 60000,
    1L,         "A",        1963L, 60000,
    1L,         "A",        1964L, 60000,
    1L,         "A",        1965L, 60000,
    2L,         "B",        1961L, 40000,
    2L,         "B",        1962L, 40000,
    2L,         "B",        1963L, 40000,
    2L,         "B",        1964L, 40000,
    2L,         "B",        1965L, 40000
  )
  out <- .smil_synth_pre_1961(synth_faostat)
  expect_true(all(out$year >= 1913L & out$year <= 1960L))
  expect_true(all(out$mg_n > 0))
  # Global pre-1961 should grow over time (Smil temporal shape).
  global_1920 <- sum(out$mg_n[out$year == 1920L])
  global_1950 <- sum(out$mg_n[out$year == 1950L])
  expect_true(global_1950 > global_1920)
  # Country A holds 60 % of the calibration-window global total, so its
  # share of any pre-1961 year should also be ~60 %.
  share_a_1950 <- out$mg_n[out$area_code == 1L & out$year == 1950L] /
    sum(out$mg_n[out$year == 1950L])
  expect_equal(share_a_1950, 0.6, tolerance = 0.01)
})


test_that(".faostat_manure_shares_const averages shares over 1961-65", {
  skip_if_not(exists(".faostat_manure_shares_const", mode = "function"))
  manure_long <- tibble::tribble(
    ~area_code, ~area_name, ~year, ~element,   ~mg_n,
    1L,         "A",        1961L, "excreted", 1000,
    1L,         "A",        1961L, "applied",  300,
    1L,         "A",        1961L, "pasture",  500,
    1L,         "A",        1962L, "excreted", 1100,
    1L,         "A",        1962L, "applied",  330,
    1L,         "A",        1962L, "pasture",  550,
    1L,         "A",        1980L, "excreted", 5000, # outside calibration window
    1L,         "A",        1980L, "applied",  100,  # should be ignored
    1L,         "A",        1980L, "pasture",  100
  )
  out <- .faostat_manure_shares_const(manure_long, share_window = 1961L:1962L)
  expect_equal(nrow(out), 1L)
  expect_equal(out$applied_share, 0.3, tolerance = 1e-6)
  expect_equal(out$pasture_share, 0.5, tolerance = 1e-6)
})


test_that(".livestock_manure_split applies per-country shares", {
  skip_if_not(exists(".livestock_manure_split", mode = "function"))
  excreted <- tibble::tribble(
    ~year, ~area_code, ~area_name, ~mg_n_excreted,
    1851L, 1L,         "A",        1000,
    1900L, 1L,         "A",        2000,
    1961L, 1L,         "A",        4000
  )
  shares <- tibble::tribble(
    ~area_code, ~area_name, ~applied_share, ~pasture_share,
    1L,         "A",        0.3,            0.5
  )
  out <- .livestock_manure_split(excreted, shares)
  expect_true(all(c("Manure", "Grassland_excretion") %in% out$fert_type))
  applied_1851 <- out$mg_n[
    out$year == 1851L & out$fert_type == "Manure"
  ]
  pasture_1851 <- out$mg_n[
    out$year == 1851L & out$fert_type == "Grassland_excretion"
  ]
  expect_equal(applied_1851, 300, tolerance = 1e-6)
  expect_equal(pasture_1851, 500, tolerance = 1e-6)
})


test_that(".fill_n_inputs_to_target_year carry-forwards beyond last obs", {
  skip_if_not(exists(".fill_n_inputs_to_target_year", mode = "function"))
  n_in <- tibble::tribble(
    ~year, ~area_code, ~area_name, ~crop_name, ~land_use, ~fert_type, ~area_ha, ~mg_n, ~kg_n_ha,
    2019L, 1L,         "A",        "wheat",    "Cropland", "Synthetic", 1000,    150,   150,
    2020L, 1L,         "A",        "wheat",    "Cropland", "Synthetic", 1000,    155,   155,
    2021L, 1L,         "A",        "wheat",    "Cropland", "Synthetic", 1000,    160,   160
  )
  out <- .fill_n_inputs_to_target_year(n_in, target_year = 2023L)
  expect_equal(max(out$year), 2023L)
  expect_true(all(2019:2023 %in% out$year))
  # whep::fill_linear carries the last observation forward as a constant;
  # 2022 and 2023 take the 2021 mg_n value (160).
  expect_equal(out$mg_n[out$year == 2022L], 160, tolerance = 1e-6)
  expect_equal(out$mg_n[out$year == 2023L], 160, tolerance = 1e-6)
})


test_that(".fill_n_inputs_to_target_year leaves pre-first-obs as NA", {
  skip_if_not(exists(".fill_n_inputs_to_target_year", mode = "function"))
  n_in <- tibble::tribble(
    ~year, ~area_code, ~area_name, ~crop_name, ~land_use, ~fert_type, ~area_ha, ~mg_n, ~kg_n_ha,
    1920L, 1L,         "A",        "wheat",    "Cropland", "Synthetic", 1000,    50,    50,
    1921L, 1L,         "A",        "wheat",    "Cropland", "Synthetic", 1000,    55,    55
  )
  out <- .fill_n_inputs_to_target_year(n_in, target_year = 1925L)
  # 1920 is the first observed year; backward fill is disabled.
  expect_false(any(out$year < 1920L))
})


test_that(".aggregate_nitrogen_pft area-weights N rates by crop area", {
  skip_if_not(exists(".aggregate_nitrogen_pft", mode = "function"))
  # Two crops share PFT band 1 in the same cell: a 100-ha crop at 200 kgN/ha and
  # a 1-ha crop at 10 kgN/ha. An unweighted mean gives 105; the area-weighted
  # mean must stay close to the dominant crop's rate.
  ng <- tibble::tribble(
    ~year, ~pft, ~fert_type, ~row, ~col, ~kg_n_ha, ~rainfed_ha, ~irrigated_ha,
    2000L, 1L, "Synthetic", 5L, 7L, 200, 100, 0,
    2000L, 1L, "Synthetic", 5L, 7L, 10, 1, 0
  )
  out <- tibble::as_tibble(.aggregate_nitrogen_pft(ng))
  expect_equal(nrow(out), 1L)
  # Area-weighted: 200 over 100 ha and 10 over 1 ha gives 20010 over 101.
  expect_equal(out$value, 20010 / 101, tolerance = 1e-9)
  expect_false(isTRUE(all.equal(out$value, 105)))
})

test_that(".aggregate_nitrogen_pft conserves total applied N mass", {
  skip_if_not(exists(".aggregate_nitrogen_pft", mode = "function"))
  ng <- tibble::tribble(
    ~year, ~pft, ~fert_type, ~row, ~col, ~kg_n_ha, ~rainfed_ha, ~irrigated_ha,
    2000L, 1L, "Synthetic", 5L, 7L, 200, 80, 20,
    2000L, 1L, "Synthetic", 5L, 7L, 10, 1, 0
  )
  out <- tibble::as_tibble(.aggregate_nitrogen_pft(ng))
  total_area <- sum(ng$rainfed_ha + ng$irrigated_ha)
  mass_in <- sum(ng$kg_n_ha * (ng$rainfed_ha + ng$irrigated_ha))
  # band rate x band area must reproduce the input applied-N mass
  expect_equal(out$value * total_area, mass_in, tolerance = 1e-6)
})

test_that(".aggregate_nitrogen_pft drops zero-area bands and NaN", {
  skip_if_not(exists(".aggregate_nitrogen_pft", mode = "function"))
  ng <- tibble::tribble(
    ~year, ~pft, ~fert_type, ~row, ~col, ~kg_n_ha, ~rainfed_ha, ~irrigated_ha,
    2000L, 1L, "Synthetic", 5L, 7L, 200, 0, 0
  )
  out <- .aggregate_nitrogen_pft(ng)
  expect_equal(nrow(out), 0L)
})


# ---- label -> area_code resolution (#494) --------------------------------
#
# `.read_crop_base_rates_local()` used to repair mueller_synthetic_n's FAO-style
# legacy ISO codes with a hand-maintained 14-entry `recode()` list before
# joining on iso3c. The list is gone; the mapping now comes from
# `whep::polity_label_aliases`. These tests are the lock that the substitution
# stays value-for-value identical, and that the bridge back to a numeric code
# stays in the grid's area space.

# The retired list, kept here as the expectation the replacement must reproduce.
.retired_mueller_recode <- function() {
  tibble::tribble(
    ~legacy, ~iso3c,
    "SRM",   "SCG",
    "GUA",   "GTM",
    "BZE",   "BLZ",
    "COS",   "CRI",
    "ELS",   "SLV",
    "HAI",   "HTI",
    "HON",   "HND",
    "ROM",   "ROU",
    "TRI",   "TTO",
    "ZAR",   "COD",
    "BHA",   "BHS",
    "BAR",   "BRB",
    "DMI",   "DMA",
    "STL",   "LCA"
  )
}


test_that(".spatialize_label_area_code reproduces the retired recode list", {
  skip_if_not(exists(".spatialize_label_area_code", mode = "function"))
  regions <- .spatialize_area_lookup()
  recode_list <- .retired_mueller_recode()
  present <- recode_list$legacy %in% whep::mueller_synthetic_n$iso3c
  # 10 of the 14 entries name a code the dataset actually uses; the other 4
  # (BHA, BAR, DMI, STL) never occur in it and were dead weight.
  expect_equal(sum(present), 10L)
  expect_setequal(
    recode_list$legacy[!present],
    c("BHA", "BAR", "DMI", "STL")
  )
  live <- recode_list[present, ]
  got <- .spatialize_label_area_code(
    live$legacy,
    source = "mueller-synthetic-n",
    year = .mueller_base_year(),
    area_lookup = regions
  )
  want <- regions$area_code[match(live$iso3c, regions$iso3c)]
  expect_equal(got, want)
})


test_that("mueller synthetic rates are unchanged by dropping the list", {
  skip_if_not(exists(".spatialize_label_area_code", mode = "function"))
  regions <- .spatialize_area_lookup()
  mueller <- whep::mueller_synthetic_n
  recode_list <- .retired_mueller_recode()
  legacy_iso <- dplyr::coalesce(
    recode_list$iso3c[match(mueller$iso3c, recode_list$legacy)],
    mueller$iso3c
  )
  before <- regions$area_code[match(legacy_iso, regions$iso3c)]
  after <- .spatialize_label_area_code(
    mueller$iso3c,
    source = "mueller-synthetic-n",
    year = .mueller_base_year(),
    area_lookup = regions
  )
  # Row-for-row identity over all 5,043 rows, not just a matching total.
  expect_equal(after, before)
  expect_equal(sum(!is.na(after)), nrow(mueller))
  expect_equal(dplyr::n_distinct(after), dplyr::n_distinct(before))
})


test_that(".spatialize_label_area_code stays in the grid's area space", {
  skip_if_not(exists(".spatialize_label_area_code", mode = "function"))
  regions <- .spatialize_area_lookup()
  got <- .spatialize_label_area_code(
    c("SDN", "ETH"),
    source = "mueller-synthetic-n",
    year = .mueller_base_year(),
    area_lookup = regions
  )
  # `polity_area_crosswalk` would answer 206 "Sudan (former)" and 238 for both
  # of Ethiopia's FAOSTAT areas. regions.csv -- the table the country grid is
  # rasterised from -- carries 276 and 238 and has no 206 and no 62 at all, so
  # a bridge through `polity_area_code` would attach rates to absent cells.
  expect_equal(got, c(276L, 238L))
  expect_false(any(c(206L, 62L) %in% regions$area_code))
  expect_true(all(got %in% regions$area_code))
})


test_that(".spatialize_label_area_code honours the alias year scope", {
  skip_if_not(exists(".spatialize_label_area_code", mode = "function"))
  regions <- .spatialize_area_lookup()
  # The published alias is `SRM -> SCG-1992-2006`. Outside that window nothing
  # claims the label, so the honest answer is NA rather than a nearest guess.
  got <- .spatialize_label_area_code(
    c("SRM", "SRM"),
    source = "mueller-synthetic-n",
    year = c(2000L, 2020L),
    area_lookup = regions
  )
  expect_equal(got[[1]], regions$area_code[match("SCG", regions$iso3c)])
  expect_true(is.na(got[[2]]))
})


test_that(".spatialize_label_area_code returns NA for unknown labels", {
  skip_if_not(exists(".spatialize_label_area_code", mode = "function"))
  regions <- .spatialize_area_lookup()
  got <- .spatialize_label_area_code(
    c("ZZZ", "not a country"),
    source = "mueller-synthetic-n",
    year = .mueller_base_year(),
    area_lookup = regions
  )
  expect_true(all(is.na(got)))
})
