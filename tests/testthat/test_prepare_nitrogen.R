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


# ---- the other two label readers (#576) ----------------------------------
#
# `crops_manure_n` and `lassaletta_grassland_share` were the two sites #494
# deliberately left on ad-hoc matching. They are not one question: the manure
# reader's labels are a modern ISO vocabulary and the alias route reproduces
# its join exactly once read at that vintage, while the grassland reader's
# labels are genuinely year-scoped and the two routes disagree on real rows.
# These tests pin both, and the duplicate-key rule that the alias route needs.

# The full regions.csv, not `.spatialize_area_lookup()`: the grassland reader's
# status-quo route joins on `area_name`, which the two-column lookup drops.
.regions_full_for_spatialize <- function() {
  utils::read.csv(
    system.file("extdata", "regions.csv", package = "whep"),
    stringsAsFactors = FALSE
  )
}


test_that("crops_manure_n names countries in a post-2011 ISO vocabulary", {
  skip_if_not(exists(".crops_manure_label_year", mode = "function"))
  iso <- unique(whep::crops_manure_n$ISO)
  # It separates the successors...
  expect_true(all(c("SRB", "MNE", "SSD", "CZE", "SVK", "COD", "TLS") %in% iso))
  # ...and never names the predecessors. SSD alone dates the vocabulary to
  # 2011 or later, which is why the labels must not be read at Mueller's
  # circa-2000 base year even though West et al.'s rates are circa 2000.
  expect_false(any(c("SCG", "SUD", "CSK", "YUG", "ZAR", "TMP") %in% iso))
  expect_gte(.crops_manure_label_year(), 2011L)
})


test_that("the manure alias route reproduces the retired iso3c join", {
  skip_if_not(exists(".crops_manure_label_year", mode = "function"))
  regions <- .regions_full_for_spatialize()
  labels <- sort(unique(whep::crops_manure_n$ISO))
  # The retired join: straight onto regions.csv's iso3c.
  before <- regions$area_code[match(labels, regions$iso3c)]
  after <- .spatialize_label_area_code(
    labels,
    source = "crops-manure-n",
    year = .crops_manure_label_year(),
    area_lookup = regions
  )
  same <- !is.na(before) & !is.na(after) & before == after
  moved <- labels[!same]
  # `RoW` is the only label whose answer changes, and the reader drops it.
  expect_equal(moved, "RoW")
  expect_true(is.na(before[labels == "RoW"]))
  expect_equal(after[labels == "RoW"], .rest_of_world_area_code())
  kept <- labels != "RoW"
  expect_equal(after[kept], before[kept])
  expect_equal(sum(!is.na(after)), length(labels))
})


test_that("the manure label year is not load-bearing from 2011 on", {
  skip_if_not(exists(".crops_manure_label_year", mode = "function"))
  regions <- .regions_full_for_spatialize()
  labels <- sort(unique(whep::crops_manure_n$ISO))
  years <- 2011L:2023L
  grid <- expand.grid(
    label = labels,
    year = years,
    stringsAsFactors = FALSE
  )
  got <- .spatialize_label_area_code(
    grid$label,
    source = "crops-manure-n",
    year = grid$year,
    area_lookup = regions
  )
  by_year <- split(got, grid$year)
  expect_true(all(vapply(by_year, identical, logical(1), by_year[[1]])))
  expect_equal(
    by_year[[1]],
    by_year[[as.character(
      .crops_manure_label_year()
    )]]
  )
})


test_that("reading the manure labels in 2000 would move three countries", {
  skip_if_not(exists(".crops_manure_label_year", mode = "function"))
  regions <- .regions_full_for_spatialize()
  labels <- c("SRB", "MNE", "SSD")
  at_2000 <- .spatialize_label_area_code(
    labels,
    source = "crops-manure-n",
    year = .mueller_base_year(),
    area_lookup = regions
  )
  scg <- regions$area_code[match("SCG", regions$iso3c)]
  sdn <- regions$area_code[match("SDN", regions$iso3c)]
  # Serbia and Montenegro fold together and South Sudan disappears into
  # Sudan -- distinctions the source itself makes, so 2000 is the wrong
  # vintage for these labels even though it is the right one for Mueller's.
  expect_equal(at_2000, c(scg, scg, sdn))
  at_vintage <- .spatialize_label_area_code(
    labels,
    source = "crops-manure-n",
    year = .crops_manure_label_year(),
    area_lookup = regions
  )
  expect_equal(at_vintage, regions$area_code[match(labels, regions$iso3c)])
  expect_equal(dplyr::n_distinct(at_vintage), 3L)
})


test_that("the grassland default route reproduces the name join", {
  skip_if_not(exists(".grass_share_area_code", mode = "function"))
  regions <- .regions_full_for_spatialize()
  lass <- whep::lassaletta_grassland_share
  before <- regions$area_code[match(lass$Country, regions$area_name)]
  after <- .grass_share_area_code(
    lass$Country,
    year = as.integer(lass$year),
    area_lookup = regions,
    route = "area_name"
  )
  # Row-for-row over all 6,909 rows, not a matching total.
  expect_equal(after, before)
  expect_equal(sum(!is.na(after)), 6370L)
  expect_equal(nrow(lass), 6909L)
})


test_that("the grassland alias route gains and loses the labels #576 named", {
  skip_if_not(exists(".grass_share_area_code", mode = "function"))
  regions <- .regions_full_for_spatialize()
  lass <- whep::lassaletta_grassland_share
  before <- regions$area_code[match(lass$Country, regions$area_name)]
  after <- .grass_share_area_code(
    lass$Country,
    year = as.integer(lass$year),
    area_lookup = regions,
    route = "alias_map"
  )
  expect_equal(sum(!is.na(after)), 6713L)
  # "FSU" joined the list in the #835 upstream re-sync. Its alias row is
  # unchanged (`FSU` -> `F228-1945-1991`, 1961-1991); what changed is that
  # upstream filled that polity's `iso3_code` with `SUN`, so the
  # polity -> iso3 -> area bridge that `.spatialize_label_area_code()` uses now
  # completes where it used to dead-end on `NA`.
  expect_setequal(
    unique(lass$Country[is.na(before) & !is.na(after)]),
    c(
      "China",
      "Cote d'Ivoire",
      "DPRepublic of Korea",
      "Cape Verde",
      "Swaziland",
      "Sudan (former)",
      "Ethiopia PDR",
      "Belgium-Luxemburg",
      "FSU",
      "Occupied Palestinian Territory"
    )
  )
  expect_setequal(
    unique(lass$Country[!is.na(before) & is.na(after)]),
    c(
      "South Sudan",
      "Yugoslav SFR",
      "Czechoslovakia",
      "Viet Nam",
      "Botswana"
    )
  )
  expect_equal(sum(is.na(before) & !is.na(after)), 445L)
  expect_equal(sum(!is.na(before) & is.na(after)), 102L)
})


test_that(".dedup_grass_share collapses only the alias route's Sudan pair", {
  skip_if_not(exists(".dedup_grass_share", mode = "function"))
  regions <- .regions_full_for_spatialize()
  resolved <- function(route) {
    whep::lassaletta_grassland_share |>
      dplyr::mutate(
        area_code = .grass_share_area_code(
          Country,
          year = as.integer(year),
          area_lookup = regions,
          route = route
        )
      ) |>
      dplyr::filter(!is.na(area_code))
  }
  by_name <- resolved("area_name")
  by_alias <- resolved("alias_map")
  # The name route has no duplicate key, so the rule is a no-op there.
  expect_equal(sum(duplicated(by_name[c("year", "area_code")])), 0L)
  expect_equal(nrow(.dedup_grass_share(by_name)), nrow(by_name))
  # The alias route has 49, all Sudan, all on one area code.
  dup <- by_alias[duplicated(by_alias[c("year", "area_code")]), ]
  expect_equal(nrow(dup), 49L)
  expect_equal(
    unique(dup$area_code),
    regions$area_code[match("SDN", regions$iso3c)]
  )
  expect_setequal(
    unique(by_alias$Country[
      by_alias$area_code == regions$area_code[match("SDN", regions$iso3c)]
    ]),
    c("Sudan", "Sudan (former)")
  )
  expect_equal(nrow(.dedup_grass_share(by_alias)), nrow(by_alias) - 49L)
  expect_equal(
    sum(duplicated(
      .dedup_grass_share(by_alias)[c("year", "area_code")]
    )),
    0L
  )
})


test_that(".dedup_grass_share aborts when two labels disagree", {
  skip_if_not(exists(".dedup_grass_share", mode = "function"))
  clash <- tibble::tribble(
    ~year, ~area_code, ~grass_share,
    1961L, 276L,       0.00,
    1961L, 276L,       0.20
  )
  expect_error(.dedup_grass_share(clash), "methodological choice")
  agree <- tibble::tribble(
    ~year, ~area_code, ~grass_share,
    1961L, 276L,       0.05,
    1961L, 276L,       0.05
  )
  expect_equal(nrow(.dedup_grass_share(agree)), 1L)
})
