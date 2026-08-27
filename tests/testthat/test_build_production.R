# test_build_production.R — unit tests for R/build_production.R helpers

# -- Fixtures ------------------------------------------------------------------

.make_afse_stub <- function() {
  list(
    items_full = tibble::tribble(
      ~item_cbs, ~item_cbs_code, ~comm_group, ~group, ~default_destiny,
      "Wheat", 2511L, "Cereals", "Crop products", "Food",
      "Maize", 2514L, "Cereals", "Crop products", "Feed"
    ),
    items_prod_full = tibble::tribble(
      ~item_prod, ~item_prod_code, ~item_cbs, ~item_cbs_code, ~live_anim, ~live_anim_code,
      "Wheat", 15L, "Wheat", 2511L, NA_character_, NA_integer_,
      "Maize", 56L, "Maize", 2514L, NA_character_, NA_integer_
    ),
    regions_full = tibble::tribble(
      ~polity_name, ~polity_code, ~iso3c,
      "Spain", 203L, "ESP",
      "France", 68L, "FRA"
    ),
    polities_cats = tibble::tribble(
      ~polity_name, ~polity_code, ~dissolved,
      "Spain", 203L, FALSE,
      "France", 68L, FALSE
    ),
    Primary_double = tibble::tibble(
      Item_area = character(),
      multi_type = character()
    ),
    NoDataProducts = character()
  )
}

.make_primary_raw <- function() {
  tibble::tribble(
    ~year, ~area, ~area_code, ~item_prod, ~item_prod_code, ~item_cbs, ~item_cbs_code, ~unit, ~value,
    2000L, "Spain", 203L, "Wheat", 15L, "Wheat", 2511L, "tonnes", 5000,
    2000L, "Spain", 203L, "Wheat", 15L, "Wheat", 2511L, "ha", 200,
    2001L, "Spain", 203L, "Wheat", 15L, "Wheat", 2511L, "tonnes", 5500,
    2001L, "Spain", 203L, "Wheat", 15L, "Wheat", 2511L, "ha", 210
  )
}


# -- filter_dissolved_countries ------------------------------------------------

test_that(".filter_dissolved_countries removes dissolved polities", {
  # `area` is periodized after `.aggregate_to_polities()`; the filter must
  # key on `area_code` (Czechoslovakia = 51), not on the plain name.
  df <- tibble::tribble(
    ~year, ~area, ~area_code, ~value,
    2000L, "Spain", 203L, 10,
    2000L, "Czechoslovakia (1947-1993)", 51L, 20,
    1990L, "Czechoslovakia (1947-1993)", 51L, 30
  )

  result <- whep:::.filter_dissolved_countries(df)
  # Czechoslovakia after 1992 should be removed
  expect_false(
    any(result$area_code == 51L & result$year > 1992)
  )
  # Czechoslovakia before 1993 should be kept
  expect_true(
    any(result$area_code == 51L & result$year == 1990)
  )
})

test_that(".filter_dissolved_countries dedups at the 1992/1993 boundary", {
  # Both the dissolved polity and its successors present in the overlap
  # years. Czechoslovakia (51) must go for 1993, successors for 1992.
  df <- tibble::tribble(
    ~year, ~area, ~area_code, ~value,
    1993L, "Czechoslovakia (1947-1993)", 51L, 20,
    1993L, "Czechia", 167L, 10,
    1993L, "Slovakia", 199L, 11,
    1992L, "Czechia", 167L, 9,
    1992L, "Slovakia", 199L, 8
  )

  result <- whep:::.filter_dissolved_countries(df)
  # dissolved parent removed after 1992
  expect_false(any(result$area_code == 51L))
  # successors removed before 1993
  expect_false(any(result$area_code %in% c(167L, 199L) & result$year < 1993))
  # successors kept in 1993
  expect_true(any(result$area_code == 167L & result$year == 1993))
  expect_true(any(result$area_code == 199L & result$year == 1993))
})


# -- add_historical_yields (pre-1962 yield proxy) ------------------------------

test_that(".add_historical_yields back-casts a periodized-name country", {
  # China's periodized `area` ("China (PRC)") never equals the plain proxy
  # name, so the proxy must join by `area_code` (41). If it matches, the
  # pre-1961 tonnes are back-cast from the yield-growth proxy.
  df <- tibble::tribble(
    ~year, ~area, ~area_code, ~item_prod, ~item_prod_code, ~item_cbs, ~item_cbs_code, ~land_use, ~live_anim, ~live_anim_code, ~unit, ~value, ~source,
    1961L, "China (PRC)", 41L, "Wheat", "15", "Wheat", 2511L, "cropland", NA_character_, NA_integer_, "tonnes", 6000, "FAOSTAT_prod",
    1961L, "China (PRC)", 41L, "Wheat", "15", "Wheat", 2511L, "cropland", NA_character_, NA_integer_, "ha", 100, "FAOSTAT_prod",
    1960L, "China (PRC)", 41L, "Wheat", "15", "Wheat", 2511L, "cropland", NA_character_, NA_integer_, "tonnes", 0, "FAOSTAT_prod",
    1960L, "China (PRC)", 41L, "Wheat", "15", "Wheat", 2511L, "cropland", NA_character_, NA_integer_, "ha", 100, "FAOSTAT_prod"
  )
  int_yields <- tibble::tribble(
    ~year, ~area_code, ~item_prod_code, ~yield,
    1960L, 41L, "15", 2.0,
    1961L, 41L, "15", 2.5
  )

  result <- whep:::.add_historical_yields(df, int_yields) |>
    tibble::as_tibble()
  # proxy joined by code → yield attached for the periodized-name country
  expect_true(all(!is.na(result$yield)))
  # 1960 tonnes back-cast from proxy: 100 * (60 * 2.0/2.5) = 4800
  tonnes_1960 <- result$tonnes[result$year == 1960L]
  expect_equal(tonnes_1960, 4800)
})


# -- merge_euadb_fodder --------------------------------------------------------

test_that(".merge_euadb_fodder merges EU rows by code, not fragmenting", {
  # FAO fodder uses periodized names; EU AgriDB uses plain names. The merge
  # must align them by `area_code` so a matched crop stays a single row.
  fodder <- tibble::tribble(
    ~year, ~area, ~area_code, ~item_prod, ~item_prod_code, ~t, ~t_dm, ~ha,
    2010L, "Foo (1990-2020)", 100L, "Grass", "3001", 5, 4, 10
  )
  fodder_euadb <- tibble::tribble(
    ~year, ~area, ~area_code, ~Name_Eurostat, ~Label, ~Unit, ~value,
    2010L, "Foo", 100L, "GrassEuro", "Area", "Mha", 5,
    2010L, "Foo", 100L, "GrassEuro", "Yield", "kgN/ha", 200
  )
  items_prod <- tibble::tribble(
    ~item_prod, ~item_prod_code, ~Name_Eurostat,
    "Grass", "3001", "GrassEuro"
  )

  result <- whep:::.merge_euadb_fodder(fodder, fodder_euadb, items_prod)
  # single merged row for the matched crop (not two fragments)
  expect_equal(nrow(result), 1L)
  expect_false(is.na(result$ha))
  expect_false(is.na(result$ha_euadb))
})


test_that(".merge_euadb_fodder keys the EU yield on area_code, not the label", {
  # Two areas can carry the same label -- the yield join used to be keyed on it,
  # so each area's row picked up both areas' yields and fanned out (#655). The
  # label rides along in the fixture only to show the old key had it available.
  fodder <- tibble::tribble(
    ~year, ~area, ~area_code, ~item_prod, ~item_prod_code, ~t, ~t_dm, ~ha,
    2010L, "Foo", 100L, "Grass", "3001", 5, 4, 10,
    2010L, "Foo", 101L, "Grass", "3001", 7, 6, 20
  )
  fodder_euadb <- tibble::tribble(
    ~year, ~area, ~area_code, ~Name_Eurostat, ~Label, ~Unit, ~value,
    2010L, "Foo", 100L, "GrassEuro", "Area", "Mha", 5,
    2010L, "Foo", 100L, "GrassEuro", "Yield", "kgN/ha", 200,
    2010L, "Foo", 101L, "GrassEuro", "Area", "Mha", 6,
    2010L, "Foo", 101L, "GrassEuro", "Yield", "kgN/ha", 300
  )
  items_prod <- tibble::tribble(
    ~item_prod, ~item_prod_code, ~Name_Eurostat,
    "Grass", "3001", "GrassEuro"
  )

  result <- whep:::.merge_euadb_fodder(fodder, fodder_euadb, items_prod)

  expect_equal(nrow(result), 2L)
  expect_equal(
    result$kgnha_euadb[result$area_code == 100L],
    200
  )
  expect_equal(
    result$kgnha_euadb[result$area_code == 101L],
    300
  )
})


test_that(".combine_fodder keeps one row per area_code and item (#655)", {
  # FAO labels are periodized, so one `area_code` carries several labels over a
  # series. While the label was a grouping key, `.fill_fodder_gaps()`'s cross
  # join gave every label the full year span, and the country ended up with one
  # full-area copy of each item per label -- Egypt (59) had three in 1961.
  i_fodder <- tibble::tribble(
    ~year, ~area, ~area_code, ~item_prod, ~item_prod_code, ~value,
    1961L, "Egypt (1925-1967)", 59L, "Clover", "640", 100,
    1962L, "Egypt", 59L, "Clover", "640", 120
  )
  fodder_euadb <- tibble::tibble(
    year = integer(),
    area = character(),
    area_code = integer(),
    Name_Eurostat = character(),
    Label = character(),
    Unit = character(),
    value = numeric()
  )
  dm_yield <- tibble::tribble(
    ~year, ~area_code, ~yield_dm,
    1961L, 59L, 4,
    1962L, 59L, 4
  )
  items_prod <- tibble::tribble(
    ~item_prod, ~item_prod_code, ~Name_biomass, ~Name_Eurostat,
    "Clover", "640", "Clover biomass", NA_character_
  )
  biomass <- tibble::tribble(
    ~Name_biomass, ~Product_kgDM_kgFM, ~Product_kgN_kgDM,
    "Clover biomass", 0.2, 0.03
  )

  result <- whep:::.combine_fodder(
    i_fodder,
    fodder_euadb,
    dm_yield,
    items_prod,
    biomass
  )

  dups <- result |>
    dplyr::count(year, area_code, item_prod_code, unit) |>
    dplyr::filter(n > 1L)
  expect_equal(nrow(dups), 0L)
  # one label per (year, area_code), taken from the polity crosswalk for that
  # year -- the same rule `.aggregate_to_polities()` labels a bucket by
  expect_equal(
    nrow(dplyr::distinct(result, year, area_code, area)),
    2L
  )
  expect_false(any(is.na(result$area)))
})


test_that(".combine_fodder ignores dm_yield years no fodder source covers", {
  # `dm_yield` spans every FAOSTAT year, the fodder sources do not: 7705 of its
  # keys carry no fodder record. They used to arrive as `NA`-label rows and be
  # dropped by `.fill_fodder_gaps()`'s `!is.na(area)`; now that the drop keys on
  # `area_code`, which they do have, the join type is what keeps them out -- and
  # they must stay out, or the cross join fabricates a fodder series for a year
  # no source covers.
  i_fodder <- tibble::tribble(
    ~year, ~area, ~area_code, ~item_prod, ~item_prod_code, ~value,
    1961L, "Egypt (1925-1967)", 59L, "Clover", "640", 100
  )
  fodder_euadb <- tibble::tibble(
    year = integer(),
    area = character(),
    area_code = integer(),
    Name_Eurostat = character(),
    Label = character(),
    Unit = character(),
    value = numeric()
  )
  dm_yield <- tibble::tribble(
    ~year, ~area_code, ~yield_dm,
    1961L, 59L, 4,
    # a year, and an area, the fodder sources say nothing about
    2020L, 59L, 4,
    1961L, 100L, 4
  )
  items_prod <- tibble::tribble(
    ~item_prod, ~item_prod_code, ~Name_biomass, ~Name_Eurostat,
    "Clover", "640", "Clover biomass", NA_character_
  )
  biomass <- tibble::tribble(
    ~Name_biomass, ~Product_kgDM_kgFM, ~Product_kgN_kgDM,
    "Clover biomass", 0.2, 0.03
  )

  result <- whep:::.combine_fodder(
    i_fodder,
    fodder_euadb,
    dm_yield,
    items_prod,
    biomass
  )

  expect_equal(sort(unique(result$year)), 1961)
  expect_equal(unique(result$area_code), 59L)
})


# -- EU AgriDB region crosswalk ------------------------------------------------

# `.read_fodder_euadb()` resolves the source's `Region` through
# `regions_full$ADB_Region`. A missing key leaves `area_code` NA and
# `.fill_fodder_gaps()`'s `dt[!is.na(area)]` then discards the rows in silence,
# so the country falls back to the dry-matter-yield estimator while its peers
# use the source. `AT` and `GB` were missing, costing 2030 rows -- 8.8% of the
# input -- for Austria and the United Kingdom over 1961-2019 (#585).
#
# The 28 codes are the distinct `Region` values of the `eu-agridb-fodder` pin,
# read on 2026-08-07. They are listed here rather than read from the pin so
# the check stays offline; a region the pin adds later is caught at runtime by
# `.warn_unmapped_adb_regions()` instead.

test_that("regions_full keys every EU AgriDB region, incl. AT and GB", {
  euadb_regions <- c(
    "AT",
    "BE",
    "BE_LU",
    "BG",
    "CZ",
    "CZ_SK",
    "DE",
    "DK",
    "EE",
    "ES",
    "FI",
    "FR",
    "GB",
    "GR",
    "HR",
    "HU",
    "IE",
    "IT",
    "LT",
    "LU",
    "LV",
    "NL",
    "PL",
    "PT",
    "RO",
    "SE",
    "SI",
    "SK"
  )
  keys <- whep::regions_full$ADB_Region

  expect_equal(setdiff(euadb_regions, keys), character(0))

  austria <- whep::regions_full |>
    dplyr::filter(.data$ADB_Region == "AT")
  expect_equal(austria$legacy_polity_prefix, "AUT")
  expect_equal(austria$code, 11L)

  uk <- whep::regions_full |>
    dplyr::filter(.data$ADB_Region == "GB")
  expect_equal(uk$legacy_polity_prefix, "GBR")
  expect_equal(uk$code, 229L)
})

test_that("an ADB region resolving to no area warns and is named", {
  euadb <- tibble::tribble(
    ~adb_region, ~area_code,
    "XX",        NA_integer_,
    "XX",        NA_integer_,
    "FR",        68L
  )

  expect_warning(
    result <- whep:::.warn_unmapped_adb_regions(euadb),
    "XX"
  )
  # the warning names how many rows go, and passes the table through untouched
  expect_warning(whep:::.warn_unmapped_adb_regions(euadb), "2 fodder rows")
  expect_equal(result, euadb)
})

test_that(".read_fodder_euadb warns on a region it cannot resolve", {
  # The warning is only useful if the reader is actually wired to it, so drive
  # the reader itself over a stubbed pin rather than the helper alone.
  fake_pin <- tibble::tribble(
    ~Region, ~Crop,   ~Year, ~Value, ~Label,           ~Unit,
    "FR",    "G3000", 2000L, 1.0,    "Harvested area", "Mha",
    "ZZ",    "G3000", 2000L, 2.0,    "Harvested area", "Mha"
  )
  testthat::local_mocked_bindings(
    .read_input = function(...) fake_pin
  )

  expect_warning(result <- whep:::.read_fodder_euadb(), "ZZ")
  expect_equal(result$area_code, c(68L, NA_integer_))
})

test_that("a fully mapped ADB table passes through without a warning", {
  euadb <- tibble::tribble(
    ~adb_region, ~area_code,
    "FR",        68L,
    "DE",        79L
  )

  expect_no_warning(result <- whep:::.warn_unmapped_adb_regions(euadb))
  expect_equal(result, euadb)
})


# -- combine_primary -----------------------------------------------------------

test_that(".combine_primary_raw aggregates and keeps item_prod columns", {
  fao_combined <- tibble::tribble(
    ~year, ~area, ~area_code, ~item_prod, ~item_prod_code, ~unit, ~value,
    2000L, "Spain", 203L, "Wheat", 15L, "t", 5000,
    2000L, "Spain", 203L, "Wheat", 15L, "ha", 200
  ) |>
    dplyr::mutate(source = NA_character_)

  fao_liv_all <- tibble::tibble(
    year = integer(),
    area = character(),
    area_code = integer(),
    item_prod = character(),
    item_prod_code = integer(),
    unit = character(),
    value = double(),
    source = character()
  )

  result <- whep:::.combine_primary_raw(
    fao_combined,
    fao_liv_all
  )
  expect_true("item_prod" %in% names(result))
  expect_true("item_prod_code" %in% names(result))
  expect_true("source" %in% names(result))
  expect_equal(nrow(result), 2L)
  # NA source gets tagged as FAOSTAT
  expect_true(all(result$source == "FAOSTAT_prod"))
})


# -- correct_tea ---------------------------------------------------------------

test_that(".correct_tea divides Tea leaves value by 4.37 after 1990", {
  df <- tibble::tribble(
    ~item_prod, ~item_prod_code, ~unit, ~value, ~year,
    "Tea leaves", 667L, "t", 437, 2000L,
    "Tea leaves", 667L, "t", 437, 1980L,
    "Wheat", 15L, "t", 200, 2000L
  )

  result <- whep:::.correct_tea(df)
  # post-1990 Tea leaves value should be divided by 4.37
  expect_equal(result$value[1], 437 / 4.37)
  # pre-1990 Tea leaves value should remain unchanged
  expect_equal(result$value[2], 437)
  # Wheat unchanged

  expect_equal(result$value[3], 200)
})


# -- deduplication regression tests -------------------------------------------

test_that(".collapse_yield_rows aggregates duplicate key rows", {
  df <- tibble::tribble(
    ~year, ~area, ~area_code, ~item_prod, ~item_prod_code, ~live_anim_code, ~unit, ~t, ~fu, ~yield_c, ~source, ~Multi_type, ~live_anim,
    2010L, "Spain", 203L, "Milk", 951L, "946", "t_LU", 8, 4, 2, NA_character_, NA_character_, NA_character_,
    2010L, "Spain", 203L, "Milk", 951L, "946", "t_LU", 12, 6, 2, "FAOSTAT_prod", "Primary", "Buffalo",
    2010L, "Spain", 203L, "Wheat", 15L, NA_character_, "t_ha", 20, 10, 2, "FAOSTAT_prod", NA_character_, NA_character_
  )

  result <- whep:::.collapse_yield_rows(df)

  expect_equal(nrow(result), 2L)

  milk <- result |>
    dplyr::filter(item_prod == "Milk")

  expect_equal(milk$t, 20)
  expect_equal(milk$fu, 10)
  expect_equal(milk$yield_c, 2)
  expect_equal(milk$source, "FAOSTAT_prod")
  expect_equal(milk$Multi_type, "Primary")
  expect_equal(milk$live_anim, "Buffalo")
})

test_that(".collapse_cbs_ratio_rows aggregates duplicate ratio rows", {
  df <- tibble::tribble(
    ~year, ~area, ~area_code, ~item_prod, ~item_prod_code, ~item_cbs, ~item_cbs_code, ~live_anim, ~live_anim_code, ~unit, ~group, ~t, ~fu, ~yield_c, ~yield_glo, ~t_cbs, ~prod_cbs_ratio, ~prod_cbs_count, ~sumprod_cbs_ratio, ~source, ~Multi_type,
    2010L, "Spain", 203L, "Wheat", 15L, "Wheat and products", 2511L, NA_character_, NA_character_, "t_ha", "Crop products", 8, 4, 2, 2, 20, 0.4, 2, 0.4, NA_character_, NA_character_,
    2010L, "Spain", 203L, "Wheat", 15L, "Wheat and products", 2511L, NA_character_, NA_character_, "t_ha", "Crop products", 12, 6, 2, 2, 20, 0.6, 2, 0.6, "FAOSTAT_prod", "Primary"
  )

  result <- whep:::.collapse_cbs_ratio_rows(df)

  expect_equal(nrow(result), 1L)
  expect_equal(result$t, 20)
  expect_equal(result$fu, 10)
  expect_equal(result$yield_c, 2)
  expect_equal(result$yield_glo, 2)
  expect_equal(result$t_cbs, 40)
  expect_equal(result$prod_cbs_ratio, 0.5)
  expect_equal(result$prod_cbs_count, 2)
  expect_equal(result$sumprod_cbs_ratio, 0.5)
  expect_equal(result$source, "FAOSTAT_prod")
  expect_equal(result$Multi_type, "Primary")
})

test_that(".compute_cbs_ratios handles duplicate year rows without warning", {
  df <- tibble::tribble(
    ~year, ~area, ~area_code, ~item_prod, ~item_prod_code, ~item_cbs, ~item_cbs_code, ~live_anim, ~live_anim_code, ~unit, ~group, ~t, ~fu, ~yield_c, ~yield_glo, ~t_cbs, ~source, ~Multi_type,
    2010L, "Spain", 203L, "Wheat", 15L, "Wheat and products", 2511L, NA_character_, NA_character_, "t_ha", "Crop products", 8, 4, 2, 2, 20, "FAOSTAT_prod", NA_character_,
    2010L, "Spain", 203L, "Wheat", 15L, "Wheat and products", 2511L, NA_character_, NA_character_, "t_ha", "Crop products", 12, 6, 2, 2, 20, "FAOSTAT_prod", "Primary"
  )

  result <- expect_no_warning(
    whep:::.compute_cbs_ratios(df)
  )

  expect_equal(nrow(result), 1L)
  expect_equal(result$prod_cbs_count, 2)
})


# -- .fill_yields item join ----------------------------------------------------

test_that(".fill_yields joins items_prod by item_prod_code only", {
  items_prod <- tibble::tribble(
    ~item_prod_code, ~item_cbs_code, ~group,
    "15", 2511L, "Primary crops",
    "305", 2570L, "Primary crops"
  )

  yield_data <- tibble::tribble(
    ~year, ~area, ~area_code, ~item_prod, ~item_prod_code,
    ~live_anim_code, ~unit, ~t, ~fu, ~yield_c,
    ~source, ~Multi_type, ~live_anim, ~yield_glo,
    2000L, "Spain", 203L, "Wheat WRONG NAME", "15",
    NA_character_, "t_ha", 100, 50, 2,
    "FAOSTAT_prod", NA_character_, NA_character_, 2
  )

  result <- dplyr::left_join(
    yield_data,
    items_prod |>
      dplyr::select(item_prod_code, item_cbs_code, group) |>
      dplyr::distinct(item_prod_code, .keep_all = TRUE),
    by = "item_prod_code"
  )
  expect_equal(result$item_cbs_code, 2511L)
  expect_equal(result$group, "Primary crops")
})


# -- year range defaults -------------------------------------------------------

test_that("build_primary_production defaults to end_year 2023", {
  formals_prod <- formals(whep::build_primary_production)
  expect_equal(formals_prod$end_year, 2023)
})

test_that(".extend_historical keeps modern rows when LUH2 land columns are absent", {
  primary <- tibble::tibble(
    year = 2023L,
    area = "Spain",
    area_code = 203L,
    item_prod = "Wheat",
    item_prod_code = 15L,
    item_cbs = "Wheat and products",
    item_cbs_code = 2511L,
    live_anim = NA_character_,
    live_anim_code = NA_integer_,
    unit = "tonnes",
    value = 100,
    source = "FAOSTAT_prod"
  )
  years <- tibble::tibble(year = 2023L)
  land <- tibble::tibble(
    year = 2023L,
    area = "Spain",
    Land_Use = "urban",
    Area_Mha = 1
  )

  result <- whep:::.extend_historical(primary, years, land)

  expect_equal(nrow(result), 1L)
  expect_equal(result$year, 2023L)
  expect_equal(result$value, 100)
})

test_that(".extend_historical matches LUH2 land by area_code, not name", {
  # Production calls the country "Türkiye"; LUH2 calls it "Turkey".
  # Joining by name would drop it; joining by area_code keeps it and
  # back-casts the pre-1962 (NA) years from the cropland proxy.
  primary <- tibble::tibble(
    year = c(1959L, 1960L, 1961L),
    area = "Türkiye",
    area_code = 99L,
    item_prod = "Wheat",
    item_prod_code = 15L,
    item_cbs = "Wheat and products",
    item_cbs_code = 2511L,
    live_anim = NA_character_,
    live_anim_code = NA_integer_,
    unit = "tonnes",
    value = c(NA, NA, 100),
    source = "FAOSTAT_prod"
  )
  years <- tibble::tibble(year = c(1959L, 1960L, 1961L))
  land <- tibble::tibble(
    year = c(1959L, 1960L, 1961L),
    area = "Turkey",
    area_code = 99L,
    Land_Use = "c3ann",
    Area_Mha = c(8, 9, 10)
  )

  result <- whep:::.extend_historical(primary, years, land) |>
    dplyr::filter(area_code == 99L) |>
    dplyr::arrange(year)

  # All three years populated; none lost to the name mismatch.
  expect_equal(result$year, c(1959L, 1960L, 1961L))
  expect_false(anyNA(result$value))
  expect_equal(result$value[result$year == 1961L], 100)
  expect_true(all(result$value > 0))
})

test_that(".extend_historical takes a historical land table at the seam", {
  # whep#761: the pre-1962 area half of the back-cast was measured on
  # present-day borders. `land_wide` lets the historical producer hand the seam
  # the same shape measured on each year's own borders. Growth is 2x here
  # against the pin's 1.25x, so the back-cast value has to follow the table it
  # was given, and the `source` label has to say which one that was.
  primary <- tibble::tibble(
    year = c(1959L, 1960L, 1961L),
    area = "Ruritania",
    area_code = 99L,
    item_prod = "Wheat",
    item_prod_code = 15L,
    item_cbs = "Wheat and products",
    item_cbs_code = 2511L,
    live_anim = NA_character_,
    live_anim_code = NA_integer_,
    unit = "tonnes",
    value = c(NA, NA, 100),
    source = "FAOSTAT_prod"
  )
  years <- tibble::tibble(year = c(1959L, 1960L, 1961L))
  land <- tibble::tibble(
    year = c(1959L, 1960L, 1961L),
    area = "Ruritania",
    area_code = 99L,
    Land_Use = "c3ann",
    Area_Mha = c(8, 9, 10)
  )
  historical <- tibble::tibble(
    year = c(1959L, 1960L, 1961L),
    area_code = 99L,
    Cropland = c(2.5, 5, 10),
    Pasture = 0,
    agriland = c(2.5, 5, 10)
  )

  default <- whep:::.extend_historical(primary, years, land) |>
    dplyr::filter(area_code == 99L, unit == "tonnes") |>
    dplyr::arrange(year)
  historic <- whep:::.extend_historical(
    primary,
    years,
    land,
    land_wide = historical
  ) |>
    dplyr::filter(area_code == 99L, unit == "tonnes") |>
    dplyr::arrange(year)

  expect_equal(default$value, c(80, 90, 100))
  expect_equal(historic$value, c(25, 50, 100))
  expect_equal(default$source[1:2], rep("LUH2_cropland", 2))
  expect_equal(historic$source[1:2], rep("LUH2_polity_cropland", 2))
  # The reported year is the anchor and cannot move under either land table.
  expect_equal(default$value[3], historic$value[3])
  expect_equal(default$source[3], historic$source[3])
})

test_that(".historical_land_wide is NULL unless the method asks for it", {
  expect_null(whep:::.historical_land_wide("present_day", 1850:1961))
  # Nothing before 1962 is requested, so there is nothing to back-cast and the
  # expensive gridded read is skipped even under the historical method.
  expect_null(whep:::.historical_land_wide("historical_polity", 1990:2000))
})

test_that("a stale historical-land pin aborts instead of shortening the series", {
  # The pin covers the whole back-cast span by construction, so a missing year
  # means it is stale against the polities snapshot that produced it. Returning
  # the short series would silently drop those years from the back-cast, which
  # is the failure this abort exists to prevent. Offline: the reader is mocked,
  # never called for real.
  # `polity_code` is part of the pin's schema and is deliberately unlabelled
  # here, so the snapshot-drift guard beside this one stays quiet and the year
  # gap is the only thing under test.
  short <- tibble::tibble(
    year = 1850:1900,
    area_code = 238L,
    polity_code = NA_character_,
    Cropland = 1,
    Pasture = 1,
    agriland = 2
  )
  testthat::with_mocked_bindings(
    expect_error(
      whep:::.historical_land_wide("historical_polity", 1850:1961),
      "does not cover"
    ),
    .read_input = function(...) short,
    .package = "whep"
  )

  # ...and it does NOT abort when the pin covers everything asked for.
  testthat::with_mocked_bindings(
    expect_equal(
      nrow(whep:::.historical_land_wide("historical_polity", 1850:1900)),
      51L
    ),
    .read_input = function(...) short,
    .package = "whep"
  )
})

test_that("a pin built on another polities snapshot warns at the seam", {
  # A year gap is not how the pin actually goes stale: a re-synced snapshot
  # leaves every year and every bucket in place and changes the territory each
  # row was measured on. The guard has to reach that through
  # `.historical_land_wide()`, not only when called directly, and it must not
  # swallow the series while doing it (whep#905). Offline: the reader is
  # mocked and the check reads package data only.
  stale <- tibble::tibble(
    year = 1850:1851,
    area_code = 238L,
    polity_code = "ZZZ-1234-5678",
    Cropland = 1,
    Pasture = 1,
    agriland = 2
  )
  testthat::with_mocked_bindings(
    {
      expect_warning(
        out <- whep:::.historical_land_wide("historical_polity", 1850:1851),
        "polities"
      )
      expect_equal(nrow(out), 2L)
      expect_false("polity_code" %in% names(out))
    },
    .read_input = function(...) stale,
    .package = "whep"
  )
})

test_that(".extend_historical warns about areas with no LUH2 land match", {
  primary <- tibble::tibble(
    year = c(1960L, 1961L),
    area = "Atlantis",
    area_code = 999L,
    item_prod = "Wheat",
    item_prod_code = 15L,
    item_cbs = "Wheat and products",
    item_cbs_code = 2511L,
    live_anim = NA_character_,
    live_anim_code = NA_integer_,
    unit = "tonnes",
    value = c(NA, 100),
    source = "FAOSTAT_prod"
  )
  years <- tibble::tibble(year = c(1960L, 1961L))
  land <- tibble::tibble(
    year = c(1960L, 1961L),
    area = "Spain",
    area_code = 203L,
    Land_Use = "c3ann",
    Area_Mha = c(9, 10)
  )

  expect_warning(
    whep:::.extend_historical(primary, years, land),
    "no LUH2 land"
  )
})

test_that(".extend_historical warns about areas whose LUH2 land is zero", {
  # Nauru matches a LUH2 land row, but LUH2 at 0.5 degrees gives it no crop
  # or pasture fraction, so the proxy is zero in every pre-1962 year and
  # fill_proxy_growth() cannot back-cast it -- exactly as silently as an
  # unmatched area used to be (whep#548).
  primary <- tibble::tibble(
    year = c(1960L, 1961L),
    area = "Nauru",
    area_code = 148L,
    item_prod = "Coconuts",
    item_prod_code = 249L,
    item_cbs = "Coconuts - Incl Copra",
    item_cbs_code = 2560L,
    live_anim = NA_character_,
    live_anim_code = NA_integer_,
    unit = "tonnes",
    value = c(NA, 100),
    source = "FAOSTAT_prod"
  )
  years <- tibble::tibble(year = c(1960L, 1961L))
  land <- tibble::tribble(
    ~year, ~area, ~area_code, ~Land_Use, ~Area_Mha,
    1960L, "Nauru", 148L, "c3ann", 0,
    1961L, "Nauru", 148L, "c3ann", 0,
    1960L, "Nauru", 148L, "pastr", 0,
    1961L, "Nauru", 148L, "pastr", 0
  )

  expect_warning(
    result <- whep:::.extend_historical(primary, years, land),
    "zero in every pre-1962 year"
  )

  # The warning is a diagnostic: no land is invented, so 1960 stays empty.
  expect_true(is.na(result$value[result$year == 1960L]))
})

test_that(".extend_historical stays quiet when LUH2 land is non-zero", {
  primary <- tibble::tibble(
    year = c(1960L, 1961L),
    area = "Spain",
    area_code = 203L,
    item_prod = "Wheat",
    item_prod_code = 15L,
    item_cbs = "Wheat and products",
    item_cbs_code = 2511L,
    live_anim = NA_character_,
    live_anim_code = NA_integer_,
    unit = "tonnes",
    value = c(NA, 100),
    source = "FAOSTAT_prod"
  )
  years <- tibble::tibble(year = c(1960L, 1961L))
  land <- tibble::tribble(
    ~year, ~area, ~area_code, ~Land_Use, ~Area_Mha,
    1960L, "Spain", 203L, "c3ann", 9,
    1961L, "Spain", 203L, "c3ann", 10
  )

  expect_no_warning(whep:::.extend_historical(primary, years, land))
})

# -- Dissolved-federation LUH2 bridge (whep#408) -------------------------------

.make_csk_land <- function() {
  # LUH2 as it really is: Czechia and Slovakia have land, Czechoslovakia (area
  # 51) does not, because LUH2 is keyed on present-day ISO3.
  tibble::tribble(
    ~iso3c, ~area_code, ~area, ~year, ~Land_Use, ~Area_Mha,
    "CZE", 167L, "Czechia", 1960L, "c3ann", 2,
    "CZE", 167L, "Czechia", 1961L, "c3ann", 3,
    "SVK", 199L, "Slovakia", 1960L, "c3ann", 4,
    "SVK", 199L, "Slovakia", 1961L, "c3ann", 6
  )
}

.make_csk_production <- function() {
  tibble::tibble(
    year = c(1960L, 1961L),
    area = "Czechoslovakia",
    area_code = 51L,
    item_prod = "Wheat",
    item_prod_code = "15",
    item_cbs = "Wheat and products",
    item_cbs_code = 2511L,
    live_anim = NA_character_,
    live_anim_code = NA_character_,
    unit = "tonnes",
    value = c(NA, 100),
    source = "FAOSTAT_prod"
  )
}

test_that(".federation_land_bridge maps a federation to its successors' ISO3", {
  bridge <- whep:::.federation_land_bridge(
    data.table::as.data.table(.make_csk_land())
  )

  csk <- bridge[bridge$area_code == 51L, ]
  expect_equal(sort(csk$iso3c), c("CZE", "SVK"))
  expect_equal(unique(csk$area), "Czechoslovakia")
})

test_that(".add_federation_land_rows is a no-op by default", {
  land <- .make_csk_land()

  expect_identical(
    whep:::.add_federation_land_rows(land, federation_land = "none"),
    land
  )
})

test_that(".add_federation_land_rows sums successor LUH2 land", {
  aug <- whep:::.add_federation_land_rows(
    .make_csk_land(),
    federation_land = "successor_union"
  ) |>
    suppressMessages()

  csk <- aug |>
    dplyr::filter(.data$area_code == 51L) |>
    dplyr::arrange(.data$year)

  # 1960: CZE 2 + SVK 4 = 6 Mha; 1961: 3 + 6 = 9 Mha.
  expect_equal(csk$Area_Mha, c(6, 9))
  expect_true(all(is.na(csk$iso3c)))
  # Successor rows themselves are untouched, so nothing double-counts upstream.
  expect_equal(
    aug$Area_Mha[aug$area_code == 167L & aug$year == 1961L],
    3
  )
})

test_that(".add_federation_land_rows leaves its input untouched", {
  # `.build_grassland()` reads the same LUH2 table straight from
  # `.read_production()`. If the bridge mutated it by reference, every
  # federation would gain a 1850-2023 grassland row that double-counts against
  # its successors' own rows.
  land <- data.table::as.data.table(.make_csk_land())
  before <- nrow(land)

  invisible(suppressMessages(
    whep:::.add_federation_land_rows(land, federation_land = "successor_union")
  ))

  expect_equal(nrow(land), before)
  expect_false(51L %in% land$area_code)
})

test_that(".add_federation_land_rows warns without the ISO3 column", {
  expect_warning(
    whep:::.add_federation_land_rows(
      .make_csk_land() |> dplyr::select(-"iso3c"),
      federation_land = "successor_union"
    ),
    "iso3c"
  )
})

test_that(".extend_historical back-casts a federation only when asked", {
  # This is whep#408: with the default the 1960 row stays NA, because
  # Czechoslovakia has no LUH2 land of its own to grow the proxy from.
  primary <- .make_csk_production()
  years <- tibble::tibble(year = c(1960L, 1961L))
  land <- .make_csk_land()

  status_quo <- whep:::.extend_historical(primary, years, land) |>
    suppressWarnings() |>
    dplyr::filter(.data$area_code == 51L, .data$year == 1960L)

  expect_true(all(is.na(status_quo$value)))

  bridged <- whep:::.extend_historical(
    primary,
    years,
    land,
    federation_land = "successor_union"
  ) |>
    suppressMessages() |>
    dplyr::filter(.data$area_code == 51L) |>
    dplyr::arrange(.data$year)

  # Cropland proxy grows 6 -> 9 Mha, so 1960 tonnes = 100 * 6 / 9.
  expect_equal(bridged$value, c(100 * 6 / 9, 100))
  expect_equal(bridged$source[bridged$year == 1960L], "LUH2_cropland")
})

test_that(".extend_historical stops warning once a federation is bridged", {
  primary <- .make_csk_production()
  years <- tibble::tibble(year = c(1960L, 1961L))
  land <- .make_csk_land()

  expect_warning(
    whep:::.extend_historical(primary, years, land),
    "no LUH2 land"
  )
  expect_no_warning(
    whep:::.extend_historical(
      primary,
      years,
      land,
      federation_land = "successor_union"
    ) |>
      suppressMessages()
  )
})

test_that("build_primary_production rejects an unknown federation_land", {
  expect_error(
    build_primary_production(federation_land = "spatial_intersection"),
    class = "rlang_error"
  )
})

test_that(".prepare_historical_production normalizes generic historical rows", {
  historical <- tibble::tribble(
    ~year, ~area_code, ~item_prod_code, ~unit, ~value, ~source,
    1950L, 203L, "15.0", "tonnes", 100, "future_source",
    1950L, 203L, "15.0", "tonnes", 120, "historical_future_source",
    1800L, 203L, "15.0", "tonnes", 999, "future_source"
  )

  result <- whep:::.prepare_historical_production(
    historical,
    years = 1950:1951
  )

  expect_equal(nrow(result), 1L)
  expect_equal(result$year, 1950L)
  expect_equal(result$area, "Spain")
  expect_equal(result$item_prod, "Wheat")
  expect_equal(result$item_prod_code, "15")
  expect_equal(result$item_cbs_code, 2511L)
  expect_equal(result$unit, "tonnes")
  expect_equal(result$value, 110)
  expect_true(stringr::str_starts(result$source, "historical_"))
  # Prod-side item codes must be character to bind with the FAOSTAT pipeline
  # (primary_raw2). live_anim_code being integer broke build on real data.
  expect_type(result$item_prod_code, "character")
  expect_type(result$live_anim_code, "character")
})

test_that(".extend_historical uses historical rows as LUH2 anchors", {
  primary <- tibble::tibble(
    year = c(1950L, 1961L),
    area = "Spain",
    area_code = 203L,
    item_prod = "Wheat",
    item_prod_code = "15",
    item_cbs = "Wheat and products",
    item_cbs_code = 2511L,
    live_anim = NA_character_,
    live_anim_code = NA_integer_,
    unit = "tonnes",
    value = c(50, 100),
    source = c("historical_test", "FAOSTAT_prod")
  )
  years <- tibble::tibble(year = 1949:1961)
  land <- tibble::tibble(
    year = 1949:1961,
    area = "Spain",
    Land_Use = "c3ann",
    Area_Mha = 1
  )

  result <- whep:::.extend_historical(primary, years, land)

  observed <- result |>
    dplyr::filter(.data$year == 1950L, .data$unit == "tonnes")
  filled <- result |>
    dplyr::filter(.data$year == 1951L, .data$unit == "tonnes")

  expect_equal(observed$value, 50)
  expect_equal(observed$source, "historical_test")
  expect_equal(filled$source, "historical_LUH2_cropland")
  expect_false(is.na(filled$value))
})

test_that(".add_historical_yields preserves direct historical tonnes", {
  df <- tibble::tibble(
    year = 1950L,
    area = "Spain",
    area_code = 203L,
    item_prod = "Wheat",
    item_prod_code = "15",
    item_cbs = "Wheat and products",
    item_cbs_code = 2511L,
    land_use = "Cropland",
    live_anim = NA_character_,
    live_anim_code = NA_integer_,
    unit = c("tonnes", "ha"),
    value = c(50, 10),
    source = "historical_test"
  )
  int_yields <- tibble::tibble(
    year = 1950L,
    area_code = 203L,
    item_prod_code = "15",
    yield = 999
  )

  result <- whep:::.add_historical_yields(df, int_yields)

  expect_equal(result$tonnes, 50)
  expect_equal(result$t_ha, 5)
})

test_that(".add_historical_yields back-casts each area code separately", {
  # Two reporting areas under ONE `area` label. That is not hypothetical:
  # `.unfold_rest_of_world()` promotes a Rest-of-World member's
  # `polity_area_code` but leaves `polity_name`, so every promoted member
  # carries its own code and the shared label "Rest of World" (whep#589).
  # Grouping the pre-1962 `t_ha` back-cast on the label puts both series in
  # one group with two rows per year, and the growth rates come out of a lag
  # between two different countries.
  shared <- function(area_code, tonnes_1961, ha) {
    tibble::tribble(
      ~year, ~area, ~area_code, ~item_prod, ~item_prod_code, ~item_cbs, ~item_cbs_code, ~land_use, ~live_anim, ~live_anim_code, ~unit, ~value, ~source,
      1961L, "Rest of World", area_code, "Wheat", "15", "Wheat", 2511L, "cropland", NA_character_, NA_character_, "tonnes", tonnes_1961, "FAOSTAT_prod",
      1961L, "Rest of World", area_code, "Wheat", "15", "Wheat", 2511L, "cropland", NA_character_, NA_character_, "ha", ha, "FAOSTAT_prod",
      1960L, "Rest of World", area_code, "Wheat", "15", "Wheat", 2511L, "cropland", NA_character_, NA_character_, "tonnes", 0, "FAOSTAT_prod",
      1960L, "Rest of World", area_code, "Wheat", "15", "Wheat", 2511L, "cropland", NA_character_, NA_character_, "ha", ha, "FAOSTAT_prod"
    )
  }
  df <- dplyr::bind_rows(shared(700L, 6000, 100), shared(701L, 900, 30))
  int_yields <- tibble::tribble(
    ~year, ~area_code, ~item_prod_code, ~yield,
    1960L, 700L, "15", 2.0,
    1961L, 700L, "15", 2.5,
    1960L, 701L, "15", 1.0,
    1961L, 701L, "15", 4.0
  )

  result <- whep:::.add_historical_yields(df, int_yields) |>
    tibble::as_tibble() |>
    dplyr::filter(.data$year == 1960L) |>
    dplyr::arrange(.data$area_code)

  # Each area's own yield proxy: 100 * (60 * 2.0/2.5) and 30 * (30 * 1.0/4.0).
  expect_equal(result$tonnes, c(4800, 225))
  expect_equal(result$t_ha, c(48, 7.5))
})


# -- LUH2 land buckets ---------------------------------------------------------

test_that(".read_land_areas gives a bucket one area label", {
  # FAOSTAT bucket 206 sums two territories LUH2 reports separately, SDN and
  # SSD. The area bridge used to pair the bucket's code with each MEMBER's
  # name, so the bucket arrived at `.build_grassland()` as two rows sharing
  # `area_code` 206 -- and `.dedup_production()`, which keys on
  # (year, area_code, item_prod_code, unit) to choose between competing
  # sources, kept one and discarded the other territory's pasture.
  local_mocked_bindings(
    .read_input = function(name, years = NULL, year_col = NULL, ...) {
      data.table::as.data.table(
        tibble::tribble(
          ~ISO3, ~Year, ~Land_Use, ~Area_Mha, ~C_stock_Tg,
          "SDN", 2000L, "pastr", 20, 200,
          "SSD", 2000L, "pastr", 5, 50
        )
      )
    }
  )

  land <- whep:::.read_land_areas(years = 2000L)

  expect_equal(sort(unique(land$area_code)), 206L)
  expect_length(unique(land$area), 1L)
  expect_equal(unique(land$area), "Sudan (former)")

  grass <- whep:::.build_grassland(land)

  # One row per (year, area_code, item_prod_code), carrying the SUM. A second
  # row here is land that `.dedup_production()` would silently drop.
  expect_equal(nrow(grass), 1L)
  expect_equal(grass$value, 25e6)
})


# -- rice unit convention ------------------------------------------------------

test_that(".fix_rice_milled_equiv converts paddy production only", {
  rate <- whep:::.rice_milled_extraction_rate()
  df <- tibble::tribble(
    ~year, ~area, ~area_code, ~item_prod, ~item_prod_code,
    ~item_cbs, ~item_cbs_code, ~live_anim, ~live_anim_code,
    ~unit, ~value, ~source,
    2000L, "China", 41L, "Rice", "27",
    "Rice and products", 2807L, NA, NA,
    "tonnes", 100, "FAOSTAT_prod",
    2000L, "China", 41L, "Rice", "27",
    "Rice and products", 2807L, NA, NA,
    "t_ha", 10, "imputed_yield:Global",
    2000L, "China", 41L, "Rice", "27",
    "Rice and products", 2807L, NA, NA,
    "ha", 20, "FAOSTAT_prod",
    2000L, "China", 41L, "Rice", "27",
    "Rice and products", 2807L, NA, NA,
    "tonnes", 80, "imputed_cbs_ratio",
    2000L, "China", 41L, "Rice", "27",
    "Rice and products", 2807L, NA, NA,
    "tonnes", 200, "historical_mitchell",
    2000L, "China", 41L, "Wheat", "15",
    "Wheat and products", 2511L, NA, NA,
    "tonnes", 50, "FAOSTAT_prod"
  )

  result <- whep:::.fix_rice_milled_equiv(df) |>
    dplyr::arrange(.data$item_prod_code, .data$unit, .data$source)

  rice <- result |>
    dplyr::filter(.data$item_prod_code == "27")

  testthat::expect_equal(
    rice$value[rice$unit == "tonnes" & rice$source == "FAOSTAT_prod"],
    100 * rate
  )
  testthat::expect_equal(
    rice$value[rice$unit == "t_ha"],
    10 * rate
  )
  testthat::expect_equal(
    rice$value[rice$unit == "ha"],
    20
  )
  testthat::expect_equal(
    rice$value[rice$source == "imputed_cbs_ratio"],
    80
  )
  # observed historical rice is paddy too and must be milled-converted
  testthat::expect_equal(
    rice$value[rice$source == "historical_mitchell"],
    200 * rate
  )
  testthat::expect_equal(
    result$value[result$item_prod_code == "15"],
    50
  )
})


# -- deduplication --------------------------------------------------------------

test_that(".dedup_production keeps highest-priority source", {
  duped <- tibble::tribble(
    ~year, ~area, ~area_code, ~item_prod, ~item_prod_code,
    ~item_cbs, ~item_cbs_code, ~live_anim, ~live_anim_code,
    ~unit, ~value, ~source,
    2000L, "Spain", 203L, "Wheat", 15L,
    "Wheat", 2511L, NA, NA, "t", 100, "imputed_yield",
    2000L, "Spain", 203L, "Wheat", 15L,
    "Wheat", 2511L, NA, NA, "t", 200, "FAOSTAT_prod",
    2000L, "Spain", 203L, "Wheat", 15L,
    "Wheat", 2511L, NA, NA, "ha", 10, "LUH2_cropland",
    2000L, "Spain", 203L, "Wheat", 15L,
    "Wheat", 2511L, NA, NA, "ha", 20, "EuropeAgriDB"
  )

  result <- whep:::.dedup_production(duped)
  expect_equal(nrow(result), 2L)

  tonnes_row <- result |>
    dplyr::filter(unit == "t")
  expect_equal(tonnes_row$source, "FAOSTAT_prod")
  expect_equal(tonnes_row$value, 200)

  ha_row <- result |>
    dplyr::filter(unit == "ha")
  expect_equal(ha_row$source, "EuropeAgriDB")
  expect_equal(ha_row$value, 20)
})

test_that(".dedup_production warns when a key repeats one source", {
  # Shape of whep#633: FAOSTAT bucket 206 arrived as two rows, one per
  # territory, under one `area_code`. They are addends, not competing
  # measurements, and dedup keeps one -- silently, until whep#650.
  addends <- tibble::tribble(
    ~year, ~area, ~area_code, ~item_prod, ~item_prod_code,
    ~unit, ~value, ~source,
    2000L, "Sudan (former)", 206L, "Grassland", 3001L,
    "ha", 20e6, "LUH2_grassland",
    2000L, "Sudan (former)", 206L, "Grassland", 3001L,
    "ha", 5e6, "LUH2_grassland"
  )

  expect_warning(
    result <- whep:::.dedup_production(addends),
    "same"
  )
  # Arbitration itself is unchanged: one row survives, nothing is summed.
  expect_equal(nrow(result), 1L)

  # The report names the key and the mass dedup discards.
  collided <- whep:::.same_source_collisions(
    data.table::as.data.table(addends),
    c("year", "area_code", "item_prod_code", "unit")
  )
  expect_equal(nrow(collided), 1L)
  expect_equal(collided$rows, 2L)
  expect_equal(collided$dropped, 5e6)
})

test_that(".dedup_production is silent for competing sources", {
  competing <- tibble::tribble(
    ~year, ~area, ~area_code, ~item_prod, ~item_prod_code,
    ~unit, ~value, ~source,
    2000L, "Spain", 203L, "Wheat", 15L, "t", 100, "imputed_yield",
    2000L, "Spain", 203L, "Wheat", 15L, "t", 200, "FAOSTAT_prod"
  )

  expect_no_warning(whep:::.dedup_production(competing))
  # Empty input must not warn either.
  expect_no_warning(whep:::.dedup_production(competing[0L, ]))
})

test_that(".dedup_production does not flag an aggregate and its members", {
  # FAOSTAT reports China both as aggregate 351 and as components 41/96/128/214
  # (the double-count of whep's harmonization notes). Those are distinct
  # `area_code`s, so they never collide on one dedup key and must not be
  # reported as same-source duplicates -- and dedup must keep all five rows.
  china <- tibble::tribble(
    ~year, ~area, ~area_code, ~item_prod, ~item_prod_code,
    ~unit, ~value, ~source,
    2000L, "China", 351L, "Wheat", 15L, "t", 100, "FAOSTAT_prod",
    2000L, "China, mainland", 41L, "Wheat", 15L, "t", 90, "FAOSTAT_prod",
    2000L, "China, Taiwan", 214L, "Wheat", 15L, "t", 5, "FAOSTAT_prod",
    2000L, "China, Hong Kong", 96L, "Wheat", 15L, "t", 3, "FAOSTAT_prod",
    2000L, "China, Macao", 128L, "Wheat", 15L, "t", 2, "FAOSTAT_prod"
  )

  expect_no_warning(result <- whep:::.dedup_production(china))
  expect_equal(nrow(result), 5L)
  expect_equal(sum(result$value), 200)
})

test_that("whep.warn_prod_dupes = FALSE silences the duplicate report", {
  addends <- tibble::tribble(
    ~year, ~area, ~area_code, ~item_prod, ~item_prod_code,
    ~unit, ~value, ~source,
    2000L, "Sudan (former)", 206L, "Grassland", 3001L,
    "ha", 20e6, "LUH2_grassland",
    2000L, "Sudan (former)", 206L, "Grassland", 3001L,
    "ha", 5e6, "LUH2_grassland"
  )

  withr::with_options(
    list(whep.warn_prod_dupes = FALSE),
    expect_no_warning(whep:::.dedup_production(addends))
  )
})

test_that(".show_prod_duplicates flags a repeated source", {
  addends <- tibble::tribble(
    ~year, ~area, ~area_code, ~item_prod, ~item_prod_code,
    ~unit, ~value, ~source,
    2000L, "Sudan (former)", 206L, "Grassland", 3001L,
    "ha", 20e6, "LUH2_grassland",
    2000L, "Sudan (former)", 206L, "Grassland", 3001L,
    "ha", 5e6, "LUH2_grassland"
  )

  msgs <- testthat::capture_messages(
    suppressWarnings(whep:::.show_prod_duplicates(addends))
  )
  expect_true(any(stringr::str_detect(msgs, "repeat a single")))
})

test_that(".show_prod_duplicates returns wide format of competing sources", {
  duped <- tibble::tribble(
    ~year, ~area, ~area_code, ~item_prod, ~item_prod_code,
    ~item_cbs, ~item_cbs_code, ~live_anim, ~live_anim_code,
    ~unit, ~value, ~source,
    2000L, "Spain", 203L, "Wheat", 15L,
    "Wheat", 2511L, NA, NA, "t", 100, "imputed_yield",
    2000L, "Spain", 203L, "Wheat", 15L,
    "Wheat", 2511L, NA, NA, "t", 200, "FAOSTAT_prod",
    2000L, "Spain", 203L, "Maize", 56L,
    "Maize", 2514L, NA, NA, "t", 50, "FAOSTAT_prod"
  )

  result <- whep:::.show_prod_duplicates(duped)
  # Only the duplicated key (Wheat/t) should appear
  expect_equal(nrow(result), 1L)
  # Columns should include the two competing sources
  expect_true("FAOSTAT_prod" %in% names(result))
  expect_true("imputed_yield" %in% names(result))
  # FAOSTAT_prod column first because it has higher priority
  src_cols <- setdiff(
    names(result),
    c("year", "area_code", "item_prod_code", "unit")
  )
  expect_equal(src_cols[1], "FAOSTAT_prod")
})

test_that("QC flags reflect post-dedup values, not competing sources", {
  # Two sources disagree by >10x on 2001; FAOSTAT_prod is a smooth series.
  competing <- tibble::tribble(
    ~year, ~area, ~area_code, ~item_prod, ~item_prod_code, ~unit, ~value, ~source,
    2000L, "Spain", 203L, "Wheat", 15L, "tonnes", 5000, "FAOSTAT_prod",
    2001L, "Spain", 203L, "Wheat", 15L, "tonnes", 5500, "FAOSTAT_prod",
    2002L, "Spain", 203L, "Wheat", 15L, "tonnes", 6000, "FAOSTAT_prod",
    2001L, "Spain", 203L, "Wheat", 15L, "tonnes", 100000, "imputed_yield"
  )

  # QC after dedup: the competing imputed_yield value for 2001 is discarded,
  # leaving a smooth single-source series with no spike.
  qc_after <- competing |>
    whep:::.dedup_production() |>
    whep:::.qc_production()
  expect_false(
    any(stringr::str_detect(qc_after$qc_flag, "spike"), na.rm = TRUE)
  )

  # Regression guard: QC before dedup compares the two 2001 sources and
  # raises a spurious spike flag.
  qc_before <- whep:::.qc_production(competing)
  expect_true(
    any(stringr::str_detect(qc_before$qc_flag, "spike"), na.rm = TRUE)
  )
})

test_that("build_primary_production output has no duplicate keys", {
  result <- whep::build_primary_production(example = TRUE)
  keys <- dplyr::select(
    result,
    year,
    area_code,
    item_prod_code,
    unit
  )
  expect_equal(nrow(keys), nrow(dplyr::distinct(keys)))
})

# -- .split_stock_share ---------------------------------------------------------

test_that(".split_stock_share splits proportionally when both sub-items have data", {
  data <- tibble::tribble(
    ~year, ~area_code, ~item_prod_code, ~value, ~value_st,
    2000, 1L, "866", 100, 30,
    2000, 1L, "866", 100, 70
  )

  result <- .split_stock_share(data)

  # value is the same unsplit QCL total repeated on every row of the group
  # (from the earlier inner_join fan-out), so the group's real total is 100,
  # not sum(data$value).
  expect_equal(result$value_comb, c(30, 70))
  expect_equal(sum(result$value_comb), 100)
})

test_that(".split_stock_share does not double-count when one sub-item is entirely NA", {
  # Regression for #144: previously sum(value_st) had no na.rm, so a single
  # NA sub-item made the whole group's share NA, and BOTH sub-rows fell back
  # to the full unsplit `value` -- doubling the country's herd count.
  data <- tibble::tribble(
    ~year, ~area_code, ~item_prod_code, ~value, ~value_st,
    2000, 1L, "866", 100, NA_real_,
    2000, 1L, "866", 100, 40
  )

  result <- .split_stock_share(data)

  # The NA sub-item gets 0 (no data to justify any share); the sub-item with
  # data absorbs the rest. Total heads must equal the original unsplit value.
  expect_equal(result$value_comb, c(0, 100))
  expect_equal(sum(result$value_comb), 100)
})

test_that(".split_stock_share splits equally when every sub-item is NA", {
  # If no sub-item has any data at all, an equal split (rather than giving
  # every sub-item the full total) is the only way to keep the total heads
  # conserved without an arbitrary preference for one sub-item.
  data <- tibble::tribble(
    ~year, ~area_code, ~item_prod_code, ~value, ~value_st,
    2000, 1L, "866", 90, NA_real_,
    2000, 1L, "866", 90, NA_real_,
    2000, 1L, "866", 90, NA_real_
  )

  result <- .split_stock_share(data)

  expect_equal(result$value_comb, rep(30, 3))
  expect_equal(sum(result$value_comb), 90)
})

test_that(".split_stock_share keeps the full value for a single-item group", {
  # A parent item_prod_code with only one mapped sub-item (no real dairy/
  # non-dairy-style split) should always receive the whole unsplit value,
  # whether or not it happens to have its own value_st.
  with_data <- tibble::tribble(
    ~year, ~area_code, ~item_prod_code, ~value, ~value_st,
    2000, 1L, "976", 50, 12
  )
  without_data <- tibble::tribble(
    ~year, ~area_code, ~item_prod_code, ~value, ~value_st,
    2000, 1L, "976", 50, NA_real_
  )

  expect_equal(.split_stock_share(with_data)$value_comb, 50)
  expect_equal(.split_stock_share(without_data)$value_comb, 50)
})

test_that(".split_stock_share keeps groups (year, area_code, item_prod_code) independent", {
  data <- tibble::tribble(
    ~year, ~area_code, ~item_prod_code, ~value, ~value_st,
    2000, 1L, "866", 100, NA_real_,
    2000, 1L, "866", 100, 60,
    2000, 2L, "866", 200, 10,
    2000, 2L, "866", 200, 30
  )

  result <- .split_stock_share(data)

  expect_equal(result$value_comb, c(0, 100, 50, 150))
})

test_that(".carry_forward_shares extends shares to QCL's latest years", {
  # Shares from the emissions pin lag QCL by 1-2 years: here they stop at
  # 2021 while slaughter data (target_years) runs to 2023.
  # `area` carries the reporting territory: one `area_code` can hold two of
  # them, so it is part of the share key. See
  # test_stock_share_territory_key.R.
  shares <- tibble::tribble(
    ~year, ~area_code, ~area, ~Item_Code, ~item_cbs_code, ~share,
    2020L, 203L, "Spain", 866L, 867L, 0.4,
    2020L, 203L, "Spain", 866L, 868L, 0.6,
    2021L, 203L, "Spain", 866L, 867L, 0.3,
    2021L, 203L, "Spain", 866L, 868L, 0.7
  )
  target_years <- 2020:2023

  result <- whep:::.carry_forward_shares(shares, target_years)

  # Every target year is covered for both split sub-items.
  expect_equal(sort(unique(result$year)), 2020:2023)
  expect_equal(nrow(result), 8L)

  # Latest known share (2021) is carried forward to 2022 and 2023.
  latest <- result |>
    dplyr::filter(year %in% c(2022L, 2023L)) |>
    dplyr::arrange(year, item_cbs_code)
  expect_equal(latest$share, c(0.3, 0.7, 0.3, 0.7))

  # Shares still sum to 1 within each (year, area_code, Item_Code).
  sums <- result |>
    dplyr::summarise(
      total = sum(share),
      .by = c(year, area_code, Item_Code)
    )
  expect_equal(sums$total, rep(1, 4))
})

test_that(".read_land_areas separates LUH2's own sentinel from real territories", {
  # The old single warning said "LUH2 ISO3 codes not found in
  # polity_area_crosswalk, dropping: BLM, ALA, -99, SXM, JEY, GGY, and IMN",
  # which reported two unrelated facts as one and was wrong about six of the
  # seven. Measured over the whole pin, 1850-2022:
  #
  #   -99, LUH2's own unassigned marker   8,620 Mha   0.358% of all LUH2 area
  #   the six real territories                19 Mha   0.0008%
  #
  # So 459 parts in 460 of what looked like lost coverage is land the source
  # itself attributes to no country. The six -- Jersey, Guernsey, Isle of Man,
  # Saint-Barthelemy, Aland, Sint Maarten -- ARE in the crosswalk under their
  # sovereign's polity; what they lack is a FAOSTAT area code. Both halves stay
  # dropped, which is whep#407's question, not this one's.
  local_mocked_bindings(
    .read_input = function(pin_alias, years = NULL, year_col = NULL) {
      data.table::data.table(
        ISO3 = c("ESP", "-99", "JEY"),
        Year = 2000L,
        Land_Use = "c3ann",
        Area_Mha = c(10, 8620, 0.01)
      )
    }
  )

  warn <- suppressMessages(
    tryCatch(whep:::.read_land_areas(years = 2000L), warning = function(w) w)
  )
  expect_s3_class(warn, "condition")
  expect_match(conditionMessage(warn), "no FAOSTAT area code")
  expect_match(conditionMessage(warn), "JEY")
  # The sentinel must NOT appear in the warning: it is not a coverage gap.
  expect_false(grepl("-99", conditionMessage(warn), fixed = TRUE))

  msgs <- suppressWarnings(
    testthat::capture_messages(whep:::.read_land_areas(years = 2000L))
  )
  expect_true(any(grepl("-99", msgs, fixed = TRUE)))
  expect_true(any(grepl("no country assignment", msgs)))
})

test_that(".fodder_crop_liv reuses a table that already spans the fodder years", {
  # The fodder chain interpolates along the year axis, so it needs yield_dm at
  # every year the fodder sources cover (#623). A full-range build already holds
  # those years, and must not pay for a second read.
  i_fodder <- tibble::tribble(
    ~year, ~value,
    1961L, 1,
    2013L, 2
  )
  spanning <- tibble::tribble(
    ~year, ~unit, ~value,
    1961L, "ha", 10,
    2020L, "ha", 20
  )

  expect_identical(
    whep:::.fodder_crop_liv(spanning, i_fodder),
    spanning
  )
})

test_that(".fodder_crop_liv ignores NA years when comparing spans", {
  i_fodder <- tibble::tribble(
    ~year, ~value,
    1961L, 1,
    NA_integer_, 2,
    2013L, 3
  )
  spanning <- tibble::tribble(
    ~year, ~unit, ~value,
    1960L, "ha", 10,
    NA_integer_, "ha", 15,
    2014L, "ha", 20
  )

  expect_identical(
    whep:::.fodder_crop_liv(spanning, i_fodder),
    spanning
  )
})

test_that(".split_stock_share keys on the code, so a shared label cannot dilute", {
  # THE #589 REGRESSION, in one fixture.
  #
  # `.unfold_rest_of_world()` promotes `polity_area_code` but leaves
  # `polity_code`/`polity_name` alone, so every promoted Rest-of-World member
  # comes out of `.aggregate_to_polities()` with its own `area_code` and the
  # SHARED label "Rest of World". Grouped by `area`, all 13 reporting members
  # landed in one group, `sum(value_st)` summed across all of them, and each
  # member's share collapsed to roughly 1/13 of its own stock.
  #
  # Measured before this fix: Syria's 2000 livestock came to 3,408,857 head
  # against 38,048,415 after, and the published values carried fractional
  # animals (1227745.45) -- the signature of a share that should have been 1.
  # `slaughtered_heads` was unaffected throughout, because it never passes
  # through this splitter, which is what made the defect look like a unit bug.
  #
  # Two areas, same label, one parent item: if the grouping keys on the label
  # each gets half its own value; on the code each keeps all of it.
  data <- tibble::tribble(
    ~year, ~area_code, ~area, ~item_prod_code, ~value, ~value_st,
    2000, 212L, "Rest of World", "976", 100, 40,
    2000, 64L, "Rest of World", "976", 60, 60
  )

  result <- .split_stock_share(data)

  expect_equal(result$value_comb, c(100, 60))
  expect_equal(sum(result$value_comb), 160)
  # Whole numbers: a single-member group has share 1, never 1/n.
  expect_equal(result$value_comb, round(result$value_comb))
})

test_that(".assemble_production_raw renames the live-animal units", {
  # The livestock branch turns the yield units into the head/LU counts the
  # published table carries. Pinned because whep#850 rewrote the rename off
  # the deprecated dplyr::case_match(): a wrong label here would put animal
  # counts under a mass unit.
  yield_all <- tibble::tribble(
    ~year, ~area, ~area_code, ~item_prod, ~item_prod_code, ~live_anim,
    ~live_anim_code, ~unit, ~source, ~fu2, ~t2, ~yield,
    # Duplicate key pair the summarise() averages: 4 and 6 -> 5.
    2010L, "Spain", 203L, "Milk", "951", "Cattle", "866", "t_LU", "FAO",
    4, 8, 2,
    2010L, "Spain", 203L, "Milk", "951", "Cattle", "866", "t_LU", "FAO",
    6, 12, 2,
    2010L, "Spain", 203L, "Eggs", "1062", "Hens", "1057", "t_head", "FAO",
    3, 9, 3,
    # A crop row the livestock filter must not pick up.
    2010L, "Spain", 203L, "Wheat", "15", NA, NA, "t_ha", "FAO", 10, 20, 2
  )

  result <- suppressMessages(.assemble_production_raw(yield_all))
  live <- result |>
    dplyr::filter(unit %in% c("LU", "heads"))

  expect_setequal(live$unit, c("LU", "heads"))
  expect_equal(live$value[live$unit == "LU"], 5)
  expect_equal(live$value[live$unit == "heads"], 3)
  # Exactly one count row per (area, unit) pair present, and the rename is
  # confined to that branch: the yield rows keep their own t_ units.
  expect_equal(nrow(live), 2L)
  expect_setequal(
    result$unit,
    c("ha", "tonnes", "t_ha", "t_LU", "t_head", "LU", "heads")
  )
})


# -- calculate_raw_yields source provenance ------------------------------------

test_that(".calculate_raw_yields keeps a reconstructed fodder source (#937)", {
  # Temporary grassland (production item 996, CBS 3002) is in neither FAOSTAT
  # production pin: its hectares come from EU AgriDB and its tonnage from the
  # EU AgriDB nitrogen yield or a dry-matter estimate. The yield dcast used to
  # drop `source`, and `.impute_missing_values()` then re-derived it from the
  # presence of a tonnage, so every such row read as `"FAOSTAT_prod"`.
  primary_raw <- tibble::tribble(
    ~year, ~area, ~area_code, ~item_prod, ~item_prod_code, ~unit, ~value,
    2019L, "Spain", 203L, "Temporary grassland", "996", "ha", 287690,
    2019L, "Spain", 203L, "Temporary grassland", "996", "t", 4e6,
    2019L, "Spain", 203L, "Wheat", "15", "ha", 1e6,
    2019L, "Spain", 203L, "Wheat", "15", "t", 5e6
  ) |>
    dplyr::mutate(
      source = c("EuropeAgriDB", "EuropeAgriDB", "FAOSTAT_prod", "FAOSTAT_prod")
    )

  result <- whep:::.calculate_raw_yields(primary_raw, whep::items_prod_full)

  expect_true("source" %in% names(result))
  # `tidyr::complete()` at the end of the helper also emits an empty carcass
  # row per key, so the rows carrying the hectares are the ones to read.
  expect_equal(
    result |>
      dplyr::filter(item_prod_code == "996", unit == "t_ha", !is.na(fu)) |>
      dplyr::pull(source),
    "EuropeAgriDB"
  )
  expect_equal(
    result |>
      dplyr::filter(item_prod_code == "15", unit == "t_ha", !is.na(fu)) |>
      dplyr::pull(source),
    "FAOSTAT_prod"
  )
})

test_that(".deduplicate_doubles keeps the original of a double key (#937)", {
  # Both copies of a double-product key carry a source now that the yield table
  # keeps one, so the copy to drop is named by `.double_combined`, not inferred
  # from a missing source. Inferring it dropped item 328 (seed cotton), 254 (oil
  # palm fruit) and 310 outright -- 9114 rows of a 2001-2023 build.
  df <- tibble::tribble(
    ~year, ~area, ~area_code, ~item_prod, ~item_prod_code, ~unit, ~t,
    2019L, "Egypt", 59L, "Seed cotton", "328", "t_ha", 5e5,
    2019L, "Egypt", 59L, "Seed cotton", "328", "t_ha", 5e5
  ) |>
    dplyr::mutate(
      source = c("FAOSTAT_prod", "Estimated"),
      .double_combined = c(FALSE, TRUE)
    )

  result <- whep:::.deduplicate_doubles(df)

  expect_equal(nrow(result), 1L)
  expect_equal(result$source, "FAOSTAT_prod")
})

test_that(".best_source_by_key ranks a key's competing sources (#937)", {
  # One key's hectares and tonnage can come from different sources; the better
  # ranked one wins, the same arbitration `.dedup_production()` applies.
  crop_dt <- data.table::data.table(
    year = 2019L,
    area = "Spain",
    area_code = 203L,
    item_prod = "Temporary grassland",
    item_prod_code = "996",
    unit = c("ha", "t"),
    value = c(287690, 4e6),
    source = c("EuropeAgriDB", "DM_yield_estimate")
  )

  expect_equal(whep:::.best_source_by_key(crop_dt)$source, "EuropeAgriDB")

  crop_dt[, source := NA_character_]
  expect_equal(nrow(whep:::.best_source_by_key(crop_dt)), 0L)
})
