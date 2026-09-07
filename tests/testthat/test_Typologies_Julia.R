# test_Typologies_Julia.R — tests for R/Typologies_Julia.R
#
# Every helper here takes its inputs as arguments, so the whole decision chain
# (livestock units -> density -> feed shares -> decision tree) is reachable
# offline from tribble fixtures. No pin read, no shapefile download.

# Flows in the schema `create_n_prov_destiny()` emits, i.e. the input
# `.grafs_prod_destiny_legacy()` translates. Kept narrow so the interesting
# columns fit on one line.
julia_prov_destiny_fixture <- function() {
  tibble::tribble(
    ~item,       ~box,                          ~origin,      ~destiny,
    "Wheat",     "Cropland",                    "Cropland",   "population_food",
    "Wheat",     "Cropland",                    "Cropland",   "population_food",
    "Barley",    "Cropland",                    "Cropland",   "livestock_rum",
    "Barley",    "Cropland",                    "Cropland",   "livestock_mono",
    "Grassland", "semi_natural_agroecosystems", "semi_nat",   "livestock_rum",
    "Soy",       "Cropland",                    "Outside",    "livestock_mono",
    "Olives",    "Cropland",                    "Cropland",   "export",
    "Wheat",     "Cropland",                    "Synthetic",  "Cropland"
  ) |>
    dplyr::mutate(
      mg_n = c(10, 5, 7, 3, 4, 6, 8, 99),
      year = 2000,
      province_name = "Lugo",
      irrig_cat = "rainfed"
    )
}

# The legacy-vocabulary view the aggregation helpers consume.
julia_legacy_feed_fixture <- function() {
  tibble::tribble(
    ~Province_name, ~Box,                          ~Destiny, ~MgN,
    "Lugo",         "Semi_natural_agroecosystems", "Feed",   30,
    "Lugo",         "Cropland",                    "Feed",   10,
    "Lugo",         "Cropland",                    "Food",   77,
    "Lugo",         "Agro-industry",               "Feed",   5,
    "Soria",        "Cropland",                    "Feed",   40
  ) |>
    dplyr::mutate(Year = 2000)
}

# .prepare_lu_coefs ------------------------------------------------------------

test_that(".prepare_lu_coefs keeps one row per livestock category", {
  livestock_units <- tibble::tribble(
    ~Livestock_cat,  ~Animal_class, ~LU_head, ~Region,
    "Dairy_cattle",  "Cattle",      1.0,      "north",
    "Dairy_cattle",  "Cattle",      1.0,      "south",
    "Sheep",         "Small_rum",   0.1,      "north"
  )

  out <- whep:::.prepare_lu_coefs(livestock_units)

  # Only the three coefficient columns survive, so the duplicated pair of
  # Dairy_cattle rows collapses instead of fanning the later join out.
  expect_equal(names(out), c("Livestock_cat", "Animal_class", "LU_head"))
  expect_equal(nrow(out), 2)
  expect_equal(
    out |> dplyr::filter(Livestock_cat == "Sheep") |> dplyr::pull(LU_head),
    0.1
  )
})

# .calculate_lu_totals --------------------------------------------------------

test_that(".calculate_lu_totals multiplies heads by the LU coefficient", {
  livestock_df <- tibble::tribble(
    ~Year, ~Province_name, ~Livestock_cat,  ~Stock_Number, ~Source,
    2000,  "Lugo",         "Dairy_cattle",  100,           "census",
    2000,  "Lugo",         "Sheep",         200,           "census",
    2000,  "Lugo",         "Horses",        10,            "census"
  )
  lu_coefs_df <- tibble::tribble(
    ~Livestock_cat,  ~Animal_class, ~LU_head,
    "Dairy_cattle",  "Cattle",      1.0,
    "Sheep",         "Small_rum",   0.1
  )

  out <- whep:::.calculate_lu_totals(livestock_df, lu_coefs_df)

  expect_equal(
    out |>
      dplyr::filter(Livestock_cat == "Dairy_cattle") |>
      dplyr::pull(LU_total),
    100
  )
  expect_equal(
    out |> dplyr::filter(Livestock_cat == "Sheep") |> dplyr::pull(LU_total),
    20
  )
  # A category with no coefficient keeps its heads but gets no LU. It is left
  # as NA rather than dropped, so the row stays visible downstream.
  expect_true(
    out |>
      dplyr::filter(Livestock_cat == "Horses") |>
      dplyr::pull(LU_total) |>
      is.na()
  )
  expect_false("Source" %in% names(out))
})

# .aggregate_lu_totals / .aggregate_area_aa -----------------------------------

test_that(".aggregate_lu_totals sums categories and sorts by year-province", {
  lu_detailed <- tibble::tribble(
    ~Year, ~Province_name, ~LU_total,
    2001,  "Soria",        5,
    2000,  "Lugo",         100,
    2000,  "Lugo",         20,
    2000,  "Soria",        NA
  )

  out <- whep:::.aggregate_lu_totals(lu_detailed)

  expect_equal(out$Year, c(2000, 2000, 2001))
  expect_equal(out$Province_name, c("Lugo", "Soria", "Soria"))
  # NA-only groups sum to 0 because na.rm = TRUE, not to NA.
  expect_equal(out$LU_total, c(120, 0, 5))
})

test_that(".aggregate_area_aa sums every land use in the province", {
  npp_df <- tibble::tribble(
    ~Year, ~Province_name, ~LandUse,            ~Area_ygpit_ha,
    2000,  "Lugo",         "Cropland",          200,
    2000,  "Lugo",         "Pasture_Shrubland", 300,
    2000,  "Soria",        "Cropland",          50
  )

  out <- whep:::.aggregate_area_aa(npp_df)

  expect_equal(
    out |> dplyr::filter(Province_name == "Lugo") |> dplyr::pull(Area_ha),
    500
  )
  expect_equal(nrow(out), 2)
})

# .calculate_livestock_density ------------------------------------------------

test_that(".calculate_livestock_density divides LU by agricultural area", {
  lu_totals_df <- tibble::tribble(
    ~Year, ~Province_name, ~LU_total,
    2000,  "Lugo",         120,
    2000,  "Soria",        10
  )
  area_df <- tibble::tribble(
    ~Year, ~Province_name, ~Area_ha,
    2000,  "Lugo",         600
  )

  out <- whep:::.calculate_livestock_density(lu_totals_df, area_df)

  expect_equal(
    out |>
      dplyr::filter(Province_name == "Lugo") |>
      dplyr::pull(Livestock_density),
    0.2
  )
  # A province with no area row keeps its LU but has no density.
  expect_true(
    out |>
      dplyr::filter(Province_name == "Soria") |>
      dplyr::pull(Livestock_density) |>
      is.na()
  )
})

# .aggregate_crop_productivity ------------------------------------------------

test_that(".aggregate_crop_productivity uses cropland only, in kgN per ha", {
  npp_df <- tibble::tribble(
    ~Year, ~Province_name, ~LandUse,            ~Prod_MgN, ~Area_ygpit_ha,
    2000,  "Lugo",         "Cropland",          8,         100,
    2000,  "Lugo",         "Cropland",          4,         100,
    2000,  "Lugo",         "Pasture_Shrubland", 500,       900
  )

  out <- whep:::.aggregate_crop_productivity(npp_df)

  expect_equal(out$Prod_MgN_total, 12)
  expect_equal(out$Area_ha_cropland, 200)
  # 12 MgN over 200 ha is 0.06 MgN/ha, i.e. 60 kgN/ha. The pasture row, which
  # would have swamped both totals, must not be in there.
  expect_equal(out$Productivity_kgN_ha, 60)
})

# feed aggregation ------------------------------------------------------------

test_that("feed aggregation splits semi-natural from cropland", {
  df <- julia_legacy_feed_fixture()

  semi_nat <- whep:::.aggregate_semi_nat_feed_mgn(df)
  cropland <- whep:::.aggregate_cropland_feed_mgn(df)
  total <- whep:::.aggregate_total_feed_mgn(df)

  # Only Lugo has semi-natural feed, so Soria is absent rather than zero.
  expect_equal(semi_nat$Province_name, "Lugo")
  expect_equal(semi_nat$Semi_nat_feed_MgN, 30)
  expect_equal(
    cropland |>
      dplyr::filter(Province_name == "Lugo") |>
      dplyr::pull(Cropland_feed_MgN),
    10
  )
  # The Food row and the Agro-industry box are both outside the total: it is
  # semi-natural + cropland feed only.
  expect_equal(
    total |>
      dplyr::filter(Province_name == "Lugo") |>
      dplyr::pull(Total_feed_MgN),
    40
  )
})

test_that(".calculate_semi_nat_feed_share treats no semi-natural as zero", {
  out <- whep:::.calculate_semi_nat_feed_share(julia_legacy_feed_fixture())

  expect_equal(
    out |>
      dplyr::filter(Province_name == "Lugo") |>
      dplyr::pull(Semi_nat_share),
    30 / 40
  )
  # Soria has cropland feed only. The share is 0, not a missing value, which
  # is what the decision tree's `Semi_nat_share > 0.6` test relies on.
  expect_equal(
    out |>
      dplyr::filter(Province_name == "Soria") |>
      dplyr::pull(Semi_nat_share),
    0
  )
})

# .calculate_feed_domest_supply -----------------------------------------------

test_that(".calculate_feed_domest_supply totals feed and attaches LU", {
  lu_df <- tibble::tribble(
    ~Year, ~Province_name, ~LU_total, ~Area_ha,
    2000,  "Lugo",         120,       600
  )

  out <- whep:::.calculate_feed_domest_supply(
    julia_legacy_feed_fixture(),
    lu_df
  )

  # Every Feed row counts here, including the Agro-industry box that
  # .aggregate_total_feed_mgn() excludes: 30 + 10 + 5.
  expect_equal(
    out |>
      dplyr::filter(Province_name == "Lugo") |>
      dplyr::pull(Domestic_feed_MgN),
    45
  )
  expect_equal(
    out |> dplyr::filter(Province_name == "Lugo") |> dplyr::pull(LU_total),
    120
  )
  expect_false("Area_ha" %in% names(out))
})

# .calculate_feed_import_share ------------------------------------------------

test_that(".calculate_feed_import_share splits imports by LU share", {
  feed_df <- tibble::tribble(
    ~Year, ~Item,    ~Element,     ~Destiny, ~Value_destiny,
    2000,  "Soy",    "Import",     "Feed",   800,
    2000,  "Maize",  "Import",     "Feed",   200,
    2000,  "Maize",  "Import",     "Food",   5000,
    2000,  "Maize",  "Production", "Feed",   5000
  )
  lu_df <- tibble::tribble(
    ~Year, ~Province_name, ~LU_total,
    2000,  "Lugo",         750,
    2000,  "Soria",        250
  )

  out <- whep:::.calculate_feed_import_share(feed_df, lu_df)

  # Shares are a partition of national LU.
  expect_equal(sum(out$LU_share), 1)
  # Only Element == "Import" & Destiny == "Feed" is the national pool: 1000,
  # not the 5000 Food import nor the 5000 Production.
  expect_equal(sum(out$Feed_import_MgN), 1000)
  expect_equal(
    out |>
      dplyr::filter(Province_name == "Lugo") |>
      dplyr::pull(Feed_import_MgN),
    750
  )
})

# .calculate_imported_feed_share ----------------------------------------------

test_that(".calculate_imported_feed_share keeps a single LU_total column", {
  # Both inputs carry LU_total from the same aggregated table. An unsuffixed
  # join would leave LU_total.x / LU_total.y and the select() would fail.
  feed_import_by_province <- tibble::tribble(
    ~Year, ~Province_name, ~LU_total, ~LU_share, ~Feed_import_MgN,
    2000,  "Lugo",         750,       0.75,      750,
    2000,  "Soria",        250,       0.25,      250
  )
  domestic_feed_by_province <- tibble::tribble(
    ~Year, ~Province_name, ~LU_total, ~Domestic_feed_MgN,
    2000,  "Lugo",         750,       250,
    2000,  "Soria",        250,       750
  )

  out <- whep:::.calculate_imported_feed_share(
    feed_import_by_province,
    domestic_feed_by_province
  )

  expect_true("LU_total" %in% names(out))
  expect_false(any(grepl("LU_total\\.", names(out))))
  expect_equal(
    out |> dplyr::filter(Province_name == "Lugo") |> dplyr::pull(LU_total),
    750
  )
  expect_equal(
    out |>
      dplyr::filter(Province_name == "Lugo") |>
      dplyr::pull(Imported_feed_share),
    0.75
  )
  expect_equal(
    out |>
      dplyr::filter(Province_name == "Soria") |>
      dplyr::pull(Imported_feed_share),
    0.25
  )
})

test_that(".calculate_imported_feed_share reports no feed at all as NA", {
  feed_import_by_province <- tibble::tribble(
    ~Year, ~Province_name, ~LU_total, ~LU_share, ~Feed_import_MgN,
    2000,  "Lugo",         0,         0,         0
  )
  domestic_feed_by_province <- tibble::tribble(
    ~Year, ~Province_name, ~Domestic_feed_MgN,
    2000,  "Lugo",         0
  )

  out <- whep:::.calculate_imported_feed_share(
    feed_import_by_province,
    domestic_feed_by_province
  )

  # 0 / 0 would be NaN, which compares FALSE against every threshold without
  # saying so. It is turned into NA instead.
  expect_true(is.na(out$Imported_feed_share))
  expect_false(is.nan(out$Imported_feed_share))
})

# .grafs_prod_destiny_legacy --------------------------------------------------

test_that(".grafs_prod_destiny_legacy maps to the legacy vocabulary", {
  out <- whep:::.grafs_prod_destiny_legacy(julia_prov_destiny_fixture())

  expect_setequal(
    out$Destiny,
    c("Food", "Feed", "Export", "Import")
  )
  # The two irrigation categories of Wheat collapse into one Food row.
  expect_equal(
    out |>
      dplyr::filter(Item == "Wheat", Destiny == "Food") |>
      dplyr::pull(MgN),
    15
  )
  # Ruminant and monogastric feed become one "Feed" row.
  expect_equal(
    out |> dplyr::filter(Item == "Barley") |> dplyr::pull(MgN),
    10
  )
  expect_equal(
    out |> dplyr::filter(Item == "Grassland") |> dplyr::pull(Box),
    "Semi_natural_agroecosystems"
  )
})

test_that(".grafs_prod_destiny_legacy folds population_food_inedible into Food", {
  # population_food_inedible is the remainder .split_food_inedible_loss()
  # (n_prov_destiny.R) split out of population_food; the legacy vocabulary
  # predates that split and must see the same undivided "Food" total, not a
  # row silently dropped by the names(legacy_destiny) filter.
  flows <- tibble::tribble(
    ~item, ~box, ~origin, ~destiny,
    "Wheat", "Cropland", "Cropland", "population_food",
    "Wheat", "Cropland", "Cropland", "population_food_inedible"
  ) |>
    dplyr::mutate(
      mg_n = c(80, 20),
      year = 2000,
      province_name = "Lugo",
      irrig_cat = "rainfed"
    )

  out <- whep:::.grafs_prod_destiny_legacy(flows)

  expect_equal(unique(out$Destiny), "Food")
  expect_equal(sum(out$MgN), 100)
})

test_that(".grafs_prod_destiny_legacy drops soil inputs, repeats imports", {
  out <- whep:::.grafs_prod_destiny_legacy(julia_prov_destiny_fixture())

  # The Synthetic -> Cropland soil-input row (99 MgN) was never in the legacy
  # file, so it must not appear under any destiny.
  expect_equal(sum(out$MgN), 15 + 10 + 4 + 6 + 8 + 6)
  expect_false(any(out$MgN == 99))

  # Imported flows are emitted twice, once under their real destiny and once
  # as "Import", which is the legacy convention downstream code inverts by
  # subtracting the import rows from the sum of the use rows.
  imports <- out |> dplyr::filter(Destiny == "Import")
  expect_equal(imports$Item, "Soy")
  expect_equal(imports$MgN, 6)
  expect_equal(
    out |> dplyr::filter(Item == "Soy", Destiny == "Feed") |> dplyr::pull(MgN),
    6
  )
})

# .assign_decision_tree -------------------------------------------------------

test_that(".assign_decision_tree reaches every typology branch", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")

  # One province per branch of the tree, plus a province whose density is
  # unknown so the fall-through NA is exercised too.
  indicators <- tibble::tribble(
    ~Province_name, ~Livestock_density, ~Productivity_kgN_ha,
    "SpecCrop",     0.1,                90,
    "ExtCrop",      0.1,                40,
    "ExtMixed",     1.0,                50,
    "IntMixed",     1.0,                50,
    "SpecLivest",   1.0,                50,
    "Unknown",      NA,                 50
  ) |>
    dplyr::mutate(
      Year = 1980,
      Semi_nat_share = c(0, 0, 0.8, 0.2, 0.2, 0.2),
      Imported_feed_share = c(0, 0, 0.1, 0.1, 0.9, 0.1)
    )

  density <- indicators |>
    dplyr::select(Year, Province_name, Livestock_density) |>
    dplyr::mutate(LU_total = 1, Area_ha = 1)
  productivity <- indicators |>
    dplyr::select(Year, Province_name, Productivity_kgN_ha)
  semi_nat <- indicators |>
    dplyr::select(Year, Province_name, Semi_nat_share)
  imported <- indicators |>
    dplyr::select(Year, Province_name, Imported_feed_share)

  provinces <- sf::st_sf(
    name = indicators$Province_name,
    geometry = sf::st_sfc(lapply(
      seq_len(nrow(indicators)),
      function(i) sf::st_point(c(i, i))
    ))
  )

  out <- whep:::.assign_decision_tree(
    density,
    productivity,
    semi_nat,
    imported,
    sf_provinces = provinces,
    year = 1980
  )

  expect_equal(
    out$Typologies |> dplyr::arrange(Province_name) |> dplyr::pull(Typologie),
    c(
      "Extensive cropping system",
      "Extensive mixed crop-livestock system",
      "Intensive mixed crop-livestock system",
      "Specialized cropping system",
      "Specialized livestock-farming system",
      NA
    )
  )
  expect_s3_class(out$Typologies_map, "ggplot")
})

test_that(".assign_decision_tree filters the map year but keeps the series", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")

  indicators <- tibble::tribble(
    ~Year, ~Province_name, ~Livestock_density, ~Productivity_kgN_ha,
    1980,  "Lugo",         0.1,                90,
    2000,  "Lugo",         1.0,                50
  ) |>
    dplyr::mutate(Semi_nat_share = c(0, 0.8), Imported_feed_share = c(0, 0.1))

  provinces <- sf::st_sf(
    name = "Lugo",
    geometry = sf::st_sfc(sf::st_point(c(0, 0)))
  )

  out <- whep:::.assign_decision_tree(
    indicators |> dplyr::select(Year, Province_name, Livestock_density),
    indicators |> dplyr::select(Year, Province_name, Productivity_kgN_ha),
    indicators |> dplyr::select(Year, Province_name, Semi_nat_share),
    indicators |> dplyr::select(Year, Province_name, Imported_feed_share),
    sf_provinces = provinces,
    year = 1980
  )

  expect_equal(nrow(out$Typologies), 1)
  expect_equal(out$Typologies$Typologie, "Specialized cropping system")
  # The all-years table is not filtered, and the same province changes class
  # between the two years.
  expect_equal(out$Typologies_all_years$Year, c(1980, 2000))
  expect_equal(
    out$Typologies_all_years$Typologie,
    c(
      "Specialized cropping system",
      "Extensive mixed crop-livestock system"
    )
  )
})
