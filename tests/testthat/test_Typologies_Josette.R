# test_Typologies_Josette.R — tests for R/Typologies_Josette.R
#
# Every helper takes its inputs as arguments, so the indicator chain and the
# decision tree run offline from tribble fixtures.

# N flows in the legacy vocabulary the helpers consume. "Seed" is a destiny the
# production identity does not know about, and is there to be excluded.
josette_flows_fixture <- function() {
  tibble::tribble(
    ~Province_name, ~Box,       ~Destiny,     ~MgN,
    "Lugo",         "Cropland", "Food",       30,
    "Lugo",         "Cropland", "Feed",       20,
    "Lugo",         "Cropland", "Other_uses", 5,
    "Lugo",         "Cropland", "Export",     10,
    "Lugo",         "Cropland", "Import",     15,
    "Lugo",         "Cropland", "Seed",       100,
    "Lugo",         "Semi_nat", "Feed",       8,
    "Soria",        "Cropland", "Food",       10
  ) |>
    dplyr::mutate(
      Year = 2000,
      Item = "Wheat",
      Box = dplyr::if_else(
        Box == "Semi_nat",
        "Semi_natural_agroecosystems",
        Box
      )
    )
}

# .calculate_consumption_prod -------------------------------------------------

test_that(".calculate_consumption_prod nets imports out of production", {
  out <- whep:::.calculate_consumption_prod(josette_flows_fixture())

  expect_named(out, c("food_consumption", "production"))
  expect_equal(
    out$food_consumption |>
      dplyr::filter(Province_name == "Lugo") |>
      dplyr::pull(Food_Consumption_MgN),
    30
  )
  # Production is Food + Feed + Other_uses + Export - Import, over every box:
  # (30 + 28 + 5 + 10) - 15. The 100 MgN of "Seed" is not in the identity.
  expect_equal(
    out$production |>
      dplyr::filter(Province_name == "Lugo") |>
      dplyr::pull(Production_MgN),
    58
  )
  expect_equal(
    out$production |>
      dplyr::filter(Province_name == "Soria") |>
      dplyr::pull(Production_MgN),
    10
  )
})

# .calculate_crop_prod_feed ---------------------------------------------------

test_that(".calculate_crop_prod_feed separates cropland from all feed", {
  out <- whep:::.calculate_crop_prod_feed(josette_flows_fixture())

  expect_named(out, c("cropland_prod", "animal_ingestion"))
  # Cropland only, so the 8 MgN of semi-natural feed is out: 30+20+5+10-15.
  expect_equal(
    out$cropland_prod |>
      dplyr::filter(Province_name == "Lugo") |>
      dplyr::pull(Cropland_Production_MgN),
    50
  )
  # Animal ingestion is every Feed row, both boxes included.
  expect_equal(
    out$animal_ingestion |>
      dplyr::filter(Province_name == "Lugo") |>
      dplyr::pull(Animal_Ingestion_MgN),
    28
  )
  # Soria has a Food row only, so the missing destinies are filled with zero
  # rather than dropping the province.
  expect_equal(
    out$cropland_prod |>
      dplyr::filter(Province_name == "Soria") |>
      dplyr::pull(Cropland_Production_MgN),
    10
  )
})

# .calculate_natural_feed_share -----------------------------------------------

test_that(".calculate_natural_feed_share shares over all feed", {
  out <- whep:::.calculate_natural_feed_share(josette_flows_fixture())

  expect_equal(out$Province_name, "Lugo")
  expect_equal(out$SemiNatural_feed_MgN, 8)
  expect_equal(out$Total_feed_MgN, 28)
  expect_equal(out$SemiNatural_feed_share, 8 / 28)
  # A province with no semi-natural feed is absent rather than zero, because
  # the semi-natural table is the left-hand side of the join.
  expect_false("Soria" %in% out$Province_name)
})

# .calculate_manure_share -----------------------------------------------------

test_that(".calculate_manure_share uses cropland inputs only", {
  n_input_df <- tibble::tribble(
    ~Province_name, ~Box,       ~MgN_dep, ~MgN_fix, ~MgN_syn, ~MgN_manure,
    "Lugo",         "Cropland", 1,        2,        3,        4,
    "Lugo",         "Cropland", 1,        0,        0,        1,
    "Lugo",         "Pasture",  100,      100,      100,      100,
    "Soria",        "Cropland", 0,        0,        5,        5
  ) |>
    dplyr::mutate(Year = 2000, MgN_urban = 0)

  out <- whep:::.calculate_manure_share(n_input_df)

  # Lugo's two cropland rows sum to 2 + 2 + 3 + 5 + 0 = 12 MgN of input, of
  # which 5 is manure. The pasture row would have swamped both.
  lugo <- out |> dplyr::filter(Province_name == "Lugo")
  expect_equal(lugo$MgN_total, 12)
  expect_equal(lugo$Manure_share, 5 / 12)
  expect_equal(
    out |> dplyr::filter(Province_name == "Soria") |> dplyr::pull(Manure_share),
    0.5
  )
})

# .assign_typologies ----------------------------------------------------------

test_that(".assign_typologies reaches every typology in order", {
  # One province per branch, named after the branch it should take. The tree is
  # ordered, so each province must also fail every earlier test.
  indicators <- tibble::tribble(
    ~Province_name, ~Food_Consumption_MgN, ~Production_MgN,
    "Urban",        100,                   50,
    "Stockless",    10,                    100,
    "SpecLivest",   10,                    100,
    "Grass",        10,                    100,
    "Forage",       10,                    100,
    "Disconnected", 10,                    100
  ) |>
    dplyr::mutate(
      Year = 2000,
      Cropland_Production_MgN = c(0, 100, 10, 10, 10, 10),
      Animal_Ingestion_MgN = c(10, 10, 100, 100, 100, 100),
      Livestock_density = c(0, 0, 2, 2, 0, 0),
      Imported_feed_share = c(0, 0, 0.5, 0.1, 0, 0),
      SemiNatural_feed_share = c(0, 0, 0.8, 0.8, 0.2, 0.2),
      local_feed_share = c(0, 0, 0.5, 0.5, 0.5, 0),
      Manure_share = c(0, 0, 0.5, 0.5, 0.5, 0)
    )

  out <- whep:::.assign_typologies(indicators)

  expect_named(out, c("Year", "Province_name", "Typology"))
  expect_equal(
    out$Typology,
    c(
      "Urban system",
      "Specialized stockless cropping system",
      "Specialized livestock system",
      "Grass-based crop & livestock system",
      "Forage-based crop & livestock system",
      "Disconnected crop & livestock system"
    )
  )
})

test_that(".assign_typologies puts the stockless test above livestock", {
  # Cropland production above 1.5x ingestion wins even when the province also
  # looks like a specialized livestock system.
  indicators <- tibble::tibble(
    Year = 2000,
    Province_name = "Both",
    Food_Consumption_MgN = 10,
    Production_MgN = 100,
    Cropland_Production_MgN = 100,
    Animal_Ingestion_MgN = 10,
    Livestock_density = 2,
    Imported_feed_share = 0.9,
    SemiNatural_feed_share = 0.9,
    local_feed_share = 0.9,
    Manure_share = 0.9
  )

  expect_equal(
    whep:::.assign_typologies(indicators)$Typology,
    "Specialized stockless cropping system"
  )
})

# .calculate_imported_feed ----------------------------------------------------

test_that(".calculate_imported_feed chains LU, density and import share", {
  livestock_df <- tibble::tribble(
    ~Province_name, ~Livestock_cat,  ~Stock_Number,
    "Lugo",         "Dairy_cattle",  600,
    "Soria",        "Dairy_cattle",  200
  ) |>
    dplyr::mutate(Year = 2000)
  livestock_units_df <- tibble::tribble(
    ~Livestock_cat,  ~Animal_class, ~LU_head,
    "Dairy_cattle",  "Cattle",      1
  )
  npp_df <- tibble::tribble(
    ~Province_name, ~LandUse,   ~Area_ygpit_ha,
    "Lugo",         "Cropland", 300,
    "Soria",        "Cropland", 400
  ) |>
    dplyr::mutate(Year = 2000)
  feed_df <- tibble::tribble(
    ~Element,  ~Destiny, ~Value_destiny,
    "Import",  "Feed",   400,
    "Import",  "Food",   9000
  ) |>
    dplyr::mutate(Year = 2000, Item = "Soy")

  out <- whep:::.calculate_imported_feed(
    livestock_df,
    livestock_units_df,
    npp_df,
    feed_df,
    josette_flows_fixture()
  )

  expect_named(
    out,
    c("lu_totals", "livestock_density_df", "imported_feed_share_df")
  )
  # 600 LU over 300 ha of agricultural area.
  expect_equal(
    out$livestock_density_df |>
      dplyr::filter(Province_name == "Lugo") |>
      dplyr::pull(Livestock_density),
    2
  )
  # Lugo holds 600 of the 800 national LU, so 75% of the 400 MgN of imported
  # feed. Its domestic feed here is cropland feed only, 20 MgN.
  share <- out$imported_feed_share_df |>
    dplyr::filter(Province_name == "Lugo")
  expect_equal(share$Feed_import_MgN, 300)
  expect_equal(share$Domestic_feed_MgN, 20)
  expect_equal(share$Imported_feed_share, 300 / 320)
})

# .calculate_feed_domestic_share -----------------------------------------------

test_that(".calculate_feed_domestic_share returns one row per year-province", {
  # Two items (Wheat, Barley) both destined as Feed for the same year. Before
  # aggregating feed_summary over Item, pivot_wider() leaves one row per
  # Year x Item, and joining that against province-level LU shares by Year
  # alone fans every province out once per item.
  feed_df <- tibble::tribble(
    ~Year, ~Item, ~Element, ~Destiny, ~Value_destiny,
    2000, "Wheat", "Production", "Feed", 100,
    2000, "Wheat", "Export", "Feed", 20,
    2000, "Wheat", "Import", "Feed", 10,
    2000, "Barley", "Production", "Feed", 50,
    2000, "Barley", "Export", "Feed", 0,
    2000, "Barley", "Import", "Feed", 5
  )

  lu_df <- tibble::tribble(
    ~Year, ~Province_name, ~LU_total,
    2000, "A", 60,
    2000, "B", 40
  )

  codes_coefs_item <- tibble::tribble(
    ~item, ~Name_biomass,
    "Wheat", "Cereal",
    "Barley", "Cereal"
  )

  biomass_coefs <- tibble::tribble(
    ~Name_biomass, ~Product_kgDM_kgFM, ~Product_kgN_kgDM,
    "Cereal", 1, 1
  )

  out <- .calculate_feed_domestic_share(
    feed_df,
    lu_df,
    codes_coefs_item,
    biomass_coefs
  )

  # No fan-out: exactly one row per Year x Province_name.
  expect_equal(nrow(out), 2)
  expect_equal(nrow(dplyr::distinct(out, Year, Province_name)), 2)

  # National totals are summed across items (Production 150, Export 20,
  # Import 15) before being split by province LU share.
  expected_share <- (150 - 20) / ((150 - 20) + 15)
  expect_equal(
    out$local_feed_share,
    rep(expected_share, 2),
    tolerance = 1e-12
  )
})
