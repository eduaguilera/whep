# test_alfredo_typologies.R — tests for R/alfredo_typologies.R
#
# `create_alfredos_typologies()` takes both of its inputs as arguments, so the
# classification runs offline from tribble fixtures; nothing here touches
# `create_n_soil_inputs()` or `create_n_prov_destiny()`.

# One province per branch of the Category tree. `mg_n` is filled after the
# tribble so the flow columns stay inside the line budget.
alfredo_prod_destiny_fixture <- function() {
  tibble::tribble(
    ~province_name, ~item,       ~box,        ~origin,    ~destiny,
    "Grass",        "Grassland", "semi_nat",  "semi_nat", "livestock_rum",
    "Grass",        "Soy",       "Cropland",  "Outside",  "livestock_mono",
    "Woody",        "Grassland", "semi_nat",  "semi_nat", "livestock_rum",
    "Woody",        "Firewood",  "Cropland",  "Cropland", "population_food",
    "Woody",        "Soy",       "Cropland",  "Outside",  "livestock_mono",
    "Herb",         "Grassland", "semi_nat",  "semi_nat", "livestock_rum",
    "Herb",         "Acorns",    "Cropland",  "Cropland", "population_food",
    "Herb",         "Soy",       "Cropland",  "Outside",  "livestock_mono",
    "Imported",     "Grassland", "semi_nat",  "semi_nat", "livestock_rum",
    "Imported",     "Soy",       "Cropland",  "Outside",  "livestock_mono"
  ) |>
    dplyr::mutate(
      mg_n = c(100, 5, 1, 10, 5, 10, 1, 5, 1, 50),
      year = 2000,
      box = dplyr::if_else(
        box == "semi_nat",
        "semi_natural_agroecosystems",
        box
      ),
      origin = dplyr::if_else(
        origin == "semi_nat",
        "semi_natural_agroecosystems",
        origin
      )
    )
}

alfredo_soil_inputs_fixture <- function() {
  tibble::tribble(
    ~year, ~province_name, ~synthetic,
    2000,  "Grass",        10,
    2000,  "Woody",        50,
    2000,  "Herb",         50,
    2000,  "Imported",     5
  )
}

alfredo_categories <- function(...) {
  whep::create_alfredos_typologies(...) |>
    dplyr::arrange(province_name) |>
    dplyr::select(province_name, Category)
}

# Category assignment ---------------------------------------------------------

test_that("create_alfredos_typologies reaches all four categories", {
  out <- alfredo_categories(
    soil_inputs = alfredo_soil_inputs_fixture(),
    prod_destiny = alfredo_prod_destiny_fixture(),
    years = 2000
  )

  expect_equal(
    out$province_name,
    c("Grass", "Herb", "Imported", "Woody")
  )
  expect_equal(
    out$Category,
    c("Grassland", "Synthetic herbaceous", "Imported feed", "Synthetic woody")
  )
})

test_that("create_alfredos_typologies reports the indicators it ranked", {
  out <- whep::create_alfredos_typologies(
    soil_inputs = alfredo_soil_inputs_fixture(),
    prod_destiny = alfredo_prod_destiny_fixture(),
    years = 2000
  )

  woody <- out |> dplyr::filter(province_name == "Woody")
  # Grassland N is feed plus export from the semi-natural box only.
  expect_equal(woody$grass_N, 1)
  expect_equal(woody$fertiliser_N, 50)
  # Imported feed is the "Outside" origin arriving at a livestock destiny.
  expect_equal(woody$feed_import_N, 5)
  # 10 MgN of firewood against 1 MgN of grassland herbage.
  expect_equal(woody$woody, 10)
  expect_equal(woody$herbaceous, 1)
  expect_equal(woody$woody_share, 10 / 11)

  herb <- out |> dplyr::filter(province_name == "Herb")
  expect_equal(herb$woody_share, 1 / 11)
})

test_that("create_alfredos_typologies nets imports out of woody biomass", {
  # The same item arriving from "Outside" is not local production, so it is
  # subtracted before the woody/herbaceous split, and the result is floored at
  # zero rather than going negative.
  prod_destiny <- tibble::tribble(
    ~province_name, ~item,       ~origin,    ~destiny,          ~mg_n,
    "Net",          "Grassland", "semi_nat", "livestock_rum",   10,
    "Net",          "Firewood",  "Cropland", "population_food", 10,
    "Net",          "Firewood",  "Outside",  "population_food", 4,
    "Over",         "Grassland", "semi_nat", "livestock_rum",   10,
    "Over",         "Firewood",  "Cropland", "population_food", 3,
    "Over",         "Firewood",  "Outside",  "population_food", 8
  ) |>
    dplyr::mutate(
      year = 2000,
      box = dplyr::if_else(
        item == "Grassland",
        "semi_natural_agroecosystems",
        "Cropland"
      ),
      origin = dplyr::if_else(
        origin == "semi_nat",
        "semi_natural_agroecosystems",
        origin
      )
    )

  out <- whep::create_alfredos_typologies(
    soil_inputs = tibble::tibble(
      year = 2000,
      province_name = c("Net", "Over"),
      synthetic = 100
    ),
    prod_destiny = prod_destiny,
    years = 2000
  )

  # The province-level import total (4) is subtracted from every firewood row:
  # the domestic row contributes 10 - 4 = 6, the import row 4 - 4 = 0.
  expect_equal(
    out |> dplyr::filter(province_name == "Net") |> dplyr::pull(woody),
    6
  )
  # Imports above local production would make the difference negative.
  expect_equal(
    out |> dplyr::filter(province_name == "Over") |> dplyr::pull(woody),
    0
  )
})

test_that("create_alfredos_typologies keeps only provinces with grassland", {
  # The grassland table is the left-hand side of the join chain, so a province
  # whose only flow is cropland feed never reaches the output at all.
  prod_destiny <- tibble::tribble(
    ~province_name, ~item,       ~box,       ~origin,    ~destiny,      ~mg_n,
    "WithGrass",    "Grassland", "semi_nat", "semi_nat", "export",      7,
    "NoGrass",      "Barley",    "Cropland", "Cropland", "livestock_rum", 7
  ) |>
    dplyr::mutate(
      year = 2000,
      box = dplyr::if_else(
        box == "semi_nat",
        "semi_natural_agroecosystems",
        box
      ),
      origin = dplyr::if_else(
        origin == "semi_nat",
        "semi_natural_agroecosystems",
        origin
      )
    )

  out <- whep::create_alfredos_typologies(
    soil_inputs = tibble::tibble(
      year = 2000,
      province_name = c("WithGrass", "NoGrass"),
      synthetic = 1
    ),
    prod_destiny = prod_destiny,
    years = 2000
  )

  expect_equal(out$province_name, "WithGrass")
  # Exported grassland N counts towards grass_N alongside feed.
  expect_equal(out$grass_N, 7)
})

test_that("create_alfredos_typologies honours the years argument", {
  prod_destiny <- alfredo_prod_destiny_fixture() |>
    dplyr::bind_rows(
      alfredo_prod_destiny_fixture() |> dplyr::mutate(year = 1990)
    )

  out <- whep::create_alfredos_typologies(
    soil_inputs = alfredo_soil_inputs_fixture(),
    prod_destiny = prod_destiny,
    years = 2000
  )

  expect_equal(unique(out$year), 2000)
})

# .plot_province_typologies ---------------------------------------------------

test_that(".plot_province_typologies fills gaps and orders provinces", {
  skip_if_not_installed("ggplot2")

  typologies <- tibble::tribble(
    ~year, ~province_name, ~fertiliser_N, ~feed_import_N, ~woody_share,
    2000,  "Alava",        NA,            NA,             NA,
    2000,  "Zamora",       10,            2,              0.3
  ) |>
    dplyr::mutate(Category = c("Grassland", "Synthetic herbaceous"))

  plot <- whep:::.plot_province_typologies(typologies)

  # A missing indicator is drawn as zero rather than silently dropping a tile.
  expect_equal(plot$data$fertiliser_N, c(0, 10))
  expect_equal(plot$data$feed_import_N, c(0, 2))
  expect_equal(plot$data$woody_share, c(0, 0.3))
  # Provinces are reversed so the y axis reads alphabetically top-to-bottom.
  expect_equal(levels(plot$data$province_name), c("Zamora", "Alava"))
  expect_s3_class(plot, "ggplot")
})
