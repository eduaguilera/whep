# test_whep_typologies_spain.R — tests for R/whep_typologies_spain.R
#
# `create_typologies_whep()` takes both of its inputs as arguments, so the
# whole decision chain runs offline from tribble fixtures.

whep_semi_natural <- "semi_natural_agroecosystems"

# One province per branch of the Category tree. `box` and `origin` follow from
# the item, which keeps the fixture readable.
whep_typo_flows_fixture <- function() {
  tibble::tribble(
    ~province_name, ~item,       ~destiny,                ~mg_n,
    "Urban",        "Wheat",     "population_food",       100,
    "Woody",        "Wheat",     "population_food",       10,
    "Woody",        "Firewood",  "population_other_uses", 5,
    "Import",       "Wheat",     "population_food",       10,
    "Import",       "Soy",       "livestock_mono",        8,
    "Import",       "Barley",    "livestock_rum",         2,
    "Crop",         "Wheat",     "population_food",       10,
    "Crop",         "Barley",    "livestock_rum",         2,
    "Grass",        "Wheat",     "population_food",       10,
    "Grass",        "Grassland", "livestock_rum",         20,
    "Grass",        "Barley",    "livestock_rum",         2,
    "CropFeed",     "Wheat",     "population_food",       10,
    "CropFeed",     "Grassland", "livestock_rum",         2,
    "CropFeed",     "Barley",    "livestock_rum",         20
  ) |>
    dplyr::mutate(
      year = 2020,
      box = dplyr::if_else(item == "Grassland", whep_semi_natural, "Cropland"),
      origin = dplyr::if_else(item == "Soy", "Outside", box)
    )
}

whep_typo_prod_fixture <- function() {
  tibble::tribble(
    ~province_name, ~box,        ~production_n,
    "Urban",        "Cropland",  50,
    "Woody",        "Cropland",  100,
    "Import",       "Cropland",  100,
    "Crop",         "Cropland",  100,
    "Grass",        "Cropland",  5,
    "Grass",        "semi_nat",  20,
    "CropFeed",     "Cropland",  5,
    "CropFeed",     "semi_nat",  20
  ) |>
    dplyr::mutate(
      year = 2020,
      box = dplyr::if_else(box == "semi_nat", whep_semi_natural, box)
    )
}

# Category assignment ---------------------------------------------------------

test_that("create_typologies_whep reaches all six categories", {
  out <- whep::create_typologies_whep(
    prod_destiny = whep_typo_flows_fixture(),
    prod_n = whep_typo_prod_fixture(),
    years = 2020
  ) |>
    dplyr::arrange(province_name)

  expect_equal(
    out$province_name,
    c("Crop", "CropFeed", "Grass", "Import", "Urban", "Woody")
  )
  expect_equal(
    out$Category,
    c(
      "Cropland-based system",
      "Local crop-based livestock system",
      "Local grass-based livestock system",
      "Imported feed-based system",
      "Urban System",
      "Woody-based system"
    )
  )
})

test_that("create_typologies_whep computes the decision variables", {
  out <- whep::create_typologies_whep(
    prod_destiny = whep_typo_flows_fixture(),
    prod_n = whep_typo_prod_fixture(),
    years = 2020
  )

  urban <- out |> dplyr::filter(province_name == "Urban")
  # Consumption above own production is what makes a province "urban" here.
  expect_equal(urban$food_consumption, 100)
  expect_equal(urban$production, 50)
  expect_equal(urban$human_share, 2)

  woody <- out |> dplyr::filter(province_name == "Woody")
  # Food consumption spans both population destinies, food and other uses.
  expect_equal(woody$food_consumption, 15)
  expect_equal(woody$woody_prod, 5)
  expect_equal(woody$woody_share, 5 / 15)

  import <- out |> dplyr::filter(province_name == "Import")
  expect_equal(import$feed_import, 8)
  expect_equal(import$animal_ingestion, 10)
  expect_equal(import$import_share, 0.8)

  grass <- out |> dplyr::filter(province_name == "Grass")
  expect_equal(grass$grass_feed_N, 20)
  expect_equal(grass$crop_feed_N, 2)
  # Production spans every box, cropland production only the cropland one.
  expect_equal(grass$production, 25)
  expect_equal(grass$crop_prod, 5)
})

test_that("create_typologies_whep counts population_food_inedible as food_consumption", {
  # population_food_inedible is the remainder .split_food_inedible_loss()
  # (n_prov_destiny.R) split out of population_food; `production` already
  # includes it, so it must count toward food_consumption too.
  flows <- tibble::tribble(
    ~province_name, ~item, ~destiny, ~mg_n,
    "Urban", "Wheat", "population_food", 80,
    "Urban", "Wheat", "population_food_inedible", 20
  ) |>
    dplyr::mutate(year = 2020, box = "Cropland", origin = "Cropland")
  prod <- tibble::tribble(
    ~province_name, ~box, ~production_n,
    "Urban", "Cropland", 200
  ) |>
    dplyr::mutate(year = 2020)

  out <- whep::create_typologies_whep(
    prod_destiny = flows,
    prod_n = prod,
    years = 2020
  )

  expect_equal(out$food_consumption, 100)
})

test_that("create_typologies_whep ranks imported feed above cropland", {
  # "Import" has crop_prod 100 against an animal ingestion of 10, so the
  # cropland branch would fire; the import branch comes first in the tree.
  out <- whep::create_typologies_whep(
    prod_destiny = whep_typo_flows_fixture(),
    prod_n = whep_typo_prod_fixture(),
    years = 2020
  ) |>
    dplyr::filter(province_name == "Import")

  expect_gt(out$crop_prod, out$animal_ingestion)
  expect_equal(out$Category, "Imported feed-based system")
})

test_that("create_typologies_whep drops Sea, Fish and Agro-industry", {
  flows <- whep_typo_flows_fixture() |>
    dplyr::bind_rows(
      tibble::tribble(
        ~province_name, ~item,   ~box,             ~origin,    ~destiny,
        "Sea",          "Hake",  "Fish",           "Fish",     "population_food",
        "Crop",         "Hake",  "Fish",           "Fish",     "population_food",
        "Crop",         "Sugar", "Agro-industry",  "Cropland", "population_food"
      ) |>
        dplyr::mutate(year = 2020, mg_n = 1000)
    )

  out <- whep::create_typologies_whep(
    prod_destiny = flows,
    prod_n = whep_typo_prod_fixture(),
    years = 2020
  )

  expect_false("Sea" %in% out$province_name)
  # The fish and agro-industry boxes would have multiplied Crop's food
  # consumption by 200 and turned it into an urban system.
  expect_equal(
    out |>
      dplyr::filter(province_name == "Crop") |>
      dplyr::pull(food_consumption),
    10
  )
})

test_that("create_typologies_whep honours the years argument", {
  flows <- whep_typo_flows_fixture() |>
    dplyr::bind_rows(
      whep_typo_flows_fixture() |> dplyr::mutate(year = 1990)
    )

  out <- whep::create_typologies_whep(
    prod_destiny = flows,
    prod_n = whep_typo_prod_fixture(),
    years = 2020
  )

  expect_equal(unique(out$year), 2020)
})

# .plot_whep_typologies -------------------------------------------------------

test_that(".plot_whep_typologies fills gaps and orders provinces", {
  skip_if_not_installed("ggplot2")

  typologies <- tibble::tribble(
    ~year, ~province_name, ~human_share, ~woody_share, ~Category,
    2020,  "Alava",        NA,           NA,           "Urban System",
    2020,  "Sea",          0.5,          0.5,          "Urban System",
    2000,  "Zamora",       0.2,          0.3,          "Woody-based system"
  )

  plot <- whep:::.plot_whep_typologies(typologies)

  # "Sea" is not a province and is dropped before plotting.
  expect_false("Sea" %in% as.character(plot$data$province_name))
  expect_equal(plot$data$human_share, c(0, 0.2))
  expect_equal(plot$data$woody_share, c(0, 0.3))
  # Provinces are reversed so the y axis reads alphabetically top-to-bottom.
  expect_equal(levels(plot$data$province_name), c("Zamora", "Alava"))
})
