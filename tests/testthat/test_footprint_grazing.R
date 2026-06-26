# One country grazes cattle (dairy) and sheep, producing milk, bovine
# meat and mutton. Cattle eat 75% of the grass, sheep 25%.
.grazing_inputs <- function() {
  list(
    grass_land = tibble::tibble(
      year = 2010L,
      area_code = 1L,
      value = 100
    ),
    grazer_intake = tibble::tibble(
      year = 2010L,
      area_code = 1L,
      live_anim_code = c(960L, 976L),
      value = c(75, 25)
    ),
    livestock_production = tibble::tibble(
      year = 2010L,
      area_code = 1L,
      live_anim_code = c(960L, 960L, 976L, 976L),
      item_cbs_code = c(2848L, 2731L, 2732L, 2748L),
      value = c(90, 10, 5, 1)
    )
  )
}

testthat::test_that("forward allocation conserves all grazing land", {
  inp <- .grazing_inputs()
  out <- whep::allocate_grazing_to_products(
    inp$grass_land,
    inp$grazer_intake,
    inp$livestock_production
  )

  out |>
    pointblank::expect_col_exists(
      c("year", "area_code", "item_cbs_code", "value", "method_allocation")
    )
  testthat::expect_equal(sum(out$value), 100)
  testthat::expect_true(all(out$method_allocation == "all"))
})

testthat::test_that("intake share drives the split across animals", {
  inp <- .grazing_inputs()
  out <- whep::allocate_grazing_to_products(
    inp$grass_land,
    inp$grazer_intake,
    inp$livestock_production
  )

  # Cattle ate 75% -> 75 ha across milk (90 t) and bovine meat (10 t):
  # milk 67.5, bovine meat 7.5. Sheep ate 25% -> 25 ha across mutton (5 t)
  # and hides (1 t): mutton 20.83, hides 4.17.
  by_item <- out |> dplyr::arrange(item_cbs_code)
  testthat::expect_equal(
    by_item$value,
    c(7.5, 20.833333, 4.166667, 67.5),
    tolerance = 1e-5
  )
})

testthat::test_that("meat_milk keeps all land on meat and dairy items", {
  inp <- .grazing_inputs()
  out <- whep::allocate_grazing_to_products(
    inp$grass_land,
    inp$grazer_intake,
    inp$livestock_production,
    products = "meat_milk"
  )

  # Hides (2748) are excluded; sheep's 25 ha now all lands on mutton.
  testthat::expect_false(2748L %in% out$item_cbs_code)
  testthat::expect_equal(sum(out$value), 100)
  mutton <- out |> dplyr::filter(item_cbs_code == 2732L)
  testthat::expect_equal(mutton$value, 25)
})

testthat::test_that("animals that did not graze receive no land", {
  inp <- .grazing_inputs()
  # Add a granivore (broilers, 1053) producing poultry meat but absent
  # from grass intake: it must not receive grazing land.
  prod <- dplyr::bind_rows(
    inp$livestock_production,
    tibble::tibble(
      year = 2010L,
      area_code = 1L,
      live_anim_code = 1053L,
      item_cbs_code = 2734L,
      value = 500
    )
  )
  out <- whep::allocate_grazing_to_products(
    inp$grass_land,
    inp$grazer_intake,
    prod
  )

  testthat::expect_false(2734L %in% out$item_cbs_code)
  testthat::expect_equal(sum(out$value), 100)
})

testthat::test_that("forward footprint routes grazing land to meat consumers", {
  grass_land <- tibble::tibble(year = 2010L, area_code = 10L, value = 200)
  grazer_intake <- tibble::tibble(
    year = 2010L,
    area_code = 10L,
    live_anim_code = 961L,
    value = 100
  )
  production <- tibble::tibble(
    year = 2010L,
    area_code = c(10L, 41L),
    live_anim_code = c(961L, 961L),
    item_cbs_code = c(2731L, 2731L),
    value = c(100, 0)
  )
  # Country 10 exports 40% of its bovine meat to country 41.
  trade <- tibble::tibble(
    from_code = 10L,
    to_code = 41L,
    item_cbs_code = 2731L,
    value = 40
  )

  fp <- whep::build_grazing_feed_footprint(
    year = 2010L,
    data = list(
      grass_land = grass_land,
      grazer_intake = grazer_intake,
      livestock_production = production,
      trade = trade
    )
  )

  fp |>
    pointblank::expect_col_exists(
      c("area_code", "item_cbs_code", "value", "method")
    )
  by_area <- fp |> dplyr::arrange(area_code)
  # 60% of 200 ha stays with producer 10, 40% follows the meat to 41.
  testthat::expect_equal(by_area$value, c(120, 80))
  testthat::expect_equal(sum(fp$value), 200)
  testthat::expect_true(all(fp$method == "grazing_feed_allocation"))
})

testthat::test_that("missing columns are reported", {
  testthat::expect_error(
    whep::allocate_grazing_to_products(
      tibble::tibble(area_code = 1L, value = 1),
      tibble::tibble(
        year = 1L,
        area_code = 1L,
        live_anim_code = 1L,
        value = 1
      ),
      tibble::tibble(
        year = 1L,
        area_code = 1L,
        live_anim_code = 1L,
        item_cbs_code = 1L,
        value = 1
      )
    ),
    "grass_land"
  )
})

testthat::test_that("grazing land with no grazer intake is surfaced", {
  grass_land <- tibble::tibble(
    year = 2010L,
    area_code = c(10L, 99L),
    value = c(100, 50)
  )
  # Area 99 has grassland but no grazer intake -> 1/3 unattributable.
  grazer_intake <- tibble::tibble(
    year = 2010L,
    area_code = 10L,
    live_anim_code = 961L,
    value = 80
  )
  testthat::expect_warning(
    whep:::.warn_unattributed_land(grass_land, grazer_intake),
    "33.3% of grazing land"
  )
  # Full coverage must stay silent.
  testthat::expect_no_warning(
    whep:::.warn_unattributed_land(
      dplyr::filter(grass_land, area_code == 10L),
      grazer_intake
    )
  )
})

testthat::test_that("example output is a tidy tibble", {
  ex <- whep::build_grazing_feed_footprint(example = TRUE)
  ex |>
    pointblank::expect_col_exists(
      c("area_code", "item_cbs_code", "value", "method")
    )
  testthat::expect_true(all(ex$method == "grazing_feed_allocation"))
  testthat::expect_true(all(ex$value > 0))
})
