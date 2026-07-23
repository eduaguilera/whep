testthat::test_that("add_area_name correctly sets new column in table", {
  table <- tibble::tibble(area_code = c(1, 2, 4444, 3))

  table |>
    add_area_name() |>
    testthat::expect_equal(
      tibble::tribble(
        ~area_code, ~area_name,
        1, "Armenia",
        2, "Afghanistan",
        4444, NA,
        3, "Albania"
      )
    )

  table |>
    dplyr::rename(dummy_name = area_code) |>
    add_area_name(code_column = "dummy_name") |>
    testthat::expect_equal(
      tibble::tribble(
        ~dummy_name, ~area_name,
        1, "Armenia",
        2, "Afghanistan",
        4444, NA,
        3, "Albania"
      )
    )

  table |>
    dplyr::rename(dummy_name = area_code) |>
    add_area_name(code_column = "dummy_name", name_column = "my_name") |>
    testthat::expect_equal(
      tibble::tribble(
        ~dummy_name, ~my_name,
        1, "Armenia",
        2, "Afghanistan",
        4444, NA,
        3, "Albania"
      )
    )
})

testthat::test_that("add_area_code correctly sets new column in table", {
  table <- tibble::tibble(
    area_name = c("Armenia", "Afghanistan", "Dummy Country", "Albania")
  )

  table |>
    add_area_code() |>
    testthat::expect_equal(
      tibble::tribble(
        ~area_name, ~area_code,
        "Armenia", 1,
        "Afghanistan", 2,
        "Dummy Country", NA,
        "Albania", 3
      )
    )

  table |>
    dplyr::rename(dummy_name = area_name) |>
    add_area_code(name_column = "dummy_name") |>
    testthat::expect_equal(
      tibble::tribble(
        ~dummy_name, ~area_code,
        "Armenia", 1,
        "Afghanistan", 2,
        "Dummy Country", NA,
        "Albania", 3
      )
    )

  table |>
    dplyr::rename(dummy_name = area_name) |>
    add_area_code(name_column = "dummy_name", code_column = "dummy_code") |>
    testthat::expect_equal(
      tibble::tribble(
        ~dummy_name, ~dummy_code,
        "Armenia", 1,
        "Afghanistan", 2,
        "Dummy Country", NA,
        "Albania", 3
      )
    )
})

testthat::test_that("add_area_name returns current country name for shared ISO3", {
  # Regression for #237: FABIO/FAOSTAT reuse one ISO3 across a defunct area and
  # its successor (e.g. ETH -> both "Ethiopia PDR" (area 62) and the current
  # "Ethiopia" (area 238); SDN -> "Sudan (former)" and current "Sudan"). The
  # ISO3 join must resolve to the current polity, not the historical one.
  table <- tibble::tibble(area_code = c("ETH", "SDN"))

  table |>
    add_area_name() |>
    dplyr::pull(area_name) |>
    testthat::expect_equal(c("Ethiopia", "Sudan"))
})

testthat::test_that("add_area_code does not fan out on a duplicated area_name", {
  # Regression for #238: add_area_code() joins by name, so the lookup must be
  # deduplicated on the name key (not the code key). Guard as an invariant so a
  # future crosswalk with a repeated area_name cannot silently fan out rows.
  polities <- whep:::.get_polities(
    "area_name",
    "area_code",
    join_column = "area_name"
  )

  testthat::expect_equal(
    nrow(polities),
    dplyr::n_distinct(polities$area_name)
  )

  table <- tibble::tibble(area_name = c("Armenia", "Afghanistan"))
  table |>
    add_area_code() |>
    nrow() |>
    testthat::expect_equal(nrow(table))
})

testthat::test_that("add_item_cbs_name correctly sets new column in table", {
  table <- tibble::tibble(item_cbs_code = c(2559, 2744, 9876))

  table |>
    add_item_cbs_name() |>
    testthat::expect_equal(
      tibble::tribble(
        ~item_cbs_code, ~item_cbs_name,
        2559, "Cottonseed",
        2744, "Eggs",
        9876, NA
      )
    )

  table |>
    dplyr::rename(dummy_code = item_cbs_code) |>
    add_item_cbs_name(code_column = "dummy_code") |>
    testthat::expect_equal(
      tibble::tribble(
        ~dummy_code, ~item_cbs_name,
        2559, "Cottonseed",
        2744, "Eggs",
        9876, NA
      )
    )

  table |>
    dplyr::rename(dummy_code = item_cbs_code) |>
    add_item_cbs_name(code_column = "dummy_code", name_column = "my_name") |>
    testthat::expect_equal(
      tibble::tribble(
        ~dummy_code, ~my_name,
        2559, "Cottonseed",
        2744, "Eggs",
        9876, NA
      )
    )
})

testthat::test_that("add_item_cbs_code correctly sets new column in table", {
  table <- tibble::tibble(item_cbs_name = c("Cottonseed", "Eggs", "Dummy item"))

  table |>
    add_item_cbs_code() |>
    testthat::expect_equal(
      tibble::tribble(
        ~item_cbs_name, ~item_cbs_code,
        "Cottonseed", 2559,
        "Eggs", 2744,
        "Dummy item", NA
      )
    )

  table |>
    dplyr::rename(dummy_name = item_cbs_name) |>
    add_item_cbs_code(name_column = "dummy_name") |>
    testthat::expect_equal(
      tibble::tribble(
        ~dummy_name, ~item_cbs_code,
        "Cottonseed", 2559,
        "Eggs", 2744,
        "Dummy item", NA
      )
    )

  table |>
    dplyr::rename(dummy_name = item_cbs_name) |>
    add_item_cbs_code(name_column = "dummy_name", code_column = "dummy_code") |>
    testthat::expect_equal(
      tibble::tribble(
        ~dummy_name, ~dummy_code,
        "Cottonseed", 2559,
        "Eggs", 2744,
        "Dummy item", NA
      )
    )
})

testthat::test_that("add_item_prod_name correctly sets new column in table", {
  table <- tibble::tibble(item_prod_code = c(27, 358, 12345))

  table |>
    add_item_prod_name() |>
    testthat::expect_equal(
      tibble::tribble(
        ~item_prod_code, ~item_prod_name,
        27, "Rice",
        358, "Cabbages",
        12345, NA
      )
    )

  table |>
    dplyr::rename(dummy_code = item_prod_code) |>
    add_item_prod_name(code_column = "dummy_code") |>
    testthat::expect_equal(
      tibble::tribble(
        ~dummy_code, ~item_prod_name,
        27, "Rice",
        358, "Cabbages",
        12345, NA
      )
    )

  table |>
    dplyr::rename(dummy_code = item_prod_code) |>
    add_item_prod_name(code_column = "dummy_code", name_column = "my_name") |>
    testthat::expect_equal(
      tibble::tribble(
        ~dummy_code, ~my_name,
        27, "Rice",
        358, "Cabbages",
        12345, NA
      )
    )
})

testthat::test_that("add_item_prod_code correctly sets new column in table", {
  table <- tibble::tibble(item_prod_name = c("Rice", "Cabbages", "Dummy item"))

  table |>
    add_item_prod_code() |>
    testthat::expect_equal(
      tibble::tribble(
        ~item_prod_name, ~item_prod_code,
        "Rice", 27,
        "Cabbages", 358,
        "Dummy item", NA
      )
    )

  table |>
    dplyr::rename(dummy_name = item_prod_name) |>
    add_item_prod_code(name_column = "dummy_name") |>
    testthat::expect_equal(
      tibble::tribble(
        ~dummy_name, ~item_prod_code,
        "Rice", 27,
        "Cabbages", 358,
        "Dummy item", NA
      )
    )

  table |>
    dplyr::rename(dummy_name = item_prod_name) |>
    add_item_prod_code(
      name_column = "dummy_name",
      code_column = "dummy_code"
    ) |>
    testthat::expect_equal(
      tibble::tribble(
        ~dummy_name, ~dummy_code,
        "Rice", 27,
        "Cabbages", 358,
        "Dummy item", NA
      )
    )
})
