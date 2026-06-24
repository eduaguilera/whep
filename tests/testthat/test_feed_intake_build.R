testthat::test_that(".normalise_feed_cbs accepts raw long CBS pins", {
  raw <- tibble::tribble(
    ~Year, ~area_code, ~item_code, ~Element, ~Value,
    2000, 1, 2514, "Feed", 10,
    2000, 1, 2514, "Food", 5,
    2000, 1, 2514, "Feed", 2
  )

  out <- whep:::.normalise_feed_cbs(raw)

  testthat::expect_equal(
    names(out),
    c("year", "area_code", "item_cbs_code", "feed")
  )
  testthat::expect_equal(out$feed, 12)
})

testthat::test_that("get_feed_intake builds internally instead of reading feed_intake pin", {
  testthat::local_mocked_bindings(
    get_wide_cbs = function(...) whep:::.example_get_wide_cbs(),
    get_primary_production = function(...) whep:::.ex_get_primary_prod(),
    whep_read_file = function(name, ...) {
      if (identical(name, "feed_intake")) {
        stop("feed_intake pin should not be read", call. = FALSE)
      }
      tibble::tibble()
    }
  )

  # Builds from cbs / production via redistribute_feed, never a feed_intake pin
  # (the mock errors if that pin is read). The toy fixture is a minimal, not a
  # coherent feed system, so the allocator can legitimately return no rows; the
  # contract shape is what this guards.
  out <- whep::get_feed_intake()

  testthat::expect_s3_class(out, "tbl_df")
  core_cols <- c(
    "year",
    "area_code",
    "live_anim_code",
    "item_cbs_code",
    "feed_type",
    "supply",
    "intake",
    "intake_dry_matter",
    "loss",
    "loss_share"
  )
  # The toy fixture is a minimal, not a coherent feed system, so the allocator
  # can legitimately return no rows; the contract shape (core columns present,
  # alongside the added reporting-polity columns) is what this guards.
  testthat::expect_true(all(core_cols %in% names(out)))
})

testthat::test_that("buffalo dairy products use dairy Bouwman demand in builder", {
  regs <- tibble::tibble(area_code = 41L, region_bouwman = "East Asia")
  fcr <- whep:::.build_bouwman_fcr(whep::conv_bouwman, 1995L)

  buffalo_demand <- function(item_prod_code) {
    primary <- tibble::tibble(
      year = 1995L,
      area_code = 41L,
      item_prod_code = item_prod_code,
      unit = "tonnes",
      value = 1
    )

    whep:::.build_feed_demand_fcr(
      primary,
      whep::items_prod_full,
      whep::animals_codes,
      regs,
      fcr
    ) |>
      dplyr::filter(.data$feed_type == "grass") |>
      dplyr::pull(.data$demand_aft)
  }

  meat_grass <- buffalo_demand(947)
  milk_grass <- buffalo_demand(951)

  testthat::expect_gt(meat_grass, 40)
  testthat::expect_lt(milk_grass, 1.1)
  testthat::expect_lt(milk_grass, meat_grass / 20)
})
