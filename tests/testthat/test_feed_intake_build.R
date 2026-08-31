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

# whep#467 --------------------------------------------------------------------
#
# Reporting bucket 999 (Rest of World) folds 62 FAOSTAT areas, 58 of which have
# a Bouwman feed region of their own. The fold keeps none of them, so the bucket
# row carried region = NA and every join keyed on the region missed. Measured on
# a full get_primary_production() at the parent commit: bucket 999's 808,638,528
# t of dry-matter demand (0.131% of world demand, demand_tier = "ipcc") was
# dropped in full by .build_feed_mix(), and with demand_tier = "fcr" the bucket
# produced no demand at all -- 191 areas instead of 194. The continent residuals
# 901-905 have the same gap but carry no production row at all.

testthat::test_that(".feed_region_lookup gives Rest of World a weighted mix", {
  lookup <- whep:::.feed_region_lookup()
  row <- dplyr::filter(lookup, .data$area_code == 999L)

  testthat::expect_gt(nrow(row), 1L)
  testthat::expect_equal(sum(row$region_weight), 1)
  testthat::expect_true(all(!is.na(row$region_bouwman)))
  # The mix must be expressible in the coefficient table it keys.
  testthat::expect_true(all(
    row$region_bouwman %in% whep::conv_bouwman$region_bouwman
  ))
})

testthat::test_that("region_fallback 'none' is the pre-467 status quo", {
  status_quo <- whep:::.feed_region_lookup(fallback = "none")

  testthat::expect_false(999L %in% status_quo$area_code)
  testthat::expect_true(all(status_quo$region_weight == 1))
  # One area_code, one region: the fallback must not disturb resolved buckets.
  testthat::expect_equal(
    nrow(status_quo),
    dplyr::n_distinct(status_quo$area_code)
  )
  resolved <- whep:::.feed_region_lookup() |>
    dplyr::filter(.data$area_code != 999L)
  testthat::expect_equal(
    dplyr::arrange(resolved, .data$area_code),
    dplyr::arrange(status_quo, .data$area_code)
  )
})

testthat::test_that("the Rest-of-World weights come from real fold members", {
  # Scoped to the explicit fold. WHEP now models the reporting members of
  # bucket 999 in their own right (#459), so there is no Rest-of-World fold
  # by default; what this pins is the fold behaviour itself, which still has
  # to work for anyone reproducing a published-before number.
  withr::local_options(whep.unfold_rest_of_world = "none")
  # Selected by the bucket they fold INTO, not by what the fold is called.
  # `fold_kind` is a classification and it moves: #556 reclassified Syria (212),
  # Eswatini (209), North Macedonia (154) and New Caledonia (153) from
  # `fabio_rest_of_world` to `cbs_reporter_folded` while this branch was open,
  # because they are CBS reporters as well as folds. All four still fold into
  # 999 and still carry herds, so a `fold_kind` filter silently dropped four real
  # members and this assertion failed on rows that were entirely correct.
  members <- whep::folded_reporting_areas() |>
    dplyr::filter(.data$polity_area_code == 999L)
  herds <- whep:::.row_member_herds()

  testthat::expect_true(all(herds$member_area_code %in% members$area_code))
  testthat::expect_true(all(herds$livestock_units > 0))
  # Each member's region is the one the crosswalk itself publishes for it, so a
  # polities re-sync that moves a member shows up here rather than silently.
  published <- whep::polity_area_crosswalk |>
    tibble::as_tibble() |>
    dplyr::distinct(
      member_area_code = as.integer(.data$area_code),
      region_bouwman = .data$region
    ) |>
    dplyr::filter(.data$member_area_code %in% herds$member_area_code)
  testthat::expect_equal(
    dplyr::arrange(
      dplyr::select(herds, "member_area_code", "region_bouwman"),
      .data$member_area_code
    ),
    dplyr::arrange(published, .data$member_area_code)
  )
})

testthat::test_that("continent residuals stay unmapped, deliberately", {
  # 901-905 (Africa/Asia/Europe/Latin America/North America Other) span several
  # Bouwman regions each and carry no production row at all, so no weight can be
  # measured for them. Pinned so it reads as a decision, not an oversight.
  lookup <- whep:::.feed_region_lookup()
  testthat::expect_equal(sum(lookup$area_code %in% 901:905), 0L)
  # 906 (Oceania Other) is different: the crosswalk resolves it directly.
  testthat::expect_equal(
    dplyr::filter(lookup, .data$area_code == 906L)$region_bouwman,
    "Oceania"
  )
})

testthat::test_that("a weighted region lookup averages the regions it mixes", {
  fcr <- whep:::.build_bouwman_fcr(whep::conv_bouwman, 1995L)
  primary <- tibble::tibble(
    year = 1995L,
    area_code = 999L,
    item_prod_code = 867,
    unit = "tonnes",
    value = 100
  )
  demand_of <- function(regs) {
    whep:::.build_feed_demand_fcr(
      primary,
      whep::items_prod_full,
      whep::animals_codes,
      regs,
      fcr
    ) |>
      dplyr::summarise(demand_aft = sum(.data$demand_aft), .by = "feed_type") |>
      dplyr::arrange(.data$feed_type)
  }

  halves <- tibble::tibble(
    area_code = 999L,
    region_bouwman = c("Middle East", "OECD Europe"),
    region_weight = c(0.5, 0.5)
  )
  mixed <- demand_of(halves)
  expected <- halves$region_bouwman |>
    purrr::map(\(r) {
      demand_of(tibble::tibble(area_code = 999L, region_bouwman = r))
    }) |>
    dplyr::bind_rows() |>
    dplyr::summarise(
      demand_aft = sum(.data$demand_aft) / 2,
      .by = "feed_type"
    ) |>
    dplyr::arrange(.data$feed_type)

  testthat::expect_gt(nrow(mixed), 0L)
  testthat::expect_equal(mixed, expected)
})

# whep#222 --------------------------------------------------------------------
#
# `.build_feed_demand` used to compute `demand_tot` / `demand_share` and join
# in `graniv_grazers` from `.feed_animal_type_lookup`, none of which the sole
# caller (`.build_feed_demand_codes`) reads -- it immediately summarises down
# to `demand_aft`. Guard the contract: those columns stay gone, and the
# columns that remain carry the same values as before the columns existed.

testthat::test_that(".build_feed_demand drops the dead columns", {
  regs <- tibble::tibble(area_code = 41L, region_bouwman = "East Asia")
  fcr <- whep:::.build_bouwman_fcr(whep::conv_bouwman, 1995L)
  primary <- tibble::tibble(
    year = 1995L,
    area_code = 41L,
    item_prod_code = 947,
    unit = "tonnes",
    value = 1200
  )

  out <- whep:::.build_feed_demand(
    primary,
    whep::items_prod_full,
    whep::animals_codes,
    whep::conv_krausmann,
    regs,
    fcr
  )

  testthat::expect_false(any(
    c("demand_tot", "demand_share", "graniv_grazers") %in% names(out)
  ))
  testthat::expect_true(all(
    c("year", "area_code", "live_anim_code", "feed_type", "demand_aft") %in%
      names(out)
  ))
  testthat::expect_gt(sum(out$demand_aft, na.rm = TRUE), 0)
})
