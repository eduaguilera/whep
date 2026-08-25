# Tests for `.backcast_crop_areas()` in inst/scripts/prepare_spatialize_all.R,
# which extends country crop areas to years before `build_primary_production()`
# reaches (it reconstructs FAOSTAT back to 1850 and no further). The helper
# lives at script scope, so the script is sourced first, as the other
# prepare_spatialize tests do.
#
# Why this needs testing rather than eyeballing: `run_crop_spatialize()` takes
# the LPJmL output year axis from `country_areas`, and LPJmL's `readdata()`
# silently clamps any year below a forcing file's first year. A gap here does
# not fail -- it produces a run that looks correct and holds land use constant
# for a century.

.source_prepare_spatialize()

# One country, two LUH2 types, two crops sharing the c3ann type so the
# many-to-many join is exercised.
.bc_crop_areas <- function() {
  tibble::tribble(
    ~year, ~area_code, ~item_prod_code, ~harvested_area_ha, ~luh2_type,
    1851L,         10L,             15L,              300,   "c3ann",
    1851L,         10L,             44L,              100,   "c3ann",
    1851L,         10L,             56L,              200,   "c4ann",
    1852L,         10L,             15L,              330,   "c3ann"
  )
}

# c3ann halves going back, c4ann quarters.
.bc_luh2_totals <- function() {
  tibble::tribble(
    ~year, ~area_code, ~luh2_type, ~crop_ha, ~irrig_ha,
    1750L,         10L,    "c3ann",      400,         0,
    1750L,         10L,    "c4ann",      100,         0,
    1851L,         10L,    "c3ann",      800,         0,
    1851L,         10L,    "c4ann",      400,         0
  )
}


test_that(".backcast_crop_areas is a no-op when no year precedes the base", {
  skip_if_not(exists(".backcast_crop_areas", mode = "function"))
  crop_areas <- .bc_crop_areas()
  out <- .backcast_crop_areas(
    crop_areas,
    .bc_luh2_totals(),
    1851:1852
  )
  expect_equal(nrow(out), nrow(crop_areas))
  expect_equal(min(out$year), 1851L)
})


test_that(".backcast_crop_areas scales each crop by its own LUH2 type", {
  skip_if_not(exists(".backcast_crop_areas", mode = "function"))
  out <- .backcast_crop_areas(
    .bc_crop_areas(),
    .bc_luh2_totals(),
    1750:1852
  )
  added <- dplyr::filter(out, .data$year == 1750L)
  expect_equal(nrow(added), 3L)
  # c3ann halves (400/800), c4ann quarters (100/400)
  expect_equal(
    added$harvested_area_ha[added$item_prod_code == 15L],
    150
  )
  expect_equal(
    added$harvested_area_ha[added$item_prod_code == 44L],
    50
  )
  expect_equal(
    added$harvested_area_ha[added$item_prod_code == 56L],
    50
  )
})


test_that(".backcast_crop_areas holds the mix within a LUH2 type", {
  skip_if_not(exists(".backcast_crop_areas", mode = "function"))
  out <- .backcast_crop_areas(
    .bc_crop_areas(),
    .bc_luh2_totals(),
    1750:1852
  )
  share <- function(yr) {
    x <- out |>
      dplyr::filter(.data$year == yr, .data$luh2_type == "c3ann")
    x$harvested_area_ha / sum(x$harvested_area_ha)
  }
  # wheat:barley stays 3:1 within c3ann, which is the frozen part
  expect_equal(sort(share(1750L)), sort(share(1851L)))
})


test_that(".backcast_crop_areas keeps the base rows untouched", {
  skip_if_not(exists(".backcast_crop_areas", mode = "function"))
  crop_areas <- .bc_crop_areas()
  out <- .backcast_crop_areas(
    crop_areas,
    .bc_luh2_totals(),
    1750:1852
  )
  kept <- out |>
    dplyr::filter(.data$year >= 1851L) |>
    dplyr::arrange(.data$year, .data$item_prod_code)
  expect_equal(
    kept$harvested_area_ha,
    dplyr::arrange(crop_areas, year, item_prod_code)$harvested_area_ha
  )
})


test_that(".backcast_crop_areas drops a country with no base LUH2 area", {
  skip_if_not(exists(".backcast_crop_areas", mode = "function"))
  # area 20 has 1750 LUH2 cropland but none in the base year, so the ratio is
  # undefined; it must be dropped rather than divided by zero.
  luh2 <- dplyr::bind_rows(
    .bc_luh2_totals(),
    tibble::tribble(
      ~year, ~area_code, ~luh2_type, ~crop_ha, ~irrig_ha,
      1750L,         20L,    "c3ann",      500,         0,
      1851L,         20L,    "c3ann",        0,         0
    )
  )
  crop_areas <- dplyr::bind_rows(
    .bc_crop_areas(),
    tibble::tibble(
      year = 1851L,
      area_code = 20L,
      item_prod_code = 15L,
      harvested_area_ha = 10,
      luh2_type = "c3ann"
    )
  )
  out <- .backcast_crop_areas(crop_areas, luh2, 1750:1852)
  expect_false(any(out$year == 1750L & out$area_code == 20L))
  expect_true(all(is.finite(out$harvested_area_ha)))
})


test_that(".backcast_crop_areas emits no zero or negative areas", {
  skip_if_not(exists(".backcast_crop_areas", mode = "function"))
  luh2 <- .bc_luh2_totals()
  luh2$crop_ha[luh2$year == 1750L & luh2$luh2_type == "c4ann"] <- 0
  out <- .backcast_crop_areas(.bc_crop_areas(), luh2, 1750:1852)
  expect_true(all(out$harvested_area_ha > 0))
  # the c4ann crop had zero LUH2 area in 1750, so it contributes no row
  expect_false(any(out$year == 1750L & out$item_prod_code == 56L))
})
