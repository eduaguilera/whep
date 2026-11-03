# The pre-1961 crop-residue back-cast (whep#1508).
#
# The `crop_residues` pin starts in 1961. Before this, the historical
# extension grew residue USES along population and land and gave the residue
# no PRODUCTION, so the balance closed through a stock draw: 2.68 Gt in 1955
# and 2.91 Gt in 1960 on a real 1955-1965 build. The residue now grows back
# along the crop it comes from.

# One first-year residue row of the `get_primary_residues()` schema.
.rb_residue <- function(
  area = 203L,
  crop = 2511L,
  residue = 2105L,
  value = 1000,
  method = "pin",
  polity_area = area
) {
  tibble::tibble(
    year = 1961,
    area_code = area,
    polity_area_code = polity_area,
    item_cbs_code_crop = crop,
    item_cbs_code_residue = residue,
    value = value,
    value_dm = 0.9 * value,
    method_residue = method
  )
}

# Spain's wheat (FAOSTAT item 15): production 50, 80 and 100 t and harvested
# area 40, 50 and 50 ha over 1959-1961, so the two bases grow differently.
.rb_primary <- function(area = 203L, years = 1959:1961) {
  n <- length(years)
  dplyr::bind_rows(
    tibble::tibble(
      year = years,
      area_code = area,
      item_prod_code = 15L,
      item_cbs_code = 2511L,
      unit = "tonnes",
      value = c(50, 80, 100)[seq_len(n)]
    ),
    tibble::tibble(
      year = years,
      area_code = area,
      item_prod_code = 15L,
      item_cbs_code = 2511L,
      unit = "ha",
      value = c(40, 50, 50)[seq_len(n)]
    )
  )
}

.rb_value <- function(out, yr) {
  out$value[out$year == yr]
}

testthat::test_that("residue grows back along its crop's production", {
  out <- whep:::.backcast_crop_residues(
    .rb_residue(),
    .rb_primary(),
    1959:1961,
    method = "production_ratio"
  )

  testthat::expect_setequal(out$year, 1959:1960)
  testthat::expect_equal(.rb_value(out, 1959), 1000 * 50 / 100)
  testthat::expect_equal(.rb_value(out, 1960), 1000 * 80 / 100)
  # Dry matter keeps the first year's content.
  testthat::expect_equal(out$value_dm, 0.9 * out$value)
  testthat::expect_equal(unique(out$method_residue), "pin")
})

testthat::test_that("area_rate grows residue along harvested area", {
  out <- whep:::.backcast_crop_residues(
    .rb_residue(),
    .rb_primary(),
    1959:1961,
    method = "area_rate"
  )

  testthat::expect_equal(.rb_value(out, 1959), 1000 * 40 / 50)
  testthat::expect_equal(.rb_value(out, 1960), 1000 * 50 / 50)
})

testthat::test_that("own_method grows cereal residue along its estimator", {
  # The first-year cereal row was computed with `calculate_crop_residues()`
  # from production and area, so it grows along that estimator applied to the
  # earlier year's crop: neither production nor area alone.
  out <- whep:::.backcast_crop_residues(
    .rb_residue(method = "ipcc"),
    .rb_primary(),
    1959:1961,
    method = "own_method"
  )
  crops <- .rb_primary() |>
    tidyr::pivot_wider(names_from = "unit", values_from = "value") |>
    dplyr::transmute(
      year,
      area_code,
      item_prod_code = "15",
      production_t = tonnes,
      area_ha = ha,
      region_hanpp = whep::regions_full$region_HANPP[
        whep::regions_full$code %in% 203L
      ][[1]]
    )
  est <- whep::calculate_crop_residues(crops, method = "ipcc")$residue_dm_t

  testthat::expect_equal(.rb_value(out, 1959), 1000 * est[1] / est[3])
  testthat::expect_equal(.rb_value(out, 1960), 1000 * est[2] / est[3])
  # A yield-dependent estimator: lower-yield years carry more residue per
  # tonne than the production ratio gives, and less per hectare than the area
  # rate gives.
  testthat::expect_gt(.rb_value(out, 1959), 1000 * 50 / 100)
  testthat::expect_lt(.rb_value(out, 1959), 1000 * 40 / 50)
})

testthat::test_that("own_method grows a pin residue along production", {
  own <- whep:::.backcast_crop_residues(
    .rb_residue(method = "pin"),
    .rb_primary(),
    1959:1961,
    method = "own_method"
  )
  ratio <- whep:::.backcast_crop_residues(
    .rb_residue(method = "pin"),
    .rb_primary(),
    1959:1961,
    method = "production_ratio"
  )
  testthat::expect_equal(own, ratio)
})

testthat::test_that("a year without the crop gets no residue", {
  primary <- .rb_primary() |>
    dplyr::filter(!(.data$year == 1959L))
  out <- whep:::.backcast_crop_residues(
    .rb_residue(),
    primary,
    1959:1961,
    method = "production_ratio"
  )

  testthat::expect_identical(as.integer(out$year), 1960L)
})

testthat::test_that("residue with no crop to grow along is reported", {
  # Production reports no crop for this row in the first year, so there is no
  # ratio to carry back. The row is left out and said out loud.
  residues <- dplyr::bind_rows(.rb_residue(), .rb_residue(crop = 2514L))

  testthat::expect_message(
    out <- whep:::.backcast_crop_residues(
      residues,
      .rb_primary(),
      1959:1961,
      method = "production_ratio"
    ),
    class = "whep_residue_backcast"
  )
  testthat::expect_setequal(out$item_cbs_code_crop, 2511L)
})

testthat::test_that("nothing is back-cast when the build starts at the pin", {
  out <- whep:::.backcast_crop_residues(
    .rb_residue(),
    .rb_primary(),
    1961:1962
  )

  testthat::expect_identical(nrow(out), 0L)
})

testthat::test_that("a code production lacks is read through its bucket", {
  # The pin books Sudan on 276 from 1961; production carries the former
  # Sudan, 206, which 276 resolves to.
  out <- whep:::.backcast_crop_residues(
    .rb_residue(area = 276L, polity_area = 206L),
    .rb_primary(area = 206L),
    1959:1961,
    method = "production_ratio"
  )

  testthat::expect_equal(.rb_value(out, 1959), 500)
  testthat::expect_equal(unique(out$area_code), 276L)
})

testthat::test_that("Rest of World grows along its members' crops", {
  members <- whep:::.rest_of_world_member_codes()
  primary <- dplyr::bind_rows(
    .rb_primary(area = members[[1]]),
    .rb_primary(area = members[[2]]) |>
      dplyr::mutate(value = 3 * .data$value)
  )
  row <- .rb_residue(area = 999L, polity_area = 999L)

  summed <- whep:::.backcast_crop_residues(
    row,
    primary,
    1959:1961,
    method = "production_ratio",
    aggregate_proxy = "member_sum"
  )
  testthat::expect_equal(.rb_value(summed, 1959), 1000 * 200 / 400)

  testthat::expect_message(
    none <- whep:::.backcast_crop_residues(
      row,
      primary,
      1959:1961,
      method = "production_ratio",
      aggregate_proxy = "none"
    ),
    class = "whep_residue_backcast"
  )
  testthat::expect_identical(nrow(none), 0L)
})

testthat::test_that("the back-cast reaches the CBS as a closed balance", {
  out <- whep:::.cbs_residue_backcast(
    .rb_residue(),
    .rb_primary(),
    1959:1961
  )
  wide <- out |>
    tidyr::pivot_wider(names_from = "element", values_from = "value")

  testthat::expect_setequal(wide$year, 1959:1960)
  testthat::expect_equal(wide$production, wide$feed + wide$other_uses)
  testthat::expect_equal(
    unique(out$source),
    "historical_residue_own_method"
  )
  # Recovered at the 1961 rate, so production is the same share of the gross
  # residue in every year.
  first <- whep:::.residue_cbs_elements(.rb_residue())
  rate <- first$value[first$element == "production"] / 1000
  testthat::expect_equal(
    wide$production[wide$year == 1959L],
    rate * 1000 * 50 / 100
  )
})

# -- Through the historical extension ----------------------------------------

# Spain's 1961 straw as the CBS carries it, recovered and split, plus the
# proxies the destiny fills read.
.rb_cbs_raw0 <- function() {
  tibble::tibble(
    year = 1961L,
    area = "Spain",
    area_code = 203L,
    item_cbs = "Straw",
    item_cbs_code = 2105L,
    element = c("production", "feed", "other_uses"),
    value = c(700, 31.5, 668.5),
    source = "FAOSTAT_prod"
  )
}

.rb_hist_inputs <- function(residue_backcast = NULL) {
  list(
    primary_cbs_area = tibble::tibble(
      year = integer(),
      area_code = integer(),
      item_cbs = character(),
      item_cbs_code = integer(),
      area_ha = numeric()
    ),
    gdp_pop = tibble::tibble(
      year = 1959:1961,
      area = "Spain",
      area_code = "ESP",
      pop = c(10, 11, 12)
    ),
    land_areas_wide = tibble::tibble(
      year = 1959:1961,
      area_code = 203L,
      Cropland = 1,
      Pasture = 0,
      agriland = c(8, 9, 10)
    ),
    residue_backcast = residue_backcast
  )
}

.rb_straw_backcast <- function() {
  tibble::tibble(
    year = rep(1959:1960, each = 3),
    area = "Spain",
    area_code = 203L,
    item_cbs = "Straw",
    item_cbs_code = 2105L,
    element = rep(c("production", "feed", "other_uses"), 2),
    source = "historical_residue_own_method",
    value = c(350, 15.75, 334.25, 560, 25.2, 534.8)
  )
}

testthat::test_that("pre-1961 residue use has production behind it", {
  # The whep#1508 defect: uses grown along population and land with no
  # production, so the whole use was a draw on stocks.
  result <- whep:::.cbs_extend_historical(
    .rb_cbs_raw0(),
    .rb_hist_inputs(.rb_straw_backcast()),
    1959:1961
  )
  pre <- result |>
    dplyr::filter(.data$year < 1961L) |>
    dplyr::select("year", "element", "value", "source") |>
    tidyr::pivot_wider(
      names_from = "element",
      values_from = c("value", "source")
    )

  testthat::expect_equal(pre$value_production, c(350, 560))
  testthat::expect_equal(
    pre$value_production,
    pre$value_feed + pre$value_other_uses
  )
  testthat::expect_true(all(
    pre$source_production == "historical_residue_own_method"
  ))
  testthat::expect_true(all(
    pre$source_feed == "historical_residue_own_method"
  ))
})

testthat::test_that("residue without a back-cast keeps the destiny fill", {
  # An area whose crops production does not reach before 1961 (whep#102) has
  # nothing to grow its residue along; its uses are filled like its crops'.
  result <- whep:::.cbs_extend_historical(
    .rb_cbs_raw0(),
    .rb_hist_inputs(),
    1959:1961
  )
  pre <- result |>
    dplyr::filter(.data$year < 1961L, .data$element == "other_uses")

  testthat::expect_true(all(pre$value > 0))
  testthat::expect_true(all(pre$source == "historical_fill"))
})

testthat::test_that("build_commodity_balances() validates residue_backcast", {
  testthat::expect_error(
    whep::build_commodity_balances(residue_backcast = "nope", example = TRUE),
    class = "rlang_error"
  )
})
