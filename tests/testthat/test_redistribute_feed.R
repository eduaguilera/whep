test_that("redistribute_feed rejects missing required demand columns", {
  bad <- whep:::.example_feed_demand() |> dplyr::select(-demand_dm_t)
  expect_error(
    whep::redistribute_feed(bad, whep:::.example_feed_avail()),
    "demand_dm_t"
  )
})

test_that("redistribute_feed rejects non-logical fixed_demand", {
  bad <- whep:::.example_feed_demand() |> dplyr::mutate(fixed_demand = "yes")
  expect_error(
    whep::redistribute_feed(bad, whep:::.example_feed_avail()),
    "fixed_demand"
  )
})

test_that("exact provincial item match allocates without exceeding availability", {
  out <- whep::redistribute_feed(
    whep:::.example_feed_demand() |> dplyr::filter(item_cbs_code == 2555L),
    whep:::.example_feed_avail() |> dplyr::filter(item_cbs_code == 2555L)
  )
  expect_equal(sum(out$intake_dm_t), 50, tolerance = 1e-6)
  expect_true(all(out$intake_dm_t >= 0))
})

test_that("fixed_demand=TRUE guarantees demand met via grassland sink", {
  d <- whep:::.example_feed_demand() |> dplyr::mutate(fixed_demand = TRUE)
  out <- whep::redistribute_feed(d, whep:::.example_feed_avail())
  met <- out |>
    dplyr::summarise(
      intake = sum(intake_dm_t),
      .by = c(livestock_category, sub_territory)
    ) |>
    dplyr::inner_join(
      d |>
        dplyr::summarise(
          dem = sum(demand_dm_t),
          .by = c(livestock_category, sub_territory)
        ),
      by = c("livestock_category", "sub_territory")
    )
  expect_true(all(abs(met$intake - met$dem) < 1e-6))
})

test_that("fixed_demand=FALSE never exceeds total availability", {
  d <- whep:::.example_feed_demand() |> dplyr::mutate(fixed_demand = FALSE)
  out <- whep::redistribute_feed(d, whep:::.example_feed_avail())
  expect_lte(
    sum(out$intake_dm_t),
    sum(whep:::.example_feed_avail()$avail_dm_t) + 1e-6
  )
})

test_that("variable surplus distribution lifts intake above demand", {
  # Abundant cereal supply (national) far exceeds demand. After the primary
  # item-exact pass meets the 30 t demand, the level-5 surplus distribution
  # must hand the leftover cereal availability to demand rows proportionally,
  # so realised intake exceeds the original demand.
  d <- tibble::tribble(
    ~year, ~territory, ~sub_territory, ~livestock_category, ~item_cbs_code,
    ~feed_group, ~feed_quality, ~demand_dm_t, ~fixed_demand,
    2000L, "ESP", "prov_a", "pigs", 2514L, "cereals", "high_quality", 30, FALSE
  )
  a <- tibble::tribble(
    ~year, ~territory, ~sub_territory, ~item_cbs_code, ~feed_group,
    ~feed_quality, ~avail_dm_t, ~feed_scale,
    2000L, "ESP", "prov_a", 2514L, "cereals", "high_quality", 500, "national"
  )
  out <- whep::redistribute_feed(d, a)
  expect_gt(sum(out$intake_dm_t), 30 + 1e-6)
  expect_lte(sum(out$intake_dm_t), 500 + 1e-6)
})

test_that("priority pool substitutes leftover availability for unmet demand", {
  # Demand is for an item/group/quality with no direct match in availability.
  # Only the priority-pool sweep (level 5) can satisfy it from the unrelated
  # leftover supply. Primary levels 1-3 leave it entirely unmet.
  d <- tibble::tribble(
    ~year, ~territory, ~sub_territory, ~livestock_category, ~item_cbs_code,
    ~feed_group, ~feed_quality, ~demand_dm_t, ~fixed_demand,
    2000L, "ESP", "prov_a", "cattle", 2999L, "roots", "low_quality", 40, TRUE
  )
  a <- tibble::tribble(
    ~year, ~territory, ~sub_territory, ~item_cbs_code, ~feed_group,
    ~feed_quality, ~avail_dm_t, ~feed_scale,
    2000L, "ESP", "prov_a", 2514L, "cereals", "high_quality", 100, "provincial"
  )
  out <- whep::redistribute_feed(d, a)
  expect_equal(sum(out$intake_dm_t), 40, tolerance = 1e-6)
  pool <- out |> dplyr::filter(hierarchy_level == "5_all_substitute")
  expect_gt(sum(pool$intake_dm_t), 1e-6)
  expect_true(all(pool$item_cbs_code == 2514L))
})

test_that("inter-provincial trade moves provincial feed across compartments", {
  # Cattle in prov_a demand a provincial item available only in prov_b
  # (same territory). Only level-4 inter-provincial trade can satisfy it.
  d <- tibble::tribble(
    ~year, ~territory, ~sub_territory, ~livestock_category, ~item_cbs_code,
    ~feed_group, ~feed_quality, ~demand_dm_t, ~fixed_demand,
    2000L, "ESP", "prov_a", "cattle", 2514L, "cereals", "high_quality", 40, FALSE
  )
  a <- tibble::tribble(
    ~year, ~territory, ~sub_territory, ~item_cbs_code, ~feed_group,
    ~feed_quality, ~avail_dm_t, ~feed_scale,
    2000L, "ESP", "prov_b", 2514L, "cereals", "high_quality", 100, "provincial"
  )
  out <- whep::redistribute_feed(
    d |> dplyr::mutate(fixed_demand = TRUE),
    a
  )
  traded <- out |> dplyr::filter(hierarchy_level == "4_inter_prov_trade")
  expect_gt(sum(traded$intake_dm_t), 1e-6)
  expect_true(all(traded$source_compartment == "prov_b"))
})

test_that("output has the locked column schema and non-negative scaling", {
  out <- whep::redistribute_feed(
    whep:::.example_feed_demand(),
    whep:::.example_feed_avail()
  )
  expect_named(
    out,
    c(
      "year",
      "territory",
      "sub_territory",
      "livestock_category",
      "item_cbs_code",
      "feed_group",
      "feed_quality",
      "demand_dm_t",
      "intake_dm_t",
      "scaling_factor",
      "hierarchy_level",
      "requested_item",
      "source_compartment",
      "fixed_demand"
    ),
    ignore.order = TRUE
  )
  # NA marks demand=0 & intake>0 (documented); use na.rm to allow that case.
  expect_true(all(out$scaling_factor >= 0, na.rm = TRUE))
})

test_that("per-item max_intake_share cap reroutes excess", {
  # Demand and availability share `territory` so the national cereal is
  # actually allocated (the shared `.example_feed_avail()` carries no
  # `territory` column and routes everything to the grassland sink, which
  # would never exercise an item cap). Cereal is abundant, so absent a cap
  # the pig diet is ~75% cereal; the 0.5 cap on item 2514 must reroute the
  # excess to grass, dropping the cereal share to <= 0.5 of total intake.
  d <- tibble::tribble(
    ~year, ~territory, ~sub_territory, ~livestock_category, ~item_cbs_code,
    ~feed_group, ~feed_quality, ~demand_dm_t, ~fixed_demand,
    2000L, "ESP", "prov_a", "pigs", 2514L, "cereals", "high_quality", 100, FALSE
  )
  a <- tibble::tribble(
    ~year, ~territory, ~sub_territory, ~item_cbs_code, ~feed_group,
    ~feed_quality, ~avail_dm_t, ~feed_scale,
    2000L, "ESP", "prov_a", 2514L, "cereals", "high_quality", 300, "national",
    2000L, "ESP", "prov_a", 2555L, "grass", "grass", 100, "provincial"
  )
  caps <- tibble::tribble(
    ~livestock_category, ~var, ~var_value, ~max_intake_share,
    "pigs", "item_cbs_code", "2514", 0.5
  )
  out <- whep::redistribute_feed(d, a, options = list(max_intake_share = caps))
  pig_cereal <- out |>
    dplyr::filter(livestock_category == "pigs", item_cbs_code == 2514L)
  total_pig <- out |>
    dplyr::filter(livestock_category == "pigs") |>
    dplyr::summarise(t = sum(intake_dm_t)) |>
    dplyr::pull(t)
  expect_lte(sum(pig_cereal$intake_dm_t), 0.5 * total_pig + 1e-6)
})

test_that("per-feed_quality max_intake_share cap is honoured", {
  # Matched-territory variable-mode scenario where grass would otherwise be
  # 50% of the cattle diet; the 0.4 cap on feed_quality == "grass" must hold.
  d <- tibble::tribble(
    ~year, ~territory, ~sub_territory, ~livestock_category, ~item_cbs_code,
    ~feed_group, ~feed_quality, ~demand_dm_t, ~fixed_demand,
    2000L, "ESP", "prov_a", "cattle", 2555L, "grass", "grass", 100, FALSE
  )
  a <- tibble::tribble(
    ~year, ~territory, ~sub_territory, ~item_cbs_code, ~feed_group,
    ~feed_quality, ~avail_dm_t, ~feed_scale,
    2000L, "ESP", "prov_a", 2555L, "grass", "grass", 200, "provincial",
    2000L, "ESP", "prov_a", 2514L, "cereals", "high_quality", 200, "national"
  )
  caps <- tibble::tribble(
    ~livestock_category, ~var, ~var_value, ~max_intake_share,
    "cattle", "feed_quality", "grass", 0.4
  )
  out <- whep::redistribute_feed(d, a, options = list(max_intake_share = caps))
  cattle_grass <- out |>
    dplyr::filter(livestock_category == "cattle", feed_quality == "grass")
  total_cattle <- out |>
    dplyr::filter(livestock_category == "cattle") |>
    dplyr::summarise(t = sum(intake_dm_t)) |>
    dplyr::pull(t)
  expect_lte(sum(cattle_grass$intake_dm_t), 0.4 * total_cattle + 1e-6)
})

# Behavioural parity against afsetools::redistribute_feed (the reference
# implementation this port reproduces) is intentionally NOT run in the suite.
# afsetools::load_general_data() reads Codes_coefs.xlsx via openxlsx, which
# segfaults intermittently on R 4.5.x and would crash the whole test run, and
# afsetools is not a CI dependency (the test would skip on CI regardless). The
# behavioural tests above assert the port's allocation outcomes (exact match,
# both demand modes, schema, and item + feed_quality caps). Run an afsetools
# cross-check manually offline if needed.

test_that(".cap_grass_to_availability bounds pasture grass at the polity supply", {
  result <- tibble::tibble(
    year = 2000L,
    territory = "A",
    feed_quality = "grass",
    item_cbs_code = NA_integer_,
    intake_dm_t = c(60, 40)
  )
  ga <- tibble::tibble(year = 2000L, area_code = "A", grass_avail_dm_t = 60)
  capped <- whep:::.cap_grass_to_availability(result, ga)
  expect_equal(sum(capped$intake_dm_t), 60, tolerance = 1e-9)
  expect_equal(sort(capped$intake_dm_t), c(24, 36), tolerance = 1e-9)
})

test_that(".cap_grass_to_availability leaves grass under the ceiling untouched", {
  result <- tibble::tibble(
    year = 2000L,
    territory = "A",
    feed_quality = "grass",
    item_cbs_code = NA_integer_,
    intake_dm_t = c(30, 20)
  )
  ga <- tibble::tibble(year = 2000L, area_code = "A", grass_avail_dm_t = 100)
  capped <- whep:::.cap_grass_to_availability(result, ga)
  expect_equal(sum(capped$intake_dm_t), 50, tolerance = 1e-9)
})

test_that(".cap_grass_to_availability binds grass per cell (sub_territory)", {
  # Provincial grain: grass availability carries sub_territory, so each cell's
  # pasture grass is bounded by its OWN ceiling, not the polity total.
  result <- tibble::tibble(
    year = 2000L,
    territory = "ES",
    sub_territory = c("cellA", "cellA", "cellB"),
    feed_quality = "grass",
    item_cbs_code = NA_integer_,
    intake_dm_t = c(60, 40, 50)
  )
  ga <- tibble::tibble(
    year = 2000L,
    sub_territory = c("cellA", "cellB"),
    grass_avail_dm_t = c(50, 100)
  )
  capped <- whep:::.cap_grass_to_availability(result, ga)
  a <- capped$intake_dm_t[capped$sub_territory == "cellA"]
  b <- capped$intake_dm_t[capped$sub_territory == "cellB"]
  # cellA (demand 100) capped at its own 50, pro-rata across its two rows.
  expect_equal(sum(a), 50, tolerance = 1e-9)
  expect_equal(sort(a), c(20, 30), tolerance = 1e-9)
  # cellB (demand 50) is under its 100 ceiling and untouched.
  expect_equal(sum(b), 50, tolerance = 1e-9)
})

test_that(".cap_grass_to_availability keeps polities sharing a cell id apart", {
  # A 0.5-degree border cell ("c1") belongs to two polities; with territory in
  # the key each is capped against its OWN ceiling, not the conflated sum.
  result <- tibble::tibble(
    year = 2000L,
    territory = c("ES", "FR"),
    sub_territory = "c1",
    feed_quality = "grass",
    item_cbs_code = NA_integer_,
    intake_dm_t = c(1000, 1000)
  )
  ga <- tibble::tibble(
    year = 2000L,
    territory = c("ES", "FR"),
    sub_territory = "c1",
    grass_avail_dm_t = c(100, 100)
  )
  capped <- whep:::.cap_grass_to_availability(result, ga)
  expect_equal(
    capped$intake_dm_t[capped$territory == "ES"],
    100,
    tolerance = 1e-9
  )
  expect_equal(
    capped$intake_dm_t[capped$territory == "FR"],
    100,
    tolerance = 1e-9
  )
})

test_that(".cap_grass_to_availability leaves cells absent from supply unbounded", {
  # A cell with grazing demand but no grass-supply row is left unbounded, not
  # capped to zero (a data gap must not silently delete grazing intake).
  result <- tibble::tibble(
    year = 2000L,
    territory = "ES",
    sub_territory = c("cellA", "cellB"),
    feed_quality = "grass",
    item_cbs_code = NA_integer_,
    intake_dm_t = c(60, 50)
  )
  ga <- tibble::tibble(
    year = 2000L,
    sub_territory = "cellA",
    grass_avail_dm_t = 30
  )
  capped <- whep:::.cap_grass_to_availability(result, ga)
  expect_equal(
    capped$intake_dm_t[capped$sub_territory == "cellA"],
    30,
    tolerance = 1e-9
  )
  expect_equal(
    capped$intake_dm_t[capped$sub_territory == "cellB"],
    50,
    tolerance = 1e-9
  )
})

test_that("grass_availability bounds the pasture grass sink in redistribute_feed", {
  d <- whep:::.example_feed_demand()
  a <- whep:::.example_feed_avail()
  base <- whep::redistribute_feed(d, a)
  is_sink <- base$hierarchy_level == "6_grassland_unlimited"
  base_grass <- sum(base$intake_dm_t[is_sink], na.rm = TRUE)
  skip_if(base_grass <= 1e-6, "fixture does not use the unlimited grass sink")
  ga <- base[is_sink, ] |>
    dplyr::summarise(
      grass_avail_dm_t = sum(intake_dm_t, na.rm = TRUE) / 2,
      .by = c(year, territory)
    )
  capped <- whep::redistribute_feed(
    d,
    a,
    options = list(grass_availability = ga)
  )
  capped_grass <- sum(
    capped$intake_dm_t[capped$hierarchy_level == "6_grassland_unlimited"],
    na.rm = TRUE
  )
  expect_lt(capped_grass, base_grass)
  expect_lte(capped_grass, base_grass / 2 + 1e-6)
})

# Helper: a single grazer with a pasture-grass intake row, in territory A.
.grass_cascade_result <- function(intake = 40) {
  tibble::tibble(
    demand_id = 1L,
    year = 2000L,
    territory = "A",
    sub_territory = "A",
    livestock_category = "cattle",
    item_cbs_code = NA_integer_,
    feed_group = "grass",
    feed_quality = "grass",
    intake_dm_t = intake,
    hierarchy_level = "6_grassland_unlimited",
    requested_item = NA_integer_,
    source_compartment = "A",
    avail_id = NA_integer_
  )
}

test_that(".redistribute_grass_deficit fills the deficit from leftover, capped at it", {
  deficit <- tibble::tibble(
    demand_id = 1L,
    year = 2000L,
    territory = "A",
    sub_territory = "A",
    livestock_category = "cattle",
    reduction = 60
  )
  live_avail <- tibble::tibble(
    year = 2000L,
    sub_territory = "A",
    territory = "A",
    feed_quality = "crops",
    avail_remaining = 25
  )
  out <- whep:::.redistribute_grass_deficit(
    .grass_cascade_result(),
    deficit,
    live_avail
  )
  sub <- out[out$hierarchy_level == "7_grass_deficit_substitute", ]
  expect_equal(nrow(sub), 1L)
  expect_equal(sub$intake_dm_t, 25, tolerance = 1e-9)
  expect_equal(sub$feed_quality, "substitute")
})

test_that(".redistribute_grass_deficit caps the fill at the deficit, not the leftover", {
  deficit <- tibble::tibble(
    demand_id = 1L,
    year = 2000L,
    territory = "A",
    sub_territory = "A",
    livestock_category = "cattle",
    reduction = 60
  )
  live_avail <- tibble::tibble(
    year = 2000L,
    sub_territory = "A",
    territory = "A",
    feed_quality = "crops",
    avail_remaining = 100
  )
  out <- whep:::.redistribute_grass_deficit(
    .grass_cascade_result(),
    deficit,
    live_avail
  )
  sub <- out[out$hierarchy_level == "7_grass_deficit_substitute", ]
  expect_equal(sub$intake_dm_t, 60, tolerance = 1e-9)
})

test_that(".redistribute_grass_deficit ignores leftover grass availability", {
  deficit <- tibble::tibble(
    demand_id = 1L,
    year = 2000L,
    territory = "A",
    sub_territory = "A",
    livestock_category = "cattle",
    reduction = 60
  )
  live_avail <- tibble::tibble(
    year = 2000L,
    sub_territory = "A",
    territory = "A",
    feed_quality = "grass",
    avail_remaining = 100
  )
  out <- whep:::.redistribute_grass_deficit(
    .grass_cascade_result(),
    deficit,
    live_avail
  )
  expect_false(any(out$hierarchy_level == "7_grass_deficit_substitute"))
})

test_that(".grass_deficit_diagnosis flags demand rows below maintenance", {
  result <- tibble::tibble(demand_id = c(1L, 2L), intake_dm_t = c(30, 80))
  demand <- tibble::tibble(
    demand_id = c(1L, 2L),
    livestock_category = c("cattle", "sheep"),
    demand_dm_t = c(100, 100)
  )
  diag <- whep:::.grass_deficit_diagnosis(
    result,
    demand,
    maintenance_share = 0.5
  )
  expect_equal(diag$demand_id, 1L)
  expect_equal(diag$maintenance_dm_t, 50, tolerance = 1e-9)
  expect_equal(diag$intake_dm_t, 30, tolerance = 1e-9)
})

test_that("redistribute_feed attaches a grass_deficit_diagnosis with maintenance_share", {
  d <- whep:::.example_feed_demand()
  a <- whep:::.example_feed_avail()
  base <- whep::redistribute_feed(d, a)
  is_sink <- base$hierarchy_level == "6_grassland_unlimited"
  skip_if(
    sum(base$intake_dm_t[is_sink], na.rm = TRUE) <= 1e-6,
    "fixture does not use the unlimited grass sink"
  )
  ga <- base[is_sink, ] |>
    dplyr::summarise(
      grass_avail_dm_t = sum(intake_dm_t, na.rm = TRUE) / 4,
      .by = c(year, territory)
    )
  out <- whep::redistribute_feed(
    d,
    a,
    options = list(grass_availability = ga, maintenance_share = 0.95)
  )
  diag <- attr(out, "grass_deficit_diagnosis")
  expect_false(is.null(diag))
  expect_s3_class(diag, "tbl_df")
})
