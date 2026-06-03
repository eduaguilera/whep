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
