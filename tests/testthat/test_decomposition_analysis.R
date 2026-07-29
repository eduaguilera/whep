# .filter_analysis_years
test_that("years after 2021 are dropped from the analysis panel", {
  panel <- tibble::tribble(
    ~year, ~value,
    2020, 1,
    2021, 2,
    2022, 3
  )

  out <- .filter_analysis_years(panel)

  expect_equal(out$year, c(2020, 2021))
})

# .national_area_panel
test_that("national area panel sums area/inputs/surplus across finer grain", {
  panel <- tibble::tribble(
    ~year, ~province_name, ~destiny_grp, ~area, ~inputs, ~surplus,
    2000, "A", "feed", 40, 100, 20,
    2000, "A", "domestic_food", 20, 50, 10,
    2000, "B", "feed", 10, 30, 5
  )

  out <- .national_area_panel(panel)

  expect_equal(out$total_area, 70)
  expect_equal(out$inputs, 180)
  expect_equal(out$surplus, 35)
})

# .simple_area_identity
test_that("simple area identity uses Size/Intensity/Inefficiency labels", {
  identity <- .simple_area_identity("Cropland N surplus")

  expect_equal(
    identity$formula,
    "surplus:total_area*(inputs/total_area)*(surplus/inputs)"
  )
  expect_equal(
    identity$labels,
    c("Cropland N surplus", "Size", "Intensity", "Inefficiency")
  )
})

# .period_average_panel
test_that("period averaging keeps only reference years and averages within them", {
  panel <- tibble::tribble(
    ~year, ~province_name, ~value,
    1860, "A", 10,
    1870, "A", 20,
    1900, "A", 999, # outside all four reference periods, dropped
    1920, "A", 100
  )

  out <- .period_average_panel(panel, "province_name")

  expect_equal(sort(out$year), c(1860, 1920))
  expect_equal(out$value[out$year == 1860], mean(c(10, 20)))
  expect_equal(out$value[out$year == 1920], 100)
})

# .reference_period_pairs
test_that("reference period pairs chain each period against the previous one", {
  out <- .reference_period_pairs()

  expect_equal(out$t0, c(1860, 1920, 1960, 1860))
  expect_equal(out$t_t, c(1920, 1960, 2010, 2010))
})

# .relabel_period_transitions
test_that("transition labels are replaced with the mean-year comparison", {
  df <- tibble::tribble(
    ~period, ~value,
    "1860-1920", 1,
    "1920-1960", 2,
    "1960-2010", 3,
    "1860-2010", 4
  )

  out <- .relabel_period_transitions(df)

  expect_equal(
    as.character(out$period),
    c("1865-1925", "1925-1965", "1965-2015", "Total (1865-2015)")
  )
  expect_equal(
    levels(out$period),
    c("1865-1925", "1925-1965", "1965-2015", "Total (1865-2015)")
  )
})

# .aggregate_period_series
test_that("period series sums contributions per transition without cumulating", {
  detail <- tibble::tribble(
    ~period, ~compartment, ~component_type, ~additive, ~period_years,
    "1860-1920", "cropland", "target", 10, 60,
    "1860-1920", "manure", "target", -4, 60,
    "1920-1960", "cropland", "target", 25, 40
  )

  out <- .aggregate_period_series(detail, "compartment", target_only = TRUE)

  expect_equal(
    out$contribution_mgn[
      out$period == "1865-1925" & out$compartment == "cropland"
    ],
    10
  )
  expect_equal(
    out$contribution_per_yr_mgn[
      out$period == "1865-1925" & out$compartment == "cropland"
    ],
    10 / 60
  )
  expect_equal(
    out$contribution_mgn[
      out$period == "1865-1925" & out$compartment == "manure"
    ],
    -4
  )
  expect_equal(
    out$contribution_mgn[
      out$period == "1925-1965" & out$compartment == "cropland"
    ],
    25
  )
})

# .destiny_group
test_that(".destiny_group maps raw destinies to the four buckets", {
  out <- .destiny_group(c(
    "population_food",
    "population_other_uses",
    "livestock_rum",
    "livestock_mono",
    "export"
  ))

  expect_equal(
    out,
    c("domestic_food", "non_food", "feed", "feed", "exported")
  )
})

# .crop_output_by_destiny + .crop_item_destiny_shares
test_that("crop destiny shares are computed proportionally per item", {
  n_prov_destiny <- tibble::tribble(
    ~year, ~province_name, ~item, ~origin, ~destiny, ~mg_n,
    2000, "A", "Wheat", "Cropland", "population_food", 60,
    2000, "A", "Wheat", "Cropland", "livestock_rum", 40,
    2000, "A", "Barley", "Cropland", "export", 10
  )

  shares <- .crop_output_by_destiny(n_prov_destiny) |>
    .crop_item_destiny_shares()

  wheat_food <- shares |>
    dplyr::filter(item == "Wheat", destiny_grp == "domestic_food") |>
    dplyr::pull(share)
  wheat_feed <- shares |>
    dplyr::filter(item == "Wheat", destiny_grp == "feed") |>
    dplyr::pull(share)
  barley_export <- shares |>
    dplyr::filter(item == "Barley", destiny_grp == "exported") |>
    dplyr::pull(share)

  expect_equal(wheat_food, 0.6)
  expect_equal(wheat_feed, 0.4)
  expect_equal(barley_export, 1)
})

# .allocate_by_destiny_share
test_that("inputs are allocated to destinies using item shares", {
  shares <- tibble::tribble(
    ~year, ~province_name, ~item, ~destiny_grp, ~share, ~output_mg,
    2000, "A", "Wheat", "domestic_food", 0.6, 60,
    2000, "A", "Wheat", "feed", 0.4, 40
  )
  item_inputs <- tibble::tribble(
    ~year, ~province_name, ~item, ~input_mg,
    2000, "A", "Wheat", 100
  )

  out <- .allocate_by_destiny_share(item_inputs, shares, "input_mg")

  expect_equal(
    out$allocated[out$destiny_grp == "domestic_food"],
    60
  )
  expect_equal(out$allocated[out$destiny_grp == "feed"], 40)
})

# .assemble_cropland_panel: closure and completion of missing destinies
test_that("cropland panel computes surplus and fills missing destinies", {
  inputs_pu <- tibble::tribble(
    ~year, ~province_name, ~destiny_grp, ~inputs,
    2000, "A", "domestic_food", 100
  )
  area_pu <- tibble::tribble(
    ~year, ~province_name, ~destiny_grp, ~area,
    2000, "A", "domestic_food", 10
  )
  outputs_pu <- tibble::tribble(
    ~year, ~province_name, ~destiny_grp, ~outputs,
    2000, "A", "domestic_food", 60
  )

  panel <- .assemble_cropland_panel(inputs_pu, area_pu, outputs_pu)

  expect_equal(nrow(panel), 4) # domestic_food, feed, exported, non_food
  expect_equal(
    panel$surplus[panel$destiny_grp == "domestic_food"],
    100 - 60
  )
  expect_equal(panel$surplus[panel$destiny_grp == "feed"], 0)
  expect_equal(unique(panel$total_area), 10)
})

# .cropland_area_surplus_units
test_that("cropland area/surplus units compute area share and per-ha surplus", {
  panel <- tibble::tribble(
    ~year, ~province_name, ~destiny_grp, ~area, ~surplus, ~total_area,
    2000, "A", "domestic_food", 40, 400, 100,
    2000, "A", "feed", 20, 100, 100,
    2000, "B", "domestic_food", 40, 200, 100
  )

  units <- .cropland_area_surplus_units(panel, "province_name")

  expect_equal(units$w[units$province_name == "A"], 0.6)
  expect_equal(units$s[units$province_name == "A"], (400 + 100) / 60)
  expect_equal(units$w[units$province_name == "B"], 0.4)
})

# .manure_species_units
test_that("manure species units compute herd share and loss per LU", {
  panel <- tibble::tribble(
    ~year, ~livestock_cat, ~herd_lu, ~loss, ~herd_total,
    2000, "Pigs", 200, 40, 1000,
    2000, "Cattle", 800, 80, 1000
  )

  units <- .manure_species_units(panel)

  expect_equal(units$w[units$livestock_cat == "Pigs"], 0.2)
  expect_equal(units$s[units$livestock_cat == "Pigs"], 40 / 200)
})

# .olley_pakes_covariance
test_that("covariance is positive when weight concentrates on the high-value unit", {
  units <- tibble::tribble(
    ~year, ~w, ~s,
    2000, 0.6, 10,
    2000, 0.4, 5
  )

  out <- .olley_pakes_covariance(units)

  # w_mean = 0.5, s_mean = 7.5
  # cov = (0.6-0.5)*(10-7.5) + (0.4-0.5)*(5-7.5) = 0.25 + 0.25 = 0.5
  expect_equal(out$covariance, 0.5)
})

# .local_feed_self_sufficiency
test_that("feed self-sufficiency is the local share of total feed", {
  n_prov_destiny <- tibble::tribble(
    ~year, ~province_name, ~origin, ~destiny, ~mg_n,
    2000, "A", "Cropland", "livestock_rum", 60,
    2000, "A", "Outside", "livestock_mono", 40
  )

  out <- .local_feed_self_sufficiency(n_prov_destiny)

  expect_equal(out$total_feed, 100)
  expect_equal(out$local_feed, 60)
  expect_equal(out$self_sufficiency, 0.6)
})

# .manure_recycling_ratio
test_that("manure recycling ratio is manure's share of total land N inputs", {
  n_prov_destiny <- tibble::tribble(
    ~year, ~province_name, ~origin, ~destiny, ~mg_n,
    2000, "A", "Livestock", "Cropland", 30,
    2000, "A", "Synthetic", "Cropland", 70
  )

  out <- .manure_recycling_ratio(n_prov_destiny)

  expect_equal(out$total_n, 100)
  expect_equal(out$manure_n, 30)
  expect_equal(out$recycling_ratio, 0.3)
})

# .crop_livestock_connectivity_panel
test_that("connectivity panel joins self-sufficiency and recycling ratio", {
  n_prov_destiny <- tibble::tribble(
    ~year, ~province_name, ~origin, ~destiny, ~mg_n,
    2000, "A", "Cropland", "livestock_rum", 60,
    2000, "A", "Outside", "livestock_mono", 40,
    2000, "A", "Livestock", "Cropland", 30,
    2000, "A", "Synthetic", "Cropland", 70
  )

  out <- .crop_livestock_connectivity_panel(n_prov_destiny)

  expect_equal(out$self_sufficiency, 0.6)
  expect_equal(out$recycling_ratio, 0.3)
})

# .assemble_semi_natural_panel
test_that("semi-natural panel computes surplus and national total area", {
  inputs_p <- tibble::tribble(
    ~year, ~province_name, ~inputs,
    2000, "A", 50,
    2000, "B", 30
  )
  outputs_p <- tibble::tribble(
    ~year, ~province_name, ~outputs,
    2000, "A", 20
  )
  area_p <- tibble::tribble(
    ~year, ~province_name, ~area,
    2000, "A", 100,
    2000, "B", 50
  )

  panel <- .assemble_semi_natural_panel(inputs_p, outputs_p, area_p)

  expect_equal(panel$surplus[panel$province_name == "A"], 50 - 20)
  expect_equal(panel$surplus[panel$province_name == "B"], 30 - 0)
  expect_equal(unique(panel$total_area), 150)
})

# .national_livestock_lu
test_that("national livestock LU sums stock across provinces and converts to LU", {
  stock_prod_ygps <- tibble::tribble(
    ~Year, ~Province_name, ~Livestock_cat, ~Item, ~Stock_number,
    2000, "A", "Pigs", "Meat", 100,
    2000, "A", "Pigs", "Live", 100, # duplicate stock_number, must be deduped
    2000, "B", "Pigs", "Meat", 50,
    2000, "A", "Pets", "Live", 10
  )
  livestock_units <- tibble::tribble(
    ~Livestock_cat, ~Lu_head, ~System,
    "Pigs", 0.5, "monogastric",
    "Pets", 0.1, "monogastric"
  )

  out <- .national_livestock_lu(stock_prod_ygps, livestock_units)

  expect_equal(out$herd_lu[out$livestock_cat == "Pigs"], (100 + 50) * 0.5)
  expect_false("Pets" %in% out$livestock_cat)
})

# .national_feed_n
test_that("national feed N sums intake_MgN across provinces per species", {
  intake_ygiac <- tibble::tribble(
    ~Year, ~Province_name, ~Livestock_cat, ~intake_MgN,
    2000, "A", "Pigs", 100,
    2000, "B", "Pigs", 50,
    2000, "A", "Pets", 10
  )

  out <- .national_feed_n(intake_ygiac)

  expect_equal(out$feed_n[out$livestock_cat == "Pigs"], 150)
  expect_false("Pets" %in% out$livestock_cat)
})

# .build_manure_panel
test_that("species without a livestock-unit coefficient are dropped, not zero-filled", {
  stock_prod_ygps <- tibble::tribble(
    ~Year, ~Province_name, ~Livestock_cat, ~Item, ~Stock_number,
    2000, "A", "Pigs", "Meat", 100
  )
  livestock_units <- tibble::tribble(
    ~Livestock_cat, ~Lu_head, ~System,
    "Pigs", 0.5, "monogastric"
  )
  intake_ygiac <- tibble::tribble(
    ~Year, ~Province_name, ~Livestock_cat, ~intake_MgN,
    2000, "A", "Pigs", 100,
    2000, "A", "Fur animals", 30 # no LU coefficient above
  )
  n_excretion_ygs <- tibble::tribble(
    ~Year, ~Province_name, ~Livestock_cat, ~N_excr_MgN,
    2000, "A", "Pigs", 40,
    2000, "A", "Fur animals", 12
  )

  panel <- .build_manure_panel(
    n_prov_destiny = tibble::tribble(
      ~year, ~origin, ~destiny, ~mg_n,
      2000, "Livestock", "Cropland", 20
    ),
    intake_ygiac,
    n_excretion_ygs,
    stock_prod_ygps,
    livestock_units
  )

  expect_false("Fur animals" %in% panel$livestock_cat)
  expect_equal(panel$feed_n[panel$livestock_cat == "Pigs"], 100)
})

# .finalize_manure_panel
test_that("manure loss fraction and herd total are computed nationally", {
  panel <- tibble::tribble(
    ~year, ~livestock_cat, ~herd_lu, ~feed_n, ~excr_n,
    2000, "Pigs", 100, 200, 80,
    2000, "Cattle", 200, 400, 120
  )
  applied <- tibble::tribble(
    ~year, ~applied,
    2000, 150
  )

  out <- .finalize_manure_panel(panel, applied)

  # excr_total = 200, applied = 150 -> loss_frac = 50/200 = 0.25
  expect_equal(unique(out$loss_frac), 0.25)
  expect_equal(out$loss[out$livestock_cat == "Pigs"], 80 * 0.25)
  expect_equal(unique(out$herd_total), 300)
})

# .national_manure_panel
test_that("national manure panel collapses species into national totals", {
  panel <- tibble::tribble(
    ~year, ~livestock_cat, ~herd_lu, ~feed_n, ~excr_n, ~herd_total, ~excr_total, ~loss_frac,
    2000, "Pigs", 100, 200, 80, 300, 200, 0.25,
    2000, "Cattle", 200, 400, 120, 300, 200, 0.25
  )

  out <- .national_manure_panel(panel)

  expect_equal(nrow(out), 1)
  expect_equal(out$feed_total, 600)
  expect_equal(out$herd_total, 300)
  expect_equal(out$excr_total, 200)
  expect_equal(out$loss, 200 * 0.25)
})

# .assemble_urban_panel
test_that("urban panel computes per-capita loss correctly", {
  excr_h <- tibble::tribble(~year, ~excr_h, 2000, 100)
  recycled <- tibble::tribble(~year, ~recycled, 2000, 40)
  pop <- tibble::tribble(~year, ~population, 2000, 10)

  out <- .assemble_urban_panel(excr_h, recycled, pop)

  expect_equal(out$excr_pc, 10)
  expect_equal(out$loss_frac, 0.6)
  expect_equal(out$loss, 60)
})

# .warn_if_sign_change
test_that("sign changes across years trigger a warning", {
  panel <- tibble::tribble(
    ~year, ~surplus,
    2000, 10,
    2001, -5
  )

  expect_warning(
    .warn_if_sign_change(panel, surplus, character(0), "Test surplus"),
    "sign changes"
  )
  expect_no_warning(
    .warn_if_sign_change(
      tibble::tribble(~year, ~surplus, 2000, 10, 2001, 20),
      surplus,
      character(0),
      "Test surplus"
    )
  )
})

# .tag_mechanism
test_that("factor rows are tagged with their mechanism and targets as Total", {
  decomp_df <- tibble::tribble(
    ~factor_label, ~component_type,
    "Size", "factor",
    "Feed intensity", "factor",
    "Cropland N surplus", "target"
  )

  out <- .tag_mechanism(decomp_df)

  expect_equal(out$mechanism[out$factor_label == "Size"], "Size")
  expect_equal(out$mechanism[out$factor_label == "Feed intensity"], "Intensification")
  expect_equal(out$mechanism[out$component_type == "target"], "Total")
})

# .cumulate_series
test_that("contributions are cumulated over time within each group", {
  detail <- tibble::tribble(
    ~t0, ~compartment, ~component_type, ~additive,
    2000, "cropland", "target", 10,
    2001, "cropland", "target", 5,
    2000, "semi_natural", "target", -2,
    2001, "semi_natural", "target", 3
  )

  out <- .cumulate_series(detail, "compartment", target_only = TRUE)

  crop <- out |> dplyr::filter(compartment == "cropland") |> dplyr::arrange(t0)
  expect_equal(crop$cumulative_mgn, c(10, 15))
  semi_nat <- out |> dplyr::filter(compartment == "semi_natural") |> dplyr::arrange(t0)
  expect_equal(semi_nat$cumulative_mgn, c(-2, 1))
})
