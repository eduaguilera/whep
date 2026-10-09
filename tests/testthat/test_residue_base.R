# The gross residue base `get_primary_residues()` builds from the pin
# (whep#1448).

# Wirsenius (2000) Table 3.16 residue:product ratio, as shipped.
wirsenius_ratio <- function(cat, region) {
  rates <- whep::whep_coef_table("residue_recovery")
  rates$residue_dm_product_dm[
    rates$cat_krausmann == cat & rates$region_krausmann == region
  ]
}

# The legacy recovery rate the pin's residue rows carry (whep#1195).
legacy_rate <- function(cat, region) {
  rates <- whep::whep_coef_table("residue_recovery")
  rates$recovery_rates[
    rates$cat_krausmann == cat & rates$region_krausmann == region
  ]
}

biomass_coef <- function(name, column) {
  coefs <- whep::biomass_coefs
  coefs[[column]][coefs$Name_biomass == name][[1]]
}

# A pin of residue rows plus the `Product` rows cereal residue is recomputed
# from. Areas are given by their canonical names, as the pin does.
residue_pin <- function(residue, product = NULL) {
  dplyr::bind_rows(
    dplyr::mutate(residue, Product_residue = "Residue"),
    if (!is.null(product)) dplyr::mutate(product, Product_residue = "Product")
  ) |>
    dplyr::mutate(Year = 2000L)
}

wheat_rows <- function(area, value) {
  tibble::tibble(
    Area = area,
    Item_prod = "Wheat",
    Item_cbs = "Straw",
    Item_cbs_crop = "Wheat and products",
    Name_biomass = "Wheat",
    Prod_ygpit_Mg = value
  )
}

wheat_product <- function(area, tonnes, hectares) {
  dplyr::mutate(
    wheat_rows(area, tonnes),
    Item_cbs = "Wheat and products",
    Area_ygpit_ha = hectares
  )
}

testthat::test_that("the pin's ratio follows Wirsenius's region membership", {
  # whep#1430, whep#1448. The predecessor looked the regional ratio up by the
  # HANPP label, which files Russia and Belarus under South & Central Asia;
  # Wirsenius (2000) Table 3.1 has them in East Europe. Pulses carry 0.4 there
  # under the HANPP label and 1.0 in East Europe.
  pea <- tibble::tibble(
    Area = c("Russian Federation", "Spain"),
    Item_prod = "Peas, dry",
    Item_cbs = "Straw",
    Item_cbs_crop = "Peas",
    Name_biomass = "Pea",
    Prod_ygpit_Mg = c(90, 70)
  )
  local_mocked_bindings(whep_read_file = function(name, ...) {
    residue_pin(pea)
  })

  out <- whep::get_primary_residues()

  russia <- out[out$area_code == 185L, ]
  spain <- out[out$area_code == 203L, ]
  # The recovery is undone on the HANPP region the pin was WRITTEN with ...
  gross <- 90 / legacy_rate("Pulses", "South and Central Asia")
  # ... and only then is the ratio re-keyed on Wirsenius's own region.
  testthat::expect_equal(
    russia$value,
    gross *
      wirsenius_ratio("Pulses", "East Europe") /
      wirsenius_ratio("Pulses", "South and Central Asia")
  )
  testthat::expect_equal(spain$value, 70 / legacy_rate("Pulses", "West Europe"))
  testthat::expect_equal(unique(out$method_residue), "pin")
})

testthat::test_that("cereal residue is estimated from production and area", {
  # whep#1448. The pin's cereal ratio (Wirsenius's early-1990s regional
  # ratios on a mis-anchored base) put world cereal residue 16% above
  # Smerald et al. (2023). Cereal residue is now WHEP's own estimator,
  # `calculate_crop_residues()`, run on the pin's production and area, so the
  # pin's residue figure no longer matters for a cereal.
  pin <- residue_pin(
    wheat_rows("Spain", 999),
    wheat_product("Spain", 1000, 400)
  )
  local_mocked_bindings(whep_read_file = function(name, ...) pin)

  out <- whep::get_primary_residues()

  expected <- whep::calculate_crop_residues(
    tibble::tibble(
      item_prod_code = "15",
      production_t = 1000,
      area_ha = 400,
      year = 2000L,
      region_hanpp = "West Europe"
    ),
    method = "ipcc"
  )$residue_dm_t
  testthat::expect_equal(out$value_dm, expected)
  testthat::expect_equal(
    out$value,
    expected / biomass_coef("Wheat", "Residue_kgDM_kgFM")
  )
  testthat::expect_equal(out$item_cbs_code_residue, 2105)
  testthat::expect_equal(out$method_residue, "ipcc")

  # The pin's own cereal residue figure does not enter.
  pin$Prod_ygpit_Mg[pin$Product_residue == "Residue"] <- 5
  local_mocked_bindings(whep_read_file = function(name, ...) pin)
  testthat::expect_equal(whep::get_primary_residues()$value_dm, expected)
})

testthat::test_that("every cereal method is selectable and recorded", {
  pin <- residue_pin(
    wheat_rows("Spain", 999),
    wheat_product("Spain", 1000, 400)
  )
  local_mocked_bindings(whep_read_file = function(name, ...) pin)
  x <- tibble::tibble(
    item_prod_code = "15",
    production_t = 1000,
    area_ha = 400,
    year = 2000L,
    region_hanpp = "West Europe"
  )

  for (method in c("ipcc", "ensemble", "ratio")) {
    out <- whep::get_primary_residues(cereal_residue = method)
    testthat::expect_equal(
      out$value_dm,
      whep::calculate_crop_residues(x, method = method)$residue_dm_t
    )
    testthat::expect_equal(out$method_residue, method)
  }
  testthat::expect_error(
    whep::get_primary_residues(cereal_residue = "smerald"),
    class = "rlang_error"
  )
})

testthat::test_that("the wirsenius method applies Table 3.16 directly", {
  # whep#1448. The predecessor scaled the `biomass_coefs` ratio by the
  # region's Wirsenius ratio over West Europe's, which presumes
  # `biomass_coefs` holds the West Europe ratio; for wheat it holds 1.34 kg DM
  # per kg DM against Wirsenius's 1.0. Applied directly, residue dry matter
  # is product dry matter times the Wirsenius ratio of the area's own
  # Wirsenius region, times the pin's harvest-index factor.
  product_fm <- 1000
  hi_factor <- 1.05
  hanpp <- "South and Central Asia"
  pinned <- product_fm *
    biomass_coef("Wheat", "kg_residue_kg_product_FM") *
    hi_factor *
    wirsenius_ratio("Wheat, other cereals", hanpp) /
    wirsenius_ratio("Wheat, other cereals", "West Europe") *
    legacy_rate("Wheat, other cereals", hanpp)
  local_mocked_bindings(whep_read_file = function(name, ...) {
    residue_pin(
      wheat_rows("Russian Federation", pinned),
      wheat_product("Russian Federation", product_fm, 400)
    )
  })

  out <- whep::get_primary_residues(cereal_residue = "wirsenius")

  testthat::expect_equal(
    out$value_dm,
    product_fm *
      biomass_coef("Wheat", "Product_kgDM_kgFM") *
      wirsenius_ratio("Wheat, other cereals", "East Europe") *
      hi_factor
  )
  testthat::expect_equal(out$method_residue, "wirsenius")
})

testthat::test_that("a cereal without production or area is refused", {
  # Without its production row, or with no harvested area, a cereal's
  # residue would be estimated as zero and dropped: an absent input read as
  # no residue. The sum still reconciles on such an input, which is why the
  # guard is needed.
  no_area <- residue_pin(
    wheat_rows("Spain", 999),
    wheat_product("Spain", 1000, 0)
  )
  vacuous <- whep::calculate_crop_residues(
    tibble::tibble(item_prod_code = "15", production_t = 1000, area_ha = 0),
    method = "ipcc"
  )$residue_dm_t
  local_mocked_bindings(whep_read_file = function(name, ...) no_area)
  expect_supplied_guard(
    identity = isTRUE(all.equal(vacuous, 0)),
    guard = whep::get_primary_residues(),
    class = "whep_residue_cereal_no_product"
  )

  local_mocked_bindings(whep_read_file = function(name, ...) {
    residue_pin(wheat_rows("Spain", 999))
  })
  testthat::expect_error(
    whep::get_primary_residues(),
    class = "whep_residue_cereal_no_product"
  )

  local_mocked_bindings(whep_read_file = function(name, ...) {
    dplyr::select(
      residue_pin(wheat_rows("Spain", 9), wheat_product("Spain", 10, 4)),
      -"Area_ygpit_ha"
    )
  })
  testthat::expect_error(
    whep::get_primary_residues(),
    class = "whep_residue_cereal_no_product"
  )
})

testthat::test_that("a cereal split over two residue items is refused", {
  split <- dplyr::bind_rows(
    wheat_rows("Spain", 5),
    dplyr::mutate(wheat_rows("Spain", 5), Item_cbs = "Other crop residues")
  )
  local_mocked_bindings(whep_read_file = function(name, ...) {
    residue_pin(split, wheat_product("Spain", 1000, 400))
  })
  testthat::expect_error(
    whep::get_primary_residues(),
    class = "whep_residue_cereal_items"
  )
})

testthat::test_that("the Wirsenius region keeps every other HANPP label", {
  region <- whep:::.residue_wirsenius_region(
    c(
      "South and Central Asia",
      "South and Central Asia",
      "South and Central Asia",
      "South and Central Asia",
      "West Europe"
    ),
    c(
      "South-eastern Asia",
      "Eastern Europe",
      "Southern Asia",
      NA,
      "Southern Europe"
    )
  )
  testthat::expect_equal(
    region,
    c(
      "East Asia",
      "East Europe",
      "South and Central Asia",
      "East Europe",
      "West Europe"
    )
  )
  # Every area of `regions_full` reaches one of the eight table regions.
  regions <- whep:::.residue_area_regions()
  testthat::expect_true(all(
    regions$region_wirsenius %in%
      whep::whep_coef_table("residue_recovery")$region_krausmann
  ))
})
