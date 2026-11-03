# Tests for build_carbon_inputs() (Module B, Task B2c-4): assemble the
# per-land-use-class carbon-input layer build_carbon_balance() consumes, by
# aggregating the per-crop cropland inputs (build_soil_carbon_inputs) to the
# cropland class and row-binding the grassland and natural inputs
# (build_grass_natural_carbon_inputs). All densities in MgC/ha/yr.

# -- Fixtures -----------------------------------------------------------------

# Per-crop cropland soil carbon inputs (build_soil_carbon_inputs grid output):
# two crops in one cell, differing per-ha C and humified fraction.
.ci_cropland_fixture <- function() {
  tibble::tribble(
    ~lon, ~lat, ~area_code, ~item_prod_code, ~year,
    ~residue_c_mgc_ha_yr, ~root_c_mgc_ha_yr, ~manure_c_mgc_ha_yr,
    ~total_c_input_mgc_ha_yr, ~humified_fraction, ~method_c_input,
    ~crop_area_ha,
    0.25, 0.25, 1L, "15", 2000L, 1.5, 1.0, 0.5, 3.0, 0.20, "humified_weighted",
    30,
    0.25, 0.25, 1L, "27", 2000L, 1.0, 0.5, 0.5, 2.0, 0.10, "humified_weighted",
    10
  )
}

# Per cell-crop harvested area (ha), used to area-weight the crop densities up
# to the cropland class. Static (time-invariant), matching the crop_patterns
# spatialization, so it carries no year key.
.ci_crop_area_fixture <- function() {
  tibble::tribble(
    ~lon, ~lat, ~area_code, ~item_prod_code, ~crop_area_ha,
    0.25, 0.25, 1L, "15", 30,
    0.25, 0.25, 1L, "27", 10
  )
}

# Grassland + natural class inputs (build_grass_natural_carbon_inputs grid).
.ci_grass_natural_fixture <- function() {
  tibble::tribble(
    ~lon, ~lat, ~area_code, ~year, ~land_use,
    ~c_input_mgc_ha_yr, ~humified_fraction, ~method_c_input,
    0.25, 0.25, 1L, 2000L, "grassland", 4.0, 0.1153467, "lpjml_npp_minus_harvest",
    0.25, 0.25, 1L, 2000L, "natural", 6.0, 0.325, "lpjml_npp_minus_harvest"
  )
}

.ci_test_data <- function() {
  list(
    cropland = .ci_cropland_fixture(),
    crop_area = .ci_crop_area_fixture(),
    grass_natural = .ci_grass_natural_fixture()
  )
}


# The single-cropland-class path, named explicitly. These tests predate crop
# groups and check the collapse of per-crop densities to ONE cropland class;
# the package default is now `rotation_groups`, which resolves each crop's
# irrigated share from the pinned spatialization inputs and so cannot run in
# an offline test that injects only `cropland`. Tests of the grouped path
# live in test_carbon_inputs_groups.R, where the share layer is injected.
.ci_single <- function(...) {
  whep::build_carbon_inputs(..., crop_groups = list(method = "none"))
}

# -- Cropland aggregation -----------------------------------------------------

test_that("cropland class = area-weighted per-ha C, C-weighted humification", {
  out <- .ci_single(resolution = "grid", data = .ci_test_data())
  crop <- dplyr::filter(out, land_use == "cropland")
  # Area-weighted per-ha C: (3.0*30 + 2.0*10) / (30+10) = 110/40 = 2.75.
  testthat::expect_equal(crop$c_input_mgc_ha_yr, 2.75, tolerance = 1e-9)
  # C-weighted humified fraction: weight by carbon MASS (C/ha * area):
  # mass15 = 3.0*30 = 90, mass27 = 2.0*10 = 20. hf = (90*0.2 + 20*0.1)/110.
  expected_hf <- (90 * 0.20 + 20 * 0.10) / 110
  testthat::expect_equal(crop$humified_fraction, expected_hf, tolerance = 1e-9)
})

test_that("year-varying crop areas weight only their matching input year", {
  cropland <- tidyr::expand_grid(
    lon = 0.25,
    lat = 0.25,
    area_code = 1L,
    item_prod_code = c("15", "27"),
    year = c(2000L, 2001L)
  ) |>
    dplyr::mutate(
      total_c_input_mgc_ha_yr = dplyr::if_else(
        .data$item_prod_code == "15",
        1,
        9
      ),
      humified_fraction = 0.2
    )
  crop_area <- tibble::tribble(
    ~lon, ~lat, ~area_code, ~item_prod_code, ~year, ~crop_area_ha,
    0.25, 0.25, 1L, "15", 2000L, 90,
    0.25, 0.25, 1L, "27", 2000L, 10,
    0.25, 0.25, 1L, "15", 2001L, 10,
    0.25, 0.25, 1L, "27", 2001L, 90
  )

  out <- whep:::.ci_cropland_class(cropland, crop_area, basis = "static") |>
    dplyr::arrange(.data$year)

  testthat::expect_equal(out$c_input_mgc_ha_yr, c(1.8, 8.2))
  testthat::expect_equal(out$class_area_ha, c(100, 100))
})

test_that("output carries every class with the balance c_inputs schema", {
  out <- .ci_single(resolution = "grid", data = .ci_test_data())
  pointblank::expect_col_exists(
    out,
    c(
      "lon",
      "lat",
      "area_code",
      "year",
      "land_use",
      "c_input_mgc_ha_yr",
      "humified_fraction"
    )
  )
  testthat::expect_setequal(
    unique(out$land_use),
    c("cropland", "grassland", "natural")
  )
  # Grassland + natural pass through build_grass_natural values unchanged.
  gr <- dplyr::filter(out, land_use == "grassland")
  testthat::expect_equal(gr$c_input_mgc_ha_yr, 4.0, tolerance = 1e-9)
})

test_that("build_carbon_inputs output joins into build_carbon_balance", {
  # The assembled c_inputs must satisfy build_carbon_balance's join contract:
  # (lon, lat, area_code, year, land_use) with c_input_mgc_ha_yr +
  # humified_fraction. Build a matching land-use + climate + clay and run.
  c_inputs <- .ci_single(
    resolution = "grid",
    data = .ci_test_data()
  )
  land_use <- tibble::tribble(
    ~lon, ~lat, ~area_code, ~year, ~land_use, ~area_ha,
    0.25, 0.25, 1L, 2000L, "cropland", 40,
    0.25, 0.25, 1L, 2000L, "grassland", 30,
    0.25, 0.25, 1L, 2000L, "natural", 30
  )
  climate <- tibble::tibble(
    lon = 0.25,
    lat = 0.25,
    area_code = 1L,
    year = 2000L,
    climate_modifier = 1
  )
  clay <- tibble::tibble(lon = 0.25, lat = 0.25, clay_pct = 20)
  cb <- whep::build_carbon_balance(
    model = "hsoc",
    resolution = "grid",
    data = list(
      c_inputs = c_inputs,
      land_use = land_use,
      climate = climate,
      clay = clay
    )
  )
  testthat::expect_true(all(cb$stock_mgc_ha >= 0))
  testthat::expect_setequal(
    unique(cb$land_use),
    c("cropland", "grassland", "natural")
  )
})

test_that("polity resolution aggregates cropland conserving carbon mass", {
  grid <- .ci_single(resolution = "grid", data = .ci_test_data())
  pol <- .ci_single(
    resolution = "polity",
    data = .ci_test_data()
  )
  # Single cell so the cropland per-ha density is identical at both grains.
  grid_crop <- dplyr::filter(grid, land_use == "cropland")$c_input_mgc_ha_yr
  pol_crop <- dplyr::filter(pol, land_use == "cropland")$c_input_mgc_ha_yr
  testthat::expect_equal(pol_crop, grid_crop, tolerance = 1e-9)
  testthat::expect_false(rlang::has_name(pol, "lon"))
})

# Two cropland cells in one polity: a cropland-heavy cell (90 ha) with a low
# per-ha density (1.0) and a cropland-light cell (10 ha) with a high per-ha
# density (4.0). One crop per cell, so the cell density equals its crop density
# and the cell cropland area equals its crop area. A plain (unweighted) mean
# would return (1.0 + 4.0) / 2 = 2.5 and overstate the polity carbon input; the
# correct area-weighted density is (1.0*90 + 4.0*10) / 100 = 1.3.
.ci_two_cell_data <- function() {
  cropland <- tibble::tribble(
    ~lon, ~lat, ~area_code, ~item_prod_code, ~year,
    ~total_c_input_mgc_ha_yr, ~humified_fraction, ~method_c_input,
    ~crop_area_ha,
    0.25, 0.25, 1L, "15", 2000L, 1.0, 0.20, "humified_weighted", 90,
    0.75, 0.25, 1L, "27", 2000L, 4.0, 0.10, "humified_weighted", 10
  )
  crop_area <- tibble::tribble(
    ~lon, ~lat, ~area_code, ~item_prod_code, ~crop_area_ha,
    0.25, 0.25, 1L, "15", 90,
    0.75, 0.25, 1L, "27", 10
  )
  grass_natural <- tibble::tribble(
    ~lon, ~lat, ~area_code, ~year, ~land_use,
    ~c_input_mgc_ha_yr, ~humified_fraction, ~method_c_input,
    0.25, 0.25, 1L, 2000L, "grassland", 4.0, 0.1153467, "lpjml_npp_minus_harvest",
    0.75, 0.25, 1L, 2000L, "grassland", 2.0, 0.1153467, "lpjml_npp_minus_harvest"
  )
  land_use <- tibble::tribble(
    ~lon, ~lat, ~area_code, ~year, ~land_use, ~area_ha,
    0.25, 0.25, 1L, 2000L, "grassland", 90,
    0.75, 0.25, 1L, 2000L, "grassland", 10
  )
  list(
    cropland = cropland,
    crop_area = crop_area,
    grass_natural = grass_natural,
    land_use = land_use
  )
}

test_that("polity cropland density is area-weighted, not a plain mean", {
  pol <- .ci_single(
    resolution = "polity",
    data = .ci_two_cell_data()
  )
  crop <- dplyr::filter(pol, land_use == "cropland")
  # Area-weighted: (1.0*90 + 4.0*10) / (90+10) = 130/100 = 1.3, not mean 2.5.
  testthat::expect_equal(crop$c_input_mgc_ha_yr, 1.3, tolerance = 1e-9)
  # Total cropland carbon mass is conserved: density * total area == sum of the
  # per-cell density * cell area over the grid.
  grid <- .ci_single(
    resolution = "grid",
    data = .ci_two_cell_data()
  )
  grid_crop <- dplyr::filter(grid, land_use == "cropland")
  grid_mass <- sum(grid_crop$c_input_mgc_ha_yr * c(90, 10))
  testthat::expect_equal(
    crop$c_input_mgc_ha_yr * 100,
    grid_mass,
    tolerance = 1e-9
  )
  # Humification fraction is carbon-mass-weighted (mass = density * area):
  # masses 90 and 40, so (0.20*90 + 0.10*40)/130 = 22/130, not mean 0.15.
  testthat::expect_equal(crop$humified_fraction, 22 / 130, tolerance = 1e-9)
})

test_that("polity grassland density uses its land-use area", {
  pol <- .ci_single(
    resolution = "polity",
    data = .ci_two_cell_data()
  )
  grass <- dplyr::filter(pol, .data$land_use == "grassland")

  testthat::expect_equal(
    grass$c_input_mgc_ha_yr,
    (4 * 90 + 2 * 10) / 100,
    tolerance = 1e-9
  )
  testthat::expect_false(isTRUE(all.equal(grass$c_input_mgc_ha_yr, 3)))
})

test_that("example = TRUE returns the documented schema", {
  out <- whep::build_carbon_inputs(example = TRUE)
  pointblank::expect_col_exists(
    out,
    c(
      "lon",
      "lat",
      "area_code",
      "year",
      "land_use",
      "c_input_mgc_ha_yr",
      "humified_fraction",
      "method_c_input"
    )
  )
  testthat::expect_true(all(
    out$land_use %in% c("cropland", "grassland", "natural")
  ))
})

# ---- density basis: which area weights the per-crop densities ----------------

.db_cropland <- function(with_area = TRUE) {
  x <- tibble::tibble(
    lon = 0.25,
    lat = 0.25,
    area_code = 1L,
    item_prod_code = c("15", "27"),
    year = 2010L,
    total_c_input_mgc_ha_yr = c(3.25, 2.75),
    humified_fraction = c(0.2, 0.1),
    method_c_input = "humified_weighted"
  )
  if (with_area) {
    x$crop_area_ha <- c(100, 300)
  }
  x
}

.db_static_area <- function() {
  tibble::tibble(
    lon = 0.25,
    lat = 0.25,
    area_code = 1L,
    item_prod_code = c("15", "27"),
    crop_area_ha = c(300, 100)
  )
}

testthat::test_that("the static basis ignores the layer's own area", {
  # The per-crop layer now carries crop_area_ha; under the default basis it
  # must be dropped before the join, not suffixed into a second column.
  with <- whep:::.ci_cropland_class(
    .db_cropland(TRUE),
    .db_static_area(),
    basis = "static"
  )
  without <- whep:::.ci_cropland_class(
    .db_cropland(FALSE),
    .db_static_area(),
    basis = "static"
  )
  testthat::expect_identical(with, without)
  testthat::expect_equal(
    with$c_input_mgc_ha_yr,
    (3.25 * 300 + 2.75 * 100) / 400
  )
  testthat::expect_identical(with$method_area_basis, "static")
})

testthat::test_that("the renormalised basis weights by the layer's area", {
  out <- whep:::.ci_cropland_class(
    .db_cropland(TRUE),
    crop_area = NULL,
    basis = "renormalised"
  )
  testthat::expect_equal(
    out$c_input_mgc_ha_yr,
    (3.25 * 100 + 2.75 * 300) / 400
  )
  testthat::expect_equal(out$class_area_ha, 400)
  testthat::expect_identical(out$method_area_basis, "renormalised")
  # Mass-weighted humification follows the same weights.
  mass <- c(3.25 * 100, 2.75 * 300)
  testthat::expect_equal(
    out$humified_fraction,
    sum(c(0.2, 0.1) * mass) / sum(mass)
  )
})

testthat::test_that("the renormalised basis refuses a layer without an area", {
  testthat::expect_error(
    whep:::.ci_cropland_class(
      .db_cropland(FALSE),
      crop_area = NULL,
      basis = "renormalised"
    ),
    "crop_area_ha"
  )
})

testthat::test_that("density_basis is validated and threaded from the builder", {
  testthat::expect_error(
    whep::build_carbon_inputs(density_basis = "faostat", example = TRUE),
    class = "rlang_error"
  )
  # The per-crop fixture carries the area the renormalised basis needs.
  testthat::expect_true(
    "crop_area_ha" %in% names(whep::build_soil_carbon_inputs(example = TRUE))
  )
  out <- .ci_single(
    data = list(
      cropland = .db_cropland(TRUE),
      grass_natural = tibble::tibble(
        lon = 0.25,
        lat = 0.25,
        area_code = 1L,
        year = 2010L,
        land_use = "natural",
        c_input_mgc_ha_yr = 1,
        humified_fraction = 0.3,
        method_c_input = "lpjml_npp"
      )
    ),
    density_basis = "renormalised"
  )
  crop <- out[out$land_use == "cropland", ]
  testthat::expect_equal(
    crop$c_input_mgc_ha_yr,
    (3.25 * 100 + 2.75 * 300) / 400
  )
  testthat::expect_identical(crop$method_area_basis, "renormalised")
  testthat::expect_true(is.na(out$method_area_basis[out$land_use == "natural"]))
  # And the polity roll-up keeps the stamp.
  pol <- .ci_single(
    resolution = "polity",
    data = list(
      cropland = .db_cropland(TRUE),
      grass_natural = tibble::tibble(
        lon = 0.25,
        lat = 0.25,
        area_code = 1L,
        year = 2010L,
        land_use = "natural",
        c_input_mgc_ha_yr = 1,
        humified_fraction = 0.3,
        method_c_input = "lpjml_npp"
      )
    ),
    density_basis = "renormalised"
  )
  testthat::expect_identical(
    pol$method_area_basis[pol$land_use == "cropland"],
    "renormalised"
  )
})

# -- An unknown crop C:N inside a class (#1123) -------------------------------

# Two crops in one cell, the second with no input C:N of its own. The class
# carbon masses are 3.25 x 100 and 2.75 x 300, so crop 15 carries 325 of the
# 1150 Mg C the class receives.
.ci_cn_cropland <- function(cn = c(30, NA)) {
  x <- .db_cropland(TRUE)
  x$input_cn <- cn
  x
}

testthat::test_that("one unknown crop C:N no longer voids the whole class", {
  out <- whep:::.ci_cropland_class(
    .ci_cn_cropland(),
    crop_area = NULL,
    basis = "renormalised"
  )
  # Formed from the crop that HAS a ratio, together with the carbon that
  # matched it -- the rule `.sci_sum_components()` applies one level down.
  testthat::expect_equal(out$input_cn, 30)
  testthat::expect_identical(out$method_input_cn, "known_crops")
})

testthat::test_that("both known crops weight the class C:N by carbon", {
  out <- whep:::.ci_cropland_class(
    .ci_cn_cropland(c(30, 10)),
    crop_area = NULL,
    basis = "renormalised"
  )
  mass <- c(3.25 * 100, 2.75 * 300)
  testthat::expect_equal(out$input_cn, sum(c(30, 10) * mass) / sum(mass))
})

testthat::test_that("method_input_cn = 'require_all' voids the class", {
  out <- whep:::.ci_cropland_class(
    .ci_cn_cropland(),
    crop_area = NULL,
    basis = "renormalised",
    method_input_cn = "require_all"
  )
  testthat::expect_true(is.na(out$input_cn))
  testthat::expect_identical(out$method_input_cn, "require_all")
})

testthat::test_that("a class with no known crop C:N stays unknown", {
  out <- whep:::.ci_cropland_class(
    .ci_cn_cropland(c(NA, NA)),
    crop_area = NULL,
    basis = "renormalised"
  )
  testthat::expect_true(is.na(out$input_cn))
})

testthat::test_that("method_input_cn is validated and reaches the output", {
  testthat::expect_error(
    whep::build_carbon_inputs(method_input_cn = "class_mean", example = TRUE),
    class = "rlang_error"
  )
  out <- .ci_single(data = .ci_test_data())
  testthat::expect_true("method_input_cn" %in% names(out))
  testthat::expect_identical(
    unique(out$method_input_cn[out$land_use == "cropland"]),
    "known_crops"
  )
  # Grassland and natural are not collapsed from crops, so they carry none.
  testthat::expect_true(all(is.na(out$method_input_cn[
    out$land_use != "cropland"
  ])))
})

testthat::test_that("method_input_cn survives the polity resolution", {
  pol <- .ci_single(resolution = "polity", data = .ci_test_data())
  testthat::expect_identical(
    pol$method_input_cn[pol$land_use == "cropland"],
    "known_crops"
  )
})

# -- Yield-weighted rainfed/irrigated split (issue #1233) ---------------------

# Fixtures in helper_regime_split.R. The crop rows and their identity key.
.rs_key <- c("lon", "lat", "area_code", "item_prod_code", "year")
.rs_comp <- c(
  "residue_c_mgc_ha_yr",
  "root_c_mgc_ha_yr",
  "weed_c_mgc_ha_yr",
  "manure_c_mgc_ha_yr",
  "total_c_input_mgc_ha_yr"
)

.rs_context <- function(data = .rs_carbon_data(), regime = "yield_split") {
  whep:::.ci_regime_context(
    data,
    years = NULL,
    whep:::.ci_group_config(list(regime = regime))
  )
}

# The per-crop parts, before the collapse to classes.
.rs_parts <- function(data = .rs_carbon_data(), regime = "yield_split") {
  suppressMessages(whep:::.ci_split_into_groups(
    data$cropland,
    data$crop_regime_share,
    .rs_context(data, regime)
  )) |>
    dplyr::mutate(irrigated = stringr::str_detect(.data$land_use, "irrig"))
}

.rs_unsplit_mass <- function(cropland = .rs_cropland()) {
  cropland |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(.rs_comp), \(v) v * .data$crop_area_ha)
    ) |>
    dplyr::select(dplyr::all_of(c(.rs_key, .rs_comp)))
}

.rs_part_mass <- function(parts) {
  parts |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(.rs_comp), \(v) v * .data$crop_area_ha)
    ) |>
    dplyr::summarise(
      dplyr::across(dplyr::all_of(.rs_comp), sum),
      .by = dplyr::all_of(.rs_key)
    )
}

.rs_rel_diff <- function(a, b) {
  max(abs(a - b) / pmax(abs(b), .Machine$double.xmin))
}

testthat::test_that("each regime part keeps the cell-crop's production", {
  data <- .rs_carbon_data()
  items <- suppressMessages(whep:::.ci_regime_items(
    data$cropland,
    data$crop_regime_share,
    .rs_context(data)
  ))
  split <- items[items$method_regime_split == "yield_ratio", ]
  testthat::expect_identical(nrow(split), 3L)
  part_production <- split$crop_area_ha *
    ((1 - split$irrigated_share) *
      split$yield_rainfed +
      split$irrigated_share * split$yield_irrigated)
  testthat::expect_lt(
    .rs_rel_diff(part_production, split$crop_area_ha * split$yield_mean),
    1e-12
  )
  testthat::expect_true(all(split$yield_irrigated > split$yield_rainfed))
})

testthat::test_that("with the irrigation terms neutral, residue and root C and N summed over parts equal the unsplit crop", {
  .rs_neutral_irrigation()
  data <- .rs_carbon_data()
  data$cropland <- .rs_cropland_with_n()
  parts <- .rs_parts(data)
  comp <- c(.rs_comp, "residue_n_mgn_ha_yr", "root_n_mgn_ha_yr")
  mass <- function(x) {
    x |>
      dplyr::mutate(
        dplyr::across(dplyr::all_of(comp), \(v) v * .data$crop_area_ha)
      ) |>
      dplyr::summarise(
        dplyr::across(dplyr::all_of(comp), sum),
        .by = dplyr::all_of(.rs_key)
      )
  }
  back <- mass(parts) |>
    dplyr::inner_join(
      mass(data$cropland),
      by = .rs_key,
      suffix = c("", "_unsplit"),
      relationship = "one-to-one"
    )
  testthat::expect_identical(nrow(back), nrow(.rs_cropland()))
  # The root cap does not bind on these yields: the model is affine in
  # yield there, and the parts keep the crop's production.
  for (col in comp) {
    testthat::expect_lt(
      .rs_rel_diff(back[[col]], back[[paste0(col, "_unsplit")]]),
      1e-10
    )
  }
  # Not vacuous: the irrigated wheat hectare carries more residue carbon than
  # the crop's mean hectare, the rainfed one less.
  wheat <- dplyr::filter(parts, .data$lon == 0.25, .data$item_prod_code == "15")
  testthat::expect_gt(wheat$residue_c_mgc_ha_yr[wheat$irrigated], 1.2)
  testthat::expect_lt(wheat$residue_c_mgc_ha_yr[!wheat$irrigated], 1.2)
})

testthat::test_that("the affine residue rule gives the irrigated part less than proportional scaling", {
  .rs_neutral_irrigation()
  data <- .rs_carbon_data()
  items <- suppressMessages(whep:::.ci_regime_items(
    data$cropland,
    data$crop_regime_share,
    .rs_context(data)
  ))
  items <- items[items$method_regime_split == "yield_ratio", ]
  parts <- .rs_parts(data)
  irrigated <- parts[parts$irrigated, ] |>
    dplyr::inner_join(
      dplyr::select(
        items,
        dplyr::all_of(.rs_key),
        "yield_mean",
        "yield_irrigated",
        residue_crop = "residue_c_mgc_ha_yr",
        root_crop = "root_c_mgc_ha_yr"
      ),
      by = .rs_key
    )
  testthat::expect_identical(nrow(irrigated), 3L)
  proportional <- irrigated$yield_irrigated / irrigated$yield_mean
  # Both residue terms rise with yield from a positive intercept, so an
  # above-mean hectare carries more than the crop's mean but less than its
  # yield share would give it.
  testthat::expect_true(all(
    irrigated$residue_c_mgc_ha_yr < irrigated$residue_crop * proportional
  ))
  testthat::expect_true(all(
    irrigated$residue_c_mgc_ha_yr > irrigated$residue_crop
  ))
  testthat::expect_true(all(
    irrigated$root_c_mgc_ha_yr < irrigated$root_crop * proportional
  ))
  # Weed and manure carbon are spread by area: the same density on both parts.
  for (col in c("weed_c_mgc_ha_yr", "manure_c_mgc_ha_yr")) {
    by_crop <- parts |>
      dplyr::summarise(
        spread = diff(range(.data[[col]])),
        .by = dplyr::all_of(.rs_key)
      )
    testthat::expect_true(all(by_crop$spread == 0))
  }
})

# One maize cell, half irrigated, R = 1.5, national yield 6 t/ha.
.rs_maize_data <- function() {
  data <- .rs_carbon_data()
  data$cropland <- .rs_cropland_with_n()[1, ] |>
    dplyr::mutate(
      lon = 1.25,
      item_prod_code = "56",
      crop_area_ha = 20
    )
  data$crop_regime_share <- tibble::tibble(
    lon = 1.25,
    lat = 40.25,
    area_code = 203L,
    item_prod_code = 56L,
    year = 2010L,
    irrigated_share = 0.5
  )
  data$regime_ratio <- dplyr::select(
    data$crop_regime_share,
    dplyr::all_of(.rs_key)
  ) |>
    dplyr::mutate(ratio_unbounded = 1.5)
  data
}

testthat::test_that("irrigated maize carbon carries the model's irrigation terms, worked by hand", {
  data <- .rs_maize_data()
  parts <- .rs_parts(data)
  crop <- data$cropland
  mean <- .rs_maize_by_hand(6)
  rainfed <- .rs_maize_by_hand(4.8)
  irrigated <- .rs_maize_by_hand(7.2, residue_factor = 0.9, rs_factor = 0.85)
  rf <- parts[!parts$irrigated, ]
  ir <- parts[parts$irrigated, ]
  testthat::expect_equal(rf$crop_area_ha, 10)
  testthat::expect_equal(ir$crop_area_ha, 10)
  testthat::expect_equal(
    c(rf$residue_c_mgc_ha_yr, ir$residue_c_mgc_ha_yr),
    crop$residue_c_mgc_ha_yr *
      c(rainfed$residue, irrigated$residue) /
      mean$residue,
    tolerance = 1e-12
  )
  testthat::expect_equal(
    c(rf$root_c_mgc_ha_yr, ir$root_c_mgc_ha_yr),
    crop$root_c_mgc_ha_yr * c(rainfed$root, irrigated$root) / mean$root,
    tolerance = 1e-12
  )
  # Nitrogen follows the same dry matter.
  testthat::expect_equal(
    ir$residue_n_mgn_ha_yr / crop$residue_n_mgn_ha_yr,
    ir$residue_c_mgc_ha_yr / crop$residue_c_mgc_ha_yr,
    tolerance = 1e-12
  )
  testthat::expect_equal(
    ir$root_n_mgn_ha_yr / crop$root_n_mgn_ha_yr,
    ir$root_c_mgc_ha_yr / crop$root_c_mgc_ha_yr,
    tolerance = 1e-12
  )
  # The irrigation terms take residue and root off irrigated land: the crop's
  # residue and root carbon fall against the unsplit crop.
  testthat::expect_lt(
    sum(parts$residue_c_mgc_ha_yr * parts$crop_area_ha),
    crop$residue_c_mgc_ha_yr * crop$crop_area_ha
  )
  testthat::expect_identical(
    c(rf$method_residue_regime, ir$method_residue_regime),
    c("part_yield", "part_yield_irrigation_terms")
  )
})

testthat::test_that("each part's input C:N comes from its own residue and root C and N", {
  data <- .rs_maize_data()
  parts <- .rs_parts(data)
  crop <- data$cropland
  # The carbon that carries a nitrogen is every component's but the weed's.
  c_with_n <- function(x) {
    x$residue_c_mgc_ha_yr + x$root_c_mgc_ha_yr + x$manure_c_mgc_ha_yr
  }
  n <- function(x) {
    x$residue_n_mgn_ha_yr + x$root_n_mgn_ha_yr + crop$manure_c_mgc_ha_yr / 12
  }
  testthat::expect_equal(
    parts$input_cn,
    c_with_n(parts) / n(parts),
    tolerance = 1e-12
  )
  testthat::expect_equal(
    parts$input_n_mgn_ha_yr,
    n(parts),
    tolerance = 1e-12
  )
  # Not vacuous: the two parts' ratios differ from each other and from the
  # crop's.
  testthat::expect_gt(abs(diff(parts$input_cn)), 1e-6)
  testthat::expect_true(all(abs(parts$input_cn - crop$input_cn) > 1e-6))
  testthat::expect_true(all(parts$method_regime_cn == "part_input_cn"))
  # A layer that does not carry the components' nitrogen keeps the crop's
  # ratio on both parts, and says so.
  data$cropland <- dplyr::select(
    data$cropland,
    -"residue_n_mgn_ha_yr",
    -"root_n_mgn_ha_yr",
    -"input_n_mgn_ha_yr"
  )
  bare <- .rs_parts(data)
  testthat::expect_identical(bare$input_cn, rep(crop$input_cn, 2))
  testthat::expect_true(all(bare$method_regime_cn == "crop_input_cn"))
})

testthat::test_that("each part's humified fraction follows its own mix", {
  h <- whep:::.sci_humification_lookup(whep::residue_humification)
  parts <- .rs_parts()
  parts_sum <- parts$residue_c_mgc_ha_yr +
    parts$root_c_mgc_ha_yr +
    parts$weed_c_mgc_ha_yr +
    parts$manure_c_mgc_ha_yr
  own_mix <- (parts$residue_c_mgc_ha_yr *
    h$crop_residue +
    parts$root_c_mgc_ha_yr * h$root +
    parts$weed_c_mgc_ha_yr * h$weed +
    parts$manure_c_mgc_ha_yr * h$manure) /
    parts_sum
  testthat::expect_lt(.rs_rel_diff(parts$humified_fraction, own_mix), 1e-12)
  # The part's total is the sum of its four components.
  testthat::expect_lt(
    .rs_rel_diff(parts$total_c_input_mgc_ha_yr, parts_sum),
    1e-12
  )
  # This layer carries no component nitrogen, so each part keeps its crop's
  # input C:N (a layer that does is tested above).
  crop <- .rs_cropland()
  at <- match(
    paste(parts$lon, parts$item_prod_code),
    paste(crop$lon, crop$item_prod_code)
  )
  testthat::expect_identical(parts$input_cn, crop$input_cn[at])
})

# The split rule of each crop row, one row per crop.
.rs_rules <- function(parts) {
  parts |>
    dplyr::distinct(
      .data$lon,
      .data$item_prod_code,
      .data$method_regime_split
    ) |>
    dplyr::arrange(.data$lon, .data$item_prod_code)
}

# Both parts of a crop carry the crop's own densities.
.rs_expect_equal_densities <- function(parts, lon, item) {
  crop <- parts[parts$lon == lon & parts$item_prod_code == item, ]
  unsplit <- .rs_cropland()
  unsplit <- unsplit[unsplit$lon == lon & unsplit$item_prod_code == item, ]
  testthat::expect_gt(nrow(crop), 0L)
  for (col in c(.rs_comp, "humified_fraction")) {
    testthat::expect_identical(crop[[col]], rep(unsplit[[col]], nrow(crop)))
  }
}

testthat::test_that("every crop row's split rule is stamped", {
  parts <- .rs_parts()
  rules <- .rs_rules(parts)
  testthat::expect_identical(
    rules$method_regime_split,
    # Ordered by cell, then by the item code as text.
    c(
      "yield_ratio", # wheat 15, s = 0.25
      "trivial_irrigated_only", # olives 260, s = 1
      "no_regime_share", # barley 44, no share row
      "trivial_rainfed_only", # maize 56, s = 0
      "yield_ratio", # wheat 15, s = 0.6
      "yield_ratio" # rice 27, s = 0.9
    )
  )
  # s = 0 and s = 1 keep one part. The wholly rainfed maize keeps the crop's
  # own densities. The wholly irrigated olives are re-evaluated at their mean
  # yield with the irrigation terms, which here change nothing: crop group
  # "Other" has no residue response, and at 2 t/ha the olive root sits on its
  # cap of three times the root:shoot ratio, which the irrigated root:shoot
  # factor does not move.
  testthat::expect_identical(sum(parts$item_prod_code == "56"), 1L)
  testthat::expect_false(any(parts$irrigated[parts$item_prod_code == "56"]))
  testthat::expect_identical(sum(parts$item_prod_code == "260"), 1L)
  testthat::expect_true(all(parts$irrigated[parts$item_prod_code == "260"]))
  .rs_expect_equal_densities(parts, 0.25, "56")
  .rs_expect_equal_densities(parts, 0.25, "44")
  .rs_expect_equal_densities(parts, 0.25, "260")
  # How each part's residue and root were set.
  stamps <- parts |>
    dplyr::distinct(
      .data$lon,
      .data$item_prod_code,
      .data$irrigated,
      .data$method_residue_regime
    ) |>
    dplyr::arrange(.data$lon, .data$item_prod_code, .data$irrigated)
  testthat::expect_identical(
    stamps$method_residue_regime,
    c(
      "part_yield", # wheat 15 rainfed
      "part_yield_irrigation_terms", # wheat 15 irrigated
      "part_yield_irrigation_terms", # olives 260, wholly irrigated
      "crop_density", # barley 44, no share row
      "part_yield", # maize 56, wholly rainfed
      "part_yield", # wheat 15 rainfed
      "part_yield_irrigation_terms", # wheat 15 irrigated
      "part_yield", # rice 27 rainfed
      "part_yield_irrigation_terms" # rice 27 irrigated
    )
  )
})

testthat::test_that("R = 1 with the irrigation terms neutral leaves the densities equal", {
  .rs_neutral_irrigation()
  parts <- .rs_parts(.rs_carbon_data(.rs_ratio(1)))
  testthat::expect_identical(
    unique(parts$method_regime_split[parts$item_prod_code == "15"]),
    "yield_ratio"
  )
  .rs_expect_equal_densities(parts, 0.25, "15")
  .rs_expect_equal_densities(parts, 0.75, "27")
  .rs_expect_equal_densities(parts, 0.25, "260")
})

testthat::test_that("R = 1 with the irrigation terms on lowers only the irrigated part", {
  parts <- .rs_parts(.rs_carbon_data(.rs_ratio(1)))
  wheat <- parts[parts$lon == 0.25 & parts$item_prod_code == "15", ]
  crop <- .rs_cropland()[1, ]
  # The rainfed part is the crop at its own yield.
  rf <- wheat[!wheat$irrigated, ]
  testthat::expect_identical(rf$residue_c_mgc_ha_yr, crop$residue_c_mgc_ha_yr)
  testthat::expect_identical(rf$root_c_mgc_ha_yr, crop$root_c_mgc_ha_yr)
  # Wheat residue does not respond to irrigation in the model; its root does.
  ir <- wheat[wheat$irrigated, ]
  testthat::expect_equal(
    ir$residue_c_mgc_ha_yr,
    crop$residue_c_mgc_ha_yr,
    tolerance = 1e-12
  )
  testthat::expect_lt(ir$root_c_mgc_ha_yr, crop$root_c_mgc_ha_yr)
})

testthat::test_that("a missing ratio keeps equal densities, stamped", {
  data <- .rs_carbon_data(.rs_ratio()[-1, ]) # no ratio for wheat at 0.25
  parts <- .rs_parts(data)
  wheat <- parts[parts$lon == 0.25 & parts$item_prod_code == "15", ]
  testthat::expect_identical(unique(wheat$method_regime_split), "no_ratio")
  testthat::expect_identical(
    unique(wheat$method_residue_regime),
    "crop_density"
  )
  testthat::expect_identical(nrow(wheat), 2L)
  .rs_expect_equal_densities(parts, 0.25, "15")
  # The other crops are still split by yield.
  testthat::expect_identical(
    unique(parts$method_regime_split[parts$lon == 0.75]),
    "yield_ratio"
  )
})

testthat::test_that("a zero or missing production keeps equal densities", {
  data <- .rs_carbon_data()
  # Rice has no production at all, wheat a zero production in 2010.
  data$regime_production <- data$regime_production |>
    dplyr::filter(.data$item_prod_code != 27L) |>
    dplyr::mutate(
      value = dplyr::if_else(
        .data$item_prod_code == 15L &
          .data$year == 2010L &
          .data$area_code == 203L &
          .data$unit == "tonnes",
        0,
        .data$value
      )
    )
  parts <- .rs_parts(data)
  rules <- .rs_rules(parts)
  testthat::expect_identical(
    rules$method_regime_split[rules$item_prod_code %in% c("15", "27")],
    rep("area_no_production", 3)
  )
  .rs_expect_equal_densities(parts, 0.25, "15")
  .rs_expect_equal_densities(parts, 0.75, "15")
  .rs_expect_equal_densities(parts, 0.75, "27")
})

testthat::test_that("a layer without residue and root densities is stamped", {
  data <- .rs_carbon_data()
  data$cropland <- dplyr::select(
    data$cropland,
    -"residue_c_mgc_ha_yr",
    -"root_c_mgc_ha_yr"
  )
  parts <- .rs_parts(data)
  testthat::expect_setequal(
    parts$method_regime_split,
    c("area_no_components", "no_regime_share")
  )
  at <- match(
    paste(parts$lon, parts$item_prod_code),
    paste(data$cropland$lon, data$cropland$item_prod_code)
  )
  testthat::expect_identical(
    parts$total_c_input_mgc_ha_yr,
    data$cropland$total_c_input_mgc_ha_yr[at]
  )
})

testthat::test_that("area_split keeps equal densities and reads no ratio", {
  testthat::local_mocked_bindings(
    build_regime_yield_ratio = function(...) rlang::abort("no ratio needed"),
    get_primary_production = function(...) rlang::abort("no yield needed"),
    .package = "whep"
  )
  data <- .rs_carbon_data()
  data$regime_ratio <- NULL
  data$regime_production <- NULL
  parts <- .rs_parts(data, regime = "area_split")
  testthat::expect_setequal(
    parts$method_regime_split,
    c("area_split", "no_regime_share")
  )
  testthat::expect_identical(
    unique(parts$method_residue_regime),
    "crop_density"
  )
  for (item in c("15", "27")) {
    .rs_expect_equal_densities(parts, 0.75, item)
  }
  .rs_expect_equal_densities(parts, 0.25, "260")
})

testthat::test_that("crop_groups$regime is validated", {
  testthat::expect_identical(
    whep:::.ci_group_config()$regime,
    "yield_split"
  )
  testthat::expect_identical(
    whep:::.ci_group_config(list(regime = "area_split"))$regime,
    "area_split"
  )
  testthat::expect_error(
    whep:::.ci_group_config(list(regime = "tiled")),
    "crop_groups"
  )
})

testthat::test_that("the split rule survives the collapse to classes", {
  data <- .rs_carbon_data()
  out <- suppressMessages(whep::build_carbon_inputs(data = data))
  crops <- out[whep:::.soc_is_cropland(out$land_use), ]
  rf <- crops[
    crops$lon == 0.25 & crops$land_use == "cropland_rainfed_herbaceous",
  ]
  testthat::expect_identical(
    rf$method_regime_split,
    "no_regime_share|trivial_rainfed_only|yield_ratio"
  )
  # Its carbon-mass share that was yield-split is the wheat part's.
  parts <- .rs_parts(data)
  members <- parts[
    parts$lon == 0.25 & parts$land_use == "cropland_rainfed_herbaceous",
  ]
  mass <- members$total_c_input_mgc_ha_yr * members$crop_area_ha
  testthat::expect_equal(
    rf$yield_split_c_share,
    sum(mass[members$method_regime_split == "yield_ratio"]) / sum(mass),
    tolerance = 1e-12
  )
  testthat::expect_true(all(crops$method_regime_cn == "crop_input_cn"))
  pointblank::expect_col_vals_between(crops, "yield_split_c_share", 0, 1)
  # The class's residue rules are the union of its parts'.
  testthat::expect_identical(
    rf$method_residue_regime,
    "crop_density|part_yield"
  )
  testthat::expect_true(all(crops$method_regime == "yield_split"))
  # Grassland and natural are not split from crops.
  other <- out[!whep:::.soc_is_cropland(out$land_use), ]
  testthat::expect_true(all(is.na(other$method_regime_split)))
  testthat::expect_true(all(is.na(other$yield_split_c_share)))
  testthat::expect_true(all(is.na(other$method_regime_cn)))
  testthat::expect_true(all(is.na(other$method_residue_regime)))
  testthat::expect_true(all(is.na(other$method_regime)))
  # The polity roll-up unions the rules and carbon-weights the share.
  pol <- suppressMessages(
    whep::build_carbon_inputs(resolution = "polity", data = data)
  )
  herb <- pol[pol$land_use == "cropland_rainfed_herbaceous", ]
  testthat::expect_identical(
    herb$method_regime_split,
    "no_regime_share|trivial_rainfed_only|yield_ratio"
  )
  grid_herb <- crops[crops$land_use == "cropland_rainfed_herbaceous", ]
  w <- grid_herb$c_input_mgc_ha_yr * grid_herb$group_area_ha
  testthat::expect_equal(
    herb$yield_split_c_share,
    sum(grid_herb$yield_split_c_share * w) / sum(w),
    tolerance = 1e-12
  )
  testthat::expect_identical(herb$method_regime_cn, "crop_input_cn")
  testthat::expect_identical(
    herb$method_residue_regime,
    "crop_density|part_yield"
  )
  testthat::expect_identical(herb$method_regime, "yield_split")
})

testthat::test_that("with the irrigation terms neutral, class carbon mass is the unsplit crop mass", {
  .rs_neutral_irrigation()
  out <- suppressMessages(whep::build_carbon_inputs(data = .rs_carbon_data()))
  crops <- out[whep:::.soc_is_cropland(out$land_use), ]
  testthat::expect_lt(
    .rs_rel_diff(
      sum(crops$c_input_mgc_ha_yr * crops$group_area_ha),
      sum(.rs_unsplit_mass()$total_c_input_mgc_ha_yr)
    ),
    1e-10
  )
})

testthat::test_that("with the irrigation terms on, the class carbon lost is lost on irrigated land", {
  out <- suppressMessages(whep::build_carbon_inputs(data = .rs_carbon_data()))
  crops <- out[whep:::.soc_is_cropland(out$land_use), ]
  mass <- crops$c_input_mgc_ha_yr * crops$group_area_ha
  irrigated <- stringr::str_detect(crops$land_use, "irrig")
  unsplit <- sum(.rs_unsplit_mass()$total_c_input_mgc_ha_yr)
  testthat::expect_lt(sum(mass), unsplit)
  # The rainfed groups carry at least what an equal-yield split gives them:
  # the irrigation terms act on irrigated parts only.
  .rs_neutral_irrigation()
  neutral <- suppressMessages(
    whep::build_carbon_inputs(data = .rs_carbon_data())
  )
  neutral <- neutral[whep:::.soc_is_cropland(neutral$land_use), ]
  testthat::expect_identical(
    neutral$c_input_mgc_ha_yr[!stringr::str_detect(neutral$land_use, "irrig")],
    crops$c_input_mgc_ha_yr[!irrigated]
  )
  # Every irrigated herbaceous class loses root carbon to the irrigated
  # root:shoot factor; the olive class is unchanged, its root on the cap.
  herbaceous <- crops$land_use == "cropland_irrigated_herbaceous"
  testthat::expect_identical(sum(herbaceous), 2L)
  testthat::expect_true(all(
    crops$c_input_mgc_ha_yr[herbaceous] < neutral$c_input_mgc_ha_yr[herbaceous]
  ))
  testthat::expect_identical(
    crops$c_input_mgc_ha_yr[irrigated & !herbaceous],
    neutral$c_input_mgc_ha_yr[irrigated & !herbaceous]
  )
})

# -- Golden: the pre-split output where the split cannot move a number --------

# Written from the same fixtures by whep main b1da02e4, before the yield split
# existed (see helper_regime_split.R). The added stamp columns are checked
# separately; every column the old output had must be bit-identical. Where a
# part is re-evaluated (R = 1), the irrigation terms are switched off, as
# the old output had none.
.rs_golden <- function() {
  readRDS(testthat::test_path("fixtures", "carbon_inputs_regime_golden.rds"))
}

.rs_expect_golden <- function(out, golden) {
  testthat::expect_true(all(names(golden) %in% names(out)))
  testthat::expect_identical(out[names(golden)], golden)
}

testthat::test_that("R = 1 everywhere with the irrigation terms neutral reproduces the pre-split output", {
  .rs_neutral_irrigation()
  golden <- .rs_golden()
  data <- .rs_carbon_data(.rs_ratio(1))
  for (resolution in c("grid", "polity")) {
    out <- suppressMessages(
      whep::build_carbon_inputs(resolution = resolution, data = data)
    )
    .rs_expect_golden(out, golden[[paste0("grouped_", resolution)]])
  }
})

testthat::test_that("irrigation = none reproduces the pre-split output", {
  golden <- .rs_golden()
  data <- .rs_carbon_data()
  data$crop_regime_share <- NULL
  for (resolution in c("grid", "polity")) {
    out <- suppressMessages(whep::build_carbon_inputs(
      resolution = resolution,
      data = data,
      crop_groups = list(irrigation = "none")
    ))
    .rs_expect_golden(out, golden[[paste0("irrigation_none_", resolution)]])
    crops <- out[whep:::.soc_is_cropland(out$land_use), ]
    testthat::expect_identical(
      unique(crops$method_regime_split),
      "no_regime_share"
    )
    testthat::expect_identical(unique(crops$yield_split_c_share), 0)
  }
})

testthat::test_that("the single cropland class reproduces the pre-split output", {
  golden <- .rs_golden()
  for (resolution in c("grid", "polity")) {
    out <- whep::build_carbon_inputs(
      resolution = resolution,
      data = .rs_carbon_data(),
      crop_groups = list(method = "none")
    )
    .rs_expect_golden(out, golden[[paste0("single_", resolution)]])
    pointblank::expect_col_exists(
      out,
      c("method_regime_split", "yield_split_c_share", "method_regime_cn")
    )
    testthat::expect_true(all(is.na(out$method_regime_split)))
    testthat::expect_true(all(is.na(out$yield_split_c_share)))
  }
})

testthat::test_that("area_split reproduces the pre-split output", {
  golden <- .rs_golden()
  for (resolution in c("grid", "polity")) {
    out <- suppressMessages(whep::build_carbon_inputs(
      resolution = resolution,
      data = .rs_carbon_data(),
      crop_groups = list(regime = "area_split")
    ))
    .rs_expect_golden(out, golden[[paste0("grouped_", resolution)]])
  }
})

# -- One ratio for both chains -----------------------------------------------

# The nitrogen chain's inputs for the same three split cell-crops: the same
# production (national yield times area), the same irrigated share and the
# same ratio table.
.rs_n_data <- function(ratio) {
  npp <- tibble::tribble(
    ~lon, ~lat, ~area_code, ~item_prod_code, ~item_cbs_code, ~year,
    ~production_t, ~area_ha,
    0.25, 40.25, 203L, 15L, 2511L, 2010L, 160, 40,
    0.75, 40.25, 203L, 15L, 2511L, 2010L, 200, 50,
    0.75, 40.25, 203L, 27L, 2805L, 2010L, 125, 25
  )
  areas <- dplyr::mutate(
    npp,
    irrigated_ha = .data$area_ha * c(0.25, 0.6, 0.9),
    rainfed_ha = .data$area_ha - .data$irrigated_ha
  ) |>
    dplyr::select(dplyr::all_of(.rs_key), "rainfed_ha", "irrigated_ha")
  list(
    .npp_cache = npp,
    regime_areas = areas,
    regime_ratio = ratio,
    regime_production = .rs_production()
  )
}

testthat::test_that("the carbon and nitrogen chains apply the same bounded R", {
  # Wheat at 0.25 is pushed to R = 40, so the irrigated-yield ceiling binds
  # and the bounded ratio differs from the unbounded one.
  ratio <- .rs_ratio()
  ratio$ratio_unbounded[1] <- 40
  calls <- 0L
  shared <- whep:::.ryr_item_split
  testthat::local_mocked_bindings(
    .ryr_item_split = function(...) {
      calls <<- calls + 1L
      shared(...)
    },
    .package = "whep"
  )
  n_items <- whep:::.nb_regime_items(.rs_n_data(ratio))
  testthat::expect_identical(calls, 1L)
  data <- .rs_carbon_data(ratio)
  c_items <- suppressMessages(whep:::.ci_regime_items(
    data$cropland,
    data$crop_regime_share,
    .rs_context(data)
  ))
  testthat::expect_identical(calls, 2L)
  both <- n_items |>
    dplyr::select(dplyr::all_of(.rs_key), n_ratio = "ratio") |>
    dplyr::inner_join(
      c_items |>
        dplyr::mutate(item_prod_code = as.integer(.data$item_prod_code)) |>
        dplyr::select(dplyr::all_of(.rs_key), c_ratio = "regime_ratio"),
      by = .rs_key
    )
  testthat::expect_identical(nrow(both), 3L)
  testthat::expect_identical(both$c_ratio, both$n_ratio)
  # The ratio applied is the bounded one, not the raw product.
  wheat <- both[both$lon == 0.25, ]
  testthat::expect_lt(wheat$c_ratio, 40)
  testthat::expect_gte(wheat$c_ratio, 1)
})

# -- One read of the ratio's inputs per build ---------------------------------

# A default build hands the collapse one year of the per-crop layer at a time,
# and each year builds its own regime yield ratio. These tests run that path
# on a fixture world with every reader it reaches mocked to serve the fixture
# and count its calls.

# Spain's crops in two cells, as SPAM2010 totals (one row per crop and
# technology): wheat, maize, olives (oil crops), barley and rice.
.rs_world_spam <- function() {
  tibble::tribble(
    ~iso3, ~spam_crop, ~technology, ~harvested_area_ha, ~production_t,
    "ESP", "whea", "I", 100, 500,
    "ESP", "whea", "R", 400, 1200,
    "ESP", "maiz", "I", 10, 60,
    "ESP", "maiz", "R", 10, 10,
    "ESP", "ooil", "I", 10, 30,
    "ESP", "ooil", "R", 10, 10,
    "ESP", "barl", "I", 50, 100,
    "ESP", "barl", "R", 150, 150,
    "ESP", "rice", "I", 10, 50,
    "ESP", "rice", "R", 10, 20
  )
}

.rs_world_fertilizer <- function() {
  tibble::tribble(
    ~`Area Code`, ~Year, ~Value,
    203L, 2009L, 1500,
    203L, 2010L, 1000
  ) |>
    dplyr::mutate(
      Element = "Agricultural Use",
      Item = "Nutrient nitrogen N (total)"
    )
}

.rs_world_cropland <- function() {
  tibble::tribble(
    ~area_code, ~year, ~cropland_ha,
    203L, 2009L, 1000,
    203L, 2010L, 1000
  )
}

# LPJmL crop yields of the fixture's two cells at the crop grain, with
# yields that differ by cell, crop and year so no two cell-crop-years share
# a ratio.
.rs_world_lpjml <- function(years) {
  crops <- c("temperate cereals", "maize", "rice", "others")
  tidyr::expand_grid(cell = 1:2, lpjml_crop = crops, year = years) |>
    dplyr::mutate(
      lon = c(0.25, 0.75)[.data$cell],
      lat = 40.25,
      yield_rainfed = 80 +
        10 * .data$cell +
        5 * match(.data$lpjml_crop, crops) +
        (.data$year - 2000L),
      yield_irrigated = .data$yield_rainfed *
        (1.4 + 0.05 * .data$cell + 0.01 * (.data$year - 2000L)),
      stand_frac_rainfed = 0.1,
      stand_frac_irrigated = 0.1,
      method_regime_yield = "lpjml_band_harvest"
    ) |>
    dplyr::select(
      "lon",
      "lat",
      "year",
      "lpjml_crop",
      "yield_rainfed",
      "yield_irrigated",
      "stand_frac_rainfed",
      "stand_frac_irrigated",
      "method_regime_yield"
    )
}

# The readers the ratio reaches when the build supplies none of its inputs
# but the production. Reaching the Linum and Hemp production or the LUH2
# back-cast, which this world has no use for, is an error.
.rs_counted_readers <- function(env = parent.frame()) {
  calls <- new.env(parent = emptyenv())
  count <- function(name) {
    assign(name, (calls[[name]] %||% 0L) + 1L, envir = calls)
  }
  testthat::local_mocked_bindings(
    read_spam_yields = function(...) {
      count("spam")
      .rs_world_spam()
    },
    whep_read_file = function(file_alias, ...) {
      count(file_alias)
      if (file_alias != "faostat-fertilizer-nutrients") {
        rlang::abort(paste("unexpected read of", file_alias))
      }
      .rs_world_fertilizer()
    },
    get_arable_permanent_land = function(years = NULL, ...) {
      count("cropland")
      .rs_world_cropland() |>
        dplyr::filter(.data$year %in% years)
    },
    .ryr_read_lpjml_window = function(run_dir) {
      count("window")
      .rs_world_lpjml(2000:2001)
    },
    .ryr_read_lpjml_grid = function(run_dir) {
      count("grid")
      tibble::tibble(lon = c(0.25, 0.75), lat = 40.25)
    },
    .lrg_crop_yield = function(years = NULL, ...) {
      count("lpjml_year")
      .rs_world_lpjml(years)
    },
    .package = "whep",
    .env = env
  )
  calls
}

# Serves the per-crop layer to the collapse one year at a time, the way the
# gridding loop does.
.rs_year_chunks <- function(layer, env = parent.frame()) {
  testthat::local_mocked_bindings(
    .sci_build = function(resolution, data, years, reduce = NULL, ...) {
      purrr::map(years, \(year) reduce(layer[layer$year == year, ])) |>
        dplyr::bind_rows()
    },
    .package = "whep",
    .env = env
  )
}

# The 2010 fixture and a 2009 with smaller crop areas and irrigated shares.
.rs_world_data <- function() {
  data <- .rs_carbon_data(ratio = NULL)
  data$cropland <- NULL
  data$crop_regime_share <- dplyr::bind_rows(
    dplyr::mutate(
      .rs_shares(),
      year = 2009L,
      irrigated_share = .data$irrigated_share * 0.8
    ),
    .rs_shares()
  )
  data
}

.rs_world_layer <- function() {
  dplyr::bind_rows(
    dplyr::mutate(
      .rs_cropland(),
      year = 2009L,
      crop_area_ha = .data$crop_area_ha * 0.9
    ),
    .rs_cropland()
  )
}

# Counts of the readers of a build, 0 for one never reached.
.rs_counts <- function(calls) {
  inputs <- c(
    "spam",
    "faostat-fertilizer-nutrients",
    "window",
    "grid",
    "lpjml_year",
    "cropland"
  )
  vapply(inputs, \(input) calls[[input]] %||% 0L, integer(1)) |>
    unname()
}

testthat::test_that("a build reads the ratio's year-invariant inputs once, not once per year", {
  calls <- .rs_counted_readers()
  .rs_year_chunks(.rs_world_layer())
  spy <- new.env(parent = emptyenv())
  spy$ratios <- list()
  item_split <- whep:::.ryr_item_split
  testthat::local_mocked_bindings(
    .ryr_item_split = function(...) {
      out <- item_split(...)
      spy$ratios[[length(spy$ratios) + 1L]] <- out
      out
    },
    .package = "whep"
  )
  data <- .rs_world_data()
  build <- function(years) {
    suppressMessages(whep::build_carbon_inputs(data = data, years = years))
  }
  # The cropland classes of a build, in key order. The grassland and natural
  # rows come from `data` whole, so a build per year repeats them.
  crops <- function(x) {
    x[whep:::.soc_is_cropland(x$land_use), ] |>
      dplyr::arrange(.data$lon, .data$lat, .data$year, .data$land_use)
  }
  # Both years in one build: the year-invariant readers once; the readers of
  # the year's own inputs, once per year.
  together <- build(2009:2010)
  testthat::expect_identical(.rs_counts(calls), c(1L, 1L, 1L, 1L, 2L, 2L))
  ratios_together <- spy$ratios
  # One build per year is the per-year reads of a build without a shared
  # cache: every input of the ratio is read for each year.
  rm(list = ls(calls), envir = calls)
  spy$ratios <- list()
  separate <- dplyr::bind_rows(build(2009L), build(2010L))
  testthat::expect_identical(.rs_counts(calls), c(2L, 2L, 2L, 2L, 2L, 2L))
  # Same ratios, cell-crop-year for cell-crop-year, and the same output.
  testthat::expect_length(ratios_together, 2L)
  testthat::expect_identical(ratios_together, spy$ratios)
  testthat::expect_identical(crops(together), crops(separate))
  # The numbers are the yield split's, not the fall-back's.
  rules <- unique(unlist(stringr::str_split(
    together$method_regime_split,
    stringr::fixed("|")
  )))
  testthat::expect_true("yield_ratio" %in% rules)
  testthat::expect_setequal(unique(together$year), 2009:2010)
})
