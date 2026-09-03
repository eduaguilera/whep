# The carbon balance marching crop GROUPS as classes (Spain_Hist convention).
# Offline throughout.

testthat::test_that("cover profiles map groups without enumerating them", {
  x <- c(
    "cropland",
    "cropland_rainfed_herbaceous",
    "cropland_irrigated_herbaceous",
    "cropland_rainfed_olive",
    "cropland_irrigated_peaches_nectarines",
    "grassland",
    "natural",
    "urban"
  )
  profiles <- whep:::.cb_cover_profile(x)
  testthat::expect_identical(
    profiles,
    c(
      "cropland",
      "cropland_rainfed",
      "cropland_irrigated",
      "woody_cropland",
      "woody_cropland",
      "grassland",
      "natural",
      "urban"
    )
  )
  # Idempotent, so a lookup can be keyed on a profile as well as on a class.
  testthat::expect_identical(whep:::.cb_cover_profile(profiles), profiles)
  # The regime profiles read cropland's curve row; nothing else is renamed.
  testthat::expect_identical(
    whep:::.cb_curve_key(profiles),
    c(
      "cropland",
      "cropland",
      "cropland",
      "woody_cropland",
      "grassland",
      "natural",
      "urban"
    )[c(1, 2, 3, 4, 4, 5, 6, 7)]
  )
})

testthat::test_that("the curve carries a perennial woody-cropland profile", {
  # An ASSUMED value: no sourced constant exists in the repo, so woody groups
  # take the perennial 0.85 grassland/natural already use, flagged in NEWS
  # as an assumption to replace. It must at least exist in every month, or a
  # woody group would silently run bare.
  curve <- whep::soc_soil_cover_curve
  woody <- curve[curve$land_use == "woody_cropland", ]
  testthat::expect_identical(nrow(woody), 12L)
  testthat::expect_true(all(woody$soil_cover == 0.85))
  testthat::expect_setequal(woody$months_from_peak, -5:6)
})

.cbg_drivers <- function() {
  tidyr::expand_grid(
    lon = 0.25,
    lat = 5.25,
    area_code = 1L,
    year = 2010L,
    month = 1:12
  ) |>
    dplyr::mutate(
      temp_c = 22,
      precip_mm = 90,
      pet_mm = 80,
      clay_pct = 25,
      water_minus_pet_mm = precip_mm - pet_mm
    )
}

testthat::test_that("crossing by profile reproduces the per-class cover", {
  classes <- c("cropland", "grassland", "natural")
  out <- whep:::.cb_attach_soil_cover(.cbg_drivers(), classes)
  # One row per class-month, every class present, and the class rows equal
  # the curve rows for that class.
  testthat::expect_identical(nrow(out), 36L)
  testthat::expect_setequal(unique(out$land_use), classes)
  curve <- whep::soc_soil_cover_curve
  for (cl in classes) {
    got <- out[out$land_use == cl, ]
    testthat::expect_setequal(
      round(got$soil_cover, 6),
      round(curve$soil_cover[curve$land_use == cl], 6)
    )
  }
})

testthat::test_that("groups sharing a profile share its cover rows", {
  classes <- c(
    "cropland_rainfed_herbaceous",
    "cropland_irrigated_herbaceous",
    "cropland_rainfed_olive",
    "natural"
  )
  out <- whep:::.cb_attach_soil_cover(.cbg_drivers(), classes)
  testthat::expect_identical(nrow(out), 48L)
  rf <- out[out$land_use == "cropland_rainfed_herbaceous", ]
  ir <- out[out$land_use == "cropland_irrigated_herbaceous", ]
  plain <- whep:::.cb_attach_soil_cover(.cbg_drivers(), "cropland")
  testthat::expect_equal(
    rf$soil_cover[order(rf$month)],
    plain$soil_cover[order(plain$month)]
  )
  testthat::expect_equal(
    ir$soil_cover[order(ir$month)],
    plain$soil_cover[order(plain$month)]
  )
  ol <- out[out$land_use == "cropland_rainfed_olive", ]
  testthat::expect_true(all(ol$soil_cover == 0.85))
})

testthat::test_that("the crop-calendar override reaches herbaceous groups only", {
  cover <- tibble::tibble(
    lon = 0.25,
    lat = 5.25,
    year = 2010L,
    month = 1:12,
    cropland_cover = seq(0, 1, length.out = 12)
  )
  classes <- c("cropland_rainfed_herbaceous", "cropland_rainfed_olive")
  out <- whep:::.cb_attach_soil_cover(
    .cbg_drivers(),
    classes,
    cropland_cover = cover
  )
  rf <- out[out$land_use == "cropland_rainfed_herbaceous", ]
  testthat::expect_equal(rf$soil_cover[order(rf$month)], cover$cropland_cover)
  ol <- out[out$land_use == "cropland_rainfed_olive", ]
  testthat::expect_true(all(ol$soil_cover == 0.85))
})

testthat::test_that("the C:N lookup treats every crop group as cropland", {
  testthat::expect_identical(
    whep:::.cb_cropland_class(c(
      "cropland",
      "cropland_irrigated_herbaceous",
      "cropland_rainfed_olive",
      "grassland"
    )),
    c("Cropland", "Cropland", "Cropland", "NonCropland")
  )
})

testthat::test_that("LUH2 cropland is split over groups in proportion to area", {
  land_use <- tibble::tribble(
    ~lon, ~lat, ~area_code, ~year, ~land_use, ~area_ha,
    0.25, 0.25, 1L, 2000L, "cropland", 100,
    0.25, 0.25, 1L, 2000L, "natural", 300,
    0.75, 0.25, 1L, 2000L, "cropland", 50
  )
  c_inputs <- tibble::tribble(
    ~lon, ~lat, ~area_code, ~year, ~land_use, ~c_input_mgc_ha_yr,
    ~humified_fraction, ~group_area_ha,
    0.25, 0.25, 1L, 2000L, "cropland_rainfed_herbaceous", 2, 0.2, 30,
    0.25, 0.25, 1L, 2000L, "cropland_rainfed_olive", 4, 0.3, 10,
    0.25, 0.25, 1L, 2000L, "natural", 1, 0.3, NA
  )
  out <- whep:::.cb_split_cropland_groups(land_use, c_inputs)
  cell_a <- out[out$lon == 0.25, ]
  # 100 ha of LUH2 cropland split 30:10 -> 75 and 25; LUH2's total is kept,
  # not the crop-pattern basis' 40.
  testthat::expect_setequal(
    cell_a$land_use,
    c("cropland_rainfed_herbaceous", "cropland_rainfed_olive", "natural")
  )
  testthat::expect_equal(
    cell_a$area_ha[cell_a$land_use == "cropland_rainfed_herbaceous"],
    75
  )
  testthat::expect_equal(
    cell_a$area_ha[cell_a$land_use == "cropland_rainfed_olive"],
    25
  )
  testthat::expect_equal(sum(cell_a$area_ha), 400)
  # A cell with cropland but no grouped inputs keeps its plain row.
  cell_b <- out[out$lon == 0.75, ]
  testthat::expect_identical(cell_b$land_use, "cropland")
  testthat::expect_equal(cell_b$area_ha, 50)
})

testthat::test_that("an ungrouped run leaves the land-use table untouched", {
  land_use <- tibble::tribble(
    ~lon, ~lat, ~area_code, ~year, ~land_use, ~area_ha,
    0.25, 0.25, 1L, 2000L, "cropland", 100,
    0.25, 0.25, 1L, 2000L, "natural", 300
  )
  c_inputs <- tibble::tribble(
    ~lon, ~lat, ~area_code, ~year, ~land_use, ~c_input_mgc_ha_yr,
    ~humified_fraction,
    0.25, 0.25, 1L, 2000L, "cropland", 2, 0.2,
    0.25, 0.25, 1L, 2000L, "natural", 1, 0.3
  )
  testthat::expect_identical(
    whep:::.cb_split_cropland_groups(land_use, c_inputs),
    land_use
  )
})

testthat::test_that("build_carbon_balance rejects a malformed crop_groups", {
  testthat::expect_error(
    whep::build_carbon_balance(crop_groups = list(method = "spainhist")),
    "crop_groups"
  )
})

testthat::test_that("crop_groups reaches the carbon-input reader on the real path", {
  # The group tests inject data$c_inputs, so a build_carbon_balance() that
  # validated crop_groups and then read ungrouped inputs passed every one of
  # them while silently ignoring the option on a real run. Both hops are
  # pinned: the resolver forwards the config to the reader, and the builder
  # forwards it to the resolver.
  seen <- NULL
  testthat::local_mocked_bindings(
    .cb_read_c_inputs = function(
      years = NULL,
      crop_groups = list(),
      density_basis = "static"
    ) {
      seen <<- crop_groups
      tibble::tibble()
    },
    .cb_read_land_use = function(years = NULL) tibble::tibble(),
    .cb_read_climate = function(years = NULL) {
      tibble::tibble(lon = 0.25, lat = 5.25, clay_pct = 25)
    },
    .package = "whep"
  )
  cfg <- whep:::.ci_group_config(list(method = "spain_hist"))
  whep:::.cb_resolve_inputs(list(), 2010L, cfg)
  testthat::expect_identical(seen, cfg)

  captured <- NULL
  testthat::local_mocked_bindings(
    .cb_resolve_inputs = function(
      data,
      years = NULL,
      crop_groups = list(),
      density_basis = "static"
    ) {
      captured <<- crop_groups
      rlang::abort("stop here", class = "cbg_stop")
    },
    .package = "whep"
  )
  testthat::expect_error(
    whep::build_carbon_balance(
      years = 2010L,
      crop_groups = list(method = "spain_hist", irrigation = "none")
    ),
    class = "cbg_stop"
  )
  testthat::expect_identical(
    captured,
    list(method = "spain_hist", irrigation = "none")
  )
})

# ---- class_water: where a cell's irrigation goes ----------------------------

# One cell-month, four classes. Rain 50, PET 80, applied irrigation 30, so the
# cell-level surplus the driver carries is 0 and the rainfed surplus is -30.
.cw_prepared <- function() {
  tibble::tibble(
    lon = 0.25,
    lat = 5.25,
    area_code = 1L,
    year = 2010L,
    month = 6L,
    land_use = c(
      "natural",
      "grassland",
      "cropland_rainfed_herbaceous",
      "cropland_irrigated_herbaceous"
    ),
    precip_mm = 50,
    pet_mm = 80,
    water_minus_pet_mm = 0
  )
}

.cw_spec <- function(irrigated_frac = 0.25) {
  list(
    method = "regime",
    irrigated_frac = tibble::tibble(
      lon = 0.25,
      lat = 5.25,
      area_code = 1L,
      year = 2010L,
      irrigated_frac = irrigated_frac
    )
  )
}

testthat::test_that("the cell rule is the previous behaviour exactly", {
  out <- whep:::.cb_attach_class_water(.cw_prepared())
  same <- whep:::.cb_attach_class_water(
    .cw_prepared(),
    list(method = "cell")
  )
  testthat::expect_identical(out, same)
  # Natural land on rain; every other class waters with the cell's irrigation.
  testthat::expect_equal(
    out$water_minus_pet_mm,
    c(-30, 0, 0, 0)
  )
  testthat::expect_named(out, names(.cw_prepared()))
})

testthat::test_that("the regime rule puts the irrigation on the irrigated group", {
  out <- whep:::.cb_attach_class_water(.cw_prepared(), .cw_spec(0.25))
  # 30 mm over the whole cell is 120 mm over the quarter that is irrigated.
  testthat::expect_equal(
    out$water_minus_pet_mm,
    c(-30, -30, -30, -30 + 30 / 0.25)
  )
  # And the class-area-weighted mean is still the cell value: no water is
  # created or lost, only placed.
  frac <- c(0.4, 0.2, 0.15, 0.25)
  testthat::expect_equal(sum(frac * out$water_minus_pet_mm), 0)
  testthat::expect_named(out, names(.cw_prepared()))
})

testthat::test_that("a cell-year with no irrigated class keeps the cell rule", {
  # The irrigation has nowhere else to go, so dropping it would lose water.
  spec <- .cw_spec(0.25)
  spec$irrigated_frac$year <- 2011L
  out <- whep:::.cb_attach_class_water(.cw_prepared(), spec)
  testthat::expect_equal(out$water_minus_pet_mm, c(-30, 0, 0, 0))
})

testthat::test_that("the year-less equilibrium normal joins on the cell", {
  prepared <- dplyr::select(.cw_prepared(), -"year")
  out <- whep:::.cb_attach_class_water(prepared, .cw_spec(0.5))
  testthat::expect_equal(out$water_minus_pet_mm, c(-30, -30, -30, 30))
})

testthat::test_that("the spec is the irrigated groups' share of the cell", {
  classes <- tibble::tibble(
    lon = 0.25,
    lat = 5.25,
    area_code = 1L,
    year = c(2010L, 2010L, 2010L, 2011L),
    land_use = c(
      "cropland_irrigated_olive",
      "cropland_irrigated_herbaceous",
      "natural",
      "natural"
    ),
    frac = c(0.1, 0.15, 0.75, 1)
  )
  spec <- whep:::.cb_class_water_spec("regime", classes)
  testthat::expect_identical(spec$method, "regime")
  testthat::expect_equal(spec$irrigated_frac$irrigated_frac, 0.25)
  testthat::expect_identical(spec$irrigated_frac$year, 2010L)
  testthat::expect_null(whep:::.cb_class_water_spec("cell", classes))
  testthat::expect_null(whep:::.cb_class_water_spec(NULL, classes))
  # Plain cropland carries no regime, so it is never irrigated here.
  testthat::expect_identical(
    whep:::.soc_is_irrigated_class(c(
      "cropland",
      "Cropland_Irrigated_Olive",
      "cropland_rainfed_olive",
      "grassland"
    )),
    c(FALSE, TRUE, FALSE, FALSE)
  )
})

testthat::test_that("regime water without crop groups is refused, not ignored", {
  # Groups are the default, so "no groups" has to be asked for to be tested.
  testthat::expect_error(
    whep::build_carbon_balance(
      class_water = "regime",
      crop_groups = list(method = "none")
    ),
    "crop_groups"
  )
  testthat::expect_error(
    whep::build_carbon_balance(class_water = "cellular"),
    class = "rlang_error"
  )
})

testthat::test_that("the balance stamps method_class_water and marches groups on it", {
  land_use <- tidyr::expand_grid(
    lon = 0.25,
    lat = 5.25,
    area_code = 1L,
    year = 2010:2011,
    land_use = c(
      "natural",
      "cropland_rainfed_herbaceous",
      "cropland_irrigated_herbaceous"
    )
  ) |>
    dplyr::mutate(
      area_ha = c(60, 30, 10)[match(
        land_use,
        c(
          "natural",
          "cropland_rainfed_herbaceous",
          "cropland_irrigated_herbaceous"
        )
      )]
    )
  c_inputs <- land_use |>
    dplyr::select(-"area_ha") |>
    dplyr::mutate(
      c_input_mgc_ha_yr = dplyr::if_else(land_use == "natural", 1.5, 2.5),
      humified_fraction = 0.3
    )
  climate <- tidyr::expand_grid(
    lon = 0.25,
    lat = 5.25,
    area_code = 1L,
    year = 2010:2011,
    month = 1:12
  ) |>
    dplyr::mutate(
      temp_c = 22,
      # Rain 30, PET 80, irrigation 30: the cell surplus is -20, a deficit the
      # moisture term responds to. (At a cell surplus >= 0 the term is already
      # capped at 1, so concentrating irrigation on the irrigated group could
      # not show; the rainfed group would still move.)
      precip_mm = 30,
      pet_mm = 80,
      water_minus_pet_mm = 30 + 30 - 80,
      clay_pct = 25
    )
  data <- list(land_use = land_use, c_inputs = c_inputs, climate = climate)
  cell <- whep::build_carbon_balance(
    data = data,
    crop_groups = list(method = "spain_hist", irrigation = "none")
  )
  regime <- whep::build_carbon_balance(
    data = data,
    crop_groups = list(method = "spain_hist", irrigation = "none"),
    class_water = "regime"
  )
  testthat::expect_true(all(cell$method_class_water == "cell"))
  testthat::expect_true(all(regime$method_class_water == "regime"))
  first <- function(x, cl) {
    x$stock_mgc_ha[x$land_use == cl & x$year == 2010L]
  }
  # Natural land is on rain under both rules: identical.
  testthat::expect_equal(first(cell, "natural"), first(regime, "natural"))
  # The rainfed group loses the cell's irrigation (drier, slower turnover,
  # more carbon); the irrigated group receives it fourfold (wetter, less).
  rf <- "cropland_rainfed_herbaceous"
  ir <- "cropland_irrigated_herbaceous"
  testthat::expect_gt(first(regime, rf), first(cell, rf))
  testthat::expect_lt(first(regime, ir), first(cell, ir))
  # Under the cell rule both groups see the same water and inputs: same stock.
  testthat::expect_equal(first(cell, rf), first(cell, ir))
  # The polity roll-up keeps the stamp.
  pol <- whep::build_carbon_balance(
    data = data,
    crop_groups = list(method = "spain_hist", irrigation = "none"),
    class_water = "regime",
    resolution = "polity"
  )
  testthat::expect_true(all(pol$method_class_water == "regime"))
})

testthat::test_that("grouped inputs with no LUH2 cropland to draw are reported", {
  # Crop patterns put an olive group in cell 2, where LUH2 carries no
  # cropland row for that year. The group cannot draw area, so its carbon
  # never enters the march; that disagreement between the two sources is
  # counted and said, not swallowed.
  land_use <- tibble::tibble(
    lon = c(0.25, 0.75, 0.75),
    lat = 5.25,
    area_code = 1L,
    year = 2010L,
    land_use = c("cropland", "grassland", "natural"),
    area_ha = c(100, 50, 50)
  )
  c_inputs <- tibble::tibble(
    lon = c(0.25, 0.75),
    lat = 5.25,
    area_code = 1L,
    year = 2010L,
    land_use = c("cropland_rainfed_herbaceous", "cropland_rainfed_olive"),
    group_area_ha = c(80, 2e6),
    c_input_mgc_ha_yr = 2,
    humified_fraction = 0.3
  )
  testthat::expect_message(
    out <- whep:::.cb_split_cropland_groups(land_use, c_inputs),
    "1 grouped carbon-input row in 1 cell-year"
  )
  testthat::expect_message(
    whep:::.cb_split_cropland_groups(land_use, c_inputs),
    "2 Mha"
  )
  # Cell 1 is split (one group takes all of it); cell 2 keeps its two
  # non-cropland rows and gains nothing.
  testthat::expect_identical(nrow(out), 3L)
  testthat::expect_equal(sum(out$area_ha), 200)
  testthat::expect_false("cropland_rainfed_olive" %in% out$land_use)
  # Nothing to report when every group has a row to draw.
  testthat::expect_no_message(
    whep:::.cb_split_cropland_groups(land_use[1, ], c_inputs[1, ])
  )
})

testthat::test_that("density_basis reaches the carbon-input reader", {
  seen <- NULL
  testthat::local_mocked_bindings(
    .cb_read_c_inputs = function(
      years = NULL,
      crop_groups = list(),
      density_basis = "static"
    ) {
      seen <<- density_basis
      tibble::tibble()
    },
    .cb_read_land_use = function(years = NULL) tibble::tibble(),
    .cb_read_climate = function(years = NULL) {
      tibble::tibble(lon = 0.25, lat = 5.25, clay_pct = 25)
    },
    .package = "whep"
  )
  whep:::.cb_resolve_inputs(list(), 2010L, list(), "renormalised")
  testthat::expect_identical(seen, "renormalised")
  testthat::expect_error(
    whep::build_carbon_balance(density_basis = "faostat"),
    class = "rlang_error"
  )
})

# ---- the LUC ledger closes on mass -------------------------------------------

.cbg_luc_data <- function(a2, b2) {
  lu <- tibble::tibble(
    lon = 0.25,
    lat = 5.25,
    area_code = 1L,
    year = rep(2010:2011, each = 3),
    land_use = rep(
      c("cropland_rainfed_herbaceous", "cropland_rainfed_olive", "natural"),
      2
    ),
    area_ha = c(60, 40, 100, a2, b2, 100)
  )
  ci <- lu |>
    dplyr::select(-"area_ha") |>
    dplyr::mutate(
      c_input_mgc_ha_yr = c(2.5, 1.0, 1.5, 2.5, 1.0, 1.5),
      humified_fraction = 0.3
    )
  cl <- tidyr::expand_grid(
    lon = 0.25,
    lat = 5.25,
    area_code = 1L,
    year = 2010:2011,
    month = 1:12
  ) |>
    dplyr::mutate(
      temp_c = 22,
      precip_mm = 50,
      pet_mm = 80,
      water_minus_pet_mm = -30,
      clay_pct = 25
    )
  list(land_use = lu, c_inputs = ci, climate = cl)
}

testthat::test_that("the transfer closes on mass even when a class vanishes", {
  # Found by the real-run grouped smoke: sum(luc_transfer_mgc_ha * area_ha)
  # over a cell-year was up to 4.3e7 Mg C, the stock of classes that had
  # gone to zero area that year. The density column cannot carry an outflow
  # at zero hectares; the mass column can, and must sum to zero.
  cfg <- list(method = "spain_hist", irrigation = "none")
  live <- whep::build_carbon_balance(
    data = .cbg_luc_data(30, 70),
    crop_groups = cfg
  )
  gone <- whep::build_carbon_balance(
    data = .cbg_luc_data(100, 0),
    crop_groups = cfg
  )
  for (out in list(live, gone)) {
    net <- out |>
      dplyr::summarise(
        mass = sum(luc_transfer_mgc),
        density_times_area = sum(luc_transfer_mgc_ha * area_ha),
        stock = sum(stock_mgc_ha * area_ha),
        .by = "year"
      )
    testthat::expect_equal(net$mass, c(0, 0), tolerance = 1e-9)
    # Stock is conserved in both runs: the vanished olive's carbon moved.
    testthat::expect_equal(net$stock[1], net$stock[2], tolerance = 1e-9)
  }
  # With both classes live the two ledgers agree; with the vanished olive
  # the density ledger shows the outflow as a spurious source of exactly the
  # vanished stock (40 ha x its 2010 density), which the mass ledger books.
  live_net <- sum(
    live$luc_transfer_mgc_ha[live$year == 2011L] *
      live$area_ha[live$year == 2011L]
  )
  testthat::expect_equal(live_net, 0, tolerance = 1e-9)
  olive_2010 <- gone[
    gone$year == 2010L & gone$land_use == "cropland_rainfed_olive",
  ]
  gone_net <- sum(
    gone$luc_transfer_mgc_ha[gone$year == 2011L] *
      gone$area_ha[gone$year == 2011L]
  )
  testthat::expect_equal(
    gone_net,
    40 * olive_2010$stock_mgc_ha,
    tolerance = 1e-6
  )
  vanished <- gone[
    gone$year == 2011L & gone$land_use == "cropland_rainfed_olive",
  ]
  testthat::expect_equal(vanished$area_ha, 0)
  testthat::expect_equal(
    vanished$luc_transfer_mgc,
    -40 * olive_2010$stock_mgc_ha,
    tolerance = 1e-6
  )
  # The polity roll-up carries the summed mass and it closes there too.
  pol <- whep::build_carbon_balance(
    data = .cbg_luc_data(100, 0),
    crop_groups = cfg,
    resolution = "polity"
  )
  testthat::expect_equal(pol$luc_transfer_mgc, c(0, 0), tolerance = 1e-9)
  # The example carries the column and, with no vanishing, equals density x area.
  ex <- whep::build_carbon_balance(example = TRUE)
  testthat::expect_equal(
    ex$luc_transfer_mgc,
    ex$luc_transfer_mgc_ha * ex$area_ha
  )
})
