# National intake: one cattle + one pig herd in ESP, sub_territory = NA.
.toy_intake_nat <- function() {
  tibble::tribble(
    ~year,
    ~territory,
    ~sub_territory,
    ~livestock_category,
    ~item_cbs_code,
    ~feed_quality,
    ~intake_dm_t,
    2020L,
    "ESP",
    NA,
    "Cattle_milk",
    2513L,
    "high_quality",
    200,
    2020L,
    "ESP",
    NA,
    "Cattle_milk",
    NA,
    "grass",
    600,
    2020L,
    "ESP",
    NA,
    "Pigs",
    2513L,
    "high_quality",
    100
  )
}

.toy_gridded_nat <- function(cap = 200) {
  list(
    crops = tibble::tribble(
      ~year,
      ~territory,
      ~sub_territory,
      ~crop,
      ~manure_n_receptivity,
      ~crop_n_cap,
      2020L,
      "ESP",
      NA,
      "barley",
      6,
      cap,
      2020L,
      "ESP",
      NA,
      "wheat",
      4,
      cap
    ),
    grass = tibble::tribble(
      ~year,
      ~territory,
      ~sub_territory,
      ~grass_n_cap,
      2020L,
      "ESP",
      NA,
      50
    )
  )
}

# Excreted N must equal field-applied N plus management losses (N2O-N, N2-N,
# volatilized, leached); indirect N2O is a sub-flux and is not added again.
.balance_n <- function(res) {
  applied <- sum(res$applied$applied_n)
  lost <- with(
    res$losses,
    sum(n_volatilized + n_leached + n2o_direct_n + n2_n)
  )
  excreted <- sum(res$excretion$n_excretion)
  c(out = applied + lost, excreted = excreted)
}

test_that("national run chains the pipeline and conserves the N balance", {
  res <- whep::build_livestock_nutrient_flows(
    .toy_intake_nat(),
    resolution = "national",
    gridded = .toy_gridded_nat()
  )
  expect_named(res, c("applied", "losses", "excretion"))
  bal <- .balance_n(res)
  expect_equal(bal[["out"]], bal[["excreted"]], tolerance = 1e-6)
})

test_that("applied output carries provenance for every stage", {
  res <- whep::build_livestock_nutrient_flows(
    .toy_intake_nat(),
    resolution = "national",
    gridded = .toy_gridded_nat()
  )
  prov <- c(
    "resolution",
    "method_n_excretion",
    "method_vs",
    "method_c_excretion",
    "method_forage_n",
    "method_mms",
    "method_losses",
    "method_allocation",
    "method_cap",
    "disposal_method"
  )
  expect_true(all(prov %in% names(res$applied)))
  expect_true(all(res$applied$resolution == "national"))
  expect_true(all(res$applied$method_n_excretion == "intake_minus_retention"))
})

test_that("method options are forwarded to the right stage", {
  res <- whep::build_livestock_nutrient_flows(
    .toy_intake_nat(),
    resolution = "national",
    methods = list(
      allocation = list(cap_method = "fixed_ceiling", method = "crop_n_demand")
    ),
    gridded = list(
      crops = dplyr::mutate(
        .toy_gridded_nat()$crops,
        crop_area_ha = c(1000, 800),
        crop_n_demand = c(3, 7)
      ),
      grass = dplyr::mutate(
        .toy_gridded_nat()$grass,
        grass_area_ha = 5000
      )
    )
  )
  expect_true(all(res$applied$method_cap == "fixed_ceiling"))
  expect_true(all(res$applied$method_allocation == "crop_n_demand"))
  # balance still holds under the alternative methods.
  bal <- .balance_n(res)
  expect_equal(bal[["out"]], bal[["excreted"]], tolerance = 1e-6)
})

test_that("subnational run transports surplus between cells and conserves N", {
  # Two ESP cells: the manure-heavy "1.5_40" has a tight cap (forces surplus),
  # the neighbour "1_40" has a loose cap (offers room).
  intake <- dplyr::bind_rows(
    dplyr::mutate(.toy_intake_nat(), sub_territory = "1.5_40"),
    dplyr::mutate(
      dplyr::filter(.toy_intake_nat(), livestock_category == "Pigs"),
      sub_territory = "1_40"
    )
  )
  gridded <- list(
    crops = tibble::tribble(
      ~year,
      ~territory,
      ~sub_territory,
      ~crop,
      ~manure_n_receptivity,
      ~crop_n_cap,
      2020L,
      "ESP",
      "1.5_40",
      "barley",
      6,
      0.5,
      2020L,
      "ESP",
      "1_40",
      "barley",
      6,
      500
    ),
    grass = tibble::tribble(
      ~year,
      ~territory,
      ~sub_territory,
      ~grass_n_cap,
      2020L,
      "ESP",
      "1.5_40",
      0.5,
      2020L,
      "ESP",
      "1_40",
      1
    )
  )
  res <- whep::build_livestock_nutrient_flows(
    intake,
    resolution = "subnational",
    gridded = gridded
  )
  bal <- .balance_n(res)
  expect_equal(bal[["out"]], bal[["excreted"]], tolerance = 1e-6)
  # Some collected manure originating in "1.5_40" is transported to "1_40".
  expect_true("transported" %in% res$applied$source_stream)
  expect_true(all(res$applied$method_transport == "room_weighted"))
  # No row escapes the documented land_use domain: transported manure lands as
  # "Cropland", never the internal "transported" label (issue #202).
  land_use_domain <- c("Cropland", "Grassland", "Disposal", "Unallocated")
  expect_true(all(res$applied$land_use %in% land_use_domain))
  # Provenance is scalar-correct on every row, including transported/disposed
  # rows: the caller's disposal choice, not the internal retain_unallocated pass.
  expect_true(all(res$applied$disposal_method == "over_apply_local"))
  expect_false(anyNA(res$applied$method_cap))
  expect_false(anyNA(res$applied$method_allocation))
})

test_that("applied output carries manure_type at national resolution", {
  res <- whep::build_livestock_nutrient_flows(
    .toy_intake_nat(),
    resolution = "national",
    gridded = .toy_gridded_nat()
  )
  expect_true(rlang::has_name(res$applied, "manure_type"))
  expect_true(all(c("Solid", "Liquid", "Excreta") %in% res$applied$manure_type))
  by_type <- dplyr::summarise(
    res$applied,
    applied_n = sum(.data$applied_n),
    .by = "manure_type"
  )
  expect_equal(sum(by_type$applied_n), sum(res$applied$applied_n))
})

test_that("subnational transport reattaches a sensible manure_type split, not a single collapsed value", {
  # Same mixed-species scenario as the transport test above: cattle + pigs
  # naturally split across Solid/Liquid/Excreta manure_type through the real
  # split_manure_management()/apply_management_losses() pipeline, so the
  # surplus that gets transported off "1.5_40" is itself a manure_type mix.
  intake <- dplyr::bind_rows(
    dplyr::mutate(.toy_intake_nat(), sub_territory = "1.5_40"),
    dplyr::mutate(
      dplyr::filter(.toy_intake_nat(), livestock_category == "Pigs"),
      sub_territory = "1_40"
    )
  )
  gridded <- list(
    crops = tibble::tribble(
      ~year,
      ~territory,
      ~sub_territory,
      ~crop,
      ~manure_n_receptivity,
      ~crop_n_cap,
      2020L,
      "ESP",
      "1.5_40",
      "barley",
      6,
      0.5,
      2020L,
      "ESP",
      "1_40",
      "barley",
      6,
      500
    ),
    grass = tibble::tribble(
      ~year,
      ~territory,
      ~sub_territory,
      ~grass_n_cap,
      2020L,
      "ESP",
      "1.5_40",
      0.5,
      2020L,
      "ESP",
      "1_40",
      1
    )
  )
  res <- whep::build_livestock_nutrient_flows(
    intake,
    resolution = "subnational",
    gridded = gridded
  )
  transported <- res$applied[res$applied$source_stream == "transported", ]
  expect_true(nrow(transported) > 0)
  expect_true(rlang::has_name(transported, "manure_type"))
  # Not lost (every transported row has a real label)...
  expect_false(anyNA(transported$manure_type))
  # ...and not collapsed to one value: the transported total is split across
  # more than one manure_type, mirroring the mixed surplus it came from.
  expect_true(length(unique(transported$manure_type)) > 1)
  # Overall mass balance still holds with the manure_type split reattached.
  bal <- .balance_n(res)
  expect_equal(bal[["out"]], bal[["excreted"]], tolerance = 1e-6)
})

test_that("transport room includes grass-only as well as crop-only cells", {
  local <- tibble::tibble(
    year = integer(),
    territory = character(),
    sub_territory = character(),
    source_stream = character(),
    land_use = character(),
    applied_n = double()
  )
  gridded <- list(
    crops = tibble::tibble(
      year = 2020L,
      territory = "ESP",
      sub_territory = "1_40",
      crop = "barley",
      manure_n_receptivity = 1,
      crop_n_cap = 100
    ),
    grass = tibble::tibble(
      year = 2020L,
      territory = "ESP",
      sub_territory = "1.5_40",
      grass_n_cap = 50
    )
  )

  out <- whep:::.cell_room(local, gridded, list())

  expect_setequal(out$sub_territory, c("1_40", "1.5_40"))
  expect_equal(out$room_n[out$sub_territory == "1_40"], 120)
  expect_equal(out$room_n[out$sub_territory == "1.5_40"], 60)
})

test_that("bedding reaches the field and widens the N balance (whep#1005)", {
  bedding <- tibble::tibble(
    year = 2020L,
    territory = "ESP",
    bedding_dm_t = 150,
    bedding_c_t = 68.7,
    bedding_n_t = 0.888
  )
  bare <- whep::build_livestock_nutrient_flows(
    .toy_intake_nat(),
    gridded = .toy_gridded_nat()
  )
  bedded <- whep::build_livestock_nutrient_flows(
    .toy_intake_nat(),
    gridded = .toy_gridded_nat(),
    bedding = bedding
  )

  # Bedding N is added outside (1 - FracLossMS), so it lands whole on the
  # field: the balance is now excreted + bedded = applied + losses.
  bal <- .balance_n(bedded)
  expect_equal(
    bal[["out"]],
    bal[["excreted"]] + sum(bedding$bedding_n_t),
    tolerance = 1e-6
  )
  expect_gt(sum(bedded$applied$applied_c), sum(bare$applied$applied_c))
  expect_true(all(bedded$applied$method_bedding_mms == "ipcc_2019"))
  expect_true(all(is.na(bare$applied$method_bedding_mms)))
})

test_that("build_livestock_nutrient_flows guards bad resolution and methods stage", {
  expect_error(
    whep::build_livestock_nutrient_flows(
      .toy_intake_nat(),
      resolution = "regional",
      gridded = .toy_gridded_nat()
    ),
    "resolution"
  )
  expect_error(
    whep::build_livestock_nutrient_flows(
      .toy_intake_nat(),
      methods = list(excrete = list(method = "x")),
      gridded = .toy_gridded_nat()
    ),
    "excrete"
  )
})

# ---- Transported manure's cropland/grassland split (whep#341) ----

# All herds sit on "1.5_40", which can hold almost nothing, so the collected
# surplus is trucked to its neighbour "1_40". That sink has little cropland
# room (1 t N cap, 1.2 t after the 1.2 tolerance) and a lot of grassland room
# (1000 t cap), so where the transported N lands depends on the split rule.
.toy_intake_sink_grass <- function() {
  dplyr::mutate(.toy_intake_nat(), sub_territory = "1.5_40")
}

.toy_gridded_sink_grass <- function() {
  list(
    crops = tibble::tribble(
      ~year , ~territory , ~sub_territory , ~crop    , ~manure_n_receptivity , ~crop_n_cap ,
      2020L , "ESP"      , "1.5_40"       , "barley" ,                     6 ,        0.01 ,
      2020L , "ESP"      , "1_40"         , "barley" ,                     6 ,           1
    ),
    grass = tibble::tribble(
      ~year , ~territory , ~sub_territory , ~grass_n_cap ,
      2020L , "ESP"      , "1.5_40"       ,         0.01 ,
      2020L , "ESP"      , "1_40"         ,         1000
    )
  )
}

.transported_by_land_use <- function(land_split = NULL) {
  methods <- if (is.null(land_split)) {
    list()
  } else {
    list(transport = list(land_split = land_split))
  }
  res <- whep::build_livestock_nutrient_flows(
    .toy_intake_sink_grass(),
    resolution = "subnational",
    methods = methods,
    gridded = .toy_gridded_sink_grass()
  )
  tr <- res$applied |>
    dplyr::filter(.data$source_stream == "transported") |>
    dplyr::summarise(
      applied_n = sum(.data$applied_n),
      applied_c = sum(.data$applied_c),
      .by = "land_use"
    )
  list(res = res, tr = tr)
}

.land_n <- function(tr, land_use) {
  sum(tr$applied_n[tr$land_use == land_use])
}

test_that("transported manure fills cropland room, then grassland (whep#341)", {
  out <- .transported_by_land_use()
  total <- sum(out$tr$applied_n)
  # The fixture really does send more than the sink's cropland can hold.
  expect_gt(total, 1.2)
  # Cropland takes exactly its remaining room, the rest lands on grassland.
  expect_equal(.land_n(out$tr, "Cropland"), 1.2, tolerance = 1e-9)
  expect_equal(.land_n(out$tr, "Grassland"), total - 1.2, tolerance = 1e-9)
  expect_true(all(
    out$res$applied$method_transport_land_use == "cropland_first"
  ))
  bal <- .balance_n(out$res)
  expect_equal(bal[["out"]], bal[["excreted"]], tolerance = 1e-6)
})

test_that("transport land split methods conserve the transported mass", {
  splits <- c("cropland_first", "room_share", "cropland_only")
  outs <- purrr::map(splits, .transported_by_land_use)
  totals_n <- purrr::map_dbl(outs, ~ sum(.x$tr$applied_n))
  totals_c <- purrr::map_dbl(outs, ~ sum(.x$tr$applied_c))
  expect_equal(totals_n, rep(totals_n[1], 3), tolerance = 1e-12)
  expect_equal(totals_c, rep(totals_c[1], 3), tolerance = 1e-12)
  purrr::walk2(outs, splits, function(o, s) {
    expect_true(all(o$res$applied$method_transport_land_use == s))
    bal <- .balance_n(o$res)
    expect_equal(bal[["out"]], bal[["excreted"]], tolerance = 1e-6)
  })
})

test_that("room_share splits transported manure by the sink's room shares", {
  out <- .transported_by_land_use("room_share")
  total <- sum(out$tr$applied_n)
  crop_share <- 1.2 / (1.2 + 1200)
  expect_equal(
    .land_n(out$tr, "Cropland"),
    total * crop_share,
    tolerance = 1e-9
  )
  expect_equal(
    .land_n(out$tr, "Grassland"),
    total * (1 - crop_share),
    tolerance = 1e-9
  )
})

test_that("cropland_only keeps the pre-#341 all-cropland landing", {
  out <- .transported_by_land_use("cropland_only")
  expect_equal(unique(out$tr$land_use), "Cropland")
})

test_that("an unknown transport land split aborts", {
  expect_error(.transported_by_land_use("grass_first"), "land_split")
})

test_that("without a grassland layer the default equals the old landing", {
  # No in-package caller passes gridded$grass, so this is the case every real
  # pipeline run is in: no grassland room, nothing for the split to move.
  crops_only <- list(crops = .toy_gridded_sink_grass()$crops)
  run <- function(land_split) {
    whep::build_livestock_nutrient_flows(
      .toy_intake_sink_grass(),
      resolution = "subnational",
      methods = list(transport = list(land_split = land_split)),
      gridded = crops_only
    )$applied |>
      dplyr::select(-"method_transport_land_use")
  }
  new <- run("cropland_first")
  expect_true("transported" %in% new$source_stream)
  expect_equal(new, run("cropland_only"))
})

test_that("non-subnational runs record no transport land split", {
  res <- whep::build_livestock_nutrient_flows(
    .toy_intake_nat(),
    gridded = .toy_gridded_nat()
  )
  expect_true(all(is.na(res$applied$method_transport_land_use)))
})

test_that("cell room is netted per land use against local placement", {
  local <- tibble::tribble(
    ~year , ~territory , ~sub_territory , ~source_stream , ~land_use   , ~applied_n ,
    2020L , "ESP"      , "1_40"         , "collected"    , "Cropland"  ,         30 ,
    2020L , "ESP"      , "1_40"         , "collected"    , "Grassland" ,         10 ,
    2020L , "ESP"      , "1_40"         , "grazing"      , "Grassland" ,        500
  )
  gridded <- list(
    crops = tibble::tibble(
      year = 2020L,
      territory = "ESP",
      sub_territory = "1_40",
      crop = "barley",
      manure_n_receptivity = 1,
      crop_n_cap = 100
    ),
    grass = tibble::tibble(
      year = 2020L,
      territory = "ESP",
      sub_territory = "1_40",
      grass_n_cap = 50
    )
  )

  out <- whep:::.cell_room(local, gridded, list())

  expect_equal(out$crop_room_n, 120 - 30)
  expect_equal(out$grass_room_n, 60 - 10)
  expect_equal(out$room_n, out$crop_room_n + out$grass_room_n)
})
