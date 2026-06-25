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
  res <- whep::build_manure_to_soil(
    .toy_intake_nat(),
    resolution = "national",
    gridded = .toy_gridded_nat()
  )
  expect_named(res, c("applied", "losses", "excretion"))
  bal <- .balance_n(res)
  expect_equal(bal[["out"]], bal[["excreted"]], tolerance = 1e-6)
})

test_that("applied output carries provenance for every stage", {
  res <- whep::build_manure_to_soil(
    .toy_intake_nat(),
    resolution = "national",
    gridded = .toy_gridded_nat()
  )
  prov <- c(
    "resolution",
    "method_n_excretion",
    "method_vs",
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
  res <- whep::build_manure_to_soil(
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
  res <- whep::build_manure_to_soil(
    intake,
    resolution = "subnational",
    gridded = gridded
  )
  bal <- .balance_n(res)
  expect_equal(bal[["out"]], bal[["excreted"]], tolerance = 1e-6)
  # Some collected manure originating in "1.5_40" is transported to "1_40".
  expect_true("transported" %in% res$applied$source_stream)
  expect_true(all(res$applied$method_transport == "room_weighted"))
})

test_that("build_manure_to_soil guards bad resolution and methods stage", {
  expect_error(
    whep::build_manure_to_soil(
      .toy_intake_nat(),
      resolution = "regional",
      gridded = .toy_gridded_nat()
    ),
    "resolution"
  )
  expect_error(
    whep::build_manure_to_soil(
      .toy_intake_nat(),
      methods = list(excrete = list(method = "x")),
      gridded = .toy_gridded_nat()
    ),
    "excrete"
  )
})
