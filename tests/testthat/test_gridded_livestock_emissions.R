# test_gridded_livestock_emissions.R ------------------------------------------

# Two cells of one country either side of the 18 degC Temperate/Warm cut, so the
# gridded run and a run on the country's head-weighted mean temperature cannot
# agree by construction.
.grid_fixture <- function() {
  tibble::tribble(
    ~lon,   ~lat, ~year, ~area_code,     ~species_group, ~heads,
    34.25, -0.25, 1961L,       197L,     "cattle_dairy", 120000,
    34.75,  0.25, 1961L,       197L,     "cattle_dairy",  80000,
    34.25, -0.25, 1961L,       197L, "cattle_non_dairy",  50000
  )
}

.climate_fixture <- function() {
  tibble::tribble(
    ~lon,   ~lat, ~year, ~mean_annual_temp_c, ~climate_zone,
    34.25, -0.25, 1961L,                21.1,        "Warm",
    34.75,  0.25, 1961L,                17.4,   "Temperate"
  ) |>
    dplyr::mutate(method_climate_zone = "cru_ts_annual")
}

# Cell-grain feed intake: the warm cell's dairy herd eats concentrates, the cool
# cell's eats grass, and the non-dairy herd eats straw.
.intake_fixture <- function() {
  tibble::tribble(
    ~year, ~area_code, ~sub_territory, ~live_anim_code, ~item_cbs_code,
    ~intake_dry_matter,
    1961L,       197L,  "34.25_-0.25",            960L,          2555L,
    9000,
    1961L,       197L,   "34.75_0.25",            960L,          3000L,
    9000,
    1961L,       197L,  "34.25_-0.25",            961L,          2106L,
    4000
  )
}

# livestock_emissions_to_kt ---------------------------------------------------

testthat::test_that("the kilogram to kilotonne factor is exactly 1e6", {
  result <- tibble::tibble(
    enteric_ch4_tier1 = 8e7,
    manure_ch4_tier1 = 1e7,
    manure_n2o_total = 5e5
  ) |>
    whep::livestock_emissions_to_kt(tier = 1)

  testthat::expect_equal(result$enteric_ch4_kt, 80)
  testthat::expect_equal(result$manure_ch4_kt, 10)
  testthat::expect_equal(result$manure_n2o_kt, 0.5)
})

testthat::test_that("the bridge reads the requested tier's columns", {
  result <- tibble::tibble(
    enteric_ch4_tier2 = 2e6,
    manure_ch4_tier2 = 1e6,
    manure_n2o_total = 1e6
  ) |>
    whep::livestock_emissions_to_kt(tier = 2)

  testthat::expect_equal(result$enteric_ch4_kt, 2)
})

testthat::test_that("a missing kilogram column aborts instead of zeroing", {
  testthat::expect_error(
    whep::livestock_emissions_to_kt(
      tibble::tibble(enteric_ch4_tier1 = 1e6),
      tier = 1
    ),
    "manure_ch4_tier1"
  )
})

# build_gridded_livestock_emissions -------------------------------------------

testthat::test_that("the example fixture matches the documented contract", {
  result <- whep::build_gridded_livestock_emissions(example = TRUE)

  result |>
    pointblank::expect_col_exists(
      c(
        "year",
        "area_code",
        "lon",
        "lat",
        "species",
        "heads",
        "enteric_ch4_kt",
        "manure_ch4_kt",
        "manure_n2o_kt",
        "enteric_ch4_national_kt",
        "manure_ch4_national_kt",
        "manure_n2o_national_kt",
        "divergence_enteric_ch4",
        "divergence_manure_ch4",
        "divergence_manure_n2o",
        "climate_zone",
        "diet_quality",
        "method_climate_zone",
        "method_diet"
      )
    )
})

testthat::test_that("emissions are per cell and positive", {
  result <- whep::build_gridded_livestock_emissions(
    .grid_fixture(),
    method_diet = "uniform_medium",
    data = list(cell_climate = .climate_fixture())
  )

  testthat::expect_equal(nrow(result), 3L)
  testthat::expect_true(all(result$enteric_ch4_kt > 0))
  testthat::expect_true(all(result$manure_ch4_kt > 0))
  testthat::expect_true(all(result$manure_n2o_kt > 0))
})

testthat::test_that("a cell's climate zone reaches its manure CH4", {
  result <- whep::build_gridded_livestock_emissions(
    .grid_fixture(),
    method_diet = "uniform_medium",
    data = list(cell_climate = .climate_fixture())
  )

  dairy <- result |>
    dplyr::filter(species == "Cattle, dairy") |>
    dplyr::mutate(per_head = manure_ch4_kt / heads)

  # Same species, same diet, same head basis: the only difference between the
  # two cells is the climate zone, so their per-head manure CH4 must differ.
  testthat::expect_false(isTRUE(all.equal(
    dairy$per_head[dairy$climate_zone == "Warm"],
    dairy$per_head[dairy$climate_zone == "Temperate"]
  )))
})

testthat::test_that("the national grain is emitted beside the gridded one", {
  result <- whep::build_gridded_livestock_emissions(
    .grid_fixture(),
    method_diet = "uniform_medium",
    data = list(cell_climate = .climate_fixture())
  )

  dairy <- dplyr::filter(result, species == "Cattle, dairy")
  # Two cells in different zones cannot reproduce a single national-mean zone.
  testthat::expect_false(isTRUE(all.equal(
    sum(dairy$manure_ch4_kt),
    sum(dairy$manure_ch4_national_kt)
  )))
  # And the rescaled column is exactly the national run spread over the cells.
  testthat::expect_equal(
    sum(dairy$manure_ch4_national_kt) * dairy$divergence_manure_ch4[1],
    sum(dairy$manure_ch4_kt)
  )
})

testthat::test_that("the divergence ratio is per country, not global", {
  result <- whep::build_gridded_livestock_emissions(
    .grid_fixture(),
    method_diet = "uniform_medium",
    data = list(cell_climate = .climate_fixture())
  )

  # The single-cell non-dairy herd cannot diverge; the two-cell dairy herd does.
  ratios <- result |>
    dplyr::distinct(species, divergence_manure_ch4)
  testthat::expect_equal(
    ratios$divergence_manure_ch4[ratios$species == "Cattle, non-dairy"],
    1
  )
  testthat::expect_false(isTRUE(all.equal(
    ratios$divergence_manure_ch4[ratios$species == "Cattle, dairy"],
    1
  )))
})

testthat::test_that("a cell with no climate row aborts", {
  testthat::expect_error(
    whep::build_gridded_livestock_emissions(
      .grid_fixture(),
      method_diet = "uniform_medium",
      data = list(cell_climate = dplyr::slice(.climate_fixture(), 1))
    ),
    "no climate zone"
  )
})

testthat::test_that("an aggregate species group aborts rather than splitting", {
  grid <- .grid_fixture() |>
    dplyr::mutate(species_group = "sheep_goats")

  testthat::expect_error(
    whep::build_gridded_livestock_emissions(
      grid,
      method_diet = "uniform_medium",
      data = list(cell_climate = .climate_fixture())
    ),
    "more than one IPCC species"
  )
})

testthat::test_that("zero-head cells are dropped before weighting", {
  grid <- dplyr::bind_rows(
    .grid_fixture(),
    tibble::tibble(
      lon = 34.75,
      lat = 0.25,
      year = 1961L,
      area_code = 197L,
      species_group = "cattle_non_dairy",
      heads = 0
    )
  )

  result <- whep::build_gridded_livestock_emissions(
    grid,
    method_diet = "uniform_medium",
    data = list(cell_climate = .climate_fixture())
  )

  testthat::expect_equal(nrow(result), 3L)
})

testthat::test_that("a missing head column aborts", {
  testthat::expect_error(
    whep::build_gridded_livestock_emissions(
      dplyr::select(.grid_fixture(), -heads),
      method_diet = "uniform_medium",
      data = list(cell_climate = .climate_fixture())
    ),
    "heads"
  )
})

# Diet ladder -----------------------------------------------------------------

testthat::test_that("the diet method used is recorded per row", {
  result <- whep::build_gridded_livestock_emissions(
    .grid_fixture(),
    method_diet = "per_cell_feed",
    data = list(
      cell_climate = .climate_fixture(),
      feed_intake = .intake_fixture()
    )
  )

  testthat::expect_true(all(result$method_diet == "per_cell_feed"))
})

testthat::test_that("the cell's own feed mix sets the cell's diet", {
  result <- whep::build_gridded_livestock_emissions(
    .grid_fixture(),
    method_diet = "per_cell_feed",
    data = list(
      cell_climate = .climate_fixture(),
      feed_intake = .intake_fixture()
    )
  )

  diets <- result |>
    dplyr::select(species, lon, diet_quality)
  # Concentrate-fed dairy cell -> High; grass-fed dairy cell -> Medium;
  # straw-fed non-dairy -> Low.
  testthat::expect_setequal(diets$diet_quality, c("High", "Medium", "Low"))
})

testthat::test_that("a cell with no classifiable feed falls to the nation", {
  intake <- dplyr::filter(.intake_fixture(), sub_territory != "34.75_0.25")

  result <- whep::build_gridded_livestock_emissions(
    .grid_fixture(),
    method_diet = "per_cell_feed",
    data = list(
      cell_climate = .climate_fixture(),
      feed_intake = intake
    )
  )

  fallen <- dplyr::filter(result, lon == 34.75)
  testthat::expect_equal(fallen$method_diet, "national_feed")
  testthat::expect_true(all(
    result$method_diet[result$lon == 34.25] == "per_cell_feed"
  ))
})

testthat::test_that("uniform_medium is never selected implicitly", {
  testthat::expect_error(
    whep::build_gridded_livestock_emissions(
      .grid_fixture(),
      method_diet = "per_cell_feed",
      data = list(cell_climate = .climate_fixture())
    ),
    "feed-intake table"
  )
})

testthat::test_that("an unresolvable diet aborts instead of becoming Medium", {
  # Scavenging carries no digestible-energy anchor, so nothing can be derived
  # from it at either grain.
  intake <- .intake_fixture() |>
    dplyr::mutate(item_cbs_code = 3500L)

  testthat::expect_error(
    whep::build_gridded_livestock_emissions(
      .grid_fixture(),
      method_diet = "per_cell_feed",
      data = list(
        cell_climate = .climate_fixture(),
        feed_intake = intake
      )
    ),
    "no diet"
  )
})

testthat::test_that("an unknown diet method is rejected", {
  testthat::expect_error(
    whep::build_gridded_livestock_emissions(
      .grid_fixture(),
      method_diet = "whatever",
      data = list(cell_climate = .climate_fixture())
    )
  )
})

# .feed_quality_de_anchors / .diet_quality_from_de ----------------------------

testthat::test_that("every DE anchor comes from feed_characteristics", {
  anchors <- whep:::.feed_quality_de_anchors()

  testthat::expect_true(all(
    anchors$de_anchor_percent %in% whep::feed_characteristics$de_percent
  ))
  testthat::expect_false(anyNA(anchors$de_anchor_percent))
})

testthat::test_that("classes with no defensible DE are left unanchored", {
  anchors <- whep:::.feed_quality_de_anchors()

  testthat::expect_false(any(
    c("zoot_fixed", "scavenging", "draught", "non_feed") %in%
      anchors$feed_quality
  ))
})

testthat::test_that("DE is classified by nearest shipped anchor", {
  # feed_characteristics ships 55 / 65 / 75, so the cuts are 60 and 70.
  testthat::expect_equal(
    whep:::.diet_quality_from_de(c(55, 59.9, 60, 65, 69.9, 70, 75)),
    c("Low", "Low", "Medium", "Medium", "Medium", "High", "High")
  )
})

testthat::test_that("unanchored feed mass never drags the mean", {
  # A herd eating equal mass of straw (Low, anchored) and minerals (unanchored)
  # is a straw diet, not a half-way one.
  testthat::expect_equal(
    whep:::.anchored_mean_de(c(55, NA), c(100, 100)),
    55
  )
  testthat::expect_true(is.na(whep:::.anchored_mean_de(c(NA, NA), c(1, 1))))
})

# Coverage gaps ---------------------------------------------------------------

testthat::test_that("a species with no Tier 2 coefficients warns and is NA", {
  grid <- .grid_fixture() |>
    dplyr::mutate(species_group = "pigs")

  testthat::expect_warning(
    result <- whep::build_gridded_livestock_emissions(
      grid,
      method_diet = "uniform_medium",
      data = list(cell_climate = .climate_fixture())
    ),
    "Unresolved emissions"
  )

  testthat::expect_true(all(is.na(result$enteric_ch4_kt)))
  # NA, never a zero that would look like a real measurement of no emission.
  testthat::expect_false(any(result$enteric_ch4_kt %in% 0))
})

testthat::test_that("a mixed herd keeps the species Tier 2 does resolve", {
  grid <- dplyr::bind_rows(
    .grid_fixture(),
    tibble::tibble(
      lon = 34.25,
      lat = -0.25,
      year = 1961L,
      area_code = 197L,
      species_group = "pigs",
      heads = 10000
    )
  )

  result <- suppressWarnings(whep::build_gridded_livestock_emissions(
    grid,
    method_diet = "uniform_medium",
    data = list(cell_climate = .climate_fixture())
  ))

  cattle <- dplyr::filter(result, species != "Pigs")
  testthat::expect_false(anyNA(cattle$enteric_ch4_kt))
  testthat::expect_true(all(is.na(
    result$enteric_ch4_kt[result$species == "Pigs"]
  )))
})
