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
        "species_group",
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
        "method_species",
        "method_climate_zone",
        "method_diet"
      )
    )
})

testthat::test_that("the example fixture is what the function actually emits", {
  # The contract test above asserts only that columns EXIST, so a fixture whose
  # numbers drift away from the code passes it. That happened: the shipped
  # `manure_n2o_kt` was generated under the flat pasture EF3 and survived the
  # rebase onto whep#1019's weighted path, leaving the pkgdown reference page
  # 23.4% high on the one quantity this branch is about. Assert the values, so
  # the next coefficient change breaks the fixture instead of the example.
  #
  # Inputs are the example's OWN, not `.grid_fixture()`, which uses a different
  # country, different cells and different temperatures.
  herd <- tibble::tribble(
    ~lon, ~lat, ~year, ~area_code, ~species_group, ~heads,
    34.25, -0.25, 1961L, 114L, "cattle_dairy", 120000,
    35.25, 0.75, 1961L, 114L, "cattle_dairy", 80000,
    34.25, -0.25, 1961L, 114L, "cattle_non_dairy", 50000
  )
  climate <- tibble::tribble(
    ~lon, ~lat, ~year, ~mean_annual_temp_c, ~climate_zone,
    34.25, -0.25, 1961L, 22.51667, "Warm",
    35.25, 0.75, 1961L, 16.97500, "Temperate"
  ) |>
    dplyr::mutate(method_climate_zone = "cru_ts_annual")

  fixture <- whep::build_gridded_livestock_emissions(example = TRUE)
  live <- whep::build_gridded_livestock_emissions(
    herd,
    method_diet = "uniform_medium",
    data = list(cell_climate = climate)
  )

  for (col in c("enteric_ch4_kt", "manure_ch4_kt", "manure_n2o_kt")) {
    testthat::expect_equal(fixture[[col]], live[[col]], tolerance = 1e-6)
  }
  testthat::expect_equal(fixture$method_manure_ch4, live$method_manure_ch4)
  testthat::expect_equal(fixture$method_manure_n2o, live$method_manure_n2o)
  testthat::expect_equal(fixture$method_species, live$method_species)
  testthat::expect_equal(fixture$species_group, live$species_group)
})

testthat::test_that("options reach the manure kernel from the cells", {
  # whep#1022 makes `mcf_source` a user choice, and whep#1042 made this the
  # default Tier 2 entry point -- so an option that does not reach the kernel
  # from here is an option the default path cannot exercise. Asserted on the
  # kernel's own output, not just on acceptance of the argument.
  run <- function(src) {
    whep::build_gridded_livestock_emissions(
      .grid_fixture(),
      method_diet = "uniform_medium",
      options = list(mcf_source = src),
      data = list(cell_climate = .climate_fixture())
    ) |>
      dplyr::arrange(lon, lat, species)
  }
  shipped <- run("as_shipped")
  refined <- run("ipcc_2019")

  testthat::expect_true(all(
    shipped$method_manure_ch4 ==
      "IPCC_2019_Tier2; climate_from_data; mcf_as_shipped"
  ))
  testthat::expect_true(all(
    refined$method_manure_ch4 ==
      "IPCC_2019_Tier2; climate_from_data; mcf_ipcc_2019"
  ))
  # The table actually bites: manure CH4 moves, and only manure CH4.
  testthat::expect_false(isTRUE(all.equal(
    shipped$manure_ch4_kt,
    refined$manure_ch4_kt
  )))
  testthat::expect_equal(shipped$enteric_ch4_kt, refined$enteric_ch4_kt)
  testthat::expect_equal(shipped$manure_n2o_kt, refined$manure_n2o_kt)
  # The default is the Refinement, so asking for it changes nothing.
  testthat::expect_equal(
    whep::build_gridded_livestock_emissions(
      .grid_fixture(),
      method_diet = "uniform_medium",
      data = list(cell_climate = .climate_fixture())
    ) |>
      dplyr::arrange(lon, lat, species),
    refined
  )
})

testthat::test_that("an unknown option aborts before the climate join", {
  # Validated at the entry point, so a misspelled option is a fast error rather
  # than a silently ignored argument.
  testthat::expect_error(
    whep::build_gridded_livestock_emissions(
      .grid_fixture(),
      method_diet = "uniform_medium",
      options = list(mcf_sauce = "ipcc_2019"),
      data = list(cell_climate = .climate_fixture())
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

# Aggregate species groups (whep#1126) ----------------------------------------

# The country reports three sheep for every goat, and a row that is not a head
# count, which must not enter the mix.
.species_heads_fixture <- function() {
  tibble::tribble(
    ~year, ~area_code, ~polity_area_code, ~item_cbs_code,   ~unit, ~value,
    1961L,       197L,              197L,           976L, "heads",    300,
    1961L,       197L,              197L,          1016L, "heads",    100,
    1961L,       197L,              197L,          1016L, "tonnes",  9999
  )
}

.sheep_goat_grid <- function() {
  dplyr::mutate(.grid_fixture(), species_group = "sheep_goats")
}

.run_split <- function(grid, heads = .species_heads_fixture(), ...) {
  whep::build_gridded_livestock_emissions(
    grid,
    method_diet = "uniform_medium",
    ...,
    data = list(cell_climate = .climate_fixture(), species_heads = heads)
  )
}

testthat::test_that("refuse still aborts on an aggregate group", {
  testthat::expect_error(
    .run_split(.sheep_goat_grid(), method_species = "refuse"),
    "more than one IPCC species"
  )
})

testthat::test_that("an aggregate group is split by the national mix", {
  result <- .run_split(.sheep_goat_grid())

  testthat::expect_setequal(result$species, c("Sheep", "Goats"))
  testthat::expect_true(all(result$method_species == "national_head_share"))
  testthat::expect_false(anyNA(result$enteric_ch4_kt))
  by_species <- result |>
    dplyr::summarise(heads = sum(heads), .by = species)
  total <- sum(.sheep_goat_grid()$heads)
  testthat::expect_equal(
    by_species$heads[by_species$species == "Sheep"],
    0.75 * total
  )
  testthat::expect_equal(
    by_species$heads[by_species$species == "Goats"],
    0.25 * total
  )
})

testthat::test_that("the split conserves every cell's head count", {
  grid <- .sheep_goat_grid()
  result <- .run_split(grid)

  per_cell <- result |>
    dplyr::summarise(heads = sum(heads), .by = c(lon, lat, year, area_code))
  expected <- grid |>
    dplyr::summarise(heads = sum(heads), .by = c(lon, lat, year, area_code))
  testthat::expect_equal(
    dplyr::arrange(per_cell, lon, lat)$heads,
    dplyr::arrange(expected, lon, lat)$heads
  )
})

testthat::test_that("a split herd equals the same herd supplied by species", {
  grid <- .sheep_goat_grid()
  by_hand <- dplyr::bind_rows(
    dplyr::mutate(grid, species = "Sheep", heads = heads * 0.75),
    dplyr::mutate(grid, species = "Goats", heads = heads * 0.25)
  ) |>
    dplyr::select(-species_group)

  split <- .run_split(grid) |>
    dplyr::summarise(
      dplyr::across(c(enteric_ch4_kt, manure_ch4_kt, manure_n2o_kt), sum),
      .by = species
    ) |>
    dplyr::arrange(species)
  supplied <- whep::build_gridded_livestock_emissions(
    by_hand,
    method_diet = "uniform_medium",
    data = list(cell_climate = .climate_fixture())
  ) |>
    dplyr::summarise(
      dplyr::across(c(enteric_ch4_kt, manure_ch4_kt, manure_n2o_kt), sum),
      .by = species
    ) |>
    dplyr::arrange(species)

  testthat::expect_equal(split, supplied)
  # And the split is not a relabelling: goats do not emit like sheep.
  testthat::expect_false(isTRUE(all.equal(
    split$enteric_ch4_kt[1] / 0.25,
    split$enteric_ch4_kt[2] / 0.75
  )))
})

testthat::test_that("an area missing from the head table uses its polity", {
  # Sudan's two successor areas are gridded under their own codes but reported
  # nationally as the former-Sudan bucket 206.
  grid <- .sheep_goat_grid() |>
    dplyr::mutate(area_code = 276L, polity_area_code = 206L)
  heads <- .species_heads_fixture() |>
    dplyr::mutate(area_code = 206L, polity_area_code = 206L)
  climate <- .climate_fixture()

  result <- whep::build_gridded_livestock_emissions(
    grid,
    method_diet = "uniform_medium",
    data = list(cell_climate = climate, species_heads = heads)
  )

  testthat::expect_true(all(
    result$method_species == "polity_bucket_head_share"
  ))
  testthat::expect_equal(sum(result$heads), sum(grid$heads))
})

testthat::test_that("a group the country reports no member of stays NA", {
  grid <- dplyr::bind_rows(
    dplyr::slice(.grid_fixture(), 1L),
    dplyr::mutate(dplyr::slice(.grid_fixture(), 2L), species_group = "equines")
  )

  testthat::expect_warning(
    result <- .run_split(grid),
    "could not be split"
  )
  unsplit <- dplyr::filter(result, species_group == "equines")
  testthat::expect_equal(nrow(unsplit), 1L)
  testthat::expect_equal(unsplit$heads, 80000)
  testthat::expect_true(is.na(unsplit$enteric_ch4_kt))
  testthat::expect_equal(unsplit$method_species, "unsplit_no_national_mix")
  testthat::expect_false(anyNA(
    result$enteric_ch4_kt[result$species_group == "cattle_dairy"]
  ))
})

testthat::test_that("a head table without the needed columns aborts", {
  testthat::expect_error(
    .run_split(
      .sheep_goat_grid(),
      heads = dplyr::select(.species_heads_fixture(), -item_cbs_code)
    ),
    "cannot split an aggregate species group"
  )
})

testthat::test_that("the head table is not read when no group is aggregate", {
  # A NULL `species_heads` would fall back to the production pins; a
  # single-species grid must never reach that read.
  testthat::local_mocked_bindings(
    .read_species_heads = function(...) stop("read species heads")
  )
  result <- whep::build_gridded_livestock_emissions(
    .grid_fixture(),
    method_diet = "uniform_medium",
    data = list(cell_climate = .climate_fixture())
  )
  testthat::expect_true(all(result$method_species == "one_to_one"))
})

testthat::test_that("every spatializer group is either mapped or split", {
  # The split reads its members from the mapping the spatializer groups with;
  # a group added there must land in one of the two paths, not neither.
  members <- whep:::.livestock_group_members()
  mapping <- readr::read_csv(
    system.file("extdata", "livestock_mapping.csv", package = "whep"),
    show_col_types = FALSE
  )
  testthat::expect_setequal(members$item_cbs_code, mapping$item_code)
  testthat::expect_false(anyNA(members$species))
  aggregates <- setdiff(
    unique(mapping$species_group),
    whep:::.gridded_species_map()$species_group
  )
  testthat::expect_setequal(
    aggregates,
    c("sheep_goats", "equines", "poultry", "other")
  )
})

testthat::test_that("the default diet rung is per_cell_feed on a split herd", {
  # whep#1126: the default rung must key a split species onto its own feed.
  intake <- tibble::tribble(
    ~year, ~area_code, ~sub_territory, ~live_anim_code, ~item_cbs_code,
    ~intake_dry_matter,
    1961L,       197L,  "34.25_-0.25",            976L,          2555L,
    9000,
    1961L,       197L,  "34.25_-0.25",           1016L,          2106L,
    9000,
    1961L,       197L,   "34.75_0.25",            976L,          3000L,
    9000,
    1961L,       197L,   "34.75_0.25",           1016L,          3000L,
    9000
  )
  result <- whep::build_gridded_livestock_emissions(
    .sheep_goat_grid(),
    data = list(
      cell_climate = .climate_fixture(),
      feed_intake = intake,
      species_heads = .species_heads_fixture()
    )
  )

  testthat::expect_true(all(result$method_diet == "per_cell_feed"))
  warm <- dplyr::filter(result, lon == 34.25)
  testthat::expect_equal(
    unique(warm$diet_quality[warm$species == "Sheep"]),
    "High"
  )
  testthat::expect_equal(
    unique(warm$diet_quality[warm$species == "Goats"]),
    "Low"
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
