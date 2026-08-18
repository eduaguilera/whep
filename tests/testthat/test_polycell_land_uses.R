# Two polities share cell A; polity X also holds cell B. Every input is injected
# so the suite never reaches the network or a WHEP_* path.

.plu_fx_support <- function() {
  tibble::tribble(
    ~polycell_id, ~lon, ~lat, ~polity_code, ~area_code, ~year, ~land_area_ha,
    "A-X", 0.25, 0.25, "X-1900-2025", 10L, 2000L, 60,
    "A-Y", 0.25, 0.25, "Y-1900-2025", 20L, 2000L, 40,
    "B-X", 0.75, 0.25, "X-1900-2025", 10L, 2000L, 100
  )
}

# `fraction` is LUH2's share of the WHOLE cell and is repeated identically on
# every polycell of a border cell, which is why the producer takes shares from
# it rather than from area_ha.
.plu_fx_pattern <- function() {
  tibble::tribble(
    ~lon, ~lat, ~year, ~land_use, ~fraction,
    0.25, 0.25, 2000L, "cropland", 0.30,
    0.25, 0.25, 2000L, "grassland", 0.10,
    0.25, 0.25, 2000L, "urban", 0.05,
    0.25, 0.25, 2000L, "natural", 0.55,
    0.75, 0.25, 2000L, "cropland", 0.20,
    0.75, 0.25, 2000L, "grassland", 0.20,
    0.75, 0.25, 2000L, "natural", 0.60
  )
}

# Injected empty rather than left NULL: an absent input falls back to its reader,
# and the suite must never reach the network (CLAUDE.md).
.plu_fx_no_meadows <- function() {
  tibble::tibble(
    area_code = integer(0),
    year = integer(0),
    meadow_ha = numeric(0)
  )
}

.plu_fx_no_trend <- function() {
  tibble::tibble(
    area_code = integer(0),
    year = integer(0),
    luh2_ha = numeric(0)
  )
}

# Spillover derives the forest split from the raw LUH2 states when it is not
# supplied, which is a download. Injected empty here so the suite stays offline;
# the test that actually exercises the preference supplies a real split.
.plu_fx_no_split <- function() {
  tibble::tibble(
    lon = numeric(0),
    lat = numeric(0),
    nonforest_share = numeric(0)
  )
}

# X: cropland level is 2x its pattern, grassland level 0.5x.
# Y: cropland 0.5x, grassland 2x.
.plu_fx_cropland <- function() {
  tibble::tribble(
    ~area_code, ~year, ~cropland_ha, ~source,
    10L, 2000L, 76, "fao",
    20L, 2000L, 6, "fao"
  )
}

.plu_fx_grassland <- function() {
  tibble::tribble(
    ~area_code, ~year, ~impact_u,
    10L, 2000L, 13,
    20L, 2000L, 8
  )
}

.plu_fx_data <- function(...) {
  # Replaced wholesale, never merged: utils::modifyList() recurses into a
  # tibble (a tibble is a list) and would splice columns of different lengths.
  base <- list(
    polycell_support = .plu_fx_support(),
    pattern = .plu_fx_pattern(),
    cropland_level = .plu_fx_cropland(),
    grassland_level = .plu_fx_grassland(),
    temporary_meadows = .plu_fx_no_meadows(),
    luh2_grassland = .plu_fx_no_trend(),
    natural_split = .plu_fx_no_split()
  )
  over <- list(...)
  base[names(over)] <- over
  base
}

.plu_run <- function(...) {
  whep::build_polycell_land_uses(data = .plu_fx_data(...))
}

test_that("classes partition the polycell's land exactly", {
  out <- .plu_run()
  totals <- out |>
    dplyr::summarise(total = sum(.data$area_ha), .by = c("polycell_id", "year"))
  expect_equal(
    totals$total[match(c("A-X", "A-Y", "B-X"), totals$polycell_id)],
    c(60, 40, 100)
  )
})

test_that("inland water and ice never become a land use", {
  out <- .plu_run()
  expect_false(any(c("inland_water", "ice") %in% out$land_use))
  expect_setequal(
    unique(out$land_use),
    c("cropland", "grassland", "urban", "natural")
  )
})

test_that("national class totals reproduce the statistical level", {
  out <- .plu_run() |>
    dplyr::filter(.data$area_source == "anchored") |>
    dplyr::summarise(
      total = sum(.data$area_ha),
      .by = c("area_code", "land_use")
    )
  expect_equal(
    out$total[out$area_code == 10L & out$land_use == "cropland"],
    76
  )
  expect_equal(
    out$total[out$area_code == 10L & out$land_use == "grassland"],
    13
  )
  expect_equal(out$total[out$area_code == 20L & out$land_use == "cropland"], 6)
  expect_equal(out$total[out$area_code == 20L & out$land_use == "grassland"], 8)
})

test_that("the pattern comes from fraction, not from a per-polity area", {
  # Both polycells of cell A see the same `fraction`; their cropland must be in
  # proportion to their own land (60:40), not to anything the pattern says about
  # territory.
  out <- .plu_run() |>
    dplyr::filter(.data$land_use == "urban")
  urban <- out$area_ha[match(c("A-X", "A-Y"), out$polycell_id)]
  expect_equal(urban, c(0.05 * 60, 0.05 * 40))
})

test_that("the statistical-versus-pattern disagreement is named, not absorbed", {
  out <- .plu_run()
  ax_crop <- out |>
    dplyr::filter(.data$polycell_id == "A-X", .data$land_use == "cropland")
  # pattern 0.30 * 60 = 18; anchored to 2x the national pattern -> 36
  expect_equal(ax_crop$area_ha, 36)
  expect_equal(ax_crop$statistical_pattern_disagreement_ha, 18)
  # natural is the residual and carries no disagreement of its own
  nat <- out |> dplyr::filter(.data$land_use == "natural")
  expect_true(all(is.na(nat$statistical_pattern_disagreement_ha)))
})

test_that("a backcast level is excluded from the disagreement diagnostic", {
  crop <- .plu_fx_cropland()
  crop$source <- "luh2"
  out <- .plu_run(cropland_level = crop) |>
    dplyr::filter(.data$land_use == "cropland")
  expect_true(all(out$level_source == "luh2_backcast_cropland"))
  expect_true(all(is.na(out$statistical_pattern_disagreement_ha)))
  # the level still binds; only the diagnostic is withheld
  expect_equal(sum(out$area_ha[out$area_code == 10L]), 76)
})

test_that("urban is pattern-only and says so", {
  out <- .plu_run() |> dplyr::filter(.data$land_use == "urban")
  expect_true(all(out$area_source == "pattern_only"))
  expect_true(all(out$allocation_status == "no_level_source"))
})

test_that("a polycell with no pattern is unclassified, never natural", {
  support <- dplyr::bind_rows(
    .plu_fx_support(),
    tibble::tibble(
      polycell_id = "C-X",
      lon = 5.25,
      lat = 0.25,
      polity_code = "X-1900-2025",
      area_code = 10L,
      year = 2000L,
      land_area_ha = 20
    )
  )
  out <- .plu_run(polycell_support = support) |>
    dplyr::filter(.data$polycell_id == "C-X")
  expect_equal(out$land_use, "unclassified")
  expect_equal(out$area_ha, 20)
  expect_equal(out$coverage_status, "unavailable")
})

test_that("a cell whose LUH2 states sum to zero is unclassified, not natural", {
  # #548: a land row of exactly 0 is PRESENT in the pattern, so an absence test
  # misses it and the residual would book the whole cell as natural.
  pattern <- dplyr::bind_rows(
    .plu_fx_pattern(),
    tibble::tibble(
      lon = 5.25,
      lat = 0.25,
      year = 2000L,
      land_use = c("cropland", "natural"),
      fraction = c(0, 0)
    )
  )
  support <- dplyr::bind_rows(
    .plu_fx_support(),
    tibble::tibble(
      polycell_id = "C-X",
      lon = 5.25,
      lat = 0.25,
      polity_code = "X-1900-2025",
      area_code = 10L,
      year = 2000L,
      land_area_ha = 20
    )
  )
  out <- .plu_run(polycell_support = support, pattern = pattern) |>
    dplyr::filter(.data$polycell_id == "C-X")
  expect_equal(out$land_use, "unclassified")
  expect_equal(out$area_ha, 20)
})

test_that("temporary meadows keep the FAO class but ride the grass pattern", {
  meadows <- tibble::tribble(
    ~area_code, ~year, ~meadow_ha,
    10L, 2000L, 38
  )
  out <- .plu_run(temporary_meadows = meadows)
  crop <- out |> dplyr::filter(.data$land_use == "cropland")
  # the level is unchanged in total: it is still FAO cropland
  expect_equal(sum(crop$area_ha[crop$area_code == 10L]), 76)
  # but half of it now follows grassland's 6:20 split, not cropland's 18:20
  ax <- crop$area_ha[crop$polycell_id == "A-X"]
  expect_equal(ax, 38 * (18 / 38) + 38 * (6 / 26))
  expect_equal(sum(out$area_ha[out$polycell_id == "A-X"]), 60)
})

test_that("the pasture backcast splices onto the FAO 1961 level", {
  support <- .plu_fx_support() |>
    dplyr::mutate(year = 1960L) |>
    dplyr::bind_rows(dplyr::mutate(.plu_fx_support(), year = 1961L))
  pattern <- dplyr::bind_rows(
    dplyr::mutate(.plu_fx_pattern(), year = 1960L),
    dplyr::mutate(.plu_fx_pattern(), year = 1961L)
  )
  crop <- tidyr::expand_grid(
    area_code = c(10L, 20L),
    year = c(1960L, 1961L)
  ) |>
    dplyr::mutate(cropland_ha = 10, source = "fao")
  grass <- tibble::tribble(
    ~area_code, ~year, ~impact_u,
    10L, 1961L, 26,
    20L, 1961L, 4
  )
  # LUH2's own national grass trend: half as much in 1960 as in 1961
  luh2 <- tibble::tribble(
    ~area_code, ~year, ~luh2_ha,
    10L, 1960L, 50,
    10L, 1961L, 100,
    20L, 1960L, 50,
    20L, 1961L, 100
  )
  out <- suppressWarnings(whep::build_polycell_land_uses(
    data = .plu_fx_data(
      polycell_support = support,
      pattern = pattern,
      cropland_level = crop,
      grassland_level = grass,
      luh2_grassland = luh2
    )
  )) |>
    dplyr::filter(.data$land_use == "grassland", .data$area_code == 10L) |>
    dplyr::summarise(total = sum(.data$area_ha), .by = "year")

  expect_equal(out$total[out$year == 1961L], 26)
  expect_equal(out$total[out$year == 1960L], 13)
  expect_true(all(
    suppressWarnings(whep::build_polycell_land_uses(
      years = 1960L,
      data = .plu_fx_data(
        polycell_support = support,
        pattern = pattern,
        cropland_level = crop,
        grassland_level = grass,
        luh2_grassland = luh2
      )
    )) |>
      dplyr::filter(.data$land_use == "grassland") |>
      dplyr::pull("level_source") ==
      "luh2_backcast_pasture"
  ))
})

# ---- overfull reconciliation -------------------------------------------------

# X's cropland level is far beyond what its two polycells can hold.
.plu_fx_overfull <- function() {
  .plu_fx_data(
    cropland_level = tibble::tribble(
    ~area_code, ~year, ~cropland_ha, ~source,
    10L, 2000L, 200, "fao",
    20L, 2000L, 6, "fao"
  )
  )
}

test_that("overfull_method is validated and recorded", {
  expect_error(
    whep::build_polycell_land_uses(overfull_method = "nope"),
    class = "rlang_error"
  )
  out <- whep::build_polycell_land_uses(
    data = .plu_fx_overfull(),
    overfull_method = "cap"
  )
  expect_true(all(out$method_overfull == "cap"))
})

test_that("cap holds the partition and names the shortfall", {
  out <- whep::build_polycell_land_uses(
    data = .plu_fx_overfull(),
    overfull_method = "cap"
  )
  totals <- out |>
    dplyr::summarise(total = sum(.data$area_ha), .by = "polycell_id")
  expect_equal(
    totals$total[match(c("A-X", "A-Y", "B-X"), totals$polycell_id)],
    c(60, 40, 100)
  )
  # X asked for 200 + 13 of agriculture across 160 ha of land, minus 3 urban
  expect_gt(sum(out$unplaceable_statistical_ha), 0)
  expect_equal(
    sum(out$area_ha[out$area_source == "anchored"]) +
      sum(out$unplaceable_statistical_ha),
    200 + 13 + 6 + 8
  )
})

test_that("spillover conserves: anchored in equals placed plus unplaceable", {
  # The fixture country is genuinely full (220 ha of level over 160 ha of land),
  # so the unplaceable warning is the documented behaviour, asserted below.
  out <- suppressWarnings(whep::build_polycell_land_uses(
    data = .plu_fx_overfull(),
    overfull_method = "spillover"
  ))
  totals <- out |>
    dplyr::summarise(total = sum(.data$area_ha), .by = "polycell_id")
  expect_equal(
    totals$total[match(c("A-X", "A-Y", "B-X"), totals$polycell_id)],
    c(60, 40, 100)
  )
  expect_equal(
    sum(out$area_ha[out$area_source == "anchored"]) +
      sum(out$unplaceable_statistical_ha),
    200 + 13 + 6 + 8
  )
})

# A neighbour with spare land that holds ONE of the classes in excess but not
# the other. Cell C carries a grassland fraction and no cropland, so C-X has a
# grassland row to be credited and no cropland row -- while
# `.plu_receiver_capacity()`, built off the SUPPORT rather than off the
# allocation, offers its spare land to either class. That is the shape that
# defeats the polycell-level prune and reaches `.plu_ring_pairs()`: country 10
# is over on both classes, so C-X is a live receiver, and the CROPLAND excess
# sent there had no (polycell_id, year, land_use) row for
# `.plu_apply_received()` to credit. The hectare was dropped, the donor stayed
# debited, the residual rebooked the land as natural and
# `unplaceable_statistical_ha` still read 0 -- the quiet cap the method contract
# forbids, and 0.35 Mha of it on the real 2020 build.
.plu_fx_orphan_receiver <- function() {
  .plu_fx_data(
    polycell_support = dplyr::bind_rows(
      .plu_fx_support(),
      tibble::tibble(
        polycell_id = "C-X",
        lon = 0.75,
        lat = 0.75,
        polity_code = "X-1900-2025",
        area_code = 10L,
        year = 2000L,
        land_area_ha = 100
      )
    ),
    pattern = dplyr::bind_rows(
      .plu_fx_pattern(),
      tibble::tribble(
        ~lon, ~lat, ~year, ~land_use, ~fraction,
        0.75, 0.75, 2000L, "grassland", 0.4,
        0.75, 0.75, 2000L, "natural", 0.6
      )
    ),
    cropland_level = tibble::tribble(
      ~area_code, ~year, ~cropland_ha, ~source,
      10L, 2000L, 200, "fao",
      20L, 2000L, 6, "fao"
    ),
    grassland_level = tibble::tribble(
      ~area_code, ~year, ~impact_u,
      10L, 2000L, 100,
      20L, 2000L, 8
    )
  )
}

test_that("spillover never credits a class a receiver has no row for", {
  args <- .plu_fx_orphan_receiver()
  out <- suppressWarnings(whep::build_polycell_land_uses(
    data = args,
    overfull_method = "spillover"
  ))
  # The identity the silent drop broke: every anchored hectare is either
  # published in a class row or named as unplaceable. Measured against `cap`,
  # which reconciles the same input without moving anything, rather than written
  # down twice.
  capped <- whep::build_polycell_land_uses(data = args, overfull_method = "cap")
  expect_equal(
    sum(out$area_ha[out$area_source == "anchored"]) +
      sum(out$unplaceable_statistical_ha),
    sum(capped$area_ha[capped$area_source == "anchored"]) +
      sum(capped$unplaceable_statistical_ha)
  )
  # And it is still a partition: what cannot be credited stays with the donor as
  # unplaceable instead of quietly becoming the receiver's natural land.
  totals <- out |>
    dplyr::summarise(total = sum(.data$area_ha), .by = "polycell_id")
  expect_equal(
    totals$total[match(c("A-X", "A-Y", "B-X", "C-X"), totals$polycell_id)],
    c(60, 40, 100, 100)
  )
  expect_false("cropland" %in% out$land_use[out$polycell_id == "C-X"])
})

test_that("spillover places more than cap discards", {
  args <- .plu_fx_overfull()
  capped <- whep::build_polycell_land_uses(
    data = args,
    overfull_method = "cap"
  )
  spilled <- suppressWarnings(whep::build_polycell_land_uses(
    data = args,
    overfull_method = "spillover"
  ))
  expect_lte(
    sum(spilled$unplaceable_statistical_ha),
    sum(capped$unplaceable_statistical_ha)
  )
})

# ---- input contracts ---------------------------------------------------------

test_that("a support repeating (polycell_id, year) is refused", {
  support <- dplyr::bind_rows(.plu_fx_support(), .plu_fx_support()[1, ])
  expect_error(
    whep::build_polycell_land_uses(
      data = .plu_fx_data(polycell_support = support)
    ),
    "repeats"
  )
})

test_that("a support missing a required column is named in the abort", {
  support <- dplyr::select(.plu_fx_support(), -"land_area_ha")
  expect_error(
    whep::build_polycell_land_uses(
      data = .plu_fx_data(polycell_support = support)
    ),
    "land_area_ha"
  )
})

test_that("an unplaceable statistical level warns rather than passing", {
  # polity Y has a grassland level but cell A carries grass everywhere, so give
  # Y a level for a class with no pattern at all by removing grass from the cell
  pattern <- .plu_fx_pattern() |>
    dplyr::filter(.data$land_use != "grassland")
  expect_warning(
    whep::build_polycell_land_uses(data = .plu_fx_data(pattern = pattern)),
    "could not be placed"
  )
})

test_that("the toy fixture matches the producer's own schema", {
  expect_identical(
    names(whep::build_polycell_land_uses(example = TRUE)),
    whep:::.plu_output_cols()
  )
})

test_that("the ledger names its grassland source rather than inheriting it", {
  # whep#759: the grassland extension's own default is "luh2", which is a
  # different class definition, not a different source. A refactor must not let
  # the ledger pick that up silently.
  src <- deparse(body(whep:::.plu_grassland_level))
  expect_true(any(grepl('source = "faostat_pasture"', src, fixed = TRUE)))
})

# ---- revision pass: the failure branches S-B7 exists to protect --------------

test_that("an NA land area is refused, not silently treated as zero", {
  # Every balance-critical sum uses na.rm = TRUE, so an NA land_area_ha would
  # propagate to an NA `natural` that the overfull guard cannot see and nothing
  # warns about. S-B7: missing stays missing, and it never becomes a zero.
  support <- .plu_fx_support()
  support$land_area_ha[2] <- NA_real_
  expect_error(
    whep::build_polycell_land_uses(
      data = .plu_fx_data(polycell_support = support)
    ),
    class = "rlang_error"
  )
})

test_that("a duplicated statistical level is refused, not fanned out", {
  # Without an explicit relationship the level join would silently double the
  # anchored area, which reads downstream as a country with twice its cropland.
  crop <- dplyr::bind_rows(.plu_fx_cropland(), .plu_fx_cropland()[1, ])
  expect_error(
    whep::build_polycell_land_uses(
      data = .plu_fx_data(cropland_level = crop)
    ),
    class = "rlang_error"
  )
})

test_that("a zero-land polycell survives as a partition of nothing", {
  support <- dplyr::bind_rows(
    .plu_fx_support(),
    tibble::tibble(
      polycell_id = "D-X",
      lon = 0.25,
      lat = 0.25,
      polity_code = "X-1900-2025",
      area_code = 10L,
      year = 2000L,
      land_area_ha = 0
    )
  )
  out <- whep::build_polycell_land_uses(
    data = .plu_fx_data(polycell_support = support)
  ) |>
    dplyr::filter(.data$polycell_id == "D-X")
  expect_equal(sum(out$area_ha), 0)
  expect_false(any(is.na(out$area_ha)))
})

test_that("spillover consumes non-forested natural land before forest", {
  # SP-2. WHEP's LUH2 reader collapses primf/secdf/primn/secdn into `natural`,
  # so without a split every hectare counts as non-forested and this preference
  # goes inert. Two receivers with equal spare land, one all forest and one all
  # non-forested: the non-forested one must absorb the excess first.
  support <- tibble::tribble(
    ~polycell_id, ~lon, ~lat, ~polity_code, ~area_code, ~year, ~land_area_ha,
    "SRC", 0.25, 0.25, "X-1900-2025", 10L, 2000L, 100,
    "FOR", 0.75, 0.25, "X-1900-2025", 10L, 2000L, 100,
    "OPEN", -0.25, 0.25, "X-1900-2025", 10L, 2000L, 100
  )
  pattern <- tibble::tribble(
    ~lon, ~lat, ~year, ~land_use, ~fraction,
    0.25, 0.25, 2000L, "cropland", 0.90,
    0.25, 0.25, 2000L, "natural", 0.10,
    0.75, 0.25, 2000L, "cropland", 0.10,
    0.75, 0.25, 2000L, "natural", 0.90,
    -0.25, 0.25, 2000L, "cropland", 0.10,
    -0.25, 0.25, 2000L, "natural", 0.90
  )
  # FOR is entirely forest, OPEN entirely non-forested.
  split <- tibble::tribble(
    ~lon, ~lat, ~nonforest_share,
    0.25, 0.25, 1,
    0.75, 0.25, 0,
    -0.25, 0.25, 1
  )
  crop <- tibble::tribble(
    ~area_code, ~year, ~cropland_ha, ~source,
    10L, 2000L, 220, "fao"
  )
  out <- whep::build_polycell_land_uses(
    data = .plu_fx_data(
      polycell_support = support,
      pattern = pattern,
      cropland_level = crop,
      grassland_level = .plu_fx_grassland()[0, ],
      natural_split = split
    )
  )
  got <- out |>
    dplyr::filter(.data$land_use == "cropland") |>
    dplyr::select("polycell_id", "area_ha")
  open_ha <- got$area_ha[got$polycell_id == "OPEN"]
  forest_ha <- got$area_ha[got$polycell_id == "FOR"]
  # both start at 22 ha of pattern cropland; the open cell must end up with more
  expect_gt(open_ha, forest_ha)
  # and the partition still holds everywhere
  expect_equal(
    out |>
      dplyr::summarise(t = sum(.data$area_ha), .by = "polycell_id") |>
      dplyr::pull("t"),
    c(100, 100, 100)
  )
})

test_that("spillover says so when it cannot place, instead of capping", {
  # The methods are alternatives, never fallbacks: spillover that runs out of
  # room reports the remainder rather than quietly behaving like `cap`.
  expect_warning(
    out <- whep::build_polycell_land_uses(
      data = .plu_fx_overfull(),
      overfull_method = "spillover"
    ),
    "could not place"
  )
  expect_gt(sum(out$unplaceable_statistical_ha), 0)
  expect_true(all(out$method_overfull == "spillover"))
})
