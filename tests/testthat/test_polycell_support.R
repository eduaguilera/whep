# Specification tests for build_polycell_support(), written before the
# function exists (plan `plans/2026-08-03-polycell-spatial-support.md`, task
# T-A3). They are the contract T-A4 implements against, so every test in this
# file fails until it lands.
#
# All fixtures are self-contained synthetic geometries: no pins, no network,
# no rasters. That is what the injectable `geometries` / `water` / `ice`
# arguments (plan A1, DA-4, DA-6) exist for.
#
# FIXTURE GEOMETRY. Every polity polygon is inset in latitude inside its
# 0.5-degree cell (45.05-45.45 rather than 45.0-45.5), or else spans the cell
# exactly. That is deliberate. Under s2 the east-west edge of a lon/lat
# rectangle is a great circle, not a parallel, so a polygon whose north or
# south edge sits ON a cell boundary but spans less longitude than the cell is
# clipped by a slightly different curve: measured here, the intersected area
# then falls short of the polygon's own area by 5.2e-5 (0.025-degree wide) to
# 2.7e-4 (0.25-degree wide) relative at latitude 45. Inset polygons reproduce
# their own area to 1e-14, so the tolerances below measure the producer rather
# than the representation of a grid cell. The bias itself is reported to T-A4:
# it is real, it is not what these tests are for, and it biases the S-A9
# reconciliation against LUH2's parallel-bounded `carea`.
#
# TOLERANCES. Measured on sf 1.0.22 / s2 1.1.9, R 4.5.2.
#
#   1e-9 relative  Arithmetic composition inside one polycell: the S-A1
#                  additivity identity, and the water apportionment when it is
#                  checked against the function's own `cell_area_ha`. Double
#                  precision resolves ~1e-16 relative at hectare magnitudes, so
#                  this leaves seven orders of head-room while still being
#                  nearly two million times tighter than the smallest defect in
#                  play (AM-8's latitude-gradient error, 0.19% at latitude 60).
#                  It is also the band EA3 already uses to call an area
#                  partition exact.
#
#   1e-6 relative  Polygon against polygon: a polycell area against the
#                  fixture polygon it comes from, and S-A2 re-aggregation.
#                  Measured: an inset polygon intersected with its cell
#                  reproduces its own area to 1e-14, and partitioning a
#                  multi-cell polygon by the grid and re-summing reproduces it
#                  to 6.6e-16. So 1e-6 cannot fail on numerical grounds, and it
#                  is 1,900x tighter than AM-8's 0.19% latitude-gradient error
#                  at latitude 60, 28,000x tighter than its 2.78%
#                  whole-subcell granularity, and 110,000x tighter than EA2's
#                  11% whole-cell error.
#
#   1e-4 relative  A reported area against a whole-cell area. Measured: s2
#                  (R = 6,371,010 m) and WHEP's own spherical cell formula
#                  (R = 6,371,000 m, `.cell_area_ha_lat()`) agree to <= 9.5e-6
#                  relative over latitudes 0-85, so 1e-4 accepts either
#                  spherical convention. It deliberately does NOT accept a
#                  WGS84-ellipsoid engine: `terra::expanse()` differs from s2
#                  by 0.45% at the equator and 0.86% at latitude 84.75. EA1
#                  blesses WHEP's spherical geometry and EA7 specifies "a true
#                  sf intersection", so an ellipsoid swap would be a silent ~1%
#                  shift in exactly the quantity this PR exists to fix, and it
#                  must fail loudly rather than pass quietly.
#
#   1e-5 absolute  A within-cell area fraction. Measured: for the fixtures
#                  used here the s2 fraction reproduces the closed-form
#                  spherical band fraction to 7.3e-9, and an exact longitude
#                  proportion to 1.6e-7; terra's ellipsoid fraction is within
#                  2.7e-6 of the same value, so the assertion is
#                  engine-agnostic. The signals it must resolve are 6.0e-3 (the
#                  latitude gradient) and 4.0e-2 (the distance to the nearest
#                  whole-subcell share), six hundred and four thousand times
#                  larger.
#
# CONTRACT FIXED HERE, NOT BY THE PLAN. Three details had to be pinned so T-A4
# has an unambiguous target. Each is flagged in the T-A3 report:
#   * `water` is a per-cell tibble (`lon`, `lat`, `water_frac`). The semantics
#     are EA10's measurement of the layer: GLWD's `unit` is `"1"`, a fraction
#     of the WHOLE 0.5-degree cell, so it multiplies `cell_area_ha` and the
#     product is then apportioned across the cell's polycells. Only the column
#     name is fixed here.
#   * the DA-5 LUH2 validation layer arrives as `data$luh2`
#     (`lon`, `lat`, `terrestrial_ha`), reusing the column
#     `inst/scripts/diagnose_polycell_support.R` already computes as
#     `(1 - icwtr) * carea_ha`.
#   * the DA-7 unassigned-land diagnostic is `attr(result, "unassigned")`, a
#     tibble carrying `unassigned_land_ha`. DA-7 says the bucket is "a
#     diagnostic output, not a polity row in the main table", which rules out
#     carrying it as rows; an attribute is the only tidy carrier left given the
#     one-row-per-polycell-year output grain.
#
# NOT ASSERTED, DELIBERATELY. Whether the inland water of a partially covered
# cell is apportioned over the cell's territory or over the whole cell changes
# `inland_water_ha`, and DA-6 does not settle it. The water fixtures below use
# a fully covered cell, where the two rules coincide.

# Helper fixtures --------------------------------------------------------------

# A closed lon/lat rectangle as an `sf` polygon.
pcs_rect <- function(xmin, xmax, ymin, ymax) {
  sf::st_polygon(list(cbind(
    c(xmin, xmax, xmax, xmin, xmin),
    c(ymin, ymin, ymax, ymax, ymin)
  )))
}

# The 0.5-degree cell centred at (lon, lat).
pcs_cell <- function(lon, lat) {
  pcs_rect(lon - 0.25, lon + 0.25, lat - 0.25, lat + 0.25)
}

# A rectangle spanning the given longitudes, inset in latitude inside the cell
# row centred at `lat`. See the FIXTURE GEOMETRY note above.
pcs_inset <- function(xmin, xmax, lat = 45.25) {
  pcs_rect(xmin, xmax, lat - 0.2, lat + 0.2)
}

# A multipolygon assembled from polygons built by pcs_rect().
pcs_multi <- function(...) {
  sf::st_multipolygon(purrr::map(list(...), unclass))
}

# One square islet per (lon, lat), as a single multipolygon. `whep::polities`
# is an sfc_MULTIPOLYGON, so scattered island states look like this.
pcs_islets <- function(lons, lats, half = 0.075) {
  purrr::map2(lons, lats, \(x, y) {
    list(cbind(
      c(x - half, x + half, x + half, x - half, x - half),
      c(y - half, y - half, y + half, y + half, y - half)
    ))
  }) |>
    sf::st_multipolygon()
}

# Geodesic area in hectares, on the same spherical engine EA7 specifies.
pcs_area_ha <- function(geom) {
  as.numeric(sf::st_area(sf::st_sfc(geom, crs = 4326))) / 1e4
}

# Closed-form spherical fraction of a latitude band, independent of sf.
pcs_band_frac <- function(lo, mid, hi) {
  band <- \(a, b) sin(b * pi / 180) - sin(a * pi / 180)
  band(lo, mid) / band(lo, hi)
}

# A `whep::polities`-shaped sf table: same column names, same `geom` geometry
# column, so a fixture is a valid stand-in for `get_polity_geometries()`
# (DA-4). `spec` supplies at least `polity_code`, `start_year`, `end_year`;
# anything else falls back to a live, real, assigned-geometry default.
pcs_polities <- function(spec, geoms) {
  spec <- tibble::as_tibble(spec)
  defaults <- tibble::tibble(
    polity_name = spec$polity_code,
    polity_type = "national",
    iso3_code = stringr::str_sub(spec$polity_code, 1L, 3L),
    wiki_status = "reviewed",
    polygon_status = "assigned",
    has_geometry = TRUE
  )
  sf::st_sf(
    dplyr::bind_cols(spec, defaults[setdiff(names(defaults), names(spec))]),
    geom = sf::st_sfc(geoms, crs = 4326)
  )
}

# Several live polities valid 2000-2020, one polygon each.
pcs_live <- function(codes, geoms) {
  pcs_polities(
    tibble::tibble(
      polity_code = codes,
      start_year = 2000L,
      end_year = 2020L
    ),
    geoms
  )
}

# One live polity holding a stated polygon, the shape most tests need.
pcs_one_polity <- function(geom, code = "AAA-2000-2020") {
  pcs_live(code, list(geom))
}

# The output grain of plan A1, one row per polycell-year.
pcs_output_cols <- function() {
  c(
    "polycell_id",
    "cell_id",
    "lon",
    "lat",
    "polity_code",
    "area_code",
    "year",
    "cell_area_ha",
    "polity_area_ha",
    "land_area_ha",
    "inland_water_ha",
    "ice_area_ha",
    "geometry_source",
    "polygon_status",
    "split_method",
    "coverage_status"
  )
}

pcs_for <- function(result, code) {
  dplyr::filter(result, .data$polity_code == .env$code)
}

pcs_in_cell <- function(result, lon, lat) {
  dplyr::filter(
    result,
    abs(.data$lon - .env$lon) < 1e-9,
    abs(.data$lat - .env$lat) < 1e-9
  )
}

# Output contract --------------------------------------------------------------

testthat::test_that("build_polycell_support returns the polycell grain", {
  testthat::skip_if_not_installed("sf")

  result <- whep::build_polycell_support(
    years = 2015L,
    geometries = pcs_one_polity(pcs_inset(10.05, 10.45))
  )

  testthat::expect_s3_class(result, "tbl_df")
  testthat::expect_equal(
    setdiff(pcs_output_cols(), names(result)),
    character(0)
  )

  result |>
    pointblank::expect_col_vals_not_null(polycell_id) |>
    pointblank::expect_col_vals_not_null(cell_id) |>
    pointblank::expect_col_vals_not_null(geometry_source) |>
    pointblank::expect_col_vals_not_null(split_method) |>
    pointblank::expect_col_vals_not_null(coverage_status) |>
    pointblank::expect_col_vals_equal(year, 2015L) |>
    pointblank::expect_col_vals_gt(polity_area_ha, 0) |>
    pointblank::expect_col_vals_gte(land_area_ha, 0) |>
    pointblank::expect_col_vals_gte(inland_water_ha, 0) |>
    pointblank::expect_col_vals_gte(ice_area_ha, 0)

  # Cell centres sit on the canonical 0.5-degree grid: the grid LUH2 is
  # aggregated to and the crosswalk is keyed on.
  testthat::expect_true(all(abs(((result$lon + 180) %% 0.5) - 0.25) < 1e-9))
  testthat::expect_true(all(abs(((result$lat + 90) %% 0.5) - 0.25) < 1e-9))

  # `polygon_status` is carried through untouched so consumers can filter on
  # polygon quality (plan open-risks table).
  testthat::expect_equal(unique(result$polygon_status), "assigned")
})

testthat::test_that("polygon_status passes through unknown levels and NA", {
  testthat::skip_if_not_installed("sf")

  # This branch's `polities` ships the old vocabulary (assigned, derived,
  # excluded, missing, proxy, unassigned) while `polities-integration` ships
  # assigned, proxy, unassigned, estimate, polygon_vintage_drift plus one NA
  # (DJI-1886-2025). EA7's correction: a `polygon_status %in% c(...)` filter
  # drops that NA row silently. Nothing here may filter on the level set.
  codes <- c("AAA-2000-2020", "BBB-2000-2020", "CCC-2000-2020")
  polities <- pcs_polities(
    tibble::tibble(
      polity_code = codes,
      start_year = 2000L,
      end_year = 2020L,
      polygon_status = c("assigned", "polygon_vintage_drift", NA)
    ),
    list(
      pcs_inset(10.05, 10.45),
      pcs_inset(20.05, 20.45),
      pcs_inset(30.05, 30.45)
    )
  )

  result <- whep::build_polycell_support(years = 2015L, geometries = polities)

  testthat::expect_setequal(result$polity_code, codes)
  testthat::expect_equal(
    result$polygon_status[match(codes, result$polity_code)],
    c("assigned", "polygon_vintage_drift", NA)
  )
})

# S-A4 — the whole-cell base is never used -------------------------------------

testthat::test_that("a coastal polycell carries its land, not the whole cell", {
  testthat::skip_if_not_installed("sf")

  # Cell (10.25, 45.25) measures 217,615.72 ha. The polity holds a coastal
  # sliver 0.025 degrees wide, 5% of the cell width. Today's convention hands
  # it the whole cell (EA2: +11.0% globally, and EA2's table shows 10.4% of
  # land-bearing cells are under half land).
  sliver <- pcs_inset(10.0, 10.025)
  sliver_ha <- pcs_area_ha(sliver)
  cell_ha <- pcs_area_ha(pcs_cell(10.25, 45.25))

  result <- whep::build_polycell_support(
    years = 2015L,
    geometries = pcs_one_polity(sliver)
  )

  testthat::expect_equal(nrow(result), 1L)
  testthat::expect_equal(result$cell_area_ha, cell_ha, tolerance = 1e-4)
  testthat::expect_equal(result$polity_area_ha, sliver_ha, tolerance = 1e-6)
  testthat::expect_equal(result$land_area_ha, sliver_ha, tolerance = 1e-6)

  # The failure mode, stated as the assertion: the answer is not the cell.
  testthat::expect_lt(result$land_area_ha, 0.06 * result$cell_area_ha)
  testthat::expect_equal(
    result$polity_area_ha / result$cell_area_ha,
    sliver_ha / cell_ha,
    tolerance = 1e-4
  )
})

# S-A8 — the four zero-land island states --------------------------------------

testthat::test_that("zero-land island states recover their polygon area", {
  testthat::skip_if_not_installed("sf")

  # EA6's four extreme cases, with their measured polycell counts: French
  # Polynesia 18, Kiribati 9, Micronesia 4, Maldives 1. Each islet is a
  # 0.15-degree square alone in its own 0.5-degree cell, so the whole-cell
  # convention over-counts by ~11x, inside EA9's measured 10x (Maldives) to
  # 34x (Kiribati) band.
  #
  # LUH2 is injected as ZERO terrestrial area in every one of these cells,
  # because that is what EA9 measured for all four states. DA-5 makes LUH2 a
  # validation layer and never a production mask, so the polycells must still
  # carry their polygon area. An implementation that masked land by LUH2 would
  # silently delete four countries.
  islands <- tibble::tribble(
    ~polity_code, ~cells, ~lon0, ~lat0,
    "PYF-1800-2025", 18L, -150.25, -17.75,
    "KIR-1800-2025", 9L, -157.25, 1.25,
    "FSM-1800-2025", 4L, 151.25, 6.75,
    "MDV-1800-2025", 1L, 73.25, 3.25
  )
  lons <- purrr::map2(
    islands$lon0,
    islands$cells,
    \(x, n) x + 0.5 * (seq_len(n) - 1)
  )
  geoms <- purrr::map2(
    lons,
    islands$lat0,
    \(x, y) pcs_islets(x, rep(y, length(x)))
  )

  result <- whep::build_polycell_support(
    years = 2015L,
    geometries = pcs_polities(
      tibble::tibble(
        polity_code = islands$polity_code,
        start_year = 1800L,
        end_year = 2025L
      ),
      geoms
    ),
    data = list(
      luh2 = tibble::tibble(
        lon = unlist(lons),
        lat = rep(islands$lat0, islands$cells),
        terrestrial_ha = 0
      )
    )
  )

  counts <- dplyr::count(result, polity_code)
  testthat::expect_equal(
    counts$n[match(islands$polity_code, counts$polity_code)],
    islands$cells
  )

  purrr::walk2(islands$polity_code, geoms, \(code, geom) {
    rows <- pcs_for(result, code)
    testthat::expect_equal(
      sum(rows$land_area_ha),
      pcs_area_ha(geom),
      tolerance = 1e-6
    )
    # The over-count that today's convention produces, stated as a bound.
    testthat::expect_gt(sum(rows$cell_area_ha) / sum(rows$land_area_ha), 5)
  })

  maldives <- pcs_for(result, "MDV-1800-2025")
  testthat::expect_equal(nrow(maldives), 1L)
  testthat::expect_lt(maldives$land_area_ha, 0.15 * maldives$cell_area_ha)
})

# EA6 — two-, three- and four-polity cells -------------------------------------

testthat::test_that("cells shared by two, three and four polities split", {
  testthat::skip_if_not_installed("sf")

  # EA6 measures 3,764 two-polity cells, 158 three-polity and 3 four-polity in
  # the live crosswalk. One cell of each class here. The two-polity cell is
  # also a coastal border cell: its polities reach only 60% of the way across
  # it, so the renormalisation defect EA3 documents (drop the rest, rescale to
  # 1) would show up as a 1/0.6 inflation.
  parts <- list(
    a1 = pcs_inset(10.0, 10.1),
    a2 = pcs_inset(10.1, 10.3),
    b1 = pcs_inset(20.0, 20.1),
    b2 = pcs_inset(20.1, 20.3),
    b3 = pcs_inset(20.3, 20.5),
    c1 = pcs_rect(30.0, 30.25, 45.05, 45.25),
    c2 = pcs_rect(30.25, 30.5, 45.05, 45.25),
    c3 = pcs_rect(30.0, 30.25, 45.25, 45.45),
    c4 = pcs_rect(30.25, 30.5, 45.25, 45.45)
  )
  codes <- paste0(toupper(names(parts)), "-2000-2020")

  result <- whep::build_polycell_support(
    years = 2015L,
    geometries = pcs_live(codes, unname(parts))
  )

  testthat::expect_equal(nrow(pcs_in_cell(result, 10.25, 45.25)), 2L)
  testthat::expect_equal(nrow(pcs_in_cell(result, 20.25, 45.25)), 3L)
  testthat::expect_equal(nrow(pcs_in_cell(result, 30.25, 45.25)), 4L)

  # Every polity gets exactly its own polygon, not a share of the cell.
  purrr::walk2(codes, parts, \(code, geom) {
    testthat::expect_equal(
      pcs_for(result, code)$polity_area_ha,
      pcs_area_ha(geom),
      tolerance = 1e-6
    )
  })

  # Nothing is renormalised up to the cell: the partly covered cell stays
  # partly covered, and the three-polity cell keeps its own total.
  two <- pcs_in_cell(result, 10.25, 45.25)
  testthat::expect_equal(
    sum(two$polity_area_ha),
    pcs_area_ha(parts$a1) + pcs_area_ha(parts$a2),
    tolerance = 1e-6
  )
  testthat::expect_lt(sum(two$polity_area_ha), 0.6 * two$cell_area_ha[[1L]])

  three <- pcs_in_cell(result, 20.25, 45.25)
  testthat::expect_equal(
    sum(three$polity_area_ha),
    sum(purrr::map_dbl(parts[c("b1", "b2", "b3")], pcs_area_ha)),
    tolerance = 1e-6
  )

  # One physical cell, one `cell_id`; one polycell per polity in it.
  four <- pcs_in_cell(result, 30.25, 45.25)
  testthat::expect_equal(dplyr::n_distinct(four$cell_id), 1L)
  testthat::expect_equal(dplyr::n_distinct(four$polycell_id), 4L)
  testthat::expect_equal(dplyr::n_distinct(result$cell_id), 3L)
  testthat::expect_equal(dplyr::n_distinct(result$polycell_id), nrow(result))
})

# EA3 secondary defect — geodesic splits, not subcell counts -------------------

testthat::test_that("a border split follows the latitude area gradient", {
  testthat::skip_if_not_installed("sf")

  # Cell (10.25, 79.75), Svalbard latitude, cut by the parallel through its
  # centre. The crosswalk divides a cell into 6 x 6 subcells of 1/12 degree
  # (`subcells = 6L`), so a centre split counts 3 rows south and 3 north and
  # returns exactly 0.5/0.5. The true geodesic split is 0.50603/0.49397,
  # because the southern half is farther from the pole. That is what EA3 calls
  # "blind to the sub-degree area gradient"; AM-8 measures the same error
  # independently, finds it does not depend on the subcell count, and brackets
  # the 6.0e-3 asserted here between 0.19% at latitude 60.25 and 0.63% at
  # 80.25. The two polygons tile the cell exactly, sharing its edges, so the
  # split is exact to 7.3e-9.
  south <- pcs_rect(10.0, 10.5, 79.5, 79.75)
  north <- pcs_rect(10.0, 10.5, 79.75, 80.0)
  expected <- pcs_band_frac(79.5, 79.75, 80.0)

  result <- whep::build_polycell_support(
    years = 2015L,
    geometries = pcs_live(
      c("SOU-2000-2020", "NOR-2000-2020"),
      list(south, north)
    )
  )

  testthat::expect_equal(nrow(result), 2L)
  got <- pcs_for(result, "SOU-2000-2020")$polity_area_ha /
    sum(result$polity_area_ha)

  testthat::expect_lt(abs(got - expected), 1e-5)
  # And it is not the subcell-count answer.
  testthat::expect_gt(abs(got - 0.5), 5e-3)
})

testthat::test_that("a border split is not quantised to whole subcells", {
  testthat::skip_if_not_installed("sf")

  # `build_cell_polity_fraction()` weights by raw subcell counts with
  # `subcells = 6L` (`inst/scripts/prepare_spatialize_all.R:949`), so a
  # 0.5-degree cell holds 6 x 6 = 36 subcells of 1/12 degree and a share can
  # only be a multiple of 1/36. EA3's "1/144" assumed 12 x 12 and is superseded
  # by AM-1 and AM-8, which measure the real granularity at 2.78% of a cell: a
  # polity whose true share is 1% receives either 0 or 2.78%. A purely
  # longitudinal border assigns whole subcell columns, so the count answer is a
  # multiple of 1/6.
  #
  # The border sits at 20.02, inside the first subcell column (edges 20.0,
  # 20.0833, ...), so the true west share is 0.04 and the count answer is 0.
  west <- pcs_inset(20.0, 20.02)
  east <- pcs_inset(20.02, 20.5)
  expected <- pcs_area_ha(west) / (pcs_area_ha(west) + pcs_area_ha(east))

  result <- whep::build_polycell_support(
    years = 2015L,
    geometries = pcs_live(
      c("WES-2000-2020", "EAS-2000-2020"),
      list(west, east)
    )
  )

  testthat::expect_equal(nrow(result), 2L)
  got <- pcs_for(result, "WES-2000-2020")$polity_area_ha /
    sum(result$polity_area_ha)

  testthat::expect_lt(abs(got - expected), 1e-5)
  # Not any share a whole-subcell count can produce. The nearest of them is
  # 0.04 away, twice the threshold.
  testthat::expect_gt(
    min(abs(got - c(0, 1 / 12, 1 / 6, 1 / 4, 1 / 3))),
    0.02
  )
})

# S-A7 — the exclusive end_year ------------------------------------------------

testthat::test_that("a border change resolves on an exclusive end_year", {
  testthat::skip_if_not_installed("sf")

  # EA7: `start_year` inclusive, `end_year` EXCLUSIVE. Filtering
  # `year <= end_year` double-counts every boundary year, so 2014 would carry
  # both RUS-1991-2014 and RUS-2014-2025 and their areas would sum to more
  # than the cell. The two epochs hold different territory here, so the choice
  # is visible in the value and not only in the row count.
  early <- pcs_inset(10.0, 10.25)
  late <- pcs_cell(10.25, 45.25)

  result <- whep::build_polycell_support(
    years = c(2013L, 2014L, 2015L),
    geometries = pcs_polities(
      tibble::tribble(
        ~polity_code, ~start_year, ~end_year,
        "RUS-1991-2014", 1991L, 2014L,
        "RUS-2014-2025", 2014L, 2025L
      ),
      list(early, late)
    )
  )

  # Exactly one polity_code per year, and no double count.
  testthat::expect_equal(nrow(result), 3L)
  testthat::expect_equal(
    result |> dplyr::arrange(year) |> dplyr::select(year, polity_code),
    tibble::tribble(
      ~year, ~polity_code,
      2013L, "RUS-1991-2014",
      2014L, "RUS-2014-2025",
      2015L, "RUS-2014-2025"
    )
  )

  testthat::expect_equal(
    result |> dplyr::filter(year == 2014L) |> dplyr::pull(polity_area_ha),
    pcs_area_ha(late),
    tolerance = 1e-6
  )
  testthat::expect_equal(
    result |> dplyr::filter(year == 2013L) |> dplyr::pull(polity_area_ha),
    pcs_area_ha(early),
    tolerance = 1e-6
  )

  # DA-2: the polycell id varies with the epoch for free, inside one cell.
  testthat::expect_equal(dplyr::n_distinct(result$cell_id), 1L)
  testthat::expect_equal(dplyr::n_distinct(result$polycell_id), 2L)
})

# DA-2 correction — never parse the polity code for dates ----------------------

testthat::test_that("year resolution reads the columns, never the code", {
  testthat::skip_if_not_installed("sf")

  # EA9/DA-2, measured on `polities-integration`: `NNG-1949-1963` really has
  # `end_year = 1969` and `TAN-1922-1964` really has `end_year = 1961`, and
  # five codes carry a hyphenated prefix (AZE-SSR-1920-1991, IDN-BLB/JVM/OTH,
  # MMR-LWR). A split-on-hyphen parse gets all three of these wrong, in both
  # directions:
  #   1965  NNG must be PRESENT  (columns 1949-1969; code says it ended 1963)
  #   1962  TAN must be ABSENT   (columns 1922-1961; code says it ran to 1964)
  #   any   AZE-SSR must be present at all (a hyphen split yields "SSR", not a
  #         year, so a parsing implementation drops it or errors)
  polities <- pcs_polities(
    tibble::tribble(
      ~polity_code, ~start_year, ~end_year,
      "NNG-1949-1963", 1949L, 1969L,
      "TAN-1922-1964", 1922L, 1961L,
      "AZE-SSR-1920-1991", 1920L, 1991L
    ),
    list(
      pcs_inset(10.05, 10.45),
      pcs_inset(20.05, 20.45),
      pcs_inset(30.05, 30.45)
    )
  )

  result <- whep::build_polycell_support(
    years = c(1962L, 1965L, 1985L),
    geometries = polities
  )

  present <- \(yr) sort(result$polity_code[result$year == yr])
  testthat::expect_equal(
    present(1962L),
    c("AZE-SSR-1920-1991", "NNG-1949-1963")
  )
  testthat::expect_equal(
    present(1965L),
    c("AZE-SSR-1920-1991", "NNG-1949-1963")
  )
  testthat::expect_equal(present(1985L), "AZE-SSR-1920-1991")
})

# EA7 correction — NA-explicit filtering ---------------------------------------

testthat::test_that("NA wiki_status and NA polity_type rows are kept", {
  testthat::skip_if_not_installed("sf")

  # EA7's live fencepost. `!wiki_status %in% c("retired", "superseded")` KEEPS
  # NA rows; `polity_type != "aggregate"` silently DROPS them, because
  # `dplyr::filter()` drops NA. On this branch the two NA rows are
  # NRH-1911-1953 and REU-1816-1946, which carry NA in BOTH columns and are
  # real territories, not aggregates.
  #
  # The retention asserted here follows DA-7, which excludes only named values
  # (`retired`, `superseded`, `aggregate`): exclusion requires positive
  # evidence, and EA7's correction calls the silent drop a defect. Dropping
  # them instead would move real land into the S-A11 unassigned bucket, so the
  # choice has to be deliberate either way.
  polities <- pcs_polities(
    tibble::tribble(
      ~polity_code, ~start_year, ~end_year, ~wiki_status, ~polity_type,
      "LIV-2000-2020", 2000L, 2020L, "reviewed", "national",
      "NAS-2000-2020", 2000L, 2020L, NA, "national",
      "NAT-2000-2020", 2000L, 2020L, "reviewed", NA,
      "NRH-1911-1953", 2000L, 2020L, NA, NA,
      "RET-2000-2020", 2000L, 2020L, "retired", "national",
      "SUP-2000-2020", 2000L, 2020L, "superseded", "national",
      "AGG-2000-2020", 2000L, 2020L, "reviewed", "aggregate"
    ),
    purrr::map(seq_len(7L), \(i) pcs_inset(10.05 + i, 10.45 + i))
  )

  result <- whep::build_polycell_support(years = 2015L, geometries = polities)

  testthat::expect_setequal(
    result$polity_code,
    c("LIV-2000-2020", "NAS-2000-2020", "NAT-2000-2020", "NRH-1911-1953")
  )
})

testthat::test_that("dead and aggregate rows receive no data and no land", {
  testthat::skip_if_not_installed("sf")

  # DA-7, and the verification-matrix line "`wiki_status` dead rows and
  # `polity_type == 'aggregate'` rows receive no data". All three polygons sit
  # in ONE cell, so an implementation that dropped the excluded rows and then
  # renormalised (EA3's defect) would inflate the live polity from a fifth of
  # the cell to the whole of it.
  live <- pcs_inset(10.0, 10.15)
  dead <- pcs_inset(10.15, 10.3)
  aggregate <- pcs_cell(10.25, 45.25)

  result <- whep::build_polycell_support(
    years = 2015L,
    geometries = pcs_polities(
      tibble::tribble(
        ~polity_code, ~start_year, ~end_year, ~wiki_status, ~polity_type,
        "LIV-2000-2020", 2000L, 2020L, "reviewed", "national",
        "RET-2000-2020", 2000L, 2020L, "retired", "national",
        "ROW-2000-2020", 2000L, 2020L, "reviewed", "aggregate"
      ),
      list(live, dead, aggregate)
    )
  )

  testthat::expect_equal(result$polity_code, "LIV-2000-2020")
  testthat::expect_equal(
    result$polity_area_ha,
    pcs_area_ha(live),
    tolerance = 1e-6
  )
  # Not renormalised up to the cell, and not handed the excluded territory.
  testthat::expect_lt(result$polity_area_ha, 0.3 * result$cell_area_ha)
})

# S-A1 / DA-3 — three separately addressable area categories -------------------

testthat::test_that("polity area decomposes into land, inland water and ice", {
  testthat::skip_if_not_installed("sf")

  # DA-3: `polity_area_ha = land_area_ha + inland_water_ha + ice_area_ha`, with
  # inland water and ice as territory that is NOT a land use. One polity fills
  # cell (10.25, 45.25); ice covers part of it as a polygon (DA-6:
  # ne_10m_glaciated_areas, exact intersection); GLWD contributes 10% of the
  # cell as inland water (DA-6: a 30-arcmin per-cell fraction).
  polity <- pcs_cell(10.25, 45.25)
  ice_poly <- pcs_inset(10.0, 10.1)

  result <- whep::build_polycell_support(
    years = 2015L,
    geometries = pcs_one_polity(polity),
    water = tibble::tibble(lon = 10.25, lat = 45.25, water_frac = 0.1),
    ice = sf::st_sf(geometry = sf::st_sfc(ice_poly, crs = 4326))
  )

  testthat::expect_equal(nrow(result), 1L)

  # S-A1, the identity itself.
  testthat::expect_equal(
    result$land_area_ha + result$inland_water_ha + result$ice_area_ha,
    result$polity_area_ha,
    tolerance = 1e-9
  )

  # Each category is separately addressable and carries the right quantity.
  testthat::expect_equal(
    result$ice_area_ha,
    pcs_area_ha(ice_poly),
    tolerance = 1e-6
  )
  testthat::expect_equal(
    result$inland_water_ha,
    0.1 * result$cell_area_ha,
    tolerance = 1e-9
  )
  testthat::expect_equal(
    result$land_area_ha,
    result$polity_area_ha - result$ice_area_ha - result$inland_water_ha,
    tolerance = 1e-9
  )

  # Ice and water are not land: the land denominator shrinks by both, and by
  # more than either alone.
  testthat::expect_lt(result$land_area_ha, result$polity_area_ha)
  testthat::expect_lt(
    result$land_area_ha,
    result$polity_area_ha - result$ice_area_ha
  )
  testthat::expect_gt(result$ice_area_ha, 0)
  testthat::expect_gt(result$inland_water_ha, 0)
})

testthat::test_that("without water and ice layers both categories are zero", {
  testthat::skip_if_not_installed("sf")

  # The categories must be genuinely additive rather than a relabelling: with
  # neither layer supplied, all the territory is land and the identity still
  # holds exactly.
  result <- whep::build_polycell_support(
    years = 2015L,
    geometries = pcs_one_polity(pcs_cell(10.25, 45.25))
  )

  testthat::expect_equal(result$inland_water_ha, 0)
  testthat::expect_equal(result$ice_area_ha, 0)
  testthat::expect_equal(
    result$land_area_ha,
    result$polity_area_ha,
    tolerance = 1e-9
  )
})

testthat::test_that("a shared cell's inland water is conserved pro rata", {
  testthat::skip_if_not_installed("sf")

  # DA-6's accepted cost: GLWD is a per-cell fraction, so inside a shared cell
  # its water is apportioned across polycells pro rata rather than placed
  # exactly. The two polities tile the cell exactly, so "pro rata over the
  # cell" and "pro rata over the territory" coincide and the test does not
  # silently pick one. What it does pin is conservation, which neither reading
  # relaxes.
  south <- pcs_rect(10.0, 10.5, 45.0, 45.25)
  north <- pcs_rect(10.0, 10.5, 45.25, 45.5)

  result <- whep::build_polycell_support(
    years = 2015L,
    geometries = pcs_live(
      c("SOU-2000-2020", "NOR-2000-2020"),
      list(south, north)
    ),
    water = tibble::tibble(lon = 10.25, lat = 45.25, water_frac = 0.2)
  )

  testthat::expect_equal(nrow(result), 2L)
  testthat::expect_equal(
    sum(result$inland_water_ha),
    0.2 * result$cell_area_ha[[1L]],
    tolerance = 1e-9
  )
  testthat::expect_equal(
    result$inland_water_ha / sum(result$inland_water_ha),
    result$polity_area_ha / sum(result$polity_area_ha),
    tolerance = 1e-9
  )
  testthat::expect_equal(
    result$land_area_ha + result$inland_water_ha + result$ice_area_ha,
    result$polity_area_ha,
    tolerance = 1e-9
  )
})

# S-A11 — unclaimed land emitted, never renormalised ---------------------------

testthat::test_that("unclaimed land is emitted and never renormalised away", {
  testthat::skip_if_not_installed("sf")

  # DA-7 and S-A11. The defect being avoided is
  # `build_cell_polity_fraction()`'s "drop subcells with no polity code, then
  # renormalise so fractions sum to 1", which EA3 measures as holding in
  # 100.00% of cells. EA9 measures the live consequence: 1,294 LUH2 terrestrial
  # cells holding 49.5 Mha are absent from the crosswalk and silently dropped
  # today.
  #
  # Here one live polity covers part of the cell while the DA-5 LUH2 layer says
  # 70% of it is land. The difference must appear in the unassigned diagnostic,
  # and the polity must keep exactly its own polygon.
  claimed <- pcs_inset(10.0, 10.2)
  claimed_ha <- pcs_area_ha(claimed)
  luh2_ha <- 0.7 * pcs_area_ha(pcs_cell(10.25, 45.25))

  result <- whep::build_polycell_support(
    years = 2015L,
    geometries = pcs_one_polity(claimed),
    data = list(
      luh2 = tibble::tibble(
        lon = 10.25,
        lat = 45.25,
        terrestrial_ha = luh2_ha
      )
    )
  )

  # No renormalisation: the polity keeps its polygon, not the LUH2 land.
  testthat::expect_equal(nrow(result), 1L)
  testthat::expect_equal(result$land_area_ha, claimed_ha, tolerance = 1e-6)
  testthat::expect_lt(result$land_area_ha, 0.9 * luh2_ha)

  # And the remainder is emitted rather than absorbed.
  unassigned <- attr(result, "unassigned")
  testthat::expect_s3_class(unassigned, "data.frame")
  testthat::expect_true(rlang::has_name(unassigned, "unassigned_land_ha"))
  testthat::expect_gt(nrow(unassigned), 0L)
  testthat::expect_equal(
    sum(unassigned$unassigned_land_ha),
    luh2_ha - claimed_ha,
    tolerance = 1e-4
  )
})

# S-A2 — re-aggregation without value change -----------------------------------

testthat::test_that("polycell areas re-aggregate to the polity polygon", {
  testthat::skip_if_not_installed("sf")

  # S-A2. The polygon spans 12 cells and is cut by 5 grid lines, so every
  # boundary cell holds a partial polycell. Summing them must reproduce the
  # polygon's own area, 1,756,184.222522 ha. Measured reconstruction error for
  # this construction is 6.6e-16 relative, so the 1e-6 tolerance is a
  # thousand-million times looser than the numerics and still 1,900x tighter
  # than the border error it has to exclude.
  polity <- pcs_rect(10.13, 11.37, 44.19, 45.81)

  result <- whep::build_polycell_support(
    years = c(2010L, 2015L),
    geometries = pcs_one_polity(polity)
  )

  testthat::expect_equal(dplyr::n_distinct(result$cell_id), 12L)

  totals <- result |>
    dplyr::summarise(total_ha = sum(polity_area_ha), .by = "year")
  testthat::expect_equal(
    totals$total_ha,
    rep(pcs_area_ha(polity), 2L),
    tolerance = 1e-6
  )

  # Land re-aggregates too, and no polycell exceeds its own cell.
  testthat::expect_equal(
    sum(result$land_area_ha) / 2,
    pcs_area_ha(polity),
    tolerance = 1e-6
  )
  testthat::expect_true(
    all(result$polity_area_ha <= result$cell_area_ha * (1 + 1e-4))
  )
})

testthat::test_that("polycell ids key on the cell and the polity code", {
  testthat::skip_if_not_installed("sf")

  # DA-2: `polycell_id = f(cell_id, polity_code)`. `cell_id` is a property of
  # the physical cell alone, so it is shared by the polities in it and stable
  # across years; `polycell_id` is unique per (cell, polity) and likewise
  # stable across years.
  result <- whep::build_polycell_support(
    years = c(2010L, 2015L),
    geometries = pcs_live(
      c("WES-2000-2020", "EAS-2000-2020"),
      list(pcs_inset(10.0, 10.25), pcs_inset(10.25, 10.75))
    )
  )

  # Two cells, three polycells (EAS straddles the cell boundary), two years.
  testthat::expect_equal(dplyr::n_distinct(result$cell_id), 2L)
  testthat::expect_equal(dplyr::n_distinct(result$polycell_id), 3L)
  testthat::expect_equal(nrow(result), 6L)

  # One row per polycell-year, no duplicates.
  testthat::expect_equal(
    nrow(dplyr::distinct(result, polycell_id, year)),
    nrow(result)
  )
  # `cell_id` determines the cell and nothing else.
  testthat::expect_equal(nrow(dplyr::distinct(result, cell_id, lon, lat)), 2L)
  # `polycell_id` determines exactly one (cell, polity) pair.
  testthat::expect_equal(
    nrow(dplyr::distinct(result, polycell_id, cell_id, polity_code)),
    3L
  )
  # The shared cell hands its two polities different polycell ids.
  shared <- pcs_in_cell(result, 10.25, 45.25)
  testthat::expect_equal(dplyr::n_distinct(shared$cell_id), 1L)
  testthat::expect_equal(dplyr::n_distinct(shared$polycell_id), 2L)
})

# S-A6 / S-A3 — the pig fixture ------------------------------------------------

testthat::test_that("a shared cell carries no quantity across the border", {
  testthat::skip_if_not_installed("sf")

  # S-A6, the pig case. PIG and NOP share cell (10.25, 45.25) one-quarter to
  # three-quarters; PIG also holds the whole of cell (9.75, 45.25). Their
  # national totals are deliberately incompatible: PIG has a million head, NOP
  # has none.
  #
  # `build_polycell_support()` allocates nothing, so what is tested is the
  # property that makes polycell-keyed allocation possible: each polity's own
  # land in the shared cell is separately addressable. The allocation below is
  # the two-line consumer form, and the contrast is the defect — forming the
  # quantity on the cell and splitting it afterwards by area share hands NOP
  # three-quarters of PIG's animals. Enforcing this across `spatialize.R` is
  # T-A6.
  # The two PIG parts are kept disjoint: a multipolygon whose rings share an
  # edge is invalid under s2.
  shared_pig <- pcs_inset(10.0, 10.125)
  shared_nop <- pcs_inset(10.125, 10.5)
  home_pig <- pcs_inset(9.55, 9.95)

  result <- whep::build_polycell_support(
    years = 2015L,
    geometries = pcs_live(
      c("PIG-2000-2020", "NOP-2000-2020"),
      list(pcs_multi(shared_pig, home_pig), pcs_multi(shared_nop))
    )
  )

  national <- c("PIG-2000-2020" = 1e6, "NOP-2000-2020" = 0)
  allocated <- result |>
    dplyr::mutate(
      head_count = unname(national[.data$polity_code]) *
        .data$land_area_ha /
        sum(.data$land_area_ha),
      .by = "polity_code"
    )

  # Nothing crosses the border: NOP's polycell in the shared cell gets zero.
  shared <- pcs_in_cell(allocated, 10.25, 45.25)
  testthat::expect_equal(nrow(shared), 2L)
  nop <- shared$polity_code == "NOP-2000-2020"
  testthat::expect_equal(shared$head_count[nop], 0)
  testthat::expect_gt(shared$head_count[!nop], 0)

  # Each polity's animals sum to its own national total, unchanged.
  totals <- allocated |>
    dplyr::summarise(head_count = sum(head_count), .by = "polity_code")
  testthat::expect_equal(
    totals$head_count[match(names(national), totals$polity_code)],
    unname(national),
    tolerance = 1e-9
  )

  # The defect, made explicit: a cell-level quantity split afterwards by area
  # share would hand NOP three-quarters of what the cell holds.
  cell_head <- sum(shared$head_count)
  post_hoc_nop <- cell_head *
    shared$polity_area_ha[nop] /
    sum(shared$polity_area_ha)
  testthat::expect_gt(post_hoc_nop, 0.7 * cell_head)
  testthat::expect_equal(shared$head_count[nop], 0)
})
