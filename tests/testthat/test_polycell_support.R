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
# NOT ASSERTED, DELIBERATELY.
#   * Whether the inland water of a partially covered cell is apportioned over
#     the cell's territory or over the whole cell changes `inland_water_ha`,
#     and DA-6 does not settle it. The water fixtures below use a fully covered
#     cell, where the two rules coincide.
#   * The schema check is a subset check, so DA-13's transitional `polity_frac`
#     and DA-12's second footprint may ride alongside. Both are claims about
#     the interim crosswalk geometry, which these polygon fixtures do not
#     exercise; they belong to T-A5's before-and-after measurement.

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

# T-A4 additions ---------------------------------------------------------------
#
# The 18 blocks above are the T-A3 contract and are left untouched. What
# follows covers the decisions taken after that contract was written: the
# interval grain (DA-16), the interim shim (DA-13), the two footprints (DA-12),
# the water clamp (DA-19), unusable polity geometry (DA-15) and the recorded
# LUH2 vintage (DA-9). All fixtures stay self-contained.

testthat::test_that("the default grain is interval-keyed, not per-year", {
  testthat::skip_if_not_installed("sf")

  # DA-16: no area column varies inside a validity interval, so a per-year
  # grain would repeat identical rows ~173 times. The interval grain is the
  # form to store; `expand_polycell_years()` recovers the per-year view.
  geometries <- pcs_polities(
    tibble::tribble(
      ~polity_code, ~start_year, ~end_year,
      "RUS-1991-2014", 1991L, 2014L,
      "RUS-2014-2025", 2014L, 2025L
    ),
    list(pcs_inset(10.0, 10.25), pcs_cell(10.25, 45.25))
  )

  intervals <- whep::build_polycell_support(geometries = geometries)

  testthat::expect_false(rlang::has_name(intervals, "year"))
  testthat::expect_true(all(c("start_year", "end_year") %in% names(intervals)))
  testthat::expect_equal(nrow(intervals), 2L)
  testthat::expect_equal(sort(intervals$start_year), c(1991L, 2014L))

  yearly <- whep::expand_polycell_years(intervals, 2010L:2015L)
  testthat::expect_equal(nrow(yearly), 6L)
  testthat::expect_equal(
    whep::build_polycell_support(years = 2010L:2015L, geometries = geometries),
    yearly,
    ignore_attr = TRUE
  )
  # The exclusive end_year still holds after expansion.
  testthat::expect_equal(
    yearly$polity_code[yearly$year == 2014L],
    "RUS-2014-2025"
  )
})

testthat::test_that("an interval splits where a cell's occupants change", {
  testthat::skip_if_not_installed("sf")

  # A cell's inland water is apportioned across its polycells, so an interval
  # is only constant-area if it is split wherever a co-occupant arrives. WES
  # holds half the cell throughout; EAS arrives in 2010, so WES must emit two
  # intervals with different water even though its own polygon never changes.
  geometries <- pcs_polities(
    tibble::tribble(
      ~polity_code, ~start_year, ~end_year,
      "WES-2000-2020", 2000L, 2020L,
      "EAS-2010-2020", 2010L, 2020L
    ),
    list(
      pcs_rect(10.0, 10.25, 45.0, 45.5),
      pcs_rect(10.25, 10.5, 45.0, 45.5)
    )
  )

  result <- whep::build_polycell_support(
    geometries = geometries,
    water = tibble::tibble(lon = 10.25, lat = 45.25, water_frac = 0.2)
  )

  # Restricted to the cell the fixture is about. Both polygons run to the cell
  # boundary at latitude 45.0, and under s2 the neighbouring cell's own edge is
  # a great circle that bulges past that line, so each polity also holds a
  # ~30 ha sliver in the cell below. That sliver is real -- including it is what
  # makes the polity re-aggregate to its own polygon exactly -- and it is not
  # what this test is about.
  wes <- pcs_for(result, "WES-2000-2020") |>
    pcs_in_cell(10.25, 45.25) |>
    dplyr::arrange(start_year)
  testthat::expect_equal(wes$start_year, c(2000L, 2010L))
  testthat::expect_equal(wes$end_year, c(2010L, 2020L))
  # Alone in the cell it takes all the water; sharing it, half.
  testthat::expect_equal(
    wes$inland_water_ha,
    c(1, 0.5) * 0.2 * wes$cell_area_ha,
    tolerance = 1e-6
  )
  testthat::expect_equal(wes$polity_area_ha[[1L]], wes$polity_area_ha[[2L]])
})

testthat::test_that("the candidate window follows the spherical extent", {
  testthat::skip_if_not_installed("sf")
  testthat::skip_if_not_installed("s2")

  # DA-21. Enumerating candidate cells from the polygon's COORDINATE bounding
  # box omits the cells its neighbours reach into: under s2 a cell edge is a
  # great circle that bulges past the nominal grid line, so a polity whose own
  # border runs along that line holds a sliver in the cell beyond it. Measured
  # on this fixture the omission is 29.80 ha of 108,808.13, a relative
  # -2.739e-04; the real table lost 1.95e-04 of SWA-1884-1912 the same way, in
  # whole pieces (78 enumerated against 82). Unioning the coordinate box with
  # `s2::s2_bounds_rect()` is exact by construction rather than padded.
  polity <- pcs_rect(10.0, 10.25, 45.0, 45.5)
  result <- whep::build_polycell_support(
    years = 2015L,
    geometries = pcs_one_polity(polity)
  )

  testthat::expect_equal(
    sum(result$polity_area_ha),
    pcs_area_ha(polity),
    tolerance = 1e-9
  )
  # The recovered sliver is a real, separate polycell in the cell below.
  testthat::expect_equal(dplyr::n_distinct(result$cell_id), 2L)
  below <- pcs_in_cell(result, 10.25, 44.75)
  testthat::expect_equal(nrow(below), 1L)
  testthat::expect_gt(below$polity_area_ha, 1)
  testthat::expect_lt(below$polity_area_ha, 1e-3 * pcs_area_ha(polity))
})

testthat::test_that("the shim reproduces the crosswalk bit-for-bit", {
  testthat::skip_if_not_installed("sf")

  # DA-13. The crosswalk is a present-day product with no epochs, so its
  # `polity_frac` cannot be recomputed from the geodesic intersection without
  # moving border shares by up to a whole 1/36 subcell. It is carried through
  # instead, and the claim that an unmigrated consumer is unchanged is asserted
  # with `identical()`, not with a tolerance.
  crosswalk <- tibble::tribble(
    ~lon, ~lat, ~area_code, ~polity_frac,
    10.25, 45.25, 11L, 0.7,
    10.25, 45.25, 22L, 0.3,
    99.75, 45.25, 33L, 1
  ) |>
    dplyr::mutate(cell_area_ha = whep:::.cell_area_ha_lat(.data$lat))

  geometries <- pcs_polities(
    tibble::tibble(
      polity_code = c("AAA-2000-2020", "BBB-2000-2020"),
      start_year = 2000L,
      end_year = 2020L,
      area_code = c(11L, 22L)
    ),
    list(pcs_inset(10.0, 10.35), pcs_inset(10.35, 10.5))
  )

  result <- whep::build_polycell_support(
    years = 2015L,
    geometries = geometries,
    data = list(crosswalk = crosswalk)
  )

  shim <- whep::polycell_shim_view(result) |>
    dplyr::arrange(.data$lon, .data$area_code)
  expected <- crosswalk |>
    dplyr::select("lon", "lat", "area_code", "polity_frac", "cell_area_ha") |>
    dplyr::arrange(.data$lon, .data$area_code)

  testthat::expect_identical(shim$polity_frac, expected$polity_frac)
  testthat::expect_identical(shim$cell_area_ha, expected$cell_area_ha)
  testthat::expect_identical(shim$area_code, expected$area_code)
  testthat::expect_equal(shim, expected)

  # The crosswalk cell the intersection never reaches is still carried, and it
  # is flagged rather than silently invented as a polycell.
  orphan <- dplyr::filter(result, .data$lon == 99.75)
  testthat::expect_equal(orphan$coverage_status, "crosswalk_only")
  testthat::expect_true(is.na(orphan$polity_area_ha))
})

testthat::test_that("both footprints are emitted and reconciled", {
  testthat::skip_if_not_installed("sf")

  # DA-12. The deployed crosswalk is the measurement baseline because it is the
  # geometry every published number was computed from; today's producer is a
  # different footprint. Silently picking either would make the migration's
  # movement and the restriction's movement inseparable.
  deployed <- tibble::tribble(
    ~lon, ~lat, ~area_code, ~polity_frac,
    10.25, 45.25, 11L, 1,
    99.75, 45.25, 33L, 1
  )
  producer <- deployed[1L, ]
  geometries <- pcs_polities(
    tibble::tibble(
      polity_code = "AAA-2000-2020",
      start_year = 2000L,
      end_year = 2020L,
      area_code = 11L
    ),
    list(pcs_inset(10.0, 10.5))
  )

  result <- whep::build_polycell_support(
    years = 2015L,
    geometries = geometries,
    data = list(crosswalk = deployed, producer_crosswalk = producer)
  )

  footprints <- attr(result, "footprints")
  testthat::expect_setequal(
    footprints$footprint,
    c("deployed_crosswalk", "producer_crosswalk", "polycell")
  )
  testthat::expect_equal(
    footprints$rows[match("deployed_crosswalk", footprints$footprint)],
    2L
  )
  testthat::expect_equal(
    footprints$rows[match("producer_crosswalk", footprints$footprint)],
    1L
  )

  # The disagreement is a first-class row, not a count in a message.
  diff <- attr(result, "footprint_diff")
  testthat::expect_equal(nrow(diff), 1L)
  testthat::expect_equal(diff$lon, 99.75)
  testthat::expect_true(diff$deployed_crosswalk)
  testthat::expect_false(diff$producer_crosswalk)
  testthat::expect_false(diff$polycell)
})

testthat::test_that("apportioned water is clamped to the polycell territory", {
  testthat::skip_if_not_installed("sf")

  # DA-19. The water layer carries its own land mask, so in a cell where it
  # disagrees with the polity polygons the apportioned water can exceed the
  # territory. It is capped there and the excess emitted, because
  # `land_area_ha` must never go negative and the disagreement must stay
  # visible instead of being absorbed.
  result <- whep::build_polycell_support(
    years = 2015L,
    geometries = pcs_one_polity(pcs_inset(10.0, 10.05)),
    water = tibble::tibble(lon = 10.25, lat = 45.25, water_frac = 0.5)
  )

  testthat::expect_equal(result$inland_water_ha, result$polity_area_ha)
  testthat::expect_equal(result$land_area_ha, 0)
  testthat::expect_gte(result$land_area_ha, 0)

  excess <- attr(result, "water_excess")
  testthat::expect_equal(nrow(excess), 1L)
  testthat::expect_equal(
    excess$water_excess_ha,
    0.5 * result$cell_area_ha - result$polity_area_ha,
    tolerance = 1e-9
  )
})

testthat::test_that("a polity without a usable polygon is reported", {
  testthat::skip_if_not_installed("sf")

  # DA-15: on the shipped table 23 polities have empty geometry and 3 more
  # carry a polygon no repair makes readable. None of them can host a polycell,
  # and a polity that silently contributes zero area is indistinguishable from
  # a polity with no territory.
  geometries <- pcs_polities(
    tibble::tibble(
      polity_code = c("AAA-2000-2020", "NOG-2000-2020"),
      start_year = 2000L,
      end_year = 2020L
    ),
    list(pcs_inset(10.05, 10.45), sf::st_multipolygon())
  )

  testthat::expect_warning(
    result <- whep::build_polycell_support(
      years = 2015L,
      geometries = geometries
    ),
    "receive"
  )

  testthat::expect_equal(result$polity_code, "AAA-2000-2020")
  coverage <- attr(result, "coverage")
  testthat::expect_equal(
    coverage$coverage_status[coverage$polity_code == "NOG-2000-2020"],
    "no_geometry"
  )
  testthat::expect_equal(
    coverage$coverage_status[coverage$polity_code == "AAA-2000-2020"],
    "has_geometry"
  )
})

testthat::test_that("the LUH2 vintage is recorded in an output column", {
  testthat::skip_if_not_installed("sf")

  # DA-9: the vintage is selectable and recorded, so the choice is auditable
  # rather than implicit in an environment variable.
  luh2 <- tibble::tibble(lon = 10.25, lat = 45.25, terrestrial_ha = 1e5)
  attr(luh2, "luh2_vintage") <- "GCB2022"

  result <- whep::build_polycell_support(
    years = 2015L,
    geometries = pcs_one_polity(pcs_inset(10.05, 10.45)),
    data = list(luh2 = luh2)
  )

  testthat::expect_equal(unique(result$luh2_vintage), "GCB2022")
  testthat::expect_true(
    is.na(
      whep::build_polycell_support(
        years = 2015L,
        geometries = pcs_one_polity(pcs_inset(10.05, 10.45))
      )$luh2_vintage
    )
  )
})

testthat::test_that("overlapping polity polygons are emitted, not absorbed", {
  testthat::skip_if_not_installed("sf")

  # Two live polities handed the SAME polygon claim the same ground twice. On
  # the shipped table that is real: GNQ-1968-2025 and STP-1800-2025 each take
  # all of cell (10.25, 1.75) in 2015, and 441 of 67,629 cells are affected.
  # Deciding who owns the ground is a territorial judgement the producer must
  # not make, so the double count is emitted where it lands.
  same <- pcs_cell(10.25, 45.25)
  testthat::expect_warning(
    result <- whep::build_polycell_support(
      years = 2015L,
      geometries = pcs_live(
        c("AAA-2000-2020", "BBB-2000-2020"),
        list(same, same)
      )
    ),
    "more territory than the cell"
  )

  testthat::expect_equal(nrow(result), 2L)
  overlap <- attr(result, "overlap")
  testthat::expect_equal(nrow(overlap), 1L)
  testthat::expect_equal(overlap$polities, 2L)
  testthat::expect_equal(
    overlap$excess_ha,
    result$cell_area_ha[[1L]],
    tolerance = 1e-4
  )
  # Nothing is renormalised: each polity keeps its own polygon area.
  testthat::expect_equal(
    result$polity_area_ha,
    rep(pcs_area_ha(same), 2L),
    tolerance = 1e-6
  )
})

testthat::test_that("a cell fully covered by one polity is not an overlap", {
  testthat::skip_if_not_installed("sf")

  # The whole-cell tolerance has to accept the two spherical conventions in
  # play: `polity_area_ha` comes from s2 (R = 6,371,010 m) with great-circle
  # edges, `cell_area_ha` from the package's parallel-bounded formula
  # (R = 6,371,000 m). They disagree by between +9.5e-6 and -9.3e-6 relative
  # over latitudes 0-85, in both directions, and no fully covered cell may be
  # flagged as an overlap anywhere in that band.
  cells <- purrr::map(
    c(0.25, 45.25, 79.75),
    \(lat) {
      whep::build_polycell_support(
        years = 2015L,
        geometries = pcs_one_polity(pcs_cell(10.25, lat))
      )
    }
  )

  purrr::walk(cells, \(result) {
    testthat::expect_null(attr(result, "overlap"))
    testthat::expect_lt(
      abs(result$polity_area_ha / result$cell_area_ha - 1),
      1e-4
    )
  })
})

testthat::test_that("unassigned land is reported for years with no claim", {
  testthat::skip_if_not_installed("sf")

  # S-A11 again, at the fencepost the interval grain introduces. A cell held
  # 2000-2010 and unclaimed afterwards must still report its land as unassigned
  # in 2015: keying the diagnostic on the claimed intervals alone leaves that
  # year with no row at all, and the unclaimed land silently disappears from
  # the slice. Measured on the shipped polities, that halved the 2015 figure
  # (158 Mha against 315 Mha).
  luh2_ha <- 0.7 * pcs_area_ha(pcs_cell(10.25, 45.25))
  geometries <- pcs_polities(
    tibble::tribble(
      ~polity_code, ~start_year, ~end_year,
      "GON-2000-2010", 2000L, 2010L,
      "STA-2000-2020", 2000L, 2020L
    ),
    list(pcs_inset(10.0, 10.2), pcs_inset(20.0, 20.2))
  )

  result <- whep::build_polycell_support(
    years = 2015L,
    geometries = geometries,
    data = list(
      luh2 = tibble::tibble(
        lon = c(10.25, 20.25),
        lat = 45.25,
        terrestrial_ha = luh2_ha
      )
    )
  )

  # The gone polity holds no polycell in 2015 ...
  testthat::expect_equal(result$polity_code, "STA-2000-2020")

  # ... and all of its cell's land is unassigned that year, not absent.
  unassigned <- attr(result, "unassigned") |>
    dplyr::filter(.data$start_year <= 2015L, 2015L < .data$end_year)
  gone <- dplyr::filter(unassigned, .data$lon == 10.25)
  testthat::expect_equal(nrow(gone), 1L)
  testthat::expect_equal(gone$claimed_land_ha, 0)
  testthat::expect_equal(gone$unassigned_land_ha, luh2_ha, tolerance = 1e-9)

  # The still-live polity's cell reports only its uncovered remainder.
  live <- dplyr::filter(unassigned, .data$lon == 20.25)
  testthat::expect_equal(nrow(live), 1L)
  testthat::expect_lt(live$unassigned_land_ha, gone$unassigned_land_ha)
  testthat::expect_gt(live$claimed_land_ha, 0)

  # Every year of the domain resolves to exactly one row per cell.
  every <- attr(result, "unassigned")
  purrr::walk(c(2000L, 2005L, 2010L, 2019L), \(yr) {
    slice <- dplyr::filter(every, .data$start_year <= yr, yr < .data$end_year)
    testthat::expect_equal(
      nrow(dplyr::distinct(slice, .data$lon, .data$lat)),
      nrow(slice)
    )
  })
})

testthat::test_that("an unreadable clip piece is measured, never dropped", {
  testthat::skip_if_not_installed("sf")
  testthat::skip_if_not_installed("terra")

  # Real-data regression, on shipped package data so it needs no pins and no
  # network. `sf::st_intersection()` emits pieces the spherical engine will not
  # read back, and a planar repair does not always fix them. Discarding those
  # pieces deleted 1,419,140.84 ha over 21 pieces and 5 polities, among them
  # seven Peloponnese and Aegean pieces worth 466,032 ha -- 10.08% of
  # GRC-1830-1913 -- while the polity still reported
  # `coverage_status == "has_geometry"`. They are pieces of cells, not whole
  # cells: their shares run 0.858 down to 1.7e-05. The loss broke S-A2
  # re-aggregation at every pre-1950 year and re-emerged as fake unclaimed land
  # in the S-A11 diagnostic, so it is pinned here.
  greece <- whep::get_polity_geometries("GRC-1830-1913")
  vanished <- c(404252L, 405252L, 407256L, 407257L, 408256L, 408257L, 409255L)

  testthat::expect_warning(
    result <- whep::build_polycell_support(geometries = greece),
    "could not measure"
  )

  # Every cell the drop used to swallow is present and carries real area.
  testthat::expect_true(all(vanished %in% result$cell_id))
  recovered <- dplyr::filter(result, .data$cell_id %in% vanished)
  testthat::expect_equal(nrow(recovered), length(vanished))
  testthat::expect_true(all(recovered$polity_area_ha > 0))
  testthat::expect_equal(unique(recovered$area_engine), "terra")

  # The substitution is addressable, not inferred: the row column and the
  # diagnostic agree, and the rest of the polity stays on the spherical engine.
  testthat::expect_setequal(
    dplyr::filter(result, .data$area_engine == "terra")$cell_id,
    vanished
  )
  terra_measured <- attr(result, "terra_measured")
  testthat::expect_equal(nrow(terra_measured), length(vanished))
  testthat::expect_equal(
    sum(terra_measured$polity_area_ha),
    sum(recovered$polity_area_ha)
  )

  # The recovered area is real land, not a degenerate sliver: an independent
  # `terra::expanse()` of the polity's own polygon puts it near 466,032 ha.
  testthat::expect_gt(sum(recovered$polity_area_ha), 4e5)
  testthat::expect_lt(
    abs(sum(recovered$polity_area_ha) / 466032 - 1),
    0.01
  )

  # And the polity re-aggregates: no piece is missing from the total.
  testthat::expect_equal(
    sum(result$polity_area_ha),
    as.numeric(sf::st_area(sf::st_geometry(greece))) / 1e4,
    tolerance = 1e-3
  )
})

testthat::test_that("inland water never goes negative on a full ice cover", {
  testthat::skip_if_not_installed("sf")

  # T-A3's contract asserts `inland_water_ha >= 0`. Ice and water are two
  # independent intersections, so on a polycell the ice covers completely the
  # headroom `polity_area_ha - ice_area_ha` comes out at -1e-9 rather than 0,
  # and the clamp then produced a negative water area. 56 Greenland rows in the
  # real build did exactly that.
  cell <- pcs_cell(10.25, 45.25)
  result <- whep::build_polycell_support(
    years = 2015L,
    geometries = pcs_one_polity(cell),
    water = tibble::tibble(lon = 10.25, lat = 45.25, water_frac = 0.3),
    ice = sf::st_sf(geometry = sf::st_sfc(cell, crs = 4326))
  )

  testthat::expect_gte(result$inland_water_ha, 0)
  testthat::expect_gte(result$land_area_ha, 0)
  testthat::expect_equal(result$ice_area_ha, result$polity_area_ha)
  testthat::expect_equal(
    result$land_area_ha + result$inland_water_ha + result$ice_area_ha,
    result$polity_area_ha,
    tolerance = 1e-9
  )
  result |>
    pointblank::expect_col_vals_gte(inland_water_ha, 0) |>
    pointblank::expect_col_vals_gte(land_area_ha, 0)
})

testthat::test_that("split_method records the water rule wherever it ran", {
  testthat::skip_if_not_installed("sf")

  # DA-6: the column records which of the two placement rules applied. Water
  # arrives as a whole-cell fraction and is apportioned, so a single-polity
  # cell holding water was placed by the pro-rata rule just as a shared one is;
  # labelling it `polygon_intersection` hid that the cell's water was never
  # exactly located.
  alone <- whep::build_polycell_support(
    years = 2015L,
    geometries = pcs_one_polity(pcs_cell(10.25, 45.25)),
    water = tibble::tibble(lon = 10.25, lat = 45.25, water_frac = 0.1)
  )
  testthat::expect_equal(
    alone$split_method,
    "polygon_intersection+water_pro_rata"
  )

  dry <- whep::build_polycell_support(
    years = 2015L,
    geometries = pcs_one_polity(pcs_cell(10.25, 45.25))
  )
  testthat::expect_equal(dry$split_method, "polygon_intersection")
})

testthat::test_that("an ice layer over an unreadable piece does not abort", {
  testthat::skip_if_not_installed("sf")
  testthat::skip_if_not_installed("terra")

  # The regression this file previously missed. Keeping an s2-invalid piece in
  # the geometry column is what makes its area recoverable, but it also means
  # no s2 predicate may be run across the whole column: `sf::st_intersects()`
  # aborts on the first such piece with "Loop 0 is not valid". With the shipped
  # polities and the real ice layer that killed the production call at EVERY
  # year, because `years` is applied after the clipping and GRC-1830-1913 is
  # always in the table.
  #
  # The two earlier ice tests used synthetic geometry with no invalid pieces,
  # and the terra test passed no `ice` at all, so nothing exercised the pair.
  # This does: Greece from shipped package data, with ice over the Aegean.
  greece <- whep::get_polity_geometries("GRC-1830-1913")
  aegean <- sf::st_sf(
    geometry = sf::st_sfc(pcs_rect(23.0, 25.0, 37.5, 39.0), crs = 4326)
  )

  testthat::expect_warning(
    result <- whep::build_polycell_support(
      geometries = greece,
      ice = aegean
    ),
    "could not measure"
  )

  # It completes, and it completes with the same polycells as without ice.
  testthat::expect_warning(
    bare <- whep::build_polycell_support(geometries = greece),
    "could not measure"
  )
  testthat::expect_setequal(result$cell_id, bare$cell_id)
  testthat::expect_equal(sum(result$polity_area_ha), sum(bare$polity_area_ha))

  # Ice is subtracted on the terra-measured pieces too, not skipped: cells
  # 407256 and 408256 sit under the Aegean rectangle and are both terra rows.
  terra_rows <- dplyr::filter(result, .data$area_engine == "terra")
  testthat::expect_gt(sum(terra_rows$ice_area_ha), 0)
  testthat::expect_true(all(
    terra_rows$ice_area_ha <= terra_rows$polity_area_ha * (1 + 1e-9)
  ))

  # And the identity still holds on every row, whichever engine measured it.
  testthat::expect_equal(
    result$land_area_ha + result$inland_water_ha + result$ice_area_ha,
    result$polity_area_ha,
    tolerance = 1e-9
  )
  testthat::expect_true(all(result$land_area_ha >= 0))
})

testthat::test_that("a degenerate validity interval is reported, not dropped", {
  testthat::skip_if_not_installed("sf")

  # A polity whose interval is empty or NA-bounded matches no year, so the
  # interval algebra drops all of its polycells and the polity disappears
  # whole. That is the unusable-polygon failure mode relocated, and it is
  # latent on the 603-row table but live the moment the geometry source is
  # refreshed to periods that can overlap or invert.
  geometries <- pcs_polities(
    tibble::tribble(
      ~polity_code, ~start_year, ~end_year,
      "LIV-2000-2020", 2000L, 2020L,
      "EMP-2000-2000", 2000L, 2000L,
      "INV-2010-2000", 2010L, 2000L,
      "NAY-2000-NA", 2000L, NA_integer_
    ),
    purrr::map(seq_len(4L), \(i) pcs_inset(10.05 + i, 10.45 + i))
  )

  testthat::expect_warning(
    result <- whep::build_polycell_support(
      years = 2015L,
      geometries = geometries
    ),
    "receive"
  )

  testthat::expect_equal(result$polity_code, "LIV-2000-2020")
  coverage <- attr(result, "coverage")
  broken <- c("EMP-2000-2000", "INV-2010-2000", "NAY-2000-NA")
  testthat::expect_equal(
    coverage$coverage_status[match(broken, coverage$polity_code)],
    rep("invalid_interval", 3L)
  )
  testthat::expect_equal(
    coverage$coverage_status[coverage$polity_code == "LIV-2000-2020"],
    "has_geometry"
  )
})

testthat::test_that("cells the water layer and the polycells miss are named", {
  testthat::skip_if_not_installed("sf")

  # EA10: the water layer carries the CRU land mask and the polycells carry the
  # polity polygons, so their footprints differ. A polycell with no water row
  # is booked as having none, which turns that water into land; a water cell no
  # polycell reaches loses its water entirely. Both are emitted.
  result <- whep::build_polycell_support(
    years = 2015L,
    geometries = pcs_one_polity(pcs_inset(10.05, 10.45)),
    water = tibble::tibble(
      lon = c(20.25, 30.25),
      lat = 45.25,
      water_frac = c(0.5, 0)
    )
  )

  unmatched <- attr(result, "water_unmatched")
  testthat::expect_s3_class(unmatched, "data.frame")
  testthat::expect_setequal(
    unmatched$side,
    c("polycell_without_water_cell", "water_cell_without_polycell")
  )
  # The polycell's own cell has no water row, so it is reported ...
  testthat::expect_equal(
    dplyr::filter(unmatched, .data$side == "polycell_without_water_cell")$lon,
    10.25
  )
  # ... and so is the wet cell no polycell reaches, but not the dry one, which
  # carries nothing to lose.
  testthat::expect_equal(
    dplyr::filter(unmatched, .data$side == "water_cell_without_polycell")$lon,
    20.25
  )
  testthat::expect_equal(result$inland_water_ha, 0)
})

testthat::test_that("both sides of the LUH2 disagreement are emitted", {
  testthat::skip_if_not_installed("sf")

  # DA-5 requires the disagreement emitted, never silently reconciled. Keeping
  # only `pmax(terrestrial - claimed, 0)` reconciles the over-claim away by
  # construction: at 2015 the real table under-claims 315.50 Mha in some cells
  # and over-claims 103.03 Mha in others, and only the first was reported.
  claimed <- pcs_inset(10.0, 10.4)
  result <- whep::build_polycell_support(
    years = 2015L,
    geometries = pcs_one_polity(claimed),
    data = list(
      luh2 = tibble::tibble(
        lon = c(10.25, 20.25),
        lat = 45.25,
        # One cell where LUH2 sees more land than the polity claims, one where
        # it sees far less.
        terrestrial_ha = c(0.95 * pcs_area_ha(pcs_cell(10.25, 45.25)), 0)
      )
    )
  )

  unassigned <- attr(result, "unassigned")
  testthat::expect_true(
    rlang::has_name(unassigned, "over_claimed_land_ha")
  )
  under <- dplyr::filter(unassigned, .data$lon == 10.25)
  testthat::expect_gt(under$unassigned_land_ha, 0)
  testthat::expect_equal(under$over_claimed_land_ha, 0)
  # The cell LUH2 calls sea but the run never claimed contributes nothing to
  # either side, so it must not appear at all.
  testthat::expect_equal(
    nrow(dplyr::filter(unassigned, .data$lon == 20.25)),
    0L
  )
})

testthat::test_that("interval diagnostics carry the interval they describe", {
  testthat::skip_if_not_installed("sf")

  # The roxygen tells a consumer to filter each interval-grain diagnostic to
  # the interval covering the year of interest. That is only possible if the
  # diagnostic carries both bounds.
  same <- pcs_cell(10.25, 45.25)
  testthat::expect_warning(
    result <- whep::build_polycell_support(
      geometries = pcs_live(
        c("AAA-2000-2020", "BBB-2000-2020"),
        list(same, same)
      ),
      water = tibble::tibble(lon = 10.25, lat = 45.25, water_frac = 0.9),
      data = list(
        luh2 = tibble::tibble(lon = 10.25, lat = 45.25, terrestrial_ha = 1e5)
      )
    ),
    "more territory than the cell"
  )

  purrr::walk(c("overlap", "water_excess", "unassigned"), \(nm) {
    diagnostic <- attr(result, nm)
    testthat::expect_true(rlang::has_name(diagnostic, "start_year"))
    testthat::expect_true(rlang::has_name(diagnostic, "end_year"))
  })
})
