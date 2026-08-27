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
#                  WGS84-ellipsoid engine: signed `terra / s2 - 1` is -0.447%
#                  at the equator, crosses zero near 35.32 degrees, and reaches
#                  +0.888% at latitude 84.75. EA1 blesses WHEP's spherical
#                  geometry and EA7 specifies "a true
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
#   * The schema check is a subset check, so DA-12's second footprint may ride
#     alongside. That is a claim about the interim crosswalk geometry, which
#     these polygon fixtures do not exercise; it belongs to T-A5's
#     before-and-after measurement. DA-13's transitional `polity_frac` used to
#     be the other rider; C9 removed it, and its ABSENCE is now asserted
#     positively rather than tolerated by the subset check.

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

# How many of a polity's clipped pieces the spherical engine emits and then
# refuses to read back. It repeats the producer's clip -- the same candidate
# cells, the same intersection, the same repair -- but takes the verdict from
# `.s2_repair()` rather than from the `area_engine` column, so it reports what
# the platform's own geometry stack does and stays true under any mutation of
# how the producer picks an engine. The two blocks that exist for that hazard
# use it as a liveness precondition: s2 validity turns on ULP-level degeneracy,
# so a build that read every piece back would leave them passing while
# exercising nothing. Returns 0 for a polity the live filter drops, so a
# superseded fixture fails the precondition rather than erroring.
pcs_s2_refuses <- function(geometries) {
  polity <- whep:::.pcs_prepare_polities(geometries)
  if (nrow(polity) == 0L) {
    return(0L)
  }
  geom <- sf::st_geometry(polity)
  cells <- whep:::.pcs_cells_sf(whep:::.pcs_candidate_cells(geom))
  cells <- cells[lengths(sf::st_intersects(cells, geom)) > 0L, ]
  sf::st_agr(cells) <- "constant"
  pieces <- sf::st_intersection(cells, geom)
  sum(whep:::.s2_repair(sf::st_geometry(pieces))$status == "invalid")
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

# whep#803 — the aggregate overlap layer ---------------------------------------
#
# An aggregate polity is a reporting bucket's own territory and its polygon
# covers its members'. FAOSTAT keys its pre-2000 data on exactly those buckets:
# 15 Belgium-Luxembourg has data where 255 Belgium and 256 Luxembourg have none,
# and `BLX-1850-1999` is the only polity bucket 15 ever resolves to. Dropping
# aggregates therefore drops the territory of the bucket that HAS the data,
# while admitting one to the partition hands the same ground out twice.
#
# The fixture is that situation exactly: WES and EAS tile one cell between them,
# AGG is their exact union, and everything below is measured on the same cell so
# a double count cannot hide in a total.
#
# AGG carries the shared vertex at 10.25 that WES and EAS meet on, so its north
# and south edges are the SAME two great circles theirs are. Written as a plain
# rectangle it is a different curve -- see the FIXTURE GEOMETRY note at the top
# of this file -- and its area differs from their sum by 2.4e-6 relative, which
# is the bulge and not the producer. Sharing the vertex lets the assertions
# below run at 1e-9 instead of hiding a real defect under a loose tolerance.
pcs_aggregate_fixture <- function() {
  union <- pcs_rect(10.0, 10.5, 45.05, 45.45)
  union[[1L]] <- rbind(
    c(10.0, 45.05),
    c(10.25, 45.05),
    c(10.5, 45.05),
    c(10.5, 45.45),
    c(10.25, 45.45),
    c(10.0, 45.45),
    c(10.0, 45.05)
  )
  pcs_polities(
    tibble::tribble(
      ~polity_code, ~start_year, ~end_year, ~polity_type,
      "WES-2000-2020", 2000L, 2020L, "national",
      "EAS-2000-2020", 2000L, 2020L, "national",
      "AGG-2000-2020", 2000L, 2020L, "aggregate"
    ),
    list(
      pcs_rect(10.0, 10.25, 45.05, 45.45),
      pcs_rect(10.25, 10.5, 45.05, 45.45),
      union
    )
  )
}

testthat::test_that("the aggregate layer is opt-in and marked on the row", {
  testthat::skip_if_not_installed("sf")

  geometries <- pcs_aggregate_fixture()

  excluded <- whep::build_polycell_support(
    years = 2015L,
    geometries = geometries
  )
  included <- whep::build_polycell_support(
    years = 2015L,
    geometries = geometries,
    aggregates = "overlap_layer"
  )

  # The default is unchanged: no aggregate, and the column says so on every row
  # rather than leaving a consumer to infer it from the absence of one.
  testthat::expect_setequal(
    excluded$polity_code,
    c("WES-2000-2020", "EAS-2000-2020")
  )
  testthat::expect_equal(unique(excluded$support_role), "partition")

  testthat::expect_setequal(
    included$polity_code,
    c("WES-2000-2020", "EAS-2000-2020", "AGG-2000-2020")
  )
  testthat::expect_equal(
    included$support_role[included$polity_code == "AGG-2000-2020"],
    "overlap"
  )
  testthat::expect_true(all(
    included$support_role[included$polity_code != "AGG-2000-2020"] ==
      "partition"
  ))
})

testthat::test_that("admitting an aggregate moves no partition value", {
  testthat::skip_if_not_installed("sf")

  # THE DOCSTRING INVARIANT, under the layer: aggregating polycells to a polity
  # changes no absolute value and no quantity crosses a border it does not
  # belong to. Measured against the build that has no layer at all, with water
  # and the LUH2 validation layer both live, so the water apportionment and the
  # unassigned reconciliation are both exercised rather than assumed.
  geometries <- pcs_aggregate_fixture()
  water <- tibble::tibble(lon = 10.25, lat = 45.25, water_frac = 0.2)
  luh2 <- tibble::tibble(
    lon = 10.25,
    lat = 45.25,
    terrestrial_ha = 0.9 * pcs_area_ha(pcs_cell(10.25, 45.25))
  )

  by_polity <- function(support) {
    support |>
      dplyr::filter(.data$support_role == "partition") |>
      dplyr::summarise(
        polity_area_ha = sum(.data$polity_area_ha),
        land_area_ha = sum(.data$land_area_ha),
        inland_water_ha = sum(.data$inland_water_ha),
        ice_area_ha = sum(.data$ice_area_ha),
        .by = c("polity_code", "year")
      ) |>
      dplyr::arrange(.data$polity_code, .data$year)
  }

  excluded <- whep::build_polycell_support(
    years = 2005L:2015L,
    geometries = geometries,
    water = water,
    data = list(luh2 = luh2)
  )
  included <- whep::build_polycell_support(
    years = 2005L:2015L,
    geometries = geometries,
    water = water,
    data = list(luh2 = luh2),
    aggregates = "overlap_layer"
  )

  # Every area, every polity, every year: identical, not merely close.
  testthat::expect_equal(by_polity(included), by_polity(excluded))

  # And the diagnostics that describe the partition are the same table, so
  # admitting the layer cannot make the polygons look like they over-claim.
  testthat::expect_equal(
    attr(included, "unassigned"),
    attr(excluded, "unassigned")
  )
  testthat::expect_null(attr(excluded, "overlap"))
  testthat::expect_null(attr(included, "overlap"))
})

testthat::test_that("an aggregate carries its members' territory, once", {
  testthat::skip_if_not_installed("sf")

  # The layer is only worth having if it answers the question the bucket asks:
  # how much territory is in this cell. AGG is the exact union of WES and EAS,
  # so its polycell must carry their sum -- in territory, in land and in the
  # apportioned water -- and the water is the part that could silently go wrong,
  # because it is the one quantity in this table that depends on which OTHER
  # rows share the cell.
  cell_ha <- pcs_area_ha(pcs_cell(10.25, 45.25))

  result <- whep::build_polycell_support(
    years = 2015L,
    geometries = pcs_aggregate_fixture(),
    water = tibble::tibble(lon = 10.25, lat = 45.25, water_frac = 0.2),
    aggregates = "overlap_layer"
  )
  members <- dplyr::filter(result, .data$support_role == "partition")
  aggregate <- dplyr::filter(result, .data$support_role == "overlap")

  testthat::expect_equal(nrow(aggregate), 1L)
  testthat::expect_equal(
    aggregate$polity_area_ha,
    sum(members$polity_area_ha),
    tolerance = 1e-9
  )
  testthat::expect_equal(
    aggregate$land_area_ha,
    sum(members$land_area_ha),
    tolerance = 1e-9
  )
  testthat::expect_equal(
    aggregate$inland_water_ha,
    sum(members$inland_water_ha),
    tolerance = 1e-9
  )
  # S-A1 holds on the layer too: it is a polycell, not a summary row.
  testthat::expect_equal(
    aggregate$land_area_ha +
      aggregate$inland_water_ha +
      aggregate$ice_area_ha,
    aggregate$polity_area_ha,
    tolerance = 1e-9
  )

  # THE PARTITION IS STILL A PARTITION. The members share the cell exactly
  # once; it is summing ACROSS the roles that double counts, which is why the
  # role rides on the row and why `read_polycell_support()` returns one layer.
  testthat::expect_equal(
    sum(members$polity_area_ha),
    pcs_area_ha(sf::st_geometry(pcs_aggregate_fixture())[[3L]]),
    tolerance = 1e-9
  )
  testthat::expect_lt(sum(members$polity_area_ha), cell_ha)
  testthat::expect_equal(
    sum(result$polity_area_ha),
    2 * sum(members$polity_area_ha),
    tolerance = 1e-9
  )
})

testthat::test_that("the water denominator is the partition, not the layer", {
  testthat::skip_if_not_installed("sf")

  # THE NEAR MISS. A cell's inland water is apportioned pro rata over the
  # territory sharing the cell. Take that denominator over every row present and
  # the overlap layer -- which by construction covers the same ground again --
  # HALVES what each member receives here, while every row still satisfies
  # S-A1, the water still sums to something plausible and no polygon has
  # changed. The two assertions below are what separate the two rules: the
  # partition must still conserve the cell's whole water, and the members must
  # receive exactly what they receive with no layer in the table at all.
  water <- tibble::tibble(lon = 10.25, lat = 45.25, water_frac = 0.2)
  geometries <- pcs_aggregate_fixture()

  result <- whep::build_polycell_support(
    years = 2015L,
    geometries = geometries,
    water = water,
    aggregates = "overlap_layer"
  )
  alone <- whep::build_polycell_support(
    years = 2015L,
    geometries = geometries,
    water = water
  )
  members <- dplyr::filter(result, .data$support_role == "partition")

  testthat::expect_equal(
    sum(members$inland_water_ha),
    0.2 * members$cell_area_ha[[1L]],
    tolerance = 1e-9
  )
  testthat::expect_equal(
    sort(members$inland_water_ha),
    sort(alone$inland_water_ha),
    tolerance = 1e-12
  )
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
# interval grain (DA-16), the removal of the interim shim (DA-13, at C9), the
# two footprints (DA-12), the water clamp (DA-19), unusable polity geometry
# (DA-15) and the recorded LUH2 vintage (DA-9). All fixtures stay
# self-contained.

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

testthat::test_that("a repeated polycell key aborts rather than vanishing", {
  testthat::skip_if_not_installed("sf")

  # `.pcs_split_intervals()` reads the next breakpoint with `dplyr::lead()`
  # inside `(cell_id, polity_code, start_year, end_year)`, which is the next
  # breakpoint only while that key is unique. Two rows sharing it interleave in
  # the sorted frame, so every second row comes back with
  # `end_year == start_year` -- an empty interval that resolves to no year at
  # all and takes its territory with it.
  #
  # Measured on this fixture before the guard existed: the split returned 5
  # rows of which 2 were empty, and AAA resolved to 30.0 of its 100 ha at 2000,
  # 2005, 2010 and 2015 alike -- 70% of the polycell gone, with no error, no
  # warning, and no invariant broken, because each surviving row is itself well
  # formed.
  pieces <- tibble::tibble(
    cell_id = 1L,
    lon = 10.25,
    lat = 45.25,
    cell_area_ha = 1000,
    # AAA arrives as TWO pieces of one polycell, 70 + 30 ha, which is the shape
    # the GEOMETRYCOLLECTION branch of `.pcs_restore_intersection_rows()`
    # emits. BBB is an ordinary co-occupant, and its 2010 arrival is what makes
    # the cell carry a second breakpoint for the two AAA rows to interleave on.
    polity_area_ha = c(70, 30, 60),
    area_engine = "s2",
    polity_code = c("AAA-2000-2020", "AAA-2000-2020", "BBB-2010-2020"),
    start_year = c(2000L, 2000L, 2010L),
    end_year = c(2020L, 2020L, 2020L),
    area_code = c(11L, 11L, 12L),
    polygon_status = "assigned",
    coverage_status = "has_geometry",
    ice_area_ha = 0
  )

  testthat::expect_error(
    whep:::.pcs_split_intervals(pieces),
    class = "whep_pcs_repeated_key"
  )
  # The count and an offending key are named. "Some key is repeated" would send
  # the reader back to a 400,000-piece clip with nothing to look for.
  testthat::expect_error(
    whep:::.pcs_split_intervals(pieces),
    "1 polycell key is repeated"
  )
  testthat::expect_error(whep:::.pcs_split_intervals(pieces), "AAA-2000-2020")

  # It is the KEY that must be unique, not the cell and not the polity. Two
  # epochs of one polity in one cell, and two polities in one cell, are the
  # ordinary case and still split.
  unique_keys <- pieces[-2L, ]
  unique_keys$polity_area_ha[[1L]] <- 100
  split <- whep:::.pcs_split_intervals(unique_keys)
  testthat::expect_equal(nrow(split), 3L)
  testthat::expect_true(all(split$end_year > split$start_year))
  testthat::expect_equal(
    sum(split$polity_area_ha[split$polity_code == "AAA-2000-2020"]),
    200
  )

  # Reachable from the exported entry point with no mock at all: a geometry
  # table carrying one polity interval twice clips to two identical pieces in
  # every cell it touches. Before the guard that call returned an interval
  # table of 12 rows, 6 of them empty.
  geometries <- whep::polycell_example_geometries()
  testthat::expect_error(
    whep::build_polycell_support(geometries = rbind(geometries, geometries)),
    class = "whep_pcs_repeated_key"
  )
})

testthat::test_that("overlapping intervals of one polity in a cell abort", {
  testthat::skip_if_not_installed("sf")

  # whep#758. The repeated-key guard keys on `end_year`, so it sees only the
  # subset of overlapping validity in which the two intervals are IDENTICAL.
  # `[2000, 2015)` against `[2010, 2020)` is an overlap it cannot see, and the
  # split then emits the shared years twice: measured before this guard,
  # `build_polycell_support()` on the example geometry supplied at those two
  # intervals completed with no abort and returned `[2010, 2015)` twice for
  # every one of its six cells, doubling that polity's territory over the
  # shared years.
  pieces <- tibble::tibble(
    cell_id = 1L,
    lon = 10.25,
    lat = 45.25,
    cell_area_ha = 1000,
    polity_area_ha = c(70, 30),
    area_engine = "s2",
    polity_code = "AAA-2000-2020",
    # NOT a shared start year and NOT an identical key, which is what makes
    # this fixture unreachable for the repeated-key guard: a test built on a
    # shared start would also pass a key widened to
    # `(cell_id, polity_code, start_year)` and prove nothing about overlap.
    start_year = c(2000L, 2010L),
    end_year = c(2015L, 2020L),
    area_code = 11L,
    polygon_status = "assigned",
    coverage_status = "has_geometry",
    ice_area_ha = 0
  )

  testthat::expect_error(
    whep:::.pcs_split_intervals(pieces),
    class = "whep_pcs_overlapping_interval"
  )
  # The offending PAIR is named, both intervals of it: "some interval overlaps"
  # sends the reader back to a 400,000-piece clip with nothing to look for.
  testthat::expect_error(
    whep:::.pcs_split_intervals(pieces),
    "1 pair of polycell intervals overlap"
  )
  testthat::expect_error(
    whep:::.pcs_split_intervals(pieces),
    "\\[2000, 2015\\) overlaps \\[2010, 2020\\)"
  )

  # Reachable from the exported entry point with no mock: `geometries` is a
  # user-supplied argument, and two overlapping rows of it clip to two
  # overlapping pieces in every cell the polity touches.
  geometries <- whep::polycell_example_geometries()
  overlapping <- rbind(geometries, geometries)
  overlapping$start_year <- c(2000L, 2010L)
  overlapping$end_year <- c(2015L, 2020L)
  testthat::expect_error(
    suppressWarnings(whep::build_polycell_support(geometries = overlapping)),
    class = "whep_pcs_overlapping_interval"
  )

  # And the shared-start case the issue was filed on, which the repeated-key
  # guard also misses because `end_year` differs.
  shared_start <- rbind(geometries, geometries)
  shared_start$end_year <- c(2010L, 2020L)
  testthat::expect_error(
    suppressWarnings(whep::build_polycell_support(geometries = shared_start)),
    class = "whep_pcs_overlapping_interval"
  )
})

testthat::test_that("touching intervals of one polity in a cell still split", {
  testthat::skip_if_not_installed("sf")

  # The convention the guard has to respect: `end_year` is EXCLUSIVE at a
  # succession, so `[2000, 2010)` followed by `[2010, 2020)` partitions time
  # and is the ordinary shape of two epochs of one polity in one cell. A guard
  # comparing `<=` instead of `<` would reject every such pair, and a key
  # widened to `(cell_id, polity_code, start_year)` would let a genuine overlap
  # through, so this is the false-positive half of the same contract.
  pieces <- tibble::tibble(
    cell_id = 1L,
    lon = 10.25,
    lat = 45.25,
    cell_area_ha = 1000,
    polity_area_ha = c(70, 30),
    area_engine = "s2",
    polity_code = "AAA-2000-2020",
    start_year = c(2000L, 2010L),
    end_year = c(2010L, 2020L),
    area_code = 11L,
    polygon_status = "assigned",
    coverage_status = "has_geometry",
    ice_area_ha = 0
  )

  split <- whep:::.pcs_split_intervals(pieces)
  testthat::expect_equal(nrow(split), 2L)
  testthat::expect_true(all(split$end_year > split$start_year))
  testthat::expect_equal(split$start_year, c(2000L, 2010L))
  testthat::expect_equal(split$end_year, c(2010L, 2020L))
  testthat::expect_equal(sum(split$polity_area_ha), 100)

  # An empty input reaches neither `dplyr::lag()` nor the abort.
  testthat::expect_equal(
    nrow(whep:::.pcs_split_intervals(pieces[0L, ])),
    0L
  )
  # A single interval has no predecessor: `dplyr::lag()` gives NA and `NA < x`
  # is NA, which `dplyr::filter()` drops rather than treating as an overlap.
  testthat::expect_silent(
    whep:::.pcs_abort_interval_overlap(pieces[1L, ])
  )
})

testthat::test_that("the collection fan-out reaches the repeated-key guard", {
  testthat::skip_if_not_installed("sf")

  # The other way in, and the one that matters: `.pcs_restore_intersection_rows`
  # repeats its source row once per polygonal component when a clip returns a
  # GEOMETRYCOLLECTION, which is the macOS/GEOS shape T-A16 added it for, and
  # `.pcs_intersect_by_source()` does the same where `idx` is unusable. That
  # repetition is correct there -- no piece may be dropped -- and it is exactly
  # what puts a repeated key in front of the interval split, so the guard is
  # reached by the producer's own code rather than only by a malformed
  # geometry table.
  source <- sf::st_sf(
    cell_id = 380270L,
    lon = 10.25,
    lat = 45.25,
    cell_area_ha = whep:::.cell_area_ha_lat(45.25),
    geometry = sf::st_sfc(pcs_cell(10.25, 45.25), crs = 4326)
  )
  hit <- sf::st_sfc(
    sf::st_geometrycollection(list(
      pcs_rect(10.05, 10.15, 45.05, 45.15),
      pcs_rect(10.30, 10.40, 45.05, 45.15),
      sf::st_linestring(cbind(c(10.2, 10.25), c(45.2, 45.2)))
    )),
    crs = 4326
  )
  attr(hit, "idx") <- matrix(
    c(1L, 1L),
    ncol = 2L,
    dimnames = list(NULL, c("x", "y"))
  )

  fanned <- whep:::.pcs_restore_intersection_rows(source, hit)
  testthat::expect_equal(nrow(fanned), 2L)
  testthat::expect_equal(fanned$cell_id, c(380270L, 380270L))

  pieces <- fanned |>
    whep:::.pcs_measure_pieces() |>
    whep:::.pcs_label_cells(tibble::tibble(
      polity_code = "AAA-2000-2020",
      start_year = 2000L,
      end_year = 2020L,
      area_code = 11L,
      polygon_status = "assigned",
      coverage_status = "has_geometry"
    )) |>
    whep:::.pcs_add_ice(NULL)

  testthat::expect_error(
    whep:::.pcs_split_intervals(pieces),
    class = "whep_pcs_repeated_key"
  )
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

testthat::test_that("the window widens in longitude as well as latitude", {
  testthat::skip_if_not_installed("sf")
  testthat::skip_if_not_installed("s2")

  # Both halves of `s2::s2_bounds_rect()` are load-bearing, and the latitude
  # half alone looks sufficient on synthetic rectangles. On the shipped table
  # the longitude half matters for exactly one polity: F228-1800-1856, whose
  # spherical longitude bound runs 10.46407 degrees past its coordinate box,
  # against at most 2.8e-14 for every other non-wrapping polity. Discarding the
  # longitude bound therefore passes every fixture and loses that polity's
  # westernmost cells on real data.
  window <- whep:::.pcs_cell_window(
    sf::st_geometry(whep::get_polity_geometries("F228-1800-1856"))
  )
  box <- sf::st_bbox(
    sf::st_geometry(whep::get_polity_geometries("F228-1800-1856"))
  )

  testthat::expect_lt(window[["xmin"]], box[["xmin"]])
  testthat::expect_equal(
    box[["xmin"]] - window[["xmin"]],
    10.46407,
    tolerance = 1e-4
  )
  # The latitude half is unchanged by this polity, so a test that only watched
  # latitude would see nothing here.
  testthat::expect_equal(window[["ymin"]], box[["ymin"]])
})

testthat::test_that("the DA-13 shim is gone and a crosswalk cannot revive it", {
  testthat::skip_if_not_installed("sf")

  # DA-13, flipped at C9. This block used to assert the shim reproduced the
  # crosswalk bit-for-bit; it now asserts the shim is ABSENT under exactly the
  # input that used to produce it -- a crosswalk carrying `polity_frac`, a
  # border cell it splits 0.7/0.3, and a cell (99.75) the intersection never
  # reaches, which is what used to be appended as `crosswalk_only` padding.
  #
  # Asserted on the SAME fixture as the old validator so the two are directly
  # comparable, and with `data$crosswalk` supplied rather than omitted: a
  # removal proved only on the no-crosswalk path would pass while the shim was
  # still live on the path that had it.
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

  # 1. No shim column.
  testthat::expect_false(rlang::has_name(result, "polity_frac"))
  # 2. No padding row: the crosswalk-only cell is NOT carried, and every row is
  #    a real polycell the intersection measured.
  testthat::expect_false(any(result$coverage_status == "crosswalk_only"))
  testthat::expect_equal(nrow(dplyr::filter(result, .data$lon == 99.75)), 0L)
  testthat::expect_false(anyNA(result$polity_code))
  testthat::expect_false(anyNA(result$polycell_id))
  # 3. The padding was the reason an unfiltered sum used to return NA. It no
  #    longer does, which is the property a consumer actually relies on.
  testthat::expect_false(is.na(sum(result$land_area_ha)))
  testthat::expect_gt(sum(result$land_area_ha), 0)
  # 4. `cell_area_ha` is NOT shim and must survive: `build_n_deposition()`
  #    divides the cell mass by it and the carbon support carries it.
  testthat::expect_true(rlang::has_name(result, "cell_area_ha"))
  # 5. The projection function is gone from the namespace, so a consumer that
  #    still calls it fails loudly instead of finding a stale export.
  testthat::expect_false("polycell_shim_view" %in% getNamespaceExports("whep"))
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

  # Every year of the domain resolves to exactly one row per cell, asserted in
  # BOTH directions. Comparing `nrow(distinct)` against `nrow(slice)` catches
  # duplication and can never catch omission, which is how a dead
  # `.pcs_gaps_before()` -- every leading gap zero-length and filtered away --
  # survived: cells claimed only later simply had no row before their first
  # claim, and the counts still matched.
  every <- attr(result, "unassigned")
  cells <- dplyr::distinct(every, .data$lon, .data$lat)
  purrr::walk(2000L:2019L, \(yr) {
    slice <- dplyr::filter(every, .data$start_year <= yr, yr < .data$end_year)
    seen <- dplyr::distinct(slice, .data$lon, .data$lat)
    # No cell twice ...
    testthat::expect_equal(nrow(seen), nrow(slice))
    # ... and no cell missing.
    testthat::expect_equal(nrow(seen), nrow(cells))
  })
})

testthat::test_that("the leading gap before a first claim is emitted", {
  testthat::skip_if_not_installed("sf")

  # `.pcs_gaps_before()` was dead: `transmute()` evaluates sequentially, so
  # `end_year = start_year` read the `start_year` rebound a line earlier, every
  # leading gap came out zero-length, and the `start_year < end_year` filter
  # deleted them all. Re-run over the shipped build the broken version returns
  # 0 gap rows against the fixed one's 7,340, leaving 6,487 of the 32,248 wet
  # GLWD cells that some polycell reaches with no row at the domain start.
  # Here, a cell first claimed in 2010 loses its whole unassigned row at 2005.
  #
  # LAT is claimed only from 2010; EAR spans the whole domain and is what fixes
  # the domain's lower bound at 2000. Before 2010 LAT's cell is unclaimed, so
  # its LUH2 land must be reported as unassigned in that stretch.
  luh2_ha <- 0.6 * pcs_area_ha(pcs_cell(20.25, 45.25))
  result <- whep::build_polycell_support(
    geometries = pcs_polities(
      tibble::tribble(
        ~polity_code, ~start_year, ~end_year,
        "EAR-2000-2020", 2000L, 2020L,
        "LAT-2010-2020", 2010L, 2020L
      ),
      list(pcs_inset(10.05, 10.45), pcs_inset(20.05, 20.45))
    ),
    data = list(
      luh2 = tibble::tibble(
        lon = c(10.25, 20.25),
        lat = 45.25,
        terrestrial_ha = luh2_ha
      )
    )
  )

  late_cell <- attr(result, "unassigned") |>
    dplyr::filter(.data$lon == 20.25) |>
    dplyr::arrange(.data$start_year)

  # Two rows: the leading gap, then the claimed stretch.
  testthat::expect_equal(nrow(late_cell), 2L)
  testthat::expect_equal(late_cell$start_year, c(2000L, 2010L))
  testthat::expect_equal(late_cell$end_year, c(2010L, 2020L))
  # In the leading gap nothing is claimed, so ALL the layer's land is
  # unassigned. Without the fix this row does not exist at all.
  testthat::expect_equal(late_cell$claimed_land_ha[[1L]], 0)
  testthat::expect_equal(
    late_cell$unassigned_land_ha[[1L]],
    luh2_ha,
    tolerance = 1e-9
  )
  # And once claimed, only the remainder is.
  testthat::expect_gt(late_cell$claimed_land_ha[[2L]], 0)
  testthat::expect_lt(
    late_cell$unassigned_land_ha[[2L]],
    late_cell$unassigned_land_ha[[1L]]
  )
})

testthat::test_that("an unreadable clip piece is measured, never dropped", {
  testthat::skip_if_not_installed("sf")
  testthat::skip_if_not_installed("terra")

  # Real-data regression, on shipped package data so it needs no pins and no
  # network. `sf::st_intersection()` can emit pieces the spherical engine will not
  # read back, and a planar repair does not always fix them. The original defect
  # exposed eight Ionian, Peloponnese and Aegean risk pieces worth about 530,418
  # ha -- 8.34% of GRC-1881-1913. On polities 751 / 0e52f1ff the reference runtime
  # reads four through s2 and substitutes terra for four; another platform may
  # choose a different mix. All eight remain the engine-independent loss fixture.
  # They are pieces of cells, not whole cells, and dropping any of them broke S-A2
  # re-aggregation and re-emerged as fake unclaimed land in the S-A11 diagnostic.
  #
  # FIXTURE RE-PINNED from GRC-1830-1913, which the 751-row `whep::polities`
  # refresh marks `superseded`. DA-7's live filter then drops it, the build
  # returns zero rows with no error and no warning, and every assertion below
  # runs against an absent frame (plan AM-25). Its live successor
  # GRC-1881-1913 carries the same `cshapes-europe` feature 350 at a later
  # vintage and reproduces the defect on all seven originally pinned cells,
  # plus 401255 in the Ionian islands the 1886 polygon adds and the 1835 one
  # does not. The liveness precondition is asserted rather than assumed, so a
  # future supersession fails by name instead of cascading through nine
  # downstream expectations.
  #
  # PLATFORM INDEPENDENCE (T-A15). WHICH engine measured a piece is the
  # platform's decision, not WHEP's: s2 validity turns on ULP-level degeneracy,
  # so pieces the Windows and Linux runners refuse are read on macOS ARM64.
  # Pinning the engine failed there on five expectations. This block therefore
  # asserts the property -- no piece is dropped, every piece carries a positive
  # finite area, the recovered area matches an engine-independent reference,
  # and the partition still sums to the polity -- and never which engine ran.
  # The spread behind the tolerances is measured on this polity rather than
  # assumed: over its 59 s2-readable pieces `terra::expanse()` differs from
  # `sf::st_area()` by +2.09e-04 to +9.88e-04 relative, area-weighted
  # +6.85e-04. The global signed range (-0.447% at the equator to +0.888% at
  # latitude 84.75) does not apply here -- the WGS84-over-sphere area ratio
  # crosses 1 near latitude 35.3 and this polity spans 36.25 to 39.75.
  greece <- whep::get_polity_geometries("GRC-1881-1913")
  testthat::expect_equal(nrow(whep:::.pcs_prepare_polities(greece)), 1L)
  # The hazard must still be present, read from the geometry stack rather than
  # from the producer's own report: with no unreadable piece left there is
  # nothing to recover and the block would pass while exercising nothing.
  testthat::expect_gt(pcs_s2_refuses(greece), 0L)
  vanished <- c(
    401255L,
    404252L,
    405252L,
    407256L,
    407257L,
    408256L,
    408257L,
    409255L
  )

  # The substitution is announced. Asserted as a LINK rather than as a bare
  # `expect_warning()`, which would itself pin the engine: the producer must
  # warn exactly when it substituted, so a platform that needs no substitution
  # stays silent and legal while a producer that substitutes silently still
  # fails here.
  warned <- testthat::capture_warnings(
    result <- whep::build_polycell_support(geometries = greece)
  )
  testthat::expect_equal(
    any(stringr::str_detect(warned, "could not measure")),
    any(result$area_engine == "terra")
  )

  # MEASURED, NEVER DROPPED. The census is exact and engine-independent by
  # construction: the pieces come out of the s2 clip, and the only filter after
  # it is the 1e-6 ha area floor, which sits 4.9e+08 times below this polity's
  # smallest piece (490.82 ha). No engine difference of ~1e-03 relative can
  # move a piece across that floor, so the count cannot depend on the engine.
  testthat::expect_equal(nrow(result), 63L)
  testthat::expect_true(all(vanished %in% result$cell_id))
  recovered <- dplyr::filter(result, .data$cell_id %in% vanished)
  testthat::expect_equal(nrow(recovered), length(vanished))
  # Both engines return a finite positive area or the piece was never measured,
  # so this holds for any mix and catches the NA a half-filled branch leaves.
  testthat::expect_true(all(is.finite(result$polity_area_ha)))
  testthat::expect_true(all(result$polity_area_ha > 0))

  # The substitution is addressable, not inferred: whichever pieces went to
  # terra, the diagnostic names exactly those and books exactly their area.
  # That is an internal-consistency property of one output, so it holds for
  # every engine mix, including the empty one.
  terra_rows <- dplyr::filter(result, .data$area_engine == "terra")
  terra_measured <- attr(result, "terra_measured")
  if (nrow(terra_rows) == 0L) {
    testthat::expect_null(terra_measured)
  } else {
    testthat::expect_setequal(terra_measured$cell_id, terra_rows$cell_id)
    testthat::expect_equal(
      sum(terra_measured$polity_area_ha),
      sum(terra_rows$polity_area_ha)
    )
  }

  # The recovered area is real land, not a degenerate sliver, and the reference
  # shares neither engine with the producer: clipping the same eight cells with
  # s2 switched off and measuring the pieces in a Lambert azimuthal equal-area
  # projection gives 530,417.74 ha (re-measured here at 530,418.32). The
  # producer's mixed s2/terra result must stay within the derived envelope.
  #
  # TOLERANCE 2e-03, derived rather than rounded. Only two terms can move this
  # sum between platforms: the measured engine spread on this polity's own
  # pieces, at most 9.88e-04 relative, and the 7.5e-05 planar-clip-against-
  # s2-clip gap between the reference and the producer -- 1.06e-03 together,
  # so 2e-03 carries 1.9x headroom. It stays 5.7x tighter than the smallest
  # defect it must catch, dropping the smallest of the eight pieces
  # (6,110.99 ha, 1.15e-02 of the sum).
  testthat::expect_gt(sum(recovered$polity_area_ha), 5e5)
  testthat::expect_equal(
    sum(recovered$polity_area_ha),
    530417.74,
    tolerance = 2e-3
  )
  # Material rather than rounding: the eight pieces are 8.3% of the polity. A
  # mixed engine flip moves numerator and denominator by at most 9.1e-04
  # relative between them, so the same 2e-03 applies, and dropping the smallest
  # piece still moves the share by 1.15e-02.
  testthat::expect_equal(
    sum(recovered$polity_area_ha) / sum(result$polity_area_ha),
    0.083432,
    tolerance = 2e-3
  )

  # And the polity re-aggregates: no piece is missing from the total. The
  # residual is exactly the terra-measured share times the engine offset, so it
  # runs from 0 with no piece substituted to the area-weighted 6.88e-04 with
  # every piece substituted; 2e-03 covers that whole range with 2.9x headroom.
  testthat::expect_equal(
    sum(result$polity_area_ha),
    as.numeric(sf::st_area(sf::st_geometry(greece))) / 1e4,
    tolerance = 2e-3
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
  # year, because `years` is applied after the clipping and a Greek interval
  # carrying invalid pieces is always in the table.
  #
  # The two earlier ice tests used synthetic geometry with no invalid pieces,
  # and the terra test passed no `ice` at all, so nothing exercised the pair.
  # This does: Greece from shipped package data, with ice over the Aegean.
  #
  # FIXTURE RE-PINNED from GRC-1830-1913 to its live successor GRC-1881-1913
  # for the reason given in "an unreadable clip piece is measured, never
  # dropped": the 751-row table marks the former `superseded`, DA-7's live
  # filter drops it, and the build then returns zero rows. Two assertions in
  # this block passed VACUOUSLY while that was true, comparing one empty frame
  # with another (plan AM-25). The census pins below exist so that can never
  # read as green again.
  #
  # The ice rectangle is chosen to give the ice reader a VALUE to meet, not a
  # sign. It covers cells 407256 and 408256 exactly and entirely, so their ice
  # must equal their whole territory; every other polycell must come back at
  # essentially zero. Asserting only `sum(ice) > 0` admits a helper that
  # returns the whole piece, one that crops too small, and one that halves the
  # ice.
  #
  # PLATFORM INDEPENDENCE (T-A15). Which engine measured a piece is the
  # platform's decision -- s2 validity turns on ULP-level degeneracy -- so
  # three expectations here that pinned `area_engine` failed on macOS ARM64 and
  # are gone. The two engines also disagree about the rectangle's own edges,
  # and that is measured against the shipped Greek polygon rather than assumed:
  # under s2 a lon/lat rectangle's east-west edges are great circles, so this
  # 1-degree-wide rectangle bulges 1.06e-03 degrees poleward of its stated
  # parallels while the 0.5-degree cells the pieces were clipped from bulge
  # only 2.65e-04. That puts
  #   * 271.31 ha of Greek land inside the two covered cells BELOW the
  #     rectangle's southern arc, which an s2 ice clip misses and a planar one
  #     covers (247.42 of it in cell 407256, 1.19e-03 of its territory);
  #   *  92.76 ha above the parallel 38.5 but inside those cells, which a
  #     planar ice clip misses and an s2 one covers;
  #   * 476.27 ha above the parallel 38.5 in the cells NORTH of it, which an s2
  #     ice clip books onto polycells outside the rectangle and a planar one
  #     does not.
  # Every tolerance below is derived from those three figures, so the block
  # accepts either engine on any piece and nothing looser.
  covered <- c(407256L, 408256L)
  greece <- whep::get_polity_geometries("GRC-1881-1913")
  testthat::expect_equal(nrow(whep:::.pcs_prepare_polities(greece)), 1L)
  # The unreadable piece this block is named for must still exist, read from
  # the geometry stack and not from the producer's own report: with none left
  # there is no s2 predicate to abort on and the block would pass vacuously.
  testthat::expect_gt(pcs_s2_refuses(greece), 0L)
  aegean <- sf::st_sf(
    geometry = sf::st_sfc(pcs_rect(23.5, 24.5, 38.0, 38.5), crs = 4326)
  )

  # The substitution is announced, asserted as a link rather than as a bare
  # `expect_warning()`; see "an unreadable clip piece is measured, never
  # dropped" for why that phrasing is the engine-independent one.
  iced <- testthat::capture_warnings(
    result <- whep::build_polycell_support(
      geometries = greece,
      ice = aegean
    )
  )
  testthat::expect_equal(
    any(stringr::str_detect(iced, "could not measure")),
    any(result$area_engine == "terra")
  )

  # It completes, and it completes with the same polycells as without ice.
  plain <- testthat::capture_warnings(
    bare <- whep::build_polycell_support(geometries = greece)
  )
  testthat::expect_equal(
    any(stringr::str_detect(plain, "could not measure")),
    any(bare$area_engine == "terra")
  )
  # NON-VACUITY. `expect_setequal()` holds between two empty vectors and a sum
  # over an empty frame equals a sum over another empty frame, so both of the
  # next two expectations pass when the build returns nothing. Pin the census
  # and a positive total first; then the comparisons have something to compare.
  # The census is engine-independent: the pieces come out of the s2 clip and
  # the only filter after it is the 1e-6 ha area floor, 4.9e+08 times below the
  # smallest piece.
  testthat::expect_equal(nrow(result), 63L)
  testthat::expect_equal(nrow(bare), 63L)
  testthat::expect_gt(sum(bare$polity_area_ha), 6e6)
  testthat::expect_setequal(result$cell_id, bare$cell_id)
  # Both builds ran on one platform, so they made the same engine choice on
  # every piece and this equality is exact whichever choice that was.
  testthat::expect_equal(sum(result$polity_area_ha), sum(bare$polity_area_ha))

  # The two fully covered cells hold ice over their whole territory: a crop
  # that misses part of the cell cannot reach this, and a halved ice area
  # cannot either.
  # TOLERANCE 3e-03. The uncovered remainder is whichever edge strip the engine
  # that ran leaves behind: 3.33e-04 of the larger cell's territory planar-side
  # (69.37 of 208,242 ha, measured 99.982% cover) and 1.19e-03 spherical-side
  # (247.42 ha), so 3e-03 clears the worse of the two by 2.5x. The failures it
  # must exclude are two orders of magnitude larger -- a crop shrunk by 20%
  # loses tens of percent, halved ice loses half.
  under_ice <- dplyr::filter(result, .data$cell_id %in% covered)
  testthat::expect_equal(nrow(under_ice), 2L)
  testthat::expect_equal(
    under_ice$ice_area_ha,
    under_ice$polity_area_ha,
    tolerance = 3e-3
  )
  testthat::expect_true(all(
    under_ice$ice_area_ha / under_ice$polity_area_ha > 0.995
  ))
  testthat::expect_true(all(
    under_ice$land_area_ha < 5e-3 * under_ice$polity_area_ha
  ))

  # Every other polycell is essentially untouched by the rectangle: a helper
  # that ignored the ice geometry and handed back the whole piece would light
  # these up with 6.06e+06 ha. The bound is the 476.27 ha the rectangle's
  # northern arc can reach into the cells above the parallel, so it is scaled
  # off the two covered cells' territory (294,136 ha) -- a quantity the ice
  # reader does not produce -- at 1e-02, clearing the arc by 6.2x and still
  # sitting 2,060x below the whole-piece failure. A zero sum over an empty
  # frame would light nothing up, so both populations are counted before they
  # are summed.
  outside <- dplyr::filter(result, !.data$cell_id %in% covered)
  testthat::expect_equal(nrow(outside), 61L)
  testthat::expect_lt(
    sum(outside$ice_area_ha),
    1e-2 * sum(under_ice$polity_area_ha)
  )
  # The ice actually placed is the polity's own land inside the rectangle,
  # against a reference that shares neither engine with the producer: the same
  # rectangle clipped with `sf_use_s2(FALSE)` and measured in Lambert azimuthal
  # equal-area gives 294,181.62 ha against the producer's 294,078.84,
  # 3.5e-04 relative. TOLERANCE 1e-02: the three edge strips bound the
  # engine disagreement at (271.31 + 92.76 + 476.27) / 294,182 = 2.86e-03 and
  # the area engines add at most 9.88e-04, so 1e-02 clears 3.85e-03 by 2.6x
  # while still failing a helper that hands back the whole piece (21.6x), one
  # that crops 20% short, and one that halves the ice.
  testthat::expect_equal(sum(result$ice_area_ha), 294181.62, tolerance = 1e-2)

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
  #
  # BOTH directions must be exercised with a VALUE, not merely a column that
  # exists. Two cells, each holding the whole polity's polygon for that cell:
  # in the first LUH2 sees 95% of the cell as land against the polity's 80%, an
  # under-claim; in the second it sees 40% against the same 80%, an over-claim.
  cell_ha <- pcs_area_ha(pcs_cell(10.25, 45.25))
  under_cell <- pcs_rect(10.0, 10.4, 45.05, 45.45)
  over_cell <- pcs_rect(20.0, 20.4, 45.05, 45.45)
  result <- whep::build_polycell_support(
    years = 2015L,
    geometries = pcs_live(
      c("UND-2000-2020", "OVR-2000-2020"),
      list(under_cell, over_cell)
    ),
    data = list(
      luh2 = tibble::tibble(
        lon = c(10.25, 20.25),
        lat = 45.25,
        terrestrial_ha = c(0.95 * cell_ha, 0.40 * cell_ha)
      )
    )
  )

  unassigned <- attr(result, "unassigned")
  testthat::expect_true(rlang::has_name(unassigned, "over_claimed_land_ha"))
  claimed_by <- \(lon) {
    dplyr::filter(result, abs(.data$lon - .env$lon) < 1e-9)$land_area_ha
  }

  under <- dplyr::filter(unassigned, .data$lon == 10.25)
  testthat::expect_equal(nrow(under), 1L)
  testthat::expect_equal(
    under$unassigned_land_ha,
    0.95 * cell_ha - claimed_by(10.25),
    tolerance = 1e-9
  )
  testthat::expect_equal(under$over_claimed_land_ha, 0)

  # The value the fix exists to emit, pinned: without it this is 0.
  over <- dplyr::filter(unassigned, .data$lon == 20.25)
  testthat::expect_equal(nrow(over), 1L)
  testthat::expect_equal(
    over$over_claimed_land_ha,
    claimed_by(20.25) - 0.40 * cell_ha,
    tolerance = 1e-9
  )
  testthat::expect_gt(over$over_claimed_land_ha, 0.2 * cell_ha)
  testthat::expect_equal(over$unassigned_land_ha, 0)

  # And the two sides are not the same number by accident.
  testthat::expect_false(
    isTRUE(all.equal(over$over_claimed_land_ha, under$unassigned_land_ha))
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

testthat::test_that("the shipped snapshot has no unresolved long parallel", {
  testthat::skip_if_not_installed("sf")

  # DA-22 (issue #529). CShapes 2.0 carries intermediate vertices along the
  # 49th parallel, but an earlier whep-polities simplification removed that
  # collinear run and manufactured long great-circle segments. Polities 749 /
  # 9320e033 exposed 43 such edges across 30 polities. WHEP PR #662 repaired the
  # source geometries, and the refreshed polities 751 / 0e52f1ff census is
  # exactly zero. The following synthetic block separately proves that the
  # detector still finds a long parallel and rejects short or sloping edges.
  edges <- whep:::.pcs_long_edges(
    whep:::.pcs_prepare_polities(whep::get_polity_geometries())
  )

  testthat::expect_s3_class(edges, "data.frame")
  testthat::expect_named(
    edges,
    c(
      "polity_code",
      "start_year",
      "end_year",
      "lon_from",
      "lon_to",
      "lat",
      "span_deg",
      "bulge_deg"
    )
  )
  testthat::expect_equal(nrow(edges), 0L)

  # The bulge is the great-circle maximum, not a linear interpolation: a
  # segment half as long bulges roughly a quarter as far.
  testthat::expect_equal(
    whep:::.pcs_great_circle_bulge(49, 27.603621 / 2),
    whep:::.pcs_great_circle_bulge(49, 27.603621) / 4,
    tolerance = 0.02
  )
  # A parallel at the equator cannot bulge at all, and the sign is the same
  # either side of it.
  testthat::expect_equal(whep:::.pcs_great_circle_bulge(0, 27.6), 0)
  testthat::expect_equal(
    whep:::.pcs_great_circle_bulge(-49, 27.603621),
    whep:::.pcs_great_circle_bulge(49, 27.603621)
  )
})

testthat::test_that("a short or sloping edge is not flagged", {
  testthat::skip_if_not_installed("sf")

  # The threshold has to separate a border the source drew as one long parallel
  # segment from ordinary densely-vertexed coastline. A cell-sized edge is
  # under the span threshold; a long edge that climbs in latitude is not a
  # parallel and is not what DA-22 is about.
  flat_long <- pcs_rect(10.0, 15.0, 45.0, 45.5)
  short <- pcs_rect(10.0, 10.4, 45.0, 45.4)
  sloping <- sf::st_polygon(list(cbind(
    c(20.0, 25.0, 25.0, 20.0, 20.0),
    c(45.0, 47.0, 47.5, 45.5, 45.0)
  )))

  edges <- whep:::.pcs_long_edges(
    whep:::.pcs_prepare_polities(
      pcs_live(
        c("FLA-2000-2020", "SHO-2000-2020", "SLO-2000-2020"),
        list(flat_long, short, sloping)
      )
    )
  )

  testthat::expect_setequal(unique(edges$polity_code), "FLA-2000-2020")
  # Both parallels of the long rectangle, north and south.
  testthat::expect_equal(nrow(edges), 2L)
  testthat::expect_equal(edges$span_deg, rep(5, 2L))
  testthat::expect_true(all(edges$bulge_deg > 0))
})

testthat::test_that("long edges ride as a diagnostic and change no area", {
  testthat::skip_if_not_installed("sf")

  # The producer must not redraw the border: which curve the border follows is
  # a territorial judgement, not a geometry repair. The diagnostic is emitted
  # and the areas are exactly what they were without it.
  polity <- pcs_rect(10.0, 15.0, 45.0, 45.5)
  testthat::expect_warning(
    flagged <- whep::build_polycell_support(
      years = 2015L,
      geometries = pcs_one_polity(polity)
    ),
    "near-constant latitude"
  )

  long_edges <- attr(flagged, "long_edges")
  testthat::expect_equal(nrow(long_edges), 2L)
  testthat::expect_true(all(
    c("lon_from", "lon_to", "lat", "span_deg", "bulge_deg") %in%
      names(long_edges)
  ))
  # No area moved: the polity still re-aggregates to its own polygon.
  testthat::expect_equal(
    sum(flagged$polity_area_ha),
    pcs_area_ha(polity),
    tolerance = 1e-9
  )

  # A polity with no long edge gets no attribute and no warning.
  quiet <- whep::build_polycell_support(
    years = 2015L,
    geometries = pcs_one_polity(pcs_inset(10.05, 10.45))
  )
  testthat::expect_null(attr(quiet, "long_edges"))
})

testthat::test_that("the window contains the spherical extent on all sides", {
  testthat::skip_if_not_installed("sf")
  testthat::skip_if_not_installed("s2")

  # Both longitude bounds are load-bearing, and only one of them shows up on
  # the shipped table: F228-1800-1856 extends 10.46407 degrees BELOW its
  # coordinate box, while the largest extension ABOVE any coordinate box is
  # 2.842e-14 (MHL-1874-2025 among others). A test that only looked at the
  # values would therefore never notice the upper bound being dropped, so what
  # is asserted here is the containment property itself, exactly: the window
  # must cover the spherical rectangle on every side, however small the margin.
  # Over EVERY polity, not a hand-picked few. Walking three codes leaves the
  # coordinate-box half of the union untested, because on those three the
  # spherical rectangle already contains it -- the same shape of hole that let
  # the longitude bound go uncovered in the first place. BNB-1881-1963's box
  # extends 1.42e-14 below its spherical rect, so only a sweep over all of them
  # exercises both sides.
  polities <- whep::get_polity_geometries()
  geoms <- sf::st_geometry(polities)
  usable <- which(!sf::st_is_empty(geoms))

  failures <- purrr::map_dfr(usable, \(i) {
    geom <- geoms[i]
    rect <- try(s2::s2_bounds_rect(geom), silent = TRUE)
    if (inherits(rect, "try-error")) {
      return(NULL)
    }
    window <- whep:::.pcs_cell_window(geom)
    box <- sf::st_bbox(geom)
    wraps <- rect$lng_lo > rect$lng_hi
    tibble::tibble(
      polity_code = polities$polity_code[i],
      # The spherical rectangle. Longitude is exempt only where the interval
      # wraps, because a wrapped interval is not a min/max pair; latitude is
      # never exempt, and asserting it caught F228's nine intervals and
      # KIR-1800-2025 losing their spherical latitude bound on the wrap path.
      lon_lo = if (wraps) FALSE else window[["xmin"]] > rect$lng_lo,
      lon_hi = if (wraps) FALSE else window[["xmax"]] < rect$lng_hi,
      lat_lo = window[["ymin"]] > rect$lat_lo,
      lat_hi = window[["ymax"]] < rect$lat_hi,
      # And the coordinate box, on every side.
      box_lo = window[["xmin"]] > box[["xmin"]],
      box_hi = window[["xmax"]] < box[["xmax"]],
      box_lat_lo = window[["ymin"]] > box[["ymin"]],
      box_lat_hi = window[["ymax"]] < box[["ymax"]]
    )
  })

  testthat::expect_gt(nrow(failures), 500L)
  purrr::walk(setdiff(names(failures), "polity_code"), \(side) {
    testthat::expect_equal(
      failures$polity_code[failures[[side]]],
      character(0)
    )
  })

  # The one polity where the margin is large, pinned so the direction is not
  # silently reversed.
  f228 <- sf::st_geometry(whep::get_polity_geometries("F228-1800-1856"))
  testthat::expect_equal(
    sf::st_bbox(f228)[["xmin"]] - whep:::.pcs_cell_window(f228)[["xmin"]],
    10.46407,
    tolerance = 1e-4
  )
})

testthat::test_that("water is floored where ice slightly exceeds the piece", {
  testthat::skip_if_not_installed("sf")

  # The real numeric situation, reproduced directly on the helper because no
  # fixture reaches it through the producer: ice and territory come from two
  # INDEPENDENT intersections, so on a fully ice-covered polycell their
  # difference lands at -1e-9 rather than 0. Built from one geometry, as every
  # fixture is, the difference is exactly 0.000e+00 and the floor is never
  # exercised. 56 Greenland rows in the real build were negative before it.
  pieces <- tibble::tibble(
    cell_id = 1L,
    lon = 10.25,
    lat = 45.25,
    cell_area_ha = 200000,
    polity_area_ha = 100000,
    ice_area_ha = 100000 + 1e-9,
    start_year = 2000L,
    end_year = 2020L
  )
  water <- tibble::tibble(lon = 10.25, lat = 45.25, water_frac = 0.4)

  out <- whep:::.pcs_add_water(pieces, water)

  testthat::expect_gte(out$inland_water_ha, 0)
  testthat::expect_equal(out$inland_water_ha, 0)
  # The whole apportioned amount is then excess, not a negative water area.
  testthat::expect_equal(
    out$water_excess_ha,
    0.4 * 200000 * 1,
    tolerance = 1e-6
  )
  # And the land it feeds cannot go negative either.
  testthat::expect_gte(
    max(out$polity_area_ha - out$inland_water_ha - out$ice_area_ha, 0),
    0
  )
})

testthat::test_that("a terra measurement totals every polygonal part", {
  testthat::skip_if_not_installed("sf")
  testthat::skip_if_not_installed("terra")

  # `sf::st_intersection()` can hand back a GEOMETRYCOLLECTION holding several
  # disjoint polygons for one piece. Measuring only the first would return a
  # plausible number that is quietly short, and every current fixture has a
  # single part, so nothing would notice.
  west <- sf::st_polygon(list(cbind(
    c(10.0, 10.2, 10.2, 10.0, 10.0),
    c(45.0, 45.0, 45.2, 45.2, 45.0)
  )))
  east <- sf::st_polygon(list(cbind(
    c(10.3, 10.5, 10.5, 10.3, 10.3),
    c(45.0, 45.0, 45.2, 45.2, 45.0)
  )))
  collection <- sf::st_sfc(
    sf::st_geometrycollection(list(west, east)),
    crs = 4326
  )

  total <- whep:::.pcs_terra_one_ha(collection)
  west_only <- whep:::.pcs_terra_one_ha(sf::st_sfc(west, crs = 4326))
  east_only <- whep:::.pcs_terra_one_ha(sf::st_sfc(east, crs = 4326))

  testthat::expect_equal(total, west_only + east_only, tolerance = 1e-9)
  testthat::expect_gt(total, west_only * 1.5)
  # A line or a point in the collection contributes nothing but must not make
  # the whole measurement fail.
  with_line <- sf::st_sfc(
    sf::st_geometrycollection(list(
      west,
      sf::st_linestring(cbind(c(20, 21), c(45, 45)))
    )),
    crs = 4326
  )
  testthat::expect_equal(
    whep:::.pcs_terra_one_ha(with_line),
    west_only,
    tolerance = 1e-9
  )
})

testthat::test_that("polygonal intersections restore the source row after dropping lines", {
  testthat::skip_if_not_installed("sf")

  # macOS GEOS can return these two geometries from one ice clip. Calling the
  # sf data-frame method tries to put both on its one source row before the
  # zero-length line can be discarded. The sfc method normally records that
  # both results came from source row 1 in `idx`, but some macOS sf/GEOS builds
  # omit `idx` for a one-row x one-row intersection. Either way, the normalizer
  # must retain the polygon and reattach that row exactly once.
  source <- sf::st_sf(
    cell_id = "cell-a",
    piece = 7L,
    geometry = sf::st_sfc(pcs_cell(10.25, 45.25), crs = 4326)
  )
  zero_line <- sf::st_linestring(cbind(
    c(10, 10, 10),
    c(45.05, 45.05, 45.05)
  ))
  polygon <- pcs_rect(10.1, 10.2, 45.1, 45.2)
  hit <- sf::st_sfc(zero_line, polygon, crs = 4326)
  attr(hit, "idx") <- matrix(
    c(1L, 1L, 1L, 1L),
    ncol = 2L,
    dimnames = list(NULL, c("x", "y"))
  )

  restored <- whep:::.pcs_restore_intersection_rows(source, hit)

  testthat::expect_equal(nrow(restored), 1L)
  testthat::expect_equal(restored$cell_id, "cell-a")
  testthat::expect_equal(restored$piece, 7L)
  testthat::expect_equal(
    as.character(sf::st_geometry_type(restored)),
    "POLYGON"
  )
  testthat::expect_equal(
    sf::st_as_text(sf::st_geometry(restored)),
    sf::st_as_text(sf::st_sfc(polygon, crs = 4326))
  )

  unindexed <- hit
  attr(unindexed, "idx") <- NULL
  testthat::expect_equal(
    whep:::.pcs_restore_intersection_rows(source, unindexed),
    restored
  )
  # With more than one possible source row, absence of `idx` is ambiguous and
  # must fail closed rather than attach a plausible but incorrect identity.
  testthat::expect_error(
    whep:::.pcs_restore_intersection_rows(rbind(source, source), unindexed),
    "did not retain its source-row mapping"
  )

  source_b <- sf::st_sf(
    cell_id = "cell-b",
    piece = 8L,
    geometry = sf::st_sfc(pcs_cell(10.75, 45.25), crs = 4326)
  )
  sources <- rbind(source, source_b)
  clip <- sf::st_sfc(pcs_rect(10.1, 10.9, 45.1, 45.2), crs = 4326)
  by_source <- whep:::.pcs_intersect_by_source(sources, clip)
  testthat::expect_equal(by_source$cell_id, c("cell-a", "cell-b"))
  testthat::expect_equal(by_source$piece, c(7L, 8L))
  testthat::expect_true(all(
    sf::st_geometry_type(by_source) %in% c("POLYGON", "MULTIPOLYGON")
  ))

  line_only <- sf::st_sfc(zero_line, crs = 4326)
  attr(line_only, "idx") <- matrix(
    c(1L, 1L),
    ncol = 2L,
    dimnames = list(NULL, c("x", "y"))
  )
  testthat::expect_equal(
    nrow(whep:::.pcs_restore_intersection_rows(source, line_only)),
    0L
  )
})

testthat::test_that("the ice union is repaired before it is used", {
  testthat::skip_if_not_installed("sf")

  # `read_glaciated_areas()` returns features that are individually valid, and
  # `sf::st_union()` of them is NOT: measured on ne_10m_glaciated_areas, all
  # 1,885 kept features pass `st_is_valid()` while their union fails it, so
  # every later `st_intersects()` and `st_intersection()` against that union
  # would abort. The repair inside `.pcs_prepare_ice()` is what prevents it.
  #
  # The fixture is the smallest reproducing subset of the real layer, two
  # Greenland features carrying 22,544 vertices, stored VERBATIM. Simplifying
  # either collapses it, so the vertices are the fixture; xz brings the pair to
  # 106 KB, which is worth paying to keep the deepest defect in this task
  # covered in CI rather than only where the 1.6 MB shapefile happens to sit.
  ice <- readRDS(testthat::test_path("fixtures", "ice_invalid_union.rds"))
  features <- sf::st_geometry(ice)

  testthat::expect_equal(length(features), 2L)
  testthat::expect_true(all(whep:::.s2_valid(features)))
  testthat::expect_false(all(whep:::.s2_valid(sf::st_union(features))))

  prepared <- whep:::.pcs_prepare_ice(ice)
  testthat::expect_true(all(whep:::.s2_valid(prepared)))
  # And the repaired union is usable by the spherical engine, which is the
  # whole point: unrepaired, this call aborts. The planar repair splits it into
  # more than one valid piece, so the area comes back as a vector.
  testthat::expect_true(all(is.finite(as.numeric(sf::st_area(prepared)))))
  testthat::expect_equal(
    sum(as.numeric(sf::st_area(prepared))) / 1e6,
    1010665,
    tolerance = 1e-4
  )

  # End to end: the producer completes against an ice layer whose union is
  # invalid, and subtracts real ice from a polycell over Greenland.
  greenland <- whep::get_polity_geometries("GRL-1800-2025")
  result <- whep::build_polycell_support(
    years = 2015L,
    geometries = greenland[,],
    ice = ice
  )
  testthat::expect_gt(sum(result$ice_area_ha), 0)
  testthat::expect_true(all(result$land_area_ha >= 0))
})

testthat::test_that("the span prefilter is implied by the bulge floor", {
  # Two mutants survive the sweep because they are equivalent, and both are
  # pinned here so that stops being true the moment the constants move.
  #
  # First: `min_span_deg` is a performance prefilter, not a criterion. No span
  # under a degree can reach the 0.01-degree bulge floor at any latitude, so
  # dropping the span test changes no output. If the floor were ever lowered
  # below 0.0011 the two would stop agreeing and this fails.
  # The thresholds are READ FROM THE FUNCTION, not copied. Hard-coding 0.01
  # here would let the floor be lowered below the maximum sub-degree bulge --
  # destroying the very equivalence this block claims -- while the block still
  # passed.
  defaults <- formals(whep:::.pcs_long_edges)
  floor_deg <- eval(defaults$min_bulge_deg)
  span_deg <- eval(defaults$min_span_deg)

  grid <- tidyr::expand_grid(
    lat = c(0, 30, 45, 60, 80, 89, 89.9),
    span = span_deg * c(0.01, 0.1, 0.5, 0.9, 0.999)
  )
  worst <- max(whep:::.pcs_great_circle_bulge(grid$lat, grid$span))

  testthat::expect_lt(worst, floor_deg)
  testthat::expect_equal(worst, 0.001088664, tolerance = 1e-6)
  # And the span a real flag needs is several times the prefilter, not one.
  testthat::expect_gt(
    stats::uniroot(
      \(s) whep:::.pcs_great_circle_bulge(45, s) - floor_deg,
      c(0.1, 20)
    )$root,
    3 * span_deg
  )
})

testthat::test_that("terra measures a collection the same either way", {
  testthat::skip_if_not_installed("sf")
  testthat::skip_if_not_installed("terra")

  # Second equivalent mutant: `.pcs_polygonal_part()` does not change the AREA
  # terra reports, because terra extracts the polygonal parts itself. The
  # helper is kept for the explicit empty return, and the equality is pinned so
  # a future terra that stopped extracting would be caught here rather than in
  # a silent under-measurement.
  west <- sf::st_polygon(list(cbind(
    c(10.0, 10.2, 10.2, 10.0, 10.0),
    c(45.0, 45.0, 45.2, 45.2, 45.0)
  )))
  east <- sf::st_polygon(list(cbind(
    c(10.3, 10.5, 10.5, 10.3, 10.3),
    c(45.0, 45.0, 45.2, 45.2, 45.0)
  )))
  collection <- sf::st_sfc(
    sf::st_geometrycollection(list(west, east)),
    crs = 4326
  )
  raw <- suppressWarnings(terra::vect(sf::st_sf(geometry = collection)))

  testthat::expect_equal(
    whep:::.pcs_terra_one_ha(collection),
    sum(terra::expanse(raw, unit = "m")) / 1e4,
    tolerance = 1e-9
  )
  # A geometry with no polygonal part measures zero, which is the case the
  # helper's early return exists for.
  line <- sf::st_sfc(
    sf::st_linestring(cbind(c(20, 21), c(45, 45))),
    crs = 4326
  )
  testthat::expect_equal(whep:::.pcs_terra_one_ha(line), 0)
})

# DA-24 — the open interval covers its terminal year ---------------------------
#
# `end_year` is EXCLUSIVE at a succession and INCLUSIVE at the open end. The
# producer had kept the uniformly exclusive read at three sites, so
# `expand_polycell_years(..., <domain end>)` returned nothing, the shim (then
# still live) attached no `polity_frac` and the footprint reconciliation came
# back empty.
#
# These blocks assert the PROPERTY over every year of a fixture built to carry
# every shape the rule has to survive, not a handful of named years. The
# fixture is deliberately awkward:
#
#   ALPHA  one interval, open, spanning TWO cells
#   BETA   a plain succession inside one cell (1900-1990, 1990-2025)
#   GAMMA  TWO intervals of one family both ending at the domain end, in one
#          cell -- the AGO-1816-2025 / AGO-1975-2025 shape that makes a bare
#          "end_year is the maximum" test open both
#   DELTA  open in cell 1, with a LATER-starting interval of the same family in
#          cell 3 -- which must not close cell 1's
#   EPSIL  dissolves long before the domain end
#   ZETA   open with `area_code` NA, as 31 of the 220 live at-end polities are
#   SDN2 / SSD2  two DIFFERENT families sharing one `area_code`, as
#          SDN-2011-2025 and SSD-2011-2025 do
#   ETA    open, with a sibling that starts LATER but ends EARLIER, which
#          succeeds nothing at the domain end

# One of four non-overlapping vertical strips inside the cell centred on `lon`.
pcs_strip <- function(lon, k) {
  x0 <- lon - 0.25 + 0.01 + (k - 1L) * 0.12
  pcs_rect(x0, x0 + 0.10, 45.05, 45.45)
}

pcs_da24_spec <- function() {
  tibble::tribble(
    ~polity_code, ~start_year, ~end_year, ~area_code, ~cell, ~strip,
    "ALPHA-1900-2025", 1900L, 2025L, 101L, 0L, 1L,
    "BETA-1900-1990", 1900L, 1990L, 102L, 2L, 2L,
    "BETA-1990-2025", 1990L, 2025L, 102L, 2L, 2L,
    "GAMMA-1900-2025", 1900L, 2025L, 103L, 3L, 1L,
    "GAMMA-1975-2025", 1975L, 2025L, 103L, 3L, 1L,
    "DELTA-1900-2025", 1900L, 2025L, 104L, 1L, 2L,
    "DELTA-1990-2025", 1990L, 2025L, 104L, 3L, 2L,
    "EPSIL-1900-1950", 1900L, 1950L, 105L, 3L, 3L,
    "ZETA-1950-2025", 1950L, 2025L, NA_integer_, 1L, 3L,
    "SDN2-2011-2025", 2011L, 2025L, 206L, 2L, 3L,
    "SSD2-2011-2025", 2011L, 2025L, 206L, 3L, 4L,
    "ETA-1900-2025", 1900L, 2025L, 107L, 1L, 4L,
    "ETA-1950-1960", 1950L, 1960L, 107L, 1L, 4L
  )
}

pcs_da24_geometries <- function(spec = pcs_da24_spec()) {
  lons <- c(10.25, 10.75, 11.25)
  geoms <- purrr::map2(spec$cell, spec$strip, \(cell, strip) {
    if (cell == 0L) {
      # ALPHA spans two cells, so its open end has to hold in both.
      pcs_multi(pcs_strip(lons[[1L]], strip), pcs_strip(lons[[2L]], strip))
    } else {
      pcs_strip(lons[[cell]], strip)
    }
  })
  pcs_polities(dplyr::select(spec, -"cell", -"strip"), geoms)
}

testthat::test_that("every polycell resolves at the open end, once", {
  testthat::skip_if_not_installed("sf")

  spec <- pcs_da24_spec()
  intervals <- whep::build_polycell_support(
    geometries = pcs_da24_geometries(spec)
  )
  domain_end <- max(intervals$end_year)
  testthat::expect_equal(domain_end, 2025L)

  years <- seq(min(intervals$start_year), domain_end + 1L)
  yearly <- whep::expand_polycell_years(intervals, years)

  # Nothing beyond the domain, and the open end is NOT empty -- without this
  # the uniqueness assertion below would be vacuous exactly where it matters.
  testthat::expect_equal(sum(yearly$year > domain_end), 0L)
  open_end <- dplyr::filter(yearly, .data$year == domain_end)
  testthat::expect_gt(nrow(open_end), 0L)

  # No year resolves one polycell twice, at the open end or anywhere else.
  per_year <- yearly |>
    dplyr::summarise(
      n = dplyr::n(),
      distinct = dplyr::n_distinct(.data$polycell_id),
      .by = "year"
    )
  testthat::expect_equal(per_year$n, per_year$distinct)

  # Every polycell of the table is reachable in at least one year: a wrong
  # grouping loses whole polycells rather than duplicating them.
  testthat::expect_setequal(
    unique(yearly$polycell_id),
    unique(intervals$polycell_id)
  )

  # Each polity is present exactly on the years its own columns cover, with the
  # domain end added for the ones nothing succeeds.
  observed <- yearly |>
    dplyr::summarise(
      first = min(.data$year),
      last = max(.data$year),
      .by = "polity_code"
    ) |>
    dplyr::arrange(.data$polity_code)
  expected <- spec |>
    dplyr::summarise(
      first = min(.data$start_year),
      last = max(dplyr::if_else(
        .data$end_year == domain_end,
        .data$end_year,
        .data$end_year - 1L
      )),
      .by = "polity_code"
    ) |>
    dplyr::arrange(.data$polity_code)
  testthat::expect_equal(observed, expected)
})

testthat::test_that("the open end resolves to the year before it, by value", {
  testthat::skip_if_not_installed("sf")

  # S-A10: measured on values, not on row counts. Nothing succeeds anything
  # between 2024 and 2025 in this fixture, so the two years must carry the same
  # polycells AND the same area, per cell and per polity family.
  intervals <- whep::build_polycell_support(geometries = pcs_da24_geometries())
  domain_end <- max(intervals$end_year)
  pair <- whep::expand_polycell_years(intervals, c(domain_end - 1L, domain_end))

  key <- \(yr) {
    pair |>
      dplyr::filter(.data$year == yr) |>
      dplyr::arrange(.data$polycell_id) |>
      dplyr::select("polycell_id", "cell_id", "polity_code", "polity_area_ha")
  }
  testthat::expect_equal(key(domain_end), key(domain_end - 1L))

  # And the same multiplicity per cell and family: the open-end rule must not
  # add a row where the year before had one, nor drop one where it had two.
  multiplicity <- \(yr) {
    pair |>
      dplyr::filter(.data$year == yr) |>
      dplyr::mutate(family = whep:::.polity_family(.data$polity_code)) |>
      dplyr::count(.data$cell_id, .data$family) |>
      dplyr::arrange(.data$cell_id, .data$family)
  }
  testthat::expect_equal(
    multiplicity(domain_end),
    multiplicity(domain_end - 1L)
  )
  # GAMMA's two co-located intervals, present in both years alike.
  testthat::expect_equal(max(multiplicity(domain_end)$n), 2L)
})

testthat::test_that("an interval with a successor never covers its own end", {
  testthat::skip_if_not_installed("sf")

  intervals <- whep::build_polycell_support(geometries = pcs_da24_geometries())
  domain_end <- max(intervals$end_year)
  years <- seq(min(intervals$start_year), domain_end)
  yearly <- whep::expand_polycell_years(intervals, years)

  # Every interval that ends BEFORE the domain end is succeeded -- by the next
  # epoch, or by the next slice `.pcs_split_intervals()` cut out of its own
  # occupancy -- so no such interval may resolve on its own `end_year`. Stated
  # on the resolved INTERVAL rather than on the polycell: a polycell holds many
  # consecutive slices, and its presence at a breakpoint is the next slice
  # doing its job, not a leak.
  succeeded <- dplyr::filter(yearly, .data$end_year < domain_end)
  testthat::expect_gt(nrow(succeeded), 0L)
  testthat::expect_equal(sum(succeeded$year == succeeded$end_year), 0L)
  # ... while every interval that reaches the domain end resolves on it.
  testthat::expect_setequal(
    dplyr::filter(yearly, .data$year == domain_end)$polycell_id,
    dplyr::filter(intervals, .data$end_year == domain_end)$polycell_id
  )

  # BETA's boundary year is the successor's alone, in value as in label.
  boundary <- dplyr::filter(yearly, .data$year == 1990L)
  testthat::expect_equal(
    sort(boundary$polity_code[
      whep:::.polity_family(boundary$polity_code) == "BETA"
    ]),
    "BETA-1990-2025"
  )
})

testthat::test_that("the domain end is read from the table, never fixed", {
  testthat::skip_if_not_installed("sf")

  # PR #551 already moves the shipped table, so "still open" cannot be a
  # hardcoded 2025. A table whose coverage stops in 2000 opens 2000.
  spec <- dplyr::mutate(
    pcs_da24_spec(),
    start_year = .data$start_year - 25L,
    end_year = .data$end_year - 25L
  )
  intervals <- whep::build_polycell_support(
    geometries = pcs_da24_geometries(spec)
  )
  testthat::expect_equal(max(intervals$end_year), 2000L)
  testthat::expect_gt(nrow(whep::expand_polycell_years(intervals, 2000L)), 0L)
  testthat::expect_equal(
    nrow(whep::expand_polycell_years(intervals, 2001L)),
    0L
  )
  testthat::expect_equal(
    nrow(whep::expand_polycell_years(intervals, 2025L)),
    0L
  )
})

testthat::test_that("still-open is absence of a successor, not the year", {
  testthat::skip_if_not_installed("sf")

  # The trap AM-27 records. GAMMA-1900-2025 and GAMMA-1975-2025 both end on the
  # domain end, and DELTA-1990-2025 starts later than DELTA-1900-2025 but lives
  # in a DIFFERENT cell. A test that opens by the maximum year alone opens too
  # many; one that keys on `polycell_id` or `polity_code` opens every terminal
  # row; one that keys on the family without the cell closes DELTA in cell 1.
  # Each of those is visible here.
  intervals <- whep::build_polycell_support(geometries = pcs_da24_geometries())
  domain_end <- max(intervals$end_year)
  at_end <- whep::expand_polycell_years(intervals, domain_end)

  # DELTA is open in the cell where nothing succeeded it.
  delta <- dplyr::filter(
    at_end,
    whep:::.polity_family(.data$polity_code) == "DELTA"
  )
  testthat::expect_setequal(
    delta$polity_code,
    c("DELTA-1900-2025", "DELTA-1990-2025")
  )
  testthat::expect_equal(dplyr::n_distinct(delta$cell_id), 2L)

  # ETA's earlier-ending sibling succeeds nothing, so ETA keeps its own end.
  testthat::expect_true("ETA-1900-2025" %in% at_end$polity_code)

  # An `area_code` of NA cannot cost a polity its open end.
  testthat::expect_true("ZETA-1950-2025" %in% at_end$polity_code)
  testthat::expect_true(all(is.na(
    at_end$area_code[at_end$polity_code == "ZETA-1950-2025"]
  )))

  # Two families sharing one `area_code` are two lineages, not one.
  testthat::expect_true(all(
    c("SDN2-2011-2025", "SSD2-2011-2025") %in% at_end$polity_code
  ))

  # A polity that dissolved before the domain end stays dissolved.
  testthat::expect_false("EPSIL-1900-1950" %in% at_end$polity_code)
})

testthat::test_that("the producer and the polity resolver agree on the year", {
  testthat::skip_if_not_installed("sf")

  # The point of DA-24 living in ONE predicate: `expand_polycell_years()` and
  # `.active_polities()` must call the same year the same way. The two differ
  # in ONE respect, and only where the fixture's own intervals overlap in time:
  # `.active_polities()` dedupes an overlap to one interval per family, which
  # the producer's grain deliberately does not, at every year alike. So the
  # producer's set is a superset always, and equal on every family whose
  # intervals do not overlap. The overlapping families are derived from the
  # fixture and pinned, so the exception cannot silently grow.
  spec <- pcs_da24_spec()
  geometries <- pcs_da24_geometries(spec)
  intervals <- whep::build_polycell_support(geometries = geometries)
  polities <- sf::st_transform(
    geometries[, c("polity_code", "start_year", "end_year")],
    6933
  )

  families <- split(spec, whep:::.polity_family(spec$polity_code))
  overlaps <- \(g) {
    yrs <- seq(min(g$start_year), max(g$end_year) - 1L)
    any(
      vapply(
        yrs,
        \(y) sum(g$start_year <= y & y < g$end_year),
        integer(1)
      ) >
        1L
    )
  }
  overlapping <- names(families)[vapply(families, overlaps, logical(1))]
  testthat::expect_setequal(overlapping, c("DELTA", "ETA", "GAMMA"))

  years <- seq(min(intervals$start_year), max(intervals$end_year) + 1L)
  compare <- purrr::map(years, \(yr) {
    producer <- unique(whep::expand_polycell_years(intervals, yr)$polity_code)
    resolver <- whep:::.active_polities(polities, yr)$polity_code
    clean <- \(x) x[!whep:::.polity_family(x) %in% overlapping]
    tibble::tibble(
      year = yr,
      missing = length(setdiff(resolver, producer)),
      unequal = !setequal(clean(producer), clean(resolver))
    )
  }) |>
    dplyr::bind_rows()

  testthat::expect_equal(sum(compare$missing), 0L)
  testthat::expect_equal(sum(compare$unequal), 0L)
  # Not vacuous: the years compared really do resolve something.
  testthat::expect_gt(
    nrow(whep::expand_polycell_years(intervals, max(intervals$end_year))),
    0L
  )
})

testthat::test_that("the footprint resolves a crosswalk year at the open end", {
  testthat::skip_if_not_installed("sf")

  # `.pcs_polycell_footprint()` resolves `data$crosswalk_year` with the
  # package's own predicate. Under the uniformly exclusive read a crosswalk
  # describing the domain end matched NOTHING, so the footprint reconciliation
  # -- the diagnostic that exists to measure the migration's movement -- came
  # back empty while still reporting a row for itself.
  #
  # Before C9 this block also pinned the shim's `polity_frac` attachment and
  # its `crosswalk_only` padding; both are gone, so what is left is the year
  # resolution and the requirement that a crosswalk cell the intersection
  # cannot reproduce stays OUT of the polycell footprint. That is now the only
  # thing keeping the reconciliation from agreeing with itself by construction.
  geometries <- pcs_da24_geometries()
  base <- whep::build_polycell_support(years = 2015L, geometries = geometries)
  crosswalk <- base |>
    dplyr::filter(!is.na(.data$area_code)) |>
    dplyr::summarise(
      polity_frac = sum(.data$polity_area_ha) /
        dplyr::first(.data$cell_area_ha),
      .by = c("lon", "lat", "area_code")
    ) |>
    # two cells the intersection cannot reproduce
    dplyr::bind_rows(tibble::tibble(
      lon = c(-179.75, -179.25),
      lat = c(-89.75, -89.25),
      area_code = c(9001L, 9002L),
      polity_frac = c(0.5, 1)
    ))

  built <- \(yr) {
    whep::build_polycell_support(
      geometries = geometries,
      data = list(crosswalk = crosswalk, crosswalk_year = yr)
    )
  }
  mid <- built(2015L)
  at_end <- built(2025L)

  # No crosswalk column and no padding row, on either year.
  testthat::expect_false(rlang::has_name(mid, "polity_frac"))
  testthat::expect_false(rlang::has_name(at_end, "polity_frac"))
  testthat::expect_false(any(mid$coverage_status == "crosswalk_only"))
  testthat::expect_false(any(at_end$coverage_status == "crosswalk_only"))
  testthat::expect_equal(nrow(at_end), nrow(mid))

  polycell_row <- \(x) {
    fp <- attr(x, "footprints")
    fp[fp$footprint == "polycell", c("rows", "cells")]
  }
  testthat::expect_equal(polycell_row(at_end), polycell_row(mid))
  # The footprint is the POLYCELLS covering the year, and nothing else: the two
  # unreproducible crosswalk cells must not be counted into it.
  testthat::expect_equal(
    polycell_row(mid)$rows,
    nrow(dplyr::distinct(base, .data$lon, .data$lat, .data$area_code))
  )
  testthat::expect_gt(polycell_row(at_end)$rows, 0L)
  # And they ARE still reported, as a footprint disagreement rather than as
  # rows of the support -- otherwise the removal would have lost information
  # rather than moved it.
  diff <- attr(mid, "footprint_diff")
  testthat::expect_setequal(
    dplyr::filter(diff, !.data$polycell)$area_code,
    c(9001L, 9002L)
  )
})

testthat::test_that("an unreproducible crosswalk cell adds no row at all", {
  testthat::skip_if_not_installed("sf")

  # C9's sharpest padding case, kept as a regression. `.pcs_append_crosswalk_
  # only()` pinned its rows to `[crosswalk_year, crosswalk_year + 1)`, so with
  # `crosswalk_year` one year short of the domain end the synthetic window
  # ENDED on the domain end. Only its NA `polity_code` kept it from moving the
  # table's coverage past the real intervals and closing every one of them.
  # With the padding gone the hazard is structural rather than guarded.
  geometries <- pcs_da24_geometries()
  crosswalk <- tibble::tibble(
    lon = c(-179.75, -179.25),
    lat = c(-89.75, -89.25),
    area_code = c(9001L, 9002L),
    polity_frac = c(0.5, 1)
  )
  bare <- whep::build_polycell_support(geometries = geometries)
  support <- whep::build_polycell_support(
    geometries = geometries,
    data = list(crosswalk = crosswalk, crosswalk_year = 2024L)
  )
  # Supplying the crosswalk changes no row and no VALUE of the support at all;
  # it only adds the DA-12 footprint diagnostics, which ride as attributes.
  # Compared as frames these two would differ on those attributes alone, which
  # is not what this block is about, so the comparison is column by column.
  testthat::expect_identical(names(support), names(bare))
  testthat::expect_identical(nrow(support), nrow(bare))
  purrr::walk(names(bare), \(nm) {
    testthat::expect_identical(support[[nm]], bare[[nm]], info = nm)
  })
  # ... and the footprint diagnostics ARE the difference, so the crosswalk is
  # doing something and this is not a comparison of two identical calls.
  testthat::expect_null(attr(bare, "footprints"))
  testthat::expect_equal(nrow(attr(support, "footprints")), 2L)
  testthat::expect_false(any(support$coverage_status == "crosswalk_only"))
  testthat::expect_false(anyNA(support$polity_code))
  testthat::expect_equal(max(support$end_year), 2025L)

  # The real intervals still resolve on the domain end ...
  at_end <- whep::expand_polycell_years(support, 2025L)
  testthat::expect_gt(nrow(at_end), 0L)
  testthat::expect_true(all(!is.na(at_end$polity_code)))
  # ... and 2024 carries no polity-less row for the padding to have been.
  in_2024 <- whep::expand_polycell_years(support, 2024L)
  testthat::expect_equal(sum(is.na(in_2024$polity_code)), 0L)
  testthat::expect_gt(nrow(in_2024), 0L)
})

testthat::test_that("the open-end key is the cell and the polity family", {
  # Unit-level, on `.pcs_open_intervals()` itself, because
  # `.pcs_split_intervals()` happens to give every interval reaching the domain
  # end in one cell the SAME start year, which hides most grouping errors on
  # producer output. The helper's contract is wider than that accident, and
  # this is where the four candidate keys separate:
  #
  #   cell + family (used)  AGO's later interval alone; FOO and BAR both open
  #   cell + polity_code    opens BOTH AGO intervals -- the double count
  #   cell alone            closes FOO, whose family nothing succeeded
  #   cell + area_code      closes FOO too: FOO and BAR share code 206, as the
  #                         real SDN-2011-2025 and SSD-2011-2025 do
  #   family alone          closes DELTA in the cell where it is still open
  tbl <- tibble::tribble(
    ~cell_id, ~polity_code, ~start_year, ~end_year, ~area_code,
    1L, "AGO-1816-1975", 1816L, 1975L, 7L,
    1L, "AGO-1816-2025", 1816L, 2025L, 7L,
    1L, "AGO-1975-2025", 1975L, 2025L, 7L,
    1L, "FOO-1900-2025", 1900L, 2025L, 206L,
    1L, "BAR-1990-2025", 1990L, 2025L, 206L,
    2L, "DELTA-1900-2025", 1900L, 2025L, 104L,
    3L, "DELTA-1990-2025", 1990L, 2025L, 104L,
    4L, NA_character_, 2024L, 2025L, 9001L
  )
  open <- whep:::.pcs_open_intervals(tbl)
  testthat::expect_equal(
    tbl$polity_code[open],
    c(
      "AGO-1975-2025",
      "FOO-1900-2025",
      "BAR-1990-2025",
      "DELTA-1900-2025",
      "DELTA-1990-2025"
    )
  )

  # And the same statement through the predicate: the domain end resolves to
  # the open intervals, the year before to everything live then, and the year
  # after to nothing.
  covered <- \(yr) tbl$polity_code[whep:::.pcs_covers_year(tbl, yr) %in% TRUE]
  testthat::expect_equal(covered(2025L), tbl$polity_code[open])
  testthat::expect_equal(
    covered(2024L),
    c(
      "AGO-1816-2025",
      "AGO-1975-2025",
      "FOO-1900-2025",
      "BAR-1990-2025",
      "DELTA-1900-2025",
      "DELTA-1990-2025",
      NA_character_
    )
  )
  testthat::expect_equal(covered(2026L), character())

  # The polity-less row can never be open, and its `end_year` must not define
  # the domain either. Pushed one year past every real interval it takes
  # `max(end_year)` with it, and if it were counted the real intervals would no
  # longer reach the end and every one of them would close. They must not: the
  # polity-less row still covers 2025 here, but only because its own half-open
  # window plainly does.
  #
  # Until C9 the DA-13 shim's `crosswalk_only` padding was the only thing that
  # produced such a row. The padding is gone, and this fixture is why the
  # `!is.na(polity_code)` guard in `.pcs_open_intervals()` stayed: `polity_code`
  # is taken from the geometry source as supplied, and `.pcs_open_intervals()`
  # is called on tables the caller builds.
  padded <- dplyr::mutate(
    tbl,
    end_year = dplyr::if_else(is.na(.data$polity_code), 2026L, .data$end_year)
  )
  testthat::expect_equal(max(padded$end_year), 2026L)
  testthat::expect_equal(whep:::.pcs_open_intervals(padded), open)
  at_end <- whep:::.pcs_covers_year(padded, 2025L) %in% TRUE
  testthat::expect_equal(
    padded$polity_code[at_end & !is.na(padded$polity_code)],
    tbl$polity_code[open]
  )
})

testthat::test_that("expanding a year needs the succession key present", {
  testthat::skip_if_not_installed("sf")

  # Without `cell_id` and `polity_code` the open end cannot be told from a
  # succession, so the absence is an error rather than a quieter answer.
  intervals <- whep::build_polycell_support(geometries = pcs_da24_geometries())
  testthat::expect_error(
    whep::expand_polycell_years(
      dplyr::select(intervals, -"polity_code"),
      2025L
    ),
    "polity_code"
  )
  testthat::expect_error(
    whep::expand_polycell_years(dplyr::select(intervals, -"cell_id"), 2025L),
    "cell_id"
  )
})

testthat::test_that("a water layer that matches almost nothing is called out", {
  testthat::skip_if_not_installed("sf")
  # Off-grid by 1e-13: too small to print, enough to miss every join. Without
  # the guard this builds "successfully" with every hectare of water booked as
  # land.
  support <- whep::build_polycell_support(
    geometries = whep::polycell_example_geometries()
  )
  drifted <- tibble::tibble(
    lon = unique(support$lon) + 1e-13,
    lat = unique(support$lat)[[1L]],
    water_frac = 0.5
  )

  testthat::expect_warning(
    whep::build_polycell_support(
      geometries = whep::polycell_example_geometries(),
      water = drifted
    ),
    "booked as DRY"
  )
})

# An ABSENT optional layer must not be silent (#885) ---------------------------
#
# `water` and `ice` default to NULL and the columns are zero-filled. The
# producer's identity `polity_area_ha == land + inland_water + ice` still holds,
# so nothing downstream can tell a zero-filled pin from a measured one -- which
# is how the deployed pin `20260818T105426Z-a0330` came to book every lake,
# river and glacier as land, 536.0 Mha (+4.15%) of 2015 land, under a
# regeneration whose commit said "No published values move".

testthat::test_that("an absent water layer warns instead of zero-filling silently", {
  testthat::skip_if_not_installed("sf")
  polities <- pcs_polities(
    tibble::tibble(
      polity_code = "PCSW-2000-2020",
      start_year = 2000L,
      end_year = 2020L
    ),
    list(pcs_inset(10.05, 10.45))
  )
  cnd <- testthat::expect_warning(
    result <- whep::build_polycell_support(
      years = 2015L,
      geometries = polities
    ),
    class = "whep_polycell_absent_water"
  )
  # The message has to name the consequence, not just the absence: a reader who
  # sees "no water layer" and not "lakes are booked as land" does not act.
  testthat::expect_match(conditionMessage(cnd), "identically zero")
  # And the zero-fill still happens -- this is a warning, not a behaviour change.
  testthat::expect_true(all(result$inland_water_ha == 0))
})

testthat::test_that("an absent ice layer warns on its own class", {
  testthat::skip_if_not_installed("sf")
  polities <- pcs_polities(
    tibble::tibble(
      polity_code = "PCSI-2000-2020",
      start_year = 2000L,
      end_year = 2020L
    ),
    list(pcs_inset(20.05, 20.45))
  )
  # Asserted separately from water so one firing cannot stand in for the other:
  # the deployed pin lost BOTH layers, and a single class would let a future
  # build lose only ice and still look covered.
  testthat::expect_warning(
    result <- whep::build_polycell_support(
      years = 2015L,
      geometries = polities
    ),
    class = "whep_polycell_absent_ice"
  )
  testthat::expect_true(all(result$ice_area_ha == 0))
})

testthat::test_that("a supplied water layer does not warn about absence", {
  testthat::skip_if_not_installed("sf")
  polities <- pcs_polities(
    tibble::tibble(
      polity_code = "PCSD-2000-2020",
      start_year = 2000L,
      end_year = 2020L
    ),
    list(pcs_inset(10.05, 10.45))
  )
  # The negative control. Without it the two tests above would pass against a
  # warning that fires unconditionally, which would make the signal worthless.
  water <- tibble::tibble(lon = 10.25, lat = 10.25, water_frac = 0.1)
  cnd <- tryCatch(
    {
      whep::build_polycell_support(
        years = 2015L,
        geometries = polities,
        water = water
      )
      NULL
    },
    whep_polycell_absent_water = function(w) w
  )
  testthat::expect_null(cnd)
})

# -- whep#907: the label is the reporting area_code, not the matrix bucket ----

testthat::test_that("a polity's label is its reporting area, not its bucket", {
  # `polity_area_crosswalk` carries both codes on the same row. The bucket
  # merges Sudan with South Sudan onto 206 and folds Syria into 999, and both
  # are codes a `country_areas`-keyed caller cannot join to.
  codes <- whep:::.polity_reporting_area_code(c(
    "SDN-2011-2025",
    "SSD-2011-2025",
    "SYR-1967-2025",
    "MKD-1991-2025",
    "SWZ-1894-2025"
  ))
  testthat::expect_equal(codes, c(276L, 277L, 212L, 154L, 209L))
  buckets <- whep::polity_area_crosswalk |>
    dplyr::filter(polity_code %in% c("SDN-2011-2025", "SYR-1967-2025")) |>
    dplyr::pull(polity_area_code) |>
    unique() |>
    sort()
  testthat::expect_equal(as.integer(buckets), c(206L, 999L))
})

testthat::test_that("a residual aggregate keeps the bucket it is defined by", {
  # `ROW` answers for 62 reporting areas at once, so no single reporting code
  # names it; the historical Ethiopian polities carry both 62 and 238. Neither
  # may be handed one of its members' codes.
  testthat::expect_equal(
    whep:::.polity_reporting_area_code("ROW-1850-2025"),
    999L
  )
  testthat::expect_equal(
    whep:::.polity_reporting_area_code("ETH-1952-1993"),
    238L
  )
})

testthat::test_that("a polity with no reporting region keeps its bucket", {
  # Greenland and Western Sahara have a FAOSTAT code but no `regions.csv` row,
  # so 999 is the only home the reporting vocabulary gives them. Inventing 85
  # or 205 here would put a code in the grid that joins to nothing.
  live <- whep:::.regions_csv_area_codes()
  testthat::expect_false(85L %in% live)
  testthat::expect_equal(
    whep:::.polity_reporting_area_code(c("GRL-1800-2025", "ESH-1975-2025")),
    c(999L, 999L)
  )
})

testthat::test_that("an unknown polity resolves to NA, never to a guess", {
  testthat::expect_true(is.na(
    whep:::.polity_reporting_area_code("ZZZ-1900-2000")
  ))
})

testthat::test_that("every resolved label is a code some caller can join", {
  # The invariant, rather than a list of expectations: whatever the crosswalk
  # grows to, the producer may only emit codes the reporting vocabulary knows.
  # Exactly two escape it, both as a bucket rather than a reporting code and
  # neither reaching the shipped support: 206, which `regions.csv` retired but
  # which pre-2011 FAOSTAT reported Sudan under (whep#860), and 351, the
  # FAOSTAT China aggregate that double-counts its own components (whep#384).
  codes <- whep:::.polity_area_code_lookup()$area_code
  extra <- sort(setdiff(unique(codes), whep:::.regions_csv_area_codes()))
  testthat::expect_equal(extra, c(206L, 351L))
})
