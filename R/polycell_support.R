#' Build the polycell spatial support table
#'
#' @description
#' Produce WHEP's canonical spatial support unit, the **polycell**: a
#' 0.5-degree grid cell intersected with a polity over that polity's validity
#' interval. Each row carries the polity's territory in the cell decomposed into
#' three separately addressable categories,
#' `polity_area_ha = land_area_ha + inland_water_ha + ice_area_ha`, so that
#' aggregating polycells to a polity changes no absolute value and no quantity
#' crosses a border it does not belong to.
#'
#' Areas are geodesic, from `sf::st_area()` on unprojected WGS84 with spherical
#' (`s2`) geometry, matching WHEP's own spherical convention; `cell_area_ha`
#' keeps the package formula so it stays bit-identical to
#' [build_cell_polity()]. Territory is the union of live real polities; land
#' claimed by no live polity is emitted in the `"unassigned"` attribute rather
#' than renormalised away.
#'
#' The default grain is **interval-keyed**: one row per polycell per interval
#' over which every area is constant, carrying `start_year` and `end_year`.
#' Supply `years` to expand to one row per polycell-year, which is what
#' [expand_polycell_years()] does on demand. No area varies within an interval,
#' so the interval grain is the form to store.
#'
#' @param years Optional integer vector of calendar years. `NULL` (default)
#'   returns the interval-keyed grain; a vector expands to one row per
#'   polycell-year and adds a `year` column.
#' @param aggregates What to do with `polity_type == "aggregate"` rows, which
#'   cannot join the partition because an aggregate's polygon covers its
#'   members'. `"exclude"` (default) drops them, which is what every published
#'   polycell table holds. `"overlap_layer"` clips them too and emits them
#'   alongside the partition marked `support_role == "overlap"` -- see
#'   *The aggregate overlap layer* below.
#' @param geometries An `sf` table of polity geometries with at least
#'   `polity_code`, `start_year` and `end_year`; defaults to
#'   [get_polity_geometries()]. `start_year` is inclusive; `end_year` is
#'   **exclusive at a succession** and **inclusive at the open end**, the
#'   convention `polities` is documented under, and neither bound is ever
#'   parsed out of `polity_code`. The intervals of one polity must partition
#'   time: two that overlap are an error rather than a shape the producer
#'   reconciles, and abort with class `whep_pcs_overlapping_interval`. Optional
#'   `wiki_status`, `polity_type`, `polygon_status` and `area_code` columns are
#'   honoured.
#' @param water Optional per-cell `tibble` of inland water with `lon`, `lat`
#'   and `water_frac`, a fraction of the **whole** cell, as
#'   [read_glwd_water()] returns it.
#' @param ice Optional `sf` polygon layer of glaciated area, as
#'   [read_glaciated_areas()] returns it, subtracted per polycell by exact
#'   geodesic intersection.
#' @param data Optional named list of auxiliary layers: `luh2` the validation
#'   layer (`lon`, `lat`, `terrestrial_ha`, e.g. [read_luh2_terrestrial()]);
#'   `crosswalk` the deployed [build_cell_polity()] table; `producer_crosswalk`
#'   a freshly built `build_cell_polity_fraction()` table; and
#'   `crosswalk_year`, the year whose polycells the crosswalk's present-day
#'   geometry describes (default 2015). The two crosswalks are read **only** by
#'   the DA-12 footprint reconciliation: no crosswalk column is carried into
#'   the output, and no crosswalk row the intersection did not reproduce is
#'   appended to it.
#'
#' @return A `tibble` whose columns are a superset of `polycell_id`, `cell_id`,
#'   `lon`, `lat`, `polity_code`, `area_code`, `start_year`, `end_year`,
#'   `cell_area_ha`, `polity_area_ha`, `land_area_ha`, `inland_water_ha`,
#'   `ice_area_ha`, `geometry_source`, `polygon_status`, `split_method`,
#'   `coverage_status`, `support_role`, `area_engine` and `luh2_vintage`, plus
#'   `year` when `years` is supplied. `support_role` is `"partition"` on every
#'   row unless `aggregates = "overlap_layer"` was asked for. `area_engine` is
#'   `"s2"` except on the pieces the spherical engine cannot read back, which
#'   are measured with `terra::expanse()` rather than dropped. Diagnostics ride
#'   as attributes:
#'   `"unassigned"` (the validation-layer disagreement, in both directions:
#'   `unassigned_land_ha` where the polities claim less than the layer and
#'   `over_claimed_land_ha` where they claim more), `"coverage"` (every live
#'   polity interval and why it did or did not produce polycells),
#'   `"overlap"` (cells holding more territory than the cell, because two
#'   polities were handed the same polygon), `"long_edges"` (polity edges the
#'   source stores as one long segment along a parallel, which s2 draws as a
#'   bulging great circle), `"terra_measured"` (polycells whose area came from
#'   `terra`), `"water_excess"` (inland water clamped to the polycell's
#'   territory), `"water_unmatched"` (cells the water layer and the polycells do
#'   not share), `"footprints"` and `"footprint_diff"` (the deployed crosswalk,
#'   the current producer and the polycell footprint, reconciled at
#'   `data$crosswalk_year`).
#'
#'   `"overlap"`, `"terra_measured"`, `"water_excess"`, `"water_unmatched"` and
#'   `"unassigned"` are **interval-grain**, like the table itself: they carry
#'   `start_year` and `end_year`, and one cell contributes a row per interval.
#'   Summing them without first filtering to the interval covering the year of
#'   interest counts the same cell once per epoch. On the shipped polities that
#'   is the difference between 1,343 clamped polycells over all epochs and 94 in
#'   2015.
#'
#'   Every row is a real polycell: `polity_code`, `polycell_id` and the area
#'   columns are populated on all of them, so `sum(land_area_ha)` over the
#'   output is the land the intersection measured. The DA-13 transition, which
#'   padded the table with the crosswalk rows the intersection did not
#'   reproduce and carried `polity_frac` alongside, ended with C9; the
#'   footprint diagnostics below are where that disagreement is now reported.
#'
#'   **Identity is `polity_code`, and only `polity_code`.** `area_code` rides
#'   along as a label and is not a key: `polity_area_crosswalk` folds 505
#'   polity codes into 201 reporting buckets, 113 of which hold more than one
#'   polity and one of which (206) holds Sudan and South Sudan at the same
#'   time. A table whose whole purpose is correct territorial attribution
#'   cannot be keyed on a bucket that merges two countries, so this one is
#'   not, and no `reporting_polity_code` or `polity_area_code` is derived
#'   here. A consumer joining to a reporting-vocabulary output converts at its
#'   own boundary, and **that conversion is where the lossy fold happens** --
#'   deliberately visible at the consumer rather than hidden in the support.
#'   [build_n_deposition()] refuses an unconverted support instead of
#'   converting one silently.
#'
#' @section Land definitions in play:
#' Four definitions of "land" are live in this pipeline and they disagree by
#' up to 10%, so a global area only means something next to the definition it
#' was measured on. At 2015:
#'
#' | Definition | Global area |
#' |------------|-------------|
#' | Whole 0.5-degree cells holding any land | 14.3195 Gha |
#' | HaNi's own land mask | 13.5977 Gha |
#' | Union of the live polity polygons | 13.4267 Gha |
#' | LUH2 terrestrial, `(1 - icwtr) * carea` | 12.9931 Gha |
#'
#' `polity_area_ha` carries the third row's territory -- the polity polygons,
#' decomposed into land, inland water and ice -- but **summing it does not
#' give the third row**. The union is unique ground, which is what makes it
#' comparable with the other three; a sum counts shared ground once per
#' claiming polity. At 2015 `sum(polity_area_ha)` is 13.4599 Gha, exceeding
#' the union by the 0.0332 Gha two live polities both claim. Quote the union
#' for a land definition and the sum for attributed territory, and never read
#' the difference between them as a leak. The first row is the
#' convention this table replaces -- a per-hectare rate multiplied by
#' `cell_area_ha` -- and it over-counts by 11.0%. The fourth is the DA-5
#' validation layer: its disagreement with the polygons is emitted in the
#' `"unassigned"` attribute and never silently reconciled, and the polygons
#' exceeding it by about 2.2% is what `inland_water_ha + ice_area_ha` has to
#' account for. The second belongs to the deposition source and governs a
#' different quantity -- see [build_n_deposition()], where WHEP's territory
#' decides *placement* while HaNi's mask decides the *total*.
#'
#' Only the first and fourth rows are constants of the inputs;
#' `inst/scripts/diagnose_polycell_support.R` re-derives both. The polygon row
#' moves with the polity vintage, so read it back off the table in hand rather
#' than quoting it -- `inst/scripts/reconcile_polity_areas.R` measures it.
#'
#' A fifth land mask is present but deliberately absent from the ladder,
#' because nothing is measured on it: the GLWD water layer carries the CRU
#' mask (67,420 cells against LUH2's 64,493 terrestrial), so cells one carries
#' and the other does not are reported in `"water_unmatched"` rather than
#' dropped by an inner join.
#'
#' @section What does not vary historically:
#' `ice_area_ha` comes from `ne_10m_glaciated_areas` (see
#' [read_glaciated_areas()]), a coarse **present-day snapshot**, so it is the
#' same number in 1850 as in 2015: a historical run carries today's ice
#' extent, and land that lay under ice in 1850 is credited to
#' `land_area_ha`. This is accepted only because ice is a **reporting
#' category and not a driver** -- nothing in the package divides by
#' `ice_area_ha` or drives a flux with it. If ice ever becomes a driver, the
#' source has to be reopened rather than the caveat restated.
#'
#' Polity geometry is likewise constant within an interval, and the GLWD file
#' carries a single time step. That is why the default grain is
#' interval-keyed: no area column varies by year, so a per-year grain would
#' repeat identical rows about 173 times.
#'
#' @section The aggregate overlap layer:
#' An **aggregate** polity -- `BLX-1850-1999` Belgium-Luxembourg,
#' `F249-1918-1990` Yemen, the six residual `"Other"` regions -- is a reporting
#' bucket's territory, and its polygon covers its members'. It therefore cannot
#' be a row of the partition: two rows claiming the same ground would hand the
#' cell's land out twice, which is exactly what the rasterised cover this table
#' replaced did, halving Belgium's 1961 cropland (whep#800).
#'
#' Dropping it outright is not free either. FAOSTAT keys its pre-2000 data on
#' those buckets: Belgium (255) and Luxembourg (256) carry no data before 2000,
#' bucket 15 does, and bucket 15's only territory is `BLX-1850-1999`. Measured
#' against `polity_area_crosswalk` over 1850-1961, **ten** of the 460 polities
#' the pre-1962 resolver reaches are aggregates and **ten** reporting buckets
#' have no other territory in at least one year: 15, 151, 237 (1954-1961 only),
#' 249 and 901-906.
#'
#' So both granularities are kept, and they are kept apart. With
#' `aggregates = "overlap_layer"`:
#'
#' * every row carries `support_role`, `"partition"` or `"overlap"`;
#' * `"partition"` rows are exactly what `"exclude"` emits -- same polities,
#'   same territory, same land, water and ice, split into more intervals only
#'   where an aggregate's validity adds a breakpoint to a cell;
#' * the cell's inland water is apportioned over the **partition's** territory
#'   in that cell, so an aggregate receives what its members receive and the
#'   members' share is not diluted by the layer covering them;
#' * every diagnostic that describes the partition -- `"overlap"`,
#'   `"unassigned"`, `"water_unmatched"`, `"footprints"` -- is measured on the
#'   partition alone, so admitting the layer cannot make the polygons look like
#'   they over-claim the validation layer.
#'
#' The consumer contract is the other half: [read_polycell_support()] returns
#' the **partition** unless asked otherwise, so no existing consumer can pick
#' up an overlapping row by accident, and a consumer that wants a bucket's own
#' territory asks for `role = "overlap"` (or `"all"`) and states that it is
#' summing a layer that double-counts by construction. Never aggregate across
#' the two roles.
#'
#' **The layer is not a partition of itself either**, and that is not a defect
#' to be fixed by a tolerance: `ROW-1850-2025` Rest of World contains the six
#' regional residuals it is the sum of. Built on the 19 live aggregates of
#' `whep::polities` (779 rows, ingest 2026-08-13) it is 12,644 polycells, and
#' at 2015 it puts more territory in a cell than the cell holds in 2,751 cells
#' shared by `ROW` and `REUR`, 92 by `ROW` and `RAFR`, 8 by `ROW` and `ROCE`
#' and 7 by `ROW` and `RLAM` -- plus 2 cells where `CODRU-1922-1960` and
#' `EGYSUD-1934-1956` overlap in 1950, which is a polygon disagreement rather
#' than nesting. So a consumer takes **one polity's** polycells out of the
#' layer -- the one its bucket resolves to that year -- and never sums the
#' layer as a whole. That is also why the `"overlap"` diagnostic keeps
#' measuring the partition only: an over-full cell means something there, and
#' in this layer it means nothing.
#' @export
#'
#' @examples
#' if (requireNamespace("sf", quietly = TRUE)) {
#'   build_polycell_support(
#'     years = 2015L,
#'     geometries = polycell_example_geometries()
#'   )
#' }
build_polycell_support <- function(
  years = NULL,
  geometries = NULL,
  water = NULL,
  ice = NULL,
  data = list(),
  aggregates = c("exclude", "overlap_layer")
) {
  rlang::check_installed("sf")
  aggregates <- rlang::arg_match(aggregates)
  old_s2 <- sf::sf_use_s2()
  withr::defer(suppressMessages(sf::sf_use_s2(old_s2)))
  suppressMessages(sf::sf_use_s2(TRUE))

  geometries <- geometries %||% get_polity_geometries()
  polities <- .pcs_prepare_polities(geometries, aggregates)
  support <- polities |>
    .pcs_intersect_grid() |>
    .pcs_add_ice(.pcs_prepare_ice(ice)) |>
    .pcs_split_intervals() |>
    .pcs_add_water(water) |>
    .pcs_finalize(.pcs_geometry_source(geometries), data)

  .pcs_inform_overlap_layer(support)
  support |>
    .pcs_attach_diagnostics(polities, data, water) |>
    .pcs_expand(years)
}

#' A minimal polity geometry table for examples and smoke tests
#'
#' @description
#' Returns one live polity holding a rectangle that spans six 0.5-degree cells,
#' in the shape [get_polity_geometries()] returns: enough to run
#' [build_polycell_support()] end to end in a fraction of a second, with no
#' pins, no rasters and no environment variables.
#'
#' @return An `sf` table with `polity_code`, `polity_type`, `wiki_status`,
#'   `polygon_status`, `start_year`, `end_year`, `area_code` and a `geom`
#'   multipolygon in WGS84.
#' @export
#'
#' @examples
#' if (requireNamespace("sf", quietly = TRUE)) {
#'   polycell_example_geometries()
#' }
polycell_example_geometries <- function() {
  rlang::check_installed("sf")
  sf::st_sf(
    polity_code = "AAA-2000-2020",
    polity_type = "national",
    wiki_status = "reviewed",
    polygon_status = "assigned",
    start_year = 2000L,
    end_year = 2020L,
    area_code = 11L,
    geom = sf::st_sfc(
      sf::st_polygon(list(cbind(
        c(10.1, 11.4, 11.4, 10.1, 10.1),
        c(44.9, 44.9, 45.4, 45.4, 44.9)
      ))),
      crs = 4326
    )
  )
}

#' Expand the interval-keyed polycell support to one row per year
#'
#' @description
#' Repeats every polycell interval over the calendar years it covers, adding a
#' `year` column. `start_year` is inclusive; `end_year` is **exclusive at a
#' succession**, so a boundary year resolves to the successor alone and is
#' never counted twice, and **inclusive at the open end**, so the last year the
#' table covers still resolves to the polity nothing succeeds instead of to
#' nothing at all.
#'
#' @param support A [build_polycell_support()] table in the interval grain,
#'   carrying `cell_id`, `polity_code`, `start_year` and `end_year`. The first
#'   two are what identify successive intervals of one polity in one cell, and
#'   without them the open end cannot be told from a succession.
#' @param years Integer vector of calendar years.
#'
#' @return A `tibble` with one row per polycell-year, `year` placed after
#'   `area_code`.
#' @export
#'
#' @examples
#' if (requireNamespace("sf", quietly = TRUE)) {
#'   build_polycell_support(geometries = polycell_example_geometries()) |>
#'     expand_polycell_years(2010L:2012L)
#' }
expand_polycell_years <- function(support, years) {
  .pcs_require_cols(
    support,
    c("cell_id", "polity_code", "start_year", "end_year"),
    "support"
  )
  years <- as.integer(years)
  # Whether an interval is succeeded is a property of the TABLE, not of the
  # year being asked for, so the succession key is built once here rather than
  # rebuilt for every year expanded.
  open <- .pcs_open_intervals(support)
  years |>
    purrr::map(\(yr) {
      covers <- .pcs_covers_year(support, yr, open)
      # Subset outside the data mask, as `.filter_country_grid_year()` does:
      # `support` is the caller's table, and a column of its own called
      # `covers` would shadow the vector and filter on the wrong thing.
      support[which(covers), , drop = FALSE] |>
        dplyr::mutate(year = yr, .after = "area_code")
    }) |>
    dplyr::bind_rows()
}

# -- Geometry source ----------------------------------------------------------

.pcs_geometry_source <- function(geometries) {
  attr(geometries, "geometry_source") %||% "whep::polities"
}

# -- THE SNAPSHOT AND RUNTIME EACH CENSUS BELOW WAS MEASURED AGAINST ----------
#
# Every census below is a property of one upstream snapshot, not of the
# producer. Engine-specific figures are ALSO properties of the geometry
# runtime: T-A15 proved that ULP-level s2 validity differs by platform. The
# snapshot is `whep::polities` at **753 rows**, `data/polities.rda` at git blob
# **4f1fa9415736b7d8f4b42e26b8b8809a286e70e3**. The reference runtime is
# Windows 11 x64, R 4.5.2, sf 1.0-22, s2 1.1.9, terra 1.8-80 and GEOS 3.13.1.
# Sites repeat "polities 753 / 4f1fa941" and, where engine choice matters,
# "reference runtime" at the point of use.
#
# This is not bookkeeping. The figures these comments used to carry rotted
# precisely because they read as facts about WHEP while being facts about a
# snapshot: nothing named the vintage, so nothing could detect the drift, and a
# superseded `GRC-1830-1913` stayed the worked example, and a hardcoded
# fixture, until a CI failure on an unrelated platform surfaced it. A stamped
# figure fails loudly and locally at the next refresh; an unstamped one rots
# again in silence. A runtime stamp prevents a valid macOS result being
# misreported as snapshot drift.
#
# whep#734 refreshed the snapshot again (archipelago geometry, plus Aruba and
# the Holy See). Its 753-row / 4f1fa941 census was re-measured here rather than
# adjusted; do not carry these figures onto another refresh.
# `inst/scripts/verify_polycell_support.R` carries the same census as a pin and
# aborts when it moves; re-measure with that and update both together.

# Normalise the geometry source: keep the columns the producer reads, coerce to
# WGS84 and drop dead rows -- and, unless the caller asks for the overlap layer,
# aggregate rows too -- NA-explicitly. `%in%` is FALSE for NA, so
# `!(x %in% dead)` KEEPS an NA row, unlike `dplyr::filter(x != dead)`, which
# silently drops it. Exclusion needs positive evidence, and that cuts both ways:
# a row whose `polity_type` is NA is not evidence of an aggregate, so it stays
# in the PARTITION under either setting rather than being swept into a layer
# whose whole contract is that it double-counts.
.pcs_prepare_polities <- function(
  geometries,
  aggregates = c("exclude", "overlap_layer")
) {
  aggregates <- rlang::arg_match(aggregates)
  if (!inherits(geometries, "sf")) {
    cli::cli_abort("{.arg geometries} must be an {.cls sf} table.")
  }
  .pcs_require_cols(
    geometries,
    c("polity_code", "start_year", "end_year"),
    "geometries"
  )
  attrs <- sf::st_drop_geometry(geometries)
  usable <- .pcs_usable_geometry(sf::st_geometry(geometries))
  is_aggregate <- .pcs_col(attrs, "polity_type", NA_character_) %in% "aggregate"
  out <- sf::st_sf(
    polity_code = as.character(attrs$polity_code),
    start_year = as.integer(attrs$start_year),
    end_year = as.integer(attrs$end_year),
    polygon_status = .pcs_col(attrs, "polygon_status", NA_character_),
    area_code = .pcs_area_code(attrs),
    coverage_status = .pcs_coverage_status(usable$coverage_status, attrs),
    support_role = dplyr::if_else(is_aggregate, "overlap", "partition"),
    geometry = usable$geom
  )
  # `.polity_is_live()` is the package's one reading of which rows are dead, so
  # the producer's filter and `.active_polities()`'s tie-break cannot drift.
  live <- .polity_is_live(.pcs_col(attrs, "wiki_status", NA_character_)) &
    (identical(aggregates, "overlap_layer") | !is_aggregate)
  out[live, ]
}

# How usable each polity polygon is, recorded on every polycell it produces and
# in the "coverage" diagnostic, so a missing or unusable geometry is never a
# silent zero area. Over the 694 live non-aggregate rows of polities 753 /
# 4f1fa941 under the reference runtime: 666 `has_geometry` and 28
# `no_geometry`; none require input-level s2 repair and none remain s2-invalid.
# The 666 readable polities get clipped, while the other 28 receive no
# polycell at all.
.pcs_usable_geometry <- function(geom) {
  empty <- sf::st_is_empty(geom)
  fixed <- .s2_repair(.pcs_geom_4326(geom))
  status <- dplyr::case_when(
    empty ~ "no_geometry",
    fixed$status == "repaired" ~ "s2_repaired",
    fixed$status == "invalid" ~ "s2_invalid",
    .default = "has_geometry"
  )
  list(geom = fixed$geom, coverage_status = status)
}

# A polity whose validity interval is empty or NA-bounded matches no year, so
# the interval algebra drops every one of its polycells and the polity vanishes
# whole -- the same failure mode as an unusable polygon, and just as invisible.
# The shipped 603-row table has none, but #485 refreshes it to 740 rows with
# overlapping periods, so this is the class of defect that arrives with the
# very switch the injectable geometry argument exists for.
.pcs_coverage_status <- function(status, attrs) {
  start <- as.integer(attrs$start_year)
  end <- as.integer(attrs$end_year)
  dplyr::if_else(
    is.na(start) | is.na(end) | end <= start,
    "invalid_interval",
    status
  )
}

# `area_code` is a label, resolved from the periodized crosswalk rather than
# invented. It stays NA where the crosswalk has no entry for the polity.
.pcs_area_code <- function(attrs) {
  if (rlang::has_name(attrs, "area_code")) {
    return(as.integer(attrs$area_code))
  }
  lookup <- whep::polity_area_crosswalk |>
    dplyr::distinct(.data$polity_code, .data$polity_area_code) |>
    dplyr::filter(!is.na(.data$polity_area_code)) |>
    dplyr::distinct(.data$polity_code, .keep_all = TRUE)
  as.integer(
    lookup$polity_area_code[match(attrs$polity_code, lookup$polity_code)]
  )
}

.pcs_col <- function(df, nm, default) {
  if (rlang::has_name(df, nm)) df[[nm]] else rep(default, nrow(df))
}

.pcs_geom_4326 <- function(geom) {
  crs <- sf::st_crs(geom)
  if (is.na(crs)) {
    return(sf::st_set_crs(geom, 4326))
  }
  if (crs == sf::st_crs(4326)) {
    return(geom)
  }
  sf::st_transform(geom, 4326)
}

# -- The intersection ---------------------------------------------------------

# One geodesic intersection per polity interval, not per polity-year: no area
# depends on the year inside an interval, so a per-year loop would repeat
# identical work and emit identical rows.
.pcs_intersect_grid <- function(polities) {
  rows <- which(
    polities$coverage_status %in% c("has_geometry", "s2_repaired")
  )
  .pcs_warn_unusable(polities, rows)
  parts <- rows |>
    purrr::map(\(i) .pcs_polity_cells(polities[i, ])) |>
    purrr::compact()
  if (length(parts) == 0L) {
    return(NULL)
  }
  out <- do.call(rbind, parts)
  .pcs_warn_terra(out, "polity clip")
  out
}

# A polity that carries no usable polygon, or no usable validity interval,
# receives no polycell at all -- the one failure mode that otherwise looks
# exactly like a polity with no territory. It is named here and listed in the
# "coverage" diagnostic.
.pcs_warn_unusable <- function(polities, rows) {
  # `x[-integer(0)]` returns nothing rather than everything, so the complement
  # is taken explicitly: otherwise a run where EVERY polity is unusable would
  # be the one run that warns about none of them.
  skipped <- setdiff(seq_len(nrow(polities)), rows)
  if (length(skipped) == 0L) {
    return(invisible(NULL))
  }
  dropped <- polities$polity_code[skipped]
  reasons <- sort(unique(polities$coverage_status[skipped]))
  cli::cli_warn(c(
    "{length(dropped)} live polit{?y/ies} receive{?s/} no polycell.",
    i = "Reasons: {.val {reasons}}.",
    i = "See the {.val coverage} attribute; codes: {.val {dropped}}."
  ))
}

.pcs_polity_cells <- function(polity_row) {
  geom <- sf::st_geometry(polity_row)
  cells <- .pcs_cells_sf(.pcs_candidate_cells(geom))
  cells <- cells[lengths(sf::st_intersects(cells, geom)) > 0L, ]
  if (nrow(cells) == 0L) {
    return(NULL)
  }
  sf::st_agr(cells) <- "constant"
  inter <- .pcs_intersect_polygonal(cells, geom)
  if (nrow(inter) == 0L) {
    return(NULL)
  }
  inter <- .pcs_measure_pieces(inter)
  inter <- inter[inter$polity_area_ha > .pcs_area_floor_ha(), ]
  if (nrow(inter) == 0L) {
    return(NULL)
  }
  .pcs_label_cells(inter, sf::st_drop_geometry(polity_row))
}

.pcs_label_cells <- function(inter, attrs) {
  inter$polity_code <- attrs$polity_code
  inter$start_year <- attrs$start_year
  inter$end_year <- attrs$end_year
  inter$area_code <- attrs$area_code
  inter$polygon_status <- attrs$polygon_status
  inter$coverage_status <- attrs$coverage_status
  # Defaulted rather than read directly: a caller reaching this helper with a
  # geometry table `.pcs_prepare_polities()` never touched has no role column,
  # and the partition is what such a row is.
  inter$support_role <- .pcs_col(attrs, "support_role", "partition")
  inter
}

# The spherical engine can emit a clipped piece it then refuses to read back.
# Of the 414,485 pieces measured on polities 753 / 4f1fa941 under the reference
# runtime, planar repair makes 160 readable and leaves 21 unreadable. None of
# RUS-2014-2025's 12,730 pieces needs terra measurement on this snapshot.
#
# A minority stay invalid even after that repair, and they are NOT assumed to
# be slivers. On polities 753 / 4f1fa941 under the reference runtime that is 21
# pieces holding 1,429,276.70 ha across 9 polities, including four pieces worth
# 227,311.58 ha in GRC-1881-1913. Dropping such pieces deleted real territory,
# broke S-A2 re-aggregation, and re-emerged as fake unclaimed land in the S-A11
# diagnostic. They are therefore kept and measured with
# `terra::expanse()`, which does not go through s2, exactly as the ice reader
# already does.
#
# A cleaner upstream snapshot does not remove the need for this. GEOS validity
# is not s2 validity, and an upstream gate written against shapely cannot see
# the difference, so a polygon can pass validation there and still be
# unreadable here -- which is why the fix belongs at the consumer, either as a
# repair before intersecting or as the engine substitution below. That the two
# validity notions disagree is an upstream finding; what is measured here is
# that 21 pieces survive this producer's own planar repair and still cannot be
# read back under the reference runtime. Other platforms may assign a different
# subset to s2 without changing the no-piece-dropped property.
#
# `area_engine` records which engine measured each row, because terra measures
# on the WGS84 ellipsoid and s2 on a sphere, and the two disagree by a SIGNED
# amount that depends on latitude. Under the convention `terra / s2 - 1`,
# measured on whole 0.5-degree cells under the reference runtime, it is
# **-0.447%** at the equator, crosses **zero at latitude 35.32**, and reaches
# **+0.888%** at latitude 84.75.
#
# The old wording here, "0.45% at the equator to 0.86% at latitude 84.75", read
# as a magnitude bound and was wrong twice over: the equatorial figure is
# NEGATIVE -- terra reads smaller there, not larger -- and anywhere near the
# crossing the disagreement is far below either end rather than between them.
#
# So do not size a tolerance off that global range. Where the substitution
# actually lands on the shipped table -- the 2,322 readable pieces of the nine
# terra-carrying polities, latitude 36.25 to 71.25 -- the offset is +0.0209% to
# +0.7562%, area-weighted +0.4070%; over GRC-1881-1913's 59 alone it is
# +0.0209% to +0.0988%, area-weighted +0.0685%, an order of magnitude under the
# global figure. A consumer must be able to see where the substitution happened
# rather than infer it.
.pcs_measure_pieces <- function(inter) {
  fixed <- .s2_repair(sf::st_geometry(inter))
  sf::st_geometry(inter) <- fixed$geom
  usable <- fixed$status != "invalid"
  inter$area_engine <- dplyr::if_else(usable, "s2", "terra")
  inter$polity_area_ha <- rep(NA_real_, nrow(inter))
  if (any(usable)) {
    inter$polity_area_ha[usable] <-
      as.numeric(sf::st_area(fixed$geom[usable])) / 1e4
  }
  if (any(!usable)) {
    inter$polity_area_ha[!usable] <- .pcs_terra_area_ha(fixed$geom[!usable])
  }
  inter
}

# Measured one feature at a time. Handing terra a whole sfc is unsafe here:
# clipping can return a GEOMETRYCOLLECTION, and `terra::vect()` then warns
# "not all geometries were transferred" and returns fewer features than it was
# given, so the shorter area vector would recycle against the rows it is
# assigned to and mis-align them. It is observed on this layer, not
# hypothetical: the warning fires on the pieces reaching here. A
# scalar per feature cannot mis-align, and the polygonal part is extracted
# first so a mixed-type piece contributes its area rather than nothing.
.pcs_terra_area_ha <- function(geom) {
  rlang::check_installed("terra")
  vapply(seq_along(geom), \(i) .pcs_terra_one_ha(geom[i]), numeric(1L))
}

.pcs_terra_one_ha <- function(geom) {
  vect <- .pcs_terra_vect(geom)
  if (is.null(vect)) {
    return(0)
  }
  sum(terra::expanse(vect, unit = "m")) / 1e4
}

# Type extraction only, run planar-side because the pieces that reach here are
# exactly the ones the spherical engine refuses to read. `st_collection_extract`
# errors rather than passing through when the geometry is already singular and
# polygonal, so that case returns untouched.
#
# For AREA this is belt and braces: `terra::vect()` performs the same
# extraction internally, and measured either way a two-polygon collection comes
# back at 69,978.2271 ha and a polygon-plus-line collection at 34,989.1136 ha,
# with a line-only geometry giving 0 on both paths. Skipping it is an
# equivalent mutant. What the helper buys is the explicit empty return, so
# `terra::expanse()` is never handed a geometry with nothing to measure.
.pcs_polygonal_part <- function(geom) {
  types <- as.character(sf::st_geometry_type(geom))
  polygonal <- types %in% c("POLYGON", "MULTIPOLYGON")
  if (all(polygonal)) {
    return(geom)
  }
  # A crop that only grazes a piece comes back as a line or a point, which
  # `st_collection_extract()` rejects outright rather than returning empty.
  if (!any(types %in% c("GEOMETRY", "GEOMETRYCOLLECTION"))) {
    return(geom[polygonal])
  }
  old <- sf::sf_use_s2()
  on.exit(suppressMessages(sf::sf_use_s2(old)), add = TRUE)
  suppressMessages(sf::sf_use_s2(FALSE))
  suppressWarnings(sf::st_collection_extract(geom, "POLYGON"))
}

# `st_intersection.sf()` restores attributes before it has discarded non-area
# output. If one source feature produces both a zero-length line and a polygon,
# that asks sf to attach two geometries to one row. Intersect the geometry
# columns instead, filter to polygonal pieces, then restore the attributes with
# the source-row index that the sfc method records in `idx`.
.pcs_intersect_polygonal <- function(x, y) {
  hit <- sf::st_intersection(sf::st_geometry(x), sf::st_geometry(y))
  if (length(hit) > 0L && !.pcs_has_intersection_index(hit)) {
    return(.pcs_intersect_by_source(x, y))
  }
  .pcs_restore_intersection_rows(x, hit)
}

.pcs_has_intersection_index <- function(hit) {
  idx <- attr(hit, "idx")
  is.matrix(idx) && ncol(idx) >= 1L && nrow(idx) == length(hit)
}

# A few sf/GEOS combinations return an sfc result without a usable `idx`.
# Recompute that exceptional case one source row at a time: this is slower than
# the vectorized path, but the source identity is then exact by construction.
.pcs_intersect_by_source <- function(x, y) {
  parts <- vector("list", nrow(x))
  for (i in seq_len(nrow(x))) {
    hit <- sf::st_intersection(sf::st_geometry(x)[i], sf::st_geometry(y))
    polygonal <- .pcs_polygonal_part(hit)
    if (length(polygonal) == 0L) {
      next
    }
    parts[[i]] <- sf::st_sf(
      sf::st_drop_geometry(x)[rep.int(i, length(polygonal)), , drop = FALSE],
      geometry = polygonal
    )
  }
  parts <- purrr::compact(parts)
  if (length(parts) == 0L) {
    return(x[0, ])
  }
  do.call(rbind, parts)
}

.pcs_restore_intersection_rows <- function(x, hit) {
  idx <- attr(hit, "idx")
  if (length(hit) == 0L) {
    return(x[0, ])
  }
  # Some sf/GEOS builds omit `idx` for a one-row x one-row intersection. That
  # mapping is still exact: every returned component came from source row 1.
  # Never make the same guess when several source rows are possible.
  if (is.null(idx) && nrow(x) == 1L) {
    idx <- matrix(1L, nrow = length(hit), ncol = 1L)
    attr(hit, "idx") <- idx
  }
  if (!.pcs_has_intersection_index(hit)) {
    cli::cli_abort(
      "The sfc intersection did not retain its source-row mapping."
    )
  }

  types <- as.character(sf::st_geometry_type(hit))
  polygonal <- types %in% c("POLYGON", "MULTIPOLYGON")
  if (all(polygonal)) {
    return(sf::st_sf(
      sf::st_drop_geometry(x)[idx[, 1L], , drop = FALSE],
      geometry = hit
    ))
  }

  # Keep ordinary polygon results vectorized: they are virtually the entire
  # whole-table clip. A GEOMETRYCOLLECTION can split into several polygons, so
  # normalize only those results one at a time and repeat their source index.
  parts <- list()
  source_rows <- integer()
  result_rows <- integer()
  if (any(polygonal)) {
    direct <- which(polygonal)
    parts[[1L]] <- hit[direct]
    source_rows <- idx[direct, 1L]
    result_rows <- direct
  }
  collections <- which(types %in% c("GEOMETRY", "GEOMETRYCOLLECTION"))
  for (i in collections) {
    part <- .pcs_polygonal_part(hit[i])
    if (length(part) == 0L) {
      next
    }
    parts[[length(parts) + 1L]] <- part
    source_rows <- c(source_rows, rep.int(idx[i, 1L], length(part)))
    result_rows <- c(result_rows, rep.int(i, length(part)))
  }
  if (length(parts) == 0L) {
    return(x[0, ])
  }

  geometry <- do.call(c, parts)
  order <- order(result_rows, seq_along(result_rows))
  sf::st_sf(
    sf::st_drop_geometry(x)[source_rows[order], , drop = FALSE],
    geometry = geometry[order]
  )
}

# Two polygons that only touch intersect in a line, whose area is zero. The
# floor is 0.01 m2, far below any real polycell and far above float noise.
.pcs_area_floor_ha <- function() {
  1e-6
}

# Candidate cells covering the polity. Cells that merely touch it are
# enumerated here and removed by the area floor above.
.pcs_candidate_cells <- function(geom) {
  bounds <- .pcs_cell_window(geom)
  tidyr::expand_grid(
    klon = .pcs_k(bounds[["xmin"]], 720L):.pcs_k(bounds[["xmax"]], 720L),
    klat = .pcs_k(bounds[["ymin"]], 360L):.pcs_k(bounds[["ymax"]], 360L)
  )
}

# The window is the COORDINATE bounding box unioned with the SPHERICAL one.
# The two differ: s2 draws an edge between two vertices as a great circle,
# which on a long east-west border bows outside the box the coordinates span,
# so a coordinate-only window omits the cells that stretch of border reaches
# and the polity silently loses their area. It is not a rounding effect --
# SWA-1884-1912 lost 1.95e-04 of itself and KEN-1888-1891 6.29e-05, and the
# missing cells are whole pieces (SWA 78 enumerated against 82 real). Unioning
# the two is exact by construction rather than tuned, so it stays correct when
# the geometry source is refreshed.
.pcs_cell_window <- function(geom) {
  box <- sf::st_bbox(geom)
  spherical <- .pcs_s2_window(geom)
  c(
    xmin = min(box[["xmin"]], spherical[["xmin"]]),
    xmax = max(box[["xmax"]], spherical[["xmax"]]),
    ymin = min(box[["ymin"]], spherical[["ymin"]]),
    ymax = max(box[["ymax"]], spherical[["ymax"]])
  )
}

# `s2::s2_bounds_rect()` reports the spherical extent. It returns a longitude
# interval that WRAPS (lo > hi) for a polygon crossing the antimeridian, and a
# wrapped interval cannot be used as a `min`/`max` pair, so longitude falls
# back to the coordinate box there -- which already spans the globe for such a
# polygon anyway. The LATITUDE bounds stay usable when longitude wraps, and are
# kept. On the shipped table this changes nothing -- discarding them moves the
# window by 2.8e-14 degrees, which floors to the same cell index, so no
# candidate cell differs for any of the 580 polities. It is kept because a
# refreshed geometry source (#382, #485) need not be so forgiving, and because
# dropping a bound on one axis is the same omission the longitude bound exists
# to prevent. An unreadable geometry cannot be bounded spherically at all and
# falls back on both axes rather than aborting the build.
.pcs_s2_window <- function(geom) {
  box <- sf::st_bbox(geom)
  fallback <- c(
    xmin = box[["xmin"]],
    xmax = box[["xmax"]],
    ymin = box[["ymin"]],
    ymax = box[["ymax"]]
  )
  if (!rlang::is_installed("s2")) {
    return(fallback)
  }
  rect <- try(s2::s2_bounds_rect(geom), silent = TRUE)
  if (inherits(rect, "try-error")) {
    return(fallback)
  }
  wraps <- rect$lng_lo > rect$lng_hi
  c(
    xmin = if (wraps) box[["xmin"]] else rect$lng_lo,
    xmax = if (wraps) box[["xmax"]] else rect$lng_hi,
    ymin = rect$lat_lo,
    ymax = rect$lat_hi
  )
}

# Integer cell index; the cell centre is `k * 0.5 + 0.25`, which sits on the
# canonical `(coord + 180) %% 0.5 == 0.25` half-degree convention. The index is
# clamped so a bounding box touching a pole or the antimeridian cannot ask for
# a cell off the grid.
.pcs_k <- function(x, n) {
  min(max(as.integer(floor(x / 0.5)), -n / 2L), n / 2L - 1L)
}

# The cell rectangles are NOT densified along parallels, and they do not need
# to be. Under s2 a lon/lat rectangle's east-west edge is a great circle rather
# than a parallel, which for a while looked like a clipping bias worth removing
# by densifying. It is not: the whole shortfall came from `.pcs_candidate_cells`
# enumerating cells from the polygon's coordinate box, so the cells a bulging
# edge reaches into were never offered to the intersection at all. With the
# window unioned against the spherical extent, SWA-1884-1912 reproduces its own
# area to -1.4e-14 and KEN-1888-1891 to -4.6e-15, against -1.95e-04 and
# -6.29e-05 before.
#
# Densifying is also the wrong tool: `sf::st_segmentize()` on longlat is
# geodesic, so it adds vertices along the existing great circle and is
# area-preserving -- it cannot turn a great circle into a parallel -- while it
# destroys polygons crossing the antimeridian. Raw edges are additionally the
# convention the polity polygons themselves are stored in, so the engine
# matches the data, and the whole-cell area agrees with the parallel-bounded
# formula LUH2's `carea` uses to 1.2e-7.
.pcs_cells_sf <- function(idx) {
  lon <- idx$klon * 0.5 + 0.25
  lat <- idx$klat * 0.5 + 0.25
  sf::st_sf(
    cell_id = .pcs_cell_id(idx$klon, idx$klat),
    lon = lon,
    lat = lat,
    cell_area_ha = .cell_area_ha_lat(lat),
    geometry = sf::st_sfc(purrr::map2(lon, lat, .pcs_cell_poly), crs = 4326)
  )
}

.pcs_cell_id <- function(klon, klat) {
  (klon + 360L) * 1000L + (klat + 180L)
}

.pcs_cell_poly <- function(lon, lat) {
  sf::st_polygon(list(cbind(
    c(lon - 0.25, lon + 0.25, lon + 0.25, lon - 0.25, lon - 0.25),
    c(lat - 0.25, lat - 0.25, lat + 0.25, lat + 0.25, lat - 0.25)
  )))
}

# -- Ice ----------------------------------------------------------------------

# The layer is unioned so overlapping ice features cannot be counted twice. The
# union is repaired before use for the same reason the clipped pieces are: s2
# emits a duplicate vertex in its own output and then refuses to read it back.
# This is measured, not defensive. On ne_10m_glaciated_areas all 1,885 kept
# features pass `st_is_valid()` and their union does not, so every later
# predicate against that union would abort. The smallest reproducing subset is
# two Greenland features carrying 22,544 vertices, and simplifying either to an
# inlineable size collapses it, so the regression test needs the real layer and
# skips without it.
.pcs_prepare_ice <- function(ice) {
  if (is.null(ice)) {
    return(NULL)
  }
  geom <- .pcs_geom_4326(sf::st_geometry(ice))
  if (length(geom) == 0L) {
    return(NULL)
  }
  fixed <- .s2_repair(sf::st_union(geom))
  if (any(fixed$status == "invalid")) {
    cli::cli_abort("The {.arg ice} layer does not union into a usable polygon.")
  }
  fixed$geom
}

# Subtract ice per polycell by exact geodesic intersection, then drop geometry:
# ice depends on the polygon pair alone, never on the year, so it is resolved
# before the intervals are split.
.pcs_add_ice <- function(polycells_sf, ice_union) {
  if (is.null(polycells_sf)) {
    return(.pcs_empty_pieces())
  }
  ice_area_ha <- rep(0, nrow(polycells_sf))
  if (!is.null(ice_union)) {
    ice_area_ha <- .pcs_ice_areas(polycells_sf, ice_union)
  }
  out <- tibble::as_tibble(sf::st_drop_geometry(polycells_sf))
  out$ice_area_ha <- ice_area_ha
  out
}

# The polycell geometry column deliberately still holds the pieces the
# spherical engine cannot read, so their area is recoverable. That means no s2
# operation may be run across the whole column: `sf::st_intersects()` aborts on
# the first such piece, and with the shipped table and the real ice layer that
# killed the production call outright at any year, because `years` is applied
# only after every polity has been clipped. The hazard is live rather than
# historical: on polities 753 / 4f1fa941 under the reference runtime,
# GRC-1881-1913 clips to 63 pieces of which 4 are unreadable, and it is one of
# 9 polities carrying such pieces.
# (The example used to be GRC-1830-1913, which that vintage marks `superseded`,
# so DA-7's live filter now drops it before the clip ever sees it.) The two
# populations are therefore separated before either engine is asked to do
# anything: s2 clips the pieces it can read, terra clips the rest.
.pcs_ice_areas <- function(polycells_sf, ice_union) {
  out <- rep(0, nrow(polycells_sf))
  on_s2 <- polycells_sf$area_engine %in% "s2"
  if (any(on_s2)) {
    out[on_s2] <- .pcs_ice_areas_s2(polycells_sf[on_s2, ], ice_union)
  }
  if (any(!on_s2)) {
    out[!on_s2] <- .pcs_ice_areas_terra(polycells_sf[!on_s2, ], ice_union)
  }
  out
}

.pcs_ice_areas_s2 <- function(polycells_sf, ice_union) {
  out <- rep(0, nrow(polycells_sf))
  sub <- polycells_sf["cell_id"]
  sub$piece <- seq_len(nrow(sub))
  # The indexed predicate is orders of magnitude cheaper than the clip, and ice
  # touches a small minority of polycells, so only those are clipped.
  sub <- sub[lengths(sf::st_intersects(sub, ice_union)) > 0L, ]
  if (nrow(sub) == 0L) {
    return(out)
  }
  sf::st_agr(sub) <- "constant"
  hit <- .pcs_intersect_polygonal(sub, ice_union)
  if (nrow(hit) == 0L) {
    return(out)
  }
  hit <- .pcs_measure_pieces(hit)
  .pcs_warn_terra(hit, "ice clip")
  areas <- tapply(hit$polity_area_ha, hit$piece, sum)
  out[as.integer(names(areas))] <- as.numeric(areas)
  out
}

# Ice on a polycell the spherical engine cannot read. Clipping in terra keeps
# the subtraction honest: skipping it would leave ice inside `land_area_ha`,
# which is the same silent inflation the drop used to cause, only inverted.
.pcs_ice_areas_terra <- function(polycells_sf, ice_union) {
  rlang::check_installed("terra")
  vapply(
    seq_len(nrow(polycells_sf)),
    \(i) .pcs_ice_one_terra(sf::st_geometry(polycells_sf)[i], ice_union),
    numeric(1L)
  )
}

# The ice layer is one worldwide union carrying millions of vertices, and
# handing it to terra whole costs tens of gigabytes -- measured: the resident
# set passed 22 GB before this crop was added, on a job whose s2 path peaks
# near 1.2 GB. A polycell spans at most half a degree, so the ice is cropped to
# its bounding box first and only that crop is converted. The crop runs
# planar-side because these are precisely the pieces the spherical engine will
# not read.
.pcs_ice_one_terra <- function(geom, ice_union) {
  piece <- .pcs_terra_vect(geom)
  if (is.null(piece)) {
    return(0)
  }
  local_ice <- .pcs_terra_vect(.pcs_crop_planar(ice_union, sf::st_bbox(geom)))
  if (is.null(local_ice)) {
    return(0)
  }
  hit <- suppressWarnings(terra::intersect(piece, local_ice))
  if (nrow(hit) == 0L) {
    return(0)
  }
  sum(terra::expanse(hit, unit = "m")) / 1e4
}

.pcs_crop_planar <- function(geom, box) {
  old <- sf::sf_use_s2()
  on.exit(suppressMessages(sf::sf_use_s2(old)), add = TRUE)
  suppressMessages(sf::sf_use_s2(FALSE))
  suppressWarnings(sf::st_crop(geom, box))
}

.pcs_terra_vect <- function(geom) {
  polys <- .pcs_polygonal_part(geom)
  if (length(polys) == 0L || all(sf::st_is_empty(polys))) {
    return(NULL)
  }
  suppressWarnings(terra::vect(sf::st_sf(geometry = polys)))
}

# A substituted area engine is a fact about the numbers, so it is announced
# once with its magnitude rather than left for a reader to discover. The count
# leads the noun so `cli` can pluralise it.
.pcs_warn_terra <- function(pieces, what) {
  n <- sum(pieces$area_engine == "terra")
  if (n == 0L) {
    return(invisible(NULL))
  }
  ha <- sum(pieces$polity_area_ha[pieces$area_engine == "terra"])
  cli::cli_warn(c(
    "The {what} left {n} piece{?s} the spherical engine could not measure.",
    i = "Measured with {.fn terra::expanse} instead: {round(ha / 1e6, 4)} Mha.
         Rows carry {.code area_engine == \"terra\"}."
  ))
}

# -- Interval algebra ---------------------------------------------------------

# The set of polities sharing a cell changes over time, and the inland-water
# apportionment depends on that set, so an interval is only constant-area if it
# is split wherever a co-occupant appears or leaves. Splitting on the cell's own
# breakpoints makes every emitted interval atomic, which is what lets the grain
# be interval-keyed instead of per-year.
.pcs_split_intervals <- function(pieces) {
  if (nrow(pieces) == 0L) {
    return(pieces)
  }
  keys <- c("cell_id", "polity_code", "start_year", "end_year")
  .pcs_abort_repeated_key(pieces, keys)
  .pcs_abort_interval_overlap(pieces)
  pieces |>
    dplyr::inner_join(
      .pcs_breakpoints(pieces),
      by = "cell_id",
      relationship = "many-to-many"
    ) |>
    dplyr::filter(
      .data$breakpoint >= .data$start_year,
      .data$breakpoint < .data$end_year
    ) |>
    dplyr::arrange(.data$cell_id, .data$polity_code, .data$breakpoint) |>
    dplyr::mutate(
      next_break = dplyr::lead(.data$breakpoint),
      .by = dplyr::all_of(keys)
    ) |>
    dplyr::mutate(
      start_year = .data$breakpoint,
      end_year = dplyr::coalesce(.data$next_break, .data$end_year)
    ) |>
    dplyr::select(-"breakpoint", -"next_break")
}

# `dplyr::lead()` above reads whatever row follows in the sorted frame, which is
# the next BREAKPOINT only while the key is unique. Two rows sharing it
# interleave, so every second row comes back with `end_year == start_year` -- an
# empty interval that resolves to no year at all and takes its territory with
# it. Measured on a two-piece fixture: 70 of a polycell's 100 ha resolve to
# nothing at every year of its life, with no error and no warning, because each
# surviving row is individually well formed and S-A1 additivity, the water
# apportionment and the LUH2 reconciliation all still pass.
#
# Two paths can put a repeated key here. `.pcs_restore_intersection_rows()`
# repeats its source row once per polygonal component of a GEOMETRYCOLLECTION,
# and `.pcs_intersect_by_source()` does the same on the sf/GEOS builds that
# return no usable `idx`; a geometry table carrying one polity interval twice
# reaches it from the other end.
#
# This ABORTS rather than summing the duplicates. Summing is what
# `.pcs_ice_areas_s2()` does with its own repeated piece index, and it is right
# there because that repetition is this producer's own bookkeeping. Here a
# repeated key means the geometry table is not one row per polity interval, so
# a silent repair would fix the arithmetic and leave the fan-out invisible.
.pcs_abort_repeated_key <- function(pieces, keys) {
  repeated <- pieces |>
    dplyr::count(dplyr::pick(dplyr::all_of(keys)), name = "rows") |>
    dplyr::filter(.data$rows > 1L)
  if (nrow(repeated) == 0L) {
    return(invisible(NULL))
  }
  shown <- utils::head(repeated, 3L)
  labels <- stringr::str_glue(
    "cell {shown$cell_id} / {shown$polity_code} ",
    "[{shown$start_year}, {shown$end_year}) x{shown$rows}"
  )
  cli::cli_abort(
    c(
      "{nrow(repeated)} polycell key{?s} {?is/are} repeated in the clipped
       pieces.",
      x = "Showing up to three: {.val {labels}}.",
      i = "{.fn build_polycell_support} needs one row per cell, polity and
           validity interval. The interval split reads the next breakpoint
           with {.fn dplyr::lead}, so a repeated key makes every second row an
           empty interval that resolves to no year and drops its territory
           without a warning."
    ),
    class = "whep_pcs_repeated_key"
  )
}

# A repeated key is only the subset of overlapping validity that a comparison
# of whole keys happens to see: two intervals of one polity in one cell that
# are not identical but do overlap, `[2000, 2015)` against `[2010, 2020)`, pass
# it because `end_year` is part of the key. Measured on the example geometry
# supplied twice at those two intervals, `build_polycell_support()` completed
# with no abort and emitted `[2010, 2015)` TWICE for every one of the six
# cells, double counting that polity's territory over the shared years. The
# failure mode the guard exists for is overlapping validity, so this checks for
# that directly.
#
# `end_year` is EXCLUSIVE at a succession, so touching intervals -- `[2000,
# 2010)` then `[2010, 2020)`, the ordinary shape of two epochs of one polity in
# one cell -- are not an overlap and must still split. The comparison is
# therefore strict: only a `start_year` BELOW the previous `end_year` overlaps.
# The inclusive open end cannot make a false positive, because an interval that
# another interval of the same polity and cell starts on is a succession, not
# an open end.
#
# Sorting by `start_year` makes the consecutive comparison complete rather than
# merely cheap: if every interval starts at or after its predecessor's end then
# the ends chain, `end_k <= start_(k+1) <= start_j`, so no non-consecutive pair
# can overlap either.
#
# This ABORTS, like the repeated-key guard it extends. An overlap silently
# doubles a polity's area over the shared years while every row stays
# individually well formed, so the additivity and reaggregation checks
# downstream all still pass; and whep#461's overlap WARNING was ignored long
# enough for a bad artifact to be adopted, which is the evidence that a warning
# is not enough here.
.pcs_abort_interval_overlap <- function(pieces) {
  # No `distinct()` first: `.pcs_abort_repeated_key()` runs ahead of this and
  # aborts on a repeated key, so the keys here are already unique -- and a
  # direct caller that skips it gets the repeat reported as the overlap it also
  # is, rather than deduplicated away.
  overlaps <- pieces |>
    dplyr::select(
      "cell_id",
      "polity_code",
      "start_year",
      "end_year"
    ) |>
    dplyr::arrange(
      .data$cell_id,
      .data$polity_code,
      .data$start_year,
      .data$end_year
    ) |>
    dplyr::mutate(
      previous_start = dplyr::lag(.data$start_year),
      previous_end = dplyr::lag(.data$end_year),
      .by = c("cell_id", "polity_code")
    ) |>
    dplyr::filter(.data$start_year < .data$previous_end)
  if (nrow(overlaps) == 0L) {
    return(invisible(NULL))
  }
  shown <- utils::head(overlaps, 3L)
  labels <- stringr::str_glue(
    "cell {shown$cell_id} / {shown$polity_code} ",
    "[{shown$previous_start}, {shown$previous_end}) overlaps ",
    "[{shown$start_year}, {shown$end_year})"
  )
  cli::cli_abort(
    c(
      "{nrow(overlaps)} pair{?s} of polycell intervals overlap in the
       clipped pieces.",
      x = "Showing up to three: {.val {labels}}.",
      i = "{.fn build_polycell_support} needs the intervals of one polity in
           one cell to partition time. {.field end_year} is exclusive at a
           succession, so touching intervals are fine; an overlap emits the
           shared years twice and doubles that polity's territory over them."
    ),
    class = "whep_pcs_overlapping_interval"
  )
}

.pcs_breakpoints <- function(pieces) {
  pieces |>
    dplyr::distinct(.data$cell_id, .data$start_year, .data$end_year) |>
    tidyr::pivot_longer(
      c("start_year", "end_year"),
      values_to = "breakpoint"
    ) |>
    dplyr::distinct(.data$cell_id, .data$breakpoint)
}

# -- Year resolution ----------------------------------------------------------

# THE CONVENTION (DA-24) is stated once for the whole package, above
# `.open_ended_intervals()` in `R/constant_territory.R`, and resolved by
# `.covers_year()` there: `start_year` is inclusive, `end_year` is EXCLUSIVE at
# a succession and INCLUSIVE at the open end. This file calls that predicate
# instead of restating the rule -- the package carried three readings of
# `end_year` before C2, and a second reading here is the same defect.
#
# What is decided HERE is the two things the predicate needs from THIS table.
#
# THE DOMAIN. Only a row with a `polity_code` can be open. Until C9 the shim's
# `crosswalk_only` padding was the one thing that produced rows without one,
# and its synthetic `[crosswalk_year, crosswalk_year + 1)` window could not be
# allowed to sit at `max(end_year)` and move the domain end off the year the
# real intervals reach. The padding is gone, but the guard is kept and is not
# vacuous: `polity_code` is taken from the geometry source as supplied
# (`.pcs_prepare_polities()` coerces, it does not require non-NA), so an NA
# there must still be excluded from the succession rather than pasted into a
# key, where it would stringify and collapse every such row into one group.
#
# THE GROUP is the physical cell plus the polity FAMILY -- the two things that
# do NOT change when one epoch succeeds another. Getting it wrong is silent in
# both directions, because the rule opens the latest-starting member of a group
# and closes the rest:
#
#   TOO FINE and every terminal row is its own group maximum, so all of them
#   open. `polycell_id` and `polity_code` are both too fine -- DA-2 makes them
#   a function of the epoch, so an interval and its own successor land in
#   different groups -- and on a grain with no dedupe downstream that is the
#   boundary-year double count C2 removed.
#
#   TOO COARSE and one lineage closes another. `cell_id` alone merges every
#   polity in the cell. `area_code` is a label rather than an identity: it is
#   resolved from the periodized crosswalk, it is NA for 31 of the 220 live
#   intervals reaching the domain end -- and a pasted key stringifies NA, so
#   those collapse into one bucket rather than being skipped -- and
#   `SDN-2011-2025` and `SSD-2011-2025` are two lineages under one code, 206.
#
# `cell_id` belongs in the key because a family's successor need not occupy the
# same cells: keyed on the family alone, an interval starting later in some
# OTHER cell would close a still-open interval here and punch a hole in the
# terminal year of a cell where nothing succeeded anything. `polity_code` is
# required of the table and `.polity_family()` of it is exactly what
# `.active_polities()` groups on, so the package keeps one notion of
# succession. (`.filter_country_grid_year()` reaches the same key from the
# other end, cell + `area_code`: a country grid need not carry `polity_code`
# at all, so it uses the coarsest succession-stable key it is given.)
.pcs_open_intervals <- function(x) {
  open <- rep(FALSE, nrow(x))
  live <- which(!is.na(x$polity_code))
  if (length(live) == 0L) {
    return(open)
  }
  open[live] <- .open_ended_intervals(
    x$start_year[live],
    x$end_year[live],
    paste(x$cell_id[live], .polity_family(x$polity_code[live]), sep = "\r")
  )
  open
}

# TRUE where a row's interval covers `yr`. `open` is the open-end flag of the
# whole table; pass it whenever one table is resolved at more than one year, so
# the succession key is built once instead of once per year. `.covers_year()`
# only derives a group when it is not given the flag, so `NULL` is never read.
.pcs_covers_year <- function(x, yr, open = NULL) {
  .covers_year(
    x$start_year,
    x$end_year,
    NULL,
    yr,
    open_ended = open %||% .pcs_open_intervals(x)
  )
}

# -- Inland water -------------------------------------------------------------

# The layer gives water as a fraction of the WHOLE cell, and it is INLAND
# water: lakes and rivers lie on land by definition, never on ocean, so all of
# a cell's water belongs to its territory and is apportioned across that cell's
# polycells pro rata. Summing the polycells therefore recovers
# `water_frac * cell_area_ha` exactly. Where the water layer's own land mask
# disagrees with the polity polygons and the apportioned water would exceed a
# polycell's territory, it is capped and the excess is emitted, so
# `land_area_ha` can never go negative and the disagreement stays visible.
.pcs_add_water <- function(pieces, water) {
  if (is.null(water) || nrow(water) == 0L || nrow(pieces) == 0L) {
    pieces$inland_water_ha <- rep(0, nrow(pieces))
    pieces$water_excess_ha <- rep(0, nrow(pieces))
    return(pieces)
  }
  .pcs_require_cols(water, c("lon", "lat", "water_frac"), "water")
  .pcs_warn_water_footprint(pieces, water)
  pieces$support_role <- .pcs_col(pieces, "support_role", "partition")
  pieces |>
    dplyr::left_join(
      dplyr::distinct(water, .data$lon, .data$lat, .data$water_frac),
      by = c("lon", "lat")
    ) |>
    dplyr::mutate(
      water_pro_rata_ha = dplyr::coalesce(.data$water_frac, 0) *
        .data$cell_area_ha *
        .data$polity_area_ha /
        .pcs_water_denominator(.data$polity_area_ha, .data$support_role),
      .by = c("cell_id", "start_year")
    ) |>
    dplyr::mutate(
      # The headroom for water is the territory left after ice. Ice is itself
      # clipped to the polycell, so it cannot really exceed it, but on a fully
      # ice-covered polycell the two independent intersections differ in the
      # last bits and the headroom comes out at -1e-9 ha. Flooring the headroom
      # is what keeps `inland_water_ha` non-negative, which T-A3's contract
      # asserts and which 56 Greenland rows violated before this floor.
      inland_water_ha = pmin(
        .data$water_pro_rata_ha,
        pmax(.data$polity_area_ha - .data$ice_area_ha, 0)
      ),
      water_excess_ha = .data$water_pro_rata_ha - .data$inland_water_ha
    ) |>
    dplyr::select(-"water_frac", -"water_pro_rata_ha")
}

# THE DENOMINATOR IS THE PARTITION, not every row sharing the cell. All of a
# cell's inland water belongs to the polities that partition it, so the share
# each receives must not depend on whether an aggregate covering some of them
# is also in the table: adding the overlap layer to the denominator would
# silently move water off every member it covers and onto the aggregate. With
# the partition as the denominator, an aggregate's share is exactly the sum of
# the shares of the members it covers -- it is the same rate applied to the same
# territory -- and the members keep theirs unchanged. That is what makes the
# layer additive against the partition rather than a second, disagreeing
# measurement of the same cell.
#
# Where NO partition row reaches the cell the aggregate is all there is, so the
# fallback is the rows in hand. This is not the aggregate case only: a table
# with no `support_role` at all -- a direct caller of this helper -- lands here
# too, and gets the pre-whep#803 rule back exactly.
.pcs_water_denominator <- function(area, role = NULL) {
  if (is.null(role)) {
    return(sum(area))
  }
  partition <- sum(area[!(role %in% "overlap")])
  if (partition > 0) partition else sum(area)
}

# A water layer that joins to almost nothing is indistinguishable, downstream,
# from a world with almost no lakes: the join is a `left_join` and a missing row
# legitimately means "this cell is dry", so every unmatched hectare of water
# silently becomes land and no total moves in a direction anyone would question.
#
# This is not hypothetical. `terra::xyFromCell()` returns a centre of -130.25 as
# -130.24999999999994, which prints identically and compares FALSE; the layer
# then matched 36 of 720 longitudes and the whole build reported 0.00 Mha of
# inland water without a single warning. `.pcs_water_unmatched()` recorded it
# faithfully in an attribute nobody had to read.
#
# The threshold is deliberately loose. A genuine footprint disagreement between
# the CRU mask and the polity polygons is a few thousand cells out of tens of
# thousands; missing more than HALF the polycells means the grids do not share a
# convention, which is a different kind of fact and the only one worth stopping
# a build for.
.pcs_warn_water_footprint <- function(pieces, water) {
  cells <- dplyr::distinct(pieces, .data$lon, .data$lat)
  matched <- nrow(dplyr::semi_join(
    cells,
    dplyr::distinct(water, .data$lon, .data$lat),
    by = c("lon", "lat")
  ))
  if (matched > nrow(cells) / 2) {
    return(invisible(NULL))
  }
  cli::cli_warn(c(
    "The {.arg water} layer matches {matched} of {nrow(cells)} covered
     cell{?s}.",
    x = "Unmatched cells are booked as DRY, so their inland water becomes
         land silently.",
    i = "Check that {.field lon}/{.field lat} are on the same half-degree
         centres as the polycells: a float drift too small to print is enough
         to miss every one."
  ))
}

# The water layer carries the CRU land mask and the polycells carry the polity
# polygons, so the two footprints do not coincide. A polycell the layer has no
# row for is booked as having no inland water, which turns that water into
# land; a water cell no polycell reaches has its water dropped entirely.
# Neither is a rounding effect, and both are INTERVAL-GRAIN, for the same
# reason `unassigned` is: a cell covered in one epoch and not in another is
# unmatched only in the second, so a footprint taken over all intervals at once
# answers the wrong question. Measured on the shipped polities against GLWD:
# 6,658 covered cell-intervals have no water row, and the wet side carries
# 7,817 rows over 7,506 distinct cells. Sliced, that side reads 6,597 cells in
# 1800, 1,233 in 1900 and 477 in 2015 -- the territory grows, so the count
# falls -- and only 110 wet cells are unreached in EVERY interval, with 703
# further GLWD cells unreached throughout but carrying no water. Only WET cells
# are reported there, because a dry cell no polycell reaches loses nothing.
#
# Those slice figures are only correct because the gap rows partition the whole
# domain. While `.pcs_gaps_before()` was dead the wet side reported its 110
# never-reached cells and nothing else, so 2015 read 110 rather than 477.
# EA10 required this disagreement handled explicitly rather than absorbed.
.pcs_water_unmatched <- function(support, water) {
  covered <- support |>
    dplyr::distinct(
      .data$lon,
      .data$lat,
      .data$start_year,
      .data$end_year,
      .data$cell_area_ha
    )
  layer <- dplyr::distinct(water, .data$lon, .data$lat, .data$water_frac)
  wet <- dplyr::filter(layer, .data$water_frac > 0)
  dplyr::bind_rows(
    covered |>
      dplyr::anti_join(layer, by = c("lon", "lat")) |>
      dplyr::mutate(
        side = "polycell_without_water_cell",
        water_frac = NA_real_
      ),
    .pcs_interval_gaps(covered, wet, .pcs_domain(support)) |>
      dplyr::select(-"claimed_land_ha") |>
      dplyr::mutate(
        side = "water_cell_without_polycell",
        cell_area_ha = NA_real_
      )
  )
}

# -- Assembly -----------------------------------------------------------------

.pcs_finalize <- function(pieces, geometry_source, data) {
  pieces |>
    dplyr::mutate(
      polycell_id = paste0(.data$polity_code, "@", .data$cell_id),
      # Water is clamped to the territory left after ice and ice is clipped to
      # the polycell, so the difference is non-negative in exact arithmetic.
      # Recomputing an already-zero value in float64 still returns -1e-13 ha on
      # 8 of 564,304 real polycells, which is what this floor removes. It is
      # not a general safety net: the two clamps above are what bound the
      # terms, and each is floored where it is formed.
      land_area_ha = pmax(
        .data$polity_area_ha - .data$inland_water_ha - .data$ice_area_ha,
        0
      ),
      geometry_source = geometry_source,
      luh2_vintage = .pcs_luh2_vintage(data$luh2)
    ) |>
    .pcs_add_split_method() |>
    dplyr::select(
      dplyr::all_of(.pcs_output_cols()),
      dplyr::everything()
    )
}

# DA-6: the two placement rules are recorded, so a consumer can tell an exactly
# intersected area from an apportioned one. The pro-rata step is recorded
# wherever it actually ran, including in a single-polity cell where it runs
# trivially: the water still arrived as a whole-cell fraction, not as a
# polygon, and that is the fact the column exists to carry.
.pcs_add_split_method <- function(pieces) {
  dplyr::mutate(
    pieces,
    split_method = dplyr::if_else(
      .data$inland_water_ha > 0,
      "polygon_intersection+water_pro_rata",
      "polygon_intersection"
    )
  )
}

.pcs_luh2_vintage <- function(luh2) {
  as.character(attr(luh2, "luh2_vintage") %||% NA_character_)
}

.pcs_output_cols <- function() {
  c(
    "polycell_id",
    "cell_id",
    "lon",
    "lat",
    "polity_code",
    "area_code",
    "start_year",
    "end_year",
    "cell_area_ha",
    "polity_area_ha",
    "land_area_ha",
    "inland_water_ha",
    "ice_area_ha",
    "geometry_source",
    "polygon_status",
    "split_method",
    "coverage_status",
    "support_role",
    "area_engine",
    "luh2_vintage"
  )
}

# A table that no longer partitions its cells says so on the way out. The layer
# is opt-in, so this cannot fire on a caller that did not ask for it, and it is
# `cli_inform` rather than `cli_warn` because nothing has gone wrong -- what has
# happened is that the table now answers two different questions and a reader
# who sums it whole will get the wrong one.
.pcs_inform_overlap_layer <- function(support) {
  layer <- support$support_role %in% "overlap"
  if (!any(layer)) {
    return(invisible(NULL))
  }
  codes <- sort(unique(support$polity_code[layer]))
  cli::cli_inform(c(
    "i" = "{sum(layer)} polycell{?s} of {length(codes)} aggregate polit{?y/ies}
           ride{?s/} alongside the partition, marked
           {.code support_role == \"overlap\"}.",
    "*" = "They cover their members' territory, and the residual
           {.val Rest of World} covers the regional residuals, so this layer
           partitions nothing: take one polity's polycells from it, never a
           sum over it. Codes: {.val {codes}}."
  ))
}

# The rows that PARTITION each cell: everything the overlap layer did not put
# there. Read as a negation, so a table built before whep#803 -- no column at
# all -- and a row whose role is NA are both partition rows, which is what they
# are: the layer is opt-in, and nothing else in the table overlaps by design.
.pcs_partition <- function(support) {
  if (!rlang::has_name(support, "support_role")) {
    return(support)
  }
  support[!(support$support_role %in% "overlap"), , drop = FALSE]
}

# -- The DA-13 transitional shim, removed at C9 -------------------------------
#
# `.pcs_add_shim()` and `.pcs_append_crosswalk_only()` lived here. Together they
# made this table readable as `build_cell_polity()`'s: the first joined the
# deployed crosswalk's `polity_frac` onto the intervals covering the crosswalk's
# year, the second appended the crosswalk rows the intersection did not
# reproduce as `coverage_status == "crosswalk_only"` padding, and
# `polycell_shim_view()` projected the pair back out. That let each consumer
# migrate on its own commit while every unmigrated one stayed provably
# unchanged, which is the only reason the movement of C3a, C3b, C5, C7 and C8
# could be attributed to a commit each.
#
# All three are gone, and none should return. The padding is the reason
# `sum(land_area_ha)` over this table used to be `NA`, and the reason four
# diagnostics and the domain rule each carried their own `crosswalk_only`
# guard; `cell_area_ha` STAYS, because it is not shim -- it is the cell's own
# area, which `build_n_deposition()` divides the cell mass by to form
# `deposition_kgn_ha` and the carbon support carries through
# `.carbon_support_to_area_code()`.
#
# `data$crosswalk` and `data$crosswalk_year` are still read, by
# `.pcs_footprints()` and `.pcs_footprint_diff()` alone (DA-12). That is a
# reconciliation of three footprints, not a shim: it reports the disagreement
# instead of papering over it with padding rows.

# -- Diagnostics --------------------------------------------------------------

# Each diagnostic is attached only when its input is present or it has
# something to report. dplyr copies user attributes onto every derived frame,
# so an unconditional attribute would ride through a consumer's unrelated
# `arrange()` or `select()` and turn up in comparisons that have nothing to do
# with it.
.pcs_attach_diagnostics <- function(support, polities, data, water) {
  # THE PARTITION IS WHAT THESE DESCRIBE. A cell holding more territory than it
  # has, land the polities do not claim, the footprint against the crosswalks:
  # each is a statement about the layer that partitions the cell. Measured over
  # every row, the opt-in overlap layer would flood the first, cancel the second
  # against itself and inflate the third, so admitting an aggregate would look
  # like the polygons had gone wrong.
  partition <- .pcs_partition(support)
  coverage <- .pcs_coverage(polities)
  if (any(coverage$coverage_status != "has_geometry")) {
    attr(support, "coverage") <- coverage
  }
  overlap <- .pcs_overlap(partition)
  if (nrow(overlap) > 0L) {
    attr(support, "overlap") <- overlap
    .pcs_warn_overlap(overlap)
  }
  terra_measured <- .pcs_terra_measured(support)
  if (nrow(terra_measured) > 0L) {
    attr(support, "terra_measured") <- terra_measured
  }
  long_edges <- .pcs_long_edges(polities)
  if (nrow(long_edges) > 0L) {
    attr(support, "long_edges") <- long_edges
    .pcs_warn_long_edges(long_edges)
  }
  if (!is.null(water)) {
    attr(support, "water_unmatched") <- .pcs_water_unmatched(partition, water)
    # Every row, both roles: the clamp is a fact about the row it happened on,
    # and an aggregate whose apportioned water exceeds its own territory is
    # exactly the disagreement this attribute exists to surface.
    attr(support, "water_excess") <- .pcs_water_excess(support)
  }
  if (!is.null(data$crosswalk) || !is.null(data$producer_crosswalk)) {
    attr(support, "footprints") <- .pcs_footprints(partition, data)
    attr(support, "footprint_diff") <- .pcs_footprint_diff(partition, data)
  }
  if (!is.null(data$luh2)) {
    attr(support, "unassigned") <- .pcs_unassigned(partition, data$luh2)
  }
  support
}

# Two live polities can be handed the SAME polygon by the geometry source, and
# then their polycells both claim the whole of a cell: on the shipped table
# GNQ-1968-2025 and STP-1800-2025 each take all of cell (10.25, 1.75) in 2015,
# and colonial-era IDN/IND/PAK share one 1800 polygon. That is a defect in the
# polygons, not in the intersection, and deciding who owns the ground is a
# territorial judgement this producer must not make. It is emitted instead, so
# the double count is visible where it lands rather than buried in a total.
.pcs_overlap <- function(support) {
  support |>
    dplyr::summarise(
      territory_ha = sum(.data$polity_area_ha),
      polities = dplyr::n(),
      .by = c(
        "cell_id",
        "lon",
        "lat",
        "start_year",
        "end_year",
        "cell_area_ha"
      )
    ) |>
    dplyr::filter(
      .data$territory_ha > .data$cell_area_ha * (1 + .pcs_cell_tolerance())
    ) |>
    dplyr::mutate(excess_ha = .data$territory_ha - .data$cell_area_ha)
}

# DA-22 (issue #529). A polygon that stores a border following a parallel as ONE
# segment between distant vertices hands s2 a great circle, and s2 renders it
# bulging poleward. Polities 749 / 9320e033 exposed 43 such edges across 30
# polities, led by the 49th and 22nd parallels. WHEP PR #662 repaired those
# upstream geometries: polities 753 / 4f1fa941 has zero edges above this
# detector's threshold. The zero census is pinned in the regression test, while
# synthetic long, short and sloping edges keep the detector itself exercised.
#
# ATTRIBUTION CORRECTED. This comment used to blame `cshapes-2.0` for storing
# that edge sparsely. Upstream reports that CShapes 2.0 in fact carries 124
# vertices along the parallel with a widest gap of 1.95 degrees, and that the
# old single long segment was manufactured by whep-polities' own
# `SimplifyPreserveTopology(0.01)`: Douglas-Peucker measures deviation from the
# chord in planar degrees, and every vertex sitting ON a parallel lies exactly
# on that chord, so the whole run was deleted. What CShapes itself contains is
# taken from the upstream report. The ~123,276 km2 of Canadian prairie DA-22
# measured as displaced to the USA is that task's historical figure and is NOT
# re-measured here.
#
# Nothing here changes an area. Any segment the detector reports IS what the
# input says the border is, and re-drawing it would be a territorial judgement
# the producer must not make. The diagnostic makes such borders visible: both
# polities are clipped against the same curve, so their shares can still sum to
# 1.0000 in every cell and no conservation check can see the transfer.
#
# The criterion is the BULGE, not the span: what matters is how far the great
# circle departs from the parallel the vertices sit on, and that grows with the
# square of the span and with latitude. A 1.24-degree segment at latitude 45
# bulges 0.0017 degrees and is ordinary polygon detail; the 49th parallel's
# 27.6-degree segment bulges 0.83 degrees and moves a province. The default
# floor of 0.01 degrees is about 1.1 km of displacement.
#
# `min_span_deg` is therefore a PERFORMANCE prefilter, not a second criterion:
# the largest bulge any sub-degree span can produce is 0.0011 degrees, measured
# over latitudes 0 to 89.9, and reaching the 0.01 floor at latitude 45 needs a
# 3.03-degree span. Removing it changes no output, which a mutation sweep
# correctly reports as an equivalent mutant rather than a gap in the tests.
.pcs_long_edges <- function(
  polities,
  min_span_deg = 1,
  max_drift_deg = 0.01,
  min_bulge_deg = 0.01
) {
  geom <- sf::st_geometry(polities)
  attrs <- sf::st_drop_geometry(polities)
  found <- seq_along(geom) |>
    purrr::map(\(i) {
      .pcs_polity_long_edges(geom[i], attrs[i, ], min_span_deg, max_drift_deg)
    }) |>
    purrr::compact()
  if (length(found) == 0L) {
    return(.pcs_empty_long_edges())
  }
  dplyr::filter(purrr::list_rbind(found), .data$bulge_deg >= min_bulge_deg)
}

.pcs_empty_long_edges <- function() {
  tibble::tibble(
    polity_code = character(),
    start_year = integer(),
    end_year = integer(),
    lon_from = double(),
    lon_to = double(),
    lat = double(),
    span_deg = double(),
    bulge_deg = double()
  )
}

.pcs_polity_long_edges <- function(geom, attrs, min_span_deg, max_drift_deg) {
  if (sf::st_is_empty(geom)[[1L]]) {
    return(NULL)
  }
  edges <- .pcs_ring_edges(
    sf::st_coordinates(geom),
    min_span_deg,
    max_drift_deg
  )
  if (nrow(edges) == 0L) {
    return(NULL)
  }
  dplyr::mutate(
    edges,
    polity_code = attrs$polity_code,
    start_year = attrs$start_year,
    end_year = attrs$end_year,
    .before = 1L
  )
}

# `sf::st_coordinates()` labels rings with however many `L` columns the
# geometry type needs, so they are pasted rather than named individually.
.pcs_ring_edges <- function(xy, min_span_deg, max_drift_deg) {
  ring_cols <- grep("^L[0-9]+$", colnames(xy), value = TRUE)
  rings <- apply(xy[, ring_cols, drop = FALSE], 1L, paste, collapse = "-")
  split(seq_len(nrow(xy)), rings) |>
    purrr::keep(\(i) length(i) > 1L) |>
    purrr::map(\(i) {
      .pcs_edges_of_ring(xy[i, , drop = FALSE], min_span_deg, max_drift_deg)
    }) |>
    purrr::list_rbind()
}

.pcs_edges_of_ring <- function(m, min_span_deg, max_drift_deg) {
  span <- abs(diff(m[, "X"]))
  drift <- abs(diff(m[, "Y"]))
  # A span past 180 degrees is the antimeridian wrap, not a long edge.
  keep <- which(span > min_span_deg & span < 180 & drift < max_drift_deg)
  if (length(keep) == 0L) {
    return(NULL)
  }
  tibble::tibble(
    lon_from = unname(m[keep, "X"]),
    lon_to = unname(m[keep + 1L, "X"]),
    lat = unname(m[keep, "Y"]),
    span_deg = unname(span[keep]),
    bulge_deg = unname(.pcs_great_circle_bulge(m[keep, "Y"], span[keep]))
  )
}

# A great circle through two points at latitude `lat` separated by `span_deg`
# of longitude reaches `atan(tan(lat) / cos(span_deg / 2))` at its midpoint.
# The bulge is how far that is from the parallel the two vertices sit on.
.pcs_great_circle_bulge <- function(lat, span_deg) {
  rad <- pi / 180
  abs(atan(tan(lat * rad) / cos(span_deg * rad / 2)) / rad) - abs(lat)
}

.pcs_warn_long_edges <- function(long_edges) {
  worst <- max(long_edges$bulge_deg)
  cli::cli_warn(c(
    "{nrow(long_edges)} polity edge{?s} span{?s/} over a degree of longitude at
     near-constant latitude.",
    i = "s2 draws each as a great circle bulging up to
         {round(worst, 4)} degrees off the parallel its vertices sit on.
         No area is changed; see the {.val long_edges} attribute."
  ))
}

.pcs_warn_overlap <- function(overlap) {
  cli::cli_warn(c(
    "{nrow(overlap)} cell-interval{?s} hold{?s/} more territory than the cell.",
    i = "Overlapping polity polygons; see the {.val overlap} attribute.
         Excess: {round(sum(overlap$excess_ha) / 1e6, 2)} Mha."
  ))
}

# The whole-cell tolerance: s2 and the package's parallel-bounded cell formula
# agree to <= 9.5e-6 relative over latitudes 0-85, so 1e-4 accepts either
# spherical convention while still rejecting a genuine overlap.
.pcs_cell_tolerance <- function() {
  1e-4
}

.pcs_coverage <- function(polities) {
  polities |>
    sf::st_drop_geometry() |>
    tibble::as_tibble() |>
    dplyr::select(
      "polity_code",
      "start_year",
      "end_year",
      "polygon_status",
      "coverage_status",
      "support_role"
    )
}

# Every polycell whose area came from terra rather than s2, so the engine
# substitution is addressable per polity rather than only per row.
.pcs_terra_measured <- function(support) {
  support |>
    dplyr::filter(.data$area_engine %in% "terra") |>
    dplyr::select(
      "polycell_id",
      "cell_id",
      "lon",
      "lat",
      "polity_code",
      "start_year",
      "end_year",
      "polity_area_ha"
    )
}

.pcs_water_excess <- function(support) {
  cols <- c(
    "polycell_id",
    "cell_id",
    "polity_code",
    "start_year",
    "end_year",
    "polity_area_ha",
    "water_excess_ha"
  )
  if (!rlang::has_name(support, "water_excess_ha")) {
    support$water_excess_ha <- rep(0, nrow(support))
  }
  support |>
    dplyr::filter(.data$water_excess_ha > .pcs_area_floor_ha()) |>
    dplyr::select(dplyr::all_of(cols))
}

# Both footprints, side by side: the deployed crosswalk every published WHEP
# number was computed from, the crosswalk today's producer would rebuild, and
# the polycell intersection. Picking one silently would make the migration's
# movement and the restriction's movement inseparable.
.pcs_footprints <- function(support, data) {
  sources <- list(
    deployed_crosswalk = data$crosswalk,
    producer_crosswalk = data$producer_crosswalk,
    polycell = .pcs_polycell_footprint(support, data)
  )
  sources |>
    purrr::compact() |>
    purrr::imap(\(x, nm) .pcs_footprint_row(x, nm)) |>
    dplyr::bind_rows()
}

.pcs_footprint_row <- function(x, nm) {
  tibble::tibble(
    footprint = nm,
    rows = nrow(x),
    cells = nrow(dplyr::distinct(x, .data$lon, .data$lat)),
    area_codes = dplyr::n_distinct(x$area_code)
  )
}

# Both crosswalks are present-day products with no epochs, so the polycell
# footprint is taken at the same year. Comparing every historical interval
# against them would count a cell once per epoch and make the reconciliation
# meaningless: on the shipped table that is 129,047 rows against 68,527.
#
# Until C9 this also had to drop the shim's `crosswalk_only` padding, or the
# polycell footprint would have contained the very crosswalk rows it is
# measured against and agreed with itself by construction. The padding is gone,
# so only the year filter remains. `distinct()` below means a wrongly OPENED
# interval could not double-count here -- the risk this diagnostic runs is the
# other one, a wrongly CLOSED interval dropping a cell from the footprint and
# manufacturing the very disagreement against the two crosswalks that it exists
# to measure.
.pcs_polycell_footprint <- function(support, data) {
  yr <- as.integer(data$crosswalk_year %||% 2015L)
  support[which(.pcs_covers_year(support, yr)), , drop = FALSE] |>
    dplyr::distinct(.data$lon, .data$lat, .data$area_code)
}

.pcs_footprint_diff <- function(support, data) {
  members <- list(
    deployed_crosswalk = data$crosswalk,
    producer_crosswalk = data$producer_crosswalk,
    polycell = .pcs_polycell_footprint(support, data)
  ) |>
    purrr::compact() |>
    purrr::imap(\(x, nm) {
      x |>
        dplyr::distinct(.data$lon, .data$lat, .data$area_code) |>
        dplyr::mutate(footprint = nm, present = TRUE)
    }) |>
    dplyr::bind_rows()
  if (nrow(members) == 0L) {
    return(members)
  }
  members |>
    tidyr::pivot_wider(
      names_from = "footprint",
      values_from = "present",
      values_fill = FALSE
    ) |>
    .pcs_keep_disagreements()
}

.pcs_keep_disagreements <- function(wide) {
  flags <- setdiff(names(wide), c("lon", "lat", "area_code"))
  wide |>
    dplyr::filter(rowSums(dplyr::pick(dplyr::all_of(flags))) < length(flags))
}

# Land present in the validation layer but claimed by no live polity is
# emitted, never renormalised into the polities: an unexplained gap that could
# be either a geometry error or discarded unclaimed land is unattributable.
#
# The rows a cell contributes must cover the whole domain, not only the
# intervals in which somebody claimed it. Keying on the claimed intervals alone
# under-reports: a cell held 1900-1950 and unclaimed afterwards then has no row
# covering 2015, and its unassigned land disappears from that year's slice. On
# the shipped polities that halved the 2015 figure, 158 Mha against 315 Mha.
#
# BOTH sides of the disagreement are emitted. Reporting only the under-claim
# reconciles the over-claim away by construction, which is the silent
# reconciliation DA-5 forbids: at 2015 the polities claim 315.50 Mha less land
# than LUH2 in some cells and 103.03 Mha more in others, and a consumer told
# only the first cannot tell a coastline the polygons miss from one they
# overshoot.
.pcs_unassigned <- function(support, luh2) {
  .pcs_require_cols(luh2, c("lon", "lat", "terrestrial_ha"), "data$luh2")
  claimed <- support |>
    dplyr::summarise(
      claimed_land_ha = sum(.data$land_area_ha),
      .by = c("lon", "lat", "start_year", "end_year")
    )
  cells <- dplyr::distinct(luh2, .data$lon, .data$lat, .data$terrestrial_ha)
  .pcs_cover_domain(claimed, cells, .pcs_domain(support)) |>
    dplyr::mutate(
      unassigned_land_ha = pmax(
        .data$terrestrial_ha - .data$claimed_land_ha,
        0
      ),
      over_claimed_land_ha = pmax(
        .data$claimed_land_ha - .data$terrestrial_ha,
        0
      )
    ) |>
    dplyr::filter(
      .data$unassigned_land_ha > .pcs_area_floor_ha() |
        .data$over_claimed_land_ha > .pcs_area_floor_ha()
    ) |>
    tibble::as_tibble()
}

.pcs_domain <- function(support) {
  if (nrow(support) == 0L) {
    return(c(NA_integer_, NA_integer_))
  }
  c(min(support$start_year), max(support$end_year))
}

# Every stretch of the domain a cell's claimed intervals leave uncovered gets
# its own row carrying no claim at all, so a year resolves to exactly one row
# per cell whether or not anybody held it then.
.pcs_cover_domain <- function(claimed, cells, domain) {
  covered <- dplyr::inner_join(cells, claimed, by = c("lon", "lat"))
  gaps <- .pcs_interval_gaps(claimed, cells, domain)
  dplyr::bind_rows(covered, gaps)
}

.pcs_interval_gaps <- function(claimed, cells, domain) {
  dplyr::bind_rows(
    .pcs_gaps_before(claimed, domain),
    .pcs_gap_after(claimed, domain),
    .pcs_never_claimed(claimed, cells, domain)
  ) |>
    dplyr::filter(.data$start_year < .data$end_year) |>
    dplyr::mutate(claimed_land_ha = 0) |>
    dplyr::inner_join(cells, by = c("lon", "lat"), relationship = "many-to-one")
}

# The stretch before each claimed interval, back to the previous one.
#
# The gap bounds are built under NEW names and renamed afterwards. Assigning
# `start_year` and `end_year` directly inside one `transmute()` reads the
# rebound value, not the original: `end_year = start_year` resolved to the
# `start_year` assigned a line earlier, so every gap came out zero-length and
# was then dropped by the `start_year < end_year` filter. The whole helper was
# dead: re-running the broken version over the shipped build returns 0 rows
# where this one returns 7,340, and 6,487 of the 32,248 wet GLWD cells that
# some polycell reaches had no row at the domain start. Distinct names make
# that class of mistake impossible here.
.pcs_gaps_before <- function(claimed, domain) {
  claimed |>
    dplyr::arrange(.data$lon, .data$lat, .data$start_year) |>
    dplyr::mutate(
      previous_end = dplyr::lag(.data$end_year, default = domain[[1L]]),
      .by = c("lon", "lat")
    ) |>
    dplyr::transmute(
      .data$lon,
      .data$lat,
      gap_start = .data$previous_end,
      gap_end = .data$start_year
    ) |>
    dplyr::rename(start_year = "gap_start", end_year = "gap_end")
}

# The stretch after the last claimed interval, out to the end of the domain.
.pcs_gap_after <- function(claimed, domain) {
  claimed |>
    dplyr::summarise(start_year = max(.data$end_year), .by = c("lon", "lat")) |>
    dplyr::mutate(end_year = domain[[2L]])
}

.pcs_never_claimed <- function(claimed, cells, domain) {
  cells |>
    dplyr::anti_join(claimed, by = c("lon", "lat")) |>
    dplyr::transmute(
      .data$lon,
      .data$lat,
      start_year = domain[[1L]],
      end_year = domain[[2L]]
    )
}

# -- Shared helpers -----------------------------------------------------------

.pcs_expand <- function(support, years) {
  if (is.null(years)) {
    return(support)
  }
  out <- expand_polycell_years(support, years)
  .pcs_copy_diagnostics(out, support)
}

.pcs_copy_diagnostics <- function(out, support) {
  purrr::reduce(.pcs_diagnostic_names(), .init = out, \(acc, nm) {
    attr(acc, nm) <- attr(support, nm)
    acc
  })
}

# `.pcs_strip_diagnostics()` lived here until C9. Its only caller was
# `polycell_shim_view()`, which had to hand a consumer a table indistinguishable
# from `build_cell_polity()`'s: dplyr copies user attributes onto every derived
# frame, so an unstripped shim leaked the producer's diagnostics into a
# comparison that had nothing to do with them. With the shim gone the producer's
# own table is the only thing it returns, and its diagnostics belong on it.

.pcs_diagnostic_names <- function() {
  c(
    "coverage",
    "overlap",
    "long_edges",
    "terra_measured",
    "water_excess",
    "water_unmatched",
    "footprints",
    "footprint_diff",
    "unassigned"
  )
}

.pcs_require_cols <- function(x, cols, arg) {
  missing <- setdiff(cols, names(x))
  if (length(missing) > 0L) {
    cli::cli_abort("{.arg {arg}} is missing column{?s}: {.field {missing}}.")
  }
  invisible(x)
}

.pcs_empty_pieces <- function() {
  tibble::tibble(
    cell_id = integer(),
    lon = double(),
    lat = double(),
    cell_area_ha = double(),
    polity_area_ha = double(),
    polity_code = character(),
    start_year = integer(),
    end_year = integer(),
    area_code = integer(),
    polygon_status = character(),
    coverage_status = character(),
    support_role = character(),
    area_engine = character(),
    ice_area_ha = double()
  )
}
