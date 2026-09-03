#' Read the cell-to-polity country grid at a containment depth
#'
#' @description
#' The spatialization engines allocate a national total into the compartments
#' of a *country grid*: one row per physical 0.5-degree cell and territorial
#' unit, carrying that unit's share of the cell. `level = 0L` is today's grid,
#' one row per cell and reporting `area_code`. `level >= 1L` resolves the
#' polycell support one containment step deeper, so a cell held by a province
#' arrives as a province-keyed row instead of being folded into its country.
#'
#' Level 0 is served by the unchanged `.read_polycell_country_grid()` path, so
#' a default run is bit-for-bit what it was. Deeper levels bypass that path
#' entirely -- in particular they never reach `.carbon_support_to_area_code()`,
#' whose fold is what deletes a province.
#'
#' @section Which rows count as a level:
#' A polity is at depth *d* when the containment edge ([polity_containment])
#' places it *d* steps inside a container that is not itself contained. An edge
#' is admitted only when its container resolves, through
#' [polity_area_crosswalk], to a single reporting `area_code` **and** is not of
#' `polity_type` `"aggregate"`. An aggregate's reporting code is a matrix
#' *bucket*: `999` alone pools 62 territories, so filing a province under one
#' would attribute it to Rest of World rather than to a country. Dropped edges
#' are counted and named, never skipped in silence.
#'
#' @section How the share of the cell is measured:
#' `cell_area_frac` is the unit's share of the **physical cell on the land
#' basis** -- `land_area_ha / cell_land_ha`, the same basis
#' `.carbon_attach_land_share()` uses at level 0, with the cell's whole
#' measured land as the denominator. Everything the fraction later splits
#' (LUH2 class areas, crop-pattern hectares) is already land-only, so dividing
#' by the whole cell would subtract the water twice. Because the support is
#' read in its interval grain, the denominator is evaluated per interval-start
#' year: the cell's land partition genuinely differs between epochs, and taking
#' one denominator across all of them would count a cell once per epoch.
#'
#' Two support shapes are recognised, and which one is in hand is **detected,
#' never assumed**:
#'
#' \describe{
#'   \item{replacement}{The support carries the units *instead of* their
#'     container (the shape a level-tagged pin delivers). `land_area_ha` is
#'     already the unit's absolute land, so the share is taken directly. A
#'     container row surviving beside its own members in one cell is refused:
#'     both claim the same ground, and choosing one silently is exactly the
#'     double count this epic exists to remove.}
#'   \item{nested}{The support carries a within-container share column
#'     (`container_area_frac`, `container_frac` or `parent_frac`) alongside the
#'     container's own row. The unit's share of the cell is then the
#'     composition `f_unit x f_container`, with `f_container` measured from the
#'     container's own row on the same land basis.}
#' }
#'
#' @param level Containment depth, a non-negative whole number. `0L` (default)
#'   is today's cell-to-`area_code` grid.
#' @param support Polycell support table in the [build_polycell_support()]
#'   grain, overriding [read_polycell_support()]. Only read at `level >= 1L`.
#' @param containment Containment edge table in the [polity_containment]
#'   schema, overriding the packaged one. Only read at `level >= 1L`.
#' @param reference_year Optional year to snapshot the grid at, applied with
#'   the package's own validity predicate. `NULL` (default) keeps the interval
#'   grain, which is what the engines' per-year filter expects. Only read at
#'   `level >= 1L`.
#'
#' @return A `tibble` with `lon`, `lat`, `area_code` (integer, the container's
#'   reporting code), `level_polity_code` (character, `NA` at level 0),
#'   `level` (integer), `cell_area_frac`, `polycell_id`, `start_year`,
#'   `end_year`, `cell_area_ha` and `land_area_ha`. At level 0 the six columns
#'   `.carbon_cell_support()` returns are passed through unchanged.
#'
#' @seealso [build_allocation_layer()], [read_polycell_support()].
#' @export
#'
#' @examples
#' # Offline: a two-cell support for one Japanese prefecture, with the
#' # containment edge that puts it inside Japan. No pin and no network.
#' support <- tibble::tribble(
#'   ~polycell_id, ~cell_id, ~lon, ~lat, ~polity_code,
#'   "JPN-AICHI-1871-2025@1", 1L, 137.25, 35.25, "JPN-AICHI-1871-2025",
#'   "JPN-AICHI-1871-2025@2", 2L, 137.75, 35.25, "JPN-AICHI-1871-2025"
#' ) |>
#'   dplyr::mutate(
#'     area_code = 110L,
#'     start_year = 1952L,
#'     end_year = 2025L,
#'     cell_area_ha = 3000,
#'     land_area_ha = c(1200, 2000)
#'   )
#' containment <- tibble::tibble(
#'   member_code = "JPN-AICHI-1871-2025",
#'   container_code = "JPN-1952-2025",
#'   start_year = 1952L,
#'   end_year = 2025L,
#'   basis = "prefecture inside JPN-1952-2025 for those years"
#' )
#' read_level_country_grid(
#'   level = 1L,
#'   support = support,
#'   containment = containment
#' )
read_level_country_grid <- function(
  level = 0L,
  support = NULL,
  containment = NULL,
  reference_year = NULL
) {
  level <- .check_grid_level(level)
  if (level == 0L) {
    .refuse_level0_arguments(support, containment, reference_year)
    return(.read_polycell_country_grid())
  }
  support <- support %||% read_polycell_support()
  support <- .level_support_intervals(tibble::as_tibble(support))
  containment <- tibble::as_tibble(containment %||% whep::polity_containment)
  edges <- .level_admit_edges(containment, level)
  units <- .level_support_units(support, edges, level)
  grid <- .level_attach_cell_share(units, support)
  if (!is.null(reference_year)) {
    grid <- .filter_country_grid_year(grid, as.integer(reference_year))
  }
  cli::cli_alert_info(
    "country_grid: polycell support at level {level}, {nrow(grid)} \\
     compartment{?s} over \\
     {dplyr::n_distinct(paste(grid$lon, grid$lat))} cell{?s} in \\
     {dplyr::n_distinct(grid$area_code)} container{?s}."
  )
  grid
}

#' Choose an allocation depth per country and assert the result partitions
#'
#' @description
#' Decision 10's allocation layer: ONE country grid in which each country is
#' represented at exactly one depth. A container listed in `granted` arrives as
#' its granted-depth units and as nothing else; every other country arrives as
#' its level-0 row. The engines see a single grid and need no depth logic.
#'
#' @section The two-part assertion:
#' \describe{
#'   \item{(a), an abort}{The shares sum to 1 per physical cell, within
#'     `1e-8`, **across the layer as passed** -- unconditionally, not after a
#'     per-year filter (the fixture convention agreed at T37). A layer that is
#'     not a partition is either double-claiming ground (a container kept
#'     beside its own units) or losing it (a container dropped with no units to
#'     replace it), and both are invisible downstream because every national
#'     total still reconciles. Raised with class
#'     `whep_alloc_layer_not_partition`. A compartment whose share of one cell
#'     differs between two of its own validity intervals cannot be summed
#'     unconditionally and is refused with class
#'     `whep_alloc_layer_varying_share`; making a unit's share move through
#'     time is the territory-basis mechanism, which belongs to T28.}
#'   \item{(b), a diagnostic}{For every granted country: no container-keyed row
#'     survives anywhere, and per cell the unit shares reproduce the country's
#'     level-0 share. Cells that fail are RETURNED, in the
#'     `"ragged_coverage"` attribute, and are never back-filled with the
#'     container -- back-filling would restore exactly the fold this feature
#'     removes. (b) failing is not by itself a defect: level 0 is a fixed 2015
#'     snapshot while granted depths are year-filtered by edge validity, and
#'     how the two coexist is open at T31(j).}
#' }
#'
#' @param grid0 The level-0 country grid, e.g. `read_level_country_grid(0L)`.
#' @param grid_deep A granted-depth country grid, e.g.
#'   `read_level_country_grid(1L)`. May also carry level-0 rows for countries
#'   that are not granted; they are ignored here and taken from `grid0`.
#' @param granted A tibble with `area_code` (integer container code) and
#'   `level` (integer depth), one row per container -- the granted rows of the
#'   T25 coverage report. `NULL` or a zero-row tibble returns `grid0`.
#'
#' @return `grid0`-shaped `tibble` with `level_polity_code` and `level`,
#'   carrying the ragged-coverage diagnostic in its `"ragged_coverage"`
#'   attribute (a zero-row tibble when nothing failed).
#'
#' @seealso [read_level_country_grid()].
#' @export
#'
#' @examples
#' grid0 <- tibble::tribble(
#'   ~lon,  ~lat, ~area_code, ~cell_area_frac,
#'   10.25, 40.25,       900L,             0.6,
#'   10.25, 40.25,       901L,             0.4
#' )
#' grid_deep <- tibble::tribble(
#'   ~lon,  ~lat, ~area_code, ~level_polity_code, ~level, ~cell_area_frac,
#'   10.25, 40.25,       900L, "A-A1-1900-2100",      1L,             0.25,
#'   10.25, 40.25,       900L, "A-A2-1900-2100",      1L,             0.35,
#'   10.25, 40.25,       901L, NA_character_,         0L,             0.4
#' )
#' granted <- tibble::tibble(area_code = 900L, level = 1L)
#' build_allocation_layer(grid0, grid_deep, granted)
build_allocation_layer <- function(grid0, grid_deep, granted) {
  .check_columns(grid0, c("lon", "lat", "area_code"), "grid0")
  grid0 <- .level_ensure_level_cols(grid0, 0L, "grid0")
  granted <- .level_check_granted(granted)
  if (nrow(granted) == 0L) {
    cli::cli_inform(
      "No container is granted a depth; the allocation layer is {.arg grid0}."
    )
    return(.level_attach_ragged(grid0, .level_ragged_prototype()))
  }
  .check_columns(grid_deep, c("lon", "lat", "area_code"), "grid_deep")
  grid_deep <- .level_ensure_level_cols(grid_deep, NULL, "grid_deep")
  deep <- dplyr::semi_join(grid_deep, granted, by = c("area_code", "level"))
  .level_check_supply(deep, granted)
  base <- dplyr::filter(grid0, !(area_code %in% granted$area_code))
  layer <- dplyr::bind_rows(base, deep)
  shares <- .level_compartment_shares(layer)
  .assert_layer_partition(shares)
  ragged <- .level_ragged_coverage(shares, grid0, granted)
  .level_warn_ragged(ragged)
  .level_attach_ragged(layer, ragged)
}

#' Zero-row prototype of the run's admin-coverage report
#'
#' @description
#' The schema of `admin_coverage.csv`, which [run_spatialize()] writes beside
#' `run_metadata.yaml` whenever a run requests a depth. One row per container x
#' item x year, saying which admin source constrained it and at what grain. The
#' populated table is produced by the T25 resolver; until a run is wired to one,
#' the file is written with this header and no rows, so a reader can tell "no
#' coverage was granted" from "the file was never written".
#'
#' @return A zero-row `tibble` with the report's columns.
#' @export
#'
#' @examples
#' admin_coverage_prototype()
admin_coverage_prototype <- function() {
  tibble::tibble(
    area_code = integer(),
    item_prod_code = integer(),
    year = integer(),
    source = character(),
    tier = integer(),
    grain = character(),
    level = integer(),
    reporting_units = character(),
    n_units_reporting = integer(),
    coverage_change = logical(),
    not_shipped = character()
  )
}

# --- Level validation --------------------------------------------------------

# One place decides what a level is, so the loader, the reader and the override
# key cannot disagree about whether `1` and `1L` are the same request.
.check_grid_level <- function(level, arg = "level") {
  if (is.null(level)) {
    return(0L)
  }
  ok <- is.numeric(level) &&
    length(level) == 1L &&
    !is.na(level) &&
    level >= 0 &&
    level == round(level)
  if (!ok) {
    cli::cli_abort(c(
      "{.arg {arg}} must be a single non-negative whole number.",
      x = "Got {.val {level}}."
    ))
  }
  as.integer(level)
}

# Level 0 is the carbon path's own snapshot, taken at `.carbon_support_year()`
# through `.carbon_cell_support()`. Accepting a support, an edge table or a year
# here and then ignoring them is how a run ends up believing it read one thing
# while it read another, so they are refused rather than dropped.
.refuse_level0_arguments <- function(support, containment, reference_year) {
  given <- c(
    support = !is.null(support),
    containment = !is.null(containment),
    reference_year = !is.null(reference_year)
  )
  if (!any(given)) {
    return(invisible(NULL))
  }
  cli::cli_abort(c(
    "{.arg {names(given)[given]}} {?is/are} not read at {.code level = 0}.",
    x = "Level 0 is the carbon path's own snapshot at
         {.code .carbon_support_year()}, resolved by
         {.fn .read_polycell_country_grid}.",
    i = "Pass {.code level >= 1} to supply your own support and edges."
  ))
}

# --- The containment edge ----------------------------------------------------

# Admit the edges whose member sits at exactly `level` steps inside a container
# the reporting vocabulary can name.
.level_admit_edges <- function(containment, level) {
  .check_columns(
    containment,
    c("member_code", "container_code", "start_year", "end_year"),
    "containment"
  )
  edges <- containment |>
    dplyr::mutate(
      start_year = as.integer(start_year),
      end_year = as.integer(end_year)
    ) |>
    dplyr::filter(!is.na(member_code), !is.na(container_code))
  depth <- .level_edge_depth(edges)
  edges <- edges |>
    dplyr::mutate(level = depth[match(member_code, names(depth))]) |>
    dplyr::filter(level == .env$level)
  if (nrow(edges) == 0L) {
    cli::cli_abort(
      c(
        "No containment edge places a polity at depth {level}.",
        i = "Depths present: {.val {sort(unique(depth))}}."
      ),
      class = "whep_level_no_edges"
    )
  }
  .level_resolve_containers(edges)
}

# Depth by walking the edge set: a member whose container is itself a member is
# one step deeper. Bounded by the number of distinct members, so a cycle in the
# edge table aborts instead of looping.
.level_edge_depth <- function(edges) {
  members <- unique(edges$member_code)
  depth <- rep(NA_integer_, length(members))
  names(depth) <- members
  # Depth is a property of the MEMBER, so the first container found for it
  # decides: a member listed inside two containers at different depths would be
  # at two depths at once, which the vocabulary does not express.
  parent <- edges$container_code[match(members, edges$member_code)]
  # A container that is never itself a member is the root the walk stops on.
  depth[!(parent %in% members)] <- 1L
  for (step in seq_along(members)) {
    if (!anyNA(depth)) {
      break
    }
    depth[is.na(depth)] <- depth[parent[is.na(depth)]] + 1L
  }
  if (anyNA(depth)) {
    cli::cli_abort(c(
      "{sum(is.na(depth))} containment edge{?s} form a cycle.",
      x = "Member{?s}: {.val {names(depth)[is.na(depth)]}}.",
      i = "A member cannot contain, directly or transitively, its container."
    ))
  }
  depth
}

# The container's reporting `area_code`, resolved through the package's own
# polity lookup and NEVER by name. An aggregate container is refused: its
# reporting code is a matrix bucket (999 pools 62 territories), so a unit filed
# under one would be attributed to Rest of World.
.level_resolve_containers <- function(edges) {
  types <- .level_polity_types()
  edges <- edges |>
    dplyr::mutate(
      container_area_code = .polity_reporting_area_code(container_code),
      polity_type = types$polity_type[match(container_code, types$polity_code)]
    )
  drop_type <- edges$polity_type %in% "aggregate"
  drop_code <- is.na(edges$container_area_code)
  if (any(drop_type | drop_code)) {
    .level_warn_dropped_edges(edges, drop_type, drop_code)
  }
  kept <- edges[!(drop_type | drop_code), , drop = FALSE]
  if (nrow(kept) == 0L) {
    cli::cli_abort(
      "No containment edge has a container the reporting vocabulary names.",
      class = "whep_level_no_edges"
    )
  }
  dplyr::select(
    kept,
    "member_code",
    "container_code",
    "container_area_code",
    "level",
    "start_year",
    "end_year"
  )
}

.level_polity_types <- function() {
  types <- whep::polity_area_crosswalk |>
    dplyr::distinct(polity_code, polity_type)
  dup <- unique(types$polity_code[duplicated(types$polity_code)])
  if (length(dup) > 0L) {
    cli::cli_abort(c(
      "{.field polity_area_crosswalk} gives {length(dup)} polit{?y/ies} more
       than one {.field polity_type}.",
      x = "{.val {dup}}."
    ))
  }
  types
}

.level_warn_dropped_edges <- function(edges, drop_type, drop_code) {
  aggregates <- unique(edges$container_code[drop_type])
  unnamed <- unique(edges$container_code[drop_code & !drop_type])
  cli::cli_warn(c(
    "!" = "{sum(drop_type | drop_code)} containment edge{?s} {?is/are} not
           admitted at this depth.",
    "*" = "{length(aggregates)} aggregate container{?s}: {.val {aggregates}}.",
    "*" = "{length(unnamed)} container{?s} with no single reporting
           {.field area_code}: {.val {unnamed}}.",
    i = "Their members stay folded into whatever level 0 reports."
  ))
}

# --- Support rows at a depth -------------------------------------------------

# The support rows of the admitted members, intersected with the edge's own
# validity. A member may sit inside several successive containers (Japan's
# prefectures span four Japanese epochs), so one support row can yield one row
# per overlapping edge; that is the point, since the container -- and therefore
# the reporting code the row is filed under -- is what changes.
.level_support_units <- function(support, edges, level) {
  .check_columns(
    support,
    c("lon", "lat", "polity_code", "cell_area_ha", "land_area_ha"),
    "support"
  )
  units <- support |>
    dplyr::inner_join(
      edges,
      by = dplyr::join_by(polity_code == member_code),
      relationship = "many-to-many",
      suffix = c("", "_edge")
    ) |>
    dplyr::mutate(
      start_year = pmax(start_year, start_year_edge),
      end_year = pmin(end_year, end_year_edge)
    ) |>
    dplyr::filter(start_year < end_year)
  if (nrow(units) == 0L) {
    cli::cli_abort(
      c(
        "The support carries no cells for any polity at depth {level}.",
        i = "{dplyr::n_distinct(edges$member_code)} member{?s} were looked
             for; the support holds
             {dplyr::n_distinct(support$polity_code)} polit{?y/ies}.",
        i = "Regenerate the polycell support so it carries the granted
             depth; see {.fn build_polycell_support}."
      ),
      class = "whep_level_support_empty"
    )
  }
  units
}

# A support with no validity interval is a single-epoch table; give it the open
# interval rather than treating a missing bound as year zero.
.level_support_intervals <- function(support) {
  if (!rlang::has_name(support, "start_year")) {
    support$start_year <- NA_integer_
  }
  if (!rlang::has_name(support, "end_year")) {
    support$end_year <- NA_integer_
  }
  support |>
    dplyr::mutate(
      start_year = dplyr::coalesce(as.integer(start_year), -2147483647L),
      end_year = dplyr::coalesce(as.integer(end_year), 2147483647L)
    )
}

# --- The share of the physical cell ------------------------------------------

.level_attach_cell_share <- function(units, support) {
  nested_col <- intersect(.level_container_frac_cols(), names(support))
  if (length(nested_col) > 0L) {
    return(.level_share_nested(units, support, nested_col[[1L]]))
  }
  .level_check_no_double_claim(units, support)
  cell_land <- .level_cell_land(support, unique(units$start_year))
  units |>
    dplyr::left_join(cell_land, by = c("lon", "lat", "start_year")) |>
    dplyr::mutate(cell_area_frac = land_area_ha / cell_land_ha) |>
    .level_finish_grid()
}

#' Column names that hold a unit's share of its CONTAINER, not of the cell.
#'
#' Present only on a support that delivers within-container partitions. The
#' share of the physical cell is then the composition `f_unit x f_container`,
#' never the column itself: read as a cell share it would hand a province the
#' whole of its country's cell.
#' @noRd
.level_container_frac_cols <- function() {
  c("container_area_frac", "container_frac", "parent_frac")
}

# Nested support: the container keeps its own row (that is where `f_container`
# is measured) and the unit's column is a share of it.
.level_share_nested <- function(units, support, frac_col) {
  years <- unique(units$start_year)
  cell_land <- .level_cell_land(support, years)
  container_land <- .level_container_land(support, years)
  out <- units |>
    dplyr::left_join(cell_land, by = c("lon", "lat", "start_year")) |>
    dplyr::left_join(
      container_land,
      by = c("lon", "lat", "container_code", "start_year")
    )
  if (anyNA(out$container_land_ha)) {
    cli::cli_abort(
      c(
        "{sum(is.na(out$container_land_ha))} unit row{?s} have no container
         row in the same cell.",
        x = "A within-container share ({.field {frac_col}}) can only be
             composed where the container's own land is measured.",
        i = "Either supply the container rows or publish absolute
             {.field land_area_ha} per unit."
      ),
      class = "whep_level_container_missing"
    )
  }
  cli::cli_inform(
    "Support carries {.field {frac_col}}: composing the unit's share of its
     container with the container's share of the cell."
  )
  out |>
    dplyr::mutate(
      cell_area_frac = .data[[frac_col]] *
        (container_land_ha / cell_land_ha)
    ) |>
    .level_finish_grid()
}

# The cell's whole measured land, evaluated once per interval-start year the
# unit set uses. `.carbon_attach_land_share()` takes the same denominator at one
# reference year; in interval grain it has to be taken per epoch, because
# summing an interval-grain table whole counts each cell once per epoch.
.level_cell_land <- function(support, years) {
  purrr::map(
    sort(unique(years)),
    \(yr) {
      .filter_country_grid_year(support, yr) |>
        dplyr::summarise(
          cell_land_ha = sum(land_area_ha, na.rm = TRUE),
          .by = c("lon", "lat")
        ) |>
        dplyr::mutate(start_year = yr)
    }
  ) |>
    purrr::list_rbind()
}

# The container's own measured land per cell, taken on the same per-epoch basis
# as `.level_cell_land()`. Only the nested shape needs it.
.level_container_land <- function(support, years) {
  purrr::map(
    sort(unique(years)),
    \(yr) {
      .filter_country_grid_year(support, yr) |>
        dplyr::summarise(
          container_land_ha = sum(land_area_ha, na.rm = TRUE),
          .by = c("lon", "lat", "polity_code")
        ) |>
        dplyr::rename(container_code = "polity_code") |>
        dplyr::mutate(start_year = yr)
    }
  ) |>
    purrr::list_rbind()
}

# A container row surviving beside its own members in one cell claims that
# ground twice. Nothing downstream can see it -- the container's national total
# still reconciles -- so it is refused here rather than resolved by picking one.
# This is the consumer-side half of the T07 gate.
.level_check_no_double_claim <- function(units, support) {
  clash <- units |>
    dplyr::select("lon", "lat", "container_code", "start_year", "end_year") |>
    dplyr::distinct() |>
    dplyr::inner_join(
      dplyr::select(
        support,
        "lon",
        "lat",
        container_code = "polity_code",
        "land_area_ha",
        con_start = "start_year",
        con_end = "end_year"
      ),
      by = c("lon", "lat", "container_code"),
      relationship = "many-to-many"
    ) |>
    dplyr::filter(start_year < .data$con_end, .data$con_start < end_year)
  if (nrow(clash) == 0L) {
    return(invisible(NULL))
  }
  codes <- sort(unique(clash$container_code))
  cli::cli_abort(
    c(
      "{nrow(clash)} cell{?s} carry a container row beside its own units
       ({round(sum(clash$land_area_ha, na.rm = TRUE) / 1e6, 2)} Mha).",
      x = "Container{?s}: {.val {codes}}.",
      i = "The two claim the same ground, so the cell's land no longer
           partitions. A level-tagged support returns the units INSTEAD of
           their container; see the T07 package-wide level gate."
    ),
    class = "whep_level_support_double_claim"
  )
}

.level_finish_grid <- function(grid) {
  bad <- which(
    is.na(grid$cell_area_frac) |
      grid$cell_area_frac < -1e-8 |
      grid$cell_area_frac > 1 + 1e-8
  )
  if (length(bad) > 0L) {
    cli::cli_abort(c(
      "{length(bad)} compartment{?s} have a share outside {.code [0, 1]}.",
      x = "Range: {.val {range(grid$cell_area_frac[bad])}}.",
      i = "A cell whose measured land is zero has no share to take."
    ))
  }
  grid |>
    dplyr::mutate(
      area_code = as.integer(container_area_code),
      level_polity_code = as.character(polity_code),
      level = as.integer(level),
      polycell_id = .level_polycell_id(grid)
    ) |>
    dplyr::select(
      "lon",
      "lat",
      "area_code",
      "level_polity_code",
      "level",
      "cell_area_frac",
      "polycell_id",
      "start_year",
      "end_year",
      dplyr::any_of(c("cell_area_ha", "land_area_ha"))
    )
}

# The support's own identifier where it has one, else the producer's
# `polity_code@cell_id` convention (`R/polycell_support.R:1562`).
.level_polycell_id <- function(grid) {
  if (rlang::has_name(grid, "polycell_id")) {
    return(as.character(grid$polycell_id))
  }
  cell <- if (rlang::has_name(grid, "cell_id")) {
    grid$cell_id
  } else {
    paste0(grid$lon, "_", grid$lat)
  }
  paste0(grid$polity_code, "@", cell)
}

# --- The allocation layer ----------------------------------------------------

.level_ensure_level_cols <- function(grid, default_level, arg) {
  grid <- tibble::as_tibble(grid)
  if (!rlang::has_name(grid, "level_polity_code")) {
    grid$level_polity_code <- NA_character_
  }
  if (!rlang::has_name(grid, "level")) {
    if (is.null(default_level)) {
      cli::cli_abort(c(
        "{.arg {arg}} carries no {.field level} column.",
        i = "A granted-depth grid must declare the depth its rows are at;
             {.fn read_level_country_grid} writes it."
      ))
    }
    grid$level <- default_level
  }
  frac_col <- intersect(.polity_share_cols(), names(grid))
  if (length(frac_col) == 0L) {
    .abort_missing_polity_share(grid, arg)
  }
  grid |>
    dplyr::mutate(
      area_code = as.integer(area_code),
      level = as.integer(level),
      level_polity_code = as.character(level_polity_code),
      cell_area_frac = as.numeric(.data[[frac_col[[1L]]]])
    )
}

.level_check_granted <- function(granted) {
  if (is.null(granted)) {
    return(tibble::tibble(area_code = integer(), level = integer()))
  }
  .check_columns(granted, c("area_code", "level"), "granted")
  granted <- granted |>
    tibble::as_tibble() |>
    dplyr::mutate(
      area_code = as.integer(area_code),
      level = as.integer(level)
    ) |>
    dplyr::select("area_code", "level")
  if (any(granted$level < 1L, na.rm = TRUE) || anyNA(granted)) {
    cli::cli_abort(c(
      "{.arg granted} must give every container a depth of at least 1.",
      i = "A container at level 0 is simply absent from {.arg granted}."
    ))
  }
  dup <- unique(granted$area_code[duplicated(granted$area_code)])
  if (length(dup) > 0L) {
    cli::cli_abort(c(
      "{.arg granted} names {length(dup)} container{?s} twice.",
      x = "{.val {dup}}.",
      i = "A country is allocated at exactly one depth."
    ))
  }
  granted
}

# A grant with no rows to honour it would silently leave the country out of the
# layer altogether, which is worse than allocating it at level 0.
.level_check_supply <- function(deep, granted) {
  supplied <- unique(deep$area_code)
  missing <- setdiff(granted$area_code, supplied)
  if (length(missing) == 0L) {
    return(invisible(NULL))
  }
  cli::cli_abort(
    c(
      "{length(missing)} granted container{?s} have no rows in
       {.arg grid_deep} at the granted depth.",
      x = "area_code{?s}: {.val {missing}}.",
      i = "Withdraw the grant or supply the depth; the country must not
           vanish from the layer."
    ),
    class = "whep_alloc_layer_no_supply"
  )
}

# One row per (cell, compartment). A compartment appearing in several of its own
# validity intervals contributes its share ONCE; a share that differs between
# those intervals cannot be summed unconditionally and is refused.
.level_compartment_shares <- function(layer, tol = 1e-8) {
  shares <- layer |>
    dplyr::summarise(
      share = dplyr::first(cell_area_frac),
      share_span = max(cell_area_frac) - min(cell_area_frac),
      n_rows = dplyr::n(),
      .by = c("lon", "lat", "area_code", "level_polity_code")
    )
  varying <- dplyr::filter(shares, share_span > tol)
  if (nrow(varying) > 0L) {
    cli::cli_abort(
      c(
        "{nrow(varying)} compartment{?s} hold different shares of one cell in
         different validity intervals.",
        x = "Worst span: {.val {max(varying$share_span)}}.",
        i = "Assertion (a) is evaluated on the layer as passed, so a share
             that moves through time cannot be checked here. Moving one is
             the territory-basis mechanism (T28)."
      ),
      class = "whep_alloc_layer_varying_share"
    )
  }
  dplyr::select(shares, "lon", "lat", "area_code", "level_polity_code", "share")
}

# Assertion (a).
.assert_layer_partition <- function(shares, tol = 1e-8) {
  cells <- dplyr::summarise(
    shares,
    total = sum(share),
    .by = c("lon", "lat")
  )
  bad <- dplyr::filter(cells, abs(.data$total - 1) > tol)
  if (nrow(bad) == 0L) {
    return(invisible(NULL))
  }
  worst <- bad[which.max(abs(bad$total - 1)), , drop = FALSE]
  cli::cli_abort(
    c(
      "{nrow(bad)} cell{?s} of the allocation layer do not partition.",
      x = "Worst: ({worst$lon}, {worst$lat}) sums to
           {.val {worst$total}}, not 1.",
      i = "A sum above 1 is a container kept beside its own units; a sum
           below 1 is ground dropped with nothing to replace it. Both leave
           every national total reconciling.",
      i = "A level-0 grid built through {.fn .carbon_cell_support} drops the
           polycells the reporting vocabulary cannot name, so its own cells
           can sum below 1 before any depth is granted; that is a property of
           {.arg grid0}, not of the grant."
    ),
    class = "whep_alloc_layer_not_partition"
  )
}

# Assertion (b), a diagnostic and never a repair.
.level_ragged_coverage <- function(shares, grid0, granted, tol = 1e-8) {
  units <- shares |>
    dplyr::filter(area_code %in% granted$area_code) |>
    dplyr::summarise(
      unit_share = sum(share),
      n_units = sum(!is.na(level_polity_code)),
      n_container_rows = sum(is.na(level_polity_code)),
      .by = c("lon", "lat", "area_code")
    )
  base <- grid0 |>
    dplyr::filter(area_code %in% granted$area_code) |>
    dplyr::summarise(
      level0_share = sum(cell_area_frac),
      .by = c("lon", "lat", "area_code")
    )
  units |>
    dplyr::full_join(base, by = c("lon", "lat", "area_code")) |>
    dplyr::mutate(
      n_units = dplyr::coalesce(n_units, 0L),
      n_container_rows = dplyr::coalesce(n_container_rows, 0L),
      difference = dplyr::coalesce(unit_share, 0) -
        dplyr::coalesce(level0_share, 0),
      reason = .level_ragged_reason(
        n_container_rows,
        n_units,
        level0_share,
        difference,
        tol
      )
    ) |>
    dplyr::filter(!is.na(reason)) |>
    dplyr::select(
      "lon",
      "lat",
      "area_code",
      "n_units",
      "n_container_rows",
      "unit_share",
      "level0_share",
      "difference",
      "reason"
    )
}

.level_ragged_reason <- function(
  n_container_rows,
  n_units,
  level0_share,
  difference,
  tol
) {
  dplyr::case_when(
    n_container_rows > 0L ~ "container_row_present",
    n_units == 0L ~ "no_unit_rows",
    is.na(level0_share) ~ "unit_outside_level0",
    abs(difference) > tol ~ "unit_share_mismatch",
    .default = NA_character_
  )
}

.level_ragged_prototype <- function() {
  tibble::tibble(
    lon = numeric(),
    lat = numeric(),
    area_code = integer(),
    n_units = integer(),
    n_container_rows = integer(),
    unit_share = numeric(),
    level0_share = numeric(),
    difference = numeric(),
    reason = character()
  )
}

.level_warn_ragged <- function(ragged) {
  if (nrow(ragged) == 0L) {
    return(invisible(NULL))
  }
  counts <- table(ragged$reason)
  cli::cli_warn(c(
    "!" = "{nrow(ragged)} cell{?s} of the allocation layer are ragged: a
           granted country's units do not reproduce its level-0 share.",
    "*" = "{paste(names(counts), unname(counts), sep = ': ',
             collapse = '; ')}.",
    i = "Returned in the {.field ragged_coverage} attribute. They are NOT
         back-filled with the container -- that would restore the fold."
  ))
}

.level_attach_ragged <- function(layer, ragged) {
  attr(layer, "ragged_coverage") <- ragged
  layer
}

# --- Output grain ------------------------------------------------------------

# Decision 10's output grain, applied AFTER the engine returns rather than
# inside its year loop: the engine allocates at the granted depth, and what the
# caller gets back is a choice about reporting, not about allocation.
#
# The default folds granted-depth rows onto the container, so
# `(lon, lat, area_code, item_prod_code, year)` stays unique and the schema
# equals `main`'s. A result that carries no `level_polity_code` -- every level-0
# run -- is returned IDENTICALLY, which is what keeps the default path
# bit-for-bit.
.level_fold_output <- function(
  result,
  output_level = 0L,
  value_cols = c("rainfed_ha", "irrigated_ha")
) {
  output_level <- .check_grid_level(output_level, "output_level")
  if (!rlang::has_name(result, "level_polity_code")) {
    return(result)
  }
  if (output_level > 0L) {
    return(result)
  }
  value_cols <- intersect(value_cols, names(result))
  drop_cols <- c("level_polity_code", "level", "polycell_id", "cell_id")
  group_cols <- setdiff(names(result), c(value_cols, drop_cols))
  folded <- result |>
    dplyr::summarise(
      dplyr::across(dplyr::all_of(value_cols), \(x) sum(x, na.rm = TRUE)),
      .by = dplyr::all_of(group_cols)
    )
  cli::cli_inform(
    "Output grain: folded {nrow(result)} granted-depth row{?s} onto
     {nrow(folded)} container-keyed row{?s}."
  )
  .level_check_output_key(folded, group_cols)
  folded
}

.level_check_output_key <- function(folded, group_cols) {
  key <- intersect(
    c("lon", "lat", "area_code", "item_prod_code", "cft_name", "year"),
    group_cols
  )
  dup <- sum(duplicated(folded[key]))
  if (dup > 0L) {
    cli::cli_abort(c(
      "The container-keyed output has {dup} duplicated key row{?s}.",
      x = "Key: {.field {key}}.",
      i = "A column that varies within a container's units survived the
           fold; declare it as a value column or summarise it first."
    ))
  }
  invisible(NULL)
}

# --- The run's coverage report -----------------------------------------------

.write_admin_coverage <- function(out_dir, coverage = NULL) {
  coverage <- coverage %||% admin_coverage_prototype()
  missing <- setdiff(names(admin_coverage_prototype()), names(coverage))
  if (length(missing) > 0L) {
    cli::cli_abort(c(
      "{.arg coverage} is missing {length(missing)} column{?s}.",
      x = "{.field {missing}}.",
      i = "Its schema is {.fn admin_coverage_prototype}."
    ))
  }
  path <- file.path(out_dir, "admin_coverage.csv")
  # Subset to the prototype's columns so the file's schema is the same in every
  # run: a missing column is refused above, an extra one is a resolver
  # diagnostic that does not belong in the run's report.
  data.table::fwrite(
    coverage[names(admin_coverage_prototype())],
    path
  )
  path
}

# --- Real-data comparison (verification surface (ii)) ------------------------

# Compare two spatialization outputs by their sorted contents, at a stated
# tolerance. Values are rounded before hashing, because a bit-level hash of two
# sums that followed different input orders differs at the last ulp and would
# report every run as changed; the row keys are compared exactly.
#
# Used by the `WHEP_SPATIALIZE_OUT_DIR` leg in
# `tests/testthat/test_spatialize_levels.R`, and unit-tested offline on two
# small tibbles so the comparison itself is verified even where the real
# outputs are not available.
.compare_spatialize_outputs <- function(
  new,
  reference,
  key_cols = c("year", "area_code", "lon", "lat", "item_prod_code"),
  value_cols = c("rainfed_ha", "irrigated_ha"),
  tolerance = 1e-9
) {
  key_cols <- intersect(key_cols, intersect(names(new), names(reference)))
  value_cols <- intersect(value_cols, intersect(names(new), names(reference)))
  digest_new <- .spatialize_content_hash(new, key_cols, value_cols, tolerance)
  digest_ref <- .spatialize_content_hash(
    reference,
    key_cols,
    value_cols,
    tolerance
  )
  list(
    identical = identical(digest_new$hash, digest_ref$hash),
    rows_new = nrow(new),
    rows_reference = nrow(reference),
    hash_new = digest_new$hash,
    hash_reference = digest_ref$hash,
    max_abs_diff = .spatialize_max_diff(
      digest_new$data,
      digest_ref$data,
      key_cols,
      value_cols
    ),
    key_cols = key_cols,
    value_cols = value_cols,
    tolerance = tolerance
  )
}

.spatialize_content_hash <- function(data, key_cols, value_cols, tolerance) {
  digits <- max(0L, as.integer(round(-log10(tolerance))))
  slim <- data |>
    tibble::as_tibble() |>
    dplyr::select(dplyr::all_of(c(key_cols, value_cols))) |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(value_cols), \(x) round(x, digits))
    ) |>
    dplyr::arrange(dplyr::pick(dplyr::all_of(key_cols)))
  list(hash = rlang::hash(as.data.frame(slim)), data = slim)
}

.spatialize_max_diff <- function(new, reference, key_cols, value_cols) {
  if (nrow(new) != nrow(reference)) {
    return(NA_real_)
  }
  joined <- dplyr::inner_join(
    new,
    reference,
    by = key_cols,
    suffix = c("_new", "_ref")
  )
  if (nrow(joined) != nrow(new)) {
    return(NA_real_)
  }
  diffs <- purrr::map_dbl(value_cols, \(cl) {
    max(abs(joined[[paste0(cl, "_new")]] - joined[[paste0(cl, "_ref")]]))
  })
  max(c(0, diffs))
}

# Peak memory of the current session, in MB. `gc()`'s per-row max-used megabyte
# column (the sixth) is the only peak figure base R exposes on every platform,
# so the floor recorded in the PR body is this and is LABELLED as such: it is
# the R heap high-water mark, not the process RSS the operating system reports,
# and it never falls until `gc(reset = TRUE)`.
.spatialize_peak_mb <- function() {
  usage <- gc(reset = FALSE, full = TRUE)
  round(sum(usage[, 6L]), 1)
}
