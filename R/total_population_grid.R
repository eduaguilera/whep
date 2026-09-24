# Gridded TOTAL population: UN WPP country totals downscaled to WHEP's
# 0.5-degree polycells by HYDE's total-population (`popc`) pattern.
#
# This is the population basis build_urban_n() uses under
# `population_basis = "total"`. It ports the afse-viewer's D14 level
# (afse-viewer scripts/grid/nb_inputs.R, build_urban_population()) into WHEP.
#
# THE METHOD. For a requested year y and a polycell (c, a) -- cell c, grid area
# code a, whose polity_area_code bucket is p:
#
#   population of (c, a) in y  =  WPP total of p in y  x  share of (c, a) in y
#
# where the share of (c, a) is the polycell's share of bucket p's HYDE `popc` count
# (the cell's count split across the areas holding it by `polity_frac`),
# linearly interpolated in y between the two nearest HYDE snapshots and HELD at
# the last snapshot past it. WPP is keyed on the polity_area_code bucket
# (read_wpp_population() resolves ISO3 through `polity_area_code`), so the
# shares are taken within the bucket and the output keeps the caller's own grid
# `area_code`: a bucket holding two grid codes (206 holds 206 and 276) splits
# its WPP total between them by their HYDE population.
#
# WHY THE SHARE IS INTERPOLATED AND NOT THE COUNT. WPP sets the LEVEL (annual,
# the 2024 revision WHEP already uses elsewhere); HYDE sets only the SHAPE,
# where inside a country the people are, which moves slowly enough for a
# decadal snapshot to stand in for the years between. Interpolating HYDE's
# count instead would import HYDE's own country totals and discard the annual
# WPP level. A share cannot leak a level: the shares of a bucket sum to 1 at
# every snapshot, a convex combination of two partitions is a partition, so
# each bucket's polycells sum back to exactly its WPP total. That identity is
# what the tests assert.
#
# WHY THE OUTPUT STAYS POLYCELL-RESOLVED. The viewer summed its result to cells
# before handing it to build_urban_n(), which re-split each cell by
# `polity_frac`. That keeps the global total but blends two countries'
# WPP/HYDE ratios on every border cell, so no country's cells summed to its own
# WPP total any more. Keeping `area_code` on the rows, and having
# build_urban_n() take them as they are, keeps the per-polity identity through
# to the nitrogen. Global totals are identical either way.
#
# WHAT IS REFUSED, NOT GUESSED (fail closed):
#   * a year before the first HYDE snapshot -- no bracket to interpolate from;
#   * a year the WPP table carries no row for -- WPP 2024 starts in 1950;
#   * a `popc` member missing from a snapshot archive (.read_hyde_year()).
#
# THE BUCKET of a grid code is the one build_cell_polity(area_key =
# "polity_area") assigns: its polity_area_code, or the code itself where the
# crosswalk gives it none (.cell_polity_to_bucket()), so both routes agree on
# which WPP total a polycell is levelled to.
#
# WHAT IS REPORTED, NOT DROPPED. Mass that cannot be placed stays visible in
# the `coverage` attribute and in a warning: WPP totals with no HYDE pattern
# (`unplaced`), HYDE population in a bucket WPP does not carry
# (`unmatched_hyde`), and interpolated share sums that left 1 and were
# renormalised (`share_gaps`; a bucket present at only one bracketing
# snapshot). Held years are announced.

#' Build gridded total population from UN WPP and HYDE.
#'
#' @description
#' Downscales UN World Population Prospects 2024 country totals
#' ([read_wpp_population()], `by = "total"`) to WHEP's 0.5-degree polycells by
#' HYDE's total-population (`popc`) pattern. Each polycell's share of its
#' country's HYDE population is linearly interpolated between the two nearest
#' HYDE snapshots (decadal before 2001, annual 2001-2017 in the HYDE 3.x
#' baseline release) and held at the last snapshot for later years, and WPP
#' sets the level. So every country's cells sum to exactly its WPP total in
#' every year.
#'
#' This is the population [build_urban_n()] reads under
#' `population_basis = "total"`, where it is paired with the per-capita rate
#' [urban_kgn_cap_total_reference], whose denominator is Spain's WPP total
#' population, so the level and the rate share one basis.
#'
#' @details
#' WPP is keyed on the `polity_area_code` bucket, so shares are taken within
#' each bucket (the one `build_cell_polity(area_key = "polity_area")` assigns)
#' and the output keeps the grid `area_code` of `data$cell_polity`. A bucket
#' with a WPP total but no HYDE population, and one with HYDE population but no
#' WPP total, are each reported in a warning and in the `coverage` attribute
#' rather than dropped silently.
#'
#' The `coverage` attribute is a list recording what the numbers are:
#' `population_basis` (`"total"`), `level_source`, `pattern_variable`
#' (`"popc"`), the `exact`, `interpolated` and `held` years with `held_at`,
#' the snapshots used, the per-year `plan`, and the `unplaced`,
#' `unmatched_hyde` and `share_gaps` tables.
#'
#' @param years Integer vector of calendar years. Required. A year before the
#'   first HYDE snapshot, or one the WPP table does not carry (WPP 2024 starts
#'   in 1950), aborts with class `whep_total_population_uncovered`.
#' @inheritParams build_water_balance
#' @param hyde_dir Directory holding the HYDE `"{year}AD_pop.zip"` archives,
#'   whose file names set the available snapshot years. Defaults to
#'   `Sys.getenv("WHEP_HYDE_DIR")`. Ignored when `data$hyde` is supplied.
#' @param data Named list of inputs: `cell_polity` (required; `lon`, `lat`,
#'   `area_code`, optional `polity_frac`, as [build_cell_polity()] emits it,
#'   with the numeric WHEP area code), `wpp` (optional; `year`, `area_code`,
#'   `population`, bypassing [read_wpp_population()]) and `hyde` (optional;
#'   `lon`, `lat`, `year`, `popc` on the 0.5-degree grid, bypassing the HYDE
#'   archives; its years are the snapshot years).
#' @param example If `TRUE`, return a small fixture instead of reading data.
#'   Defaults to `FALSE`.
#' @return A tibble with `lon`, `lat`, `area_code`, `year` and `population`
#'   (persons), one row per populated polycell-year, plus the polity columns
#'   below, and the `coverage` attribute described in Details.
#' @inheritSection whep_polity_columns Polity columns
#' @export
#' @examples
#' build_total_population_grid(example = TRUE)
build_total_population_grid <- function(
  years = NULL,
  polity_validity = c("keep", "flag", "drop"),
  hyde_dir = NULL,
  data = list(),
  example = FALSE
) {
  polity_validity <- rlang::arg_match(polity_validity)
  if (isTRUE(example)) {
    return(.resolve_polity_validity(
      .example_total_population_grid(),
      polity_validity
    ))
  }
  years <- .tp_check_years(years)
  polity <- .wb_require_input(data$cell_polity, "cell_polity", "area_code") |>
    .urban_resolve_area_code("cell_polity") |>
    .tp_prepare_polity()
  snapshots <- .tp_snapshot_years(data$hyde, hyde_dir)
  plan <- .tp_share_plan(years, snapshots)
  wpp <- .tp_wpp_level(
    data$wpp %||% read_wpp_population(years = years, by = "total"),
    years
  )
  shares <- .tp_all_snapshot_shares(plan, data$hyde, hyde_dir, polity)
  interpolated <- .tp_interpolate_shares(plan, shares)
  placed <- .tp_apply_level(interpolated$shares, wpp)
  coverage <- .tp_coverage(plan, snapshots, placed, interpolated)
  .tp_report(coverage)
  out <- placed$rows |>
    .resolve_polity_validity(polity_validity) |>
    tibble::as_tibble()
  attr(out, "coverage") <- coverage
  out
}

# ---- Private helpers: inputs --------------------------------------------

.tp_check_years <- function(years) {
  if (is.null(years) || length(years) == 0L) {
    cli::cli_abort(c(
      "{.arg years} must be specified.",
      i = "Each HYDE snapshot is a real ~30 MB archive; pass the calendar
           year(s) you need explicitly."
    ))
  }
  if (!is.numeric(years) || anyNA(years) || any(years != trunc(years))) {
    cli::cli_abort("{.arg years} must be whole calendar years.")
  }
  sort(unique(as.integer(years)))
}

# The caller's crosswalk, with `polity_frac` defaulting to 1 as
# build_urban_n() defaults it, and each grid code's bucket attached -- the key
# WPP totals are published on -- by the same rule .cell_polity_to_bucket()
# applies. A missing area code would join no WPP total and leave its people
# unplaced without saying so.
.tp_prepare_polity <- function(polity) {
  if (anyNA(polity$area_code)) {
    cli::cli_abort(c(
      "{.field area_code} in {.arg data$cell_polity} holds
       {sum(is.na(polity$area_code))} missing value{?s}.",
      i = "A polycell keyed on NA joins no WPP total."
    ))
  }
  if (!rlang::has_name(polity, "polity_frac")) {
    polity <- dplyr::mutate(polity, polity_frac = 1)
  }
  lookup <- .cell_polity_bucket_lookup()
  polity |>
    dplyr::transmute(
      lon = as.numeric(.data$lon),
      lat = as.numeric(.data$lat),
      area_code = .data$area_code,
      polity_frac = as.numeric(.data$polity_frac),
      bucket = dplyr::coalesce(
        lookup$polity_area_code[match(.data$area_code, lookup$area_code)],
        .data$area_code
      )
    )
}

# Which snapshots exist is MEASURED, never assumed from HYDE's documented
# cadence: it changes across the archive (centennial, decadal, annual), and the
# file set is what decides what can be bracketed.
.tp_snapshot_years <- function(hyde, hyde_dir) {
  if (!is.null(hyde)) {
    .check_columns(hyde, c("lon", "lat", "year", "popc"), "data$hyde")
    return(sort(unique(as.integer(hyde$year))))
  }
  dir <- .resolve_hyde_dir(hyde_dir)
  files <- list.files(dir, pattern = "^[0-9]+AD_pop\\.zip$")
  if (length(files) == 0L) {
    cli::cli_abort(
      "No HYDE {.file {{year}}AD_pop.zip} archives in {.file {dir}}."
    )
  }
  sort(as.integer(sub("AD_pop\\.zip$", "", files)))
}

# One requested year per row, and the snapshots it is built from:
#   exact        -- the year is a snapshot (weight 0, lower == upper);
#   interpolated -- bracketed; the weight is the year's distance from the
#                   lower snapshot over the bracket's width;
#   held         -- past the last snapshot; both ends are that snapshot.
# A year before the first snapshot has no bracket and is refused before any
# archive is opened.
.tp_share_plan <- function(years, snapshots) {
  before <- years[years < min(snapshots)]
  if (length(before) > 0L) {
    cli::cli_abort(
      c(
        "{cli::qty(length(before))}Year{?s} {.val {before}} lie{?s/} before
         the first HYDE snapshot ({min(snapshots)}).",
        i = "There is no bracket to interpolate the population pattern from,
             and extrapolating one backwards is a reconstruction, not an
             interpolation."
      ),
      class = "whep_total_population_uncovered"
    )
  }
  last <- max(snapshots)
  lower <- purrr::map_int(years, \(y) max(snapshots[snapshots <= y]))
  upper <- purrr::map_int(
    years,
    \(y) if (y > last) last else min(snapshots[snapshots >= y])
  )
  tibble::tibble(
    year = years,
    lower = lower,
    upper = upper,
    weight = dplyr::if_else(
      upper == lower,
      0,
      (years - lower) / (upper - lower)
    ),
    status = dplyr::case_when(
      years %in% snapshots ~ "exact",
      years > last ~ "held",
      .default = "interpolated"
    )
  )
}

# The WPP level, one row per year and bucket. A requested year with no WPP row
# has no total to place and is refused rather than extrapolated.
.tp_wpp_level <- function(wpp, years) {
  .check_columns(wpp, c("year", "area_code", "population"), "data$wpp")
  level <- wpp |>
    dplyr::filter(.data$year %in% years) |>
    dplyr::summarise(
      population = sum(.data$population),
      .by = c("year", "area_code")
    ) |>
    dplyr::transmute(
      year = as.integer(.data$year),
      bucket = as.integer(.data$area_code),
      wpp_population = .data$population
    )
  missing <- setdiff(years, level$year)
  if (length(missing) > 0L) {
    cli::cli_abort(
      c(
        "The WPP table carries no row for {cli::qty(length(missing))}
         year{?s} {.val {missing}}.",
        i = "WPP 2024 starts in 1950; a year outside it has no country total
             to place, and extrapolating one is a reconstruction."
      ),
      class = "whep_total_population_uncovered"
    )
  }
  level
}

# ---- Private helpers: shares --------------------------------------------

.tp_all_snapshot_shares <- function(plan, hyde, hyde_dir, polity) {
  needed <- sort(unique(c(plan$lower, plan$upper)))
  dir <- if (is.null(hyde)) .resolve_hyde_dir(hyde_dir) else NULL
  needed |>
    purrr::map(\(snapshot) {
      .tp_snapshot_grid(snapshot, hyde, dir) |>
        .tp_snapshot_shares(polity)
    }) |>
    rlang::set_names(needed)
}

# One snapshot's `popc` on the 0.5-degree grid, from the injected table or
# from the archive through read_hyde_population()'s own parse and block sum.
.tp_snapshot_grid <- function(snapshot, hyde, dir) {
  if (!is.null(hyde)) {
    return(
      hyde |>
        dplyr::filter(.data$year == snapshot) |>
        dplyr::transmute(
          lon = as.numeric(.data$lon),
          lat = as.numeric(.data$lat),
          year = as.integer(.data$year),
          pop = as.numeric(.data$popc)
        )
    )
  }
  .read_hyde_year(snapshot, dir, variable = "popc") |>
    tibble::as_tibble() |>
    dplyr::transmute(
      .data$lon,
      .data$lat,
      year = as.integer(.data$year),
      .data$pop
    )
}

# Each polycell's share of its bucket's HYDE population at one snapshot. A
# cell's count is split across the areas holding it by `polity_frac`, the same
# split build_urban_n() applies. A bucket whose whole count is zero has no
# pattern (0/0) and is left for the level step to report as unplaced. The
# grid is one snapshot, and the groups carry its `year` so each share is
# visibly that snapshot's; the year is dropped on the way out because the
# interpolation step pairs two snapshots by their plan row, not by year.
.tp_snapshot_shares <- function(grid, polity) {
  polity |>
    dplyr::inner_join(grid, by = c("lon", "lat")) |>
    dplyr::summarise(
      pop_cp = sum(.data$pop * .data$polity_frac),
      .by = c("lon", "lat", "area_code", "bucket", "year")
    ) |>
    dplyr::mutate(
      bucket_total = sum(.data$pop_cp),
      .by = c("bucket", "year")
    ) |>
    dplyr::filter(.data$bucket_total > 0, .data$pop_cp > 0) |>
    dplyr::transmute(
      .data$lon,
      .data$lat,
      .data$area_code,
      .data$bucket,
      share = .data$pop_cp / .data$bucket_total
    )
}

# Every requested year's shares, from the two snapshot tables its plan row
# names, then renormalised per bucket-year. The full join with zero fill keeps
# a polycell present at only one end. A bucket present at only one bracketing
# snapshot sums to its weight rather than to 1; that is renormalised and
# reported, never silently rescaled.
.tp_interpolate_shares <- function(plan, shares) {
  keys <- c("lon", "lat", "area_code", "bucket")
  raw <- purrr::pmap(plan, \(year, lower, upper, weight, status) {
    dplyr::full_join(
      dplyr::rename(shares[[as.character(lower)]], share_lo = "share"),
      dplyr::rename(shares[[as.character(upper)]], share_hi = "share"),
      by = keys
    ) |>
      dplyr::transmute(
        dplyr::across(dplyr::all_of(keys)),
        year = !!year,
        share = (1 - !!weight) *
          dplyr::coalesce(.data$share_lo, 0) +
          !!weight * dplyr::coalesce(.data$share_hi, 0)
      ) |>
      dplyr::filter(.data$share > 0)
  }) |>
    dplyr::bind_rows() |>
    dplyr::mutate(share_sum = sum(.data$share), .by = c("year", "bucket"))
  gaps <- raw |>
    dplyr::distinct(.data$year, .data$bucket, .data$share_sum) |>
    dplyr::filter(abs(.data$share_sum - 1) > 1e-9)
  list(
    shares = raw |>
      dplyr::mutate(share = .data$share / .data$share_sum) |>
      dplyr::select(-"share_sum"),
    gaps = gaps
  )
}

# ---- Private helpers: level x shape -------------------------------------

.tp_apply_level <- function(shares, wpp) {
  rows <- shares |>
    dplyr::inner_join(wpp, by = c("year", "bucket")) |>
    dplyr::transmute(
      .data$lon,
      .data$lat,
      .data$area_code,
      .data$year,
      population = .data$wpp_population * .data$share
    ) |>
    dplyr::filter(.data$population > 0) |>
    dplyr::arrange(.data$year, .data$lon, .data$lat, .data$area_code)
  placed_keys <- dplyr::distinct(shares, .data$year, .data$bucket)
  list(
    rows = rows,
    unplaced = wpp |>
      dplyr::anti_join(placed_keys, by = c("year", "bucket")) |>
      dplyr::filter(.data$wpp_population > 0) |>
      dplyr::arrange(.data$year, .data$bucket),
    unmatched_hyde = placed_keys |>
      dplyr::anti_join(wpp, by = c("year", "bucket")) |>
      dplyr::arrange(.data$year, .data$bucket)
  )
}

.tp_coverage <- function(plan, snapshots, placed, interpolated) {
  held <- plan$year[plan$status == "held"]
  list(
    population_basis = "total",
    level_basis = "total_population",
    level_source = "read_wpp_population(by = \"total\"), UN WPP 2024 medium",
    pattern_variable = "popc",
    pattern_source = "HYDE baseline popc",
    method = paste(
      "polycell share of the bucket's HYDE popc, interpolated linearly",
      "between the nearest snapshots and held past the last; WPP sets the",
      "level"
    ),
    years = plan$year,
    exact = plan$year[plan$status == "exact"],
    interpolated = plan$year[plan$status == "interpolated"],
    held = held,
    held_at = if (length(held) > 0L) max(snapshots) else NA_integer_,
    snapshots_available = snapshots,
    snapshots_used = sort(unique(c(plan$lower, plan$upper))),
    plan = plan,
    unplaced = placed$unplaced,
    unmatched_hyde = placed$unmatched_hyde,
    share_gaps = interpolated$gaps
  )
}

# Say what could not be placed, and which years carry a held pattern. A held
# year is a deliberate choice, so it informs; unplaced people and renormalised
# shares are defects in coverage, so they warn.
.tp_report <- function(coverage) {
  if (length(coverage$held) > 0L) {
    cli::cli_inform(c(
      i = "{cli::qty(length(coverage$held))}Year{?s} {.val {coverage$held}}
           {?is/are} past the last HYDE snapshot and carr{?ies/y} its
           {coverage$held_at} population pattern under the current WPP
           total{?s}."
    ))
  }
  if (nrow(coverage$unplaced) > 0L) {
    cli::cli_warn(c(
      "!" = "{nrow(coverage$unplaced)} country-year{?s} hold a WPP total but
             no HYDE population pattern;
             {signif(sum(coverage$unplaced$wpp_population), 6)} persons were
             not placed on the grid.",
      i = "See {.code attr(x, \"coverage\")$unplaced}."
    ))
  }
  if (nrow(coverage$unmatched_hyde) > 0L) {
    cli::cli_warn(c(
      "!" = "{nrow(coverage$unmatched_hyde)} country-year{?s} hold HYDE
             population but no WPP total; their cells carry no population.",
      i = "See {.code attr(x, \"coverage\")$unmatched_hyde}."
    ))
  }
  if (nrow(coverage$share_gaps) > 0L) {
    cli::cli_warn(c(
      "!" = "{nrow(coverage$share_gaps)} country-year{?s} had interpolated
             shares summing away from 1 and {?was/were} renormalised.",
      i = "A country present at only one bracketing HYDE snapshot; see
           {.code attr(x, \"coverage\")$share_gaps}."
    ))
  }
  invisible(coverage)
}

# Toy fixture for a runnable example (one Spanish polycell, one year; the
# cell the other urban fixtures use).
.example_total_population_grid <- function() {
  tibble::tribble(
    ~lon, ~lat, ~area_code, ~year, ~population,
    -0.25, -0.25, 203L, 2010L, 46.6e6
  ) |>
    .add_reporting_polity_columns()
}
