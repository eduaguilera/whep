# diagnose_polycell_support.R
#
# Re-derives every preflight number quoted as evidence for the polycell
# spatial-support work (issue #423, and the mechanism behind #276). The
# numbers were first measured interactively; this script exists so they are
# reproducible by anyone with the same inputs, and so a later change to the
# grid, the crosswalk or the LUH2 vintage shows up as a moved number rather
# than as a silent drift.
#
# What it reports:
#   EA1  sum(12 LUH2 states) + icwtr == 1 over the native 0.25-degree cells,
#        and LUH2's own `carea` against .luh2_native_cell_area_ha().
#   EA2  the whole-cell-area convention against LUH2 terrestrial area on the
#        0.5-degree grid.
#   EA3  cell_polity_fraction.parquet: shape, the per-cell polity_frac
#        partition, and its area base.
#   EA6  border-cell cost: polities per cell, shared-land polities, row cost.
#   Z    what the zero-terrestrial crosswalk cells and the zero-land island
#        states actually are.
#   H    whether the HaNi deposition mass is referenced to the whole grid
#        cell or to the land inside it (the A2 deposition migration depends
#        on this).
#   P    the polity vocabulary a geometry consumer must filter on.
#
# Run:
#   Rscript inst/scripts/diagnose_polycell_support.R
#
# Inputs, all resolved from environment variables (never hardcode the path):
#   WHEP_LUH2_DIR             states.nc + staticData_quarterdeg.nc. REQUIRED.
#                             Report which LUH2 vintage it holds: the base
#                             v2h release stops at 2015, the Global Carbon
#                             Budget variants run further, and EA1's residual
#                             differs between them. The time origin is read
#                             from the file's own `time` units, never assumed,
#                             because DA-9 makes the vintage selectable.
#   WHEP_POLITY_FRACTION_PATH cell_polity_fraction.parquet. REQUIRED.
#   WHEP_HANI_DIR             ndep_nhx.nc. Optional; section H is skipped
#                             when unset.
#   WHEP_NE_COUNTRIES_PATH    ne_10m_admin_0_countries.shp. Optional; the
#                             Natural Earth half of section Z is skipped when
#                             unset. No download is performed.
#   WHEP_POLITIES_PATH        A polity table to check INSTEAD of the one this
#                             build ships: an `sf` object saved with
#                             `saveRDS()`, or any layer `sf::st_read()` opens.
#                             Optional. Section P's findings are properties of
#                             a polity VINTAGE, not of this package, so the
#                             upstream integration table has to be reachable
#                             without rebuilding the package around it.
#   WHEP_DIAGNOSE_YEAR        Evidence year for every section. Optional,
#                             default 2015. The plan's HaNi figures in DA-10
#                             and EA9 (34.77 Tg NHx, coastal median 0.9937)
#                             were measured at 2014, so pass 2014 to reproduce
#                             those specific numbers.
#
# Note for anyone whose environment variables look unset: R reads a `.Renviron`
# in the working directory INSTEAD of `~/.Renviron`. Run with R_ENVIRON_USER
# pointing at the user file, or export the variables in the shell.

suppressPackageStartupMessages({
  library(data.table)
})

.pc_states <- c(
  "c3ann",
  "c4ann",
  "c3per",
  "c4per",
  "c3nfx",
  "pastr",
  "range",
  "primf",
  "secdf",
  "primn",
  "secdn",
  "urban"
)

# ---- Small shared helpers ---------------------------------------------------

# Abort naming the environment variable rather than inventing a default.
.pc_require_env <- function(var, what) {
  path <- Sys.getenv(var, "")
  if (!nzchar(path) || !file.exists(path)) {
    cli::cli_abort(c(
      "No {what} available.",
      i = "Set {.envvar {var}} to an existing path."
    ))
  }
  path
}

.pc_optional_env <- function(var) {
  path <- Sys.getenv(var, "")
  if (nzchar(path) && file.exists(path)) path else NULL
}

# One evidence year for every section. Section H used to carry its own
# hardcoded 2014 while the rest of the script ran at 2015, so two numbers in
# the same report described two different years without saying so.
.pc_evidence_year <- function(default = 2015L) {
  raw <- Sys.getenv("WHEP_DIAGNOSE_YEAR", "")
  if (!nzchar(raw)) {
    return(default)
  }
  year <- suppressWarnings(as.integer(raw))
  if (is.na(year)) {
    cli::cli_abort("{.envvar WHEP_DIAGNOSE_YEAR} is not a year: {.val {raw}}.")
  }
  year
}

# The calendar years a NetCDF time axis covers, READ from the file rather than
# assumed. CF units are "<interval> since <origin>"; only a yearly interval is
# handled, and anything else aborts instead of being misread confidently. This
# matters because DA-9 makes the LUH2 vintage a selectable input: a tree with a
# different origin would otherwise be sliced at the wrong year and every number
# below it would still look plausible.
.pc_nc_years <- function(nc, what) {
  units <- nc$dim$time$units
  origin <- stringr::str_match(units, "^\\s*years\\s+since\\s+(-?[0-9]+)-")
  if (is.na(origin[1, 1])) {
    cli::cli_abort(c(
      "{what}'s time axis is not expressed in years since a calendar year.",
      i = "Its {.field units} attribute reads {.val {units}}."
    ))
  }
  as.integer(origin[1, 2]) + as.integer(round(nc$dim$time$vals))
}

# Position of `year` on a time axis, or an abort naming the covered span.
.pc_time_index <- function(years, year, what) {
  idx <- match(year, years)
  if (is.na(idx)) {
    cli::cli_abort(
      "Year {year} is outside {what}'s time axis
       ({min(years)}-{max(years)})."
    )
  }
  idx
}

# Spherical area (ha) of a cell of side `step` degrees centred at `lat`. The
# 0.25 and 0.5 degree cases reproduce .luh2_native_cell_area_ha() and
# .cell_area_ha_lat() exactly; they are restated here so the script stays
# runnable without loading the package.
.pc_cell_area_ha <- function(lat, step) {
  earth_radius_m <- 6371000
  half_rad <- step / 2 * pi / 180
  lon_step_rad <- step * pi / 180
  band <- sin(lat * pi / 180 + half_rad) - sin(lat * pi / 180 - half_rad)
  earth_radius_m^2 * lon_step_rad * band / 1e4
}

# The 0.5-degree cell centre a coordinate falls in, matching
# .luh2_aggregate_half_degree().
.pc_half_deg_centre <- function(coord) {
  floor((coord + 180) / 0.5) * 0.5 - 180 + 0.25
}

.pc_h1 <- function(x) cli::cli_h1(x)
.pc_h2 <- function(x) cli::cli_h2(x)

# Report one reproduced number against the value the plan records.
.pc_check <- function(label, got, expected, fmt = "%s") {
  cli::cli_text(sprintf(
    "{.strong %s}  plan %s  |  reproduced %s%s",
    label,
    sprintf(fmt, expected),
    sprintf(fmt, got),
    if (isTRUE(all.equal(got, expected))) "" else "   <-- DIFFERS"
  ))
}

# ---- Input readers ----------------------------------------------------------

# The 12 state fractions at `year`, summed per native 0.25-degree cell, plus
# LUH2's own ice/water fraction and cell area. `ncvar_get` returns [lon, lat]
# and as.vector() runs lon fastest, so the coordinate table is CJ(lat, lon).
.pc_read_luh2 <- function(luh2_dir, year) {
  nc <- ncdf4::nc_open(file.path(luh2_dir, "states.nc"))
  on.exit(ncdf4::nc_close(nc), add = TRUE)
  lon <- ncdf4::ncvar_get(nc, "lon")
  lat <- ncdf4::ncvar_get(nc, "lat")
  years <- .pc_nc_years(nc, "states.nc")
  cli::cli_alert_info(
    "LUH2 states.nc: {length(years)} time steps, years
     {min(years)}-{max(years)}, from {.val {nc$dim$time$units}}."
  )
  time_idx <- .pc_time_index(years, year, "states.nc")
  states_sum <- NULL
  for (state in .pc_states) {
    slab <- ncdf4::ncvar_get(
      nc,
      state,
      start = c(1L, 1L, time_idx),
      count = c(-1L, -1L, 1L)
    )
    states_sum <- if (is.null(states_sum)) slab else states_sum + slab
  }
  static <- .pc_read_luh2_static(luh2_dir, lon, lat)
  grid <- data.table::CJ(lat = lat, lon = lon, sorted = FALSE)
  grid[, states_sum := as.vector(states_sum)]
  grid[, icwtr := static$icwtr]
  grid[, carea_ha := static$carea_ha]
  grid[, terrestrial_ha := (1 - icwtr) * carea_ha]
  grid[, lon5 := .pc_half_deg_centre(lon)]
  grid[, lat5 := .pc_half_deg_centre(lat)]
  grid[]
}

.pc_read_luh2_static <- function(luh2_dir, lon, lat) {
  st <- ncdf4::nc_open(file.path(luh2_dir, "staticData_quarterdeg.nc"))
  on.exit(ncdf4::nc_close(st), add = TRUE)
  if (
    !identical(ncdf4::ncvar_get(st, "lon"), lon) ||
      !identical(ncdf4::ncvar_get(st, "lat"), lat)
  ) {
    cli::cli_abort("staticData_quarterdeg.nc is not on the states.nc grid.")
  }
  list(
    icwtr = as.vector(ncdf4::ncvar_get(st, "icwtr")),
    carea_ha = as.vector(ncdf4::ncvar_get(st, "carea")) * 100
  )
}

.pc_read_crosswalk <- function(path) {
  raw <- nanoparquet::read_parquet(path)
  data.table::as.data.table(raw)
}

# ---- EA1 --------------------------------------------------------------------

.pc_ea1 <- function(grid) {
  .pc_h1("EA1 - LUH2 confirms the terrestrial mask exactly")
  land <- grid[!is.na(states_sum) & !is.na(icwtr)]
  land[, residual := states_sum + icwtr - 1]
  .pc_check("native cells", nrow(land), 258539L, "%d")
  .pc_check(
    "residual mean",
    signif(mean(land$residual), 2),
    5.1e-10,
    "%.2g"
  )
  cli::cli_text("residual median: {stats::median(land$residual)}")
  cli::cli_text(sprintf(
    "residual range: [%.3g, %.3g]  (plan [-5.6e-07, 2.4e-05])",
    min(land$residual),
    max(land$residual)
  ))
  .pc_check(
    "cells with |residual| >= 1e-3",
    land[abs(residual) >= 1e-3, .N],
    0L,
    "%d"
  )

  .pc_h2("LUH2 carea against the package spherical formula")
  for (scope in c("all cells", "states-bearing cells")) {
    sub <- if (scope == "all cells") {
      grid[!is.na(carea_ha)]
    } else {
      grid[!is.na(carea_ha) & !is.na(states_sum)]
    }
    ratio <- sub$carea_ha / .pc_cell_area_ha(sub$lat, 0.25)
    cli::cli_text(sprintf(
      "%-21s n=%d  mean=%.6f  range=[%.6f, %.6f]",
      scope,
      length(ratio),
      mean(ratio),
      min(ratio),
      max(ratio)
    ))
  }
  cli::cli_alert_info(
    "The plan's 1.000002 / [0.999989, 1.000404] is the all-cells row."
  )
  invisible(land)
}

# ---- EA2 --------------------------------------------------------------------

# Aggregate the native grid to 0.5 degrees. `cell_ha` is the package's
# spherical 0.5-degree area (what build_cell_polity() multiplies by);
# `carea_sum_ha` is LUH2's own area summed over the four native subcells.
.pc_half_degree <- function(grid) {
  agg <- grid[,
    .(
      terrestrial_ha = sum(terrestrial_ha, na.rm = TRUE),
      carea_sum_ha = sum(carea_ha, na.rm = TRUE),
      n_native_with_states = sum(!is.na(states_sum))
    ),
    by = .(lon = lon5, lat = lat5)
  ]
  agg[, cell_ha := .pc_cell_area_ha(lat, 0.5)]
  agg[]
}

.pc_ea2 <- function(agg) {
  .pc_h1("EA2 - the whole-cell convention over-counts")
  pos <- agg[terrestrial_ha > 0]
  .pc_check("0.5-degree cells with terrestrial area", nrow(pos), 64493L, "%d")
  .pc_check(
    "global terrestrial (Gha)",
    round(sum(pos$terrestrial_ha) / 1e9, 4),
    12.9931,
    "%.4f"
  )
  .pc_check(
    "whole-cell area of those cells (Gha)",
    round(sum(pos$cell_ha) / 1e9, 4),
    14.3195,
    "%.4f"
  )
  .pc_check(
    "ratio",
    round(sum(pos$cell_ha) / sum(pos$terrestrial_ha), 4),
    1.1021,
    "%.4f"
  )

  .pc_h2("terrestrial fraction over land-bearing cells")
  pos[, frac := terrestrial_ha / cell_ha]
  pos[, frac_carea := terrestrial_ha / carea_sum_ha]
  .pc_check("median", round(stats::median(pos$frac), 3), 1.000, "%.3f")
  .pc_check("mean", round(mean(pos$frac), 3), 0.889, "%.3f")
  .pc_check(
    "10th percentile",
    round(unname(stats::quantile(pos$frac, 0.10)), 3),
    0.476,
    "%.3f"
  )
  .pc_check(
    "1st percentile",
    round(unname(stats::quantile(pos$frac, 0.01)), 3),
    0.029,
    "%.3f"
  )

  .pc_h2("cells below a terrestrial-fraction threshold")
  expected <- c(20927L, 15145L, 9561L, 6677L)
  thresholds <- c(0.99, 0.95, 0.75, 0.50)
  total <- sum(pos$terrestrial_ha)
  for (i in seq_along(thresholds)) {
    thr <- thresholds[i]
    sub <- pos[frac < thr]
    cli::cli_text(sprintf(
      paste0(
        "below %.2f: n=%d (plan %d)%s  share_cells=%.1f%%  ",
        "share_terr=%.1f%%  [n with LUH2 carea denominator: %d]"
      ),
      thr,
      nrow(sub),
      expected[i],
      if (nrow(sub) == expected[i]) "" else " <-- DIFFERS",
      100 * nrow(sub) / nrow(pos),
      100 * sum(sub$terrestrial_ha) / total,
      pos[frac_carea < thr, .N]
    ))
  }
  invisible(pos)
}

# ---- EA3 --------------------------------------------------------------------

.pc_ea3 <- function(xw, agg) {
  .pc_h1("EA3 - polity_frac partitions the land, over the wrong base")
  cells <- unique(xw[, .(lon, lat)])
  .pc_check("rows", nrow(xw), 68527L, "%d")
  .pc_check("cells", nrow(cells), 64438L, "%d")
  .pc_check("polities", data.table::uniqueN(xw$area_code), 191L, "%d")

  sums <- xw[, .(total = sum(polity_frac)), by = .(lon, lat)]
  cli::cli_text(sprintf(
    "per-cell polity_frac sum: min %.10f  max %.10f",
    min(sums$total),
    max(sums$total)
  ))
  .pc_check(
    "share of cells summing to 1 within 1e-9",
    round(mean(abs(sums$total - 1) < 1e-9), 4),
    1.0,
    "%.4f"
  )

  cells[, cell_ha := .pc_cell_area_ha(lat, 0.5)]
  cells <- merge(
    cells,
    agg[, .(lon, lat, terrestrial_ha, n_native_with_states)],
    by = c("lon", "lat"),
    all.x = TRUE
  )
  cells[is.na(terrestrial_ha), terrestrial_ha := 0]
  cells[is.na(n_native_with_states), n_native_with_states := 0L]
  .pc_check(
    "crosswalk whole-cell area (Gha)",
    round(sum(cells$cell_ha) / 1e9, 4),
    14.3659,
    "%.4f"
  )
  .pc_check(
    "crosswalk LUH2 terrestrial area (Gha)",
    round(sum(cells$terrestrial_ha) / 1e9, 4),
    12.9435,
    "%.4f"
  )
  .pc_check(
    "ratio",
    round(sum(cells$cell_ha) / sum(cells$terrestrial_ha), 4),
    1.1099,
    "%.4f"
  )

  .pc_h2("land the crosswalk does not carry (DA-7 unclaimed diagnostic)")
  outside <- agg[terrestrial_ha > 0][
    !cells,
    on = c("lon", "lat")
  ]
  cli::cli_text(
    "LUH2 terrestrial cells absent from the crosswalk: {nrow(outside)}"
  )
  cli::cli_text(sprintf(
    "their terrestrial area: %.4f Gha (%.2f%% of global land)",
    sum(outside$terrestrial_ha) / 1e9,
    100 * sum(outside$terrestrial_ha) / sum(agg$terrestrial_ha)
  ))
  cli::cli_text(sprintf(
    "their latitude range: [%.2f, %.2f]",
    min(outside$lat),
    max(outside$lat)
  ))
  invisible(cells)
}

# ---- EA6 --------------------------------------------------------------------

.pc_ea6 <- function(xw, cells, agg) {
  .pc_h1("EA6 - what the border problem is worth")
  per_cell <- xw[, .(n_polities = .N), by = .(lon, lat)]
  counts <- per_cell[, .N, by = n_polities][order(n_polities)]
  print(counts)
  expected <- c(60513L, 3764L, 158L, 3L)
  got <- counts$N[match(1:4, counts$n_polities)]
  .pc_check(
    "cells with 1 / 2 / 3 / 4 polities",
    paste(got, collapse = " / "),
    paste(expected, collapse = " / ")
  )

  multi <- per_cell[n_polities > 1L]
  .pc_check("multi-polity cells", nrow(multi), 3925L, "%d")
  cli::cli_text(sprintf(
    "share of crosswalk cells: %.1f%%",
    100 * nrow(multi) / nrow(per_cell)
  ))
  shared_cells <- merge(multi, cells, by = c("lon", "lat"))
  .pc_check(
    "share of global terrestrial area (%)",
    round(100 * sum(shared_cells$terrestrial_ha) / sum(agg$terrestrial_ha), 1),
    7.4,
    "%.1f"
  )

  by_polity <- .pc_shared_land_by_polity(xw, cells, per_cell)
  .pc_check(
    "polities with >50% of their land in shared cells",
    by_polity[shared_share > 0.5, .N],
    23L,
    "%d"
  )
  cli::cli_text(
    "polities with no terrestrial area at all: \\
     {by_polity[is.na(shared_share), .N]}"
  )
  .pc_check(
    "row cost of the polycell unit (%)",
    round(100 * (nrow(xw) / nrow(per_cell) - 1), 1),
    6.3,
    "%.1f"
  )
  invisible(by_polity)
}

# Each polity's land, and the part of it sitting in a border-shared cell.
# Weighted by LUH2 terrestrial area, which is the quantity "their land"
# refers to; the cell-area weighting is reported alongside because it gives
# a different answer.
.pc_shared_land_by_polity <- function(xw, cells, per_cell) {
  joined <- merge(xw, cells, by = c("lon", "lat"))
  joined <- merge(joined, per_cell, by = c("lon", "lat"))
  joined[, shared := n_polities > 1L]
  out <- joined[,
    .(
      land_ha = sum(terrestrial_ha * polity_frac),
      shared_land_ha = sum(terrestrial_ha * polity_frac * shared),
      cell_ha_w = sum(cell_ha * polity_frac),
      shared_cell_ha_w = sum(cell_ha * polity_frac * shared)
    ),
    by = area_code
  ]
  out[,
    shared_share := data.table::fifelse(
      land_ha > 0,
      shared_land_ha / land_ha,
      NA_real_
    )
  ]
  out[, shared_share_cellbase := shared_cell_ha_w / cell_ha_w]
  cli::cli_text(
    "same count on a cell-area basis: \\
     {out[shared_share_cellbase > 0.5, .N]}"
  )
  out[]
}

# ---- Z: what the zero-terrestrial cells are ---------------------------------

.pc_zero_cells <- function(grid, xw, cells, ne_path) {
  .pc_h1("Z - the zero-terrestrial cells and the zero-land island states")
  .pc_h2("LUH2's own masks")
  no_states <- grid[is.na(states_sum)]
  cli::cli_text(sprintf(
    "native cells with no state field: %d, icwtr range [%g, %g]",
    nrow(no_states),
    min(no_states$icwtr),
    max(no_states$icwtr)
  ))
  with_states <- grid[!is.na(states_sum)]
  cli::cli_text(sprintf(
    "native cells with a state field but icwtr == 1: %d of %d",
    with_states[icwtr == 1, .N],
    nrow(with_states)
  ))

  zero <- cells[terrestrial_ha == 0]
  .pc_h2("crosswalk cells carrying no LUH2 terrestrial area")
  .pc_check("zero-terrestrial crosswalk cells", nrow(zero), 1239L, "%d")
  cli::cli_text(
    "of which LUH2 has no state field in any native subcell: \\
     {zero[n_native_with_states == 0L, .N]}"
  )
  cli::cli_text(
    "of which LUH2 has state fields that are wholly ice/water: \\
     {zero[n_native_with_states > 0L, .N]}"
  )
  cli::cli_text(sprintf(
    "their whole-cell area: %.2f Mha; median latitude %.2f",
    sum(zero$cell_ha) / 1e6,
    stats::median(zero$lat)
  ))

  if (!is.null(ne_path)) {
    zero <- .pc_natural_earth_land(zero, ne_path)
  } else {
    cli::cli_alert_warning(
      "WHEP_NE_COUNTRIES_PATH unset: skipping the Natural Earth land test."
    )
  }

  .pc_h2("polities whose whole territory carries zero LUH2 terrestrial area")
  totals <- merge(xw, cells, by = c("lon", "lat"))[,
    .(
      polycells = .N,
      land_ha = sum(terrestrial_ha * polity_frac),
      cell_ha = sum(cell_ha * polity_frac)
    ),
    by = area_code
  ]
  print(totals[land_ha == 0][order(-polycells)])
  invisible(zero)
}

# Measure how much Natural Earth country land sits inside each zero cell.
# Natural Earth is used here only as an independent land witness; nothing is
# downloaded and no layer is added to the pipeline.
.pc_natural_earth_land <- function(zero, ne_path) {
  .pc_h2("Natural Earth country land inside those cells")
  countries <- sf::st_read(ne_path, quiet = TRUE) |> sf::st_make_valid()
  land <- sf::st_union(sf::st_geometry(countries))
  cells_sf <- .pc_cells_to_sf(as.data.frame(zero))
  overlap <- suppressWarnings(sf::st_intersection(cells_sf, land))
  overlap$ne_land_ha <- as.numeric(sf::st_area(overlap)) / 1e4
  by_cell <- data.table::as.data.table(sf::st_drop_geometry(overlap))[,
    .(ne_land_ha = sum(ne_land_ha)),
    by = .(lon, lat)
  ]
  zero <- merge(zero, by_cell, by = c("lon", "lat"), all.x = TRUE)
  zero[is.na(ne_land_ha), ne_land_ha := 0]
  cli::cli_text("cells with no Natural Earth land: {zero[ne_land_ha <= 0, .N]}")
  cli::cli_text("cells with Natural Earth land: {zero[ne_land_ha > 0, .N]}")
  cli::cli_text(sprintf(
    "Natural Earth land in them: %.2f Mha, against %.2f Mha of whole cell",
    sum(zero$ne_land_ha) / 1e6,
    sum(zero$cell_ha) / 1e6
  ))
  zero[, ne_frac := ne_land_ha / cell_ha]
  print(zero[,
    .(cells = .N, median_ne_frac = round(stats::median(ne_frac), 4)),
    by = .(
      band = cut(ne_frac, c(-Inf, 0, 0.01, 0.1, 0.5, Inf))
    )
  ][order(band)])
  zero[]
}

.pc_cells_to_sf <- function(d) {
  polys <- purrr::map2(d$lon, d$lat, \(x, y) {
    sf::st_polygon(list(cbind(
      c(x - 0.25, x + 0.25, x + 0.25, x - 0.25, x - 0.25),
      c(y - 0.25, y - 0.25, y + 0.25, y + 0.25, y - 0.25)
    )))
  })
  sf::st_sf(d, geometry = sf::st_sfc(polys, crs = 4326))
}

# ---- H: HaNi flux referencing -----------------------------------------------

# build_n_deposition() divides a HaNi per-cell mass by the WHOLE 0.5-degree
# cell area. Whether that is right depends on what the mass is referenced to,
# which the NetCDF long_name alone does not settle ("deposition to land within
# the grid cell" names the domain, not a denominator). HaNi is land-masked, so
# a coastal 5-arcmin cell is only partly land. If the mass were scaled by the
# land fraction, dividing by the whole cell area would collapse coastal cells
# towards their land fraction (~0.5 on average) relative to their inland
# neighbours. If it is referenced to the whole cell, coastal and inland cells
# agree. The control run (interior against interior) sets the null.
.pc_hani <- function(hani_dir, year) {
  .pc_h1("H - what the HaNi deposition mass is referenced to")
  path <- file.path(hani_dir, "ndep_nhx.nc")
  if (!file.exists(path)) {
    cli::cli_alert_warning("No ndep_nhx.nc under WHEP_HANI_DIR; skipping.")
    return(invisible(NULL))
  }
  nc <- ncdf4::nc_open(path)
  on.exit(ncdf4::nc_close(nc), add = TRUE)
  attrs <- ncdf4::ncatt_get(nc, "ndep_nhx")
  cli::cli_text("long_name: {.val {attrs$long_name}}")
  cli::cli_text("units: {.val {attrs$units}}")
  years <- .pc_nc_years(nc, "ndep_nhx.nc")
  cli::cli_text(
    "time axis: {min(years)}-{max(years)}, from {.val {nc$dim$time$units}}"
  )
  lat <- ncdf4::ncvar_get(nc, "lat")
  values <- ncdf4::ncvar_get(
    nc,
    "ndep_nhx",
    start = c(1L, 1L, .pc_time_index(years, year, "ndep_nhx.nc")),
    count = c(-1L, -1L, 1L)
  )
  cli::cli_text(sprintf(
    "%d: cells %d, NA (ocean) %d, positive %d, global sum %.2f Tg N",
    year,
    length(values),
    sum(is.na(values)),
    sum(values > 0, na.rm = TRUE),
    sum(values, na.rm = TRUE) / 1e12
  ))
  .pc_hani_edge_test(values, lat)
}

.pc_hani_edge_test <- function(values, lat) {
  rate <- sweep(values, 2, .pc_cell_area_ha(lat, 1 / 12), "/")
  land <- !is.na(values)
  neighbours <- .pc_shift(land, 1, 0) +
    .pc_shift(land, -1, 0) +
    .pc_shift(land, 0, 1) +
    .pc_shift(land, 0, -1)
  edge <- land & neighbours < 4
  interior <- land & neighbours == 4
  interior_rate <- rate
  interior_rate[!interior] <- 0
  weight <- interior * 1
  sum_rate <- .pc_shift(interior_rate, 1, 0, 0) +
    .pc_shift(interior_rate, -1, 0, 0) +
    .pc_shift(interior_rate, 0, 1, 0) +
    .pc_shift(interior_rate, 0, -1, 0)
  sum_weight <- .pc_shift(weight, 1, 0, 0) +
    .pc_shift(weight, -1, 0, 0) +
    .pc_shift(weight, 0, 1, 0) +
    .pc_shift(weight, 0, -1, 0)
  reference <- sum_rate / sum_weight
  cli::cli_text(
    "land cells {sum(land)}, coastal {sum(edge)}, interior {sum(interior)}"
  )
  probs <- c(0.05, 0.25, 0.5, 0.75, 0.95)
  for (which_set in c("coastal", "interior (control)")) {
    mask <- if (which_set == "coastal") edge else interior
    ok <- mask & sum_weight > 0 & is.finite(reference) & reference > 0
    ratio <- rate[ok] / reference[ok]
    cli::cli_text(sprintf(
      "%-20s n=%d  median=%.4f  mean=%.4f  q05=%.4f  q95=%.4f",
      which_set,
      length(ratio),
      stats::median(ratio),
      mean(ratio),
      unname(stats::quantile(ratio, probs[1])),
      unname(stats::quantile(ratio, probs[5]))
    ))
  }
  cli::cli_alert_info(
    "A land-referenced mass would put the coastal median near 0.5."
  )
}

.pc_shift <- function(m, dx, dy, fill = FALSE) {
  nx <- nrow(m)
  ny <- ncol(m)
  out <- matrix(fill, nx, ny)
  xs <- max(1L, 1L - dx):min(nx, nx - dx)
  ys <- max(1L, 1L - dy):min(ny, ny - dy)
  out[xs + dx, ys + dy] <- m[xs, ys]
  out
}

# ---- P: the polity vocabulary -----------------------------------------------

# Resolve which polity table section P reads. Everything section P reports --
# the status vocabularies, the dead-row count, the DA-2 code-versus-column
# disagreement -- is a property of the polity VINTAGE, not of this package, and
# the shipped table is one snapshot of one vintage. Reading `whep::polities`
# and nothing else meant the report could only ever describe the snapshot that
# happened to be embedded, so a finding measured upstream could not be checked
# here at all. The default stays the shipped table; the override reaches any
# other one.
.pc_polity_source <- function() {
  path <- .pc_optional_env("WHEP_POLITIES_PATH")
  if (is.null(path)) {
    return(list(
      label = "whep::polities (embedded in this build)",
      table = whep::polities
    ))
  }
  table <- if (stringr::str_detect(path, "(?i)\\.rds$")) {
    readRDS(path)
  } else {
    sf::st_read(path, quiet = TRUE)
  }
  if (!inherits(table, "sf")) {
    cli::cli_abort("{.file {path}} does not hold an {.cls sf} polity table.")
  }
  list(label = path, table = table)
}

# A geometry consumer must filter `wiki_status` and drop
# `polity_type == "aggregate"` (DA-7). Both vocabularies have changed between
# polity releases, so print what the table in hand actually carries rather than
# trusting a remembered list.
.pc_polities <- function(source, year) {
  .pc_h1("P - the polity vocabulary this table carries")
  polities <- source$table
  cli::cli_text("source: {source$label}")
  cli::cli_text("class: {paste(class(polities), collapse = ' / ')}")
  cli::cli_text("rows: {nrow(polities)}")
  flat <- tibble::as_tibble(sf::st_drop_geometry(polities))
  for (column in c("wiki_status", "polity_type", "polygon_status")) {
    .pc_h2(column)
    print(table(flat[[column]], useNA = "ifany"))
  }
  dead <- c("retired", "superseded")
  cli::cli_text(
    "dead rows (retired + superseded): {sum(flat$wiki_status %in% dead)}"
  )
  cli::cli_text("rows with NA wiki_status: {sum(is.na(flat$wiki_status))}")
  cli::cli_text("rows with NA polity_type: {sum(is.na(flat$polity_type))}")

  .pc_h2("polity_code carries its own validity interval (DA-2)")
  years <- stringr::str_match(flat$polity_code, "-([0-9]{4})-([0-9]{4})$")
  mismatch <- which(
    as.integer(years[, 2]) != flat$start_year |
      as.integer(years[, 3]) != flat$end_year |
      is.na(years[, 2])
  )
  cli::cli_text(
    "codes whose years disagree with the columns: {length(mismatch)}"
  )
  if (length(mismatch) > 0L) {
    print(flat[mismatch, c("polity_code", "start_year", "end_year")])
  }
  cli::cli_alert_info(
    "This count belongs to the polity vintage above, not to this package: it
     moves whenever the table does. DA-2's correction measured 2 on
     `polities-integration`. Any count above zero means year resolution must
     read `start_year`/`end_year` and never parse the code."
  )

  .pc_h2("Antarctica (DA-7)")
  antarctic <- flat[
    grepl("antarct", flat$polity_name, ignore.case = TRUE) |
      flat$iso3_code %in% c("ATA", "ATF"),
  ]
  cli::cli_text("rows naming Antarctica or carrying ATA/ATF: {nrow(antarctic)}")

  .pc_h2("live real polities covering the evidence year")
  live <- flat[
    !flat$wiki_status %in% dead &
      !is.na(flat$polity_type) &
      flat$polity_type != "aggregate" &
      flat$start_year <= year &
      flat$end_year > year,
  ]
  cli::cli_text("live real polities valid in {year}: {nrow(live)}")
  cli::cli_text("of which without geometry: {sum(!live$has_geometry)}")
  invisible(flat)
}

# ---- Run --------------------------------------------------------------------

luh2_dir <- .pc_require_env("WHEP_LUH2_DIR", "LUH2 directory")
crosswalk_path <- .pc_require_env(
  "WHEP_POLITY_FRACTION_PATH",
  "cell-polity fraction parquet"
)
hani_dir <- .pc_optional_env("WHEP_HANI_DIR")
ne_path <- .pc_optional_env("WHEP_NE_COUNTRIES_PATH")
polity_source <- .pc_polity_source()
pc_year <- .pc_evidence_year()

cli::cli_alert_info("Evidence year: {pc_year} (every section, including H).")
grid <- .pc_read_luh2(luh2_dir, pc_year)
crosswalk <- .pc_read_crosswalk(crosswalk_path)

.pc_ea1(grid)
half_degree <- .pc_half_degree(grid)
.pc_ea2(half_degree)
crosswalk_cells <- .pc_ea3(crosswalk, half_degree)
.pc_ea6(crosswalk, crosswalk_cells, half_degree)
.pc_zero_cells(grid, crosswalk, crosswalk_cells, ne_path)
if (!is.null(hani_dir)) {
  .pc_hani(hani_dir, pc_year)
} else {
  cli::cli_alert_warning("WHEP_HANI_DIR unset: skipping section H.")
}
.pc_polities(polity_source, pc_year)
cli::cli_alert_success("Done.")
