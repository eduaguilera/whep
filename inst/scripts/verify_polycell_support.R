# verify_polycell_support.R
#
# Re-derives every number quoted as evidence for build_polycell_support()
# (plan `plans/2026-08-03-polycell-spatial-support.md`, task T-A4). The numbers
# were first measured interactively; this script exists so anyone with the same
# inputs can reproduce them, and so a later change to the polity table, the
# water layer, the ice layer or the LUH2 vintage shows up as a moved number
# rather than as silent drift.
#
# What it reports:
#   L    the three input layers against the plan's EA9/EA10 measurements.
#   S-A1 the three area categories sum to polity_area_ha.
#   S-A2 re-aggregation to the polity polygon, AT A YEAR. Never summed across
#        intervals: the table is interval-keyed, so summing a polity over every
#        interval counts a cell once per epoch and inflates it. Reported at a
#        historical year, at the modern year, and then over EVERY interval at a
#        year inside its own validity, which is the only view that shows the
#        whole exception list: two of its five members are live in neither
#        1900 nor 2015.
#   S-A4 the global land denominator against the whole-cell base.
#   DA-12 the deployed crosswalk, today's producer and the polycell footprint.
#   DA-13 that the transitional shim is GONE. Until C9 this asserted the shim
#        reproduced build_cell_polity() bit-for-bit; it now aborts if a
#        `polity_frac` column, a crosswalk-only padding row or an unmeasured
#        row has come back.
#   DA-15 polities with no usable polygon, and pieces measured with terra.
#   DA-19 inland water clamped to the polycell's territory, and the cells the
#        water layer and the polycells do not share.
#   S-A9/S-A11 the LUH2 reconciliation, in BOTH directions, and the
#        unclaimed-land magnitude.
#   O    cells holding more territory than the cell (duplicate polygons).
#   Q-P6 the four orphan cells.
#
# Run:
#   Rscript inst/scripts/verify_polycell_support.R
#
# Inputs, all resolved from environment variables (never hardcode the path):
#   WHEP_LPJML_INPUT_DIR      grid.clm + glwd_lakes_and_rivers_30arcmin.clm.
#                             Optional; inland water is skipped when unset.
#   WHEP_NATURALEARTH_DIR     ne_10m_glaciated_areas/. Optional; ice is
#                             skipped when unset.
#   WHEP_LUH2_DIR             staticData_quarterdeg.nc. Optional; the DA-5
#                             reconciliation and S-A11 are skipped when unset.
#   WHEP_POLITY_FRACTION_PATH cell_polity_fraction.parquet. Optional; the
#                             DA-12 reconciliation is skipped when unset. The
#                             DA-13 check always runs -- it is about the
#                             producer's own output, not about the crosswalk.
#
# Note for anyone whose environment variables look unset: R reads `.Renviron`
# in the working directory INSTEAD of `~/.Renviron`, and this repository has
# its own (issue #456). Run with R_ENVIRON_USER pointing at the user file, or
# export the variables in the shell.

.vps_h <- function(x) cli::cli_h2(x)

.vps_codes_from_env <- function() {
  codes <- Sys.getenv("WHEP_VPS_POLITY_CODES", "")
  if (!nzchar(codes)) {
    return(NULL)
  }
  stringr::str_split_1(codes, ",") |> stringr::str_trim()
}

.vps_env <- function(name) {
  value <- Sys.getenv(name, "")
  if (nzchar(value)) value else NULL
}

# ---- Inputs -----------------------------------------------------------------

.vps_water <- function() {
  if (is.null(.vps_env("WHEP_LPJML_INPUT_DIR"))) {
    cli::cli_alert_warning("WHEP_LPJML_INPUT_DIR unset: no inland water.")
    return(NULL)
  }
  water <- whep::read_glwd_water()
  cli::cli_alert_info(
    "GLWD: {nrow(water)} cells (EA10 67,420),
     {sum(water$water_frac > 0)} wet (EA10 32,358),
     {round(sum(water$water_frac * whep:::.cell_area_ha_lat(water$lat)) / 1e8, 4)}
     Mkm2 (EA10 2.4759)."
  )
  water
}

.vps_ice <- function() {
  if (is.null(.vps_env("WHEP_NATURALEARTH_DIR"))) {
    cli::cli_alert_warning("WHEP_NATURALEARTH_DIR unset: no ice.")
    return(NULL)
  }
  ice <- whep::read_glaciated_areas()
  cli::cli_alert_info(
    "ne_10m_glaciated_areas: {nrow(ice)} usable features,
     {sum(ice$s2_repaired)} repaired planar-side,
     {nrow(attr(ice, 'unrepaired'))} still s2-invalid."
  )
  ice
}

.vps_luh2 <- function() {
  if (is.null(.vps_env("WHEP_LUH2_DIR"))) {
    cli::cli_alert_warning("WHEP_LUH2_DIR unset: no DA-5 validation layer.")
    return(NULL)
  }
  luh2 <- whep::read_luh2_terrestrial()
  cli::cli_alert_info(
    "LUH2 {attr(luh2, 'luh2_vintage')}: {nrow(luh2)} cells,
     {round(sum(luh2$terrestrial_ha) / 1e9, 4)} Gha (EA1/EA2 12.9931)."
  )
  luh2
}

.vps_crosswalk <- function() {
  if (is.null(.vps_env("WHEP_POLITY_FRACTION_PATH"))) {
    cli::cli_alert_warning(
      "WHEP_POLITY_FRACTION_PATH unset: no DA-12 crosswalk reconciliation."
    )
    return(NULL)
  }
  whep::build_cell_polity()
}

# ---- Sections ---------------------------------------------------------------

.vps_identity <- function(polycells) {
  .vps_h("S-A1: the three categories sum to polity_area_ha")
  residual <- polycells$land_area_ha +
    polycells$inland_water_ha +
    polycells$ice_area_ha -
    polycells$polity_area_ha
  cli::cli_text(
    "max relative residual {max(abs(residual) / polycells$polity_area_ha)}
     (DA-18 1e-9); negative land rows {sum(polycells$land_area_ha < 0)};
     negative water rows {sum(polycells$inland_water_ha < 0)}."
  )
}

# Re-aggregation is checked AT A YEAR. The polity's own polygon area is the
# reference, taken through the same planar repair the producer applies.
.vps_reaggregation <- function(polycells, year) {
  .vps_h(paste0("S-A2: re-aggregation at ", year))
  got <- whep::expand_polycell_years(polycells, year) |>
    dplyr::summarise(
      got_ha = sum(.data$polity_area_ha),
      terra_pieces = sum(.data$area_engine == "terra"),
      .by = "polity_code"
    )
  comparison <- got |>
    dplyr::inner_join(.vps_own_areas(got$polity_code), by = "polity_code") |>
    dplyr::mutate(rel = abs(.data$got_ha - .data$own_ha) / .data$own_ha) |>
    dplyr::arrange(dplyr::desc(.data$rel))
  cli::cli_text(
    "{nrow(comparison)} polities: max {signif(max(comparison$rel), 3)},
     median {signif(stats::median(comparison$rel), 3)},
     over 1e-6: {sum(comparison$rel > 1e-6)}."
  )
  print(utils::head(as.data.frame(comparison), 8), digits = 6)
  invisible(comparison)
}

# The S-A2 exception list, checked over EVERY interval at a year inside its
# own validity rather than at one calendar year. Measured across all 567
# clipped intervals: max 6.6957e-05, median 1.1962e-14, five above 1e-6. All
# five carry pieces the spherical engine could not read, so their residual is
# the terra/s2 engine substitution and nothing else; a polity appearing here
# without terra pieces is a new defect. Two of the five are not live in 1900 or
# 2015, which is why a single-year check reports three and misses them.
.vps_exception_list <- function(polycells) {
  .vps_h("S-A2: the exception list, over every interval at its own year")
  expected <- c(
    "GRC-1830-1913",
    "DEU-1800-1866",
    "DEU-1866-1871",
    "GBR-1800-1921",
    "FRA-1800-1919"
  )
  # ONE interval per polycell, never a total over interval rows. Summing every
  # row of a polity counts each of its cells once per epoch: GRC-1830-1913 has
  # 51 polycells across 62 interval rows, and the total reads 5,194,796 ha
  # against a 4,624,216 ha polygon, a spurious 0.1234 where the truth at a year
  # is 6.696e-05. TUR-1800-1913 carries 1,399 split polycells, so a gate built
  # that way fires on dozens of polities that are correct.
  #
  # The probe is the polity's own earliest interval start, which every one of
  # its polycells covers because the splits partition the polity's validity.
  measured <- polycells |>
    dplyr::inner_join(
      dplyr::summarise(
        polycells,
        probe_year = min(.data$start_year),
        .by = "polity_code"
      ),
      by = "polity_code"
    ) |>
    dplyr::filter(
      .data$start_year <= .data$probe_year,
      .data$probe_year < .data$end_year
    ) |>
    dplyr::summarise(
      got_ha = sum(.data$polity_area_ha),
      polycells = dplyr::n(),
      terra_pieces = sum(.data$area_engine == "terra"),
      .by = c("polity_code", "probe_year")
    )
  comparison <- measured |>
    dplyr::inner_join(
      .vps_own_areas(measured$polity_code),
      by = "polity_code"
    ) |>
    dplyr::mutate(rel = abs(.data$got_ha - .data$own_ha) / .data$own_ha) |>
    dplyr::filter(.data$rel > 1e-6) |>
    dplyr::arrange(dplyr::desc(.data$rel))
  print(as.data.frame(comparison), digits = 6)
  # SET EQUALITY, reported in both directions and as counts. "Within the
  # expected five" passes just as happily on an EMPTY list, so a relaxed
  # tolerance or a broken measurement reads as success; the machine-checkable
  # line below cannot. `present` is restricted to the subset actually built, so
  # running on a handful of polities does not read as a shrunken list.
  built <- intersect(expected, polycells$polity_code)
  unexpected <- setdiff(comparison$polity_code, expected)
  missing <- setdiff(built, comparison$polity_code)
  no_terra <- comparison$polity_code[comparison$terra_pieces == 0L]
  cli::cli_text(
    "exception list: expected {length(built)}, present
     {length(intersect(comparison$polity_code, built))}, unexpected
     {length(unexpected)}, missing {length(missing)}, without terra
     {length(no_terra)}."
  )
  if (length(unexpected) > 0L) {
    cli::cli_alert_danger(
      "The S-A2 exception list GREW: {.val {unexpected}}. Investigate before
       accepting this build."
    )
  }
  if (length(missing) > 0L) {
    cli::cli_alert_danger(
      "The S-A2 exception list SHRANK: {.val {missing}} no longer exceeds the
       tolerance. That is not automatically good -- it also happens when the
       measurement stops measuring."
    )
  }
  if (length(no_terra) > 0L) {
    cli::cli_alert_danger(
      "Over tolerance WITHOUT a terra piece: {.val {no_terra}}. That is not
       the engine substitution and needs its own explanation."
    )
  }
  if (length(c(unexpected, missing, no_terra)) == 0L) {
    cli::cli_alert_success(
      "Exception list is exactly the expected {length(built)}: {.val {built}}."
    )
  }
  invisible(comparison)
}

.vps_own_areas <- function(codes) {
  polities <- whep::get_polity_geometries(codes)
  fixed <- whep:::.s2_repair(sf::st_geometry(polities))
  usable <- fixed$status != "invalid"
  own_ha <- rep(NA_real_, nrow(polities))
  own_ha[usable] <- as.numeric(sf::st_area(fixed$geom[usable])) / 1e4
  tibble::tibble(polity_code = polities$polity_code, own_ha = own_ha)
}

.vps_denominator <- function(polycells, year) {
  .vps_h(paste0("S-A4: the global denominator at ", year))
  year_rows <- whep::expand_polycell_years(polycells, year)
  cells <- dplyr::distinct(year_rows, .data$cell_id, .data$cell_area_ha)
  cli::cli_text(
    "land {round(sum(year_rows$land_area_ha) / 1e9, 4)} Gha against a whole-cell
     base of {round(sum(cells$cell_area_ha) / 1e9, 4)} Gha
     (ratio {round(sum(cells$cell_area_ha) / sum(year_rows$land_area_ha), 4)});
     inland water {round(sum(year_rows$inland_water_ha) / 1e8, 4)} Mkm2;
     ice {round(sum(year_rows$ice_area_ha) / 1e8, 4)} Mkm2."
  )
}

.vps_footprints <- function(support, crosswalk) {
  .vps_h("DA-12: the three footprints")
  print(as.data.frame(attr(support, "footprints")))
  if (is.null(crosswalk)) {
    return(invisible(NULL))
  }
  diff <- attr(support, "footprint_diff")
  cli::cli_text(
    "build_cell_polity() {nrow(crosswalk)} rows;
     {nrow(diff)} cell-area_code pairs on which the three footprints disagree."
  )
}

# DA-13, flipped at C9. This check used to print "identical: TRUE" for the shim
# against `build_cell_polity()`. The shim is gone, so the check is now that it
# has NOT come back -- and it ABORTS rather than reporting, because a diagnostic
# that prints a false line is exactly what a re-introduced shim would produce.
# It runs on the support built WITH `data$crosswalk` supplied, which is the
# input that used to produce the shim -- checking a support built without one
# would pass on a path where the shim could not have appeared anyway.
.vps_shim_removed <- function(support) {
  .vps_h("DA-13: the transitional shim is gone")
  if (rlang::has_name(support, "polity_frac")) {
    cli::cli_abort(
      "The support carries {.field polity_frac}: the DA-13 shim is back."
    )
  }
  if (any(support$coverage_status == "crosswalk_only")) {
    cli::cli_abort(
      "{sum(support$coverage_status == 'crosswalk_only')} crosswalk-only
       padding row{?s}: the DA-13 shim is back."
    )
  }
  if (anyNA(support$polity_code) || anyNA(support$land_area_ha)) {
    cli::cli_abort(
      "{sum(is.na(support$polity_code))} row{?s} carry no {.field polity_code}
       and {sum(is.na(support$land_area_ha))} no {.field land_area_ha}: every
       row of this table must be a measured polycell."
    )
  }
  cli::cli_alert_success(
    "No shim column, no padding row; sum(land_area_ha) =
     {round(sum(support$land_area_ha) / 1e9, 4)} Gha over
     {nrow(support)} interval rows, not NA."
  )
}

.vps_coverage <- function(support) {
  .vps_h("DA-15: polygon coverage and substituted area engines")
  print(as.data.frame(dplyr::count(attr(support, "coverage"), coverage_status)))
  terra_measured <- attr(support, "terra_measured")
  if (is.null(terra_measured)) {
    cli::cli_alert_success("Every piece was measured by the spherical engine.")
    return(invisible(NULL))
  }
  cli::cli_text(
    "{nrow(terra_measured)} pieces measured with terra::expanse():
     {round(sum(terra_measured$polity_area_ha) / 1e6, 4)} Mha."
  )
  print(as.data.frame(dplyr::summarise(
    terra_measured,
    pieces = dplyr::n(),
    ha = sum(.data$polity_area_ha),
    .by = "polity_code"
  )))
}

.vps_water_clamp <- function(support, polycells, year) {
  .vps_h(paste0("DA-19: inland water clamped to the territory, ", year))
  excess <- attr(support, "water_excess")
  if (is.null(excess)) {
    return(invisible(NULL))
  }
  keys <- whep::expand_polycell_years(polycells, year) |>
    dplyr::distinct(.data$polycell_id, .data$start_year)
  at_year <- dplyr::semi_join(
    excess,
    keys,
    by = c("polycell_id", "start_year")
  )
  cli::cli_text(
    "{nrow(excess)} clamped polycells over all intervals
     ({round(sum(excess$water_excess_ha) / 1e8, 4)} Mkm2);
     {nrow(at_year)} at {year}
     ({round(sum(at_year$water_excess_ha) / 1e8, 4)} Mkm2)."
  )
}

.vps_unassigned <- function(support, polycells, luh2, year) {
  .vps_h(paste0("S-A9/S-A11: LUH2 reconciliation at ", year))
  if (is.null(luh2)) {
    return(invisible(NULL))
  }
  claimed <- sum(whep::expand_polycell_years(polycells, year)$land_area_ha)
  cli::cli_text(
    "claimed land {round(claimed / 1e9, 4)} Gha against LUH2
     {round(sum(luh2$terrestrial_ha) / 1e9, 4)} Gha
     ({round(100 * (claimed / sum(luh2$terrestrial_ha) - 1), 2)}%)."
  )
  # Both directions. Reporting only the shortfall reconciles the overshoot away
  # by construction, which is the silent reconciliation DA-5 forbids.
  disagreement <- attr(support, "unassigned") |>
    dplyr::filter(.data$start_year <= year, year < .data$end_year)
  cli::cli_text(
    "at {year}: {sum(disagreement$unassigned_land_ha > 0)} cells under-claim
     {round(sum(disagreement$unassigned_land_ha) / 1e6, 2)} Mha;
     {sum(disagreement$over_claimed_land_ha > 0)} cells over-claim
     {round(sum(disagreement$over_claimed_land_ha) / 1e6, 2)} Mha."
  )
}

# The diagnostic is interval-grain, so a row is a cell-INTERVAL, not a cell.
# Both are reported, and the wet side is also sliced, because the two answer
# different questions: how many cells are unreached in some epoch, and how many
# at the year in hand.
.vps_water_unmatched <- function(support, year) {
  .vps_h("EA10: cells the water layer and the polycells do not share")
  unmatched <- attr(support, "water_unmatched")
  if (is.null(unmatched) || nrow(unmatched) == 0L) {
    cli::cli_alert_success("The two footprints coincide.")
    return(invisible(NULL))
  }
  print(as.data.frame(dplyr::summarise(
    unmatched,
    cell_intervals = dplyr::n(),
    distinct_cells = dplyr::n_distinct(.data$lon, .data$lat),
    whole_cell_gha = round(sum(.data$cell_area_ha, na.rm = TRUE) / 1e9, 4),
    .by = "side"
  )))
  wet <- unmatched |>
    dplyr::filter(
      .data$side == "water_cell_without_polycell",
      .data$start_year <= year,
      year < .data$end_year
    )
  cli::cli_text(
    "wet cells no polycell reaches at {year}:
     {nrow(dplyr::distinct(wet, .data$lon, .data$lat))}."
  )
}

.vps_overlap <- function(support, polycells, year) {
  .vps_h(paste0("Duplicate polygons: territory beyond the cell, ", year))
  overlap <- attr(support, "overlap")
  if (is.null(overlap)) {
    cli::cli_alert_success("No cell holds more territory than the cell.")
    return(invisible(NULL))
  }
  at_year <- whep::expand_polycell_years(polycells, year) |>
    dplyr::summarise(
      territory_ha = sum(.data$polity_area_ha),
      .by = c("cell_id", "cell_area_ha")
    ) |>
    dplyr::filter(.data$territory_ha > .data$cell_area_ha * (1 + 1e-4))
  cli::cli_text(
    "{nrow(overlap)} cell-intervals overall; {nrow(at_year)} cells at {year},
     excess {round(sum(at_year$territory_ha - at_year$cell_area_ha) / 1e6, 2)}
     Mha, worst ratio
     {round(max(at_year$territory_ha / at_year$cell_area_ha), 4)}."
  )
}

.vps_orphans <- function(polycells, year) {
  .vps_h("Q-P6: the four orphan cells")
  orphans <- tibble::tribble(
    ~lon, ~lat, ~area_code,
    -75.75, 22.25, 12L,
    98.25, 11.25, 28L,
    131.75, -4.75, 101L,
    -80.75, -33.75, 40L
  )
  year_rows <- whep::expand_polycell_years(polycells, year)
  found <- orphans |>
    dplyr::left_join(
      dplyr::summarise(
        year_rows,
        polycells = dplyr::n(),
        land_ha = sum(.data$land_area_ha),
        .by = c("lon", "lat")
      ),
      by = c("lon", "lat")
    )
  print(as.data.frame(found))
}

# ---- Run --------------------------------------------------------------------

# `polity_codes` restricts the build to a subset. The default is the whole
# table, which is the production call and takes about an hour; a subset runs in
# minutes and is what makes it practical to EXECUTE this script after editing
# it. `inst/scripts/` is under no test, so an unexecuted change here is
# unverified by construction, which is exactly how a broken S-A2 gate once
# shipped from this file. Set WHEP_VPS_POLITY_CODES to a comma-separated list
# to drive it from the shell.
.vps_main <- function(
  year = 2015L,
  historical_year = 1900L,
  polity_codes = .vps_codes_from_env()
) {
  rlang::check_installed(c("sf", "terra"))
  water <- .vps_water()
  ice <- .vps_ice()
  luh2 <- .vps_luh2()
  crosswalk <- .vps_crosswalk()
  cli::cli_alert_info("Building the polycell support table...")
  support <- whep::build_polycell_support(
    geometries = whep::get_polity_geometries(polity_codes),
    water = water,
    ice = ice,
    data = list(luh2 = luh2, crosswalk = crosswalk, crosswalk_year = year)
  )
  .vps_shim_removed(support)
  # Every row is a measured polycell since C9 removed the padding, so this is
  # the whole table rather than a filtered view of it.
  polycells <- support
  cli::cli_alert_success(
    "{nrow(support)} interval rows
     ({dplyr::n_distinct(polycells$polycell_id)} polycells)."
  )
  .vps_identity(polycells)
  .vps_reaggregation(polycells, historical_year)
  .vps_reaggregation(polycells, year)
  .vps_exception_list(polycells)
  .vps_denominator(polycells, year)
  .vps_footprints(support, crosswalk)
  .vps_coverage(support)
  .vps_water_clamp(support, polycells, year)
  .vps_water_unmatched(support, year)
  .vps_unassigned(support, polycells, luh2, year)
  .vps_unassigned(support, polycells, luh2, historical_year)
  .vps_overlap(support, polycells, year)
  .vps_orphans(polycells, year)
  cli::cli_alert_success("Done.")
  invisible(support)
}

.vps_main()
