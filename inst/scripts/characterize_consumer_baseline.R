# characterize_consumer_baseline.R
#
# C0 of task T-A5 in `plans/2026-08-03-polycell-spatial-support.md`: the
# characterisation baseline, measured on UNMODIFIED consumer code before any
# consumer is migrated to the polycell.
#
# This script pins the numbers that need real inputs. The invariants that can
# be pinned on fixtures live in the test suite instead, where they run in CI:
#   tests/testthat/test_n_deposition.R  deposition mass conservation.
#   tests/testthat/test_n_urban.R       urban population partition.
#   tests/testthat/test_feed_lpjml.R    grass conservation.
#
# THESE ARE CHARACTERISATIONS, NOT CORRECTNESS ASSERTIONS. Everything here
# records what the code does today, including behaviour that is arguably
# wrong, so that a value moved by the migration is visible and attributable
# rather than silent.
#
# What it reports:
#   A  the deployed cell_polity_fraction.parquet fingerprint, which DA-12
#      names as the S-A10 measurement baseline because it is the geometry
#      every published WHEP number was computed from.
#   B  deposition mass conservation at full crosswalk scale, driven by a
#      synthetic per-cell mass so the check needs no HaNi rasters.
#   C  grass conservation at full crosswalk scale, likewise synthetic.
#
# Run:
#   Rscript inst/scripts/characterize_consumer_baseline.R
#
# Inputs, resolved from environment variables (never hardcode the path):
#   WHEP_POLITY_FRACTION_PATH cell_polity_fraction.parquet. REQUIRED.
#
# Note for anyone whose environment variables look unset: R reads `.Renviron`
# in the working directory INSTEAD of `~/.Renviron`, and this repository has
# its own (issue #456). Run with R_ENVIRON_USER pointing at the user file, or
# export the variables in the shell.

.ccb_crosswalk <- function() {
  path <- Sys.getenv("WHEP_POLITY_FRACTION_PATH", "")
  if (!nzchar(path)) {
    cli::cli_abort("Set {.envvar WHEP_POLITY_FRACTION_PATH} and re-run.")
  }
  whep::build_cell_polity()
}

# ---- A. The deployed crosswalk fingerprint ----------------------------------

.ccb_shape <- function(crosswalk) {
  cli::cli_h2("A1: shape (DA-12 baseline 68,527 rows / 64,438 cells / 191)")
  cells <- dplyr::distinct(crosswalk, lon, lat)
  cli::cli_alert_info(
    "rows {nrow(crosswalk)}, cells {nrow(cells)},
     polities {dplyr::n_distinct(crosswalk$area_code)}."
  )
  cli::cli_alert_info(
    "area_code is {class(crosswalk$area_code)},
     range {min(crosswalk$area_code)}-{max(crosswalk$area_code)}."
  )
}

# polity_frac is a partition of the cell, and every mass-conservation
# property downstream is inherited from that. EA3 reported the per-cell sum
# as "min = max = 1.000000", which is true only to six decimals: two cells
# fall one float ulp short. Whatever replaces polity_frac has to hold this
# at least as tightly or deposition, urban and grass totals all move.
.ccb_partition <- function(crosswalk) {
  cli::cli_h2("A2: per-cell sum(polity_frac)")
  sums <- dplyr::summarise(crosswalk, s = sum(polity_frac), .by = c(lon, lat))
  cli::cli_alert_info(
    "min {sprintf('%.17g', min(sums$s))}, max {sprintf('%.17g', max(sums$s))}."
  )
  cli::cli_alert_info(
    "{sum(sums$s != 1)} of {nrow(sums)} cells are not exactly 1;
     max |sum - 1| = {sprintf('%.3g', max(abs(sums$s - 1)))}."
  )
}

# The border split is a raw subcell COUNT at 1/12 degree with subcells = 6L
# (inst/scripts/prepare_spatialize_all.R:949), so a share can only be a
# multiple of 1/36. A migration to geodesic intersection moves border shares
# by up to that quantum; min(polity_frac) departing from 1/36 is the signal
# that the split method changed.
.ccb_quantum <- function(crosswalk) {
  cli::cli_h2("A3: split quantum (1/36, from subcells = 6L)")
  smallest <- min(crosswalk$polity_frac)
  cli::cli_alert_info(
    "min polity_frac {sprintf('%.17g', smallest)},
     1/36 {sprintf('%.17g', 1 / 36)},
     identical {identical(smallest, 1 / 36)}."
  )
  cli::cli_alert_info(
    "max polity_frac {sprintf('%.17g', max(crosswalk$polity_frac))}."
  )
}

# EA6's border-cell census: the multi-polity cells are the only ones where a
# migration can move value between polities at all.
.ccb_sharing <- function(crosswalk) {
  cli::cli_h2("A4: polities per cell (EA6 60,513 / 3,764 / 158 / 3)")
  per_cell <- dplyr::count(crosswalk, lon, lat, name = "polities")
  counts <- dplyr::arrange(dplyr::count(per_cell, polities), polities)
  cli::cli_alert_info(
    "{paste(counts$polities, counts$n, sep = ': ', collapse = ', ')}."
  )
  shared <- sum(counts$n[counts$polities > 1])
  cli::cli_alert_info(
    "{shared} shared cells
     ({sprintf('%.1f%%', 100 * shared / nrow(per_cell))} of cells)."
  )
}

# ---- B. Deposition mass conservation, at crosswalk scale --------------------

# R/n_deposition.R:196 divides the HaNi cell mass by cell_area_ha and :198
# multiplies it straight back, so cell_area_ha cancels exactly and today
# deposition_n_t == value_g_total * polity_frac / 1e6. A synthetic uniform
# per-cell mass exercises that on all 64,438 real cells without needing the
# HaNi rasters; the property is about the arithmetic, not about the values.
.ccb_deposition <- function(crosswalk) {
  cli::cli_h2("B: deposition conserves the source mass (R/n_deposition.R)")
  cells <- dplyr::distinct(crosswalk, lon, lat)
  nhx <- dplyr::mutate(cells, year = 2000L, value_g = 1e9)
  out <- whep::build_n_deposition(
    data = list(nhx = nhx, noy = nhx[0, ], cell_polity = crosswalk)
  )
  source_g <- sum(nhx$value_g)
  gap <- abs(sum(out$deposition_n_t) * 1e6 - source_g) / source_g
  cli::cli_alert_info(
    "{nrow(out)} rows; source {sprintf('%.17g', source_g)} g,
     recovered {sprintf('%.17g', sum(out$deposition_n_t) * 1e6)} g."
  )
  cli::cli_alert_info(
    "relative gap {sprintf('%.3g', gap)} (DA-18 bound 1e-9)."
  )
  .ccb_deposition_identity(crosswalk, out)
  .ccb_deposition_area(crosswalk, nhx, out)
}

# The row-wise algebraic identity today: deposition_n_t is the cell mass
# times polity_frac, with no area term surviving. Reported with the count of
# rows that differ in the last bits, so the float noise floor is on the
# record and cannot later be mistaken for a real movement.
.ccb_deposition_identity <- function(crosswalk, out) {
  exact <- dplyr::transmute(
    crosswalk,
    lon,
    lat,
    area_code,
    expected_n_t = 1e9 * polity_frac / 1e6
  )
  joined <- dplyr::inner_join(out, exact, by = c("lon", "lat", "area_code"))
  differ <- joined$deposition_n_t != joined$expected_n_t
  cli::cli_alert_info(
    "deposition_n_t == value_g_total x polity_frac / 1e6 on
     {sum(!differ)} of {nrow(joined)} rows bitwise;
     max relative gap
     {sprintf('%.3g',
       max(abs(joined$deposition_n_t - joined$expected_n_t) /
             joined$expected_n_t))}."
  )
}

# The sharper statement: triple every cell_area_ha and the mass column does
# not move. Swapping cell_area_ha for a land area at :198 while leaving :196
# would move it ~10% DOWN, in the direction a reviewer expects a fix to move
# it. The cancellation is exact in real arithmetic; in float it round-trips
# through a division and a multiplication, so the per-row agreement is a
# couple of ulp rather than bitwise. Both numbers are reported, because
# quoting the bare bit-identity flag would read as a failure when it is
# nothing but IEEE-754 noise.
.ccb_deposition_area <- function(crosswalk, nhx, base) {
  tripled <- whep::build_n_deposition(
    data = list(
      nhx = nhx,
      noy = nhx[0, ],
      cell_polity = dplyr::mutate(crosswalk, cell_area_ha = cell_area_ha * 3)
    )
  )
  moved <- tripled$deposition_n_t - base$deposition_n_t
  cli::cli_alert_info(
    "3x cell_area_ha moves {sum(moved != 0)} of {nrow(base)} rows;
     max relative move
     {sprintf('%.3g', max(abs(moved) / base$deposition_n_t))};
     total relative move
     {sprintf('%.3g',
       abs(sum(tripled$deposition_n_t) - sum(base$deposition_n_t)) /
         sum(base$deposition_n_t))}."
  )
  .ccb_deposition_rate(base)
}

# AM-5's first silent-breakage risk: dividing the allocated mass by each
# polycell's own land area gives every polity of a shared cell its own rate
# and makes rate x area recover the whole cell mass once per polity. Today
# every polity of a cell sees the same whole-cell rate, which is the guard.
.ccb_deposition_rate <- function(base) {
  rates <- dplyr::summarise(
    base,
    n_rates = dplyr::n_distinct(deposition_kgn_ha),
    .by = c(lon, lat)
  )
  cli::cli_alert_info(
    "cells whose polities disagree on deposition_kgn_ha:
     {sum(rates$n_rates > 1)} (a shared cell has ONE whole-cell rate today)."
  )
}

# ---- C. Grass conservation, at crosswalk scale ------------------------------

# aggregate_grass_to_polity() multiplies an already-absolute tonnage by
# polity_frac, so it carries no area term and conserves whenever polity_frac
# partitions the cell. R/feed_lpjml.R:110's OTHER cell_area_ha use is frozen
# by AM-2 and is deliberately not characterised.
.ccb_grass <- function(crosswalk) {
  cli::cli_h2("C: grass conserves through aggregate_grass_to_polity()")
  grass <- dplyr::distinct(crosswalk, lon, lat) |>
    dplyr::mutate(year = 2000L, grass_avail_dm_t = 100)
  agg <- whep::aggregate_grass_to_polity(grass, crosswalk)
  source_t <- sum(grass$grass_avail_dm_t)
  gap <- abs(sum(agg$grass_avail_dm_t) - source_t) / source_t
  cli::cli_alert_info(
    "{nrow(agg)} polity-years; source {sprintf('%.17g', source_t)} t,
     recovered {sprintf('%.17g', sum(agg$grass_avail_dm_t))} t."
  )
  cli::cli_alert_info(
    "relative gap {sprintf('%.3g', gap)} (DA-18 bound 1e-9)."
  )
}

# ---- Run --------------------------------------------------------------------

cli::cli_h1("C0 consumer characterisation baseline")
.ccb_data <- .ccb_crosswalk()
.ccb_shape(.ccb_data)
.ccb_partition(.ccb_data)
.ccb_quantum(.ccb_data)
.ccb_sharing(.ccb_data)
.ccb_deposition(.ccb_data)
.ccb_grass(.ccb_data)
cli::cli_alert_success("C0 baseline reported.")
