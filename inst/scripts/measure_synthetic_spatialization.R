# measure_synthetic_spatialization.R
#
# The S-A10 value-movement measurement for task C5 of
# `plans/2026-08-03-polycell-spatial-support.md`: moving
# `spatialize_country_n_to_crops()`'s grid weights off the crosswalk's
# `polity_frac` and onto the polycell's measured `land_area_ha`.
#
# WHY THIS CONSUMER IS THE CONTROL CASE (AM-4). Its cell weights are
# RENORMALISED inside each polity-crop-year, so a change of the polity share
# redistributes the national total and cannot change it. Two predictions follow,
# and this script exists to test them rather than to assert them:
#
#   1. per-polity `sum(n_t)` is UNCHANGED. It is a national total being
#      redistributed; redistribution cannot change a sum. Any movement here is
#      a bug, not a migration result.
#   2. the cell shares move only where the two partitions disagree, i.e. on
#      shared cells. A movement resembling the 11.0% whole-cell over-count
#      (EA2) would mean an ABSOLUTE area had been substituted into a normalised
#      weight -- AM-5 risk 3 in its C5 form -- and that is the cheap signal this
#      commit exists to raise before the expensive consumers.
#
# WHAT IT REPORTS:
#   F  the environment fingerprint, printed on both sides of every comparison.
#   A  the inputs, against the plan's own EA/AM figures.
#   B  the DA-23 boundary conversion. `spatialize_country_n_to_crops()` REFUSES
#      a support whose `area_code` is duplicated or NA, so the fold happens
#      here, where it can be reported. Both conversions are run: the naive one
#      that drops unkeyable polycells first (which renormalises the survivors
#      onto the whole cell) and the one that keeps their land in the
#      denominator under a bucket no country total can join.
#   C  the footprints: which cells each support reaches.
#   D  per-polity `sum(n_t)` before and after -- prediction 1.
#   E  the per-(polity, crop, year) L1 distance between the old and the new
#      normalised cell-share vectors, and how much of it is the partition as
#      against the footprint and identity churn that travels with it -- the
#      AM-29 decomposition, without which a movement is unattributable.
#   G  cells receiving a non-zero allocation, before and after.
#
# Run:
#   Rscript inst/scripts/measure_synthetic_spatialization.R
#
# Inputs, all resolved from environment variables (never hardcode the path):
#   WHEP_POLYCELL_SUPPORT_PATH  a parquet written by build_polycell_support().
#                               REQUIRED.
#   WHEP_POLITY_FRACTION_PATH   cell_polity_fraction.parquet. REQUIRED.
#   WHEP_CROP_PATTERNS_PATH     crop_patterns.parquet. REQUIRED.
#   WHEP_TYPE_CROPLAND_PATH     type_cropland.parquet. REQUIRED.
#   WHEP_MSN_PRIMARY_PROD       primary production csv/parquet carrying year,
#                               area_code, item_cbs_code, unit, value. Optional;
#                               without it the crop split is uniform, which
#                               cannot affect the comparison because it is
#                               identical on both sides.
#   WHEP_MSN_YEAR               reference year. Optional, default 2015.
#
# Note for anyone whose environment variables look unset: R reads `.Renviron`
# in the working directory INSTEAD of `~/.Renviron` (issue #456). Run with
# R_ENVIRON_USER pointing at the user file, or export them in the shell.

msn_year <- function() as.integer(Sys.getenv("WHEP_MSN_YEAR", "2015"))

msn_num <- function(x, digits = 4) {
  formatC(x, format = "f", digits = digits, big.mark = ",")
}

msn_h <- function(x) cli::cli_h2(x)

# ---- F. Fingerprint ---------------------------------------------------------

# Printed unconditionally, because a comparison whose two sides ran under
# different environment variables is the failure mode this branch has already
# hit: a `WHEP_*` variable silently unset turns a block that RUNS into a block
# that SKIPS, and the two look alike in a summary.
msn_fingerprint <- function() {
  msn_h("F: environment fingerprint")
  vars <- c(
    "WHEP_POLYCELL_SUPPORT_PATH",
    "WHEP_POLITY_FRACTION_PATH",
    "WHEP_CROP_PATTERNS_PATH",
    "WHEP_TYPE_CROPLAND_PATH",
    "WHEP_MSN_PRIMARY_PROD",
    "WHEP_MSN_YEAR"
  )
  for (v in vars) {
    cli::cli_text("{v} = {.val {Sys.getenv(v, '')}}")
  }
  cli::cli_text("R {getRversion()}; reference year {msn_year()}")
}

# ---- A. Inputs --------------------------------------------------------------

msn_support <- function() {
  path <- Sys.getenv("WHEP_POLYCELL_SUPPORT_PATH", "")
  if (!nzchar(path) || !file.exists(path)) {
    cli::cli_abort("Set {.envvar WHEP_POLYCELL_SUPPORT_PATH} to a parquet.")
  }
  tibble::as_tibble(nanoparquet::read_parquet(path))
}

msn_inputs <- function() {
  msn_h("A: inputs")
  support <- msn_support()
  crosswalk <- whep::build_cell_polity()
  at_year <- whep::expand_polycell_years(support, msn_year()) |>
    dplyr::filter(.data$coverage_status != "crosswalk_only")
  cli::cli_text(
    "support {nrow(support)} interval rows; {nrow(at_year)} polycells at
     {msn_year()} over {dplyr::n_distinct(at_year$lon, at_year$lat)} cells;
     land {msn_num(sum(at_year$land_area_ha) / 1e9, 5)} Gha (AM-38 12.98317)."
  )
  cli::cli_text(
    "crosswalk {nrow(crosswalk)} rows (EA3 68,527) over
     {dplyr::n_distinct(crosswalk$lon, crosswalk$lat)} cells (EA3 64,438),
     {dplyr::n_distinct(crosswalk$area_code)} area_codes (EA3 191)."
  )
  list(polycells = at_year, crosswalk = crosswalk)
}

# The nitrogen being spatialized. Real FAOSTAT national synthetic-N totals when
# the pin resolves, so the per-polity conservation claim is made on published
# magnitudes rather than on a flat field.
msn_totals <- function(shares) {
  fert <- tryCatch(
    whep::whep_read_file("faostat-fertilizer-nutrients"),
    error = function(e) NULL
  )
  if (is.null(fert)) {
    cli::cli_alert_warning("fertilizer pin unavailable: uniform 1,000 t.")
    return(dplyr::transmute(
      dplyr::distinct(shares, .data$year, .data$area_code),
      year = .data$year,
      area_code = .data$area_code,
      n_t = 1000
    ))
  }
  whep:::.synthetic_n_country(fert) |>
    dplyr::filter(.data$year == msn_year()) |>
    dplyr::transmute(.data$year, .data$area_code, n_t = .data$synthetic_n_t) |>
    dplyr::semi_join(shares, by = c("year", "area_code"))
}

# The crop split. It is IDENTICAL on both sides of every comparison, so it
# cannot move a single number here; real harvested-area shares are used when a
# production table is supplied purely so the crop set is the real one.
msn_shares <- function() {
  path <- Sys.getenv("WHEP_MSN_PRIMARY_PROD", "")
  if (!nzchar(path) || !file.exists(path)) {
    cli::cli_alert_warning("no WHEP_MSN_PRIMARY_PROD: uniform crop shares.")
    return(msn_uniform_shares())
  }
  data.table::fread(
    path,
    select = c("year", "area_code", "item_cbs_code", "unit", "value")
  ) |>
    tibble::as_tibble() |>
    dplyr::filter(.data$year == msn_year()) |>
    whep:::.n_crop_area_shares()
}

# Every crop the pattern raster carries, split evenly inside each polity.
msn_uniform_shares <- function() {
  codes <- whep::items_prod_full |>
    dplyr::transmute(item_cbs_code = as.integer(.data$item_cbs_code)) |>
    dplyr::filter(!is.na(.data$item_cbs_code)) |>
    dplyr::distinct()
  crosswalk <- whep::build_cell_polity()
  tidyr::expand_grid(
    year = msn_year(),
    area_code = sort(unique(as.integer(crosswalk$area_code))),
    item_cbs_code = codes$item_cbs_code
  ) |>
    dplyr::mutate(area_share = 1 / dplyr::n(), .by = c("year", "area_code"))
}

# ---- B. The DA-23 boundary conversion ---------------------------------------

# `spatialize_country_n_to_crops()` refuses a support whose `area_code` is
# duplicated or NA, so the fold is performed HERE, where it can be reported.
#
# Two conversions, because the difference between them is the load-bearing
# hazard of this migration:
#   "drop"     removes the unkeyable polycells before the call. Their land then
#              leaves the share denominator, so the survivors renormalise onto
#              the whole cell and take hectares that are not theirs -- while
#              the shares still sum to 1, so no conservation check can see it.
#              This is C7's own defect, in its C5 form.
#   "sentinel" keeps that land in the denominator under one bucket per cell
#              that no country total can join, so it dilutes nobody's share and
#              receives no nitrogen.
msn_convert <- function(polycells, mode = c("sentinel", "drop")) {
  mode <- rlang::arg_match(mode)
  keyed <- dplyr::filter(polycells, !is.na(.data$area_code))
  unkeyed <- dplyr::filter(polycells, is.na(.data$area_code))
  out <- msn_fold(keyed)
  if (mode == "drop" || nrow(unkeyed) == 0L) {
    return(out)
  }
  dplyr::bind_rows(out, msn_sentinel(unkeyed))
}

# Polycells sharing an `area_code` in one cell have their land summed. That is
# right for a WEIGHT (bucket 206's land really is Sudan plus South Sudan) and
# would be wrong for a value, which is why it is done once, here, at the
# boundary that owns it.
msn_fold <- function(keyed) {
  keyed |>
    dplyr::summarise(
      cell_area_ha = dplyr::first(.data$cell_area_ha),
      land_area_ha = sum(.data$land_area_ha),
      .by = c("lon", "lat", "area_code")
    )
}

msn_sentinel <- function(unkeyed) {
  unkeyed |>
    dplyr::summarise(
      cell_area_ha = dplyr::first(.data$cell_area_ha),
      land_area_ha = sum(.data$land_area_ha),
      .by = c("lon", "lat")
    ) |>
    dplyr::mutate(area_code = -1L)
}

msn_report_conversion <- function(polycells) {
  msn_h("B: the DA-23 boundary conversion")
  unkeyed <- dplyr::filter(polycells, is.na(.data$area_code))
  folded <- polycells |>
    dplyr::filter(!is.na(.data$area_code)) |>
    dplyr::summarise(
      n_polities = dplyr::n(),
      land_area_ha = sum(.data$land_area_ha),
      .by = c("lon", "lat", "area_code")
    ) |>
    dplyr::filter(.data$n_polities > 1L)
  cli::cli_text(
    "no area_code: {nrow(unkeyed)} polycells,
     {msn_num(sum(unkeyed$land_area_ha) / 1e6, 3)} Mha of land, over
     {dplyr::n_distinct(unkeyed$lon, unkeyed$lat)} cells."
  )
  cli::cli_text(
    "shared area_code: {nrow(folded)} cell-area_code groups,
     {msn_num(sum(folded$land_area_ha) / 1e6, 3)} Mha, folded."
  )
  shared <- dplyr::semi_join(
    unkeyed,
    dplyr::filter(polycells, !is.na(.data$area_code)),
    by = c("lon", "lat")
  )
  cli::cli_text(
    "of the unkeyable land, {msn_num(sum(shared$land_area_ha) / 1e6, 3)} Mha
     sits in {dplyr::n_distinct(shared$lon, shared$lat)} cells that a KEYED
     polity also holds: that is the land a naive drop-then-renormalise would
     hand to the neighbour."
  )
}

# ---- The two spatializations ------------------------------------------------

# The polities a support can place at all: those with at least one grid cell
# carrying positive cropland. `spatialize_country_n_to_crops()` ABORTS on a
# total it cannot place (it will not silently drop nitrogen), and the two
# supports do not agree on that set, so the comparison is restricted to the
# polities BOTH can place and the difference is reported rather than absorbed.
# Restricting to one side's set would measure the footprint change as if it were
# the partition change.
msn_placeable <- function(cell_polity, data, split) {
  key <- whep:::.n_resolve_split(cell_polity, split)
  whep:::.n_cropland_ha(data, msn_year()) |>
    whep:::.n_cropland_cell_weights(whep:::.n_cell_frac(cell_polity, key)) |>
    dplyr::distinct(.data$year, .data$area_code)
}

msn_restrict <- function(totals, sets, labels) {
  keep <- Reduce(
    function(a, b) dplyr::inner_join(a, b, by = c("year", "area_code")),
    sets
  )
  for (i in seq_along(sets)) {
    lost <- dplyr::anti_join(totals, sets[[i]], by = c("year", "area_code"))
    cli::cli_text(
      "{labels[[i]]}: {nrow(sets[[i]])} placeable polity-years;
       {nrow(lost)} of the {nrow(totals)} totals unplaceable
       ({msn_num(sum(lost$n_t) / 1e6, 4)} Mt N)
       -- codes {.val {sort(unique(lost$area_code))}}."
    )
  }
  out <- dplyr::semi_join(totals, keep, by = c("year", "area_code"))
  cli::cli_text(
    "comparison runs on {nrow(out)} polities,
     {msn_num(sum(out$n_t) / 1e6, 4)} Mt N of the
     {msn_num(sum(totals$n_t) / 1e6, 4)} Mt N total."
  )
  out
}

msn_grid <- function(totals, shares, cell_polity, split, data) {
  suppressWarnings(whep::spatialize_country_n_to_crops(
    country_totals = totals,
    crop_shares = shares,
    cell_polity = cell_polity,
    resolution = "grid",
    split = split,
    data = data
  ))
}

# Normalised cell-share vectors, which is what the migration actually changes.
# `n_t` is the share times a national total identical on both sides, so
# dividing it back out isolates the weight.
msn_shares_of <- function(grid) {
  grid |>
    dplyr::filter(.data$area_code >= 0L) |>
    # The grid output is NOT one row per (cell, crop). `.n_item_prod_codes()`
    # maps 32 of 108 `item_cbs_code`s onto several `item_prod_code`s (2605 onto
    # 27), and `.n_cell_weights()` keeps one row per production item, each
    # carrying a partial share that sums to the cell's share inside the
    # (year, area_code, item_cbs_code) group. Summing first is what makes the
    # key unique; joining on it unaggregated is many-to-many and produced an L1
    # of 33.9 on a scale whose maximum is 2.
    dplyr::summarise(
      n_t = sum(.data$n_t),
      .by = c("lon", "lat", "year", "area_code", "item_cbs_code")
    ) |>
    dplyr::mutate(
      group_t = sum(.data$n_t),
      .by = c("year", "area_code", "item_cbs_code")
    ) |>
    dplyr::filter(.data$group_t > 0) |>
    dplyr::transmute(
      .data$lon,
      .data$lat,
      .data$year,
      .data$area_code,
      .data$item_cbs_code,
      share = .data$n_t / .data$group_t
    )
}

# ---- D. Per-polity totals ---------------------------------------------------

msn_polity_totals <- function(old, new) {
  msn_h("D: per-polity sum(n_t), before and after")
  totals <- function(x) {
    x |>
      dplyr::filter(.data$area_code >= 0L) |>
      dplyr::summarise(n_t = sum(.data$n_t), .by = c("year", "area_code"))
  }
  joined <- dplyr::full_join(
    dplyr::rename(totals(old), old_t = "n_t"),
    dplyr::rename(totals(new), new_t = "n_t"),
    by = c("year", "area_code")
  ) |>
    dplyr::mutate(
      old_t = dplyr::coalesce(.data$old_t, 0),
      new_t = dplyr::coalesce(.data$new_t, 0),
      diff_t = .data$new_t - .data$old_t
    )
  cli::cli_text(
    "{nrow(joined)} polities; global old {msn_num(sum(joined$old_t) / 1e6, 6)}
     Mt N, new {msn_num(sum(joined$new_t) / 1e6, 6)} Mt N, difference
     {msn_num(sum(joined$diff_t), 9)} t."
  )
  cli::cli_text(
    "worst per-polity |difference| {msn_num(max(abs(joined$diff_t)), 9)} t;
     polities moving more than 1e-6 t: {sum(abs(joined$diff_t) > 1e-6)}."
  )
  moved <- dplyr::filter(joined, abs(.data$diff_t) > 1e-6)
  if (nrow(moved) > 0L) {
    cli::cli_alert_danger("A polity total moved. That is a bug, not a result.")
    print(dplyr::arrange(moved, dplyr::desc(abs(.data$diff_t))), n = 20)
  }
  joined
}

# ---- E. Cell-share movement -------------------------------------------------

msn_l1 <- function(old, new, label) {
  msn_h(paste0("E: cell-share L1 distance (", label, ")"))
  # `relationship = "one-to-one"` is the guard, not decoration: without it a
  # non-unique key joins many-to-many and inflates every statistic below behind
  # a warning that reads like advice.
  joined <- dplyr::full_join(
    dplyr::rename(old, old_share = "share"),
    dplyr::rename(new, new_share = "share"),
    by = c("lon", "lat", "year", "area_code", "item_cbs_code"),
    relationship = "one-to-one"
  ) |>
    dplyr::mutate(
      old_share = dplyr::coalesce(.data$old_share, 0),
      new_share = dplyr::coalesce(.data$new_share, 0)
    )
  l1 <- joined |>
    dplyr::summarise(
      l1 = sum(abs(.data$new_share - .data$old_share)),
      .by = c("year", "area_code", "item_cbs_code")
    )
  q <- stats::quantile(l1$l1, c(0.5, 0.9, 0.99, 1), names = FALSE)
  cli::cli_text(
    "{nrow(l1)} polity-crop-years; L1 (0 = identical, 2 = disjoint):
     median {msn_num(q[1], 6)}, p90 {msn_num(q[2], 6)},
     p99 {msn_num(q[3], 6)}, max {msn_num(q[4], 6)}."
  )
  cli::cli_text(
    "exactly zero: {sum(l1$l1 == 0)} ({msn_num(100 * mean(l1$l1 == 0), 2)}%);
     above 0.01: {sum(l1$l1 > 0.01)}; above 0.10: {sum(l1$l1 > 0.10)}."
  )
  msn_relative(joined)
  l1
}

# The number directly comparable to EA2's 11.0%: how far each cell's share
# moved, relative to what it was. An absolute area substituted into a
# normalised weight would show up here as a large, cell-size-correlated shift;
# a partition change on shared cells shows up as a small one confined to those
# cells. Weighted by the OLD share, so a cell holding a millionth of a polity's
# nitrogen cannot dominate the statistic.
msn_relative <- function(joined) {
  moved <- dplyr::filter(joined, .data$old_share > 0)
  rel <- abs(moved$new_share - moved$old_share) / moved$old_share
  # The unweighted mean is NOT reported: cells whose old share is ~1e-300 make
  # it 1e19%, which says something about float64 and nothing about the
  # migration. The old-share-weighted mean is the one comparable to EA2's
  # 11.0%, because it asks how far the NITROGEN moved rather than how far the
  # smallest share moved.
  cli::cli_text(
    "per-cell |relative share change| over {nrow(moved)} cells with a positive
     old share: median {msn_num(100 * stats::median(rel), 4)}%, p90
     {msn_num(100 * stats::quantile(rel, 0.90, names = FALSE), 4)}%, p99
     {msn_num(100 * stats::quantile(rel, 0.99, names = FALSE), 4)}%,
     old-share-weighted mean
     {msn_num(100 * sum(rel * moved$old_share) / sum(moved$old_share), 4)}%."
  )
  cli::cli_text(
    "cells whose share is EXACTLY unchanged:
     {sum(moved$new_share == moved$old_share)}
     ({msn_num(100 * mean(moved$new_share == moved$old_share), 2)}%)."
  )
}

msn_top_movers <- function(l1) {
  top <- dplyr::slice_max(l1, .data$l1, n = 10)
  cli::cli_text("largest movers (polity-crop-year, L1):")
  print(as.data.frame(top))
}

# ---- E3. The weight alone ---------------------------------------------------

# The decisive cut for "how far did the WEIGHT move": polity-crop-years that
# land on exactly the same set of cells under both supports. Everything else is
# the FOOTPRINT moving -- a polity gaining or losing a cell, and with it a crop
# switching between the crop-pattern route and the uniform-cropland fallback,
# which is what puts the extreme tail at L1 near 2. Attributing that to the
# split key would be the unattributable movement S-A10 forbids.
msn_same_cells <- function(old, new) {
  msn_h("E3: polity-crop-years landing on exactly the same cells")
  cells <- function(x) {
    x |>
      dplyr::arrange(
        .data$year,
        .data$area_code,
        .data$item_cbs_code,
        .data$lon,
        .data$lat
      ) |>
      dplyr::summarise(
        key = paste(.data$lon, .data$lat, collapse = "|"),
        .by = c("year", "area_code", "item_cbs_code")
      )
  }
  same <- dplyr::inner_join(
    cells(old),
    cells(new),
    by = c("year", "area_code", "item_cbs_code", "key")
  )
  cli::cli_text(
    "{nrow(same)} of {nrow(cells(old))} polity-crop-years keep the same cells."
  )
  if (nrow(same) == 0L) {
    return(invisible(NULL))
  }
  keep <- function(x) {
    dplyr::semi_join(x, same, by = c("year", "area_code", "item_cbs_code"))
  }
  msn_l1(keep(old), keep(new), "same cell set -- the weight alone")
}

# ---- G. Non-zero allocation -------------------------------------------------

msn_cells <- function(old, new) {
  msn_h("G: cells receiving a non-zero allocation")
  cells <- function(x) {
    x |>
      dplyr::filter(.data$n_t > 0, .data$area_code >= 0L) |>
      dplyr::distinct(.data$lon, .data$lat)
  }
  o <- cells(old)
  n <- cells(new)
  cli::cli_text(
    "old {nrow(o)} cells; new {nrow(n)} cells;
     shared {nrow(dplyr::inner_join(o, n, by = c('lon', 'lat')))};
     old-only {nrow(dplyr::anti_join(o, n, by = c('lon', 'lat')))};
     new-only {nrow(dplyr::anti_join(n, o, by = c('lon', 'lat')))}."
  )
  cli::cli_text(
    "grid rows: old {nrow(old)}, new {nrow(new)}."
  )
}

# ---- Main -------------------------------------------------------------------

msn_main <- function() {
  msn_fingerprint()
  inputs <- msn_inputs()
  msn_report_conversion(inputs$polycells)
  shares <- msn_shares()
  totals <- msn_totals(shares)
  cli::cli_text(
    "spatializing {nrow(totals)} polity totals,
     {msn_num(sum(totals$n_t) / 1e6, 4)} Mt N, over
     {dplyr::n_distinct(shares$item_cbs_code)} crops."
  )
  # `type_cropland` is ~27.5M rows; it is filtered to the reference year here,
  # which is exactly what `.n_read_type_cropland()` does immediately after
  # reading, so the three runs share one copy instead of three.
  data <- list(
    crop_patterns = whep:::.n_read_parquet_env("WHEP_CROP_PATTERNS_PATH"),
    type_cropland = whep:::.n_read_parquet_env("WHEP_TYPE_CROPLAND_PATH") |>
      dplyr::filter(.data$year == msn_year())
  )

  sentinel <- msn_convert(inputs$polycells, "sentinel")
  naive_support <- msn_convert(inputs$polycells, "drop")
  msn_h("C2: which polities each support can place at all")
  totals <- msn_restrict(
    totals,
    list(
      msn_placeable(inputs$crosswalk, data, "polity_frac"),
      msn_placeable(sentinel, data, "land_area_ha"),
      msn_placeable(naive_support, data, "land_area_ha")
    ),
    c("crosswalk", "polycell (sentinel)", "polycell (naive drop)")
  )

  old <- msn_grid(totals, shares, inputs$crosswalk, "polity_frac", data)
  new <- msn_grid(totals, shares, sentinel, "land_area_ha", data)
  naive <- msn_grid(totals, shares, naive_support, "land_area_ha", data)

  msn_footprints(inputs)
  msn_polity_totals(old, new)
  msn_polity_totals(old, naive)
  full <- msn_l1(msn_shares_of(old), msn_shares_of(new), "full migration")
  msn_top_movers(full)
  msn_l1(
    msn_shares_of(new),
    msn_shares_of(naive),
    "sentinel vs naive drop -- the C7 defect, in C5's form"
  )
  msn_pure(old, new, inputs)
  msn_same_cells(msn_shares_of(old), msn_shares_of(new))
  msn_cells(old, new)
}

# ---- C. Footprints ----------------------------------------------------------

msn_footprints <- function(inputs) {
  msn_h("C: footprints")
  cw <- dplyr::distinct(inputs$crosswalk, .data$lon, .data$lat)
  pc <- dplyr::distinct(inputs$polycells, .data$lon, .data$lat)
  cli::cli_text(
    "crosswalk {nrow(cw)} cells; polycell {nrow(pc)} cells;
     shared {nrow(dplyr::inner_join(cw, pc, by = c('lon', 'lat')))};
     crosswalk-only {nrow(dplyr::anti_join(cw, pc, by = c('lon', 'lat')))};
     polycell-only {nrow(dplyr::anti_join(pc, cw, by = c('lon', 'lat')))}."
  )
}

# ---- E2. The pure partition -------------------------------------------------

# AM-29's decomposition. A polity whose CELL SET changed between the two
# supports moves for a reason that is not the partition, and attributing that to
# the key swap is exactly the unattributable movement S-A10 forbids. This
# restricts to polities holding the same cells in both, so what is left is the
# partition alone.
msn_pure <- function(old, new, inputs) {
  msn_h("E2: the pure-partition subset")
  cells_of <- function(x) {
    x |>
      dplyr::filter(!is.na(.data$area_code)) |>
      dplyr::distinct(.data$area_code, .data$lon, .data$lat) |>
      dplyr::arrange(.data$area_code, .data$lon, .data$lat)
  }
  same <- dplyr::inner_join(
    cells_of(inputs$crosswalk) |>
      dplyr::summarise(
        key = paste(.data$lon, .data$lat, collapse = "|"),
        .by = "area_code"
      ),
    cells_of(inputs$polycells) |>
      dplyr::summarise(
        key = paste(.data$lon, .data$lat, collapse = "|"),
        .by = "area_code"
      ),
    by = c("area_code", "key")
  )
  cli::cli_text(
    "{nrow(same)} polities hold exactly the same cells in both supports."
  )
  if (nrow(same) == 0L) {
    return(invisible(NULL))
  }
  keep <- function(x) dplyr::semi_join(x, same, by = "area_code")
  msn_l1(
    msn_shares_of(keep(old)),
    msn_shares_of(keep(new)),
    "same-cell-set polities only"
  )
}

msn_main()
