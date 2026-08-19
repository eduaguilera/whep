# measure_deposition_categories.R
#
# The DA-14 measurement, required by task C3b of
# `plans/2026-08-03-polycell-spatial-support.md` before the repo owner can
# decide whether the gridded nitrogen ledger should take only the terrestrial
# share of a cell's deposited mass.
#
# DA-10 treats the HaNi per-cell value as a MASS of deposition to land in that
# cell, splits it across the cell's polycells in proportion to territory, and
# reports the land, inland-water and ice shares separately. AM-6 predicts the
# ledger's deposition input would then fall by each cell's water+ice fraction:
# bounded at roughly 3.7% globally ((GLWD 2.4759 + Natural Earth ice
# ex-Antarctica 2.4254 Mkm2) / 13.2795 Gha) and STRUCTURED on Canada, Russia,
# Finland, Sweden, Norway and the Great Lakes. That is an AREA-weighted bound.
# What the decision needs is the DEPOSITION-weighted figure, because
# deposition is concentrated where agriculture and industry are, not on the
# Greenland ice sheet.
#
# What it reports:
#   A  the three input layers and the union territory, against AM-15's
#      whole-domain production build.
#   B  the decomposed support, in DA-19's apportionment shape, and its
#      footprint against the crosswalk.
#   C  conservation across the three categories, to DA-18's 1e-9 relative.
#   D  global terrestrial Tg N before and after, plus the freshwater and ice
#      totals as new quantities.
#   E  the top 20 polities by absolute and by relative change, named.
#   F  AM-6's ~3.7% bound, reconciled with the measured figure.
#   G  the partition mismatch C3b has to reconcile against: how much of the
#      effect is partition-dependent at all.
#
# METHOD, and its one approximation. Territory, inland water and ice are
# measured per CELL by running the shipped build_polycell_support() with a
# single geometry: the s2 union of every live real polity in the reference
# year. Same producer, same GLWD layer, same Natural Earth ice clip, same
# DA-19 clamp as a production build, but one polygon is intersected against
# the grid instead of ~220, which is what makes the measurement affordable. It
# differs from a full per-polity build in exactly one way: a union counts land
# claimed by two polities once, where the sum of per-polity areas counts it
# twice (AM-15 measured that excess at 12.68 Mha, 0.096% of territory, over
# 441 cells at 2015). Section A prints this build's totals beside AM-15's so
# the agreement is on the record rather than assumed.
#
# The per-cell decomposition is then apportioned across the cell's polities by
# `polity_frac`, the deployed crosswalk's partition. That is deliberate rather
# than a shortcut: `polity_frac` is the partition build_ag_land_support() uses
# for the land the ledger charges deposition to, so it is the partition the
# DA-14 question is actually asked in. Section G measures how much of the
# answer could move if that partition changed.
#
# Run:
#   Rscript inst/scripts/measure_deposition_categories.R
#
# Inputs, all resolved from environment variables (never hardcode the path):
#   WHEP_POLITY_FRACTION_PATH cell_polity_fraction.parquet. REQUIRED.
#   WHEP_HANI_DIR             ndep_nhx.nc + ndep_noy.nc. REQUIRED.
#   WHEP_LPJML_INPUT_DIR      grid.clm + glwd_lakes_and_rivers_30arcmin.clm.
#                             REQUIRED.
#   WHEP_NATURALEARTH_DIR     ne_10m_glaciated_areas/. REQUIRED.
#   WHEP_MDC_YEAR             reference year. Optional, default 2014.
#   WHEP_MDC_SUPPORT_RDS      cache for the union territory build. Optional;
#                             written on first run and read afterwards.
#
# Note for anyone whose environment variables look unset: R reads `.Renviron`
# in the working directory INSTEAD of `~/.Renviron`, and this repository has
# its own (issue #456). Run with R_ENVIRON_USER pointing at the user file, or
# export the variables in the shell.

mdc_year <- function() {
  as.integer(Sys.getenv("WHEP_MDC_YEAR", "2014"))
}

mdc_num <- function(x, digits = 4) {
  formatC(x, format = "f", digits = digits, big.mark = ",")
}

mdc_pct <- function(x, digits = 3) {
  sprintf(paste0("%.", digits, "f%%"), 100 * x)
}

# ---- A. Inputs and the union territory --------------------------------------

# Every live real polity in the reference year, as ONE geometry. The
# wiki_status / polity_type / year filters are the producer's own (DA-7,
# DA-24); the year predicate is `.covers_year()`, never a parse of
# `polity_code` (DA-2). A polity s2 cannot read is reported, not dropped in
# silence.
mdc_world_geometry <- function(yr) {
  geoms <- whep::get_polity_geometries()
  attrs <- sf::st_drop_geometry(geoms)
  open <- whep:::.open_ended_intervals(
    attrs$start_year,
    attrs$end_year,
    whep:::.polity_family(as.character(attrs$polity_code))
  )
  keep <- !(attrs$wiki_status %in% c("retired", "superseded")) &
    !(attrs$polity_type %in% "aggregate") &
    whep:::.covers_year(attrs$start_year, attrs$end_year, NULL, yr, open) &
    !sf::st_is_empty(sf::st_geometry(geoms))
  mdc_union(geoms[which(keep), ], yr)
}

mdc_union <- function(live, yr) {
  usable <- whep:::.pcs_usable_geometry(sf::st_geometry(live))
  bad <- which(usable$coverage_status != "has_geometry")
  cli::cli_alert_info(
    "{nrow(live)} live real polit{?y/ies} at {yr}; {length(bad)} unreadable
     by s2 and excluded from the union
     ({.val {live$polity_code[bad]}})."
  )
  geom <- if (length(bad) > 0L) usable$geom[-bad] else usable$geom
  union <- s2::s2_union_agg(
    s2::as_s2_geography(geom),
    options = s2::s2_options(model = "closed")
  )
  sf::st_sf(
    polity_code = "WORLD-1800-2025",
    polity_type = "country",
    wiki_status = "active",
    polygon_status = "assigned",
    start_year = 1800L,
    end_year = 2025L,
    area_code = 1L,
    geom = sf::st_as_sfc(union, crs = 4326)
  )
}

mdc_territory <- function(yr) {
  cache <- Sys.getenv("WHEP_MDC_SUPPORT_RDS", "")
  if (nzchar(cache) && file.exists(cache)) {
    cli::cli_alert_info("Reading cached union territory from {.file {cache}}.")
    return(readRDS(cache))
  }
  water <- whep::read_glwd_water()
  cli::cli_alert_info(
    "GLWD: {nrow(water)} cells, {sum(water$water_frac > 0)} wet (EA10 32,358)."
  )
  ice <- whep::read_glaciated_areas()
  cli::cli_alert_info("Natural Earth ice: {nrow(ice)} features (EA9 1,886).")
  support <- whep::build_polycell_support(
    years = yr,
    geometries = mdc_world_geometry(yr),
    water = water,
    ice = ice
  )
  if (nzchar(cache)) {
    saveRDS(support, cache)
  }
  support
}

# AM-15's whole-domain per-polity build at 2015 is the yardstick. A union
# build that reproduced it poorly would invalidate everything below, so the
# comparison is printed rather than asserted in a comment.
mdc_report_territory <- function(cells) {
  cli::cli_h2("A: union territory versus AM-15's whole-domain build")
  territory <- mdc_num(sum(cells$polity_area_ha) / 1e9, 6)
  land <- mdc_num(sum(cells$land_area_ha) / 1e9, 6)
  water <- mdc_num(sum(cells$inland_water_ha) / 1e8, 6)
  ice <- mdc_num(sum(cells$ice_area_ha) / 1e8, 6)
  residual <- max(abs(
    (cells$land_area_ha +
      cells$inland_water_ha +
      cells$ice_area_ha -
      cells$polity_area_ha) /
      cells$polity_area_ha
  ))
  cli::cli_alert_info(
    "cells {nrow(cells)} | territory {territory} Gha (AM-15 13.2483) |
     land {land} Gha (AM-15 12.7806)."
  )
  cli::cli_alert_info(
    "inland water {water} Mkm2 (AM-15 2.3983) | ice {ice} Mkm2
     (AM-15 2.2789)."
  )
  cli::cli_alert_info(
    "S-A1 max relative residual {sprintf('%.3g', residual)}
     (DA-18 bound 1e-9)."
  )
}

# ---- B. The decomposed support ----------------------------------------------

# DA-19's apportionment shape, which the producer already uses: a cell's
# territory, water and ice are shared out pro rata, so every polity of a cell
# takes the same fraction of each and the categories sum to the polity's own
# territory exactly.
mdc_support <- function(crosswalk, cells) {
  dplyr::inner_join(
    crosswalk,
    dplyr::select(
      cells,
      "lon",
      "lat",
      cell_territory_ha = "polity_area_ha",
      cell_land_ha = "land_area_ha",
      cell_water_ha = "inland_water_ha",
      cell_ice_ha = "ice_area_ha"
    ),
    by = c("lon", "lat")
  ) |>
    dplyr::transmute(
      lon = .data$lon,
      lat = .data$lat,
      area_code = .data$area_code,
      polity_frac = .data$polity_frac,
      cell_area_ha = .data$cell_area_ha,
      polity_area_ha = .data$polity_frac * .data$cell_territory_ha,
      land_area_ha = .data$polity_frac * .data$cell_land_ha,
      inland_water_ha = .data$polity_frac * .data$cell_water_ha,
      ice_area_ha = .data$polity_frac * .data$cell_ice_ha
    )
}

# Cells one side carries and the other does not. Deposition on a cell with no
# measured territory cannot be decomposed at all, so it is named and excluded
# from BOTH the before and the after figure rather than quietly counted in one.
#
# This section is load-bearing, not bookkeeping. The deployed crosswalk has NO
# area_code for Greenland, so most of the world's ice sits on cells the ledger
# cannot reach, and the deposition-weighted ice term in section D is therefore
# a ledger figure rather than a territorial one. Both are printed.
mdc_report_footprint <- function(crosswalk, cells, support, mass) {
  cli::cli_h2("B: footprint of the decomposed support")
  cw_cells <- dplyr::distinct(crosswalk, .data$lon, .data$lat)
  only_cw <- dplyr::anti_join(cw_cells, cells, by = c("lon", "lat"))
  only_pc <- dplyr::anti_join(
    dplyr::distinct(cells, .data$lon, .data$lat),
    cw_cells,
    by = c("lon", "lat")
  )
  dropped <- dplyr::inner_join(mass, only_cw, by = c("lon", "lat"))
  unreached <- dplyr::semi_join(cells, only_pc, by = c("lon", "lat"))
  unreached_mass <- dplyr::inner_join(mass, only_pc, by = c("lon", "lat"))
  cli::cli_alert_info(
    "crosswalk {nrow(crosswalk)} rows / {nrow(cw_cells)} cells; union
     territory {nrow(cells)} cells; decomposed support {nrow(support)} rows."
  )
  cli::cli_alert_info(
    "crosswalk-only {nrow(only_cw)} cells carrying
     {mdc_num(sum(dropped$value_g_total) / 1e12)} Tg N: no measured territory,
     so excluded from both sides."
  )
  cli::cli_alert_info(
    "territory-only {nrow(only_pc)} cells holding
     {mdc_num(sum(unreached$ice_area_ha) / 1e6)} Mha of ice
     ({mdc_pct(sum(unreached$ice_area_ha) / sum(cells$ice_area_ha), 1)} of the
     world's) and {mdc_num(sum(unreached$inland_water_ha) / 1e6)} Mha of
     water, receiving {mdc_num(sum(unreached_mass$value_g_total) / 1e12)}
     Tg N: the ledger cannot reach them."
  )
  mdc_report_unrestricted(cells, mass)
}

# What the DA-14 fractions would be if every cell with territory were
# reachable. The gap between this and section D is exactly what the crosswalk's
# missing polities cost, and it keeps the headline figure from being read as a
# territorial statement when it is a ledger one.
mdc_report_unrestricted <- function(cells, mass) {
  weighted <- dplyr::inner_join(
    cells,
    dplyr::select(mass, "lon", "lat", "value_g_total"),
    by = c("lon", "lat")
  )
  total <- sum(weighted$value_g_total)
  frac <- function(col) {
    sum(weighted$value_g_total * weighted[[col]] / weighted$polity_area_ha) /
      total
  }
  cli::cli_alert_info(
    "over EVERY cell with measured territory: water
     {mdc_pct(frac('inland_water_ha'))}, ice {mdc_pct(frac('ice_area_ha'))},
     together {mdc_pct(frac('inland_water_ha') + frac('ice_area_ha'))}."
  )
}

# The mass the deposition table can actually see: build_n_deposition() inner
# joins the support, so mass on a cell the support lacks never enters either
# side. Conservation has to be checked against THAT total, not against the
# whole HaNi grid, or it would report a 4% shortfall that is a footprint
# difference rather than a leak.
mdc_source_mass <- function(mass, support) {
  cells <- dplyr::distinct(support, .data$lon, .data$lat)
  sum(dplyr::inner_join(mass, cells, by = c("lon", "lat"))$value_g_total)
}

# ---- C, D. Conservation and the DA-14 global figure -------------------------

mdc_deposition <- function(support, nhx, noy, categories) {
  whep::build_n_deposition(
    data = list(nhx = nhx, noy = noy, cell_polity = support),
    categories = categories
  )
}

mdc_report_conservation <- function(decomposed, undecomposed, source_g) {
  cli::cli_h2("C: the three categories conserve the source mass")
  recovered <- sum(decomposed$deposition_n_t) * 1e6
  gap <- abs(recovered - source_g) / source_g
  cli::cli_alert_info(
    "source {sprintf('%.17g', source_g)} g; three categories recover
     {sprintf('%.17g', recovered)} g; relative gap {sprintf('%.3g', gap)}
     (DA-18 bound 1e-9)."
  )
  merged <- dplyr::inner_join(
    dplyr::summarise(
      decomposed,
      after = sum(.data$deposition_n_t),
      .by = c("lon", "lat", "area_code")
    ),
    dplyr::select(
      undecomposed,
      "lon",
      "lat",
      "area_code",
      before = "deposition_n_t"
    ),
    by = c("lon", "lat", "area_code")
  ) |>
    dplyr::filter(.data$before > 0)
  per_polycell <- max(abs(merged$after - merged$before) / merged$before)
  cli::cli_alert_info(
    "per polycell, the three categories re-sum to the undecomposed share:
     max relative gap {sprintf('%.3g', per_polycell)} over {nrow(merged)}
     polycells."
  )
}

mdc_by_category <- function(decomposed) {
  decomposed |>
    dplyr::summarise(
      tg_n = sum(.data$deposition_n_t) / 1e6,
      .by = "area_category"
    ) |>
    dplyr::arrange(dplyr::desc(.data$tg_n))
}

mdc_report_global <- function(decomposed, source_g) {
  cli::cli_h2("D: global terrestrial Tg N before and after (DA-14)")
  by_cat <- mdc_by_category(decomposed)
  total <- sum(by_cat$tg_n)
  land <- by_cat$tg_n[by_cat$area_category == "land"]
  water <- by_cat$tg_n[by_cat$area_category == "inland_water"]
  ice <- by_cat$tg_n[by_cat$area_category == "ice"]
  cli::cli_alert_info(
    "BEFORE (all territory) {mdc_num(total)} Tg N | AFTER (land only)
     {mdc_num(land)} Tg N | change {mdc_num(land - total)} Tg N
     ({mdc_pct((land - total) / total)})."
  )
  cli::cli_alert_info(
    "NEW QUANTITIES: freshwater {mdc_num(water)} Tg N
     ({mdc_pct(water / total)}) | ice {mdc_num(ice)} Tg N
     ({mdc_pct(ice / total)})."
  )
  cli::cli_alert_info(
    "the support sees {mdc_num(source_g / 1e12)} Tg N; the three categories
     account for {mdc_num(total)} Tg N of it."
  )
  invisible(by_cat)
}

# ---- E. The polities that move ----------------------------------------------

mdc_polity_change <- function(decomposed) {
  decomposed |>
    dplyr::mutate(
      polity = dplyr::coalesce(
        .data$reporting_polity_name,
        paste0("area_code ", .data$area_code)
      )
    ) |>
    dplyr::summarise(
      t_n = sum(.data$deposition_n_t),
      .by = c("area_code", "polity", "area_category")
    ) |>
    tidyr::pivot_wider(
      names_from = "area_category",
      values_from = "t_n",
      values_fill = 0
    ) |>
    dplyr::mutate(
      before = .data$land + .data$inland_water + .data$ice,
      change_t = .data$land - .data$before,
      change_pct = 100 * .data$change_t / .data$before
    ) |>
    dplyr::filter(.data$before > 0)
}

mdc_print_top <- function(changes, column, label, n = 20L) {
  cli::cli_h3(label)
  top <- changes |>
    dplyr::arrange(.data[[column]]) |>
    head(n) |>
    dplyr::mutate(
      line = paste0(
        formatC(dplyr::row_number(), width = 2),
        ". ",
        formatC(.data$polity, width = -32, flag = " "),
        formatC(.data$before / 1e3, format = "f", digits = 1, width = 10),
        " ->",
        formatC(.data$land / 1e3, format = "f", digits = 1, width = 10),
        " kt ",
        formatC(.data$change_t / 1e3, format = "f", digits = 1, width = 9),
        " kt ",
        formatC(.data$change_pct, format = "f", digits = 2, width = 8),
        "%"
      )
    )
  cat(paste(top$line, collapse = "\n"), "\n")
  invisible(top)
}

mdc_report_polities <- function(decomposed) {
  cli::cli_h2("E: the polities the DA-14 change moves")
  changes <- mdc_polity_change(decomposed)
  cli::cli_alert_info("{nrow(changes)} polities receive deposition.")
  mdc_print_top(changes, "change_t", "Top 20 by ABSOLUTE change (kt N)")
  mdc_print_top(changes, "change_pct", "Top 20 by RELATIVE change")
  # Matched by pattern, not by literal: the reporting vocabulary carries
  # epoch-suffixed names ("Finland (1940-2025)"), so an exact-string check
  # would silently report AM-6's prediction as unconfirmed for a polity that
  # is in fact ranked second.
  named <- "^Canada|^Russia|^Finland|^Sweden|^Norway|United States"
  hit <- changes |>
    dplyr::mutate(rank_abs = rank(.data$change_t)) |>
    dplyr::filter(grepl(named, .data$polity)) |>
    dplyr::arrange(.data$rank_abs)
  line <- paste0(
    hit$polity,
    " #",
    hit$rank_abs,
    " (",
    sprintf("%.2f%%", hit$change_pct),
    ")",
    collapse = "; "
  )
  cli::cli_alert_info("AM-6's named polities, by absolute rank: {line}.")
  mdc_report_water_cells(decomposed)
  invisible(changes)
}

# AM-6 also names the Great Lakes basin, which is a place rather than a
# polity, so the polity table cannot confirm or refute it. The cells taking
# the most deposition to freshwater can: if the prediction holds they are the
# large lakes by name, and their coordinates say which. The Great Lakes box is
# then located explicitly, so "not in the top 12" is reported as a rank rather
# than as an absence.
mdc_report_water_cells <- function(decomposed) {
  cli::cli_h3("Top 12 cells by deposition to freshwater")
  ranked <- decomposed |>
    dplyr::filter(.data$area_category == "inland_water") |>
    dplyr::summarise(t_n = sum(.data$deposition_n_t), .by = c("lon", "lat")) |>
    dplyr::arrange(dplyr::desc(.data$t_n)) |>
    dplyr::mutate(rank = dplyr::row_number())
  top <- head(ranked, 12L)
  cat(
    paste(
      paste0(
        formatC(top$rank, width = 2),
        ". lon ",
        formatC(top$lon, format = "f", digits = 2, width = 7),
        "  lat ",
        formatC(top$lat, format = "f", digits = 2, width = 7),
        "  ",
        formatC(top$t_n, format = "f", digits = 1, width = 8),
        " t N"
      ),
      collapse = "\n"
    ),
    "\n"
  )
  lakes <- dplyr::filter(
    ranked,
    .data$lon > -93,
    .data$lon < -75,
    .data$lat > 40.5,
    .data$lat < 49.5
  )
  cli::cli_alert_info(
    "Great Lakes box (lon -93..-75, lat 40.5..49.5): {nrow(lakes)} cells,
     {mdc_num(sum(lakes$t_n) / 1e3)} kt N to freshwater, best rank
     {min(lakes$rank)} of {nrow(ranked)}."
  )
}

# ---- F, G. The bound, and what is partition-dependent -----------------------

mdc_report_bound <- function(cells, by_cat) {
  cli::cli_h2("F: AM-6's ~3.7% bound, reconciled")
  area_frac <- (sum(cells$inland_water_ha) + sum(cells$ice_area_ha)) /
    sum(cells$polity_area_ha)
  total <- sum(by_cat$tg_n)
  mass_frac <- 1 - by_cat$tg_n[by_cat$area_category == "land"] / total
  cli::cli_alert_info(
    "AM-6 predicted (GLWD 2.4759 + NE ice 2.4254) / 13.2795 Gha = 3.691%, an
     AREA-weighted bound on the raw layers."
  )
  cli::cli_alert_info(
    "measured AREA-weighted water+ice share of territory
     {mdc_pct(area_frac)}: the raw layers lose area to the DA-19 clamp, to ice
     outside any polity, and to a smaller denominator."
  )
  cli::cli_alert_info(
    "measured DEPOSITION-weighted share {mdc_pct(mass_frac)} -- this is the
     DA-14 figure, and it is the one the decision turns on."
  )
}

# The DA-14 change is a per-CELL water fraction: DA-19 apportions water pro
# rata over territory, so every polity of a cell carries the same water
# fraction whichever partition splits the cell. Ice is clipped exactly per
# polycell in a production build, so only the ice sitting in cells more than
# one polity holds is partition-dependent at all. That bounds how much of this
# measurement a later migration of build_ag_land_support() could move.
mdc_report_partition <- function(crosswalk, cells, decomposed) {
  cli::cli_h2("G: how much of the effect is partition-dependent")
  shared <- crosswalk |>
    dplyr::count(.data$lon, .data$lat, name = "polities") |>
    dplyr::filter(.data$polities > 1L)
  n_cells <- nrow(dplyr::distinct(crosswalk, .data$lon, .data$lat))
  ice_cells <- dplyr::filter(cells, .data$ice_area_ha > 0)
  shared_ice <- dplyr::inner_join(ice_cells, shared, by = c("lon", "lat"))
  ice_mass <- decomposed |>
    dplyr::filter(.data$area_category == "ice") |>
    dplyr::inner_join(shared, by = c("lon", "lat"))
  total_ice <- sum(decomposed$deposition_n_t[decomposed$area_category == "ice"])
  cli::cli_alert_info(
    "{nrow(shared)} shared cells of {n_cells}; {nrow(ice_cells)} cells carry
     ice, of which {nrow(shared_ice)} are shared."
  )
  cli::cli_alert_info(
    "ice in shared cells {mdc_num(sum(shared_ice$ice_area_ha) / 1e6)} Mha of
     {mdc_num(sum(ice_cells$ice_area_ha) / 1e6)} Mha
     ({mdc_pct(sum(shared_ice$ice_area_ha) / sum(ice_cells$ice_area_ha), 2)})."
  )
  cli::cli_alert_info(
    "deposition to ice in shared cells
     {mdc_num(sum(ice_mass$deposition_n_t) / 1e6, 6)} Tg of
     {mdc_num(total_ice / 1e6, 6)} Tg: only this is sensitive to WHICH
     partition splits the cell."
  )
}

# ---- Run --------------------------------------------------------------------

cli::cli_h1("DA-14 deposition category measurement")
mdc_ref_year <- mdc_year()
cli::cli_alert_info("Reference year {mdc_ref_year}.")

mdc_cells <- mdc_territory(mdc_ref_year)
mdc_report_territory(mdc_cells)

mdc_crosswalk <- whep::build_cell_polity()
mdc_nhx <- whep::read_n_deposition("nhx", years = mdc_ref_year)
mdc_noy <- whep::read_n_deposition("noy", years = mdc_ref_year)
mdc_mass <- dplyr::full_join(
  mdc_nhx,
  mdc_noy,
  by = c("lon", "lat", "year"),
  suffix = c("_nhx", "_noy")
) |>
  dplyr::mutate(
    value_g_total = dplyr::coalesce(.data$value_g_nhx, 0) +
      dplyr::coalesce(.data$value_g_noy, 0)
  )

mdc_table <- mdc_support(mdc_crosswalk, mdc_cells)
mdc_report_footprint(mdc_crosswalk, mdc_cells, mdc_table, mdc_mass)

mdc_decomposed <- mdc_deposition(mdc_table, mdc_nhx, mdc_noy, "auto")
mdc_undecomposed <- mdc_deposition(mdc_table, mdc_nhx, mdc_noy, "none")
mdc_source_g <- mdc_source_mass(mdc_mass, mdc_table)
mdc_report_conservation(mdc_decomposed, mdc_undecomposed, mdc_source_g)
mdc_cats <- mdc_report_global(mdc_decomposed, mdc_source_g)
mdc_report_polities(mdc_decomposed)
mdc_report_bound(mdc_cells, mdc_cats)
mdc_report_partition(mdc_crosswalk, mdc_cells, mdc_decomposed)
cli::cli_alert_success("DA-14 measurement reported.")
