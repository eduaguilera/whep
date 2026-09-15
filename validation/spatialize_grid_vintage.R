# What changes when level 0 stops being a 2015 snapshot (issue #1000, T39).
#
# ## What is measured
#
# `run_spatialize()` allocates a national total into a *country grid*: one row
# per 0.5-degree cell and reporting `area_code`, carrying that unit's share of
# the cell's land. Until whep#1000 T39 that grid was always the polycell
# support read at ONE reference year, `.carbon_support_year()` = 2015, so every
# year of a run -- 1851 included -- was allocated into the present-day
# cell-to-country map. T31(j) made the grid year-aware, and
# `read_level_country_grid(grid_vintage = )` now selects between the two.
#
# They are two different geographies, not two precisions of one, and this
# script sizes the difference. It measures three things, in increasing distance
# from the support and increasing cost:
#
#   A. THE SUPPORT ITSELF. Per year: how many cells and reporting areas each
#      vintage carries, how much land, and -- weighting each cell by the LUH2
#      cropland and pasture it holds that year -- how much of that land each
#      country LOSES and GAINS between the two vintages, and how much falls in
#      a cell no polity claims that year and is therefore attributed to nobody.
#      Both directions are reported: a one-sided metric cannot see a defect
#      whose signature is absorption, which is how the first version of this
#      script scored a broken share as if it were sound.
#
#   B. THE NATIONAL TABLES THAT HAVE TO LAND IN IT. Per year: which reporting
#      areas of `country_areas` / `livestock_country_data` have no cell at all
#      under each vintage, and the harvested area and head count they carry.
#      `.warn_grid_missing_reporters()` warns about exactly this set at run
#      time; its whole national total is dropped from the gridded output.
#
#   C. THE GRIDDED OUTPUT. For a small year set, `build_gridded_landuse()`
#      runs twice on identical inputs, differing only in the grid, and the two
#      outputs are differenced per cell. Crops only: the livestock engine's
#      exposure is bounded by B, which is where a species' whole national head
#      count is dropped, and the crop engine is the one with a capacity step
#      that can redistribute what the grid moves. Off by default because it is
#      minutes per year; set VAL_GV_ENGINE_YEARS.
#
# A and B are cheap and are the honest bound on C: nothing the engine does can
# put a national total into a cell the grid does not offer it, and nothing
# moves a cell's crops between countries except the grid.
#
# ## What this does NOT measure
#
# The SOC and nitrogen chains read the same polycell support through
# `.carbon_cell_support()`, NOT through `read_level_country_grid()`, so nothing
# here reaches them and `grid_vintage` does not either (whep#1002). Their
# exposure is a code question, answered in the T39 report, not a number this
# script can produce.
#
# ## Usage
#
#   Rscript validation/spatialize_grid_vintage.R
#
#   WHEP_POLYCELL_SUPPORT_PATH  optional; a local support parquet, overriding
#                               the pin, as `read_polycell_support()` reads it.
#   VAL_GV_YEARS                optional; comma-separated years for A and B.
#                               Default: every decade 1851-2021 plus 1961 and
#                               2015.
#   VAL_GV_ENGINE_YEARS         optional; comma-separated years for C. Unset
#                               (default) skips C.
#   VAL_GV_ENGINE_ITEMS         optional; comma-separated `item_prod_code`s
#                               for C. Default: six large-area crops.
#   VAL_GV_REBUILD              optional; "1" forces the year-aware grid to be
#                               rebuilt rather than read from the cache.
#
# The pinned inputs are read through `whep_read_file()`, so a machine with no
# pins cache needs the network for them; the script skips with exit 0 when an
# input cannot be resolved, and says which.
#
# Outputs, under `validation/cache/`:
#   grid_vintage_support.csv    A, one row per year
#   grid_vintage_reporters.csv  B, one row per year and table
#   grid_vintage_missing_areas.csv  B2, the ten largest losers per year
#   grid_vintage_engine.csv     C, one row per year and component (when run)

suppressPackageStartupMessages({
  pkgload::load_all(".", quiet = TRUE)
  library(dplyr)
})

gv_cache <- "validation/cache"
dir.create(gv_cache, showWarnings = FALSE, recursive = TRUE)

.gv_years <- function() {
  set <- Sys.getenv("VAL_GV_YEARS", "")
  if (nzchar(set)) {
    return(sort(unique(as.integer(strsplit(set, ",")[[1L]]))))
  }
  sort(unique(c(seq(1851L, 2021L, by = 10L), 1961L, 2015L)))
}

.gv_engine_years <- function() {
  set <- Sys.getenv("VAL_GV_ENGINE_YEARS", "")
  if (!nzchar(set)) {
    return(integer())
  }
  sort(unique(as.integer(strsplit(set, ",")[[1L]])))
}

.gv_try <- function(what, expr) {
  tryCatch(
    force(expr),
    error = function(e) {
      cli::cli_alert_warning(
        "Could not read {.val {what}}: {conditionMessage(e)}"
      )
      NULL
    }
  )
}

# --- Inputs ------------------------------------------------------------------

support <- .gv_try("polycell_support", read_polycell_support())
if (is.null(support)) {
  cat("METRIC status=skipped reason=no_polycell_support\n")
  quit(save = "no", status = 0L)
}

# The 2015 fold, exactly as `.read_polycell_country_grid()` takes it.
snapshot <- whep:::.carbon_cell_support(support) |>
  tibble::as_tibble()

# The year-aware grid costs minutes to build (152 epoch denominators over
# ~484k rows), so it is cached beside the outputs and rebuilt on request.
gv_grid_cache <- file.path(gv_cache, "grid_vintage_year_aware.rds")
aware <- if (
  file.exists(gv_grid_cache) && !identical(Sys.getenv("VAL_GV_REBUILD"), "1")
) {
  cli::cli_alert_info("Reading the cached year-aware grid.")
  readRDS(gv_grid_cache)
} else {
  cli::cli_alert_info("Building the year-aware grid (minutes).")
  built <- whep:::.polycell_grid_year_aware(support)
  saveRDS(built, gv_grid_cache)
  built
}

cropland <- .gv_try(
  "spatialize-gridded-cropland",
  whep_read_file("spatialize-gridded-cropland")
)
pasture <- .gv_try(
  "spatialize-gridded-pasture",
  whep_read_file("spatialize-gridded-pasture")
)
country_areas <- .gv_try(
  "spatialize-country-areas",
  whep_read_file("spatialize-country-areas")
)
livestock <- .gv_try(
  "spatialize-livestock-country-data",
  whep_read_file("spatialize-livestock-country-data")
)

years <- .gv_years()

# --- A. The support ----------------------------------------------------------

# A cell's land under one vintage, keyed on the reporting code. `cell_area_frac`
# is the share of the cell's LAND, so a weight `w` held by the cell is split
# `w * cell_area_frac` -- which is exactly what the engines do with LUH2
# cropland and pasture.
.gv_weighted_claim <- function(grid, weights, weight_col) {
  grid |>
    dplyr::inner_join(weights, by = c("lon", "lat")) |>
    dplyr::mutate(claim = .data[[weight_col]] * .data$cell_area_frac) |>
    dplyr::summarise(
      claim = sum(.data$claim),
      .by = c("lon", "lat", "area_code")
    )
}

# How much of `weights` each vintage attributes, to whom, and how the two
# attributions differ -- IN BOTH DIRECTIONS.
#
# The comparison has to be two-sided. An earlier version reported only
# `sum(pmax(snap - aware, 0))`, what a country LOSES; a defect that makes one
# country ABSORB a neighbour's hectares is a pure gain and cannot appear in
# such a metric at all, so the metric could not see the share bug it was being
# used to size. Both sides are now reported, with the net between them:
#
#   loss   a country holds it under the snapshot and not under year-aware
#   gain   a country holds it under year-aware and not under the snapshot
#   net    aware_total - snapshot_total, identically gain - loss
#
# `unclaimed` is the part of the loss the year-aware grid cannot place with
# ANY country, because no polity claims that cell in that year; `reattributed`
# is the rest of the loss, which happens inside cells both vintages carry --
# picked up by another country there, or left unattributed because the cell's
# keyable polities do not cover it.
.gv_compare_claims <- function(snapshot, aware_yr, weights, weight_col) {
  a <- .gv_weighted_claim(snapshot, weights, weight_col)
  b <- .gv_weighted_claim(aware_yr, weights, weight_col)
  both <- dplyr::full_join(
    dplyr::rename(a, snap = "claim"),
    dplyr::rename(b, aware = "claim"),
    by = c("lon", "lat", "area_code")
  ) |>
    dplyr::mutate(
      snap = dplyr::coalesce(.data$snap, 0),
      aware = dplyr::coalesce(.data$aware, 0)
    )
  # Only cells the SNAPSHOT carries can be lost by the year-aware read. A cell
  # neither vintage has -- 0.03 Mha of LUH2 cropland sits in cells no polity
  # claims in any epoch -- is a gap in the support, not a difference between
  # the two, and counting it here made `reattributed` come out negative in
  # years where nothing moved at all.
  absent <- weights |>
    dplyr::semi_join(
      dplyr::distinct(snapshot, .data$lon, .data$lat),
      by = c("lon", "lat")
    ) |>
    dplyr::anti_join(
      dplyr::distinct(aware_yr, .data$lon, .data$lat),
      by = c("lon", "lat")
    )
  unclaimed <- sum(absent[[weight_col]])
  loss <- sum(pmax(both$snap - both$aware, 0))
  gain <- sum(pmax(both$aware - both$snap, 0))
  list(
    snapshot_total = sum(both$snap),
    aware_total = sum(both$aware),
    loss = loss,
    gain = gain,
    net = sum(both$aware) - sum(both$snap),
    unclaimed = unclaimed,
    reattributed = loss - unclaimed
  )
}

.gv_weights <- function(table, yr, cols) {
  if (is.null(table)) {
    return(NULL)
  }
  table |>
    dplyr::filter(.data$year == yr) |>
    dplyr::mutate(weight = rowSums(dplyr::across(dplyr::all_of(cols)))) |>
    dplyr::summarise(weight = sum(.data$weight), .by = c("lon", "lat")) |>
    dplyr::filter(.data$weight > 0)
}

.gv_support_row <- function(yr) {
  aware_yr <- whep:::.filter_country_grid_year(aware, yr)
  cells_snap <- dplyr::distinct(snapshot, .data$lon, .data$lat)
  cells_aware <- dplyr::distinct(aware_yr, .data$lon, .data$lat)
  out <- tibble::tibble(
    year = yr,
    n_rows_snapshot = nrow(snapshot),
    n_rows_aware = nrow(aware_yr),
    n_cells_snapshot = nrow(cells_snap),
    n_cells_aware = nrow(cells_aware),
    n_cells_absent = nrow(dplyr::anti_join(
      cells_snap,
      cells_aware,
      by = c("lon", "lat")
    )),
    n_areas_snapshot = dplyr::n_distinct(snapshot$area_code),
    n_areas_aware = dplyr::n_distinct(aware_yr$area_code),
    n_areas_absent = length(setdiff(
      unique(snapshot$area_code),
      unique(aware_yr$area_code)
    )),
    land_snapshot_mha = sum(snapshot$land_area_ha) / 1e6,
    land_aware_mha = sum(aware_yr$land_area_ha) / 1e6
  )
  for (nm in c("cropland", "pasture")) {
    w <- if (nm == "cropland") {
      .gv_weights(cropland, yr, "cropland_ha")
    } else {
      .gv_weights(pasture, yr, c("pasture_ha", "rangeland_ha"))
    }
    if (is.null(w)) {
      next
    }
    cmp <- .gv_compare_claims(snapshot, aware_yr, w, "weight")
    out[[paste0(nm, "_snapshot_mha")]] <- cmp$snapshot_total / 1e6
    out[[paste0(nm, "_aware_mha")]] <- cmp$aware_total / 1e6
    out[[paste0(nm, "_loss_mha")]] <- cmp$loss / 1e6
    out[[paste0(nm, "_gain_mha")]] <- cmp$gain / 1e6
    out[[paste0(nm, "_net_mha")]] <- cmp$net / 1e6
    out[[paste0(nm, "_unclaimed_mha")]] <- cmp$unclaimed / 1e6
    out[[paste0(nm, "_reattributed_mha")]] <- cmp$reattributed / 1e6
    out[[paste0(nm, "_loss_pct")]] <- 100 * cmp$loss / cmp$snapshot_total
    out[[paste0(nm, "_gain_pct")]] <- 100 * cmp$gain / cmp$snapshot_total
  }
  out
}

cli::cli_h2("A. The support, per year")
support_tbl <- purrr::map(years, .gv_support_row) |> purrr::list_rbind()
readr::write_csv(
  support_tbl,
  file.path(gv_cache, "grid_vintage_support.csv")
)
print(
  dplyr::select(
    support_tbl,
    "year",
    "n_cells_absent",
    "n_areas_aware",
    "n_areas_absent",
    dplyr::any_of(c(
      "cropland_loss_pct",
      "cropland_gain_pct",
      "cropland_unclaimed_mha"
    ))
  ),
  n = 40
)

# --- B. The national tables --------------------------------------------------

# A reporting area the grid holds no cell for loses its WHOLE national total:
# `.warn_grid_missing_reporters()` names the set, and the engine's join drops
# it. This is the coverage question a year-aware grid raises and the snapshot
# does not, because the snapshot offers every modern country a cell in 1851.
.gv_reporter_row <- function(yr, national, value_col, label) {
  nat <- national |>
    dplyr::filter(.data$year == yr) |>
    dplyr::summarise(
      value = sum(.data[[value_col]], na.rm = TRUE),
      .by = "area_code"
    ) |>
    dplyr::filter(.data$value > 0)
  if (nrow(nat) == 0L) {
    return(NULL)
  }
  aware_codes <- unique(
    whep:::.filter_country_grid_year(aware, yr)$area_code
  )
  snap_codes <- unique(snapshot$area_code)
  miss_aware <- dplyr::filter(nat, !(.data$area_code %in% aware_codes))
  miss_snap <- dplyr::filter(nat, !(.data$area_code %in% snap_codes))
  tibble::tibble(
    year = yr,
    table = label,
    n_areas = nrow(nat),
    total = sum(nat$value),
    n_areas_missing_snapshot = nrow(miss_snap),
    missing_snapshot = sum(miss_snap$value),
    n_areas_missing_aware = nrow(miss_aware),
    missing_aware = sum(miss_aware$value),
    missing_aware_pct = 100 * sum(miss_aware$value) / sum(nat$value),
    worst_areas_aware = paste(
      utils::head(
        dplyr::arrange(miss_aware, dplyr::desc(.data$value))$area_code,
        8L
      ),
      collapse = "|"
    )
  )
}

cli::cli_h2("B. Reporting areas with no cell")
reporters <- purrr::list_rbind(c(
  if (!is.null(country_areas)) {
    purrr::map(
      years,
      \(y) .gv_reporter_row(y, country_areas, "harvested_area_ha", "crops")
    )
  },
  if (!is.null(livestock)) {
    purrr::map(
      years,
      \(y) .gv_reporter_row(y, livestock, "heads", "livestock")
    )
  }
))
if (nrow(reporters) > 0L) {
  readr::write_csv(
    reporters,
    file.path(gv_cache, "grid_vintage_reporters.csv")
  )
  print(
    dplyr::select(
      reporters,
      "year",
      "table",
      "n_areas",
      "n_areas_missing_aware",
      "missing_aware_pct"
    ),
    n = 80
  )
}

# Which reporting areas, by name, and how much they carry. The per-year
# totals above say how big the coverage hole is; this says whose it is, which
# is what decides whether the hole is a handful of micro-states or the Soviet
# successor set.
.gv_missing_areas <- function(yr, national, value_col, label, k = 10L) {
  nat <- national |>
    dplyr::filter(.data$year == yr) |>
    dplyr::summarise(
      value = sum(.data[[value_col]], na.rm = TRUE),
      .by = "area_code"
    ) |>
    dplyr::filter(.data$value > 0)
  if (nrow(nat) == 0L) {
    return(NULL)
  }
  codes <- unique(whep:::.filter_country_grid_year(aware, yr)$area_code)
  nat |>
    dplyr::filter(!(.data$area_code %in% codes)) |>
    dplyr::mutate(pct_of_world = 100 * .data$value / sum(nat$value)) |>
    dplyr::left_join(.gv_area_names(), by = "area_code") |>
    dplyr::arrange(dplyr::desc(.data$value)) |>
    utils::head(k) |>
    dplyr::mutate(year = yr, table = label, .before = 1L)
}

# `regions_full` keys the reporting vocabulary on `code`; the FAOSTAT name is
# the one the national tables are compiled under, with `name` as the fallback.
.gv_area_names <- function() {
  whep::regions_full |>
    dplyr::transmute(
      area_code = as.integer(.data$code),
      area_name = dplyr::coalesce(.data$FAOSTAT_name, .data$name)
    ) |>
    dplyr::distinct(.data$area_code, .keep_all = TRUE)
}

cli::cli_h2("B2. Which reporting areas lose their grid")
missing_areas <- purrr::list_rbind(c(
  if (!is.null(country_areas)) {
    purrr::map(
      years,
      \(y) .gv_missing_areas(y, country_areas, "harvested_area_ha", "crops")
    )
  },
  if (!is.null(livestock)) {
    purrr::map(
      years,
      \(y) .gv_missing_areas(y, livestock, "heads", "livestock")
    )
  }
))
if (nrow(missing_areas) > 0L) {
  readr::write_csv(
    missing_areas,
    file.path(gv_cache, "grid_vintage_missing_areas.csv")
  )
  print(
    dplyr::filter(missing_areas, .data$year %in% c(1961L, 1991L)),
    n = 40
  )
}

# --- C. The gridded output ---------------------------------------------------

# Both engines, twice, on identical inputs. The only difference is the grid, so
# every hectare that moves is the vintage's doing.
.gv_engine_items <- function() {
  set <- Sys.getenv("VAL_GV_ENGINE_ITEMS", "")
  if (!nzchar(set)) {
    # Wheat, rice, barley, maize, oats and millet: six of the largest area
    # crops, enough to show the mechanism end to end without the memory a
    # 147-crop cartesian over 68k cells needs. A, above, is the global bound.
    return(c(15L, 27L, 44L, 56L, 79L, 116L))
  }
  as.integer(strsplit(set, ",")[[1L]])
}

.gv_engine_row <- function(yr) {
  items <- .gv_engine_items()
  ca <- dplyr::filter(
    country_areas,
    .data$year == yr,
    .data$item_prod_code %in% items
  )
  cl <- dplyr::filter(cropland, .data$year == yr)
  cp <- dplyr::filter(
    whep_read_file("spatialize-crop-patterns"),
    .data$item_prod_code %in% items
  )
  runs <- purrr::map(
    list(
      snapshot_2015 = snapshot,
      year_aware = whep:::.filter_country_grid_year(aware, yr)
    ),
    \(grid) {
      out <- build_gridded_landuse(
        country_areas = ca,
        crop_patterns = cp,
        gridded_cropland = cl,
        country_grid = grid,
        config = list(years = yr)
      )
      if (inherits(out, "data.frame")) out else out[["allocation"]]
    }
  )
  key <- c("lon", "lat", "area_code", "item_prod_code")
  joined <- dplyr::full_join(
    dplyr::summarise(
      runs$snapshot_2015,
      snap = sum(.data$rainfed_ha + .data$irrigated_ha),
      .by = dplyr::all_of(key)
    ),
    dplyr::summarise(
      runs$year_aware,
      aware = sum(.data$rainfed_ha + .data$irrigated_ha),
      .by = dplyr::all_of(key)
    ),
    by = key
  ) |>
    dplyr::mutate(
      snap = dplyr::coalesce(.data$snap, 0),
      aware = dplyr::coalesce(.data$aware, 0)
    )
  tibble::tibble(
    year = yr,
    component = "landuse",
    n_items = length(items),
    total_snapshot_mha = sum(joined$snap) / 1e6,
    total_aware_mha = sum(joined$aware) / 1e6,
    # `dropped` is the NET of the two directions; the two sides are kept
    # beside it, for the same reason A reports both -- a cell-row that gains
    # hectares cancels one that loses them and neither shows in the net.
    #
    # `moved` is (loss + gain) / 2, which measures a TRANSFER only where the
    # two totals agree. Where they do not -- 1961, where a fifth of the six
    # crops' area has no year-aware cell to land in -- it is a midpoint
    # between a drop and a move and must be read beside `dropped`, `loss` and
    # `gain` rather than instead of them.
    dropped_mha = (sum(joined$snap) - sum(joined$aware)) / 1e6,
    dropped_pct = 100 *
      (sum(joined$snap) - sum(joined$aware)) /
      sum(joined$snap),
    loss_mha = sum(pmax(joined$snap - joined$aware, 0)) / 1e6,
    gain_mha = sum(pmax(joined$aware - joined$snap, 0)) / 1e6,
    moved_mha = sum(abs(joined$snap - joined$aware)) / 2e6,
    moved_pct = 100 *
      sum(abs(joined$snap - joined$aware)) /
      (2 * sum(joined$snap)),
    n_rows_snapshot = sum(joined$snap > 0),
    n_rows_aware = sum(joined$aware > 0)
  )
}

engine_years <- .gv_engine_years()
engine_tbl <- NULL
if (length(engine_years) > 0L && !is.null(country_areas)) {
  cli::cli_h2("C. The gridded crop output")
  engine_tbl <- purrr::map(engine_years, .gv_engine_row) |>
    purrr::list_rbind()
  readr::write_csv(
    engine_tbl,
    file.path(gv_cache, "grid_vintage_engine.csv")
  )
  print(engine_tbl, n = 40)
}

worst <- support_tbl |>
  dplyr::filter(!is.na(.data$cropland_loss_pct)) |>
  dplyr::slice_max(.data$cropland_loss_pct, n = 1L)
cat(sprintf(
  paste0(
    "METRIC status=run n_years=%d n_cells_absent_1851=%s ",
    "cropland_loss_pct_max=%s at_year=%s n_engine_years=%d\n"
  ),
  nrow(support_tbl),
  format(support_tbl$n_cells_absent[[1L]]),
  if (nrow(worst) == 1L) sprintf("%.2f", worst$cropland_loss_pct) else "NA",
  if (nrow(worst) == 1L) format(worst$year) else "NA",
  length(engine_years)
))
