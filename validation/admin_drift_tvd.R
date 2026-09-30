# Admin-unit drift: how much within-country geography a pattern frozen at one
# reference year cannot represent (issue #1000, plan task T18a).
#
# ## What is measured
#
# `run_spatialize()` allocates a national total to 0.5-degree cells in
# proportion to a weight frozen at circa 2000 (the Monfreda `harvest_fraction`
# for crops, a LUH2 land-use proxy for livestock). Only the LUH2 extent layers
# vary annually, so the *shape* of the allocation inside a country is the
# year-2000 shape in every year of the run.
#
# This script sizes the error that convention makes, using independently
# compiled administrative statistics (states, provinces, prefectures,
# departments, NUTS regions) as the observation. For one country, indicator and
# item, let s_u(t) be admin unit u's share of the national quantity in year t.
# Against a reference year r,
#
#   TVD(t, r) = 0.5 * sum_u | s_u(t) - s_u(r) |
#
# is the total variation distance between the two share vectors: the fraction of
# the national total that sits in a different unit than the reference-year
# pattern implies. It is reported in percent. It is a *lower* bound on the
# allocation error, because a unit-level score cannot see misallocation between
# cells inside one unit.
#
# ## Method (this reproduces the plan's scoping measurement; do not change it
# ## silently -- the recorded table is the evidence Phase 3 is asked with)
#
# - Rows: year >= 1961, `lane == "observational"` (directly transcribed
#   statistics only -- the compilation's `legacy_balanced` lane is its own
#   gap-filling and would score the filler, not the geography), `indicator` in
#   `area` / `livestock_stock`, `unit_canonical` in `ha` / `heads`, value
#   non-missing and non-negative.
# - Rows whose `quality_flag` carries `duplicate_cell` or `admin_level_overlap`
#   are dropped: the first double-counts a unit-year, the second mixes two
#   administrative levels into one share vector.
# - Residual duplicates per (country, indicator, item, year, unit) are collapsed
#   by mean.
# - A pair (t, r) is scored on the units present in BOTH years, with the shares
#   renormalised inside that common set, and only when at least
#   `.adt_min_common_units` units are common and both totals are positive. A
#   country-year with no such item is reported as absent, never as zero drift.
# - Per country and year the per-item scores are summarised two ways: the
#   **area-weighted mean**, weighting each item by its own common-set total in
#   the compared year t (the quantity whose geography the score describes), and
#   the **unweighted median** over items.
#
# The measured indicator is harvested area, which is the quantity the plan's
# decision 8 binds; production is available in the panel and is deliberately not
# scored here, because a production share embeds a within-unit yield.
#
# ## What this does NOT produce
#
# The zero-pattern-unit count that plan task T31(b) needs -- unit x crop pairs
# with reported area > 0 and no positive Monfreda cell -- requires a
# cell-to-unit crosswalk, which does not exist until T37/T12 deliver the level-1
# grid. It is out of scope here by construction, not omitted; the plan produces
# it as the first step of T13's fixture work.
#
# ## Relation to the rest of the validation harness
#
# This **extends** `subnational.workflow.js` / `compare_findings.R` and replaces
# neither. Those score WHEP's *national* value against subnational statistics
# summed to a national total: a check on the headline figure WHEP ingests. This
# script never uses a national value as skill; it measures the *within-country
# share geography* that the national figure says nothing about, and reads a
# different artifact (the harmonized panel at `WHEP_SUBNATIONAL`, not
# `cache/findings/<iso3>.json`). The one place the two touch is the discrepancy
# section below, which is the same shape of comparison as `compare_findings.R`
# but reported as a distribution per source rather than as a per-probe verdict;
# `nass_sum.R` remains the USA national-sum extractor.
#
# ## Two further outputs the plan needs
#
# - **Discrepancy** (for T31(d)): per source, the distribution of
#   `admin_sum / national_total - 1` per (country, item, year). Needs a WHEP
#   primary-production table; point `VAL_ADT_NATIONAL_CSV` at one (a
#   `build_primary_production()` export with columns `year`, `area_code`,
#   `item_prod_code`, `unit`, `value`). Skipped, with a message, when unset.
# - **Gap years** (for T31(e)): per source, the count and run lengths of
#   interior missing years between each series' first and last observed year.
#   Measured on the cleaned panel, so a gap can also be one the flag filter
#   above opened; that is the panel a reader would actually resolve from.
#
# ## Usage
#
#   Rscript validation/admin_drift_tvd.R
#   Rscript validation/admin_drift_tvd.R --record    # re-record the baseline
#   Rscript validation/admin_drift_tvd.R --perturb   # must FAIL
#   Rscript validation/admin_drift_tvd.R --refresh   # rebuild national cache
#
#   WHEP_SUBNATIONAL        required; the harmonized subnational panel
#                           (parquet). Unset => the script skips, exit 0.
#   VAL_ADT_NATIONAL_CSV    optional; WHEP primary production for the
#                           discrepancy section.
#   VAL_ADT_PERTURB         perturbation factor (default 1; `--perturb` without
#                           it means 1.2).
#
# `--perturb` multiplies every other admin unit's value, ordered by unit id
# within each (country, indicator, item, year), by the factor. A flat scale
# would leave every share -- and therefore every TVD -- untouched, so the
# tripwire has to distort the shape, which is the thing being recorded. A run
# under it MUST fail; that is how this check was shown to fire rather than
# merely to pass.
#
# Exits non-zero when any recorded row moved, so CI can gate on it.

suppressPackageStartupMessages({
  # devtools::load_all() is a thin re-export of this one; calling pkgload
  # directly keeps the script runnable where only pkgload is installed.
  pkgload::load_all(".", quiet = TRUE)
  library(dplyr)
})

source("validation/validate.R")

adt_baseline <- "validation/gt_admin_drift.json"
adt_cache <- "validation/cache"

# --- Measurement parameters ---------------------------------------------------
#
# These are the scoping run's choices. Changing one changes what the recorded
# table means, so each is named here rather than buried in a call.

# Observed constraints begin in 1961 (the plan's period rule).
.adt_year_min <- 1961L

# Below this many units common to both years the share vector is too short for
# TVD to mean anything, and one unit's reclassification dominates the score.
.adt_min_common_units <- 5L

# Relative slack on a recorded number. Everything here is a deterministic sum
# over a fixed panel, so nothing larger is forgiven; a recorded number is a
# measured state to be re-recorded when it changes, not a tolerance.
.adt_floor <- 1e-6

.adt_crop_ref <- 2000L
.adt_crop_years <- c(1961L, 1970L, 1980L, 1990L, 2010L, 2020L)
.adt_crop_countries <- c(
  "Spain",
  "Japan",
  "United States of America",
  "France",
  "Italy",
  "Australia"
)
.adt_crop_items <- c(
  "Wheat",
  "Barley",
  "Maize (corn)",
  "Rice",
  "Potatoes",
  "Grapes",
  "Olives",
  "Soya beans",
  "Oats",
  "Sunflower seed",
  "Tomatoes",
  "Sugar beet",
  "Rye",
  "Beans, dry",
  "Apples",
  "Oranges"
)

# Two references for livestock: 2000 matches the crop reference, 2010 is the
# GLW3 vintage the engine's livestock proxy would be frozen at.
.adt_livestock_refs <- c(2000L, 2010L)
.adt_livestock_years <- c(1961L, 1970L, 1980L, 1990L, 2000L, 2010L, 2020L)
.adt_livestock_countries <- c("France", "Italy", "Australia", "Portugal")
.adt_livestock_items <- c(
  "Cattle, dairy",
  "Cattle, non-dairy",
  "Sheep",
  "Pigs",
  "Goats",
  "Chickens, layers",
  "Chickens, broilers",
  "Horses"
)

# The panel's `indicator` -> the unit its values carry, which is also the unit
# the national production table reports the same quantity in.
.adt_units <- c(area = "ha", livestock_stock = "heads")

# --- Panel --------------------------------------------------------------------

# Read only the columns the measurement uses. The panel is ~90 MB / 8.9M rows,
# and the row filter runs inside arrow so only the observed subset is collected.
adt_read_panel <- function(path) {
  arrow::open_dataset(path) |>
    dplyr::select(
      "year",
      "country_clean",
      "indicator",
      "item_clean",
      "item_code",
      "unit_canonical",
      "value_canonical",
      "admin_unit_id",
      "quality_flag",
      "lane",
      "source_primary"
    ) |>
    dplyr::filter(
      .data$lane == "observational",
      .data$year >= .adt_year_min,
      .data$indicator %in% names(.adt_units),
      .data$unit_canonical %in% .adt_units,
      !is.na(.data$value_canonical),
      .data$value_canonical >= 0
    ) |>
    dplyr::collect()
}

# Drop the two flags that break a share vector, then collapse whatever
# duplicates remain per unit-year by mean.
adt_clean_panel <- function(panel) {
  panel |>
    dplyr::mutate(quality_flag = dplyr::coalesce(.data$quality_flag, "")) |>
    dplyr::filter(
      !stringr::str_detect(
        .data$quality_flag,
        stringr::fixed("duplicate_cell")
      ),
      !stringr::str_detect(
        .data$quality_flag,
        stringr::fixed("admin_level_overlap")
      )
    ) |>
    dplyr::summarise(
      value = mean(.data$value_canonical),
      item_code = dplyr::first(.data$item_code),
      source_primary = .adt_mode(.data$source_primary),
      .by = c(
        "country_clean",
        "indicator",
        "item_clean",
        "year",
        "admin_unit_id"
      )
    )
}

# Most frequent value, ties broken by sort order. Used where a series spans more
# than one source (Japan's three ministry vintages are one series, not three).
.adt_mode <- function(x) {
  names(sort(table(x), decreasing = TRUE))[[1]]
}

# Distort the share vector without touching the totals' order of magnitude:
# every other unit, ordered by id inside each country-indicator-item-year, is
# scaled. A flat scale would cancel in the shares and prove nothing.
adt_perturb <- function(dat, factor) {
  if (factor == 1) {
    return(dat)
  }
  dat |>
    dplyr::mutate(
      unit_rank = rank(.data$admin_unit_id, ties.method = "first"),
      value = dplyr::if_else(
        .data$unit_rank %% 2 == 1,
        .data$value * factor,
        .data$value
      ),
      .by = c("country_clean", "indicator", "item_clean", "year")
    ) |>
    dplyr::select(-"unit_rank")
}

# --- Total variation distance -------------------------------------------------

# One row per (country, indicator, item, year) scored against `ref_year`, on the
# units common to both years with shares renormalised inside that set.
adt_tvd <- function(dat, ref_year, years) {
  reference <- dat |>
    dplyr::filter(.data$year == ref_year) |>
    dplyr::select(
      "country_clean",
      "indicator",
      "item_clean",
      "admin_unit_id",
      value_ref = "value"
    )
  dat |>
    dplyr::filter(.data$year %in% years, .data$year != ref_year) |>
    dplyr::inner_join(
      reference,
      by = c("country_clean", "indicator", "item_clean", "admin_unit_id")
    ) |>
    dplyr::summarise(
      n_units = dplyr::n(),
      total_year = sum(.data$value),
      total_ref = sum(.data$value_ref),
      tvd_pct = 50 *
        sum(abs(
          .data$value /
            sum(.data$value) -
            .data$value_ref / sum(.data$value_ref)
        )),
      .by = c("country_clean", "indicator", "item_clean", "year")
    ) |>
    dplyr::filter(
      .data$n_units >= .adt_min_common_units,
      .data$total_year > 0,
      .data$total_ref > 0
    ) |>
    dplyr::mutate(ref_year = ref_year)
}

# Both summaries the plan reports, plus the item that drives the maximum, which
# is the per-crop figure the "reaches 64%" claim rests on.
adt_aggregate <- function(per_item) {
  per_item |>
    dplyr::summarise(
      n_items = dplyr::n(),
      n_unit_pairs = sum(.data$n_units),
      tvd_weighted = sum(.data$tvd_pct * .data$total_year) /
        sum(.data$total_year),
      tvd_median = stats::median(.data$tvd_pct),
      max_item = .data$item_clean[[which.max(.data$tvd_pct)]],
      max_tvd = max(.data$tvd_pct),
      .by = c("country_clean", "indicator", "ref_year", "year")
    ) |>
    dplyr::arrange(
      .data$indicator,
      .data$country_clean,
      .data$ref_year,
      .data$year
    ) |>
    dplyr::mutate(
      key = paste(
        .data$country_clean,
        .data$indicator,
        .data$ref_year,
        .data$year,
        sep = "|"
      )
    )
}

# --- Discrepancy against the national total (T31(d)) --------------------------

# WHEP's national quantity per (year, area_code, item_prod_code, unit). Only
# numeric and code columns are read, and the file is never written back, so
# `fread()` is safe here (the repo's quote round-trip rule).
adt_read_national <- function(path) {
  data.table::fread(
    path,
    select = c("year", "area_code", "item_prod_code", "unit", "value"),
    showProgress = FALSE
  ) |>
    tibble::as_tibble() |>
    dplyr::filter(
      .data$unit %in% .adt_units,
      .data$year >= .adt_year_min
    ) |>
    dplyr::summarise(
      national = sum(.data$value),
      .by = c("year", "area_code", "item_prod_code", "unit")
    )
}

# The panel names countries; WHEP keys on codes. This is the one place a name
# join is unavoidable, so it is explicit and its misses are counted rather than
# silently dropped.
adt_country_codes <- function(country_names) {
  regions <- whep::regions_full
  tibble::tibble(country_clean = unique(country_names)) |>
    dplyr::mutate(
      area_code = dplyr::coalesce(
        regions$code[match(.data$country_clean, regions$FAOSTAT_name)],
        regions$code[match(.data$country_clean, regions$name)]
      )
    )
}

# `admin_sum / national - 1` per (country, item, year), labelled by source.
adt_discrepancy <- function(panel, national) {
  panel |>
    dplyr::summarise(
      admin_sum = sum(.data$value),
      n_units = dplyr::n(),
      item_code = dplyr::first(.data$item_code),
      n_item_codes = dplyr::n_distinct(.data$item_code),
      source_primary = .adt_mode(.data$source_primary),
      .by = c("country_clean", "indicator", "item_clean", "year")
    ) |>
    dplyr::filter(.data$n_item_codes == 1L) |>
    dplyr::left_join(
      adt_country_codes(panel$country_clean),
      by = "country_clean"
    ) |>
    dplyr::mutate(unit = unname(.adt_units[.data$indicator])) |>
    dplyr::left_join(
      national,
      by = c("year", "area_code", "item_code" = "item_prod_code", "unit")
    ) |>
    dplyr::mutate(
      rel_discrepancy = dplyr::if_else(
        !is.na(.data$national) & .data$national > 0,
        .data$admin_sum / .data$national - 1,
        NA_real_
      )
    )
}

# Quantiles of that ratio per source and indicator. `n_no_national` is part of
# the finding: a source whose items WHEP does not carry cannot be reconciled at
# all, and that is a coverage fact, not a zero discrepancy.
adt_discrepancy_summary <- function(discrepancy) {
  discrepancy |>
    dplyr::summarise(
      n = sum(!is.na(.data$rel_discrepancy)),
      n_no_national = sum(is.na(.data$rel_discrepancy)),
      p05 = .adt_quantile(.data$rel_discrepancy, 0.05),
      p25 = .adt_quantile(.data$rel_discrepancy, 0.25),
      p50 = .adt_quantile(.data$rel_discrepancy, 0.50),
      p75 = .adt_quantile(.data$rel_discrepancy, 0.75),
      p95 = .adt_quantile(.data$rel_discrepancy, 0.95),
      .by = c("source_primary", "indicator")
    ) |>
    dplyr::arrange(.data$indicator, .data$source_primary) |>
    dplyr::mutate(
      key = paste(.data$source_primary, .data$indicator, sep = "|")
    )
}

# Cache key for the national build. `harness_build_or_cache()` keys only on the
# file path, so the source file's identity has to go into the name: otherwise
# re-pointing VAL_ADT_NATIONAL_CSV at a different export silently reuses the
# previous one.
.adt_national_cache <- function(path) {
  sprintf(
    ".whep_cache/admin_drift_national_%s_%.0f.rds",
    tools::file_path_sans_ext(basename(path)),
    as.numeric(file.info(path)$mtime)
  )
}

.adt_quantile <- function(x, prob) {
  x <- x[!is.na(x)]
  if (length(x) == 0) {
    return(NA_real_)
  }
  unname(stats::quantile(x, probs = prob, names = FALSE))
}

# --- Interior gap years (T31(e)) ----------------------------------------------

# One row per interior gap run: the years missing between two consecutive
# observed years of a series. `fill_proxy_growth()`'s `max_gap` settings are
# chosen against this distribution, so the run lengths matter, not just the
# count.
adt_gap_runs <- function(panel) {
  panel |>
    dplyr::arrange(
      .data$country_clean,
      .data$indicator,
      .data$item_clean,
      .data$admin_unit_id,
      .data$year
    ) |>
    dplyr::mutate(
      run_length = .data$year - dplyr::lag(.data$year) - 1L,
      .by = c("country_clean", "indicator", "item_clean", "admin_unit_id")
    ) |>
    dplyr::filter(!is.na(.data$run_length), .data$run_length > 0L) |>
    dplyr::select(
      "country_clean",
      "indicator",
      "item_clean",
      "admin_unit_id",
      "source_primary",
      "year",
      "run_length"
    )
}

# Per series: span, observations, and the gaps inside it.
adt_gap_series <- function(panel, runs) {
  gaps <- runs |>
    dplyr::summarise(
      n_gap_runs = dplyr::n(),
      gap_years = sum(.data$run_length),
      max_run = max(.data$run_length),
      .by = c("country_clean", "indicator", "item_clean", "admin_unit_id")
    )
  panel |>
    dplyr::summarise(
      source_primary = .adt_mode(.data$source_primary),
      n_sources = dplyr::n_distinct(.data$source_primary),
      first_year = min(.data$year),
      last_year = max(.data$year),
      n_obs = dplyr::n(),
      .by = c("country_clean", "indicator", "item_clean", "admin_unit_id")
    ) |>
    dplyr::left_join(
      gaps,
      by = c("country_clean", "indicator", "item_clean", "admin_unit_id")
    ) |>
    dplyr::mutate(
      n_gap_runs = dplyr::coalesce(.data$n_gap_runs, 0L),
      gap_years = dplyr::coalesce(.data$gap_years, 0L),
      max_run = dplyr::coalesce(.data$max_run, 0L),
      span_years = .data$last_year - .data$first_year + 1L
    )
}

adt_gap_summary <- function(series) {
  series |>
    dplyr::summarise(
      n_series = dplyr::n(),
      n_series_with_gaps = sum(.data$n_gap_runs > 0L),
      gap_years = sum(.data$gap_years),
      observed_years = sum(.data$n_obs),
      max_run = max(.data$max_run),
      median_span = stats::median(.data$span_years),
      .by = c("source_primary", "indicator")
    ) |>
    dplyr::arrange(.data$indicator, .data$source_primary) |>
    dplyr::mutate(
      key = paste(.data$source_primary, .data$indicator, sep = "|")
    )
}

# --- Baseline I/O and judging -------------------------------------------------

# A recorded group is a named list of flat scalar lists keyed as in `measured`.
adt_recorded_tbl <- function(recorded) {
  if (length(recorded) == 0) {
    return(NULL)
  }
  purrr::map_dfr(names(recorded), function(k) {
    row <- lapply(recorded[[k]], function(v) if (is.null(v)) NA else v)
    tibble::as_tibble(row) |> dplyr::mutate(key = k)
  })
}

# Judge every measured row against its recorded state, and fail equally loudly
# for a recorded row that stopped being measured -- a country-year dropping out
# of the panel is exactly the kind of silent change this exists to catch.
adt_judge <- function(measured, recorded, num_fields, chr_fields) {
  reference <- adt_recorded_tbl(recorded)
  if (is.null(reference)) {
    return(dplyr::mutate(measured, fail = TRUE, why = "not recorded"))
  }
  joined <- measured |>
    dplyr::left_join(reference, by = "key", suffix = c("", "_rec"))
  drifted <- purrr::map(num_fields, function(f) {
    recorded_value <- as.numeric(joined[[paste0(f, "_rec")]])
    abs(joined[[f]] - recorded_value) /
      pmax(abs(recorded_value), .adt_floor) >
      .adt_floor
  })
  changed <- purrr::map(chr_fields, function(f) {
    as.character(joined[[f]]) != as.character(joined[[paste0(f, "_rec")]])
  })
  moved <- Reduce(`|`, c(drifted, changed))
  judged <- joined |>
    dplyr::mutate(
      missing_record = is.na(.data[[paste0(num_fields[[1]], "_rec")]]),
      fail = .data$missing_record | dplyr::coalesce(moved, TRUE),
      why = dplyr::case_when(
        .data$missing_record ~ "not recorded",
        .data$fail ~ "moved against baseline",
        .default = ""
      )
    )
  dropped <- reference |>
    dplyr::anti_join(measured, by = "key") |>
    dplyr::transmute(
      key = .data$key,
      fail = TRUE,
      why = "recorded row no longer measured"
    )
  dplyr::bind_rows(judged, dropped)
}

adt_record_group <- function(measured, drop_cols) {
  rows <- dplyr::select(measured, -dplyr::any_of(drop_cols))
  stats::setNames(
    lapply(seq_len(nrow(rows)), function(i) {
      as.list(dplyr::select(rows[i, ], -"key"))
    }),
    rows$key
  )
}

# --- Driver -------------------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)
record <- "--record" %in% args
refresh <- "--refresh" %in% args
perturb <- as.numeric(Sys.getenv(
  "VAL_ADT_PERTURB",
  if ("--perturb" %in% args) "1.2" else "1"
))

panel_path <- Sys.getenv("WHEP_SUBNATIONAL")
if (!nzchar(panel_path) || !file.exists(panel_path)) {
  cli::cli_alert_info(
    "Skipping admin drift: {.envvar WHEP_SUBNATIONAL} is unset or does not
     point at a file."
  )
  cat("METRIC status=skipped reason=no_WHEP_SUBNATIONAL\n")
  quit(save = "no", status = 0L)
}

cli::cli_h1("Admin-unit drift (TVD) from {.path {basename(panel_path)}}")
if (perturb != 1) {
  cli::cli_alert_warning(
    "{.envvar VAL_ADT_PERTURB}={perturb}: every other unit's value is scaled,
     so the recorded rows are expected to FAIL."
  )
}

raw_panel <- adt_read_panel(panel_path)
panel <- adt_clean_panel(raw_panel) |> adt_perturb(perturb)
cli::cli_alert_info(
  "{nrow(raw_panel)} observed row{?s} read, {nrow(panel)} after the flag filter
   and duplicate collapse."
)

crops <- panel |>
  dplyr::filter(
    .data$indicator == "area",
    .data$country_clean %in% .adt_crop_countries,
    .data$item_clean %in% .adt_crop_items
  )
livestock <- panel |>
  dplyr::filter(
    .data$indicator == "livestock_stock",
    .data$country_clean %in% .adt_livestock_countries,
    .data$item_clean %in% .adt_livestock_items
  )

per_item <- dplyr::bind_rows(
  adt_tvd(crops, .adt_crop_ref, .adt_crop_years),
  purrr::map_dfr(.adt_livestock_refs, function(r) {
    adt_tvd(livestock, r, .adt_livestock_years)
  })
)
measured <- adt_aggregate(per_item)

cli::cli_h2("TVD vs the reference year, percent of the national total")
measured |>
  dplyr::select(
    "country_clean",
    "indicator",
    "ref_year",
    "year",
    "n_items",
    "tvd_weighted",
    "tvd_median",
    "max_item",
    "max_tvd"
  ) |>
  print(n = Inf, width = Inf)

cli::cli_h2("Per item, largest first")
per_item |>
  dplyr::arrange(dplyr::desc(.data$tvd_pct)) |>
  dplyr::select(
    "country_clean",
    "indicator",
    "item_clean",
    "ref_year",
    "year",
    "n_units",
    "tvd_pct"
  ) |>
  print(n = 30, width = Inf)

dir.create(adt_cache, showWarnings = FALSE, recursive = TRUE)
readr::write_csv(per_item, file.path(adt_cache, "admin_drift_tvd_by_item.csv"))

# The two companion outputs.
gap_runs <- adt_gap_runs(panel)
gap_series <- adt_gap_series(panel, gap_runs)
gap_summary <- adt_gap_summary(gap_series)
readr::write_csv(gap_series, file.path(adt_cache, "admin_drift_gap_years.csv"))

cli::cli_h2("Interior gap years per source (T31(e))")
gap_summary |> print(n = Inf, width = Inf)

national_path <- Sys.getenv("VAL_ADT_NATIONAL_CSV")
discrepancy_summary <- NULL
if (!nzchar(national_path) || !file.exists(national_path)) {
  cli::cli_alert_warning(c(
    "Discrepancy section skipped: {.envvar VAL_ADT_NATIONAL_CSV} is unset or
     does not point at a file.",
    i = "It wants a {.fn build_primary_production} export with columns
         {.field year}, {.field area_code}, {.field item_prod_code},
         {.field unit}, {.field value}."
  ))
} else {
  national <- harness_build_or_cache(
    .adt_national_cache(national_path),
    function() adt_read_national(national_path),
    refresh = refresh
  )
  discrepancy <- adt_discrepancy(panel, national)
  discrepancy_summary <- adt_discrepancy_summary(discrepancy)
  readr::write_csv(
    discrepancy,
    file.path(adt_cache, "admin_drift_discrepancy.csv")
  )
  cli::cli_h2("admin_sum / national_total - 1 per source (T31(d))")
  discrepancy_summary |> print(n = Inf, width = Inf)
}

if (record) {
  baseline <- list(
    recorded_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    panel = list(
      env = "WHEP_SUBNATIONAL",
      file = basename(panel_path),
      rows_observed = nrow(raw_panel),
      rows_scored = nrow(panel)
    ),
    note = paste(
      "Measured within-country drift of admin-unit share vectors against a",
      "frozen reference year, on directly transcribed statistics only. Not a",
      "tolerance: every number is a measurement to be re-recorded when the",
      "panel changes. `tvd` and `gaps` are judged every run; `discrepancy` is",
      "judged only when VAL_ADT_NATIONAL_CSV supplied national totals."
    ),
    tvd = adt_record_group(measured, c("country_clean", "ref_year")),
    gaps = adt_record_group(gap_summary, "source_primary")
  )
  if (!is.null(discrepancy_summary)) {
    baseline$discrepancy <- adt_record_group(
      discrepancy_summary,
      "source_primary"
    )
  }
  writeLines(
    jsonlite::toJSON(baseline, auto_unbox = TRUE, pretty = TRUE, digits = 17),
    adt_baseline
  )
  cli::cli_alert_success("Recorded into {.path {adt_baseline}}.")
}

baseline <- if (file.exists(adt_baseline)) {
  jsonlite::fromJSON(adt_baseline, simplifyVector = FALSE)
} else {
  list()
}

verdict <- dplyr::bind_rows(
  adt_judge(
    measured,
    baseline$tvd,
    c("n_items", "n_unit_pairs", "tvd_weighted", "tvd_median", "max_tvd"),
    "max_item"
  ) |>
    dplyr::mutate(group = "tvd"),
  adt_judge(
    gap_summary,
    baseline$gaps,
    c(
      "n_series",
      "n_series_with_gaps",
      "gap_years",
      "observed_years",
      "max_run"
    ),
    character()
  ) |>
    dplyr::mutate(group = "gaps")
)
if (is.null(discrepancy_summary)) {
  cli::cli_alert_info(
    "Discrepancy rows not compared: no national totals in this run."
  )
} else {
  verdict <- dplyr::bind_rows(
    verdict,
    adt_judge(
      discrepancy_summary,
      baseline$discrepancy,
      c("n", "n_no_national", "p05", "p25", "p50", "p75", "p95"),
      character()
    ) |>
      dplyr::mutate(group = "discrepancy")
  )
}

cli::cli_h2("Judged against {.path {adt_baseline}}")
verdict |>
  dplyr::select("group", "key", "fail", "why") |>
  dplyr::filter(.data$fail) |>
  print(n = 40, width = Inf)

n_fail <- sum(verdict$fail)
spain_1961 <- measured$tvd_weighted[measured$key == "Spain|area|2000|1961"]
cat(sprintf(
  paste0(
    "METRIC status=run rows_observed=%d rows_scored=%d n_tvd_rows=%d ",
    "n_items_scored=%d spain_area_1961=%s n_gap_sources=%d n_failed=%d ",
    "perturb=%s\n"
  ),
  nrow(raw_panel),
  nrow(panel),
  nrow(measured),
  nrow(per_item),
  if (length(spain_1961) == 1L) sprintf("%.4f", spain_1961) else "NA",
  nrow(gap_summary),
  n_fail,
  format(perturb)
))

if (n_fail > 0) {
  cli::cli_abort(
    "{n_fail} recorded row{?s} moved against {.path {adt_baseline}}; re-record
     with {.code --record} once the change is understood."
  )
}
cli::cli_alert_success("All {nrow(verdict)} recorded rows match.")
