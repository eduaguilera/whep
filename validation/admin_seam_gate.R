# Seam gate on a real spatialization run (issue #1000, plan task T29).
#
# ## What is measured
#
# `seam_gate()` judges a back-cast admin-share table, and the cell grid it
# produced, at every seam the resolver found: the year a country's constraint
# starts, and every source, grain, NUTS-version, coverage or indicator switch
# after it. The plan's decision 6 is that the series must be continuous across
# every one of them, and this is the live evidence for that claim.
#
# The three tiers are documented on `seam_gate()` itself and are not restated
# here. What this script adds is where the live inputs come from, and one thing
# the in-suite leg cannot have: the **window scan**. The plan asks the live gate
# to look at every consecutive pair around the seam, not only the seam pair, as
# a cheap check that the back-cast did not simply move the step one year. That
# window is DERIVED -- each series' own first constrained year, widened by
# `VAL_ASG_WINDOW` years -- never a constant. For a country whose statistics
# start in 1961 the default window is the pairs spanning 1959-1962; the plan's
# 1958-1962 is `VAL_ASG_WINDOW=3`. Those years are named here to tie the
# script to the plan; not one of them is written into the code below.
#
# ## Inputs (all local; nothing here reads a pin or the network)
#
#   WHEP_SPATIALIZE_OUT_DIR  required; an existing `run_spatialize()` output
#                            directory. Unset => the script skips, exit 0.
#     - `gridded_landuse_crops.parquet`  the crop-level cells, tier C's input.
#       The CFT aggregation is never read: its rows pool several items.
#     - `admin_coverage.csv`             the run's own coverage report, from
#       which the seam list is rebuilt with the resolver's own
#       `.admin_seam_list()`.
#   VAL_ASG_SHARES           optional; a local parquet or CSV holding the
#                            back-cast share table (the `shares` element of
#                            `backcast_admin_shares()`). Tiers A and B need it.
#                            `run_spatialize()` does not write one today, so
#                            without it this script runs tier C alone, and says
#                            so.
#   VAL_ASG_SEAMS            optional; a local parquet or CSV holding the
#                            resolver's own `seams` table. Prefer it: the run's
#                            `admin_coverage.csv` carries no indicator or NUTS
#                            version, so those two seam kinds CANNOT be
#                            recovered from it and are silently absent
#                            otherwise. The count of kinds found is reported.
#   VAL_ASG_WINDOW           years before each series' first constrained year
#                            the window scan reaches. Default 2.
#   VAL_ASG_PERTURB          perturbation factor (default 1; `--perturb`
#                            without it means 3, see below).
#
# ## Usage
#
#   Rscript validation/admin_seam_gate.R
#   Rscript validation/admin_seam_gate.R --record    # re-record the baseline
#   Rscript validation/admin_seam_gate.R --perturb   # must FAIL
#
# `--perturb` scales every other cell of each (container, crop), ordered by
# (lon, lat), from the seam year onwards. A flat scale would cancel in the cell
# share and leave every tier untouched, so the tripwire has to distort the
# within-country shape at the seam and nowhere else -- which is exactly the
# artefact tier C exists to catch. A run under it MUST fail, and the script
# aborts if it does not: a tripwire that fires on nothing has proved nothing.
#
# The default factor of 3 is not arbitrary. Scaling half the mass by `f`
# renormalises the shares by about `(f + 1) / 2`, so the untouched half's share
# ratio at the seam is about `2 / (f + 1)`. That leaves `check_series_jumps()`'s
# default low bound of 0.55 only once `f > 2.64`; at 1.5, which is a visibly
# large distortion, both halves stay inside the band and the tripwire is silent.
#
# Exits non-zero when any recorded row moved, so CI can gate on it.

suppressPackageStartupMessages({
  pkgload::load_all(".", quiet = TRUE)
  library(dplyr)
})

source("validation/validate.R")

asg_baseline <- "validation/gt_admin_seam_gate.json"
asg_cache <- "validation/cache"

# `max()` that reports an empty or all-missing input as missing rather than as
# `-Inf` with a warning.
.asg_max <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) {
    return(NA_real_)
  }
  max(x)
}

# Relative slack on a recorded number. Every number here is a deterministic
# count or ratio over a fixed run, so nothing larger is forgiven; a recorded
# number is a measured state to re-record when it changes, not a tolerance.
.asg_floor <- 1e-6

# --- Local readers ------------------------------------------------------------

# Parquet or CSV, by extension, always from the local filesystem.
# `whep_read_file()` is deliberately not used: this script must not reach a
# pins board or the network.
asg_read_table <- function(path) {
  if (grepl("\\.parquet$", path, ignore.case = TRUE)) {
    return(tibble::as_tibble(arrow::read_parquet(path)))
  }
  readr::read_csv(path, show_col_types = FALSE, progress = FALSE)
}

# The run's coverage report, mapped onto the resolver's coverage schema so its
# own seam builder can consume it. Two of the six seam kinds cannot survive the
# mapping, because the written report carries neither column.
asg_seams_from_coverage <- function(path) {
  coverage <- readr::read_csv(path, show_col_types = FALSE, progress = FALSE)
  needed <- c("area_code", "item_prod_code", "year", "source", "grain", "level")
  missing <- setdiff(needed, names(coverage))
  if (length(missing) > 0) {
    cli::cli_abort(
      "{.path {path}} is missing {.field {missing}}; it is not an
       {.fn admin_coverage_prototype} file."
    )
  }
  coverage |>
    dplyr::transmute(
      area_code = as.integer(.data$area_code),
      level = as.integer(.data$level),
      item_prod_code = as.integer(.data$item_prod_code),
      year = as.integer(.data$year),
      resolved_source = as.character(.data$source),
      resolved_grain = as.character(.data$grain),
      resolved_nuts_version = NA_character_,
      resolved_indicator = NA_character_,
      reporting_units = as.character(.data$reporting_units)
    ) |>
    whep:::.admin_seam_list()
}

# Only the years the gate scans are read: three pairs per seam year plus the
# window, which is what bounds this script's memory on a full run.
asg_read_cells <- function(path, years) {
  arrow::open_dataset(path) |>
    dplyr::filter(.data$year %in% years) |>
    dplyr::collect() |>
    tibble::as_tibble()
}

asg_cell_years <- function(seams, window) {
  offsets <- seq.int(-(window + 1L), 1L)
  sort(unique(as.vector(outer(as.integer(seams$seam_year), offsets, `+`))))
}

# --- The window scan ----------------------------------------------------------

# Every consecutive pair from `window` years before a series' first seam to one
# year after it, as extra seam rows. Derived per series from the seam list, so
# a country constrained from 1974 and one constrained from 1850 get their own
# windows and no year is written down.
asg_window_seams <- function(seams, window) {
  if (nrow(seams) == 0L) {
    return(seams)
  }
  seams |>
    dplyr::slice_min(
      .data$seam_year,
      n = 1L,
      by = c("area_code", "level", "item_prod_code"),
      with_ties = FALSE
    ) |>
    tidyr::expand_grid(offset = seq.int(-window, 1L)) |>
    dplyr::transmute(
      area_code = .data$area_code,
      level = .data$level,
      item_prod_code = .data$item_prod_code,
      seam_year = .data$seam_year + .data$offset,
      seam_kind = "live_window"
    ) |>
    dplyr::distinct()
}

# --- Perturbation -------------------------------------------------------------

# Scale every other cell of each (container, crop) from the seam year onwards.
asg_perturb <- function(cells, seams, factor) {
  if (factor == 1) {
    return(cells)
  }
  first_seam <- seams |>
    dplyr::summarise(seam_from = min(.data$seam_year), .by = "area_code")
  cells |>
    dplyr::left_join(first_seam, by = "area_code") |>
    dplyr::arrange(.data$area_code, .data$crop_name, .data$lon, .data$lat) |>
    dplyr::mutate(
      rank = dplyr::dense_rank(paste(.data$lon, .data$lat)),
      .by = c("area_code", "crop_name")
    ) |>
    dplyr::mutate(
      scale = dplyr::if_else(
        !is.na(.data$seam_from) &
          .data$year >= .data$seam_from &
          .data$rank %% 2L == 0L,
        factor,
        1
      ),
      rainfed_ha = .data$rainfed_ha * .data$scale,
      irrigated_ha = .data$irrigated_ha * .data$scale
    ) |>
    dplyr::select(-"seam_from", -"rank", -"scale")
}

# --- Measurement summaries ----------------------------------------------------

asg_tier_a_summary <- function(tier_a) {
  tier_a |>
    dplyr::summarise(
      n_series = dplyr::n(),
      n_failing = sum(!.data$pass),
      n_value_basis = sum(.data$basis == "value"),
      max_share_sum_dev = .asg_max(abs(.data$share_sum - 1)),
      max_rel_diff = .asg_max(.data$max_rel_diff),
      .by = "area_code"
    ) |>
    dplyr::mutate(key = as.character(.data$area_code))
}

asg_tier_b_summary <- function(tier_b) {
  tier_b |>
    dplyr::summarise(
      n_pairs = dplyr::n(),
      n_gated = dplyr::first(.data$n_gated),
      n_beyond = dplyr::first(.data$n_beyond),
      frac_beyond = dplyr::first(.data$frac_beyond),
      threshold = dplyr::first(.data$threshold),
      n_failing = sum(!dplyr::coalesce(.data$pass, TRUE)),
      .by = c("area_code", "level")
    ) |>
    dplyr::mutate(key = paste(.data$area_code, .data$level, sep = "|"))
}

asg_tier_c_summary <- function(tier_c) {
  tier_c |>
    dplyr::summarise(
      n_gates = dplyr::n(),
      n_failing = sum(!dplyr::coalesce(.data$pass, TRUE)),
      n_regime_mismatch = sum(.data$n_regime_mismatch, na.rm = TRUE),
      max_excess = .asg_max(.data$excess),
      n_series_seam = sum(.data$n_series_seam, na.rm = TRUE),
      n_flag_seam = sum(.data$n_flag_seam, na.rm = TRUE),
      .by = "area_code"
    ) |>
    dplyr::mutate(key = as.character(.data$area_code))
}

# --- Baseline I/O and judging -------------------------------------------------

asg_recorded_tbl <- function(recorded) {
  if (length(recorded) == 0) {
    return(NULL)
  }
  purrr::map_dfr(names(recorded), function(k) {
    row <- lapply(recorded[[k]], function(v) if (is.null(v)) NA else v)
    tibble::as_tibble(row) |> dplyr::mutate(key = k)
  })
}

# Judge every measured row against its recorded state, and fail equally loudly
# for a recorded row that stopped being measured: a container dropping out of
# the run is exactly the kind of silent change this exists to catch.
asg_judge <- function(measured, recorded, num_fields) {
  reference <- asg_recorded_tbl(recorded)
  if (nrow(measured) == 0L) {
    return(tibble::tibble(
      key = character(),
      fail = logical(),
      why = character()
    ))
  }
  if (is.null(reference)) {
    return(dplyr::mutate(measured, fail = TRUE, why = "not recorded"))
  }
  joined <- measured |>
    dplyr::left_join(reference, by = "key", suffix = c("", "_rec"))
  drifted <- purrr::map(num_fields, function(f) {
    recorded_value <- as.numeric(joined[[paste0(f, "_rec")]])
    abs(joined[[f]] - recorded_value) /
      pmax(abs(recorded_value), .asg_floor) >
      .asg_floor
  })
  judged <- joined |>
    dplyr::mutate(
      missing_record = is.na(.data[[paste0(num_fields[[1]], "_rec")]]),
      fail = .data$missing_record |
        dplyr::coalesce(Reduce(`|`, drifted), TRUE),
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

asg_record_group <- function(measured, drop_cols) {
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
perturb <- as.numeric(Sys.getenv(
  "VAL_ASG_PERTURB",
  if ("--perturb" %in% args) "3" else "1"
))
window <- as.integer(Sys.getenv("VAL_ASG_WINDOW", "2"))

out_dir <- Sys.getenv("WHEP_SPATIALIZE_OUT_DIR")
crops_path <- file.path(out_dir, "gridded_landuse_crops.parquet")
coverage_path <- file.path(out_dir, "admin_coverage.csv")
if (!nzchar(out_dir) || !dir.exists(out_dir) || !file.exists(crops_path)) {
  cli::cli_alert_info(
    "Skipping the seam gate: {.envvar WHEP_SPATIALIZE_OUT_DIR} is unset or
     holds no {.path gridded_landuse_crops.parquet}."
  )
  cat("METRIC status=skipped reason=no_WHEP_SPATIALIZE_OUT_DIR\n")
  quit(save = "no", status = 0L)
}

cli::cli_h1("Seam gate on {.path {basename(out_dir)}}")
if (perturb != 1) {
  cli::cli_alert_warning(
    "{.envvar VAL_ASG_PERTURB}={perturb}: every other cell is scaled from the
     seam year on, so the recorded rows are expected to FAIL."
  )
}

seams_path <- Sys.getenv("VAL_ASG_SEAMS")
seams <- if (nzchar(seams_path) && file.exists(seams_path)) {
  asg_read_table(seams_path)
} else if (file.exists(coverage_path)) {
  cli::cli_alert_info(
    "No {.envvar VAL_ASG_SEAMS}: rebuilding the seam list from
     {.path admin_coverage.csv}, which carries no indicator or NUTS version,
     so those two seam kinds cannot appear."
  )
  asg_seams_from_coverage(coverage_path)
} else {
  cli::cli_abort(c(
    "No seam list.",
    i = "Point {.envvar VAL_ASG_SEAMS} at the resolver's {.field seams} table,
         or run {.fn run_spatialize} at a granted depth so it writes
         {.path admin_coverage.csv}."
  ))
}

if (nrow(seams) == 0L) {
  cli::cli_alert_info("No seams in this run: nothing to gate.")
  cat("METRIC status=skipped reason=no_seams\n")
  quit(save = "no", status = 0L)
}

cli::cli_alert_info(
  "{nrow(seams)} seam row{?s} over
   {dplyr::n_distinct(seams$area_code)} container{?s}; kinds:
   {.val {sort(unique(seams$seam_kind))}}."
)

cells <- asg_read_cells(crops_path, asg_cell_years(seams, window)) |>
  asg_perturb(seams, perturb)
cli::cli_alert_info(
  "{nrow(cells)} cell row{?s} read over
   {dplyr::n_distinct(cells$year)} year{?s}."
)

shares_path <- Sys.getenv("VAL_ASG_SHARES")
shares <- if (nzchar(shares_path) && file.exists(shares_path)) {
  asg_read_table(shares_path)
} else {
  cli::cli_alert_warning(c(
    "Tiers A and B skipped: {.envvar VAL_ASG_SHARES} is unset or does not
     point at a file.",
    i = "They want the {.field shares} element of
         {.fn backcast_admin_shares}, which {.fn run_spatialize} does not
         write today."
  ))
  NULL
}

gated <- if (is.null(shares)) {
  # Tier C alone still needs a share table's shape to reach the gate, so a
  # zero-row one is passed: tiers A and B then report themselves unevaluated.
  seam_gate(
    tibble::tibble(
      area_code = integer(),
      level = integer(),
      item_prod_code = integer(),
      level_polity_code = character(),
      year = integer(),
      share = numeric(),
      treatment = character()
    ),
    seams,
    cells = cells
  )
} else {
  seam_gate(shares, seams, cells = cells)
}

window_gated <- if (is.null(shares)) {
  NULL
} else {
  cli::cli_h2("Window scan, {window} year{?s} before each series' first seam")
  seam_gate(shares, asg_window_seams(seams, window))
}

tier_a <- asg_tier_a_summary(gated$tier_a)
tier_b <- asg_tier_b_summary(gated$tier_b)
tier_c <- asg_tier_c_summary(gated$tier_c)
window_b <- if (is.null(window_gated)) {
  asg_tier_b_summary(gated$tier_b[0, ])
} else {
  asg_tier_b_summary(window_gated$tier_b)
}

cli::cli_h2("Tier C, per container")
gated$tier_c |> print(n = 40, width = Inf)

dir.create(asg_cache, showWarnings = FALSE, recursive = TRUE)
readr::write_csv(gated$tier_c, file.path(asg_cache, "admin_seam_gate_c.csv"))
readr::write_csv(gated$tier_b, file.path(asg_cache, "admin_seam_gate_b.csv"))

if (record) {
  baseline <- list(
    recorded_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    run = list(
      env = "WHEP_SPATIALIZE_OUT_DIR",
      dir = basename(out_dir),
      n_seams = nrow(seams),
      n_cell_rows = nrow(cells),
      window = window,
      shares = nzchar(shares_path)
    ),
    note = paste(
      "Seam-gate state of one spatialization run: tier A identity at each",
      "anchor, tier B seam log-ratios against the observed distribution, tier",
      "C cell-share jump rates around each seam. Not a tolerance: every number",
      "is a measurement to be re-recorded when the run changes."
    ),
    tier_a = asg_record_group(tier_a, "area_code"),
    tier_b = asg_record_group(tier_b, c("area_code", "level")),
    tier_c = asg_record_group(tier_c, "area_code"),
    window_b = asg_record_group(window_b, c("area_code", "level"))
  )
  writeLines(
    jsonlite::toJSON(baseline, auto_unbox = TRUE, pretty = TRUE, digits = 17),
    asg_baseline
  )
  cli::cli_alert_success("Recorded into {.path {asg_baseline}}.")
}

baseline <- if (file.exists(asg_baseline)) {
  jsonlite::fromJSON(asg_baseline, simplifyVector = FALSE)
} else {
  list()
}

verdict <- dplyr::bind_rows(
  asg_judge(
    tier_a,
    baseline$tier_a,
    c("n_series", "n_failing", "n_value_basis", "max_share_sum_dev")
  ) |>
    dplyr::mutate(group = "tier_a"),
  asg_judge(
    tier_b,
    baseline$tier_b,
    c("n_pairs", "n_gated", "n_beyond", "frac_beyond", "n_failing")
  ) |>
    dplyr::mutate(group = "tier_b"),
  asg_judge(
    tier_c,
    baseline$tier_c,
    c("n_gates", "n_failing", "n_regime_mismatch", "n_flag_seam")
  ) |>
    dplyr::mutate(group = "tier_c"),
  asg_judge(
    window_b,
    baseline$window_b,
    c("n_pairs", "n_gated", "n_beyond", "frac_beyond", "n_failing")
  ) |>
    dplyr::mutate(group = "window_b")
)

cli::cli_h2("Judged against {.path {asg_baseline}}")
verdict |>
  dplyr::select("group", "key", "fail", "why") |>
  dplyr::filter(.data$fail) |>
  print(n = 40, width = Inf)

n_moved <- sum(verdict$fail)
n_gate_fail <- sum(tier_a$n_failing) +
  sum(tier_b$n_failing) +
  sum(tier_c$n_failing)
cat(sprintf(
  paste0(
    "METRIC status=run n_seams=%d n_seam_kinds=%d n_cell_rows=%d ",
    "n_tier_c_gates=%d n_gate_failures=%d n_moved=%d window=%d perturb=%s\n"
  ),
  nrow(seams),
  dplyr::n_distinct(seams$seam_kind),
  nrow(cells),
  nrow(gated$tier_c),
  n_gate_fail,
  n_moved,
  window,
  format(perturb)
))

if (n_moved > 0) {
  cli::cli_abort(
    "{n_moved} recorded row{?s} moved against {.path {asg_baseline}}; re-record
     with {.code --record} once the change is understood."
  )
}
if (perturb != 1 && !record) {
  cli::cli_abort(c(
    "The perturbation moved no recorded row, so it proved nothing.",
    x = "{.envvar VAL_ASG_PERTURB}={perturb} left every gate where it was.",
    i = "Raise the factor until the untouched half's share ratio leaves
         {.fn check_series_jumps}'s band, or fix the gate."
  ))
}
cli::cli_alert_success("All {nrow(verdict)} recorded rows match.")
