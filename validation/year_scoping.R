# Year-scoping equivalence check (internal-consistency archetype).
#
# A `years =` / `start_year`-`end_year` window is a request for a SUBSET, not a
# request for a different method. So the invariant is an identity:
#
#     build_x(years = Y)  ==  build_x(full range) |> filter(year in Y)
#
# Nothing in `tests/` can check it. The full builds cost ~170 s / 14 GB
# (primary production) and ~250 s / 23 GB (wide CBS) and read pins, so the
# comparison cannot live in the offline suite -- which is exactly why three
# separate violations of it shipped green on CI:
#
# Issue 623: `.fill_fodder_gaps()` interpolates along the year axis, so a narrow
# window lost EVERY forage crop -- 137 rows, 1.16% of production tonnes, 1.85%
# of `t_ha`, 1.36% of wide-CBS `feed`. PR 570 was 10/10 green while doing it.
# Fixed by PR 626.
#
# Issue 625: the residual underneath 623, later split into issue 665 (Singapore
# slaughter rows), 666 (duck-product rows never synthesised) and 667 (crop
# `t_ha` dropped in `.finalise_primary()`).
#
# PR 570: the trade and stock imputation reads neighbouring years, addressed
# with the `.context_years()` margin rather than a fix.
#
# ## What counts as a failure (the tolerance decision)
#
# The target is EXACT equality, because the identity above is arithmetic, not
# empirical. Two slacks are unavoidable and both are explicit:
#
#  1. Floating point. The two builds sum the same numbers in a different order,
#     so totals differ at the ~1e-15 level per operation. `.scoping_floor`
#     (1e-9 relative) absorbs that and nothing larger.
#  2. Known, filed defects. `gt_year_scoping.json` records the measured state
#     per layer and per unit. A group fails when it exceeds its recorded value
#     plus the floor -- so the check is a regression net today and it RATCHETS:
#     when a filed defect is fixed, re-record with `--record` and the tighter
#     number becomes the new ceiling.
#
# A recorded baseline is therefore a budget for a defect that has an issue
# number, never a claim that the difference is acceptable.
#
# ## Usage
#
#   Rscript validation/year_scoping.R                      # production, 2010
#   Rscript validation/year_scoping.R wide_cbs 2010
#   Rscript validation/year_scoping.R production 2010 --record
#   Rscript validation/year_scoping.R production 2010 --refresh
#
# One layer per process, on purpose: the session build cache would otherwise
# hold the full production AND the full CBS at once. `validate_all.R` shells out
# once per layer for the same reason.
#
# `--refresh` rebuilds the cached FULL build. Only the full build is cached
# (`.whep_cache/`, gitignored): it is the expensive half and a scoping fix by
# definition leaves it untouched, while the scoped half is rebuilt every run so
# a stale cache cannot mask a regression. Pass `--refresh` when WHEP code or its
# input pins change anything the full build reads.
#
# Exits non-zero when any group fails, so CI can gate on it.

suppressPackageStartupMessages({
  devtools::load_all(".")
  library(dplyr)
})

source("validation/validate.R")

scoping_baseline <- "validation/gt_year_scoping.json"

# Floating-point slack on a relative difference. The two builds add the same
# values in a different order; nothing else is forgiven.
.scoping_floor <- 1e-9

# Long-format keys for each layer, plus how to reach a (key, unit, value) shape.
# Production is already long. Wide CBS is melted so one comparator serves both:
# its quantity columns become `unit`, which is also the grain #631 asks for --
# an aggregate over everything hid #625 behind #623.
.scoping_prod_keys <- c(
  "year",
  "area_code",
  "item_prod_code",
  "item_cbs_code",
  "live_anim_code",
  "unit",
  "source"
)

.scoping_cbs_keys <- c("year", "area_code", "item_cbs_code", "unit")

# Named explicitly, not as "every numeric column that is not a key": wide CBS
# also carries `polity_area_code`, which is numeric and is an identifier, so a
# by-type rule silently compares area codes as if they were tonnes.
.scoping_cbs_quantities <- c(
  "production",
  "import",
  "stock_withdrawal",
  "food",
  "feed",
  "export",
  "seed",
  "processing",
  "other_uses",
  "stock_addition",
  "domestic_supply"
)

.scoping_melt_cbs <- function(x) {
  missing <- setdiff(.scoping_cbs_quantities, names(x))
  if (length(missing) > 0) {
    cli::cli_abort("Wide CBS is missing quantity column{?s}: {missing}.")
  }
  x |>
    tidyr::pivot_longer(
      dplyr::all_of(.scoping_cbs_quantities),
      names_to = "unit",
      values_to = "value"
    )
}

# Collapse to one row per key, carrying the row count so a duplication
# difference shows up as well as a value difference.
.scoping_by_key <- function(x, key_cols) {
  x |>
    dplyr::summarise(
      n_rows = dplyr::n(),
      value = sum(.data$value, na.rm = TRUE),
      .by = dplyr::all_of(key_cols)
    )
}

# Per-unit comparison of a scoped build against the full build filtered to the
# same years. Reports row-set loss/gain, total drift, and the worst per-key
# disagreement among keys BOTH builds have -- #625's second half was 20 shared
# rows whose values differed while the row sets agreed.
scoping_compare <- function(scoped, full, key_cols) {
  joined <- dplyr::full_join(
    .scoping_by_key(scoped, key_cols),
    .scoping_by_key(full, key_cols),
    by = key_cols,
    suffix = c("_scoped", "_full")
  ) |>
    dplyr::mutate(
      in_scoped = !is.na(.data$n_rows_scoped),
      in_full = !is.na(.data$n_rows_full),
      # Scale-free and bounded in [0, 1]; 0 when both sides are 0.
      rel_value = dplyr::if_else(
        .data$in_scoped & .data$in_full,
        abs(.data$value_scoped - .data$value_full) /
          pmax(abs(.data$value_scoped), abs(.data$value_full), .scoping_floor),
        NA_real_
      )
    )

  joined |>
    dplyr::summarise(
      rows_scoped = sum(.data$n_rows_scoped[.data$in_scoped]),
      rows_full = sum(.data$n_rows_full[.data$in_full]),
      keys_only_full = sum(!.data$in_scoped),
      keys_only_scoped = sum(!.data$in_full),
      total_scoped = sum(.data$value_scoped, na.rm = TRUE),
      total_full = sum(.data$value_full, na.rm = TRUE),
      rel_total = abs(.data$total_scoped - .data$total_full) /
        pmax(abs(.data$total_full), .scoping_floor),
      max_rel_key = max(c(0, .data$rel_value[!is.na(.data$rel_value)])),
      .by = "unit"
    ) |>
    dplyr::arrange(.data$unit)
}

# Judge each unit against its recorded budget. `baseline` is the per-layer list
# read from gt_year_scoping.json; a unit absent from it is required to be exact.
scoping_verdict <- function(comparison, baseline) {
  budget <- function(field) {
    vapply(
      comparison$unit,
      function(unit) {
        value <- baseline[[unit]][[field]]
        if (is.null(value)) 0 else as.numeric(value)
      },
      numeric(1),
      USE.NAMES = FALSE
    )
  }
  comparison |>
    dplyr::mutate(
      budget_keys_only_full = budget("keys_only_full"),
      budget_keys_only_scoped = budget("keys_only_scoped"),
      budget_rel_total = budget("rel_total"),
      budget_max_rel_key = budget("max_rel_key")
    ) |>
    dplyr::mutate(
      fail = .data$keys_only_full > .data$budget_keys_only_full |
        .data$keys_only_scoped > .data$budget_keys_only_scoped |
        .data$rel_total > .data$budget_rel_total + .scoping_floor |
        .data$max_rel_key > .data$budget_max_rel_key + .scoping_floor
    )
}

# --- Driver -------------------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)
record <- "--record" %in% args
refresh <- "--refresh" %in% args
positional <- args[!startsWith(args, "--")]
layer <- if (length(positional) >= 1) {
  positional[[1]]
} else {
  Sys.getenv("VAL_SCOPING_LAYER", "production")
}
year <- as.integer(
  if (length(positional) >= 2) {
    positional[[2]]
  } else {
    Sys.getenv("VAL_SCOPING_YEAR", "2010")
  }
)
layer <- rlang::arg_match(layer, c("production", "wide_cbs"))
# The budget is recorded PER YEAR, not once per layer. 2010 is the favourable
# year: issue 666 measured its own cluster at 2.18e-04 there and 3.06e-03 at
# 1995, ~14x worse. A single shared budget would let a recording at 1995 raise
# the ceiling a later 2010 run is judged against. An unrecorded year demands
# exactness and therefore fails loudly the first time -- record it deliberately.
year_key <- as.character(year)

cli::cli_h1("Year-scoping equivalence: {layer} at {year}")

if (layer == "production") {
  full <- harness_build_or_cache(
    ".whep_cache/scoping_full_production.rds",
    function() build_primary_production(),
    refresh = refresh
  )
  scoped <- build_primary_production(start_year = year, end_year = year)
  key_cols <- .scoping_prod_keys
} else {
  full <- harness_build_or_cache(
    ".whep_cache/scoping_full_wide_cbs.rds",
    function() get_wide_cbs(),
    refresh = refresh
  )
  scoped <- .scoping_melt_cbs(get_wide_cbs(years = year))
  full <- .scoping_melt_cbs(full)
  key_cols <- .scoping_cbs_keys
}

comparison <- scoping_compare(
  dplyr::filter(scoped, .data$year == .env$year),
  dplyr::filter(full, .data$year == .env$year),
  key_cols
)

if (record) {
  baseline <- if (file.exists(scoping_baseline)) {
    jsonlite::fromJSON(scoping_baseline, simplifyVector = FALSE)
  } else {
    list(layers = list())
  }
  baseline$recorded_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  baseline$note <- paste(
    "Divergence budget per layer and unit, not a tolerance: a scoped build",
    "should equal the full-range build filtered to the same years. Every",
    "non-zero entry is a filed defect. Tighten with --record when one is fixed."
  )
  budgets <- comparison |>
    dplyr::select(
      "unit",
      "keys_only_full",
      "keys_only_scoped",
      "rel_total",
      "max_rel_key"
    )
  baseline$layers[[layer]][[year_key]] <- stats::setNames(
    lapply(
      seq_len(nrow(budgets)),
      function(i) as.list(dplyr::select(budgets[i, ], -"unit"))
    ),
    budgets$unit
  )
  writeLines(
    jsonlite::toJSON(baseline, auto_unbox = TRUE, pretty = TRUE, digits = 17),
    scoping_baseline
  )
  cli::cli_alert_success(
    "Recorded {nrow(comparison)} unit{?s} for {.val {layer}} into
     {.path {scoping_baseline}}."
  )
}

baseline <- if (file.exists(scoping_baseline)) {
  jsonlite::fromJSON(scoping_baseline, simplifyVector = FALSE)
} else {
  list(layers = list())
}
verdict <- scoping_verdict(comparison, baseline$layers[[layer]][[year_key]])

verdict |>
  dplyr::transmute(
    .data$unit,
    .data$rows_scoped,
    .data$rows_full,
    .data$keys_only_full,
    .data$keys_only_scoped,
    .data$rel_total,
    .data$max_rel_key,
    .data$fail
  ) |>
  print(n = Inf, width = Inf)

# A group well inside its budget is the ratchet prompt: the recorded number is a
# defect budget, so it should be re-recorded downwards once the defect is fixed.
slack <- verdict |>
  dplyr::filter(
    !.data$fail,
    .data$keys_only_full < .data$budget_keys_only_full |
      .data$rel_total * 10 < .data$budget_rel_total
  )
if (nrow(slack) > 0) {
  cli::cli_alert_info(
    "Now better than recorded for {.val {slack$unit}}; re-record with
     {.code --record} to tighten the ceiling."
  )
}

cat(sprintf(
  paste0(
    "METRIC layer=%s year=%d units=%d failing=%d keys_only_full=%d ",
    "keys_only_scoped=%d max_rel_total=%.3e max_rel_key=%.3e\n"
  ),
  layer,
  year,
  nrow(verdict),
  sum(verdict$fail),
  sum(verdict$keys_only_full),
  sum(verdict$keys_only_scoped),
  max(c(0, verdict$rel_total)),
  max(c(0, verdict$max_rel_key))
))

if (any(verdict$fail)) {
  failing <- verdict |> dplyr::filter(.data$fail)
  cli::cli_abort(c(
    "Year-scoped {layer} build diverges beyond the recorded baseline in
     {nrow(failing)} unit{?s}: {failing$unit}.",
    i = "A scoped build must equal the full-range build filtered to the same
         years; see {.file validation/year_scoping.R} for why.",
    i = "If the divergence is understood and filed, re-record with
         {.code --record} and say why in the commit message."
  ))
}
cli::cli_alert_success("Year-scoped {layer} matches the full-range build.")
