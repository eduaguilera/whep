# Variable-aware comparator. Given a variable name and a ground-truth findings
# file, extract WHEP's value via the registry's extractor, join on the variable's
# grain keys, and judge the ratio (reusing judge_probes' unit canonicalization +
# tolerance). Works for the parameter (cycle_length, cropping_intensity) and
# external (occupation) archetypes; production/area keep using run_validation.
#
# GT findings JSON: an array of objects with gt_value, gt_unit and the keys the
# grain needs - crop (name) and/or area_iso3 and/or year.
#
# Usage:
#   Rscript validation/compare_variable.R <variable> <gt.json> [years_csv]

suppressPackageStartupMessages({
  devtools::load_all(".")
  library(dplyr)
})

source("validation/validate.R")
source("validation/variables.R")

# --- per-variable WHEP extraction + grain plumbing ---------------------------

.prod_cache_path <- function() {
  y0 <- as.integer(Sys.getenv("VAL_YEAR_MIN", "1970"))
  y1 <- as.integer(Sys.getenv("VAL_YEAR_MAX", "2010"))
  sprintf(".whep_cache/primary_prod_%d_%d.rds", y0, y1)
}

.whep_for_variable <- function(variable, years) {
  if (variable == "cycle_length") {
    return(extract_cycle_length())
  }
  prod <- readRDS(.prod_cache_path())
  if (variable == "cropping_intensity") {
    return(extract_cropping_intensity(prod, years))
  }
  if (variable == "occupation") {
    return(extract_occupation_intensity(prod, years))
  }
  if (variable == "land_per_tonne") {
    return(extract_land_per_tonne(prod, years))
  }
  cli::cli_abort(
    "Variable {.val {variable}} uses run_validation (production/area)."
  )
}

.grain_keys <- function(grain) {
  switch(
    grain,
    crop = "item_cbs_code",
    country_crop = c("area_code", "item_cbs_code"),
    country_crop_year = c("area_code", "item_cbs_code", "year"),
    cli::cli_abort("Unsupported grain {.val {grain}}.")
  )
}

.resolve_gt_keys <- function(gt, lookups) {
  # Prefer a provided item_cbs_code; otherwise derive it from the crop name.
  if (rlang::has_name(gt, "item_cbs_code")) {
    gt <- dplyr::mutate(
      gt,
      item_cbs_code = suppressWarnings(as.integer(.data$item_cbs_code))
    )
  } else if (rlang::has_name(gt, "crop")) {
    items <- lookups$items_prod |>
      dplyr::transmute(
        crop_lc = tolower(.data$item_prod),
        item_cbs_code = suppressWarnings(as.integer(.data$item_cbs_code))
      ) |>
      dplyr::distinct(.data$crop_lc, .keep_all = TRUE)
    gt <- gt |>
      dplyr::mutate(crop_lc = tolower(.data$crop)) |>
      dplyr::left_join(items, by = "crop_lc")
  }
  if (rlang::has_name(gt, "area_code")) {
    gt <- dplyr::mutate(
      gt,
      area_code = suppressWarnings(as.integer(.data$area_code))
    )
  } else if (rlang::has_name(gt, "area_iso3")) {
    areas <- lookups$regions |>
      dplyr::transmute(
        area_iso3 = .data$iso3c,
        area_code = suppressWarnings(as.integer(.data$code))
      ) |>
      dplyr::filter(!is.na(.data$area_iso3)) |>
      dplyr::distinct(.data$area_iso3, .keep_all = TRUE)
    gt <- dplyr::left_join(gt, areas, by = "area_iso3")
  }
  if (rlang::has_name(gt, "year")) {
    gt <- dplyr::mutate(gt, year = as.integer(.data$year))
  }
  gt
}

# --- main --------------------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) {
  stop(
    "Usage: Rscript validation/compare_variable.R <variable> <gt.json> [years]"
  )
}
variable <- args[[1]]
gt_path <- args[[2]]
years <- if (length(args) >= 3 && nzchar(args[[3]])) {
  as.integer(strsplit(args[[3]], ",")[[1]])
} else {
  NULL
}
# "ratio" (default): symmetric tolerance. "bound": one-sided - the GT is a
# ceiling (e.g. GAEZ potential); pass if WHEP value stays at/below it.
mode <- if (length(args) >= 4) args[[4]] else "ratio"

reg <- whep_variable_registry()
spec <- reg[reg$name == variable, ]
if (nrow(spec) != 1) {
  cli::cli_abort("Unknown variable {.val {variable}}.")
}
lookups <- whep_validation_lookups()

whep <- .whep_for_variable(variable, years)
gt <- jsonlite::fromJSON(gt_path) |>
  tibble::as_tibble() |>
  .resolve_gt_keys(lookups)

# Join on whatever subset of the grain keys the GT actually provides, so a
# per-crop global reference (e.g. literature occupation) broadcasts across
# WHEP's per-country/year rows.
keys <- intersect(.grain_keys(spec$grain), intersect(names(gt), names(whep)))
if (length(keys) == 0) {
  cli::cli_abort("Ground truth shares no grain key with the WHEP extractor.")
}
joined <- dplyr::inner_join(whep, gt, by = keys)
checks <- if (identical(mode, "bound")) {
  joined |>
    dplyr::mutate(
      ratio = .data$whep_value / .data$gt_value,
      deviation_pct = (.data$ratio - 1) * 100,
      verdict = dplyr::if_else(
        .data$whep_value <= .data$gt_value * (1 + spec$tolerance_pct / 100),
        "within_potential",
        "exceeds_potential"
      )
    )
} else {
  joined |>
    dplyr::mutate(
      whep_unit = spec$unit,
      tolerance_pct = spec$tolerance_pct,
      resolve_status = "ok"
    ) |>
    judge_probes()
}

checks |>
  dplyr::transmute(
    variable = variable,
    dplyr::across(dplyr::any_of(c(
      "area_iso3",
      "area_code",
      "crop",
      "item_cbs_code",
      "year"
    ))),
    .data$verdict,
    .data$whep_value,
    .data$gt_value,
    .data$gt_unit,
    ratio = round(.data$ratio, 3),
    deviation_pct = round(.data$deviation_pct, 1),
    .data$source
  ) |>
  print(n = Inf, width = Inf)
