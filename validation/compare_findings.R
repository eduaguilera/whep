# Compare subnational-derived national totals against WHEP national production.
#
# Reads per-country findings files written by the discover subagents
# (validation/cache/findings/<iso3>.json) plus the cached WHEP production,
# resolves each (country, crop, year) to a WHEP value, and judges the ratio.
# Prints a VERDICTS_JSON block.
#
# Findings files are written by agents (not schema-checked by the workflow
# runtime), so validation lives HERE, at the point of use - see read_findings().
# Each finding must have: country_iso3, crop, year, value (national total summed
# from subnational units), unit, basis, source, url.
#
# Usage:
#   Rscript validation/compare_findings.R [findings_dir_or_file]
#   (defaults to validation/cache/findings; respects VAL_YEAR_MIN/VAL_YEAR_MAX)

suppressPackageStartupMessages({
  devtools::load_all(".")
  library(dplyr)
})

source("validation/validate.R")

# Read findings from a directory of per-country JSON files (or a single file),
# validating required columns at the point of use.
read_findings <- function(path) {
  files <- if (dir.exists(path)) {
    list.files(path, pattern = "\\.json$", full.names = TRUE)
  } else {
    path
  }
  required <- c("country_iso3", "crop", "year", "value", "unit")
  rows <- files |>
    purrr::map(function(f) {
      parsed <- jsonlite::fromJSON(f, simplifyDataFrame = TRUE)
      if (length(parsed) == 0) {
        return(NULL)
      }
      tib <- tibble::as_tibble(parsed)
      missing <- setdiff(required, names(tib))
      if (length(missing) > 0) {
        cli::cli_warn("Skipping {.file {f}}: missing {.val {missing}}.")
        return(NULL)
      }
      tib
    }) |>
    purrr::compact()
  if (length(rows) == 0) {
    cli::cli_abort("No valid findings found at {.path {path}}.")
  }
  out <- dplyr::bind_rows(rows) |>
    dplyr::mutate(value = suppressWarnings(as.numeric(.data$value))) |>
    dplyr::filter(
      !is.na(.data$value),
      !is.na(.data$year),
      !is.na(.data$crop),
      !is.na(.data$country_iso3)
    )
  if (nrow(out) == 0) {
    cli::cli_abort("All findings rows dropped (bad value or missing keys).")
  }
  cli::cli_alert_info(
    "Loaded {nrow(out)} finding{?s} from {length(rows)} file{?s}."
  )
  out
}

args <- commandArgs(trailingOnly = TRUE)
findings_path <- if (length(args) >= 1) {
  args[[1]]
} else {
  "validation/cache/findings"
}
year_min <- as.integer(Sys.getenv("VAL_YEAR_MIN", "1970"))
year_max <- as.integer(Sys.getenv("VAL_YEAR_MAX", "2010"))

production <- readRDS(
  sprintf(".whep_cache/primary_prod_%d_%d.rds", year_min, year_max)
)

findings <- read_findings(findings_path) |>
  dplyr::mutate(
    year = as.integer(.data$year),
    crop_lc = tolower(.data$crop),
    probe_id = sprintf(
      "sub-%s-%s-%d",
      .data$country_iso3,
      .data$crop_lc,
      .data$year
    )
  )

lookups <- whep_validation_lookups()
items <- lookups$items_prod |>
  dplyr::transmute(
    crop_lc = tolower(.data$item_prod),
    item_name = .data$item_prod
  ) |>
  dplyr::distinct(.data$crop_lc, .keep_all = TRUE)

probes <- findings |>
  dplyr::left_join(items, by = "crop_lc") |>
  dplyr::transmute(
    .data$probe_id,
    pool = "subnational",
    layer = "production",
    area_iso3 = .data$country_iso3,
    item_name = dplyr::coalesce(.data$item_name, .data$crop),
    .data$year,
    element = NA_character_,
    # Extraction matches WHEP production rows, which are in tonnes. The
    # finding's own unit (e.g. Mt) is kept as gt_unit below; the judge
    # canonicalises across them.
    unit = "tonnes"
  )

corpus <- findings |>
  dplyr::transmute(
    .data$probe_id,
    gt_value = as.numeric(.data$value),
    gt_unit = .data$unit,
    source = .data$source,
    url = .data$url,
    definition = .data$basis,
    tolerance_pct = 10,
    confidence = "medium"
  )

verdicts <- run_validation(
  probes,
  list(production = production),
  corpus,
  lookups
)

cat("VERDICTS_JSON_START\n")
cat(jsonlite::toJSON(
  verdicts,
  dataframe = "rows",
  auto_unbox = TRUE,
  pretty = TRUE
))
cat("\nVERDICTS_JSON_END\n")
