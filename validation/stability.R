# Time-series stability check (internal-consistency archetype).
#
# WHEP's own production series should be smooth; a single year that deviates
# sharply from its local neighbourhood is a candidate bug (bad gap-fill, a unit
# slip in one year, a boundary artifact) even when each year looks fine on its
# own. This needs NO external ground truth - it flags discontinuities within
# WHEP itself, across all years, for the top-N producers of each crop.
#
# Usage:
#   Rscript validation/stability.R         (uses the cached WHEP production)
# Config via env: VAL_CROPS, VAL_N_COUNTRIES, VAL_YEAR_MIN/MAX, VAL_STAB_THRESH.

suppressPackageStartupMessages({
  devtools::load_all(".")
  library(dplyr)
})

source("validation/validate.R")

# Reference value for spike detection: median of a year's immediate neighbours
# (excludes the year itself, so a one-year spike/dip stands out). x is ordered
# by year within the group.
.neighbor_median <- function(x) {
  n <- length(x)
  vapply(
    seq_len(n),
    function(i) {
      nb <- x[c(i - 1, i + 1)]
      nb <- nb[!is.na(nb) & is.finite(nb)]
      if (length(nb) == 0) NA_real_ else stats::median(nb)
    },
    numeric(1)
  )
}

crops <- strsplit(Sys.getenv("VAL_CROPS", "Wheat,Rice,Maize (corn)"), ",")[[1]]
n_countries <- as.integer(Sys.getenv("VAL_N_COUNTRIES", "5"))
year_min <- as.integer(Sys.getenv("VAL_YEAR_MIN", "1970"))
year_max <- as.integer(Sys.getenv("VAL_YEAR_MAX", "2010"))
# Flag a year whose value is >threshold away from its 3-year local median.
thresh <- as.numeric(Sys.getenv("VAL_STAB_THRESH", "0.4"))

production <- readRDS(
  sprintf(".whep_cache/primary_prod_%d_%d.rds", year_min, year_max)
)
lookups <- whep_validation_lookups()

crop_codes <- lookups$items_prod |>
  dplyr::transmute(
    item_name = .data$item_prod,
    item_prod_code = suppressWarnings(as.integer(.data$item_prod_code))
  ) |>
  dplyr::filter(.data$item_name %in% crops, !is.na(.data$item_prod_code))

iso_by_area <- lookups$regions |>
  dplyr::transmute(
    area_code = suppressWarnings(as.integer(.data$code)),
    area_iso3 = .data$iso3c
  ) |>
  dplyr::filter(!is.na(.data$area_code), !is.na(.data$area_iso3)) |>
  dplyr::distinct(.data$area_code, .keep_all = TRUE)

series <- production |>
  dplyr::filter(
    .data$unit == "tonnes",
    .data$item_prod_code %in% crop_codes$item_prod_code,
    .data$value > 0
  ) |>
  dplyr::inner_join(iso_by_area, by = "area_code") |>
  dplyr::inner_join(crop_codes, by = "item_prod_code") |>
  dplyr::transmute(
    .data$area_iso3,
    .data$item_name,
    year = as.integer(.data$year),
    .data$value
  )

# keep the top-N producers (by max output) per crop
top_keys <- series |>
  dplyr::summarise(
    peak = max(.data$value),
    .by = c("area_iso3", "item_name")
  ) |>
  dplyr::slice_max(.data$peak, n = n_countries, by = "item_name") |>
  dplyr::select("area_iso3", "item_name")

flagged <- series |>
  dplyr::inner_join(top_keys, by = c("area_iso3", "item_name")) |>
  dplyr::arrange(.data$area_iso3, .data$item_name, .data$year) |>
  dplyr::mutate(
    local_median = .neighbor_median(.data$value),
    rel = .data$value / .data$local_median - 1,
    .by = c("area_iso3", "item_name")
  ) |>
  dplyr::filter(
    is.finite(.data$rel),
    abs(.data$rel) > thresh,
    .data$local_median > 0
  ) |>
  dplyr::transmute(
    .data$area_iso3,
    .data$item_name,
    .data$year,
    value = round(.data$value),
    local_median = round(.data$local_median),
    deviation_pct = round(100 * .data$rel, 1)
  ) |>
  dplyr::arrange(dplyr::desc(abs(.data$deviation_pct)))

cat(sprintf(
  "METRIC n_discontinuities=%d threshold=%.0f%% crops=%d top_n=%d years=%d-%d\n",
  nrow(flagged),
  100 * thresh,
  nrow(crop_codes),
  n_countries,
  year_min,
  year_max
))
cat("\nDISCONTINUITIES_JSON_START\n")
cat(jsonlite::toJSON(
  flagged,
  dataframe = "rows",
  auto_unbox = TRUE,
  pretty = TRUE
))
cat("\nDISCONTINUITIES_JSON_END\n")
