# Rank the top-N production countries for a set of crops, to seed the
# subnational-validation subagents (one per country).
#
# Builds (and caches) WHEP production, ranks countries by total tonnes of the
# requested crops in a ranking year, excludes FAO aggregates (keeps only real
# ISO3 countries), and prints a COUNTRIES_JSON block for the workflow to parse.
#
# Config via env vars (all optional):
#   VAL_CROPS=Wheat,Rice,Maize  VAL_N_COUNTRIES=5
#   VAL_YEAR_MIN=1970  VAL_YEAR_MAX=2010  VAL_RANK_YEAR=2010

suppressPackageStartupMessages({
  devtools::load_all(".")
  library(dplyr)
})

source("validation/validate.R")

crops <- strsplit(Sys.getenv("VAL_CROPS", "Wheat,Rice,Maize (corn)"), ",")[[1]]
n_countries <- as.integer(Sys.getenv("VAL_N_COUNTRIES", "5"))
year_min <- as.integer(Sys.getenv("VAL_YEAR_MIN", "1970"))
year_max <- as.integer(Sys.getenv("VAL_YEAR_MAX", "2010"))
rank_year <- as.integer(Sys.getenv("VAL_RANK_YEAR", "2010"))

production <- harness_build_or_cache(
  sprintf(".whep_cache/primary_prod_%d_%d.rds", year_min, year_max),
  function() {
    build_primary_production(start_year = year_min, end_year = year_max) |>
      dplyr::select(
        "area_code",
        "item_prod_code",
        "item_cbs_code",
        "year",
        "unit",
        "value"
      )
  }
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
    iso3 = .data$iso3c,
    name = .data$polity_name
  ) |>
  dplyr::filter(!is.na(.data$area_code), !is.na(.data$iso3)) |>
  dplyr::distinct(.data$area_code, .keep_all = TRUE)

top <- production |>
  dplyr::filter(
    .data$unit == "tonnes",
    .data$year == rank_year,
    .data$item_prod_code %in% crop_codes$item_prod_code
  ) |>
  dplyr::inner_join(iso_by_area, by = "area_code") |>
  dplyr::summarise(
    total = sum(.data$value),
    .by = c("area_code", "iso3", "name")
  ) |>
  dplyr::slice_max(.data$total, n = n_countries, with_ties = FALSE) |>
  dplyr::arrange(dplyr::desc(.data$total))

payload <- list(
  crops = crop_codes$item_name,
  year_min = year_min,
  year_max = year_max,
  rank_year = rank_year,
  countries = dplyr::transmute(top, .data$iso3, .data$name, .data$area_code)
)

cat("COUNTRIES_JSON_START\n")
cat(jsonlite::toJSON(
  payload,
  dataframe = "rows",
  auto_unbox = TRUE,
  pretty = TRUE
))
cat("\nCOUNTRIES_JSON_END\n")
