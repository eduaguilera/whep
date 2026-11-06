# Run from a WHEP source checkout:
# Rscript inst/scripts/prepare_cane_history.R observations.csv land.csv out.csv \
#   source_anchored_land
# This writes a local historical_data artifact and manifest, never a pin.
# Inputs carry canonical reporting polity/year, quantities and source metadata.
# All observations are preserved; estimates are separately method-stamped.
# The same territorial source/cohort must be configured as for the consumer.
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 4L) {
  cli::cli_abort("Supply observations.csv, land.csv, output.csv and method.")
}
if (any(file.exists(c(args[[3L]], paste0(args[[3L]], ".manifest.json"))))) {
  cli::cli_abort("Output or manifest exists; choose a new local artifact path.")
}
pkgload::load_all(quiet = TRUE)
observations <- readr::read_csv(
  args[[1L]],
  show_col_types = FALSE,
  col_types = readr::cols(
    .default = readr::col_character(),
    year = readr::col_integer(),
    area_code = readr::col_integer(),
    ha = readr::col_double(),
    tonnes = readr::col_double()
  )
)
land <- readr::read_csv(
  args[[2L]],
  show_col_types = FALSE,
  col_types = readr::cols(
    .default = readr::col_character(),
    year = readr::col_integer(),
    area_code = readr::col_integer(),
    land_area_ha = readr::col_double()
  )
)
artifact <- whep:::.prepare_cane_history(observations, land, args[[4L]])
# Exercise the actual ingestion boundary before writing the dry-run artifact.
prepared <- whep:::.prepare_historical_production(artifact, unique(land$year))
whep:::.check_cane_ingestion(artifact, prepared)
readr::write_csv(artifact, args[[3L]])
manifest <- list(
  method = args[[4L]],
  rows = nrow(artifact),
  observation_rows = nrow(observations),
  year_range = range(artifact$year),
  reporting_polities = unique(artifact$reporting_polity_code),
  max_extrapolation_years = max(artifact$cane_extrapolation_years),
  method_counts = dplyr::count(artifact, .data$method_cane),
  input_sha256 = as.list(vapply(
    stats::setNames(args[1:2], c("observations", "land")),
    digest::digest,
    character(1),
    file = TRUE,
    algo = "sha256"
  )),
  output_sha256 = digest::digest(file = args[[3L]], algo = "sha256"),
  code_sha256 = as.list(vapply(
    c(
      "R/production_cane_history.R",
      "R/build_production.R",
      "inst/scripts/prepare_cane_history.R"
    ),
    digest::digest,
    character(1),
    file = TRUE,
    algo = "sha256"
  )),
  publication = "Local dry-run only; no pin upload or default adoption"
)
jsonlite::write_json(
  manifest,
  paste0(args[[3L]], ".manifest.json"),
  pretty = TRUE,
  auto_unbox = TRUE
)
cli::cli_inform("Wrote {nrow(artifact)} local historical production rows.")
