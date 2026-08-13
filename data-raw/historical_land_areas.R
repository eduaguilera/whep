# Materialise the pre-1962 land series measured on each year's own borders.
#
# `build_historical_land_areas()` reads gridded LUH2 once per back-cast year and
# rasterises every polity polygon it needs, so a full 1850-1961 run is tens of
# minutes. The result is STATIC -- it depends only on the LUH2 vintage and the
# polities snapshot, neither of which moves between builds -- so it is computed
# once here rather than inside every
# `build_primary_production(land_method = "historical_polity")` call.
#
# This writes a parquet rather than a `data/*.rda`: it is an INPUT to the
# pipeline, not a harmonisation table, so its home is the pins board that
# `whep_inputs.csv` indexes. Publishing it there is the remaining step (see
# `inst/scripts/prepare_upload.R` and the pin guidance in CLAUDE.md);
# until it is published, pass the parquet to
# `build_historical_land_areas(data = )` or let the pipeline recompute it.
#
# Provenance to record with the pin:
#   * LUH2 states vintage -- read it off the result of
#     `read_luh2_landuse()` with `get_provenance()`; the reference is
#     LUH2-GCB2022, doi:10.5281/zenodo.15556812.
#   * The `polities` snapshot -- `max(whep::polities$last_ingest)`.
#
# Usage:
#   Rscript --vanilla data-raw/historical_land_areas.R [out_path]

devtools::load_all(here::here(), quiet = TRUE)

args <- commandArgs(trailingOnly = TRUE)
out_path <- if (length(args) > 0L) {
  args[[1]]
} else {
  here::here("data-raw", "historical_land_areas.parquet")
}

land <- whep::build_historical_land_areas(years = 1850:1961)

cli::cli_alert_info(
  "{nrow(land)} row{?s}, {dplyr::n_distinct(land$area_code)} bucket{?s},
   {min(land$year)}-{max(land$year)}."
)

arrow::write_parquet(land, out_path)
cli::cli_alert_success("Wrote {.file {out_path}}.")
