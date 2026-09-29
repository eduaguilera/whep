# Build the `spatialize-fodder-patterns` pin (whep#1118).
#
# The pooled 0.5-degree fodder harvest fraction that
# `build_soil_carbon_inputs(method_unspatialized = "fodder_pattern")` -- the
# default -- places the fodder crops on: the 16 forage layers of the Monfreda
# et al. (2008) 175-crop archive (doi:10.1029/2007GB002947), aggregated and
# pooled by `.sci_fodder_harvest_fraction()`, in the same schema as the
# `spatialize-crop-patterns` pin (lon, lat, item_prod_code, harvest_fraction).
#
# The package reads the pin; this script only regenerates it. Setting
# `WHEP_MONFREDA_DIR` makes the package rebuild the same layer from the rasters
# on the fly instead (see `.sci_read_fodder_patterns()`).
#
# 1. Fetch the rasters: inst/scripts/download/download_monfreda.R. They land in
#    <dest_dir>/HarvestedAreaYield175Crops_Geotiff.
# 2. Run this script from the package root with `monfreda_dir` pointing there.
# 3. Upload from the ~/whep_inputs project:
#      upload_input(out_path, "spatialize-fodder-patterns", type = "files")
#    and put the printed version into the `spatialize-fodder-patterns` row of
#    inst/extdata/whep_inputs.csv, then run data-raw/whep_inputs.R.
#
# The 2026-09-28 build (version 20260928T091935Z-a07a9) has 419,456 rows:
# 26,216 cells x 16 fodder items (636-649, 651, 655), md5
# 56bd4cea74831b87de7ca965cebec26a (reproduced by this script on 2026-09-28,
# arrow 24.0.0).

devtools::load_all(".", quiet = TRUE)

monfreda_dir <- Sys.getenv(
  "WHEP_MONFREDA_DIR",
  file.path("LPJmL_inputs", "HarvestedAreaYield175Crops_Geotiff")
)
out_path <- file.path(tempdir(), "fodder_patterns.parquet")

fodder_patterns <- whep:::.sci_fodder_harvest_fraction(monfreda_dir)

stopifnot(
  setequal(
    unique(fodder_patterns$item_prod_code),
    whep:::.fodder_earthstat_layers()$item_prod_code
  ),
  all(fodder_patterns$harvest_fraction > 0)
)

# arrow, not nanoparquet: the pinned file was written by arrow, and only the
# same writer reproduces its md5 (the content is identical either way).
arrow::write_parquet(fodder_patterns, out_path)

cli::cli_inform(c(
  v = "Wrote {nrow(fodder_patterns)} rows to {.path {out_path}}.",
  i = "md5 {tools::md5sum(out_path)[[1]]}."
))
