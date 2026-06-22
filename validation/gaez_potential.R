# Per-country potential cropping intensity from GAEZ multi-cropping suitability
# (a CEILING, not an observed value). Used to bound-check WHEP's observed
# cropping intensity (harvested/physical): observed should not exceed the
# climatic multi-cropping potential, and the >1 pattern should track GAEZ's
# double/triple-crop zones.
#
# GAEZ rasters (LandInG-derived) are local and NOT committed: their dir comes
# from WHEP_GAEZ_DIR or validation/cache/local_paths.json. Output (per-country
# potential) is written under the gitignored cache/files/GAEZ/.
#
# Suitability codes per 0.5-deg cell: -9 NODATA, 0 none, 1 single, 2 double,
# 3 triple cropping. Potential = max(rainfed, irrigated).
#
# Usage: Rscript validation/gaez_potential.R

suppressPackageStartupMessages({
  devtools::load_all(".")
  library(data.table)
})

.resolve_gaez_dir <- function() {
  d <- Sys.getenv("WHEP_GAEZ_DIR", "")
  if (nzchar(d) && dir.exists(d)) {
    return(d)
  }
  cfg <- "validation/cache/local_paths.json"
  if (file.exists(cfg)) {
    p <- jsonlite::fromJSON(cfg)[["WHEP_GAEZ_DIR"]]
    if (!is.null(p) && dir.exists(p)) {
      return(p)
    }
  }
  stop(
    "GAEZ dir not found. Set WHEP_GAEZ_DIR or validation/cache/local_paths.json"
  )
}

# Read a 0.5-deg ESRI ASCII grid into a (lon, lat, value) data.table.
.read_asc_grid <- function(path, ncols = 720L, nrows = 360L, cellsize = 0.5) {
  vals <- scan(path, skip = 6, quiet = TRUE)
  vals[vals == -9] <- NA_real_
  idx <- seq_along(vals) - 1L
  data.table(
    lon = round(-180 + (idx %% ncols + 0.5) * cellsize, 2),
    lat = round(90 - (idx %/% ncols + 0.5) * cellsize, 2),
    value = vals
  )
}

gaez_dir <- .resolve_gaez_dir()
rain <- .read_asc_grid(file.path(
  gaez_dir,
  "multicropping_suitability_GAEZ_rainfed.asc"
))
irr <- .read_asc_grid(file.path(
  gaez_dir,
  "multicropping_suitability_GAEZ_irrigated.asc"
))

cells <- merge(rain, irr, by = c("lon", "lat"), suffixes = c("_r", "_i"))
cells[, potential := pmax(value_r, value_i, na.rm = TRUE)]
cells <- cells[is.finite(potential) & potential >= 1]

grid <- nanoparquet::read_parquet(
  "LPJmL_inputs/whep/inputs/country_grid.parquet"
)
setDT(grid)
grid[, `:=`(
  lon = round(lon, 2),
  lat = round(lat, 2),
  area_code = as.integer(area_code)
)]

cells <- merge(cells, grid, by = c("lon", "lat"))

# Country ceiling = mean potential over its croppable cells (potential >= 1).
ceiling <- cells[,
  .(gt_value = round(mean(potential), 3), n_cells = .N),
  by = area_code
]

iso <- as.data.table(whep::regions_full)[,
  .(area_code = suppressWarnings(as.integer(code)), area_iso3 = iso3c)
]
iso <- iso[!is.na(area_iso3)][!duplicated(area_code)]
out <- merge(ceiling, iso, by = "area_code")[n_cells >= 3]

out[, `:=`(
  gt_unit = "ratio",
  source = "GAEZ v3 multi-cropping suitability (LandInG), mean over croppable cells",
  url = "local:GAEZ",
  basis = "POTENTIAL cropping-intensity ceiling (climatic), not observed"
)]

dir.create(
  "validation/cache/ground_truth",
  recursive = TRUE,
  showWarnings = FALSE
)
writeLines(
  jsonlite::toJSON(
    out[, .(
      area_iso3,
      area_code,
      gt_value,
      gt_unit,
      n_cells,
      source,
      url,
      basis
    )],
    dataframe = "rows",
    auto_unbox = TRUE,
    pretty = TRUE
  ),
  "validation/cache/ground_truth/cropping_intensity.json"
)
cat(sprintf("Wrote GAEZ potential for %d countries.\n", nrow(out)))
print(out[
  area_iso3 %in% c("USA", "CHN", "IND", "BRA", "IDN", "EGY", "VNM"),
  .(area_iso3, gt_value, n_cells)
])
