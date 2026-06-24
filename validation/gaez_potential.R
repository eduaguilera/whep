# Per-country potential cropping intensity from GAEZ v4 multiple-cropping zones
# (a CEILING, not an observed value). Used to bound-check WHEP's observed
# cropping intensity (harvested/physical): observed should not exceed the
# climatic multi-cropping potential, and the >1 pattern should track GAEZ's
# double/triple-crop zones.
#
# Source: GAEZ v4 (FAO/IIASA, open access CC BY 4.0) multiple-cropping zones,
# rainfed (mcr) and irrigated (mci), 1981-2010 climatology, downloaded from the
# open FAO Google Cloud Storage bucket (no local copy needed; WHEP_GAEZ_DIR
# still overrides with local layers by basename). Output (per-country potential)
# is written under the gitignored cache/files/GAEZ/.
#
# GAEZ v4 multiple-cropping-zone classes (1-8) are mapped to crops-per-year:
#   1 no cropping -> NA; 2 single -> 1; 3 limited double / 4 double /
#   5 double+rice / 6 double rice -> 2; 7 triple / 8 triple rice -> 3.
# Potential per cell = max(rainfed, irrigated), aggregated 5-arc-min -> 0.5 deg.
#
# Usage: Rscript validation/gaez_potential.R

suppressPackageStartupMessages({
  devtools::load_all(".")
  library(data.table)
  library(terra)
})

.gaez_layer <- function(key) {
  override <- Sys.getenv("WHEP_GAEZ_DIR", "")
  if (nzchar(override)) {
    local <- file.path(override, basename(key))
    if (file.exists(local)) {
      return(local)
    }
  }
  out <- "validation/cache/files/GAEZ"
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  dest <- file.path(out, basename(key))
  if (!file.exists(dest)) {
    url <- paste0("https://storage.googleapis.com/gaez-v4-data/", key)
    message("Downloading GAEZ v4 layer ", basename(key))
    utils::download.file(url, dest, mode = "wb", quiet = TRUE)
  }
  dest
}

# Multiple-cropping-zone class (1-8) -> crops-per-year, aggregated to 0.5 deg.
.mcz_intensity <- function(key) {
  terra::rast(.gaez_layer(key)) |>
    terra::subst(from = 1:8, to = c(NA, 1, 2, 2, 2, 2, 3, 3)) |>
    terra::aggregate(fact = 6, fun = "mean", na.rm = TRUE)
}

rain <- .mcz_intensity("data/res01/CRUTS32/Hist/mcr_CRUTS32_Hist_8110.tif")
irr <- .mcz_intensity("data/res01/CRUTS32/Hist/mci_CRUTS32_Hist_8110.tif")

grid <- nanoparquet::read_parquet(
  "LPJmL_inputs/whep/inputs/country_grid.parquet"
)
setDT(grid)
grid[, `:=`(
  lon = round(lon, 2),
  lat = round(lat, 2),
  area_code = as.integer(area_code)
)]

cellxy <- unique(grid[, .(lon, lat)])
pts <- terra::vect(
  as.data.frame(cellxy),
  geom = c("lon", "lat"),
  crs = terra::crs(rain)
)
cellxy[,
  potential := pmax(
    terra::extract(rain, pts)[, 2],
    terra::extract(irr, pts)[, 2],
    na.rm = TRUE
  )
]

cells <- merge(grid, cellxy, by = c("lon", "lat"))
cells <- cells[is.finite(potential) & potential >= 1]

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
  source = "GAEZ v4 multiple cropping zones (mcr/mci, 1981-2010), mean over croppable cells",
  url = "https://storage.googleapis.com/gaez-v4-data/data/res01/CRUTS32/Hist/",
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
