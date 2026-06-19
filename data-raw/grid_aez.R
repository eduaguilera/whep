# Build inst/extdata/grid_aez.csv: per 0.5-degree cell agro-climatic indicators
# used by gridded_fallow_weights()/.aez_zone() to classify each grid cell.
#
# Source: GAEZ v4 (FAO/IIASA) theme layers, 5 arc-min, aggregated to 0.5 degrees
# (mean length of growing period; modal thermal-climate class):
#   - reference_length_growing_period (LGP, days): moisture/aridity proxy.
#   - thermal_climates (class 1-12; 1-5 = tropics/subtropics).
# Set the GAEZ directory via WHEP_GAEZ_DIR. Requires the `terra` package.

library(terra)
library(data.table)
library(readr)
devtools::load_all(".")

gaez_dir <- Sys.getenv(
  "WHEP_GAEZ_DIR",
  "/home/usuario/LandInG/landuse/GAEZ"
)

lgp <- rast(file.path(gaez_dir, "reference_length_growing_period/data.asc"))
tc <- rast(file.path(gaez_dir, "thermal_climates/data.asc"))
lgp5 <- aggregate(lgp, fact = 6, fun = "mean", na.rm = TRUE)
tc5 <- aggregate(tc, fact = 6, fun = "modal", na.rm = TRUE)

# spatializer grid cells (0.5 degrees)
input_dir <- Sys.getenv(
  "WHEP_LFILES_INPUT_DIR",
  file.path(getwd(), "LPJmL_inputs", "whep", "inputs")
)
cells <- as.data.table(
  nanoparquet::read_parquet(file.path(input_dir, "country_grid.parquet"))
)
cells <- unique(cells[, .(lon, lat)])
pts <- vect(as.data.frame(cells), geom = c("lon", "lat"), crs = crs(lgp))

grid_aez <- cells[, .(
  lon,
  lat,
  lgp = round(terra::extract(lgp5, pts)[, 2], 1),
  thermal = as.integer(terra::extract(tc5, pts)[, 2])
)][!is.na(lgp) | !is.na(thermal)]

write_csv(grid_aez, "inst/extdata/grid_aez.csv")
