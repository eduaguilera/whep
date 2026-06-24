# Build inst/extdata/grid_aez.csv: per 0.5-degree cell agro-climatic indicators
# used by gridded_fallow_weights()/.aez_zone() to classify each grid cell.
#
# Source: GAEZ v4 (FAO/IIASA, open access CC BY 4.0) theme layers, 5 arc-min,
# aggregated to 0.5 degrees (mean length of growing period; modal thermal-climate
# class), downloaded by data-raw/_gaez.R:
#   - length of growing period (LGP, days, 1981-2010 climatology): res01
#     lgd_CRUTS32_Hist_8110 (lgd = LGP in days; the `lgp` code is the 1-16 LGP
#     class, not days) -- moisture/aridity proxy.
#   - thermal-climate class (1-12; 1-5 = tropics/subtropics): LR/aez
#     thz_class_CRUTS32_Hist_8110.
# Requires the `terra` package. Set WHEP_GAEZ_DIR to override the download with
# locally-held layers (by bucket basename).

library(terra)
library(data.table)
library(readr)
devtools::load_all(".")

source("data-raw/_gaez.R")
lgp <- rast(gaez_layer("data/res01/CRUTS32/Hist/lgd_CRUTS32_Hist_8110.tif"))
tc <- rast(gaez_layer("data/LR/aez/thz_class_CRUTS32_Hist_8110_100_avg.tif"))
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
