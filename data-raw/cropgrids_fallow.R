# Build inst/extdata/cropgrids_fallow_land.csv: per-(area_code, item_cbs_code)
# crop PHYSICAL area WITH rotational fallow attributed to crops (the AEZ-grid
# fallow-inclusive companion of cropgrids_land.csv).
#
# Method: cropped physical area = CROPGRIDS national (cropgrids_land.csv). The
# fallow MAGNITUDE per country = FAOSTAT "Land with temporary fallow" (Land Use
# item 6640, reported separately from temporary meadows/pastures, so it excludes
# fodder). It is distributed to crops by gridded_fallow_weights(): each grid
# cell's rainfed crop area x crop-by-agro-climatic-zone propensity (GAEZ LGP +
# thermal), summed to country x item -> rainfed dryland cereals and rainfed
# monsoon rice get fallow; irrigated/continuous and perennial crops do not.
# build_cropgrids_land_extension(source = "cropgrids_fallow") then applies the
# resulting physical/harvested ratio to WHEP harvested area in every year.
#
# Inputs: shipped cropgrids_land.csv, grid_aez.csv, fallow_propensity.csv; the
# spatialization inputs (<l_files>/whep/inputs via WHEP_LFILES_INPUT_DIR); and
# the FAOSTAT Land Use bulk CSV (Inputs_LandUse_E_All_Data_NOFLAG.csv via
# WHEP_FAOSTAT_LANDUSE).

library(dplyr)
library(data.table)
library(readr)
devtools::load_all(".")

input_dir <- Sys.getenv(
  "WHEP_LFILES_INPUT_DIR",
  file.path(getwd(), "LPJmL_inputs", "whep", "inputs")
)
fao_path <- Sys.getenv(
  "WHEP_FAOSTAT_LANDUSE",
  "/home/usuario/LandInG/landuse/Faostat/landuse_all_data/Inputs_LandUse_E_All_Data_NOFLAG.csv"
)
ref_year <- as.integer(Sys.getenv("WHEP_CROPGRIDS_FALLOW_YEAR", "2020"))

cropgrids <- read_csv(
  system.file("extdata", "cropgrids_land.csv", package = "whep"),
  show_col_types = FALSE
)

# Spatialize harvested area for the reference year (crop-level, no CFT agg).
inputs <- whep:::.load_landuse_inputs(
  input_dir,
  list(use_type_constraint = FALSE)
)
avail <- sort(unique(inputs$country_areas$year))
ref_year <- max(avail[avail <= ref_year])
message("Reference year: ", ref_year)
gridded <- build_gridded_landuse(
  inputs$country_areas,
  inputs$crop_patterns,
  inputs$gridded_cropland,
  inputs$country_grid,
  config = list(
    years = ref_year,
    multicropping = inputs$multicropping,
    n_workers = 1L
  )
)

# map item_prod -> item_cbs per cell (rainfed area is the fallow-weighting base)
prod2cbs <- whep::items_prod_full |>
  transmute(
    item_prod_code = as.integer(item_prod_code),
    item_cbs_code = as.integer(item_cbs_code)
  ) |>
  filter(!is.na(item_prod_code), !is.na(item_cbs_code)) |>
  distinct()
gridded_cbs <- as.data.table(gridded)[
  as.data.table(prod2cbs),
  on = "item_prod_code",
  nomatch = 0
][,
  .(rainfed_ha = sum(rainfed_ha, na.rm = TRUE)),
  by = .(lon, lat, area_code, item_cbs_code)
]

weights <- gridded_fallow_weights(tibble::as_tibble(gridded_cbs))

# FAOSTAT "Temporary fallow" (item 6640), reference year, ha
ycol <- paste0("Y", ref_year)
fallow_total <- fread(fao_path, showProgress = FALSE)[
  `Item Code` == 6640 & Element == "Area",
  .(area_code = `Area Code`, fallow_ha = get(ycol) * 1000)
][!is.na(fallow_ha) & fallow_ha > 0]

cropgrids_fallow_land <- attribute_fallow_to_crops(
  cropgrids,
  tibble::as_tibble(fallow_total),
  weights
) |>
  transmute(
    area_code = as.integer(area_code),
    item_cbs_code = as.integer(item_cbs_code),
    physical_ha = round(physical_ha, 1),
    harvested_ha = round(harvested_ha, 1)
  ) |>
  filter(harvested_ha > 0) |>
  arrange(area_code, item_cbs_code)

write_csv(cropgrids_fallow_land, "inst/extdata/cropgrids_fallow_land.csv")
