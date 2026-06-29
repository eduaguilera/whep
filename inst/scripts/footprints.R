suppressPackageStartupMessages({
  devtools::load_all(".")
})

# Years to process
years <- c(1986, 1987)
grassland_metric <- tolower(Sys.getenv("WHEP_GRASSLAND_METRIC", "occupation"))
usable_grass_yield_dm_t_ha <- as.numeric(
  Sys.getenv("WHEP_USABLE_GRASS_YIELD_DM_T_HA", "2.06")
)

if (!grassland_metric %in% c("occupation", "active_grazing")) {
  stop(
    "`WHEP_GRASSLAND_METRIC` must be \"occupation\" or \"active_grazing\".",
    call. = FALSE
  )
}
if (is.na(usable_grass_yield_dm_t_ha) || usable_grass_yield_dm_t_ha <= 0) {
  stop(
    "`WHEP_USABLE_GRASS_YIELD_DM_T_HA` must be a positive number.",
    call. = FALSE
  )
}

# Land extension source for crops:
#   "spatial_physical" per-cell physical cropland from the gridded land-use
#                      pipeline (apportions LUH2 cropland by harvested share)
#   "cropgrids"        per-crop physical cropland from CROPGRIDS (harvested area
#                      x per-crop physical/harvested ratio; corrects
#                      double-cropping per crop, e.g. rice)
#   "cropgrids_fallow" (default) as "cropgrids" plus rotational fallow attributed
#                      to crops (FAOSTAT temporary fallow split by rainfed x
#                      propensity)
#   "hayr"             land occupation in hectare-years: active growing
#                      occupation (harvested area x MIRCA cycle length, counting
#                      every harvest) plus attributed rotational fallow. Measures
#                      land-TIME tied up, distinct from the physical-area methods;
#                      perennials/long-cycle crops occupy more land-time per
#                      hectare, short single-cropped crops less.
# Grassland always comes from the native grassland extension
# (build_grassland_land_extension); no external pin is needed.
crop_land_source <- tolower(
  Sys.getenv("WHEP_CROP_LAND_SOURCE", "cropgrids_fallow")
)
valid_sources <- c(
  "spatial_physical",
  "cropgrids",
  "cropgrids_fallow",
  "hayr"
)
if (!crop_land_source %in% valid_sources) {
  stop(
    paste0(
      "`WHEP_CROP_LAND_SOURCE` must be one of: ",
      paste(valid_sources, collapse = ", "),
      "."
    ),
    call. = FALSE
  )
}

# Environmental pressure traced through the supply chain:
#   "land" (default) physical/occupation land use (ha or ha-yr), assembled from
#                    the crop and grassland land extensions below.
#   "ghg"            livestock greenhouse-gas emissions (kg CO2e) from the IPCC
#                    enteric + manure pipeline (build_livestock_ghg_extension).
# GHG tier (WHEP_GHG_TIER, 1 or 2) and GWP100 standard (WHEP_GHG_GWP, ar6/ar5/
# ar4) follow the multi-method convention; see build_livestock_ghg_extension().
pressure <- tolower(Sys.getenv("WHEP_FOOTPRINT_PRESSURE", "land"))
if (!pressure %in% c("land", "ghg")) {
  stop("`WHEP_FOOTPRINT_PRESSURE` must be \"land\" or \"ghg\".", call. = FALSE)
}
ghg_tier <- as.integer(Sys.getenv("WHEP_GHG_TIER", "1"))
ghg_gwp <- tolower(Sys.getenv("WHEP_GHG_GWP", "ar6"))

# Build IO model for selected years.
io <- build_io_model(years = years)

extension_use <- if (pressure == "ghg") {
  build_livestock_ghg_extension(tier = ghg_tier, gwp = ghg_gwp) |>
    dplyr::filter(year %in% years) |>
    dplyr::select(year, area_code, item_cbs_code, impact_u)
} else {
  crop_land <- if (crop_land_source %in% c("cropgrids", "cropgrids_fallow")) {
    build_cropgrids_land_extension(source = crop_land_source) |>
      dplyr::filter(year %in% years)
  } else if (crop_land_source == "hayr") {
    build_hayr_land_extension() |>
      dplyr::filter(year %in% years)
  } else {
    input_dir <- Sys.getenv(
      "WHEP_LFILES_INPUT_DIR",
      file.path(getwd(), "LPJmL_inputs", "whep", "inputs")
    )
    get_crop_land_extension(input_dir = input_dir, years = years)
  }

  grass_land <- build_grassland_land_extension(
    grassland_metric = grassland_metric,
    usable_grass_yield_dm_t_ha = usable_grass_yield_dm_t_ha
  ) |>
    dplyr::filter(year %in% years) |>
    dplyr::select(year, area_code, item_cbs_code, impact_u)

  dplyr::bind_rows(crop_land, grass_land)
}

# Trace the chosen extension through the supply chain for all selected years.
footprints <- build_footprint(extension_use, io = io)

footprints |>
  add_area_name(
    name_column = "origin_area_name",
    code_column = "origin_area"
  ) |>
  add_area_name(
    name_column = "target_area_name",
    code_column = "target_area"
  ) |>
  add_item_cbs_name(
    name_column = "origin_item_name",
    code_column = "origin_item"
  ) |>
  add_item_cbs_name(
    name_column = "target_item_name",
    code_column = "target_item"
  )
