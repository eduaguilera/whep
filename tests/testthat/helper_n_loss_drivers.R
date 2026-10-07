# Offline inputs for build_n_loss_drivers() and the balance that builds it
# (test_n_loss_drivers.R, test_n_balance.R). Every layer is injected through
# `data`; nothing reads CRU, LPJmL or a pin.
#
# Three cells on one parallel, 47.75 N, all temperate (12 degC, 40 frost
# days, PET 1000 mm):
# - A (9.25): 600 mm, dry on precipitation alone.
# - B (9.75): 1200 mm, wet.
# - C (10.25): no CRU climate at all (a coastal cell off CRU's land mask).
#   Its nearest CRU land cell is B, 0.5 degrees of longitude away; A is 1.0.

.nld_climate <- function(years = 1981:2010) {
  tidyr::expand_grid(
    tibble::tibble(
      lon = c(9.25, 9.75),
      lat = 47.75,
      annual_precip_mm = c(600, 1200)
    ),
    year = as.integer(years)
  ) |>
    dplyr::mutate(
      mean_annual_temp_c = 12,
      annual_pet_mm = 1000,
      annual_frost_days = 40
    )
}

# LPJmL at the crop grain: an irrigated maize stand of 120 mm in A, rainfed
# maize in B and C.
.nld_crop <- function(years = 1981:2010) {
  stands <- tibble::tribble(
    ~lon,  ~lat,  ~lpjml_crop, ~irrig_mm_irrigated, ~stand_frac_irrigated,
    9.25,  47.75, "maize",     120,                 0.4,
    9.75,  47.75, "maize",     NA,                  NA,
    10.25, 47.75, "maize",     NA,                  NA
  ) |>
    dplyr::mutate(irrig_mm_rainfed = 0, stand_frac_rainfed = 0.3)
  tidyr::expand_grid(stands, year = as.integer(years)) |>
    dplyr::mutate(
      lpjml_version = "LPJmL C Version 6.1.1",
      lpjml_git_hash = "e6e6c42b88354f4a0df945e55f2ccf3c4a3cb605",
      lpjml_run = "fixture/scenario_1"
    )
}

# WHEP's regime areas: maize (56 -> 2514) irrigated and rainfed in A, rainfed
# in C. Nothing in B: its only rows are grass and items without regime land.
.nld_areas <- function(years = 1981:2010) {
  tibble::tribble(
    ~lon,  ~lat,  ~area_code, ~item_prod_code, ~rainfed_ha, ~irrigated_ha,
    9.25,  47.75, 79L,        56L,             30,          50,
    10.25, 47.75, 79L,        56L,             40,          0
  ) |>
    tidyr::expand_grid(year = as.integer(years))
}

.nld_inputs <- function(years = 1981:2010) {
  list(
    climate = .nld_climate(years),
    crop_depth = .nld_crop(years),
    regime_areas = .nld_areas(years)
  )
}

# Balance rows of every kind the builder meets: crop rows with a class of
# their own, rows without a crop (NA item, the SOM sequestration rows), grass
# and rice rows with no regime land, and a crop row in the cell CRU misses.
.nld_rows <- function() {
  tibble::tribble(
    ~lon,  ~lat,  ~area_code, ~item_cbs_code, ~year, ~water_regime,
    9.25,  47.75, 79L,        2514L,          2010L, "irrigated",
    9.25,  47.75, 79L,        2514L,          2010L, "rainfed",
    9.25,  47.75, 79L,        NA,             2010L, "rainfed",
    9.25,  47.75, 79L,        NA,             2010L, "irrigated",
    9.25,  47.75, 79L,        2807L,          2010L, "irrigated",
    9.25,  47.75, 79L,        2807L,          2010L, "rainfed",
    9.75,  47.75, 79L,        3000L,          2010L, "rainfed",
    9.75,  47.75, 79L,        2807L,          2010L, "irrigated",
    9.75,  47.75, 79L,        2807L,          2010L, "rainfed",
    10.25, 47.75, 79L,        2514L,          2010L, "rainfed"
  )
}

# Great-circle distance on the IUGG mean Earth radius, written out here
# independently of the package's own helper.
.nld_km <- function(lon1, lat1, lon2, lat2) {
  rad <- pi / 180
  h <- sin((lat2 - lat1) * rad / 2)^2 +
    cos(lat1 * rad) * cos(lat2 * rad) * sin((lon2 - lon1) * rad / 2)^2
  2 * 6371.0088 * asin(sqrt(h))
}
