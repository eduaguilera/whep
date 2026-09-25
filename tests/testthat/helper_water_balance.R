# Synthetic LPJmL water inputs shared by test_water_balance.R and
# test_crop_water_consumption.R.

# Build synthetic monthly LPJmL-style inputs for `n_cells` cells x 12 months x
# one year. Soil-water saturation drops linearly Jan -> Dec so dStorage != 0 and
# the storage term participates. prec is solved so the 4-term budget closes.
.wb_synthetic_monthly <- function() {
  cells <- tibble::tribble(
    ~lon, ~lat, ~area_code,
    9.25, 47.75, 11L,
    -55.25, -12.25, 21L
  )
  porosity <- 0.4
  thickness_mm <- c(200, 300, 500, 1000, 1000, 10000)
  swc_jan <- c(0.50, 0.45, 0.40, 0.35, 0.30, 0.25)
  swc_dec <- c(0.40, 0.38, 0.34, 0.31, 0.28, 0.24)
  d_storage_mm <- sum((swc_dec - swc_jan) * thickness_mm * porosity)

  months <- 1:12
  flux <- tidyr::expand_grid(cells, month = months) |>
    dplyr::mutate(
      year = 2000L,
      transp = 40 + lon * 0,
      evap = 15,
      interc = 5,
      irrig = 8,
      runoff = 12,
      seepage = 10
    )
  aet_annual <- (40 + 15 + 5) * 12
  irrig_annual <- 8 * 12
  runoff_annual <- 12 * 12
  seepage_annual <- 10 * 12
  water_input_annual <- aet_annual +
    runoff_annual +
    seepage_annual +
    d_storage_mm
  prec_monthly <- (water_input_annual - irrig_annual) / 12
  flux <- dplyr::mutate(flux, prec = prec_monthly)

  swc <- tidyr::expand_grid(
    cells,
    month = months,
    layer = seq_along(swc_jan)
  ) |>
    dplyr::mutate(
      year = 2000L,
      value = swc_jan[layer] +
        (swc_dec[layer] - swc_jan[layer]) * (month - 1) / 11
    )

  to_long <- function(var) {
    dplyr::select(flux, lon, lat, year, month, value = dplyr::all_of(var))
  }
  cell_polity <- dplyr::mutate(cells, polity_frac = 1, cell_area_ha = 30000)
  list(
    inputs = list(
      transp = to_long("transp"),
      evap = to_long("evap"),
      interc = to_long("interc"),
      prec = to_long("prec"),
      irrig = to_long("irrig"),
      runoff = to_long("runoff"),
      seepage = to_long("seepage"),
      swc = swc,
      cell_polity = cell_polity,
      # Per-CFT cubes are per-STAND densities, so build_water_balance()
      # weights every band by its stand fraction before summing it to a cell.
      # These synthetic bands each get a fraction of 1, meaning "this band
      # covers its cell", which leaves the arithmetic in the tests below
      # exactly as it was when the sum was unweighted. The weighting itself is
      # exercised with realistic fractions in its own tests at the end of this
      # file -- mixing the two would make every expectation here depend on a
      # fraction as well as on the value under test.
      stand_frac = tidyr::expand_grid(
        cells,
        # Every band name any test in this file uses. A name missing here
        # weights to zero and silently removes that band from the totals,
        # which is what the join is meant to do for a band with no area --
        # so the list has to stay in step with the fixtures below.
        tibble::tribble(
          ~band, ~band_name,
          1L, "rainfed maize",
          2L, "rainfed grassland",
          3L, "irrigated maize",
          4L, "rainfed rice",
          14L, "rainfed grassland",
          30L, "irrigated grassland"
        ) |>
          dplyr::distinct(band_name, .keep_all = TRUE)
      ) |>
        dplyr::mutate(year = 2000L, value = 1)
    ),
    water_input_annual = water_input_annual
  )
}
