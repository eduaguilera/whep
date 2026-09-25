# Fixtures for the intensive/extensive grassland split of
# build_n_boundary_exceedance(land_use = "all") (issue #1285). Inputs only:
# the expected values live in the tests, each derived there by hand.
#
# Seven cells on the 0.25 N row of the canonical 0.5-degree grid, one row per
# cell and scenario (IMAGE 2010 areas in ha, rates in kg N/ha, pressure in t N):
#
#   cell lon  country role
#   A    0.25 1       cropland + intensive grassland, stays intensive
#   B    0.75 1       cropland + extensive grassland: managed excess 10 t,
#                     extensive headroom 4 t (the no-netting cell)
#   C    1.25 1       extensive-only, promoted to intensive: no archive igl
#                     rate. B (0.5 degrees) has none either (IMAGE-extensive,
#                     cropland only); A and D (IMAGE-intensive) are both 1.0
#                     degree away, and the tie goes to the smaller cell_id,
#                     A (igl 80 kg/ha)
#   D    2.25 1       cropland + IMAGE-intensive grassland demoted to
#                     extensive: keeps its own ara rate (70) on its cropland;
#                     no archive extensive rate, borrows C's (1.0 degree; B
#                     is 1.5)
#   E    2.75 1       cropland, no IMAGE grassland, WHEP grassland 20 ha:
#                     extensive on WHEP's area, rate borrowed from C (1.5
#                     degrees; B is 2.0)
#   F    5.25 2       extensive-only with WHEP crop pressure: zero managed area
#   G    3.75 1       cropland only, absent from the class table
#
# C and F are NODATA in the `all`-scope critical surface, as extensive-only
# cells are in the deposited archive, so they are absent from `critical`.
#
# The `ara` and `igl` layers combine exactly into `all`, as they do in the
# archive: all = (ara * crop_ha + igl * igl_ha) / (crop_ha + igl_ha).
#   A is 50 on 100 ha with 80 on 50 ha, so all = 60 on 150 ha;
#   D is 70 on 50 ha with 25 on 100 ha, so all = 40 on 150 ha;
#   B, E, G are cropland only, so all == ara; igl is NODATA there.

.gs_lon <- function() {
  c(A = 0.25, B = 0.75, C = 1.25, D = 2.25, E = 2.75, F = 5.25, G = 3.75)
}

.gs_cell_id <- function(cell) {
  lon <- .gs_lon()[cell]
  whep:::.nbx_add_cell_key(
    tibble::tibble(lon = unname(lon), lat = 0.25),
    "fixture"
  )$cell_id
}

.gs_surplus <- function(year = 2015L) {
  tibble::tribble(
    ~cell, ~area_code, ~item_cbs_code, ~area_ha, ~surplus_n_t,
    "A",   1L,         2511L,          80,       8,
    "A",   2L,         2513L,          20,       1,
    "A",   1L,         3000L,          50,       3,
    "B",   1L,         2511L,          100,      15,
    "B",   1L,         3000L,          200,      2,
    "C",   1L,         3000L,          80,       6,
    "D",   1L,         2511L,          50,       3,
    "D",   1L,         3002L,          100,      5,
    "E",   1L,         2511L,          100,      4,
    "E",   1L,         3000L,          20,       1,
    "F",   2L,         2511L,          10,       2,
    "F",   2L,         3000L,          60,       1,
    "G",   1L,         2511L,          100,      9
  ) |>
    dplyr::mutate(
      lon = unname(.gs_lon()[.data$cell]),
      lat = 0.25,
      year = as.integer(year),
      n_input_std_t = .data$surplus_n_t
    ) |>
    dplyr::select(-"cell")
}

# The `all`-scope critical surface: `critical_kgn_ha` per ha of cropland plus
# IMAGE-intensive grassland, `source_area_ha` = a_crop + a_gr_int.
.gs_critical <- function(land_use = "all") {
  tibble::tribble(
    ~cell, ~critical_kgn_ha, ~source_area_ha,
    "A",   60,               150,
    "B",   50,               100,
    "D",   40,               150,
    "E",   50,               100,
    "G",   70,               100
  ) |>
    dplyr::mutate(
      lon = unname(.gs_lon()[.data$cell]),
      lat = 0.25,
      image_region = 11L,
      critical_threshold = "mi",
      critical_land_use = land_use,
      critical_year = 2010L
    ) |>
    dplyr::select(-"cell")
}

# The `ara` and `igl` critical surfaces of the same threshold and metric.
.gs_layer <- function(scope, metric = "surplus") {
  rates <- if (scope == "ara") {
    c(A = 50, B = 50, D = 70, E = 50, G = 70)
  } else {
    c(A = 80, D = 25)
  }
  tibble::tibble(
    lon = unname(.gs_lon()[names(rates)]),
    lat = 0.25,
    value = unname(rates),
    critical_var = if (metric == "input") {
      "critical_n_input"
    } else {
      "critical_n_surplus"
    },
    critical_threshold = "mi",
    critical_land_use = scope,
    critical_year = 2010L
  )
}

# `.critical_n_extensive_budget()` output: rates NA where the cell has no IMAGE
# extensive grassland.
.gs_budget <- function() {
  tibble::tribble(
    ~cell, ~ext_input_n_kg, ~ext_surplus_n_kg, ~ext_input_kgn_ha,
    ~ext_surplus_kgn_ha,
    "A",   0,               0,                 NA_real_, NA_real_,
    "B",   9000,            6000,              45,       30,
    "C",   1600,            960,               20,       12,
    "D",   0,               0,                 NA_real_, NA_real_,
    "E",   0,               0,                 NA_real_, NA_real_,
    "F",   900,             600,               15,       10,
    "G",   0,               0,                 NA_real_, NA_real_
  ) |>
    dplyr::mutate(cell_id = .gs_cell_id(.data$cell)) |>
    dplyr::select(
      "cell_id",
      "ext_input_n_kg",
      "ext_surplus_n_kg",
      "ext_input_kgn_ha",
      "ext_surplus_kgn_ha"
    )
}

# The class table, one row per cell and year. 2010 reproduces the IMAGE map;
# in 2015 C is promoted and D demoted. G has no grassland in either map and
# is absent.
.gs_classes <- function(year = 2015L) {
  static <- tibble::tribble(
    ~cell, ~country_2010, ~a_crop_ha, ~grass_ha_image, ~whep_grass_ha,
    ~image_class_2010,
    "A",   1L,            100,        50,              50,
    "intensive",
    "B",   1L,            100,        200,             200,
    "extensive",
    "C",   1L,            0,          80,              80,
    "extensive",
    "D",   1L,            50,         100,             100,
    "intensive",
    "E",   1L,            100,        0,               20,
    NA_character_,
    "F",   2L,            0,          60,              60,
    "extensive"
  )
  y2015 <- tibble::tribble(
    ~cell, ~grassland_class, ~method_grassland_split,
    "A",   "intensive",      "image2010_density_rank",
    "B",   "extensive",      "image2010_density_rank",
    "C",   "intensive",      "image2010_density_rank",
    "D",   "extensive",      "image2010_density_rank",
    "E",   "extensive",      "no_image_grassland",
    "F",   "extensive",      "image2010_fixed_no_density"
  ) |>
    dplyr::mutate(year = as.integer(year))
  y2010 <- static |>
    dplyr::transmute(
      .data$cell,
      grassland_class = dplyr::coalesce(.data$image_class_2010, "extensive"),
      method_grassland_split = dplyr::if_else(
        is.na(.data$image_class_2010),
        "no_image_grassland",
        "image2010_density_rank"
      ),
      year = 2010L
    )
  dplyr::bind_rows(y2010, y2015) |>
    dplyr::left_join(static, by = "cell") |>
    dplyr::mutate(
      lon = unname(.gs_lon()[.data$cell]),
      lat = 0.25,
      cell_id = .gs_cell_id(.data$cell),
      image_region = 11L,
      density_ratio = 1,
      target_share = NA_real_
    ) |>
    dplyr::select(
      "cell_id",
      "lon",
      "lat",
      "year",
      "country_2010",
      "image_region",
      "a_crop_ha",
      "grass_ha_image",
      "whep_grass_ha",
      "image_class_2010",
      "grassland_class",
      "density_ratio",
      "target_share",
      "method_grassland_split"
    )
}

.gs_grassland <- function(
  classes = .gs_classes(),
  budget = .gs_budget(),
  metric = "surplus"
) {
  list(
    classes = classes,
    extensive_budget = budget,
    critical_ara = .gs_layer("ara", metric),
    critical_igl = .gs_layer("igl", metric)
  )
}

.gs_run <- function(
  resolution = "cell",
  metric = "surplus",
  land_use = "all",
  ...
) {
  whep::build_n_boundary_exceedance(
    surplus = .gs_surplus(),
    critical = .gs_critical(land_use),
    land_use = land_use,
    resolution = resolution,
    metric = metric,
    actual_year = 2015L,
    critical_reference_year = 2010L,
    ...
  )
}
