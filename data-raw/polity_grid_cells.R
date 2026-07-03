# Periodized cell -> polity membership for zonal aggregation.
#
# Assigns each 0.5-degree grid cell of the gridded land-use series to every
# polity whose vintage polygon contains it, tagged with the polity's active
# period. Membership is deliberately NON-exclusive: a cell inside both a
# federation and one of its members (e.g. French West Africa and Mali), or
# inside a former entity and its successor, belongs to each of them. Zonal
# sums are then computed per polity independently, so overlaps are correct by
# construction -- each polity gets the land inside its own borders, and we
# never try to partition the globe into one polity per cell.
#
# Shipped as package data because it is derived purely from the polity
# polygons; the build then needs only a tabular join, no live `sf`/geometry
# dependency. Regenerate when the polity polygons or the grid change.

devtools::load_all(".", quiet = TRUE)
library(sf)
library(data.table)

sf::sf_use_s2(FALSE)

# Grid cells come from the gridded pasture series so membership aligns exactly
# with the cells we later aggregate.
grid <- whep::whep_read_file("spatialize-gridded-pasture")
cells <- unique(data.table::as.data.table(grid)[, .(lon, lat)])

pts <- sf::st_as_sf(
  cells,
  coords = c("lon", "lat"),
  crs = 4326,
  remove = FALSE
)

geoms <- whep::get_polity_geometries()
geoms <- geoms[!sf::st_is_empty(geoms), ]

joined <- sf::st_join(
  pts,
  geoms[, c("polity_code", "start_year", "end_year")],
  join = sf::st_intersects,
  left = FALSE
)

polity_grid_cells <- joined |>
  sf::st_drop_geometry() |>
  data.table::as.data.table() |>
  _[!is.na(polity_code), .(lon, lat, polity_code, start_year, end_year)] |>
  unique() |>
  _[order(polity_code, lon, lat)] |>
  tibble::as_tibble()

usethis::use_data(polity_grid_cells, overwrite = TRUE, compress = "xz")
