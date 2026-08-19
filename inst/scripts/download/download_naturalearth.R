# -----------------------------------------------------------------------
# download_naturalearth.R
#
# Downloads the Natural Earth 10m layers WHEP reads:
#   * admin-0 countries  (cultural) -- country outlines
#   * glaciated areas    (physical) -- the ice source for
#                                      build_polycell_support()
#
# Source: https://www.naturalearthdata.com (public domain)

# One layer, fetched only when its shapefile is absent. `category` is Natural
# Earth's own split of the 10m collection into "cultural" and "physical", which
# is part of the URL and not a WHEP choice.
.download_ne_layer <- function(ne_dir, layer, category, sub_dir, size) {
  layer_dir <- file.path(ne_dir, sub_dir)
  shp_path <- file.path(layer_dir, paste0(layer, ".shp"))

  if (file.exists(shp_path)) {
    cli::cli_alert_info("NaturalEarth {layer}: already exists")
    return(invisible())
  }

  if (!dir.exists(layer_dir)) {
    dir.create(layer_dir, recursive = TRUE)
  }
  url <- paste0(
    "https://naciscdn.org/naturalearth/10m/",
    category,
    "/",
    layer,
    ".zip"
  )
  zip_path <- file.path(layer_dir, paste0(layer, ".zip"))

  cli::cli_alert("Downloading Natural Earth {layer} (~{size})...")
  download.file(url, zip_path, mode = "wb", quiet = FALSE)
  utils::unzip(zip_path, exdir = layer_dir)
  file.remove(zip_path)
  cli::cli_alert_success("NaturalEarth {layer}: saved")
  invisible()
}

download_naturalearth <- function(dest_dir) {
  ne_dir <- file.path(dest_dir, "NaturalEarth")

  # Kept in `Countries_shape/` because that is where it already lives on every
  # machine that has run this script before.
  .download_ne_layer(
    ne_dir,
    "ne_10m_admin_0_countries",
    "cultural",
    "Countries_shape",
    "5 MB"
  )

  # `read_glaciated_areas()` expects `<layer>/<layer>.shp` under
  # `WHEP_NATURALEARTH_DIR`, so the directory is named for the layer.
  .download_ne_layer(
    ne_dir,
    "ne_10m_glaciated_areas",
    "physical",
    "ne_10m_glaciated_areas",
    "1 MB"
  )

  invisible()
}
