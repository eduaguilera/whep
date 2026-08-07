# Natural Earth administrative boundaries.
#
# Source: https://www.naturalearthdata.com (public domain, no attribution
# required, but please credit Natural Earth when reusing the maps).
#
# The shapefiles are downloaded on demand from the official Natural Earth
# CDN rather than redistributed with this package, so users always get the
# upstream data straight from the source.

.natural_earth_url <- function(layer) {
  paste0("https://naciscdn.org/naturalearth/10m/cultural/", layer, ".zip")
}

.natural_earth_cache_dir <- function() {
  file.path(rappdirs::user_cache_dir("whep"), "naturalearth")
}

# Resolve the path to the Natural Earth 10m admin-1 (states/provinces)
# shapefile, downloading it on first use.
#
# Resolution order:
#   1. `path`, if given (an explicit user-supplied shapefile);
#   2. `getOption("whep.provinces_shapefile")`, for a session-wide override;
#   3. the local cache, populated by downloading from Natural Earth.
.provinces_shapefile <- function(path = NULL) {
  path <- path %||% getOption("whep.provinces_shapefile")

  if (!is.null(path)) {
    if (!file.exists(path)) {
      cli::cli_abort(c(
        "Provinces shapefile not found at {.path {path}}.",
        i = "Pass a valid {.arg shapefile_path}, or leave it {.code NULL} to
             download the layer from Natural Earth."
      ))
    }
    return(path)
  }

  .download_natural_earth("ne_10m_admin_1_states_provinces")
}

# Download and unzip a Natural Earth layer into the cache, returning the
# path to its .shp. Re-uses the cached copy when already present.
.download_natural_earth <- function(layer) {
  dir <- .natural_earth_cache_dir()
  shp <- file.path(dir, paste0(layer, ".shp"))

  if (file.exists(shp)) {
    return(shp)
  }

  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }

  zip <- file.path(dir, paste0(layer, ".zip"))
  # Resolved into a local rather than interpolated inline: cli >= 3.4.0 reads
  # a `{}` expression starting with a dot as a style name, so
  # `{.url {.natural_earth_url(layer)}}` aborted with "Invalid cli literal"
  # instead of showing this function's own recovery instructions.
  url <- .natural_earth_url(layer)
  cli::cli_alert_info("Downloading {layer} from Natural Earth...")

  ok <- tryCatch(
    {
      utils::download.file(
        url,
        zip,
        mode = "wb",
        quiet = TRUE
      )
      utils::unzip(zip, exdir = dir)
      TRUE
    },
    error = function(e) e
  )

  if (file.exists(zip)) {
    file.remove(zip)
  }

  if (inherits(ok, "error") || !file.exists(shp)) {
    cli::cli_abort(c(
      "Could not download the Natural Earth layer {.val {layer}}.",
      x = if (inherits(ok, "error")) conditionMessage(ok),
      i = "Download {.url {url}} manually and pass the
           unzipped {.file .shp} via {.arg shapefile_path}, or set
           {.code options(whep.provinces_shapefile = \"<path>\")}."
    ))
  }

  cli::cli_alert_success("Cached {layer} in {.path {dir}}")
  shp
}
