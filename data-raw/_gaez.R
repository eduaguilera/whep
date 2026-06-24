# Helper for grid_aez.R: download a GAEZ v4 (FAO/IIASA) GeoTIFF layer and return
# its local path. GAEZ v4 is open access (CC BY 4.0); the full-resolution
# 5-arc-min layers are served as individual public objects from the FAO Google
# Cloud Storage bucket, even though bucket listing is restricted. Layers are
# cached under WHEP_DATARAW_CACHE. Set WHEP_GAEZ_DIR to a directory of
# pre-downloaded layers (by bucket basename) to override the download.

gaez_layer <- function(key) {
  override <- Sys.getenv("WHEP_GAEZ_DIR", "")
  if (nzchar(override)) {
    local <- file.path(override, basename(key))
    if (file.exists(local)) {
      return(local)
    }
  }

  cache_dir <- Sys.getenv(
    "WHEP_DATARAW_CACHE",
    rappdirs::user_cache_dir("whep")
  )
  out <- file.path(cache_dir, "gaez")
  dir.create(out, showWarnings = FALSE, recursive = TRUE)

  dest <- file.path(out, basename(key))
  if (!file.exists(dest)) {
    url <- paste0("https://storage.googleapis.com/gaez-v4-data/", key)
    message("Downloading GAEZ v4 layer ", basename(key))
    utils::download.file(url, dest, mode = "wb", quiet = TRUE)
  }
  dest
}
