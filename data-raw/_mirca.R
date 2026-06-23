# Helper for mirca_season.R: returns the directory holding the MIRCA2000
# condensed cropping calendars (cropping_calendar_rainfed.txt /
# cropping_calendar_irrigated.txt). By default it downloads the official Zenodo
# archive (Portmann et al. 2010, MIRCA2000 v1.1, record 7422506) and
# decompresses the two calendar files, so the data-prep is reproducible without
# a local copy. Set WHEP_MIRCA_DIR to a local directory to override; set
# WHEP_DATARAW_CACHE to change the cache directory.

mirca_calendar_dir <- function() {
  override <- Sys.getenv("WHEP_MIRCA_DIR", "")
  if (nzchar(override)) {
    return(override)
  }

  cache_dir <- Sys.getenv(
    "WHEP_DATARAW_CACHE",
    file.path(tempdir(), "whep-dataraw")
  )
  out <- file.path(cache_dir, "condensed_cropping_calendars")
  kinds <- c("rainfed", "irrigated")
  txts <- file.path(out, paste0("cropping_calendar_", kinds, ".txt"))
  if (all(file.exists(txts))) {
    return(out)
  }

  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  url <- paste0(
    "https://zenodo.org/records/7422506/files/",
    "condensed_cropping_calendars.zip?download=1"
  )
  zip <- file.path(cache_dir, "condensed_cropping_calendars.zip")
  message("Downloading MIRCA2000 condensed cropping calendars from Zenodo")
  utils::download.file(url, zip, mode = "wb", quiet = TRUE)
  utils::unzip(zip, exdir = cache_dir)

  # Zenodo ships the calendars gzipped; the parser reads plain text.
  for (kind in kinds) {
    gz <- file.path(out, paste0("cropping_calendar_", kind, ".txt.gz"))
    txt <- file.path(out, paste0("cropping_calendar_", kind, ".txt"))
    if (!file.exists(txt) && file.exists(gz)) {
      writeLines(readLines(gzfile(gz)), txt)
    }
  }
  out
}
