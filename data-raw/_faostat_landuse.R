# Shared helper for the FAOSTAT-Land-Use data-raw scripts.
#
# Returns the path to Inputs_LandUse_E_All_Data_NOFLAG.csv. By default it
# downloads the official FAOSTAT bulk (domain RL, "Land Use") from
# bulks-faostat.fao.org and caches it, so the data-prep is reproducible without
# any local copy. Set WHEP_FAOSTAT_LANDUSE to a local NOFLAG csv to override the
# download; set WHEP_DATARAW_CACHE to change the cache directory.

faostat_landuse_noflag <- function() {
  override <- Sys.getenv("WHEP_FAOSTAT_LANDUSE", "")
  if (nzchar(override)) {
    return(override)
  }

  cache_dir <- Sys.getenv(
    "WHEP_DATARAW_CACHE",
    file.path(tempdir(), "whep-dataraw")
  )
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

  csv <- file.path(cache_dir, "Inputs_LandUse_E_All_Data_NOFLAG.csv")
  if (!file.exists(csv)) {
    url <- paste0(
      "https://bulks-faostat.fao.org/production/",
      "Inputs_LandUse_E_All_Data.zip"
    )
    zip <- file.path(cache_dir, "Inputs_LandUse_E_All_Data.zip")
    message("Downloading FAOSTAT Land Use bulk from ", url)
    utils::download.file(url, zip, mode = "wb", quiet = TRUE)
    utils::unzip(
      zip,
      files = "Inputs_LandUse_E_All_Data_NOFLAG.csv",
      exdir = cache_dir
    )
  }
  csv
}
