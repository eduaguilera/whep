# -----------------------------------------------------------------------
# download_monfreda.R
#
# Downloads Monfreda et al. (2008) harvested area and yield GeoTIFFs
# for 175 crops from Google Cloud Storage.
#
# Reference:
#   Monfreda, C. et al. (2008) doi:10.1029/2007GB002947

download_monfreda <- function(dest_dir) {
  gcs_url <- "https://storage.googleapis.com/earthstat/HarvestedAreaYield175Crops_Geotiff.zip"
  target_dir <- file.path(
    dest_dir,
    "HarvestedAreaYield175Crops_Geotiff",
    "GeoTiff"
  )

  if (dir.exists(target_dir) && length(.monfreda_missing(target_dir)) == 0L) {
    n_crops <- length(list.dirs(target_dir, recursive = FALSE))
    cli::cli_alert_info("Monfreda: already extracted ({n_crops} crops)")
    return(invisible())
  }

  zip_path <- file.path(dest_dir, "HarvestedAreaYield175Crops_Geotiff.zip")
  if (!file.exists(zip_path)) {
    cli::cli_alert("Downloading Monfreda yields (~902 MB)...")
    download.file(gcs_url, zip_path, mode = "wb")
  }

  cli::cli_alert("Extracting...")
  utils::unzip(zip_path, exdir = dest_dir)
  file.remove(zip_path)

  # Remove macOS metadata folder
  macosx_dir <- file.path(dest_dir, "__MACOSX")
  if (dir.exists(macosx_dir)) {
    unlink(macosx_dir, recursive = TRUE)
  }

  n_crops <- length(list.dirs(target_dir, recursive = FALSE))
  n_tifs <- length(list.files(
    target_dir,
    pattern = "\\.tif$",
    recursive = TRUE
  ))
  .monfreda_check_complete(target_dir)
  cli::cli_alert_success("Monfreda: {n_crops} crops, {n_tifs} GeoTIFFs")
  invisible()
}

# The crop layers `prepare_crop_patterns()` will look for, which is the
# authority for what "complete" means: `earthstat_mapping.csv` carries one row
# per layer of the 175-crop archive, mapped or not.
#
# Counting directories is not enough and was not enough. The old guard passed
# any extraction with 170 of the 175, and a local copy sat at 169 for long
# enough that `earthstat_mapping.csv` was built from it -- which is how barley,
# a major cereal, came to have no crosswalk row at all. A count cannot say
# WHICH layer is absent, and that is the only thing worth knowing here.
.monfreda_expected_crops <- function() {
  path <- system.file("extdata", "earthstat_mapping.csv", package = "whep")
  if (!nzchar(path)) {
    path <- file.path("inst", "extdata", "earthstat_mapping.csv")
  }
  if (!file.exists(path)) {
    return(character())
  }
  utils::read.csv(path, stringsAsFactors = FALSE)$earthstat_name
}

.monfreda_missing <- function(target_dir) {
  expected <- .monfreda_expected_crops()
  if (length(expected) == 0L) {
    return(character())
  }
  setdiff(expected, basename(list.dirs(target_dir, recursive = FALSE)))
}

# Warn rather than abort: a partial archive is still usable, and the crops
# that did arrive should not be thrown away over the ones that did not. But
# it has to be said out loud, because the consumer is silent about it --
# `prepare_crop_patterns()` only reaches a layer that has a crosswalk row,
# and `.read_one_earthstat_crop()` returns an empty tibble for a missing tif.
.monfreda_check_complete <- function(target_dir) {
  missing <- .monfreda_missing(target_dir)
  if (length(missing) == 0L) {
    return(invisible())
  }
  cli::cli_alert_warning(
    "Monfreda: {length(missing)} crop layer{?s} absent from
     {.path {target_dir}}: {.val {missing}}. Every one of them will be
     silently missing from crop_patterns."
  )
  invisible()
}
