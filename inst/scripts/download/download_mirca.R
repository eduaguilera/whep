# -----------------------------------------------------------------------
# download_mirca.R
#
# Downloads MIRCA2000 monthly crop area grids from Zenodo.
# The data was originally hosted at Goethe University Frankfurt but has
# been moved to Zenodo (record 7422506).
#
# Reference:
#   Portmann, F. T. et al. (2010) doi:10.1029/2008GB003435

download_mirca <- function(dest_dir) {
  mirca_dir <- file.path(dest_dir, "Irrigation maps_CIRCA-2000")
  if (!dir.exists(mirca_dir)) {
    dir.create(mirca_dir, recursive = TRUE)
  }

  # Check if files already exist
  expected_files <- sprintf(
    "crop_%02d_%s_12.flt.gz",
    rep(1:26, each = 2),
    rep(c("irrigated", "rainfed"), 26)
  )
  existing <- file.exists(file.path(mirca_dir, expected_files))
  if (all(existing)) {
    cli::cli_alert_info("MIRCA: all files already exist")
    return(invisible())
  }

  # Download from Zenodo
  zip_url <- "https://zenodo.org/records/7422506/files/monthly_growing_area_grids.zip?download=1"
  zip_path <- file.path(dest_dir, "mirca_monthly_growing_area_grids.zip")

  if (!file.exists(zip_path)) {
    cli::cli_alert("Downloading MIRCA2000 from Zenodo (~210 MB)...")
    download.file(zip_url, zip_path, mode = "wb", quiet = FALSE)
  }

  # Extract to temporary directory
  tmp_dir <- file.path(dest_dir, "mirca_tmp")
  if (dir.exists(tmp_dir)) {
    unlink(tmp_dir, recursive = TRUE)
  }
  dir.create(tmp_dir, recursive = TRUE)
  cli::cli_alert_info("Extracting MIRCA2000 files...")
  utils::unzip(zip_path, exdir = tmp_dir)

  # Find and move .flt.gz files to target directory
  flt_files <- list.files(
    tmp_dir,
    pattern = "crop_\\d{2}_(irrigated|rainfed)_12\\.flt\\.gz$",
    recursive = TRUE,
    full.names = TRUE
  )

  if (length(flt_files) == 0) {
    cli::cli_abort("No MIRCA .flt.gz files found in downloaded archive")
  }

  for (src in flt_files) {
    fname <- basename(src)
    dst <- file.path(mirca_dir, fname)
    if (!file.exists(dst)) {
      file.copy(src, dst)
    }
  }

  # Clean up
  unlink(tmp_dir, recursive = TRUE)
  file.remove(zip_path)

  cli::cli_alert_success("MIRCA: {length(flt_files)} files extracted")
  invisible()
}
