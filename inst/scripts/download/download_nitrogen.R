# -----------------------------------------------------------------------
# download_nitrogen.R
#
# Downloads HaNi atmospheric N deposition zips from PANGAEA and
# delegates Coello 2025 download to download_coello().
#
# References:
#   Tian et al. (2022) doi:10.5194/essd-14-4551-2022
#   Coello et al. (2025) doi:10.1038/s41597-024-04215-x

download_nitrogen <- function(dest_dir) {
  .download_hani <- function(dest_dir) {
    hani_dir <- file.path(dest_dir, "HaNi")
    if (!dir.exists(hani_dir)) {
      dir.create(hani_dir, recursive = TRUE)
    }
    base_url <- "https://download.pangaea.de/dataset/942069/files"
    files <- c(ndep_nhx = "ndep_nhx.zip", ndep_noy = "ndep_noy.zip")

    for (nm in names(files)) {
      fname <- files[[nm]]
      out_path <- file.path(hani_dir, fname)
      if (file.exists(out_path)) {
        cli::cli_alert_info("HaNi {fname}: already exists")
        next
      }
      cli::cli_alert("Downloading {fname}...")
      download.file(
        paste0(base_url, "/", fname),
        out_path,
        mode = "wb"
      )
      cli::cli_alert_success("HaNi {fname}: saved")
    }
    .extract_hani(hani_dir, files)
    invisible()
  }

  # PANGAEA ships the two deposition grids zipped, but read_n_deposition()
  # reads NetCDF, so downloading is only half the job: without this the
  # directory this script fills cannot be used as WHEP_HANI_DIR. Each archive
  # holds one ~12 GB .nc, so extract only when that .nc is absent.
  .extract_hani <- function(hani_dir, files) {
    for (fname in files) {
      nc_name <- sub("[.]zip$", ".nc", fname)
      if (file.exists(file.path(hani_dir, nc_name))) {
        cli::cli_alert_info("HaNi {nc_name}: already extracted")
        next
      }
      cli::cli_alert("Extracting {nc_name} (~12 GB)...")
      utils::unzip(file.path(hani_dir, fname), exdir = hani_dir)
      cli::cli_alert_success("HaNi {nc_name}: extracted")
    }
    invisible()
  }

  .download_hani(dest_dir)
  download_coello(dest_dir)
  invisible()
}
