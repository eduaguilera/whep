# -----------------------------------------------------------------------
# download_hydrology.R
#
# Downloads GLWD v2 and DRT 0.5-degree flow direction.
#
# References:
#   Lehner et al. (2025) GLWD v2, doi:10.6084/m9.figshare.28519994.v1
#   Wu et al. (2012) DRT, http://files.ntsg.umt.edu/data/DRT/

download_hydrology <- function(dest_dir) {
  .download_drt <- function(dest_dir) {
    drt_dir <- file.path(dest_dir, "DRT")
    drt_path <- file.path(drt_dir, "DRT_half_FDR_globe.asc")
    if (file.exists(drt_path)) {
      cli::cli_alert_info("DRT: already exists")
      return(invisible())
    }
    if (!dir.exists(drt_dir)) dir.create(drt_dir, recursive = TRUE)
    url <- paste0("http://files.ntsg.umt.edu/data/DRT/upscaled_global_hydrography/",
                  "by_HydroSHEDS_Hydro1k/flow_direction/DRT_half_FDR_globe.asc")
    cli::cli_alert("Downloading DRT flow direction (~1 MB)...")
    download.file(url, drt_path, mode = "wb", quiet = FALSE)
    cli::cli_alert_success("DRT: saved")
    invisible()
  }

  .download_glwd_v2 <- function(dest_dir) {
    glwd_dir <- file.path(dest_dir, "GLWD")
    extracted_dir <- file.path(glwd_dir, "GLWD_v2")
    if (dir.exists(extracted_dir) &&
        length(list.files(extracted_dir, pattern = "\\.tif$", recursive = TRUE)) > 0) {
      cli::cli_alert_info("GLWD v2: already extracted")
      return(invisible())
    }
    if (!dir.exists(glwd_dir)) dir.create(glwd_dir, recursive = TRUE)
    zip_path <- file.path(glwd_dir, "GLWD_v2_0_combined_classes_tif.zip")
    if (!file.exists(zip_path)) {
      cli::cli_alert("Downloading GLWD v2 (~925 MB)...")
      download.file("https://ndownloader.figshare.com/files/54001814", zip_path, mode = "wb")
    }
    if (!dir.exists(extracted_dir)) dir.create(extracted_dir, recursive = TRUE)
    cli::cli_alert("Extracting GLWD v2...")
    utils::unzip(zip_path, exdir = extracted_dir)
    n_tif <- length(list.files(extracted_dir, pattern = "\\.tif$", recursive = TRUE))
    cli::cli_alert_success("GLWD v2: {n_tif} GeoTIFFs")
    invisible()
  }

  .download_drt(dest_dir)
  .download_glwd_v2(dest_dir)
  invisible()
}
