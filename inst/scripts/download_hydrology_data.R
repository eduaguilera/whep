# -----------------------------------------------------------------------
# download_hydrology_data.R
#
# Downloads freely available hydrology datasets to L_files for use in
# prepare_hydrology_inputs.R:
#
#   1. GLWD v2 (Lehner et al., 2025) - Global Lakes and Wetlands Database
#      Source: Figshare (CC-BY 4.0)
#      DOI: 10.6084/m9.figshare.28519994.v1
#      File: Combined classes GeoTIFF (~925 MB)
#
#   2. DRT 0.5-degree flow direction (Wu et al., 2012)
#      Source: NTSG/UMT (freely available)
#      URL: http://files.ntsg.umt.edu/data/DRT/upscaled_global_hydrography/
#      File: DRT_half_FDR_globe.asc (~961 KB)
#
# These replace the older GLWD v1 (restricted) and DDM30 (request-only)
# datasets. The prepare_hydrology_inputs.R script supports both old and
# new formats.
# -----------------------------------------------------------------------

l_files_dir <- "WHEP_L_FILES_DIR_PLACEHOLDER"

# ==== 1. DRT 0.5-degree flow direction ================================

.download_drt <- function(l_files_dir) {
  cli::cli_h2("DRT 0.5-degree flow direction")


  drt_dir <- file.path(l_files_dir, "DRT")
  drt_path <- file.path(drt_dir, "DRT_half_FDR_globe.asc")

  if (file.exists(drt_path)) {
    cli::cli_alert_success("Already exists: {drt_path}")
    return(invisible(drt_path))
  }

  if (!dir.exists(drt_dir)) {
    dir.create(drt_dir, recursive = TRUE)
  }

  url <- paste0(
    "http://files.ntsg.umt.edu/data/DRT/",
    "upscaled_global_hydrography/by_HydroSHEDS_Hydro1k/",
    "flow_direction/DRT_half_FDR_globe.asc"
  )
  cli::cli_alert("Downloading DRT half-degree flow direction (~1 MB)...")
  download.file(url, drt_path, mode = "wb", quiet = FALSE)

  if (!file.exists(drt_path)) {
    cli::cli_abort("Download failed for DRT flow direction")
  }

  sz <- round(file.size(drt_path) / 1024)
  cli::cli_alert_success("Saved: {drt_path} ({sz} KB)")
  invisible(drt_path)
}

# ==== 2. GLWD v2 ======================================================

.download_glwd_v2 <- function(l_files_dir) {
  cli::cli_h2("GLWD v2 (Lehner et al., 2025)")

  glwd_dir <- file.path(l_files_dir, "GLWD")
  zip_path <- file.path(glwd_dir, "GLWD_v2_0_combined_classes_tif.zip")
  extracted_dir <- file.path(glwd_dir, "GLWD_v2")

  # Check if already extracted
  if (dir.exists(extracted_dir) &&
    length(list.files(extracted_dir, pattern = "\\.tif$")) > 0
  ) {
    n_tif <- length(list.files(extracted_dir, pattern = "\\.tif$",
                               recursive = TRUE))
    cli::cli_alert_success(
      "Already extracted: {extracted_dir} ({n_tif} GeoTIFF files)"
    )
    return(invisible(extracted_dir))
  }

  if (!dir.exists(glwd_dir)) {
    dir.create(glwd_dir, recursive = TRUE)
  }

  # Download combined classes TIF (925 MB)
  # Figshare file ID 54001814
  url <- "https://ndownloader.figshare.com/files/54001814"

  if (!file.exists(zip_path)) {
    cli::cli_alert(
      "Downloading GLWD v2 combined classes GeoTIFF (~925 MB)..."
    )
    cli::cli_alert_info(
      "This may take 10-30 minutes depending on connection speed."
    )
    download.file(url, zip_path, mode = "wb", quiet = FALSE)

    if (!file.exists(zip_path)) {
      cli::cli_abort("Download failed for GLWD v2")
    }
    sz <- round(file.size(zip_path) / 1024 / 1024)
    cli::cli_alert_success("Downloaded: {zip_path} ({sz} MB)")
  } else {
    sz <- round(file.size(zip_path) / 1024 / 1024)
    cli::cli_alert_info("ZIP already exists ({sz} MB), extracting...")
  }

  # Extract
  cli::cli_alert("Extracting to {extracted_dir}...")
  if (!dir.exists(extracted_dir)) {
    dir.create(extracted_dir, recursive = TRUE)
  }
  utils::unzip(zip_path, exdir = extracted_dir)
  n_tif <- length(list.files(extracted_dir, pattern = "\\.tif$",
                             recursive = TRUE))
  cli::cli_alert_success("Extracted {n_tif} GeoTIFF files")

  invisible(extracted_dir)
}

# ==== Main execution ==================================================

cli::cli_h1("Downloading hydrology data to L_files")

.download_drt(l_files_dir)
.download_glwd_v2(l_files_dir)

cli::cli_h1("Summary")
cli::cli_alert_info(
  "DRT: {file.path(l_files_dir, 'DRT', 'DRT_half_FDR_globe.asc')}"
)
cli::cli_alert_info(
  "GLWD v2: {file.path(l_files_dir, 'GLWD', 'GLWD_v2')}"
)
cli::cli_alert_success(
  "Now re-run inst/scripts/prepare_hydrology_inputs.R to generate ",
  "lakes_rivers.parquet and drainage.parquet"
)
