# -----------------------------------------------------------------------
# download_image_gnm.R
#
# Downloads the supplement of Beusen et al. (2015), the IMAGE-GNM model
# code and inputs, from its DOI into <dest_dir>/IMAGE_GNM/, verifies the
# archive against the SHA-256 WHEP recorded on 2026-10-12 (the publisher
# prints none) and unpacks the two 0.5-degree layers read_image_gnm_runoff()
# reads: water_input/pcrglobwb_100/fQsro.map (the surface runoff fraction)
# and fix_input/slope.map (the median slope, m/km). Then point
# WHEP_IMAGE_GNM_DIR at <dest_dir>/IMAGE_GNM. Without it,
# read_image_gnm_runoff() downloads the same archive into its own cache.
#
# The download, the checksum and the unpacking are the package's own
# (.gnm_cached_dir()), so the script and the reader cannot disagree on which
# archive is the right one.
#
# Reference:
#   Beusen et al. (2015) Geosci. Model Dev. 8:4045-4067,
#   doi:10.5194/gmd-8-4045-2015; supplement
#   doi:10.5194/gmd-8-4045-2015-supplement (CC BY 3.0, 59.7 MB).

download_image_gnm <- function(dest_dir) {
  gnm_dir <- file.path(dest_dir, "IMAGE_GNM")
  whep:::.gnm_cached_dir(gnm_dir)
  # Read once with verification, so a corrupted unpack fails here and not in
  # the first balance that uses it.
  layers <- whep::read_image_gnm_runoff(dir = gnm_dir)
  cli::cli_alert_success(
    "IMAGE-GNM runoff layers: {nrow(layers)} land cells in {.path {gnm_dir}}"
  )
  cli::cli_alert_info("Set {.envvar WHEP_IMAGE_GNM_DIR} to {.path {gnm_dir}}")
  invisible(gnm_dir)
}
