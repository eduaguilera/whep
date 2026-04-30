# -----------------------------------------------------------------------
# download_global_dams.R
#
# Fetches GRanD v1.1 shapefiles from the WHEP pins board.
#
# Reference:
#   Lehner, B. et al. (2011) doi:10.1890/100125

download_global_dams <- function(dest_dir) {
  target_dir <- file.path(dest_dir, "GIS", "Global dams")

  if (dir.exists(target_dir) &&
      length(list.files(target_dir, pattern = "GRanD_dams.*\\.shp$")) > 0) {
    cli::cli_alert_info("GRanD: already exists")
    return(invisible())
  }

  if (!dir.exists(target_dir)) dir.create(target_dir, recursive = TRUE)

  cli::cli_alert("Fetching GRanD v1.1 from pins board...")
  extracted <- whep::whep_read_file("global-dams", type = "tar.gz")

  shp_files <- grep("\\.shp$", extracted, value = TRUE)
  if (length(shp_files) == 0) stop("No .shp files found in GRanD archive")

  shp_bases <- unique(tools::file_path_sans_ext(shp_files))
  for (base in shp_bases) {
    components <- grep(paste0("^", basename(base), "\\."), extracted, value = TRUE)
    for (f in components) {
      dst <- file.path(target_dir, basename(f))
      if (!file.exists(dst)) file.copy(f, dst)
    }
  }

  n_shp <- length(list.files(target_dir, pattern = "\\.shp$"))
  cli::cli_alert_success("GRanD: {n_shp} shapefiles")
  invisible()
}
