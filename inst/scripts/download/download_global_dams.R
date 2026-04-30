# -----------------------------------------------------------------------
# download_global_dams.R
#
# Fetches GRanD v1.1 (Global Reservoir and Dam database) shapefiles
# from the WHEP pins board and extracts them to L_files.
#
# Reference:
#   Lehner, B. et al. (2011) "High-resolution mapping of the world's
#   reservoirs and dams for sustainable river-flow management."
#   Frontiers in Ecology and the Environment, 9, 494-502.
#   doi:10.1890/100125
#
# Source: WHEP pin "global-dams" (tar.gz)
#   GRanD_dams_v1_1.shp + associated files (.dbf, .prj, .shx, etc.)
#   GRanD_reservoirs_v1_1.shp + associated files
#
# Output: $WHEP_L_FILES_DIR/GIS/Global dams/
#
# Usage:
#   Rscript inst/scripts/download/download_global_dams.R
# -----------------------------------------------------------------------

l_files_dir <- Sys.getenv("WHEP_L_FILES_DIR")
if (!nzchar(l_files_dir)) {
  stop("WHEP_L_FILES_DIR environment variable is not set.")
}

target_dir <- file.path(l_files_dir, "GIS", "Global dams")

cli::cli_h1("Downloading GRanD v1.1 dam database")

if (
  dir.exists(target_dir) &&
    length(list.files(target_dir, pattern = "GRanD_dams.*\\.shp$")) > 0
) {
  cli::cli_alert_success("Already exists: {target_dir}")
  quit(status = 0)
}

if (!dir.exists(target_dir)) {
  dir.create(target_dir, recursive = TRUE)
}

cli::cli_alert("Fetching global-dams from WHEP pins board...")
extracted <- whep::whep_read_file("global-dams", type = "tar.gz")

# Find the extracted directory containing .shp files
shp_files <- grep("\\.shp$", extracted, value = TRUE)
if (length(shp_files) == 0) {
  stop("No .shp files found in extracted archive")
}

src_root <- dirname(shp_files[1])
# Copy all shapefile components (multiple extensions per .shp)
shp_bases <- unique(tools::file_path_sans_ext(shp_files))
for (base in shp_bases) {
  components <- grep(
    paste0("^", basename(base), "\\."),
    extracted,
    value = TRUE
  )
  for (f in components) {
    dst <- file.path(target_dir, basename(f))
    if (!file.exists(dst)) {
      file.copy(f, dst)
    }
  }
}

n_shp <- length(list.files(target_dir, pattern = "\\.shp$"))
n_total <- length(list.files(target_dir))
cli::cli_alert_success(
  "Done! {n_shp} shapefiles ({n_total} files) in {target_dir}"
)
cli::cli_alert_success("Ready for prepare_spatialize_all.R")
