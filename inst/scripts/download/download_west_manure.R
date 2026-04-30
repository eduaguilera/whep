# -----------------------------------------------------------------------
# download_west_manure.R
#
# Fetches West et al. (2014) gridded manure N and P application rates
# from the WHEP pins board and extracts them to L_files.
#
# Reference:
#   West, P. C. et al. (2014) "Leverage points for improving global
#   food security and the environment." Science 345, 325-328.
#   doi:10.1126/science.1246067
#
# Source: WHEP pin "manure-west-et-al-2014" (tar.gz)
#   172 crops × 2 nutrients (N, P) at 5 arc-min, NetCDF format
#
# Output: $WHEP_L_FILES_DIR/Manure_Westetal2014/{N,P}/
#
# Usage:
#   Rscript inst/scripts/download/download_west_manure.R
# -----------------------------------------------------------------------

l_files_dir <- Sys.getenv("WHEP_L_FILES_DIR")
if (!nzchar(l_files_dir)) {
  stop("WHEP_L_FILES_DIR environment variable is not set.")
}

target_dir <- file.path(l_files_dir, "Manure_Westetal2014")

cli::cli_h1("Downloading West et al. 2014 manure data")

if (dir.exists(file.path(target_dir, "N")) &&
    length(list.files(file.path(target_dir, "N"), pattern = "\\.nc$")) >= 170) {
  n_n <- length(list.files(file.path(target_dir, "N"), pattern = "\\.nc$"))
  n_p <- length(list.files(file.path(target_dir, "P"), pattern = "\\.nc$"))
  cli::cli_alert_success(
    "Already exists: {n_n} N files, {n_p} P files in {target_dir}"
  )
  quit(status = 0)
}

cli::cli_alert("Fetching manure-west-et-al-2014 from WHEP pins board...")
extracted <- whep::whep_read_file("manure-west-et-al-2014", type = "tar.gz")

# The archive should contain Manure_Westetal2014/N/ and Manure_Westetal2014/P/
# Move from temp extraction to target location
src_dir <- extracted[1]  # First file path
# Find the root of the extracted archive
# Walk up to find the directory containing N/ and P/
find_manure_root <- function(paths) {
  for (p in paths) {
    parent <- dirname(p)
    if (dir.exists(file.path(parent, "N")) &&
        dir.exists(file.path(parent, "P"))) {
      return(parent)
    }
    grandparent <- dirname(parent)
    if (dir.exists(file.path(grandparent, "N")) &&
        dir.exists(file.path(grandparent, "P"))) {
      return(grandparent)
    }
  }
  # Fallback: find dirs named N and P among extracted paths
  n_dirs <- unique(dirname(grep("/N/|/N$", paths, value = TRUE)))
  if (length(n_dirs) > 0) return(dirname(n_dirs[1]))
  stop("Could not find N/ and P/ directories in extracted archive")
}

manure_root <- find_manure_root(extracted)
cli::cli_alert_info("Extracted to: {manure_root}")

if (!dir.exists(target_dir)) {
  dir.create(target_dir, recursive = TRUE)
}

# Copy N and P directories
for (nutrient in c("N", "P")) {
  src_nut <- file.path(manure_root, nutrient)
  dst_nut <- file.path(target_dir, nutrient)
  if (dir.exists(src_nut) && !dir.exists(dst_nut)) {
    cli::cli_alert("Copying {nutrient}/ directory...")
    file.copy(src_nut, target_dir, recursive = TRUE)
  }
}

n_n <- length(list.files(file.path(target_dir, "N"), pattern = "\\.nc$"))
n_p <- length(list.files(file.path(target_dir, "P"), pattern = "\\.nc$"))
cli::cli_alert_success("Done! {n_n} N files, {n_p} P files in {target_dir}")
cli::cli_alert_success("Ready for prepare_spatialize_all.R")
