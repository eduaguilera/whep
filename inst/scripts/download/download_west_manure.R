# -----------------------------------------------------------------------
# download_west_manure.R
#
# Fetches West et al. (2014) gridded manure N and P NetCDFs from the
# WHEP pins board and extracts them.
#
# Reference:
#   West, P. C. et al. (2014) doi:10.1126/science.1246067

download_west_manure <- function(dest_dir) {
  target_dir <- file.path(dest_dir, "Manure_Westetal2014")

  if (
    dir.exists(file.path(target_dir, "N")) &&
      length(list.files(
        file.path(target_dir, "N"),
        pattern = "\\.nc(\\.gz)?$"
      )) >=
        170
  ) {
    n_n <- length(list.files(
      file.path(target_dir, "N"),
      pattern = "\\.nc(\\.gz)?$"
    ))
    n_p <- length(list.files(
      file.path(target_dir, "P"),
      pattern = "\\.nc(\\.gz)?$"
    ))
    cli::cli_alert_info("West manure: already exists ({n_n} N, {n_p} P)")
    return(invisible())
  }

  cli::cli_alert("Fetching West manure from pins board...")
  extracted <- whep::whep_read_file("manure-west-et-al-2014", type = "tar.gz")

  find_manure_root <- function(paths) {
    for (p in paths) {
      parent <- dirname(p)
      if (
        dir.exists(file.path(parent, "N")) && dir.exists(file.path(parent, "P"))
      ) {
        return(parent)
      }
      grandparent <- dirname(parent)
      if (
        dir.exists(file.path(grandparent, "N")) &&
          dir.exists(file.path(grandparent, "P"))
      ) {
        return(grandparent)
      }
    }
    n_dirs <- unique(dirname(grep("/N/|/N$", paths, value = TRUE)))
    if (length(n_dirs) > 0) {
      return(dirname(n_dirs[1]))
    }
    stop("Could not find N/ and P/ directories in extracted archive")
  }

  manure_root <- find_manure_root(extracted)
  if (!dir.exists(target_dir)) {
    dir.create(target_dir, recursive = TRUE)
  }

  for (nutrient in c("N", "P")) {
    src_nut <- file.path(manure_root, nutrient)
    dst_nut <- file.path(target_dir, nutrient)
    if (dir.exists(src_nut) && !dir.exists(dst_nut)) {
      file.copy(src_nut, target_dir, recursive = TRUE)
    }
  }

  n_n <- length(list.files(
    file.path(target_dir, "N"),
    pattern = "\\.nc(\\.gz)?$"
  ))
  n_p <- length(list.files(
    file.path(target_dir, "P"),
    pattern = "\\.nc(\\.gz)?$"
  ))
  cli::cli_alert_success("West manure: {n_n} N, {n_p} P NetCDFs")
  invisible()
}
