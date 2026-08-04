# -----------------------------------------------------------------------
# download_luh2.R
#
# Downloads the LUH2-GCB2022 land-use grids, the vintage read_luh2_landuse()
# treats as the reference.
#
# states.nc and management.nc come from Zenodo record 15556812 (CC-BY-4.0),
# where they are published with MD5 checksums, and are verified against them.
# luh.umd.edu serves byte-identical copies at LUH2/LUH2_GCB_2022/, but its TLS
# chain does not verify (see the -k workaround below), so it is not the source
# of record.
#
# staticData_quarterdeg.nc is NOT part of the GCB record -- it is the
# vintage-independent static grid (cell areas, land mask), published only under
# luh.umd.edu/LUH2/LUH2_v2h/, so that one file is still fetched from there.
#
# References:
#   Hurtt, G. C. et al. (2020) doi:10.5194/gmd-13-5425-2020   (LUH2 v2h)
#   Chini, L. et al. (2021)   doi:10.5194/essd-13-4175-2021   (GCB variants)
#   Data:                     doi:10.5281/zenodo.15556812     (LUH2-GCB2022)

# Zenodo assets, with the byte size and MD5 published on the record. The local
# name is what the WHEP readers and prepare_spatialize_all.R expect to find.
.luh2_zenodo_assets <- function() {
  tibble::tribble(
    ~asset, ~local_name, ~bytes, ~md5,
    "states4.nc",
    "states.nc",
    6657587367,
    "411ef3d657c3108942954c895f658a17",
    "management4.nc",
    "management.nc",
    2172626928,
    "4b8a2090876f410c6573e47f441566dc"
  )
}

download_luh2 <- function(dest_dir) {
  target_dir <- file.path(dest_dir, "LUH2", "LUH2-GCB2022")
  if (!dir.exists(target_dir)) {
    dir.create(target_dir, recursive = TRUE)
  }
  # 6.7 GB over the default 60 s timeout never finishes; download_all() raises
  # this too, but this script is also run on its own.
  old_timeout <- getOption("timeout")
  on.exit(options(timeout = old_timeout), add = TRUE)
  options(timeout = max(old_timeout, 14400))

  assets <- .luh2_zenodo_assets()
  purrr::pwalk(assets, \(asset, local_name, bytes, md5) {
    .luh2_fetch_zenodo(target_dir, asset, local_name, bytes, md5)
  })
  .luh2_fetch_static_grid(target_dir)

  cli::cli_alert_success("LUH2-GCB2022 ready in {.path {target_dir}}")
  cli::cli_alert_info(
    "Point {.envvar WHEP_LUH2_DIR} at that directory. Note the path changed
     from {.path LUH2/LUH2 v2h}: that held the base v2h release (850-2015),
     a different product from GCB2022 (850-2022)."
  )
  invisible(target_dir)
}

# Fetch one Zenodo asset and verify it against the published MD5. An existing
# file of the right size is left alone rather than re-hashed, so re-running is
# cheap; a wrong-sized file is re-downloaded.
.luh2_fetch_zenodo <- function(target_dir, asset, local_name, bytes, md5) {
  fpath <- file.path(target_dir, local_name)
  if (file.exists(fpath) && file.size(fpath) == bytes) {
    cli::cli_alert_info(
      "LUH2 {local_name}: already present ({round(bytes / 1e9, 1)} GB)"
    )
    return(invisible(fpath))
  }
  url <- paste0(
    "https://zenodo.org/api/records/15556812/files/",
    asset,
    "/content"
  )
  cli::cli_alert(
    "Downloading LUH2-GCB2022 {asset} ({round(bytes / 1e9, 1)} GB)..."
  )
  utils::download.file(url, fpath, mode = "wb")
  .luh2_check_md5(fpath, md5)
  invisible(fpath)
}

.luh2_check_md5 <- function(fpath, md5) {
  cli::cli_alert("Verifying {basename(fpath)} against its published MD5...")
  if (!identical(unname(tools::md5sum(fpath)), md5)) {
    unlink(fpath)
    cli::cli_abort(c(
      "{.file {basename(fpath)}} does not match the MD5 published on Zenodo.",
      x = "The partial or corrupt file was removed.",
      i = "Re-run to download it again."
    ))
  }
  cli::cli_alert_success("{basename(fpath)}: MD5 verified")
}

# The static grid is only published under luh.umd.edu/LUH2/LUH2_v2h/, whose TLS
# chain does not verify, so the -k retry stays for this one small file. It is
# vintage-independent, so taking it from the v2h path is correct for GCB2022.
.luh2_fetch_static_grid <- function(target_dir) {
  fname <- "staticData_quarterdeg.nc"
  fpath <- file.path(target_dir, fname)
  if (file.exists(fpath)) {
    cli::cli_alert_info(
      "LUH2 {fname}: already present ({round(file.size(fpath) / 1024)} KB)"
    )
    return(invisible(fpath))
  }
  url <- paste0("https://luh.umd.edu/LUH2/LUH2_v2h/", fname)
  cli::cli_alert("Downloading LUH2 {fname} from luh.umd.edu...")
  dl_result <- tryCatch(
    utils::download.file(
      url,
      fpath,
      mode = "wb",
      method = "libcurl",
      extra = "-k"
    ),
    error = function(e) 1L
  )
  if (dl_result != 0) {
    utils::download.file(url, fpath, mode = "wb")
  }
  cli::cli_alert_success("LUH2 {fname}: saved")
  invisible(fpath)
}
