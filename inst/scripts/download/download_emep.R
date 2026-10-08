# -----------------------------------------------------------------------
# download_emep.R
#
# Downloads the yearly EMEP MSC-W model results (EMEP01 rv5.6, 2025
# reporting round, ~78 MB per year) from the Norwegian Meteorological
# Institute THREDDS server into <dest_dir>/EMEP/, the form
# read_emep_deposition() reads. Point WHEP_EMEP_DIR at that directory.
#
# The default years are 1990-2019, the overlap with HaNi that
# correct_n_deposition() works on (EMEP rv5.6 starts in 1990; HaNi ends in
# 2019). Files from 2023 onwards belong to a different naming round.
#
# Reference:
#   Simpson et al. (2012) doi:10.5194/acp-12-7825-2012
#   Data: https://www.emep.int/mscw/mscw_moddata.html

download_emep <- function(dest_dir, years = 1990:2019) {
  emep_dir <- file.path(dest_dir, "EMEP")
  if (!dir.exists(emep_dir)) {
    dir.create(emep_dir, recursive = TRUE)
  }
  base_url <- paste0(
    "https://thredds.met.no/thredds/fileServer/data/EMEP/2025_Reporting"
  )
  for (year in years) {
    fname <- sprintf("EMEP01_rv5.6_year.%dmet_%demis_rep2025.nc", year, year)
    out_path <- file.path(emep_dir, fname)
    if (file.exists(out_path)) {
      cli::cli_alert_info("EMEP {year}: already exists")
      next
    }
    cli::cli_alert("Downloading EMEP {year} (~78 MB)...")
    # Download beside the target and rename, so an interrupted transfer never
    # leaves a truncated file under the name the reader looks for.
    part_path <- paste0(out_path, ".part")
    utils::download.file(
      paste0(base_url, "/", fname),
      part_path,
      mode = "wb"
    )
    file.rename(part_path, out_path)
    cli::cli_alert_success("EMEP {year}: saved")
  }
  invisible(emep_dir)
}
