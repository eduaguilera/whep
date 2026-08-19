# -----------------------------------------------------------------------
# download_hyde.R
#
# Downloads the HYDE 3.2.1 baseline archive, which carries the gridded
# urban population count read_hyde_population() reads for the urban/human
# excreta nitrogen stream (build_urban_n()).
#
# HYDE is distributed as one 5 GB baseline archive containing per-year ZIPs
# ("2010AD_pop.zip"), which is the granularity read_hyde_population() wants:
# it opens one year's archive at a time rather than holding the series. So
# this extracts the outer archive and leaves the per-year ZIPs alone.
#
# Point WHEP_HYDE_DIR at the directory holding the per-year ZIPs, reported
# at the end of the run.
#
# Not to be confused with download_population.R, which fetches the ISIMIP3a
# gridded TOTAL population for LPJmL's popdens forcing. HYDE supplies the
# urban/rural split that ISIMIP's total does not carry, so the two are
# different quantities for different consumers, not duplicates.
#
# Source: DANS Data Station Archaeology, doi:10.17026/DANS-25G-GEZ3
#   (Dataverse; file ids resolved from the dataset's own metadata).
# Licence: CC BY 3.0.
#
# References:
#   Klein Goldewijk, K., Beusen, A., Doelman, J. & Stehfest, E. (2017).
#   Anthropogenic land use estimates for the Holocene - HYDE 3.2.
#   Earth System Science Data 9, 927-953. doi:10.5194/essd-9-927-2017
# -----------------------------------------------------------------------

HYDE_DOI <- "doi:10.17026/DANS-25G-GEZ3"
HYDE_BASE <- "https://archaeology.datastations.nl/api/access/datafile"

# Dataverse file ids for HYDE 3.2.1, from the dataset metadata endpoint
# (api/datasets/export?exporter=dataverse_json). Ids are stable per file
# version; .hyde_check_ids() re-resolves them so a silent republication
# surfaces as an error rather than a wrong download.
HYDE_FILES <- list(
  baseline = list(id = 5490328L, name = "HYDE3_2_1-baseline.zip"),
  readme = list(id = 5396388L, name = "readme_release_HYDE3.2.1.txt")
)

download_hyde <- function(dest_dir, years = NULL, timeout = 7200) {
  hyde_dir <- file.path(dest_dir, "HYDE")
  dir.create(hyde_dir, recursive = TRUE, showWarnings = FALSE)

  .hyde_check_ids()
  for (entry in HYDE_FILES) {
    .hyde_download_file(entry, hyde_dir, timeout)
  }
  zip_dir <- .hyde_extract_baseline(hyde_dir, years)

  cli::cli_alert_success("HYDE 3.2.1 ready.")
  cli::cli_inform(c(i = "Set {.envvar WHEP_HYDE_DIR} to {.file {zip_dir}}."))
  invisible(zip_dir)
}

# -- Private helpers ----------------------------------------------------

# Confirm the recorded file ids still name the files we expect. A Dataverse
# id points at one file version, so if the archive is republished the id
# either 404s or serves something else; either way we want to know before
# spending an hour on a 5 GB download.
.hyde_check_ids <- function() {
  url <- paste0(
    "https://archaeology.datastations.nl/api/datasets/export",
    "?exporter=dataverse_json&persistentId=",
    HYDE_DOI
  )
  meta <- try(jsonlite::fromJSON(url, simplifyVector = FALSE), silent = TRUE)
  if (inherits(meta, "try-error")) {
    cli::cli_warn(
      "Could not reach DANS to verify file ids; downloading anyway."
    )
    return(invisible(NULL))
  }
  published <- .hyde_published_names(meta)
  for (entry in HYDE_FILES) {
    name <- published[[as.character(entry$id)]]
    if (is.null(name)) {
      cli::cli_abort(c(
        "HYDE file id {entry$id} is no longer in {.val {HYDE_DOI}}.",
        i = "The archive was probably republished; re-resolve the ids from
             the dataset metadata and update {.code HYDE_FILES}."
      ))
    }
    if (!identical(name, entry$name)) {
      cli::cli_abort(
        "HYDE file id {entry$id} now names {.file {name}}, not
         {.file {entry$name}}."
      )
    }
  }
  invisible(NULL)
}

.hyde_published_names <- function(meta) {
  files <- meta$datasetVersion$files
  ids <- vapply(files, \(f) as.character(f$dataFile$id), character(1))
  names <- vapply(files, \(f) f$dataFile$filename, character(1))
  stats::setNames(as.list(names), ids)
}

.hyde_download_file <- function(entry, hyde_dir, timeout) {
  out_path <- file.path(hyde_dir, entry$name)
  if (file.exists(out_path)) {
    cli::cli_alert_info("HYDE {entry$name}: already exists")
    return(invisible(out_path))
  }
  cli::cli_alert("Downloading {entry$name}...")
  old <- options(timeout = timeout)
  on.exit(options(old), add = TRUE)
  utils::download.file(
    paste0(HYDE_BASE, "/", entry$id),
    out_path,
    mode = "wb",
    quiet = TRUE
  )
  cli::cli_alert_success("HYDE {entry$name}: saved")
  invisible(out_path)
}

# Turn the downloaded archive into the per-year ZIPs read_hyde_population()
# opens, and return the directory holding them (the value for WHEP_HYDE_DIR).
#
# Two things make this more than an unzip. The archive is 4.97 GB, i.e. zip64,
# which R's internal unzip cannot even list -- so an external unzip is required.
# And DANS packages the data differently from the PBL portal the reader was
# written against: it ships baseline/asc/<year>AD_pop/urbc_<year>AD.asc loose,
# not "<year>AD_pop.zip". Of the 78 GB and 2738 entries inside, the reader wants
# one ~66 MB file per year, so extract only those and repackage them under the
# names it opens.
.hyde_extract_baseline <- function(hyde_dir, years = NULL) {
  archive <- file.path(hyde_dir, HYDE_FILES$baseline$name)
  zip_dir <- file.path(hyde_dir, "pop_zip")
  dir.create(zip_dir, recursive = TRUE, showWarnings = FALSE)
  unzip_bin <- .hyde_unzip_bin()

  members <- .hyde_urbc_members(archive, unzip_bin, years)
  if (length(members) == 0L) {
    cli::cli_abort(c(
      "No {.file urbc_<year>AD.asc} entries in {.file {archive}}.",
      i = "The HYDE packaging has changed; {.fn read_hyde_population} reads
           the urban population count from one per-year archive."
    ))
  }
  cli::cli_alert(
    "Repackaging {length(members)} year{?s} of urban population..."
  )
  for (member in members) {
    .hyde_repackage_year(archive, member, zip_dir, unzip_bin)
  }
  zip_dir
}

# R's internal unzip is limited to the classic (non-zip64) format, so it fails
# on this 4.97 GB archive. Require a real unzip rather than silently producing
# an empty directory.
.hyde_unzip_bin <- function() {
  bin <- Sys.which("unzip")
  if (!nzchar(bin)) {
    cli::cli_abort(c(
      "No {.command unzip} on PATH.",
      i = "The HYDE archive is zip64 (4.97 GB); R's internal unzip cannot
           read it. Install {.command unzip} (or extract by hand and point
           {.envvar WHEP_HYDE_DIR} at the per-year ZIPs)."
    ))
  }
  unname(bin)
}

.hyde_urbc_members <- function(archive, unzip_bin, years) {
  listing <- system2(unzip_bin, c("-Z1", shQuote(archive)), stdout = TRUE)
  members <- grep("urbc_[0-9]+AD[.]asc$", listing, value = TRUE)
  if (is.null(years)) {
    return(members)
  }
  wanted <- paste0("urbc_", years, "AD.asc")
  members[basename(members) %in% wanted]
}

.hyde_repackage_year <- function(archive, member, zip_dir, unzip_bin) {
  asc <- basename(member)
  year <- sub("^urbc_([0-9]+)AD[.]asc$", "\\1", asc)
  out_zip <- file.path(zip_dir, paste0(year, "AD_pop.zip"))
  if (file.exists(out_zip)) {
    return(invisible(out_zip))
  }
  staging <- file.path(tempdir(), paste0("hyde_", year))
  dir.create(staging, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(staging, recursive = TRUE), add = TRUE)
  system2(
    unzip_bin,
    c(
      "-o",
      "-j",
      "-q",
      shQuote(archive),
      shQuote(member),
      "-d",
      shQuote(staging)
    )
  )
  utils::zip(
    zipfile = out_zip,
    files = file.path(staging, asc),
    flags = "-q -j"
  )
  invisible(out_zip)
}
