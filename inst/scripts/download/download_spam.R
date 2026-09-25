# -----------------------------------------------------------------------
# download_spam.R
#
# Downloads SPAM2010 v2.0 harvested-area, production and yield Global_CSV
# zips from Harvard Dataverse, verifies each against its published MD5, and
# extracts the irrigated ("TI") and all-rainfed ("TR") technology members
# read_spam_yields() consumes -- the same six files
# whep:::.spam_locate_member() would produce in the on-demand cache, just
# written under `dest_dir` instead of rappdirs::user_cache_dir("whep").
#
# SPAM2020 v2.0 is deliberately NOT downloaded here. Harvard Dataverse gates
# every file of that dataset (doi:10.7910/DVN/SWPENT) behind a mandatory
# guestbook ("IFPRI Guestbook 2021", id 380: email and institution
# required), and the API's documented way past it needs a logged-in
# Dataverse account submitting that response -- WHEP does not automate a
# login or a licence click-through. To use
# SPAM2020 with read_spam_yields(vintage = "2020"):
#   1. Open https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/SWPENT
#      and fill the guestbook once (name/email/institution).
#   2. Download the three Global_CSV zips: spam2020V2r2_global_harvested_area
#      .csv.zip, spam2020V2r2_global_production.csv.zip,
#      spam2020V2r2_global_yield.csv.zip.
#   3. Put them in one directory and point WHEP_SPAM_DIR (or
#      read_spam_yields(dir = ...)) at its PARENT -- the reader appends
#      "2020" itself, e.g. WHEP_SPAM_DIR=<dir> for files under <dir>/2020/.
#   read_spam_yields() verifies those zips against the MD5s Dataverse
#   publishes on the record (see whep:::.spam_manifest()) and parses them
#   through the exact same code path as SPAM2010; it aborts clearly, naming
#   what it found, if the expected columns are not there -- SPAM2020's
#   layout has not been verified against SPAM2010's, since the guestbook
#   blocks even that dataset's own 6 KB ReadMe.
#
# Reference:
#   Yu, Q. et al. (2020) doi:10.5194/essd-12-3545-2020 (SPAM2010 methods)
#   Data: SPAM2010 v2.0, doi:10.7910/DVN/PRFF8V (CC-BY 4.0)

download_spam <- function(dest_dir) {
  target_dir <- file.path(dest_dir, "SPAM", "2010")
  if (!dir.exists(target_dir)) {
    dir.create(target_dir, recursive = TRUE)
  }

  manifest <- .spam_download_manifest_2010()
  purrr::pwalk(manifest, \(var, zip_name, bytes, md5, dataverse_file_id, ...) {
    .spam_dl_fetch_zip(target_dir, zip_name, bytes, md5, dataverse_file_id)
    .spam_dl_extract_member(target_dir, var, zip_name, "I")
    .spam_dl_extract_member(target_dir, var, zip_name, "R")
  })

  cli::cli_alert_success("SPAM2010 v2.0 ready in {.path {target_dir}}")
  cli::cli_alert_info(
    "Point {.envvar WHEP_SPAM_DIR} at {.path {dest_dir}}/SPAM (its PARENT --
     read_spam_yields() appends the vintage itself, so files land under
     {.path {target_dir}})."
  )
  invisible(target_dir)
}

# The same three rows whep:::.spam_manifest() carries for vintage \"2010\",
# duplicated here (not sourced from the package) because this script also
# has to run standalone via download_all.R without whep installed.
.spam_download_manifest_2010 <- function() {
  tibble::tribble(
    ~var,
    ~zip_name,
    ~bytes,
    ~md5,
    ~dataverse_file_id,
    "harvested_area",
    "spam2010v2r0_global_harv_area.csv.zip",
    144504301,
    "e45d61cc0694a905c9760cfff6ea66b8",
    3984976L,
    "production",
    "spam2010v2r0_global_prod.csv.zip",
    156093566,
    "5db136ca02ed9bb8fb6053cebf7cdffa",
    3984975L,
    "yield",
    "spam2010v2r0_global_yield.csv.zip",
    184388916,
    "d6955e55ea6b3addd6c853706566ab7c",
    3984974L
  )
}

.spam_dl_fetch_zip <- function(
  target_dir,
  zip_name,
  bytes,
  md5,
  dataverse_file_id
) {
  zip_path <- file.path(target_dir, zip_name)
  if (file.exists(zip_path) && file.size(zip_path) == bytes) {
    cli::cli_alert_info(
      "SPAM {zip_name}: already present ({round(bytes / 1e6)} MB)"
    )
    return(invisible(zip_path))
  }
  url <- paste0(
    "https://dataverse.harvard.edu/api/access/datafile/",
    dataverse_file_id
  )
  cli::cli_alert("Downloading SPAM {zip_name} ({round(bytes / 1e6)} MB)...")
  old_timeout <- getOption("timeout")
  on.exit(options(timeout = old_timeout), add = TRUE)
  options(timeout = max(600, old_timeout %||% 60))
  utils::download.file(url, zip_path, mode = "wb")
  if (!identical(unname(tools::md5sum(zip_path)), md5)) {
    unlink(zip_path)
    cli::cli_abort(c(
      "{.file {zip_name}} does not match the MD5 published on Harvard
       Dataverse (doi:10.7910/DVN/PRFF8V).",
      x = "The partial or corrupt file was removed.",
      i = "Re-run to download it again."
    ))
  }
  cli::cli_alert_success("{zip_name}: MD5 verified")
  invisible(zip_path)
}

# Extracts the one technology member read_spam_yields() reads, to the exact
# normalized name whep:::.spam_cached_member_path() looks for, so a fresh
# WHEP_SPAM_DIR pointed at this script's output is an instant cache hit
# (never re-opens the zip). The real member name inside the zip is
# discovered from the zip's own listing, not assumed.
.spam_dl_extract_member <- function(target_dir, var, zip_name, tech) {
  normalized <- file.path(
    target_dir,
    "extracted",
    sprintf("spam_2010_%s_%s.csv", var, tolower(tech))
  )
  if (file.exists(normalized)) {
    cli::cli_alert_info("SPAM {var} ({tech}): already extracted")
    return(invisible(normalized))
  }
  letter <- switch(var, harvested_area = "H", production = "P", yield = "Y")
  zip_path <- file.path(target_dir, zip_name)
  entries <- utils::unzip(zip_path, list = TRUE)$Name
  pattern <- sprintf("_%s_T?%s\\.csv$", letter, tech)
  member <- entries[grepl(pattern, entries, ignore.case = TRUE)]
  if (length(member) != 1L) {
    cli::cli_abort(c(
      "Could not find a single {.val {var}} ({.val {tech}}) member inside
       {.file {zip_name}}.",
      i = "Archive entries: {.file {entries}}."
    ))
  }
  extract_dir <- dirname(normalized)
  dir.create(extract_dir, recursive = TRUE, showWarnings = FALSE)
  cli::cli_alert("Extracting {member}...")
  utils::unzip(zip_path, files = member, exdir = extract_dir, junkpaths = TRUE)
  file.rename(file.path(extract_dir, basename(member)), normalized)
  cli::cli_alert_success("SPAM {var} ({tech}): extracted")
  invisible(normalized)
}
