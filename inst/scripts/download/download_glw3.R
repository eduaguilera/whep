# -----------------------------------------------------------------------
# download_glw3.R
#
# Downloads the Gridded Livestock of the World version 3 rasters (GLW 3,
# Gilbert et al. 2018, Scientific Data 5:180227,
# doi:10.1038/sdata.2018.227) that read_glw_density() reads.
#
# GLW3 is published on Harvard Dataverse as eight separate datasets, one
# per species, collected in the `glw_3` dataverse -- so there is one DOI
# per species, not one for the product. Each dataset ships a dasymetric
# raster (5_<Sp>_2010_Da.tif), an areal-weighted one (6_<Sp>_2010_Aw.tif),
# a prediction-status raster, a per-pixel area raster, three quickview
# PNGs and an 11 MB metadata HTML. Only the two data rasters are fetched:
# the pixel values are absolute animal counts per pixel, so the area
# raster is needed only to build a per-km2 product, which WHEP does not.
#
# Both variants are downloaded by default (~159 MB dasymetric + ~80 MB
# areal-weighted over the eight species) because read_glw_density() offers
# both and a half-filled directory would make `variant = "AW"` abort.
#
# CHECKSUMS. Dataverse publishes an MD5 per file through its dataset API,
# and every file below is pinned to the MD5 that API returned on
# 2026-09-03 for version 3.0 of each dataset. Each download is verified
# against the pinned value, and the listing's MD5 is compared with it
# first, so a republished dataset stops this script instead of silently
# handing WHEP a different vintage under the same name.
#
# Licence CC0 1.0 Universal, per every dataset record: no redistribution
# condition. The paper is still the citation.
#
# The rasters land in <dest_dir>/GLW3/, which is what WHEP_GLW3_DIR points
# at. Verified against the live Dataverse API on 2026-09-03.
# -----------------------------------------------------------------------

download_glw3 <- function(dest_dir, variants = c("Da", "Aw")) {
  variants <- match.arg(variants, c("Da", "Aw"), several.ok = TRUE)
  target_dir <- file.path(dest_dir, "GLW3")
  if (!dir.exists(target_dir)) {
    dir.create(target_dir, recursive = TRUE)
  }

  species <- .glw3_species()
  for (i in seq_len(nrow(species))) {
    listing <- .glw3_file_listing(species$doi[[i]])
    for (variant in variants) {
      .glw3_download_one(
        species$glw_code[[i]],
        variant,
        listing,
        target_dir
      )
    }
  }

  cli::cli_alert_success("GLW3: rasters in {.file {target_dir}}")
  invisible()
}

# One Harvard Dataverse DOI per species, read off the `glw_3` dataverse
# listing (/api/dataverses/glw_3/contents) on 2026-09-03.
.glw3_species <- function() {
  tibble::tribble(
    ~glw_species,  ~glw_code, ~doi,
    "buffaloes",   "Bf",      "10.7910/DVN/5U8MWI",
    "cattle",      "Ct",      "10.7910/DVN/GIVQ75",
    "chickens",    "Ch",      "10.7910/DVN/SUFASB",
    "ducks",       "Dk",      "10.7910/DVN/ICHCBH",
    "goats",       "Gt",      "10.7910/DVN/OCPH42",
    "horses",      "Ho",      "10.7910/DVN/7Q52MV",
    "pigs",        "Pg",      "10.7910/DVN/33N0JG",
    "sheep",       "Sh",      "10.7910/DVN/BLWPZN"
  )
}

# The published MD5 and byte size of each raster, as the dataset API
# reported them for version 3.0 on 2026-09-03. Do not edit these to make a
# download pass: a mismatch means the file on Dataverse is not the one
# WHEP's results were built on.
.glw3_checksums <- function() {
  tibble::tribble(
    ~glw_code, ~variant, ~md5,                               ~bytes,
    "Bf",      "Da",     "bf8544b37a6a52504feda5e8d9b3ea40", 11101498,
    "Bf",      "Aw",     "839723b8cbe03a7a6e0072f8005cab7a",  5257329,
    "Ct",      "Da",     "dca9086b131859a69d4dcb90a47d8e00", 21982446,
    "Ct",      "Aw",     "93241e39a1b8e8a866c0378b1c622c6c", 11416768,
    "Ch",      "Da",     "b4e46d8fe77f7ac0594a7423206529ca", 20066334,
    "Ch",      "Aw",     "68b6a4b4a011a578493c61a6827c04c1", 10727526,
    "Dk",      "Da",     "96479d3aaf240cebece0317738d203c6", 22441017,
    "Dk",      "Aw",     "abaf52f0c36dc06a58f0cc401737f70a",  9689582,
    "Gt",      "Da",     "7734b1120397b9a9a69b1f4573923624", 21615857,
    "Gt",      "Aw",     "9b337e96278a7e854b8aa5d64c2b7cbd", 11003799,
    "Ho",      "Da",     "bacf2cf0efb9ef31a0b86ea40fa914c6", 21609006,
    "Ho",      "Aw",     "b021a8b9ea5d59d33c492871c3190c1c", 10316910,
    "Pg",      "Da",     "ddbe18cbc66c7c8de1f9e37e1602ff43", 19425702,
    "Pg",      "Aw",     "0ec2ac2854bbe9cbedbcbf1b5bec2f84", 10511833,
    "Sh",      "Da",     "f15da29e8d3276d63fb97e4def3dfad7", 20928082,
    "Sh",      "Aw",     "5ce1c333aba6fe3b967e4c6a6446054a", 11213964
  )
}

.glw3_api_url <- function() {
  "https://dataverse.harvard.edu/api/datasets/:persistentId?persistentId="
}

.glw3_access_url <- function(file_id) {
  paste0("https://dataverse.harvard.edu/api/access/datafile/", file_id)
}

.glw3_file_name <- function(code, variant) {
  prefix <- c(Da = "5", Aw = "6")[[variant]]
  sprintf("%s_%s_2010_%s.tif", prefix, code, variant)
}

# The dataset's file list: name, numeric file id (the access endpoint's
# key) and published MD5. Dataverse serves this without a key.
.glw3_file_listing <- function(doi) {
  url <- paste0(.glw3_api_url(), "doi:", doi)
  response <- httr::GET(url)
  if (httr::http_error(response)) {
    cli::cli_abort(
      "Failed to reach the Dataverse record for {doi}
       ({httr::status_code(response)})."
    )
  }
  files <- httr::content(
    response,
    as = "parsed",
    type = "application/json"
  )$data$latestVersion$files
  if (length(files) == 0) {
    cli::cli_abort("The Dataverse record for {doi} lists no file.")
  }
  do.call(
    rbind,
    lapply(files, function(entry) {
      checksum <- entry$dataFile$checksum$value
      data.frame(
        filename = entry$dataFile$filename,
        file_id = entry$dataFile$id,
        md5 = if (is.null(checksum)) NA_character_ else checksum,
        stringsAsFactors = FALSE
      )
    })
  )
}

.glw3_download_one <- function(code, variant, listing, target_dir) {
  file_name <- .glw3_file_name(code, variant)
  expected <- .glw3_checksums()
  expected <- expected[
    expected$glw_code == code & expected$variant == variant,
  ]
  entry <- listing[listing$filename == file_name, ]
  if (nrow(entry) != 1L) {
    cli::cli_abort(
      "The Dataverse record offers {nrow(entry)} file{?s} named
       {.file {file_name}}; expected exactly one."
    )
  }
  if (!identical(entry$md5[[1]], expected$md5[[1]])) {
    cli::cli_abort(c(
      "{.file {file_name}} on Dataverse is not the pinned GLW3 version.",
      "x" = "Record MD5 {.val {entry$md5[[1]]}}, pinned
             {.val {expected$md5[[1]]}}.",
      "i" = "The dataset has been republished. Re-verify the raster and
             update .glw3_checksums() deliberately; do not overwrite the
             pinned value to make this pass."
    ))
  }

  out_path <- file.path(target_dir, file_name)
  if (.glw3_file_ok(out_path, expected$md5[[1]])) {
    cli::cli_alert_info("GLW3 {file_name}: already downloaded")
    return(invisible())
  }

  size_mb <- round(expected$bytes[[1]] / 1e6)
  cli::cli_alert("Downloading {file_name} (~{size_mb} MB)...")
  utils::download.file(
    .glw3_access_url(entry$file_id[[1]]),
    out_path,
    mode = "wb"
  )
  if (!.glw3_file_ok(out_path, expected$md5[[1]])) {
    unlink(out_path)
    cli::cli_abort(c(
      "{.file {file_name}} failed MD5 verification and was deleted.",
      "i" = "Expected {.val {expected$md5[[1]]}}."
    ))
  }
  cli::cli_alert_success("GLW3 {file_name}: saved")
  invisible()
}

.glw3_file_ok <- function(path, md5) {
  file.exists(path) && identical(unname(tools::md5sum(path)), md5)
}
