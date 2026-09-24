# SPAM harvested area, production and yield by crop and technology, for the
# irrigated:rainfed regime yield ratio (issue #1233, plan decision D15/D16).
#
# CONFIRMED FORMAT (verified 2026-09-24 against live Harvard Dataverse
# downloads; do not re-guess):
# - Each SPAM2010 v2.0 Global_CSV zip (harv_area/prod/yield) holds SIX
#   per-technology member CSVs (TA/TH/TI/TL/TR/TS), e.g.
#   "spam2010V2r0_global_H_TI.csv". Only the "TI" (irrigated) and "TR"
#   (all-rainfed; SPAM's own TA-TI = TH+TL+TS aggregate) members are read;
#   TA/TH/TL/TS are never extracted.
# - Each member is wide: one row per SPAM pixel ("alloc_key"/"cell5m", both
#   confirmed 1:1 with the row -- 832,827 data rows, no duplicates either
#   way), identifying columns iso3, alloc_key, cell5m, x, y, rec_type,
#   tech_type, unit, name_cntr, name_adm1, name_adm2 (plus crea_date,
#   year_data, source, not kept here), and one column per crop coded
#   "<spam_crop>_i" or "<spam_crop>_r" (42 crops in SPAM2010 v2.0).
# - Units, read from the files' own "unit" column: harvested area "ha",
#   production "mt" (metric tons -> `production_t`), yield "kg/ha" (kept
#   as published, not converted to t/ha).
# - Known SPAM2010 data quirk: exactly one row (alloc_key "04383640", a
#   Heilongjiang/China cell) carries iso3 "ind" (lowercase, wrong) in the
#   production TI file only, while the harvested-area and yield files give
#   "CHN" for the same alloc_key and name_cntr reads "China" everywhere.
#   Passed through unmodified (raw values, not corrected) and documented
#   here rather than silently fixed; `iso3` therefore is not perfectly
#   reliable at the single-row level, is otherwise clean over 832,827 rows.
#
# SPAM2020 v2.0 IS NOT FETCHED (plan decision D16, 2026-09-24 investigation):
# Harvard Dataverse gates every file of doi:10.7910/DVN/SWPENT, including its
# own 6 KB ReadMe, behind a mandatory guestbook ("IFPRI Guestbook 2021", id
# 380: `GET /api/guestbooks/380` returns `"enabled": true, "emailRequired":
# true, "institutionRequired": true`). The Dataverse API's own documented way
# past it needs an authenticated account (`X-Dataverse-key`) submitting a
# guestbookResponse with name/email/institution, retained by Harvard and
# shared with IFPRI per its Terms of Use -- i.e. a login AND a licence
# click-through, which WHEP never automates. `vintage = "2020"` therefore
# reads ONLY from a directory the caller supplies (`dir` or
# `WHEP_SPAM_DIR`); its files are verified against the MD5s Dataverse
# publishes on the record (recorded in `.spam_manifest()` from the API,
# unverified by download), never downloaded. SPAM2010 v2.0 has no such gate
# (`GET /api/guestbooks/9` returns `"enabled": false`) and is fetched
# on demand like WHEP's other verified downloads.
#
# The member-resolution code path (`.spam_zip_find_member()`) is identical
# for both vintages: it searches the zip's own file listing for a
# `_<H|P|Y>_T?<I|R>.csv`-shaped name rather than assuming SPAM2020 kept
# SPAM2010's exact member names, and `.spam_check_member_columns()` aborts
# clearly, naming what it found, if the expected identifying or
# technology-suffixed crop columns are not there. SPAM2020's layout has not
# been verified against SPAM2010's (the guestbook blocks even the readme).

#' Read SPAM harvested area, production and yield by crop and technology.
#'
#' @description
#' Reads the SPAM (Spatial Production Allocation Model) global gridded crop
#' tables for the irrigated ("I") and all-rainfed ("R") technologies, per
#' ~5-arcmin pixel and crop, and joins harvested area, production and yield
#' into one row per (pixel, crop, technology). This is the per-crop,
#' per-country "Level" input to the irrigated:rainfed regime yield ratio
#' (plan decision D15); it does not itself compute the ratio.
#'
#' `vintage = "2010"` (SPAM2010 v2.0, doi:10.7910/DVN/PRFF8V) is fetched
#' automatically: the three needed Global_CSV zips (harvested area,
#' production, yield) are downloaded from Harvard Dataverse on first use,
#' verified against their published MD5s, and cached under
#' `rappdirs::user_cache_dir("whep")`; `dir` or the `WHEP_SPAM_DIR`
#' environment variable overrides the cache with a directory of your own
#' (holding the zip files, or their already-extracted `_TI`/`_TR` members).
#'
#' `vintage = "2020"` (SPAM2020 v2.0, doi:10.7910/DVN/SWPENT) is **never**
#' downloaded: Harvard Dataverse gates every file of that dataset behind a
#' mandatory guestbook requiring an email and institution, and the API's
#' documented way past it needs a logged-in Dataverse account submitting
#' that guestbook response. `dir`/`WHEP_SPAM_DIR` therefore must point at a
#' directory you have already populated by hand -- fill the guestbook once in
#' a browser at the dataset's Dataverse page, download the three Global_CSV
#' zips, and pass their directory. The zips are verified against the MD5s
#' Dataverse publishes on the record (not re-checked against a live
#' download, since none is attempted); the column layout is read through
#' the exact same parser as SPAM2010, and aborts clearly, naming what it
#' found, if the expected identifying or crop columns are not there --
#' SPAM2020's layout has not been verified against SPAM2010's, since the
#' guestbook blocks even its own 6 KB ReadMe.
#'
#' @param vintage Which SPAM release to read: `"2010"` (default, fetched
#'   automatically) or `"2020"` (never fetched; needs `dir`/`WHEP_SPAM_DIR`,
#'   see Description).
#' @param dir Optional path to a directory holding the SPAM Global_CSV zip
#'   files (or their extracted `_TI`/`_TR` members), overriding
#'   `WHEP_SPAM_DIR`. A `vintage`-named subdirectory is read from it (e.g.
#'   `file.path(dir, "2010")`), matching what
#'   `inst/scripts/download/download_spam.R` writes.
#' @param example If `TRUE`, return a small fixture instead of reading SPAM
#'   data. Defaults to `FALSE`.
#' @return A tibble with `cell5m` (SPAM's pixel id), `lon`, `lat`, `iso3`,
#'   `name_cntr`, `name_adm1`, `name_adm2`, `alloc_key` (SPAM's own
#'   admin-coded pixel key), `spam_crop` (SPAM's short crop code, e.g.
#'   `"whea"`), `technology` (`"I"` irrigated or `"R"` all-rainfed),
#'   `harvested_area_ha`, `production_t`, `yield_kg_ha` (as SPAM publishes
#'   it, not converted), `vintage` and `method_spam_source`
#'   (`"cache"`: fetched and MD5-verified on demand; `"user_supplied"`:
#'   read from `dir`/`WHEP_SPAM_DIR`, MD5-verified when a zip was found
#'   there, unverified when only already-extracted CSVs were). A
#'   provenance record (DOI, vintage, origin) is attached; read it back with
#'   [get_provenance()].
#' @source Yu, Q. et al. (2020). A cultivated planet in 2010 -- Part 2: The
#'   global gridded agricultural-production maps. Earth System Science Data
#'   12, 3545-3572. \doi{10.5194/essd-12-3545-2020}. Data: SPAM2010 v2.0,
#'   \doi{10.7910/DVN/PRFF8V} (CC-BY 4.0). SPAM2020 v2.0:
#'   \doi{10.7910/DVN/SWPENT} (CC-BY 4.0; gated behind a Harvard Dataverse
#'   guestbook, see Description).
#' @export
#' @examples
#' read_spam_yields(example = TRUE)
read_spam_yields <- function(
  vintage = c("2010", "2020"),
  dir = NULL,
  example = FALSE
) {
  if (isTRUE(example)) {
    return(.example_spam_yields())
  }
  vintage <- rlang::arg_match(vintage)
  resolved <- .resolve_spam_dir(vintage, dir)
  vars <- c("harvested_area", "production", "yield")
  tables <- purrr::map(vars, \(v) .spam_read_variable(vintage, v, resolved))
  names(tables) <- vars
  .spam_combine_variables(tables) |>
    dplyr::rename(lon = "x", lat = "y") |>
    dplyr::mutate(
      vintage = vintage,
      method_spam_source = resolved$origin
    ) |>
    dplyr::relocate(
      "cell5m",
      "lon",
      "lat",
      "iso3",
      "name_cntr",
      "name_adm1",
      "name_adm2",
      "alloc_key",
      "spam_crop",
      "technology",
      "harvested_area_ha",
      "production_t",
      "yield_kg_ha",
      "vintage",
      "method_spam_source"
    ) |>
    tibble::as_tibble() |>
    attach_provenance(.spam_provenance(vintage, resolved))
}

# ---- Private helpers --------------------------------------------------

# The Dataverse file manifest, read off each record's own API metadata
# (`GET /api/datasets/:persistentId/?persistentId=doi:...`) on 2026-09-24.
# SPAM2020's `dataverse_file_id` is NA: it is never downloaded (see the file
# header), so there is nothing to fetch it with.
.spam_manifest <- function() {
  tibble::tribble(
    ~vintage,
    ~var,
    ~zip_name,
    ~bytes,
    ~md5,
    ~dataverse_file_id,
    "2010",
    "harvested_area",
    "spam2010v2r0_global_harv_area.csv.zip",
    144504301,
    "e45d61cc0694a905c9760cfff6ea66b8",
    3984976L,
    "2010",
    "production",
    "spam2010v2r0_global_prod.csv.zip",
    156093566,
    "5db136ca02ed9bb8fb6053cebf7cdffa",
    3984975L,
    "2010",
    "yield",
    "spam2010v2r0_global_yield.csv.zip",
    184388916,
    "d6955e55ea6b3addd6c853706566ab7c",
    3984974L,
    "2020",
    "harvested_area",
    "spam2020V2r2_global_harvested_area.csv.zip",
    109231267,
    "9e82eb7c6202cdcf10daf3508356c2dd",
    NA_integer_,
    "2020",
    "production",
    "spam2020V2r2_global_production.csv.zip",
    119056485,
    "9afa78da4239ddfa55155d53c41be106",
    NA_integer_,
    "2020",
    "yield",
    "spam2020V2r2_global_yield.csv.zip",
    146052256,
    "6892989966876b66f0cc30e3982cada8",
    NA_integer_
  )
}

.spam_manifest_row <- function(vintage, var) {
  row <- dplyr::filter(
    .spam_manifest(),
    .data$vintage == .env$vintage,
    .data$var == .env$var
  )
  if (nrow(row) != 1L) {
    cli::cli_abort(
      "No SPAM manifest entry for vintage {.val {vintage}}, var {.val {var}}."
    )
  }
  row
}

.spam_doi <- function(vintage) {
  if (identical(vintage, "2010")) "10.7910/DVN/PRFF8V" else "10.7910/DVN/SWPENT"
}

.spam_cache_dir <- function(vintage) {
  file.path(rappdirs::user_cache_dir("whep"), "spam", vintage)
}

# `dir`, else `WHEP_SPAM_DIR`, else (2010 only) the on-demand cache.
# 2020 never resolves to the cache: it aborts with the guestbook
# explanation, naming the exact blocker so a human can go and clear it.
.resolve_spam_dir <- function(vintage, dir = NULL) {
  base <- dir %||% Sys.getenv("WHEP_SPAM_DIR", "")
  if (.has_path(base)) {
    return(list(dir = file.path(base, vintage), origin = "user_supplied"))
  }
  if (identical(vintage, "2020")) {
    doi <- .spam_doi("2020")
    cli::cli_abort(c(
      "SPAM2020 v2.0 ({.val {doi}}) is not fetched automatically.",
      x = "Harvard Dataverse gates every file in this dataset behind a
           mandatory guestbook (\"IFPRI Guestbook 2021\", id 380: email and
           institution required), and its API needs a logged-in Dataverse
           account (an {.envvar X-Dataverse-key} token) to submit that
           response -- WHEP never does this on your behalf.",
      i = "Fill the guestbook once in a browser at
           {.url https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/SWPENT},
           download the three Global_CSV zips (harvested area, production,
           yield), and point {.arg dir} or {.envvar WHEP_SPAM_DIR} at their
           directory.",
      i = "The expected file names, sizes and MD5s are in
           {.fun whep:::.spam_manifest}; they are verified against your
           local copy, never against a fresh download."
    ))
  }
  list(dir = .spam_cache_dir(vintage), origin = "cache")
}

.spam_value_col <- function(var) {
  switch(
    var,
    harvested_area = "harvested_area_ha",
    production = "production_t",
    yield = "yield_kg_ha"
  )
}

.spam_full_id_cols <- function() {
  c(
    "iso3",
    "alloc_key",
    "cell5m",
    "x",
    "y",
    "name_cntr",
    "name_adm1",
    "name_adm2"
  )
}

.spam_known_nondata_cols <- function() {
  c(
    .spam_full_id_cols(),
    "rec_type",
    "tech_type",
    "unit",
    "crea_date",
    "year_data",
    "source"
  )
}

# Fail closed on a member whose header does not have the identifying columns
# every SPAM2010 member carries, or no crop column at all. Returns the crop
# columns (both technologies) so the caller does not re-derive them. This is
# the "abort clearly if the expected columns are missing" path for
# SPAM2020's unverified layout (plan D16), exercised identically for
# SPAM2010.
.spam_check_member_columns <- function(header, path) {
  required <- setdiff(.spam_full_id_cols(), c("x", "y"))
  required <- c(required, "x", "y")
  missing <- setdiff(required, header)
  crop_cols <- header[
    grepl("_[ir]$", header, ignore.case = TRUE) &
      !(header %in% .spam_known_nondata_cols())
  ]
  if (length(missing) > 0L || length(crop_cols) == 0L) {
    cli::cli_abort(c(
      "{.file {basename(path)}} does not have the expected SPAM
       technology-suffixed layout.",
      x = if (length(missing) > 0L) {
        "Missing column{?s}: {.field {missing}}."
      },
      x = if (length(crop_cols) == 0L) {
        "No crop column ending in {.val _i} or {.val _r} was found."
      },
      i = "Columns found: {.field {header}}.",
      i = "This is the expected outcome if SPAM2020's CSV layout differs from
           SPAM2010's -- WHEP has not verified it (see {.fun read_spam_yields}
           for why)."
    ))
  }
  crop_cols
}

# One technology's long table for one variable: `id_cols` are read verbatim
# (untransformed except x/y renamed by the caller), the matching `_i`/`_r`
# crop columns are pivoted to (spam_crop, <value_col>).
.spam_read_member <- function(path, tech, value_col, id_cols) {
  header <- names(data.table::fread(path, nrows = 0))
  crop_cols <- .spam_check_member_columns(header, path)
  suffix <- if (identical(tech, "I")) "_i" else "_r"
  tech_crop_cols <- crop_cols[grepl(
    paste0(suffix, "$"),
    crop_cols,
    ignore.case = TRUE
  )]
  if (length(tech_crop_cols) == 0L) {
    cli::cli_abort(
      "{.file {basename(path)}} has no crop column ending in {.val {suffix}}
       for technology {.val {tech}}."
    )
  }
  dt <- data.table::fread(path, select = c(id_cols, tech_crop_cols))
  long <- data.table::melt(
    dt,
    id.vars = id_cols,
    measure.vars = tech_crop_cols,
    variable.name = "spam_crop",
    value.name = value_col,
    variable.factor = FALSE
  )
  long[,
    spam_crop := sub(paste0(suffix, "$"), "", spam_crop, ignore.case = TRUE)
  ]
  long[, technology := tech]
  tibble::as_tibble(long)
}

# Both technologies for one variable (harvested_area/production/yield).
# `harvested_area` carries the full identifying columns (the join anchor in
# `.spam_combine_variables()`); production/yield need only the join key,
# which keeps the ~35M-row-per-technology melt from tripling the id-column
# memory for no benefit.
.spam_read_variable <- function(
  vintage,
  var,
  resolved,
  download = .spam_download_zip
) {
  value_col <- .spam_value_col(var)
  id_cols <- if (identical(var, "harvested_area")) {
    .spam_full_id_cols()
  } else {
    "cell5m"
  }
  i_path <- .spam_locate_member(resolved, vintage, var, "I", download)
  r_path <- .spam_locate_member(resolved, vintage, var, "R", download)
  dplyr::bind_rows(
    .spam_read_member(i_path, "I", value_col, id_cols),
    .spam_read_member(r_path, "R", value_col, id_cols)
  )
}

.spam_combine_variables <- function(tables) {
  join_keys <- c("cell5m", "technology", "spam_crop")
  tables$harvested_area |>
    dplyr::left_join(
      dplyr::select(
        tables$production,
        dplyr::all_of(c(join_keys, "production_t"))
      ),
      by = join_keys,
      relationship = "one-to-one"
    ) |>
    dplyr::left_join(
      dplyr::select(tables$yield, dplyr::all_of(c(join_keys, "yield_kg_ha"))),
      by = join_keys,
      relationship = "one-to-one"
    )
}

# The normalized cache path WHEP controls (not SPAM's own member file name,
# which is only discovered once the zip is opened): a cache hit here skips
# opening the zip at all on every read after the first.
.spam_cached_member_path <- function(dir, vintage, var, tech) {
  file.path(
    dir,
    "extracted",
    sprintf("spam_%s_%s_%s.csv", vintage, var, tolower(tech))
  )
}

# Find, verifying as needed, the local path of one technology's member CSV
# for one variable. Order: our own normalized cache: a zip already present
# (verified, downloaded only for `origin == "cache"`): abort. A verified zip
# is never re-verified on a later call (the normalized cache short-circuits
# first), and a user-supplied zip that fails verification is reported but
# never deleted -- it is the caller's file, not WHEP's.
.spam_locate_member <- function(resolved, vintage, var, tech, download) {
  normalized <- .spam_cached_member_path(resolved$dir, vintage, var, tech)
  if (file.exists(normalized)) {
    return(normalized)
  }
  manifest_row <- .spam_manifest_row(vintage, var)
  zip_path <- file.path(resolved$dir, manifest_row$zip_name)
  if (file.exists(zip_path)) {
    .spam_verify_zip(zip_path, manifest_row, unlink_on_fail = FALSE)
  } else if (identical(resolved$origin, "cache")) {
    zip_path <- download(resolved$dir, manifest_row)
  } else {
    cli::cli_abort(c(
      "Neither {.file {basename(normalized)}} nor {.file {manifest_row$zip_name}}
       was found under {.path {resolved$dir}}.",
      i = "Point {.arg dir} / {.envvar WHEP_SPAM_DIR} at a directory holding
           the SPAM {vintage} Global_CSV zip (or its already-extracted
           {.val I}/{.val R} member)."
    ))
  }
  .spam_extract_member(zip_path, var, tech, normalized)
}

# Discover the real member name inside the zip (never assumed -- SPAM2020's
# internal naming is unverified, see the file header) and extract it to
# WHEP's own normalized cache path.
.spam_extract_member <- function(zip_path, var, tech, normalized) {
  member <- .spam_zip_find_member(zip_path, var, tech)
  extract_dir <- dirname(normalized)
  dir.create(extract_dir, recursive = TRUE, showWarnings = FALSE)
  utils::unzip(zip_path, files = member, exdir = extract_dir, junkpaths = TRUE)
  extracted <- file.path(extract_dir, basename(member))
  if (!file.exists(extracted)) {
    cli::cli_abort(
      "Extracting {.file {member}} from {.file {basename(zip_path)}} failed."
    )
  }
  if (!identical(extracted, normalized)) {
    file.rename(extracted, normalized)
  }
  normalized
}

# A `_<H|P|Y>_T?<I|R>.csv`-shaped entry in the zip's own listing -- never a
# hardcoded SPAM2010 file name, so the exact same call works whether
# SPAM2020 kept that naming or not (plan D16: "parse it with the same code
# path"). Aborts, listing every entry the zip actually has, on anything but
# exactly one match.
.spam_zip_find_member <- function(zip_path, var, tech) {
  letter <- switch(var, harvested_area = "H", production = "P", yield = "Y")
  entries <- utils::unzip(zip_path, list = TRUE)$Name
  pattern <- sprintf("_%s_T?%s\\.csv$", letter, tech)
  matches <- entries[grepl(pattern, entries, ignore.case = TRUE)]
  if (length(matches) != 1L) {
    cli::cli_abort(c(
      "Could not find a single {.val {var}} ({.val {tech}}) member inside
       {.file {basename(zip_path)}}.",
      i = "Looked for a file name matching {.val {pattern}}.",
      i = "Archive entries: {.file {entries}}.",
      i = "SPAM2020's internal file naming has not been verified against
           SPAM2010's; a naming change is an expected reason for this abort."
    ))
  }
  matches
}

# `unlink_on_fail` is FALSE for a zip found under a caller-supplied `dir` /
# `WHEP_SPAM_DIR`: it is the caller's file, and only WHEP's own cache
# downloads are ours to discard on a bad checksum.
.spam_verify_zip <- function(zip_path, manifest_row, unlink_on_fail) {
  ok <- isTRUE(file.exists(zip_path)) &&
    isTRUE(file.size(zip_path) == manifest_row$bytes) &&
    identical(unname(tools::md5sum(zip_path)), manifest_row$md5)
  if (ok) {
    return(invisible(TRUE))
  }
  if (isTRUE(unlink_on_fail)) {
    unlink(zip_path)
  }
  # Bound in a local: cli >= 3.4 reads a `{}` expression starting with a dot
  # as a style, not a call, so `.spam_doi(...)` cannot be interpolated
  # directly (#621 has the same fix elsewhere in the package).
  doi <- .spam_doi(manifest_row$vintage)
  cli::cli_abort(c(
    "{.file {basename(zip_path)}} does not match its published MD5.",
    x = "Expected {manifest_row$bytes} bytes, MD5 {.val {manifest_row$md5}}.",
    i = if (isTRUE(unlink_on_fail)) {
      "The file was removed; re-run to download it again."
    } else {
      "Re-download {.file {manifest_row$zip_name}} from the SPAM
       {manifest_row$vintage} Dataverse record (doi:{.val {doi}}) and replace
       the file at {.path {zip_path}}."
    }
  ))
}

.spam_download_zip <- function(cache_dir, manifest_row, fetch = .spam_fetch) {
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  zip_path <- file.path(cache_dir, manifest_row$zip_name)
  cli::cli_alert_info(
    "Downloading SPAM{manifest_row$vintage} {manifest_row$zip_name}
     ({round(manifest_row$bytes / 1e6)} MB) from Harvard Dataverse..."
  )
  fetch(manifest_row$dataverse_file_id, zip_path)
  .spam_verify_zip(zip_path, manifest_row, unlink_on_fail = TRUE)
  cli::cli_alert_success("{manifest_row$zip_name}: MD5 verified.")
  zip_path
}

# SPAM2010's files are anonymous, ungated downloads (no guestbook, no
# account): a plain GET to the Dataverse access-by-id endpoint 303-redirects
# to a signed S3 URL. Never called for SPAM2020 (`.resolve_spam_dir()`
# aborts first).
.spam_fetch <- function(file_id, path) {
  old <- options(timeout = max(600, getOption("timeout")))
  on.exit(options(old), add = TRUE)
  url <- paste0("https://dataverse.harvard.edu/api/access/datafile/", file_id)
  utils::download.file(url, path, mode = "wb", quiet = TRUE)
  invisible(path)
}

.spam_provenance <- function(vintage, resolved) {
  tibble::tibble(
    recorded_at = Sys.time(),
    whep_version = as.character(utils::packageVersion("whep")),
    r_version = as.character(getRversion()),
    input_alias = "spam_yields",
    input_version = vintage,
    input_origin = resolved$origin,
    input_source_id = .spam_doi(vintage),
    input_first_year = NA_integer_,
    input_last_year = NA_integer_
  )
}
