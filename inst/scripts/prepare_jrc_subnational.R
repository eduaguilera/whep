# -----------------------------------------------------------------------
# prepare_jrc_subnational.R
#
# Prepares the JRC harmonised sub-national crop statistics of the European
# Union as a WHEP pin: downloads the published archive from the AGRI4CAST
# DataPortal, harmonises it into the source-native long shape the other
# admin-statistics readers emit, writes the content manifest that pins the
# exact bytes it was built from, and stages the pin folder. It uploads
# nothing.
#
# Dataset
# -------
# Ronchetti, G., Nisini Scacchiafichi, L., Seguini, L., Cerrani, I. and
# Van der Velde, M. (2024) "Harmonized European Union subnational crop
# statistics reveal climate impacts and crop cultivation shifts", Earth
# System Science Data 16, 1623-1649, doi:10.5194/essd-16-1623-2024.
# Dataset record: doi:10.2905/685949ff-56de-4646-a8df-844b5bb5f835
# (JRC-catalogue DOI doi:10.2905/jrc.g3w5s7r; both resolve, verified on
# 2026-09-02 against the DataCite API).
#
# Licence: the European Commission reuse notice (Commission Decision
# 2011/833/EU of 12 December 2011) -- "Reuse is authorised, provided the
# source is acknowledged", as stated on the catalogue record. It is NOT
# published under a Creative Commons licence: DataCite carries an empty
# `rightsList` for both DOIs and data.europa.eu carries no licence field.
#
# Access, established 2026-09-02
# ------------------------------
# The ESSD paper says the database is obtained through an ECAS/EU Login
# account. That is no longer how the portal serves it. The whole route
# below runs anonymously, with no account, no cookie of ours and no
# credential:
#
#   1. GET  /assets/env-json-config.json      -> the portal's route table
#   2. POST /sicherheitstoken  (empty body)   -> opens an anonymous
#      session; the PostgREST API answers 401 "Anonymous access is
#      disabled" without it
#   3. GET  /apirpc/ResourceTree?id=39&selector=MLH
#      -> resource 39, "Harmonised sub-national crop statistics of the
#         European Union", label EU27-CROP-STATS-2025, release 2025.01,
#         and the static distribution's path
#   4. GET  /download/dataportal_resources/static_data/
#           EU27_CROP_STATS_2025.zip          -> 200, application/zip,
#      2,352,373 bytes, no session needed for this step
#
# Why a pin and not a verified on-demand download
# -----------------------------------------------
# The repository prefers an on-demand download verified against a
# published checksum over a pin whenever the publisher gives a stable DOI
# *and* a checksum (whep#457). The second half is missing here, so that
# route is not available:
#
#   - No checksum is published anywhere. The DataCite records carry none,
#     data.europa.eu returns `checksum: null` and `byte_size: null` for
#     the single distribution, and the portal's own resource metadata
#     carries `data_static_size: null`. The server sends an nginx ETag
#     ("68669fd2-23e4f5" = mtime plus size) and a Content-Length, but
#     those are server-side artefacts of one deployment, not published
#     integrity values.
#   - No stable file URL. The distribution path carries the release year
#     (`EU27_CROP_STATS_2025.zip`) and is reachable only through the
#     session-gated resource API, so it changes at every release.
#   - No mirror. data.europa.eu holds a harvested *metadata* record whose
#     only distribution points back at the portal landing page, with no
#     download URL. A Zenodo search for the dataset title returns one
#     unrelated record (CY-Bench, doi:10.5281/zenodo.17279151).
#
# So the pin is justified on exactly that ground, and this script's
# manifest supplies the integrity values the publisher does not.
#
# What the harmonised table is
# ----------------------------
# The published CSV is already long -- one row per region x crop x year x
# variable -- and its six quality flags differ *between* Area, Production
# and Yield within the same region-crop-year (CALCULATED_V alone differs
# in 59,350 of 140,406 groups). Pivoting to one row with area/production/
# yield columns would therefore have to drop or collapse those flags, so
# the long shape is kept and nothing is lost. The identity columns the
# task brief names map onto the shared reader contract as: nuts_code ->
# `source_native_unit_id`, crop_class -> `source_native_item_code`,
# country -> `country_code`, area_ha / production_t / yield_t_ha ->
# `quantity` + `value` + `value_unit`.
#
# Usage:
#   source("inst/scripts/prepare_jrc_subnational.R")
#   prepared <- prepare_jrc_subnational()
#   # then follow the printed steps; nothing is uploaded by this script.
# -----------------------------------------------------------------------

JRC_PORTAL_ROOT <- "https://agri4cast.jrc.ec.europa.eu"

# Resource 39 on the AGRI4CAST DataPortal. The id is the portal's own key
# for "Harmonised sub-national crop statistics of the European Union"; it
# is what the resource API is queried with, and it is stable across
# releases where the file name is not.
JRC_RESOURCE_ID <- 39L

JRC_PIN_ALIAS <- "admin-stats-jrc"

JRC_SOURCE_LABEL <- "JRC_subnational_crops"

# ---- Portal metadata --------------------------------------------------

# The portal is an Angular single-page app: every path returns the same
# shell, and the routes live in a runtime config file. Reading them from
# there rather than hardcoding them means a moved endpoint aborts here
# instead of silently returning the shell HTML.
.jrc_portal_config <- function(root = JRC_PORTAL_ROOT) {
  response <- httr::GET(
    paste0(root, "/assets/env-json-config.json"),
    httr::timeout(60)
  )
  if (httr::http_error(response)) {
    cli::cli_abort(
      "The AGRI4CAST route table answered
       {httr::status_code(response)}."
    )
  }
  httr::content(response, type = "application/json", encoding = "UTF-8")
}

# The resource API refuses anonymous reads until this endpoint has been
# POSTed to; it answers {"status":"ok"} and sets the session cookie that
# httr's handle pool then carries. No credential is sent or stored.
.jrc_open_session <- function(root = JRC_PORTAL_ROOT, config = NULL) {
  config <- config %||% .jrc_portal_config(root)
  path <- config$modules$core$tokenApi
  response <- httr::POST(
    paste0(root, path),
    httr::content_type_json(),
    body = "{}",
    httr::timeout(60)
  )
  if (httr::http_error(response)) {
    cli::cli_abort(c(
      "Could not open an anonymous AGRI4CAST session
       ({httr::status_code(response)}).",
      i = "The resource API answers 401 {.val PGRST302} without it."
    ))
  }
  invisible(response)
}

#' Read the portal's own metadata for the crop-statistics resource.
#'
#' Returns the release identifier, the static distribution's path and the
#' publication date exactly as the portal reports them, so the pin's
#' `source_version` is the publisher's string rather than one parsed out
#' of a file name.
jrc_resource_metadata <- function(root = JRC_PORTAL_ROOT) {
  config <- .jrc_portal_config(root)
  .jrc_open_session(root, config)
  module <- config$modules$resourceDataPortal
  path <- stringr::str_replace(
    module$publicResourcesInfo,
    stringr::fixed("{id}"),
    as.character(JRC_RESOURCE_ID)
  )
  url <- paste0(root, "/", module$rootApi, path)
  response <- httr::GET(url, httr::timeout(180))
  if (httr::http_error(response)) {
    cli::cli_abort(
      "The AGRI4CAST resource API answered
       {httr::status_code(response)} for {.url {url}}."
    )
  }
  tree <- httr::content(response, type = "application/json", encoding = "UTF-8")
  .jrc_metadata_from_tree(tree, root)
}

.jrc_metadata_from_tree <- function(tree, root) {
  static <- tree$resource$data_static
  if (length(static) == 0L) {
    cli::cli_abort(
      "AGRI4CAST resource {JRC_RESOURCE_ID} published no static
       distribution to download."
    )
  }
  enabled <- purrr::keep(static, ~ isTRUE(.x$data_static_enabled))
  if (length(enabled) == 0L) {
    cli::cli_abort(
      "Every static distribution of AGRI4CAST resource
       {JRC_RESOURCE_ID} is disabled."
    )
  }
  releases <- purrr::map_chr(
    tree$events,
    \(event) event$release %||% NA_character_
  )
  releases <- sort(releases[!is.na(releases)])
  if (length(releases) == 0L) {
    cli::cli_abort(
      "AGRI4CAST resource {JRC_RESOURCE_ID} reported no release
       identifier to use as {.field source_version}."
    )
  }
  list(
    resource_id = JRC_RESOURCE_ID,
    label = tree$resource$label %||% NA_character_,
    title = tree$resource$title %||% NA_character_,
    licence = tree$resource$licence %||% NA_character_,
    release = releases[[length(releases)]],
    static_uri = enabled[[1L]]$data_static_uri,
    static_version = enabled[[1L]]$data_static_version %||% NA_character_,
    published = enabled[[1L]]$data_static_published %||% NA_character_,
    url = paste0(root, enabled[[1L]]$data_static_uri)
  )
}

# ---- Download ---------------------------------------------------------

#' Download the published archive and record what came back.
#'
#' Returns the local path together with the exact URL, HTTP status,
#' content type, byte count, server timestamps and the checksums this
#' script computes, because the publisher supplies none.
download_jrc_subnational <- function(
  dest_dir = tempdir(),
  root = JRC_PORTAL_ROOT,
  metadata = NULL
) {
  metadata <- metadata %||% jrc_resource_metadata(root)
  target_dir <- file.path(dest_dir, "JRC_subnational_crops")
  if (!dir.exists(target_dir)) {
    dir.create(target_dir, recursive = TRUE)
  }
  path <- file.path(target_dir, basename(metadata$static_uri))
  response <- httr::GET(
    metadata$url,
    httr::write_disk(path, overwrite = TRUE),
    httr::timeout(1800)
  )
  if (httr::http_error(response)) {
    cli::cli_abort(
      "The AGRI4CAST download answered {httr::status_code(response)}
       for {.url {metadata$url}}."
    )
  }
  headers <- httr::headers(response)
  record <- list(
    path = path,
    url = metadata$url,
    http_status = httr::status_code(response),
    content_type = headers[["content-type"]] %||% NA_character_,
    bytes = as.numeric(unname(file.info(path)$size)),
    last_modified = headers[["last-modified"]] %||% NA_character_,
    etag = headers[["etag"]] %||% NA_character_,
    md5 = unname(tools::md5sum(path)),
    sha256 = unname(tools::sha256sum(path)),
    retrieved_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    metadata = metadata
  )
  cli::cli_alert_success(
    "JRC: {record$bytes} bytes of {.val {record$content_type}} in
     {.file {path}}"
  )
  record
}

# ---- Read -------------------------------------------------------------

# The 13 published fields, documented in the dataset's own
# "Regional_db_Structure_and_Flagging_system.pdf". Everything is read as
# character so the reader, not readr's guesser, decides what a value is.
.jrc_raw_columns <- function() {
  c(
    "REGION",
    "CROP_NAME",
    "YEAR",
    "VARIABLE",
    "VALUE",
    "UoM",
    "SOURCE",
    "CALCULATED_R",
    "CALCULATED_C",
    "CALCULATED_V",
    "ZERO_AS_NULL",
    "COHERENCE_APY",
    "COHERENCE_CROP"
  )
}

#' Read the published archive verbatim.
#'
#' Accepts the downloaded `.zip` or the CSV it contains, and returns the
#' publisher's 13 columns unchanged, as character.
read_jrc_subnational <- function(path) {
  csv_path <- if (tools::file_ext(path) == "zip") {
    .jrc_unzip_csv(path)
  } else {
    path
  }
  raw <- readr::read_csv(
    csv_path,
    col_types = readr::cols(.default = readr::col_character()),
    progress = FALSE
  )
  missing <- setdiff(.jrc_raw_columns(), names(raw))
  if (length(missing) > 0L) {
    cli::cli_abort(
      "The JRC file is missing column{?s} {.field {missing}}."
    )
  }
  dplyr::select(raw, dplyr::all_of(.jrc_raw_columns()))
}

.jrc_unzip_csv <- function(path) {
  members <- utils::unzip(path, list = TRUE)$Name
  wanted <- members[stringr::str_detect(members, "(?i)\\.csv$")]
  if (length(wanted) != 1L) {
    cli::cli_abort(c(
      "Expected exactly one CSV in {.file {path}}.",
      i = "It holds {length(members)} member{?s}: {.file {members}}."
    ))
  }
  exdir <- file.path(tempdir(), "jrc_subnational_unzip")
  utils::unzip(path, files = wanted, exdir = exdir, overwrite = TRUE)
  file.path(exdir, wanted)
}

# ---- Harmonise --------------------------------------------------------

# The three published variables, with the admin-shares indicator each
# anchors to and the unit the publisher states for it ("UoM" on every
# row, and Table 1 of the structure document). Yield is kept, unlike in
# the Eurostat reader which drops a yield that is exactly production over
# area: the JRC yield is recomputed under the NUTS-2016 area weighting
# described in "Summary_of_algorithm_for_disaggregation.pdf", which is
# why the dataset ships a COHERENCE_APY flag at all -- 4,428 rows of the
# 2025.01 release fail abs(P - A*Y) <= 0.01*P.
.jrc_variable_map <- function() {
  tibble::tribble(
    ~VARIABLE,     ~quantity,     ~indicator_used,  ~value_unit,
    "Area",        "area",        "area_harvested", "ha",
    "Production",  "production",  "production",     "t",
    "Yield",       "yield",       "yield",          "t/ha"
  )
}

# Eurostat's pre-2025 area concept is "the area actually harvested"
# (ESMS apro_cp_esms 3.4), and this dataset's crop names are "as in the
# Eurostat definition" with National Statistical Institutes, Eurostat or
# a mix of the two as the value's origin, so Area maps to
# `area_harvested`. Rows whose origin is NSI inherit that institute's own
# area concept; `statistic_origin` keeps which one each row came from.
.jrc_concept_break_year <- function() 2025L

# NUTS 2 is admin1 and NUTS 3 is admin2, the same mapping
# `.eurostat_grain()` uses in R/admin_stats_eurostat.R. NUTS 0 and NUTS 1
# get no grain: NUTS 1 is a first-order division in some countries and an
# aggregate of admin1 units in others, and NUTS 0 is the container.
.jrc_grain <- function(nuts_level) {
  dplyr::case_when(
    nuts_level == 2L ~ "admin1",
    nuts_level == 3L ~ "admin2",
    .default = NA_character_
  )
}

.jrc_is_nuts_code <- function(code) {
  stringr::str_detect(code, "^[A-Z]{2}[A-Z0-9]{0,3}$")
}

#' Harmonise the published rows onto the admin-statistics reader contract.
#'
#' `source_version` is the publisher's release string, e.g. `"2025.01"`;
#' it is not derived here because a file name is not a version.
harmonize_jrc_subnational <- function(
  raw,
  source_version,
  recorded_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
) {
  if (!rlang::is_string(source_version)) {
    cli::cli_abort(
      "{.arg source_version} must be the publisher's release string."
    )
  }
  raw |>
    .jrc_check_key() |>
    .jrc_apply_variables() |>
    .jrc_add_geography() |>
    .jrc_add_values(source_version, recorded_at) |>
    .jrc_select_output()
}

.jrc_check_key <- function(raw) {
  key <- c("REGION", "CROP_NAME", "YEAR", "VARIABLE")
  duplicated_rows <- raw |>
    dplyr::count(dplyr::across(dplyr::all_of(key))) |>
    dplyr::filter(.data$n > 1L)
  if (nrow(duplicated_rows) > 0L) {
    cli::cli_abort(
      "The JRC file repeats {nrow(duplicated_rows)}
       region-crop-year-variable key{?s}, so no row is uniquely
       identified."
    )
  }
  raw
}

.jrc_apply_variables <- function(raw) {
  known <- .jrc_variable_map()
  unknown <- setdiff(unique(raw$VARIABLE), known$VARIABLE)
  if (length(unknown) > 0L) {
    cli::cli_abort(c(
      "The JRC file carries unknown variable{?s} {.val {unknown}}.",
      i = "Known: {.val {known$VARIABLE}}."
    ))
  }
  joined <- dplyr::inner_join(raw, known, by = "VARIABLE")
  mismatched <- dplyr::filter(joined, .data$UoM != .data$value_unit)
  if (nrow(mismatched) > 0L) {
    cli::cli_abort(c(
      "{nrow(mismatched)} row{?s} carry a unit this reader does not
       expect.",
      i = "Seen: {.val {unique(mismatched$UoM)}}."
    ))
  }
  joined
}

.jrc_add_geography <- function(rows) {
  out <- rows |>
    dplyr::mutate(
      is_nuts = .jrc_is_nuts_code(.data$REGION),
      nuts_level = dplyr::if_else(
        .data$is_nuts,
        nchar(.data$REGION) - 2L,
        NA_integer_
      ),
      country_code = dplyr::if_else(
        .data$is_nuts,
        stringr::str_sub(.data$REGION, 1L, 2L),
        NA_character_
      ),
      grain = .jrc_grain(.data$nuts_level)
    )
  .jrc_report_bad_codes(out)
}

# The 2025.01 release ships three region codes that Excel turned into
# dates ("2-Dec", "4-Dec", "5-Dec", four rows in total, all Soft wheat
# yields for 2021-2022). They are kept, flagged and counted rather than
# repaired: what the original codes were is not recoverable from the
# file, and guessing one would invent a territory.
.jrc_report_bad_codes <- function(rows) {
  bad <- dplyr::filter(rows, !.data$is_nuts)
  if (nrow(bad) > 0L) {
    codes <- sort(unique(bad$REGION))
    cli::cli_warn(c(
      "{nrow(bad)} row{?s} carry a region code that is not NUTS-shaped.",
      i = "Code{?s}: {.val {codes}}.",
      i = "Kept with {.field value_flag} {.val invalid_region_code} and
           no {.field nuts_level}."
    ))
  }
  rows
}

# A published value that is neither a number nor blank aborts rather
# than coercing to NA: as.numeric() would turn it into a missing
# observation that looks exactly like a withheld one. The 2025.01
# release has none, but a later one introducing a thousands separator or
# a suppression code must be seen, not absorbed.
.jrc_check_numeric <- function(rows) {
  present <- !is.na(rows$VALUE) & nzchar(rows$VALUE)
  unparsed <- present & is.na(suppressWarnings(as.numeric(rows$VALUE)))
  if (any(unparsed)) {
    offending <- sort(unique(rows$VALUE[unparsed]))
    cli::cli_abort(c(
      "{sum(unparsed)} published value{?s} {?is/are} not numeric.",
      i = "Seen: {.val {utils::head(offending, 10L)}}."
    ))
  }
  rows
}

.jrc_add_values <- function(rows, source_version, recorded_at) {
  rows |>
    .jrc_check_numeric() |>
    dplyr::mutate(
      value = as.numeric(.data$VALUE),
      statistic_origin = .data$SOURCE,
      calculated_region = .jrc_yes(.data$CALCULATED_R),
      calculated_crop = .jrc_yes(.data$CALCULATED_C),
      calculated_value = .jrc_yes(.data$CALCULATED_V),
      zero_as_null = .jrc_yes(.data$ZERO_AS_NULL),
      coherence_apy = .jrc_coherence(.data$COHERENCE_APY),
      coherence_crop = .jrc_coherence(.data$COHERENCE_CROP),
      source = JRC_SOURCE_LABEL,
      source_native_unit_id = .data$REGION,
      source_native_unit_name = NA_character_,
      source_native_item_code = .data$CROP_NAME,
      source_native_item_name = .data$CROP_NAME,
      year = as.integer(.data$YEAR),
      nuts_version = "2016",
      concept_break = .data$year >= .jrc_concept_break_year(),
      source_version = source_version,
      recorded_at = recorded_at
    ) |>
    dplyr::mutate(
      value_flag = .jrc_join_tokens(
        .jrc_token(.data$calculated_value, "calculated_value"),
        .jrc_token(.data$calculated_region, "calculated_region"),
        .jrc_token(.data$calculated_crop, "calculated_crop"),
        .jrc_token(.data$zero_as_null, "zero_as_null"),
        .jrc_token(
          !is.na(.data$coherence_apy) & .data$coherence_apy == "no",
          "coherence_apy_failed"
        ),
        .jrc_token(
          !is.na(.data$coherence_crop) & .data$coherence_crop == "no",
          "coherence_crop_failed"
        ),
        .jrc_token(!.data$is_nuts, "invalid_region_code"),
        .jrc_token(is.na(.data$value), "missing_value")
      )
    )
}

# The flags are "Yes" or blank, per the structure document.
.jrc_yes <- function(x) {
  !is.na(x) & x == "Yes"
}

# The two coherence fields are the exception: "Yes", "No", or blank when
# a missing value made the check impossible.
.jrc_coherence <- function(x) {
  dplyr::case_when(
    !is.na(x) & x == "Yes" ~ "yes",
    !is.na(x) & x == "No" ~ "no",
    .default = NA_character_
  )
}

# `value_flag` is a one-column summary of everything exceptional about
# the row, in the same "|"-joined shape `.eurostat_value_flag()` uses.
# The typed columns beside it carry the full detail; this is what a
# consumer scanning one column across readers sees.
.jrc_token <- function(condition, token) {
  dplyr::if_else(condition, token, NA_character_)
}

.jrc_join_tokens <- function(...) {
  Reduce(
    \(acc, token) {
      dplyr::case_when(
        is.na(token) ~ acc,
        is.na(acc) ~ token,
        .default = paste(acc, token, sep = "|")
      )
    },
    list(...)
  )
}

.jrc_select_output <- function(rows) {
  rows |>
    dplyr::select(
      "source",
      "source_native_unit_id",
      "source_native_unit_name",
      "source_native_item_code",
      "source_native_item_name",
      "quantity",
      "indicator_used",
      "year",
      "value",
      "value_unit",
      "value_flag",
      "concept_break",
      "grain",
      "nuts_level",
      "nuts_version",
      "source_version",
      "recorded_at",
      "country_code",
      "statistic_origin",
      "calculated_region",
      "calculated_crop",
      "calculated_value",
      "zero_as_null",
      "coherence_apy",
      "coherence_crop"
    ) |>
    dplyr::arrange(
      .data$source_native_unit_id,
      .data$source_native_item_code,
      .data$year,
      .data$quantity
    )
}

# ---- Manifest ---------------------------------------------------------

JRC_MANIFEST_PATH <- file.path(
  "inst",
  "extdata",
  "jrc_subnational_source_manifest.csv"
)

# Attribution, rights and identifiers, all read off the publisher's own
# records on 2026-09-02 and carried per file so a manifest row is
# self-contained.
.jrc_attribution <- function() {
  list(
    attribution = paste(
      "Ronchetti, G., Nisini Scacchiafichi, L., Seguini, L., Cerrani, I.",
      "and van der Velde, M. (2023): Harmonized European Union",
      "subnational crop statistics. European Commission, Joint Research",
      "Centre (JRC)."
    ),
    doi = "10.2905/685949ff-56de-4646-a8df-844b5bb5f835",
    catalogue_doi = "10.2905/jrc.g3w5s7r",
    paper_doi = "10.5194/essd-16-1623-2024",
    licence = paste(
      "European Commission reuse notice (Commission Decision",
      "2011/833/EU of 12 December 2011): reuse is authorised, provided",
      "the source is acknowledged."
    )
  )
}

# The support files are manifested beside the data because they are what
# the harmoniser's flag semantics and NUTS-2016 reference are read from;
# a silent revision of either would change what the columns mean.
.jrc_support_files <- function() {
  paste0(
    "/download/dataportal_resources/support_files/",
    JRC_RESOURCE_ID,
    "/",
    c(
      "Regional_db_Structure_and_Flagging_system.pdf",
      "Summary_of_algorithm_for_disaggregation.pdf",
      "Mapping_eurostat_legend.xlsx",
      "Country_Fiches.zip"
    )
  )
}

#' Write the content manifest for one downloaded archive.
#'
#' One row per file, carrying attribution, both DOIs, the licence, the
#' retrieval date and the checksums this script computed, since the
#' publisher publishes none.
write_jrc_source_manifest <- function(
  record,
  path = JRC_MANIFEST_PATH,
  root = JRC_PORTAL_ROOT,
  support_dir = NULL
) {
  meta <- .jrc_attribution()
  rows <- .jrc_manifest_rows(record, root, support_dir)
  out <- rows |>
    dplyr::mutate(
      dataset_release = record$metadata$release,
      dataset_static_version = record$metadata$static_version,
      nuts_version = "2016",
      attribution = meta$attribution,
      doi = meta$doi,
      catalogue_doi = meta$catalogue_doi,
      paper_doi = meta$paper_doi,
      licence = meta$licence
    )
  readr::write_csv(out, path)
  cli::cli_alert_success("JRC: manifest at {.file {path}}")
  out
}

.jrc_manifest_rows <- function(record, root, support_dir) {
  data_row <- tibble::tibble(
    relative_path = basename(record$path),
    source_url = record$url,
    bytes = record$bytes,
    md5 = record$md5,
    sha256 = record$sha256,
    last_modified = record$last_modified,
    retrieved_at = record$retrieved_at,
    role = "data"
  )
  support <- .jrc_download_support(root, support_dir, record$retrieved_at)
  dplyr::bind_rows(data_row, support)
}

.jrc_download_support <- function(root, support_dir, retrieved_at) {
  support_dir <- support_dir %||% file.path(tempdir(), "jrc_support")
  if (!dir.exists(support_dir)) {
    dir.create(support_dir, recursive = TRUE)
  }
  purrr::map(
    .jrc_support_files(),
    \(uri) .jrc_support_row(root, uri, support_dir, retrieved_at)
  ) |>
    purrr::list_rbind()
}

.jrc_support_row <- function(root, uri, support_dir, retrieved_at) {
  path <- file.path(support_dir, basename(uri))
  response <- httr::GET(
    paste0(root, uri),
    httr::write_disk(path, overwrite = TRUE),
    httr::timeout(600)
  )
  if (httr::http_error(response)) {
    cli::cli_abort(
      "The JRC support file {.file {basename(uri)}} answered
       {httr::status_code(response)}."
    )
  }
  tibble::tibble(
    relative_path = basename(uri),
    source_url = paste0(root, uri),
    bytes = as.numeric(unname(file.info(path)$size)),
    md5 = unname(tools::md5sum(path)),
    sha256 = unname(tools::sha256sum(path)),
    last_modified = httr::headers(response)[["last-modified"]] %||%
      NA_character_,
    retrieved_at = retrieved_at,
    role = "support"
  )
}

# ---- Stage the pin ----------------------------------------------------

# The same two files `create_version()` in prepare_upload.R writes, in
# the same `<alias>/<version>/` layout, staged locally. Nothing is
# uploaded: registering the pin (the _pins.yaml section and the
# whep_inputs.csv row) is a separate, deliberate step.
stage_jrc_pin <- function(
  harmonized,
  staging_dir = file.path("inst", "scripts", "pin_upload"),
  alias = JRC_PIN_ALIAS,
  version = NULL
) {
  version <- version %||% format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")
  target <- file.path(staging_dir, alias, version)
  dir.create(target, recursive = TRUE, showWarnings = FALSE)
  paths <- file.path(target, paste0(alias, c(".csv", ".parquet")))
  readr::write_csv(harmonized, paths[[1L]])
  nanoparquet::write_parquet(harmonized, paths[[2L]])
  cli::cli_alert_success("JRC: pin folder staged at {.file {target}}")
  cli::cli_alert_info(c(
    "Not uploaded. To publish it: upload {.file {target}} to the board,
     add {.val {paste0(alias, '/', version, '/')}} under the
     {.val {alias}} section of {.file _pins.yaml}, then add the
     {.val {alias}} row to {.file inst/extdata/whep_inputs.csv} and
     rebuild with {.file data-raw/whep_inputs.R}."
  ))
  list(dir = target, version = version, paths = paths)
}

# ---- Entry point ------------------------------------------------------

#' Download, harmonise, manifest and stage the JRC pin. Uploads nothing.
prepare_jrc_subnational <- function(
  dest_dir = tempdir(),
  staging_dir = file.path("inst", "scripts", "pin_upload"),
  manifest_path = JRC_MANIFEST_PATH,
  root = JRC_PORTAL_ROOT
) {
  record <- download_jrc_subnational(dest_dir, root)
  harmonized <- record$path |>
    read_jrc_subnational() |>
    harmonize_jrc_subnational(record$metadata$release)
  cli::cli_alert_info(
    "JRC: {nrow(harmonized)} row{?s},
     {dplyr::n_distinct(harmonized$source_native_unit_id)} region{?s},
     {dplyr::n_distinct(harmonized$source_native_item_code)} crop
     class{?es}, {min(harmonized$year)}-{max(harmonized$year)}."
  )
  manifest <- write_jrc_source_manifest(record, manifest_path, root)
  staged <- stage_jrc_pin(harmonized, staging_dir)
  list(
    record = record,
    data = harmonized,
    manifest = manifest,
    staged = staged
  )
}
