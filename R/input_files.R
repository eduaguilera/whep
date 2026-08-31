#' Download, cache and read files
#'
#' @description
#' Used to fetch input files that are needed for the package's functions
#' and that were built in external sources and are too large to include
#' directly. This is a public function for transparency purposes, so that
#' users can inspect the original inputs of this package that were not
#' directly processed here.
#'
#' If the requested file doesn't exist locally, it is downloaded from a public
#' link and cached before reading it. This is all implemented using the
#' [`pins`](https://pins.rstudio.com/index.html) package. It supports multiple
#' file formats and file versioning.
#'
#' @param file_alias Internal name of the requested file. You can find the
#'   possible values in the `alias` column of the [`whep_inputs`] dataset.
#' @param type The extension of the file that must be read. Possible values:
#'   - `parquet`: This is the default value for code efficiency reasons.
#'   - `csv`: Mainly available for those who want a more human-readable option.
#'     If the `parquet` version is available, this is useless because this
#'     function already returns the dataset in an `R` object, so the origin is
#'     irrelevant, and `parquet` is read faster.
#'
#'   - `nc` / `nc4`: Returns the path to the downloaded NetCDF instead of its
#'     contents, because these grids are read lazily by `ncdf4`/`terra` and are
#'     far too large to materialise as a tibble.
#'   - `raw`: Returns the file path without any processing.
#'
#'   Saving each file in both formats is for transparency and accessibility
#'   purposes, e.g., having to share the data with non-programmers who can
#'   easily import a CSV into a spreadsheet. You will most likely never have
#'   to set this option manually unless for some reason a file could not be
#'   supplied in e.g. `parquet` format but was in another one.
#' @param version The version of the file that must be read. Possible values:
#'   - `NULL`: This is the default value. A frozen version is chosen to make
#'     the code reproducible when the file has a registry version. Each release
#'     will have its own frozen versions. The version is the string that can be
#'     found in [`whep_inputs`] in the `version` column. A blank registry version
#'     requests the latest board version.
#'   - `"latest"`: This overrides the frozen version and instead fetches the
#'     latest one that is available. This might or might not match the frozen
#'     version.
#'   - Other: A specific version can also be used. For more details read the
#'     `version` column information from [`whep_inputs`].
#'
#' @returns A tibble with the dataset. Some information about each dataset can
#'   be found in the code where it's used as input for further processing.
#'
#' @export
#'
#' @examples
#' whep_read_file("read_example")
#' whep_read_file("read_example", type = "parquet", version = "latest")
#' whep_read_file(
#'   "read_example",
#'   type = "csv",
#'   version = "20250721T152646Z-ce61b"
#' )
whep_read_file <- function(file_alias, type = "parquet", version = NULL) {
  cli::cli_alert_info("Fetching files for {file_alias}...")

  file_info <- .fetch_file_info(file_alias, whep::whep_inputs)
  version <- .choose_version(file_info$version, version)

  paths <- tryCatch(
    .get_local_board() |>
      pins::pin_download(file_alias, version = version),
    error = function(e) {
      tryCatch(
        file_info |>
          .get_remote_board() |>
          pins::pin_download(file_alias, version = version),
        error = function(e) {
          .get_cache_paths(file_info, file_alias, version, e)
        }
      )
    }
  )

  paths |>
    .read_file(type)
}

#' Input file versions
#'
#' @description
#' Lists all existing versions of an input file from [`whep_inputs`].
#'
#' @param file_alias Internal name of the requested file. You can find the
#'   possible values in the [`whep_inputs`] dataset.
#'
#' @returns A tibble where each row is a version. For details about its format,
#'   see `pins::pin_versions()`.
#'
#' @export
#'
#' @examples
#' whep_list_file_versions("read_example")
whep_list_file_versions <- function(file_alias) {
  board <- if (file_alias == "read_example") {
    .get_local_board()
  } else {
    file_alias |>
      .fetch_file_info(whep::whep_inputs) |>
      .get_remote_board()
  }

  board |>
    pins::pin_versions(file_alias)
}

.read_file <- function(paths, extension) {
  path <- purrr::detect(paths, ~ stringr::str_ends(.x, extension))

  # Only for formats this function knows how to read: an unrecognised
  # `extension` must still fall through to the "unknown file type" error. "nc"
  # and "nc4" belong here even though they are handed back as paths: without
  # them a pin with no NetCDF member returned NULL, so the caller failed later
  # and somewhere else instead of being told which formats the pin does have.
  known <- c("csv", "parquet", "tar.gz", "tgz", "raw", "nc", "nc4")

  if (is.null(path) && extension %in% known) {
    # data.txt / _pins.yaml are pins bookkeeping, not readable inputs.
    available <- paths |>
      purrr::map_chr(fs::path_ext) |>
      unique() |>
      setdiff(c("", "txt", "yaml"))
    cli::cli_abort(c(
      "This input has no {.val {extension}} file.",
      i = "Available format{?s}: {.val {available}}.",
      i = "Pass {.code type = } to pick one, e.g.
           {.code whep_read_file(alias, type = \"{available[1]}\")}."
    ))
  }

  if (extension == "csv") {
    readr::read_csv(path, show_col_types = FALSE)
  } else if (extension == "parquet") {
    path |>
      nanoparquet::read_parquet() |>
      tibble::as_tibble()
  } else if (extension %in% c("tar.gz", "tgz")) {
    # Decompress archive and return paths to extracted files
    tmpdir <- file.path(tempdir(), basename(tempfile()))
    dir.create(tmpdir, recursive = TRUE)
    utils::untar(path, exdir = tmpdir)
    list.files(tmpdir, full.names = TRUE, recursive = TRUE)
  } else if (extension %in% c("nc", "nc4")) {
    # NetCDF is read lazily by the caller (ncdf4/terra open by path, and these
    # grids are far too large to materialise as a tibble), so hand back the
    # path rather than the contents.
    path
  } else if (extension == "raw") {
    # Return the raw file path without processing
    path
  } else {
    extensions <- purrr::map(paths, fs::path_ext)
    cli::cli_abort(
      "Unknown file type {extension}. Available for this file: {extensions}"
    )
  }
}

.get_remote_board <- function(file_info) {
  board_url <- purrr::pluck(file_info, "board_url")
  .check_remote_reachable(board_url)

  board_url |>
    .build_board_with_progress()
}

.get_local_board <- function() {
  system.file("extdata", "examples", package = "whep") |>
    pins::board_folder()
}

.check_remote_reachable <- function(board_url) {
  url <- board_url[[1]]
  host <- httr::parse_url(url)$hostname

  response <- tryCatch(
    httr::HEAD(
      paste0("https://", host),
      httr::timeout(5)
    ),
    error = function(e) NULL
  )

  if (is.null(response)) {
    cli::cli_abort(
      "Remote host {.val {host}} is not reachable."
    )
  }

  invisible(NULL)
}

.get_cache_paths <- function(
  file_info,
  file_alias,
  version,
  original_error
) {
  cache_dir <- .find_cache_dir(file_info, file_alias, version)

  if (is.null(cache_dir)) {
    cli::cli_abort(
      c(
        "Could not fetch {.val {file_alias}} from remote source.",
        "x" = "No local cached copy was found either.",
        "i" = "Connect to the internet and try again.",
        "Caused by" = conditionMessage(original_error)
      )
    )
  }

  cli::cli_warn(
    c(
      "Could not reach remote data source.",
      "i" = "Using cached local copy of {.val {file_alias}}."
    )
  )

  cache_dir |>
    list.files(full.names = TRUE)
}

.find_cache_dir <- function(file_info, file_alias, version) {
  pin_url <- file_info |>
    purrr::pluck("board_url") |>
    stringr::str_replace("_pins\\.yaml$", "") |>
    paste0(file_alias, "/")

  # `version` arrives as NULL whenever the caller asked for `"latest"` or the
  # registry froze no version, and pasting NULL into the URL hashed
  # `.../alias//`, which never matches the directory the download wrote (#245).
  # The board that would resolve "latest" to a concrete version is exactly what
  # is unreachable on this path, so resolve it from the cache instead.
  version <- version %||% .latest_cached_version(pin_url)

  if (is.null(version)) {
    return(NULL)
  }

  cache_path <- fs::path(
    .pins_cache_base(),
    "url",
    rlang::hash(paste0(pin_url, version, "/"))
  )

  if (fs::dir_exists(cache_path)) cache_path else NULL
}

# Newest cached version of `pin_url`, or NULL if none is cached. pins version
# strings start with a UTC timestamp, so they sort chronologically.
.latest_cached_version <- function(pin_url) {
  cache_root <- fs::path(.pins_cache_base(), "url")

  if (!fs::dir_exists(cache_root)) {
    return(NULL)
  }

  versions <- cache_root |>
    fs::dir_ls(type = "directory") |>
    purrr::map_chr(.cached_version_of, pin_url = pin_url) |>
    purrr::discard(is.na)

  if (length(versions) == 0L) NULL else max(versions)
}

# The version one cache directory holds, or NA if it is not a version of
# `pin_url`. `data.txt` is the pin metadata pins downloads before the data
# itself, and a pins version is `<created>-<first 5 of pin_hash>`. Re-hashing
# the resulting URL and comparing it with the directory name is what attributes
# the directory to this pin, so an unrelated pin can never be mistaken for one.
.cached_version_of <- function(cache_dir, pin_url) {
  meta_path <- fs::path(cache_dir, "data.txt")

  if (!fs::file_exists(meta_path)) {
    return(NA_character_)
  }

  # An interrupted download can leave a truncated `data.txt` behind, and this
  # runs while the remote is already unreachable: an unparseable neighbour must
  # not turn "cache found" into a crash.
  meta <- tryCatch(
    yaml::read_yaml(meta_path, eval.expr = FALSE),
    error = function(e) NULL
  )

  if (!all(rlang::has_name(meta, c("created", "pin_hash")))) {
    return(NA_character_)
  }

  version <- paste0(
    meta$created,
    "-",
    stringr::str_sub(meta$pin_hash, 1L, 5L)
  )
  version_hash <- rlang::hash(paste0(pin_url, version, "/"))

  if (fs::path_file(cache_dir) == version_hash) version else NA_character_
}

# Must resolve to the same directory `pins` writes to, which honours these
# environment variables (see `pins:::board_cache_path()`).
.pins_cache_base <- function() {
  if (.has_env_vars(c("R_CONFIG_ACTIVE", "PINS_USE_CACHE"))) {
    fs::path(tempdir(), "pins")
  } else if (.has_env_vars("PINS_CACHE_DIR")) {
    Sys.getenv("PINS_CACHE_DIR")
  } else {
    rappdirs::user_cache_dir("pins")
  }
}

.has_env_vars <- function(var_names) {
  any(nzchar(Sys.getenv(var_names)))
}

.choose_version <- function(frozen_version, user_version) {
  if (is.null(user_version)) {
    if (
      length(frozen_version) == 0L ||
        is.na(frozen_version) ||
        !nzchar(frozen_version) ||
        identical(frozen_version, "latest")
    ) {
      NULL
    } else {
      frozen_version
    }
  } else if (user_version == "latest") {
    NULL
  } else {
    user_version
  }
}

.fetch_file_info <- function(file_alias, input_files) {
  file_info <- input_files |>
    dplyr::filter(alias == file_alias)

  if (nrow(file_info) == 0) {
    cli::cli_abort("There is no file entry with alias {file_alias}")
  }
  if (nrow(file_info) > 1) {
    cli::cli_abort(
      paste0(
        "There are {nrow(file_info)} file entries with alias {file_alias} ",
        "and there should be only one. Double check the content of ",
        "'whep_inputs' dataset."
      )
    )
  }

  c(file_info)
}

.build_board_with_progress <- function(board_url) {
  board <- pins::board_url(board_url)
  # Make our own pin_fetch method to include progress bar
  # https://github.com/rstudio/pins-r/issues/873
  class(board) <- c("pins_with_progress", class(board))

  board
}

#' @importFrom pins pin_fetch
#' @method pin_fetch pins_with_progress
#' @export
#' @noRd
pin_fetch.pins_with_progress <- function(
  board,
  name,
  version = NULL,
  ...
) {
  meta <- pins::pin_meta(board, name, version = version)
  .pins_cache_touch(board, meta)

  purrr::pmap_chr(
    list(
      meta$local$file_url,
      meta$file,
      meta$file_size
    ),
    function(url, file, size) {
      .pins_http_download(
        url = url,
        path_dir = meta$local$dir,
        path_file = file,
        use_cache_on_failure = board$use_cache_on_failure,
        headers = board$headers,
        .pins_http_utils_progress(size = size)
      )
    }
  )

  meta
}
