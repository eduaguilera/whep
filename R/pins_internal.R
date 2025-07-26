# The code in this file is copied from https://github.com/rstudio/pins-r,
# mainly from file https://github.com/rstudio/pins-r/blob/main/R/board_url.R,
# as a temp workaround because of https://github.com/rstudio/pins-r/issues/873.
# Original copyright 2023, Posit, PBC
# Licensed under the Apache License, Version 2.0

.pins_http_download <- function(url, path_dir, path_file, ...,
                                use_cache_on_failure = FALSE,
                                headers = NULL,
                                on_failure = NULL) {
  cache_path <- fs::path(path_dir, "download-cache.yaml")
  cache <- .pins_read_cache(cache_path)[[url]]

  if (!is.null(cache)) {
    if (!.pins_has_expired(cache$expires)) {
      rlang::signal("", "pins_cache_cached")
      return(cache$path)
    }

    headers <- c(
      headers,
      `If-Modified-Since` = .pins_http_date(cache$modified),
      `If-None-Match` = cache$etag
    )
  }

  path <- fs::path(path_dir, path_file)
  # only want to replace existing cache path if request is successful
  tmp_path <- tempfile()
  write_out <- httr::write_disk(tmp_path)

  req <- tryCatch(
    httr::GET(url, httr::add_headers(headers), ..., write_out),
    error = function(e) {
      if (!is.null(cache) && use_cache_on_failure) {
        NULL
      } else {
        stop(e)
      }
    }
  )

  if (is.null(req)) {
    cli::cli_warn(
      "Downloading '{path_file}' failed; falling back to cached version"
    )
    cache$path
  } else if (httr::status_code(req) <= 200) {
    rlang::signal("", "pins_cache_downloaded")
    if (fs::file_exists(path)) fs::file_chmod(path, "u+w")
    fs::file_copy(tmp_path, path, overwrite = TRUE)
    fs::file_chmod(path, "u=r")

    info <- httr::cache_info(req)
    if (info$cacheable) {
      .pins_update_cache(cache_path, url, list(
        expires = info$expires,
        etag = info$etag,
        modified = unclass(info$modified),
        path = path
      ))
    } else {
      cli::cli_alert("{.url {url}} is not cacheable")
    }

    path
  } else if (httr::status_code(req) == 304) {
    rlang::signal("", "pins_cache_not_modified")
    cache$path
  } else {
    if (!is.null(cache) && use_cache_on_failure) {
      cli::cli_warn(
        "Downloading '{path_file}' failed; falling back to cached version"
      )
      httr::warn_for_status(req)
      cache$path
    } else {
      if (is.null(on_failure)) {
        httr::stop_for_status(req)
      } else {
        on_failure(req)
      }
    }
  }
}

.pins_read_cache <- function(path) {
  if (file.exists(path)) {
    yaml::read_yaml(path, eval.expr = FALSE)
  } else {
    list()
  }
}

.pins_update_cache <- function(path, key, value) {
  cache <- .pins_read_cache(path)
  cache[[key]] <- value
  .pins_write_yaml(cache, path)

  value
}

.pins_write_yaml <- function(x, path) {
  x <- .pins_to_utf8(x)
  yaml::write_yaml(x, path)
}

# On Windows, yaml::write_yaml() crashes with Latin1 data
# https://github.com/viking/r-yaml/issues/90
.pins_to_utf8 <- function(x) {
  if (is.list(x)) {
    if (!is.null(names(x))) {
      names(x) <- enc2utf8(names(x))
    }
    lapply(x, .pins_to_utf8)
  } else if (is.character(x)) {
    enc2utf8(x)
  } else {
    x
  }
}

.pins_has_expired <- function(x) {
  if (is.null(x)) {
    TRUE
  } else {
    unclass(Sys.time()) > x
  }
}

.pins_http_date <- function(x = Sys.time(), tz = "UTC") {
  if (is.null(x)) {
    return(NULL)
  }

  withr::local_locale(LC_TIME = "C")
  strftime(.POSIXct(x), "%a, %d %b %Y %H:%M:%S", tz = tz, usetz = TRUE)
}

.pins_cache_touch <- function(board, meta, time = Sys.time()) {
  path <- fs::path(meta$local$dir, "data.txt")
  if (fs::file_exists(path)) {
    fs::file_chmod(path, "u+w")
    fs::file_touch(path, access_time = time)
    fs::file_chmod(path, "u=r")
  } else {
    fs::file_create(path)
  }
}

.pins_http_utils_progress <- function(type = "down", size = 0) {
  if (.pins_show_progress(size = size)) {
    httr::progress(type = type)
  } else {
    NULL
  }
}

.pins_show_progress <- function(size = 0) {
  if (is.character(size)) size <- as.integer(size)

  large_file <- getOption("pins.progress.size", 10^7)
  identical(getOption("pins.progress", size > large_file), TRUE) &&
    interactive()
}
