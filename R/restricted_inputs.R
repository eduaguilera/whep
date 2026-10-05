#' Check access to restricted input datasets
#'
#' @description
#' Most inputs in [`whep_inputs`] are read from a public board. Some cannot be
#' public: licensed or confidential data, or data shared with the project
#' under an agreement. Those rows have `access == "restricted"` and are read
#' from a private board that each authorised person reaches with their own
#' credentials, so nobody shares a common secret (#1386).
#'
#' This function lists the restricted inputs registered in [`whep_inputs`]
#' and whether each is usable on this machine. It never returns or prints the
#' secret, the login or the full board address.
#'
#' @section Restricted access:
#' WHEP uses public data unless the caller explicitly chooses a restricted
#' option, and it never switches between the two on its own. A restricted
#' option is chosen in one of two ways:
#'
#' - by reading a restricted alias of [`whep_inputs`] with
#'   [whep_read_file()];
#' - by picking a `method =` or `source =` value that is labelled
#'   **restricted access** in the function's documentation. The default of
#'   such an argument is always a public option.
#'
#' Choosing a restricted option without working access aborts with an error
#' of class `whep_restricted_unavailable` (fields `input` and `reason`). It
#' says why access failed (`not_configured`, `unauthorised`, `unreachable` or
#' `missing`, when the board holds no such pin), how to set access up, and
#' which public input the registry's `public_alternative` column suggests
#' instead. That suggestion is advice only: nothing is read in its place.
#'
#' Data read from the restricted board carries a `data_access` column equal
#' to `"restricted"` (or, for a file path, a `data_access` attribute), so its
#' provenance stays visible downstream. Public inputs are returned unchanged.
#'
#' ## Configuring access
#'
#' Set these environment variables in `~/.Renviron` (not in a project
#' `.Renviron`, which would hide every other `WHEP_*` path):
#'
#' - `WHEP_RESTRICTED_BOARD`: the address of your copy of the restricted
#'   folder. Either your own WebDAV path,
#'   `https://saco.csic.es/remote.php/dav/files/<login>/<folder>/` (saco >
#'   Files > Settings shows the WebDAV address), or the share link you were
#'   sent, `https://saco.csic.es/s/<token>`, which is converted to its WebDAV
#'   form `https://saco.csic.es/public.php/dav/files/<token>/`.
#' - `WHEP_RESTRICTED_SECRET`: for your own path, an app password (saco >
#'   Personal settings > Security > Create new app password); for a share
#'   link, the share's password.
#' - `WHEP_RESTRICTED_USER` (optional): your login. It is taken from the
#'   WebDAV path when unset, and is `anonymous` for a share link.
#'
#' Requests carry `Authorization: Basic` and `X-Requested-With:
#' XMLHttpRequest`; Nextcloud refuses share-link WebDAV without the latter.
#' Credentials are read from the environment at each request and never
#' stored in any returned object or message.
#'
#' ## Republishing restricted data
#'
#' Data read from the restricted board also carries the attribute
#' `whep_access = "restricted"`. Pin upload code calls
#' [whep_assert_publishable()] before writing, which refuses data carrying
#' either mark. The marks survive `dplyr::filter()` and `dplyr::mutate()`,
#' but a summary can drop both, so whoever publishes derived data must check
#' that no restricted input fed it.
#'
#' ## Offering a restricted option (for package authors)
#'
#' A function argument that offers a restricted option must:
#'
#' - keep a public option as its default;
#' - label the restricted value in its `@param` with the phrase from
#'   `.restricted_label()`, written as an inline roxygen R expression, which
#'   renders as "(**restricted access**, see [whep_data_access_status()])";
#' - inherit this section with
#'   `@inheritSection whep_data_access_status Restricted access`;
#' - validate the argument with `.arg_match_access()`, whose error names the
#'   restricted values as such;
#' - read the data through [whep_read_file()], which aborts when access fails
#'   and records `data_access`;
#' - record the chosen method in a `method_<quantity>` column, as for any
#'   other method choice.
#'
#' @param check If `TRUE`, the default, contact the board to verify that it
#'   answers, accepts the credentials and holds each input. If `FALSE`, only
#'   report whether the environment variables are set.
#'
#' @returns A tibble with one row per restricted input in [`whep_inputs`],
#'   or one row with `input = NA` describing the board itself when none is
#'   registered. Columns:
#'   - `input`: the restricted alias.
#'   - `public_alternative`: the public alias suggested instead, or `NA`.
#'   - `configured`: whether `WHEP_RESTRICTED_BOARD` and
#'     `WHEP_RESTRICTED_SECRET` are set.
#'   - `credential_type`: `"account"` (own WebDAV path), `"share_link"`, or
#'     `NA` when not configured.
#'   - `host`: the board's host name, or `NA`.
#'   - `status`: `"available"`, `"not_configured"`, `"not_checked"`,
#'     `"unauthorised"`, `"unreachable"` or `"missing"`.
#'
#' @export
#'
#' @examples
#' whep_data_access_status(check = FALSE)
whep_data_access_status <- function(check = TRUE) {
  creds <- .restricted_credentials()
  restricted <- .whep_registry() |>
    dplyr::filter(.data$access == "restricted")

  inputs <- if (nrow(restricted) == 0L) {
    tibble::tibble(input = NA_character_, public_alternative = NA_character_)
  } else {
    tibble::tibble(
      input = restricted$alias,
      public_alternative = restricted$public_alternative
    )
  }

  inputs |>
    dplyr::mutate(
      configured = !is.null(creds),
      credential_type = creds$type %||% NA_character_,
      host = .url_host(creds$base),
      status = purrr::map_chr(
        .data$input,
        \(alias) .access_status(alias, restricted, creds, check)
      )
    )
}

#' Refuse to publish restricted data
#'
#' @description
#' Abort if `data` was read from the restricted board, so it cannot be
#' uploaded as a public pin. Data counts as restricted when it carries the
#' attribute `whep_access = "restricted"` or a `data_access` column holding
#' `"restricted"`, both of which [whep_read_file()] sets. Pin upload scripts
#' (`inst/scripts/prepare_upload.R`) call it before writing.
#'
#' @inheritSection whep_data_access_status Restricted access
#'
#' @param data The object about to be published.
#'
#' @returns `data`, invisibly, when it carries no restricted mark. Otherwise
#'   an error of class `whep_restricted_publish`.
#'
#' @export
#'
#' @examples
#' public <- tibble::tibble(x = 1:3)
#' whep_assert_publishable(public)
#'
#' restricted <- tibble::tibble(x = 1:3, data_access = "restricted")
#' try(whep_assert_publishable(restricted))
whep_assert_publishable <- function(data) {
  marked <- identical(attr(data, "whep_access"), "restricted") ||
    identical(attr(data, "data_access"), "restricted") ||
    (is.data.frame(data) &&
      rlang::has_name(data, "data_access") &&
      any(data$data_access == "restricted", na.rm = TRUE))

  if (marked) {
    cli::cli_abort(
      c(
        "Refusing to publish data read from the restricted board.",
        x = "Restricted inputs are licensed or confidential and must not be
             republished as a public pin.",
        i = "Publish only data that no restricted input fed."
      ),
      class = "whep_restricted_publish"
    )
  }

  invisible(data)
}

# -- Labelling restricted options ----------------------------------------------

# The phrase every restricted argument value or method carries in its docs.
# Use it inline in roxygen, so the wording cannot drift between functions.
.restricted_label <- function() {
  "(**restricted access**, see [whep_data_access_status()])"
}

# `rlang::arg_match()` for an argument offering restricted options. A wrong
# value aborts listing which of the valid values need restricted access, so
# the error tells the caller what each choice implies.
.arg_match_access <- function(
  arg,
  values,
  restricted,
  error_arg = rlang::caller_arg(arg),
  error_call = rlang::caller_env()
) {
  rlang::try_fetch(
    rlang::arg_match(
      arg,
      values,
      error_arg = error_arg,
      error_call = error_call
    ),
    error = function(e) {
      cli::cli_abort(
        c(
          "Invalid {.arg {error_arg}}.",
          i = "Restricted access: {.val {intersect(values, restricted)}}.
               Public: {.val {setdiff(values, restricted)}}."
        ),
        parent = e,
        call = error_call
      )
    }
  )
}

# -- Credentials ---------------------------------------------------------------

# NULL when restricted access is not configured. The secret lives only in the
# returned list, which never leaves this file's helpers.
.restricted_credentials <- function() {
  base <- Sys.getenv("WHEP_RESTRICTED_BOARD")
  secret <- Sys.getenv("WHEP_RESTRICTED_SECRET")
  if (!nzchar(base) || !nzchar(secret)) {
    return(NULL)
  }

  share <- .share_link_dav(base)
  type <- if (is.null(share)) "account" else "share_link"
  base <- .with_trailing_slash(share %||% base)
  user <- Sys.getenv("WHEP_RESTRICTED_USER")
  if (!nzchar(user)) {
    user <- if (type == "share_link") "anonymous" else .dav_login(base)
  }
  if (is.na(user)) {
    return(NULL)
  }

  list(base = base, user = user, secret = secret, type = type)
}

# A share link `https://host/s/<token>` (or `/index.php/s/<token>`) becomes its
# public WebDAV root; a link already in that form is kept as such. NULL for an
# account's own WebDAV path.
.share_link_dav <- function(base) {
  link <- stringr::str_match(
    base,
    "^(https?://[^/]+)/(?:index\\.php/)?s/([A-Za-z0-9]+)/?$"
  )
  if (!is.na(link[1, 1])) {
    return(paste0(link[1, 2], "/public.php/dav/files/", link[1, 3], "/"))
  }
  if (stringr::str_detect(base, "/public\\.php/dav/files/")) {
    return(base)
  }
  NULL
}

.dav_login <- function(base) {
  login <- stringr::str_match(base, "/remote\\.php/dav/files/([^/]+)/")[1, 2]
  if (is.na(login)) NA_character_ else utils::URLdecode(login)
}

.with_trailing_slash <- function(url) {
  if (stringr::str_ends(url, "/")) url else paste0(url, "/")
}

.url_host <- function(url) {
  if (is.null(url)) NA_character_ else httr::parse_url(url)$hostname
}

.restricted_headers <- function(creds) {
  token <- paste0(creds$user, ":", creds$secret) |>
    charToRaw() |>
    jsonlite::base64_enc() |>
    stringr::str_remove_all("\\s")

  c(
    Authorization = paste("Basic", token),
    `X-Requested-With` = "XMLHttpRequest"
  )
}

# -- Resolving a restricted row ------------------------------------------------

.is_restricted <- function(file_info) {
  identical(file_info$access, "restricted")
}

# Where a registry row is read from: list(file_info, board, data_access).
# A public row keeps `board = NULL` and `data_access = NULL`, so its output is
# unchanged. A restricted row was chosen explicitly, so failing access aborts:
# there is no fallback.
.resolve_access <- function(file_info) {
  if (!.is_restricted(file_info)) {
    return(list(file_info = file_info, board = NULL, data_access = NULL))
  }

  opened <- .open_restricted_board(file_info, .restricted_credentials())
  if (is.null(opened$board)) {
    .abort_restricted_unavailable(file_info, opened$reason)
  }

  list(file_info = file_info, board = opened$board, data_access = "restricted")
}

# list(board, reason): the board when it can be used, otherwise why not.
.open_restricted_board <- function(file_info, creds) {
  if (is.null(creds)) {
    return(list(board = NULL, reason = "not_configured"))
  }

  url <- .restricted_board_url(file_info$board_url, creds$base)
  headers <- .restricted_headers(creds)
  status <- .probe_restricted(url, headers, "GET")
  if (status != "available") {
    return(list(board = NULL, reason = status))
  }

  board <- .build_board_with_progress(url, headers)
  if (!file_info$alias %in% pins::pin_list(board)) {
    return(list(board = NULL, reason = "missing"))
  }

  list(board = board, reason = NULL)
}

# One row of `whep_data_access_status()`. `alias` is NA for the board-level
# row reported when no restricted input is registered.
.access_status <- function(alias, restricted, creds, check) {
  if (is.null(creds)) {
    return("not_configured")
  }
  if (!check) {
    return("not_checked")
  }
  if (is.na(alias)) {
    return(.probe_restricted(
      creds$base,
      .restricted_headers(creds),
      "PROPFIND"
    ))
  }

  file_info <- .fetch_file_info(alias, restricted)
  opened <- .open_restricted_board(file_info, creds)
  if (is.null(opened$board)) opened$reason else "available"
}

# Restricted rows store a path relative to the restricted board root, so no
# personal address ever enters the registry.
.restricted_board_url <- function(board_url, base) {
  if (!stringr::str_starts(board_url, "restricted:")) {
    cli::cli_abort(
      c(
        "A restricted registry row must store a {.val restricted:} path.",
        i = "Got {.val {board_url}}; expected e.g.
             {.val restricted:<folder>/_pins.yaml}."
      ),
      class = "whep_registry_error"
    )
  }

  path <- board_url |>
    stringr::str_remove("^restricted:") |>
    stringr::str_remove("^/+")
  paste0(base, path)
}

.probe_restricted <- function(url, headers, verb) {
  if (verb == "PROPFIND") {
    headers <- c(headers, Depth = "0")
  }
  # A fresh handle, so no session cookie from an earlier request to the host
  # can stand in for these credentials: httr pools one handle per host, and
  # Nextcloud answers a valid session cookie even when the password is wrong.
  response <- tryCatch(
    httr::VERB(
      verb,
      url,
      httr::add_headers(.headers = headers),
      httr::timeout(10),
      handle = httr::handle(url)
    ),
    error = function(e) NULL
  )
  if (is.null(response)) {
    return("unreachable")
  }
  .status_from_code(httr::status_code(response))
}

.status_from_code <- function(code) {
  if (code < 300) {
    "available"
  } else if (code %in% c(401L, 403L)) {
    "unauthorised"
  } else if (code == 404L) {
    "missing"
  } else {
    "unreachable"
  }
}

.abort_restricted_unavailable <- function(file_info, reason) {
  alias <- file_info$alias
  alternative <- file_info$public_alternative %||% NA_character_

  cli::cli_abort(
    c(
      "{.val {alias}} needs restricted access, which is not available here.",
      x = .restricted_reason(reason),
      .restricted_setup_help(),
      .alternative_hint(alternative)
    ),
    class = "whep_restricted_unavailable",
    input = alias,
    reason = reason,
    public_alternative = alternative
  )
}

.alternative_hint <- function(alternative) {
  if (is.na(alternative)) {
    return(c(i = "No public alternative is registered for this input."))
  }
  c(
    i = paste0(
      "Public alternative you can use instead: ",
      "{.code whep_read_file(\"{alternative}\")}. ",
      "It is a different dataset, so results can differ."
    )
  )
}

.restricted_reason <- function(reason) {
  switch(
    reason,
    not_configured = "Restricted access is not configured:
      {.envvar WHEP_RESTRICTED_BOARD} or {.envvar WHEP_RESTRICTED_SECRET} is
      unset, or no login could be derived for the board.",
    unauthorised = "The restricted board refused the credentials (wrong
      secret, expired app password, or no access to the folder).",
    unreachable = "The restricted board could not be reached.",
    missing = "The restricted board does not hold this input, or the folder
      address is wrong.",
    "Unknown reason {.val {reason}}."
  )
}

.restricted_setup_help <- function() {
  c(
    i = "Ask the project for access to the restricted-datasets folder, then
         set in {.file ~/.Renviron}:",
    " " = "{.envvar WHEP_RESTRICTED_BOARD}: your WebDAV path to it
           ({.code https://saco.csic.es/remote.php/dav/files/<login>/<folder>/})
           or the share link you were sent
           ({.code https://saco.csic.es/s/<token>}).",
    " " = "{.envvar WHEP_RESTRICTED_SECRET}: an app password, or the share's
           password.",
    " " = "{.envvar WHEP_RESTRICTED_USER} (optional): your login.",
    i = "Check with {.run whep::whep_data_access_status()}."
  )
}

# Mark data read from the restricted board: a `data_access` column on tables
# (an attribute on a returned path) and a `whep_access` attribute, both of
# which `whep_assert_publishable()` refuses. Public inputs are left untouched.
.mark_access <- function(x, data_access) {
  if (is.null(data_access)) {
    return(x)
  }

  if (data.table::is.data.table(x)) {
    data.table::set(x, j = "data_access", value = rep(data_access, nrow(x)))
    data.table::setattr(x, "whep_access", data_access)
  } else if (is.data.frame(x)) {
    x$data_access <- rep(data_access, nrow(x))
    attr(x, "whep_access") <- data_access
  } else {
    attr(x, "data_access") <- data_access
    attr(x, "whep_access") <- data_access
  }
  x
}
