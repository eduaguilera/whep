# Restricted input datasets (#1386). Everything here is offline: the registry,
# the board and the HTTP probe are stubbed. The one real read is the smoke
# test at the bottom, which runs only when WHEP_RESTRICTED_BOARD is set.

.mock_available_board <- function(board, env = parent.frame()) {
  testthat::local_mocked_bindings(
    .whep_registry = .toy_registry,
    .probe_restricted = function(...) "available",
    .build_board_with_progress = function(...) board,
    .env = env
  )
}

# Explicit choice: public stays public --------------------------------------

testthat::test_that("a public input is read unchanged, with no marks", {
  result <- suppressMessages(whep_read_file("read_example"))

  testthat::expect_false("data_access" %in% names(result))
  testthat::expect_null(attr(result, "whep_access"))
})

testthat::test_that("every registry row is public on main", {
  testthat::expect_true(all(whep::whep_inputs$access == "public"))
  testthat::expect_true(all(is.na(whep::whep_inputs$public_alternative)))
})

# Restricted chosen and available --------------------------------------------

testthat::test_that("a restricted alias reads the board and records it", {
  board <- .toy_restricted_board()
  .local_restricted_env()
  .mock_available_board(board)

  result <- suppressMessages(whep_read_file("restricted_toy"))

  testthat::expect_equal(nrow(result), 3L)
  testthat::expect_equal(unique(result$data_access), "restricted")
  testthat::expect_identical(attr(result, "whep_access"), "restricted")
})

testthat::test_that("the csv read is marked too", {
  board <- .toy_restricted_board()
  .local_restricted_env()
  .mock_available_board(board)

  result <- suppressMessages(whep_read_file("restricted_toy", type = "csv"))

  testthat::expect_equal(result$data_access, rep("restricted", 3L))
})

testthat::test_that("a returned path carries the mark as attributes", {
  path <- .mark_access("pins/restricted_toy.nc", "restricted")

  testthat::expect_identical(attr(path, "data_access"), "restricted")
  testthat::expect_error(
    whep_assert_publishable(path),
    class = "whep_restricted_publish"
  )
  testthat::expect_identical(.mark_access("x.nc", NULL), "x.nc")
})

testthat::test_that(".read_input marks a restricted data.table", {
  board <- .toy_restricted_board()
  .local_restricted_env()
  .mock_available_board(board)

  dt <- suppressMessages(.read_input("restricted_toy"))

  testthat::expect_true(data.table::is.data.table(dt))
  testthat::expect_equal(unique(dt$data_access), "restricted")
  testthat::expect_identical(attr(dt, "whep_access"), "restricted")
})

testthat::test_that("versions of a restricted alias come from its board", {
  board <- .toy_restricted_board()
  .local_restricted_env()
  .mock_available_board(board)

  versions <- whep_list_file_versions("restricted_toy")

  testthat::expect_equal(nrow(versions), 1L)
})

testthat::test_that("the board is opened with the credentials as headers", {
  board <- .toy_restricted_board()
  .local_restricted_env(secret = "s3cret", user = "alice")
  seen <- new.env()
  testthat::local_mocked_bindings(
    .whep_registry = .toy_registry,
    .probe_restricted = function(url, headers, verb) {
      seen$probe <- list(url = url, headers = headers, verb = verb)
      "available"
    },
    .build_board_with_progress = function(url, headers) {
      seen$board <- list(url = url, headers = headers)
      board
    }
  )

  suppressMessages(whep_read_file("restricted_toy"))

  expected_url <- paste0(
    "https://saco.example/remote.php/dav/files/someone/restricted/",
    "toy/_pins.yaml"
  )
  testthat::expect_equal(seen$board$url, expected_url)
  testthat::expect_equal(seen$probe$url, expected_url)
  testthat::expect_equal(seen$probe$verb, "GET")
  testthat::expect_equal(
    seen$board$headers[["Authorization"]],
    paste("Basic", jsonlite::base64_enc("alice:s3cret"))
  )
  testthat::expect_equal(
    seen$board$headers[["X-Requested-With"]],
    "XMLHttpRequest"
  )
})

# Restricted chosen and unavailable: loud, classed, no fallback --------------

.expect_unavailable <- function(expr, reason) {
  err <- testthat::expect_error(expr, class = "whep_restricted_unavailable")
  testthat::expect_equal(err$reason, reason)
  testthat::expect_equal(err$input, "restricted_toy")
  testthat::expect_equal(err$public_alternative, "read_example")
  message <- conditionMessage(err)
  testthat::expect_match(message, "needs restricted access")
  testthat::expect_match(message, "read_example")
  testthat::expect_match(message, "WHEP_RESTRICTED_BOARD")
  testthat::expect_false(grepl("s3cret", message, fixed = TRUE))
  invisible(err)
}

testthat::test_that("an unconfigured restricted read aborts", {
  withr::local_envvar(
    WHEP_RESTRICTED_BOARD = NA,
    WHEP_RESTRICTED_SECRET = NA,
    WHEP_RESTRICTED_USER = NA
  )
  testthat::local_mocked_bindings(.whep_registry = .toy_registry)

  err <- .expect_unavailable(
    suppressMessages(whep_read_file("restricted_toy")),
    "not_configured"
  )
  testthat::expect_match(conditionMessage(err), "not configured")
})

testthat::test_that("a secret without a board counts as unconfigured", {
  withr::local_envvar(
    WHEP_RESTRICTED_BOARD = NA,
    WHEP_RESTRICTED_SECRET = "s3cret"
  )
  testthat::local_mocked_bindings(.whep_registry = .toy_registry)

  .expect_unavailable(
    suppressMessages(whep_read_file("restricted_toy")),
    "not_configured"
  )
})

testthat::test_that("refused credentials abort as unauthorised", {
  .local_restricted_env(secret = "s3cret")
  testthat::local_mocked_bindings(
    .whep_registry = .toy_registry,
    .probe_restricted = function(...) "unauthorised"
  )

  err <- .expect_unavailable(
    suppressMessages(whep_read_file("restricted_toy")),
    "unauthorised"
  )
  testthat::expect_match(conditionMessage(err), "refused the credentials")
})

testthat::test_that("an unreachable board aborts as unreachable", {
  .local_restricted_env(secret = "s3cret")
  testthat::local_mocked_bindings(
    .whep_registry = .toy_registry,
    .probe_restricted = function(...) "unreachable"
  )

  .expect_unavailable(
    suppressMessages(whep_read_file("restricted_toy")),
    "unreachable"
  )
})

testthat::test_that("a board without the pin aborts as missing", {
  .local_restricted_env(secret = "s3cret")
  empty <- pins::board_folder(withr::local_tempdir())
  testthat::local_mocked_bindings(
    .whep_registry = .toy_registry,
    .probe_restricted = function(...) "available",
    .build_board_with_progress = function(...) empty
  )

  .expect_unavailable(
    suppressMessages(whep_read_file("restricted_toy")),
    "missing"
  )
})

testthat::test_that("the public alternative is never read in its place", {
  withr::local_envvar(WHEP_RESTRICTED_BOARD = NA)
  read <- new.env()
  testthat::local_mocked_bindings(
    .whep_registry = .toy_registry,
    .download_public = function(file_info, ...) {
      read$alias <- file_info$alias
      character()
    }
  )

  testthat::expect_error(
    suppressMessages(whep_read_file("restricted_toy")),
    class = "whep_restricted_unavailable"
  )
  testthat::expect_null(read$alias)
})

testthat::test_that("an input with no alternative says so", {
  withr::local_envvar(WHEP_RESTRICTED_BOARD = NA)
  testthat::local_mocked_bindings(.whep_registry = .toy_registry)

  err <- testthat::expect_error(
    suppressMessages(whep_read_file("restricted_only")),
    class = "whep_restricted_unavailable"
  )
  testthat::expect_true(is.na(err$public_alternative))
  testthat::expect_match(conditionMessage(err), "No public alternative")
})

testthat::test_that("a restricted row with an absolute board_url is refused", {
  .local_restricted_env()
  registry <- .toy_registry() |>
    dplyr::mutate(
      board_url = dplyr::if_else(
        alias == "restricted_toy",
        "https://saco.example/remote.php/dav/files/x/_pins.yaml",
        board_url
      )
    )
  testthat::local_mocked_bindings(.whep_registry = function() registry)

  testthat::expect_error(
    suppressMessages(whep_read_file("restricted_toy")),
    class = "whep_registry_error"
  )
})

# Credentials -----------------------------------------------------------------

testthat::test_that("a share link becomes its public WebDAV root", {
  testthat::expect_equal(
    .share_link_dav("https://saco.csic.es/s/AbC123xyz"),
    "https://saco.csic.es/public.php/dav/files/AbC123xyz/"
  )
  testthat::expect_equal(
    .share_link_dav("https://saco.csic.es/index.php/s/AbC123xyz/"),
    "https://saco.csic.es/public.php/dav/files/AbC123xyz/"
  )
  testthat::expect_null(
    .share_link_dav("https://saco.csic.es/remote.php/dav/files/me/x/")
  )
})

testthat::test_that("a share link authenticates as anonymous", {
  .local_restricted_env(board = "https://saco.csic.es/s/AbC123xyz")

  creds <- .restricted_credentials()

  testthat::expect_equal(creds$type, "share_link")
  testthat::expect_equal(creds$user, "anonymous")
  testthat::expect_equal(
    creds$base,
    "https://saco.csic.es/public.php/dav/files/AbC123xyz/"
  )
})

testthat::test_that("an account path takes the login from the URL", {
  .local_restricted_env(
    board = "https://saco.example/remote.php/dav/files/someone/restricted"
  )

  creds <- .restricted_credentials()

  testthat::expect_equal(creds$type, "account")
  testthat::expect_equal(creds$user, "someone")
  testthat::expect_true(endsWith(creds$base, "/restricted/"))
})

testthat::test_that("WHEP_RESTRICTED_USER overrides the derived login", {
  .local_restricted_env(user = "other")

  testthat::expect_equal(.restricted_credentials()$user, "other")
})

testthat::test_that("an account path with no login is unconfigured", {
  .local_restricted_env(board = "https://saco.example/some/folder/")

  testthat::expect_null(.restricted_credentials())
})

testthat::test_that("the auth header is unwrapped base64 plus the DAV header", {
  long <- strrep("x", 80)
  headers <- .restricted_headers(list(user = "u", secret = long))

  testthat::expect_false(grepl("\\s", sub("^Basic ", "", headers[[1]])))
  testthat::expect_equal(
    rawToChar(jsonlite::base64_dec(sub("^Basic ", "", headers[[1]]))),
    paste0("u:", long)
  )
  testthat::expect_equal(headers[["X-Requested-With"]], "XMLHttpRequest")
})

testthat::test_that("HTTP codes map onto access reasons", {
  testthat::expect_equal(.status_from_code(200L), "available")
  testthat::expect_equal(.status_from_code(207L), "available")
  testthat::expect_equal(.status_from_code(401L), "unauthorised")
  testthat::expect_equal(.status_from_code(403L), "unauthorised")
  testthat::expect_equal(.status_from_code(404L), "missing")
  testthat::expect_equal(.status_from_code(503L), "unreachable")
})

testthat::test_that("a failed request reads as unreachable", {
  testthat::local_mocked_bindings(
    VERB = function(...) stop("could not resolve host"),
    .package = "httr"
  )

  testthat::expect_equal(
    .probe_restricted("https://saco.example/x", c(a = "b"), "GET"),
    "unreachable"
  )
})

testthat::test_that("PROPFIND probes ask for depth 0", {
  seen <- new.env()
  testthat::local_mocked_bindings(
    VERB = function(verb, url, config, ..., handle = NULL) {
      seen$verb <- verb
      seen$headers <- config$headers
      seen$handle <- handle
      structure(list(status_code = 207L), class = "response")
    },
    .package = "httr"
  )

  status <- .probe_restricted("https://saco.example/", c(a = "b"), "PROPFIND")

  testthat::expect_equal(status, "available")
  testthat::expect_equal(seen$verb, "PROPFIND")
  testthat::expect_equal(seen$headers[["Depth"]], "0")
  # A fresh handle: a pooled one would let an earlier session cookie answer
  # for a wrong password, which the real smoke test caught.
  testthat::expect_s3_class(seen$handle, "handle")
})

# Status report ---------------------------------------------------------------

testthat::test_that("the status lists restricted inputs, unconfigured", {
  withr::local_envvar(WHEP_RESTRICTED_BOARD = NA, WHEP_RESTRICTED_SECRET = NA)
  testthat::local_mocked_bindings(.whep_registry = .toy_registry)

  status <- whep_data_access_status()

  testthat::expect_equal(status$input, c("restricted_toy", "restricted_only"))
  testthat::expect_equal(status$public_alternative, c("read_example", NA))
  testthat::expect_false(any(status$configured))
  testthat::expect_equal(unique(status$status), "not_configured")
})

testthat::test_that("the status reports usability without the secret", {
  board <- .toy_restricted_board()
  .local_restricted_env(secret = "s3cret")
  .mock_available_board(board)

  status <- whep_data_access_status()

  # `restricted_only` is registered but the board does not hold it.
  testthat::expect_equal(status$status, c("available", "missing"))
  testthat::expect_equal(unique(status$credential_type), "account")
  testthat::expect_equal(unique(status$host), "saco.example")
  values <- unlist(lapply(status, as.character))
  testthat::expect_false(any(grepl("s3cret|someone", values)))
})

testthat::test_that("check = FALSE never contacts the board", {
  .local_restricted_env()
  testthat::local_mocked_bindings(
    .whep_registry = .toy_registry,
    .probe_restricted = function(...) stop("must not be called")
  )

  status <- whep_data_access_status(check = FALSE)

  testthat::expect_equal(unique(status$status), "not_checked")
})

testthat::test_that("with no restricted input the board itself is probed", {
  .local_restricted_env()
  testthat::local_mocked_bindings(
    .whep_registry = function() dplyr::slice(.toy_registry(), 1L),
    .probe_restricted = function(url, headers, verb) {
      if (verb == "PROPFIND") "unauthorised" else "available"
    }
  )

  status <- whep_data_access_status()

  testthat::expect_equal(nrow(status), 1L)
  testthat::expect_true(is.na(status$input))
  testthat::expect_equal(status$status, "unauthorised")
})

# Labels ----------------------------------------------------------------------

testthat::test_that("the restricted label names the access level", {
  testthat::expect_match(.restricted_label(), "restricted access")
  testthat::expect_match(.restricted_label(), "whep_data_access_status")
})

testthat::test_that("the label renders in the whep_read_file docs", {
  # The source man/ under load_all(), the installed Rd database otherwise.
  source_rd <- system.file("man", "whep_read_file.Rd", package = "whep")
  rd <- if (nzchar(source_rd)) {
    readLines(source_rd)
  } else {
    tryCatch(
      as.character(tools::Rd_db("whep")[["whep_read_file.Rd"]]),
      error = function(e) NULL
    )
  }
  testthat::skip_if(is.null(rd), "Rd database not available")

  text <- paste(rd, collapse = " ")
  testthat::expect_match(text, "restricted access")
  testthat::expect_match(text, "Restricted access")
})

testthat::test_that("an invalid choice names the restricted values", {
  pick <- function(source = c("public_db", "licensed_db")) {
    .arg_match_access(source, c("public_db", "licensed_db"), "licensed_db")
  }

  testthat::expect_equal(pick(), "public_db")
  testthat::expect_equal(pick("licensed_db"), "licensed_db")
  err <- testthat::expect_error(pick("nope"))
  message <- conditionMessage(err)
  testthat::expect_match(message, "Restricted access")
  testthat::expect_match(message, "licensed_db")
  testthat::expect_match(message, "source")
})

# Leak guard ------------------------------------------------------------------

testthat::test_that("restricted data cannot be published", {
  marked_column <- tibble::tibble(x = 1, data_access = "restricted")
  marked_attr <- structure(tibble::tibble(x = 1), whep_access = "restricted")

  testthat::expect_error(
    whep_assert_publishable(marked_column),
    class = "whep_restricted_publish"
  )
  testthat::expect_error(
    whep_assert_publishable(marked_attr),
    class = "whep_restricted_publish"
  )
})

testthat::test_that("the mark survives filter and mutate", {
  board <- .toy_restricted_board()
  .local_restricted_env()
  .mock_available_board(board)

  derived <- suppressMessages(whep_read_file("restricted_toy")) |>
    dplyr::filter(year > 2001) |>
    dplyr::mutate(value = value * 2) |>
    dplyr::select(-data_access)

  testthat::expect_error(
    whep_assert_publishable(derived),
    class = "whep_restricted_publish"
  )
})

testthat::test_that("public data passes the guard unchanged", {
  data <- tibble::tibble(x = 1:2)

  testthat::expect_identical(whep_assert_publishable(data), data)
  testthat::expect_silent(whep_assert_publishable(data))
})

# Real smoke test -------------------------------------------------------------

# Reads a real restricted board. It runs only where WHEP_RESTRICTED_BOARD,
# WHEP_RESTRICTED_SECRET (and WHEP_RESTRICTED_USER for an account path) point
# at a board holding the `restricted_smoke` pin, a 3-row tibble; never on CRAN,
# r-universe or CI, where the variables are unset.
testthat::test_that("smoke: a real restricted board is read and marked", {
  testthat::skip_on_cran()
  testthat::skip_if(Sys.getenv("WHEP_RESTRICTED_BOARD") == "")

  registry <- tibble::tribble(
    ~alias,             ~board_url,          ~version, ~access,      ~public_alternative,
    "read_example",     "unused",            NA,       "public",     NA,
    "restricted_smoke", "restricted:_pins.yaml", NA,   "restricted", "read_example"
  )
  testthat::local_mocked_bindings(.whep_registry = function() registry)

  result <- suppressMessages(whep_read_file("restricted_smoke"))
  testthat::expect_equal(nrow(result), 3L)
  testthat::expect_equal(unique(result$data_access), "restricted")

  withr::local_envvar(WHEP_RESTRICTED_SECRET = "deliberately-wrong")
  err <- testthat::expect_error(
    suppressMessages(whep_read_file("restricted_smoke")),
    class = "whep_restricted_unavailable"
  )
  testthat::expect_equal(err$reason, "unauthorised")
})
