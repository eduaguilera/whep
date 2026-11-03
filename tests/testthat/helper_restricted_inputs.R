# Fixtures for test_restricted_inputs.R (#1386): a registry with restricted
# rows, and a local pins board standing in for the restricted saco board.

.toy_registry <- function() {
  tibble::tribble(
    ~alias,            ~board_url,                    ~version, ~access,     ~public_alternative,
    "read_example",    "https://example.org/_pins.yaml", NA,    "public",    NA,
    "restricted_toy",  "restricted:toy/_pins.yaml",   NA,       "restricted", "read_example",
    "restricted_only", "restricted:toy/_pins.yaml",   NA,       "restricted", NA
  )
}

# A board folder holding `restricted_toy` as csv + parquet, as WHEP pins are.
.toy_restricted_board <- function(env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  files <- file.path(dir, c("restricted_toy.csv", "restricted_toy.parquet"))
  data <- tibble::tibble(year = 2001:2003, value = c(1.5, 2.5, 3.5))
  readr::write_csv(data, files[[1]])
  nanoparquet::write_parquet(data, files[[2]])
  board <- pins::board_folder(file.path(dir, "board"), versioned = TRUE)
  suppressMessages(pins::pin_upload(board, files, "restricted_toy"))
  board
}

.local_restricted_env <- function(
  board = "https://saco.example/remote.php/dav/files/someone/restricted/",
  secret = "not-a-real-secret",
  user = "",
  env = parent.frame()
) {
  withr::local_envvar(
    WHEP_RESTRICTED_BOARD = board,
    WHEP_RESTRICTED_SECRET = secret,
    WHEP_RESTRICTED_USER = user,
    .local_envir = env
  )
}
