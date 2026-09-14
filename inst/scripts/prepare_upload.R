# Small script useful to setup a version folder for your data.
# Remember doing the manual steps. See prepare_for_upload call at the end
# of the script, fill with your data, run and follow printed instructions.

create_version <- function(data, board, name, ...) {
  paths <- file.path(
    tempdir(),
    c(
      stringr::str_glue("{name}.csv"),
      stringr::str_glue("{name}.parquet")
    )
  )

  readr::write_csv(data, paths[[1]])
  nanoparquet::write_parquet(data, paths[[2]])

  board |>
    pins::pin_upload(paths, name, ...)

  board |>
    pins::pin_versions(name) |>
    tail(1) |>
    dplyr::pull(version)
}

# Change this accordingly if your data is not CSV.
# Please make the output a tibble.
#
# Pass col_types whenever a text column could be mistaken for something else.
# readr guesses per column from the values it sees, so a label column whose
# every value happens to be a logical literal is read as a logical: FAOSTAT
# writes tonnes as "t", and readr parses "t" as TRUE. That is how the
# faostat-cbs-new pin came to hold TRUE in the Unit column of every one of
# its rows, destroying the unit label of the whole source (whep#1025).
# faostat_balance_col_types() in prepare_faostat_balances.R is the spec for
# the FAOSTAT bulk CSVs; give an equivalent one for any other labelled input.
read_input <- function(path, sheet = NULL, col_types = NULL) {
  ext <- tools::file_ext(path)

  data <-
    if (ext == "rds") {
      readRDS(path)
    } else if (ext == "csv") {
      readr::read_csv(path, col_types = col_types, show_col_types = FALSE)
    } else {
      readxl::read_excel(path, sheet = sheet)
    }

  tibble::as_tibble(data)
}


prepare_for_upload <- function(input_path, data_name, ...) {
  board <- pins::board_temp(versioned = TRUE)

  version <- read_input(input_path, ...) |>
    create_version(
      board,
      data_name
    )

  output_path <- file.path(board$path, data_name, version)

  cli::cli_alert_info(
    paste(
      "1. Manually upload the folder {output_path} into your board.",
      "Folder path copied to your clipboard."
    )
  )
  clipr::write_clip(output_path)
  cli::cli_alert_info(
    paste(
      "2. Add the corresponding line",
      "- {data_name}/{version}/",
      "in _pins.yaml at the end of the '{data_name}:' section",
      sep = "\n"
    )
  )
  cli::cli_alert_info(
    paste(
      "3. If you want the package to use this version, add a new row to",
      "whep_inputs.csv if it's a new file or update the version in the",
      "existing row. The version is {version}."
    )
  )
}

prepare_for_upload(
  "C:/PhD/GRAFS/Inputs_SACO/inputs_saco_new/n_balance_ygpit_all.csv",
  "n_balance_ygpit_all"
)
