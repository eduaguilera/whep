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
read_input <- function(path) {
  readr::read_csv(path) |>
    tibble::as_tibble()
}



prepare_for_upload <- function(input_path, data_name, ...) {
  board <- pins::board_temp(versioned = TRUE)

  version <- input_path |>
    read_input() |>
    create_version(
      board,
      data_name,
      ...
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
  "C:/PhD/GRAFS/Inputs_SACO/Livestock_Prod_ygps.csv",
  "livestock_prod_ygps"
)
