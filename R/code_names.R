#' Get area names from area codes
#'
#' @description
#' Add a new column to an existing tibble with the corresponding name
#' for each code. The codes are assumed to be from those defined by
#' the `FABIO` model.
#'
#' @param table The table that will be modified with a new column.
#' @param code_column The name of the column in `table` containing the codes.
#' @param name_column The name of the output column containing the names.
#'
#' @returns A tibble with all the contents of `table` and an extra column
#' named `name_column`, which contains the names. If there is no name match,
#' an `NA` is included.
#'
#' @export
#'
#' @examples
#' table <- tibble::tibble(area_code = c(1, 2, 4444, 3))
#'
#' add_area_name(table)
#'
#' table |>
#'  dplyr::rename(my_area_code = area_code) |>
#'  add_area_name(code_column = "my_area_code")
#'
#' add_area_name(table, name_column = "my_custom_name")
add_area_name <- function(table, code_column = "area_code", name_column = "area_name") {
  regions <- "input/raw/regions.csv" |>
    .read_local_csv() |>
    dplyr::select({{ name_column }} := name, {{ code_column }} := fabio_code)

  table |>
    dplyr::left_join(regions, {{ code_column }})
}

#' Get area codes from area names
#'
#' @description
#' Add a new column to an existing tibble with the corresponding code
#' for each name. The codes are assumed to be from those defined by
#' the `FABIO` model.
#'
#' @param table The table that will be modified with a new column.
#' @param code_column The name of the output column containing the codes.
#' @param name_column The name of the column in `table` containing the names.
#'
#' @returns A tibble with all the contents of `table` and an extra column
#' named `code_column`, which contains the codes. If there is no code match,
#' an `NA` is included.
#'
#' @export
#'
#' @examples
#' table <- tibble::tibble(
#'   area_name = c("Armenia", "Afghanistan", "Dummy Country", "Albania")
#' )
#'
#' add_area_code(table)
#'
#' table |>
#'  dplyr::rename(my_area_name = area_name) |>
#'  add_area_code(name_column = "my_area_name")
#'
#' add_area_code(table, code_column = "my_custom_code")
add_area_code <- function(table, name_column = "area_name", code_column = "area_code") {
  regions <- "input/raw/regions.csv" |>
    .read_local_csv() |>
    dplyr::select({{ name_column }} := name, {{ code_column }} := fabio_code)

  table |>
    dplyr::left_join(regions, {{ name_column }})
}
