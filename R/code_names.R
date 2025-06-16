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
#'   dplyr::rename(my_area_code = area_code) |>
#'   add_area_name(code_column = "my_area_code")
#'
#' add_area_name(table, name_column = "my_custom_name")
add_area_name <- function(
    table,
    code_column = "area_code",
    name_column = "area_name") {
  regions <- .get_regions(name_column, code_column)

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
#'   dplyr::rename(my_area_name = area_name) |>
#'   add_area_code(name_column = "my_area_name")
#'
#' add_area_code(table, code_column = "my_custom_code")
add_area_code <- function(
    table,
    name_column = "area_name",
    code_column = "area_code") {
  regions <- .get_regions(name_column, code_column)

  table |>
    dplyr::left_join(regions, {{ name_column }})
}

#' Get item names from item codes
#'
#' @description
#' Add a new column to an existing tibble with the corresponding name
#' for each item code. The codes are assumed to be from those defined by
#' FAOSTAT.
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
#' table <- tibble::tibble(item_cbs_code = c(2559, 2744, 9876))
#' add_item_cbs_name(table)
#'
#' table |>
#'   dplyr::rename(my_item_cbs_code = item_cbs_code) |>
#'   add_item_cbs_name(code_column = "my_item_cbs_code")
#'
#' add_item_cbs_name(table, name_column = "my_custom_name")
add_item_cbs_name <- function(
    table,
    code_column = "item_cbs_code",
    name_column = "item_cbs_name") {
  items <- .get_cbs_items(name_column, code_column)

  table |>
    dplyr::left_join(items, {{ code_column }})
}

#' Get item codes from item names
#'
#' @description
#' Add a new column to an existing tibble with the corresponding code
#' for each item name. The codes are assumed to be from those defined by
#' the FAOSTAT.
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
#'   item_cbs_name = c("Cottonseed", "Eggs", "Dummy Item")
#' )
#' add_item_cbs_code(table)
#'
#' table |>
#'   dplyr::rename(my_item_cbs_name = item_cbs_name) |>
#'   add_item_cbs_code(name_column = "my_item_cbs_name")
#'
#' add_item_cbs_code(table, code_column = "my_custom_code")
add_item_cbs_code <- function(
    table,
    name_column = "item_cbs_name",
    code_column = "item_cbs_code") {
  items <- .get_cbs_items(name_column, code_column)

  table |>
    dplyr::left_join(items, {{ name_column }})
}

#' Get process names from process codes
#'
#' @description
#' Add a new column to an existing tibble with the corresponding name
#' for each process code. The codes are assumed to be from those defined by
#' the FABIO model.
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
#' table <- tibble::tibble(process_code = c("p017", "p076", "dummy"))
#' add_process_name(table)
#'
#' table |>
#'   dplyr::rename(my_process_code = process_code) |>
#'   add_process_name(code_column = "my_process_code")
#'
#' add_process_name(table, name_column = "my_custom_name")
add_process_name <- function(
    table,
    code_column = "process_code",
    name_column = "process_name") {
  processes <- .get_processes(name_column, code_column)

  table |>
    dplyr::left_join(processes, {{ code_column }})
}

#' Get process codes from process names
#'
#' @description
#' Add a new column to an existing tibble with the corresponding code
#' for each process name. The codes are assumed to be from those defined by
#' the FABIO model.
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
#'   process_name = c("Beans production", "Olive Oil extraction", "Dummy")
#' )
#' add_process_code(table)
#'
#' table |>
#'   dplyr::rename(my_process_name = process_name) |>
#'   add_process_code(name_column = "my_process_name")
#'
#' add_process_code(table, code_column = "my_custom_code")
add_process_code <- function(
    table,
    name_column = "process_name",
    code_column = "process_code") {
  processes <- .get_processes(name_column, code_column)

  table |>
    dplyr::left_join(processes, {{ name_column }})
}

.get_regions <- function(name_column, code_column) {
  "input/raw/regions.csv" |>
    .read_local_csv() |>
    dplyr::select(!!name_column := area, !!code_column := area_code)
}

.get_cbs_items <- function(name_column, code_column) {
  "input/raw/items_cbs.csv" |>
    .read_local_csv() |>
    dplyr::select(!!name_column := item, !!code_column := item_cbs_code)
}

.get_processes <- function(name_column, code_column) {
  "input/raw/processes.csv" |>
    .read_local_csv() |>
    dplyr::select(!!name_column := process, !!code_column := process_code)
}
