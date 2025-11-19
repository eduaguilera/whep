#' Get area names from area codes
#'
#' @description
#' Add a new column to an existing tibble with the corresponding name
#' for each code. The codes are assumed to be from those defined by
#' the `FABIO` model, which them themselves come from `FAOSTAT` internal
#' codes. Equivalences with ISO 3166-1 numeric can be found in the
#' _Area Codes_ CSV from the zip file that can be downloaded from
#' [FAOSTAT](https://www.fao.org/faostat/en/#data/FBS). TODO: Think about
#' this, would be nice to use ISO3 codes but won't be enough for our periods.
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
  name_column = "area_name"
) {
  countries <- .get_countries(name_column, code_column)

  table |>
    dplyr::left_join(countries, {{ code_column }})
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
  code_column = "area_code"
) {
  countries <- .get_countries(name_column, code_column)

  table |>
    dplyr::left_join(countries, {{ name_column }})
}

#' Get commodity balance sheet item names from item codes
#'
#' @description
#' Add a new column to an existing tibble with the corresponding name
#' for each commodity balance sheet item code. The codes are assumed to be
#' from those defined by FAOSTAT.
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
  name_column = "item_cbs_name"
) {
  items <- .get_cbs_items(name_column, code_column)

  table |>
    dplyr::left_join(items, {{ code_column }})
}

#' Get commodity balance sheet item codes from item names
#'
#' @description
#' Add a new column to an existing tibble with the corresponding code
#' for each commodity balance sheet item name. The codes are assumed to be
#' from those defined by FAOSTAT.
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
  code_column = "item_cbs_code"
) {
  items <- .get_cbs_items(name_column, code_column)

  table |>
    dplyr::left_join(items, {{ name_column }})
}

#' Get production item names from item codes
#'
#' @description
#' Add a new column to an existing tibble with the corresponding name
#' for each production item code. The codes are assumed to be from those
#' defined by FAOSTAT.
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
#' table <- tibble::tibble(item_prod_code = c(27, 358, 12345))
#' add_item_prod_name(table)
#'
#' table |>
#'   dplyr::rename(my_item_prod_code = item_prod_code) |>
#'   add_item_prod_name(code_column = "my_item_prod_code")
#'
#' add_item_prod_name(table, name_column = "my_custom_name")
add_item_prod_name <- function(
  table,
  code_column = "item_prod_code",
  name_column = "item_prod_name"
) {
  items <- .get_prod_items(name_column, code_column)

  table |>
    dplyr::left_join(items, {{ code_column }})
}

#' Get production item codes from item names
#'
#' @description
#' Add a new column to an existing tibble with the corresponding code
#' for each production item name. The codes are assumed to be from those
#' defined by FAOSTAT.
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
#'   item_prod_name = c("Rice", "Cabbages", "Dummy Item")
#' )
#' add_item_prod_code(table)
#'
#' table |>
#'   dplyr::rename(my_item_prod_name = item_prod_name) |>
#'   add_item_prod_code(name_column = "my_item_prod_name")
#'
#' add_item_prod_code(table, code_column = "my_custom_code")
add_item_prod_code <- function(
  table,
  name_column = "item_prod_name",
  code_column = "item_prod_code"
) {
  items <- .get_prod_items(name_column, code_column)

  table |>
    dplyr::left_join(items, {{ name_column }})
}

.get_countries <- function(name_column, code_column) {
  whep::countries |>
    dplyr::select(!!name_column := area_name, !!code_column := area_code)
}

.get_cbs_items <- function(name_column, code_column) {
  whep::items_cbs |>
    dplyr::select(
      !!name_column := item_cbs_name,
      !!code_column := item_cbs_code
    )
}

.get_prod_items <- function(name_column, code_column) {
  whep::items_prod |>
    dplyr::select(
      !!name_column := item_prod_name,
      !!code_column := item_prod_code
    )
}
