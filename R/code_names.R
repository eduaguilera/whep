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
  polities <- .get_polities(
    name_column,
    code_column,
    join_column = code_column,
    table = table
  )

  table |>
    dplyr::left_join(polities, {{ code_column }})
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
  polities <- .get_polities(
    name_column,
    code_column,
    join_column = name_column
  )

  table |>
    dplyr::left_join(polities, {{ name_column }}) |>
    .fill_area_code_via_polities(name_column, code_column)
}

# Second pass for names the exact join could not match, routed through the
# polities database instead of the crosswalk's own label column.
#
# The crosswalk is keyed on the FAOSTAT area name, so a source using any other
# spelling gets NA even when the area exists. `get_primary_residues()` measured
# the cost: 44,985 of 475,688 residue rows (9.5%) had no area code, across 14
# labels, every one of which is a common English short form of an area the
# crosswalk holds under a long form — "Netherlands" against "Netherlands
# (Kingdom of the)", "Tanzania" against "United Republic of Tanzania", "United
# Kingdom" against "United Kingdom of Great Britain and Northern Ireland". The
# codes were reachable; the spellings were not.
#
# THIS ONLY FILLS NA. A row the exact join matched keeps its code, so the
# crosswalk stays authoritative wherever it has an answer and this cannot
# silently move existing data.
#
# Resolution is year-aware because the answer depends on the year: "Czech
# Republic" is Czechia (area 167) from 1993 and Czechoslovakia (area 51) before
# it. Rows are resolved per distinct (label, year) pair rather than per row —
# the residue table alone would otherwise resolve the same handful of labels
# hundreds of thousands of times.
.fill_area_code_via_polities <- function(table, name_column, code_column) {
  if (
    !rlang::has_name(table, code_column) || !rlang::has_name(table, name_column)
  ) {
    return(table)
  }

  unresolved <- is.na(table[[code_column]]) & !is.na(table[[name_column]])
  if (!any(unresolved)) {
    return(table)
  }

  key_column <- .resolve_polity_key(table, code_column)
  # as.data.frame first: `.current_area_lookup()` returns a data.table, where
  # `dt[i, c("a", "b")]` EVALUATES j and hands back the character vector itself
  # rather than those two columns.
  lookup <- as.data.frame(.current_area_lookup(include_unmapped = TRUE))
  lookup <- lookup[
    !is.na(lookup$polity_code) & !is.na(lookup[[key_column]]),
    c("polity_code", key_column)
  ]
  # A polity mapping to more than one reporting code cannot be resolved this way
  # without choosing for the caller, so it is dropped rather than guessed. That
  # is 3 polities against 215 reachable ones, and both named cases are ones where
  # a guess would be wrong: ETH-1993-2025 spans FAOSTAT areas 62 and 238, and
  # ROW-1850-2023 is the rest-of-world aggregate standing in for dozens of areas.
  lookup <- unique(lookup)
  ambiguous <- lookup$polity_code[duplicated(lookup$polity_code)]
  lookup <- lookup[!lookup$polity_code %in% ambiguous, ]
  if (nrow(lookup) == 0L) {
    return(table)
  }

  has_year <- rlang::has_name(table, "year")
  keys <- unique(data.frame(
    .label = table[[name_column]][unresolved],
    .year = if (has_year) {
      as.integer(table[["year"]][unresolved])
    } else {
      NA_integer_
    },
    stringsAsFactors = FALSE
  ))
  keys$polity_code <- resolve_polity_label(keys$.label, year = keys$.year)
  keys <- keys[!is.na(keys$polity_code), , drop = FALSE]
  if (nrow(keys) == 0L) {
    return(table)
  }

  keys <- merge(keys, lookup, by = "polity_code", all.x = FALSE)
  if (nrow(keys) == 0L) {
    return(table)
  }
  # Match on the (label, year) pair, and only for the rows this pass is allowed to
  # touch. Keying every row would let a row whose name is NA paste to the literal
  # "NA" and collide with a resolved label of that spelling — unlikely, and cheaper
  # to exclude than to reason about.
  row_key <- paste0(
    table[[name_column]],
    "\r",
    if (has_year) as.integer(table[["year"]]) else NA_integer_
  )
  row_key[!unresolved] <- NA_character_
  filled <- keys[[key_column]][
    match(row_key, paste0(keys$.label, "\r", keys$.year))
  ]
  table[[code_column]] <- dplyr::coalesce(
    table[[code_column]],
    methods::as(filled, class(table[[code_column]])[1])
  )
  table
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

.get_polities <- function(name_column, code_column, join_column, table = NULL) {
  key_column <- .resolve_polity_key(table, code_column)

  .current_area_lookup(include_unmapped = TRUE) |>
    tibble::as_tibble() |>
    dplyr::arrange(
      dplyr::desc(polity_end_year),
      dplyr::desc(polity_start_year),
      dplyr::desc(area_code)
    ) |>
    dplyr::select(
      !!name_column := area_name,
      !!code_column := dplyr::all_of(key_column)
    ) |>
    dplyr::filter(!is.na(.data[[code_column]])) |>
    dplyr::distinct(
      dplyr::across(dplyr::all_of(join_column)),
      .keep_all = TRUE
    )
}

.resolve_polity_key <- function(table, code_column) {
  if (is.null(table) || !rlang::has_name(table, code_column)) {
    return("area_code")
  }

  codes <- table[[code_column]]

  if (!is.character(codes)) {
    return("area_code")
  }

  codes <- unique(stats::na.omit(codes))

  if (length(codes) == 0L) {
    return("area_code")
  }

  looks_numeric <- stringr::str_detect(codes, "^[0-9]+$")

  if (all(looks_numeric)) {
    return("area_code")
  }

  "area_iso3c"
}

.get_cbs_items <- function(name_column, code_column) {
  whep::items_cbs |>
    dplyr::select(
      !!name_column := item_cbs_name,
      !!code_column := item_cbs_code
    ) |>
    dplyr::distinct(
      dplyr::across(dplyr::all_of(code_column)),
      .keep_all = TRUE
    )
}

.get_prod_items <- function(name_column, code_column) {
  whep::items_prod |>
    dplyr::select(
      !!name_column := item_prod_name,
      !!code_column := item_prod_code
    ) |>
    dplyr::distinct(
      dplyr::across(dplyr::all_of(code_column)),
      .keep_all = TRUE
    )
}
