#' Compute a footprint end-to-end from an extension table.
#'
#' @description
#' Trace a long-format environmental extension table through the supply chain
#' for one or more years and return a tidy footprint. This wraps the three
#' steps that the footprint driver scripts used to repeat inline: build (or
#' reuse) the input-output model with [build_io_model()], align the extension
#' to each year's sector labels with [align_extension()], and trace it with
#' [compute_footprint()].
#'
#' The `extension` table is the output of any `build_*_extension()` builder,
#' such as [build_grassland_land_extension()] or
#' [build_livestock_ghg_extension()]: rows keyed by `year`, `area_code` and
#' `item_cbs_code`, with the pressure magnitude in `value_col`.
#'
#' @param extension Long-format extension tibble with columns `year`,
#'   `area_code`, `item_cbs_code` and the column named by `value_col`.
#' @param years Years to compute. Defaults to the distinct years present in
#'   `extension`. Ignored when `io` is supplied.
#' @param io Optional pre-built [build_io_model()] result (a tibble with one
#'   row per year). Supply it to reuse one model across several extensions
#'   instead of rebuilding it. When `NULL` (default), it is built for `years`.
#' @param value_col Name of the extension magnitude column, `"impact_u"` by
#'   default.
#' @param ... Further arguments passed to [compute_footprint()] (e.g.
#'   `conserve_extensions`, `report_conservation`).
#'
#' @return A tibble of footprint flows as returned by [compute_footprint()],
#'   with an added `year` column.
#'
#' @export
#'
#' @examples
#' io <- tibble::tibble(
#'   year = 2000L,
#'   Z = list(matrix(c(0, 5, 10, 0), nrow = 2)),
#'   X = list(c(100, 200)),
#'   Y = list(matrix(c(85, 195), ncol = 1)),
#'   labels = list(tibble::tibble(
#'     index = 1:2,
#'     area_code = c(1L, 1L),
#'     item_cbs_code = c(1L, 2L)
#'   )),
#'   fd_labels = list(tibble::tibble(area_code = 1L, fd_col = "food"))
#' )
#' extension <- tibble::tibble(
#'   year = 2000L,
#'   area_code = 1L,
#'   item_cbs_code = c(1L, 2L),
#'   impact_u = c(50, 30)
#' )
#' build_footprint(extension, io = io)
build_footprint <- function(
  extension,
  years = NULL,
  io = NULL,
  value_col = "impact_u",
  ...
) {
  .check_extension_table(extension, value_col)
  if (is.null(io)) {
    if (is.null(years)) {
      years <- sort(unique(extension$year))
    }
    io <- build_io_model(years = years)
  }
  .check_io_model(io)

  purrr::pmap(
    list(io$year, io$Z, io$X, io$Y, io$labels, io$fd_labels),
    function(yr, z_mat, x_vec, y_mat, labels, fd_labels) {
      compute_footprint(
        z_mat = z_mat,
        x_vec = x_vec,
        y_mat = y_mat,
        extensions = align_extension(extension, labels, yr, value_col),
        labels = labels,
        fd_labels = fd_labels,
        ...
      ) |>
        dplyr::mutate(year = yr)
    }
  ) |>
    dplyr::bind_rows()
}

#' Align an extension table to input-output sector labels.
#'
#' @description
#' Turn a long-format extension table into the dense per-sector numeric vector
#' that [compute_footprint()] expects, ordered to match a single year's
#' `labels` from [build_io_model()]. Sectors absent from the extension are
#' filled with zero, and extension rows outside the model are dropped. Rows
#' sharing an `(area_code, item_cbs_code)` are summed.
#'
#' @param extension Long-format extension tibble with `year`, `area_code`,
#'   `item_cbs_code` and the column named by `value_col`.
#' @param labels One year's `labels` tibble from [build_io_model()], with
#'   `area_code`, `item_cbs_code` and `index` columns.
#' @param year Year to select from `extension`.
#' @param value_col Name of the extension magnitude column, `"impact_u"` by
#'   default.
#'
#' @return A numeric vector with one entry per row of `labels`, ordered by the
#'   label `index`.
#'
#' @export
#'
#' @examples
#' extension <- tibble::tibble(
#'   year = 2000L,
#'   area_code = 1L,
#'   item_cbs_code = 10L,
#'   impact_u = 5
#' )
#' labels <- tibble::tibble(
#'   index = 1:2,
#'   area_code = c(1L, 1L),
#'   item_cbs_code = c(10L, 20L)
#' )
#' align_extension(extension, labels, 2000L)
align_extension <- function(extension, labels, year, value_col = "impact_u") {
  .check_extension_table(extension, value_col)
  if (!all(c("area_code", "item_cbs_code", "index") %in% names(labels))) {
    cli::cli_abort(
      "{.arg labels} must have columns {.field area_code},
      {.field item_cbs_code} and {.field index}."
    )
  }

  extension |>
    dplyr::filter(.data$year == .env$year) |>
    dplyr::summarise(
      value = sum(.data[[value_col]], na.rm = TRUE),
      .by = c(area_code, item_cbs_code)
    ) |>
    dplyr::right_join(labels, by = c("area_code", "item_cbs_code")) |>
    tidyr::replace_na(list(value = 0)) |>
    dplyr::arrange(.data$index) |>
    dplyr::pull(.data$value)
}

.check_extension_table <- function(extension, value_col) {
  required <- c("year", "area_code", "item_cbs_code", value_col)
  missing <- required[!required %in% names(extension)]
  if (length(missing) > 0L) {
    cli::cli_abort(
      "{.arg extension} is missing required column{?s}: {.field {missing}}."
    )
  }
}

.check_io_model <- function(io) {
  required <- c("year", "Z", "X", "Y", "labels", "fd_labels")
  missing <- required[!required %in% names(io)]
  if (length(missing) > 0L) {
    cli::cli_abort(
      "{.arg io} is missing required column{?s}: {.field {missing}}.
      Pass the result of {.fn build_io_model}."
    )
  }
}
