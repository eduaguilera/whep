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
#' @param method Co-product allocation method passed to [build_io_model()],
#'   `"mass"` (default) or `"value"`. Ignored when `io` is supplied (the model
#'   already encodes its allocation).
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
  method = c("mass", "value"),
  value_col = "impact_u",
  ...
) {
  method <- rlang::arg_match(method)
  .check_extension_table(extension, value_col)
  check_inputs_supplied(
    extension,
    stats::setNames(value_col, "extension magnitude"),
    details = c(
      i = "Every sector would be charged zero, and
           {.fn assert_footprint_invariants} would report the result as
           conserved, because zero satisfies a conservation identity."
    )
  )
  if (is.null(io)) {
    if (is.null(years)) {
      years <- sort(unique(extension$year))
    }
    io <- build_io_model(years = years, method = method)
  }
  .check_io_model(io)

  aligned <- purrr::map2(
    io$labels,
    io$year,
    \(labels, yr) align_extension(extension, labels, yr, value_col)
  )
  .check_extension_reach(aligned, io$year)

  purrr::pmap(
    list(io$year, io$Z, io$X, io$Y, io$labels, io$fd_labels, aligned),
    function(yr, z_mat, x_vec, y_mat, labels, fd_labels, ext_vec) {
      compute_footprint(
        z_mat = z_mat,
        x_vec = x_vec,
        y_mat = y_mat,
        extensions = ext_vec,
        labels = labels,
        fd_labels = fd_labels,
        ...
      ) |>
        dplyr::mutate(year = yr)
    }
  ) |>
    dplyr::bind_rows()
}

# Issue whep#1034: `align_extension()` zero-fills every sector that an
# extension does not reach, which is right, since a land extension does not
# charge a livestock sector.
# What it cannot distinguish is an extension that reaches NOTHING, because its
# keys were built on a vocabulary the IO model does not share, or because its
# own upstream label vanished. That state is invisible downstream by
# construction: the footprint is a zero vector, and
# `assert_footprint_invariants()` reports every origin "ok" and `rel_loss = 0`,
# because zero is conserved exactly.
#
# Reaching nothing in EVERY year is not an answer and aborts. Reaching nothing
# in some years is ordinary -- an extension's span is routinely shorter than the
# model's -- so that warns and names the years, rather than stopping a
# multi-decade run over its first decade.
.check_extension_reach <- function(aligned, years) {
  empty <- years[purrr::map_lgl(aligned, \(v) !any(v != 0, na.rm = TRUE))]
  if (length(empty) == 0L) {
    return(invisible(aligned))
  }
  reach <- c(
    x = "An extension that matches no sector is a zero footprint, and a zero
         footprint passes every conservation check there is."
  )
  if (length(empty) == length(years)) {
    cli::cli_abort(
      c(
        "The extension reaches no sector of the input-output model in any of
         its {length(years)} year{?s}.",
        reach,
        i = "Check that {.field area_code} and {.field item_cbs_code} are on
             the same vocabulary as {.fn build_io_model}'s labels, and that the
             extension's own inputs arrived."
      ),
      class = "whep_absent_input",
      absent = "extension reach"
    )
  }
  shown <- utils::head(sort(empty), 6L)
  cli::cli_warn(
    c(
      "The extension reaches no sector in {length(empty)} of
       {length(years)} {cli::qty(length(empty))}year{?s}, including
       {.val {shown}}.",
      reach,
      i = "Expected where the extension's span is shorter than the model's."
    ),
    class = "whep_absent_input",
    absent = as.character(empty)
  )
  invisible(aligned)
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
