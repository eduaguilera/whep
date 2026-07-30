#' Trade data sources
#'
#' @description
#' Create a new dataframe where each row has a year range into one where each
#' row is a single year, effectively 'expanding' the whole year range.
#'
#' @param trade_sources A tibble dataframe where each row contains the
#'   year range.
#'
#' @returns A tibble dataframe where each row corresponds to a single year for
#'   a given source.
#'
#' @inheritSection whep_polity_columns Polity columns
#'
#' @export
#'
#' @examples
#' trade_sources <- tibble::tibble(
#'   Name = c("a", "b", "c"),
#'   Trade = c("t1", "t2", "t3"),
#'   Info_Format = c("year", "partial_series", "year"),
#'   Timeline_Start = c(1, 1, 2),
#'   Timeline_End = c(3, 4, 5),
#'   Timeline_Freq = c(1, 1, 2),
#'   `Imp/Exp` = "Imp",
#'   SACO_link = NA,
#' )
#' expand_trade_sources(trade_sources)
expand_trade_sources <- function(trade_sources) {
  non_na_cols <- c("Trade", "Timeline_Start", "Timeline_End", "Timeline_Freq")
  trade_sources |>
    dplyr::filter(!.any_na_col(non_na_cols)) |>
    .expand_trade_years() |>
    dplyr::mutate(
      Name = dplyr::if_else(
        Info_Format == "year",
        paste(Name, Year, sep = "_"),
        Name
      ),
      ImpExp = `Imp/Exp`,
      In_Saco = as.integer(!is.na(SACO_link)),
    ) |>
    .add_reporter_polity()
}

# Attach the polity each expanded row's reporter names IN THAT YEAR.
#
# This function's output was the last exported area-keyed table in the package with no
# polity on it. Its rows are already one-year-per-row, which is exactly the shape
# year-aware resolution needs, so the reporter and the Year settle it with no
# interpolation.
#
# Resolution goes through the alias table under source "trade-sources", added upstream
# (whep-polities#39) rather than by name-matching here, because that repository owns
# label-to-polity identity. 34 aliases over the 8 reporters, one per polity period the
# reporter's own year span crosses:
#
#   United Kingdom  2 periods   Germany  8    United States  4    India  3
#   France          3           Canada   3    Egypt          1    China  10
#
# The two reporters spelled with a trailing "(the)" -- the United Kingdom and the
# United States -- differ from the canonical area names by nothing else, which is
# precisely what the alias table is for.
#
# CHINA IS THE ONE THAT COULD NOT GO THROUGH AN AREA. Its FAOSTAT area 351 is the
# deliberate China aggregate that maps to no polity, so an area-mediated lookup returns
# nothing for it. The aliases target the CHN chain directly, which the alias table
# permits because it maps labels to polities, not to areas. The superseded
# CHN-1921-1945 is excluded, so the 1921-1945 span resolves through CHN-1921-1932 and
# CHN-1932-1945 rather than a row that may never receive data.
#
# `Reporter` may be absent: expand_trade_sources() is exported and documented with an
# example that has no such column, so a missing reporter yields NA rather than an error.
.add_reporter_polity <- function(x) {
  if (!all(c("Reporter", "Year") %in% names(x))) {
    return(dplyr::mutate(x, reporting_polity_code = NA_character_))
  }
  years <- unique(stats::na.omit(as.integer(x$Year)))
  resolved <- lapply(years, function(y) {
    data.frame(
      .yr = y,
      .lbl = unique(as.character(x$Reporter)),
      stringsAsFactors = FALSE
    ) |>
      (\(d) {
        d$reporting_polity_code <- resolve_polity_label(
          d$.lbl,
          source = "trade-sources",
          year = y
        )
        d
      })()
  })
  key <- do.call(rbind, resolved)
  x$.yr <- as.integer(x$Year)
  x$.lbl <- as.character(x$Reporter)
  out <- dplyr::left_join(x, key, by = c(".yr", ".lbl"))
  dplyr::select(out, -".yr", -".lbl")
}

.expand_trade_years <- function(trade_sources) {
  trade_sources <- dplyr::mutate(trade_sources, No = dplyr::row_number())

  trade_sources |>
    dplyr::group_by(No) |>
    tidyr::expand(Year = seq(Timeline_Start, Timeline_End, Timeline_Freq)) |>
    dplyr::inner_join(trade_sources, by = "No")
}

.any_na_col <- function(cols_to_check) {
  dplyr::if_any(dplyr::all_of(cols_to_check), is.na)
}
