#' Create a new dataframe where each row has a year range into one where each
#' row is a single year, effectively 'expanding' the whole year range
#'
#' @param trade_sources A tibble dataframe
#' where each row contains the year range
#'
#' @return A tibble dataframe where each row
#' corresponds to a single year for a given source
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
        Info_Format == "year", paste(Name, Year, sep = "_"), Name
      ),
      ImpExp = `Imp/Exp`,
      In_Saco = as.integer(!is.na(SACO_link)),
    )
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
