% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/code_names.R
\name{add_area_name}
\alias{add_area_name}
\title{Get area names from area codes}
\usage{
add_area_name(table, code_column = "area_code", name_column = "area_name")
}
\arguments{
\item{table}{The table that will be modified with a new column.}

\item{code_column}{The name of the column in \code{table} containing the codes.}

\item{name_column}{The name of the output column containing the names.}
}
\value{
A tibble with all the contents of \code{table} and an extra column
named \code{name_column}, which contains the names. If there is no name match,
an \code{NA} is included.
}
\description{
Add a new column to an existing tibble with the corresponding name
for each code. The codes are assumed to be from those defined by
the \code{FABIO} model, which them themselves come from \code{FAOSTAT} internal
codes. Equivalences with ISO 3166-1 numeric can be found in the
\emph{Area Codes} CSV from the zip file that can be downloaded from
\href{https://www.fao.org/faostat/en/#data/FBS}{FAOSTAT}. TODO: Think about
this, would be nice to use ISO3 codes but won't be enough for our periods.
}
\examples{
table <- tibble::tibble(area_code = c(1, 2, 4444, 3))

add_area_name(table)

table |>
  dplyr::rename(my_area_code = area_code) |>
  add_area_name(code_column = "my_area_code")

add_area_name(table, name_column = "my_custom_name")
}
