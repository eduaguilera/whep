# TODO: Revisit the whole file. I'm pretty sure there are mistakes,
# along with incomplete parts. It's better to first finish the model
# and then come back here.

#' Supply and use tables
#'
#' @description
#' Create a table with processes, their inputs (_use_) and their
#' outputs (_supply_).
#'
#' @returns
#' A tibble with the supply and use data for processes.
#' It contains the following columns:
#' - `year`: The year in which the recorded event occurred.
#' - `area`: The name of the country where the data is from.
#' - `area_code`: FAOSTAT internal code for each country. Equivalences
#'    with ISO 3166-1 numeric can be found in the _Area Codes_ CSV from the
#'    zip file that can be downloaded from
#'    [FAOSTAT](https://www.fao.org/faostat/en/#data/FBS). TODO: Think about
#'    this, would be nice to use ISO3 codes but won't be enough for our periods
#' - `item`: Natural language name for the item.
#' - `item_code`: FAOSTAT internal code for each item.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' build_supply_use()
#' }
build_supply_use <- function() {
  # TODO: There's a name mismatch in two items
  # CBS_item                  supply_process_item
  # Fodder cereal and grasses Fodder crops
  # Fodder legumes            Grazing
  # Keep balance sheet names for now

  .build_supply_use_from_inputs(
    supply_processes = .read_local_csv("input/raw/items_supply.csv"),
    use_processes = .read_local_csv("input/raw/items_use.csv"),
    coeffs = get_processing_coefs(get_file_path("processing_coefs")),
    cbs = get_wide_cbs(get_file_path("commodity_balance_sheet"))
  )
}

.build_supply_use_from_inputs <- function(
    supply_processes,
    use_processes,
    coeffs,
    cbs) {
  processes_table <- .join_supply_use_processes(supply_processes, use_processes)

  tibble::tibble() |>
    .add_use_for_feed(processes_table) |>
    .add_use_for_seed(processes_table, cbs) |>
    .add_use_for_slaughtering(processes_table) |>
    .add_supply_use_for_processing(processes_table, coeffs) |>
    .add_rest_of_supply(supply_processes, cbs)
}

.join_supply_use_processes <- function(supply_processes, use_processes) {
  use_processes |>
    dplyr::full_join(
      supply_processes,
      by = c("proc", "proc_code"),
      relationship = "many-to-many",
      suffix = c("_to_process", "_processed")
    )
}

.add_rest_of_supply <- function(supply_use, supply_process_table, cbs) {
  items_done <- supply_use |>
    dplyr::filter(type == "supply") |>
    dplyr::distinct(item)

  cbs |>
    dplyr::anti_join(items_done, "item") |>
    dplyr::inner_join(supply_process_table, "item") |>
    dplyr::filter(domestic_supply > 0) |>
    dplyr::select(year, area, proc, item, domestic_supply) |>
    dplyr::rename(value = domestic_supply) |>
    dplyr::mutate(type = "supply") |>
    dplyr::bind_rows(supply_use)
}

.add_use_for_seed <- function(supply_use, processes_table, cbs) {
  processes_table |>
    dplyr::filter(type == "seedwaste") |>
    # TODO: compare with full_join, why seedwaste use on animal products?
    dplyr::inner_join(cbs, dplyr::join_by(item_code_to_process == item_code)) |>
    dplyr::filter(seed > 0) |>
    dplyr::select(year, area, proc, item_to_process, seed) |>
    dplyr::rename(item = item_to_process, value = seed) |>
    dplyr::mutate(type = "use") |>
    dplyr::bind_rows(supply_use)
}

# TODO: Treat slaughtering processes
# (probably need conversion factor from Livestock Units to physical unit)
.add_use_for_slaughtering <- function(supply_use, processes_table) {
  supply_use
}

# TODO: Treat animal feed use processes
# Use feed intake data from Eduardo
.add_use_for_feed <- function(supply_use, processes_table) {
  supply_use
}

.add_supply_use_for_processing <- function(
    supply_use,
    processes_table,
    coeffs) {
  processes <- processes_table |>
    dplyr::filter(!type %in% c("feed", "seedwaste", "slaughtering")) |>
    # TODO: compare with full_join, deal with missing processes
    dplyr::inner_join(coeffs, by = c("item_processed", "item_to_process"))

  supply <- processes |>
    dplyr::group_by(year, area, proc, item_processed) |>
    dplyr::summarise(value = sum(final_value_processed)) |>
    dplyr::ungroup() |>
    dplyr::rename(item = item_processed) |>
    dplyr::mutate(type = "supply")

  use <- processes |>
    dplyr::distinct(year, area, proc, item_to_process, value_to_process) |>
    dplyr::rename(item = item_to_process, value = value_to_process) |>
    dplyr::mutate(type = "use")

  dplyr::bind_rows(supply, use, supply_use)
}
