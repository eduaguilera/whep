build_supply_use_table <- function() {
  # TODO: There's a name mismatch in two items
  # CBS_item                  supply_process_item
  # Fodder cereal and grasses Fodder crops
  # Fodder legumes            Grazing
  # Keep balance sheet names for now
  supply_process_table <- .get_processes_table("input/raw/items_supply.csv")
  use_process_table <- .get_processes_table("input/raw/items_use.csv")
  coeffs <- get_processing_coefs(get_file_path("processing_coefs"))

  processes_table <- use_process_table |>
    dplyr::full_join(
      supply_process_table,
      by = c("proc", "proc_code"),
      relationship = "many-to-many",
      suffix = c("_to_process", "_processed")
    )

  dplyr::bind_rows(
    .process_feed(processes_table),
    .process_seed(processes_table),
    .process_slaughtering(processes_table),
    .process_others(processes_table, coeffs)
  )
}

.process_feed <- function(processes_table) {
  tibble::tibble()
}

.process_seed <- function(processes_table) {
  tibble::tibble()
}

.process_slaughtering <- function(processes_table) {
  tibble::tibble()
}

.process_others <- function(processes_table, coeffs) {
  processes <- processes_table |>
    dplyr::filter(!type %in% c("feed", "seedwaste", "slaughtering")) |>
    # TODO: compare with full_join, deal with missing processes
    dplyr::inner_join(coeffs, by = c("item_processed", "item_to_process"))

  supply <- processes |>
    dplyr::group_by(year, area, proc, item_processed) |>
    dplyr::summarise(value = sum(final_value_processed)) |>
    dplyr::rename(item = item_processed) |>
    dplyr::mutate(type = "supply")

  use <- processes |>
    dplyr::distinct(year, area, proc, item_to_process, value_to_process) |>
    dplyr::rename(item = item_to_process, value = value_to_process) |>
    dplyr::mutate(type = "use")

  dplyr::bind_rows(supply, use)
}

.get_processes_table <- function(csv_path) {
  "extdata" |>
    system.file(csv_path, package = utils::packageName()) |>
    readr::read_csv(show_col_types = FALSE)
}
