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
#' - `proc`: Natural language name of the process taking place.
#' - `item_cbs`: Natural language name of the item taking part in the process.
#' - `type`: Can have two values:
#'    - `use`: The given item is an input of the process.
#'    - `supply`: The given item is an output of the process.
#' - `value`: Quantity in tonnes.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' build_supply_use()
#' }
build_supply_use <- function() {
  .build_supply_use_from_inputs(
    supply_processes = .read_local_csv("input/raw/items_supply.csv"),
    use_processes = .read_local_csv("input/raw/items_use.csv"),
    coeffs = get_processing_coefs(get_file_path("processing_coefs")),
    cbs = get_wide_cbs(get_file_path("commodity_balance_sheet")),
    crop_residues = get_primary_residues(get_file_path("crop_residues")),
    primary_prod = get_primary_production(get_file_path("primary_prod"))
  )
}

.build_supply_use_from_inputs <- function(
    supply_processes,
    use_processes,
    coeffs,
    cbs,
    crop_residues,
    primary_prod) {
  processes_table <- .join_supply_use_processes(supply_processes, use_processes)

  dplyr::bind_rows(
    .build_supply(processes_table, crop_residues, primary_prod)
  )
  # .add_use_for_feed(processes_table) |>
  # .add_use_for_seed(processes_table, cbs) |>
  # .add_supply_use_for_processing(processes_table, coeffs) |>
  # .add_rest_of_supply(supply_processes, cbs)
}

.build_supply <- function(processes_table, crop_residues, primary_prod) {
  dplyr::bind_rows(
    .build_supply_crop_residue(processes_table, crop_residues),
    .build_supply_crop_product(processes_table, primary_prod)
  ) |>
    dplyr::mutate(type = "supply")
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

.build_supply_crop_residue <- function(processes_table, crop_residues) {
  processes_table <- processes_table |>
    dplyr::filter(data_group == "crop_residue")

  browser()

  crop_residues <- crop_residues |>
    dplyr::filter(Product_residue == "Residue") |>
    dplyr::summarise(
      value = sum(Prod_ygpit_Mg),
      .by = c(Year, area, item_cbs_crop, item_cbs)
    ) |>
    dplyr::filter(value > 0) |>
    dplyr::left_join(
      processes_table,
      dplyr::join_by(
        item_cbs_crop == item_cbs_to_process,
        item_cbs == item_cbs_processed
      )
    ) |>
    dplyr::select(
      year = Year,
      area,
      item_cbs_to_process = item_cbs_crop,
      item_cbs_processed = item_cbs,
      proc,
      value
    )

  no_process_found <- crop_residues |>
    dplyr::filter(is.na(proc)) |>
    dplyr::pull(item_cbs_to_process) |>
    unique()

  if (length(no_process_found) > 0) {
    items <- paste0(no_process_found, collapse = ", ")
    warning(stringr::str_glue("No process found for items {items}"))
  }

  crop_residues |>
    dplyr::select(-item_cbs_to_process, item_cbs = item_cbs_processed)
}

.build_supply_crop_product <- function(processes_table, primary_prod) {
  primary_prod <- primary_prod |>
    dplyr::filter(unit == "tonnes")

  processes_table <- processes_table |>
    dplyr::filter(data_group == "crop_product")

  multiproduct_crop_processes <- processes_table |>
    dplyr::filter(!is.na(item_prod_code))

  singleproduct_crop_processes <- processes_table |>
    dplyr::filter(is.na(item_prod_code))

  multiproduct_crops <- .build_multiproduct_crops(
    multiproduct_crop_processes,
    primary_prod
  )
  singleproduct_crops <- .build_singleproduct_crops(
    multiproduct_crop_processes,
    singleproduct_crop_processes,
    primary_prod
  )

  dplyr::bind_rows(singleproduct_crops, multiproduct_crops)
}

.build_multiproduct_crops <- function(
    multiproduct_crop_processes,
    primary_prod) {
  multiproduct_crop_processes |>
    dplyr::left_join(primary_prod, "item_prod_code") |>
    dplyr::select(
      year, area_code, proc_code,
      item_cbs_code = item_cbs_code_processed, value
    )
}

.build_singleproduct_crops <- function(
    multiproduct_crop_processes,
    singleproduct_crop_processes,
    primary_prod) {
  primary_prod |>
    dplyr::anti_join(multiproduct_crop_processes, "item_prod_code") |>
    dplyr::summarise(
      value = sum(value),
      .by = c(year, area_code, item_cbs_code)
    ) |>
    dplyr::right_join(
      singleproduct_crop_processes,
      dplyr::join_by(item_cbs_code == item_cbs_code_processed)
    ) |>
    dplyr::select(year, area_code, proc_code, item_cbs_code, value)
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
    dplyr::filter(!type %in% c("feed", "seedwaste")) |>
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
