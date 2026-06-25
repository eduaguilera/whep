#' Read a WHEP coefficient table.
#'
#' Reads one of the coefficient tables shipped as CSV under
#' `inst/extdata/coefs`. These tables are small and versioned inside the
#' package (read at runtime, not stored remotely), so no download is needed.
#'
#' @param name Coefficient table name (the file stem), for example
#'   `"bio_coefs"` or `"ipcc_residue_coefs"`.
#' @return A tibble with the coefficient table.
#' @export
#' @examples
#' whep_coef_table("residue_feed_fraction")
whep_coef_table <- function(name) {
  available <- .coef_table_names()
  if (!name %in% available) {
    cli::cli_abort(c(
      "Unknown coefficient table {.val {name}}.",
      i = "Available tables: {.val {available}}."
    ))
  }
  .read_coef(name)
}

.read_coef <- function(name) {
  out <- system.file(
    "extdata",
    "coefs",
    paste0(name, ".csv"),
    package = "whep"
  ) |>
    data.table::fread(na.strings = "") |>
    tibble::as_tibble()
  # The item key is character in some tables (e.g. "Fallow") and numeric in
  # others; coerce to character everywhere so joins on it never mismatch.
  if (rlang::has_name(out, "item_prod_code")) {
    out$item_prod_code <- as.character(out$item_prod_code)
  }
  out
}

.coef_table_names <- function() {
  system.file("extdata", "coefs", package = "whep") |>
    list.files(pattern = "\\.csv$") |>
    stringr::str_remove("\\.csv$")
}
