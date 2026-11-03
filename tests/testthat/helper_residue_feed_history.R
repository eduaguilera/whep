.fi_residue_history <- function() {
  tibble::tribble(
    ~year, ~area_code, ~polity_area_code, ~item_cbs_code_crop,
    ~item_cbs_code_residue, ~value, ~value_dm, ~method_residue,
    1961L, 203L, 203L, 2601L, 2106L, 1000, 132, "pin",
    1961L, 203L, 203L, 2547L, 2106L, 1000, 900, "pin"
  )
}

.fi_residue_crops <- function() {
  tibble::tribble(
    ~year, ~area_code, ~item_prod_code, ~item_cbs_code, ~unit, ~value,
    1955L, 203L, 388L, 2601L, "tonnes", 500,
    1955L, 203L, 187L, 2547L, "tonnes", 100,
    1961L, 203L, 388L, 2601L, "tonnes", 1000,
    1961L, 203L, 187L, 2547L, "tonnes", 1000
  )
}
