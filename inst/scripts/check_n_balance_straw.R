library(whep)
library(dplyr)

n_balance <- whep_read_file("n_balance_ygpit_all")
crop_area <- whep_read_file("crop_area_npp_ygpitr_no_fallow")

cat("n_balance columns include Item:", "Item" %in% names(n_balance), "\n")
cat("n_balance columns include Name_biomass:", "Name_biomass" %in% names(n_balance), "\n")
cat(
  "n_balance rows with straw token:",
  sum(
    grepl("straw", apply(n_balance, 1, paste, collapse = "|"), ignore.case = TRUE)
  ),
  "\n"
)

if ("Item" %in% names(n_balance)) {
  cat("n_balance Item==Straw rows:", nrow(filter(n_balance, Item == "Straw")), "\n")
}

if ("Item" %in% names(crop_area)) {
  cat("crop_area Item==Straw rows:", nrow(filter(crop_area, Item == "Straw")), "\n")
}

if (all(c("Item", "Product_residue") %in% names(crop_area))) {
  print(
    crop_area |>
      filter(Item == "Straw") |>
      count(Product_residue, name = "rows")
  )
}
