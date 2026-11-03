# .calculate_lu_total --------------------------------------------------------

test_that(".calculate_lu_total deduplicates repeated per-product stock rows", {
  # stock_prod_ygps carries one row per derived product, each repeating the
  # same province-level Stock_Number (e.g. Pigs: offal, fat, meat, lard).
  # Summing without deduplicating first inflates the head count by however
  # many product rows exist for that Livestock_cat.
  livestock_prod_ygps <- tibble::tribble(
    ~Year, ~Province_name, ~Livestock_cat, ~item_prod, ~Stock_Number,
    2020, "A", "Pigs", "Offals, Edible", 1000,
    2020, "A", "Pigs", "Fats, Animals, Raw", 1000,
    2020, "A", "Pigs", "Pigmeat", 1000,
    2020, "A", "Pigs", "Fats, Animals, Raw (rendered)", 1000,
    2020, "A", "Sheep", "Meat", 200,
    2020, "A", "Sheep", "Offal", 200
  )

  livestockcat_to_class <- tibble::tribble(
    ~Livestock_cat, ~Animal_class,
    "Pigs", "Pigs",
    "Sheep", "Sheep_goats"
  )

  lu_mapping <- tibble::tribble(
    ~Animal_class, ~LU_head,
    "Pigs", 0.3,
    "Sheep_goats", 0.1
  )

  out <- .calculate_lu_total(
    livestock_prod_ygps,
    livestockcat_to_class,
    lu_mapping
  )

  # 1000 head * 0.3 + 200 head * 0.1 = 320, not (1000*4)*0.3 + (200*2)*0.1
  expect_equal(out$LU_total[out$Province_name == "A"], 1000 * 0.3 + 200 * 0.1)
})

test_that(".calculate_lu_total handles unmapped Livestock_cat as zero LU", {
  livestock_prod_ygps <- tibble::tribble(
    ~Year, ~Province_name, ~Livestock_cat, ~Stock_Number,
    2020, "A", "Other_birds", 5000
  )

  livestockcat_to_class <- tibble::tribble(
    ~Livestock_cat, ~Animal_class,
    "Pigs", "Pigs"
  )

  lu_mapping <- tibble::tribble(
    ~Animal_class, ~LU_head,
    "Pigs", 0.3
  )

  out <- .calculate_lu_total(
    livestock_prod_ygps,
    livestockcat_to_class,
    lu_mapping
  )

  expect_equal(out$LU_total, 0)
})
