# Complete a table to the admin-shares contract

Complete `x` onto
[`admin_shares_prototype()`](https://eduaguilera.github.io/whep/reference/admin_shares_prototype.md)
with
[`ensure_columns()`](https://eduaguilera.github.io/whep/reference/ensure_columns.md)
– adding absent columns as typed missing values and casting present ones
to the contract's types – then prove the result against
[`admin_shares_schema()`](https://eduaguilera.github.io/whep/reference/admin_shares_schema.md)
with
[`assert_table_schema()`](https://eduaguilera.github.io/whep/reference/assert_table_schema.md),
which aborts naming the offending columns and values when it does not
conform.

Two rules are proved here rather than in the schema, because a
[`check_table_schema()`](https://eduaguilera.github.io/whep/reference/check_table_schema.md)
specification speaks of one column at a time and skips a missing value
on every bound:

- Every row must carry `value`, `share`, or both. A row with neither
  aborts with class `whep_error_admin_no_measure`. A row with `share`
  alone is accepted – see the *Shares-only rows* section of
  [`admin_shares_schema()`](https://eduaguilera.github.io/whep/reference/admin_shares_schema.md),
  and never repair one by inventing a value.

- Neither measurement may be non-finite. `NaN` or `Inf` aborts with
  class `whep_error_admin_nonfinite`, because `is.na(NaN)` is `TRUE` and
  a `NaN` would otherwise travel as a shares-only row.

## Usage

``` r
ensure_admin_shares(x)
```

## Arguments

- x:

  Tibble to complete. May already carry extra columns or be missing
  contract columns; see
  [`ensure_columns()`](https://eduaguilera.github.io/whep/reference/ensure_columns.md).

## Value

`x` completed to the admin-shares contract, invisibly.

## Examples

``` r
# Omits columns the contract allows missing: `share`, `nuts_version`,
# `source_native_id`, `source_native_name`, `source_version` and
# `value_flag`. (`value` is allow-missing too, but a row must carry it
# or `share`, so this valued fixture keeps it.) `level_polity_code` is
# part of the key, so it must stay present and distinct per row even
# though the contract allows it to be `NA` for a genuinely unresolved
# row.
partial <- tibble::tibble(
  area_code = c(840L, 840L),
  level_polity_code = c("USA-IOWA", "USA-ILLINOIS"),
  level = c(1L, 1L),
  item_prod_code = c(44L, 44L),
  indicator_used = c("area_harvested", "area_harvested"),
  year = c(2020L, 2020L),
  value = c(1000000, 800000),
  source = c("USDA_NASS", "USDA_NASS"),
  tier = c(1L, 1L),
  grain = c("admin1", "admin1"),
  concept_break = c(FALSE, FALSE),
  source_id = c("USDA_NASS", "USDA_NASS"),
  recorded_at = "2026-01-01T00:00:00Z",
  treatment_year = c("observed", "observed")
)
completed <- ensure_admin_shares(partial)
names(completed)
#>  [1] "area_code"          "level_polity_code"  "level"             
#>  [4] "item_prod_code"     "species_group"      "indicator_used"    
#>  [7] "year"               "value"              "share"             
#> [10] "source"             "tier"               "grain"             
#> [13] "concept_break"      "nuts_version"       "source_native_id"  
#> [16] "source_native_name" "source_id"          "source_version"    
#> [19] "recorded_at"        "treatment_year"     "treatment_value"   
#> [22] "value_flag"        
nrow(check_table_schema(completed, admin_shares_schema()))
#> [1] 0
```
