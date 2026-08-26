# Carry row evidence through a join or a filter.

Re-align an evidence table onto a table that has since been joined,
filtered, reordered or reduced: the evidence rows whose `row_key` still
occurs in `data` are returned, in canonical order, with their
`source_id` and `source_version` untouched. Evidence is therefore
narrowed by composition but never re-attributed by it.

The key is taken from the evidence's own `key_columns`, not from the
caller, so a table that no longer carries those columns — an aggregation
that dropped `year`, say — aborts instead of matching on whatever is
left. Rows of `data` that carry no evidence are a warning, not a silent
gap: after a left join they are exactly the rows whose provenance was
lost.

## Usage

``` r
evidence_for(data, evidence)
```

## Arguments

- data:

  Table the evidence is being carried onto. Must carry the evidence's
  key columns. Duplicated keys are allowed: a fan-out join does not
  change which claims apply.

- evidence:

  Evidence table, validated against
  [`row_evidence_schema()`](https://eduaguilera.github.io/whep/reference/row_evidence_schema.md).

## Value

A tibble of row evidence, in canonical order, containing the rows of
`evidence` that apply to `data`.

## Examples

``` r
rows <- tibble::tibble(area_code = c(724L, 76L), year = 2020L)
evidence <- row_evidence(
  rows,
  source_id = "FAOSTAT_prod",
  key = c("area_code", "year"),
  fields = list(fao_flag = c("A", "E")),
  recorded_at = as.POSIXct("2026-01-01", tz = "UTC")
)

# A filter narrows the evidence to the rows that survive.
evidence_for(dplyr::filter(rows, area_code == 724L), evidence)
#> # A tibble: 1 × 7
#>   row_key         key_columns   source_id source_version recorded_at field value
#>   <chr>           <chr>         <chr>     <chr>          <chr>       <chr> <chr>
#> 1 "724\u001f2020" area_code,ye… FAOSTAT_… NA             2026-01-01… fao_… A    

# A row with no evidence is reported rather than passed over.
extended <- dplyr::bind_rows(rows, tibble::tibble(
  area_code = 231L,
  year = 2020L
))
suppressWarnings(evidence_for(extended, evidence))
#> # A tibble: 2 × 7
#>   row_key         key_columns   source_id source_version recorded_at field value
#>   <chr>           <chr>         <chr>     <chr>          <chr>       <chr> <chr>
#> 1 "724\u001f2020" area_code,ye… FAOSTAT_… NA             2026-01-01… fao_… A    
#> 2 "76\u001f2020"  area_code,ye… FAOSTAT_… NA             2026-01-01… fao_… E    
```
