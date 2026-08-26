# Report where row evidence disagrees.

Find the `(row_key, field)` pairs carrying more than one distinct value,
i.e. the places where two producers, or two runs of one producer, made
different claims about the same row. Reports rather than aborts, so a
caller can decide which disagreements matter; `nrow() == 0` is the clean
result.

A repeated *identical* claim is not a conflict, so re-recording the same
evidence is harmless.

## Usage

``` r
evidence_conflicts(evidence)
```

## Arguments

- evidence:

  Evidence table, validated against
  [`row_evidence_schema()`](https://eduaguilera.github.io/whep/reference/row_evidence_schema.md).

## Value

A tibble with one row per conflicting `(row_key, field)`, ordered by
both:

- `row_key`, `field`: The row and evidence field in dispute.

- `n_values`: Number of distinct values claimed. `NA` counts as a value
  of its own, since "unknown" and "0.5" are a disagreement.

- `values`: The claimed values, `" | "`-separated, with `NA` rendered as
  `"NA"`.

- `source_ids`: The producers involved, comma-separated.

## Examples

``` r
rows <- tibble::tibble(area_code = 724L, year = 2020L)
key <- c("area_code", "year")
recorded_at <- as.POSIXct("2026-01-01", tz = "UTC")
agreeing <- combine_row_evidence(
  row_evidence(rows, "A", key, list(flag = "E"), recorded_at = recorded_at),
  row_evidence(rows, "B", key, list(flag = "E"), recorded_at = recorded_at)
)
evidence_conflicts(agreeing)
#> # A tibble: 0 × 5
#> # ℹ 5 variables: row_key <chr>, field <chr>, n_values <int>, values <chr>,
#> #   source_ids <chr>

disagreeing <- combine_row_evidence(
  agreeing,
  row_evidence(rows, "C", key, list(flag = "A"), recorded_at = recorded_at)
)
evidence_conflicts(disagreeing)
#> # A tibble: 1 × 5
#>   row_key         field n_values values source_ids
#>   <chr>           <chr>    <int> <chr>  <chr>     
#> 1 "724\u001f2020" flag         2 A | E  A, B, C   
```
