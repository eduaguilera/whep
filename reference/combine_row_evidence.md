# Combine row evidence from several producers.

Merge evidence tables deterministically. Nothing is overwritten: the
result is the union of the claims, with exact duplicates collapsed and
the surviving rows in the format's canonical order, so the same inputs
always give the same output whatever order they arrive in.

Two producers claiming different values for the same row and field is
kept as what it is — two claims — instead of one silently winning by
argument position. Find those with
[`evidence_conflicts()`](https://eduaguilera.github.io/whep/reference/evidence_conflicts.md)
and decide explicitly.

All inputs must agree on `key_columns`: evidence keyed on
`(area_code, year)` and evidence keyed on `(area_code)` address
different things, and merging them would change what a `row_key` means.
That aborts rather than being reconciled.

## Usage

``` r
combine_row_evidence(...)
```

## Arguments

- ...:

  Evidence tables from
  [`row_evidence()`](https://eduaguilera.github.io/whep/reference/row_evidence.md),
  or lists of them. Each is validated against
  [`row_evidence_schema()`](https://eduaguilera.github.io/whep/reference/row_evidence_schema.md).
  Zero-row tables are allowed and contribute no rows or key constraint.

## Value

A tibble of row evidence, in canonical order.

## Examples

``` r
production <- tibble::tibble(area_code = c(724L, 76L), year = 2020L)
key <- c("area_code", "year")
recorded_at <- as.POSIXct("2026-01-01", tz = "UTC")

faostat <- row_evidence(
  production,
  source_id = "FAOSTAT_prod",
  key = key,
  fields = list(method_land = "reported"),
  recorded_at = recorded_at
)
luh2 <- row_evidence(
  production,
  source_id = "LUH2",
  key = key,
  fields = list(method_land = "back-cast"),
  recorded_at = recorded_at
)

combined <- combine_row_evidence(faostat, luh2)
combined
#> # A tibble: 4 × 7
#>   row_key         key_columns   source_id source_version recorded_at field value
#>   <chr>           <chr>         <chr>     <chr>          <chr>       <chr> <chr>
#> 1 "724\u001f2020" area_code,ye… FAOSTAT_… NA             2026-01-01… meth… repo…
#> 2 "724\u001f2020" area_code,ye… LUH2      NA             2026-01-01… meth… back…
#> 3 "76\u001f2020"  area_code,ye… FAOSTAT_… NA             2026-01-01… meth… repo…
#> 4 "76\u001f2020"  area_code,ye… LUH2      NA             2026-01-01… meth… back…

# Both claims survive, and the disagreement is visible.
evidence_conflicts(combined)
#> # A tibble: 2 × 5
#>   row_key         field       n_values values               source_ids        
#>   <chr>           <chr>          <int> <chr>                <chr>             
#> 1 "724\u001f2020" method_land        2 back-cast | reported FAOSTAT_prod, LUH2
#> 2 "76\u001f2020"  method_land        2 back-cast | reported FAOSTAT_prod, LUH2
```
