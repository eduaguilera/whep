# Write a table to disk safely and verifiably.

One writer for WHEP's tabular artifacts, with the four properties an
ad-hoc
[`nanoparquet::write_parquet()`](https://nanoparquet.r-lib.org/reference/write_parquet.html)
or
[`readr::write_csv()`](https://readr.tidyverse.org/reference/write_delim.html)
call does not have (whep#375):

- **The parent directory is created**, recursively. This is not a
  convenience:
  [`nanoparquet::write_parquet()`](https://nanoparquet.r-lib.org/reference/write_parquet.html)
  given a path whose parent does not exist returns `NULL` and writes
  nothing at all, without warning, so a mistyped output directory looks
  like a successful build until something tries to read the file back.

- **The write is atomic.** The table goes to a temporary file beside the
  target and is renamed onto it only after it has been read back and
  verified. An interrupted, failed or corrupt write therefore leaves the
  previous artifact exactly as it was, instead of replacing hours of
  pipeline output with a truncated file.

- **The artifact is verified before it is published.** For Parquet that
  is
  [`assert_parquet_integrity()`](https://eduaguilera.github.io/whep/reference/assert_parquet_integrity.md)
  plus a row and column-name check; for CSV the file is re-read and its
  header and row count compared. `write_table_checked()` is the atomic,
  format-agnostic layer *on top of*
  [`write_parquet_checked()`](https://eduaguilera.github.io/whep/reference/write_parquet_checked.md),
  which stays the in-place Parquet primitive it already was.

- **Overwriting is a decision.** `overwrite = FALSE` refuses an existing
  target instead of clobbering it.

Column order, column types and text encoding are preserved by round
trip, including for zero-row tables: a Parquet round trip returns an
identical tibble, and a CSV is always written as UTF-8. Prefer Parquet
for anything large: CSV cannot carry types, its verification has to
re-read the whole file, and a zero-column table is refused outright
because its CSV is a zero-byte file, indistinguishable from a write that
never happened.

## Usage

``` r
write_table_checked(
  data,
  path,
  format = c("auto", "parquet", "csv"),
  overwrite = TRUE,
  sidecars = character(),
  ...
)
```

## Arguments

- data:

  Table to write. Must be a data frame; a tibble is returned unchanged
  by the round trip.

- path:

  Destination path. Its parent is created if needed.

- format:

  Output format: `"parquet"`, `"csv"`, or `"auto"` to take it from the
  file extension.

- overwrite:

  Whether an existing target may be replaced.

- sidecars:

  Which sidecars to write: any of `"schema"` and `"provenance"`.
  Defaults to none.

- ...:

  Passed to the underlying writer,
  [`nanoparquet::write_parquet()`](https://nanoparquet.r-lib.org/reference/write_parquet.html)
  or
  [`readr::write_csv()`](https://readr.tidyverse.org/reference/write_delim.html).

## Value

Invisibly, a one-row tibble describing what was written: `path`,
`format`, `n_rows`, `n_cols`, `bytes`, `md5`, `schema_path` and
`provenance_path` (the last two `NA` when the sidecar was not
requested).

## Sidecar contract

`sidecars` optionally writes YAML files beside the artifact, named after
it: `<path>.schema.yaml` and `<path>.provenance.yaml`. They are written
after the data file has landed, so the data file is never waiting on
them.

The schema sidecar is
`{format: "whep-table-schema/1", n_rows: <int>, columns: [{name, type}]}`
with `columns` in the table's own column order. `type` is the column's
first class, with `"numeric"` reported as `"double"` so that every
atomic type name is one [`vector()`](https://rdrr.io/r/base/vector.html)
accepts (`"integer"`, `"double"`, `"character"`, `"logical"`, `"list"`);
a classed column reports its class (`"factor"`, `"Date"`, `"POSIXct"`).
A consumer can therefore rebuild the prototype with no WHEP-specific
lookup. This is deliberately the same shape a declarative schema
validator (whep#373) and a typed empty-table constructor (whep#374)
would consume; neither exists yet, and this function does not implement
either.

The provenance sidecar is `{format: "whep-table-provenance/1"}` plus
`path`, `table_format`, `n_rows`, `n_cols`, `bytes`, `md5`, `written_at`
(UTC, ISO 8601), `whep_version`, `r_version` and `writer`. Every field
except `written_at` is a function of the artifact, so two builds of the
same table differ only in that field.

## Examples

``` r
path <- tempfile(fileext = ".parquet")
table <- tibble::tibble(area_code = 724L, year = 2020L, value = 1.5)
manifest <- write_table_checked(table, path, sidecars = "schema")
manifest$n_rows
#> [1] 1
nanoparquet::read_parquet(path)
#> # A data frame: 1 × 3
#>   area_code  year value
#>       <int> <int> <dbl>
#> 1       724  2020   1.5
unlink(c(path, manifest$schema_path))
```
